MODULE XYplane;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT WinApi, Ports, Stores, Views, Properties, HostPorts, HostWindows;

	CONST
		erase* = 0; draw* = 1;
		maxW = 256; maxH = 256;

	TYPE
		View = POINTER TO RECORD (Views.View)
			dc: WinApi.HANDLE;
			map: WinApi.HANDLE
		END;

	VAR
		X*, Y*, W*, H*: INTEGER;
		current: View;
		frame: Views.Frame;

	PROCEDURE (v: View) FINALIZE;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.DeleteDC(v.dc);
		res := WinApi.DeleteObject(v.map);
		v.dc := 0; v.map := 0
	END FINALIZE;

	PROCEDURE Open*;
		VAR res: INTEGER; v: View; dc: WinApi.HANDLE;
	BEGIN
		NEW(v);
		dc := WinApi.GetDC(0);
		v.dc := WinApi.CreateCompatibleDC(dc);
		v.map := WinApi.CreateCompatibleBitmap(dc, W, H);
		res := WinApi.SelectObject(v.dc, v.map);
		res := WinApi.ReleaseDC(0, dc);
		res := WinApi.SelectObject(v.dc, WinApi.GetStockObject(WinApi.WHITE_BRUSH));
		res := WinApi.SelectObject(v.dc, WinApi.GetStockObject(WinApi.NULL_PEN));
		res := WinApi.Rectangle(v.dc, 0, 0, W + 1, H + 1);
		Views.OpenAux(v, "XYplane");
		current := v
	END Open;

	PROCEDURE Dot* (x, y, mode: INTEGER);
		VAR res, u: INTEGER; p: HostPorts.Port; col: Ports.Color;
	BEGIN
		IF (x >= 0) & (x < maxW) & (y >= 0) & (y < maxH) THEN
			y := maxH - 1 - y;
			IF mode = draw THEN col := Ports.black ELSE col := Ports.white END;
			res := WinApi.SetPixel(current.dc, x, y, col);
			IF (frame # NIL) & (frame.rider # NIL) THEN
				u := frame.unit; p := frame.rider(HostPorts.Rider).port;
				res := WinApi.SetPixel(p.dc, frame.gx DIV u + x, frame.gy DIV u + y, col);
				IF res = -1 THEN
					frame.DrawRect(x * u, y * u, (x + 1) * u, (y + 1) * u, Ports.fill, col)
				END
			END
		END
	END Dot;

	PROCEDURE IsDot* (x, y: INTEGER): BOOLEAN;
	BEGIN
		RETURN WinApi.GetPixel(current.dc, x, maxH - 1 - y) # Ports.white
	END IsDot;

	PROCEDURE ReadKey* (): CHAR;
		VAR res: INTEGER; msg: WinApi.MSG;
	BEGIN
		IF WinApi.PeekMessageW(msg, 0, WinApi.WM_KEYDOWN, WinApi.WM_CHAR, 1) # 0 THEN
			IF msg.message = WinApi.WM_CHAR THEN
				RETURN CHR(msg.wParam)
			ELSE
				res := WinApi.TranslateMessage(msg);
				res := WinApi.DispatchMessageW(msg)
			END
		END;
		RETURN 0X
	END ReadKey;

	PROCEDURE Clear*;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.Rectangle(current.dc, 0, 0, W + 1, H + 1);
		Views.Update(current, Views.keepFrames)
	END Clear;


	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; res, x, y, i: INTEGER; dc: WinApi.HANDLE; s: SET;
	BEGIN
		v.Internalize^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(0, 0, version);
			IF ~rd.cancelled THEN
				dc := WinApi.GetDC(0);
				v.dc := WinApi.CreateCompatibleDC(dc);
				v.map := WinApi.CreateCompatibleBitmap(dc, W, H);
				res := WinApi.SelectObject(v.dc, v.map);
				res := WinApi.ReleaseDC(0, dc);
				res := WinApi.SelectObject(v.dc, WinApi.GetStockObject(WinApi.WHITE_BRUSH));
				res := WinApi.SelectObject(v.dc, WinApi.GetStockObject(WinApi.NULL_PEN));
				res := WinApi.Rectangle(v.dc, 0, 0, W + 1, H + 1);
				y := 0;
				WHILE y < maxH DO
					x := 0;
					WHILE x < maxW DO
						rd.ReadSet(s); i := 0;
						WHILE i < 32 DO
							IF i IN s THEN res := WinApi.SetPixel(v.dc, x, y, Ports.black) END;
							INC(i); INC(x)
						END
					END;
					INC(y)
				END
			END
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
		VAR x, y, i: INTEGER; s: SET;
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(0);
		y := 0;
		WHILE y < maxH DO
			x := 0;
			WHILE x < maxW DO
				i := 0; s := {};
				WHILE i < 32 DO
					IF WinApi.GetPixel(v.dc, x, y) # Ports.white THEN INCL(s, i) END;
					INC(i); INC(x)
				END;
				wr.WriteSet(s)
			END;
			INC(y)
		END
	END Externalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
		VAR res: INTEGER;
	BEGIN
		WITH source: View DO v.dc := source.dc; 
			v.dc := WinApi.CreateCompatibleDC(source.dc);
			v.map := WinApi.CreateCompatibleBitmap(source.dc, W, H);
			res := WinApi.SelectObject(v.dc, v.map);
			res := WinApi.BitBlt(v.dc, 0, 0, W, H, source.dc, 0, 0, 00CC0020H)
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR rd: HostPorts.Rider; x, y, u, rl, rt, rr, rb: INTEGER;
	BEGIN
		rd := f.rider(HostPorts.Rider);
		IF rd.port.wnd # 0 THEN	(* copy to screen *)
			frame := f;
			f.rider.GetRect(rl, rt, rr, rb);
			rd.CopyFrom(v.dc, rl - f.gx DIV f.unit, rt - f.gy DIV f.unit)
		ELSE	(* copy to printer *)
			u := HostWindows.unit; y := 0;
			WHILE y < maxH DO
				x := 0;
				WHILE x < maxW DO
					IF WinApi.GetPixel(v.dc, x, y) # Ports.white THEN
						f.DrawRect(x * u, y * u, (x + 1) * u, (y + 1) * u, Ports.fill, Ports.black)
					END;
					INC(x)
				END;
				INC(y)
			END
		END
	END Restore;
	
	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			msg.w := W * HostWindows.unit;
			msg.h := H * HostWindows.unit
		| msg: Properties.ResizePref DO
			msg.fixed := TRUE
		ELSE
		END
	END HandlePropMsg;
	
BEGIN
	X := 0; Y := 0; W := maxW; H := maxH
END XYplane.
