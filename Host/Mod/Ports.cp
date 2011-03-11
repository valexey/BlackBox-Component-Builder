MODULE HostPorts;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Marco Ciot, Alexander Iljin"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT
		SYSTEM, WinApi, Kernel, Fonts, Ports, Dialog, Services, HostFonts;
	
	CONST
		resizeHCursor* = 16; resizeVCursor* = 17; resizeLCursor* = 18; resizeRCursor* = 19; resizeCursor* = 20;
		busyCursor* = 21; stopCursor* = 22;
		moveCursor* = 23; copyCursor* = 24; linkCursor* = 25; pickCursor* = 26;
		focusPat* = 5;
		
		extend = 1; modify = 2; (* same as Controllers.extend and Controllers.modify  !!! *)

		(** buttons **)
		left* = 16; middle* = 17; right* = 18;
		shift* = 24; ctrl* = 25; opt* = 26; cmd* = 27; alt* = 28;
		
		copy = 00CC0020H;	(* raster code for source copy *)
		xor = 00A50065H;	(* raster code for xor function *)
		figureSpace = 8FX;
		
	TYPE
		Port* = POINTER TO RECORD (Ports.Port)
			w-, h-: INTEGER;
			dc-: WinApi.HANDLE;	(* actual dc *)
			wnd-: WinApi.HANDLE;	(* used for invalidation, may be null *)
			homedc-: WinApi.HANDLE;	(* unbuffered dc *)
			map: WinApi.HANDLE;	(* buffer bitmap *)
			bl, bt, br, bb: INTEGER;	(* buffer rectangle *)
			bx, by: INTEGER	(* bitmap pattern offset *)
		END;

		Rider* = POINTER TO EXTENSIBLE RECORD (Ports.Rider)
			l-, t-, r-, b-: INTEGER;
			port-: Port;
			dx, dy: INTEGER;	(* scroll offset *)
			map: WinApi.HANDLE;	(* save bitmap *)
			dc: WinApi.HANDLE;	(* save dc *)
			sl, st, sr, sb: INTEGER	(* save rect *)
		END;


	VAR
		(* system colors *)
		textCol-, selBackground-, selTextCol-,
		dialogTextCol-, dialogShadowCol-, dialogLightCol-: Ports.Color;
		
		dialogBrush*, dim50Brush-: WinApi.HANDLE;
		
		debug*, noBuffer*, flag*: BOOLEAN;

		cursors-: ARRAY 32 OF WinApi.HANDLE;
		
		nullBrush, nullPen: WinApi.HANDLE;
		invertBrush, dim25Brush, dim75Brush, focusBrush: WinApi.HANDLE;
		grgn: WinApi.HANDLE;
		mx, my: INTEGER;	(* actual mouse coordinates *)
		mb: SET;	(* actual mouse buttons & modifiers *)
		

	PROCEDURE Wait;
		VAR t: LONGINT;
	BEGIN
		t := Kernel.Time() + Kernel.timeResolution;
		REPEAT UNTIL Kernel.Time() > t
	END Wait;
	
		
	(** Port **)

	PROCEDURE (p: Port) SetSize* (w, h: INTEGER);
	BEGIN
		ASSERT(w >= 0, 20); ASSERT(h >= 0, 21);
		p.w := w; p.h := h
	END SetSize;
	
	PROCEDURE (p: Port) GetSize* (OUT w, h: INTEGER);
	BEGIN
		w := p.w; h := p.h
	END GetSize;
	
	PROCEDURE (p: Port) NewRider* (): Rider;
		VAR h: Rider;
	BEGIN
		NEW(h); h.port := p; RETURN h
	END NewRider;
	
	PROCEDURE (p: Port) SetDC* (dc, wnd: WinApi.HANDLE), NEW;
		VAR res: INTEGER;
	BEGIN
		ASSERT(p.unit # 0, 20); ASSERT(dc # 0, 21);
		p.dc := dc; p.homedc := dc;
		res := WinApi.SetBkMode(dc, WinApi.TRANSPARENT);
		res := ORD(WinApi.SetTextAlign(dc, BITS(24)));
		res := WinApi.SetPolyFillMode(dc, 2);
		res := WinApi.SelectObject(dc, nullPen);
		res := WinApi.SelectObject(dc, nullBrush);
		p.wnd := wnd
	END SetDC;

	PROCEDURE (p: Port) OpenBuffer* (l, t, r, b: INTEGER);
		VAR res: INTEGER; dc: WinApi.HANDLE; pt: WinApi.POINT;
	BEGIN
		ASSERT(p.dc # 0, 20);
		IF l < 0 THEN l := 0 END;
		IF t < 0 THEN t := 0 END;
		IF r > p.w THEN r := p.w END;
		IF b > p.h THEN b := p.h END;
		IF (l < r) & (t < b) THEN
			p.bl := l; p.bt := t; p.br := r; p.bb := b;
			IF ~noBuffer THEN
				dc := WinApi.CreateCompatibleDC(p.homedc);
				IF dc # 0 THEN
					IF p.wnd # 0 THEN	(* byte align bitmap to screen *)
						pt.x := l; pt.y := 0;
						res := WinApi.ClientToScreen(p.wnd, pt);
						pt.x := pt.x DIV 8 * 8;
						res := WinApi.ScreenToClient(p.wnd, pt);
						l := pt.x
					END;
					p.map := WinApi.CreateCompatibleBitmap(p.homedc, r - l, b - t);
					IF p.map # 0 THEN
						res := WinApi.SelectObject(dc, p.map);
						res := WinApi.SetBkMode(dc, WinApi.TRANSPARENT);
						res := ORD(WinApi.SetTextAlign(dc, BITS(24)));
						res := WinApi.SetPolyFillMode(dc, 2);
						res := WinApi.SetWindowOrgEx(dc, l, t, NIL);
						res := WinApi.SelectObject(dc, nullPen);
						res := WinApi.SelectObject(dc, nullBrush);
						p.dc := dc; p.bx := l; p.by := t;
					ELSE
						res := WinApi.DeleteDC(dc)
					END
				END
			END
		END
	END OpenBuffer;
	
	PROCEDURE (p: Port) CloseBuffer*;
		VAR res: INTEGER; rect: WinApi.RECT;
	BEGIN
		IF p.map # 0 THEN
			res := WinApi.SelectClipRgn(p.homedc, 0);
			res := WinApi.BitBlt(p.homedc, p.bl, p.bt, p.br - p.bl, p.bb - p.bt, p.dc, p.bl, p.bt, copy);
			res := WinApi.DeleteDC(p.dc);
			res := WinApi.DeleteObject(p.map)
		END;
(*
		IF p.wnd # 0 THEN
			rect.left := p.bl; rect.top := p.bt; rect.right := p.br; rect.bottom := p.bb;
			res := WinApi.ValidateRect(p.wnd, rect)
		END;
*)
		p.dc := p.homedc; p.map := 0;
		p.bx := 0; p.by := 0
	END CloseBuffer;
	

	(** Rider **)

	PROCEDURE (rd: Rider) Base* (): Ports.Port, EXTENSIBLE;
	BEGIN
		ASSERT(rd.port # NIL, 20); ASSERT(rd.port.dc # 0, 21);
		RETURN rd.port
	END Base;

	PROCEDURE (rd: Rider) SetRect* (l, t, r, b: INTEGER);
	BEGIN
		ASSERT((l <= r) & (t <= b), 20);
		ASSERT(rd.port # NIL, 21);
		rd.l := l; rd.t := t; rd.r := r; rd.b := b
	END SetRect;

	PROCEDURE (rd: Rider) GetRect* (OUT l, t, r, b: INTEGER);
	BEGIN
		l := rd.l; t := rd.t; r := rd.r; b := rd.b
	END GetRect;

	PROCEDURE SetClipRegion (rd: Rider; dc: WinApi.HANDLE);
		VAR res, res1: INTEGER;
	BEGIN
		IF rd.port.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b)
	END SetClipRegion;

	PROCEDURE (rd: Rider) InitPort* (port: Port), NEW;
	BEGIN
		ASSERT(rd.port = NIL, 20); ASSERT(port # NIL, 21); ASSERT(port.dc # 0, 22);
		rd.port := port; rd.dx := 0; rd.dy := 0
	END InitPort;

	PROCEDURE (rd: Rider) Move* (dx, dy: INTEGER);
	BEGIN
		INC(rd.dx, dx); INC(rd.dy, dy)
	END Move;

	PROCEDURE (rd: Rider) DrawRect* (l, t, r, b, s: INTEGER; col: Ports.Color);
		VAR res, h: INTEGER; p: Port; dc, oldb, oldp: WinApi.HANDLE; pt: WinApi.POINT;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		IF col = Ports.defaultColor THEN col := textCol END;
		IF s = 0 THEN s := 1 END;
		IF (s < 0) OR (r-l < 2*s) OR (b-t < 2*s) THEN
			INC(r); INC(b);
			IF (col # textCol) & (col # Ports.background) THEN
				res := WinApi.SetBrushOrgEx(dc, (rd.dx - p.bx) MOD 8, (rd.dy - p.by) MOD 8, NIL)
			END;
			oldb := WinApi.SelectObject(dc, WinApi.CreateSolidBrush(col));
			oldp := WinApi.SelectObject(dc, nullPen);
			res := WinApi.Rectangle(dc, l, t, r, b);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldb));
			res := WinApi.SelectObject(dc, oldp)
		ELSE
			h := s DIV 2; INC(l, h); INC(t, h); h := (s-1) DIV 2; DEC(r, h); DEC(b, h);
			oldb := WinApi.SelectObject(dc, nullBrush);
			oldp := WinApi.SelectObject(dc, WinApi.CreatePen(WinApi.PS_SOLID, s, col));
			res := WinApi.Rectangle(dc, l, t, r, b);
			res := WinApi.SelectObject(dc, oldb);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldp))
		END;
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		;IF debug THEN Wait END
	END DrawRect;

	PROCEDURE (rd: Rider) DrawOval* (l, t, r, b, s: INTEGER; col: Ports.Color);
		VAR res, h: INTEGER; p: Port; dc, oldb, oldp: WinApi.HANDLE; pt: WinApi.POINT; rect: WinApi.RECT;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		IF col = Ports.defaultColor THEN col := textCol END;
		IF s < 0 THEN
			INC(r); INC(b);
			IF (col # textCol) & (col # Ports.background) THEN
				res := WinApi.SetBrushOrgEx(dc, (rd.dx - p.bx) MOD 8, (rd.dy - p.by) MOD 8, NIL)
			END;
			oldb := WinApi.SelectObject(dc, WinApi.CreateSolidBrush(col));
			oldp := WinApi.SelectObject(dc, nullPen);
			res := WinApi.Ellipse(dc, l, t, r, b);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldb));
			res := WinApi.SelectObject(dc, oldp)
		ELSE
			IF s = 0 THEN s := 1 END;
			h := s DIV 2; INC(l, h); INC(t, h); h := (s-1) DIV 2; DEC(r, h); DEC(b, h);
			oldb := WinApi.SelectObject(dc, nullBrush);
			oldp := WinApi.SelectObject(dc, WinApi.CreatePen(WinApi.PS_SOLID, s, col));
			res := WinApi.Ellipse(dc, l, t, r, b);
			res := WinApi.SelectObject(dc, oldb);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldp))
		END;
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		;IF debug THEN Wait END
	END DrawOval;

	PROCEDURE (rd: Rider) DrawLine* (x0, y0, x1, y1, s: INTEGER; col: Ports.Color);
		VAR res: INTEGER; pt: WinApi.POINT; p: Port; dc, oldb, oldp: WinApi.HANDLE;
	BEGIN
		ASSERT(s >= 0, 20);
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		IF col = Ports.defaultColor THEN col := textCol END;
		IF s <= 0 THEN s := 1 END;
		oldp := WinApi.SelectObject(dc, WinApi.CreatePen(WinApi.PS_SOLID, s, col));
		res := WinApi.MoveToEx(dc, x0, y0, pt);
		res := WinApi.LineTo(dc, x1, y1);
		res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldp));
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		;IF debug THEN Wait END
	END DrawLine;
	
	PROCEDURE (rd: Rider) DrawPath* (
		IN pts: ARRAY OF Ports.Point; n, s: INTEGER; col: Ports.Color; path: INTEGER
	);

		TYPE
			PAP = POINTER TO ARRAY [1] OF WinApi.POINT;

		VAR
			res, i, j, k: INTEGER; p: Port; dc, oldp, oldb: WinApi.HANDLE;
			pap: PAP; pt: WinApi.POINT; poly: ARRAY 256 OF WinApi.POINT;
			polyPtr: POINTER TO ARRAY OF Ports.Point; polyLen: INTEGER;

		PROCEDURE Bezier(x0, y0, xd0, yd0, x1, y1, xd1, yd1: INTEGER);
			VAR x, y, xd, yd, i: INTEGER;
		BEGIN
			IF ABS(x0 + xd0 - x1) + ABS(x0 + xd1 - x1) + ABS(y0 + yd0 - y1) + ABS(y0 + yd1 - y1) < 8 THEN
				IF k > polyLen - 2 THEN
					NEW(polyPtr, polyLen * 2);
					i := 0; WHILE i < polyLen DO polyPtr[i] := SYSTEM.VAL(Ports.Point, pap[i]); INC(i) END;
					polyLen := polyLen * 2; pap := SYSTEM.VAL(PAP, SYSTEM.ADR(polyPtr^))
				END;
				pap[k].x := x0; pap[k].y := y0; INC(k)
			ELSE
				x := ((xd0 - xd1) DIV 4 + x0 + x1 + 1) DIV 2;
				y := ((yd0 - yd1) DIV 4 + y0 + y1 + 1) DIV 2;
				xd := ((x1 - x0) * 3 - (xd0 + xd1) DIV 2 + 2) DIV 4;
				yd := ((y1 - y0) * 3 - (yd0 + yd1) DIV 2 + 2) DIV 4;
				Bezier(x0, y0, xd0 DIV 2, yd0 DIV 2, x, y, xd, yd);
				Bezier(x, y, xd, yd, x1, y1, xd1 DIV 2, yd1 DIV 2)
			END
		END Bezier;
	
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		IF col = Ports.defaultColor THEN col := textCol END;
		pap := SYSTEM.VAL(PAP, SYSTEM.ADR(pts));
		
		ASSERT(n >= 0, 20); ASSERT(n <= LEN(pts), 21);
		ASSERT(s >= Ports.fill, 23);
		IF s < 0 THEN
			res := WinApi.SetBrushOrgEx(dc, (rd.dx - p.bx) MOD 8, (rd.dy - p.by) MOD 8, NIL);
			oldb := WinApi.SelectObject(dc, WinApi.CreateSolidBrush(col));
			oldp := WinApi.SelectObject(dc, nullPen);
			IF path = Ports.closedPoly THEN
				ASSERT(n >= 2, 20);
				res := WinApi.Polygon(dc, pap[0], n)
			ELSE
				ASSERT(n >= 3, 20);
				ASSERT(path = Ports.closedBezier, 22);
				ASSERT(n MOD 3 = 0, 24);
				pap := SYSTEM.VAL(PAP, SYSTEM.ADR(poly)); polyLen := LEN(poly);
				i := 0; k := 0;
				WHILE i < n DO
					j := i+3;
					IF j = n THEN j := 0 END;
					Bezier(pts[i].x, pts[i].y, (pts[i+1].x - pts[i].x) * 3, (pts[i+1].y - pts[i].y) * 3,
							pts[j].x, pts[j].y, (pts[j].x - pts[i+2].x) * 3, (pts[j].y - pts[i+2].y) * 3);
					INC(i, 3)
				END;
				res := WinApi.Polygon(dc, pap[0], k)
			END;
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldb));
			res := WinApi.SelectObject(dc, oldp)
		ELSE
			IF s = 0 THEN s := 1 END;
			oldb := WinApi.SelectObject(dc, nullBrush);
			oldp := WinApi.SelectObject(dc, WinApi.CreatePen(WinApi.PS_SOLID, s, col));
			IF path = Ports.closedPoly THEN
				ASSERT(n >= 2, 20);
				res := WinApi.Polygon(dc, pap[0], n)
			ELSIF path = Ports.openPoly THEN
				ASSERT(n >= 2, 20);
				res := WinApi.Polyline(dc, pap[0], n)
			ELSE
				IF path = Ports.closedBezier THEN
					ASSERT(n >= 3, 20);
					ASSERT(n MOD 3 = 0, 24)
				ELSE
					ASSERT(n >= 4, 20);
					ASSERT(path = Ports.openBezier, 25);
					ASSERT(n MOD 3 = 1, 24)
				END;
				pap := SYSTEM.VAL(PAP, SYSTEM.ADR(poly)); polyLen := LEN(poly);
				i := 0;
				WHILE i < n-2 DO
					k := 0; j := i+3;
					IF j = n THEN j := 0 END;
					Bezier(pts[i].x, pts[i].y, (pts[i+1].x - pts[i].x) * 3, (pts[i+1].y - pts[i].y) * 3,
							pts[j].x, pts[j].y, (pts[j].x - pts[i+2].x) * 3, (pts[j].y - pts[i+2].y) * 3);
					pap[k].x := pts[j].x; pap[k].y := pts[j].y; INC(k);
					res := WinApi.Polyline(dc, pap[0], k);
					INC(i, 3)
				END
			END;
			res := WinApi.SelectObject(dc, oldb);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldp))
		END;
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		;IF debug THEN Wait END
	END DrawPath;

	PROCEDURE (rd: Rider) MarkRect* (l, t, r, b, s, mode: INTEGER; show: BOOLEAN);
		VAR res: INTEGER; p: Port; dc, old: WinApi.HANDLE; pt: WinApi.POINT;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		res := WinApi.SetBrushOrgEx(dc, (rd.dx - p.bx) MOD 8, (rd.dy - p.by) MOD 8, NIL);
		IF (mode = Ports.invert) OR (mode = Ports.hilite) THEN
			old := WinApi.SelectObject(dc, invertBrush)
		ELSIF mode = Ports.dim25 THEN
			res := WinApi.UnrealizeObject(dim25Brush); old := WinApi.SelectObject(dc, dim25Brush)
		ELSIF mode = Ports.dim50 THEN
			res := WinApi.UnrealizeObject(dim50Brush); old := WinApi.SelectObject(dc, dim50Brush)
		ELSIF mode = Ports.dim75 THEN
			res := WinApi.UnrealizeObject(dim75Brush); old := WinApi.SelectObject(dc, dim75Brush)
		ELSE (* mode = focusPat *)
			res := WinApi.UnrealizeObject(focusBrush); old := WinApi.SelectObject(dc, focusBrush)
		END;
		res := WinApi.SetTextColor(dc, Ports.black);
		
		IF l < -32768 THEN l := -32768 END;	(* ??? *)
		IF t < -32768 THEN t := -32768 END;
		IF r > 32767 THEN r := 32767 END;
		IF b > 32767 THEN b := 32767 END;
		
		IF s = 0 THEN s := 1 END;
		IF (s < 0) OR (r-l < 2*s) OR (b-t < 2*s) THEN
			res := WinApi.PatBlt(dc, l, t, r-l, b-t, xor);
		ELSE
			res := WinApi.PatBlt(dc, l, t, s, b-t, xor); DEC(r, s);
			res := WinApi.PatBlt(dc, r, t, s, b-t, xor); INC(l, s);
			res := WinApi.PatBlt(dc, l, t, r-l, s, xor); DEC(b, s);
			res := WinApi.PatBlt(dc, l, b, r-l, s, xor);
		END;
		old := WinApi.SelectObject(dc, old);
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		;IF debug THEN Wait END
	END MarkRect;

	
	PROCEDURE (rd: Rider) Scroll* (dx, dy: INTEGER);
		VAR res: INTEGER; p: Port; dc, par: WinApi.HANDLE; rect, rt: WinApi.RECT; pt: WinApi.POINT;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
(*
		IF p.wnd # 0 THEN res := WinApi.UpdateWindow(p.wnd) END;
*)
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		rect.left := rd.l; rect.top := rd.t; rect.right := rd.r; rect.bottom := rd.b;
		res := WinApi.ScrollDC(dc, dx, dy, rect, rect, grgn, rt);
		IF p.wnd # 0 THEN
			res := WinApi.InvalidateRgn(p.wnd, grgn, 1);
			par := WinApi.GetParent(p.wnd);
			IF (par # 0) & (WinApi.GetTopWindow(par) # p.wnd) THEN
				res := WinApi.UpdateWindow(p.wnd)
			END
		END;
		(* pattern origin correction *)
		INC(rd.dx, dx); INC(rd.dy, dy);
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		;IF debug THEN Wait END
	END Scroll;


	PROCEDURE (rd: Rider) SetCursor* (cursor: INTEGER);
		VAR old: WinApi.HANDLE;
	BEGIN
		old := WinApi.SetCursor(cursors[cursor])
	END SetCursor;
	
	PROCEDURE SetMouseState* (x, y: INTEGER; but: SET; isDown: BOOLEAN);
	BEGIN
		mx := x; my := y; mb := but
	END SetMouseState;
	
	PROCEDURE (rd: Rider) Input* (OUT x, y: INTEGER; OUT modifiers: SET; OUT isDown: BOOLEAN);
		VAR msg: WinApi.MSG; wnd, mw: WinApi.HANDLE; pt: WinApi.POINT; res: INTEGER; set: SET;
	BEGIN
		WinApi.Sleep(1);
		wnd := rd.port.wnd; mw := WinApi.GetCapture();
		res := WinApi.UpdateWindow(wnd);
		IF WinApi.PeekMessageW(msg, mw, WinApi.WM_MOUSEMOVE,
															WinApi.WM_MBUTTONDBLCLK, 1) # 0 THEN
			mx := (msg.lParam + 32768) MOD 65536 - 32768; my := msg.lParam DIV 65536;
			IF (mw # 0) & (wnd # mw) THEN
				pt.x := mx; pt.y := my; res := WinApi.ClientToScreen(mw, pt);
				res := WinApi.ScreenToClient(wnd, pt); mx := pt.x; my := pt.y
			END;
			mb := {};
			set := SYSTEM.VAL(SET, msg.wParam);
			IF WinApi.MK_LBUTTON * set # {} THEN INCL(mb, left) END;
			IF WinApi.MK_MBUTTON * set # {} THEN INCL(mb, middle) END;
			IF WinApi.MK_RBUTTON * set # {} THEN INCL(mb, right) END;
			IF WinApi.MK_CONTROL * set # {} THEN INCL(mb, ctrl) END;
			IF WinApi.MK_SHIFT * set # {} THEN INCL(mb, shift) END;
			IF WinApi.GetAsyncKeyState(12H) < 0 THEN INCL(mb, alt) END;
		END;
		IF WinApi.GetSystemMetrics(WinApi.SM_SWAPBUTTON) # 0 THEN
			IF WinApi.GetAsyncKeyState(1) >= 0 THEN EXCL(mb, right) END;
			IF WinApi.GetAsyncKeyState(2) >= 0 THEN EXCL(mb, left) END
		ELSE
			IF WinApi.GetAsyncKeyState(1) >= 0 THEN EXCL(mb, left) END;
			IF WinApi.GetAsyncKeyState(2) >= 0 THEN EXCL(mb, right) END
		END;
		IF WinApi.GetAsyncKeyState(4) >= 0 THEN EXCL(mb, middle) END;
		IF WinApi.GetAsyncKeyState(WinApi.VK_SHIFT) < 0 THEN
			mb := mb + {shift, extend} ELSE  mb := mb - {shift, extend}
		END;
		IF WinApi.GetAsyncKeyState(WinApi.VK_CONTROL) < 0 THEN
			mb := mb + {ctrl, modify} ELSE  mb := mb - {ctrl, modify}
		END;
		IF WinApi.GetAsyncKeyState(WinApi.VK_MENU) < 0 THEN INCL(mb, alt) ELSE EXCL(mb, alt) END;
		x := mx; y := my; modifiers := mb; isDown := mb * {left, middle, right} # {}
	END Input;
	
	PROCEDURE (rd: Rider) DrawSString* (
		x, y: INTEGER; col: Ports.Color; IN s: ARRAY OF SHORTCHAR; font: Fonts.Font
	);
		VAR res, i, a, b, c, w, u, n: INTEGER; p: Port; dc, old: WinApi.HANDLE; ch: SHORTCHAR;
			df: HostFonts.DevFont; dx: ARRAY 1024 OF INTEGER;
			s1: ARRAY 1024 OF SHORTCHAR; fsp: BOOLEAN;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		WITH font: HostFonts.Font DO
			p := rd.port; dc := p.dc; u := p.unit;
			IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
			ELSE res := WinApi.SelectClipRgn(dc, 0);
			END;
			res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
			df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			IF df = NIL THEN HostFonts.InsertDevFont(dc, font, df, u) 
			ELSE old := WinApi.SelectObject(dc, df.id)
			END;
			IF col = Ports.defaultColor THEN col := textCol END;
			res := WinApi.SetTextColor(dc, col);
			IF df.noGap THEN INC(x) END;	(* caret pos optimization *)
			INC(x, font.ftab[ORD(s[0])] DIV u);
			a := LEN(s) - 1; n := 0; ch := s[0]; fsp := FALSE;
			WHILE (n < a) & (ch # 0X) DO
				IF ch = figureSpace THEN fsp := TRUE END;
				INC(n); ch := s[n]
			END;
			IF (df.id = font.id) & ~fsp THEN	(* native metric *)
				res := WinApi.TextOutA(dc, x, y, s, n)
			ELSE	(* adapt to meta metric *)
				a := 0; b := 0; i := 0; ch := s[0];
				WHILE i < n DO
					c := df.wtab[ORD(ch)]; INC(a, c); dx[i] := c;
					INC(b, font.wtab[ORD(ch)]);
					s1[i] := s[i];
					IF ch = figureSpace THEN s1[i] := " " END;
					INC(i); ch := s[i]
				END;
				n := i; c := b DIV u - a; i := 0; w := 0; a := 0;
				WHILE i < n DO
					INC(w, c); b := w DIV n; INC(dx[i], b - a); a := b; INC(i)
				END;
				res := WinApi.ExtTextOutA(dc, x, y, 0, NIL, s1, n, dx[0])
			END
		END;
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
	END DrawSString;

	PROCEDURE (rd: Rider) SCharIndex* (
		x, pos: INTEGER; IN s: ARRAY OF SHORTCHAR; font: Fonts.Font
	): INTEGER;
		VAR d, u, i, n, a, b, c, w: INTEGER; df: HostFonts.DevFont; ch: SHORTCHAR; dc: WinApi.HANDLE;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		WITH font: HostFonts.Font DO
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			IF df = NIL THEN HostFonts.InsertDevFont(rd.port.dc, font, df, u) END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0; ch := s[0];
			WHILE (i < n) & (ch # 0X) DO
				INC(a, df.wtab[ORD(ch)]);
				INC(b, font.wtab[ORD(ch)]);
				INC(i); ch := s[i]
			END;
			n := i; c := b DIV u - a; i := 0; w := 0; a := 0;
			INC(x, font.ftab[ORD(s[0])] DIV u);
			d := df.wtab[ORD(s[0])];
			WHILE (i < n) & (pos > x + d DIV 2) DO
				INC(w, c); b := w DIV n; INC(d, b - a); a := b;
				INC(i); INC(x, d); d := df.wtab[ORD(s[i])]
			END
		END;
		RETURN i
	END SCharIndex;

	PROCEDURE (rd: Rider) SCharPos* (
		x, index: INTEGER; IN s: ARRAY OF SHORTCHAR; font: Fonts.Font
	): INTEGER;
		VAR i, u, n, a, b, c, w: INTEGER; df: HostFonts.DevFont; ch: SHORTCHAR;
	BEGIN
		ASSERT(rd.port # NIL, 100); ASSERT(index <= LEN(s), 101);
		WITH font: HostFonts.Font DO
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			IF df = NIL THEN HostFonts.InsertDevFont(rd.port.dc, font, df, u) END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0; ch := s[0];
			WHILE (i < n) & (ch # 0X) DO
				INC(a, df.wtab[ORD(ch)]);
				INC(b, font.wtab[ORD(ch)]);
				INC(i); ch := s[i]
			END;
			n := i; c := b DIV u - a; i := 0; w := 0; a := 0;
			INC(x, font.ftab[ORD(s[0])] DIV u);
			WHILE (i < n) & (i < index) DO
				INC(w, c); b := w DIV n; INC(x, b - a); a := b;
				INC(x, df.wtab[ORD(s[i])]); INC(i)
			END
		END;
		RETURN x
	END SCharPos;

	PROCEDURE (rd: Rider) DrawString* (
		x, y: INTEGER; col: Ports.Color; IN s: ARRAY OF CHAR; font: Fonts.Font
	);
		VAR res, i, a, b, c, n, w, u: INTEGER; p: Port; dc, old: WinApi.HANDLE;
			df: HostFonts.DevFont; dx: ARRAY 1024 OF INTEGER;
			s1: ARRAY 1024 OF CHAR; fsp: BOOLEAN;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		WITH font: HostFonts.Font DO
			p := rd.port; dc := p.dc; u := p.unit;
			IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
			ELSE res := WinApi.SelectClipRgn(dc, 0);
			END;
			res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
			IF col = Ports.defaultColor THEN col := textCol END;
			df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			n := LEN(s) - 1; i := 0; fsp := FALSE;
			WHILE (i < n) & (s[i] # 0X) DO
				IF s[i] = figureSpace THEN fsp := TRUE END;
				INC(i)
			END;
			IF (df # NIL) & (df.id = font.id) & ~fsp THEN	(* native metric *)
				i := 0;
				WHILE (i < n) & (s[i] # 0X) DO INC(i) END;
				res := WinApi.SelectObject(dc, df.id);
				IF df.noGap THEN INC(x) END;	(* caret pos optimization *)
				res := WinApi.SetTextColor(dc, col);
				res := WinApi.TextOutW(dc, x, y, s, i)
			ELSE	(* adapt to meta metric *)
				old := WinApi.SelectObject(dc, font.id);
				a := 0; b := 0; i := 0;
				WHILE (i < n) & (s[i] # 0X) DO INC(b, font.wTab(dc, s[i])); INC(i) END;
				INC(x, font.fTab(dc, s[0]) DIV u);
				IF df = NIL THEN HostFonts.InsertDevFont(dc, font, df, u)
				ELSE res := WinApi.SelectObject(dc, df.id)
				END;
				n := i; i := 0;
				WHILE i < n DO
					c := df.wTab(dc, s[i]);
					IF s[i] = figureSpace THEN s1[i] := " " ELSE s1[i] := s[i] END;
					INC(a, c); dx[i] := c; INC(i)
				END;
				c := b DIV u - a; i := 0; w := 0; a := 0;
				WHILE i < n DO
					INC(w, c); b := w DIV n; INC(dx[i], b - a); a := b; INC(i)
				END;
				res := WinApi.SetTextColor(dc, col);
				IF df.noGap THEN INC(x) END;	(* caret pos optimization *)
				res := WinApi.ExtTextOutW(dc, x, y, 0, NIL, s1, n, dx[0])
			END
		END;
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
	END DrawString;

	PROCEDURE (rd: Rider) CharIndex* (x, pos: INTEGER; IN s: ARRAY OF CHAR; font: Fonts.Font): INTEGER;
		VAR res, d, u, i, a, b, c, w, n: INTEGER;
			df: HostFonts.DevFont; dc: WinApi.HANDLE;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		dc := rd.port.dc;
		WITH font: HostFonts.Font DO
			res := WinApi.SelectObject(dc, font.id);
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0;
			WHILE (i < n) & (s[i] # 0X) DO INC(b, font.wTab(dc, s[i])); INC(i) END;
			INC(x, font.fTab(dc, s[0]) DIV u);
			IF df = NIL THEN HostFonts.InsertDevFont(dc, font, df, u)
			ELSE res := WinApi.SelectObject(dc, df.id)
			END;
			n := i; i := 0;
			WHILE i < n DO
				c := df.wTab(dc, s[i]);
				INC(a, c); INC(i)
			END;
			c := b DIV u - a; i := 0; w := 0; a := 0;
			d := df.wTab(dc, s[0]);
			WHILE (i < n) & (pos > x + d DIV 2) DO
				INC(w, c); b := w DIV n; INC(d, b - a); a := b;
				INC(i); INC(x, d);
				d := df.wTab(dc, s[i])
			END
		END;
		RETURN i
	END CharIndex;

	PROCEDURE (rd: Rider) CharPos* (x, index: INTEGER; IN s: ARRAY OF CHAR; font: Fonts.Font): INTEGER;
		VAR res, i, u, a, b, c, w, d, n: INTEGER;
			df: HostFonts.DevFont; dc: WinApi.HANDLE;
	BEGIN
		ASSERT(rd.port # NIL, 100); ASSERT(index <= LEN(s), 101);
		dc := rd.port.dc;
		WITH font: HostFonts.Font DO
			res := WinApi.SelectObject(dc, font.id);
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0;
			WHILE (i < n) & (s[i] # 0X) DO INC(b, font.wTab(dc, s[i])); INC(i) END;
			INC(x, font.fTab(dc, s[0]) DIV u);
			IF df = NIL THEN HostFonts.InsertDevFont(dc, font, df, u)
			ELSE res := WinApi.SelectObject(dc, df.id)
			END;
			n := i; i := 0;
			WHILE i < n DO
				c := df.wTab(dc, s[i]);
				INC(a, c); INC(i)
			END;
			c := b DIV u - a; i := 0; w := 0; a := 0;
			WHILE (i < n) & (i < index) DO
				INC(w, c); b := w DIV n; INC(x, b - a); a := b;
				d := df.wTab(dc, s[i]);
				INC(x, d); INC(i)
			END
		END;
		RETURN x
	END CharPos;
	
	PROCEDURE (rd: Rider) SaveRect* (l, t, r, b: INTEGER; VAR res: INTEGER);
		VAR rs: INTEGER; p: Port;
	BEGIN
		res := 1; p := rd.port;
		ASSERT(p.dc # 0, 20);
		IF l < 0 THEN l := 0 END;
		IF t < 0 THEN t := 0 END;
		IF r > p.w THEN r := p.w END;
		IF b > p.h THEN b := p.h END;
		IF (l < r) & (t < b) THEN
			rd.sl := l; rd.st := t; rd.sr := r; rd.sb := b;
			rd.dc := WinApi.CreateCompatibleDC(p.dc);
			IF rd.dc # 0 THEN
				rd.map := WinApi.CreateCompatibleBitmap(p.dc, r - l, b - t);
				IF rd.map # 0 THEN
					rs := WinApi.SelectObject(rd.dc, rd.map);
					rs := WinApi.BitBlt(rd.dc, 0, 0, r - l, b - t, p.dc, l, t, copy);
					res := 0
				ELSE
					rs := WinApi.DeleteDC(rd.dc); rd.dc := 0
				END
			END
		END
	END SaveRect;
	
	PROCEDURE (rd: Rider) RestoreRect* (l, t, r, b: INTEGER; dispose: BOOLEAN);
		VAR res: INTEGER; p: Port; dc: WinApi.HANDLE;
	BEGIN
		IF rd.dc # 0 THEN
			p := rd.port; dc := p.dc;
			IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
			ELSE res := WinApi.SelectClipRgn(dc, 0);
			END;
			res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
			IF l < rd.sl THEN l := rd.sl END;
			IF t < rd.st THEN t := rd.st END;
			IF r > rd.sr THEN r := rd.sr END;
			IF b > rd.sb THEN b := rd.sb END;
			res := WinApi.BitBlt(dc, l, t, r - l, b - t, rd.dc, l - rd.sl, t - rd.st, copy);
			IF dispose THEN
				res := WinApi.DeleteDC(rd.dc);
				res := WinApi.DeleteObject(rd.map);
				rd.dc := 0; rd.map := 0
			END;
			IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
		END
	END RestoreRect;
	
	PROCEDURE (rd: Rider) CopyFrom* (sdc: WinApi.HANDLE; x, y: INTEGER), NEW;
		VAR res: INTEGER; p: Port; dc: WinApi.HANDLE;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port; dc := p.dc;
		IF p.wnd = 0 THEN res := WinApi.SaveDC(dc)
		ELSE res := WinApi.SelectClipRgn(dc, 0);
		END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		res := WinApi.BitBlt(dc, rd.l, rd.t, rd.r - rd.l, rd.b - rd.t, sdc, x, y, copy);
		IF p.wnd = 0 THEN res := WinApi.RestoreDC(dc, -1) END
	END CopyFrom;
	
	PROCEDURE (rd: Rider) DrawBitmap* (bmdc: WinApi.HANDLE; bw, bh, x, y, w, h: INTEGER), NEW;
		VAR res, u: INTEGER; p: Port; dc: WinApi.HANDLE;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port;
		dc := p.dc; u := p.unit;
		res := WinApi.SaveDC(dc);
		IF p.wnd # 0 THEN res := WinApi.SelectClipRgn(dc, 0) END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		IF (bw * u = w) & (bh * u = h) THEN
			res := WinApi.BitBlt(dc, x DIV u, y DIV u, bw, bh, bmdc, 0, 0, copy)
		ELSE
			res := WinApi.StretchBlt(dc, x DIV u, y DIV u, w DIV u, h DIV u, bmdc, 0, 0, bw, bh, copy)
		END;
		res := WinApi.RestoreDC(dc, -1)
	END DrawBitmap;
	
	PROCEDURE (rd: Rider) DrawMetafile* (mf: WinApi.HANDLE; mode, x, y, w, h: INTEGER), NEW;
		VAR res, oldMode, u: INTEGER; p: Port; dc: WinApi.HANDLE;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		p := rd.port;
		p.CloseBuffer;
		dc := p.dc; u := p.unit;
		res := WinApi.SaveDC(dc);
		IF p.wnd # 0 THEN res := WinApi.SelectClipRgn(dc, 0) END;
		res := WinApi.IntersectClipRect(dc, rd.l, rd.t, rd.r, rd.b);
		oldMode := WinApi.SetMapMode(dc, mode);
		res := WinApi.SetViewportOrgEx(dc, x DIV u, y DIV u, NIL);
		res := WinApi.SetViewportExtEx(dc, w DIV u, h DIV u, NIL);
		res := ORD(WinApi.SetTextAlign(dc, {}));
		res := WinApi.PlayMetaFile(dc, mf);
		res := WinApi.SetViewportOrgEx(dc, 0, 0, NIL);
		res := WinApi.SetMapMode(dc, oldMode);
		res := WinApi.RestoreDC(dc, -1)
	END DrawMetafile;
	
	PROCEDURE (rd: Rider) FixOrigin*, NEW;
		VAR p: Port; res: INTEGER;
	BEGIN
		p := rd.port;
		res := WinApi.SetBrushOrgEx(p.dc, (rd.dx - p.bx) MOD 8, (rd.dy - p.by) MOD 8, NIL)
	END FixOrigin;

	
	(** miscellaneous **)

	PROCEDURE ResetColors*;
	BEGIN
		Ports.background := WinApi.GetSysColor(5);
		textCol := WinApi.GetSysColor(8);
		selBackground := WinApi.GetSysColor(13);
		selTextCol := WinApi.GetSysColor(14);
		Ports.dialogBackground := WinApi.GetSysColor(15);
		dialogTextCol := WinApi.GetSysColor(18);
		dialogShadowCol := WinApi.GetSysColor(16);
		dialogLightCol := WinApi.GetSysColor(20);
	END ResetColors;
	
	PROCEDURE SetPrinterColors*;
	BEGIN
		Ports.background := Ports.white;
		textCol := Ports.black;
		selBackground := Ports.white;
		selTextCol := Ports.black;
		Ports.dialogBackground := Ports.white
	END SetPrinterColors;

	PROCEDURE ToggleDebug*;
	BEGIN
		debug := ~debug; noBuffer := debug
	END ToggleDebug;
	
	PROCEDURE ToggleBuffer*;
	BEGIN
		noBuffer := ~noBuffer
	END ToggleBuffer;
	
	PROCEDURE Init;
		VAR i: INTEGER; instance, bm: WinApi.HANDLE; pat: ARRAY 12 OF SHORTINT;
	BEGIN
		instance := WinApi.GetModuleHandleW(NIL);
		ResetColors;
		grgn := WinApi.CreateRectRgn(0, 0, 0, 0);
		nullBrush := WinApi.GetStockObject(WinApi.NULL_BRUSH);
		nullPen := WinApi.GetStockObject(WinApi.NULL_PEN);
		cursors[Ports.arrowCursor] :=
			WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_ARROW));
		cursors[Ports.textCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_IBEAM));
		cursors[Ports.graphicsCursor] :=
			WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_CROSS));
		cursors[Ports.bitmapCursor] :=
			WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_CROSS));
		cursors[Ports.tableCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 7));
		IF cursors[Ports.tableCursor] = 0 THEN cursors[Ports.tableCursor] := cursors[Ports.graphicsCursor] END;
		cursors[Ports.refCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 6));
		cursors[resizeHCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_SIZEWE));
		cursors[resizeVCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_SIZENS));
		cursors[resizeLCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_SIZENWSE));
		cursors[resizeRCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_SIZENESW));
		cursors[resizeCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_SIZE));
		cursors[busyCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_WAIT));
		cursors[stopCursor] := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_NO));
		cursors[moveCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 1));
		cursors[copyCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 2));
		cursors[linkCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 3));
		cursors[pickCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 4));
		IF cursors[stopCursor] = 0 THEN
			cursors[stopCursor] := WinApi.LoadCursorW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 5))
		END;
		i := 0;
		WHILE i < LEN(cursors) DO
			IF cursors[i] = 0 THEN cursors[i] := cursors[Ports.arrowCursor] END;
			INC(i)
		END;
		pat[0] := 0EEH; pat[1] := 0DDH; pat[2] := 0BBH; pat[3] := 077H;
		pat[4] := 0EEH; pat[5] := 0DDH; pat[6] := 0BBH; pat[7] := 077H;
		bm := WinApi.CreateBitmap(8, 8, 1, 1, SYSTEM.ADR(pat));
		focusBrush := WinApi.CreatePatternBrush(bm);
		pat[0] := 0EEH; pat[1] := 0BBH; pat[2] := 0DDH; pat[3] := 077H;
		pat[4] := 0EEH; pat[5] := 0BBH; pat[6] := 0DDH; pat[7] := 077H;
		bm := WinApi.CreateBitmap(8, 8, 1, 1, SYSTEM.ADR(pat));
		dim25Brush := WinApi.CreatePatternBrush(bm);
		pat[0] := 0AAH; pat[1] := 055H; pat[2] := 0AAH; pat[3] := 055H;
		pat[4] := 0AAH; pat[5] := 055H; pat[6] := 0AAH; pat[7] := 055H;
		bm := WinApi.CreateBitmap(8, 8, 1, 1, SYSTEM.ADR(pat));
		dim50Brush := WinApi.CreatePatternBrush(bm);
		pat[0] := 011H; pat[1] := 044H; pat[2] := 022H; pat[3] := 088H;
		pat[4] := 011H; pat[5] := 044H; pat[6] := 022H; pat[7] := 088H;
		bm := WinApi.CreateBitmap(8, 8, 1, 1, SYSTEM.ADR(pat));
		dim75Brush := WinApi.CreatePatternBrush(bm);
		invertBrush := WinApi.GetStockObject(WinApi.BLACK_BRUSH);
	END Init;

BEGIN
	Init
END HostPorts.
