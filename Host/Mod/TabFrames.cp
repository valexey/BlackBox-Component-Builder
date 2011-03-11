MODULE HostTabFrames;
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

	IMPORT
		SYSTEM, Kernel, WinApi, WinCtl, Fonts, Ports, Views,
		Controllers, Dialog, HostFonts, HostPorts, StdTabViews;

	CONST
		DEL = 07X;
		dlgWindowExtra = 30;

	TYPE
		Info = RECORD
			wnd, ctrl: WinApi.HANDLE;
			x1, y1, w1, h1, x2, y2, w2, h2: INTEGER;
			hasFocus: BOOLEAN
		END;
		
		Tab = POINTER TO RECORD (StdTabViews.Frame)
			i: Info;
			dispLeft, dispTop, dispWidth, dispHeight: INTEGER
		END;

		Directory = POINTER TO RECORD (StdTabViews.FrameDirectory) END;

	VAR
		instance: WinApi.HANDLE;
		SubclassCtl: PROCEDURE(wnd: WinApi.HANDLE);
	
	(* auxiliary procedures *)
	
	PROCEDURE GetSize (f: StdTabViews.Frame; VAR x, y, w, h: INTEGER);
		VAR u, x0, y0: INTEGER;
	BEGIN
		u := f.unit; f.view.context.GetSize(w, h); x0 := f.gx; y0 := f.gy;
		x := x0 DIV u; y := y0 DIV u;
		w := (x0 + w) DIV u - x; h := (y0 + h) DIV u - y
	END GetSize;
	
	PROCEDURE Adapt (f: StdTabViews.Frame; VAR i: Info);
		VAR res, x, y, w, h, cx, cy, cw, ch: INTEGER; r: HostPorts.Rider;
	BEGIN
		IF i.wnd # 0 THEN
			r := f.rider(HostPorts.Rider);
			IF r.port.wnd # 0 THEN	(* port is a window *)
				GetSize(f, x, y, w, h);
				cx := r.l; cy := r.t; cw := r.r - r.l; ch := r.b - r.t;
				IF (cx # i.x1) OR (cy # i.y1) OR (cw # i.w1) OR (ch # i.h1) THEN
					i.x1 := cx; i.y1 := cy; i.w1 := cw; i.h1 := ch;
					res := WinApi.MoveWindow(i.wnd, cx, cy, cw, ch, 0)
				END;
				DEC(x, cx); DEC(y, cy);
				IF (x # i.x2) OR (y # i.y2) OR (w # i.w2) OR (h # i.h2) THEN
					i.x2 := x; i.y2 := y; i.w2 := w; i.h2 := h;
					res := WinApi.MoveWindow(i.ctrl, x, y, w, h, 0)
				END
			END
		END
	END Adapt;

	PROCEDURE Open (f: StdTabViews.Frame; class, name: WinApi.PtrWSTR; style, ex: SET; VAR i: Info);
		VAR res, x, y, w, h, cx, cy, cw, ch: INTEGER; p: HostPorts.Port; s: SET; r: Ports.Rider;
	BEGIN
		f.noRedraw := TRUE;
		r := f.rider; GetSize(f, x, y, w, h);
		r.GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
		p := f.rider(HostPorts.Rider).port;
		IF p.wnd # 0 THEN	(* port is a window *)
			s := {27, 30};	(* disabled, child *)
			i.wnd := WinApi.CreateWindowExW({}, "Oberon Tab", "", s, cx, cy, cw, ch, p.wnd, 0, instance, 0);
			IF i.wnd # 0 THEN
				DEC(x, cx); DEC(y, cy);
				res := WinApi.SetWindowLongW(i.wnd, dlgWindowExtra, SYSTEM.VAL(INTEGER, f));
				i.ctrl := WinApi.CreateWindowExW(ex, class, name, style, x, y, w, h, i.wnd, 0, instance, 0);
				IF i.ctrl # 0 THEN
					IF (SubclassCtl # NIL) THEN SubclassCtl(i.ctrl) END;
					IF f.font # NIL THEN
						res := WinApi.SendMessageW(i.ctrl, WinApi.WM_SETFONT, f.font(HostFonts.Font).dev.id, 0)
					END;
					Adapt(f, i);
					res := WinApi.ShowWindow(i.wnd, 1);
					res := WinApi.ShowWindow(i.ctrl, 1)
				ELSE res := WinApi.DestroyWindow(i.wnd); i.wnd := 0
				END
			ELSE i.ctrl := 0
			END
		ELSE	(* port is a printer -> define temp windows *)
			i.wnd := 0; i.ctrl := 0
		END
	END Open;
	
	PROCEDURE (f: Tab) Print (l, r, t, b: INTEGER), NEW;
		VAR 
			w, h, sw, asc, dsc, fw, len, sel, i, dm, x, y, top: INTEGER; 
			font: Fonts.Font; tv: StdTabViews.View; s: Dialog.String; v: Views.View; 
	BEGIN
		IF f.font # NIL THEN font := f.font ELSE font := HostFonts.dlgFont END;
		font.GetBounds(asc, dsc, fw); y := asc + dsc; x := 0; dm := 0;(*y DIV 10;*) top := 0;
		f.view.context.GetSize(w, h); f.DrawRect(0, 0, w, h, 1, Ports.black);
		tv := f.view(StdTabViews.View); sel := tv.Index();
		len := tv.NofTabs();
		IF len > 0 THEN 
			i := 0; 
			tv.GetItem(i, s, v); Dialog.MapString(s, s);
			WHILE (i < len) & (s # "") & (v # NIL) DO
				tv.GetItem(i, s, v); Dialog.MapString(s, s);
				sw := SHORT(ENTIER(font.StringWidth(s) * 1.20)) + 2*dsc;
				IF (x + sw + 2*dm) > w THEN 
					f.DrawLine(0, y + 2*dm, w, y + 2*dm, 1, Ports.black);	
					x := 0; top := y + 2*dm; y := y + 2*dm + asc + dsc
				END;
				IF i = sel THEN 
					f.DrawRect(x, top, x + sw + 2*dm, y + 2*dm, Ports.fill, Ports.black);
					f.DrawString(x + dm + dsc, y - dsc + dm, Ports.white, s, font)
				ELSE
					f.DrawRect(x, top, x + sw + 2*dm, y + 2*dm, 1, Ports.black);
					f.DrawString(x + dm + dsc, y - dsc + dm, Ports.black, s, font)
				END;
				x := x + sw + 2*dm;
				INC(i)
			END;
			f.DrawLine(0, y + 2*dm, w, y + 2*dm, 1, Ports.black);			
			f.dispLeft := f.unit; f.dispTop := y + 2*dm + f.unit;
			f.dispWidth := w - 2*f.unit; f.dispHeight := h - (y + 2*dm + f.unit)
		END
		
		(*
		IF f.font # NIL THEN font := f.font ELSE font := HostFonts.dlgFont END;
		font.GetBounds(asc, dsc, fw); y := asc + dsc; x := 0; dm := y DIV 10; top := 0;
		f.view.context.GetSize(w, h); f.DrawRect(0, 0, w, h, 1, Ports.black);
		tv := f.view(StdTabViews.View); sel := tv.Index();
		len := tv.NofTabs();
		IF len > 0 THEN 
			tv.GetItem(sel, lbl, v); Dialog.MapString(lbl, lbl);
			s := " *" + lbl$ + "*"; sv := v;
			i := 0; 
			WHILE (i < len) & (lbl # "") & (v # NIL) DO
				IF i # sel THEN tv.GetItem(i, lbl, v); Dialog.MapString(lbl, lbl); s := s + " | " +  lbl END;
				INC(i);
			END;
			f.DrawString(dm, y + dm, Ports.black, s, font);
			f.DrawLine(0, y + 2*dm, w, y + 2*dm, 1, Ports.black);			
			f.dispLeft := f.unit; f.dispTop := y + 2*dm + f.unit;
			f.dispWidth := w - 2*f.unit; f.dispHeight := h - (y + 2*dm + f.unit);
		END;
		*)
	END Print;

	PROCEDURE SendKey (ch: CHAR; wnd: WinApi.HANDLE);
		VAR res, code: INTEGER;
	BEGIN
		CASE ch OF
		| 10X: code := 21H
		| 11X: code := 22H
		| 12X: code := 21H
		| 13X: code := 22H
		| 14X: code := 24H
		| 15X: code := 23H
		| 16X: code := 24H
		| 17X: code := 23H
		| 1CX: code := 25H
		| 1DX: code := 27H
		| 1EX: code := 26H
		| 1FX: code := 28H
		| DEL: code := 2EH
		ELSE code := 0
		END;
		IF code # 0 THEN 
			res := WinApi.SendMessageW(wnd, WinApi.WM_KEYDOWN, code, 0);
			res := WinApi.SendMessageW(wnd, WinApi.WM_KEYUP, code, 0)
		ELSE
			res := WinApi.SendMessageW(wnd, WinApi.WM_CHAR, ORD(ch), 0)
		END
	END SendKey;

	PROCEDURE HandleMouse (wnd: WinApi.HANDLE; x, y: INTEGER; buttons: SET);
		VAR res, b, hc, m: INTEGER; pt: WinApi.POINT; w: WinApi.HANDLE; msg: WinApi.MSG;
	BEGIN
		res := WinApi.ReleaseCapture(); b := 0;
		IF HostPorts.left IN buttons THEN INC(b) END;
		IF HostPorts.right IN buttons THEN INC(b, 2) END;
		IF Controllers.extend IN buttons THEN INC(b, 4) END;
		IF Controllers.modify IN buttons THEN INC(b, 8) END;
		pt.x := x; pt.y := y;
		REPEAT
			w := wnd; wnd := WinApi.ChildWindowFromPoint(wnd, pt);
			res := WinApi.ClientToScreen(w, pt);
			hc := WinApi.SendMessageW(wnd, WinApi.WM_NCHITTEST, 0, pt.x + pt.y * 65536);
			res := WinApi.ScreenToClient(wnd, pt)
		UNTIL (hc # 1) OR (w = wnd);
		IF hc > 1 THEN
			res := WinApi.ClientToScreen(wnd, pt);
			IF ODD(b) THEN m := WinApi.WM_NCLBUTTONDOWN
			ELSE m := WinApi.WM_NCRBUTTONDOWN
			END;
			res := WinApi.SendMessageW(wnd, m, hc, pt.x + pt.y * 65536)
		ELSE
			res := WinApi.GetClassLongW(wnd, -26);	(* classStyle *)
			IF (Controllers.doubleClick IN buttons) & ODD(res DIV 8) THEN	(* DblClks IN classStyle *)
				IF ODD(b) THEN m := WinApi.WM_LBUTTONDBLCLK ELSE m := WinApi.WM_RBUTTONDBLCLK END
			ELSE
				IF ODD(b) THEN m := WinApi.WM_LBUTTONDOWN ELSE m := WinApi.WM_RBUTTONDOWN END
			END;
			res := WinApi.SendMessageW(wnd, m, b, pt.x + pt.y * 65536)
		END;
		REPEAT
			res := WinApi.GetMessageW(msg, 0, 0, 0);
			IF (msg.message >= WinApi.WM_MOUSEMOVE) & (msg.message <= WinApi.WM_MBUTTONDBLCLK)
			THEN
				b := msg.wParam
			END;
			res := WinApi.TranslateMessage(msg);
			res := WinApi.DispatchMessageW(msg)
		UNTIL b MOD 4 = 0
	END HandleMouse;

	PROCEDURE Mark (on, focus: BOOLEAN; VAR i: Info);
		VAR res: INTEGER;
	BEGIN
		IF focus THEN
			IF on THEN
				IF ~i.hasFocus THEN
					res := WinApi.SendMessageW(i.ctrl, WinApi.WM_SETFOCUS, 0, 0);
					i.hasFocus := TRUE
				END
			ELSE
				IF i.hasFocus THEN
					res := WinApi.SendMessageW(i.ctrl, WinApi.WM_KILLFOCUS, 0, 0);
					i.hasFocus := FALSE
				END
			END
		END
	END Mark;

	(* Tab *)

	PROCEDURE (f: Tab) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: Tab) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;
	
	PROCEDURE (f: Tab) Update;
		VAR 
			res: INTEGER;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;
	
	PROCEDURE (f: Tab) UpdateList;
		VAR 
			res, i, sel, len: INTEGER; 
			ti: WinCtl.TCITEMW;
			s: Dialog.String;
			rect: WinApi.RECT;
			v: Views.View;
			tv: StdTabViews.View;
			hrgn, hrgn1: WinApi.HRGN;
	BEGIN
		tv := f.view(StdTabViews.View); sel := tv.Index();
		res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TCM_DELETEALLITEMS, 0, 0); 
		len := tv.NofTabs();
		(* Add the tabs to the tabcontrol *)
		IF len > 0 THEN
			i := 0; tv.GetItem(i, s, v); Dialog.MapString(s, s);
			WHILE (i < len) & (s # "") & (v # NIL) DO
				ti.mask := WinCtl.TCIF_TEXT + WinCtl.TCIF_IMAGE; 
				ti.pszText := s; ti.iImage := -1;
				res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TCM_INSERTITEMW, i, SYSTEM.ADR(ti));
				INC(i); IF i < len THEN tv.GetItem(i, s, v); Dialog.MapString(s, s) END
			END
		END;
		(* Get the display area *)
		rect.left := 0; rect.top := 0; rect.right := f.i.w2; rect.bottom := f.i.h2;
		res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TCM_ADJUSTRECT, WinApi.FALSE, SYSTEM.ADR(rect));
		(* set the area which the control should redraw *)
		hrgn := WinApi.CreateRectRgn(0, 0, f.i.w2, f.i.h2);
		hrgn1 := WinApi.CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
		res := WinApi.CombineRgn(hrgn, hrgn, hrgn1, WinApi.RGN_DIFF);
		res := WinApi.SetWindowRgn(f.i.ctrl, hrgn, WinApi.FALSE);
		IF res = 0 THEN res := WinApi.DeleteObject(hrgn) END;
		res := WinApi.DeleteObject(hrgn1);
		(* set the area which the contained views should draw *)
		f.dispLeft := rect.left * f.unit; f.dispTop := rect.top * f.unit;
		f.dispWidth := (rect.right - rect.left) * f.unit; f.dispHeight := (rect.bottom - rect.top) * f.unit;
		(* Set the size and the domain of the views *)
		IF len > 0 THEN
			i := 0; tv.GetItem(i, s, v);
			WHILE (i < len) & (s # "") & (v # NIL) DO
				v.context.SetSize(f.dispWidth, f.dispHeight);
				INC(i); IF i < len THEN  tv.GetItem(i, s, v) END
			END
		END;
		(* Reset selection *)
		res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TCM_SETCURSEL, sel, 0);
		f.Update
	END UpdateList;
	
	PROCEDURE (f: Tab) Paint (il, it, ir, ib: INTEGER), NEW;
		VAR res, l, t, r, b, w, h: INTEGER; ri: HostPorts.Rider; rect: WinApi.RECT;
	BEGIN
		res := WinApi.GetWindowRect(f.i.wnd, rect);
		w := rect.right  - rect.left; h := rect.bottom - rect.top;
		ri := f.rider(HostPorts.Rider); 
		ri.port.CloseBuffer;
		IF it < f.dispTop THEN (* Paint the top part *)
			l := (il DIV f.unit) - 1; IF l < 0 THEN l := 0 END; r := (ir DIV f.unit) + 1; IF r > w THEN r := w END;
			rect.left := l; rect.top := 0; rect.right := r; rect.bottom := f.dispTop DIV f.unit;
			res := WinApi.InvalidateRect(f.i.wnd, rect, 0); res := WinApi.InvalidateRect(f.i.ctrl, rect, 0);
			res := WinApi.UpdateWindow(f.i.wnd); res := WinApi.UpdateWindow(f.i.ctrl)
		END;
		IF il < f.dispLeft THEN (* Paint the left part *)
			t := (it DIV f.unit) - 1; IF t < 0 THEN t := 0 END; b := (ib DIV f.unit) + 1; IF b > h THEN b := h END;
			rect.left := 0; rect.top := t; rect.right := f.dispLeft DIV f.unit; rect.bottom := b;
			res := WinApi.InvalidateRect(f.i.wnd, rect, 0); res := WinApi.InvalidateRect(f.i.ctrl, rect, 0);
			res := WinApi.UpdateWindow(f.i.wnd); res := WinApi.UpdateWindow(f.i.ctrl)
		END;
		IF ib > f.dispTop + f.dispHeight THEN (* Paint the bottom part *)
			l := (il DIV f.unit) - 1; IF l < 0 THEN l := 0 END; r := (ir DIV f.unit) + 1; IF r > w THEN r := w END;
			rect.left := l; rect.top := (f.dispTop + f.dispHeight) DIV f.unit; rect.right := r; rect.bottom := h;
			res := WinApi.InvalidateRect(f.i.wnd, rect, 0); res := WinApi.InvalidateRect(f.i.ctrl, rect, 0);
			res := WinApi.UpdateWindow(f.i.wnd); res := WinApi.UpdateWindow(f.i.ctrl)
		END;
		IF ir > f.dispLeft + f.dispWidth THEN (* Paint the right part *)
			t := (it DIV f.unit) - 1; IF t < 0 THEN t := 0 END; b := (ib DIV f.unit) + 1; IF b > h THEN b := h END;
			rect.left := (f.dispLeft + f.dispWidth) DIV f.unit; rect.top := t; rect.right := w; rect.bottom := b;
			res := WinApi.InvalidateRect(f.i.wnd, rect, 0); res := WinApi.InvalidateRect(f.i.ctrl, rect, 0);
			res := WinApi.UpdateWindow(f.i.wnd); res := WinApi.UpdateWindow(f.i.ctrl)
		END
	END Paint;
	
	PROCEDURE (f: Tab) Restore (l, t, r, b: INTEGER);
		VAR 
			icex: WinCtl.INITCOMMONCONTROLSEX;
			ok: WinApi.BOOL;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			icex.dwSize := SIZE(WinCtl.INITCOMMONCONTROLSEX);
			icex.dwICC := WinCtl.ICC_TAB_CLASSES;
			ok :=  WinCtl.InitCommonControlsEx(icex);
			Open(f, WinCtl.WC_TABCONTROLW, "", 
				WinApi.WS_CLIPSIBLINGS + WinApi.WS_CHILD + WinApi.WS_VISIBLE + WinCtl.TCS_MULTILINE,
				{},  f.i);
			f.UpdateList
		END;
		f.Update;
		IF f.i.wnd # 0 THEN f.Paint(l, t, r, b) ELSE f.Print(l, t, r, b) END
	END Restore;

	PROCEDURE (f: Tab) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " " THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(ch), 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(ch), 0)
		ELSE SendKey(ch, f.i.ctrl)
		END
	END KeyDown;
	
	PROCEDURE (f: Tab) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;
	
	PROCEDURE (f: Tab) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;
	
	PROCEDURE (f: Tab) InDispArea (x, y: INTEGER): BOOLEAN;
	BEGIN
		IF ((x >= f.dispLeft) & (x <= f.dispLeft + f.dispWidth) &
			(y >= f.dispTop) & (y <= f.dispTop + f.dispHeight)) THEN
			RETURN TRUE
		ELSE
			RETURN FALSE
		END
	END InDispArea;
	
	PROCEDURE (f: Tab) GetDispSize (OUT x, y, w, h: INTEGER);
		VAR res: INTEGER; rect: WinApi.RECT;
	BEGIN
		IF ~Views.IsPrinterFrame(f) THEN
			rect.left := 0; rect.top := 0; rect.right := f.i.w2; rect.bottom := f.i.h2;
			res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TCM_ADJUSTRECT, WinApi.FALSE, SYSTEM.ADR(rect));
			f.dispLeft := rect.left * f.unit; f.dispTop := rect.top * f.unit;
			f.dispWidth := (rect.right - rect.left) * f.unit; f.dispHeight := (rect.bottom - rect.top) * f.unit
		END;
		x := f.dispLeft; y := f.dispTop; w := f.dispWidth; h := f.dispHeight		
	END GetDispSize;
	
	
	(* Directory *)

	PROCEDURE (d: Directory) GetTabSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 150 * Ports.point END;
		IF h = Views.undefined THEN h := 100 * Ports.point END
	END GetTabSize;
	
	PROCEDURE (d: Directory) New (): StdTabViews.Frame;
		VAR f: Tab;
	BEGIN
		NEW(f); RETURN f
	END New;
	
	(* control window class *)
	
	(* Used for common controls, not standard controls *)
	PROCEDURE HandleNotifyMsg (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR  c: StdTabViews.Frame; pnmhdr: WinApi.PtrNMHDR;
	BEGIN
		c := SYSTEM.VAL(StdTabViews.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
		pnmhdr := SYSTEM.VAL(WinApi.PtrNMHDR, lParam);
		WITH c: Tab DO
			IF c.i.ctrl #0 THEN
				IF pnmhdr.code = WinCtl.TCN_SELCHANGE THEN
					c.SetIndex(WinApi.SendMessageW(c.i.ctrl, WinCtl.TCM_GETCURSEL, 0, 0))
				END
			END
		END
	END HandleNotifyMsg;
	
	PROCEDURE [2] CtrlHandler (wnd: WinApi.HANDLE; message, wParam, lParam: INTEGER): INTEGER;
	BEGIN
		IF message = WinApi.WM_NOTIFY THEN
			Kernel.Try(HandleNotifyMsg, wnd, wParam, lParam);
			RETURN 0
		END;
		RETURN WinApi.DefWindowProcW(wnd, message, wParam, lParam)
	END CtrlHandler;

	PROCEDURE Install;
		VAR dir: Directory;
	BEGIN
		NEW(dir); StdTabViews.SetFrameDir(dir)
	END Install;

	PROCEDURE InitClass;
		VAR class: WinApi.WNDCLASSW; res: INTEGER;
	BEGIN
		class.hCursor := WinApi.NULL;
		class.hIcon := WinApi.NULL;
		class.lpszMenuName := NIL;
		class.lpszClassName := "Oberon Tab";
		class.hbrBackground := 0; (* no background *)
		class.style := {};
		class.hInstance := instance;
		class.lpfnWndProc := CtrlHandler;
		class.cbClsExtra := 0;
		class.cbWndExtra := dlgWindowExtra + 4;
		res := WinApi.RegisterClassW(class)
	END InitClass;
	
	PROCEDURE Init*;
	BEGIN
		StdTabViews.setFocus := TRUE;
		instance := WinApi.GetModuleHandleW(NIL);
		InitClass;
		Install
	END Init;
	
END HostTabFrames.
