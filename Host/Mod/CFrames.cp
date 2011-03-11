MODULE HostCFrames;
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

	(* ColorField, DateField, TimeField, UpDownField *)

	IMPORT
		SYSTEM, WinApi, WinCtl,
		Kernel, Strings, Dates, Fonts, Ports, Views,
		Controllers, Dialog, HostFonts, HostRegistry, HostPorts, HostWindows, StdCFrames;

	CONST
		ENTER = 0DX; ESC = 1BX; DEL = 07X; BS = 08X; TAB = 09X; LTAB = 0AX;
		AL = 1CX; AR = 1DX; AU = 1EX; AD = 1FX;
		PL = 10X; PR = 11X; PU = 12X; PD = 13X;
		DL = 14X; DR = 15X; DU = 16X; DD = 17X;
		dlgWindowExtra = 30;
		dropDownHeight = 30 * Ports.point;
		upDownWidth = 11 * Ports.point;
		numColors = 7;
		scrollRange = 16384;

	TYPE
		Procedure = PROCEDURE;

		Info = RECORD
			wnd, ctrl, ud: WinApi.HANDLE;
			x1, y1, w1, h1, x2, y2, w2, h2: INTEGER;
			hasFocus, dropDown, readOnly, undef: BOOLEAN;
			bkgnd: Ports.Color
		END;

		PushButton = POINTER TO RECORD (StdCFrames.PushButton)
			i: Info
		END;

		CheckBox = POINTER TO RECORD (StdCFrames.CheckBox)
			i: Info
		END;

		RadioButton = POINTER TO RECORD (StdCFrames.RadioButton)
			i: Info
		END;

		ScrollBar = POINTER TO RECORD (StdCFrames.ScrollBar)
			i: Info
		END;

		Field = POINTER TO RECORD (StdCFrames.Field)
			i: Info;
			isUpdate: BOOLEAN;
			del: BOOLEAN (* needed know when an empty field is allowed. not perfect. *)
		END;

		UpDownField = POINTER TO RECORD (StdCFrames.UpDownField)
			i: Info;
			val: INTEGER;
			isUpdate: BOOLEAN
		END;

		DateField = POINTER TO RECORD (StdCFrames.DateField)
			i: Info;
			isUpdate: BOOLEAN;
			cnt, val: INTEGER	(* input state: val = current val, cnt = number of key strokes *)
		END;

		TimeField = POINTER TO RECORD (StdCFrames.TimeField)
			i: Info;
			isUpdate: BOOLEAN;
			cur: INTEGER
		END;

		ColorField = POINTER TO RECORD (StdCFrames.ColorField)
			i: Info;
			color: Ports.Color
		END;

		ListBox = POINTER TO RECORD (StdCFrames.ListBox)
			i: Info
		END;

		SelectionBox = POINTER TO RECORD (StdCFrames.SelectionBox)
			i: Info;
			num: INTEGER
		END;

		ComboBox = POINTER TO RECORD (StdCFrames.ComboBox)
			i: Info;
			edit: WinApi.HANDLE
		END;

		Caption = POINTER TO RECORD (StdCFrames.Caption)
			i: Info
		END;

		Group = POINTER TO RECORD (StdCFrames.Group)
			i: Info
		END;

		TFInfo = RECORD
			tn: Dialog.TreeNode;
			wndAdr: INTEGER
		END;

		TreeFrame = POINTER TO RECORD (StdCFrames.TreeFrame)
			i: Info;
			inUpdateList: BOOLEAN;
			treeArray: POINTER TO ARRAY OF TFInfo;
			curindex, selIndex: INTEGER;
			folderimg, openimg, leafimg: INTEGER;
			himl: WinCtl.Ptr_IMAGELIST
		END;

		Directory = POINTER TO RECORD (StdCFrames.Directory) END;

		TrapCleaner = POINTER TO RECORD (Kernel.TrapCleaner) END;

		BrushCache = POINTER TO RECORD
			next: BrushCache;
			col: Ports.Color;
			brush: WinApi.HANDLE
		END;

	VAR
		instance: WinApi.HANDLE;
		SubclassCtl: PROCEDURE(wnd: WinApi.HANDLE);
		CtlColor: PROCEDURE(msg, wPar, lPar: INTEGER): INTEGER;
		scW, scH: INTEGER;	(* screen width and height *)
		updateAllowed: BOOLEAN;
		colors: ARRAY numColors OF Ports.Color;
		(* date format *)
		dateSep: CHAR;	(* separator character *)
		yearPart, monthPart, dayPart: INTEGER;	(* first = 1, last = 3 *)
		del1, del2: INTEGER;	(* position of separators *)
		(* time format *)
		timeSep: CHAR;	(* separator character *)
		lastPart: INTEGER;	(* 3 for 24h format, 4 for 12h format *)
		desig: ARRAY 2, 8 OF CHAR;	(* AM, PM strings *)
		inHandleMouse*: BOOLEAN; (* to disable the main window handler in HostMenus *)
		brushes: BrushCache;
		

	(* auxiliary procedures *)

	PROCEDURE ConvertFromUnicode (VAR s: ARRAY OF CHAR);
		VAR i: INTEGER;
	BEGIN
		i := 0;
		WHILE s[i] # 0X DO
			INC(i)
		END
	END ConvertFromUnicode;

	PROCEDURE ConvertToUnicode (VAR s: ARRAY OF CHAR);
		VAR i: INTEGER;
	BEGIN
		i := 0;
		WHILE s[i] # 0X DO
			INC(i)
		END
	END ConvertToUnicode;

	PROCEDURE GetSize (f: StdCFrames.Frame; VAR x, y, w, h: INTEGER);
		VAR u, x0, y0: INTEGER;
	BEGIN
		u := f.unit; f.view.context.GetSize(w, h); x0 := f.gx; y0 := f.gy;
		x := x0 DIV u; y := y0 DIV u;
		w := (x0 + w) DIV u - x; h := (y0 + h) DIV u - y;
		IF (SubclassCtl # NIL)
			& ((f IS Field) OR (f IS UpDownField) OR (f IS TimeField) OR (f IS DateField)
				OR (f IS ColorField) OR (f IS ListBox) OR (f IS SelectionBox) OR (f IS ComboBox)) THEN
			INC(x, 1); INC(y, 1); DEC(w, 2); DEC(h, 2)	(* space for 3d border *)
		END
	END GetSize;

	PROCEDURE Adapt (f: StdCFrames.Frame; VAR i: Info);
		VAR res, x, y, w, h, cx, cy, cw, ch, udw, n, h0, h1: INTEGER; r: HostPorts.Rider;
	BEGIN
		IF i.wnd # 0 THEN
			r := f.rider(HostPorts.Rider);
			IF r.port.wnd # 0 THEN	(* port is a window *)
				GetSize(f, x, y, w, h);
				IF i.dropDown THEN
					n := WinApi.SendMessageW(i.ctrl, WinApi.CB_GETCOUNT, 0, 0);
					h0 := WinApi.SendMessageW(i.ctrl, WinApi.CB_GETITEMHEIGHT, -1, 0);
					h1 := WinApi.SendMessageW(i.ctrl, WinApi.CB_GETITEMHEIGHT, 0, 0);
					IF n < 1 THEN n := 1 ELSIF n > 8 THEN n := 8 END;
					h := h0 + h1 * n + 8
				END;
				cx := r.l; cy := r.t; cw := r.r - r.l; ch := r.b - r.t;
				IF (cx # i.x1) OR (cy # i.y1) OR (cw # i.w1) OR (ch # i.h1) THEN
					i.x1 := cx; i.y1 := cy; i.w1 := cw; i.h1 := ch;
					res := WinApi.MoveWindow(i.wnd, cx, cy, cw, ch, 0)
				END;
				DEC(x, cx); DEC(y, cy);
				IF (x # i.x2) OR (y # i.y2) OR (w # i.w2) OR (h # i.h2) THEN
					i.x2 := x; i.y2 := y; i.w2 := w; i.h2 := h;
					IF i.ud # 0 THEN
						udw := upDownWidth DIV f.unit;
						res := WinApi.MoveWindow(i.ud, x + w - udw, y + 1, udw, h - 2, 0);
						DEC(w, udw);
						IF SubclassCtl # NIL THEN DEC(w) END
					END;
					res := WinApi.MoveWindow(i.ctrl, x, y, w, h, 0)
				END
			END
		END
	END Adapt;

	PROCEDURE BkGndColor (f: Views.Frame): Ports.Color;
		VAR bgnd: Ports.Color; g: Views.Frame;
	BEGIN
		g := f;
		REPEAT
			g := Views.HostOf(g);
			bgnd := Views.transparent;
			g.view.GetBackground(bgnd)
		UNTIL bgnd # Views.transparent;
		RETURN bgnd
	END BkGndColor;

	PROCEDURE Open (f: StdCFrames.Frame; class, name: WinApi.PtrWSTR;
																style, ex: SET; VAR i: Info);
		VAR res, x, y, w, h, cx, cy, cw, ch, udw: INTEGER; p: HostPorts.Port; s: SET; r: Ports.Rider;
	BEGIN
		f.noRedraw := TRUE;
		r := f.rider; GetSize(f, x, y, w, h);
		r.GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
		p := f.rider(HostPorts.Rider).port;
		IF p.wnd # 0 THEN	(* port is a window *)
			i.bkgnd := BkGndColor(f);
			s := {27, 30};	(* disabled, child *)
			i.wnd := WinApi.CreateWindowExW({}, "Oberon Ctrl", "", s, cx, cy, cw, ch, p.wnd, 0, instance, 0);
			IF i.wnd # 0 THEN
				DEC(x, cx); DEC(y, cy);
				res := WinApi.SetWindowLongW(i.wnd, dlgWindowExtra, SYSTEM.VAL(INTEGER, f));
				i.ctrl := WinApi.CreateWindowExW(ex, class, name, style, x, y, w, h, i.wnd, 0, instance, 0);
				IF i.ctrl # 0 THEN
					IF (SubclassCtl # NIL) & ~(f IS PushButton) THEN SubclassCtl(i.ctrl) END;
					IF f.font # NIL THEN
						res := WinApi.SendMessageW(i.ctrl, WinApi.WM_SETFONT, f.font(HostFonts.Font).dev.id, 0)
					END;
					IF (f IS UpDownField) OR (f IS TimeField) OR (f IS DateField) THEN
						style := {23, 28, 30} + WinCtl.UDS_ALIGNRIGHT + WinCtl.UDS_ARROWKEYS;
						udw := upDownWidth DIV f.unit;
						i.ud := WinCtl.CreateUpDownControl(style, x + w - udw, y, udw, h, i.wnd, 0, instance, 0, 1, 0, 0)
					END;
					Adapt(f, i);
					res := WinApi.ShowWindow(i.wnd, 1);
					res := WinApi.ShowWindow(i.ctrl, 1);
					IF i.ud # 0 THEN res := WinApi.ShowWindow(i.ud, 1) END
				ELSE res := WinApi.DestroyWindow(i.wnd); i.wnd := 0
				END
			ELSE i.ctrl := 0
			END
		ELSE	(* port is a printer -> define temp windows *)
			i.wnd := 0; i.ctrl := 0
(*
			s := {27, 31};	(* disabled, popup *)
			i.wnd := WinApi.CreateWindowExW({}, "Oberon Ctrl", "", s, scW - cw, scH - ch, cw, ch, 0, 0, instance, 0);
			res := WinApi.SetWindowLongW(i.wnd, dlgWindowExtra, SYSTEM.VAL(LONGINT, f));
			i.ctrl := WinApi.CreateWindowExW({}, class, name, style, x - cx, y - cy, w, h, i.wnd, 0, instance, 0)
*)
		END
	END Open;

	PROCEDURE Paint (f: StdCFrames.Frame; VAR i: Info);
		VAR res, u: INTEGER; r: HostPorts.Rider;
	BEGIN
		r := f.rider(HostPorts.Rider); u := f.unit;
		r.port.CloseBuffer;
		updateAllowed := TRUE;
		res := WinApi.InvalidateRect(i.wnd, NIL, 0);
		res := WinApi.InvalidateRect(i.ctrl, NIL, 0);
		res := WinApi.UpdateWindow(i.wnd);
		res := WinApi.UpdateWindow(i.ctrl);
		IF i.ud # 0 THEN res := WinApi.UpdateWindow(i.ud) END;
		updateAllowed := FALSE
	END Paint;

	PROCEDURE PaintRect (f: StdCFrames.Frame; VAR i: Info; l, t, r, b: INTEGER);
		VAR res, u: INTEGER; ri: HostPorts.Rider; rect: WinApi.RECT;
	BEGIN
		ri := f.rider(HostPorts.Rider); u := f.unit;
		ri.port.CloseBuffer;
		updateAllowed := TRUE;
		rect.left := l DIV f.unit; rect.top := t DIV f.unit; rect.right := r DIV f.unit; rect.bottom := b DIV f.unit;
		rect.left := rect.left; rect.top := rect.top; rect.right := rect.right; rect.bottom := rect.bottom ;
		(* round up if possible *)
		IF rect.left > i.x1 THEN rect.left := rect.left - 1 END;
		IF rect.top > i.y1 THEN rect.top := rect.top - 1 END;
		IF rect.right < i.x1 + i.w1 THEN rect.right := rect.right + 1 END;
		IF rect.bottom < i.y1 + i.h1 THEN rect.bottom := rect.bottom + 1 END;
		res := WinApi.InvalidateRect(i.wnd, rect, 0);
		res := WinApi.InvalidateRect(i.ctrl, rect, 0);
		res := WinApi.UpdateWindow(i.wnd);
		res := WinApi.UpdateWindow(i.ctrl);
		IF i.ud # 0 THEN res := WinApi.UpdateWindow(i.ud) END;
		updateAllowed := FALSE
	END PaintRect;

	PROCEDURE ExtractLine (IN s: ARRAY OF CHAR; beg: INTEGER; fnt: Fonts.Font; maxW: INTEGER;
		OUT line: Dialog.String; OUT lineW, end: INTEGER
	);
		VAR i, len, w: INTEGER; ch: CHAR;
	BEGIN
		len := LEN(s$);
		ASSERT(beg >= 0, 20); ASSERT(beg < LEN(s$), 21); ASSERT(maxW > 0, 22); ASSERT(fnt # NIL, 23);
		i := 0; end := beg; lineW := 0; w := 0;
		REPEAT
			ch := s[i + beg];
			IF ch <= " " THEN end := i + beg; lineW := w END;
			line[i] := ch; INC(i); line[i] := 0X; w := fnt.StringWidth(line);
			IF (w <= maxW) & ((ch = "-") OR (ch = "_")) THEN end := i + beg; lineW := w END
		UNTIL (i + beg = len) OR (w > maxW);
		IF w <= maxW THEN end := beg + i; line[end - beg] := 0X; lineW := w
		ELSIF end # beg THEN line[end - beg] := 0X
		ELSE end := beg + i - 1; line[i - 1] := 0X; lineW := fnt.StringWidth(line)
		END;
		ASSERT(end > beg, 80)
	END ExtractLine;

	PROCEDURE PrintString (f: Views.Frame; w: INTEGER;
		IN string: ARRAY OF CHAR; fnt: Fonts.Font; col: Ports.Color;
		x0, y0: INTEGER; left, right, multiLine: BOOLEAN
	);
		VAR x, sw, asc, dsc, fw, beg, end, len, n: INTEGER; s: Dialog.String;
	BEGIN
		fnt.GetBounds(asc, dsc, fw); sw := fnt.StringWidth(string);
		w := MAX(fw, w - 2 * x0); (* ensures that always at least one character fits *)
		beg := 0; len := LEN(string$); n := 0;
		WHILE beg < len DO
			IF multiLine THEN ExtractLine(string, beg, fnt, w, s, sw, end)
			ELSE s := string$; sw := fnt.StringWidth(s); end := len
			END;
			IF ~left & ~right THEN x := (w - sw) DIV 2 ELSIF right & ~left THEN x := w - sw ELSE x := 0 END;
			f.DrawString(x0 + x, n * (asc + dsc) + y0, col, s, fnt);
			beg := end; INC(n);
			IF (beg < len) & (string[beg] = " ") THEN INC(beg) END
		END
	END PrintString;

	PROCEDURE Print (f: StdCFrames.Frame; d, x, y: INTEGER; label: ARRAY OF CHAR);
		VAR w, h, sw, a, asc, dsc, fw: INTEGER; font: Fonts.Font; on: BOOLEAN;
	BEGIN
		IF f.font # NIL THEN font := f.font ELSE font := HostFonts.dlgFont END;
		f.view.context.GetSize(w, h);
		font.GetBounds(asc, dsc, fw);
		IF x < 0 THEN sw := font.StringWidth(label); x := (w - sw) DIV 2 END;
		IF y < 0 THEN y := (h + asc - dsc) DIV 2
		ELSIF y = 0 THEN y := asc
		END;
		a := (h - 8 * Ports.point) DIV 2;
		IF d = -1 THEN
			f.DrawRect(2 * Ports.point, a, 10 * Ports.point, a + 8 * Ports.point, f.dot, Ports.defaultColor);
			IF (f IS CheckBox) & (f(CheckBox).Get # NIL) THEN
				f(CheckBox).Get(f(CheckBox), on);
				IF on THEN
					f.DrawLine(
						2 * Ports.point + 2 * f.dot, a + 4 * Ports.point, 5 * Ports.point, a + 8 * Ports.point - 2 * f.dot,
						2 * Ports.point, Ports.defaultColor
						);
					f.DrawLine(
						5 * Ports.point, a + 8 * Ports.point - 2 * f.dot, 10 * Ports.point - 2 * f.dot, a + 2 * f.dot,
						2 * Ports.point, Ports.defaultColor
						)
				END
			END
		ELSIF d = -2 THEN f.DrawOval(2 * Ports.point, a, 10 * Ports.point, a + 8 * Ports.point, f.dot, Ports.defaultColor);
			IF f(RadioButton).Get # NIL THEN f(RadioButton).Get(f(RadioButton), on);
				IF on THEN f.DrawOval(2*Ports.point + 2*f.dot, a+2*f.dot,
											10*Ports.point - 2*f.dot, a + 8*Ports.point-2*f.dot, Ports.fill, Ports.black)
				END
			END
		ELSIF d = -3 THEN
			f.DrawRect(0, MIN(asc - f.dot, h), w, MAX(asc - f.dot, h), f.dot, Ports.defaultColor);
			f.DrawRect(x, y - asc, x + font.StringWidth(label), y + dsc, Ports.fill, Ports.background)
		ELSIF d > 0 THEN f.DrawRect(0, 0, w, h, d, Ports.defaultColor)
		END;
(*
		WITH f: Field DO
			IF f.right & ~f.left THEN x := w - x - font.SStringWidth(label)
			ELSIF ~f.right & ~f.left THEN x := (w DIV 2) - (font.SStringWidth(label) DIV 2)
			END
		| f: Caption DO
			IF f.right & ~f.left THEN x := w - x - font.SStringWidth(label)
			ELSIF ~f.right & ~f.left THEN x := (w DIV 2) - (font.SStringWidth(label) DIV 2)
			END
		ELSE
		END;
		f.DrawSString(x, y, Ports.defaultColor, label, font)
*)
		WITH f: Field DO
			PrintString(f, w, label, font, Ports.defaultColor, x, y, f.left, f.right, f.multiLine)
		| f: Caption DO
			PrintString(f, w, label, font, Ports.defaultColor, x, y, f.left, f.right, TRUE)
		ELSE
			PrintString(f, w, label, font, Ports.defaultColor, x, y, TRUE, FALSE, FALSE)
		END
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

	PROCEDURE (t: TrapCleaner) Cleanup;
	BEGIN
		inHandleMouse := FALSE
	END Cleanup;

	PROCEDURE HandleMouse (wnd: WinApi.HANDLE; x, y: INTEGER; buttons: SET);
		VAR res, b, hc, m: INTEGER; pt: WinApi.POINT; w: WinApi.HANDLE; msg: WinApi.MSG;
			tc: TrapCleaner;
	BEGIN
		NEW(tc); Kernel.PushTrapCleaner(tc); inHandleMouse := TRUE;
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
			IF ODD(b) THEN m := WinApi.WM_NCLBUTTONDOWN ELSE m := WinApi.WM_NCRBUTTONDOWN END;
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
			IF (msg.message >= WinApi.WM_MOUSEMOVE) & (msg.message <= WinApi.WM_MBUTTONDBLCLK) THEN
				b := msg.wParam
			END;
			res := WinApi.TranslateMessage(msg);
			res := WinApi.DispatchMessageW(msg)
		UNTIL b MOD 4 = 0;
		inHandleMouse := FALSE; Kernel.PopTrapCleaner(tc)
	END HandleMouse;

	PROCEDURE HandleWheel (wnd: WinApi.HANDLE; x, y: INTEGER; op, nofLines: INTEGER;
		VAR done: BOOLEAN
	);
		VAR res: INTEGER;
	BEGIN
		IF op = Controllers.incPage THEN
			res := WinApi.SendMessageW(wnd, WinApi.WM_VSCROLL, WinApi.SB_PAGEDOWN, WinApi.NULL)
		ELSIF op = Controllers.decPage THEN
			res := WinApi.SendMessageW(wnd, WinApi.WM_VSCROLL, WinApi.SB_PAGEUP, WinApi.NULL)
		ELSE
			IF op = Controllers.incLine THEN
				op := WinApi.SB_LINEDOWN
			ELSE
				op := WinApi.SB_LINEUP
			END;
			WHILE nofLines > 0 DO
				res := WinApi.SendMessageW(wnd, WinApi.WM_VSCROLL, op, WinApi.NULL);
				DEC(nofLines)
			END
		END;
		done := TRUE
	END HandleWheel;

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

	PROCEDURE SetLabel (IN in: ARRAY OF CHAR; OUT out: ARRAY OF CHAR);
	BEGIN
		Dialog.MapString(in, out); ConvertFromUnicode(out);
	END SetLabel;

	PROCEDURE CheckLabel (IN label: ARRAY OF CHAR; ctrl: INTEGER);
		VAR res: INTEGER; lbl, s: Dialog.String;
	BEGIN
		Dialog.MapString(label, lbl);
		ConvertFromUnicode(lbl);
		res := WinApi.GetWindowTextW(ctrl, s, LEN(s));
		IF s # lbl THEN res := WinApi.SetWindowTextW(ctrl, lbl) END
	END CheckLabel;

	(* PushButton *)

	PROCEDURE (f: PushButton) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: PushButton) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: PushButton) Update;
		VAR res: INTEGER; style: SET;
	BEGIN
		IF ~f.disabled & ~f.readOnly THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN
				IF f.default THEN
					style := BITS(WinApi.GetWindowLongW(f.i.ctrl, -16));	(* window style *)
					res := WinApi.SetWindowLongW(f.i.ctrl, -16, ORD(style + {0}))
				END;
				res := WinApi.EnableWindow(f.i.ctrl, 1)
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				IF f.default THEN
					style := BITS(WinApi.GetWindowLongW(f.i.ctrl, -16));	(* window style *)
					res := WinApi.SetWindowLongW(f.i.ctrl, -16, ORD(style - {0}))
				END;
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		CheckLabel(f.label, f.i.ctrl);
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: PushButton) Restore (l, t, r, b: INTEGER);
		VAR style: SET; lbl: Dialog.String;
	BEGIN
		SetLabel(f.label, lbl);
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			style := {16, 30};	(* pushbutton, tabstop, child *)
			IF f.default & ~f.disabled THEN INCL(style, 0) END;	(* default *)
			Open(f, "BUTTON", lbl, style, {}, f.i)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, f.dot, -1, -1, lbl) END
	END Restore;

	PROCEDURE (f: PushButton) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE Execute (f: PushButton);
		VAR old: WinApi.HANDLE;
	BEGIN
		IF f.Do # NIL THEN
			old := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor]);
			Dialog.ShowStatus("");
			f.Do(f);
			old := WinApi.SetCursor(old)
		END
	END Execute;

	PROCEDURE (f: PushButton) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		Execute(f)
	END KeyDown;

	PROCEDURE (f: PushButton) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* CheckBox *)

	PROCEDURE (f: CheckBox) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: CheckBox) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: CheckBox) Update;
		VAR res: INTEGER; value: BOOLEAN;
	BEGIN
		IF ~f.disabled THEN
			f.Get(f, value);
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			IF f.undef THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 2, 0)
			ELSIF value THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 1, 0)
			ELSE res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 0, 0)
			END;
			IF f.readOnly THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETSTATE, WinApi.TRUE, 0)
			ELSE res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETSTATE, WinApi.FALSE, 0)
			END
		ELSE
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 0, 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETSTATE, WinApi.FALSE, 0);
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		CheckLabel(f.label, f.i.ctrl);
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: CheckBox) Restore (l, t, r, b: INTEGER);
		VAR style: SET; lbl: Dialog.String;
	BEGIN
		SetLabel(f.label, lbl);
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
(*
			f.Get(f, value);	(* update f.undef *)
			IF f.undef THEN style := {1, 2, 16, 30}	(* auto 3state, tabstop, child *)
			ELSE style := {0, 1, 16, 30}	(* auto checkbox, tabstop, child *)
			END;
*)
			style := {0, 2, 16, 30};	(* 3state, tabstop, child *)
			Open(f, "BUTTON", lbl, style, {}, f.i)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, -1, 12 * Ports.point, -1, lbl) END
	END Restore;

	PROCEDURE (f: CheckBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: CheckBox) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch >= " " THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(" "), 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(" "), 0)
		END
	END KeyDown;

	PROCEDURE (f: CheckBox) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* RadioButton *)

	PROCEDURE (f: RadioButton) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: RadioButton) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: RadioButton) Update;
		VAR res: INTEGER; value: BOOLEAN;
	BEGIN
		IF ~f.disabled THEN
			f.Get(f, value);
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			IF f.undef THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 0, 0)
			ELSIF value THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 1, 0)
			ELSE res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 0, 0)
			END;
			IF f.readOnly THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETSTATE, WinApi.TRUE, 0)
			ELSE res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETSTATE, WinApi.FALSE, 0)
			END
		ELSE
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETCHECK, 0, 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.BM_SETSTATE, WinApi.FALSE, 0);
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		END;
		CheckLabel(f.label, f.i.ctrl);
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: RadioButton) Restore (l, t, r, b: INTEGER);
		VAR lbl: Dialog.String;
	BEGIN
		SetLabel(f.label, lbl);
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			Open(f, "BUTTON", lbl, {0, 3, 16, 30}, {}, f.i)	(* auto radiobutton, tabstop, child *)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, -2, 12 * Ports.point, -1, lbl) END
	END Restore;

	PROCEDURE (f: RadioButton) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: RadioButton) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(" "), 0);
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(" "), 0)
	END KeyDown;

	PROCEDURE (f: RadioButton) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* ScrollBar *)

	PROCEDURE (f: ScrollBar) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: ScrollBar) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: ScrollBar) Update;
		VAR res, size, sect, pos, p, q, m: INTEGER; i: WinApi.SCROLLINFO;
	BEGIN
		IF ~f.disabled THEN
			f.Get(f, size, sect, pos);
			IF size > sect THEN
				IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;

				p := WinApi.MulDiv(pos, scrollRange, size - sect);
				i.cbSize := SIZE(WinApi.SCROLLINFO); i.fMask := {0, 1, 2};	(* range, page, pos *)
				res := WinApi.GetScrollInfo(f.i.ctrl, WinApi.SB_CTL, i);
				IF (res # 0) THEN
					IF sect > 0 THEN q := WinApi.MulDiv(sect, scrollRange, size - sect); m := scrollRange + q
					ELSE q := -1; m := scrollRange
					END;
					IF (i.nPos # p) OR (i.nPage # q + 1) THEN
						i.nPos := p; i.nPage := q + 1; i.nMax := m;
						res := WinApi.SetScrollInfo(f.i.ctrl, WinApi.SB_CTL, i, 1)
					END
				ELSIF p # WinApi.GetScrollPos(f.i.ctrl, WinApi.SB_CTL) THEN
					res := WinApi.SetScrollPos(f.i.ctrl, WinApi.SB_CTL, p, 1)
				END
	(*
				IF WinApi.GetScrollPos(f.i.ctrl, WinApi.SBCtl) # pos THEN
					res := WinApi.SetScrollRange(f.i.ctrl, WinApi.SBCtl, 0, size, 1);
					res := WinApi.SetScrollPos(f.i.ctrl, WinApi.SBCtl, pos, 1)
				END
	*)
			ELSE
				IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: ScrollBar) Restore (l, t, r, b: INTEGER);
		VAR res, w, h: INTEGER; style: SET;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			style := {(*1,*) 16, 30};	(* topalign, tabstop, child *)
			f.view.context.GetSize(w, h);
			IF h > w THEN INCL(style, 0) END;	(* vertical *)
			Open(f, "SCROLLBAR", "", style, {}, f.i);
			res := WinApi.SetScrollRange(f.i.ctrl, WinApi.SB_CTL, 0, scrollRange, 1)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, f.dot, -1, -1, "") END
	END Restore;

	PROCEDURE (f: ScrollBar) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: ScrollBar) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		SendKey(ch, f.i.ctrl)
	END KeyDown;

	PROCEDURE (f: ScrollBar) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* Field *)

	PROCEDURE (f: Field) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: Field) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE InsLF (VAR x: ARRAY OF CHAR);
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE x[i] # 0X DO
			IF x[i] = 0DX THEN INC(j) END;
			INC(i); INC(j)
		END;
		x[j] := 0X;
		WHILE i # j DO
			DEC(i); DEC(j);
			IF x[i] = 0DX THEN x[j] := 0AX; DEC(j) END;
			x[j] := x[i]
		END
	END InsLF;

	PROCEDURE DelLF (VAR x: ARRAY OF CHAR);
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE x[i] # 0X DO
			IF x[i] = 0AX THEN INC(i) END;
			x[j] := x[i]; INC(i); INC(j)
		END;
		x[j] := 0X
	END DelLF;

	PROCEDURE Equal (f: Field; VAR x, y: ARRAY OF CHAR): BOOLEAN;
	BEGIN
		DelLF(y);
		RETURN f.Equal(f, x, y)
	END Equal;

	PROCEDURE (f: Field) Update;
		VAR res: INTEGER; s, s1: ARRAY 512 OF CHAR; ps, ps1: POINTER TO ARRAY OF CHAR; style: SET;
	BEGIN
		IF f.maxLen > 255 THEN
			NEW(ps, 2 * f.maxLen + 1); NEW(ps1, 2 * f.maxLen + 1);
			IF f.undef OR f.disabled THEN ps[0] := 0X ELSE f.Get(f, ps^) END;
			res := WinApi.GetWindowTextW(f.i.ctrl, ps1^, LEN(ps1^)); ConvertToUnicode(ps1^);
			IF (WinApi.GetWindowTextLengthW(f.i.ctrl) >= LEN(ps^)) OR ~Equal(f, ps^, ps1^) THEN
				f.isUpdate := TRUE;
				IF f.multiLine THEN InsLF(ps^) END;
				ConvertFromUnicode(ps^);
				res := WinApi.SetWindowTextW(f.i.ctrl, ps^);
				f.isUpdate := FALSE
			END
		ELSE
			IF f.undef OR f.disabled THEN s := "" ELSE f.Get(f, s) END;
			res := WinApi.GetWindowTextW(f.i.ctrl, s1, LEN(s1)); ConvertToUnicode(s1);
			IF (WinApi.GetWindowTextLengthW(f.i.ctrl) >= LEN(s)) OR ~Equal(f, s, s1) THEN
				f.isUpdate := TRUE;
				IF f.multiLine THEN InsLF(s) END;
				ConvertFromUnicode(s);
				res := WinApi.SetWindowTextW(f.i.ctrl, s);
				f.isUpdate := FALSE
			END
		END;
		style := BITS(WinApi.GetWindowLongW(f.i.ctrl, -16));	(* window style *)
		IF (f.readOnly # f.i.readOnly) OR (f.undef # f.i.undef) THEN
(*
			res := WinApi.SetWindowLongW(f.i.ctrl, -16, ORD(style / {11}));
*)
			res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
			f.i.readOnly := f.readOnly; f.i.undef := f.undef
		END;
		IF f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: Field) Restore (l, t, r, b: INTEGER);
		VAR res, w, h: INTEGER; style: SET; s: Dialog.String;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.view.context.GetSize(w, h);
			IF h > dropDownHeight THEN
				style := {2, 6, 16, 23, 30};	(* multiline, autovscroll, tabstop, border, child *)
				IF f.multiLine THEN INCL(style, 21) END	(* ver scroll *)
			ELSE
				style := {7, 16, 23, 30}	(* autohscroll, tabstop, border, child *)
			END;
(*
			IF f.readOnly THEN INCL(style, 11) END;	(* readonly *)
*)
			f.i.readOnly := f.readOnly; f.i.undef := f.undef;
			IF f.right & ~f.left THEN style := style + {1, 2}	(* right align, multiline *)
			ELSIF ~f.left THEN style := style + {0, 2}	(* center, multiline *)
			END;
			IF f.password THEN INCL(style, 5) END;	(* password *)
			Open(f, "EDIT", "", style, WinApi.WS_EX_CLIENTEDGE, f.i);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_LIMITTEXT, f.maxLen, 0);
			f.Update;
			IF f.front & ~f.disabled THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0);
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, 23H, 0)
			END
		ELSE f.Update
		END;
		IF f.i.wnd # 0 THEN
			Paint(f, f.i)
		ELSE
			f.Get(f, s);
			Print(f, f.dot, 2 * Ports.point, 0, s)
		END
	END Restore;

	PROCEDURE (f: Field) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: Field) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.multiLine OR (ch # 0DX) THEN
			f.del := FALSE;
			IF (ch = DEL) OR (ch = BS) THEN f.del := TRUE END;
			SendKey(ch, f.i.ctrl)
		END
	END KeyDown;

	PROCEDURE (f: Field) Edit (op: INTEGER; VAR v: Views.View; VAR w, h: INTEGER;
										VAR singleton, clipboard: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		IF clipboard THEN
			IF op = Controllers.cut THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_CUT, 0, 0)
			ELSIF op = Controllers.copy THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_COPY, 0, 0)
			ELSIF op = Controllers.paste THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_PASTE, 0, 0)
			END
		END
	END Edit;

	PROCEDURE (f: Field) Idle, EMPTY;

	PROCEDURE (f: Field) Select (from, to: INTEGER);
		VAR res: INTEGER;
	BEGIN
		IF to = MAX(INTEGER) THEN to := -1 END;
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_SETSEL, from, to)
	END Select;

	PROCEDURE (f: Field) GetSelection (OUT from, to: INTEGER);
		VAR res: INTEGER;
	BEGIN
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_GETSEL, SYSTEM.ADR(from), SYSTEM.ADR(to));
		IF from = -1 THEN to := -1 ELSIF to = -1 THEN to := MAX(INTEGER) END
	END GetSelection;

	PROCEDURE (f: Field) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;

	PROCEDURE (f: Field) Length (): INTEGER;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.GetWindowTextLengthW(f.i.ctrl);
		RETURN res
	END Length;

	PROCEDURE (f: Field) GetCursor (x, y: INTEGER; modifiers: SET; VAR cursor: INTEGER);
		VAR res, hc: INTEGER; pt: WinApi.POINT;
	BEGIN
		pt.x := (x - 1) DIV f.unit + 1; pt.y := (y - 1) DIV f.unit + 1;
		res := WinApi.ClientToScreen(f.i.wnd, pt);
		hc := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_NCHITTEST, 0, pt.x + pt.y * 65536);
		IF hc = 1 THEN cursor := Ports.textCursor END
	END GetCursor;



	(* UpDownField *)

	PROCEDURE (f: UpDownField) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: UpDownField) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0; f.i.ud := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: UpDownField) Update;
		VAR res, val: INTEGER; s: ARRAY 16 OF CHAR; style: SET; upd: BOOLEAN;
	BEGIN
		IF ~f.disabled THEN
			IF f.undef THEN upd := TRUE ELSE f.Get(f, val); upd := val # f.val END;
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 1); upd := TRUE
			END;
			IF f.readOnly THEN
				IF WinApi.IsWindowEnabled(f.i.ud) # 0 THEN res := WinApi.EnableWindow(f.i.ud, 0) END
			ELSE
				IF WinApi.IsWindowEnabled(f.i.ud) = 0 THEN res := WinApi.EnableWindow(f.i.ud, 1) END
			END;
			IF upd THEN
				f.isUpdate := TRUE;
				IF f.undef THEN s := ""
				ELSE Strings.IntToString(val, s)
				END;
				res := WinApi.SetWindowTextW(f.i.ctrl, s);
				f.val := val; f.isUpdate := FALSE
			END;
			style := BITS(WinApi.GetWindowLongW(f.i.ctrl, -16));	(* window style *)
		IF (f.readOnly # f.i.readOnly) OR (f.undef # f.i.undef) THEN
(*
			res := WinApi.SetWindowLongW(f.i.ctrl, -16, ORD(style / {11}));
*)
			res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
			f.i.readOnly := f.readOnly; f.i.undef := f.undef
		END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				f.isUpdate := TRUE;
				res := WinApi.SetWindowTextW(f.i.ctrl, "");
				f.isUpdate := FALSE;
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END;
			IF WinApi.IsWindowEnabled(f.i.ud) # 0 THEN res := WinApi.EnableWindow(f.i.ud, 0) END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: UpDownField) Restore (l, t, r, b: INTEGER);
		VAR res, w, h: INTEGER; style: SET; s: ARRAY 16 OF CHAR;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.view.context.GetSize(w, h);
			style := {7, 16, 23, 30};	(* autohscroll, tabstop, border, child *)
(*
			IF f.readOnly THEN INCL(style, 11) END;	(* readonly *)
*)
			f.i.readOnly := f.readOnly; f.i.undef := f.undef;
			f.val := 0;
			Open(f, "EDIT", "0", style, WinApi.WS_EX_CLIENTEDGE, f.i);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_LIMITTEXT, 16, 0);
			f.Update;
			IF f.front & ~f.disabled THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0);
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, 23H, 0)
			END
		ELSE f.Update
		END;
		IF f.i.wnd # 0 THEN
			Paint(f, f.i)
		ELSE
			f.Get(f, w);
			Strings.IntToString(w, s);
			Print(f, f.dot, 2 * Ports.point, 0, s)
		END
	END Restore;

	PROCEDURE (f: UpDownField) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: UpDownField) KeyDown (ch: CHAR);
		VAR val: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF (ch = AU) OR (ch = AD) THEN
			val := f.val;
			IF ch = AU THEN
				IF val <= f.max - f.inc THEN val := val + f.inc ELSE val := f.max END
			ELSE
				IF val >= f.min + f.inc THEN val := val - f.inc ELSE val := f.min END
			END;
			IF val # f.val THEN f.Set(f, val); f.Update END
		ELSIF (ch # 0DX) THEN
			SendKey(ch, f.i.ctrl)
		END
	END KeyDown;

	PROCEDURE (f: UpDownField) Edit (op: INTEGER; VAR v: Views.View; VAR w, h: INTEGER;
										VAR singleton, clipboard: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		IF clipboard THEN
			IF op = Controllers.cut THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_CUT, 0, 0)
			ELSIF op = Controllers.copy THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_COPY, 0, 0)
			ELSIF op = Controllers.paste THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_PASTE, 0, 0)
			END
		END
	END Edit;

	PROCEDURE (f: UpDownField) Idle, EMPTY;

	PROCEDURE (f: UpDownField) Select (from, to: INTEGER);
		VAR res: INTEGER;
	BEGIN
		IF to = MAX(INTEGER) THEN to := -1 END;
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_SETSEL, from, to)
	END Select;

	PROCEDURE (f: UpDownField) GetSelection (OUT from, to: INTEGER);
		VAR res: INTEGER;
	BEGIN
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_GETSEL, SYSTEM.ADR(from), SYSTEM.ADR(to));
		IF from = -1 THEN to := -1 ELSIF to = -1 THEN to := MAX(INTEGER) END
	END GetSelection;

	PROCEDURE (f: UpDownField) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;

	PROCEDURE (f: UpDownField) GetCursor (x, y: INTEGER; modifiers: SET; VAR cursor: INTEGER);
		VAR res, hc: INTEGER; pt: WinApi.POINT;
	BEGIN
		pt.x := (x - 1) DIV f.unit + 1; pt.y := (y - 1) DIV f.unit + 1;
		res := WinApi.ClientToScreen(f.i.wnd, pt);
		hc := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_NCHITTEST, 0, pt.x + pt.y * 65536);
		IF hc = 1 THEN cursor := Ports.textCursor END
	END GetCursor;



	(* TimeField *)

	PROCEDURE GetTimePart (VAR time: Dates.Time; part: INTEGER; VAR val, min, max: INTEGER);
	BEGIN
		IF part = -1 THEN part := lastPart END;
		IF part = 4 THEN val := time.hour DIV 12; min := 0; max := 1
		ELSIF part = 3 THEN val := time.second; min := 0; max := 59
		ELSIF part = 2 THEN val := time.minute; min := 0; max := 59
		ELSIF lastPart = 3 THEN val := time.hour; min := 0; max := 23
		ELSE val := (time.hour - 1) MOD 12 + 1; min := 1; max := 12
		END
	END GetTimePart;

	PROCEDURE SetTimePart (VAR time: Dates.Time; part: INTEGER; val: INTEGER);
	BEGIN
		IF part = -1 THEN part := lastPart END;
		IF part = 4 THEN time.hour := val * 12 + time.hour MOD 12
		ELSIF part = 3 THEN time.second := val
		ELSIF part = 2 THEN time.minute := val
		ELSIF lastPart = 3 THEN time.hour := val
		ELSE time.hour := time.hour DIV 12 * 12 + val MOD 12
		END
	END SetTimePart;

	PROCEDURE TimeToString (VAR time: Dates.Time; VAR str: ARRAY OF CHAR);
		VAR val, min, max, i, j, k: INTEGER;
	BEGIN
		GetTimePart(time, 1, val, min, max);
		str[0] := CHR(val DIV 10 + ORD("0"));
		str[1] := CHR(val MOD 10 + ORD("0"));
		str[2] := timeSep;
		GetTimePart(time, 2, val, min, max);
		str[3] := CHR(val DIV 10 + ORD("0"));
		str[4] := CHR(val MOD 10 + ORD("0"));
		str[5] := timeSep;
		GetTimePart(time, 3, val, min, max);
		str[6] := CHR(val DIV 10 + ORD("0"));
		str[7] := CHR(val MOD 10 + ORD("0"));
		IF lastPart = 3 THEN
			str[8] := 0X
		ELSE
			str[8] := " ";
			i := 9; j := 0; k := time.hour DIV 12;
			WHILE desig[k, j] # 0X DO str[i] := desig[k, j]; INC(i); INC(j) END;
			str[i] := 0X
		END
	END TimeToString;

	PROCEDURE (f: TimeField) Select, NEW;
		VAR res: INTEGER; sel: INTEGER;
	BEGIN
		f.GetSel(f, sel);
		IF sel = -1 THEN sel := lastPart END;
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_SETSEL, 3 * sel - 3, 3 * sel - 1)
	END Select;

	PROCEDURE (f: TimeField) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: TimeField) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0; f.i.ud := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: TimeField) Update;
		VAR res: INTEGER; s, s1: ARRAY 20 OF CHAR; time: Dates.Time; style: SET;
	BEGIN
		IF ~f.disabled THEN
			IF f.undef THEN s := "" ELSE f.Get(f, time); TimeToString(time, s) END;
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			IF f.readOnly THEN
				IF WinApi.IsWindowEnabled(f.i.ud) # 0 THEN res := WinApi.EnableWindow(f.i.ud, 0) END
			ELSE
				IF WinApi.IsWindowEnabled(f.i.ud) = 0 THEN res := WinApi.EnableWindow(f.i.ud, 1) END
			END;
			res := WinApi.GetWindowTextW(f.i.ctrl, s1, LEN(s1));
			IF s # s1 THEN
				f.isUpdate := TRUE;
				res := WinApi.SetWindowTextW(f.i.ctrl, s);
				f.isUpdate := FALSE
			END;
			style := BITS(WinApi.GetWindowLongW(f.i.ctrl, -16));	(* window style *)
		IF (f.readOnly # f.i.readOnly) OR (f.undef # f.i.undef) THEN
(*
			res := WinApi.SetWindowLongW(f.i.ctrl, -16, ORD(style / {11}));
*)
			res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
			f.i.readOnly := f.readOnly; f.i.undef := f.undef
		END;
			f.Select
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				f.isUpdate := TRUE;
				res := WinApi.SetWindowTextW(f.i.ctrl, "");
				f.isUpdate := FALSE;
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END;
			IF WinApi.IsWindowEnabled(f.i.ud) # 0 THEN res := WinApi.EnableWindow(f.i.ud, 0) END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: TimeField) Restore (l, t, r, b: INTEGER);
		VAR res, w, h: INTEGER; style: SET; s: ARRAY 20 OF CHAR; time: Dates.Time;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.view.context.GetSize(w, h);
			style := {7, 16, 23, 30};	(* autohscroll, tabstop, border, child *)
(*
			IF f.readOnly THEN INCL(style, 11) END;	(* readonly *)
*)
			f.i.readOnly := f.readOnly; f.i.undef := f.undef;
			Open(f, "EDIT", "", style, WinApi.WS_EX_CLIENTEDGE, f.i);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_LIMITTEXT, 16, 0);
			f.Update;
			IF f.front & ~f.disabled THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0);
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, 23H, 0)
			END
		ELSE f.Update
		END;
		IF f.i.wnd # 0 THEN
			Paint(f, f.i)
		ELSE
			f.Get(f, time);
			TimeToString(time, s);
			Print(f, f.dot, 2 * Ports.point, 0, s)
		END
	END Restore;

	PROCEDURE (f: TimeField) MouseDown (x, y: INTEGER; buttons: SET);
		VAR res: INTEGER; sel: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			f.GetSel(f, sel);
			IF sel = 0 THEN f.SetSel(f, 1); f.Select END;
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_GETSEL, 0, 0);
			sel := res MOD 65536 DIV 3 + 1;
			IF sel >= lastPart THEN sel := -1 END;
			f.cur := 0; f.SetSel(f, sel); f.Select
		END
	END MouseDown;

	PROCEDURE (f: TimeField) KeyDown (ch: CHAR);
		VAR sel, s, val, v, min, max: INTEGER; time: Dates.Time;
	BEGIN
		ASSERT(~f.disabled, 100);
		f.GetSel(f, sel); s := sel;
		IF s = -1 THEN s := lastPart END;
		f.Get(f, time);
		GetTimePart(time, s, val, min, max); v := val;
		IF (ch = TAB) OR (ch = AR) THEN
			s := s MOD lastPart + 1; f.cur := 0
		ELSIF (ch = LTAB) OR (ch = AL) THEN
			DEC(s); f.cur := 0;
			IF s <= 0 THEN s := lastPart END
		ELSIF (ch = PL) OR (ch = DL) THEN
			s := 1; f.cur := 0
		ELSIF (ch = PR) OR (ch = DR) THEN
			s := lastPart; f.cur := 0
		ELSIF (ch = DEL) OR (ch = BS) THEN
			v := min
		ELSIF ch = AU THEN
			IF v < max THEN INC(v) ELSE v := min END
		ELSIF ch = AD THEN
			IF v > min THEN DEC(v) ELSE v := max END
		ELSIF (ch >= "0") & (ch <= "9") & (s < 4) THEN
			v := v * 10 MOD 100 + ORD(ch) - ORD("0");
			IF v > max THEN v := v MOD 10 ELSIF v < min THEN v := min END
		ELSIF s = 4 THEN
			IF (ch = " ") OR (CAP(ch) = CAP(desig[1 - v, 0])) THEN v := 1 - v END
		END;
		IF s = lastPart THEN s := -1 END;
		IF v # val THEN
			SetTimePart(time, s, v); f.Set(f, time); f.Update
		ELSIF s # sel THEN
			f.SetSel(f, s); f.Select
		END
	END KeyDown;

	PROCEDURE (f: TimeField) Edit (op: INTEGER; VAR v: Views.View; VAR w, h: INTEGER;
										VAR singleton, clipboard: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		IF clipboard THEN
			IF op = Controllers.copy THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_COPY, 0, 0)
			END
		END
	END Edit;

	PROCEDURE (f: TimeField) Mark (on, focus: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		Mark(on, f.front, f.i);
		IF ~on & focus THEN
			f.SetSel(f, 0); f.cur := 0;
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_SETSEL, 0, 0)
		END
	END Mark;

	PROCEDURE (f: TimeField) GetCursor (x, y: INTEGER; modifiers: SET; VAR cursor: INTEGER);
		VAR res, hc: INTEGER; pt: WinApi.POINT;
	BEGIN
		pt.x := (x - 1) DIV f.unit + 1; pt.y := (y - 1) DIV f.unit + 1;
		res := WinApi.ClientToScreen(f.i.wnd, pt);
		hc := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_NCHITTEST, 0, pt.x + pt.y * 65536);
		IF hc = 1 THEN cursor := Ports.textCursor END
	END GetCursor;



	(* DateField *)

	PROCEDURE GetDatePart (VAR date: Dates.Date; part: INTEGER; OUT val, min, max: INTEGER);
	(* GetDatePart picks the day, month or year part out of a given date and asigns it to the out
		parameter val, together with the min and max possible values for this part
	*)
	BEGIN
		IF part = -1 THEN part := 3 END;
		IF part = yearPart THEN val := date.year; min := 1; max := 9999
		ELSIF part = monthPart THEN val := date.month; min := 1; max := 12
		ELSE
			val := date.day; min := 1;
			IF date.month = 0 THEN
				max := 31
			ELSIF date.month = 2 THEN
				IF (date.year MOD 4 = 0)
					& ((date.year < 1583) OR (date.year MOD 100 # 0) OR (date.year MOD 400 = 0))
				THEN
					max := 29
				ELSE max := 28
				END
			ELSIF date.month IN {1, 3, 5, 7, 8, 10, 12} THEN max := 31
			ELSE max := 30
			END
		END
	END GetDatePart;

	PROCEDURE SetDatePart (VAR date: Dates.Date; part: INTEGER; val: INTEGER);
	(* SetDatePart sets the day, month or year part in a given date to the value specivied
		by the parameter val.
		If the month is set, the day is adjusted to the possible range of days in this month.
		If the day is set, then the month may be changed in order to obtain a valid date
	*)
		VAR v, min, max: INTEGER;
	BEGIN
		IF part = -1 THEN part := 3 END;
		IF part = yearPart THEN date.year := val
		ELSIF part = monthPart THEN date.month := val
		ELSE date.day := val
		END;
		GetDatePart(date, dayPart, v, min, max);
		IF (part = monthPart) THEN (* adjust day if month value is set and day > max *)
			IF v > max THEN date.day := max END
		ELSIF part = yearPart THEN (* adjust month is day value is set and day > max *)
			IF v > max THEN INC(date.month) END
		END
	END SetDatePart;

	PROCEDURE DateToString (VAR date: Dates.Date; VAR str: ARRAY OF CHAR);
		VAR val, min, max, p, i: INTEGER;
	BEGIN
		p := 1; i := 0;
		WHILE p <= 3 DO
			IF p > 1 THEN str[i] := dateSep; INC(i) END;
			GetDatePart(date, p, val, min, max);
			IF max = 9999 THEN
				str[i] := CHR(val DIV 1000 MOD 10 + ORD("0")); INC(i);
				str[i] := CHR(val DIV 100 MOD 10 + ORD("0")); INC(i)
			END;
			str[i] := CHR(val DIV 10 MOD 10 + ORD("0")); INC(i);
			str[i] := CHR(val MOD 10 + ORD("0")); INC(i);
			INC(p)
		END;
		str[i] := 0X
	END DateToString;

	PROCEDURE (f: DateField) Select, NEW;
		VAR res: INTEGER; sel, a, b: INTEGER;
	BEGIN
		f.GetSel(f, sel);
		IF sel = 1 THEN a := 0; b := del1
		ELSIF sel = 2 THEN a := del1 + 1; b := del2
		ELSE a := del2 + 1; b := del2 + 5
		END;
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_SETSEL, a, b)
	END Select;

	PROCEDURE (f: DateField) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: DateField) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0; f.i.ud := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: DateField) Update;
		VAR res: INTEGER; s, s1: ARRAY 20 OF CHAR; date: Dates.Date; style: SET; sel: INTEGER;
	BEGIN
		IF ~f.disabled THEN
			IF f.undef THEN s := ""
			ELSE f.Get(f, date);
				IF f.cnt > 0 THEN f.GetSel(f, sel); SetDatePart(date, sel, f.val) END;
				DateToString(date, s)
			END;
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			IF f.readOnly THEN
				IF WinApi.IsWindowEnabled(f.i.ud) # 0 THEN res := WinApi.EnableWindow(f.i.ud, 0) END
			ELSE
				IF WinApi.IsWindowEnabled(f.i.ud) = 0 THEN res := WinApi.EnableWindow(f.i.ud, 1) END
			END;
			res := WinApi.GetWindowTextW(f.i.ctrl, s1, LEN(s1));
			IF s # s1 THEN
				f.isUpdate := TRUE;
				res := WinApi.SetWindowTextW(f.i.ctrl, s);
				f.isUpdate := FALSE
			END;
			style := BITS(WinApi.GetWindowLongW(f.i.ctrl, -16));	(* window style *)
			IF (f.readOnly # f.i.readOnly) OR (f.undef # f.i.undef) THEN
	(*
				res := WinApi.SetWindowLongW(f.i.ctrl, -16, ORD(style / {11}));
	*)
				res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
				f.i.readOnly := f.readOnly; f.i.undef := f.undef
			END;
			f.Select
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				f.isUpdate := TRUE;
				res := WinApi.SetWindowTextW(f.i.ctrl, "");
				f.isUpdate := FALSE;
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END;
			IF WinApi.IsWindowEnabled(f.i.ud) # 0 THEN res := WinApi.EnableWindow(f.i.ud, 0) END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: DateField) Restore (l, t, r, b: INTEGER);
		VAR res, w, h: INTEGER; style: SET; s: ARRAY 20 OF CHAR; date: Dates.Date;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.view.context.GetSize(w, h);
			style := {7, 16, 23, 30};	(* autohscroll, tabstop, border, child *)
(*
			IF f.readOnly THEN INCL(style, 11) END;	(* readonly *)
*)
			f.i.readOnly := f.readOnly; f.i.undef := f.undef;
			Open(f, "EDIT", "", style, WinApi.WS_EX_CLIENTEDGE, f.i);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_LIMITTEXT, 16, 0);
			f.Update;
			IF f.front & ~f.disabled THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0);
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, 23H, 0)
			END
		ELSE f.Update
		END;
		IF f.i.wnd # 0 THEN
			Paint(f, f.i)
		ELSE
			f.Get(f, date);
			DateToString(date, s);
			Print(f, f.dot, 2 * Ports.point, 0, s)
		END
	END Restore;

	PROCEDURE ActualizeDate(f: DateField);
		VAR sel: INTEGER; date: Dates.Date; val, min, max: INTEGER;
	BEGIN
		IF f.cnt > 0 THEN
			f.GetSel(f, sel); IF sel = -1 THEN sel := 3 END;
			f.Get(f, date);
			IF (sel = yearPart) & (f.cnt <= 2) THEN
				IF f.val < 50 THEN f.val := f.val + 2000
				ELSIF f.val < 100 THEN f.val := f.val + 1900
				END
			END;
			GetDatePart(date, sel, val, min, max);
			IF (min <= f.val) & (f.val <= max) THEN
				SetDatePart(date, sel, f.val); f.Set(f, date)
			END;
			f.cnt := 0;
			f.Update
		END
	END ActualizeDate;

	PROCEDURE (f: DateField) MouseDown (x, y: INTEGER; buttons: SET);
		VAR res: INTEGER; sel: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			ActualizeDate(f);
			f.GetSel(f, sel);
			IF sel = 0 THEN f.SetSel(f, 1); f.Select END;
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_GETSEL, 0, 0);
			res := res MOD 65536;
			IF res <= del1 THEN sel := 1
			ELSIF res <= del2 THEN sel := 2
			ELSE sel := -1
			END;
			f.SetSel(f, sel); f.Select
		END
	END MouseDown;

	PROCEDURE (f: DateField) KeyDown (ch: CHAR);
		VAR sel, s, val, min, max: INTEGER; date: Dates.Date;
	BEGIN
		ASSERT(~f.disabled, 100);
		f.GetSel(f, sel); s := sel;
		IF s = -1 THEN s := 3 END;
		f.Get(f, date);
		GetDatePart(date, s, val, min, max);

		IF (ch = TAB) OR (ch = AR) THEN
			s := s MOD 3 + 1;
			ActualizeDate(f)
		ELSIF ((ch = ".") OR (ch = ",")) & (f.cnt > 0) THEN
			s := s MOD 3 + 1;
			ActualizeDate(f)
		ELSIF (ch = LTAB) OR (ch = AL) THEN
			IF s = 0 THEN s := 1 END;
			s := (s - 2) MOD 3 + 1;
			ActualizeDate(f)
		ELSIF (ch = PL) OR (ch = DL) THEN
			s := 1;
			ActualizeDate(f)
		ELSIF (ch = PR) OR (ch = DR) THEN
			s := 3;
			ActualizeDate(f)
		ELSIF (ch = DEL) OR (ch = BS) THEN
			f.cnt := 4; f.val := min;
			ActualizeDate(f)
		ELSIF ch = AU THEN
			IF f.cnt = 0 THEN f.cnt := 4; f.val := val END;
			IF f.val < max THEN INC(f.val) ELSE f.val := min END;
			ActualizeDate(f)
		ELSIF ch = AD THEN
			IF f.cnt = 0 THEN f.cnt := 4; f.val := val END;
			IF f.val > min THEN DEC(f.val) ELSE f.val := max END;
			ActualizeDate(f)
		ELSIF (ch >= "0") & (ch <= "9") THEN
			IF s = yearPart THEN
				IF f.cnt = 0 THEN
					f.val := ORD(ch) - ORD("0"); INC(f.cnt);
					f.Update
				ELSE
					f.val := f.val * 10 + ORD(ch) - ORD("0");
					INC(f.cnt);
					IF f.cnt = 4 THEN ActualizeDate(f) ELSE f.Update END
				END
			ELSE
				IF f.cnt = 0 THEN
					f.val := ORD(ch) - ORD("0"); f.cnt := 1;
					f.Update
				ELSE
					f.val := f.val * 10 MOD 100 + ORD(ch) - ORD("0");
					IF (s = dayPart) & (max < f.val) & (f.val <= 31) THEN
						INC(date.month); f.Set(f, date); max := 31
					END;
					IF f.val > max THEN f.val := f.val MOD 10 END;
					ActualizeDate(f);
					INC(s)
				END
			END
		END;

		IF s = 3 THEN s := -1 END;
		IF s # sel THEN
			f.SetSel(f, s); f.Select
		END
	END KeyDown;

	PROCEDURE (f: DateField) Edit (op: INTEGER; VAR v: Views.View; VAR w, h: INTEGER;
										VAR singleton, clipboard: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		IF clipboard THEN
			IF op = Controllers.copy THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_COPY, 0, 0)
			END
		END
	END Edit;

	PROCEDURE (f: DateField) Mark (on, focus: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		IF ~on THEN ActualizeDate(f) END;

		Mark(on, f.front, f.i);
		IF ~on & focus THEN
			f.SetSel(f, 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.EM_SETSEL, 0, 0)
		END
	END Mark;

	PROCEDURE (f: DateField) GetCursor (x, y: INTEGER; modifiers: SET; VAR cursor: INTEGER);
		VAR res, hc: INTEGER; pt: WinApi.POINT;
	BEGIN
		pt.x := (x - 1) DIV f.unit + 1; pt.y := (y - 1) DIV f.unit + 1;
		res := WinApi.ClientToScreen(f.i.wnd, pt);
		hc := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_NCHITTEST, 0, pt.x + pt.y * 65536);
		IF hc = 1 THEN cursor := Ports.textCursor END
	END GetCursor;



	(* ColorField *)

	PROCEDURE (f: ColorField) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: ColorField) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: ColorField) Update;
		VAR res, i, j: INTEGER; c: Ports.Color;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			IF f.undef THEN j := -1
			ELSE
				f.Get(f, c); j := 0;
				WHILE (j < numColors) & (c # colors[j]) DO INC(j) END
			END;
			i := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_GETCURSEL, 0, 0);
			IF (i # j) OR (j = numColors) & (c # f.color) THEN
				f.color := c;
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETCURSEL, j, 0)
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETCURSEL, -1, 0);
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		IF f.readOnly # f.i.readOnly THEN
			res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
			f.i.readOnly := f.readOnly
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: ColorField) Restore (l, t, r, b: INTEGER);
		VAR res, w, h, i: INTEGER; style: SET;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.view.context.GetSize(w, h);
			f.i.dropDown := TRUE; f.i.readOnly := f.readOnly;
			(* drop down list, auto scroll, ownerdrawn, tabstop, ver scroll, border, child *)
			style := {0, 1, 4, 6, 16, 21, 23, 30};
			Open(f, "COMBOBOX", "", style, WinApi.WS_EX_CLIENTEDGE, f.i); i := 0;
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETEXTENDEDUI, 1, 0);
			WHILE i < numColors DO
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_ADDSTRING, 0, colors[i]);
				INC(i)
			END;
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_ADDSTRING, 0, -1);
			Adapt(f, f.i)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, f.dot, -1, -1, "") END
	END Restore;

	PROCEDURE (f: ColorField) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: ColorField) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " " THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(ch), 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(ch), 0)
		ELSE SendKey(ch, f.i.ctrl)
		END
	END KeyDown;

	PROCEDURE (f: ColorField) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* ListBox *)

	PROCEDURE (f: ListBox) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: ListBox) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: ListBox) Update;
		VAR res, i, j: INTEGER;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			IF f.undef THEN i := -1 ELSE f.Get(f, i) END; j := i;
			IF f.i.dropDown THEN
				IF (i < 0) OR (i >= WinApi.SendMessageW(f.i.ctrl, WinApi.CB_GETCOUNT, 0, 0)) THEN j := -1
				ELSIF f.sorted THEN
					j := 0;
					WHILE i # WinApi.SendMessageW(f.i.ctrl, WinApi.CB_GETITEMDATA, j, 0) DO INC(j) END
				END;
				i := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_GETCURSEL, j, 0);
				IF i # j THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETCURSEL, j, 0) END
			ELSE
				IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
				IF (i < 0) OR (i >= WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETCOUNT, 0, 0)) THEN j := -1
				ELSIF f.sorted THEN
					j := 0;
					WHILE i # WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETITEMDATA, j, 0) DO INC(j) END
				END;
				i := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETCURSEL, j, 0);
				IF i # j THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETCURSEL, j, 0) END
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				IF f.i.dropDown THEN
					res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETCURSEL, -1, 0)
				ELSE
					res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETCURSEL, -1, 0)
				END;
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		IF f.readOnly # f.i.readOnly THEN
			res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
			f.i.readOnly := f.readOnly
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: ListBox) UpdateList;
		VAR res, i: INTEGER; s: Dialog.String;
	BEGIN
		IF f.i.dropDown THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_RESETCONTENT, 0, 0);
			i := 0; f.GetName(f, i, s);
			Dialog.MapString(s, s); ConvertFromUnicode(s);
			WHILE s # "" DO
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_ADDSTRING, 0, SYSTEM.ADR(s));
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETITEMDATA, res, i);
				INC(i); f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s);
			END;
			Adapt(f, f.i)
		ELSE
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_RESETCONTENT, 0, 0);
			i := 0; f.GetName(f, i, s);
			Dialog.MapString(s, s); ConvertFromUnicode(s);
			WHILE s # "" DO
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_ADDSTRING, 0, SYSTEM.ADR(s));
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETITEMDATA, res, i);
				INC(i); f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s);
			END
		END;
		f.Update
	END UpdateList;

	PROCEDURE (f: ListBox) Restore (l, t, r, b: INTEGER);
		VAR i, res, w, h: INTEGER; style: SET; s: ARRAY 512 OF CHAR;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.view.context.GetSize(w, h);
			IF h > dropDownHeight THEN
				f.i.dropDown := FALSE; f.i.readOnly := f.readOnly;
				style := {0, 16, 21, 23, 30};	(* notify, tabstop, ver scroll, border, child *)
				IF f.sorted THEN INCL(style, 1) END;	(* sort *)
				Open(f, "LISTBOX", "", style, WinApi.WS_EX_CLIENTEDGE, f.i)
			ELSE
				f.i.dropDown := TRUE;
				style := {0, 1, 6, 16, 21, 23, 30};	(* drop down list, auto scroll, tabstop, ver scroll, border, child *)
				IF f.sorted THEN INCL(style, 8) END;	(* sort *)
				Open(f, "COMBOBOX", "", style, WinApi.WS_EX_CLIENTEDGE, f.i);
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETEXTENDEDUI, WinApi.TRUE, 0)
			END;
			f.UpdateList
		ELSE
			f.Update
		END;
		IF f.i.wnd # 0 THEN Paint(f, f.i) 
		ELSE
			f.Get(f, i); f.GetName(f, i, s);
			Print(f, f.dot, 2 * Ports.point, 0, s)
		END
	END Restore;


	PROCEDURE (f: ListBox) DblClickOk (x, y: INTEGER): BOOLEAN;
		VAR res, i, j, msg: INTEGER; rect: WinApi.RECT;
	BEGIN
		f.Get(f, i); j := i;
		IF f.sorted THEN
			IF f.i.dropDown THEN msg := WinApi.CB_GETITEMDATA ELSE msg := WinApi.LB_GETITEMDATA END;
			j := 0;
			WHILE i # WinApi.SendMessageW(f.i.ctrl, msg, j, 0) DO INC(j) END
		END;		
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETITEMRECT, j, SYSTEM.ADR(rect));
		IF res = WinApi.LB_ERR THEN RETURN FALSE END;
		x := x DIV f.unit; y := y DIV f.unit;
		RETURN (x >= rect.left) & (x <= rect.right) & (y >= rect.top) & (y <= rect.bottom)
	END DblClickOk;

	PROCEDURE (f: ListBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: ListBox) WheelMove (x, y: INTEGER; op, nofLines: INTEGER; VAR done: BOOLEAN);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleWheel(f.i.ctrl, x DIV f.unit, y DIV f.unit, op, nofLines, done)
		END
	END WheelMove;

	PROCEDURE (f: ListBox) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " " THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(ch), 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(ch), 0)
		ELSE
			SendKey(ch, f.i.ctrl)
		END
	END KeyDown;

	PROCEDURE (f: ListBox) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* SelectionBox *)

	PROCEDURE (f: SelectionBox) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: SelectionBox) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: SelectionBox) DblClickOk (x, y: INTEGER): BOOLEAN;
		VAR res, i: INTEGER; s: Dialog.String;
		sel: BOOLEAN; rect: WinApi.RECT;
	BEGIN
		i := 0; f.GetName(f, i, s); ConvertFromUnicode(s);
		x := x DIV f.unit; y := y DIV f.unit;
		WHILE s # "" DO
			f.Get(f, i, sel);
			IF sel THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETITEMRECT, i, SYSTEM.ADR(rect));
				IF res = WinApi.LB_ERR THEN RETURN FALSE END;
				IF (x >= rect.left) & (x <= rect.right) & (y >= rect.top) & (y <= rect.bottom) THEN RETURN TRUE END
			END;
			INC(i); f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s)
		END;
		RETURN FALSE
	END DblClickOk;

	PROCEDURE (f: SelectionBox) Update;
		VAR res, i, j, a, b: INTEGER; sel: BOOLEAN;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			i := 0;
			WHILE i < f.num DO j := i;
				IF f.sorted THEN
					j := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETITEMDATA, i, 0)
				END;
				f.Get(f, j, sel);
				IF sel & ~f.undef THEN a := 1 ELSE a := 0 END;
				b := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETSEL, i, 0);
				IF a # b THEN
					res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETSEL, a, i) END;
				INC(i)
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETSEL, 0, -1);
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		IF f.readOnly # f.i.readOnly THEN
			res := WinApi.InvalidateRect(f.i.ctrl, NIL, 1);
			f.i.readOnly := f.readOnly
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: SelectionBox) UpdateRange (op, from, to: INTEGER);
		VAR res, i, a, b: INTEGER; sel: BOOLEAN;
	BEGIN
		ASSERT((from >= 0) & (from <= to) & (to < f.num), 100);
		IF (op = Dialog.set) OR (from # to) THEN f.Update; RETURN END;
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			i := from;
			IF f.sorted THEN
				i := 0;
				WHILE WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETITEMDATA, i, 0) # from DO INC(i) END
			END;
			f.Get(f, from, sel);
			IF sel THEN a := 1 ELSE a := 0 END;
			b := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_GETSEL, i, 0);
			IF a # b THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETSEL, a, i) END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		END
	END UpdateRange;

	PROCEDURE (f: SelectionBox) UpdateList;
		VAR res, i: INTEGER; s: Dialog.String;
	BEGIN
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_RESETCONTENT, 0, 0);
		i := 0; f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s);
		WHILE s # "" DO
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_ADDSTRING, 0, SYSTEM.ADR(s));
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.LB_SETITEMDATA, res, i);
			INC(i); f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s)
		END;
		f.num := i;
		f.Update
	END UpdateList;

	PROCEDURE (f: SelectionBox) Restore (l, t, r, b: INTEGER);
		VAR style: SET;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.i.readOnly := f.readOnly;
			style := {0, 3, 16, 21, 23, 30} + WinApi.LBS_EXTENDEDSEL;
				(* notify, multiple sel, tabstop, ver scroll, border, child *)
			IF f.sorted THEN INCL(style, 1) END;	(* sort *)
			Open(f, "LISTBOX", "", style, WinApi.WS_EX_CLIENTEDGE, f.i);
			f.UpdateList
		ELSE
			f.Update
		END;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, f.dot, -1, -1, "") END
	END Restore;

	PROCEDURE (f: SelectionBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: SelectionBox) WheelMove (x, y: INTEGER; op, nofLines: INTEGER; VAR done: BOOLEAN);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleWheel(f.i.ctrl, x, y, op, nofLines, done)
		END
	END WheelMove;

	PROCEDURE (f: SelectionBox) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " " THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(ch), 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(ch), 0)
		ELSE
			SendKey(ch, f.i.ctrl)
		END
	END KeyDown;

	PROCEDURE (f: SelectionBox) Select (from, to: INTEGER), EMPTY;

	PROCEDURE (f: SelectionBox) GetSelection (OUT from, to: INTEGER);
	BEGIN
		from := 0; to := MAX(INTEGER)
	END GetSelection;

	PROCEDURE (f: SelectionBox) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;



	(* ComboBox *)

	PROCEDURE (f: ComboBox) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: ComboBox) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: ComboBox) Update;
		VAR res: INTEGER; s, s1: Dialog.String;
	BEGIN
		IF ~f.disabled THEN
			IF f.undef THEN s := "" ELSE f.Get(f, s); ConvertFromUnicode(s) END;
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END;
			res := WinApi.GetWindowTextW(f.i.ctrl, s1, LEN(s1));
			IF (WinApi.GetWindowTextLengthW(f.i.ctrl) >= LEN(s)) OR (s # s1) THEN
				IF ~f.i.dropDown THEN
					res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_FINDSTRINGEXACT, -1, SYSTEM.ADR(s));
					res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETCURSEL, res, 0)
				END;
				res := WinApi.SetWindowTextW(f.i.ctrl, s)
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETCURSEL, -1, 0);
				res := WinApi.SetWindowTextW(f.i.ctrl, "");
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: ComboBox) UpdateList;
		VAR res, i: INTEGER; s: Dialog.String;
	BEGIN
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_RESETCONTENT, 0, 0);
		i := 0; f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s);
		WHILE s # "" DO
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_ADDSTRING, 0, SYSTEM.ADR(s));
			INC(i); f.GetName(f, i, s); Dialog.MapString(s, s); ConvertFromUnicode(s)
		END;
		Adapt(f, f.i);
		f.Update
	END UpdateList;

	PROCEDURE (f: ComboBox) Restore (l, t, r, b: INTEGER);
		VAR res, w, h: INTEGER; s: Dialog.String;
			style: SET; pt: WinApi.POINT;
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.Get(f, s); ConvertFromUnicode(s); f.view.context.GetSize(w, h);
			style := {6, 16, 21, 23, 30};	(* auto scroll, tabstop, ver scroll, border, child *)
			IF f.sorted THEN INCL(style, 8) END;	(* sort *)
			f.i.dropDown := h <= dropDownHeight;
			IF f.i.dropDown THEN INCL(style, 1) ELSE INCL(style, 0) END;	(* dropdown / simple *)
			Open(f, "COMBOBOX", s, style, WinApi.WS_EX_CLIENTEDGE, f.i);
			pt.x := 4; pt.y := 4;
			f.edit := WinApi.ChildWindowFromPoint(f.i.ctrl, pt);	(* hack found in win api manual ! *)
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETEXTENDEDUI, 1, 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_LIMITTEXT, 255, 0);
			f.UpdateList
		ELSE
			f.Update
		END;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, f.dot, -1, -1, "") END
	END Restore;

	PROCEDURE (f: ComboBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: ComboBox) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		SendKey(ch, f.edit)
	END KeyDown;

	PROCEDURE (f: ComboBox) Edit (op: INTEGER;
		VAR v: Views.View; VAR w, h: INTEGER; VAR singleton, clipboard: BOOLEAN
	);
		VAR res: INTEGER;
	BEGIN
		IF clipboard THEN
			IF op = Controllers.cut THEN
				res := WinApi.SendMessageW(f.edit, WinApi.WM_CUT, 0, 0)
			ELSIF op = Controllers.copy THEN
				res := WinApi.SendMessageW(f.edit, WinApi.WM_COPY, 0, 0)
			ELSIF op = Controllers.paste THEN
				res := WinApi.SendMessageW(f.edit, WinApi.WM_PASTE, 0, 0)
			END
		END
	END Edit;

	PROCEDURE (f: ComboBox) Idle, EMPTY;

	PROCEDURE (f: ComboBox) Select (from, to: INTEGER);
		VAR res: INTEGER;
	BEGIN
		IF to > 32767 THEN to := -1 END;
		IF from < 0 THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETEDITSEL, 0, 65535)
		ELSE res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_SETEDITSEL, 0, from MOD 65536 + to * 65536)
		END
	END Select;

	PROCEDURE (f: ComboBox) GetSelection (OUT from, to: INTEGER);
		VAR res: INTEGER;
	BEGIN
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.CB_GETEDITSEL, SYSTEM.ADR(from), SYSTEM.ADR(to));
		IF from = -1 THEN to := -1 ELSIF to = -1 THEN to := MAX(INTEGER) END
	END GetSelection;

	PROCEDURE (f: ComboBox) Mark (on, focus: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		IF f.front THEN
			IF on THEN
				IF ~f.i.hasFocus THEN
					res := WinApi.SendMessageW(f.edit, WinApi.WM_SETFOCUS, 0, 0);
					f.i.hasFocus := TRUE
				END
			ELSE
				IF f.i.hasFocus THEN
					res := WinApi.SendMessageW(f.edit, WinApi.WM_KILLFOCUS, 0, 0);
					f.i.hasFocus := FALSE
				END
			END
		END
	END Mark;

	PROCEDURE (f: ComboBox) Length (): INTEGER;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.GetWindowTextLengthW(f.i.ctrl);
		RETURN res
	END Length;

	PROCEDURE (f: ComboBox) GetCursor (x, y: INTEGER; modifiers: SET; VAR cursor: INTEGER);
		VAR res, hc: INTEGER; pt: WinApi.POINT;
	BEGIN
		pt.x := (x - 1) DIV f.unit + 1; pt.y := (y - 1) DIV f.unit + 1;
		res := WinApi.ClientToScreen(f.i.wnd, pt);
		hc := WinApi.SendMessageW(f.edit, WinApi.WM_NCHITTEST, 0, pt.x + pt.y * 65536);
		IF hc = 1 THEN cursor := Ports.textCursor END
(*
		u := f.unit; pt.x := x DIV u - 1; pt.y := y DIV u - 1;
		wnd := WinApi.ChildWindowFromPoint(f.i.ctrl, pt);
		IF wnd = f.edit THEN cursor := Ports.textCursor
		ELSE cursor := Ports.arrowCursor
		END

		u := f.unit; pt.x := x DIV u; pt.y := y DIV u;
		res := WinApi.ClientToScreen(f.i.wnd, pt);
		ChildWindowAt(f.i.wnd, pt, wnd, hc);
		res := WinApi.ScreenToClient(wnd, pt);
		res := WinApi.SendMessageW(wnd, WinApi.WMSetCursor, wnd, hc + WinApi.WMMouseMove * 65536);
		cursor := -1
*)
	END GetCursor;



	(* Caption *)

	PROCEDURE (f: Caption) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: Caption) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f:Caption ) Update;
		VAR res: INTEGER;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		END;
		CheckLabel(f.label, f.i.ctrl);
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: Caption) Restore (l, t, r, b: INTEGER);
		VAR lbl: Dialog.String; style: SET;
	BEGIN
		SetLabel(f.label, lbl);
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			style := {30};
			IF f.left THEN style := style + WinApi.SS_LEFT
			ELSIF f.right THEN style := style + WinApi.SS_RIGHT
			ELSE style := style + WinApi.SS_CENTER
			END;
			Open(f, "STATIC", lbl, style, {}, f.i)	(* left, child *)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, 0, 2 * Ports.point, 0, lbl) END
	END Restore;



	(* Group *)

	PROCEDURE (f: Group) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: Group) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END
		(*f.Close^*)
	END Close;

	PROCEDURE (f: Group) Update;
		VAR res: INTEGER;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 1) END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN res := WinApi.EnableWindow(f.i.ctrl, 0) END
		END;
		CheckLabel(f.label, f.i.ctrl);
		res := WinApi.UpdateWindow(f.i.ctrl)
	END Update;

	PROCEDURE (f: Group) Restore (l, t, r, b: INTEGER);
		VAR lbl: Dialog.String;
	BEGIN
		SetLabel(f.label, lbl);
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			Open(f, "BUTTON", lbl, {0, 1, 2, 30}, {}, f.i)	(* group box, child *)
		END;
		f.Update;
		IF f.i.wnd # 0 THEN Paint(f, f.i) ELSE Print(f, -3, 2 * Ports.point, 0, lbl) END
	END Restore;


	(* TreeFrame *)

	PROCEDURE (f: TreeFrame) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
		Adapt(f, f.i)
	END SetOffset;

	PROCEDURE (f: TreeFrame) Close;
		VAR res: INTEGER;
	BEGIN
		IF f.i.wnd # 0 THEN	(* deallocate *)
			ASSERT(f.rider # NIL, 100); ASSERT(f.rider.Base() # NIL, 101);
			res := WinApi.DestroyWindow(f.i.wnd);
			f.i.wnd := 0; f.i.ctrl := 0
		END;
		IF f.himl # NIL THEN res := WinCtl.ImageList_Destroy(f.himl) END
	END Close;

	PROCEDURE (f: TreeFrame) Recreate (), NEW;
		VAR res: INTEGER; style: SET;
			icex: WinCtl.INITCOMMONCONTROLSEX; ok: WinApi.BOOL; hbmp: WinApi.HANDLE;
	BEGIN
		f.Close(); f.i.readOnly := f.readOnly;
		icex.dwSize := SIZE(WinCtl.INITCOMMONCONTROLSEX);
		icex.dwICC := WinCtl.ICC_TREEVIEW_CLASSES;
		ok := WinCtl.InitCommonControlsEx(icex);
		style := WinApi.WS_VISIBLE + WinApi.WS_CHILD + WinApi.WS_BORDER + WinCtl.TVS_SHOWSELALWAYS
				+ WinCtl.TVS_DISABLEDRAGDROP;
		IF f.haslines THEN style := style + WinCtl.TVS_HASLINES END;
		IF f.hasbuttons THEN style := style + WinCtl.TVS_HASBUTTONS END;
		IF f.atroot THEN style := style + WinCtl.TVS_LINESATROOT END;
		Open(f, WinCtl.WC_TREEVIEWW, "", style, {}, f.i);	(* 3 *)
		res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TV_FIRST + 033, 100, 0); (* TVM_SETSCROLLTIME*)
		IF f.foldericons THEN
			(* Create an imagelist and associate it with the control *)
			IF f.himl # NIL THEN ok := WinCtl.ImageList_Destroy(f.himl) END;
			f.himl := WinCtl.ImageList_Create(16, 16, {}, 3, 0);
			IF f.himl # NIL THEN
				hbmp := WinApi.LoadImageW(
					instance, "#6", WinApi.IMAGE_ICON, 16, 16, ORD(WinApi.LR_LOADTRANSPARENT));
				f.folderimg := WinCtl.ImageList_ReplaceIcon(f.himl, -1, hbmp);
				ok := WinApi.DestroyIcon(hbmp);
				hbmp := WinApi.LoadImageW(
					instance, "#7", WinApi.IMAGE_ICON, 16, 16, ORD(WinApi.LR_LOADTRANSPARENT));
				f.openimg := WinCtl.ImageList_ReplaceIcon(f.himl, -1, hbmp);
				ok := WinApi.DestroyIcon(hbmp);
				hbmp := WinApi.LoadImageW(
					instance, "#8", WinApi.IMAGE_ICON, 16, 16, ORD(WinApi.LR_LOADTRANSPARENT));
				f.leafimg := WinCtl.ImageList_ReplaceIcon(f.himl, -1, hbmp);
				ok := WinApi.DestroyIcon(hbmp);
				res := WinApi.SendMessageW(
					f.i.ctrl, WinCtl.TVM_SETIMAGELIST, WinCtl.TVSIL_NORMAL, SYSTEM.VAL(INTEGER, f.himl))
			END
		END
	END Recreate;

	PROCEDURE (f: TreeFrame) UpdateNT4, NEW;
		VAR res: INTEGER;
	BEGIN
		IF ~f.disabled THEN
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 1)
			END
		ELSE
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl);
		IF ~f.disabled & (f.readOnly # f.i.readOnly) THEN
			IF f.readOnly OR ~f.i.hasFocus THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KILLFOCUS, 0, 0)
			END;
			f.i.readOnly := f.readOnly
		END
	END UpdateNT4;

	PROCEDURE (f: TreeFrame) Update;
		VAR res: INTEGER;
	BEGIN
		IF Dialog.platform = Dialog.windowsNT4 THEN f.UpdateNT4; RETURN END;
		IF ~f.disabled & ~f.readOnly THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TV_FIRST + 029, 0, -1); (* TVM_SETBKCOLOR *)
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 1)
			END
		ELSIF ~f.disabled & f.readOnly THEN
			res := WinApi.SendMessageW(
				f.i.ctrl, WinCtl.TV_FIRST + 029, 0, Ports.dialogBackground); (* TVM_SETBKCOLOR *)
			IF WinApi.IsWindowEnabled(f.i.ctrl) = 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 1)
			END
		ELSE
			res := WinApi.SendMessageW(
				f.i.ctrl, WinCtl.TV_FIRST + 029, 0, Ports.dialogBackground); (* TVM_SETBKCOLOR *)
			IF WinApi.IsWindowEnabled(f.i.ctrl) # 0 THEN
				res := WinApi.EnableWindow(f.i.ctrl, 0)
			END
		END;
		res := WinApi.UpdateWindow(f.i.ctrl);
		IF ~f.disabled & (f.readOnly # f.i.readOnly) THEN
			IF f.readOnly THEN
				(* Selection has to be focused since it won't be visible on a gray backround otherwise *)
				res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0)
			ELSE
				IF ~f.i.hasFocus THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KILLFOCUS, 0, 0) END
			END;
			f.i.readOnly := f.readOnly
		END;
		res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0)
	END Update;

	PROCEDURE (f: TreeFrame) WinInsertItem (
		VAR tn: Dialog.TreeNode; pWndAdr: INTEGER; OUT wndAdr: INTEGER
	), NEW;
		VAR res: INTEGER; p: Dialog.TreeNode; s: Dialog.String;
			tvi: WinCtl.TVITEMW; tvins: WinCtl.TVINSERTSTRUCTW;
	BEGIN
		ASSERT(tn # NIL, 20);
		f.treeArray[f.curindex].tn := tn;
		p := f.Parent(f, tn);
		tvi.mask := WinCtl.TVIF_TEXT + WinCtl.TVIF_PARAM + WinCtl.TVIF_IMAGE + WinCtl.TVIF_SELECTEDIMAGE;
		tn.GetName(s); Dialog.MapString(s, s); ConvertFromUnicode(s);
		tvi.pszText := s; tvi.cchTextMax := LEN(s); tvi.lParam := f.curindex;
		IF tn.IsFolder() THEN
			IF tn.IsExpanded() THEN tvi.iImage := f.openimg
			ELSE tvi.iImage := f.folderimg
			END
		ELSE
			tvi.iImage := f.leafimg
		END;
		tvi.iSelectedImage := tvi.iImage; tvins.item := tvi;
		tvins.hParent := SYSTEM.VAL(WinCtl.Ptr_TREEITEM, pWndAdr);
		IF f.sorted THEN tvins.hInsertAfter := SYSTEM.VAL(WinCtl.Ptr_TREEITEM, WinCtl.TVI_SORT)
		ELSE tvins.hInsertAfter := SYSTEM.VAL(WinCtl.Ptr_TREEITEM, WinCtl.TVI_LAST)
		END;
		wndAdr := WinApi.SendMessageW(f.i.ctrl, WinCtl.TVM_INSERTITEMW, 0, SYSTEM.ADR(tvins));
		f.treeArray[f.curindex].wndAdr := wndAdr;
		IF p # NIL THEN
			IF p.IsExpanded() THEN
				res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TVM_EXPAND, WinCtl.TVE_EXPAND, pWndAdr)
			ELSE
				res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TVM_EXPAND, WinCtl.TVE_COLLAPSE, pWndAdr)
			END
		END;
		INC(f.curindex)
	END WinInsertItem;

	PROCEDURE (f: TreeFrame) InsertNode(tn: Dialog.TreeNode; pWndAdr: INTEGER), NEW;
		VAR wndAdr: INTEGER;
	BEGIN
		IF tn # NIL THEN
			f.WinInsertItem(tn, pWndAdr, wndAdr);
			f.InsertNode(f.Child(f, tn), wndAdr);
			f.InsertNode(f.Next(f, tn), pWndAdr)
		END
	END InsertNode;

	PROCEDURE (f: TreeFrame) UpdateList;
		VAR res, len, i: INTEGER; done: BOOLEAN; sel: Dialog.TreeNode;
	BEGIN
		f.inUpdateList := TRUE;
		len := 0; f.curindex := 0; sel := f.Selected(f);
		len := f.NofNodes(f); IF len > 0 THEN NEW(f.treeArray, len) END;
		(* Throw away the old windows control and create a new one *)
		f.Recreate();
		(* Go through the tree and insert new nodes into the treeview *)
		f.InsertNode(f.Child(f, NIL), WinCtl.TVI_ROOT);
		(* Select the node that is selected *)
		IF sel = NIL THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TVM_SELECTITEM, WinCtl.TVGN_CARET, 0)
		ELSE
			i := 0; done := FALSE;
			WHILE (i < len) & (~done) DO
				IF sel = f.treeArray[i].tn THEN
					res := WinApi.SendMessageW(
						f.i.ctrl, WinCtl.TVM_SELECTITEM, WinCtl.TVGN_CARET, f.treeArray[i].wndAdr);
					done := TRUE
				END;
				INC(i)
			END
		END;
		f.Select(f, sel);
		f.Update;
		IF f.i.hasFocus THEN res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_SETFOCUS, 0, 0) END;
		f.inUpdateList := FALSE
	END UpdateList;

	PROCEDURE (f: TreeFrame) ExpandCollapse(index, action: INTEGER), NEW;
		VAR res: INTEGER; tn: Dialog.TreeNode; tvi: WinCtl.TVITEMW;
	BEGIN
		tn := f.treeArray[index].tn;
		IF action = WinCtl.TVE_COLLAPSE THEN
			f.SetExpansion(f, tn, FALSE)
		ELSIF action = WinCtl.TVE_EXPAND THEN
			f.SetExpansion(f, tn, TRUE)
		ELSIF action = WinCtl.TVE_TOGGLE THEN
			f.SetExpansion(f, tn, ~tn.IsExpanded())
		END;
		tvi.mask := WinCtl.TVIF_IMAGE + WinCtl.TVIF_SELECTEDIMAGE;
		tvi.hItem := SYSTEM.VAL(WinCtl.Ptr_TREEITEM, f.treeArray[index].wndAdr);
		IF tn.IsExpanded() THEN
			tvi.iImage := f.openimg; tvi.iSelectedImage := f.openimg
		ELSE
			tvi.iImage := f.folderimg; tvi.iSelectedImage := f.folderimg
		END;
		res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TVM_SETITEMW, 0, SYSTEM.ADR(tvi))
	END ExpandCollapse;

	PROCEDURE (f: TreeFrame) DblClickOk (x, y: INTEGER): BOOLEAN;
		VAR res: INTEGER; hinf: WinCtl.TVHITTESTINFO;
	BEGIN
		hinf.pt.x := x DIV f.unit; hinf.pt.y := y DIV f.unit;
		res := WinApi.SendMessageW(f.i.ctrl, WinCtl.TVM_HITTEST, 0, SYSTEM.ADR(hinf));
		IF hinf.hItem = NIL THEN
			RETURN FALSE
		ELSE
			RETURN SYSTEM.VAL(INTEGER, hinf.hItem) = f.treeArray[f.selIndex].wndAdr
		END
	END DblClickOk;

	PROCEDURE (f: TreeFrame) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i.wnd, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;

	PROCEDURE (f: TreeFrame) WheelMove (x, y: INTEGER; op, nofLines: INTEGER; VAR done: BOOLEAN);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleWheel(f.i.ctrl, x, y, op, nofLines, done)
		END
	END WheelMove;

	PROCEDURE (f: TreeFrame) Restore (l, t, r, b: INTEGER);
	BEGIN
		IF f.i.ctrl = 0 THEN	(* lazy allocation *)
			f.UpdateList
		ELSE
			f.Update
		END;
		IF f.i.wnd # 0 THEN PaintRect(f, f.i, l, t, r, b) ELSE Print(f, f.dot, -1, -1, "") END
	END Restore;

	PROCEDURE (f: TreeFrame) KeyDown (ch: CHAR);
		VAR res: INTEGER;
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " " THEN
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYDOWN, ORD(ch), 0);
			res := WinApi.SendMessageW(f.i.ctrl, WinApi.WM_KEYUP, ORD(ch), 0)
		ELSE
			SendKey(ch, f.i.ctrl)
		END
	END KeyDown;

	PROCEDURE (f: TreeFrame) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i)
	END Mark;

	PROCEDURE (f: TreeFrame) GetSize (OUT w, h: INTEGER);
		VAR rect: WinApi.RECT; res: INTEGER;
	BEGIN
		res := WinApi.GetWindowRect(f.i.wnd, rect);
		w := (rect.right - rect.left) * f.unit; h := (rect.bottom - rect.top) * f.unit
	END GetSize;


	(* Directory *)

	PROCEDURE (d: Directory) GetPushButtonSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 56 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetPushButtonSize;

	PROCEDURE (d: Directory) GetCheckBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 60 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetCheckBoxSize;

	PROCEDURE (d: Directory) GetRadioButtonSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 60 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetRadioButtonSize;

	PROCEDURE (d: Directory) GetScrollBarSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 120 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetScrollBarSize;

	PROCEDURE (d: Directory) GetFieldSize (max: INTEGER; VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN
			IF max = 0 THEN w := 80 * Ports.point
			ELSIF max < 10 THEN w := 32 * Ports.point
			ELSIF max < 15 THEN w := 56 * Ports.point
			ELSIF max < 30 THEN w := 80 * Ports.point
			ELSIF max < 100 THEN w := 120 * Ports.point
			ELSE w := 150 * Ports.point
			END
		END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetFieldSize;

	PROCEDURE (d: Directory) GetUpDownFieldSize (max: INTEGER; VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 56 * Ports.point END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetUpDownFieldSize;

	PROCEDURE (d: Directory) GetDateFieldSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 72 * Ports.point END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetDateFieldSize;

	PROCEDURE (d: Directory) GetTimeFieldSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 72 * Ports.point END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetTimeFieldSize;

	PROCEDURE (d: Directory) GetColorFieldSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 36 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetColorFieldSize;

	PROCEDURE (d: Directory) GetListBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetListBoxSize;

	PROCEDURE (d: Directory) GetSelectionBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 54 * Ports.point END
	END GetSelectionBoxSize;

	PROCEDURE (d: Directory) GetComboBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetComboBoxSize;

	PROCEDURE (d: Directory) GetCaptionSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 50 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetCaptionSize;

	PROCEDURE (d: Directory) GetGroupSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 100 * Ports.point END
	END GetGroupSize;

	PROCEDURE (d: Directory) GetTreeFrameSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 100 * Ports.point END
	END GetTreeFrameSize;

	PROCEDURE (d: Directory) NewPushButton (): StdCFrames.PushButton;
		VAR f: PushButton;
	BEGIN
		NEW(f); RETURN f
	END NewPushButton;

	PROCEDURE (d: Directory) NewCheckBox (): StdCFrames.CheckBox;
		VAR f: CheckBox;
	BEGIN
		NEW(f); RETURN f
	END NewCheckBox;

	PROCEDURE (d: Directory) NewRadioButton (): StdCFrames.RadioButton;
		VAR f: RadioButton;
	BEGIN
		NEW(f); RETURN f
	END NewRadioButton;

	PROCEDURE (d: Directory) NewScrollBar (): StdCFrames.ScrollBar;
		VAR f: ScrollBar;
	BEGIN
		NEW(f); RETURN f
	END NewScrollBar;

	PROCEDURE (d: Directory) NewField (): StdCFrames.Field;
		VAR f: Field;
	BEGIN
		NEW(f); RETURN f
	END NewField;

	PROCEDURE (d: Directory) NewUpDownField (): StdCFrames.UpDownField;
		VAR f: UpDownField;
	BEGIN
		NEW(f); RETURN f
	END NewUpDownField;

	PROCEDURE (d: Directory) NewDateField (): StdCFrames.DateField;
		VAR f: DateField;
	BEGIN
		NEW(f); RETURN f
	END NewDateField;

	PROCEDURE (d: Directory) NewTimeField (): StdCFrames.TimeField;
		VAR f: TimeField;
	BEGIN
		NEW(f); RETURN f
	END NewTimeField;

	PROCEDURE (d: Directory) NewColorField (): StdCFrames.ColorField;
		VAR f: ColorField;
	BEGIN
		NEW(f); RETURN f
	END NewColorField;

	PROCEDURE (d: Directory) NewListBox (): StdCFrames.ListBox;
		VAR f: ListBox;
	BEGIN
		NEW(f); RETURN f
	END NewListBox;

	PROCEDURE (d: Directory) NewSelectionBox (): StdCFrames.SelectionBox;
		VAR f: SelectionBox;
	BEGIN
		NEW(f); RETURN f
	END NewSelectionBox;

	PROCEDURE (d: Directory) NewComboBox (): StdCFrames.ComboBox;
		VAR f: ComboBox;
	BEGIN
		NEW(f); RETURN f
	END NewComboBox;

	PROCEDURE (d: Directory) NewCaption (): StdCFrames.Caption;
		VAR f: Caption;
	BEGIN
		NEW(f); RETURN f
	END NewCaption;

	PROCEDURE (d: Directory) NewGroup (): StdCFrames.Group;
		VAR f: Group;
	BEGIN
		NEW(f); RETURN f
	END NewGroup;

	PROCEDURE (d: Directory) NewTreeFrame (): StdCFrames.TreeFrame;
		VAR f: TreeFrame;
	BEGIN
		NEW(f); f.inUpdateList := FALSE; RETURN f
	END NewTreeFrame;

	(* control window class *)

	(* Used for common controls, not standard controls *)
	PROCEDURE HandleNotifyMsg (wnd: WinApi.HANDLE; lParam, out: INTEGER);
		VAR
			c: StdCFrames.Frame; ret: INTEGER;
			pnmhdr: WinApi.PtrNMHDR; pnmtv: WinCtl.PtrNMTREEVIEWW;
	BEGIN
		ret := WinApi.FALSE;
		c := SYSTEM.VAL(StdCFrames.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
		pnmhdr := SYSTEM.VAL(WinApi.PtrNMHDR, lParam);
		WITH c: TreeFrame DO
			IF c.i.ctrl # 0 THEN
				IF ~c.inUpdateList & c.readOnly & (pnmhdr.code = WinCtl.TVN_SELCHANGINGW) THEN
					ret := WinApi.TRUE
				ELSIF pnmhdr.code = WinCtl.TVN_SELCHANGEDW THEN
					pnmtv := SYSTEM.VAL(WinCtl.PtrNMTREEVIEWW, lParam);
					c.selIndex := pnmtv.itemNew.lParam;
					c.Select(c, c.treeArray[c.selIndex].tn)
				ELSIF pnmhdr.code = WinCtl.TVN_ITEMEXPANDEDW THEN
					pnmtv := SYSTEM.VAL(WinCtl.PtrNMTREEVIEWW, lParam);
					c.ExpandCollapse(pnmtv.itemNew.lParam, pnmtv.action)
				END
			END
		ELSE
		END;
		SYSTEM.PUT(out, ret)
	END HandleNotifyMsg;

	PROCEDURE HandleCommand (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR res, nc, i, j: INTEGER; c: StdCFrames.Frame; s: ARRAY 512 OF CHAR;
			ps: POINTER TO ARRAY OF CHAR; b: BOOLEAN;
	BEGIN
		c := SYSTEM.VAL(StdCFrames.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
		nc := wParam DIV 65536;
		WITH c: PushButton DO
			IF c.i.ctrl # 0 THEN
				IF nc = WinApi.BN_CLICKED THEN Execute(c)
				END
			END
		| c: CheckBox DO
			IF c.i.ctrl # 0 THEN
				IF nc = WinApi.BN_CLICKED THEN
					i := WinApi.SendMessageW(c.i.ctrl, WinApi.BM_GETCHECK, 0, 0);
					IF i = 0 THEN i := 1 ELSE i := 0 END;
					res := WinApi.SendMessageW(c.i.ctrl, WinApi.BM_SETCHECK, i, 0);
					c.undef := FALSE; c.Set(c, i = 1)
(*
					c.undef := res = 2;
					c.Set(c, res = 1)
*)
				END
			END
		| c: RadioButton DO
			IF c.i.ctrl # 0 THEN
				IF nc = WinApi.BN_CLICKED THEN
					res := WinApi.SendMessageW(c.i.ctrl, WinApi.BM_GETCHECK, 0, 0);
					c.Set(c, res # 0)
				END
			END
		| c: Field DO
			IF c.i.ctrl # 0 THEN
				IF (nc = WinApi.EN_CHANGE) & ~c.isUpdate THEN
					IF ~c.left THEN (* right center alignment implies multiline which sends two update messages *)
						IF c.maxLen > 255 THEN
							NEW(ps, 2 * c.maxLen + 1);
							res := WinApi.GetWindowTextW(c.i.ctrl, ps^, LEN(ps^));
							IF c.multiLine THEN DelLF(ps^) END;
							ConvertToUnicode(ps^);
							IF c.del OR (ps$ # "") THEN c.Set(c, ps^) END
						ELSE
							res := WinApi.GetWindowTextW(c.i.ctrl, s, LEN(s));
							IF c.multiLine THEN DelLF(s) END;
							ConvertToUnicode(s);
							IF c.del OR (s$ # "") THEN c.Set(c, s) END
						END
					ELSE
						IF c.maxLen > 255 THEN
							NEW(ps, 2 * c.maxLen + 1);
							res := WinApi.GetWindowTextW(c.i.ctrl, ps^, LEN(ps^));
							IF c.multiLine THEN DelLF(ps^) END;
							ConvertToUnicode(ps^);
							c.Set(c, ps^)
						ELSE
							res := WinApi.GetWindowTextW(c.i.ctrl, s, LEN(s));
							IF c.multiLine THEN DelLF(s) END;
							ConvertToUnicode(s);
							c.Set(c, s)
						END
					END
				END
			END
		| c: UpDownField DO
			IF c.i.ctrl # 0 THEN
				IF (nc = WinApi.EN_CHANGE) & ~c.isUpdate THEN
					res := WinApi.GetWindowTextW(c.i.ctrl, s, LEN(s));
					Strings.StringToInt(s, c.val, res);
					c.Set(c, c.val)
				END
			END
		| c: ColorField DO
			IF c.i.ctrl # 0 THEN
				IF nc = WinApi.CBN_SELCHANGE THEN
					i := WinApi.SendMessageW(c.i.ctrl, WinApi.CB_GETCURSEL, 0, 0);
					IF i = numColors THEN
						Dialog.GetColor(c.color, i, b);
						IF b THEN c.Set(c, i); c.Update END
					ELSIF i >= 0 THEN
						c.color := colors[i]; c.Set(c, c.color)
					END
				END
			END
		| c: ListBox DO
			IF c.i.ctrl # 0 THEN
				IF c.i.dropDown THEN
					IF nc = WinApi.CBN_SELCHANGE THEN
						i := WinApi.SendMessageW(c.i.ctrl, WinApi.CB_GETCURSEL, 0, 0);
						IF c.sorted THEN i := WinApi.SendMessageW(c.i.ctrl, WinApi.CB_GETITEMDATA, i, 0) END;
						IF i >= 0 THEN c.Set(c, i) END
					END
				ELSE
					IF nc = WinApi.LBN_SELCHANGE THEN
						i := WinApi.SendMessageW(c.i.ctrl, WinApi.LB_GETCURSEL, 0, 0);
						IF c.sorted THEN i := WinApi.SendMessageW(c.i.ctrl, WinApi.LB_GETITEMDATA, i, 0) END;
						IF i >= 0 THEN c.Set(c, i) END
					END
				END
			END
		| c: SelectionBox DO
			IF c.i.ctrl # 0 THEN
				IF nc = WinApi.LBN_SELCHANGE THEN
(*
					i := WinApi.SendMessageW(c.i.ctrl, WinApi.LBGetAnchorIndex, 0, 0);
					j := WinApi.SendMessageW(c.i.ctrl, WinApi.LBGetCaretIndex, 0, 0);
					res := WinApi.SendMessageW(c.i.ctrl, WinApi.LBGetSel, i, 0);
					IF c.sorted THEN
						i := WinApi.SendMessageW(c.i.ctrl, WinApi.LBGetItemData, i, 0);
						j := WinApi.SendMessageW(c.i.ctrl, WinApi.LBGetItemData, j, 0)
					END;
					IF (i >= 0) & (i < c.num) THEN
						Log.Int(i); Log.Int(j); Log.Ln; Log.Int(wParam); Log.Int(lParam); Log.Int(res); Log.Ln;
						IF (res # 0) THEN c.Incl(c, i, i) ELSE c.Excl(c, i, i) END
					END
*)
					i := 0;
					WHILE i < c.num DO
						j := i;
						res := WinApi.SendMessageW(c.i.ctrl, WinApi.LB_GETSEL, j, 0);
						IF c.sorted THEN j := WinApi.SendMessageW(c.i.ctrl, WinApi.LB_GETITEMDATA, j, 0) END;
						c.Get(c, j, b);
						IF (res # 0) & ~b THEN c.Incl(c, j, j)
						ELSIF (res = 0) & b THEN c.Excl(c, j, j)
						END;
						INC(i)
					END
				END
			END
		| c: ComboBox DO
			IF c.i.ctrl # 0 THEN
				IF nc = WinApi.CBN_EDITCHANGE THEN
					res := WinApi.GetWindowTextW(c.i.ctrl, s, LEN(s));
					c.Set(c, s)
				ELSIF nc = WinApi.CBN_SELCHANGE THEN
					i := WinApi.SendMessageW(c.i.ctrl, WinApi.CB_GETCURSEL, 0, 0);
					IF i >= 0 THEN
						res := WinApi.SendMessageW(c.i.ctrl, WinApi.CB_GETLBTEXT, i, SYSTEM.ADR(s));
						c.Set(c, s)
					END
				END
			END
		ELSE
		END
	END HandleCommand;

	PROCEDURE HandleScroll (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR nc, size, sect, pos: INTEGER; c: StdCFrames.Frame; arrows: BOOLEAN;
	BEGIN
		arrows := FALSE;
		c := SYSTEM.VAL(StdCFrames.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
		WITH c: ScrollBar DO
			IF c.i.ctrl # 0 THEN
				Views.ValidateRoot(Views.RootOf(c));
				nc := wParam MOD 65536; c.Get(c, size, sect, pos);
				IF (nc = WinApi.SB_THUMBPOSITION) OR
					HostWindows.visualScroll & (nc = WinApi.SB_THUMBTRACK) THEN
						c.Set(c, WinApi.MulDiv(wParam DIV 65536, size - sect, scrollRange))
				ELSIF nc = WinApi.SB_LINEUP THEN c.Track(c, StdCFrames.lineUp, pos)
				ELSIF nc = WinApi.SB_LINEDOWN THEN c.Track(c, StdCFrames.lineDown, pos)
				ELSIF nc = WinApi.SB_PAGEUP THEN c.Track(c, StdCFrames.pageUp, pos)
				ELSIF nc = WinApi.SB_PAGEDOWN THEN c.Track(c, StdCFrames.pageDown, pos)
				END;
				c.Update
			END
		| c: UpDownField DO
			IF c.i.ctrl # 0 THEN arrows := TRUE END
		| c: TimeField DO
			IF c.i.ctrl # 0 THEN arrows := TRUE END
		| c: DateField DO
			IF c.i.ctrl # 0 THEN arrows := TRUE END
		ELSE
		END;
		IF arrows THEN
			nc := wParam MOD 65536;
			IF nc = WinApi.SB_THUMBPOSITION THEN
				IF wParam DIV 65536 # 0 THEN c.KeyDown(AU)
				ELSE c.KeyDown(AD)
				END
			END
		END
	END HandleScroll;

	PROCEDURE HandleDraw (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR res: INTEGER; c: StdCFrames.Frame; p: WinApi.PtrDRAWITEMSTRUCT;
			brush: WinApi.HANDLE; rect: WinApi.RECT; col: Ports.Color;
	BEGIN
		c := SYSTEM.VAL(StdCFrames.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
		p := SYSTEM.VAL(WinApi.PtrDRAWITEMSTRUCT, lParam);
		WITH c: ColorField DO
			IF 0 IN p.itemState THEN
				rect := p.rcItem;
				brush := WinApi.CreateSolidBrush(HostPorts.selBackground);
				res := WinApi.FillRect(p.hDC, rect, brush);
				res := WinApi.DeleteObject(brush);
				INC(rect.left); INC(rect.top); DEC(rect.right); DEC(rect.bottom);
				brush := WinApi.CreateSolidBrush(Ports.background);
				res := WinApi.FillRect(p.hDC, rect, brush);
				res := WinApi.DeleteObject(brush)
			ELSE
				IF c.disabled OR c.readOnly THEN
					brush := WinApi.CreateSolidBrush(Ports.dialogBackground)
				ELSE
					brush := WinApi.CreateSolidBrush(Ports.background)
				END;
				res := WinApi.FillRect(p.hDC, p.rcItem, brush);
				res := WinApi.DeleteObject(brush)
			END;
			IF (p.itemID = numColors) & (p.rcItem.top > 20) THEN
				brush := WinApi.CreateSolidBrush(HostPorts.textCol);
				rect.top := p.rcItem.bottom - 6; rect.bottom := rect.top + 2;
				rect.left := p.rcItem.left + 4; rect.right := rect.left + 2;
				res := WinApi.FillRect(p.hDC, rect, brush);
				rect.left := p.rcItem.left + 8; rect.right := rect.left + 2;
				res := WinApi.FillRect(p.hDC, rect, brush);
				rect.left := p.rcItem.left + 12; rect.right := rect.left + 2;
				res := WinApi.FillRect(p.hDC, rect, brush)
			ELSIF p.itemID >= 0 THEN
				rect := p.rcItem; INC(rect.left, 2); INC(rect.top, 2); DEC(rect.right, 2); DEC(rect.bottom, 2);
				IF p.itemID = numColors THEN col := c.color
				ELSIF (p.itemID >= 0) & (p.itemID < LEN(colors)) THEN col := colors[p.itemID]
				ELSE col := HostPorts.textCol
				END;
				brush := WinApi.CreateSolidBrush(col);
				res := WinApi.FillRect(p.hDC, rect, brush);
				res := WinApi.DeleteObject(brush)
			END
		END
	END HandleDraw;

	PROCEDURE ColorBrush (col: Ports.Color): WinApi.HANDLE;
		VAR p: BrushCache;
	BEGIN
		IF col = Ports.white THEN RETURN WinApi.GetStockObject(WinApi.WHITE_BRUSH)
		ELSIF col = Ports.dialogBackground THEN RETURN HostPorts.dialogBrush
		ELSE
			p := brushes;
			WHILE (p # NIL) & (p.col # col) DO p := p.next END;
			IF p # NIL THEN RETURN p.brush
			ELSE
				NEW(p); p.col := col; p.brush := WinApi.CreateSolidBrush(col); p.next := brushes; brushes := p;
				RETURN p.brush
			END
		END
	END ColorBrush;

	PROCEDURE [2] CtrlHandler (wnd: WinApi.HANDLE; message, wParam, lParam: INTEGER): INTEGER;
		VAR res: INTEGER; c: StdCFrames.Frame; p: WinApi.PtrMEASUREITEMSTRUCT;
	BEGIN
		IF message = WinApi.WM_COMMAND THEN
			Kernel.Try(HandleCommand, wnd, wParam, lParam);
			RETURN 0
		ELSIF message = WinApi.WM_NOTIFY THEN
			res := -1;
			Kernel.Try(HandleNotifyMsg, wnd, lParam, SYSTEM.ADR(res));
			IF res # -1 THEN RETURN res ELSE RETURN 0 END
		ELSIF (message = WinApi.WM_HSCROLL) OR (message = WinApi.WM_VSCROLL) THEN
			Kernel.Try(HandleScroll, wnd, wParam, lParam);
			RETURN 0
		ELSIF message = WinApi.WM_DRAWITEM THEN
			Kernel.Try(HandleDraw, wnd, wParam, lParam);
			RETURN 1
		ELSIF message = WinApi.WM_MEASUREITEM THEN
			c := SYSTEM.VAL(StdCFrames.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
			WITH c: ColorField DO
				p := SYSTEM.VAL(WinApi.PtrMEASUREITEMSTRUCT, lParam);
				IF p.itemID = -1 THEN p.itemHeight := 11 * Ports.point DIV c.unit + 1
				ELSE p.itemHeight := 11 * Ports.point DIV c.unit - 1
				END;
				RETURN 1
			ELSE
			END
		ELSIF (message >= WinApi.WM_CTLCOLORMSGBOX) & (message <= WinApi.WM_CTLCOLORSTATIC) THEN
			c := SYSTEM.VAL(StdCFrames.Frame, WinApi.GetWindowLongW(wnd, dlgWindowExtra));
			IF ((c IS Field) OR (c IS UpDownField) OR (c IS TimeField) OR (c IS DateField)
					OR (c IS ListBox) OR (c IS SelectionBox) OR (c IS ColorField))
					& (c.disabled OR c.readOnly) THEN
				res := WinApi.SetBkColor(wParam, Ports.dialogBackground);
				res := WinApi.SetBkMode(wParam, 2);	(* opaque *)
				RETURN HostPorts.dialogBrush
			ELSIF (c IS TreeFrame) & (c.disabled OR c.readOnly) & (Dialog.platform # Dialog.windowsNT4) THEN
				res := WinApi.SetBkColor(wParam, Ports.dialogBackground);
				res := WinApi.SetBkMode(wParam, 2);	(* opaque *)
				RETURN HostPorts.dialogBrush
			ELSIF ((c IS Field) OR (c IS UpDownField) OR (c IS TimeField) OR (c IS DateField))
					& ~c.disabled & c.undef THEN
				res := WinApi.SetBkMode(wParam, 1); (* transparent *)
				res := WinApi.SetTextColor(wParam, 0C0C0C0H);
				RETURN HostPorts.dim50Brush
			ELSIF c IS Caption THEN
				res := WinApi.SetTextColor(wParam, HostPorts.dialogTextCol);
				res := WinApi.SetBkColor(wParam, c(Caption).i.bkgnd);
				res := WinApi.SetBkMode(wParam, 2);	(* opaque *)
				RETURN ColorBrush(c(Caption).i.bkgnd)
			ELSIF CtlColor # NIL THEN
				res := CtlColor(message, wParam, lParam);
				IF res # 0 THEN RETURN res END
			ELSIF c IS CheckBox THEN
				res := WinApi.SetTextColor(wParam, HostPorts.dialogTextCol);
				res := WinApi.SetBkColor(wParam, c(CheckBox).i.bkgnd);
				res := WinApi.SetBkMode(wParam, 2);	(* opaque *)
				RETURN ColorBrush(c(CheckBox).i.bkgnd)
			ELSIF c IS RadioButton THEN
				res := WinApi.SetTextColor(wParam, HostPorts.dialogTextCol);
				res := WinApi.SetBkColor(wParam, c(RadioButton).i.bkgnd);
				res := WinApi.SetBkMode(wParam, 2);	(* opaque *)
				RETURN ColorBrush(c(RadioButton).i.bkgnd)
			ELSIF c IS Group THEN
				res := WinApi.SetTextColor(wParam, HostPorts.dialogTextCol);
				res := WinApi.SetBkColor(wParam, c(Group).i.bkgnd);
				res := WinApi.SetBkMode(wParam, 2);	(* opaque *)
				RETURN ColorBrush(c(Group).i.bkgnd)
			END
		ELSIF message = WinApi.WM_ERASEBKGND THEN
			RETURN 1
		END;
		RETURN WinApi.DefWindowProcW(wnd, message, wParam, lParam)
	END CtrlHandler;


	PROCEDURE SetDefFonts*;
	BEGIN
		StdCFrames.defaultFont := HostFonts.dlgFont;
		StdCFrames.defaultLightFont := Fonts.dir.This(
			HostFonts.dlgFont.typeface, HostFonts.dlgFont.size, HostFonts.dlgFont.style, Fonts.normal)
	END SetDefFonts;

	PROCEDURE Install;
		VAR dir: Directory;
	BEGIN
		NEW(dir); StdCFrames.SetDir(dir)
	END Install;

	PROCEDURE InitClass;
		VAR res: INTEGER; class: WinApi.WNDCLASSW;
	BEGIN
		class.hCursor := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_ARROW));
		class.hIcon := WinApi.LoadIconW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 1));
		class.lpszMenuName := NIL;
		class.lpszClassName := "Oberon Ctrl";
		class.hbrBackground := 0;	(* no background *)
		class.style := {};
		class.hInstance := instance;
		class.lpfnWndProc := CtrlHandler;
		class.cbClsExtra := 0;
		class.cbWndExtra := dlgWindowExtra + 4;
		res := WinApi.RegisterClassW(class)
	END InitClass;

	PROCEDURE InitNationalInfo;
		VAR res: INTEGER; str: ARRAY 8 OF CHAR;
	BEGIN
		res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_SDATE, str, LEN(str));
		dateSep := str[0];
		res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_STIME, str, LEN(str));
		timeSep := str[0];
		res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_IDATE, str, LEN(str));
		IF str = "1" THEN dayPart := 1; monthPart := 2; yearPart := 3; del1 := 2; del2 := 5
		ELSIF str = "2" THEN yearPart := 1; monthPart := 2; dayPart := 3; del1 := 4; del2 := 7
		ELSE monthPart := 1; dayPart := 2; yearPart := 3; del1 := 2; del2 := 5
		END;
		res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_ITIME, str, LEN(str));
		IF str = "1" THEN
			lastPart := 3
		ELSE
			lastPart := 4;
			res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_S1159, str, LEN(str));
			desig[0] := str$;
			res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_S2359, str, LEN(str));
			desig[1] := str$
		END
	END InitNationalInfo;

	PROCEDURE InitColors;
	BEGIN
		colors[0] := 000000FFH;
		colors[1] := 0000FF00H;
		colors[2] := 00FF0000H;
		colors[3] := 0000FFFFH;
		colors[4] := 00FFFF00H;
		colors[5] := 00FF00FFH;
		colors[6] := 00000000H
	END InitColors;

	PROCEDURE Init;
	BEGIN
		StdCFrames.setFocus := TRUE;
		SetDefFonts;
		scW := WinApi.GetSystemMetrics(0);	(* screen width *)
		scH := WinApi.GetSystemMetrics(1);	(* screen height *)
		instance := WinApi.GetModuleHandleW(NIL);
		InitClass;
		InitNationalInfo;
		InitColors;
		Install
	END Init;

BEGIN
	Init
END HostCFrames.
