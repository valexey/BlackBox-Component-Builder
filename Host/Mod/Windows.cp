MODULE HostWindows;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Alexander Iljin, Josef Templ"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT Log,
		SYSTEM, WinApi, Services,
		Kernel, Meta, Files, HostFiles, Ports, HostRegistry, HostPorts, Properties,
		Views, Controllers, Containers, Dialog, Converters, Documents, Windows,
		HostMechanisms (* don't remove *);

	CONST
		inPlace* = 31;	(* flag for in place windows *)
		untitledKey = "#System:untitled";
		allocKey = "#Host:AllocatedMemory";
		totalKey = "#Host:Total";
		byteKey = "#Host:Bytes";
		iClose = 100;	(* known in HostMenus *)
		scrollRange = 16384;
		borderW = 5 * Ports.point;
		guardCheck = 4;
		useSeparators = TRUE; noSeparators = FALSE; 	(* parameters to be used with AppendInt *)

		ENTER = 0DX; ESC = 1BX;
		TAB = 09X; LTAB = 0AX; RDEL = 07X; LDEL = 08X;
		PL = 10X; PR = 11X; PU = 12X; PD = 13X;
		DL = 14X; DR = 15X; DU = 16X; DD = 17X;
		AL = 1CX; AR = 1DX; AU = 1EX; AD = 1FX;

		debug = FALSE;

	TYPE
		Hook* = POINTER TO ABSTRACT RECORD END;

		Window* = POINTER TO EXTENSIBLE RECORD (Windows.Window)
			hook*: Hook;
			wnd-: WinApi.HANDLE;
			child-: BOOLEAN;	(* if window is a child of the application window *)
			dlg: BOOLEAN;	(* if window has a 3d dialog border *)
			fix: BOOLEAN;	(* window is opened with fix coordinates *)
			next: Window;	(* window ring, to prevent garbage collection of windows *)
			trapped: BOOLEAN;	(* if window caused trap, it won't get idle messages anymore *)
			setup: BOOLEAN;
			dirty: BOOLEAN;	(* dirty mark shown *)
			used: BOOLEAN;	(* window received at least on message *)
			dw, dh: INTEGER;	(* window size correction for tools *)
			mw, mh: INTEGER;	(* max window size *)
			destroyed: BOOLEAN;
			minimized: BOOLEAN;
			wheelPos: INTEGER;
			title: Views.Title
		END;

		Directory* = POINTER TO EXTENSIBLE RECORD (Windows.Directory)
			unit*: INTEGER;
			invisible*: BOOLEAN;
			unmoveable*: BOOLEAN;
			background*: BOOLEAN
		END;

		ScrollInfo* = RECORD [1]
			size*: INTEGER;
			mask*: SET;
			min*, max*, page*, pos*: INTEGER;
			trackPos*: INTEGER
		END;


	VAR
		visualScroll*: BOOLEAN;
		memInStatus*: BOOLEAN;
		noAppWin*: BOOLEAN; (* If true only tool windows are shown *)
		noClientScroll*: BOOLEAN; (* If true the client window of the application window doesn't display scroll bars*)
		fullSize*: BOOLEAN; (* If true the client window of the application window doesn't display scroll bars*)
		dir-: Directory;
		main-, client-, status-: WinApi.HANDLE;	(* main windows *)
		unit-: INTEGER;	(* resolution of main window *)
		ctl3d-: WinApi.HANDLE;
		scW-, scH-: INTEGER;	(* screen width and height *)
		mainW-, mainH-: INTEGER;	(* main window client area size *)
		mainHook*: Hook;

		tWindow, fWindow: Window;	(* target and front focus windows *)
		aWindow: Window;	(* activated child window *)
		newNumber: INTEGER;	(* number for next untitled document *)
		winAnchor: Window;	(* list of all windows, from top to bottom, first is dumy header *)
		bgWindow: Window; 	(* current background window, if any *)
		whiteBrush, nullPen: WinApi.HANDLE;
		docIcon, dirtyIcon: WinApi.HANDLE;
		instance: WinApi.HANDLE;	(* application instance *)
		cbViewer: Window;	(* pseudo window for clipboard drawing *)
		cbValid: BOOLEAN;	(* clipboard contents is valid *)
		mainActive: BOOLEAN;	(* main is active window *)
		activating: BOOLEAN;	(* used for mouse window activation *)
		actMod: Kernel.Module;

		font, info: WinApi.HANDLE;
		statusH, alloc, total: INTEGER;
		allocStr, totalStr, byteStr: ARRAY 256 OF CHAR;
		idleTraped: BOOLEAN;
		showState: INTEGER;	(* show command for main window *)

		lBorder, tBorder, rBorder, bBorder: INTEGER;	(* space for tools in main window *)

		hookDocWinHandler*: PROCEDURE (
			wnd: WinApi.HANDLE; message, wParam, lParam: INTEGER; VAR res: INTEGER): BOOLEAN;
			(* hook for handling application specific messages; returns TRUE iff handled *)

	PROCEDURE ScrollModPressed (): BOOLEAN;
	BEGIN
		RETURN WinApi.GetAsyncKeyState(11H) < 0
	END ScrollModPressed;

	(* auxiliary portable prcedures *)

	PROCEDURE GetSection (w: Window; focus, vertical: BOOLEAN;
								VAR size, sect, pos: INTEGER; VAR valid: BOOLEAN);
		VAR msg: Controllers.PollSectionMsg;
	BEGIN	(* portable *)
		msg.focus := focus; msg.vertical := vertical;
		msg.wholeSize := 1; msg.partSize := 0; msg.partPos := 0;
		msg.valid := FALSE; msg.done := FALSE;
		w.ForwardCtrlMsg(msg);
		IF msg.done THEN
			size := msg.wholeSize; sect := msg.partSize; pos := msg.partPos;
			IF size < 0 THEN size := 0 END;
			IF sect < 0 THEN sect := 0 ELSIF sect > size THEN sect := size END;
			IF pos > size - sect THEN pos := size - sect END;
			IF pos < 0 THEN pos := 0 END
		ELSE size := 1; sect := 0; pos := 0
		END;
		valid := msg.valid
	END GetSection;

	PROCEDURE SetOrigin (w: Window; focus, vertical: BOOLEAN; pos: INTEGER);
	(* set origin of window's view *)
		VAR msg: Controllers.ScrollMsg;
	BEGIN	(* portable *)
		msg.focus := focus; msg.vertical := vertical;
		msg.op := Controllers.gotoPos; msg.pos := pos;
		msg.done := FALSE;
		w.ForwardCtrlMsg(msg)
	END SetOrigin;

	PROCEDURE Scroll (w: Window; focus, vertical: BOOLEAN; dir: INTEGER);
	(* scroll relative, by line or page increment or decrement *)
		VAR msg: Controllers.ScrollMsg; c: Containers.Controller; v: Views.View;
	BEGIN	(* portable *)
		c := w.doc.ThisController(); v := c.ThisFocus();
		IF (v # NIL) & (v IS Containers.View) THEN
			Containers.FadeMarks(v(Containers.View).ThisController(), FALSE)
		END;
		msg.focus := focus; msg.vertical := vertical;
		msg.op := dir;
		msg.done := FALSE;
		w.ForwardCtrlMsg(msg)
	END Scroll;


	(** miscellaneous procedures **)

	PROCEDURE ActualWnd* (): WinApi.HANDLE;
	BEGIN
(*
		IF (fWindow # NIL) & ~fWindow.child & fWindow.used THEN RETURN fWindow.wnd
		ELSE RETURN main
		END
*)
		IF (fWindow # NIL) & ~fWindow.child & fWindow.used THEN RETURN fWindow.wnd
		ELSIF WinApi.IsWindowVisible(main) # 0 THEN RETURN main
		ELSE RETURN 0
		END
	END ActualWnd;

	PROCEDURE ThisWindow (wnd: WinApi.HANDLE): Window;
	(* determine window by its WindowPtr *)
	BEGIN
		IF wnd = cbViewer.wnd THEN RETURN cbViewer
		ELSE RETURN SYSTEM.VAL(Window, WinApi.GetWindowLongW(wnd, 0))
		END
	END ThisWindow;

	PROCEDURE AppendInt (VAR s: ARRAY OF CHAR; n: INTEGER; useSeparators: BOOLEAN);
		VAR len: INTEGER; i, j: INTEGER; d: ARRAY 32 OF CHAR;
	BEGIN
		ASSERT(n >= 0, 20);
		i := 0; REPEAT
			d[i] := CHR(30H + n MOD 10); INC(i); n := n DIV 10;
			IF useSeparators & (i MOD 4 = 3) & (n # 0) THEN d[i] := "'"; INC(i) END
		UNTIL n = 0;
		len := LEN(s) - 1;
		j := 0; WHILE s[j] # 0X DO INC(j) END;
		IF j + i < len THEN
			REPEAT DEC(i); s[j] := d[i]; INC(j) UNTIL i = 0;
			s[j] := 0X
		END
	END AppendInt;

	PROCEDURE Append (VAR s: ARRAY OF CHAR; t: ARRAY OF CHAR);
		VAR len: INTEGER; i, j: INTEGER; ch: CHAR;
	BEGIN
		len := LEN(s);
		i := 0; WHILE s[i] # 0X DO INC(i) END;
		j := 0; REPEAT ch := t[j]; s[i] := ch; INC(j); INC(i) UNTIL (ch = 0X) OR (i = len);
		s[len - 1] := 0X
	END Append;

	PROCEDURE StripTitle (VAR s: Views.Title);
		VAR i: INTEGER;
	BEGIN
		IF s[0] = "<" THEN
			i := 1; WHILE (s[i] # ">") & (s[i] # 0X) DO s[i - 1] := s[i]; INC(i) END;
			DEC(i); s[i] := 0X
		END
	END StripTitle;

	PROCEDURE GenTitle (w: Window; name: ARRAY OF CHAR; VAR title: ARRAY OF CHAR);
	(* generate window title for a document *)
		VAR newName: ARRAY 64 OF CHAR; i: INTEGER;
	BEGIN
		IF w.sub THEN title[0] := "<"; title[1] := 0X ELSE title[0] := 0X END;
		IF name # "" THEN
			i := 0;
			WHILE name[i] # 0X DO INC(i) END;
			IF (i > 4) & (name[i-4] = ".") & (CAP(name[i-3]) = "O") & (CAP(name[i-2]) = "D") & (CAP(name[i-1]) = "C")
			THEN
				name[i-4] := 0X
			END;
			Append(title, name)
		ELSE
			Dialog.MapString(untitledKey, newName);
			Append(title, newName); AppendInt(title, newNumber, noSeparators);
			INC(newNumber)
		END;
		IF w.sub THEN Append(title, ">") END
	END GenTitle;

	PROCEDURE GenPathTitle (w: Window; OUT title: ARRAY OF CHAR);
		VAR loc: Files.Locator; ch: CHAR; s1, s2: HostFiles.FullName; i, j: INTEGER;
	BEGIN
		loc := w.loc; title := "";
		WITH loc: HostFiles.Locator DO
			i := 0; ch := loc.path[0]; j := 0; s2 := "";
			WHILE ch # 0X DO
				IF (ch = "\") OR (ch = "/") THEN
					s1[j] := 0X; s2 := s1$; j := 0
				ELSE
					s1[j] := ch; INC(j)
				END;
				INC(i); ch := loc.path[i]
			END;
			s1[j] := 0X;
			IF ((CAP(s1[0]) = "M") & (CAP(s1[1]) = "O") & (CAP(s1[2]) = "D") & (s1[3] = 0X) OR
				(CAP(s1[0]) = "D") & (CAP(s1[1]) = "O") & (CAP(s1[2]) = "C") & (CAP(s1[3]) = "U") & (s1[4] = 0X) OR
				(CAP(s1[0]) = "R") & (CAP(s1[1]) = "S") & (CAP(s1[2]) = "R") & (CAP(s1[3]) = "C") & (s1[4] = 0X))
				& (s2 # "") THEN
				title := "("; Append(title, s2); Append(title, ")")
			END
		ELSE
		END;
		Append(title, w.name)
	END GenPathTitle;

	PROCEDURE ThisStyle (base, flags: SET): SET;
	BEGIN
		IF ~(Windows.noHScroll IN flags) THEN INCL(base, 20) END;	(* hor scrollbar *)
		IF ~(Windows.noVScroll IN flags) THEN INCL(base, 21) END;	(* ver scrollbar *)
		IF ~(Windows.noResize IN flags) THEN INCL(base, 18) END;	(* sizing border *)
		RETURN base
	END ThisStyle;

	PROCEDURE^ (w: Window) UpdateScrollbars (focus: BOOLEAN), NEW;


	(* Hook *)

	PROCEDURE (hk: Hook) Activate* (on: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (hk: Hook) Focus* (on: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (hk: Hook) Resize* (w, h: INTEGER), NEW, ABSTRACT;


	(* Window creation *)

	PROCEDURE OpenDoc (w: Window; l, t, r, b: INTEGER; min, max: BOOLEAN);
		(* first part of Open, called from directory.open *)
		VAR dw, dh, res: INTEGER; wnd: WinApi.HANDLE; m: WinApi.MDICREATESTRUCTW; c: Containers.Controller;
	BEGIN
		ASSERT(~(Windows.isTool IN w.flags), 20);
		m.szTitle := "";
		IF Windows.isAux IN w.flags THEN m.szClass := "Oberon Aux"
		ELSE m.szClass := "Oberon Doc"
		END;
		m.hOwner := instance;
		m.x := WinApi.CW_USEDEFAULT;
		m.y := WinApi.CW_USEDEFAULT;
		m.cx := WinApi.CW_USEDEFAULT;
		m.cy := WinApi.CW_USEDEFAULT;
		IF (l >= 0) & (t >= 0) & ~((l = 0) & (t = 0) & (r = 0) & (b = 0)) THEN
			m.x := l; m.y := t;
			IF (r > l) & (b > t) THEN
				m.cx := r - l; m.cy := b - t; w.fix := TRUE
			END
		END;
(*
		IF (l < r) & (t < b) THEN
			m.x := l; m.w := r - l; m.y := t; m.h := b - t; w.fix := TRUE
		ELSE
			m.x := WinApi.CWUseDefault;
			m.y := WinApi.CWUseDefault;
			m.w := WinApi.CWUseDefault;
			m.h := WinApi.CWUseDefault
		END;
*)
		m.style := ThisStyle({}, w.flags);
		IF ~(Windows.noResize IN w.flags) THEN
			IF max THEN INCL(m.style, 24) END	(* maximized *)
(*
			IF min THEN INCL(m.style, 29) END	(* minimized *)
*)
		END;
		m.lParam := SYSTEM.VAL(INTEGER, w);
		w.child := TRUE;

		c := w.doc.ThisController();
		IF dir.background THEN
			w.doc.PollRect(l, t, r, b);
			dw := (r - l ) DIV w.frame.unit; dh := (b - t) DIV w.frame.unit; (* TODO: Correct this *)
			IF Documents.winWidth IN c.opts THEN m.cx := mainW ELSE m.cx := dw END;
			IF Documents.winHeight IN c.opts THEN m.cy := mainH ELSE m.cy := dh END;
			m.x := 0; m.y := 0;
			w.fix := TRUE;
			m.style := {}
		END;
		wnd := WinApi.SendMessageW(client, WinApi.WM_MDICREATE, 0, SYSTEM.ADR(m));
		IF dir.background THEN
			res := WinApi.SetWindowPos(
				wnd, 1 (* HWND_BOTTOM *), 0, 0, 0, 0, WinApi.SWP_NOSIZE + WinApi.SWP_NOMOVE)
		END;
		IF ~(Windows.noResize IN w.flags) THEN
(*
			IF max THEN res := WinApi.ShowWindow(wnd, 3) END;	(* maximized *)
*)
			IF min THEN res := WinApi.ShowWindow(wnd, 6) END	(* minimized *)
		END;
		c.SetFocus(w.doc.ThisView());
		res := WinApi.SetFocus(wnd);
		res := WinApi.UpdateWindow(wnd)
	END OpenDoc;

	PROCEDURE CreateDoc (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		(* second part of Open, called from window handler *)
		VAR res, cw, ch, u, dl, dt, dr, db: INTEGER; w: Window; style, f: SET; v: Views.View; col: Ports.Color;
			dc: WinApi.HANDLE; rect, crect: WinApi.RECT; c: Containers.Controller;
			cs: WinApi.PtrCREATESTRUCTW; mdics: WinApi.PtrMDICREATESTRUCTW;
	BEGIN
		cs := SYSTEM.VAL(WinApi.PtrCREATESTRUCTW, lParam);
		mdics := SYSTEM.VAL(WinApi.PtrMDICREATESTRUCTW, cs.lpCreateParams);
		w := SYSTEM.VAL(Window, mdics.lParam);
		res := WinApi.SetWindowLongW(wnd, 0, SYSTEM.VAL(INTEGER, w));
		w.wnd := wnd;
		dc := WinApi.GetDC(wnd);
		w.port(HostPorts.Port).SetDC(dc, wnd);
		IF ~(Windows.noHScroll IN w.flags) THEN
			res := WinApi.SetScrollRange(wnd, WinApi.SB_HORZ, 0, scrollRange, 1)
		END;
		IF ~(Windows.noVScroll IN w.flags) THEN
			res := WinApi.SetScrollRange(wnd, WinApi.SB_VERT, 0, scrollRange, 1)
		END;
		v := w.doc.ThisView(); f := {};
		WITH v: Containers.View DO
			c := v.ThisController();
			IF c # NIL THEN f := c.opts END
		ELSE
		END;
		col := Views.transparent; v.GetBackground(col);
		w.dlg := ({Containers.noCaret, Containers.noSelection} - f = {})	(* mask mode *)
				& (col = Ports.dialogBackground);	(* dialog background *)

		style := BITS(WinApi.GetWindowLongW(wnd, -16));
		style := ThisStyle(style - {20, 21, 18}, w.flags);
		IF Windows.noResize IN w.flags THEN
			style := style - (WinApi.WS_THICKFRAME + WinApi.WS_MAXIMIZEBOX) + WinApi.WS_BORDER
		ELSE
			style := style + WinApi.WS_THICKFRAME
		END;

		IF dir.background THEN
			style := style - (WinApi.WS_THICKFRAME + WinApi.WS_MAXIMIZEBOX +
				 WinApi.WS_MINIMIZEBOX + WinApi.WS_BORDER + WinApi.WS_DLGFRAME);
			res := WinApi.ShowScrollBar(wnd, WinApi.SB_BOTH, WinApi.FALSE)
		END;

		res := WinApi.SetWindowLongW(wnd, -16, ORD(style));
		style := BITS(WinApi.GetWindowLongW(wnd, -20));
		IF ~dir.background THEN INCL(style, 8) ELSE EXCL(style, 8) END;	(* window edge *)
		IF w.dlg THEN (* INCL(style, 0) *) ELSE INCL(style, 9) END;	(* dialog modal frame, client edge *)

		res := WinApi.SetWindowLongW(wnd, -20, ORD(style));
		res := WinApi.SetWindowPos(w.wnd, 0, 0, 0, 0, 0, {0, 1, 2, 3, 5});
			(* no size, no move, no z order, no redraw, frame changed *)
		res := WinApi.GetClientRect(wnd, rect);
		u := w.frame.unit; cw := rect.right; ch := rect.bottom;
		w.port.SetSize(0, 0);
		w.doc.PollRect(dl, dt, dr, db);
		IF w.fix THEN
			IF dir.background THEN
				w.doc.SetRect(0, 0, dr - dl , db - dt) (* TODO: Corrct this *)
			ELSE
				IF w.dlg THEN w.doc.SetRect(0, 0, cw * u, ch * u)
				ELSE w.doc.SetRect(borderW, borderW, cw * u - borderW, ch * u - borderW)
				END
			END
		ELSIF w.dlg THEN
			cw := (dr - dl) DIV u;
			ch := (db - dt) DIV u;
			w.doc.SetRect(0, 0, dr - dl, db - dt)
		ELSE
			res := WinApi.GetClientRect(client, crect);
			cw := (dr - dl + 2 * borderW) DIV u + 1;
			ch := (db - dt + 2 * borderW) DIV u + 1;
			IF ~(Windows.noHScroll IN w.flags) & (cw > crect.right - 40) THEN cw := crect.right - 80 END;
			IF ~(Windows.noVScroll IN w.flags) & (ch > crect.bottom - 40) THEN ch := crect.bottom - 80 END;
(*
			IF cw > rect.right THEN cw := rect.right END;
			IF ch > rect.bottom THEN ch := rect.bottom END;
*)
			w.doc.SetRect(borderW, borderW, borderW + dr - dl, borderW + db - dt)
		END;
		IF cw < 0 THEN cw := 0 END;
		IF ch < 0 THEN ch := 0 END;
		w.SetSize(cw, ch);
		w.Restore(0, 0, cw, ch);
		w.Update;
		w.UpdateScrollbars(FALSE);
		IF ~w.fix THEN w.SetSize(cw, ch) END;
		res := WinApi.GetWindowRect(wnd, rect);
		w.mw := rect.right - rect.left; w.mh := rect.bottom - rect.top;
		HostMechanisms.InstallDropTarget(wnd, w);
		w.setup := TRUE
	END CreateDoc;

	PROCEDURE OpenDlg (w: Window; l, t, r, b: INTEGER; min, max: BOOLEAN);
		(* first part of Open, called from directory.open *)
		VAR res, cx, cy, cw, ch: INTEGER; wnd: WinApi.HANDLE; style: SET;
			rect: WinApi.RECT; c: Containers.Controller;
	BEGIN
		ASSERT(Windows.isTool IN w.flags, 20);
		style := ThisStyle({7, 19, 22, 23, (*25,*) 31}, w.flags);	(* dialog, sysmenu, border, clipchildren, popup *)
		w.child := FALSE; w.dlg := TRUE;
		res := WinApi.GetWindowRect(main, rect);
		cw := (rect.right - rect.left) DIV 2;
		cx := rect.left + cw DIV 2;
		ch := (rect.bottom - rect.top) DIV 2;
		cy := rect.top + ch DIV 2;
		IF (l >= 0) & (t >= 0) & ~((l = 0) & (t = 0) & (r = 0) & (b = 0)) THEN
			cx := l; cy := t;
			IF (r > l) & (b > t) THEN
				cw := r - l; ch := b - t; w.fix := TRUE
			END
		END;
(*
		IF (l < r) & (t < b) THEN
			cx := l; cw := r - l; cy := t; ch := b - t; w.fix := TRUE
		ELSE
			res := WinApi.GetWindowRect(main, rect);
			cw := (rect.right - rect.left) DIV 2;
			cx := rect.left + cw DIV 2;
			ch := (rect.bottom - rect.top) DIV 2;
			cy := rect.top + ch DIV 2
		END;
*)
		IF noAppWin THEN
			style := style + WinApi.WS_MINIMIZEBOX;
			wnd := WinApi.CreateWindowExW(WinApi.WS_EX_APPWINDOW + WinApi.WS_EX_DLGMODALFRAME,
															"Oberon Dlg", "", style, cx, cy, cw, ch,
															ActualWnd(), 0, instance, SYSTEM.VAL(INTEGER, w))
		ELSE
			wnd := WinApi.CreateWindowExW({0}, "Oberon Dlg", "", style, cx, cy, cw, ch,
															ActualWnd(), 0, instance, SYSTEM.VAL(INTEGER, w))
		END;
		res := WinApi.ShowWindow(wnd, 1);
		c := w.doc.ThisController();
		c.SetFocus(w.doc.ThisView());
		res := WinApi.SetFocus(wnd);
		res := WinApi.UpdateWindow(wnd)
	END OpenDlg;

	PROCEDURE CreateDlg (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		(* second part of Open, called from window handler *)
		VAR res, cw, ch, dl, dt, dr, db: INTEGER; w: Window;
			dc, menu: WinApi.HANDLE; rect: WinApi.RECT;
			cs: WinApi.PtrCREATESTRUCTW;
	BEGIN
		cs := SYSTEM.VAL(WinApi.PtrCREATESTRUCTW, lParam);
		w := SYSTEM.VAL(Window, cs.lpCreateParams);
		res := WinApi.SetWindowLongW(wnd, 0, SYSTEM.VAL(INTEGER, w));
		w.wnd := wnd; w.child := FALSE; w.dlg := TRUE;
		IF ~(inPlace IN w.flags) THEN

			menu := WinApi.GetSystemMenu(wnd, 0);
			res := WinApi.RemoveMenu(menu, 0F000H, {});	(* SC_SIZE *)
			IF ~noAppWin THEN res := WinApi.RemoveMenu(menu, 0F020H, {}) END;	(* SC_MINIMIZE *)
			res := WinApi.RemoveMenu(menu, 0F030H, {});	(* SC_MAXIMIZE *)
			res := WinApi.RemoveMenu(menu, 0F120H, {});	(* SC_RESTORE *)
			IF dir.unmoveable THEN
				res := WinApi.RemoveMenu(menu, 0F010H, {})	(* SC_MOVE *)
			END;

			dc := WinApi.GetDC(wnd);
			w.port(HostPorts.Port).SetDC(dc, wnd);
			IF ~(Windows.noHScroll IN w.flags) THEN
				res := WinApi.SetScrollRange(wnd, WinApi.SB_HORZ, 0, scrollRange, 1)
			END;
			IF ~(Windows.noVScroll IN w.flags) THEN
				res := WinApi.SetScrollRange(wnd, WinApi.SB_VERT, 0, scrollRange, 1)
			END;
			res := WinApi.GetClientRect(wnd, rect);
			IF w.fix THEN
				cw := rect.right; ch := rect.bottom;
				w.doc.SetRect(0, 0, cw * w.frame.unit, ch * w.frame.unit)
			ELSE
				w.doc.PollRect(dl, dt, dr, db);
				cw := (dr - dl) DIV w.frame.unit;
				ch := (db - dt) DIV w.frame.unit;
				w.doc.SetRect(0, 0, dr - dl, db - dt)
			END;
			IF cw < 0 THEN cw := 0 END;
			IF ch < 0 THEN ch := 0 END;
			w.SetSize(cw, ch);
			w.Restore(0, 0, cw, ch);
			w.Update;
			w.UpdateScrollbars(FALSE);
			w.SetSize(cw, ch)
		END;
		HostMechanisms.InstallDropTarget(wnd, w);
		w.setup := TRUE
	END CreateDlg;



	(** Window **)

	PROCEDURE (w: Window) ForwardCtrlMsg* (VAR msg: Controllers.Message), EXTENSIBLE;
		VAR d: BOOLEAN; res: INTEGER;
	BEGIN
		IF w.frame # NIL THEN
			Views.SetRoot(w.frame, w.frame.view, w = fWindow, w.flags);
			w.ForwardCtrlMsg^(msg);
			WITH msg: Controllers.ScrollMsg DO
				w.UpdateScrollbars(FALSE)
			ELSE
			END;
			IF (w.flags * {Windows.isAux, Windows.isTool} = {}) & (w.seq # NIL) THEN
				d := ~(Windows.neverDirty IN w.flags) & w.seq.Dirty();
				IF (d # w.dirty) & (w = aWindow) THEN
					res := WinApi.SendMessageW(w.wnd, WinApi.WM_NCACTIVATE, 1, 0);
					IF WinApi.IsZoomed(w.wnd) # 0 THEN res := WinApi.DrawMenuBar(main) END
				END
			END
		END
	END ForwardCtrlMsg;

	PROCEDURE (w: Window) SetSize* (width, height: INTEGER);
		VAR res, x, y, dw, dh: INTEGER; cr, wr: WinApi.RECT; p: WinApi.POINT;
	BEGIN
		IF w.port # NIL THEN
			IF (width = 0) & (height = 0) THEN
				w.minimized := TRUE
				(* no SetSize^ call *)
			ELSE
				w.minimized := FALSE;
				w.SetSize^(width, height);
				res := WinApi.GetClientRect(w.wnd, cr);
				dw := width - cr.right; dh := height - cr.bottom;
				IF ~(inPlace IN w.flags) & (w # cbViewer) & ((dw # 0) OR (dh # 0)) THEN
					res := WinApi.GetWindowRect(w.wnd, wr);
					p.x := wr.left; p.y := wr.top;
					IF ~(Windows.isTool IN w.flags) THEN res := WinApi.ScreenToClient(client, p) END;
					x := p.x; y := p.y;
					IF Windows.isTool IN w.flags THEN DEC(x, dw DIV 2); DEC(y, dh DIV 2) END;
					width := wr.right - wr.left + dw; height := wr.bottom - wr.top + dh;
					IF w.child THEN
						res := WinApi.GetClientRect(client, cr);
						dw := cr.right; dh := cr.bottom
					ELSE
						dw := scW; dh := scH
					END;
					IF x + width > dw THEN x := dw - width END;
					IF y + height > dh THEN y := dh - height END;

					IF x < 0 THEN x := 0 END;
					IF y < 0 THEN y := 0 END;
	(*
					IF x + width > dw THEN width := dw - x END;
					IF y + height > dh THEN height := dh - y END;
	*)
					res := WinApi.SetWindowPos(w.wnd, 0, x, y, width, height,
						WinApi.SWP_FRAMECHANGED + WinApi.SWP_NOZORDER + WinApi.SWP_NOACTIVATE)
				END
			END
		END
	END SetSize;

	PROCEDURE (w: Window) SetTitle2* (title: Views.Title), NEW;
	(* assign name of window, generate title out of name, and update window title bar *)
		VAR res: INTEGER; h: Window; t: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(w.wnd # 0, 20);
		StripTitle(title);
		h := w;
		REPEAT
			GenTitle(h, title, t);
			res := WinApi.SetWindowTextW(h.wnd, t);
			h := h.link(Window)
		UNTIL h = w
	END SetTitle2;

	PROCEDURE (w: Window) SetTitle* (title: Views.Title);
	BEGIN
		ASSERT(w.wnd # 0, 20);
		w.title := title; Dialog.MapString(w.title, title);
		w.SetTitle2(title)
	END SetTitle;

	PROCEDURE (w: Window) RefreshTitle* ;
		VAR title: Views.Title;
	BEGIN
		ASSERT(w.wnd # 0, 20);
		Dialog.MapString(w.title, title);
		w.SetTitle2(title)
	END RefreshTitle;

	PROCEDURE (w: Window) GetTitle* (OUT title: Views.Title);
	(* get name of window *)
		VAR res: INTEGER;
	BEGIN
		ASSERT(w.wnd # 0, 20);
		title := w.title;
		IF title = "" THEN
			res := WinApi.GetWindowTextW(w.wnd, title, LEN(title));
			StripTitle(title)
		END
	END GetTitle;

	PROCEDURE (w: Window) SetSpec* (loc: Files.Locator; name: Files.Name; conv: Converters.Converter);
		VAR title: Views.Title;
	BEGIN
		IF name # "" THEN Kernel.MakeFileName(name, "") END;
		w.SetSpec^ (loc, name, conv);
		IF (loc # NIL) & (w.wnd # 0) THEN GenPathTitle(w, title); w.SetTitle(title) END
	END SetSpec;

	PROCEDURE (w: Window) Mark (do, wk: BOOLEAN), NEW;
		VAR mark: Controllers.MarkMsg;
	BEGIN
		mark.show := do;
		mark.focus := ~wk;
		w.ForwardCtrlMsg(mark);
		Properties.IncEra
	END Mark;

	PROCEDURE (w: Window) MouseDown* (x, y, time: INTEGER; modifiers: SET);
	(* handle a mouse down event in window *)
		VAR pw, ph: INTEGER; track: Controllers.TrackMsg;
	BEGIN
		track.modifiers := modifiers;
		w.port.GetSize(pw, ph); track.x := x * w.port.unit; track.y := y * w.port.unit;
		w.ForwardCtrlMsg(track);
		Properties.IncEra
	END MouseDown;

	PROCEDURE (w: Window) KeyDown* (ch: CHAR; buttons: SET);
	BEGIN
		w.KeyDown^(ch, buttons);
		Properties.IncEra
	END KeyDown;


	PROCEDURE UpdateScrollbar (w: Window; vertical, focus: BOOLEAN);
		VAR res, size, sect, pos, type, p, q, m: INTEGER; valid: BOOLEAN; i: WinApi.SCROLLINFO;
			msg: Controllers.PollSectionMsg; f: Views.Frame;
	BEGIN
		IF w.frame = NIL THEN RETURN END;
		IF vertical THEN type := WinApi.SB_VERT ELSE type := WinApi.SB_HORZ END;
		GetSection(w, focus, vertical, size, sect, pos, valid);
		IF valid THEN
			res := WinApi.ShowScrollBar(w.wnd, type, 1);
			res := WinApi.EnableScrollBar(w.wnd, type, WinApi.ESB_ENABLE_BOTH);
			p := WinApi.MulDiv(pos, scrollRange, size - sect);
			i.cbSize := SIZE(WinApi.SCROLLINFO); i.fMask := {0, 1, 2};	(* range, page, pos *)
			res := WinApi.GetScrollInfo(w.wnd, type, i);
			IF res # 0 THEN
				IF sect > 0 THEN q := WinApi.MulDiv(sect, scrollRange, size - sect); m := scrollRange + q
				ELSE q := -1; m := scrollRange
				END;
				IF (i.nPos # p) OR (i.nPage # q + 1) THEN
					i.nPos := p; i.nPage := q + 1; i.nMax := m;
					res := WinApi.SetScrollInfo(w.wnd, type, i, 1)
				END
			ELSIF p # WinApi.GetScrollPos(w.wnd, type) THEN
				res := WinApi.SetScrollPos(w.wnd, type, p, 1)
			END
		ELSIF ~focus THEN
			msg.focus := FALSE; msg.vertical := vertical; msg.done := FALSE;
			f := Views.ThisFrame(w.frame, w.doc.ThisView());
			IF f # NIL THEN
				Views.ForwardCtrlMsg(f, msg);
				IF msg.done THEN
					res := WinApi.ShowScrollBar(w.wnd, type, 1);
					res := WinApi.EnableScrollBar(w.wnd, type, WinApi.ESB_DISABLE_BOTH)
				ELSE res := WinApi.ShowScrollBar(w.wnd, type, 0)
				END
			ELSE res := WinApi.ShowScrollBar(w.wnd, type, 0)
			END
		END
	END UpdateScrollbar;

	PROCEDURE (w: Window) Scroll (code, p: INTEGER; focus, vertical: BOOLEAN), NEW;
		VAR res, size, sect, pos, type: INTEGER; valid, noBuf: BOOLEAN;
	BEGIN
		GetSection(w, focus, vertical, size, sect, pos, valid);
		IF valid THEN
			noBuf := HostPorts.noBuffer; HostPorts.noBuffer := TRUE;
			IF code = WinApi.SB_THUMBPOSITION THEN
				SetOrigin(w, focus, vertical, WinApi.MulDiv(p, size - sect, scrollRange))
			ELSIF visualScroll & (code = WinApi.SB_THUMBTRACK) THEN
				SetOrigin(w, focus, vertical, WinApi.MulDiv(p, size - sect, scrollRange));
				res := WinApi.UpdateWindow(w.wnd);
				dir.Update(w)
			ELSIF code = WinApi.SB_LINEUP THEN Scroll(w, focus, vertical, Controllers.decLine)
			ELSIF code = WinApi.SB_LINEDOWN THEN Scroll(w, focus, vertical, Controllers.incLine)
			ELSIF code = WinApi.SB_PAGEUP THEN Scroll(w, focus, vertical, Controllers.decPage)
			ELSIF code = WinApi.SB_PAGEDOWN THEN Scroll(w, focus, vertical, Controllers.incPage)
			END;
(*
			UpdateScrollbar(w, vertical, focus);
*)
			GetSection(w, focus, vertical, size, sect, pos, valid);
			IF vertical THEN type := WinApi.SB_VERT ELSE type := WinApi.SB_HORZ END;
			res := WinApi.SetScrollPos(w.wnd, type, WinApi.MulDiv(pos, scrollRange, size - sect), 1);
			dir.Update(w);
			HostPorts.noBuffer := noBuf
		END
	END Scroll;

	PROCEDURE (w: Window) UpdateScrollbars (focus: BOOLEAN), NEW;
	BEGIN
		IF (WinApi.GetAsyncKeyState(1) >= 0)
				& (WinApi.GetAsyncKeyState(4) >= 0)
				& (WinApi.GetAsyncKeyState(2) >= 0) THEN
			IF ~(Windows.noHScroll IN w.flags) THEN UpdateScrollbar(w, FALSE, focus) END;
			IF ~(Windows.noVScroll IN w.flags) THEN UpdateScrollbar(w, TRUE, focus) END
		END
	END UpdateScrollbars;

	PROCEDURE (w: Window) UpdateCursor (x, y: INTEGER; modifiers: SET), NEW;
		VAR pw, ph: INTEGER; msg: Controllers.PollCursorMsg; cur: INTEGER;
	BEGIN
		w.port.GetSize(pw, ph);
		IF ((w = fWindow) OR (w = tWindow) OR ~w.child) & (x >= 0) & (x < pw) & (y >= 0) & (y < ph) THEN
			msg.x := x * w.frame.unit; msg.y := y * w.frame.unit; msg.cursor := Ports.arrowCursor;
			msg.modifiers := modifiers;
			w.ForwardCtrlMsg(msg); cur := msg.cursor
		ELSE cur := Ports.arrowCursor
		END;
		IF cur >= 0 THEN w.frame.SetCursor(cur) END
	END UpdateCursor;

	PROCEDURE (w: Window) PutOnTop, NEW;
		VAR v: Window;
	BEGIN
		IF w # bgWindow THEN
			v := winAnchor;
			WHILE (v # NIL) & (v.next # w) DO v := v.next END;
			IF v # NIL THEN
				v.next := w.next; w.next := winAnchor.next; winAnchor.next := w
			END
		END
	END PutOnTop;

	PROCEDURE (w: Window) Close*, EXTENSIBLE;
		VAR res: INTEGER; h: Window;
	BEGIN
		ASSERT(w.frame # NIL, 20);
		IF bgWindow = w THEN bgWindow := NIL END;
		IF fWindow = w THEN
			w.Mark(FALSE, FALSE); fWindow := NIL;
			IF tWindow = w THEN tWindow := NIL END
		ELSIF tWindow = w THEN
			w.Mark(FALSE, FALSE); tWindow := NIL
		END;
(*
		(* remove all shown marks in all windows *)
		IF fWindow # NIL THEN
			ASSERT(fWindow.frame # NIL, 125); ASSERT(fWindow.wnd # 0, 126);
			mark.show := FALSE; fWindow.ForwardCtrlMsg(mark)
		END;
		tWindow := NIL; fWindow := NIL;
*)
(*
		IF w = fWindow THEN fWindow := NIL END;
		IF w = tWindow THEN tWindow := NIL END;
*)
		h := winAnchor;
		WHILE (h.next # NIL) & (h.next # w) DO h := h.next END;
		ASSERT(h.next = w, 21);
		h.next := w.next; w.next := NIL;
		HostMechanisms.RemoveDropTarget(w.wnd);
		w.Close^;
		IF ~w.destroyed THEN
			w.destroyed := TRUE;
			IF w.child THEN
				IF WinApi.IsZoomed(w.wnd) # 0 THEN
					res := WinApi.SendMessageW(client, WinApi.WM_MDIRESTORE, w.wnd, 0)
				END;
				res := WinApi.SendMessageW(client, WinApi.WM_MDIDESTROY, w.wnd, 0)
			ELSE
				res := WinApi.DestroyWindow(w.wnd)
			END;
			w.trapped := TRUE; w.wnd := 0
		END;
		ASSERT(w.frame = NIL, 60)
	END Close;


	PROCEDURE ShowMain*;
		VAR res: INTEGER; menu: WinApi.HANDLE;
	BEGIN
		IF debug THEN Log.String("show main"); Log.Ln END;
		IF fullSize THEN
			menu := WinApi.GetSystemMenu(main, 0);
			res := WinApi.RemoveMenu(menu, WinApi.SC_MOVE, {})
		END;
		res := WinApi.ShowWindow(main, showState);
		res := WinApi.UpdateWindow(main);
		showState := WinApi.SW_SHOW
	END ShowMain;


	(* Directory *)

	PROCEDURE (d: Directory) Open* (
		w: Windows.Window; doc: Documents.Document; flags: SET; name: Views.Title;
		loc: Files.Locator; fname: Files.Name; conv: Converters.Converter
	), EXTENSIBLE;
		VAR res: INTEGER; v, bg: Window; p: HostPorts.Port;
	BEGIN
		WITH w: Window DO
			(* if background then it has to be and AuxDialog *)
			IF d.background
				& ((Windows.isTool IN flags) OR ~(Windows.noHScroll IN flags) OR ~(Windows.noVScroll IN flags))
			THEN
				d.background := FALSE
			END;
			(* port allocation *)
			NEW(p);
			IF d.unit # 0 THEN p.Init(d.unit, Ports.screen) ELSE p.Init(unit, Ports.screen) END;
			(* initialization *)
			w.trapped := FALSE;
			w.Init(p);
			d.Open^(w, doc, flags, name, loc, fname, conv);
			IF w # cbViewer THEN
				IF ~d.background THEN
					w.next := winAnchor.next; winAnchor.next := w	(* new top window *)
				ELSE
					v := winAnchor;
					WHILE v.next # NIL DO v := v.next END; v.next := w; w.next := NIL;
					bg := bgWindow; bgWindow := w (* new background window *)
				END;
				IF ~(inPlace IN w.flags) THEN
					(* window creation *)
					IF Windows.isTool IN flags THEN
						OpenDlg(w, d.l, d.t, d.r, d.b, d.minimized, d.maximized)
					ELSE
						IF ~d.invisible & (WinApi.IsWindowVisible(main) = 0) THEN ShowMain END;
						OpenDoc(w, d.l, d.t, d.r, d.b, d.minimized, d.maximized)
					END;
					IF (loc # NIL) & (name = fname) THEN GenPathTitle(w, name) END;
					w.SetTitle(name);
					IF Windows.isTool IN flags THEN
						res := WinApi.SendMessageW(w.wnd, WinApi.WM_NCACTIVATE, 0, 0);
						res := WinApi.SendMessageW(w.wnd, WinApi.WM_NCACTIVATE, 1, 0)
					END;
					ASSERT(w.frame # NIL, 60)
				END
			END;
			d.l := -1; d.t := -1; d.r := -1; d.b := -1;
			d.minimized := FALSE; d.maximized := FALSE;
			d.unit := 0;
			d.unmoveable := FALSE;
			IF d.background THEN
				IF bg # NIL THEN dir.Close(bg) END;
				bgWindow := w;
				d.background := FALSE
			END
		END
	END Open;

	PROCEDURE (d: Directory) First* (): Window;
	BEGIN
		RETURN winAnchor.next
	END First;

	PROCEDURE (d: Directory) Next* (w: Windows.Window): Window;
	BEGIN
		IF w # NIL THEN RETURN w(Window).next ELSE RETURN NIL END
	END Next;

	PROCEDURE (d: Directory) New* (): Window, EXTENSIBLE;
		VAR w: Window;
	BEGIN
		NEW(w); RETURN w
	END New;

	PROCEDURE (d: Directory) Focus* (target: BOOLEAN): Window;
	BEGIN
		IF target THEN RETURN tWindow ELSE RETURN fWindow END
	END Focus;

	PROCEDURE (d: Directory) Select* (w: Windows.Window; lazy: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		WITH w: Window DO
			IF ~(inPlace IN w.flags) THEN
				res := WinApi.SetForegroundWindow(main);
				IF w.child THEN res := WinApi.BringWindowToTop(main) END
			END;
			IF WinApi.IsIconic(w.wnd) # 0 THEN
				res := WinApi.ShowWindow(w.wnd, 9);	(* restore *)
				res := WinApi.InvalidateRect(w.wnd, NIL, 0)
			END;
			IF WinApi.GetTopWindow(main) # w.wnd THEN
				res := WinApi.BringWindowToTop(w.wnd)
			END;
(*
			res := WinApi.UpdateWindow(w.wnd);
*)
			res := WinApi.SetFocus(w.wnd)
		END
	END Select;

	PROCEDURE (d: Directory) GetThisWindow* (p: Ports.Port; px, py: INTEGER;
																		OUT x, y: INTEGER; OUT w: Windows.Window);
		VAR res: INTEGER; wnd: WinApi.HANDLE; pt: WinApi.POINT; s: ARRAY 32 OF CHAR;
	BEGIN
		pt.x := px; pt.y := py;
		IF p # NIL THEN
			wnd := p(HostPorts.Port).wnd;
			res := WinApi.ClientToScreen(wnd, pt)
		END;
		wnd := WinApi.WindowFromPoint(pt);
		IF wnd # 0 THEN
			res := WinApi.GetClassNameW(wnd, s, LEN(s));
			IF (s = "Oberon Doc") OR (s = "Oberon Aux") OR (s = "Oberon Dlg") THEN
				res := WinApi.ScreenToClient(wnd, pt);
				x := pt.x; y := pt.y; w := ThisWindow(wnd)
			ELSE
				w := NIL
			END
		ELSE
			w := NIL
		END
	END GetThisWindow;

	PROCEDURE (d: Directory) Close* (w: Windows.Window);
		VAR v, u: Windows.Window; h: Window;
	BEGIN
		h := winAnchor; WHILE (h.next # NIL) & (h.next # w) DO h := h.next END;
		IF h.next = w THEN
			IF ~w.sub THEN
				v := w.link;
				WHILE v # w DO u := v.link; v.Close; v := u END
			END;
			w.Close
		END
	END Close;

	PROCEDURE (d: Directory) GetBounds* (OUT w, h: INTEGER);
		VAR rect: WinApi.RECT; res: INTEGER;
	BEGIN
		res := WinApi.GetClientRect(client, rect);
		w := rect.right; h := rect.bottom
	END GetBounds;


	(** window handlers **)

	PROCEDURE CallHeapShow* (a: INTEGER);
		TYPE P = PROCEDURE(a: INTEGER; t: ARRAY OF CHAR);
			V = RECORD (Meta.Value) p: P END;
		VAR i: Meta.Item; ok: BOOLEAN; v: V;
	BEGIN
		Meta.Lookup("DevDebug", i);
		IF i.obj = Meta.modObj THEN i.Lookup("ShowHeapObject", i);
			IF i.obj = Meta.procObj THEN i.GetVal(v, ok);
				IF ok THEN v.p(a, "") END
			END
		END
	END CallHeapShow;

	PROCEDURE KeyModifiers (data: INTEGER): SET;
		VAR b: SET;
	BEGIN
		b := {};
		IF WinApi.GetKeyState(10H) < 0 THEN INCL(b, Controllers.extend); INCL(b, HostPorts.shift) END;
		IF WinApi.GetKeyState(11H) < 0 THEN INCL(b, Controllers.modify); INCL(b, HostPorts.ctrl) END;
		IF ODD(data DIV 20000000H) THEN INCL(b, HostPorts.alt) END;
		RETURN b
	END KeyModifiers;

	PROCEDURE HandleKey (wnd: WinApi.HANDLE; code, ext: INTEGER);
		VAR b: SET; w: Window; pmsg: Controllers.PollFocusMsg; scroll: BOOLEAN; c: Containers.Controller;
	BEGIN
		w := ThisWindow(wnd); b := KeyModifiers(ext); w.used := TRUE;
		scroll := ODD(WinApi.GetKeyState(91H)) (* scroll lock *);
		pmsg.focus := NIL; w.ForwardCtrlMsg(pmsg);
		IF (pmsg.focus # NIL) & (pmsg.focus.view IS Containers.View) THEN
			c := pmsg.focus.view(Containers.View).ThisController();
			IF (c # NIL) & (Containers.noCaret IN c.opts) THEN scroll := TRUE END
		END;
		CASE code OF
		| 2EH: w.KeyDown(RDEL, b)	(* delete -> right delete *)
		| 08H: w.KeyDown(LDEL, b)	(* backspace -> left delete *)
		| 09H: (* tab *)
			IF Controllers.extend IN b THEN w.KeyDown(LTAB, b)	(* left tab *)
			ELSE w.KeyDown(TAB, b)	(* right tab *)
			END
		| 0DH: w.KeyDown(ENTER, b)	(* enter *)
		| 1BH: w.KeyDown(ESC, b)	(* escape *)
		| 21H:	(* page up *)
			IF scroll THEN
				w.Scroll(WinApi.SB_PAGEUP, 0, TRUE, ~(Controllers.modify IN b))
			ELSIF Controllers.modify IN b THEN (* move caret left one page *)
				w.KeyDown(PL, b - {Controllers.modify})
			ELSE (* move caret up one page *)
				w.KeyDown(PU, b)
			END
		| 22H: (* page down *)
			IF scroll THEN
				w.Scroll(WinApi.SB_PAGEDOWN, 0, TRUE, ~(Controllers.modify IN b))
			ELSIF Controllers.modify IN b THEN (* move caret right one page *)
				w.KeyDown(PR, b - {Controllers.modify})
			ELSE (* move caret down one page *)
				w.KeyDown(PD, b)
			END
		| 23H: (* end *)
			IF scroll THEN
				w.Scroll(WinApi.SB_THUMBPOSITION, scrollRange, TRUE, Controllers.modify IN b)
			ELSIF Controllers.modify IN b THEN (* move caret to doc end *)
				w.KeyDown(DD, b - {Controllers.modify})
			ELSE (* move caret to line end *)
				w.KeyDown(DR, b)
			END
		| 24H: (* home *)
			IF scroll THEN
				w.Scroll(WinApi.SB_THUMBPOSITION, 0, TRUE, Controllers.modify IN b)
			ELSIF Controllers.modify IN b THEN (* move caret to doc start *)
				w.KeyDown(DU, b - {Controllers.modify})
			ELSE (* move caret to line start *)
				w.KeyDown(DL, b)
			END
		| 25H: (* left *)
			IF scroll THEN
				w.Scroll(WinApi.SB_LINEUP, 0, TRUE, FALSE)
			ELSE
				w.KeyDown(AL, b)
			END
		| 26H: (* up *)
			IF scroll THEN
				w.Scroll(WinApi.SB_LINEUP, 0, TRUE, TRUE)
			ELSE
				w.KeyDown(AU, b)
			END
		| 27H: (* right *)
			IF scroll THEN
				w.Scroll(WinApi.SB_LINEDOWN, 0, TRUE, FALSE)
			ELSE
				w.KeyDown(AR, b)
			END
		| 28H: (* down *)
			IF scroll THEN
				w.Scroll(WinApi.SB_LINEDOWN, 0, TRUE, TRUE)
			ELSE
				w.KeyDown(AD, b)
			END
		ELSE
		END;
		Properties.IncEra
	END HandleKey;

	PROCEDURE HandleChar (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR w: Window; mod: SET; ch: CHAR;
	BEGIN
		IF (wParam >= 20H) & (wParam # 7FH) THEN
			w := ThisWindow(wnd); mod := KeyModifiers(lParam); w.used := TRUE;
			IF WinApi.VkKeyScanW(CHR(wParam)) >= 4 * 256 THEN EXCL(mod, HostPorts.alt) END;
			w.KeyDown(CHR(wParam), mod)
		END
	END HandleChar;

	PROCEDURE HandleMouse (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR w: Window; isDown: BOOLEAN; x, y: INTEGER; b: SET; f, g: Views.Frame;
	BEGIN
		w := ThisWindow(wnd); b := {}; w.used := TRUE;
		IF ODD(wParam) THEN INCL(b, HostPorts.left) END;
		IF ODD(wParam DIV 16) THEN INCL(b, HostPorts.middle) END;
		IF ODD(wParam DIV 2) THEN INCL(b, HostPorts.right) END;
		isDown := b # {};
		IF ODD(wParam DIV 4) THEN INCL(b, HostPorts.shift); INCL(b, Controllers.extend) END;
		IF ODD(wParam DIV 8) THEN INCL(b, HostPorts.ctrl); INCL(b, Controllers.modify) END;
		IF WinApi.GetAsyncKeyState(12H) < 0 THEN INCL(b, HostPorts.alt) END;
		IF ODD(wParam DIV 128) THEN INCL(b, Controllers.doubleClick) END;
		x := lParam MOD 65536;
		y := lParam DIV 65536;
		HostPorts.SetMouseState(x, y, b, isDown);
		IF wParam DIV 256 = 1 THEN
			IF {HostPorts.middle, HostPorts.shift, HostPorts.ctrl} - b = {} THEN
				CallHeapShow(SYSTEM.VAL(INTEGER, w))
			ELSIF {HostPorts.middle, HostPorts.shift, HostPorts.alt} - b = {} THEN
				f := w.frame; x := x * f.unit; y := y * f.unit;
				REPEAT g := f; f := Views.FrameAt(g, x - g.gx, y - g.gy) UNTIL f = NIL;
				CallHeapShow(SYSTEM.VAL(INTEGER, g))
			ELSIF ~activating THEN
				w.MouseDown(x, y, 0, b)
			END
		ELSIF wParam DIV 256 = 2 THEN
			w.UpdateCursor(x, y, b)
		END;
		IF ~isDown THEN activating := FALSE END;
		Properties.IncEra
	END HandleMouse;

	PROCEDURE DeactivateWin (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR w: Window;
	BEGIN
		w := ThisWindow(wnd);
		IF fWindow = w THEN
			w.Mark(FALSE, TRUE); fWindow := NIL;
			IF (inPlace IN w.flags) OR ~(Windows.isTool IN w.flags) THEN
				w.Mark(TRUE, TRUE);
				IF (w # aWindow) & ~(inPlace IN w.flags) THEN tWindow := NIL END
			END
		END
	END DeactivateWin;

	PROCEDURE ActivateWin (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR res: INTEGER; w: Window;
	BEGIN
		w := ThisWindow(wnd);
		IF fWindow # w THEN
			IF fWindow # NIL THEN
				DeactivateWin(fWindow.wnd, 0, 0)
			END;
			w.PutOnTop;
			res := WinApi.UpdateWindow(wnd);
			IF (inPlace IN w.flags) OR ~(Windows.isTool IN w.flags) THEN
				w.Mark(FALSE, TRUE);
				tWindow := w
			END;
			fWindow := w; w.Mark(TRUE, TRUE);
			Properties.IncEra;
(*
			Dialog.CheckGuards
*)
			Dialog.Notify(0, 0, {guardCheck})
		END
	END ActivateWin;




	PROCEDURE ActivateWindow* (w: Windows.Window; do: BOOLEAN);
	BEGIN
		IF debug THEN
			IF do THEN Log.String("Activate ") ELSE Log.String("Deactivate ") END;
			Log.IntForm(SYSTEM.VAL(INTEGER, w), 16, 8, "0", FALSE);
			Log.Char(" ");
			Log.IntForm(SYSTEM.VAL(INTEGER, fWindow), 16, 8, "0", FALSE);
			IF inPlace IN w.flags THEN Log.String(" ip") END;
			IF Windows.isTool IN w.flags THEN Log.String(" tool") END;
			Log.Ln
		END;

		IF do THEN ActivateWin(w(Window).wnd, 0, 0)
		ELSE DeactivateWin(w(Window).wnd, 0, 0)
		END
	END ActivateWindow;

	PROCEDURE SizeWin (wnd: WinApi.HANDLE; w, h: INTEGER);
		VAR v: Window;
	BEGIN
		v := ThisWindow(wnd);
		IF ~(inPlace IN v.flags) THEN
			v.SetSize(w, h)
		END;
		IF v.hook # NIL THEN v.hook.Resize(w, h) END
	END SizeWin;

	PROCEDURE ScrollWin (wnd: WinApi.HANDLE; wParam, vertical: INTEGER);
		VAR w: Window;
	BEGIN
		w := ThisWindow(wnd);
		IF ~activating THEN w.Scroll(wParam MOD 65536, wParam DIV 65536, ScrollModPressed(), vertical # 0)
		END
	END ScrollWin;

	PROCEDURE WheelScroll (wnd: WinApi.HANDLE; wParam, lParam: INTEGER);
		VAR w: Window; res, lines, delta, keys: INTEGER; msg: Controllers.WheelMsg;
			p: WinApi.POINT;
	BEGIN
		delta := wParam DIV 10000H; keys := wParam MOD 10000H;
		w := ThisWindow(wnd);
		lines := 3;
		res := WinApi.SystemParametersInfoW(104 (*SPI_GETWHEELSCROLLLINES*), 0, SYSTEM.ADR(lines), 0);
		p.x := lParam MOD 65536; p.y := lParam DIV 65536;
		res := WinApi.ScreenToClient(wnd, p);
		msg.x := p.x * w.port.unit; msg.y := p.y * w.port.unit;
		msg.nofLines := 0; msg.op := -1;
		IF lines > 10 THEN	(* scroll pages *)
			INC(w.wheelPos, delta);
			IF w.wheelPos >= 120 THEN
				msg.op := Controllers.decPage;
				DEC(w.wheelPos, 120)
			ELSIF w.wheelPos <= -120 THEN
				msg.op := Controllers.incPage;
				INC(w.wheelPos, 120)
			END
		ELSIF lines > 0 THEN
			INC(w.wheelPos, delta * lines);
			WHILE w.wheelPos >= 120 DO
				msg.op := Controllers.decLine;
				INC(msg.nofLines);
				DEC(w.wheelPos, 120)
			END;
			WHILE w.wheelPos <= -120 DO
				msg.op := Controllers.incLine;
				INC(msg.nofLines);
				INC(w.wheelPos, 120)
			END
		END;
		msg.done := FALSE;
		IF msg.op >= 0 THEN w.ForwardCtrlMsg(msg) END;
		IF ~msg.done THEN (* scroll document *)
			CASE msg.op OF
			| Controllers.decPage:
				w.Scroll(WinApi.SB_PAGEUP, 0, ODD(keys DIV 8), ~ODD(keys DIV 4))
			| Controllers.incPage:
				w.Scroll(WinApi.SB_PAGEDOWN, 0, ODD(keys DIV 8), ~ODD(keys DIV 4))
			| Controllers.decLine:
				WHILE msg.nofLines > 0 DO
					w.Scroll(WinApi.SB_LINEUP, 0, ODD(keys DIV 8), ~ODD(keys DIV 4));
					DEC(msg.nofLines)
				END
			| Controllers.incLine:
				WHILE msg.nofLines > 0 DO
					w.Scroll(WinApi.SB_LINEDOWN, 0, ODD(keys DIV 8), ~ODD(keys DIV 4));
					DEC(msg.nofLines)
				END
			ELSE
			END
(*
			IF lines > 10 THEN	(* scroll pages *)
				INC(w.wheelPos, delta);
				IF w.wheelPos >= 120 THEN
					w.Scroll(WinApi.SBPageUp, 0, ODD(keys DIV 8), ~ODD(keys DIV 4));
					DEC(w.wheelPos, 120)
				ELSIF w.wheelPos <= -120 THEN
					w.Scroll(WinApi.SBPageDown, 0, ODD(keys DIV 8), ~ODD(keys DIV 4));
					INC(w.wheelPos, 120)
				END
			ELSIF lines > 0 THEN
				INC(w.wheelPos, delta * lines);
				WHILE w.wheelPos >= 120 DO
					w.Scroll(WinApi.SBLineUp, 0, ODD(keys DIV 8), ~ODD(keys DIV 4));
					DEC(w.wheelPos, 120)
				END;
				WHILE w.wheelPos <= -120 DO
					w.Scroll(WinApi.SBLineDown, 0, ODD(keys DIV 8), ~ODD(keys DIV 4));
					INC(w.wheelPos, 120)
				END
			END
*)
		END
	END WheelScroll;

	PROCEDURE InvalidateWin (wnd, dc: WinApi.HANDLE; paint: INTEGER);
		TYPE RectPtr = POINTER TO WinApi.RECT;
		VAR w: Window; rp: RectPtr;
	BEGIN
		w := ThisWindow(wnd);
		w.port(HostPorts.Port).SetDC(dc, wnd);
		rp := SYSTEM.VAL(RectPtr, paint);
		w.Restore(rp.left, rp.top, rp.right, rp.bottom)
	END InvalidateWin;

	PROCEDURE PaintWin (wnd: WinApi.HANDLE; a, b: INTEGER);
		VAR w: Window;
	BEGIN
		w := ThisWindow(wnd);
		w.Update
	END PaintWin;

	PROCEDURE [2] DocWinHandler (wnd: WinApi.HANDLE; message, wParam, lParam: INTEGER): INTEGER;
		VAR res, x, dx, dy: INTEGER; w: Window; ps: WinApi.PAINTSTRUCT;
			(* pm: WinApi.PMinMaxInfo; *)
			rec: WinApi.RECT; style, st: SET; wp: WinApi.WINDOWPOS;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		IF (hookDocWinHandler # NIL) & hookDocWinHandler(wnd, message, wParam, lParam, res) THEN
			Controllers.ResetCurrentPath();
			RETURN res
		END;
		CASE message OF
		| WinApi.WM_CREATE:
			Kernel.Try(CreateDoc, wnd, wParam, lParam)
		| WinApi.WM_CLOSE:
			res := WinApi.SendMessageW(main, WinApi.WM_COMMAND, iClose, 0);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_DESTROY:
			w := ThisWindow(wnd);
			IF ~w.destroyed THEN
				w.destroyed := TRUE;
				w.Close
			END;
			IF w = aWindow THEN aWindow := NIL END;
			IF w = tWindow THEN tWindow := NIL END
		| WinApi.WM_CHILDACTIVATE:
			w := ThisWindow(wnd);
			w.PutOnTop; aWindow := w;
			tWindow := w
		| WinApi.WM_SETFOCUS:
			w := ThisWindow(wnd);
			IF debug THEN
				Log.String("Doc: SetFocus ");
				Log.IntForm(SYSTEM.VAL(INTEGER, w), 16, 8, "0", FALSE);
				Log.Char(" ");
				Log.IntForm(SYSTEM.VAL(INTEGER, fWindow), 16, 8, "0", FALSE);
				IF inPlace IN w.flags THEN Log.String(" ip") END;
				IF Windows.isTool IN w.flags THEN Log.String(" tool") END;
				Log.Ln
			END;
			IF tWindow # w THEN activating := TRUE END;
			IF w = bgWindow THEN
				res := WinApi.SetWindowPos(wnd, 1 (* HWND_BOTTOM *), 0, 0, 0, 0,
					WinApi.SWP_NOSIZE + WinApi.SWP_NOMOVE + WinApi.SWP_NOREDRAW)
			END;
			Kernel.Try(ActivateWin, wnd, wParam, lParam);
			IF w.hook # NIL THEN w.hook.Focus(TRUE) END
		| WinApi.WM_KILLFOCUS:
			w := ThisWindow(wnd);
			IF debug THEN
				Log.String("Doc: KillFocus ");
				Log.IntForm(SYSTEM.VAL(INTEGER, w), 16, 8, "0", FALSE);
				Log.Char(" ");
				Log.IntForm(SYSTEM.VAL(INTEGER, fWindow), 16, 8, "0", FALSE);
				IF inPlace IN w.flags THEN Log.String(" ip") END;
				IF Windows.isTool IN w.flags THEN Log.String(" tool") END;
				Log.Ln
			END;
			IF wParam # main THEN Kernel.Try(DeactivateWin, wnd, wParam, lParam) END;
			IF w.hook # NIL THEN w.hook.Focus(FALSE) END
		| WinApi.WM_SIZE:
			w := ThisWindow(wnd);
			IF (w = bgWindow) & ((wParam = WinApi.SIZE_MAXHIDE) OR (wParam = WinApi.SIZE_MAXSHOW)) THEN
				Controllers.ResetCurrentPath();
				RETURN 0
			ELSE
				Kernel.Try(SizeWin, wnd, lParam MOD 65536, lParam DIV 65536)
			END
		| WinApi.WM_HSCROLL:
			Kernel.Try(ScrollWin, wnd, wParam, 0);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_VSCROLL:
			Kernel.Try(ScrollWin, wnd, wParam, 1);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_MOUSEWHEEL:
			Kernel.Try(WheelScroll, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_PAINT:
			w := ThisWindow(wnd);
			w.GetSize(dx, dy);
			IF ~w.minimized THEN
				(* clip region must be reset to ensure correct ps.paint ! *)
				res := WinApi.SelectClipRgn(w.port(HostPorts.Port).dc, 0);
				x := WinApi.BeginPaint(w.wnd, ps);
				Kernel.Try(InvalidateWin, wnd, x, SYSTEM.ADR(ps.rcPaint));
				res := WinApi.EndPaint(w.wnd, ps);
				Kernel.Try(PaintWin, wnd, 0, 0);
				Controllers.ResetCurrentPath();
				RETURN 0
			END
		| WinApi.WM_LBUTTONDOWN, WinApi.WM_RBUTTONDOWN, WinApi.WM_MBUTTONDOWN:
			IF wnd # WinApi.GetFocus() THEN
				res := WinApi.SendMessageW(client, WinApi.WM_MDIACTIVATE, wnd, 0);
				res := WinApi.SetFocus(wnd)
			END;
			res := WinApi.SetCapture(wnd);
			Kernel.Try(HandleMouse, wnd, wParam + 256, lParam);
			IF WinApi.GetCapture() = wnd THEN res := WinApi.ReleaseCapture() END;
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_LBUTTONDBLCLK, WinApi.WM_RBUTTONDBLCLK, WinApi.WM_MBUTTONDBLCLK:
			res := WinApi.SetCapture(wnd);
			Kernel.Try(HandleMouse, wnd, wParam + (128 + 256), lParam);
			IF WinApi.GetCapture() = wnd THEN res := WinApi.ReleaseCapture() END;
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_LBUTTONUP, WinApi.WM_RBUTTONUP, WinApi.WM_MBUTTONUP:
			Kernel.Try(HandleMouse, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_MOUSEMOVE:
			Kernel.Try(HandleMouse, wnd, wParam + (2 * 256), lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_KEYDOWN:
			Kernel.Try(HandleKey, wnd, wParam, lParam);
			IF (wParam = WinApi.VK_TAB)
				OR (wParam = WinApi.VK_LEFT) OR (wParam = WinApi.VK_UP)
				OR (wParam = WinApi.VK_RIGHT) OR (wParam = WinApi.VK_DOWN)
			THEN
				res := WinApi.SendMessageW(wnd, 0127H (* WM_CHANGEUISTATE *),
							2 (* UIS_CLEAR *) + 10000H (* UISF_HIDEFOCUS *), 0)
			END
		| WinApi.WM_SYSKEYDOWN:
			IF wParam = WinApi.VK_MENU THEN
				res := WinApi.SendMessageW(wnd, 0127H (* WM_CHANGEUISTATE *),
							2 (* UIS_CLEAR *) + 10000H (* UISF_HIDEFOCUS *) + 20000H (* UISF_HIDEACCEL *), 0)
			END
		| WinApi.WM_CHAR:
			Kernel.Try(HandleChar, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
(*
		| WinApi.WM_UNICHAR:
			IF wParam = WinApi.UNICODE_NOCHAR THEN RETURN 1 END;
			Kernel.Try(HandleChar, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
*)
		| WinApi.WM_NCLBUTTONDOWN:
			IF activating & ((wParam = 6) OR (wParam = 7) OR (wParam = 20)) THEN	(* in scrollbar, close *)
				Controllers.ResetCurrentPath();
				RETURN 0
			END
		| WinApi.WM_NCLBUTTONUP, WinApi.WM_NCRBUTTONUP:
			activating := FALSE
		| WinApi.WM_MDIACTIVATE:
			w := ThisWindow(wnd);
			IF w.hook # NIL THEN w.hook.Activate(wnd = lParam) END
		| WinApi.WM_NCACTIVATE:
			w := ThisWindow(wnd);
			IF (w = bgWindow) OR ((bgWindow # NIL) & (wParam MOD 65536 = WinApi.FALSE)) THEN
				res := WinApi.SetWindowPos(bgWindow.wnd, 1 (* HWND_BOTTOM *), 0, 0, 0, 0,
					WinApi.SWP_NOSIZE + WinApi.SWP_NOMOVE + WinApi.SWP_NOACTIVATE)
			END;
			style := BITS(WinApi.GetWindowLongW(wnd, -16));
			IF wParam MOD 65536 # 0 THEN
				IF ~mainActive & (w # tWindow) & (tWindow # NIL) THEN
					st := BITS(WinApi.GetWindowLongW(tWindow.wnd, -16));
					st := st - {16, 17, 19};	(* minimize box, maximize box, sysmenu *)
					res := WinApi.SetWindowLongW(tWindow.wnd, -16, ORD(st));
					res := WinApi.GetWindowRect(tWindow.wnd, rec);
					x := WinApi.CreateRectRgnIndirect(rec);
					res := WinApi.SendMessageW(tWindow.wnd, WinApi.WM_NCPAINT, x, 0)
				END;
				IF (w.flags * {Windows.isAux, Windows.isTool} = {}) THEN
					IF ~(Windows.neverDirty IN w.flags) & w.seq.Dirty() THEN
						res := WinApi.SetClassLongW(wnd, -14, dirtyIcon); w.dirty := TRUE
					ELSE
						res := WinApi.SetClassLongW(wnd, -14, docIcon); w.dirty := FALSE
					END
				END;
				style := style + WinApi.WS_MINIMIZEBOX + WinApi.WS_SYSMENU;
				IF ~(Windows.noResize IN w.flags) THEN style := style + WinApi.WS_MAXIMIZEBOX END
			ELSIF mainActive THEN
				style := style - {16, 17, 19}	(* minimize box, maximize box, sysmenu *)
			END;
			res := WinApi.SetWindowLongW(wnd, -16, ORD(style))
		| WinApi.WM_WINDOWPOSCHANGED:
			w := ThisWindow(wnd);
			wp := SYSTEM.VAL(WinApi.WINDOWPOS, lParam);
			IF (w = bgWindow) & (wp.hwndInsertAfter # 1 (* HWND_BOTTOM *)) THEN
				res := WinApi.SetWindowPos(wnd, 1 (* HWND_BOTTOM *), 0, 0, 0, 0,
					WinApi.SWP_NOSIZE + WinApi.SWP_NOMOVE + WinApi.SWP_NOACTIVATE)
			END
(*
		| WinApi.WM_GETMINMAXINFO:
			w := ThisWindow(wnd);
			IF (w # NIL) & (w.mw > 0) & (Windows.noResize IN w.flags) THEN
				pm := SYSTEM.VAL(WinApi.PMinMaxInfo, lParam);
				pm.maxTrackSize.x := w.mw;
				pm.maxTrackSize.y := w.mh;
				Controllers.ResetCurrentPath();
				RETURN 0
			END
*)
		ELSE
		END;
		Controllers.ResetCurrentPath();
		RETURN WinApi.DefMDIChildProcW(wnd, message, wParam, lParam)
	END DocWinHandler;

	PROCEDURE [2] DialogHandler (wnd: WinApi.HANDLE; message, wParam, lParam: INTEGER): INTEGER;
		VAR res, x, dx, dy: INTEGER; w: Window; ps: WinApi.PAINTSTRUCT;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		CASE message OF
		| WinApi.WM_CREATE:
			Kernel.Try(CreateDlg, wnd, wParam, lParam)
		| WinApi.WM_CLOSE:
			res := WinApi.SendMessageW(main, WinApi.WM_COMMAND, iClose, 0);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_DESTROY:
			w := ThisWindow(wnd);
			IF ~w.destroyed THEN
				w.destroyed := TRUE;
				w.Close
			END
		| WinApi.WM_SETFOCUS:
			w := ThisWindow(wnd);
			IF debug THEN
				Log.String("Dlg: SetFocus ");
				Log.IntForm(SYSTEM.VAL(INTEGER, w), 16, 8, "0", FALSE);
				Log.Char(" ");
				Log.IntForm(SYSTEM.VAL(INTEGER, fWindow), 16, 8, "0", FALSE);
				IF inPlace IN w.flags THEN Log.String(" ip") END;
				IF Windows.isTool IN w.flags THEN Log.String(" tool") END;
				Log.Ln
			END;
			Kernel.Try(ActivateWin, wnd, wParam, lParam);
			IF w.hook # NIL THEN w.hook.Focus(TRUE) END
		| WinApi.WM_KILLFOCUS:
			w := ThisWindow(wnd);
			IF debug THEN
				Log.String("Dlg: KillFocus ");
				Log.IntForm(SYSTEM.VAL(INTEGER, w), 16, 8, "0", FALSE);
				Log.Char(" ");
				Log.IntForm(SYSTEM.VAL(INTEGER, fWindow), 16, 8, "0", FALSE);
				IF inPlace IN w.flags THEN Log.String(" ip") END;
				IF Windows.isTool IN w.flags THEN Log.String(" tool") END;
				Log.Ln
			END;
			IF ~(inPlace IN w.flags) THEN Kernel.Try(DeactivateWin, wnd, wParam, lParam) END;
			IF w.hook # NIL THEN w.hook.Focus(FALSE) END
		| WinApi.WM_SIZE:
			w := ThisWindow(wnd);
			Kernel.Try(SizeWin, wnd, lParam MOD 65536 + w.dw, lParam DIV 65536 + w.dh)
		| WinApi.WM_HSCROLL:
			Kernel.Try(ScrollWin, wnd, wParam, 0);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_VSCROLL:
			Kernel.Try(ScrollWin, wnd, wParam, 1);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_MOUSEWHEEL:
			Kernel.Try(WheelScroll, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_PAINT:
			w := ThisWindow(wnd);
			w.GetSize(dx, dy);
			IF ~w.minimized THEN
				(* clip region must be reset to ensure correct ps.paint ! *)
				res := WinApi.SelectClipRgn(w.port(HostPorts.Port).dc, 0);
				x := WinApi.BeginPaint(w.wnd, ps);
				Kernel.Try(InvalidateWin, wnd, x, SYSTEM.ADR(ps.rcPaint));
				res := WinApi.EndPaint(w.wnd, ps);
				Kernel.Try(PaintWin, wnd, 0, 0);
				Controllers.ResetCurrentPath();
				RETURN 0
			END
		| WinApi.WM_LBUTTONDOWN, WinApi.WM_RBUTTONDOWN, WinApi.WM_MBUTTONDOWN:
			res := WinApi.SetCapture(wnd);
			Kernel.Try(HandleMouse, wnd, wParam + 256, lParam);
			IF WinApi.GetCapture() = wnd THEN res := WinApi.ReleaseCapture() END;
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_LBUTTONDBLCLK, WinApi.WM_RBUTTONDBLCLK, WinApi.WM_MBUTTONDBLCLK:
			res := WinApi.SetCapture(wnd);
			Kernel.Try(HandleMouse, wnd, wParam + (128 + 256), lParam);
			IF WinApi.GetCapture() = wnd THEN res := WinApi.ReleaseCapture() END;
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_LBUTTONUP, WinApi.WM_RBUTTONUP, WinApi.WM_MBUTTONUP:
			Kernel.Try(HandleMouse, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_MOUSEMOVE:
			Kernel.Try(HandleMouse, wnd, wParam + (2 * 256), lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_KEYDOWN:
			Kernel.Try(HandleKey, wnd, wParam, lParam);
			IF (wParam = WinApi.VK_TAB)
				OR (wParam = WinApi.VK_LEFT) OR (wParam = WinApi.VK_UP)
				OR (wParam = WinApi.VK_RIGHT) OR (wParam = WinApi.VK_DOWN)
			THEN
				res := WinApi.SendMessageW(wnd, 0127H (* WM_CHANGEUISTATE *),
							2 (* UIS_CLEAR *) + 10000H (* UISF_HIDEFOCUS *), 0)
			END
		| WinApi.WM_SYSKEYDOWN:
			IF wParam = WinApi.VK_MENU THEN
				res := WinApi.SendMessageW(wnd, 0127H (* WM_CHANGEUISTATE *),
							2 (* UIS_CLEAR *) + 10000H (* UISF_HIDEFOCUS *) + 20000H (* UISF_HIDEACCEL *), 0)
			END
		| WinApi.WM_CHAR, WinApi.WM_SYSCHAR:
			Kernel.Try(HandleChar, wnd, wParam, lParam);
			Controllers.ResetCurrentPath();
			RETURN 0
		| WinApi.WM_ACTIVATE:
			w := ThisWindow(wnd);
			IF w.hook # NIL THEN w.hook.Activate(wParam # 0) END
		ELSE
		END;
		Controllers.ResetCurrentPath();
		RETURN WinApi.DefWindowProcW(wnd, message, wParam, lParam)
	END DialogHandler;



	(** clipboard handling **)

	PROCEDURE OpenClipboard* (doc: Documents.Document);
	BEGIN
		NEW(cbViewer);
		dir.Open(cbViewer, doc, {Windows.neverDirty}, "Clipboard", NIL, "", NIL)
	END OpenClipboard;
(*
	PROCEDURE SizeClipboard* (wnd: WinApi.Handle; w, h: LONGINT);
		VAR res: LONGINT; dc: WinApi.Handle;
	BEGIN
		cbViewer.wnd := wnd;
		dc := WinApi.GetDC(wnd);
		cbViewer.port(HostPorts.Port).SetDC(dc, wnd);
(*
		res := WinApi.ShowScrollBar(wnd, WinApi.SBHorz, 1);
		res := WinApi.SetScrollRange(wnd, WinApi.SBHorz, 0, scrollRange, 1);
		res := WinApi.ShowScrollBar(wnd, WinApi.SBVert, 1);
		res := WinApi.SetScrollRange(wnd, WinApi.SBVert, 0, scrollRange, 1);
*)
		Kernel.Try(SizeWin, wnd, w, h);
(*
		cbViewer.UpdateScrollbars(FALSE);
*)
		res := WinApi.ReleaseDC(wnd, dc);
	END SizeClipboard;

	PROCEDURE PaintClipboard* (wnd: WinApi.Handle; VAR ps: WinApi.PaintStruct);
		VAR res: LONGINT; dc: WinApi.Handle;
	BEGIN
		cbViewer.wnd := wnd;
		res := WinApi.SelectClipRgn(ps.dc, 0);
		Kernel.Try(InvalidateWin, wnd, ps.dc, ADR(ps.paint));
		Kernel.Try(PaintWin, wnd, 0, 0)
	END PaintClipboard;

	PROCEDURE ScrollClipboard* (wnd: WinApi.Handle; code, pos: LONGINT; vertical: BOOLEAN);
		VAR res: LONGINT; dc: WinApi.Handle;
	BEGIN
		cbViewer.wnd := wnd;
(*
		dc := WinApi.GetDC(wnd);
		cbViewer.port(HostPorts.Port).SetDC(dc, wnd);
		cbViewer.Scroll(code, pos, vertical);
		cbViewer.SetSize(cbViewer.port.w, cbViewer.port.h);
		cbViewer.UpdateScrollbars(FALSE);
		res := WinApi.ReleaseDC(wnd, dc)
*)
	END ScrollClipboard;
*)

	(** miscellaneous **)

	PROCEDURE UpdateInfo;
		VAR res: INTEGER; str: ARRAY 256 OF CHAR;
	BEGIN
		IF memInStatus & ((alloc # Kernel.Allocated()) (* OR (total # Kernel.Used()) *) ) THEN
			alloc := Kernel.Allocated(); total := Kernel.Used();
			str := allocStr$; AppendInt(str, alloc, useSeparators); Append(str, byteStr);
(*
			Append(str, totalStr); AppendInt(str, total, useSeparators); Append(str, byteStr);
*)
			res := WinApi.SetWindowTextW(info, str)
		END
	END UpdateInfo;
(*
	PROCEDURE Check;
		VAR i: INTEGER; s: SET; obj: Kernel.Object;
	BEGIN
		IF Documents.dispMsgHeight = 0 THEN
			IF actMod = NIL THEN actMod := Kernel.modList END;
			IF actMod.refcnt >= 0 THEN
				obj := Kernel.ThisObject(actMod, "~");
				s := BITS(actMod.csize);
				i := ORD(s / SYSTEM.ROT(s, 7) / SYSTEM.ROT(s, -8));
				IF (obj = NIL) OR (obj.fprint # i) OR (obj.offs # ADR(obj^)) THEN
					SYSTEM.PUT(ADR(Documents.dispMsgHeight), (* LONG( *)(* LONG( *)1)
				END
			END;
			actMod := actMod.next
		END
	END Check;
*)
	PROCEDURE SetVisualScroll* (do: BOOLEAN);
	BEGIN
		IF visualScroll # do THEN
			visualScroll := do;
			HostRegistry.WriteBool("VisualScroll", do)
		END
	END SetVisualScroll;

	PROCEDURE Idle*;
		VAR w: Window; tick: Controllers.TickMsg; focus: BOOLEAN;
	BEGIN
		w := dir.Focus(FALSE);
		IF (w # NIL) & ~w.trapped THEN
			w.trapped := TRUE;
			IF w.frame # NIL THEN
				tick.tick := WinApi.GetTickCount();
				w.ForwardCtrlMsg(tick)
			END;
			w.trapped := FALSE
		END;
		focus := ScrollModPressed(); w := dir.First();
		WHILE w # NIL DO
			IF ~w.trapped THEN
				w.trapped := TRUE;
				w.UpdateScrollbars(focus & (w = fWindow));
				w.trapped := FALSE
			END;
			w := dir.Next(w)
		END;
		IF ~idleTraped THEN
			idleTraped := TRUE;
			IF WinApi.GetAsyncKeyState(1) >= 0 THEN activating := FALSE END;
			IF WinApi.GetAsyncKeyState(2) >= 0 THEN activating := FALSE END;
			UpdateInfo;
(*
			Check;
*)
			idleTraped := FALSE
		END;
		Services.actionHook.Step
	END Idle;

	PROCEDURE ActivateMain* (do: BOOLEAN);
	BEGIN
		mainActive := do
	END ActivateMain;


	PROCEDURE ResizeMainWindow* (type, w, h: INTEGER);
		VAR res, m: INTEGER;
	BEGIN
		m := w DIV 2;
		mainW := w - lBorder - rBorder;
		mainH := h - tBorder - bBorder;
		IF Dialog.showsStatus THEN DEC(mainH, statusH) END;
(*
		res := WinApi.DefFrameProcW(main, client, WinApi.WMSize, type, mainW + mainH * 65536);
*)
		IF debug THEN Log.String("resize main window"); Log.Ln END;
		res := WinApi.MoveWindow(client, lBorder, tBorder, mainW, mainH, 1);
		IF Dialog.showsStatus THEN
			IF memInStatus THEN
				res := WinApi.MoveWindow(status, 0, h - statusH, m + 1, statusH, 1);
				res := WinApi.MoveWindow(info, m, h - statusH, w - m, statusH, 1);
				res := WinApi.ShowWindow(info, 1)
			ELSE
				res := WinApi.MoveWindow(status, 0, h - statusH, w, statusH, 1);
				res := WinApi.ShowWindow(info, 0)
			END;
			res := WinApi.ShowWindow(status, 1)
		ELSE
			res := WinApi.ShowWindow(status, 0);
			res := WinApi.ShowWindow(info, 0)
		END
	END ResizeMainWindow;

	PROCEDURE SetMainBorderWidth* (l, t, r, b: INTEGER);
		VAR res: INTEGER; rect: WinApi.RECT;
	BEGIN
		lBorder := l; tBorder := t; rBorder := r; bBorder := b;
		res := WinApi.GetClientRect(main, rect);
		ResizeMainWindow(0, rect.right, rect.bottom)
	END SetMainBorderWidth;

	PROCEDURE GetMainBorder* (VAR l, t, r, b: INTEGER);
		VAR res: INTEGER; rect: WinApi.RECT;
	BEGIN
		res := WinApi.GetClientRect(main, rect);
		l := 0; t := 0; r := rect.right; b := rect.bottom;
		IF Dialog.showsStatus THEN DEC(b, statusH) END
	END GetMainBorder;

	PROCEDURE SaveWindowState*;
		VAR res: INTEGER; wp: WinApi.WINDOWPLACEMENT; list: ARRAY 10 OF INTEGER;
	BEGIN
		IF ~fullSize THEN
			wp.length := SIZE(WinApi.WINDOWPLACEMENT);
			res := WinApi.GetWindowPlacement(main, wp);
			IF res # 0 THEN
				list[0] := wp.rcNormalPosition.left;
				list[1] := wp.rcNormalPosition.top;
				list[2] := wp.rcNormalPosition.right;
				list[3] := wp.rcNormalPosition.bottom;
				list[4] := wp.ptMinPosition.x;
				list[5] := wp.ptMinPosition.y;
				list[6] := wp.ptMaxPosition.x;
				list[7] := wp.ptMaxPosition.y;
				list[8] := wp.showCmd;
				IF 1 IN wp.flags THEN list[9] := 1 ELSE list[9] := 0 END;
				HostRegistry.WriteIntList("MainWindow", list)
			ELSE
				res := WinApi.GetLastError(); HALT(120)
			END
		END
	END SaveWindowState;

	PROCEDURE LoadWindowState ();
		VAR res: INTEGER; wp: WinApi.WINDOWPLACEMENT; list: ARRAY 10 OF INTEGER;
			si: WinApi.STARTUPINFOW; rect: WinApi.RECT;
	BEGIN
		list[8] := 0; list[9] := 0;
		IF fullSize THEN
			wp.length := SIZE(WinApi.WINDOWPLACEMENT);
			wp.flags := WinApi.WPF_SETMINPOSITION + WinApi.WPF_RESTORETOMAXIMIZED;
			res := WinApi.SystemParametersInfoW(WinApi.SPI_GETWORKAREA, 0, SYSTEM.ADR(rect), 0);
			wp.rcNormalPosition.left := rect.left;
			wp.rcNormalPosition.top := rect.top;
			wp.rcNormalPosition.right := rect.right;
			wp.rcNormalPosition.bottom := rect.bottom;
			wp.ptMinPosition.x := rect.left;
			wp.ptMinPosition.y := rect.top;
			wp.ptMaxPosition.x := rect.left;
			wp.ptMaxPosition.y := rect.top;
			wp.showCmd := WinApi.SW_HIDE;
			showState := WinApi.SW_SHOW;
			res := WinApi.SetWindowPlacement(main, wp)
		ELSE
			HostRegistry.ReadIntList("MainWindow", list, res);
			IF res = 0 THEN
				wp.length := SIZE(WinApi.WINDOWPLACEMENT);
				wp.flags := {0}; 	(* set min pos *)
				wp.rcNormalPosition.left := list[0];
				wp.rcNormalPosition.top := list[1];
				wp.rcNormalPosition.right := list[2];
				wp.rcNormalPosition.bottom := list[3];
				wp.ptMinPosition.x := list[4];
				wp.ptMinPosition.y := list[5];
				IF list[8] = 0 THEN	(* v1.0..v1.1 *)
					wp.showCmd := list[6];
					wp.ptMaxPosition.x := -1;
					wp.ptMaxPosition.y := -1
				ELSE	(* v1.11.. *)
					wp.ptMaxPosition.x := list[6];
					wp.ptMaxPosition.y := list[7];
					wp.showCmd := list[8];
					IF list[9] > 0 THEN INCL(wp.flags, 1) END	(* restore maximized *)
				END;
				si.cb := SIZE(WinApi.STARTUPINFOW);
				WinApi.GetStartupInfoW(si);
				IF 0 IN si.dwFlags THEN	(* showWindow valid *)
					IF si.wShowWindow > 1 THEN wp.showCmd := si.wShowWindow
					ELSIF wp.showCmd = 2 THEN	(* restore minimized *)
						IF 1 IN wp.flags THEN wp.showCmd := 3 ELSE wp.showCmd := 9 END
					END
				END;
				showState := wp.showCmd; wp.showCmd := WinApi.SW_HIDE;
				res := WinApi.SetWindowPlacement(main, wp)
			END
		END
	END LoadWindowState;

	PROCEDURE CreateMainWindows* (menuBar, winMenu: WinApi.HANDLE; Handler: WinApi.WNDPROC);
		VAR res: INTEGER; class: WinApi.WNDCLASSW; ccs: WinApi.CLIENTCREATESTRUCT;
			dc: WinApi.HANDLE; tm: WinApi.TEXTMETRICW; str: ARRAY 256 OF CHAR; ver: ARRAY 8 OF CHAR;
	BEGIN
		(* init window classes *)
		class.hCursor := WinApi.LoadCursorW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDC_ARROW));
		class.hIcon := WinApi.LoadIconW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 1));
		IF (class.hIcon = 0) OR (class.hIcon = 1) THEN
			class.hIcon := WinApi.LoadIconW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDI_APPLICATION))
		END;
		class.lpszMenuName := NIL;
		class.lpszClassName := "Oberon App";
		class.hbrBackground := 0; (* 12 + 1; *)	(* application workspace color *)
		class.style := {3, 5};	(* doubleclicks, privat dc *)
		class.hInstance := instance;
		class.lpfnWndProc := Handler;
		class.cbClsExtra := 0;
		class.cbWndExtra := 4;
		res := WinApi.RegisterClassW(class);
		class.hCursor := 0;	(* no class cursor *)
		class.lpszMenuName := NIL;
		class.hbrBackground := 0;	(* no background *)
		class.lpszClassName := "Oberon Dlg";
		class.lpfnWndProc := DialogHandler;
		res := WinApi.RegisterClassW(class);
		class.lpszClassName := "Oberon Aux";
		class.lpfnWndProc := DocWinHandler;
		res := WinApi.RegisterClassW(class);
		docIcon := WinApi.LoadIconW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 2));
		IF (docIcon = 0) OR (docIcon = 1) THEN
			docIcon := WinApi.LoadIconW(0, SYSTEM.VAL(WinApi.PtrWSTR, WinApi.IDI_APPLICATION))
		END;
		dirtyIcon := WinApi.LoadIconW(instance, SYSTEM.VAL(WinApi.PtrWSTR, 5));
		IF (dirtyIcon = 0) OR (dirtyIcon = 1) THEN dirtyIcon := docIcon END;
		class.hIcon := docIcon;
		class.lpszClassName := "Oberon Doc";
		class.lpfnWndProc := DocWinHandler;
		res := WinApi.RegisterClassW(class);
		(* open main windows *)
		IF ~fullSize THEN
			main := WinApi.CreateWindowExW({8, 9}, "Oberon App", Dialog.appName,
													{16..19, 22, 23},	(* maxbox, minbox, frame, sysmenu, title, border *)
													WinApi.CW_USEDEFAULT, WinApi.CW_USEDEFAULT,
													WinApi.CW_USEDEFAULT, WinApi.CW_USEDEFAULT,
													0, menuBar, instance, 0)
		ELSE
			main := WinApi.CreateWindowExW({8, 9}, "Oberon App", Dialog.appName,
													WinApi.WS_MINIMIZEBOX + WinApi.WS_SYSMENU + WinApi.WS_CAPTION,
													WinApi.CW_USEDEFAULT, WinApi.CW_USEDEFAULT,
													WinApi.CW_USEDEFAULT, WinApi.CW_USEDEFAULT,
													0, menuBar, instance, 0)
		END;
		ccs.hWindowMenu := winMenu;
		ccs.idFirstChild := 10000;
		IF noClientScroll THEN
			client := WinApi.CreateWindowExW({}, "MDICLIENT", NIL,
													{25, 30},	(* clipchildren, child *)
													0, 0, 0, 0,
													main, 32767, instance, SYSTEM.ADR(ccs))
		ELSE
			client := WinApi.CreateWindowExW({}, "MDICLIENT", NIL,
													{20, 21, 25, 30},	(* scroll, clipchildren, child *)
													0, 0, 0, 0,
													main, 32767, instance, SYSTEM.ADR(ccs))
		END;
		status := WinApi.CreateWindowExW({}, "STATIC", NIL,
												{7, 23, 30},	(* left, noprefix, border, child *)
												0, 0, 0, 0,
												main, 32766, instance, 0);
		info := WinApi.CreateWindowExW({}, "STATIC", NIL,
											{7, 23, 30},	(* left, noprefix, border, child *)
											0, 0, 0, 0,
											main, 32765, instance, 0);
		res := WinApi.SendMessageW(status, WinApi.WM_SETFONT, font, 1);
		res := WinApi.SendMessageW(info, WinApi.WM_SETFONT, font, 1);
		dc := WinApi.GetDC(status);
		res := WinApi.GetTextMetricsW(dc, tm);
		res := WinApi.ReleaseDC(status, dc);
		statusH := tm.tmHeight;
		showState := WinApi.SW_SHOW;
		LoadWindowState();
		res := WinApi.ShowWindow(client, 1);
		IF Dialog.showsStatus THEN
			res := WinApi.ShowWindow(status, 1);
			IF memInStatus THEN
				res := WinApi.ShowWindow(info, 1)
			END
		END;
(*	moved to HostMenus
		res := WinApi.ShowWindow(main, WinApi.SW_SHOW);
		res := WinApi.UpdateWindow(main);
*)
		Kernel.mainWnd := main;
		dc := WinApi.GetDC(main);
		unit := Ports.inch DIV WinApi.GetDeviceCaps(dc, WinApi.LOGPIXELSY);
		res := WinApi.ReleaseDC(main, dc);

		Dialog.MapString(allocKey, allocStr);
		Dialog.MapString(totalKey, totalStr);
		Dialog.MapString(byteKey, byteStr);

		IF Dialog.appName = "BlackBox" THEN
			str := Dialog.appName$;
			ver := " x.y";
			ver[1] := CHR(Dialog.version DIV 10 + ORD("0"));
			ver[3] := CHR(Dialog.version MOD 10 + ORD("0"));
			Append(str, ver);
			Dialog.ShowStatus(str)
		END;

		HostMechanisms.InstallDropTarget(client, NIL)
	END CreateMainWindows;

	PROCEDURE Init;
		VAR res: INTEGER; d: Directory; dc: WinApi.HANDLE;
			Register: PROCEDURE(i: WinApi.HANDLE): INTEGER; p: WinApi.PtrWSTR;
	BEGIN
		instance := WinApi.GetModuleHandleW(NIL);
		dc := WinApi.GetDC(0);
		unit := Ports.inch DIV WinApi.GetDeviceCaps(dc, WinApi.LOGPIXELSY);
		res := WinApi.ReleaseDC(0, dc);
		scW := WinApi.GetSystemMetrics(0);	(* screen width *)
		scH := WinApi.GetSystemMetrics(1);	(* screen height *)
		NEW(d); d.l := -1; d.t := -1; d.r := -1; d.b := -1; dir := d; Windows.SetDir(d);
		NEW(winAnchor); winAnchor.next := NIL;	(* dummy header *)
		tWindow := NIL; fWindow := NIL; aWindow := NIL; bgWindow := NIL;
		noClientScroll := FALSE; fullSize := FALSE; noAppWin := FALSE;
		whiteBrush := WinApi.GetStockObject(WinApi.WHITE_BRUSH);
		nullPen := WinApi.GetStockObject(WinApi.NULL_PEN);
		font := WinApi.GetStockObject(WinApi.DEFAULT_GUI_FONT);
		cbValid := FALSE; newNumber := 1;
		visualScroll := TRUE; HostRegistry.ReadBool("VisualScroll", visualScroll, res);
	END Init;

BEGIN
	Init
END HostWindows.
