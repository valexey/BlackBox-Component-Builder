MODULE HostDialog;
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
		WinApi, WinDlg,  SYSTEM,
		Kernel, Strings, Dates, Fonts, Ports, Files, Stores, Views, Controllers, Properties,
		Printers, Dialog, Windows, Converters, Services,
		HostFonts, HostFiles, HostRegistry, HostPorts, HostWindows, HostPrinters,
		StdCmds,
		HostCFrames (* don't remove *);


	CONST
		(** CloseDialog res **)
		save* = 1; cancel* = 2;

		dirtyString = "#Host:SaveChanges";


	TYPE
		Preview = POINTER TO RECORD (Views.View) END;

		UpdateMsg = RECORD (Views.Message) END;

		DatesHook = POINTER TO RECORD (Dates.Hook) END;
		DialogHook = POINTER TO RECORD (Dialog.GetHook) END;
		ShowHook = POINTER TO RECORD (Dialog.ShowHook) END;
		GetSpecHook = POINTER TO RECORD (Views.GetSpecHook) END;
		LanguageHook = POINTER TO RECORD (Dialog.LanguageHook) END;


	VAR
		window-: Windows.Window;	(** window created/selected by most recent Old or Open **)
		oldWindow-: BOOLEAN;	(** most recent Old or Open selected existing window **)

		osVersion-: INTEGER;
		extType*: Files.Type;

		impType*: RECORD
			list*: Dialog.List;
			done: BOOLEAN
		END;

		setup*: RECORD
			decorate*: BOOLEAN;
			landscape*: BOOLEAN;
			left*, right*, top*, bottom*: REAL;
			w, h: INTEGER;
			hs, vs: REAL
		END;

		prefs*: RECORD
			useTTMetric*: BOOLEAN;
			visualScroll*: BOOLEAN;
			statusbar*: INTEGER;
			thickCaret*: BOOLEAN;
			caretPeriod*: INTEGER
		END;

		ShowOleStatus*: PROCEDURE (w: Windows.Window; s: ARRAY OF CHAR);

		actualPath*: ARRAY 256 OF CHAR;

		fn: WinDlg.OPENFILENAMEW;
		ftype: ARRAY 16 OF CHAR;
		filter: ARRAY 2048 OF CHAR;
		fname: ARRAY 260 OF CHAR;
		prt: WinDlg.PRINTDLGW;
		font: WinDlg.CHOOSEFONTW;
		logFont: WinApi.LOGFONTW;
		col: WinDlg.CHOOSECOLORW;
		customColors: ARRAY 16 OF Ports.Color;
		prefFName, prefDName: Fonts.Typeface;
		prefFSize, prefDSize, prefDWght: INTEGER;
		prefDStyle: SET;
		typeList: ARRAY 64 OF Converters.Converter;
		defConv: Converters.Converter;
		all: Converters.Converter;

		dialogHook: DialogHook;


	PROCEDURE Append (VAR s: ARRAY OF CHAR; VAR idx: INTEGER; IN t: ARRAY OF CHAR);
		VAR len, i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := t[0]; len := LEN(s) - 1;
		WHILE (idx < len) & (ch # 0X) DO s[idx] := ch; INC(idx); INC(i); ch := t[i] END;
		s[idx] := 0X
	END Append;

	PROCEDURE SAppend (VAR s: ARRAY OF CHAR; VAR idx: INTEGER; IN t: ARRAY OF CHAR);
		VAR len, i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := t[0]; len := LEN(s) - 1;
		WHILE (idx < len) & (ch # 0X) DO s[idx] := ch; INC(idx); INC(i); ch := t[i] END;
		s[idx] := 0X
	END SAppend;

	PROCEDURE MapConv (cmd: ARRAY OF CHAR; VAR name: ARRAY OF CHAR);
		VAR i: INTEGER; sub, str: Dialog.String;
	BEGIN
		str := cmd$; i := 0;
		WHILE str[i] >= "0" DO INC(i) END;
		str[i] := 0X;
		Kernel.SplitName(str, sub, str);
		IF sub = "" THEN sub := "System" END;
		str := "#"; i := 1;
		Append(str, i, sub);
		Append(str, i, ":");
		Append(str, i, cmd);
		Dialog.MapString(str, name)
	END MapConv;

	PROCEDURE WaitDialogClose;
	(* simulates modal dialog *)
		VAR res: INTEGER; w: HostWindows.Window; msg: WinApi.MSG;
	BEGIN
		w := HostWindows.dir.First();
		REPEAT
			IF WinApi.PeekMessageW(msg, 0, 0, 0, 1) # 0 THEN
				IF (msg.hwnd = w.wnd) OR (msg.message = WinApi.WM_PAINT) THEN
					res := WinApi.TranslateMessage(msg);
					res := WinApi.DispatchMessageW(msg)
				END
			END
		UNTIL w.wnd = 0
	END WaitDialogClose;

	PROCEDURE ShowParamStatus* (IN str, p0, p1, p2: ARRAY OF CHAR);
		VAR res: INTEGER; st: ARRAY 513 OF CHAR; w: Windows.Window;
	BEGIN
		Dialog.MapParamString(str, p0, p1, p2, st);
		w := Windows.dir.Focus(Controllers.targetPath);
		IF (w # NIL) & (HostWindows.inPlace IN w.flags) & (ShowOleStatus # NIL) THEN
			ShowOleStatus(w, " " + st)
		ELSE
			st := " " + st;
			res := WinApi.SetWindowTextW(HostWindows.status, st);
			res := WinApi.UpdateWindow(HostWindows.status)
		END
	END ShowParamStatus;

	PROCEDURE ShowParamMsg* (IN str, p0, p1, p2: ARRAY OF CHAR);
		VAR res: INTEGER; st: ARRAY 512 OF CHAR;
	BEGIN
		ASSERT(str # "", 20);
(*
		IF Dialog.showsStatus THEN
			ShowParamStatus(str, p0, p1, p2)
		ELSE
*)
			Dialog.MapParamString(str, p0, p1, p2, st);
			res := WinApi.MessageBoxW(HostWindows.ActualWnd(), st, Dialog.appName, {4, 5})
(*
		END
*)
	END ShowParamMsg;

	PROCEDURE (h: ShowHook) ShowParamMsg (IN str, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		ShowParamMsg(str, p0, p1, p2)
	END ShowParamMsg;

	PROCEDURE (h: ShowHook) ShowParamStatus (IN str, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		ShowParamStatus(str, p0, p1, p2)
	END ShowParamStatus;

	(** general OK dialog **)

	PROCEDURE (hook: DialogHook) GetOK (IN str, p0, p1, p2: ARRAY OF CHAR; form: SET; OUT res: INTEGER);
		VAR r: INTEGER; st: ARRAY 512 OF CHAR; type: SET;
	BEGIN
		ASSERT(str # "", 20);
		Dialog.MapParamString(str, p0, p1, p2, st);
		IF Dialog.yes IN form THEN
			IF Dialog.cancel IN form THEN type := WinApi.MB_YESNOCANCEL + WinApi.MB_ICONQUESTION
			ELSE type := WinApi.MB_YESNO + WinApi.MB_ICONQUESTION
			END
		ELSE
			IF Dialog.cancel IN form THEN type := WinApi.MB_OKCANCEL + WinApi.MB_ICONWARNING
			ELSE type := WinApi.MB_OK + WinApi.MB_ICONWARNING
			END
		END;
		r := WinApi.MessageBoxW(HostWindows.ActualWnd(), st, Dialog.appName, type);
		IF r = WinApi.IDOK THEN res := Dialog.ok
		ELSIF r = WinApi.IDCANCEL THEN res := Dialog.cancel
		ELSIF r = WinApi.IDYES THEN res := Dialog.yes
		ELSIF r = WinApi.IDNO THEN res := Dialog.no
		ELSE res := 0
		END
	END GetOK;


	(** time **)

	PROCEDURE (hook: DatesHook) GetTime (OUT d: Dates.Date; OUT t: Dates.Time);
		VAR dt: WinApi.SYSTEMTIME;
	BEGIN
		WinApi.GetLocalTime(dt);
		d.year := dt.wYear; d.month := dt.wMonth; d.day := dt.wDay;
		t.hour := dt.wHour; t.minute := dt.wMinute; t.second := dt.wSecond
	END GetTime;

	PROCEDURE (hook: DatesHook) GetUTCTime (OUT d: Dates.Date; OUT t: Dates.Time);
		VAR dt: WinApi.SYSTEMTIME;
	BEGIN
		WinApi.GetSystemTime(dt);
		d.year := dt.wYear; d.month := dt.wMonth; d.day := dt.wDay;
		t.hour := dt.wHour; t.minute := dt.wMinute; t.second := dt.wSecond
	END GetUTCTime;

	PROCEDURE (hook: DatesHook) GetUTCBias (OUT bias: INTEGER);
		VAR res: INTEGER; info: WinApi.TIME_ZONE_INFORMATION;
	BEGIN
		bias := 0;
		res := WinApi.GetTimeZoneInformation(info);
		IF res # -1 THEN
			IF BITS(res) = WinApi.TIME_ZONE_ID_DAYLIGHT THEN bias := info.Bias + info.DaylightBias
			ELSE bias := info.Bias + info.StandardBias
			END
		END
	END GetUTCBias;


	(** import type dialog **)

	PROCEDURE ImpOk*;
	BEGIN
		impType.done := TRUE;
		StdCmds.CloseDialog
	END ImpOk;


	(** file dialogs **)

	PROCEDURE (hook: DialogHook) GetIntSpec (
		IN defType: Files.Type; VAR loc: Files.Locator; OUT name: Files.Name
	);
	(* asks user for a file name (for file internalization) *)
		VAR res, i, j: INTEGER; lfn: ARRAY 260 OF CHAR;
	BEGIN
		(* set filter *)
		ftype := defType$; i := 0;
		IF ftype = "" THEN ftype := "odc" END;
		IF ftype # "*" THEN
			SAppend(filter, i, "*."); SAppend(filter, i, ftype); INC(i);
			SAppend(filter, i, "*."); SAppend(filter, i, ftype); INC(i)
		ELSE ftype := ""
		END;
		SAppend(filter, i, "*.*"); INC(i);
		SAppend(filter, i, "*.*"); INC(i);
		filter[i] := 0X;
		(* open dialog *)
		fn.nFilterIndex := 1;
		fname := "";
		IF loc # NIL THEN	(* set old dir *)
			actualPath := loc(HostFiles.Locator).path$; i := 0
(*
			WHILE actualPath[i] # 0X DO INC(i) END;
			actualPath[i] := "\"; actualPath[i+1] := 0X
*)
		END;
		fn.Flags := {2, 3, 11, 12, 19};	(* hideReadOnly, noChangeDir, pathMustExist, fileMustExist, new look *)
		fn.hwndOwner := HostWindows.ActualWnd();
		IF WinDlg.GetOpenFileNameW(fn) # 0 THEN
			i := 0; j := fn.nFileOffset;
			WHILE fname[j] # 0X DO name[i] := fname[j]; INC(i); INC(j) END;
			name[i] := 0X; fname[fn.nFileOffset - 1] := 0X;
			actualPath := fname$; lfn := fname$;
			loc := HostFiles.NewLocator(lfn)
		ELSE
			loc := NIL;
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END;
		res := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END GetIntSpec;

	PROCEDURE GetIntSpec* (VAR loc: Files.Locator; VAR name: Files.Name; VAR conv: Converters.Converter);
	(* asks user for a file name (for file internalization) *)
		VAR res, i, j, n: INTEGER; t: Dialog.String; c: Converters.Converter; lfn: ARRAY 260 OF CHAR;
	BEGIN
		(* set filter *)
		ftype := "odc";
		IF loc # NIL THEN
			defConv := conv;
			IF conv # NIL THEN ftype := conv.fileType$ END
		END;
		typeList[0] := Converters.list;	(* document converter *)
		Dialog.MapString("#Host:Document", t);
		i := 0; SAppend(filter, i, t); SAppend(filter, i, " (*.odc)"); INC(i);
		SAppend(filter, i, "*.odc"); INC(i); n := 1;
		fn.nFilterIndex := 1;
		c := Converters.list;
		WHILE c # NIL DO
			IF (c.imp # "") & (c.fileType # "odc") THEN
				typeList[n] := c;
				MapConv(c.imp, t); SAppend(filter, i, t);
				SAppend(filter, i, " (*."); SAppend(filter, i, c.fileType);
				SAppend(filter, i, ")"); INC(i);
				SAppend(filter, i, "*."); SAppend(filter, i, c.fileType); INC(i);
				IF defConv = c THEN fn.nFilterIndex := n + 1 END;
				INC(n)
			END;
			c := c.next
		END;
		Dialog.MapString("#Host:AllFiles", t);
		SAppend(filter, i, t); SAppend(filter, i, " (*.*)"); INC(i);
		SAppend(filter, i, "*.*"); INC(i);
		filter[i] := 0X;
		IF defConv = all THEN fn.nFilterIndex := n + 1 END;
		(* open dialog *)
		IF loc # NIL THEN fname := name$
		ELSE fname := ""
		END;
		IF loc # NIL THEN	(* set old dir *)
			actualPath := loc(HostFiles.Locator).path$; i := 0;
			WHILE actualPath[i] # 0X DO INC(i) END;
			actualPath[i] := "\"; actualPath[i+1] := 0X
		END;
		fn.Flags := {2, 3, 11, 12, 19};	(* hideReadOnly, noChangeDir, pathMustExist, fileMustExist, new look *)
		fn.hwndOwner := HostWindows.ActualWnd();
		IF WinDlg.GetOpenFileNameW(fn) # 0 THEN
			i := 0; j := fn.nFileOffset;
			WHILE fname[j] # 0X DO name[i] := fname[j]; INC(i); INC(j) END;
			name[i] := 0X; fname[fn.nFileOffset - 1] := 0X;
			actualPath := fname$; lfn := fname$;
			loc := HostFiles.NewLocator(lfn);
			IF fn.nFilterIndex <= n THEN
				conv := typeList[fn.nFilterIndex - 1];
				defConv := conv
			ELSE	(* ask for desired file type *)
				impType.list.SetLen(n);
				Dialog.MapString("#Host:Document", t); impType.list.SetItem(0, t);
				i := 1;
				WHILE i < n DO
					MapConv(typeList[i].imp, t); impType.list.SetItem(i, t);
					INC(i)
				END;
				impType.list.index := 0;
				impType.done := FALSE;
				Dialog.Call("StdCmds.OpenToolDialog('HostDialog.impType', '#Host:OpenFile')", " ", res);
				WaitDialogClose;
				IF impType.done THEN
					conv := typeList[impType.list.index]
				ELSE
					loc := NIL; conv := NIL
				END;
				defConv := all
			END
		ELSE
			loc := NIL; conv := NIL;
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END;
		res := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END GetIntSpec;

	PROCEDURE (h: GetSpecHook) GetIntSpec (VAR loc: Files.Locator; VAR name: Files.Name;
															VAR conv: Converters.Converter);
	BEGIN
		GetIntSpec(loc, name, conv)
	END GetIntSpec;

	PROCEDURE [2] HookProc (wnd: WinApi.HANDLE; msg, wParam, lParam: INTEGER): INTEGER;
	(* 06.11.02. not needed anymore.  *)
		VAR i, j: INTEGER; s: ARRAY 256 OF CHAR; t: ARRAY 8 OF CHAR; edit: WinApi.HANDLE;
	BEGIN
		IF (msg = WinApi.WM_COMMAND) & (lParam # 0) & (wParam = 10470H) THEN
			i := WinApi.GetWindowTextW(lParam, s, LEN(s));
			DEC(i);
			IF s[i] = ")" THEN DEC(i) END;
			j := i;
			WHILE (j >= 0) & (s[j] # ".") DO DEC(j) END;
			IF (j > 0) & (s[j-1] = "*") & (j >= i - 3) & (j < i) THEN
				t[0] := s[j+1]; t[1] := s[j+2]; t[2] := s[j+3]; t[i-j] := 0X;
				edit := WinApi.GetDlgItem(WinApi.GetParent(lParam), 480H);
				IF edit # 0 THEN
					i := WinApi.GetWindowTextW(edit, s, LEN(s)); j := i - 1;
					WHILE (j >= 0) & (s[j] # ".") DO DEC(j) END;
					IF j < 0 THEN j := i END;
					s[j] := "."; s[j+1] := t[0]; s[j+2] := t[1]; s[j+3] := t[2]; s[j+4] := 0X;
					i := WinApi.SetWindowTextW(edit, s)
				END
			END
		END;
		RETURN 0
	END HookProc;

	PROCEDURE (hook: DialogHook) GetExtSpec (
		IN default: Files.Name; IN defType: Files.Type; VAR loc: Files.Locator; OUT name: Files.Name
	);
	(* ask user for a file name (for file externalization) *)
		VAR res, i, j: INTEGER; lfn: ARRAY 260 OF CHAR;
	BEGIN
		(* set filter *)
		ftype := defType$; i := 0;
		IF ftype = "" THEN ftype := "odc" END;
		IF ftype # "*" THEN
			SAppend(filter, i, "*."); SAppend(filter, i, ftype); INC(i);
			SAppend(filter, i, "*."); SAppend(filter, i, ftype); INC(i)
		ELSE ftype := ""
		END;
		SAppend(filter, i, "*.*"); INC(i);
		SAppend(filter, i, "*.*"); INC(i);
		filter[i] := 0X;
		(* open dialog *)
		fn.nFilterIndex := 1;
		fname := default$;
		IF loc # NIL THEN	(* set old dir *)
			actualPath := loc(HostFiles.Locator).path$; i := 0
(*
			WHILE actualPath[i] # 0X DO INC(i) END;
			actualPath[i] := "\"; actualPath[i+1] := 0X
*)
		END;
		fn.Flags := {1, 2, 3, 11, 19};	(* overwritePrompt, hideReadOnly, noChangeDir, pathMustExist, new look *)
		fn.hwndOwner := HostWindows.ActualWnd();
		res := WinDlg.GetSaveFileNameW(fn);
		IF (res = 0) & (WinDlg.CommDlgExtendedError() = 3002H) THEN
			fname := "";
			res := WinDlg.GetSaveFileNameW(fn)
		END;
		IF res # 0 THEN
			i := 0; j := fn.nFileOffset;
			WHILE fname[j] # 0X DO name[i] := fname[j]; INC(i); INC(j) END;
			name[i] := 0X; fname[fn.nFileOffset - 1] := 0X;
			actualPath := fname$; lfn := fname$;
			loc := HostFiles.NewLocator(lfn)
		ELSE
			loc := NIL;
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END;
		res := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END GetExtSpec;

	PROCEDURE GetExtSpec* (
		s: Stores.Store; VAR loc: Files.Locator; VAR name: Files.Name; VAR conv: Converters.Converter
	);
	(* ask user for a file name (for file externalization) *)
		VAR res, i, j, n: INTEGER; t: ARRAY 64 OF CHAR; c: Converters.Converter;
			lfn: ARRAY 260 OF CHAR;
	BEGIN
		(* set filter *)
		IF conv # NIL THEN ftype := conv.fileType$ ELSE ftype := "odc" END;
		typeList[0] := Converters.list;	(* document converter *)
		c := Converters.list; fn.nFilterIndex := 1;
		Dialog.MapString("#Host:Document", t);
		i := 0; SAppend(filter, i, t); SAppend(filter, i, " (*.odc)"); INC(i);
		SAppend(filter, i, "*.odc"); INC(i); n := 1;
		WHILE c # NIL DO
			IF (c.exp # "") & ((c.storeType = "") OR Services.Is(s, c.storeType)) & (c.fileType # "odc") THEN
				typeList[n] := c;
				MapConv(c.exp, t); SAppend(filter, i, t);
				SAppend(filter, i, " (*."); SAppend(filter, i, c.fileType);
				SAppend(filter, i, ")"); INC(i);
				SAppend(filter, i, "*."); SAppend(filter, i, c.fileType); INC(i);
				IF c = conv THEN fn.nFilterIndex := n + 1 END;
				INC(n)
			END;
			c := c.next
		END;
		filter[i] := 0X;
		(* open dialog *)
		fname := name$; i := 0;
		(* overwritePrompt, hideReadOnly, noChangeDir, (* en hook, *) pathMustExist *)
		fn.Flags := {1, 2, 3, (* 5 ,*) 11, 19};
		WHILE fname[i] # 0X DO INC(i) END;
		WHILE (i > 0) & (fname[i] # ".") DO DEC(i) END;
		IF i > 0 THEN
			IF (conv # NIL) & (conv.exp = "") THEN
				ftype := "odc"; fname[i] := 0X
			ELSE
				j := 0;
				WHILE (ftype[j] # 0X) & (CAP(fname[i+j+1]) = CAP(ftype[j])) DO INC(j) END;
				IF fname[i+j+1] = ftype[j] THEN fname[i] := 0X END
			END
		END;
		IF loc # NIL THEN	(* set old dir *)
			actualPath := loc(HostFiles.Locator).path$; i := 0;
			WHILE actualPath[i] # 0X DO INC(i) END;
			actualPath[i] := "\"; actualPath[i+1] := 0X
		END;
		fn.hwndOwner := HostWindows.ActualWnd();
		res := WinDlg.GetSaveFileNameW(fn);
		IF (res = 0) & (WinDlg.CommDlgExtendedError() = 3002H) THEN
			fname := "";
			res := WinDlg.GetSaveFileNameW(fn)
		END;
		IF res # 0 THEN
			i := 0; j := fn.nFileOffset;
			WHILE fname[j] # 0X DO name[i] := fname[j]; INC(i); INC(j) END;
			name[i] := 0X; fname[fn.nFileOffset - 1] := 0X;
			actualPath := fname$; lfn := fname$;
			loc := HostFiles.NewLocator(lfn);
			conv := typeList[fn.nFilterIndex - 1]
		ELSE
			loc := NIL;
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END;
		res := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END GetExtSpec;

	PROCEDURE (h: GetSpecHook) GetExtSpec (
		s: Stores.Store; VAR loc: Files.Locator; VAR name: Files.Name; VAR conv: Converters.Converter
	);
	BEGIN
		GetExtSpec(s, loc, name, conv)
	END GetExtSpec;

	(* printer dialogs *)

	(* page setup previewer view *)

	PROCEDURE (v: Preview) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		CONST scale = 16; rmm = Ports.mm DIV scale; size = 460 * rmm;
		VAR u, w, h, x, y, uu: INTEGER;
	BEGIN
		u := f.unit;
		IF Dialog.metricSystem THEN uu := 10 * rmm ELSE uu := Ports.inch DIV scale END;
		w := setup.w DIV scale;
		h := setup.h DIV scale;
		x := (size - w) DIV 2;
		y := (size - h) DIV 2;
		l := SHORT(ENTIER(setup.left * uu));
		t := SHORT(ENTIER(setup.top * uu));
		r := SHORT(ENTIER(setup.right * uu));
		b := SHORT(ENTIER(setup.bottom * uu));
		f.DrawRect(x, y, x + w, y + h, Ports.fill, Ports.background);
		f.DrawRect(x - u, y - u, x + w + u, y + h + u, 0, Ports.defaultColor);
		IF setup.decorate THEN
			IF t < 14 * rmm THEN t := 14 * rmm END;
			f.DrawRect(x + l, y + 10 * rmm, x + l + 20 * rmm, y + 10 * rmm + u, Ports.fill, Ports.defaultColor);
			f.DrawRect(x + w - r - 8 * rmm, y + 10 * rmm, x + w - r, y + 10 * rmm + u, Ports.fill, Ports.defaultColor)
		END;
		IF (w - r > l) & (h - b > t) THEN
			f.DrawRect(x + l, y + t, x + w - r, y + h - b, 0, Ports.defaultColor)
		END
	END Restore;

	PROCEDURE (v: Preview) HandleViewMsg (f: Views.Frame; VAR msg: Views.Message);
	BEGIN
		WITH msg: UpdateMsg DO
			Views.Update(v, Views.keepFrames)
		ELSE
		END
	END HandleViewMsg;

	PROCEDURE Deposit*;
		VAR v: Preview;
	BEGIN
		NEW(v); Views.Deposit(v)
	END Deposit;

	(* page setup dialog *)

	PROCEDURE SetupNotify* (op, from, to: INTEGER);
		VAR msg: UpdateMsg; t: INTEGER;
	BEGIN
		IF op = Dialog.changed THEN
			IF setup.landscape # (setup.w > setup.h) THEN
				t := setup.w; setup.w := setup.h; setup.h := t
			END;
			Views.Omnicast(msg);
			Dialog.Update(setup)
		END
	END SetupNotify;

	PROCEDURE SetupOk*;
		VAR win: Windows.Window; w, h, l, t, r, b, uu: INTEGER;
	BEGIN
		win := Windows.dir.Focus(Controllers.targetPath);
		IF win # NIL THEN
			IF Dialog.metricSystem THEN uu := 10 * Ports.mm ELSE uu := Ports.inch END;
			w := setup.w; h := setup.h;
			l := SHORT(ENTIER(setup.left * uu));
			t := SHORT(ENTIER(setup.top * uu));
			r := w - SHORT(ENTIER(setup.right * uu));
			b := h - SHORT(ENTIER(setup.bottom * uu));
			IF (0 <= l) & (l < r) & (r <= w) & (0 <= t) & (t < b) & (b <= h) THEN
				win.doc.SetPage(w, h, l, t, r, b, setup.decorate);
				StdCmds.CloseDialog
			ELSE
				Dialog.Beep
			END
		END
	END SetupOk;

	PROCEDURE InitPageSetup*;
		VAR win: Windows.Window; w, h, pw, ph, l, t, r, b, uu: INTEGER; p: Printers.Printer;
	BEGIN
		win := Windows.dir.Focus(Controllers.targetPath);
		IF win # NIL THEN
			IF Dialog.metricSystem THEN uu := Ports.mm DIV 10 ELSE uu := Ports.inch DIV 100 END;
			win.doc.PollPage(w, h, l, t, r, b, setup.decorate);
			p := Printers.dir.Current();
			IF p # NIL THEN HostPrinters.GetPage(p, pw, ph);
				IF (pw > ph) = (w > h) THEN w := pw; h := ph ELSE w := ph; h := pw END
			END;
			r := w - r; b := h - b;
			setup.left := l DIV uu / 100;
			setup.right := r DIV uu / 100;
			setup.top := t DIV uu / 100;
			setup.bottom := b DIV uu / 100;
			setup.w := w; setup.h := h;
			setup.hs := setup.right + setup.left;
			setup.vs := setup.bottom + setup.top;
			setup.landscape := w > h
		END
	END InitPageSetup;


	PROCEDURE PrintDialog* (
		hasSelection: BOOLEAN; VAR from, to, copies: INTEGER; VAR selection: BOOLEAN
	);
		VAR res: INTEGER;
	BEGIN
		prt.Flags := {18 (*, 20 *)};	(* use dev mode copies, hide print to file *)
		IF ~hasSelection THEN INCL(prt.Flags, 2) END;	(* no selection *)
		prt.nCopies := 1;
		prt.hwndOwner := HostWindows.ActualWnd();
		prt.nFromPage := 1; prt.nToPage := 1;
		res := WinDlg.PrintDlgW(prt);
		IF (res = 0) & (WinDlg.CommDlgExtendedError() = 4106) THEN
			prt.hDevMode := 0;
			res := WinDlg.PrintDlgW(prt)
		END;
		IF (res = 0) & (WinDlg.CommDlgExtendedError() = 4108) THEN
			prt.hDevMode := 0; prt.hDevNames := 0;
			res := WinDlg.PrintDlgW(prt)
		END;
		HostPrinters.SetCurrent(prt.hDevNames, prt.hDevMode);
		IF res # 0 THEN
			IF 0 IN prt.Flags THEN selection := TRUE; from := 0; to := 0	(* print selection *)
			ELSIF 1 IN prt.Flags THEN selection := FALSE; from := prt.nFromPage - 1; to := prt.nToPage - 1 (* print pages *)
			ELSE selection := FALSE; from := 0; to := 32767	(* print all *)
			END;
			copies := prt.nCopies
		ELSE
			copies := 0;
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END
	END PrintDialog;

	PROCEDURE PrintSetup*;
		VAR res: INTEGER; pt: Printers.Printer;
	BEGIN
		pt := Printers.dir.Current();
		IF pt # NIL THEN
			pt.SetOrientation(setup.landscape);
			HostPrinters.GetCurrent(prt.hDevNames, prt.hDevMode)
		END;
		prt.Flags := {6};	(* PrintSetup *)
		prt.hwndOwner := HostWindows.ActualWnd();
		res := WinDlg.PrintDlgW(prt);
		IF (res = 0) & (WinDlg.CommDlgExtendedError() = 4108) THEN
			prt.hDevMode := 0; prt.hDevNames := 0;
			res := WinDlg.PrintDlgW(prt)
		END;
		HostPrinters.SetCurrent(prt.hDevNames, prt.hDevMode);
		pt := Printers.dir.Current();
		IF pt # NIL THEN
			HostPrinters.GetPage(pt, setup.w, setup.h);
			setup.landscape := setup.w > setup.h
		END;
		SetupNotify(Dialog.changed, 0, 0)
	END PrintSetup;


	PROCEDURE CloseDialog* (w: Windows.Window; quit: BOOLEAN; VAR res: INTEGER);
		VAR r: INTEGER; title: Views.Title; text: ARRAY 256 OF CHAR;
	BEGIN
		w.GetTitle(title);
		Dialog.MapParamString(dirtyString, title, 0DX, 0DX, text);
		r := WinApi.MessageBoxW(w(HostWindows.Window).wnd, text, Dialog.appName, {0, 1, 5});
		IF r = 6 THEN res := save
		ELSIF r = 2 THEN res := cancel
		ELSE res := 0
		END;
		r := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END CloseDialog;

	PROCEDURE (hook: DialogHook) GetColor (in: Ports.Color; OUT out: Ports.Color; OUT set: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		col.rgbResult := in;
		IF col.rgbResult = Ports.defaultColor THEN col.rgbResult := HostPorts.textCol END;
		col.hwndOwner := HostWindows.ActualWnd();
		set := WinDlg.ChooseColorW(col) # 0;
		out := col.rgbResult;
		IF ~set THEN
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END;
		res := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END GetColor;

	PROCEDURE ColorDialog*;
	(* open color dialog and set selection to choosen color *)
		VAR set: BOOLEAN; p: Properties.StdProp; col: Ports.Color;
	BEGIN
		Properties.CollectStdProp(p);
		IF ~(Properties.color IN p.known) THEN p.color.val := Ports.black END;
		(* ColorDialog0(p.color.val, col, set); *)
		dialogHook.GetColor(p.color.val, col, set);
		IF set THEN StdCmds.Color(col) END
	END ColorDialog;

	PROCEDURE FontDialog0 (full: BOOLEAN; VAR typeface: ARRAY OF CHAR; VAR size: INTEGER;
		VAR color: Ports.Color; VAR weight: INTEGER; VAR style: SET; VAR set: BOOLEAN
	);
		VAR res: INTEGER;
	BEGIN
		logFont.lfHeight := -((size + HostWindows.unit DIV 2) DIV HostWindows.unit);
		logFont.lfWeight := weight;
		IF Fonts.italic IN style THEN logFont.lfItalic := 1X ELSE logFont.lfItalic := 0X END;
		IF Fonts.underline IN style THEN logFont.lfUnderline := 1X ELSE logFont.lfUnderline := 0X END;
		IF Fonts.strikeout IN style THEN logFont.lfStrikeOut := 1X ELSE logFont.lfStrikeOut := 0X END;
		logFont.lfFaceName := typeface$;
		font.hwndOwner := HostWindows.ActualWnd();
		IF full THEN
			font.Flags := {0, 6, 8, 11, 16}	(* screenFonts, initToLogFont, effects, noVectorFonts, forceFontExist *)
		ELSE
			font.Flags := {0, 6, 11, 20, 21}	(* screenFonts, initToLogFont, noVectorFonts, no style, no size *)
		END;
		IF color = Ports.defaultColor THEN font.rgbColors := HostPorts.textCol ELSE font.rgbColors := color END;
		set := WinDlg.ChooseFontW(font) # 0;
		IF set THEN
			typeface := logFont.lfFaceName$;
			size := font.iPointSize * Ports.point DIV 10;
			ASSERT((size + HostWindows.unit DIV 2) DIV HostWindows.unit = ABS(logFont.lfHeight), 120);
			weight := logFont.lfWeight;
			style := {};
			IF logFont.lfItalic # 0X THEN INCL(style, Fonts.italic) END;
			IF logFont.lfUnderline # 0X THEN INCL(style, Fonts.underline) END;
			IF logFont.lfStrikeOut # 0X THEN INCL(style, Fonts.strikeout) END;
			IF font.rgbColors = HostPorts.textCol THEN color := Ports.defaultColor ELSE color := font.rgbColors END
		ELSE
			res := WinDlg.CommDlgExtendedError();
			ASSERT(res = 0, 100)
		END;
		res := WinApi.SetCursor(HostPorts.cursors[HostPorts.busyCursor])
	END FontDialog0;

	PROCEDURE FontDialog*;
	(** open font dialog and set selection to choosen attributes **)
		VAR set: BOOLEAN; p, p0: Properties.StdProp;
	BEGIN
		Properties.CollectStdProp(p0);
		IF Properties.typeface IN p0.known THEN
			NEW(p); p.typeface := p0.typeface$;
			p.size := p0.size; p.color.val := p0.color.val;
			p.weight := p0.weight; p.style := p0.style;
			FontDialog0(TRUE, p.typeface, p.size, p.color.val, p.weight, p.style.val, set);
			IF set THEN
				p.valid := {Properties.typeface, Properties.style, Properties.weight, Properties.size, Properties.color};
				p.style.mask := {Fonts.italic, Fonts.underline, Fonts.strikeout};
				Properties.EmitProp(NIL, p)
			END
		END
	END FontDialog;

	PROCEDURE TypefaceDialog*;
	(** open font dialog and set selection to choosen typeface **)
		VAR set: BOOLEAN; p, p0: Properties.StdProp; s: INTEGER; c: Ports.Color; w: INTEGER; st: SET;
	BEGIN
		Properties.CollectStdProp(p0);
		IF Properties.typeface IN p0.known THEN
			NEW(p); p.typeface := p0.typeface$;
			FontDialog0(FALSE, p.typeface, s, c, w, st, set);
			IF set THEN
				p.valid := {Properties.typeface};
				Properties.EmitProp(NIL, p)
			END
		END
	END TypefaceDialog;


	(* preferences dialog *)

	PROCEDURE DefFont*;
		VAR tf: Fonts.Typeface; size: INTEGER; color: Ports.Color; w: INTEGER; style: SET; set: BOOLEAN;
	BEGIN
		tf := prefFName;
		size := prefFSize;
		w := Fonts.normal;
		style := {};
		color := Ports.defaultColor;
		FontDialog0(TRUE, tf, size, color, w, style, set);
		IF set THEN
			prefFName := tf; prefFSize := size
		END
	END DefFont;

	PROCEDURE DlgFont*;
		VAR tf: Fonts.Typeface; size: INTEGER; color: Ports.Color; w: INTEGER; style: SET; set: BOOLEAN;
	BEGIN
		tf := prefDName;
		size := prefDSize;
		w := prefDWght;
		style := prefDStyle;
		color := Ports.defaultColor;
		FontDialog0(TRUE, tf, size, color, w, style, set);
		IF set THEN
			prefDName := tf; prefDSize := size; prefDStyle := style; prefDWght := w
		END
	END DlgFont;

	PROCEDURE PrefOk*;
		VAR res: INTEGER; rect: WinApi.RECT;
	BEGIN
		HostFonts.SetDefaultFont(prefFName, prefFSize);
		HostFonts.SetDialogFont(prefDName, prefDSize, prefDStyle, prefDWght);
		HostFonts.SetTTMetric(prefs.useTTMetric);
		HostWindows.SetVisualScroll(prefs.visualScroll);
		IF prefs.statusbar = 1 THEN Dialog.showsStatus := TRUE; HostWindows.memInStatus := FALSE
		ELSIF prefs.statusbar = 2 THEN Dialog.showsStatus := TRUE; HostWindows.memInStatus := TRUE
		ELSE Dialog.showsStatus := FALSE
		END;
		Dialog.Call("StdCmds.UpdateAll", "", res);
		Dialog.Call("StdCmds.RecalcAllSizes", "", res);
		Dialog.Call("TextCmds.UpdateDefaultAttr", "", res);
		HostCFrames.SetDefFonts;
		HostRegistry.WriteBool("noStatus", ~Dialog.showsStatus);
		HostRegistry.WriteBool("memStatus", HostWindows.memInStatus);
		res := WinApi.GetClientRect(HostWindows.main, rect);
		HostWindows.ResizeMainWindow(0, rect.right, rect.bottom);
		Dialog.thickCaret := prefs.thickCaret;
		Dialog.caretPeriod := prefs.caretPeriod;
		HostRegistry.WriteBool("thickCaret", Dialog.thickCaret);
		HostRegistry.WriteInt("caretPeriod", Dialog.caretPeriod)
	END PrefOk;

	PROCEDURE InitPrefDialog*;
	BEGIN
		prefFName := HostFonts.defFont.alias;
		prefFSize := HostFonts.defFont.size;
		prefDName := HostFonts.dlgFont.typeface;
		prefDSize := HostFonts.dlgFont.size;
		prefDStyle := HostFonts.dlgFont.style;
		prefDWght := HostFonts.dlgFont.weight;
		prefs.useTTMetric := HostFonts.useTTMetric;
		prefs.visualScroll := HostWindows.visualScroll;
		IF ~Dialog.showsStatus THEN prefs.statusbar := 0
		ELSIF HostWindows.memInStatus THEN prefs.statusbar := 2
		ELSE prefs.statusbar := 1
		END;
		prefs.thickCaret := Dialog.thickCaret;
		prefs.caretPeriod := Dialog.caretPeriod
	END InitPrefDialog;


	(* date / time *)

	PROCEDURE (hook: DatesHook) DateToString (d: Dates.Date; format: INTEGER; OUT str: ARRAY OF CHAR);
		VAR res, pos, i: INTEGER; time: WinApi.SYSTEMTIME; fmt: ARRAY 64 OF CHAR;
	BEGIN
		time.wYear := SHORT(d.year); time.wMonth := SHORT(d.month); time.wDay := SHORT(d.day);
		IF format = Dates.short THEN
			res := WinApi.GetDateFormatW(
				HostRegistry.localeId, WinApi.DATE_SHORTDATE, time, NIL, str, LEN(str))
		ELSIF format = Dates.long THEN
			res := WinApi.GetDateFormatW(HostRegistry.localeId, WinApi.DATE_LONGDATE, time, NIL, str, LEN(str))
		ELSE
			res := WinApi.GetLocaleInfoW(HostRegistry.localeId, WinApi.LOCALE_SLONGDATE, fmt, LEN(fmt));
			IF format # Dates.abbreviated THEN	(* remove weekday *)
				Strings.Find(fmt, "dddd", 0, pos); i := pos + 4;
				IF pos < 0 THEN Strings.Find(fmt, "ddd", 0, pos); i := pos + 3 END;
				IF pos >= 0 THEN
					WHILE (fmt[i] # 0X) & (CAP(fmt[i]) < "A") OR (CAP(fmt[i]) > "Z") DO INC(i) END;
					Strings.Replace(fmt, pos, i - pos, "")
				END
			END;
			IF format # Dates.plainLong THEN	(* abbreviated *)
				Strings.Find(fmt, "dddd", 0, pos);
				IF pos >= 0 THEN Strings.Replace(fmt, pos, 4, "ddd") END;
				Strings.Find(fmt, "MMMM", 0, pos);
				IF pos >= 0 THEN Strings.Replace(fmt, pos, 4, "MMM") END
			END;
			res := WinApi.GetDateFormatW(HostRegistry.localeId, {}, time, fmt, str, LEN(str))
		END;
		IF res = 0 THEN str := "?" END
	END DateToString;

	PROCEDURE (hook: DatesHook) TimeToString (t: Dates.Time; OUT str: ARRAY OF CHAR);
		VAR res: INTEGER; time: WinApi.SYSTEMTIME;
	BEGIN
		time.wHour := SHORT(t.hour); time.wMinute := SHORT(t.minute);
		time.wSecond := SHORT(t.second); time.wMilliseconds := 0;
		res := WinApi.GetTimeFormatW(HostRegistry.localeId, {}, time, NIL, str, LEN(str));
		IF res = 0 THEN str := "?" END
	END TimeToString;

	PROCEDURE (hook: LanguageHook) SetLanguage (
		lang: Dialog.Language; persistent: BOOLEAN; OUT ok: BOOLEAN
	);
	BEGIN
		ok := (lang = "") OR (LEN(lang$) = 2);
		IF ok & persistent THEN HostRegistry.WriteString("language", lang) END
	END SetLanguage;

	PROCEDURE (hook: LanguageHook) GetPersistentLanguage (OUT lang: Dialog.Language);
		VAR res: INTEGER; s: ARRAY 32 OF CHAR;
	BEGIN
		HostRegistry.ReadString("language", s, res);
		IF res = 0 THEN
			ASSERT((s = "") OR (LEN(s$) = 2), 100);
			lang := s$
		ELSE lang := ""
		END
	END GetPersistentLanguage;

	PROCEDURE Start* (name: ARRAY OF CHAR);
		VAR res: INTEGER; info: WinApi.STARTUPINFOW; process: WinApi.PROCESS_INFORMATION;
	BEGIN
		(* res := WinApi.WinExec(name, WinApi.SW_NORMAL) *)
		WinApi.GetStartupInfoW(info);
		info.wShowWindow := WinApi.SW_NORMAL;
		res := WinApi.CreateProcessW(NIL, name, NIL, NIL, WinApi.FALSE, {}, 0, NIL, info, process)
	END Start;


	(* initialization *)

	PROCEDURE InitDialogs;
		VAR i: INTEGER;
	BEGIN
		(* file dialog data structure *)
		fn.lStructSize := SIZE(WinDlg.OPENFILENAMEW);
		fn.hInstance := 0;
		fn.lpstrCustomFilter := NIL;
		fn.lpstrFilter := filter; fn.nFilterIndex := 1;
		i := WinApi.GetCurrentDirectoryW(LEN(actualPath), actualPath);
		fn.lpstrInitialDir := actualPath;
		fn.lpstrTitle := NIL; fn.lpstrFileTitle := NIL;
		fn.lpstrFile := fname; fn.nMaxFile := LEN(fname);
		fn.lpstrDefExt := ftype; ftype := "$";
		fn.lpfnHook := HookProc;
		(* print dialog data structure *)
		prt.lStructSize := SIZE(WinDlg.PRINTDLGW);
		prt.hDevMode := 0;
		prt.hDevNames := 0;
		prt.nMinPage := 1;
		prt.nMaxPage := -1;
		(* font dialog data structure *)
		font.lStructSize := SIZE(WinDlg.CHOOSEFONTW);
		font.hDC := 0;
		font.lpLogFont := SYSTEM.VAL(WinApi.PtrLOGFONTW, SYSTEM.ADR(logFont));
		font.rgbColors := 0;
		font.hInstance := 0;
		font.lpszStyle := NIL;
		font.nFontType := 2000H;	(* screenFontType *)
		logFont.lfWidth := 0;
		logFont.lfEscapement := 0;
		logFont.lfOrientation := 0;
		logFont.lfCharSet := 0X;
		logFont.lfOutPrecision := 0X;
		logFont.lfClipPrecision := 0X;
		logFont.lfQuality := 0X;
		logFont.lfPitchAndFamily := 0X;
		(* color dialog data structure *)
		col.lStructSize := SIZE(WinDlg.CHOOSECOLORW);
		col.lpCustColors := customColors;
		col.Flags := {0};	(* rgbInit *)
		i := 0;
		WHILE i < 16 DO customColors[i] := 0FFFFFFH; INC(i) END
	END InitDialogs;

	PROCEDURE Init*;
		VAR n, v, res: INTEGER; b: BOOLEAN;
			getSpecHook: GetSpecHook;
			datesHook: DatesHook;
			showHook: ShowHook;
			languageHook: LanguageHook;
	BEGIN
		NEW(all);
		v := WinApi.GetVersion();
		osVersion := v MOD 256 * 100 + v DIV 256 MOD 256;
		IF v >= 0 THEN
			IF osVersion < 400 THEN Dialog.platform := Dialog.windowsNT3
			ELSIF osVersion < 500 THEN Dialog.platform := Dialog.windowsNT4
			ELSIF osVersion = 500 THEN Dialog.platform := Dialog.windows2000
			ELSIF osVersion < 600 THEN Dialog.platform := Dialog.windowsXP
			ELSE Dialog.platform := Dialog.windowsVista
			END
		ELSE Dialog.platform := Dialog.windows98
		END;

		HostRegistry.ReadBool("noStatus", b, res); Dialog.showsStatus := (res # 0) OR ~b;
		HostRegistry.ReadBool("memStatus", b, res); HostWindows.memInStatus := (res = 0) & b;

		HostRegistry.ReadBool("thickCaret", b, res); IF res = 0 THEN Dialog.thickCaret := b END;
		HostRegistry.ReadInt("caretPeriod", n, res); IF res = 0 THEN Dialog.caretPeriod := n END;

		NEW(showHook); Dialog.SetShowHook(showHook);
(*
		Hooks.showParamMsg := ShowParamMsg;
		Hooks.showParamStatus := ShowParamStatus;
*)

		NEW(dialogHook); Dialog.SetGetHook(dialogHook);
(*
		Hooks.getOK := GetOK;
		Hooks.getIntSpec := GetIntSpec0;
		Hooks.getExtSpec := GetExtSpec0;
		Hooks.getColor := ColorDialog0;
*)

(*
		Sequencers.GetIntSpec := GetIntSpec;
		Sequencers.GetExtSpec := GetExtSpec;
*)
		NEW(getSpecHook); Views.SetGetSpecHook(getSpecHook);

		HostFiles.MapParamString := Dialog.MapParamString;

		NEW(datesHook); Dates.SetHook(datesHook);
(*
		Hooks.getTime := GetTime;
		Hooks.dateToString := DateToString;
		Hooks.timeToString := TimeToString
*)
		NEW(languageHook); Dialog.SetLanguageHook(languageHook); Dialog.ResetLanguage
	END Init;

BEGIN
	Init;
	InitDialogs
END HostDialog.
