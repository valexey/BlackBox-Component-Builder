MODULE HostPrinters;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Josef Templ"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT
		SYSTEM, WinApi, WinDlg,
		Kernel, Files, Dialog, Ports, HostPorts, Stores, Models, Views, Controllers,
		Properties, Printers, Printing, Documents, Windows, HostWindows;
	
	CONST
		eduMsgH = 30 * Ports.point;

		dialogCommand = "StdCmds.OpenToolDialog('HostPrinters.printing', '#Host:Print')";
	
	TYPE
		Printer = POINTER TO RECORD (Printers.Printer)
			w, h: INTEGER;
			(* port: HostPorts.Port;	(* port to which forwarding occurs *) *)
			devNames, devMode: WinApi.HANDLE;	(* printer description *)
			clean: TrapCleaner;
			cancel: Windows.Window	(* cancel dialog *)
		END;

(*
		Rider = POINTER TO RECORD (HostPorts.Rider)
			p: Printer
		END;
*)
		Directory = POINTER TO RECORD (Printers.Directory) END;
		
		TrapCleaner = POINTER TO RECORD (Kernel.TrapCleaner)
			p: Printer
		END;


	VAR
		printing*: RECORD
			pnum-: INTEGER;
			Cancel*: PROCEDURE
		END;

		dir: Directory;
		current: Printers.Printer;
		aborted: BOOLEAN;

		sa, sb, sc: ARRAY 64 OF CHAR;
		err: INTEGER;

		jobLevel: INTEGER;


	PROCEDURE PrinterDC (devNames, devMode: WinApi.HANDLE): WinApi.HANDLE;
		VAR adr: INTEGER; i: SHORTINT; a, b, c: WinApi.PtrWSTR; dc: WinApi.HANDLE; dm: WinApi.PtrDEVMODEW;
	BEGIN
		adr := WinApi.GlobalLock(devNames);
		dm := SYSTEM.VAL(WinApi.PtrDEVMODEW, WinApi.GlobalLock(devMode));
		SYSTEM.GET(adr + 0, i); a := SYSTEM.VAL(WinApi.PtrWSTR, adr + i * SIZE(CHAR));
		SYSTEM.GET(adr + 2, i); b := SYSTEM.VAL(WinApi.PtrWSTR, adr + i * SIZE(CHAR));
		SYSTEM.GET(adr + 4, i); c := SYSTEM.VAL(WinApi.PtrWSTR, adr + i * SIZE(CHAR));
		dc := WinApi.CreateDCW(a, b, c, dm^);
		IF dc = 0 THEN dc := WinApi.CreateDCW(a, b, c, NIL) END;
		adr := WinApi.GlobalUnlock(devMode);
		adr := WinApi.GlobalUnlock(devNames);
		RETURN dc
	END PrinterDC;

	PROCEDURE SetupPrinter (p: Printer; dc: WinApi.HANDLE);
		VAR res, w, h, pw, ph, nx, ny, unit, ux, uy: INTEGER;
			pt: WinApi.POINT; port: Ports.Port; hport: HostPorts.Port;
	BEGIN
		w := WinApi.GetDeviceCaps(dc, WinApi.HORZRES);
		h := WinApi.GetDeviceCaps(dc, WinApi.VERTRES);
		nx := WinApi.GetDeviceCaps(dc, WinApi.LOGPIXELSX);
		ny := WinApi.GetDeviceCaps(dc, WinApi.LOGPIXELSY);
		ux := (Ports.inch + nx DIV 2) DIV nx;
		uy := (Ports.inch + ny DIV 2) DIV ny;
		unit := ux;
		IF nx < ny THEN
			h := h * nx DIV ny
		ELSIF nx > ny THEN
			w := w * ny DIV nx; unit := uy
		END;
		pw := WinApi.GetDeviceCaps(dc, WinApi.PHYSICALWIDTH);
		ph := WinApi.GetDeviceCaps(dc, WinApi.PHYSICALHEIGHT);
		IF (pw = 0) OR (ph = 0) THEN
			res := WinApi.Escape(dc, 12, 0, NIL, SYSTEM.ADR(pt));
			ASSERT(res > 0, 100);
			pw := pt.x; ph := pt.y
		END;
		pw := pw * ux; ph := ph * uy;
		nx := WinApi.GetDeviceCaps(dc, WinApi.PHYSICALOFFSETX);
		ny := WinApi.GetDeviceCaps(dc, WinApi.PHYSICALOFFSETY);
		IF (nx = 0) OR (ny = 0) THEN
			res := WinApi.Escape(dc, 13, 0, NIL, SYSTEM.ADR(pt));
			ASSERT(res > 0, 100);
			nx := pt.x; ny := pt.y
		END;
		nx := nx * ux; ny := ny * uy;
		p.InitPrinter(-nx, -ny, pw - nx, ph - ny);
		port := p.ThisPort();
		IF port = NIL THEN
			NEW(hport); p.InitPort(hport); port := hport;
			port.Init(unit, Ports.printer)
		END;
		port.SetSize(w, h)
	END SetupPrinter;

	PROCEDURE NewPrinter* (devNames, devMode: WinApi.HANDLE): Printers.Printer;
		VAR p: Printer; dc: WinApi.HANDLE; res: INTEGER;
	BEGIN
		ASSERT(devNames # 0, 20);
		dc := PrinterDC(devNames, devMode);
		IF dc # 0 THEN
			NEW(p);
			p.devNames := devNames; p.devMode := devMode;
			SetupPrinter(p, dc);
			res := WinApi.DeleteDC(dc)
		ELSE p := NIL
		END;
		RETURN p
	END NewPrinter;
	
	PROCEDURE SetCurrent* (devNames, devMode: WinApi.HANDLE);	(* used in HostDialog *)
	BEGIN
		IF devNames # 0 THEN
			current := NewPrinter(devNames, devMode)
		END;
	END SetCurrent;
	
	PROCEDURE GetCurrent* (OUT devNames, devMode: WinApi.HANDLE);	(* used in HostDialog *)
	BEGIN
		IF current # NIL THEN
			WITH current: Printer DO
				devNames := current.devNames;
				devMode := current.devMode
			END
		END
	END GetCurrent;
	
	PROCEDURE GetPage* (p: Printers.Printer; VAR w, h: INTEGER);
		VAR l, t, r, b: INTEGER;
	BEGIN
		WITH p: Printer DO
			p.GetRect(l, t, r, b);
			w := r - l;
			h := b - t
		END
	END GetPage;


	PROCEDURE Error (res: INTEGER): INTEGER;
	BEGIN
		IF res = -1 THEN res := WinApi.GetLastError()
		ELSIF aborted THEN res := 1
		ELSE res := 0
		END;
		RETURN res
	END Error;
	
	PROCEDURE Cancel;
	BEGIN
		aborted := TRUE
	END Cancel;
	
	PROCEDURE [2] AbortHandler (dc: WinApi.HANDLE; error: INTEGER): INTEGER;
		VAR res: INTEGER; msg: WinApi.MSG; s: ARRAY 32 OF CHAR;
	BEGIN
		WHILE WinApi.PeekMessageW(msg, 0, 0, 0, 1) # 0 DO
			res := WinApi.TranslateMessage(msg);
(*
			IF msg.message = WinApi.WMPaint THEN
				res := WinApi.GetClassNameW(msg.wnd, s, LEN(s));
				IF (s # "Oberon Doc") & (s # "Oberon Aux") THEN
					res := WinApi.DispatchMessageW(msg)
				END
			ELSE
				res := WinApi.DispatchMessageW(msg)
			END
*)
			res := WinApi.DispatchMessageW(msg)
		END;
		IF aborted THEN RETURN 0 ELSE RETURN 1 END
	END AbortHandler;
	
	PROCEDURE (c: TrapCleaner) Cleanup;
		VAR res: INTEGER; p: Printer;
	BEGIN
		jobLevel := 0;
		p := c.p;
		res := WinApi.AbortDoc(p.ThisPort()(HostPorts.Port).dc);
		res := WinApi.DeleteDC(p.ThisPort()(HostPorts.Port).dc);
		res := WinApi.EnableWindow(HostWindows.main, 1);
		HostPorts.ResetColors;
		IF p.cancel # NIL THEN Windows.dir.Close(p.cancel); p.cancel := NIL END
	END Cleanup;


	(* Printer *)
(*
	PROCEDURE (p: Printer) NewRider (): Rider;
		VAR r: Rider;
	BEGIN
		ASSERT(p.port # NIL, 20); ASSERT(p.port.dc # 0, 21);
		NEW(r); r.p := p; r.InitPort(p.port); 
		RETURN r
	END NewRider;
*)
	PROCEDURE (p: Printer) OpenJob (VAR copies: INTEGER; name: ARRAY OF CHAR);
		VAR res: INTEGER; info: WinApi.DOCINFOW; s: Stores.Store; port: HostPorts.Port;
	BEGIN
		IF jobLevel = 0 THEN
			aborted := FALSE; copies := 1;
			port := p.ThisPort()(HostPorts.Port);
			port.SetDC(PrinterDC(p.devNames, p.devMode), 0);
			p.res := Error(WinApi.SetAbortProc(port.dc, AbortHandler));
			IF p.res = 0 THEN
				(* open cancel dialog *)
				printing.pnum := 0;
				HostWindows.dir.unmoveable := TRUE;
				Dialog.Call(dialogCommand, " ", res);
				HostWindows.dir.unmoveable := FALSE;
				p.cancel := Windows.dir.First();
				(* install trap cleaner *)
				NEW(p.clean); p.clean.p := p;
				Kernel.PushTrapCleaner(p.clean);
				(* start printing *)
				info.cbSize := SIZE(WinApi.DOCINFOW);
				info.lpszDocName := name;
				info.lpszOutput := NIL;
				p.res := Error(WinApi.StartDocW(port.dc, info));
				HostPorts.SetPrinterColors;
				res := WinApi.EnableWindow(HostWindows.main, 0)
			END;
			port.SetDC(port.dc, 0)	(* reinitialize dc *)
		END;
		INC(jobLevel)
	END OpenJob;

	PROCEDURE (p: Printer) CloseJob;
		VAR res: INTEGER; port: HostPorts.Port;
	BEGIN
		IF jobLevel = 1 THEN
			port := p.ThisPort()(HostPorts.Port);
			IF aborted THEN p.res := Error(WinApi.AbortDoc(port.dc))
			ELSE p.res := Error(WinApi.EndDoc(port.dc))
			END;
			res := WinApi.DeleteDC(port.dc);
			res := WinApi.EnableWindow(HostWindows.main, 1);
			HostPorts.ResetColors;
			IF p.cancel # NIL THEN Windows.dir.Close(p.cancel); p.cancel := NIL END;
			Kernel.PopTrapCleaner(p.clean)
		END;
		DEC(jobLevel)
	END CloseJob;
	
	PROCEDURE (p: Printer) OpenPage;
		VAR res: INTEGER; port: HostPorts.Port;
	BEGIN
		port := p.ThisPort()(HostPorts.Port);
		IF ~aborted THEN p.res := Error(WinApi.StartPage(port.dc)) END;
		printing.pnum := Printing.Current() (* Printing.par.page.current *) + 1;
		Dialog.Update(printing);
		res := WinApi.UpdateWindow(p.cancel(HostWindows.Window).wnd);
		port.SetDC(port.dc, 0)	(* reinitialize dc *)
	END OpenPage;

	PROCEDURE (p: Printer) ClosePage;
	BEGIN
		IF ~aborted THEN p.res := Error(WinApi.EndPage(p.ThisPort()(HostPorts.Port).dc)) END
	END ClosePage;

	PROCEDURE (p: Printer) SetOrientation (landscape: BOOLEAN);
		VAR res, w, h: INTEGER; dc: WinApi.HANDLE; dm: WinApi.PtrDEVMODEW;
	BEGIN
		GetPage(p, w, h);
		IF (w > h) # landscape THEN
			dm := SYSTEM.VAL(WinApi.PtrDEVMODEW, WinApi.GlobalLock(p.devMode));
			IF landscape THEN dm.dmOrientation := WinApi.DMORIENT_LANDSCAPE
			ELSE dm.dmOrientation := WinApi.DMORIENT_PORTRAIT
			END;
			dm.dmFields := dm.dmFields + WinApi.DM_ORIENTATION;
			res := WinApi.GlobalUnlock(p.devMode);
			dc := PrinterDC(p.devNames, p.devMode);
			SetupPrinter(p, dc);
			res := WinApi.DeleteDC(dc)
		END
	END SetOrientation;

(*
	PROCEDURE (p: Printer) SetSize (w, h: INTEGER);
	BEGIN
		p.w := w; p.h := h
	END SetSize;
	
	PROCEDURE (p: Printer) OpenBuffer (l, t, r, b: INTEGER);
	END OpenBuffer;
	
	PROCEDURE (p: Printer) CloseBuffer;
	END CloseBuffer;
	
	PROCEDURE (p: Printer) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := p.w; h := p.h
	END GetSize;
*)
	

	(* Rider *)
(*
	PROCEDURE (rd: Rider) Base (): Ports.Port;
	BEGIN
		RETURN rd.p
	END Base;
*)

	(* Directory *)

	PROCEDURE (d: Directory) Default (): Printers.Printer;
		VAR res: INTEGER; prt: WinDlg.PRINTDLGW;
	BEGIN
		prt.lStructSize := SIZE(WinDlg.PRINTDLGW);
		prt.hDevMode := 0; prt.hDevNames := 0; prt.hwndOwner := 0;
		prt.Flags := {7, 10};	(* no warning, return default *)
		res := WinDlg.PrintDlgW(prt);
		IF res # 0 THEN
			RETURN NewPrinter(prt.hDevNames, prt.hDevMode)
		ELSE
(*
			res := WinApi.CommDlgExtendedError();
			ASSERT(res = 1008H, 100);	(* no default printer *)
*)
			RETURN NIL
		END
	END Default;
	
	PROCEDURE (d: Directory) Current (): Printers.Printer;
	BEGIN
		RETURN current
	END Current;

	PROCEDURE (d: Directory) Available (): BOOLEAN;
	BEGIN
		RETURN current # NIL
	END Available;
	
	
	PROCEDURE Init;
	BEGIN
		printing.Cancel := Cancel;
		NEW(dir); Printers.SetDir(dir);
		current := dir.Default()
	END Init;

BEGIN
	Init
END HostPrinters.

