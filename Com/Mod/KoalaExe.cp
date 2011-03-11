MODULE ComKoalaExe;
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


	IMPORT S := SYSTEM, COM, WinApi, WinOle;

	CONST
		KoalaId = "{00021146-0000-0000-C000-000000000046}";

		
	TYPE
		Koala = POINTER TO RECORD (COM.IUnknown) END;
		KoalaFactory = POINTER TO RECORD (WinOle.IClassFactory) END;
	
	
	VAR
		instance: WinApi.HMODULE;
		mainWnd: WinApi.HWND;
		locks: INTEGER;
		objects: INTEGER;
		
		
	(* ---------- Koala ---------- *)
	
	(* use default QueryInterface implementation *)
	
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: Koala) RELEASE;	(* called when last com reference is removed *)
		VAR res: INTEGER;
	BEGIN
		DEC(objects);
		IF objects = 0 THEN
			res := WinApi.PostMessageW(mainWnd, WinApi.WM_CLOSE, 0, 0);
		END
	END RELEASE;
	
	
	(* ---------- KoalaFactory ---------- *)
	
	(* use default QueryInterface implementation *)
	
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: KoalaFactory) CreateInstance (outer: COM.IUnknown; IN [iid] iid: COM.GUID;
																			OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR res: COM.RESULT; new: Koala;
	BEGIN
		IF outer # NIL THEN RETURN WinApi.CLASS_E_NOAGGREGATION END;
		NEW(new);
		IF new # NIL THEN
			res := new.QueryInterface(iid, int);
			IF res >= 0 THEN INC(objects) END;
			RETURN res
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END CreateInstance;
	
	PROCEDURE (this: KoalaFactory) LockServer (lock: WinApi.BOOL): COM.RESULT;
		VAR res: INTEGER;
	BEGIN
		IF lock # 0 THEN INC(locks) ELSE DEC(locks) END;
		IF locks = 0 THEN
			res := WinApi.PostMessageW(mainWnd, WinApi.WM_CLOSE, 0, 0);
		END;
		RETURN WinApi.S_OK
	END LockServer;


	(* ---------- window handler ---------- *)
	
	PROCEDURE WndHandler (wnd: WinApi.HWND; message, wParam, lParam: INTEGER): INTEGER;
		VAR res: INTEGER; ps: WinApi.PAINTSTRUCT; dc: WinApi.HDC;
	BEGIN
		IF message = WinApi.WM_CLOSE THEN
			IF (locks > 0) OR (objects > 0) THEN RETURN 0 END
		ELSIF message = WinApi.WM_DESTROY THEN
			WinApi.PostQuitMessage(0);
			RETURN 0
		ELSIF message = WinApi.WM_PAINT THEN
			dc := WinApi.BeginPaint(wnd, ps);
			res := WinApi.TextOutW(dc, 50, 50, "Koala Server", 12);
			res := WinApi.EndPaint(wnd, ps);
			RETURN 0
		END;
		RETURN WinApi.DefWindowProcW(wnd, message, wParam, lParam)
	END WndHandler;
	
	PROCEDURE OpenWindow;
		VAR class: WinApi.WNDCLASSW; res: INTEGER;
	BEGIN
		class.hCursor := WinApi.LoadCursorW(0, S.VAL(WinApi.PtrWSTR, WinApi.IDC_ARROW));
		class.hIcon := WinApi.LoadIconW(0, S.VAL(WinApi.PtrWSTR, WinApi.IDI_APPLICATION));
		class.lpszMenuName := NIL;
		class.lpszClassName := "Koala";
		class.hbrBackground := WinApi.GetStockObject(WinApi.WHITE_BRUSH);
		class.style := WinApi.CS_HREDRAW + WinApi.CS_VREDRAW;
		class.hInstance := instance;
		class.lpfnWndProc := WndHandler;
		class.cbClsExtra := 0;
		class.cbWndExtra := 0;
		res := WinApi.RegisterClassW(class);
		mainWnd := WinApi.CreateWindowExW({}, "Koala", "Koala",
														WinApi.WS_OVERLAPPEDWINDOW,
														WinApi.CW_USEDEFAULT, WinApi.CW_USEDEFAULT,
														WinApi.CW_USEDEFAULT, WinApi.CW_USEDEFAULT,
														0, 0, instance, 0);
		res := WinApi.ShowWindow(mainWnd, 10);
		res := WinApi.UpdateWindow(mainWnd);
	END OpenWindow;
	

	(* ---------- main loop ---------- *)
	
	PROCEDURE Main;
		VAR msg: WinApi.MSG; res: COM.RESULT; factory: KoalaFactory; token: INTEGER;
	BEGIN
		instance := WinApi.GetModuleHandleW(NIL);
		res := WinOle.CoInitialize(0);
		NEW(factory);
		res := WinOle.CoRegisterClassObject(KoalaId, factory,
					WinOle.CLSCTX_LOCAL_SERVER, WinOle.REGCLS_MULTIPLEUSE, token);
		OpenWindow;
		WHILE WinApi.GetMessageW(msg, 0, 0, 0) # 0 DO
			res := WinApi.TranslateMessage(msg);
			res := WinApi.DispatchMessageW(msg);
		END;
		res := WinOle.CoRevokeClassObject(token);
		WinOle.CoUninitialize;
		WinApi.ExitProcess(msg.wParam)
	END Main;

BEGIN
	Main
END ComKoalaExe.


DevLinker.LinkExe EKoala1.exe := Kernel+ ComKoalaExe ~


-----------------------------------------------------------------------------------------------------------------
REGEDIT
HKEY_CLASSES_ROOT\Koala1.0 = Koala Object Chapter 5
HKEY_CLASSES_ROOT\Koala1.0\CLSID = {00021146-0000-0000-C000-000000000046}
HKEY_CLASSES_ROOT\Koala = Koala Object Chapter 5
HKEY_CLASSES_ROOT\Koala\CurVer = Koala1.0
HKEY_CLASSES_ROOT\Koala\CLSID = {00021146-0000-0000-C000-000000000046}

HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046} = Koala Object Chapter 5
HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046}\ProgID = Koala1.0
HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046}\VersionIndependentProgID = Koala
HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046}\LocalServer32 = C:\BlackBox\Com\Ekoala1.exe
HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046}\NotInsertable
-----------------------------------------------------------------------------------------------------------------

