MODULE ComKoalaDll;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	references	= "adapted from Koala sample in "Inside OLE", 2nd ed."
	changes	= ""
	issues	= ""

**)


	IMPORT COM, WinApi, WinOle;
	
	
	CONST
		KoalaId = "{00021146-0000-0000-C000-000000000046}";
		
	
	TYPE
		Koala = POINTER TO RECORD (COM.IUnknown) END;
		KoalaFactory = POINTER TO RECORD (WinOle.IClassFactory) END;
	
	
	VAR
		locks: INTEGER;
		objects: INTEGER;
		
		
	(* ---------- Koala ---------- *)
	
	(* use default QueryInterface implementation *)
	
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: Koala) RELEASE;	(* called when last com reference is removed *)
	BEGIN
		DEC(objects)
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
	BEGIN
		IF lock # 0 THEN INC(locks) ELSE DEC(locks) END;
		RETURN WinApi.S_OK
	END LockServer;
	

	(* ---------- dll interface ---------- *)
	
	PROCEDURE DllGetClassObject* (IN clsid: COM.GUID;
												IN [iid] iid: COM.GUID; OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR obj: KoalaFactory;
	BEGIN
		IF clsid = KoalaId THEN
			NEW(obj);
			IF obj # NIL THEN RETURN obj.QueryInterface(iid, int)
			ELSE RETURN WinApi.E_OUTOFMEMORY;
			END
		ELSE RETURN WinApi.E_FAIL
		END
	END DllGetClassObject;
	
	PROCEDURE DllCanUnloadNow* (): COM.RESULT;
	BEGIN
		IF (objects = 0) & (locks = 0) THEN RETURN WinApi.S_OK ELSE RETURN WinApi.S_FALSE END
	END DllCanUnloadNow;
	

BEGIN	
	locks := 0; objects := 0
END ComKoalaDll.


DevLinker.LinkDll DKoala1.dll := Kernel+ ComKoalaDll# ~


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
HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046}\InprocServer32 = C:\BlackBox\Com\DKoala1.dll
HKEY_CLASSES_ROOT\CLSID\{00021146-0000-0000-C000-000000000046}\NotInsertable
-----------------------------------------------------------------------------------------------------------------
