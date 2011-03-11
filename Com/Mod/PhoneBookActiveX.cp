MODULE ComPhoneBookActiveX;
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

	IMPORT COM, WinApi, WinOle, ComTools, WinOleAut;
	
	CONST
		CLSID* = "{E67D346B-2A5B-11D0-ADBA-00C01500554E}";
		E_NotFound = -2147467259;
		typelibrary = "C:\BlackBox\Com\Interfaces\DPhoneBook\phone.tlb";
	
	TYPE
		ILookup* = POINTER TO ABSTRACT RECORD
			["{C4910D72-BA7D-11CD-94E8-08001701A8A3}"] (WinOleAut.IDispatch)
		END;
	
		CLookup = POINTER TO RECORD (ILookup) END;
		
		LookupFactory = POINTER TO RECORD (WinOle.IClassFactory) END;
	
		Entry = POINTER TO EntryDesc;
		EntryDesc = RECORD
			next: Entry;
			name, number: ARRAY 32 OF CHAR
		END;
	
	VAR
		locks: INTEGER;
		objects: INTEGER;
		
		phoneBook: Entry;
	
	(* ---------- ILookup ---------- *)
	
	PROCEDURE (this: ILookup) LookupByName*(
		name: WinApi.PtrWSTR; OUT number: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;
	
	PROCEDURE (this: ILookup) LookupByNumber*(
		number: WinApi.PtrWSTR; OUT name: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;
		
	(* ---------- CLookup ---------- *)
		
	(* use default QueryInterface implementation *)
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: CLookup) LookupByName(name: WinApi.PtrWSTR;
			OUT number: WinApi.PtrWSTR): COM.RESULT;
		VAR e: Entry; ustr: ARRAY [untagged] 32 OF CHAR; i: INTEGER;
	BEGIN
		e := phoneBook;
		WHILE (e # NIL) & (e.name # name^) DO e := e.next END;
		IF e # NIL THEN i := 0;
			REPEAT ustr[i] := e.number[i]; INC(i) UNTIL e.number[i-1] = 0X;
			number := ComTools.NewString(ustr);
			RETURN WinApi.S_OK
		ELSE
			RETURN E_NotFound
		END
	END LookupByName;
	
	PROCEDURE (this: CLookup) LookupByNumber(number: WinApi.PtrWSTR;
				OUT name: WinApi.PtrWSTR): COM.RESULT;
		VAR e: Entry; 
			ustr: ARRAY [untagged] 32 OF CHAR; i: INTEGER;
	BEGIN 
		e := phoneBook;
		WHILE (e # NIL) & (e.number # number^) DO e := e.next END;
		IF e # NIL THEN i := 0;
			REPEAT ustr[i] := e.name[i]; INC(i) UNTIL e.name[i-1] = 0X;
			name := ComTools.NewString(ustr);
			RETURN WinApi.S_OK
		ELSE
			RETURN E_NotFound
		END
	END LookupByNumber;
	
	PROCEDURE (this: CLookup) Invoke (dispIdMember: WinOleAut.DISPID;
		IN riid: COM.GUID;
		lcid: WinOle.LCID; wFlags: SHORTINT; VAR [nil] pDispParams: WinOleAut.DISPPARAMS; 
		OUT [nil] pVarResult: WinOleAut.VARIANT; OUT [nil] pExcepInfo: WinOleAut.EXCEPINFO;
		OUT [nil] puArgErr: INTEGER
	): COM.RESULT;
		VAR wstr: WinApi.PtrWSTR; res: INTEGER; (* bstr: WinOle.BSTR; *)
	BEGIN
		IF (dispIdMember = 1) OR (dispIdMember = 2) THEN
			IF pDispParams.cArgs = 1 THEN
				IF pDispParams.rgvarg[0].vt = WinOle.VT_BSTR THEN
					IF dispIdMember = 1 THEN 
						res := this.LookupByName(pDispParams.rgvarg[0].u.bstrVal, wstr)
					ELSE
						res := this.LookupByNumber(pDispParams.rgvarg[0].u.bstrVal, wstr)
					END;
					IF res = 0 THEN
						pVarResult.vt := WinOle.VT_BSTR;
						pVarResult.u.bstrVal := WinOleAut.SysAllocString(wstr^);
						RETURN WinApi.S_OK
					ELSE
						pExcepInfo.wCode := 0;
						pExcepInfo.wReserved := 0;
						pExcepInfo.bstrSource := WinOleAut.SysAllocString("PhoneBook.Lookup");
						pExcepInfo.bstrDescription := WinOleAut.SysAllocString("Entry not found");
						pExcepInfo.bstrHelpFile := NIL;
						pExcepInfo.pfnDeferredFillIn := NIL;
						pExcepInfo.scode := res;
						RETURN WinApi.DISP_E_EXCEPTION
					END
				ELSE RETURN WinApi.DISP_E_BADVARTYPE
				END
			ELSE RETURN WinApi.DISP_E_BADPARAMCOUNT
			END;
		ELSE RETURN WinApi.DISP_E_MEMBERNOTFOUND
		END		
	END Invoke;

	PROCEDURE (this: CLookup) GetIDsOfNames (IN [nil] riid: COM.GUID (* NULL *);
		IN [nil] rgszNames: WinApi.PtrWSTR;  cNames: INTEGER; lcid: WinOle.LCID;
		OUT [nil] rgDispId: WinOleAut.DISPID
	): COM.RESULT;
		VAR lib: WinOleAut.ITypeLib; ptinfo: WinOleAut.ITypeInfo; names: WinApi.PtrWSTR;res: INTEGER;
	BEGIN
		res := WinOleAut.LoadTypeLib(ComTools.NewString(typelibrary), lib);
		res := lib.GetTypeInfoOfGuid(COM.ID(ILookup), ptinfo);
		names := ComTools.NewString(rgszNames^);
		res := WinOleAut.DispGetIDsOfNames(ptinfo, names, cNames, rgDispId);
		RETURN 0
	END GetIDsOfNames;
	
	PROCEDURE (this: CLookup) GetTypeInfo (iTInfo: INTEGER; lcid: WinOle.LCID; 
											OUT [nil] ppTInfo: WinOleAut.ITypeInfo): COM.RESULT;
		VAR lib: WinOleAut.ITypeLib; res: INTEGER;
	BEGIN
		res := WinOleAut.LoadTypeLib(ComTools.NewString(typelibrary), lib);
		RETURN lib.GetTypeInfo(iTInfo, ppTInfo)
	END GetTypeInfo;
	
	PROCEDURE (this: CLookup) GetTypeInfoCount (OUT [nil] pctinfo: INTEGER): COM.RESULT;
	BEGIN 
		pctinfo := 1;	(* type info available *)
		RETURN 0 
	END GetTypeInfoCount;

	PROCEDURE (this: CLookup) RELEASE;	(* called when last com reference is removed *)
	BEGIN
		DEC(objects)
	END RELEASE;
	
	(* ---------- LookupFactory ---------- *)
	
	(* use default QueryInterface implementation *)
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: LookupFactory) CreateInstance (outer: COM.IUnknown;
				IN [iid] iid: COM.GUID; OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR res: COM.RESULT; new: CLookup;
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
	
	PROCEDURE (this: LookupFactory) LockServer (lock: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF lock # 0 THEN INC(locks) ELSE DEC(locks) END;
		RETURN WinApi.S_OK
	END LockServer;
	
	(* ---------- dll interface ---------- *)
	
	PROCEDURE DllGetClassObject* (IN clsid: COM.GUID;
												IN [iid] iid: COM.GUID; OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR obj: LookupFactory;
	BEGIN
		IF clsid = CLSID THEN
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


	PROCEDURE NewEntry(name, number: ARRAY OF CHAR);
		VAR e: Entry;
	BEGIN
		NEW(e); e.next  := phoneBook; phoneBook := e; e.name := name$; e.number := number$
	END NewEntry;

BEGIN	
	locks := 0; objects := 0;
	NewEntry("Daffy Duck", "310-555-1212");
	NewEntry("Wile E. Coyote", "408-555-1212");
	NewEntry("Scrooge McDuck", "206-555-1212");
	NewEntry("Huey Lewis", "415-555-1212");
	NewEntry("Thomas Dewey", "617-555-1212");
END ComPhoneBookActiveX.

 DevLinker.LinkDll "Com/phone.dll" := Kernel+ ComTools ComPhoneBookActiveX# ~


REGEDIT
HKEY_CLASSES_ROOT\PhoneBook = PhoneBook ActiveX Control
HKEY_CLASSES_ROOT\PhoneBook\CLSID = {E67D346B-2A5B-11D0-ADBA-00C01500554E}
HKEY_CLASSES_ROOT\PhoneBook\TypeLib = {C4910D73-BA7D-11CD-94E8-08001701A8A3}

HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E} = PhoneBook ActiveX Control
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\ProgID = PhoneBook1.0
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\Control
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\Version = 1.0
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\VersionIndependentProgID = PhoneBook
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\TypeLib = {C4910D73-BA7D-11CD-94E8-08001701A8A3}
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\InprocServer32 = C:\BlackBox\Com\phone.dll
HKEY_CLASSES_ROOT\CLSID\{E67D346B-2A5B-11D0-ADBA-00C01500554E}\NotInsertable

HKEY_CLASSES_ROOT\TypeLib\{C4910D73-BA7D-11CD-94E8-08001701A8A3}\1.0 = PhoneBook ActiveX Control
HKEY_CLASSES_ROOT\TypeLib\{C4910D73-BA7D-11CD-94E8-08001701A8A3}\1.0\0\win32 = C:\BlackBox\Com\Interfaces\DPhoneBook\phone.tlb
HKEY_CLASSES_ROOT\TypeLib\{C4910D73-BA7D-11CD-94E8-08001701A8A3}\1.0\FLAGS = 0
HKEY_CLASSES_ROOT\TypeLib\{C4910D73-BA7D-11CD-94E8-08001701A8A3}\1.0\HELPDIR = C:\BlackBox\Com\Interfaces\DPhoneBook