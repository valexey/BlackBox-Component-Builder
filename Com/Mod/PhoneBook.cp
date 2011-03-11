MODULE ComPhoneBook;
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

	IMPORT COM, WinApi, WinOle, ComTools;
	
	CONST
		CLSID* = "{E67D346C-2A5B-11D0-ADBA-00C01500554E}";
		E_NotFound* = 80004005H;
	
	TYPE
		ILookup* = POINTER TO ABSTRACT RECORD
				["{C4910D71-BA7D-11CD-94E8-08001701A8A3}"] (COM.IUnknown) END;

		CLookup = POINTER TO RECORD (ILookup)
			phoneBook: Entry
		END;
		
		LookupFactory = POINTER TO RECORD (WinOle.IClassFactory) END;
	
		Entry = POINTER TO RECORD
			next: Entry;
			name, number: ARRAY 32 OF CHAR
		END;
	
	VAR
		locks: INTEGER;
		token: INTEGER;
		phoneBook: Entry;
	
	(* ---------- ILookup ---------- *)

	PROCEDURE (this: ILookup) LookupByName*(
		name: WinApi.PtrWSTR; OUT number: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;
	
	PROCEDURE (this: ILookup) LookupByNumber*(
		number: WinApi.PtrWSTR; OUT name: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	(* ---------- CLookup ---------- *)
	
	PROCEDURE (this: CLookup) LookupByName(name: WinApi.PtrWSTR;
			OUT number: WinApi.PtrWSTR): COM.RESULT;
		VAR e: Entry;
	BEGIN e := this.phoneBook;
		WHILE (e # NIL) & (e.name # name^) DO e := e.next END;
		IF e # NIL THEN number := ComTools.NewString(e.number); RETURN WinApi.S_OK
		ELSE RETURN E_NotFound
		END
	END LookupByName;
	
	PROCEDURE (this: CLookup) LookupByNumber(number: WinApi.PtrWSTR;
				OUT name: WinApi.PtrWSTR): COM.RESULT;
		VAR e: Entry; 
	BEGIN e := this.phoneBook;
		WHILE (e # NIL) & (e.number # number^) DO e := e.next END;
		IF e # NIL THEN name := ComTools.NewString(e.name); RETURN WinApi.S_OK
		ELSE RETURN E_NotFound
		END
	END LookupByNumber;
	
	(* ---------- LookupFactory ---------- *)
	
	PROCEDURE (this: LookupFactory) CreateInstance (outer: COM.IUnknown;
				IN [iid] iid: COM.GUID; OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR new: CLookup;
	BEGIN
		IF outer # NIL THEN RETURN WinApi.CLASS_E_NOAGGREGATION END;
		NEW(new);
		IF new # NIL THEN new.phoneBook := phoneBook;RETURN new.QueryInterface(iid, int)
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END CreateInstance;
	
	PROCEDURE (this: LookupFactory) LockServer (lock: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF lock # 0 THEN INC(locks) ELSE DEC(locks) END;
		RETURN WinApi.S_OK
	END LockServer;
	
	(* ---------- registration ---------- *)
	
	PROCEDURE Register*;
		VAR res: COM.RESULT; factory: LookupFactory;
	BEGIN
		NEW(factory);
		res := WinOle.CoRegisterClassObject(CLSID, factory,
					WinOle.CLSCTX_LOCAL_SERVER, WinOle.REGCLS_MULTIPLEUSE, token)
	END Register;
	
	PROCEDURE Unregister*;
		VAR res: COM.RESULT;
	BEGIN
		IF (locks = 0) & (token # 0) THEN res := WinOle.CoRevokeClassObject(token); token := 0 END
	END Unregister;
	
	PROCEDURE NewEntry(name, number: ARRAY 32 OF CHAR);
		VAR e: Entry;
	BEGIN
		NEW(e); e.next  := phoneBook; phoneBook := e; e.name := name$; e.number := number$
	END NewEntry;
	
BEGIN
	locks := 0; 
	NewEntry("Daffy Duck", "310-555-1212");
	NewEntry("Wile E. Coyote", "408-555-1212");
	NewEntry("Scrogge McDuck", "206-555-1212");
	NewEntry("Huey Lewis", "415-555-1212");
	NewEntry("Thomas Dewey", "617-555-1212")
END ComPhoneBook.


