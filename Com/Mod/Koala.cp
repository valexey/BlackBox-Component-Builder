MODULE ComKoala;
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
		token: INTEGER;
		
		
	(* ---------- Koala ---------- *)
	
	(* use default QueryInterface implementation *)
	
	(* AddRef & Release implemented implicitly by the compiler *)
	
	
	(* ---------- KoalaFactory ---------- *)
	
	(* use default QueryInterface implementation *)
	
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: KoalaFactory) CreateInstance (outer: COM.IUnknown; IN [iid] iid: COM.GUID;
																			OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR res: COM.RESULT; new: Koala;
	BEGIN
		IF outer # NIL THEN RETURN WinApi.CLASS_E_NOAGGREGATION END;
		NEW(new);
		IF new # NIL THEN RETURN new.QueryInterface(iid, int)
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END CreateInstance;
	
	PROCEDURE (this: KoalaFactory) LockServer (lock: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF lock # 0 THEN INC(locks) ELSE DEC(locks) END;
		RETURN WinApi.S_OK
	END LockServer;
	

	(* ---------- registration ---------- *)
	
	PROCEDURE Register*;
		VAR res: COM.RESULT; factory: KoalaFactory;
	BEGIN
		NEW(factory);
		res := WinOle.CoRegisterClassObject(KoalaId, factory,
					WinOle.CLSCTX_LOCAL_SERVER, WinOle.REGCLS_MULTIPLEUSE, token)
	END Register;
	
	PROCEDURE Unregister*;
		VAR res: COM.RESULT;
	BEGIN
		IF (locks = 0) & (token # 0) THEN
			res := WinOle.CoRevokeClassObject(token)
		END
	END Unregister;
	
	
BEGIN	
	locks := 0
END ComKoala.

ComKoala.Register
ComKoala.Unregister
