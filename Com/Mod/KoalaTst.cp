MODULE ComKoalaTst;
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

	IMPORT COM, WinApi, WinOle, StdLog;
	
	CONST
		KoalaId = "{00021146-0000-0000-C000-000000000046}";
		nMax = 10;
		
	VAR
		factory: WinOle.IClassFactory;
		koala: ARRAY nMax OF COM.IUnknown;
		n: INTEGER;
		
	PROCEDURE CreateClass*;
		VAR res: INTEGER;
	BEGIN
		res := WinOle.CoGetClassObject(KoalaId, WinOle.CLSCTX_LOCAL_SERVER, 0, COM.ID(factory), factory);
		IF res = WinApi.S_OK THEN StdLog.String("Class creation succeeded"); StdLog.Ln
		ELSE StdLog.String("Class creation failed, error = "); StdLog.Int(res); StdLog.Ln
		END
	END CreateClass;
	
	PROCEDURE ReleaseClass*;
	BEGIN
		factory := NIL
	END ReleaseClass;
	
	PROCEDURE CreateInstance*;
		VAR res: INTEGER;
	BEGIN
		IF factory # NIL THEN
			res := factory.CreateInstance(NIL, COM.ID(COM.IUnknown), koala[0]); n := 1;
			IF res = WinApi.S_OK THEN StdLog.String("Instance creation succeeded"); StdLog.Ln
			ELSE StdLog.String("Instance creation failed, error = "); StdLog.Int(res); StdLog.Ln
			END
		ELSE StdLog.String("Creation failed, class not yet created."); StdLog.Ln
		END
	END CreateInstance;
	
	PROCEDURE AddRef*;
	BEGIN
		IF (n > 0) & (n < nMax) THEN koala[n] := koala[n-1]; INC(n) END
	END AddRef;
	
	PROCEDURE Release*;
	BEGIN
		IF n > 0 THEN DEC(n); koala[n] := NIL END
	END Release;
	
END ComKoalaTst.

 ComKoala.Register
 ComKoala.Unregister

 ComKoalaTst.CreateClass
 ComKoalaTst.ReleaseClass

 ComKoalaTst.CreateInstance
 ComKoalaTst.AddRef
 ComKoalaTst.Release