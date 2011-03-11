MODULE ComAggregate;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	references	= "adapted from Reuse sample in "Inside OLE", 2nd ed."
	changes	= ""
	issues	= ""

**)


	IMPORT COM, WinApi, StdLog;
	
	
	TYPE
		IAnimal = POINTER TO
						ABSTRACT RECORD ["{00021143-0000-0000-C000-000000000046}"] (COM.IUnknown) END;
		
		CAnimal = POINTER TO RECORD (COM.IUnknown)
			impl: CImpIAnimal
		END;
		CImpIAnimal = POINTER TO RECORD (IAnimal)
			obj: CAnimal
		END;

		IKoala = POINTER TO
						ABSTRACT RECORD ["{00021144-0000-0000-C000-000000000046}"] (COM.IUnknown) END;
		
		CKoala = POINTER TO RECORD (COM.IUnknown)
			impl: CImpIKoala;
			animal: COM.IUnknown
		END;
		CImpIKoala = POINTER TO RECORD (IKoala)
			obj: CKoala
		END;
		
		
	VAR
		koala: COM.IUnknown;
		
		
	(* ---------- IAnimal ---------- *)
	
	PROCEDURE (this: IAnimal) Eat (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IAnimal) Sleep (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IAnimal) Procreate (): COM.RESULT, NEW, ABSTRACT;

	
	(* ---------- CAnimal ---------- *)
	
	PROCEDURE (this: CAnimal) QueryInterface (IN [iid] iid: COM.GUID;
																		OUT [new] int: COM.IUnknown): COM.RESULT;
	BEGIN
		IF COM.QUERY(this, iid, int) OR COM.QUERY(this.impl, iid, int) THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_NOINTERFACE
		END
	END QueryInterface;
	
	
	(* ---------- CImpIAnimal ---------- *)
	
	PROCEDURE (this: CImpIAnimal) Eat (): COM.RESULT;
	BEGIN
		StdLog.String("Animal's IAnimal.Eat called"); StdLog.Ln;
		RETURN WinApi.S_OK
	END Eat;
	
	PROCEDURE (this: CImpIAnimal) Sleep (): COM.RESULT;
	BEGIN
		StdLog.String("Animal's IAnimal.Sleep called"); StdLog.Ln;
		RETURN WinApi.S_OK
	END Sleep;
	
	PROCEDURE (this: CImpIAnimal) Procreate (): COM.RESULT;
	BEGIN
		StdLog.String("Animal's IAnimal.Procreate called"); StdLog.Ln;
		RETURN WinApi.S_OK
	END Procreate;

	
	(* ---------- Animal creation ---------- *)
	
	PROCEDURE CreateAnimal (outer: COM.IUnknown; IN [iid] iid: COM.GUID;
										OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR new: CAnimal;
	BEGIN
		IF (outer # NIL) & (iid # COM.ID(COM.IUnknown)) THEN RETURN WinApi.CLASS_E_NOAGGREGATION END;
		NEW(new);
		IF new # NIL THEN
			IF outer = NIL THEN NEW(new.impl, new)
			ELSE NEW(new.impl, outer)
			END;
			IF new.impl # NIL THEN
				new.impl.obj := new;
				StdLog.String("Animal allocated"); StdLog.Ln;
				RETURN new.QueryInterface(iid, int)
			END
		END;
		RETURN WinApi.E_OUTOFMEMORY
	END CreateAnimal;


	(* ---------- IKoala ---------- *)
	
	PROCEDURE (this: IKoala) ClimbEucalyptusTrees (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IKoala) PouchOpensDown (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IKoala) SleepForHoursAfterEating (): COM.RESULT, NEW, ABSTRACT;
	
	
	(* ---------- CKoala ---------- *)
	
	PROCEDURE (this: CKoala) QueryInterface (IN [iid] iid: COM.GUID;
																		OUT [new] int: COM.IUnknown): COM.RESULT;
	BEGIN
		IF COM.QUERY(this, iid, int) OR COM.QUERY(this.impl, iid, int) THEN RETURN WinApi.S_OK
		ELSIF iid = COM.ID(IAnimal) THEN RETURN this.animal.QueryInterface(iid, int)
		ELSE RETURN WinApi.E_NOINTERFACE
		END
	END QueryInterface;
	
	
	(* ---------- CImpIKoala ---------- *)
	
	PROCEDURE (this: CImpIKoala) ClimbEucalyptusTrees (): COM.RESULT;
	BEGIN
		StdLog.String("Koala's IKoala.ClimbEucalyptusTrees called"); StdLog.Ln;
		RETURN WinApi.S_OK
	END ClimbEucalyptusTrees;
	
	PROCEDURE (this: CImpIKoala) PouchOpensDown (): COM.RESULT;
	BEGIN
		StdLog.String("Koala's IKoala.PouchOpensDown called"); StdLog.Ln;
		RETURN WinApi.S_OK
	END PouchOpensDown;
	
	PROCEDURE (this: CImpIKoala) SleepForHoursAfterEating (): COM.RESULT;
	BEGIN
		StdLog.String("Koala's IKoala.SleepForHoursAfterEating called"); StdLog.Ln;
		RETURN WinApi.S_OK
	END SleepForHoursAfterEating;
	
	
	(* ---------- Koala creation ---------- *)
	
	PROCEDURE CreateKoalaAggregation (OUT unk: COM.IUnknown): BOOLEAN;
		VAR obj: CKoala; res: COM.RESULT;
	BEGIN
		NEW(obj);
		IF obj # NIL THEN
			NEW(obj.impl, obj);
			IF obj.impl # NIL THEN
				obj.impl.obj := obj;
				res := CreateAnimal(obj, COM.ID(obj.animal), obj.animal);
				IF res >= 0 THEN
					StdLog.String("Koala allocated"); StdLog.Ln;
					RETURN obj.QueryInterface(COM.ID(unk), unk) >= 0
				END
			END
		END;
		RETURN FALSE
	END CreateKoalaAggregation;
	
	
	(* ---------- user interface ---------- *)
	
	PROCEDURE CreateKoala*;
	BEGIN
		IF CreateKoalaAggregation(koala) THEN StdLog.String("Koala created")
		ELSE StdLog.String("Koala creation failed")
		END;
		StdLog.Ln
	END CreateKoala;
	
	PROCEDURE ReleaseKoala*;
	BEGIN
		IF koala # NIL THEN
			koala := NIL;
			StdLog.String("Koala released")
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END ReleaseKoala;
	
	PROCEDURE AnimalEat*;
		VAR a: IAnimal; res: COM.RESULT;
	BEGIN
		IF koala # NIL THEN
			res := koala.QueryInterface(COM.ID(a), a);
			IF res >= 0 THEN
				res := a.Eat();
				StdLog.String("IAnimal.Eat called")
			ELSE StdLog.String("no IAnimal interface")
			END
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END AnimalEat;

	PROCEDURE AnimalSleep*;
		VAR a: IAnimal; res: COM.RESULT;
	BEGIN
		IF koala # NIL THEN
			res := koala.QueryInterface(COM.ID(a), a);
			IF res >= 0 THEN
				res := a.Sleep();
				StdLog.String("IAnimal.Sleep called")
			ELSE StdLog.String("no IAnimal interface")
			END
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END AnimalSleep;

	PROCEDURE AnimalProcreate*;
		VAR a: IAnimal; res: COM.RESULT;
	BEGIN
		IF koala # NIL THEN
			res := koala.QueryInterface(COM.ID(a), a);
			IF res >= 0 THEN
				res := a.Procreate();
				StdLog.String("IAnimal.Procreate called")
			ELSE StdLog.String("no IAnimal interface")
			END
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END AnimalProcreate;

	PROCEDURE KoalaClimbEucalyptusTrees*;
		VAR a: IKoala; res: COM.RESULT;
	BEGIN
		IF koala # NIL THEN
			res := koala.QueryInterface(COM.ID(a), a);
			IF res >= 0 THEN
				res := a.ClimbEucalyptusTrees();
				StdLog.String("IKoala.ClimbEucalyptusTrees called")
			ELSE StdLog.String("no IKoala interface")
			END
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END KoalaClimbEucalyptusTrees;

	PROCEDURE KoalaPouchOpensDown*;
		VAR a: IKoala; res: COM.RESULT;
	BEGIN
		IF koala # NIL THEN
			res := koala.QueryInterface(COM.ID(a), a);
			IF res >= 0 THEN
				res := a.PouchOpensDown();
				StdLog.String("IKoala.PouchOpensDown called")
			ELSE StdLog.String("no IKoala interface")
			END
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END KoalaPouchOpensDown;

	PROCEDURE KoalaSleepForHoursAfterEating*;
		VAR a: IKoala; res: COM.RESULT;
	BEGIN
		IF koala # NIL THEN
			res := koala.QueryInterface(COM.ID(a), a);
			IF res >= 0 THEN
				res := a.SleepForHoursAfterEating();
				StdLog.String("IKoala.SleepForHoursAfterEating called")
			ELSE StdLog.String("no IKoala interface")
			END
		ELSE StdLog.String("no object")
		END;
		StdLog.Ln
	END KoalaSleepForHoursAfterEating;
	

END ComAggregate.

ComAggregate.CreateKoala
ComAggregate.ReleaseKoala
ComAggregate.AnimalEat
ComAggregate.AnimalSleep
ComAggregate.AnimalProcreate
ComAggregate.KoalaClimbEucalyptusTrees
ComAggregate.KoalaPouchOpensDown
ComAggregate.KoalaSleepForHoursAfterEating
