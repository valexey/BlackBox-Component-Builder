MODULE SqlDrivers;
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

	IMPORT Meta, Dates, Dialog;

	CONST
		noDriverMod = 1;
		noDriverProc = 2;
		wrongDriverType = 3;
		connectionsExceeded = 4;
		outOfTables = 5;
		notExecutable = 6;
		cannotOpenDB = 7;
		wrongIdentification = 8;
		tooManyBlobs = 9;
		
		converted = 1;
		truncated = 2;
		overflow = 3;
		incompatible = 4;
		noData = 5;

	TYPE
		Driver* = POINTER TO ABSTRACT RECORD
			blobStr-: ARRAY 8 OF CHAR;
			open: BOOLEAN;
			tables: INTEGER;	(* number of currently open tables for this driver *)
		END;
		
		Table* = POINTER TO ABSTRACT RECORD
			(** an open table must anchor its driver; a closed table must not anchor it **)
			res*: INTEGER;	(** sticky result, replaced by higher values only **)
			open: BOOLEAN;
			driver: Driver	(* only used to prevent too early garbage collection of driver *)
		END;

		String* = POINTER TO ARRAY OF CHAR;
		
		Blob* = POINTER TO RECORD
			len*: INTEGER;
			data*: POINTER TO ARRAY OF BYTE;	(** not 0X terminated **)
			next*: Blob
		END;

		DriverAllocator = PROCEDURE (id, password, datasource: ARRAY OF CHAR;
													async, showErr: BOOLEAN; OUT d: Driver; OUT res: INTEGER);
		(* Post: res = 0 <=> connection has started and should be completed later with EndOpen
					res = 0 <=> d # NIL
					~async & (res = 0) => d.Ready() *)


	PROCEDURE (d: Driver) Ready* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (d: Driver) EndOpen* (OUT res: INTEGER), NEW, ABSTRACT;
	(** Pre: d.Ready()	20 **)

	PROCEDURE (d: Driver) BeginExec* (IN statement: ARRAY OF CHAR; data: Blob;
															async, showErrors: BOOLEAN; OUT res: INTEGER), NEW, ABSTRACT;
	(** asynchronous execution is optional, a driver may legally execute synchronously if async is TRUE **)
	(** Pre: statement # ""	20
				no other execution started	21 **)
	(** Post: res = 0 <=> execution has started and should be completed later with EndExec
				~async => d.Ready() **)

	PROCEDURE (d: Driver) EndExec* (VAR t: Table; OUT rows, columns, res: INTEGER), NEW, ABSTRACT;
	(** Pre: execution must have been started successfully with BeginExec	20
				d.Ready()	21 **)
	(** Post: res = 0  <=>  (t # NIL) OR (rows = 0) & (columns = 0) **)
	
	PROCEDURE (d: Driver) Commit* (accept: BOOLEAN; OUT res: INTEGER), NEW, ABSTRACT;
	
	PROCEDURE (d: Driver) Cleanup-, NEW, EMPTY;
	
	PROCEDURE (d: Driver) Close*, NEW;
	BEGIN
		IF d.open THEN
			IF d.tables = 0 THEN
				d.Cleanup	(* release driver info *)
			END;
			d.open := FALSE;
		END
	END Close;

	PROCEDURE (d: Driver) Init* (blobStr: ARRAY OF CHAR), NEW;
	BEGIN
		d.blobStr := blobStr$;
		d.open := TRUE
	END Init;
	
	PROCEDURE (d: Driver) FINALIZE-;
	BEGIN
		d.Close
	END FINALIZE;

	
	PROCEDURE (t: Table) ReadInteger* (row, column: INTEGER; OUT val: INTEGER), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadReal* (row, column: INTEGER; OUT val: REAL), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadDate* (row, column: INTEGER; OUT val: Dates.Date), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadTime* (row, column: INTEGER; OUT val: Dates.Time), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadCurrency* (row, column: INTEGER; OUT val: Dialog.Currency), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadString* (row, column: INTEGER; OUT str: ARRAY OF CHAR), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadVarString* (row, column: INTEGER; OUT str: String), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadBlob* (row, column: INTEGER; OUT len: INTEGER;
														OUT data: POINTER TO ARRAY OF BYTE), NEW, ABSTRACT;
	(** Pre: row >= 0	20
				row < rows	21
				column >= 0	22
				column < columns	23 **)

	PROCEDURE (t: Table) ReadName* (column: INTEGER; OUT str: String), NEW, ABSTRACT;
	(** Pre: column >= 0	20
				column < columns	21 **)

	PROCEDURE (t: Table) ReadType* (column: INTEGER; OUT str: String), NEW, ABSTRACT;
	(** Pre: column >= 0	20
				column < columns	21 *)

	PROCEDURE (t: Table) Cleanup-, NEW, EMPTY;
	
	PROCEDURE (t: Table) Close*, NEW;
	BEGIN
		IF t.open THEN
			t.Cleanup;	(* release table info *)
			DEC(t.driver.tables);
			IF (t.driver.tables = 0) & ~t.driver.open THEN
				t.driver.Cleanup	(* release driver info *)
			END;
			t.driver := NIL;	(* allow for garbage collection of driver *)
			t.open := FALSE
		END
	END Close;
	
	PROCEDURE (t: Table) Init* (d: Driver), NEW;
	BEGIN
		ASSERT(d.open, 20);
		t.driver := d; INC(d.tables);
		t.open := TRUE
	END Init;
	
	PROCEDURE (t: Table) FINALIZE-;
	BEGIN
		t.Close
	END FINALIZE;
	

	PROCEDURE Open* (protocol, id, password, datasource: ARRAY OF CHAR;
											async, showErrors: BOOLEAN; OUT d: Driver; OUT res: INTEGER);
		VAR ok: BOOLEAN; m, p: Meta.Item; mod: Meta.Name;
			v: RECORD (Meta.Value)
				Open: DriverAllocator
			END;
	BEGIN
		ASSERT(protocol # "", 20);
		mod := protocol$; Meta.Lookup(mod, m); d := NIL; res := 0;
		IF m.obj = Meta.modObj THEN
			m.Lookup("Open", p);
			IF p.obj = Meta.procObj THEN
				p.GetVal(v, ok);
				IF ok THEN
					v.Open(id, password, datasource, async, showErrors, d, res);
				ELSE res := wrongDriverType
				END
			ELSE res := noDriverProc
			END
		ELSE res := noDriverMod
		END
	END Open;

END SqlDrivers.
