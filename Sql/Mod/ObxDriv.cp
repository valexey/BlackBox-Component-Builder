MODULE SqlObxDriv;
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

	(* this is a template to be filled out *)

	IMPORT Dates, Dialog, Services, SqlDrivers;

	CONST
		(* general error code *)
		connectionsExceeded = 4;
		outOfTables = 5;
		notExecutable = 6;
		cannotOpenDB = 7;
		wrongIdentification = 8;
		
		(* table read error codes *)
		converted = 1;
		truncated = 2;
		overflow = 3;
		incompatible = 4;
		
		(* states *)
		connecting = 1;
		connected = 2;
		executing = 3;
		closed = 4;
		
	TYPE
		Table = POINTER TO RECORD (SqlDrivers.Table)
			rows, columns: INTEGER;
			(* additional resultset related data *)
		END;

		Driver = POINTER TO RECORD (SqlDrivers.Driver)
			(* the state variable is not strictly necessary, it is used here to show the conceptual states *)
			state: INTEGER;	(* connecting, connected, executing, closed *)
			(* additional database related data *)
		END;

	
	(* Driver *)

	PROCEDURE (d: Driver) Ready (): BOOLEAN;
	BEGIN
		(* check for completion if asynchronous operation in progress *)
		RETURN (d.state # closed) & <asynchronous operation terminated>
	END Ready;

	PROCEDURE (d: Driver) EndOpen (OUT res: INTEGER);
	(* Pre: database connection was initiated by Open	20
				d.Ready()	21 *)
	BEGIN
		ASSERT(d.state = connecting, 20);
		ASSERT(d.Ready(), 21);
		(* deliver result of asynchronous database connection request *)
		(* return res = 0 if open is synchronous *)
		d.state := connected; res := 0
	END EndOpen;

	PROCEDURE (d: Driver) BeginExec (IN statement: ARRAY OF CHAR; data: SqlDrivers.Blob;
															async, showErr: BOOLEAN; OUT res: INTEGER);
	(* asynchronous execution is optional, a driver may legally execute synchronously if async is TRUE *)
	(* Pre: statement # ""	20
				no other execution started	21 *)
	(* Post: res = 0 <=> execution has started and will be completed later with EndExec
				~async => d.Ready() *)
	BEGIN
		ASSERT(statement # "", 20);
		ASSERT(d.state = connected, 21);
		WHILE data # NIL DO
			(* pass blob data in database specific way *)
			data := data.next
		END;
		IF async THEN
			(* try to start asynchronous execution of statement *)
		ELSE
			(* try to execute statement *)
		END;
		IF <successful> THEN
			d.state := executing; res := 0
		ELSE
			res := notExecutable
		END
	END BeginExec;

	PROCEDURE (d: Driver) EndExec (VAR t: SqlDrivers.Table; OUT rows, columns, res: INTEGER);
	(* Pre: execution was started successfully with BeginExec	20
				d.Ready()	21 *)
	(* Post: res = 0  <=>  (t # NIL) OR (rows = 0) & (columns = 0) *)
		VAR h: Table;
	BEGIN
		ASSERT(d.state = executing, 20);
		ASSERT(d.Ready(), 21);
		d.state := connected;
		IF <successful execution returning a result set> THEN
			NEW(h); h.Init(d);
			(* initialize table data *)
			h.columns := <number of columns in result set>;	(* h.columns > 0 *)
			h.rows := <number of rows in result set>;		(* h.rows >= 0 *)
			t := h; columns := h.columns; rows := h.rows; res := 0
		ELSIF <successful execution without result set> THEN
			t := NIL; columns := 0; rows := 0; res := 0
		ELSE (* execution failed *)
			t := NIL; columns := 0; rows := 0; res := notExecutable
		END
	END EndExec;

	PROCEDURE (d: Driver) Commit (accept: BOOLEAN; OUT res: INTEGER);
	BEGIN
		IF accept THEN
			(* commit actual transaction *)
			res := 0
		ELSE
			IF <rollback possible> THEN
				(* rollback actual transaction *)
				res := 0
			ELSE res := notExecutable
			END
		END
	END Commit;

	PROCEDURE (d: Driver) Cleanup;
	BEGIN
		(* cleanup database data *)
		(* release database specific resources *)
		d.state := closed
	END Cleanup;
	

	(* Table *)
	
	PROCEDURE (t: Table) ReadInteger (row, column: INTEGER; OUT val: INTEGER);
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read an integer from table *)
		IF <table value is not an integer> THEN val := 0; t.res := incompatible
		ELSIF <table value is out of range> THEN val := 0; t.res := overflow
		ELSE val := <value>
		END
	END ReadInteger;

	PROCEDURE (t: Table) ReadReal (row, column: INTEGER; OUT val: LONGREAL);
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a real from table *)
		IF <table value is not a real> THEN val := 0; t.res := incompatible
		ELSIF <table value is out of range> THEN val := 0; t.res := overflow
		ELSE val := <value>
		END
	END ReadReal;

	PROCEDURE (t: Table) ReadDate (row, column: INTEGER; OUT val: Dates.Date);
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a date from table *)
		IF <table value is not a date> THEN
			val.year := 0; val.month := 0; val.day := 0; t.res := incompatible
		ELSE
			val.year := <value>; val.month := <value>; val.day := <value>
		END
	END ReadDate;

	PROCEDURE (t: Table) ReadTime (row, column: INTEGER; OUT val: Dates.Time);
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a time from table *)
		IF <table value is not a time> THEN
			val.hour := 0; val.minute := 0; val.second := 0; t.res := incompatible
		ELSE
			val.hour := <value>; val.minute := <value>; val.second := <value>
		END
	END ReadTime;

	PROCEDURE (t: Table) ReadCurrency (row, column: INTEGER; OUT val: Dialog.Currency);
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a currency from table *)
		IF <table value is not a currency> THEN
			val.val := 0; val.scale := 0; t.res := incompatible
		ELSE
			val.val := <value>; val.scale := <value>
		END
	END ReadCurrency;

	PROCEDURE (t: Table) ReadString (row, column: INTEGER; OUT str: ARRAY OF CHAR);
		(* any value should be readable as a string *)
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a string from table *)
		IF <table value is not a string> THEN
			(* convert table value to a string *)
			t.res := converted
		END;
		IF <length of string (incl 0X)> > LEN(str) THEN
			(* copy truncated string to str *)
			t.res := truncated
		ELSE
			(* copy string to str *)
		END
	END ReadString;

	PROCEDURE (t: Table) ReadVarString (row, column: INTEGER; OUT str: SqlDrivers.String);
		(* any value should be readable as a string *)
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a string from table *)
		IF <table value is not a string> THEN
			(* convert table value to a string *)
			t.res := converted
		END;
		IF (str = NIL) OR (<length of string (incl 0X)> > LEN(str^)) THEN
			NEW(str, <length of string (incl 0X)>)
		END;
		(* copy string to str^ *)
	END ReadVarString;
	
	PROCEDURE (t: Table) ReadBlob (row, column: INTEGER; VAR len: INTEGER;
														OUT data: POINTER TO ARRAY OF BYTE);
		(* blobs are not zero terminated *)
	BEGIN
		ASSERT(row >= 0, 20); ASSERT(row < t.rows, 21);
		ASSERT(column >= 0, 22); ASSERT(column < t.columns, 23);
		(* try to read a blob from table *)
		IF <table value is not a blob> THEN
			len := 0; t.res := incompatible
		ELSE
			len := <length of blob>;
			IF (data = NIL) OR (len > LEN(data^)) THEN NEW(str, len) END;
			(* copy blob data to data^ *)
		END
	END ReadBlob;

	PROCEDURE (t: Table) ReadName (column: INTEGER; OUT str: SqlDrivers.String);
	BEGIN
		ASSERT(column >= 0, 20); ASSERT(column < t.columns, 21);
		(* get column name *)
		IF (str = NIL) OR (<length of name (incl 0X)> > LEN(str^)) THEN
			NEW(str, <length of name (incl 0X)>)
		END;
		(* copy name to str^ *)
	END ReadName;

	PROCEDURE (t: Table) ReadType (column: INTEGER; OUT str: SqlDrivers.String);
	BEGIN
		ASSERT(column >= 0, 20); ASSERT(column < t.columns, 21);
		(* get column type *)
		IF (str = NIL) OR (<length of type (incl 0X)> > LEN(str^)) THEN
			NEW(str, <length of type (incl 0X)>)
		END;
		(* copy type to str^ *)
	END ReadType;

	PROCEDURE (t: Table) Cleanup;
	BEGIN
		(* cleanup resultset data *)
		(* release resultset specific resources *)
	END Cleanup;


	PROCEDURE Open* (id, password, datasource: ARRAY OF CHAR;
												async, showErr: BOOLEAN; VAR d: SqlDrivers.Driver; OUT res: INTEGER);
	(* asynchronous operation is optional, a driver may legally open synchronously if async is TRUE *)
	(* Post: res = 0 <=> connection has started and will be completed later with EndOpen
				res = 0 <=> d # NIL
				~async & (res = 0) => d.Ready() *)
		VAR h: Driver;
	BEGIN
		(* check id & password *)
		IF <valid> THEN
			IF async THEN
				(* try to start connect request to datasource *)
			ELSE
				(* try to connect to datasource *)
			END;
			IF <successful> THEN
				NEW(h); h.Init("<blob representation in statements>");
				(* Initialize driver data *)
				h.state := connecting; d := h; res := 0
			ELSE
				d := NIL; res := cannotOpenDB
			END
		ELSE
			d := NIL; res := wrongIdentification
		END
	END Open;

END SqlObxDriv.
