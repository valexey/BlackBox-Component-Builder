MODULE SqlDB;
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
	
	IMPORT SYSTEM, Kernel, Log, Strings, Dates, Meta, Dialog, Services, SqlDrivers;

	(*
		The following framework-defined types are handled by this module, in
		addition to the data types of the language itself:
		Dialog.Currency
		Dialog.List
		Dialog.Combo
		Dates.Date
		Dates.Time
		SqlDB.Blob
	*)

	CONST
		names* = -1; types = -2;
		converted* = 1; truncated* = 2; overflow* = 3; incompatible* = 4; noData* = 5;
		sync* = FALSE; async* = TRUE;
		showErrors* = TRUE; hideErrors* = FALSE;
		readOp = -10; clearOp = -11; openOp = -12; beginOp = -13; endOp = -14;
		commitOp = -15; rollbackOp = -16; callOp = -17; tcallOp = -18;
		update = 2; guardCheck = 4;
		varLen = 48;
		maxOperations = 20;

	TYPE
		Database* = POINTER TO ABSTRACT RECORD
			res*: INTEGER;
			async*: BOOLEAN;
			showErrors*: BOOLEAN
		END;

		Table* = POINTER TO ABSTRACT RECORD
			base-: Database;
			rows*, columns*: INTEGER;
			res*: INTEGER;
			strictNotify*: BOOLEAN	(* strict notification *)
		END;

		String* = POINTER TO ARRAY OF CHAR;

		Row* = RECORD
			fields*: POINTER TO ARRAY OF String;
		END;

		Blob* = RECORD
			len*: INTEGER;
			data*: POINTER TO ARRAY OF BYTE	(* not 0X terminated **)
		END;

		Command* = PROCEDURE (p: ANYPTR);
		TableCommand* = PROCEDURE (t: Table; p: ANYPTR);


		Range = POINTER TO RECORD	(* list of pending update notifications *)
			next: Range;
			adr0, adr1: INTEGER	(* address range belonging to an interactor that has been modified *)
		END;

		StdDatabase = POINTER TO RECORD (Database)
			queue: Elem;	(* list of tasks *)
			tail: Elem;	(* first element of low priority part in queue *)
			range: Range;	(* list of pending notifications *)
			lastRange: Range; (* the last element in the list of ranges - allows for search optimization *)
			action: Action;	(* action which executes queue elements & ranges *)
			id: INTEGER;	(* timestamp of actually executing command *)
			executing: BOOLEAN;
			driver: SqlDrivers.Driver
		END;

		StdTable = POINTER TO RECORD (Table)
			table: SqlDrivers.Table;	(* current result table *)
			db: StdDatabase;	(* = base(StdDatabase) *)
			srows: INTEGER;	(* safe copy of rows *)
			id: INTEGER	(* table creation id *)
		END;

		Elem = POINTER TO RECORD	(* async operations are put into queues of such elements *)
			next: Elem;	(* list of tasks *)
			op: INTEGER;
			table: StdTable;
			statement: String;
			item: Meta.Item;
			cmd: Command;
			tcmd: TableCommand;
			par: ANYPTR
		END;

		Action = POINTER TO RECORD (Services.Action)
			database: StdDatabase	(* database # NIL *)
		END;

		Statement = RECORD	(* representation of one SQL statement *)
			buf: ARRAY 1024 OF CHAR;	(* valid if idx < LEN(buf) *)
			ext: String;	(* valid if idx >= LEN(buf) *)
			idx: INTEGER;	(* actual length without 0X *)
			blobs, last: SqlDrivers.Blob	(* blob parameters *)
		END;

		CurrencyValue = RECORD (Meta.Value)
			value: Dialog.Currency
		END;

		DateValue = RECORD (Meta.Value)
			value: Dates.Date
		END;

		TimeValue = RECORD (Meta.Value)
			value: Dates.Time
		END;

		StringValue = RECORD (Meta.Value)
			value: String
		END;

		BlobValue = RECORD (Meta.Value)
			value: Blob
		END;

		RowValue = RECORD (Meta.Value)
			value: Row
		END;


	VAR
		debug*: BOOLEAN;
		dummy: Meta.Item;	(* remains undefined *)
		ptr: POINTER TO RECORD END;	(* used for ptr field in contructed items *)
		nullStr: ARRAY 5 OF CHAR;


	(* error handling *)

	PROCEDURE WriteString (s: ARRAY OF CHAR);
	BEGIN
		IF debug THEN Log.String(s); Log.Ln END
	END WriteString;


	(** Table interface **)

	PROCEDURE (t: Table) InitBase* (base: Database), NEW;
	BEGIN
		ASSERT(base # NIL, 20); ASSERT((t.base = NIL) OR (t.base = base), 21);
		t.base := base
	END InitBase;

	PROCEDURE (t: Table) Exec* (statement: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (t: Table) Available* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (t: Table) Read* (row: INTEGER; VAR data: ANYREC), NEW, ABSTRACT;
	PROCEDURE (t: Table) Clear*, NEW, ABSTRACT;
	PROCEDURE (t: Table) Call* (command: TableCommand; par: ANYPTR), NEW, ABSTRACT;


	(** Database interface **)

	PROCEDURE (d: Database) Exec* (statement: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (d: Database) Commit*, NEW, ABSTRACT;
	PROCEDURE (d: Database) Rollback*, NEW, ABSTRACT;
	PROCEDURE (d: Database) Call* (command: Command; par: ANYPTR), NEW, ABSTRACT;
	PROCEDURE (d: Database) NewTable* (): Table, NEW, ABSTRACT;


	(* statement expansion *)

	PROCEDURE ReadPath (IN s: ARRAY OF CHAR; OUT t: ARRAY OF CHAR; VAR i: INTEGER; VAR ch: CHAR);
		VAR j: INTEGER;
	BEGIN	(* extract the designator that describes a global variable *)
		j := 0;
		IF (CAP(ch) >= "A") & (CAP(ch) < "Z") THEN
			REPEAT
				t[j] := ch; INC(j);
				INC(i); ch := s[i]
			UNTIL (ch < "0") & (ch # ".") OR ("9" < ch) & (CAP(ch) < "A") OR ("Z" < CAP(ch)) & (ch # "_") OR (j = varLen)
		END;
		t[j] := 0X
	END ReadPath;

	PROCEDURE AddStr (IN s: ARRAY OF CHAR; native: BOOLEAN; max: INTEGER; VAR t: Statement);
		VAR i, len: INTEGER; e: String; ch: CHAR;
	BEGIN	(* append a string to a statement *)
		IF (t.idx + max < LEN(t.buf)) & (t.ext = NIL) THEN
			i := 0; ch := s[0];
			WHILE (i < max) & (ch # 0X) DO
				t.buf[t.idx] := ch; INC(t.idx);
				IF (ch = "'") & ~native THEN t.buf[t.idx] := "'"; INC(t.idx) END;
				INC(i); ch := s[i]
			END
		ELSE
			IF t.ext = NIL THEN
				len := LEN(t.buf); REPEAT len := len * 4 UNTIL t.idx + max < len;
				NEW(t.ext, len); t.buf[t.idx] := 0X; t.ext^ := t.buf$
			ELSIF t.idx + max >= LEN(t.ext^) THEN
				len := LEN(t.ext^); REPEAT len := len * 4 UNTIL t.idx + max < len;
				NEW(e, len); t.ext[t.idx] := 0X; e^ := t.ext^$; t.ext := e
			END;
			i := 0; ch := s[0];
			WHILE (i < max) & (ch # 0X) DO
				t.ext[t.idx] := ch; INC(t.idx);
				IF (ch = "'") & ~native THEN t.ext[t.idx] := "'"; INC(t.idx) END;
				INC(i); ch := s[i]
			END
		END
	END AddStr;

	PROCEDURE AddChar (ch: CHAR; VAR t: Statement);
		VAR e: String;
	BEGIN	(* append a character to a statement *)
		IF (t.idx < LEN(t.buf) - 1) & (t.ext = NIL) THEN
			t.buf[t.idx] := ch; INC(t.idx)
		ELSE
			IF t.ext = NIL THEN
				NEW(t.ext, 4 * LEN(t.buf)); t.buf[t.idx] := 0X; t.ext^ := t.buf$
			ELSIF t.idx = LEN(t.ext^) - 1 THEN
				NEW(e, 4 * LEN(t.ext^)); t.ext[t.idx] := 0X; e^ := t.ext^$; t.ext := e
			END;
			t.ext[t.idx] := ch; INC(t.idx)
		END
	END AddChar;

	PROCEDURE RealToString (x: SHORTREAL; minPrec: SHORTINT; OUT s: ARRAY OF CHAR);
		VAR y: REAL; res: INTEGER;
	BEGIN
		(* find the shortest string that can represent the number as a string *)
		(* this is a hack to avoid conversion problems between binary and textual representations *)
		REPEAT
			Strings.RealToStringForm(x, minPrec, 0, 0, " ", s);
			Strings.StringToReal(s, y, res);
			INC(minPrec)
		UNTIL x = SHORT(y)
	END RealToString;

	PROCEDURE LongToString (x: LONGINT; scale: INTEGER; OUT s: ARRAY OF CHAR);
		VAR d: ARRAY 24 OF CHAR; i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		IF x < 0 THEN s[0] := "-"; i := 1; x := -x END;
		REPEAT d[j] := CHR(x MOD 10 + ORD("0")); INC(j); x := x DIV 10 UNTIL x = 0;
		WHILE j <= scale DO d[j] := "0"; INC(j) END;
		WHILE j > scale DO DEC(j); s[i] := d[j]; INC(i) END;
		IF j > 0 THEN s[i] := "."; INC(i) END;
		WHILE j > 0 DO DEC(j); s[i] := d[j]; INC(i) END;
		s[i] := 0X
	END LongToString;

	PROCEDURE AddItem (d: StdDatabase; item: Meta.Item; native: BOOLEAN; VAR s: Statement);
		TYPE Ptr = POINTER TO ARRAY [1] MAX(INTEGER) DIV 2 OF CHAR;
		VAR base: Meta.Item; ok: BOOLEAN; i, len, res: INTEGER; h: ARRAY 64 OF CHAR;
			mod, typ: Meta.Name; dc: CurrencyValue; dv: DateValue; dt: TimeValue; p: Ptr;
			sc: Meta.Scanner; x, y: REAL; z: SHORTREAL; prec: SHORTINT;
			bv: BlobValue; blob: SqlDrivers.Blob;
	BEGIN	(* read the value represented by an item, translate it into a string, and append this string to a statement *)
		CASE item.typ OF
		| Meta.byteTyp, Meta.sIntTyp, Meta.intTyp:
			Strings.IntToString(item.IntVal(), h); AddStr(h, FALSE, 12, s)
		| Meta.longTyp:
			Strings.IntToString(item.LongVal(), h); AddStr(h, FALSE, 24, s)
		| Meta.sRealTyp:
			z := SHORT(item.RealVal()); prec := 7;
			(* find the shortest string that can represent the number as a string *)
			(* this is a hack to avoid conversion problems between binary and textual representations *)
			REPEAT
				Strings.RealToStringForm(z, prec, 0, 0, " ", h);
				Strings.StringToReal(h, y, res);
				INC(prec)
			UNTIL (z = SHORT(y)) OR (prec = 10);
			AddStr(h, FALSE, 20, s)
		| Meta.realTyp:
			x := item.RealVal(); prec := 16;
			(* find the shortest string that can represent the number as a string *)
			(* this is a hack to avoid conversion problems between binary and textual representations *)
			REPEAT
				Strings.RealToStringForm(x, prec, 0, 0, " ", h);
				Strings.StringToReal(h, y, res);
				INC(prec)
			UNTIL (x = y) OR (prec = 19);
			AddStr(h, FALSE, 28, s)
		| Meta.boolTyp:
			IF item.BoolVal() THEN AddChar("1", s) ELSE AddChar("0", s) END
		| Meta.arrTyp:
			IF item.BaseTyp() = Meta.charTyp THEN	(* string *)
				p := SYSTEM.VAL(Ptr, item.adr);
				IF ~native THEN AddChar("'", s) END;
				AddStr(p^, native, item.Len(), s);
				IF ~native THEN AddChar("'", s) END
			ELSE	(* array of basic type *)
				len := item.Len(); ASSERT(len > 0, 100);
				item.Index(0, base); AddItem(d, base, native, s); i := 1;
				WHILE i < len DO
					AddChar(",", s); AddChar(" ", s);
					item.Index(i, base); AddItem(d, base, native, s);
					INC(i)
				END
			END
		| Meta.recTyp:
			item.GetTypeName(mod, typ);
			IF (mod = "Dialog") & (typ = "Currency") THEN
				item.GetVal(dc, ok); ASSERT(ok, 101);
				LongToString(dc.value.val, dc.value.scale, h); AddStr(h, FALSE, 24, s);
			ELSIF (mod = "Dialog") & (typ = "List") THEN
				item.Lookup("index", base);
				AddItem(d, base, native, s)
			ELSIF (mod = "Dialog") & (typ = "Combo") THEN
				item.Lookup("item", base);
				AddItem(d, base, native, s)
			ELSIF (mod = "Dates") & (typ = "Date") THEN
				item.GetVal(dv, ok); ASSERT(ok, 101);
				IF (dv.value.year = 0) & (dv.value.month = 0) & (dv.value.day = 0) THEN
					AddStr(nullStr, FALSE, 4, s)
				ELSE
					IF ~native THEN AddChar("'", s) END;
					Strings.IntToString(dv.value.year, h);
					AddStr(h, FALSE, 8, s); AddChar("/", s);
					Strings.IntToStringForm(dv.value.month, Strings.decimal, 2, "0", FALSE, h);
					AddStr(h, FALSE, 4, s); AddChar("/", s);
					Strings.IntToStringForm(dv.value.day, Strings.decimal, 2, "0", FALSE, h);
					AddStr(h, FALSE, 4, s);
					IF ~native THEN AddChar("'", s) END
				END
			ELSIF (mod = "Dates") & (typ = "Time") THEN
				item.GetVal(dt, ok); ASSERT(ok, 101);
				IF ~native THEN AddChar("'", s) END;
				Strings.IntToString(dt.value.hour, h); AddStr(h, native, 8, s); AddChar(":", s);
				Strings.IntToString(dt.value.minute, h); AddStr(h, native, 8, s); AddChar(":", s);
				Strings.IntToString(dt.value.second, h); AddStr(h, native, 8, s);
				IF ~native THEN AddChar("'", s) END
			ELSIF (mod = "SqlDB") & (typ = "Blob") THEN
				item.GetVal(bv, ok); ASSERT(ok, 101);
				AddStr(d.driver.blobStr, FALSE, LEN(d.driver.blobStr), s);
				NEW(blob); blob.len := bv.value.len; blob.data := bv.value.data;
				IF s.last = NIL THEN s.blobs := blob ELSE s.last.next := blob END;
				s.last := blob
			ELSE
				sc.ConnectTo(item);
				sc.Scan; ASSERT(~sc.eos, 22);
				AddItem(d, sc.this, native, s); sc.Scan;
				WHILE ~sc.eos DO
					AddChar(",", s); AddChar(" ", s);
					AddItem(d, sc.this, native, s); sc.Scan
				END
			END
		| Meta.ptrTyp:
			item.Deref(base); AddItem(d, base, native, s)
		ELSE HALT(21)
		END
	END AddItem;

	PROCEDURE Compile (d: StdDatabase; IN s: ARRAY OF CHAR; OUT t: Statement);
		VAR i: INTEGER; ch: CHAR; path: ARRAY varLen OF CHAR; quoted, native: BOOLEAN; item: Meta.Item; 
	BEGIN	(* expand designators in an SQL statement by their current values, yielding a correct SQL statement *)
		i := 0; ch := s[0]; t.idx := 0; t.ext := NIL; quoted := FALSE;
		WHILE ch # 0X DO
			IF (ch = ":") & ~quoted & (s[i+1] # "\") THEN
				(* ":\" does not trigger Meta substitution, in order to allow for Windows path names such as in
					"SELECT * FROM C:\directory\databaseName.tableName"
				*)
				INC(i); ch := s[i];
				IF ch = "!" THEN native := TRUE; INC(i); ch := s[i] ELSE native := FALSE END;
				ReadPath(s, path, i, ch); ASSERT(path # "", 20);
				Meta.LookupPath(path, item); ASSERT(item.Valid(), 21); ASSERT(item.obj = Meta.varObj, 22);
				AddItem(d, item, native, t)
			ELSE
				IF ch = "'" THEN quoted := ~quoted END;
				AddChar(ch, t);
				INC(i); ch := s[i]
			END
		END
	END Compile;

	PROCEDURE Execute (d: StdDatabase; VAR s: Statement);
	BEGIN
		IF s.ext # NIL THEN
			s.ext[s.idx] := 0X; d.driver.BeginExec(s.ext^, s.blobs, d.async, d.showErrors, d.res)
		ELSE
			s.buf[s.idx] := 0X; d.driver.BeginExec(s.buf, s.blobs, d.async, d.showErrors, d.res)
		END
	END Execute;


	(* item manipulation *)

	PROCEDURE LookupItem (OUT i: Meta.Item; VAR r: ANYREC; async: BOOLEAN);
		VAR type: Kernel.Type; mod: Kernel.Module; attr: Kernel.ItemAttr;
	BEGIN	(* create a meta item for a global variable passed as VAR parameter *)
		attr.obj := Meta.varObj;
		attr.typ := Meta.recTyp;
		attr.vis := Meta.exported;
		attr.adr := SYSTEM.ADR(r);
		attr.mod := NIL;
		attr.desc := SYSTEM.VAL(Kernel.Type, SYSTEM.TYP(r));
		attr.ptr := NIL;
		attr.ext := NIL;
		IF async THEN	(* check for global variable *)
			mod := Kernel.modList;
			WHILE (mod # NIL) & ((attr.adr < mod.data) OR (attr.adr >= mod.data + mod.dsize)) DO mod := mod.next END;
			ASSERT(mod # NIL, 24);	(* trap if variable was not found in any module *)
			attr.mod := mod
		ELSE
			attr.mod := NIL
		END;
		Meta.GetThisItem(attr, i)
	END LookupItem;

	PROCEDURE ReadItem (item: Meta.Item; t: StdTable; row: INTEGER; VAR col: INTEGER);
		VAR i, len: INTEGER; base: Meta.Item; s: Meta.Scanner; mod, typ: Meta.Name; ok: BOOLEAN;
			r: REAL; date: DateValue; time: TimeValue; cy: CurrencyValue; str: StringValue;
			blob: BlobValue; rw: RowValue; tab: SqlDrivers.Table;
	BEGIN	(* read a value from a result table, and copy it to a global variable represented by a meta item *)
		tab := t.table; tab.res := 0;
		CASE item.typ OF
		| Meta.boolTyp:
			tab.ReadInteger(row, col, i);
			IF i = 0 THEN item.PutBoolVal(FALSE) ELSE item.PutBoolVal(TRUE) END;
			INC(col)
		| Meta.byteTyp, Meta.sIntTyp, Meta.intTyp:
			tab.ReadInteger(row, col, i);
			IF (item.typ = Meta.byteTyp) & ((i < MIN(BYTE)) OR (i > MAX(BYTE)))
				OR (item.typ = Meta.sIntTyp) & ((i < MIN(SHORTINT)) OR (i > MAX(SHORTINT))) THEN
				i := 0; tab.res := overflow
			END;
			item.PutIntVal(i); INC(col)
		| Meta.sRealTyp, Meta.realTyp:
			tab.ReadReal(row, col, r);
			IF (item.typ = Meta.sRealTyp) & ((r < MIN(SHORTREAL)) OR (r > MAX(SHORTREAL))) THEN
				r := 0; tab.res := overflow
			END;
			item.PutRealVal(r); INC(col)
		| Meta.arrTyp:
			IF item.BaseTyp() = Meta.charTyp THEN
				tab.ReadString(row, col, SYSTEM.THISARRAY(item.adr, item.Len()));
				INC(col)
			ELSE
				i := 0; len := item.Len();
				WHILE i # len DO
					item.Index(i, base); ReadItem(base, t, row, col);
					INC(i)
				END
			END
		| Meta.recTyp:
			item.GetTypeName(mod, typ);
			IF (mod = "Dialog") & (typ = "Currency") THEN
				tab.ReadCurrency(row, col, cy.value);
				item.PutVal(cy, ok); ASSERT(ok, 100);
				INC(col)
			ELSIF (mod = "Dialog") & (typ = "List") THEN
				item.Lookup("index", base);
				ReadItem(base, t, row, col)
			ELSIF (mod = "Dialog") & (typ = "Combo") THEN
				item.Lookup("item", base);
				ReadItem(base, t, row, col)
			ELSIF (mod = "Dates") & (typ = "Date") THEN
				tab.ReadDate(row, col, date.value);
				item.PutVal(date, ok); ASSERT(ok, 100);
				INC(col)
			ELSIF (mod = "Dates") & (typ = "Time") THEN
				tab.ReadTime(row, col, time.value);
				item.PutVal(time, ok); ASSERT(ok, 100);
				INC(col)
			ELSIF (mod = "SqlDB") & (typ = "Blob") THEN
				tab.ReadBlob(row, col, blob.value.len, blob.value.data);
				item.PutVal(blob, ok); ASSERT(ok, 100);
				INC(col)
			ELSE
				IF (mod = "SqlDB") & (typ = "Row") THEN
					item.GetVal(rw, ok); ASSERT(ok, 100);
					IF (rw.value.fields = NIL) OR (LEN(rw.value.fields^) # t.columns - col) THEN
						NEW(rw.value.fields, t.columns - col);
						item.PutVal(rw, ok); ASSERT(ok, 100)
					END
				END;
				s.ConnectTo(item);
				s.Scan; WHILE ~s.eos DO ReadItem(s.this, t, row, col); s.Scan END
			END
		| Meta.ptrTyp:
			item.GetBaseType(base);
			IF (base.typ = Meta.arrTyp) & (base.BaseTyp() = Meta.charTyp) THEN
				item.GetVal(str, ok); ASSERT(ok, 100);
				IF row = types THEN tab.ReadType(col, str.value)
				ELSIF row = names THEN tab.ReadName(col, str.value)
				ELSE tab.ReadVarString(row, col, str.value)
				END;
				item.PutVal(str, ok); ASSERT(ok, 101);
				INC(col)
			ELSE
				item.Deref(base);
				ASSERT(base.obj = Meta.varObj, 23);
				ReadItem(base, t, row, col)
			END
		| Meta.procTyp:	(* skip *)
		ELSE HALT(22)
		END;
		IF tab.res > t.res THEN t.res := tab.res END
	END ReadItem;

	PROCEDURE ClearItem (item: Meta.Item);
		VAR s: Meta.Scanner; ok: BOOLEAN; base: Meta.Item; len, i: INTEGER;
	BEGIN	(* zero a global variable represented by a meta item *)
		ASSERT(item.Valid(), 20);
		IF item.vis = Meta.exported THEN
			CASE item.typ OF
			| Meta.byteTyp, Meta.sIntTyp, Meta.intTyp:
				item.PutIntVal(0)
			| Meta.sRealTyp, Meta.realTyp:
				item.PutRealVal(0)
			| Meta.boolTyp:
				item.PutBoolVal(FALSE)
			| Meta.arrTyp:
				IF item.BaseTyp() = Meta.charTyp THEN
					item.PutStringVal("", ok); ASSERT(ok, 22)
				ELSE
					i := 0; len := item.Len();
					WHILE i # len DO
						item.Index(i, base); ClearItem(base);
						INC(i)
					END
				END
			| Meta.recTyp:
				s.ConnectTo(item);
				s.Scan; WHILE ~s.eos DO ClearItem(s.this); s.Scan END
			| Meta.ptrTyp:
				item.Deref(base);
				IF base.obj = Meta.varObj THEN ClearItem(base) END
			END
		END
	END ClearItem;


	(* notification *)

	PROCEDURE NotifyLater (d: StdDatabase; adr0, adr1: INTEGER);
		VAR r: Range;
	BEGIN	(* register an address range for later notification via an action *)
		r := d.range; 
		IF r = NIL THEN
			Services.DoLater(d.action, Services.now)
		ELSE
			IF (d.lastRange # NIL) & (d.lastRange.adr1 < adr0) THEN 
				r := NIL
			ELSE
				WHILE (r # NIL) & ((r.adr1 < adr0) OR (r.adr0 > adr1)) DO r := r.next END
			END
		END;
		IF r # NIL THEN
			r.adr0 := MIN(r.adr0, adr0); r.adr1 := MAX(r.adr1, adr1)
		ELSE
			NEW(r); r.adr0 := adr0; r.adr1 := adr1;
			r.next := d.range; d.range := r;
			d.lastRange := r
		END
	END NotifyLater;

	PROCEDURE Update (d: StdDatabase);
		VAR r: Range;
	BEGIN	(* notify an address range immediately *)
		r := d.range;
		WHILE r # NIL DO
			Dialog.Notify(r.adr0, r.adr1, {update});
			r := r.next
		END;
		IF d.range # NIL THEN Dialog.Notify(0, 0, {guardCheck}) END;	(* check only once, after all updates *)
		d.range := NIL
	END Update;


	(* delayed operations *)

	PROCEDURE DoRead (t: StdTable; row: INTEGER; VAR item: Meta.Item);
		VAR col: INTEGER;
	BEGIN
		WriteString("DoRead");
		ASSERT(t.columns > 0, 20);
		IF row < t.rows THEN
			col := 0; t.res := 0;
			ReadItem(item, t, row, col);
		ELSE
			ClearItem(item);
			t.res := noData
		END;
		IF t.strictNotify THEN
			Dialog.Notify(item.adr, item.adr + item.Size(), {update, guardCheck});
		ELSE
			NotifyLater(t.db, item.adr, item.adr + item.Size())
		END
	END DoRead;

	PROCEDURE DoClear (t: StdTable);
	BEGIN
		WriteString("DoClear");
		IF (t.table # NIL) & (t.db.driver # NIL) THEN
			t.table.Close; t.table := NIL
		END;
		t.res := 0; t.rows := 0; t.columns := 0; t.srows := 0
	END DoClear;

	PROCEDURE DoEndOpen (d: StdDatabase);
		VAR res: INTEGER;
	BEGIN
		WriteString("DoEndOpen");
		ASSERT(d.driver.Ready(), 100);
		d.driver.EndOpen(res);
		d.res := res
	END DoEndOpen;

	PROCEDURE DoBeginExec (d: StdDatabase; t: StdTable; VAR statement: ARRAY OF CHAR);
		VAR res: INTEGER; s: Statement;
	BEGIN
		WriteString("DoBeginExec");
		Compile(d, statement, s);
		IF debug THEN		
			IF s.ext # NIL THEN
				s.ext[s.idx] := 0X; WriteString(s.ext^)
			ELSE
				s.buf[s.idx] := 0X; WriteString(s.buf)
			END
		END;
		IF t # NIL THEN DoClear(t) END;
		Execute(d, s);
		IF t # NIL THEN t.res := d.res END
	END DoBeginExec;

	PROCEDURE DoEndExec (d: StdDatabase; t: StdTable);
		VAR dt: SqlDrivers.Table; rows, columns: INTEGER;
	BEGIN
		IF d.res = 0 THEN
			WriteString("DoEndExec");
			ASSERT(d.driver.Ready(), 100);
			d.driver.EndExec(dt, rows, columns, d.res);
			IF t # NIL THEN
				t.rows := rows; t.srows := rows; t.columns := columns;
				t.table := dt; t.res := d.res;
				IF t.strictNotify THEN
					Dialog.Notify(SYSTEM.ADR(t^), SYSTEM.ADR(t^) + 4, {update, guardCheck})
				ELSE
					NotifyLater(t.db, SYSTEM.ADR(t^), SYSTEM.ADR(t^) + 4)
				END
			ELSE
				ASSERT(columns = 0, 21);
			END
		END
	END DoEndExec;

	PROCEDURE DoCommit (d: StdDatabase; accept: BOOLEAN);
	BEGIN
		WriteString("DoCommit");
		d.driver.Commit(accept, d.res)
	END DoCommit;


	(* serialization *)

	PROCEDURE Reset (d: StdDatabase);
		VAR e: Elem;
	BEGIN	(* trap cleanup *)
		e := d.queue; d.queue := NIL; d.range := NIL; d.tail := NIL;
		WHILE e # NIL DO
			IF e.table # NIL THEN e.table.Clear END;
			IF e.item.Valid() THEN ClearItem(e.item) END;
			e := e.next
		END
	END Reset;

	PROCEDURE Process (d: StdDatabase);
		VAR e: Elem; operations: INTEGER;
	BEGIN	(* this is the heart of the processing of asynchronous operations *)
		WriteString("start processing");
		operations := 0;
		WHILE (d.queue # NIL) & (operations < maxOperations) & d.driver.Ready() DO
			e := d.queue; d.queue := e.next; d.tail := d.queue; INC(operations);
			CASE e.op OF
			| openOp: DoEndOpen(d)
			| beginOp: DoBeginExec(d, e.table, e.statement^)
			| endOp: DoEndExec(d, e.table)
			| commitOp: DoCommit(d, TRUE)
			| rollbackOp: DoCommit(d, FALSE)
			| clearOp: DoClear(e.table)
			| callOp: INC(d.id); e.cmd(e.par)
			| tcallOp: INC(d.id); e.table.id := d.id; e.tcmd(e.table, e.par)
			ELSE DoRead(e.table, e.op, e.item)
			END;
			d.tail := NIL
		END;
		WriteString("stop processing")
	END Process;

	PROCEDURE (a: Action) Do;
		VAR d: StdDatabase;
	BEGIN	(* async operations are advanced by an action, which belongs to the database object *)
		d := a.database;
		IF d.executing THEN	(* trapped *)
			Reset(d)
		ELSIF (d.queue # NIL) OR (d.range # NIL) THEN
			Services.DoLater(a, Services.now);
			d.executing := TRUE;
			Process(d);
			Update(d)
		END;
		d.executing := FALSE
	END Do;

	PROCEDURE Put (d: StdDatabase; op: INTEGER; t: StdTable; s: ARRAY OF CHAR;
								VAR i: Meta.Item; cmd: Command; tcmd: TableCommand; par: ANYPTR);
		VAR e, h, k: Elem;
	BEGIN	(* put a SQL statement into a queue for later async processing *)
		NEW(e); e.op := op; e.table := t; e.item := i; e.cmd := cmd; e.tcmd := tcmd; e.par := par;
		IF s # "" THEN NEW(e.statement, LEN(s)); e.statement^ := s$ END;
		h := d.queue;
		IF h = NIL THEN Services.DoLater(d.action, Services.now) END;	(* start process *)
		IF h = d.tail THEN
			e.next := h; d.queue := e
		ELSIF op >= clearOp THEN	(* clear, read: insert after last op on same table *)
			k := NIL;
			WHILE h # d.tail DO
				IF h.table = t THEN k := h END;
				h := h.next
			END;
			IF k = NIL THEN e.next := d.queue; d.queue := e
			ELSE e.next := k.next; k.next := e
			END
		ELSE
			WHILE h.next # d.tail DO h := h.next END;
			e.next := h.next; h.next := e
		END
	END Put;

	PROCEDURE Synch (d: StdDatabase; op: INTEGER; t: StdTable): BOOLEAN;
		(* check whether it is necessary to enqueue this operation *)
		(* TRUE if Put would insert at beginning of queue *)
		VAR h: Elem;
	BEGIN
		h := d.queue;
		IF h = d.tail THEN
			RETURN TRUE
		ELSIF op >= clearOp THEN
			WHILE (h # d.tail) & (h.table # t) DO h := h.next END;
			RETURN h = d.tail
		ELSE
			RETURN FALSE
		END
	END Synch;
	

	(* StdTable *)

	PROCEDURE (t: StdTable) Exec (statement: ARRAY OF CHAR);
	BEGIN
		WriteString("Table Exec");
		ASSERT(t.db.driver # NIL, 100); ASSERT(statement # "", 20);
		ASSERT(~t.db.executing OR (t.id >= t.db.id), 21);
		IF Synch(t.db, beginOp, t) THEN DoBeginExec(t.db, t, statement)
		ELSE Put(t.db, beginOp, t, statement, dummy, NIL, NIL, NIL)
		END;
		IF Synch(t.db, endOp, t) & t.db.driver.Ready() THEN DoEndExec(t.db, t)
		ELSE Put(t.db, endOp, t, "", dummy, NIL, NIL, NIL)
		END
	END Exec;

	PROCEDURE (t: StdTable) Available (): BOOLEAN;
	BEGIN
		RETURN Synch(t.db, readOp, t)
	END Available;

	PROCEDURE (t: StdTable) Read (row: INTEGER; VAR data: ANYREC);
		VAR item: Meta.Item;
	BEGIN
		WriteString("Read");
		ASSERT((row >= 0) OR (data IS Row) & (row >= -2), 21);
		ASSERT(~t.db.executing OR (t.id >= t.db.id), 25);
		IF Synch(t.db, readOp, t) THEN
			LookupItem(item, data, FALSE);
			DoRead(t, row, item)
		ELSE
			LookupItem(item, data, TRUE);
			Put(t.db, row, t, "", item, NIL, NIL, NIL)
		END
	END Read;

	PROCEDURE (t: StdTable) Clear;
	BEGIN
		WriteString("Clear");
		ASSERT(~t.db.executing OR (t.id >= t.db.id), 20);
		IF Synch(t.db, clearOp, t) THEN DoClear(t)
		ELSE Put(t.db, clearOp, t, "", dummy, NIL, NIL, NIL)
		END
	END Clear;

	PROCEDURE (t: StdTable) Call (command: TableCommand; par: ANYPTR);
	BEGIN
		WriteString("Table Call");
		ASSERT(command # NIL, 20);
		ASSERT(~t.db.executing OR (t.id >= t.db.id), 21);
		IF Synch(t.db, tcallOp, t) THEN command(t, par)
		ELSE Put(t.db, tcallOp, t, "", dummy, NIL, command, par)
		END
	END Call;


	(* StdDatabase *)

	PROCEDURE (d: StdDatabase) Exec (statement: ARRAY OF CHAR);
	BEGIN
		WriteString("Exec");
		ASSERT(d.driver # NIL, 100); ASSERT(statement # "", 20);
		IF Synch(d, beginOp, NIL) THEN DoBeginExec(d, NIL, statement)
		ELSE Put(d, beginOp, NIL, statement, dummy, NIL, NIL, NIL)
		END;
		IF Synch(d, endOp, NIL) & d.driver.Ready() THEN DoEndExec(d, NIL)
		ELSE Put(d, endOp, NIL, "", dummy, NIL, NIL, NIL)
		END
	END Exec;

	PROCEDURE (d: StdDatabase) Commit;
	BEGIN
		WriteString("Commit");
		IF Synch(d, commitOp, NIL) THEN DoCommit(d, TRUE)
		ELSE Put(d, commitOp, NIL, "", dummy, NIL, NIL, NIL)
		END
	END Commit;

	PROCEDURE (d: StdDatabase) Rollback;
	BEGIN
		WriteString("Rollback");
		IF Synch(d, rollbackOp, NIL) THEN DoCommit(d, FALSE)
		ELSE Put(d, rollbackOp, NIL, "", dummy, NIL, NIL, NIL)
		END
	END Rollback;

	PROCEDURE (d: StdDatabase) Call (command: Command; par: ANYPTR);
	BEGIN
		WriteString("Call");
		ASSERT(command # NIL, 20);
		IF Synch(d, callOp, NIL) THEN command(par)
		ELSE Put(d, callOp, NIL, "", dummy, command, NIL, par)
		END
	END Call;

	PROCEDURE (d: StdDatabase) NewTable (): Table;
		VAR t: StdTable;
	BEGIN
		ASSERT(d.driver # NIL, 100);
		NEW(t);
		t.res := 0; t.rows := 0; t.columns := 0; t.strictNotify := FALSE;
		t.table := NIL; t.srows := 0; t.id := d.id;
		t.InitBase(d); t.db := d;
		d.res := 0;
		RETURN t
	END NewTable;


	PROCEDURE OpenDatabase* (protocol, id, password, datasource: ARRAY OF CHAR;
													async, showErrors: BOOLEAN; OUT d: Database; OUT res: INTEGER);
		VAR driver: SqlDrivers.Driver; sd: StdDatabase;
	BEGIN
		WriteString("OpenDatabase");
		ASSERT(protocol # "", 20);
		SqlDrivers.Open(protocol, id, password, datasource, async, showErrors, driver, res);
		IF res = 0 THEN
			ASSERT(driver # NIL, 100);
			NEW(sd); sd.driver := driver; sd.queue := NIL; sd.tail := NIL;
			sd.async := async; sd.showErrors := showErrors;
			NEW(sd.action); sd.action.database := sd;
			IF driver.Ready() THEN DoEndOpen(sd)
			ELSE Put(sd, openOp, NIL, "", dummy, NIL, NIL, NIL)
			END;
			sd.res := 0; d := sd
		ELSE
			d := NIL
		END
	END OpenDatabase;

BEGIN
	NEW(ptr);
	nullStr := "null"
END SqlDB.
