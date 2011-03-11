MODULE ObxViews14;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	purpose	= "Same as ObxViews13, but interfaces and implementations separated,
			and operation directly in Insert procedure"
	changes	= ""
	issues	= ""

**)

	IMPORT Fonts, Ports, Stores, Models, Views, Controllers, Properties;

	CONST d = 20 * Ports.point;

	TYPE
		Model* = POINTER TO ABSTRACT RECORD (Models.Model) END;

		ModelDirectory* = POINTER TO ABSTRACT RECORD END;

		View* = POINTER TO ABSTRACT RECORD (Views.View) END;

		Directory* = POINTER TO ABSTRACT RECORD END;


		StdModel = POINTER TO RECORD (Model)
			i: INTEGER;			(* position of next free slot in string *)
			s: ARRAY 256 OF CHAR	(* string *)
		END;

		StdModelDirectory = POINTER TO RECORD (ModelDirectory) END;

		StdView = POINTER TO RECORD (View)
			model: Model
		END;

		StdDirectory = POINTER TO RECORD (Directory) END;

		PasteCharOp = POINTER TO RECORD (Stores.Operation)
			model: StdModel;
			char: CHAR;
			do: BOOLEAN
		END;


	VAR
		mdir-: ModelDirectory;
		dir-: Directory;


	(* Model *)

	PROCEDURE (m: Model) Insert* (char: CHAR), NEW, ABSTRACT;
	PROCEDURE (m: Model) Remove*, NEW, ABSTRACT;
	PROCEDURE (m: Model) GetString* (OUT s: ARRAY OF CHAR), NEW, ABSTRACT;

	(* ModelDirectory *)

	PROCEDURE (d: ModelDirectory) New* (): Model, NEW, ABSTRACT;

	(* PasteCharOp *)

	PROCEDURE (op: PasteCharOp) Do;
		VAR m: StdModel; msg: Models.UpdateMsg;
	BEGIN
		m := op.model;
		IF op.do THEN				(* do operation's transformation *)
			m.s[m.i] := op.char; INC(m.i);
		ELSE						(* undo operation's transformation *)
			DEC(m.i)				(* remove character from string *)
		END;
		m.s[m.i] := 0X;
		op.do := ~op.do;			(* toggle between "do" and "undo" *)
		Models.Broadcast(m, msg)	(* update all views on this model *)
	END Do;


	(* StdModel *)

	PROCEDURE (m: StdModel) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(0, 0, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(m.i); rd.ReadString(m.s)
		END
	END Internalize;

	PROCEDURE (m: StdModel) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(0);
		wr.WriteInt(m.i); wr.WriteString(m.s)
	END Externalize;

	PROCEDURE (m: StdModel) CopyFrom (source: Stores.Store);
	BEGIN
		WITH source: StdModel DO
			m.i := source.i; m.s := source.s
		END
	END CopyFrom;


	PROCEDURE (m: StdModel) Insert (char: CHAR);
		VAR op: PasteCharOp;
	BEGIN
		NEW(op); op.model := m; op.char := char; op.do := TRUE;
		Models.Do(m, "insertion", op)
	END Insert;

	PROCEDURE (m: StdModel) Remove;
		VAR msg: Models.UpdateMsg;
	BEGIN
		DEC(m.i); m.s[m.i] := 0X;
		Models.Broadcast(m, msg)	(* update all views on this model *)
	END Remove;

	PROCEDURE (m: StdModel) GetString (OUT s: ARRAY OF CHAR);
	BEGIN
		s := m.s$
	END GetString;


	(* StdModelDirectory *)

	PROCEDURE (d: StdModelDirectory) New (): Model;
		VAR m: StdModel;
	BEGIN
		NEW(m); m.s := "";  m.i := 0; RETURN m
	END New;



	(* Directory *)

	PROCEDURE (d: Directory) New* (m: Model): View, NEW, ABSTRACT;


	(* StdView *)

	PROCEDURE (v: StdView) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; st: Stores.Store;
	BEGIN
		rd.ReadVersion(0, 0, version);
		IF ~rd.cancelled THEN
			rd.ReadStore(st);
			IF st IS Model THEN
				v.model := st(Model)
			ELSE
				(* concrete model implementation couldn't be loaded->
					an alien store was created *)
				rd.TurnIntoAlien(Stores.alienComponent)
				(* internalization of v is cancelled *)
			END
		END
	END Internalize;

	PROCEDURE (v: StdView) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(0);
		wr.WriteStore(v.model)
	END Externalize;

	PROCEDURE (v: StdView) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		WITH source: StdView DO
			v.model := model(Model)
		END
	END CopyFromModelView;

	PROCEDURE (v: StdView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		v.model.GetString(s);
		f.DrawString(d, d, Ports.black, s, Fonts.dir.Default())
	END Restore;

	PROCEDURE (v: StdView) ThisModel (): Models.Model;
	BEGIN
		RETURN v.model
	END ThisModel;

	PROCEDURE (v: StdView) HandleModelMsg (VAR msg: Models.Message);
	BEGIN
		Views.Update(v, Views.keepFrames)	(* restore v in any frame that displays it *)
	END HandleModelMsg;

	PROCEDURE (v: StdView) HandleCtrlMsg (f: Views.Frame;
										VAR msg: Controllers.Message;
										VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN
				v.model.Insert(msg.char)
			END
		ELSE						(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE (v:StdView) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				msg.w := 10 * d; msg.h := 2 *d 
			END
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;


	(* StdDirectory *)

	PROCEDURE (d: StdDirectory) New* (m: Model): View;
		VAR v: StdView;
	BEGIN
		ASSERT(m # NIL, 20);
		NEW(v); v.model := m; Stores.Join(v, m);
		RETURN v
	END New;


	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		v := dir.New(mdir.New());
		Views.Deposit(v)
	END Deposit;

	PROCEDURE SetModelDir* (d: ModelDirectory);
	BEGIN
		ASSERT(d # NIL, 20);
		mdir := d
	END SetModelDir;

	PROCEDURE SetDir* (d: Directory);
	BEGIN
		ASSERT(d # NIL, 20);
		dir := d
	END SetDir;

	PROCEDURE Init;
		VAR md: StdModelDirectory; d: StdDirectory;
	BEGIN
		NEW(md); mdir := md;
		NEW(d); dir := d
	END Init;

BEGIN
	Init
END ObxViews14.
