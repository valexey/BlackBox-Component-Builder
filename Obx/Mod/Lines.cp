MODULE ObxLines;
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

	IMPORT Stores, Ports, Models, Views, Controllers, Properties;

	CONST minVersion = 0; maxVersion = 0;

	TYPE
		Line = POINTER TO RECORD
			next: Line;
			x0, y0, x1, y1: INTEGER
		END;

		Model = POINTER TO RECORD (Models.Model)
			lines: Line
		END;

		View = POINTER TO RECORD (Views.View)
			color: Ports.Color;
			model: Model
		END;

		UpdateMsg = RECORD (Models.UpdateMsg)
			l, t, r, b: INTEGER
		END;

		LineOp = POINTER TO RECORD (Stores.Operation)
			model: Model;
			line: Line
		END;

		ColorOp = POINTER TO RECORD (Stores.Operation)
			view: View;
			color: Ports.Color
		END;


	PROCEDURE GetBox (x0, y0, x1, y1: INTEGER; VAR l, t, r, b: INTEGER);
	BEGIN
		IF x0 > x1 THEN l := x1; r := x0 ELSE l := x0; r := x1 END;
		IF y0 > y1 THEN t := y1; b := y0 ELSE t := y0; b := y1 END;
		INC(r, Ports.point); INC(b, Ports.point)
	END GetBox;


	PROCEDURE (op: LineOp) Do;
		VAR l: Line; msg: UpdateMsg;
	BEGIN
		l := op.line;
		IF l # op.model.lines THEN	(* insert op.line *)
			ASSERT(l # NIL, 100); ASSERT(l.next = op.model.lines, 101);
			op.model.lines := l
		ELSE	(* delete op.line *)
			ASSERT(l = op.model.lines, 102);
			op.model.lines := l.next
		END;
		GetBox(l.x0, l.y0, l.x1, l.y1, msg.l, msg.t, msg.r, msg.b); Models.Broadcast(op.model, msg)
	END Do;

	PROCEDURE (m: Model) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; x0: INTEGER; p: Line;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(x0); m.lines := NIL;
			WHILE x0 # MIN(INTEGER) DO
				NEW(p); p.next := m.lines; m.lines := p;
				p.x0 := x0; rd.ReadInt(p.y0); rd.ReadInt(p.x1); rd.ReadInt(p.y1);
				rd.ReadInt(x0)
			END
		END
	END Internalize;

	PROCEDURE (m: Model) Externalize (VAR wr: Stores.Writer);
		VAR p: Line;
	BEGIN
		wr.WriteVersion(maxVersion);
		p := m.lines;
		WHILE p # NIL DO
			wr.WriteInt(p.x0); wr.WriteInt(p.y0); wr.WriteInt(p.x1); wr.WriteInt(p.y1);
			p := p.next
		END;
		wr.WriteInt(MIN(INTEGER))
	END Externalize;

	PROCEDURE (m: Model) CopyFrom (source: Stores.Store);
	BEGIN
		m.lines := source(Model).lines	(* lines are immutable and thus can be shared *)
	END CopyFrom;

	PROCEDURE (m: Model) Insert (x0, y0, x1, y1: INTEGER), NEW;
		VAR op: LineOp; p: Line;
	BEGIN
		NEW(op); op.model := m;
		NEW(p); p.next := m.lines; op.line := p;
		p.x0 := x0; p.y0 := y0; p.x1 := x1; p.y1 := y1;
		Models.Do(m, "Insert Line", op)
	END Insert;


	PROCEDURE (op: ColorOp) Do;
		VAR color: Ports.Color;
	BEGIN
		color := op.view.color;	(* save old state *)
		op.view.color := op.color;	(* set new state *)
		Views.Update(op.view, Views.keepFrames);	(* restore everything *)
		op.color := color	(* old state becomes new state for undo *)
	END Do;


	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; st: Stores.Store;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.color);
			rd.ReadStore(st);
			v.model := st(Model)
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteInt(v.color);
		wr.WriteStore(v.model)
	END Externalize;

	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		ASSERT(model IS Model, 20);
		WITH source: View DO
			v.model := model(Model);
			v.color := source.color
		END
	END CopyFromModelView;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.model
	END ThisModel;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR p: Line;
	BEGIN
		p := v.model.lines;
		WHILE p # NIL DO
			f.DrawLine(p.x0, p.y0, p.x1, p.y1, f.dot, v.color);
			p := p.next
		END
	END Restore;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
	BEGIN
		WITH msg: UpdateMsg DO
			Views.UpdateIn(v, msg.l, msg.t, msg.r, msg.b, Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) SetColor (color: Ports.Color), NEW;
		VAR op: ColorOp;
	BEGIN
		NEW(op); op.view := v; op.color := color; Views.Do(v, "Set Color", op)
	END SetColor;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																VAR focus: Views.View);
		VAR x0, y0, x1, y1, x, y, res, l, t, r, b: INTEGER; modifiers: SET; isDown: BOOLEAN;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			x0 := msg.x; y0 := msg.y; x1 := x0; y1 := y0;
			f.SaveRect(f.l, f.t, f.r, f.b, res);	(* operation was successful if res = 0 *)
			IF res = 0 THEN f.DrawLine(x0, y0, x1, y1, Ports.point, v.color) END;
			REPEAT
				f.Input(x, y, modifiers, isDown);
				IF (x # x1) OR (y # y1) THEN
					GetBox(x0, y0, x1, y1, l, t, r, b); f.RestoreRect(l, t, r, b, Ports.keepBuffer);
					x1 := x; y1 := y;
					IF res = 0 THEN f.DrawLine(x0, y0, x1, y1, Ports.point, v.color) END
				END
			UNTIL ~isDown;
			GetBox(x0, y0, x1, y1, l, t, r, b); f.RestoreRect(l, t, r, b, Ports.disposeBuffer);
			v.model.Insert(x0, y0, x1, y1)
		| msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN
				CASE msg.char OF
				| "B": v.SetColor(Ports.black)
				| "r": v.SetColor(Ports.red)
				| "g": v.SetColor(Ports.green)
				| "b": v.SetColor(Ports.blue)
				ELSE
				END
			END
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;


	PROCEDURE Deposit*;
		VAR m: Model; v: View;
	BEGIN
		NEW(m);
		NEW(v); v.model := m; Stores.Join(v, m);
		Views.Deposit(v)
	END Deposit;

END ObxLines.