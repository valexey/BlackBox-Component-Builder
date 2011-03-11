MODULE ObxGraphs;
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

	IMPORT Services, Stores, Ports, Models, Views, Controllers, Properties, TextModels, TextViews, TextMappers;

	CONST minVersion = 0; maxVersion = 0;

	TYPE
		Value = POINTER TO RECORD
			next: Value;
			val: INTEGER
		END;

		Model = POINTER TO RECORD (Models.Model)
			values: Value
		END;

		View = POINTER TO RECORD (Views.View)
			model: Model
		END;

		ModelOp = POINTER TO RECORD (Stores.Operation)
			model: Model;
			values: Value
		END;

	PROCEDURE (op: ModelOp) Do;
		VAR v: Value; msg: Models.UpdateMsg;
	BEGIN
		v := op.model.values; op.model.values := op.values; op.values := v;	(* swap *)
		Models.Broadcast(op.model, msg)
	END Do;


	PROCEDURE (m: Model) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; n: INTEGER; v, last: Value;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			last := NIL;
			rd.ReadInt(n);	(* read number of values *)
			WHILE n # 0 DO
				NEW(v); rd.ReadInt(v.val);
				IF last = NIL THEN m.values := v ELSE last.next := v END;	(* append value *)
				last := v;
				DEC(n)
			END
		END
	END Internalize;

	PROCEDURE (m: Model) Externalize (VAR wr: Stores.Writer);
		VAR v: Value; n: INTEGER;
	BEGIN
		wr.WriteVersion(maxVersion);
		v := m.values; n := 0; WHILE v # NIL DO INC(n); v := v.next END;
		wr.WriteInt(n);	(* write number of values *)
		v := m.values; WHILE v # NIL DO wr.WriteInt(v.val); v := v.next END
	END Externalize;

	PROCEDURE (m: Model) CopyFrom (source: Stores.Store);
	BEGIN
		m.values := source(Model).values	(* values are immutable and thus can be shared *)
	END CopyFrom;

	PROCEDURE (m: Model) SetValues (v: Value), NEW;
		VAR op: ModelOp;
	BEGIN
		NEW(op); op.model := m; op.values := v;
		Models.Do(m, "Set Values", op)
	END SetValues;


	PROCEDURE OpenData (v: View);
		VAR t: TextModels.Model; f: TextMappers.Formatter; h: Value;
	BEGIN
		t := TextModels.dir.New();
		f.ConnectTo(t);
		h := v.model.values;
		WHILE h # NIL DO
			f.WriteInt(h.val); f.WriteLn;
			h := h.next
		END;
		Views.OpenAux(TextViews.dir.New(t), "Values")
	END OpenData;

	PROCEDURE DropData (t: TextModels.Model; v: View);
		VAR s: TextMappers.Scanner; first, last, h: Value;
	BEGIN
		s.ConnectTo(t);
		s.Scan;
		WHILE s.type = TextMappers.int DO
			NEW(h); h.val := s.int;
			IF last = NIL THEN first := h ELSE last.next := h END;
			last := h;
			s.Scan
		END;
		v.model.SetValues(first)
	END DropData;


	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; st: Stores.Store;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadStore(st);
			v.model := st(Model)
		END
	END Internalize;

	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		ASSERT(model IS Model, 20);
		WITH source: View DO
			v.model := model(Model)
		END
	END CopyFromModelView;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteStore(v.model)
	END Externalize;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.model
	END ThisModel;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR h: Value; n: INTEGER; width, height, d, x: INTEGER;
	BEGIN
		h := v.model.values; n := 0; WHILE h # NIL DO INC(n); h := h.next END;	(* count values *)
		IF n > 0 THEN
			v.context.GetSize(width, height);
			d := width DIV n; x := 0;
			h := v.model.values;
			WHILE h # NIL DO
				f.DrawRect(x, height - h.val * Ports.mm, x + d, height, Ports.fill, Ports.grey50);
				h := h.next; INC(x, d)
			END
		END
	END Restore;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
	BEGIN
		WITH msg: Models.UpdateMsg DO
			Views.Update(v, Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Views.CtrlMessage;
																VAR focus: Views.View);
		VAR x, y, w, h: INTEGER; modifiers: SET; isDown: BOOLEAN;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			REPEAT f.Input(x, y, modifiers, isDown) UNTIL ~isDown;
			v.context.GetSize(w, h);
			IF (x >= 0) & (y >= 0) & (x < w) & (y < h) THEN OpenData(v) END
		| msg: Controllers.PollDropMsg DO
			IF Services.Extends(msg.type, "TextViews.View") THEN
				msg.dest := f (* enable drop target feedback *)
			END
		| msg: Controllers.DropMsg DO
			IF msg.view IS TextViews.View THEN
				DropData(msg.view(TextViews.View).ThisModel(), v)	(* interpret dropped text *)
			END
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		CONST min = 10 * Ports.mm; max = 160 * Ports.mm; pref = 90 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO	(* prevent illegal sizes *)
			IF msg.w = Views.undefined THEN msg.w := pref
			ELSIF msg.w < min THEN msg.w := min
			ELSIF msg.w > max THEN msg.w := max
			END;
			IF msg.h = Views.undefined THEN msg.h := pref
			ELSIF msg.h < min THEN msg.h := min
			ELSIF msg.h > max THEN msg.h := max
			END
		| msg: Properties.FocusPref DO
			msg.hotFocus := TRUE
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

END ObxGraphs.