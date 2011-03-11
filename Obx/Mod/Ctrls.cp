MODULE ObxCtrls;
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

	IMPORT Meta, Dialog, Ports, Stores, Views, Controllers, Properties, Controls;

	CONST
		minVersion = 0; maxVersion = 0;
		arrowUp = 1EX; arrowDown = 1FX;

	TYPE Control = POINTER TO RECORD (Controls.Control) END;

	VAR x*: INTEGER;

	PROCEDURE Paint (c: Control; f: Views.Frame);
		VAR w, h, y, val: INTEGER;
	BEGIN
		c.context.GetSize(w, h);
		IF ~c.disabled & c.item.Valid() THEN
			val := c.item.IntVal();
			IF val < 0 THEN val := 0 ELSIF val > 100 THEN val := 100 END;
			y := f.dot + (h - 2 * f.dot) * (100 - val) DIV 100;
			IF val > 0 THEN f.DrawRect(f.dot, y, w - f.dot, h - f.dot, Ports.fill, Ports.blue) END;
			IF val < 100 THEN f.DrawRect(f.dot, f.dot, w - f.dot, y, Ports.fill, Ports.background) END
		ELSE
			f.DrawRect(f.dot, f.dot, w - f.dot, h - f.dot, Ports.fill, Ports.grey25)
		END;
		f.DrawRect(0, 0, w, h, f.dot, Ports.defaultColor)
	END Paint;

	PROCEDURE ChangeValue (c: Control; f: Views.Frame; new: INTEGER);
		VAR old: INTEGER;
	BEGIN
		old := c.item.IntVal();
		IF new < 0 THEN new := 0 ELSIF new > 100 THEN new := 100 END;
		IF new # old THEN
			c.item.PutIntVal(new);
			Paint(c, f);
			Controls.Notify(c, f, Dialog.changed, old, new)
		END
	END ChangeValue;

	PROCEDURE Track (c: Control; f: Views.Frame; VAR msg: Controllers.TrackMsg);
		VAR x, y, y0, w, h: INTEGER; m: SET; isDown: BOOLEAN;
	BEGIN
		c.context.GetSize(w, h); DEC(h, 2 * f.dot); y0 := -1;
		REPEAT
			f.Input(x, y, m, isDown);
			IF y # y0 THEN
				ChangeValue(c, f, 100 - y * 100 DIV h);
				y0 := y
			END
		UNTIL ~isDown;
	END Track;

	PROCEDURE Edit (c: Control; f: Views.Frame; VAR msg: Controllers.EditMsg);
		VAR val: INTEGER;
	BEGIN
		IF msg.op = Controllers.pasteChar THEN
			val := c.item.IntVal();
			IF msg.char = arrowUp THEN ChangeValue(c, f, val + 1)
			ELSIF msg.char = arrowDown THEN ChangeValue(c, f, val - 1)
			ELSE Dialog.Beep
			END
		END
	END Edit;


	(* Control *)

	PROCEDURE (c: Control) Internalize2 (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version)
	END Internalize2;

	PROCEDURE (c: Control) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion)
	END Externalize2;

	PROCEDURE (c: Control) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Paint(c, f)
	END Restore;

	PROCEDURE (c: Control) HandleCtrlMsg2 (f: Views.Frame; VAR msg: Controllers.Message;
																	VAR focus: Views.View);
	BEGIN
		IF ~c.disabled & ~c.readOnly & c.item.Valid() THEN
			WITH msg: Controllers.PollCursorMsg DO
				msg.cursor := Ports.graphicsCursor
			| msg: Controllers.TrackMsg DO
				Track(c, f, msg)
			| msg: Controllers.EditMsg DO
				Edit(c, f, msg)
			ELSE
			END
		END
	END HandleCtrlMsg2;

	PROCEDURE (c: Control) HandlePropMsg2 (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF ~c.disabled & ~c.readOnly THEN msg.setFocus := TRUE END
		| msg: Properties.SizePref DO
			msg.w := 10 * Ports.mm; msg.h := 50 * Ports.mm
		ELSE
		END
	END HandlePropMsg2;

	PROCEDURE (c: Control) CheckLink (VAR ok: BOOLEAN);
	BEGIN
		IF c.item.typ # Meta.intTyp THEN ok := FALSE END
	END CheckLink;

	PROCEDURE (c: Control) Update (f: Views.Frame; op, from, to: INTEGER);
	BEGIN
		Paint(c, f)
	END Update;


	PROCEDURE New* (p: Controls.Prop): Views.View;
		VAR c: Control;
	BEGIN
		NEW(c); Controls.OpenLink(c, p); RETURN c
	END New;

	PROCEDURE Deposit*;
		VAR p: Controls.Prop;
	BEGIN
		NEW(p); p.link := ""; p.label := ""; p.guard := ""; p.notifier := "";
		Views.Deposit(New(p))
	END Deposit;

	PROCEDURE Guard* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := (x < 0) OR (x > 100)
	END Guard;

END ObxCtrls.

"ObxCtrls.Deposit; StdCmds.PasteView"

