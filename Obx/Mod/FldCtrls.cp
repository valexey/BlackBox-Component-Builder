MODULE ObxFldCtrls;
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

	IMPORT Strings, Meta, Dialog, Stores, Views, Controllers, Properties, Controls, StdCFrames;

	CONST
		minVersion = 0; maxVersion = 0;
		rdel = 07X; ldel = 08X;  tab = 09X; ltab = 0AX; lineChar = 0DX; esc = 01BX;
		arrowLeft = 1CX; arrowRight = 1DX; arrowUp = 1EX; arrowDown = 1FX;
		maxLen = 13;

	TYPE
		Vector* = RECORD x*, y*: INTEGER END;

		VectorField = POINTER TO RECORD (Controls.Control)
			selection: INTEGER	(* 0: no selection; 1: first part selected; 2: selection or caret in second part *)
		END;

	VAR test*: Vector;

	PROCEDURE GetVectorField (f: StdCFrames.Field; OUT str: ARRAY OF CHAR);
		VAR c: VectorField; v: Meta.Item; x, y: INTEGER; s1, s2: ARRAY 16 OF CHAR;
	BEGIN
		c := f.view(VectorField);
		IF c.item.Valid() THEN
			c.item.Lookup("x", v); x := v.IntVal();
			c.item.Lookup("y", v); y := v.IntVal()
		ELSE x := 0; y := 0
		END;
		s1[0] := CHR(x DIV 10 MOD 10 + ORD("0"));
		s1[1] := CHR(x MOD 10 + ORD("0"));
		s1[2] := 0X;
		IF y < 0 THEN y := 0 END;
		Strings.IntToString(y, s2);
		str := s1 + "/" + s2
	END GetVectorField;

	PROCEDURE SetVectorField(f: StdCFrames.Field; IN str: ARRAY OF CHAR);
		VAR c: VectorField; v: Meta.Item; x, y, res: INTEGER; s: ARRAY 16 OF CHAR;
	BEGIN
		c := f.view(VectorField);
		x := (ORD(str[0]) - ORD("0")) * 10 + (ORD(str[1]) - ORD("0"));
		s := str$; s[0] := " "; s[1] := " "; s[2] := " ";
		Strings.StringToInt(s, y, res);
		IF (res = 0) & c.item.Valid() & ~c.readOnly THEN
			c.item.Lookup("x", v); v.PutIntVal(x);
			c.item.Lookup("y", v); v.PutIntVal(y);
			Controls.Notify(c, f, Dialog.changed, 0, 0)
		END
	END SetVectorField;

	PROCEDURE EqualVectorField (f: StdCFrames.Field; IN s1, s2: ARRAY OF CHAR): BOOLEAN;
	BEGIN
		RETURN s1 = s2
	END EqualVectorField;

	PROCEDURE (c: VectorField) CopyFromSimpleView2 (source: Controls.Control);
	BEGIN
		c.selection := 0
	END CopyFromSimpleView2;

	PROCEDURE (c: VectorField) Internalize2 (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		c.selection := 0
	END Internalize2;

	PROCEDURE (c: VectorField) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion)
	END Externalize2;

	PROCEDURE (c: VectorField) GetNewFrame (VAR frame: Views.Frame);
		VAR f: StdCFrames.Field;
	BEGIN
		f := StdCFrames.dir.NewField();
		f.disabled := c.disabled;
		f.undef := c.undef;
		f.readOnly := c.readOnly;
		f.font := c.font;
		f.maxLen := maxLen;
		f.left := c.prop.opt[Controls.left];
		f.right := c.prop.opt[Controls.right];
		f.Get := GetVectorField;
		f.Set := SetVectorField;
		f.Equal := EqualVectorField;
		frame := f
	END GetNewFrame;

	PROCEDURE (c: VectorField) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		WITH f: StdCFrames.Frame DO f.Restore(l, t, r, b) END
	END Restore;

	PROCEDURE (c: VectorField) HandleCtrlMsg2 (f: Views.Frame; VAR msg: Controllers.Message;
																VAR focus: Views.View);
		VAR ch: CHAR; a, b, x, x0: INTEGER; v: Meta.Item;
	BEGIN
		IF ~c.disabled & ~c.readOnly THEN
			WITH f: StdCFrames.Field DO
				WITH msg: Controllers.PollOpsMsg DO
					msg.valid := {Controllers.copy}
				| msg: Controllers.EditMsg DO
					IF msg.op = Controllers.pasteChar THEN
						ch := msg.char;
						IF c.selection = 2 THEN
							IF ch = arrowLeft THEN
								c.selection := 1; f.Select(0, 2)
							ELSIF (ch = ldel) OR (ch = rdel) OR (ch = arrowRight) OR ("0" <= ch) & (ch <= "9") THEN
								f.KeyDown(ch); f.Update
							ELSE Dialog.Beep
							END
						ELSIF c.item.Valid() THEN
							c.item.Lookup("x", v); x := v.IntVal(); x0 := x;
							IF ch = arrowRight THEN
								c.selection := 2; f.Select(3, MAX(INTEGER))
							ELSIF ch = arrowUp THEN
								x := (x + 1) MOD 100
							ELSIF ch = arrowDown THEN
								x := (x - 1) MOD 100
							ELSIF ("0" <= ch) & (ch <= "9") THEN
								x := x * 10 MOD 100 + ORD(ch) - ORD("0")
							ELSE Dialog.Beep
							END;
							IF x # x0 THEN
								v.PutIntVal(x); Controls.Notify(c, f, Dialog.changed, 0, 0);
								f.Update; f.Select(0, 2)
							END
						END
					ELSE
						f.Edit(msg.op, msg.view, msg.w, msg.h, msg.isSingle, msg.clipboard)
					END
				| msg: Controllers.PollCursorMsg DO
					f.GetCursor(msg.x, msg.y, msg.modifiers, msg.cursor)
				| msg: Controllers.TrackMsg DO
					f.MouseDown(msg.x, msg.y, msg.modifiers);
					f.GetSelection(a, b);
					IF a >= 3 THEN c.selection := 2
					ELSIF a >= 0 THEN c.selection := 1; f.Select(0, 2)
					END
				| msg: Controllers.MarkMsg DO
					f.Mark(msg.show, msg.focus);
					IF msg.focus THEN
						IF msg.show THEN	(* set selection on focus *)
							c.selection := 1; f.Select(0, 2)
						ELSE	(* remove selection on defocus *)
							c.selection := 0; f.Select(-1, -1)
						END
					END
				ELSE
				END
			END
		END
	END HandleCtrlMsg2;

	PROCEDURE (c: VectorField) HandlePropMsg2 (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF ~c.disabled & ~c.readOnly THEN msg.setFocus := TRUE END
		| msg: Properties.SizePref DO
			StdCFrames.dir.GetFieldSize(maxLen, msg.w, msg.h)
		| msg: Controls.PropPref DO
			msg.valid := msg.valid + {Controls.left, Controls.right}
			(* to enable the property editor to correctly label these options, insert the following three lines
				into Dev/Rsrc/Strings (no leading tab characters, however!):
				ObxFldCtrls.VectorField	Vector Field
				ObxFldCtrls.VectorField.Opt0	Left aligned
				ObxFldCtrls.VectorField.Opt1	Right aligned
			*)
			(* to handle font and color properties: handle type Properties.StdProp also *)
		ELSE
		END
	END HandlePropMsg2;

	PROCEDURE (c: VectorField) CheckLink (VAR ok: BOOLEAN);
		VAR mod, name: Meta.Name;
	BEGIN
		IF (c.item.typ = Meta.recTyp) THEN
			c.item.GetTypeName(mod, name);
			ok := (mod = "ObxFldCtrls") & (name = "Vector")
		ELSE ok := FALSE
		END
	END CheckLink;

	PROCEDURE (c: VectorField) Update (f: Views.Frame; op, from, to: INTEGER);
	BEGIN
		f(StdCFrames.Frame).Update;
		IF f.front & ~c.disabled THEN
			IF c.selection = 1 THEN f(StdCFrames.Field).Select(0, 1)
			ELSIF c.selection = 2 THEN f(StdCFrames.Field).Select(2, MAX(INTEGER))
			END
		END
	END Update;


	PROCEDURE New* (p: Controls.Prop): Views.View;
		VAR c: VectorField;
	BEGIN
		NEW(c); Controls.OpenLink(c, p); RETURN c
	END New;

	PROCEDURE Deposit*;
		VAR p: Controls.Prop;
	BEGIN
		NEW(p);
		p.link := "ObxFldCtrls.test";
		p.label := ""; p.guard := ""; p.notifier := "";
		p.opt[Controls.left] := TRUE;
		Views.Deposit(New(p))
	END Deposit;

END ObxFldCtrls.

"ObxFldCtrls.Deposit; StdCmds.PasteView"

