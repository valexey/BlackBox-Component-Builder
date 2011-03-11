MODULE ObxTabs;
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

	IMPORT Views, TextModels, TextViews, TextControllers;

	CONST tab = 09X; line = 0DX;

	VAR field: ARRAY 256 OF CHAR;

	PROCEDURE ReadField (r: TextModels.Reader);
		VAR i: INTEGER; ch: CHAR;
	BEGIN	(* read a field, which is a sequence of characters terminated by the end of text, or a tab or line character *)
		i := 0; r.ReadChar(ch);
		WHILE ~r.eot & (ch # tab) & (ch # line) DO
			field[i] := ch; INC(i); r.ReadChar(ch)
		END;
		field[i] := 0X
	END ReadField;

	PROCEDURE WriteField (w: TextModels.Writer);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := field[0];
		WHILE ch # 0X DO
			w.WriteChar(ch); INC(i); ch := field[i]
		END
	END WriteField;

	PROCEDURE Convert*;
		VAR c: TextControllers.Controller; t: TextModels.Model; r: TextModels.Reader;
			w: TextModels.Writer; beg, end: INTEGER;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			r := c.text.NewReader(NIL); r.SetPos(beg);
			t := TextModels.CloneOf(c.text);
			w := t.NewWriter(NIL);
			ReadField(r);	(* title *)
			WHILE ~r.eot DO
				WriteField(w); w.WriteChar(" ");
				ReadField(r); WriteField(w); w.WriteChar(" ");	(* first name *)
				ReadField(r); WriteField(w); w.WriteChar(tab);	(* name *)
				ReadField(r); WriteField(w); w.WriteChar(tab);	(* company 1 *)
				ReadField(r); WriteField(w); w.WriteChar(tab);	(* company 2 *)
				ReadField(r); WriteField(w); w.WriteChar(tab);	(* address *)
				ReadField(r); WriteField(w); w.WriteChar(" ");	(* ZIP *)
				ReadField(r); WriteField(w); w.WriteChar(tab);	(* city *)
				ReadField(r); WriteField(w); w.WriteChar(line);	(* country *)
				ReadField(r)	(* title *)
			END;
			Views.OpenView(TextViews.dir.New(t))
		END
	END Convert;

END ObxTabs.
