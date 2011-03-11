MODULE ObxCaps;
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

	IMPORT Stores, Models, TextModels, TextControllers;

	PROCEDURE Do*;
		VAR c: TextControllers.Controller; beg, end: INTEGER;
			r: TextModels.Reader; ch: CHAR;
			buf: TextModels.Model; w: TextModels.Writer; script: Stores.Operation;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			(* upper case text will be copied into this buffer *)
			buf := TextModels.CloneOf(c.text); w := buf.NewWriter(NIL);
			r := c.text.NewReader(NIL); r.SetPos(beg);
			r.ReadChar(ch);
			WHILE (r.Pos() <= end) & ~r.eot DO
				IF (ch >= "a") & (ch <= "z") THEN ch := CAP(ch) END;
				w.WriteChar(ch);
				r.ReadChar(ch)
			END;
			Models.BeginScript(c.text, "Caps", script);
			c.text.Delete(beg, end); c.text.Insert(beg, buf, 0, end - beg);
			Models.EndScript(c.text, script)
		END
	END Do;

END ObxCaps.
