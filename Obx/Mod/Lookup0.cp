MODULE ObxLookup0;
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

	IMPORT TextModels, TextMappers, TextControllers, ObxPhoneDB;

	PROCEDURE Do*;
	(** use TextCmds.SelectionGuard as guard command **)
		VAR c: TextControllers.Controller; buf: TextModels.Model; from, to: INTEGER;
			s: TextMappers.Scanner; f: TextMappers.Formatter; number: ObxPhoneDB.String;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(from, to);
			s.ConnectTo(c.text);
			s.SetPos(from);
			s.Scan;
			IF s.type = TextMappers.string THEN
				buf := TextModels.CloneOf(c.text);
				f.ConnectTo(buf);
				ObxPhoneDB.LookupByName(s.string$, number);
				f.WriteString(number);
				from := s.start; to := s.Pos() - 1;	(* scanner has already read on character beyond string! *)
				c.text.Delete(from, to);							(* delete name *)
				c.text.Insert(from, buf, 0, buf.Length());	(* move phone number from buffer into text *)
				c.SetSelection(from, from + LEN(number$))	(* select the phone number *)
			END
		END
	END Do;

END ObxLookup0.
