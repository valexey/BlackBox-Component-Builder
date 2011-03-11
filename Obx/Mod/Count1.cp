MODULE ObxCount1;
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

	IMPORT TextMappers, TextControllers, StdLog;

	PROCEDURE Do*;
	(** use TextCmds.SelectionGuard as guard for this command **)
		VAR c: TextControllers.Controller; from, to, ints, reals, strings: INTEGER;
			s: TextMappers.Scanner;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(from, to);	(* get selection range; from < to *)
			s.ConnectTo(c.text);	(* connect scanner to this text model *)
			s.SetPos(from);	(* set the reader to beginning of selection *)
			s.Scan;				(* read the first symbol of the text selection *)
			ints := 0; reals := 0; strings := 0;	(* counter variables *)
			WHILE s.start < to DO	(* read all symbols starting in the text selection *)
				IF s.type = TextMappers.int THEN	(* symbol is an integer number *)
					INC(ints)
				ELSIF s.type = TextMappers.real THEN	(* symbol is a real number *)
					INC(reals)
				ELSIF s.type = TextMappers.string THEN	(* symbol is a string/identifier *)
					INC(strings)
				END;
				s.Scan		(* read next symbol of the text selection *)
			END;
			StdLog.String("Integers: "); StdLog.Int(ints); StdLog.Ln;
			StdLog.String("Reals: "); StdLog.Int(reals); StdLog.Ln;
			StdLog.String("Strings: "); StdLog.Int(strings); StdLog.Ln;
			StdLog.Ln
		END
	END Do;

END ObxCount1.
