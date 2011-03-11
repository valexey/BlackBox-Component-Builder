MODULE ObxCount0;
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

	IMPORT TextModels, TextControllers, StdLog;

	PROCEDURE Do*;
	(** use TextCmds.SelectionGuard as guard for this command **)
		VAR c: TextControllers.Controller; from, to, schars, chars, views: INTEGER;
			rd: TextModels.Reader;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(from, to);	(* get selection range; from < to *)
			rd := c.text.NewReader(NIL);	(* create a new reader for this text model *)
			rd.SetPos(from);	(* set the reader to beginning of selection *)
			rd.Read;				(* read the first element of the text selection *)
			schars := 0; chars := 0; views := 0;	(* counter variables *)
			WHILE rd.Pos() # to DO	(* read all elements of the text selection *)
				IF rd.view # NIL THEN	(* element is a view *)
					INC(views)
				ELSIF rd.char < 100X THEN	(* element is a Latin-1 character *)
					INC(schars)
				ELSE	(* element is Unicode character *)
					INC(chars)
				END;
				rd.Read		(* read next element of the text selection *)
			END;
			StdLog.String("Latin-1 characters: "); StdLog.Int(schars); StdLog.Ln;
			StdLog.String("Unicode characters: "); StdLog.Int(chars); StdLog.Ln;
			StdLog.String("Views: "); StdLog.Int(views); StdLog.Ln;
			StdLog.Ln
		END
	END Do;

END ObxCount0.
