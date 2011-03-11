MODULE ObxParCmd;
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

	IMPORT Dialog, Models, Controls, TextModels, TextMappers, StdLog;

	PROCEDURE Connect (VAR s: TextMappers.Scanner; OUT done: BOOLEAN);
		VAR c: Models.Context;
	BEGIN
		done := FALSE;
		IF Controls.par # NIL THEN
			c := Controls.par.context;	(* the context of an open view is never NIL *)
			WITH c: TextModels.Context DO
				s.ConnectTo(c.ThisModel()); s.SetPos(c.Pos() + 1); s.Scan;
				done := TRUE
			ELSE
			END
		END
	END Connect;

	PROCEDURE Do0*;
		VAR s: TextMappers.Scanner; done: BOOLEAN;
	BEGIN
		Connect(s, done);
		IF done THEN
			IF s.type = TextMappers.string THEN
				StdLog.String(s.string); StdLog.Ln	(* write string after button to log *)
			END
		END
	END Do0;

	PROCEDURE Do1*;
		VAR s: TextMappers.Scanner; done: BOOLEAN; res: INTEGER;
	BEGIN
		Connect(s, done);
		IF done THEN
			IF s.type = TextMappers.string THEN
				Dialog.Call(s.string, " ", res)	(* execute string after button as a command sequence *)
			END
		END
	END Do1;

END ObxParCmd.
