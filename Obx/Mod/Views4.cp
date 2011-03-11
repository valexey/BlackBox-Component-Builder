MODULE ObxViews4;
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

	IMPORT Views, Ports, Properties, Controllers;

	TYPE View = POINTER TO RECORD (Views.View) END;

	PROCEDURE (v: View)  Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawRect(l, t, r, b, Ports.fill, Ports.red)
	END Restore;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		CONST min = 5 * Ports.mm; max = 50 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				 msg.w := 20 * Ports.mm; msg.h := 10 * Ports.mm
			ELSE
				Properties.ProportionalConstraint(2, 1, msg.fixedW, msg.fixedH,
										msg.w, msg.h);
				IF msg.h < min THEN
					msg.h := min; msg.w := 2 * min
				ELSIF msg.h > max THEN
					msg.h := max; msg.w := 2 * max
				END
			END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE; msg.verFitToWin := TRUE
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE	(* ignore other messages *)
		END
	END HandlePropMsg;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame;
									VAR msg: Controllers.Message; VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.PollOpsMsg DO
			msg.valid := {Controllers.paste}; msg.selectable := TRUE;
			msg.type := "Obx.Tutorial"
		ELSE	(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); Views.Deposit(v)
	END Deposit;

END ObxViews4.
