MODULE ObxViews5;
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

	TYPE
		View = POINTER TO RECORD (Views.View)
			x, y: INTEGER
		END;

	PROCEDURE (v: View)  Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawRect(l, t, r, b, Ports.fill, Ports.red);
		f.DrawLine(v.x, t, v.x, b, 0, Ports.white); f.DrawLine(l, v.y, r, v.y, 0, Ports.white)
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
				IF msg.h < min THEN msg.h := min; msg.w := 2 * min
				ELSIF msg.h > max THEN msg.h := max; msg.w := 2 * max
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
										VAR msg: Controllers.Message; 
										VAR focus: Views.View);
		VAR x, y, w, h: INTEGER; m: SET; isDown: BOOLEAN;
	BEGIN
		WITH msg: Controllers.PollOpsMsg DO
			msg.valid := {Controllers.paste}; msg.selectable := TRUE;
			msg.type := "Obx.Tutorial"
		| msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN	(* cursor keys *)
				IF msg.char = 1DX THEN INC(v.x, Ports.mm)
				ELSIF msg.char = 1CX THEN DEC(v.x, Ports.mm)
				ELSIF msg.char  = 1EX THEN DEC(v.y, Ports.mm)
				ELSIF msg.char  = 1FX THEN INC(v.y, Ports.mm)
				END;
				Views.Update(v, Views.keepFrames)
			END
		| msg: Controllers.TrackMsg DO
			v.x := msg.x; v.y := msg.y;
			v.context.GetSize(w, h); v.Restore(f, 0, 0, w, h);
			REPEAT
				f.Input(x, y, m, isDown);
				IF (x # v.x) OR (y # v.y) THEN
					v.x := x; v.y := y; v.Restore(f, 0, 0, w, h)
				END
			UNTIL ~isDown;
			Views.Update(v, Views.keepFrames)
		| msg: Controllers.PollCursorMsg DO
			msg.cursor := Ports.graphicsCursor
		ELSE	(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); v.x := 0; v.y := 0; Views.Deposit(v)
	END Deposit;

END ObxViews5.
