MODULE ObxViews10;
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

	IMPORT Fonts, Ports, Views, Controllers, Properties;

	CONST d = 20 * Ports.point;

	TYPE
		View = POINTER TO RECORD (Views.View)
			i: INTEGER;				(* position of next free slot in string *)
			s: ARRAY 256 OF CHAR	(* string *)
		END;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawString(d, d, Ports.black, v.s, Fonts.dir.Default())
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame;
										VAR msg: Controllers.Message; 
										VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN	(* accept typing *)
				v.s[v.i] := msg.char; INC(v.i); v.s[v.i] := 0X;	(* append character to string *)
				Views.Update(v, Views.keepFrames)	(* restore v in any frame that displays it *)
			END
		ELSE						(* ignore other messages *)
		END
	END HandleCtrlMsg;
	
	PROCEDURE (v:View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN 
				msg.w := 10 * d; msg.h := 2 * d
			END
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); v.s := ""; v.i := 0;
		Views.Deposit(v)
	END Deposit;

END ObxViews10.
