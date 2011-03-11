MODULE ObxViews1;
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

	IMPORT Views, Ports, Properties;

	TYPE View = POINTER TO RECORD (Views.View) END;

	PROCEDURE (v: View)  Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawRect(l, t, r, b, Ports.fill, Ports.red)
	END Restore;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				 msg.w := 20 * Ports.mm; msg.h := 10 * Ports.mm
			END
		ELSE	(* ignore other messages *)
		END
	END HandlePropMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); Views.Deposit(v)
	END Deposit;

END ObxViews1.
