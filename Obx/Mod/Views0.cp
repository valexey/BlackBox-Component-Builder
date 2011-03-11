MODULE ObxViews0;
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

	IMPORT Views, Ports;

	TYPE View = POINTER TO RECORD (Views.View) END;

	PROCEDURE (v: View)  Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawRect(l, t, r, b, Ports.fill, Ports.red)
	END Restore;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); Views.Deposit(v)
	END Deposit;

END ObxViews0.
