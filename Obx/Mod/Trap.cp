MODULE ObxTrap;
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

	IMPORT Dialog, Views, Controllers;

	VAR global: INTEGER;

	PROCEDURE Do*;
		VAR i: INTEGER; str: Dialog.String; v: Views.View;
	BEGIN
		str := "String";
		global := 13;
		v := Controllers.FocusView();
		i := 777;
		str[i] := "*"	(* index out of range, since the Dialog.String array only contains 256 elements *)
	END Do;
	
	PROCEDURE Hang*;
	BEGIN
		LOOP END
	END Hang;

END ObxTrap.