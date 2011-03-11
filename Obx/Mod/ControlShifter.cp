MODULE ObxControlShifter;
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

	IMPORT Ports, FormControllers;

	PROCEDURE Shift*;
		VAR c: FormControllers.Controller; sel: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			sel := c.GetSelection();	(* generates a list with references to the selected views *)
			WHILE sel # NIL DO
				c.form.Move(sel.view, 10 * Ports.mm, 0);	(* move to the right *)
				sel := sel.next
			END
		END
	END Shift;

END ObxControlShifter.
