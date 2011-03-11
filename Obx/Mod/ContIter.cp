MODULE ObxContIter;
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

	IMPORT Views, Containers, Controls;

	PROCEDURE Do*;
	(** focus the first control whose label is "magic name" **)
		VAR c: Containers.Controller;v: Views.View;
	BEGIN
		c := Containers.Focus();
		IF c # NIL THEN
			c.GetFirstView(Containers.any, v);
			WHILE (v # NIL) & ~((v IS Controls.Control) & (v(Controls.Control).label = "magic name")) DO
				c.GetNextView(Containers.any, v)
			END;
			IF v # NIL THEN
				c.SetFocus(v)
			END
		END
	END Do;

END ObxContIter.
