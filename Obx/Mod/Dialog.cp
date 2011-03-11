MODULE ObxDialog;
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

	IMPORT Dialog;

	VAR
		dialog*: RECORD
			list*: Dialog.List;
			combo*: Dialog.Combo;
			sum-: INTEGER;
			disable*: BOOLEAN;
			elems*: INTEGER
		END;
		sel*: Dialog.Selection; (* hadled separately to avoid race condition in notifier *)

	PROCEDURE SetList (IN l: Dialog.List; n: INTEGER);
		VAR i: INTEGER;
	BEGIN
		l.SetLen(n);
		i := 0;
		WHILE i # n DO
			CASE i OF
			| 0: l.SetItem(i, "zero")
			| 1: l.SetItem(i, "one")
			| 2: l.SetItem(i, "two")
			| 3: l.SetItem(i, "three")
			| 4: l.SetItem(i, "four")
			| 5: l.SetItem(i, "five")
			| 6: l.SetItem(i, "six")
			| 7: l.SetItem(i, "seven")
			| 8: l.SetItem(i, "eight")
			| 9: l.SetItem(i, "nine")
			| 10: l.SetItem(i, "ten")
			| 11: l.SetItem(i, "eleven")
			END;
			INC(i)
		END
	END SetList;

	PROCEDURE SelNotifier* (op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.set THEN	(* recalculate sum of selected numbers *)
			dialog.sum := (to - from + 1) * (to + from) DIV 2
		ELSIF op = Dialog.excluded THEN	(* correct sum after deselection of some numbers *)
			WHILE from <= to DO DEC(dialog.sum, from); INC(from) END
		ELSIF op = Dialog.included THEN	(* correct sum after selection of some numbers *)
			WHILE from <= to DO INC(dialog.sum, from); INC(from) END
		END;
		Dialog.Update(dialog)	(* show new dialog.sum *)
	END SelNotifier;

	PROCEDURE ElemsNotifier* (op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.changed THEN
			IF dialog.elems < 0 THEN
				dialog.elems := 0; Dialog.Update(dialog)
			ELSIF dialog.elems > 12 THEN
				dialog.elems := 12; Dialog.Update(dialog)
			END;
			SetList(dialog.list, dialog.elems);	(* rebuild list *)
			Dialog.UpdateList(dialog.list)	(* update list  *)
		END
	END ElemsNotifier;

	PROCEDURE ListGuard* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := dialog.disable
	END ListGuard;

BEGIN
	dialog.elems := 2; SetList(dialog.list, dialog.elems);
	sel.SetLen(12);
	sel.SetItem(0, "zero"); sel.SetItem(1, "one"); sel.SetItem(2, "two");
	sel.SetItem(3, "three"); sel.SetItem(4, "four"); sel.SetItem(5, "five");
	sel.SetItem(6, "six"); sel.SetItem(7, "seven"); sel.SetItem(8, "eight");
	sel.SetItem(9, "nine"); sel.SetItem(10, "ten"); sel.SetItem(11, "eleven");
	dialog.combo.SetLen(12);
	dialog.combo.SetItem(0, "zero"); dialog.combo.SetItem(1, "one"); dialog.combo.SetItem(2, "two");
	dialog.combo.SetItem(3, "three"); dialog.combo.SetItem(4, "four"); dialog.combo.SetItem(5, "five");
	dialog.combo.SetItem(6, "six"); dialog.combo.SetItem(7, "seven"); dialog.combo.SetItem(8, "eight");
	dialog.combo.SetItem(9, "nine"); dialog.combo.SetItem(10, "ten"); dialog.combo.SetItem(11, "eleven")
END ObxDialog.
