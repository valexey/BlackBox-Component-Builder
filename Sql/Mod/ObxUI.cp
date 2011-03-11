MODULE SqlObxUI;
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

	IMPORT Dialog, SqlDB, SqlObxDB;
	
	(** generic **)

	PROCEDURE ROGuard* (VAR par: Dialog.Par);
	BEGIN
		par.readOnly := TRUE
	END ROGuard;


	(** guards **)

	PROCEDURE Invalid (): BOOLEAN;
	BEGIN	(* check whether record is legal (must be a very efficient check) *)
		RETURN SqlObxDB.company.name = ""
	END Invalid;

	PROCEDURE CloseGuard* (VAR par: Dialog.Par);
	BEGIN
		IF SqlObxDB.dirty THEN par.disabled := TRUE END
	END CloseGuard;

	PROCEDURE InsertGuard* (VAR par: Dialog.Par);
	BEGIN
		IF SqlObxDB.Closed() THEN par.disabled := TRUE END;
		IF ~SqlObxDB.dirty OR Invalid() THEN par.disabled := TRUE END
	END InsertGuard;

	PROCEDURE UpdateGuard* (VAR par: Dialog.Par);
	BEGIN
		IF SqlObxDB.Closed() THEN par.disabled := TRUE END;
		IF ~SqlObxDB.dirty OR (SqlObxDB.company.id <= 0) OR Invalid() THEN par.disabled := TRUE END
	END UpdateGuard;

	PROCEDURE DeleteGuard* (VAR par: Dialog.Par);
	BEGIN
		IF SqlObxDB.Closed() THEN par.disabled := TRUE END;
		IF SqlObxDB.dirty OR (SqlObxDB.company.id <= 0) THEN par.disabled := TRUE END
	END DeleteGuard;

	PROCEDURE RevertGuard* (VAR par: Dialog.Par);
	BEGIN
		IF SqlObxDB.Closed() THEN par.disabled := TRUE END;
		IF ~SqlObxDB.dirty THEN par.disabled := TRUE END;
		IF SqlObxDB.company.id > 0 THEN par.label := "Revert" ELSE par.label := "Clear" END
	END RevertGuard;

	PROCEDURE FindGuard* (VAR par: Dialog.Par);
	BEGIN
		IF SqlObxDB.Closed() THEN par.disabled := TRUE END;
		IF SqlObxDB.dirty OR (SqlObxDB.searchId <= 0) THEN par.disabled := TRUE END
	END FindGuard;


	(** notifiers **)

	PROCEDURE TypingNotifier* (op, from, to: INTEGER);
	BEGIN
		IF ~SqlObxDB.dirty & (op = Dialog.changed) & ~SqlObxDB.Closed() THEN
			SqlObxDB.dirty := TRUE; Dialog.Update(SqlObxDB.company)
		END
	END TypingNotifier;

END SqlObxUI.
