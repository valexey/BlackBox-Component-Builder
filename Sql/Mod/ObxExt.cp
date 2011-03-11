MODULE SqlObxExt;
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

	IMPORT SqlDB;

	CONST
		protocol = "SqlOdbc";
		id = ""; password = "";
		datasource = "Test Database";

	VAR
		company0*, company1*: RECORD
			id*: INTEGER;
			name*: ARRAY 32 OF CHAR
		END;

		ownership*: RECORD
			owner*, owned*: INTEGER;
			percent*: INTEGER
		END;

	VAR table*: SqlDB.Table;

	PROCEDURE Open*;
		VAR d: SqlDB.Database; res: INTEGER;
	BEGIN
		SqlDB.OpenDatabase(protocol, id, password, datasource, SqlDB.async, SqlDB.hideErrors, d, res);
		IF d # NIL THEN
			table := d.NewTable()
		END
	END Open;


	PROCEDURE FindOwner*;
	BEGIN
		table.Exec("SELECT * FROM Companies WHERE id = :SqlObxExt.company0.id");
		table.Read(0, company0);
		table.Exec("SELECT * FROM Ownership WHERE owner = :SqlObxExt.company0.id AND owned = :SqlObxExt.company1.id");
		table.Read(0, ownership)
	END FindOwner;

	PROCEDURE FindOwned*;
	BEGIN
		table.Exec("SELECT * FROM Companies WHERE id = :SqlObxExt.company1.id");
		table.Read(0, company1);
		table.Exec("SELECT * FROM Ownership WHERE owner = :SqlObxExt.company0.id AND owned = :SqlObxExt.company1.id");
		table.Read(0, ownership)
	END FindOwned;

	PROCEDURE UpdatePercent*;
	BEGIN
		ownership.owner := company0.id; ownership.owned := company1.id;
		table.base.Exec("DELETE FROM Ownership WHERE owner = :SqlObxExt.ownership.owner AND owned = :SqlObxExt.ownership.owned");
		table.base.Exec("INSERT INTO Ownership VALUES (:SqlObxExt.ownership)");
		table.base.Commit;
		(* now the contents of table is inconsistent with database *)
		table.Exec("SELECT * FROM Ownership WHERE owner = :SqlObxExt.ownership.owner AND owned = :SqlObxExt.ownership.owned");
		table.Read(0, ownership)
	END UpdatePercent;

END SqlObxExt.
