MODULE SqlObxDB;
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

	IMPORT Dialog, SqlDB;

	CONST
		protocol = "SqlOdbc";
		id = ""; password = "";
		datasource = "Test Database";

	VAR
		company*: RECORD
			id*: INTEGER;
			name*: ARRAY 32 OF CHAR;
			ceo*: ARRAY 32 OF CHAR;
			employees*: INTEGER
		END;
		
		dirty*: BOOLEAN;	(* company is dirty *)

		max*: RECORD
			value*: INTEGER
		END;

		searchId*: INTEGER;

		table*: SqlDB.Table;
		

	PROCEDURE Reset;
	BEGIN
		company.id := 0;
		company.name := "";
		company.ceo := "";
		company.employees := 0;
		dirty := FALSE;
		Dialog.Update(company)
	END Reset;
	

	PROCEDURE Open*;
		VAR d: SqlDB.Database; res: INTEGER;
	BEGIN
		SqlDB.OpenDatabase(protocol, id, password, datasource, SqlDB.async, SqlDB.hideErrors, d, res);
		IF d # NIL THEN
			table := d.NewTable()
		END
	END Open;

	PROCEDURE Closed* (): BOOLEAN;
	BEGIN
		RETURN table = NIL
	END Closed;
	

	PROCEDURE Insert*;
	BEGIN
		table.Exec("SELECT MAX(id) FROM Companies");
		table.Read(0, max);
		company.id := max.value + 1;	(* generate a unique key *)
		table.base.Exec("INSERT INTO Companies VALUES (:SqlObxDB.company)");
		table.base.Commit;
		(* now the contents of c is inconsistent with database *)
		table.Exec("SELECT * FROM Companies WHERE id = :SqlObxDB.company.id");
		table.Read(0, company); dirty := FALSE
	END Insert;

	PROCEDURE Update*;
	BEGIN
		table.base.Exec("DELETE FROM Companies WHERE id = :SqlObxDB.company.id");
		table.base.Exec("INSERT INTO Companies VALUES (:SqlObxDB.company)");
		table.base.Commit;
		(* now the contents of table is inconsistent with database *)
		table.Exec("SELECT * FROM Companies WHERE id = :SqlObxDB.company.id");
		table.Read(0, company); dirty := FALSE
	END Update;

	PROCEDURE Delete*;
	BEGIN
		table.base.Exec("DELETE FROM Companies WHERE id = :SqlObxDB.company.id");
		table.base.Commit;
		(* now the contents of table is inconsistent with database *)
		table.Clear;	(* company has become stale *)
		Reset	(* clear interactor *)
	END Delete;

	PROCEDURE Revert*;
	BEGIN
		IF company.id > 0 THEN
			table.Exec("SELECT * FROM Companies WHERE id = :SqlObxDB.company.id");
			table.Read(0, company); dirty := FALSE
		ELSE
			table.Clear;
			Reset	(* clear interactor *)
		END
	END Revert;

	PROCEDURE Find*;
	BEGIN
		table.Exec("SELECT * FROM Companies WHERE id = :SqlObxDB.searchId");
		table.Read(0, company); dirty := FALSE
	END Find;


	PROCEDURE SetTestData*;
		VAR d: SqlDB.Database;
	BEGIN
		d := table.base;
		d.Exec("DELETE FROM Companies WHERE id > 0");
		d.Exec("DELETE FROM Ownership WHERE owner > 0");
		(* single company *)
		d.Exec("INSERT INTO Companies VALUES (11, 'Test', 'Bill', 234)");
		(* two companies (tree) *)
		d.Exec("INSERT INTO Companies VALUES (12, 'Test', 'Bill', 234)");
		d.Exec("INSERT INTO Companies VALUES (13, 'Test company AG', 'John', 45)");
		d.Exec("INSERT INTO Ownership VALUES (12, 13, 100)");
		(* four companies (with ring) *)
		d.Exec("INSERT INTO Companies VALUES (14, 'Test', 'Bill', 234)");
		d.Exec("INSERT INTO Companies VALUES (15, 'Test company AG', 'John', 45)");
		d.Exec("INSERT INTO Companies VALUES (16, 'Test Services GmbH', 'Jim', 23000)");
		d.Exec("INSERT INTO Companies VALUES (17, 'Test Commands & Co.', 'Mary', 523)");
		d.Exec("INSERT INTO Ownership VALUES (14, 15, 50)");
		d.Exec("INSERT INTO Ownership VALUES (15, 17, 100)");
		d.Exec("INSERT INTO Ownership VALUES (16, 15, 50)");
		d.Exec("INSERT INTO Ownership VALUES (15, 17, 100)");
		(* four companies (tree) *)
		d.Exec("INSERT INTO Companies VALUES (18, 'Test', 'Bill', 234)");
		d.Exec("INSERT INTO Companies VALUES (19, 'Test company AG', 'John', 45)");
		d.Exec("INSERT INTO Companies VALUES (20, 'Test Services GmbH', 'Jim', 23000)");
		d.Exec("INSERT INTO Companies VALUES (21, 'Test Commands & Co.', 'Mary', 523)");
		d.Exec("INSERT INTO Ownership VALUES (18, 19, 20)");
		d.Exec("INSERT INTO Ownership VALUES (18, 20, 30)");
		d.Exec("INSERT INTO Ownership VALUES (18, 21, 50)");
		(* most complex example *)
		d.Exec("INSERT INTO Companies VALUES (1, 'Test', 'Bill', 234)");
		d.Exec("INSERT INTO Companies VALUES (2, 'Test company AG', 'John', 45)");
		d.Exec("INSERT INTO Companies VALUES (3, 'Test Services GmbH', 'Jim', 23000)");
		d.Exec("INSERT INTO Companies VALUES (4, 'Test Commands & Co.', 'Mary', 523)");
		d.Exec("INSERT INTO Companies VALUES (5, 'Test Views KG', 'Frank', 17)");
		d.Exec("INSERT INTO Companies VALUES (6, 'Test Genossenschaft', 'Hans', 2109)");
		d.Exec("INSERT INTO Companies VALUES (7, 'Test Mentoring, Inc.', 'Marlis', 128)");
		d.Exec("INSERT INTO Companies VALUES (8, 'Test Training Plc.', 'Paul', 4)");
		d.Exec("INSERT INTO Companies VALUES (9, 'Test Trainers SA', 'Jean', 87)");
		d.Exec("INSERT INTO Companies VALUES (10, 'Test Wrappers AB', 'Gordon', 912)");
		d.Exec("INSERT INTO Ownership VALUES (1, 2, 100)");
		d.Exec("INSERT INTO Ownership VALUES (1, 3, 100)");
		d.Exec("INSERT INTO Ownership VALUES (2, 4, 100)");
		d.Exec("INSERT INTO Ownership VALUES (2, 5, 100)");
		d.Exec("INSERT INTO Ownership VALUES (3, 6, 100)");
		d.Exec("INSERT INTO Ownership VALUES (3, 7, 100)");
		d.Exec("INSERT INTO Ownership VALUES (7, 8, 100)");
		d.Exec("INSERT INTO Ownership VALUES (5, 9, 49)");
		d.Exec("INSERT INTO Ownership VALUES (8, 9, 51)");
		d.Exec("INSERT INTO Ownership VALUES (9, 10, 100)");
		d.Commit
	END SetTestData;

BEGIN
	searchId := 1
END SqlObxDB.
