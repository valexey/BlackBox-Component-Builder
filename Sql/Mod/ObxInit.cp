MODULE SqlObxInit;
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

	IMPORT Kernel, SqlDB;
	
	CONST
		protocol = "SqlOdbc";
		id = ""; password = "";
		datasource = "Test Database";

	PROCEDURE Setup*;
		VAR db: SqlDB.Database; res: INTEGER;
	BEGIN
		SqlDB.OpenDatabase(protocol, id, password, datasource, SqlDB.async, SqlDB.hideErrors, db, res);
		IF db # NIL THEN
			db.Exec("CREATE TABLE Companies (id INTEGER, name VARCHAR(255), ceo VARCHAR(255), employees INTEGER)");
			db.Exec("CREATE TABLE Ownership (owner INTEGER, owned INTEGER, percent INTEGER)");
			(* single company *)
			db.Exec("INSERT INTO Companies VALUES (11, 'Test', 'Bill', 234)");
			(* two companies (tree) *)
			db.Exec("INSERT INTO Companies VALUES (12, 'Test', 'Bill', 234)");
			db.Exec("INSERT INTO Companies VALUES (13, 'Test company AG', 'John', 45)");
			db.Exec("INSERT INTO Ownership VALUES (12, 13, 100)");
			(* four companies (with ring) *)
			db.Exec("INSERT INTO Companies VALUES (14, 'Test', 'Bill', 234)");
			db.Exec("INSERT INTO Companies VALUES (15, 'Test company AG', 'John', 45)");
			db.Exec("INSERT INTO Companies VALUES (16, 'Test Services GmbH', 'Jim', 23000)");
			db.Exec("INSERT INTO Companies VALUES (17, 'Test Commands & Co.', 'Mary', 523)");
			db.Exec("INSERT INTO Ownership VALUES (14, 15, 50)");
			db.Exec("INSERT INTO Ownership VALUES (15, 17, 100)");
			db.Exec("INSERT INTO Ownership VALUES (16, 15, 50)");
			db.Exec("INSERT INTO Ownership VALUES (15, 17, 100)");
			(* four companies (tree) *)
			db.Exec("INSERT INTO Companies VALUES (18, 'Test', 'Bill', 234)");
			db.Exec("INSERT INTO Companies VALUES (19, 'Test company AG', 'John', 45)");
			db.Exec("INSERT INTO Companies VALUES (20, 'Test Services GmbH', 'Jim', 23000)");
			db.Exec("INSERT INTO Companies VALUES (21, 'Test Commands & Co.', 'Mary', 523)");
			db.Exec("INSERT INTO Ownership VALUES (18, 19, 20)");
			db.Exec("INSERT INTO Ownership VALUES (18, 20, 30)");
			db.Exec("INSERT INTO Ownership VALUES (18, 21, 50)");
			(* most complex example *)
			db.Exec("INSERT INTO Companies VALUES (1, 'Test', 'Bill', 234)");
			db.Exec("INSERT INTO Companies VALUES (2, 'Test company AG', 'John', 45)");
			db.Exec("INSERT INTO Companies VALUES (3, 'Test Services GmbH', 'Jim', 23000)");
			db.Exec("INSERT INTO Companies VALUES (4, 'Test Commands & Co.', 'Mary', 523)");
			db.Exec("INSERT INTO Companies VALUES (5, 'Test Views KG', 'Frank', 17)");
			db.Exec("INSERT INTO Companies VALUES (6, 'Test Genossenschaft', 'Hans', 2109)");
			db.Exec("INSERT INTO Companies VALUES (7, 'Test Mentoring, Inc.', 'Marlis', 128)");
			db.Exec("INSERT INTO Companies VALUES (8, 'Test Training Plc.', 'Paul', 4)");
			db.Exec("INSERT INTO Companies VALUES (9, 'Test Trainers SA', 'Jean', 87)");
			db.Exec("INSERT INTO Companies VALUES (10, 'Test Wrappers AB', 'Gordon', 912)");
			db.Exec("INSERT INTO Ownership VALUES (1, 2, 100)");
			db.Exec("INSERT INTO Ownership VALUES (1, 3, 100)");
			db.Exec("INSERT INTO Ownership VALUES (2, 4, 100)");
			db.Exec("INSERT INTO Ownership VALUES (2, 5, 100)");
			db.Exec("INSERT INTO Ownership VALUES (3, 6, 100)");
			db.Exec("INSERT INTO Ownership VALUES (3, 7, 100)");
			db.Exec("INSERT INTO Ownership VALUES (7, 8, 100)");
			db.Exec("INSERT INTO Ownership VALUES (5, 9, 49)");
			db.Exec("INSERT INTO Ownership VALUES (8, 9, 51)");
			db.Exec("INSERT INTO Ownership VALUES (9, 10, 100)");
			db.Commit
		END;
		db := NIL;
		Kernel.Cleanup	(* garbage collector closes database *)
	END Setup;
	
END SqlObxInit.