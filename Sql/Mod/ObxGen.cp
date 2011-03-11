MODULE SqlObxGen;
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

	IMPORT Views, SqlDB, SqlObxDB, SqlObxNets, SqlObxViews;

	CONST
		protocol = "SqlOdbc";
		id = ""; password = "";
		datasource = "Test Database";

	TYPE
		Node = POINTER TO RECORD
			next: Node;
			id: INTEGER
		END;

	VAR
		company*: RECORD
			id*: INTEGER;
			name*: ARRAY 32 OF CHAR
		END;

		ownership*: RECORD
			owner*, owned*: INTEGER;
			percent*: INTEGER
		END;

	VAR
		set, queue: Node;
		g: SqlObxNets.Net;

	PROCEDURE Marked (id: INTEGER): BOOLEAN;
		VAR h: Node;
	BEGIN	(* test whether id is in set *)
		h := set; WHILE (h # NIL) & (h.id # id) DO h := h.next END;
		RETURN h # NIL
	END Marked;

	PROCEDURE Mark (id: INTEGER);
		VAR h: Node;
	BEGIN	(* include id in set (not idempotent) *)
		NEW(h); h.id := id; h.next := set; set := h
	END Mark;

	PROCEDURE Enqueue (id: INTEGER);
		VAR h: Node;
	BEGIN	(* insert id at end of queue (idempotent) *)
		IF queue = NIL THEN
			NEW(queue); h := queue
		ELSE
			h := queue; WHILE (h.next # NIL) & (h.id # id) DO h := h.next END;
			IF h.id # id THEN NEW(h.next); h := h.next END
		END;
		h.id := id
	END Enqueue;

	PROCEDURE Dequeue (VAR id: INTEGER);
	BEGIN	(* remove first queue element *)
		IF queue # NIL THEN
			id := queue.id; queue := queue.next
		ELSE
			id := 0
		END
	END Dequeue;

	PROCEDURE ProcessCompany (t: SqlDB.Table; p: ANYPTR);
		VAR h: SqlObxNets.Net; cp: SqlObxNets.Company;
	BEGIN	(* process all companies in set with id > 0 *)
		ASSERT(t.rows = 1, 20);
		t.Read(0, company);
		SqlObxNets.AddCompany(g, company.id, company.name, "", 0);
		WHILE (set # NIL) & (set.id < 0) DO set := set.next END;
		IF set # NIL THEN
			SqlObxDB.searchId := set.id; ASSERT(set.id > 0, 100); set := set.next;
			t.Exec("SELECT id, name FROM Companies WHERE id = :SqlObxDB.searchId");
			t.Call(ProcessCompany, NIL)
		ELSE
			cp := SqlObxNets.ThisCompany(g, company.id); h := g; g := NIL; Views.OpenView(SqlObxViews.New(h, cp))
		END
	END ProcessCompany;

	PROCEDURE ^ConsumeCompany(t: SqlDB.Table; p: ANYPTR);

	PROCEDURE ProduceCompanies (t: SqlDB.Table; p: ANYPTR);
		VAR i: INTEGER;
	BEGIN
		i := 0;
		WHILE i < t.rows DO
			t.Read(i, ownership);
			SqlObxNets.AddOwnership(g, ownership.owner, ownership.owned, ownership.percent);
			Enqueue(ownership.owner); Enqueue(-ownership.owner);
			Enqueue(ownership.owned); Enqueue(-ownership.owned);
			INC(i)
		END;
		t.Call(ConsumeCompany, NIL)
	END ProduceCompanies;

	PROCEDURE ConsumeCompany (t: SqlDB.Table; p: ANYPTR);
		VAR id: INTEGER;
	BEGIN
		Dequeue(id); WHILE (id # 0) & Marked(id) DO Dequeue(id) END;
		IF id # 0 THEN	(* ~Marked(id) *)
			Mark(id);
			IF id > 0 THEN	(* select owner *)
				SqlObxDB.searchId := id;
				t.Exec("SELECT * FROM Ownership WHERE owned = :SqlObxDB.searchId");
			ELSE	(* select owned *)
				SqlObxDB.searchId := -id;
				t.Exec("SELECT * FROM Ownership WHERE owner = :SqlObxDB.searchId")
			END;
			t.Call(ProduceCompanies, NIL)
		ELSE	(* queue is empty *)
			ASSERT(queue = NIL, 100);
			WHILE (set # NIL) & (set.id < 0) DO set := set.next END; ASSERT(set # NIL, 101); ASSERT(set.id > 0, 102);
			SqlObxDB.searchId := set.id; set := set.next;
			t.Exec("SELECT id, name FROM Companies WHERE id = :SqlObxDB.searchId");
			t.Call(ProcessCompany, NIL)
		END
	END ConsumeCompany;

	PROCEDURE GenNet*;
		VAR d: SqlDB.Database; res: INTEGER; t: SqlDB.Table;
	BEGIN
		ASSERT(SqlObxDB.searchId > 0, 20);
		SqlDB.OpenDatabase(protocol, id, password, datasource, SqlDB.async, SqlDB.hideErrors, d, res);
		ASSERT(d # NIL, 100);
		t := d.NewTable();
		set := NIL; queue := NIL; g := SqlObxNets.New();
		company.id := SqlObxDB.searchId;
		Enqueue(company.id); Enqueue(-company.id);	(* produce first company *)
		t.Call(ConsumeCompany, NIL);
		t.Clear
	END GenNet;

END SqlObxGen.
