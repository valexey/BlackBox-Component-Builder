MODULE SqlObxNets;
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

	TYPE
		Net* = POINTER TO NetDesc;
		Node* = POINTER TO NodeDesc;

		Company* = POINTER TO RECORD
			id-: INTEGER;
			name-, ceo-: ARRAY 32 OF CHAR;
			employees-: INTEGER;
			ownedBy-: Node;	(* list of owner companies *)
			owns-: Node;	(* list of owned companies *)
			time*: INTEGER;	(* temporary data for layout algorithm *)
			x*, y*: INTEGER;	(* temporary data for layout algorithm *)
			next: Company
		END;

		NodeDesc* = RECORD
			next-: Node;
			percent-: INTEGER;
			company-: Company
		END;

		NetDesc* = RECORD
			first: Company
		END;

	PROCEDURE AddCompany* (g: Net; id: INTEGER; name, ceo: ARRAY OF CHAR; employees: INTEGER);
		VAR c: Company;
	BEGIN
		ASSERT(g # NIL, 20); ASSERT(id > 0, 21);
		c := g.first; WHILE (c # NIL) & (c.id # id) DO c := c.next END;
		IF c # NIL THEN
			IF c.name = "" THEN
				c.name := name$; c.ceo := ceo$; c.employees := employees;
			ELSE
				ASSERT((name = "") OR (c.name = name), 22)
			END
		ELSE
			NEW(c); c.id := id; c.name := name$; c.ceo := ceo$; c.employees := employees;
			c.next := g.first; g.first := c
		END
	END AddCompany;

	PROCEDURE AddNode (VAR h: Node; percent: INTEGER; company: Company);
		VAR n: Node;
	BEGIN
		NEW(n); n.percent := percent; n.company := company;
		IF h = NIL THEN
			h := n
		ELSE
			WHILE (h.percent >= percent) & (h.next # NIL) DO h := h.next END;
			n.next := h.next; h.next := n
		END
	END AddNode;

	PROCEDURE AddOwnership* (g: Net; owner, owned: INTEGER; percent: INTEGER);
		VAR h0, h1: Company;
	BEGIN
		ASSERT(g # NIL, 20); ASSERT(owner > 0, 21); ASSERT(owned > 0, 22);
		ASSERT(owned # owner, 23); ASSERT(percent > 0, 24); ASSERT(percent <= 100, 25);
		h0 := g.first; WHILE (h0 # NIL) & (h0.id # owner) DO h0 := h0.next END;
		IF h0 = NIL THEN
			AddCompany(g, owner, "", "", 0);
			h0 := g.first; WHILE (h0 # NIL) & (h0.id # owner) DO h0 := h0.next END; ASSERT(h0 # NIL, 100)
		END;
		h1 := g.first; WHILE (h1 # NIL) & (h1.id # owned) DO h1 := h1.next END;
		IF h1 = NIL THEN
			AddCompany(g, owned, "", "", 0);
			h1 := g.first; WHILE (h1 # NIL) & (h1.id # owned) DO h1 := h1.next END; ASSERT(h1 # NIL, 100)
		END;
		AddNode(h1.ownedBy, percent, h0);
		AddNode(h0.owns, percent, h1)
	END AddOwnership;

	PROCEDURE ThisCompany* (g: Net; id: INTEGER): Company;
		VAR h: Company;
	BEGIN
		ASSERT(g # NIL, 20); ASSERT(id > 0, 21);
		h := g.first; WHILE (h # NIL) & (h.id # id) DO h := h.next END;
		RETURN h
	END ThisCompany;

	PROCEDURE CompanyAt* (g: Net; x, y, w, h: INTEGER): Company;
	(* not a nice interface; could be improved *)
		VAR p: Company;
	BEGIN
		ASSERT(g # NIL, 20);
		p := g.first; WHILE (p # NIL) & ((x < p.x) OR (y < p.y) OR (x >= p.x + w) OR (y >= p.y + h)) DO p := p.next END;
		RETURN p
	END CompanyAt;

	PROCEDURE New* (): Net;
		VAR g: Net;
	BEGIN
		NEW(g); RETURN g
	END New;

END SqlObxNets.
