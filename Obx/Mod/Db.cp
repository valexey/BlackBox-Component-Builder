MODULE ObxDb;
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

	IMPORT Views, TextModels, TextMappers, TextViews, TextControllers;

	CONST
		int = TextMappers.int; string = TextMappers.string; real = TextMappers.real; invalid = TextMappers.invalid;

	TYPE
		Node = POINTER TO RECORD
			next: Node;
			id: INTEGER;
			name: TextMappers.String;
			value: REAL
		END;

	VAR list: Node;

	PROCEDURE Enter (id: INTEGER; name: TextMappers.String; value: REAL);
		VAR n, h, p: Node;
	BEGIN	(* insert a new tuple into the list at its correct position *)
		NEW(n); n.id := id; n.name := name; n.value := value;
		h := list; p := NIL; WHILE (h # NIL) & (h.id <= id) DO p := h; h := h.next END;
		IF p # NIL THEN	(* insert between p and h *)
			p.next := n
		ELSE	(* insert at beginning *)
			list := n
		END;
		n.next := h
	END Enter;

	PROCEDURE EnterData*;
		VAR c: TextControllers.Controller; beg, end: INTEGER;
			s: TextMappers.Scanner; id: INTEGER;
			name: TextMappers.String; value: REAL;
	BEGIN	(* read a text selection and split it into an integer, string, and real field *)
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			s.ConnectTo(c.text); s.SetPos(beg);
			s.Scan;
			WHILE (s.type = TextMappers.int) & (s.Pos() <= end) DO
				IF s.type = int THEN id := s.int; s.Scan ELSE s.type := TextMappers.invalid END;
				IF s.type = string THEN name := s.string; s.Scan ELSE s.type := invalid END;
				IF s.type = real THEN value := SHORT(s.real); s.Scan ELSE s.type := invalid END;
				Enter(id, name, value)	(* enter the new tuple into a global list *)
			END;
			c.SelectAll(FALSE)
		END
	END EnterData;

	PROCEDURE ListData*;
		VAR t: TextModels.Model; n: Node; f: TextMappers.Formatter;
	BEGIN	(* write all entered tuples into a new text *)
		t := TextModels.dir.New();
		f.ConnectTo(t); f.SetPos(0);
		n := list;
		WHILE n # NIL DO
			f.WriteInt(n.id); f.WriteTab; f.WriteString(n.name); f.WriteTab;
			(* WriteRealForm allows to format real numbers in a flexible way, here rounding to two digits after the comma *)
			f.WriteRealForm(n.value, 7, 0, -2, TextModels.digitspace);	(* "-2" indicates 2 digits after comma *)
			f.WriteLn;
			n := n.next
		END;
		Views.OpenView(TextViews.dir.New(t))
	END ListData;

	PROCEDURE Reset*;
	BEGIN	(* release data structure so that garbage collector can reclaim memory *)
		list := NIL
	END Reset;

END ObxDb.
