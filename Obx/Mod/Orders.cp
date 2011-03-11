MODULE ObxOrders;
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

	IMPORT
		Files, Dialog, Fonts, Stores, Views,
		TextModels, TextViews, TextMappers, TextRulers, StdCmds, StdStamps;

	CONST
		(* values for card field of interactor *)
		amex = 0; master = 1; visa = 2;
		(* prices in 1/100 Swiss Francs *)
		ofwinfullVal = 45000; ofmacfullVal = 45000; ofwineduVal = 15000; ofmaceduVal = 15000;
		odfVal = 5000; vatVal = 65;
		type = "dat";	(* file type *)

	TYPE
		Interactor* = RECORD
			name*, company*, adr1*, adr2*, adr3*, email*: ARRAY 128 OF CHAR;
			phone*, fax*: ARRAY 32 OF CHAR;
			ofwinfull*, ofmacfull*, ofwinedu*, ofmacedu*, odf*: INTEGER;
			card*: INTEGER;
			cardno*: ARRAY 24 OF CHAR;
			vat*: BOOLEAN
		END;

		Element = POINTER TO RECORD
			prev, next: Element;
			data: Interactor
		END;

	VAR
		par*: Interactor;
		root, cur: Element;	(* header and current element of doubly-linked ring *)
		name: Files.Name;
		loc: Files.Locator;

	PROCEDURE ReadElem (VAR rd: Stores.Reader; e: Element);
	BEGIN
		rd.ReadString(e.data.name);rd.ReadString(e.data.company);
		rd.ReadString(e.data.adr1); rd.ReadString(e.data.adr2); rd.ReadString(e.data.adr3);
		rd.ReadString(e.data.email);
		rd.ReadString(e.data.phone); rd.ReadString(e.data.fax);
		rd.ReadString(e.data.cardno);
		rd.ReadInt(e.data.ofwinfull); rd.ReadInt(e.data.ofmacfull);
		rd.ReadInt(e.data.ofwinedu); rd.ReadInt(e.data.ofmacedu);
		rd.ReadInt(e.data.odf);
		rd.ReadInt(e.data.card);
		rd.ReadBool(e.data.vat)
	END ReadElem;

	PROCEDURE WriteElem (VAR wr: Stores.Writer; e: Element);
	BEGIN
		wr.WriteString(e.data.name); wr.WriteString(e.data.company);
		wr.WriteString(e.data.adr1); wr.WriteString(e.data.adr2); wr.WriteString(e.data.adr3);
		wr.WriteString(e.data.email);
		wr.WriteString(e.data.phone); wr.WriteString(e.data.fax);
		wr.WriteString(e.data.cardno);
		wr.WriteInt(e.data.ofwinfull); wr.WriteInt(e.data.ofmacfull);
		wr.WriteInt(e.data.ofwinedu); wr.WriteInt(e.data.ofmacedu);
		wr.WriteInt(e.data.odf);
		wr.WriteInt(e.data.card);
		wr.WriteBool(e.data.vat)
	END WriteElem;

	PROCEDURE Init;
	BEGIN
		cur := root; root.next := root; root.prev := root
	END Init;

	PROCEDURE Update;
	BEGIN
		par := cur.data; Dialog.Update(par)
	END Update;

	PROCEDURE Load*;
		VAR e: Element; f: Files.File; rd: Stores.Reader; count: INTEGER;
	BEGIN
		Dialog.GetIntSpec(type, loc, name);
		IF loc # NIL THEN
			f := Files.dir.Old(loc, name, Files.shared);
			IF (f # NIL) & (f.type = type) THEN
				rd.ConnectTo(f);
				rd.ReadInt(count);
				Init;
				WHILE count # 0 DO
					NEW(e);
					IF e # NIL THEN
						e.prev := cur; e.next := cur.next; e.prev.next := e; e.next.prev := e;
						ReadElem(rd, e);
						cur := e; DEC(count)
					ELSE
						Dialog.ShowMsg("out of memory"); Dialog.Beep;
						count := 0; root.next := root; root.prev := root; cur := root
					END
				END;
				Update
			ELSE
				Dialog.ShowMsg("cannot open file"); Dialog.Beep
			END
		END
	END Load;

	PROCEDURE Save*;
		VAR e: Element; f: Files.File; wr: Stores.Writer; count, res: INTEGER;
	BEGIN
		IF (loc = NIL) OR (name = "") THEN Dialog.GetExtSpec("", "", loc, name) END;
		IF (loc # NIL) & (name # "") THEN
			f := Files.dir.New(loc, Files.dontAsk); wr.ConnectTo(f);
			e := root.next; count := 0; WHILE e # root DO INC(count); e := e.next END;	(* count elements *)
			wr.WriteInt(count);
			e := root.next; WHILE e # root DO WriteElem(wr, e); e := e.next END;	(* write elements *)
			f.Register(name, type, Files.dontAsk, res);
			Init; name := ""; loc := NIL;	(* close database *)
			Update
		END
	END Save;

	PROCEDURE Insert*;
		VAR e: Element;
	BEGIN
		NEW(e);
		IF e # NIL THEN	(* insert new record at end of database *)
			IF cur # root THEN cur.data := par END;	(* save current record, in case it was changed *)
			e.prev := root.prev; e.next := root; e.prev.next := e; e.next.prev := e;
			cur := e;
			Update
		ELSE
			Dialog.ShowMsg("out of memory"); Dialog.Beep
		END
	END Insert;

	PROCEDURE Delete*;
	BEGIN
		IF cur # root THEN
			StdCmds.CloseDialog;
			cur.next.prev := cur.prev; cur.prev.next := cur.next;
			cur := cur.prev; IF cur = root THEN cur := root.next END;
			Update
		END
	END Delete;

	PROCEDURE Next*;
	BEGIN
		IF cur.next # root THEN
			cur.data := par; cur := cur.next; Update
		END
	END Next;

	PROCEDURE Prev*;
	BEGIN
		IF cur.prev # root THEN
			cur.data := par; cur := cur.prev; Update
		END
	END Prev;

	PROCEDURE NonemptyGuard* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := cur = root
	END NonemptyGuard;

	PROCEDURE NextGuard* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := cur.next = root
	END NextGuard;

	PROCEDURE PrevGuard* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := cur.prev = root
	END PrevGuard;

	PROCEDURE WriteLine (VAR f: TextMappers.Formatter; no, val: INTEGER; name: ARRAY OF CHAR;
										VAR total, vat: INTEGER);
	BEGIN
		IF no # 0 THEN
			val := no * val;
			f.WriteInt(no); f.WriteString(name);
			INC(total, val); INC(vat, val);
			f. WriteTab;
			f.WriteIntForm(val DIV 100, 10, 5, TextModels.digitspace, FALSE);
			f.WriteChar(".");
			f.WriteIntForm(val MOD 100, 10, 2, "0", FALSE);
			f.WriteLn
		END
	END WriteLine;

	PROCEDURE NewRuler (): TextRulers.Ruler;
		VAR r: TextRulers.Ruler;
	BEGIN
		r := TextRulers.dir.New(NIL);
(*	
		TextRulers.SetLeft(r, 30 * Ports.mm);
		TextRulers.SetRight(r, 165 * Ports.mm);
		TextRulers.AddTab(r, 130 * Ports.mm);
*)
		RETURN r
	END NewRuler;

	PROCEDURE Invoice*;
		VAR v: TextViews.View; f: TextMappers.Formatter; a: TextModels.Attributes;
			total, vat: INTEGER;
	BEGIN
		IF cur # root THEN
			v := TextViews.dir.New(TextModels.dir.New());
			f.ConnectTo(v.ThisModel());
			f.WriteView(NewRuler());
			(* create header of invoice *)
			f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn;
			f.WriteTab;
			f.WriteString("Basel, "); f.WriteView(StdStamps.New());
			f.WriteLn; f.WriteLn; f.WriteLn;
			(* write address *)
			IF par.name # "" THEN f.WriteString(par.name); f.WriteLn END;
			IF par.company # "" THEN f.WriteString(par.company); f.WriteLn END;
			IF par.adr1 # "" THEN f.WriteString(par.adr1); f.WriteLn END;
			IF par.adr2 # "" THEN f.WriteString(par.adr2); f.WriteLn END;
			IF par.adr3 # "" THEN f.WriteString(par.adr3); f.WriteLn END;
			f.WriteLn; f.WriteLn; f.WriteLn;
			(* set bold font weight *)
			a := f.rider.attr;
			f.rider.SetAttr(TextModels.NewWeight(a, Fonts.bold));
			f.WriteString("Invoice");	(* this string will appear in bold face *)
			f.rider.SetAttr(a);	(* restore default weight *)
			f.WriteLn; f.WriteLn;
			f.WriteString("Creditcard: ");
			CASE par.card OF
			| amex: f.WriteString("American Express")
			| master: f.WriteString("Euro/MasterCard")
			| visa: f.WriteString("Visa")
			END;
			f.WriteLn; f.WriteLn; f.WriteLn;
			(* write products with subtotals *)
			total := 0; vat := 0;
			WriteLine(f, par.ofwinfull, ofwinfullVal, " ofwin full", total, vat);
			WriteLine(f, par.ofmacfull, ofmacfullVal, " ofmac full", total, vat);
			WriteLine(f, par.ofwinedu, ofwineduVal, " ofwin edu", total, vat);
			WriteLine(f, par.ofmacedu, ofmaceduVal, " ofmac edu", total, vat);
			WriteLine(f, par.odf, odfVal, " odf", total, vat);
			(* write vat *)
			IF par.vat THEN
				f.WriteLn;
				INC(total, (vat * vatVal) DIV 1000);	(* vat is 6.5% *)
				f.WriteString("value added tax (");
				f.WriteInt(vatVal DIV 10); f.WriteChar("."); f.WriteInt(vatVal MOD 10);
				f.WriteString("% on ");
				f.WriteInt(vat DIV 100); f.WriteChar("."); f.WriteIntForm(vat MOD 100, 10, 2, "0", FALSE);
				f.WriteString(")");
				f.WriteTab;
				f.WriteIntForm((vat * vatVal) DIV 100000, 10, 5, TextModels.digitspace, FALSE);
				f.WriteChar("."); f.WriteIntForm(((vat * vatVal) DIV 1000) MOD 100, 10, 2, "0", FALSE);
				f.WriteLn
			END;
			(* write total *)
			f.WriteLn;
			f.WriteString("Total"); f.WriteTab;
			f.WriteIntForm(total DIV 100, 10, 5, TextModels.digitspace, FALSE);
			f.WriteChar("."); f.WriteIntForm(total MOD 100, 10, 2, "0", FALSE);
			f.WriteString(" sFr.");
			f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn;
			f.WriteLn; f.WriteLn; f.WriteLn; f.WriteLn;
			f.WriteString("The exporter of the products covered by this document declares that, except where otherwise clearly indicated, these products are of Swiss preferential origin.");
			f.WriteLn;
			Views.OpenAux(v, "Invoice")
		END
	END Invoice;

BEGIN
	NEW(root); Init
END ObxOrders.
