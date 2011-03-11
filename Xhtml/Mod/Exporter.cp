MODULE XhtmlExporter;
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

(*
 add the following statement to Config.Setup:
		Converters.Register("", "XhtmlExporter.ExportText", "TextViews.View", "html", {});

 supported elements & attributes: html, head, title, body, p, a(href,id), font(face,size, color), strong, em, u

 assumptions:
	- there is only one "special" non-RGB color: Ports.defaultColor
	- one specific font size (defaultSize) is used as default size
	- no embedded views are in the text, except link and target views
	- link views only contain local references within the text (i.e., StdLinks.ShowTarget('anchorname') commands)

*)

	IMPORT
		Strings, Files, Dialog, Stores, Fonts, Ports, Views, Properties,
		TextModels, TextViews, TextRulers, StdLinks,
		XhtmlWriters, XhtmlStdFileWriters,
		XhtmlEntitySets, XhtmlTextTableMarkers;

	CONST
		untitled = "#Xhtml:New Page";
		defaultSize = 10 * Fonts.point;	(* currently, BlackBox has no notion of default font size *)
		left = 0; center = 1; right = 2;

	TYPE
		Msg* = RECORD (Properties.Message)
			string*: ARRAY 2048 OF CHAR
		END;

		Exporter = POINTER TO RECORD
			inLink: BOOLEAN;	(* this variable is used for a state machine used for translating hyperlinks *)
			inTable: BOOLEAN;	(* this variable is used for a state machine used for translating tables *)
			inRow: BOOLEAN;	(* this variable is used for a state machine used for translating tables *)
			inField: BOOLEAN;	(* this variable is used for a state machine used for translating tables *)
			inPara: BOOLEAN;	(* Generating paragraph elements is delayed so that empty elements are never created.
												This variable is used for a state machine that implements the necessary delaying.
											*)
			afterSpace: BOOLEAN;	(* this variable is used for a state machine that avoids successive spaces in an element *)
			attr: TextModels.Attributes;	(* current attributes; needed to decide whether an attribute change becomes necessary *)
			level: INTEGER;	(* indentation level *)
			currentRuler: TextRulers.Ruler;	(* most recently read ruler *)
			tabIndex: INTEGER;	(* tab index in current line *)
			wr: XhtmlWriters.Writer
		END;


	PROCEDURE Invariant (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inPara OR (e.attr = NIL), 101);	(* ~e.inPara => (e.attr = NIL) *)
		ASSERT(~e.inField OR e.inRow, 102);	(* e.inField => e.inRow *)
		ASSERT(~e.inRow OR e.inTable, 103);	(* e.inRow => e.inTable *)
		ASSERT(~e.afterSpace OR e.inPara, 104);	(* e.afterSpace => e.inPara *)
		ASSERT(e.level >= 0, 105);
		ASSERT(e.tabIndex >= -1, 106);
		ASSERT(~e.inField OR (e.tabIndex >= 0), 107);	(* e.inField => e.tabIndex >= 0 *)
		ASSERT(~e.inTable OR (e.currentRuler # NIL), 108);	(* e.inTable => (e.currentRuler # NIL) *)
		ASSERT(e.wr # NIL, 109)
	END Invariant;

	PROCEDURE ColorToString (c: INTEGER; OUT str: ARRAY OF CHAR);
		VAR red, green, blue: INTEGER; h: ARRAY 16 OF CHAR;
	BEGIN
		red := c MOD 256; c := c DIV 256;
		green := c MOD 256; c := c DIV 256;
		blue := c;
		c := red;
		c := c * 256 + green;
		c := c * 256 + blue;
		Strings.IntToStringForm(c, Strings.hexadecimal, 6, "0", FALSE, h);
		str := "#" + h
	END ColorToString;

	PROCEDURE SizeToString (s: INTEGER; OUT str: ARRAY OF CHAR);
	BEGIN	(* note: defaultSize is already handled *)
		s := s DIV Fonts.point;
		IF s < 8 THEN s := 1
		ELSIF s <= 10 THEN s := 2
		ELSIF s <= 12 THEN s := 3
		ELSIF s <= 14 THEN s := 4
		ELSIF s <= 18 THEN s := 5
		ELSIF s <= 24 THEN s := 6
		ELSE s := 7
		END;
		Strings.IntToString(s, str)
	END SizeToString;


	(*
		text = p | parsed text
		body = text | (table tr td text)
	*)

	PROCEDURE SetAttr (e: Exporter; a: TextModels.Attributes);
	(* nesting: <p> <font> <strong> <em> <u>   text   </u> </em> </strong> </font> </p> *)
	(* font has face, color, size attributes. size ranges from 1..7 *)
		CONST bold = (Fonts.normal + Fonts.bold) DIV 2;
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(e # NIL, 100);
		IF e.inPara THEN
			IF (e.attr # NIL) & (e.attr # a) THEN	(* close all special attributes *)
				IF Fonts.underline IN e.attr.font.style THEN e.wr.EndTag END;
				IF Fonts.italic IN e.attr.font.style THEN e.wr.EndTag END;
				IF e.attr.font.weight >= bold THEN e.wr.EndTag END;
				IF (e.attr.font.typeface # Fonts.default) OR (e.attr.color # Ports.defaultColor) OR (e.attr.font.size # defaultSize) THEN
					e.wr.EndTag
				END
			END;
			IF (a # NIL) & (e.attr # a) THEN	(* open new special attributes *)
				IF (a.font.typeface # Fonts.default) OR (a.color # Ports.defaultColor) OR (a.font.size # defaultSize) THEN
					e.wr.StartTag("font", XhtmlWriters.preserve);
					IF a.font.typeface # Fonts.default THEN
						IF a.font.typeface = "Arial" THEN s := a.font.typeface$ ELSE s := a.font.typeface + ", Arial" END;
						e.wr.Attr("face", s$)
					END;
					IF a.color # Ports.defaultColor THEN
						ColorToString(a.color, s); e.wr.Attr("color", s$)
					END;
					SizeToString(a.font.size, s);
					IF a.font.size # defaultSize THEN e.wr.Attr("size", s$) END
				END;
				IF a.font.weight >= bold THEN e.wr.StartTag("strong", XhtmlWriters.preserve) END;
				IF Fonts.italic IN a.font.style THEN e.wr.StartTag("em", XhtmlWriters.preserve) END;
				IF Fonts.underline IN a.font.style THEN e.wr.StartTag("u", XhtmlWriters.preserve) END
			END;
			e.attr := a
		END
	END SetAttr;

	PROCEDURE BeginPara (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inPara, 20);
		IF ~e.inLink & ~ e.inTable THEN
			e.wr.StartTag("p", XhtmlWriters.preserve)
		END;
		e.inPara := TRUE
	END BeginPara;

	PROCEDURE EndPara (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inPara, 20);
		SetAttr(e, NIL);
		IF ~e.inLink & ~e.inTable THEN
			e.wr.EndTag
		END;
		e.wr.Ln;
		e.afterSpace := FALSE;
		e.inPara := FALSE
	END EndPara;


	PROCEDURE Alignment (r: TextRulers.Ruler; tabIndex: INTEGER): INTEGER;
		VAR a: INTEGER; type: SET;
	BEGIN
		ASSERT(r # NIL, 100); ASSERT(tabIndex >= 0, 101);
		IF tabIndex < r.style.attr.tabs.len THEN
			type := r.style.attr.tabs.tab[tabIndex].type;
			IF TextRulers.centerTab IN type THEN
				a := center
			ELSIF TextRulers.rightTab IN type THEN
				a := right
			ELSE
				a := left
			END
		ELSE a := left
		END;
		RETURN a
	END Alignment;

	PROCEDURE BeginField (e: Exporter);
		VAR a: INTEGER;
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inField, 20); ASSERT(~e.inPara, 21);
		a := Alignment(e.currentRuler, e.tabIndex);
		e.wr.StartTag("td", XhtmlWriters.preserve);
		IF a = center THEN
			e.wr.Attr("align", "center")
		ELSIF a = right THEN
			e.wr.Attr("align", "right")
		END;
		e.inField := TRUE
	END BeginField;

	PROCEDURE EndField (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inField, 20); ASSERT(~e.inPara, 21);
		e.wr.EndTag;
		e.inField := FALSE
	END EndField;


	PROCEDURE BeginRow (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inRow, 20); ASSERT(~e.inField, 21); ASSERT(~e.inPara, 22);
		e.wr.StartTag("tr", XhtmlWriters.prettyPrint);
		e.inRow := TRUE
	END BeginRow;

	PROCEDURE EndRow (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inRow, 20); ASSERT(~e.inField, 21);
		e.wr.EndTag;
		e.inRow := FALSE
	END EndRow;


	PROCEDURE BeginTable (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inTable, 20); ASSERT(~e.inRow, 21); ASSERT(~e.inField, 22); ASSERT(~e.inPara, 23);
		e.wr.StartTag("table", XhtmlWriters.prettyPrint); e.wr.Attr("border", "1"); e.wr.Attr("width", "100%");
		e.inTable := TRUE
	END BeginTable;

	PROCEDURE EndTable (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inTable, 20); ASSERT(~e.inRow, 21); ASSERT(~e.inField, 22); ASSERT(~e.inPara, 23);
		e.wr.EndTag;
		e.inTable := FALSE
	END EndTable;

	PROCEDURE CompleteRow (e: Exporter);
		VAR n, i: INTEGER;
	BEGIN
		ASSERT(e # NIL, 100); ASSERT(e.inTable, 101); ASSERT(e.currentRuler # NIL, 102);
		IF ~e.inRow THEN BeginRow(e) END;
		IF e.tabIndex >= 0 THEN
			IF e.inPara THEN EndPara(e) END;
			IF ~e.inField THEN BeginField(e); e.wr.Data("&nbsp;") END;
			EndField(e)
		END;
		n := e.currentRuler.style.attr.tabs.len - e.tabIndex - 1;
		IF n >= 1 THEN	(* fill row with empty fields *)
			i := 0;
			WHILE i # n DO
				INC(e.tabIndex);
				BeginField(e); e.wr.Data("&nbsp;"); EndField(e);
				INC(i)
			END
		END;
		EndRow(e)
	END CompleteRow;

	PROCEDURE BegOfTable (e: Exporter; v: Views.View): BOOLEAN;
		VAR b: BOOLEAN;
	BEGIN
		ASSERT(e # NIL, 100);
		b := FALSE;
		IF v # NIL THEN
			b := XhtmlTextTableMarkers.IsOpenMark(v)
		END;
		RETURN b
	END BegOfTable;

	PROCEDURE EndOfTable (e: Exporter; v: Views.View): BOOLEAN;
		VAR b: BOOLEAN;
	BEGIN
		ASSERT(e # NIL, 100);
		b := FALSE;
		IF v # NIL THEN
			b := XhtmlTextTableMarkers.IsCloseMark(v)
		END;
		RETURN b
	END EndOfTable;


	PROCEDURE BeginHref (e: Exporter; v: StdLinks.Link);
		VAR s, s0: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inLink, 20); ASSERT(~e.inTable, 21); ASSERT(~e.inPara, 22);
		v.GetCmd(s);
		s0 := s$; s0[19] := 0X;	(* clip the command to the (supposed) ShowTarget name *)
		e.wr.StartTag("a", XhtmlWriters.preserve);
		IF s0 = "StdLinks.ShowTarget" THEN
			Strings.Extract(s, 21, LEN(s$) - 23, s);	(* extract the name of the target anchor *)
			e.wr.Attr("href", "#" + s)
		ELSE
			e.wr.Attr("href", s$)
		END;
		e.inLink := TRUE
	END BeginHref;

	PROCEDURE EndHref (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inLink, 20); ASSERT(~e.inTable, 21); ASSERT(~e.inPara, 22);
		e.wr.EndTag;
		e.inLink := FALSE
	END EndHref;

	PROCEDURE Id (e: Exporter; v: StdLinks.Target);
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inLink, 20); ASSERT(~e.inTable, 21); ASSERT(~e.inPara, 22);
		v.GetIdent(s);
		e.wr.StartTag("a", XhtmlWriters.preserve); e.wr.Attr("id", s$)
	END Id;

	PROCEDURE View (e: Exporter; v: Views.View);
	BEGIN
		ASSERT(e # NIL, 100); ASSERT(v # NIL, 101);
		WITH v: StdLinks.Link DO
			ASSERT(v.leftSide # e.inLink, 101);	(* links must not be nested *)
			IF v.leftSide THEN	(* open link *)
				BeginHref(e, v)
			ELSE	(* close link *)
				EndHref(e)
			END
		| v: StdLinks.Target DO
			IF v.leftSide THEN Id(e, v) END	(* open anchor *)
		ELSE	(* unknown view: here, an extension mechanism would be needed to handle such views *)
			(* skip *)
		END
	END View;

	PROCEDURE ExportText* (s: Stores.Store; f: Files.File);
		VAR
			e: Exporter;
			t: TextModels.Model; rd: TextModels.Reader;
			len: INTEGER;	(* number of characters that remain to be translated *)
			ch: CHAR; str: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(s # NIL, 20); ASSERT(f # NIL, 21); ASSERT(f.Length() = 0, 22);
		ASSERT(s IS TextViews.View, 23);
		NEW(e); e.inLink := FALSE; e.inTable := FALSE; e.inPara := FALSE; e.attr := NIL; e.afterSpace := FALSE; e.level := 0;
		e.wr := XhtmlStdFileWriters.New(f);
		t := s(TextViews.View).ThisModel();
		len := t.Length();
		e.wr.DocType("html", "-//W3C//DTD XHTML 1.0 Strict//EN", "http://www.w3.org/TR/xhtml1/DTD/strict.dtd");
		e.wr.Ln;
		e.wr.StartTag("html", XhtmlWriters.prettyPrint);
		e.wr.StartTag("head", XhtmlWriters.prettyPrint);
		e.wr.StartTag("title", XhtmlWriters.preserve); Dialog.MapString(untitled, str); e.wr.Data(str$); e.wr.EndTag;
		e.wr.EndTag;	(* head *)
		e.wr.Ln;
		e.wr.StartTag("body", XhtmlWriters.prettyPrint);
		rd := t.NewReader(NIL);
		e.tabIndex := -1;	(* no tab read in this line yet *)
		WHILE len # 0 DO
			Invariant(e);
			rd.ReadChar(ch);
			IF (rd.view # NIL) & (rd.view IS TextRulers.Ruler) THEN e.currentRuler := rd.view(TextRulers.Ruler) END;
			IF BegOfTable(e, rd.view) THEN
				ASSERT(~e.inTable, 101);
				IF e.inPara THEN EndPara(e) END;
				BeginTable(e);
				e.tabIndex := -1	(* no tab read in this line yet *)
			ELSIF EndOfTable(e, rd.view) THEN
				IF e.inRow THEN CompleteRow(e) END;
				EndTable(e);
				e.tabIndex := -1	(* no tab read in this line yet *)
			ELSIF (ch = TextModels.para) OR (ch = TextModels.line) OR (ch = 0X) THEN
				IF e.inTable THEN CompleteRow(e) END;

				IF ~e.inPara THEN BeginPara(e) END;
				EndPara(e);
				e.tabIndex := -1	(* no tab read in this line yet *)
			ELSIF ch = TextModels.tab THEN
				INC(e.tabIndex);
				IF e.inTable THEN
					IF ~e.inRow THEN BeginRow(e) END;
					IF e.tabIndex >= 1 THEN	(* space until first tab is not part of a table column *)
						IF e.inField THEN
							IF e.inPara THEN EndPara(e) END
						ELSE	(* last table field is empty *)
							BeginField(e); e.wr.Data("&nbsp;")
						END;
						EndField(e)
					END
				ELSE
					IF ~e.inPara THEN BeginPara(e) END;
					e.wr.Data("&nbsp;&nbsp;&nbsp;"); e.afterSpace := TRUE	(* emulate tab with three spaces *)
				END
			ELSIF ch = TextModels.viewcode THEN	(* translate rd.view into HTML *)
				IF e.inPara THEN EndPara(e) END;
				View(e, rd.view)
			ELSE
				IF e.inTable THEN
					IF ~e.inRow THEN BeginRow(e) END;
					IF ~e.inField THEN BeginField(e) END
				END;
				IF ~e.inPara THEN BeginPara(e) END;
				SetAttr(e, rd.attr);
				IF (ch = " ") OR (ch = TextModels.nbspace) OR (ch = TextModels.digitspace) THEN
					IF e.afterSpace THEN e.wr.Data("&nbsp;") ELSE e.wr.Data(" "); e.afterSpace := TRUE END
				ELSIF ch = TextModels.zwspace THEN
					e.wr.Data("&zwnj;"); e.afterSpace := TRUE
				ELSIF (ch = TextModels.hyphen) OR (ch = TextModels.nbhyphen) THEN
					e.wr.Data("-"); e.afterSpace := FALSE
				ELSE
					XhtmlEntitySets.MapCharToEntity(ch, str);	(* this is the normal case of writing a character *)
					e.wr.Data(str$); e.afterSpace := FALSE
				END
			END;
			DEC(len)
		END;
		ASSERT(~e.inLink, 101); ASSERT(~e.inTable, 102);
		IF e.inPara THEN EndPara(e) END;
		e.wr.EndTag;	(* body *)
		e.wr.EndTag;	(* html *)
		e.wr.Ln
	END ExportText;

END XhtmlExporter.
