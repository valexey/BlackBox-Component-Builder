MODULE DevSearch;
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
		Kernel, Files, Fonts, Ports, Models, Views, Containers, Dialog, Windows,
		TextModels, TextRulers, TextViews, TextControllers, TextMappers, TextCmds, StdLinks;

	CONST
		N = 32; T = 10;
		maxPat = 64;
		noMatchFoundKey = "#Std:NoMatchFound";
		locationKey = "#Std:Location";
		countKey = "#Std:Count";
		searchingKey = "#Std:Searching";
		openFindCmdKey = "#Dev:OpenFindCmd";
		openFindCmdNotFoundKey = "#Dev:OpenFindCmdNotFound";

	TYPE
		Pattern = ARRAY maxPat OF CHAR;
		Text = POINTER TO RECORD
			next: Text;
			num: INTEGER;
			title: Files.Name
		END;

	VAR
		w: TextMappers.Formatter;

	PROCEDURE Find (t: TextModels.Model; pat: Pattern; title: Files.Name; VAR list: Text; caseSens: BOOLEAN);
		VAR r: TextModels.Reader; num: INTEGER; i, j, b, e, n: INTEGER; ch: CHAR; ref: Pattern; l: Text;
	BEGIN
		n := 0; num := 0;
		WHILE pat[n] # 0X DO
			IF ~caseSens THEN pat[n] := CAP(pat[n]) END ;
			INC(n)
		END;
		r := t.NewReader(NIL);
		r.SetPos(0); r.ReadChar(ch);
		WHILE ~r.eot DO
			ref[0] := ch; i := 0; j := 0; b := 0; e := 1;
			WHILE ~r.eot & (i < n) DO
				IF caseSens & (pat[i]=ch) OR ~caseSens & (pat[i] = CAP(ch)) THEN INC(i); j := (j + 1) MOD maxPat
				ELSE i := 0; b := (b + 1) MOD maxPat; j := b
				END;
				IF j # e THEN ch := ref[j]
				ELSE r.ReadChar(ch); ref[j] := ch; e := (e + 1) MOD maxPat
				END
			END;
			IF i = n THEN INC(num) END
		END;
		IF num > 0 THEN
			NEW(l); l.num := num; l.title := title; l.next := list; list := l
		END
	END Find;
	
	PROCEDURE List (list: Text; pat: Pattern; source, caseSens: BOOLEAN);
		VAR a0: TextModels.Attributes; cmd: ARRAY 256 OF CHAR; this, t: Text; max: INTEGER;
	BEGIN
		IF list = NIL THEN
			Dialog.MapString(noMatchFoundKey, cmd);
			w.WriteString(cmd)
		ELSE
			a0 := w.rider.attr;
			w.rider.SetAttr(TextModels.NewStyle(w.rider.attr, {Fonts.italic}));
			Dialog.MapString(locationKey, cmd);
			w.WriteString(cmd); w.WriteTab;
			Dialog.MapString(countKey, cmd);
			w.WriteString(cmd); w.WriteLn;
			w.rider.SetAttr(a0);
			REPEAT
				t := list; max := 1; this := NIL;
				WHILE t # NIL DO
					IF t.num >= max THEN max := t.num; this := t END;
					t := t.next
				END;
				IF this # NIL THEN
					w.rider.SetAttr(TextModels.NewStyle(w.rider.attr, {Fonts.underline}));
					w.rider.SetAttr(TextModels.NewColor(w.rider.attr, Ports.blue));
					IF source THEN 
						cmd := "StdCmds.OpenDoc('" + this.title + "'); DevSearch.SelectCaseSens('" + pat + "')"
					ELSE
						IF caseSens THEN
							cmd := "StdCmds.OpenBrowser('" + this.title + "', '" + this.title + "'); " +
								"DevSearch.SelectCaseSens('" + pat + "')"
						ELSE
							cmd := "StdCmds.OpenBrowser('" + this.title + "', '" + this.title + "'); " +
								"DevSearch.SelectCaseInSens('" + pat + "')"
						END
					END;
					w.WriteView(StdLinks.dir.NewLink(cmd));
					w.WriteString(this.title);
					w.WriteView(StdLinks.dir.NewLink(""));
					w.rider.SetAttr(a0);
					w.WriteTab; w.WriteInt(this.num); w.WriteLn;
					this.num := 0
				END
			UNTIL this = NIL
		END
	END List;

	PROCEDURE NewRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm;
		VAR p: TextRulers.Prop;
	BEGIN
		NEW(p);
		p.valid := {TextRulers.right, TextRulers.tabs, TextRulers.opts};
		p.opts.val := {TextRulers.rightFixed}; p.opts.mask := p.opts.val;
		p.right := 100 * mm;
		p.tabs.len := 1;
		p.tabs.tab[0].stop := 70 * mm;
		RETURN TextRulers.dir.NewFromProp(p)
	END NewRuler;

	PROCEDURE ThisText (loc: Files.Locator; VAR name: Files.Name): TextModels.Model;
		VAR v: Views.View; m: Models.Model;
	BEGIN
		v := Views.OldView(loc, name);
		IF v # NIL THEN
			m := v.ThisModel();
			IF m # NIL THEN
				WITH m: TextModels.Model DO RETURN m ELSE END
			END
		END;
		RETURN NIL
	END ThisText;

	PROCEDURE Search (source, caseSens: BOOLEAN);
		VAR pat: Pattern; t, log: TextModels.Model; v: Views.View; title: Views.Title; c: Containers.Controller;
			files: Files.FileInfo; dirs: Files.LocInfo;
			loc: Files.Locator; path, p: Files.Name; list: Text;
	BEGIN
		(*TextCmds.InitFindDialog; *)
		pat := TextCmds.find.find$;
		IF pat # "" THEN
			Dialog.ShowStatus(searchingKey);
			TextCmds.find.find := pat$;
			log := TextModels.dir.New();
			w.ConnectTo(log); w.SetPos(0);
			IF source THEN loc := Files.dir.This("Mod"); path := "Mod"
			ELSE loc := Files.dir.This("Docu"); path := "Docu"
			END;
			files := Files.dir.FileList(loc); list := NIL;
			WHILE files # NIL DO
				IF files.type = Kernel.docType THEN
					p := path + "/" + files.name;
					Find(ThisText(loc, files.name), pat, p, list, caseSens)
				END;
				files := files.next
			END;
			loc := Files.dir.This("");
			dirs := Files.dir.LocList(loc);
			WHILE dirs # NIL DO
				loc := Files.dir.This(dirs.name); path := dirs.name + "/";
				IF source THEN loc := loc.This("Mod"); path := path + "Mod"
				ELSE loc := loc.This("Docu"); path := path +"Docu"
				END;
				files := Files.dir.FileList(loc);
				WHILE files # NIL DO
					IF files.type = Kernel.docType THEN
						p := path + "/" + files.name;
						t := ThisText(loc, files.name);
						IF t # NIL THEN
							Find(t, pat, p, list, caseSens)
						END
					END;
					files := files.next
				END;
				dirs := dirs.next
			END;
			List(list, pat, source, caseSens);
			v := TextViews.dir.New(log);
			title := 'Search for "' + pat + '"';
			v(TextViews.View).SetDefaults(NewRuler(), TextViews.dir.defAttr);
			Views.OpenAux(v, title);
			c := v(Containers.View).ThisController();
			c.SetOpts(c.opts + {Containers.noCaret});
			w.ConnectTo(NIL);
			Dialog.ShowStatus("")
		END
	END Search;

	PROCEDURE SearchInSources*;
	BEGIN
		Search(TRUE, TRUE)
	END SearchInSources;

	PROCEDURE SearchInDocu* (opts: ARRAY OF CHAR);
		VAR caseSens: BOOLEAN;
	BEGIN
		caseSens := ~TextCmds.find.ignoreCase;
		IF LEN(opts$) > 0 THEN
			IF CAP(opts[0]) = 'S' THEN
				caseSens := TRUE
			ELSIF CAP(opts[0]) = 'I' THEN
				caseSens := FALSE
			END
		END;
		Search(FALSE, caseSens)
	END SearchInDocu;
	
	PROCEDURE SelectCaseSens* (pat: ARRAY OF CHAR);
	BEGIN
		TextCmds.find.find := pat$;
		TextCmds.find.ignoreCase := FALSE;
		Dialog.Update(TextCmds.find);
		TextCmds.FindFirst("")
	END SelectCaseSens;

	PROCEDURE SelectCaseInSens* (pat: ARRAY OF CHAR);
		VAR res: INTEGER; cmd: Dialog.String;
	BEGIN
		TextCmds.find.find := pat$;
		TextCmds.find.ignoreCase := TRUE;
		Dialog.Update(TextCmds.find);
		Dialog.MapString(openFindCmdKey, cmd);
		Dialog.Call(cmd, openFindCmdNotFoundKey, res);
		TextCmds.FindFirst("")
	END SelectCaseInSens;


	PROCEDURE NextChar (r: TextModels.Reader): CHAR;
		VAR ch: CHAR;
	BEGIN
		REPEAT r.ReadChar(ch) UNTIL (ch > " ") OR r.eot;
		RETURN ch
	END NextChar;

	PROCEDURE CompTexts (ta, tb: TextModels.Model; VAR sa, sb, ea, eb: INTEGER);
		VAR da, db, d, i, j, p: INTEGER; t: LONGINT; ra, rb: TextModels.Reader;
			cha, chb: CHAR; s: ARRAY N OF CHAR;
	BEGIN
		ra := ta.NewReader(NIL); ra.SetPos(ea);
		rb := tb.NewReader(NIL); rb.SetPos(eb);
		REPEAT
			cha := NextChar(ra); chb := NextChar(rb)
		UNTIL (cha # chb) OR ra.eot OR rb.eot;
		IF ra.eot THEN sa := ra.Pos() ELSE sa := ra.Pos() - 1 END;
		IF rb.eot THEN sb := rb.Pos() ELSE sb := rb.Pos() - 1 END;
		t := Kernel.Time() + T * Kernel.timeResolution;
		da := sa + 1; db := sb + 1; d := 1; j := 0;
		REPEAT
			ea := da;
			IF ea < ta.Length() THEN
				ra.SetPos(ea); s[0] := NextChar(ra);
				da := ra.Pos(); i := 1;
				WHILE i < N DO s[i] := NextChar(ra); INC(i) END;
				i := 0; rb.SetPos(sb);
				REPEAT
					eb := rb.Pos(); chb := NextChar(rb);
					IF chb = s[0] THEN
						p := rb.Pos(); j := 0;
						WHILE (j < N) & (chb = s[j]) DO chb := NextChar(rb); INC(j) END;
						rb.SetPos(p)
					END;
					INC(i)
				UNTIL (j = N) OR (i = d) OR rb.eot
			END;
			INC(d);
			IF j < N THEN
				eb := db;
				IF eb < tb.Length() THEN
					rb.SetPos(eb); s[0] := NextChar(rb);
					db := rb.Pos(); i := 1;
					WHILE i < N DO s[i] := NextChar(rb); INC(i) END;
					i := 0; ra.SetPos(sa);
					REPEAT
						ea := ra.Pos(); cha := NextChar(ra);
						IF cha = s[0] THEN
							p := ra.Pos(); j := 0;
							WHILE (j < N) & (cha = s[j]) DO cha := NextChar(ra); INC(j) END;
							ra.SetPos(p)
						END;
						INC(i)
					UNTIL (j = N) OR (i = d) OR ra.eot
				END
			END
		UNTIL (j = N) OR (ea >= ta.Length()) & (eb >= tb.Length()) OR (Kernel.Time() > t);
		IF j < N THEN ea := ta.Length(); eb := tb.Length() END
	END CompTexts;

	PROCEDURE Compare*;
		VAR wa, wb: Windows.Window; va, vb: Views.View; ca, cb: Containers.Controller; sa, sb, ea, eb: INTEGER;
	BEGIN
		wa := Windows.dir.First();
		IF wa # NIL THEN
			wb := Windows.dir.Next(wa);
			IF wb # NIL THEN
				va := wa.doc.ThisView();
				WITH va: TextViews.View DO
					vb := wb.doc.ThisView();
					WITH vb: TextViews.View DO
						ca := va.ThisController();
						WITH ca: TextControllers.Controller DO
							cb := vb.ThisController();
							WITH cb: TextControllers.Controller DO
								ca.GetSelection(sa, ea);
								IF (* ea = -1 *) sa = ea THEN ea := MAX(0, ca.CaretPos()) END;
								cb.GetSelection(sb, eb);
								IF (* eb = -1 *) sb = eb THEN eb := MAX(0, cb.CaretPos()) END;
								CompTexts(va.ThisModel(), vb.ThisModel(), sa, sb, ea, eb);
								IF ea > sa THEN ca.SetSelection(sa, ea) ELSE ca.SetCaret(sa) END;
								IF eb > sb THEN cb.SetSelection(sb, eb) ELSE cb.SetCaret(sb) END;
								va.ShowRangeIn(Views.ThisFrame(wa.frame, va), sa, ea);
								vb.ShowRangeIn(Views.ThisFrame(wb.frame, vb), sb, eb)
							END
						END
					ELSE
					END
				ELSE
				END
			END
		END
	END Compare;

END DevSearch.
