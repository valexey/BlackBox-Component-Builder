MODULE DevRBrowser;
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
		Strings, Dialog, Files, Stores, Converters, Fonts, Ports, Views, Containers,
		TextModels, TextMappers, TextRulers, TextViews, StdLinks, StdFolds, TextControllers, Models;

	TYPE
		SubDesc = RECORD	(* description of subsystem contents *)
			rsrc: BOOLEAN;	(* does subsystem directory have a Rsrc subdirectory? *)
			sym, code, mod, docu: Files.FileInfo	(* file lists of Sym/Code/Mod/Docu subdirectories *)
		END;

	PROCEDURE Eq (a, b: CHAR): BOOLEAN;
	BEGIN
		IF (a >= "a") & (a <= "z") THEN a := CAP(a) END;
		IF (b >= "a") & (b <= "z") THEN b := CAP(b) END;
		RETURN a = b
	END Eq;

	PROCEDURE Gt (a, b: CHAR): BOOLEAN;
	BEGIN
		IF (a >= "a") & (a <= "z") THEN a := CAP(a) END;
		IF (b >= "a") & (b <= "z") THEN b := CAP(b) END;
		RETURN a > b
	END Gt;

	PROCEDURE ClipName (VAR s: ARRAY OF CHAR);
		VAR h, k: INTEGER; ch: CHAR;
	BEGIN	(* strip file name suffix *)
		IF (Dialog.platform DIV 10 = 1) OR (Dialog.platform = Dialog.linux) THEN	(* some Windows variant or Linux *)
			k := - 1; h := 0; ch := s[0];
			WHILE ch # 0X DO
				IF ch = "." THEN k := h END;
				INC(h); ch := s[h]
			END;
			IF k # - 1 THEN s[k] := 0X END
		END
	END ClipName;

	PROCEDURE Equal (IN a: ARRAY OF CHAR; b: ARRAY OF CHAR): BOOLEAN;
		VAR i: INTEGER; cha, chb: CHAR;
	BEGIN	(* string comparison, not case sensitive *)
		i := 0; cha := a[0]; chb := b[0];
		WHILE (cha # 0X) & (chb # 0X) & Eq(cha, chb) DO INC(i); cha := a[i]; chb := b[i] END;
		RETURN Eq(cha, chb)
	END Equal;

	PROCEDURE ClippedEqual (a: ARRAY OF CHAR; b: ARRAY OF CHAR): BOOLEAN;
		VAR i: INTEGER; cha, chb: CHAR;
	BEGIN	(* string comparison, not case sensitive *)
		ClipName(a); ClipName(b);
		i := 0; cha := a[0]; chb := b[0];
		WHILE (cha # 0X) & (chb # 0X) & Eq(cha, chb) DO INC(i); cha := a[i]; chb := b[i] END;
		RETURN Eq(cha, chb)
	END ClippedEqual;

	PROCEDURE ClippedGreater (a: ARRAY OF CHAR; b: ARRAY OF CHAR): BOOLEAN;
		VAR i: INTEGER; cha, chb: CHAR;
	BEGIN	(* string comparison, not case sensitive *)
		ClipName(a); ClipName(b);
		i := 0; cha := a[0]; chb := b[0];
		WHILE (cha # 0X) & (chb # 0X) & Eq(cha, chb) DO INC(i); cha := a[i]; chb := b[i] END;
		RETURN Gt(cha, chb)
	END ClippedGreater;

	PROCEDURE Ident (s: ARRAY OF CHAR): BOOLEAN;
		CONST MaxIdLen = 256;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		ClipName(s); i := 0;
		REPEAT
			ch := s[i]; INC(i)
		UNTIL (ch < "0")
					OR ("9" < ch) & (CAP(ch) < "A")
					OR ("Z" < CAP(ch)) & (ch # "_") & (ch < "À")
					OR (ch = "×")
					OR (ch = "÷")
					OR (i = MaxIdLen);
		RETURN ch = 0X
	END Ident;

	PROCEDURE WriteOpenFold (VAR f: TextMappers.Formatter; hidden: ARRAY OF CHAR);
		VAR fold: StdFolds.Fold; t: TextModels.Model; w: TextMappers.Formatter;
	BEGIN
		t := TextModels.CloneOf(f.rider.Base());
		w.ConnectTo(t); w.WriteString(hidden);
		fold := StdFolds.dir.New(StdFolds.expanded, "", t);
		f.WriteView(fold)
	END WriteOpenFold;

	PROCEDURE WriteCloseFold (VAR f: TextMappers.Formatter; collaps: BOOLEAN);
		VAR fold: StdFolds.Fold; m: TextModels.Model;
	BEGIN
		fold := StdFolds.dir.New(StdFolds.expanded, "", NIL);
		f.WriteView(fold);
		IF collaps THEN fold.Flip; m := f.rider.Base(); f.SetPos(m.Length()) END
	END WriteCloseFold;

	PROCEDURE WriteLink (VAR f: TextMappers.Formatter; subsystem, directory, file: ARRAY OF CHAR);
		VAR v: Views.View; s: Files.Name;
	BEGIN
		IF directory = "" THEN
			s := "StdCmds.OpenBrowser('";
			s := s + subsystem + "/Docu/";
			s := s + file + "', '";
			s := s + subsystem + "/Docu/";
			s := s + file + "')"
		ELSIF Equal(directory , "Rsrc") THEN
			s := "DevRBrowser.ShowFiles('";
			s := s + subsystem + "/Rsrc')"
		ELSE
			s := "StdCmds.OpenBrowser('";
			s := s + subsystem +  "/" + directory;
			s := s +  "/" + file + "', '";
			IF subsystem # "System" THEN s := s + subsystem END;
			s := s + file + "')"
		END;
		v := StdLinks.dir.NewLink(s);
		f.WriteView(v);	(* insert left link view in text *)
		IF directory # "" THEN s := directory$ ELSE s := file$; ClipName(s) END;
		f.WriteString(s);
		v := StdLinks.dir.NewLink("");
		f.WriteView(v)	(* insert right link view in text *)
	END WriteLink;

	PROCEDURE NewRuler (): TextRulers.Ruler;
		VAR p: TextRulers.Prop;
	BEGIN
		NEW(p);
		p.tabs.len := 5;
		p.tabs.tab[0].stop := 4 * Ports.mm;
		p.tabs.tab[1].stop := 50 * Ports.mm;
		p.tabs.tab[2].stop := 70 * Ports.mm;
		p.tabs.tab[3].stop := 90 * Ports.mm;
		p.tabs.tab[4].stop := 110 * Ports.mm;
		p.valid := {TextRulers.tabs};
		RETURN TextRulers.dir.NewFromProp(p)
	END NewRuler;

	PROCEDURE MSort (l: Files.FileInfo; n: INTEGER): Files.FileInfo;
		VAR h, h0, r: Files.FileInfo; n2, i: INTEGER;
	BEGIN
		IF n > 2 THEN
			n2 := n DIV 2; h := l; i := n2; WHILE i # 1 DO DEC(i); h := h.next END;
			r := h.next; h.next := NIL;	(* split list into two half-length lists*)
			l := MSort(l, n2); r := MSort(r, n - n2);	(* sort both lists separately *)
			IF ClippedGreater(r.name, l.name) THEN h := l; l := l.next ELSE h := r; r := r.next END;
			h0 := h;	(* h0 is back pointer of newly constructed list h *)
			WHILE (l # NIL) & (r # NIL) DO
				IF ClippedGreater(r.name, l.name) THEN h0.next := l; l := l.next ELSE h0.next := r; r := r.next END;
				h0 := h0.next
			END;
			IF l # NIL THEN h0.next := l ELSIF r # NIL THEN h0.next := r END;
			l := h
		ELSIF n = 2 THEN
			IF ClippedGreater(l.name, l.next.name) THEN l.next.next := l; l := l.next; l.next.next := NIL END
		END;
		RETURN l
	END MSort;

	PROCEDURE Sort (i: Files.FileInfo): Files.FileInfo;
		VAR n: INTEGER; h: Files.FileInfo;
	BEGIN	(* merge sort *)
		n := 0; h := i; WHILE h # NIL DO INC(n); h := h.next END;	(* count number of list elements *)
		RETURN MSort(i, n)
	END Sort;

	PROCEDURE GetThisSubsystem (loc: Files.Locator; VAR sub: SubDesc; VAR isSubsystem: BOOLEAN);
		VAR i: Files.LocInfo; l: Files.Locator;
	BEGIN
		isSubsystem := FALSE; sub.rsrc := FALSE;
		i := Files.dir.LocList(loc);
		WHILE i # NIL DO
			IF Equal(i.name, "Rsrc") THEN
				isSubsystem := TRUE; sub.rsrc := TRUE
			ELSIF Equal(i.name, "Sym") OR Equal(i.name, "Code") OR
					Equal(i.name, "Mod") OR Equal(i.name, "Docu") THEN
				isSubsystem := TRUE
			END;
			i := i.next
		END;
		sub.sym := Files.dir.FileList(loc.This("Sym"));
		sub.code := Files.dir.FileList(loc.This("Code"));
		sub.mod := Files.dir.FileList(loc.This("Mod"));
		sub.docu := Files.dir.FileList(loc.This("Docu"));
		sub.sym := Sort(sub.sym);
		sub.code := Sort(sub.code);
		sub.mod := Sort(sub.mod);
		sub.docu := Sort(sub.docu)
	END GetThisSubsystem;

	PROCEDURE Convertible (l: Files.FileInfo): BOOLEAN;
		VAR c: Converters.Converter;
	BEGIN
		c := Converters.list; WHILE (c # NIL) & (c.fileType # l.type) DO c := c.next END;
		RETURN c # NIL
	END Convertible;

	PROCEDURE GetModule (VAR sub: SubDesc; OUT i: Files.FileInfo);
	BEGIN
		i := sub.sym;
		IF (sub.code # NIL) & ((i = NIL) OR ClippedGreater(i.name, sub.code.name)) THEN i := sub.code END;
		IF (sub.mod # NIL) & ((i = NIL) OR ClippedGreater(i.name, sub.mod.name)) THEN i := sub.mod END;
		IF (sub.docu # NIL) & ((i = NIL) OR ClippedGreater(i.name, sub.docu.name)) THEN i := sub.docu END
	END GetModule;

	PROCEDURE GetNonmodule (VAR l: Files.FileInfo; OUT i: Files.FileInfo);
		VAR h: Files.FileInfo;
	BEGIN
		h := l;
		IF (h # NIL) & ~Ident(h.name) THEN
			i := l; l := l.next
		ELSIF h # NIL THEN
			WHILE (h.next # NIL) & Ident(h.next.name) DO h := h.next END;
			IF h.next # NIL THEN i := h.next; h.next := i.next ELSE i := NIL END
		ELSE
			i := NIL
		END
	END GetNonmodule;

	PROCEDURE WriteSubsystem (VAR f: TextMappers.Formatter; old, new, bold: TextModels.Attributes;
													VAR sub: SubDesc; name: Files.Name);
		VAR i: Files.FileInfo; modname: Files.Name;
	BEGIN
		f.rider.SetAttr(bold); f.WriteString(name); f.rider.SetAttr(old);
		f.WriteString("   ");
		WriteOpenFold(f, ""); f.WriteLn;
		f.rider.SetAttr(new);
		IF sub.rsrc THEN f.WriteTab; WriteLink(f, name, "Rsrc", ""); f.WriteLn END;
		GetNonmodule(sub.docu, i);
		IF i # NIL THEN
			f.WriteTab;
			REPEAT
				WriteLink(f, name, "", i.name); f.WriteString("   ");
				GetNonmodule(sub.docu, i)
			UNTIL i = NIL;
			f.WriteLn
		END;
		f.rider.SetAttr(old);
		GetModule(sub, i);
		WHILE i # NIL DO	(* iterate over all modules for which there is a symbol file *)
			f.WriteTab;
			IF ~Equal(name, "System") THEN f.WriteString(name) END;	(* subsystem name *)
			modname := i.name;
			ClipName(modname);	(* file name => module name *)
			f.WriteString(modname);
			f.rider.SetAttr(new);
			f.WriteTab;
			IF (sub.sym # NIL) & ClippedEqual(sub.sym.name, i.name) THEN
				IF Convertible(sub.sym) THEN WriteLink(f, name, "Sym", sub.sym.name) END;
				sub.sym := sub.sym.next
			END;
			f.WriteTab;
			IF (sub.code # NIL) & ClippedEqual(sub.code.name, i.name) THEN
				IF Convertible(sub.code) THEN WriteLink(f, name, "Code", sub.code.name) END;
				sub.code := sub.code.next
			END;
			f.WriteTab;
			IF (sub.mod # NIL) & ClippedEqual(sub.mod.name,  i.name) THEN
				IF Convertible(sub.mod) THEN WriteLink(f, name, "Mod", sub.mod.name) END;
				sub.mod := sub.mod.next
			END;
			f.WriteTab;
			IF (sub.docu # NIL) & ClippedEqual(sub.docu.name, i.name) THEN
				IF Convertible(sub.docu) THEN WriteLink(f, name, "Docu", sub.docu.name) END;
				sub.docu := sub.docu.next
			END;
			f.rider.SetAttr(old);
			f.WriteLn;
			GetModule(sub, i)
		END;
		WriteCloseFold(f, TRUE);
		f.WriteLn
	END WriteSubsystem;
	
	PROCEDURE AddFiles(VAR  t: TextModels.Model);
		VAR f: TextMappers.Formatter; tv: TextViews.View;
			c: Containers.Controller; old, new, bold: TextModels.Attributes; title: Views.Title;
			subinfo: Files.LocInfo; root, subloc: Files.Locator; subdesc: SubDesc; isSubsystem: BOOLEAN;
			v: Views.View;
	BEGIN
		f.ConnectTo(t);
		old := f.rider.attr;
		new := TextModels.NewStyle(old, old.font.style + {Fonts.underline});	(* use underline style *)
		new := TextModels.NewColor(new, Ports.blue);	(* use blue color *)
		bold := TextModels.NewWeight(old, Fonts.bold);	(* use bold outline *)
		f.WriteView(NewRuler());
		v := StdLinks.dir.NewLink("DevRBrowser.Update");
		f.rider.SetAttr(new);
		f.WriteView(v);	(* insert left link view in text *)
		f.WriteString("Update"); f.WriteLn;
		v := StdLinks.dir.NewLink("");
		f.WriteView(v);	(* insert right link view in text *)
		root := Files.dir.This("");
		subinfo := Files.dir.LocList(root);
		WHILE subinfo # NIL DO	(* iterate over all locations; no particular sorting order is guaranteed *)
			subloc := root.This(subinfo.name);
			IF subloc # NIL THEN
				GetThisSubsystem(subloc, subdesc, isSubsystem);
				IF isSubsystem THEN WriteSubsystem(f, old, new, bold, subdesc, subinfo.name) END
			END;
			subinfo := subinfo.next
		END
	END AddFiles;
	
	PROCEDURE ShowRepository*;
		VAR t: TextModels.Model; tv: TextViews.View;
			c: Containers.Controller; v: Views.View;
	BEGIN
		t := TextModels.dir.New();
		AddFiles(t);
		tv := TextViews.dir.New(t);
		c := tv.ThisController();
		(* set Browser mode: *)
		c.SetOpts(c.opts + {Containers.noCaret} - {Containers.noSelection, Containers.noFocus});
		Views.OpenAux(tv, "Repository")
	END ShowRepository;

	PROCEDURE Update*;
		VAR t, t0: TextModels.Model; v: Views.View; script: Stores.Operation;
	BEGIN
		t0 := TextViews.FocusText();
		Models.BeginScript(t0, "Update repository", script);
		t := TextModels.CloneOf(t0);
		AddFiles(t);
		t0.Delete(0, t0.Length());  t0.Insert(0, t, 0, t.Length());
		Models.EndScript(t0, script)
	END Update;

	PROCEDURE PathToLoc (path: ARRAY OF CHAR; VAR loc: Files.Locator);
		VAR i, j: INTEGER; ch: CHAR; name: Files.Name;
	BEGIN
		loc := Files.dir.This("");
		IF path # "" THEN
			i := 0; j := 0;
			REPEAT
				ch := path[i]; INC(i);
				IF (ch = "/") OR (ch = 0X) THEN name[j] := 0X; j := 0; loc := loc.This(name)
				ELSE name[j] := ch; INC(j)
				END
			UNTIL (ch = 0X) OR (loc.res # 0)
		END
	END PathToLoc;

	PROCEDURE ShowFiles* (path: ARRAY OF CHAR);
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: Views.View; tv: TextViews.View;
			c: Containers.Controller; old, new: TextModels.Attributes; conv: Converters.Converter;
			loc: Files.Locator; fi: Files.FileInfo; s: Files.Name;
	BEGIN
		t := TextModels.dir.New();
		f.ConnectTo(t);
		old := f.rider.attr;	(* save old text attributes for later use *)
		new := TextModels.NewStyle(old, old.font.style + {Fonts.underline});	(* use underline style *)
		new := TextModels.NewColor(new, Ports.blue);	(* use blue color *)
		f.rider.SetAttr(new);	(* change current attributes of formatter *)
		(* generate list of all locations *)
		PathToLoc(path, loc);
		fi := Files.dir.FileList(loc);
		fi := Sort(fi);
		WHILE fi # NIL DO	(* no particular sorting order is guaranteed *)
			conv := Converters.list; WHILE (conv # NIL) & (conv.fileType # fi.type) DO conv := conv.next END;
			IF conv # NIL THEN	(* there is a converter for this file type *)
				s := "DevRBrowser.OpenFile('";
				s := s + path + "', '" + fi.name + "')";
				v := StdLinks.dir.NewLink(s);
				f.WriteView(v);	(* insert left link view in text *)
				s := fi.name$; ClipName(s); f.WriteString(fi.name);
				v := StdLinks.dir.NewLink("");
				f.WriteView(v);	(* insert right link view in text *)
				f.WriteLn
			END;
			fi := fi.next
		END;
		tv := TextViews.dir.New(t);
		c := tv.ThisController();
		(* set Browser mode: *)
		c.SetOpts(c.opts + {Containers.noCaret} - {Containers.noSelection, Containers.noFocus});
		Views.OpenAux(tv, path$)
	END ShowFiles;

	PROCEDURE OpenFile* (path, name: ARRAY OF CHAR);
		VAR loc: Files.Locator; f: Files.File; c: Converters.Converter; n: Files.Name; s: Stores.Store;
	BEGIN
		PathToLoc(path, loc); n := name$;
		IF loc # NIL THEN
			f := Files.dir.Old(loc, n, Files.shared);
			IF f # NIL THEN
				c := Converters.list; WHILE (c # NIL) & (c.fileType # f.type) DO c := c.next END;
				IF c # NIL THEN
					Converters.Import(loc, n, c, s);
					WITH s: Views.View DO
						Views.Open(s, loc, n, c)
					ELSE
					END
				END
			END
		END
	END OpenFile;

END DevRBrowser.
