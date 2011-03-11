MODULE DevLinkChk;
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
		Kernel, Strings, Dialog, Files, Fonts, Ports, Converters, Views, Containers,
		TextModels, TextMappers, TextViews, TextControllers, StdLinks, StdCmds;

	CONST
		oneSubsystem* = 0; globalSubsystem* = 1; allSubsystems* = 2;
		linkCommand = "DevLinkChk.Open('";

	VAR
		par*: RECORD
			scope*: INTEGER;	(* IN {oneSubsystem, globalSubsystem, allSubsystems} *)
			subsystem*: ARRAY 9 OF CHAR;
			legal: BOOLEAN	(* legal => correct syntax for a subsystem name # "" *)
		END;


	TYPE
		Iterator = POINTER TO IteratorDesc;
		IteratorDesc = RECORD
			root: Files.Locator;
			locs: Files.LocInfo;
			files: Files.FileInfo
		END;

	VAR default, link: TextModels.Attributes;

	PROCEDURE MakeDocName (VAR name: Files.Name);
	BEGIN
		Kernel.MakeFileName(name, "")
	END MakeDocName;

	PROCEDURE New (root: Files.Locator): Iterator;
		VAR i: Iterator;
	BEGIN
		NEW(i); i.root := root; i.locs := Files.dir.LocList(root); i.files := Files.dir.FileList(root);
		(* could sort by name *)
		RETURN i
	END New;

	PROCEDURE ReadLoc (i: Iterator; VAR loc: Files.Locator; VAR name: Files.Name);
	BEGIN
		IF i.locs # NIL THEN
			loc := i.root.This(i.locs.name); ASSERT(loc # NIL, 60);
			name := i.locs.name$;
			i.locs := i.locs.next
		ELSE
			loc := NIL; name := ""
		END
	END ReadLoc;

	PROCEDURE ReadFile (i: Iterator; OUT name: Files.Name);
	BEGIN
		IF i.files # NIL THEN
			name := i.files.name$;
			i.files := i.files.next
		ELSE
			name := ""
		END
	END ReadFile;

	PROCEDURE Delimiter (ch: CHAR): BOOLEAN;
	BEGIN
		RETURN (ch = "'") OR (ch = '"') OR (ch = "/")
	END Delimiter;

	PROCEDURE Stale (IN s: ARRAY OF CHAR): BOOLEAN;
		VAR i, j: INTEGER; ch: CHAR; loc: Files.Locator; name: Files.Name; t: ARRAY 1024 OF CHAR;
	BEGIN
		i := 0; ch := s[0]; WHILE (ch # 0X) & (ch # "(") DO INC(i); ch := s[i] END;
		IF ch = "(" THEN
			t := s$; t[i] := 0X;
			(* StdCmds.OpenMask doesn't exist anymore *)
			IF (t = "StdCmds.OpenMask") OR (t = "StdCmds.OpenBrowser") OR (t = "StdCmds.OpenDoc") OR
				(t = "StdCmds.OpenAuxDialog") THEN
				INC(i); ch := s[i]; WHILE (ch # 0X) & (ch # "'") & (ch # '"') DO INC(i); ch := s[i] END;
				IF ch # 0X THEN
					loc := Files.dir.This("");
					REPEAT
						j := 0;
						INC(i); ch := s[i]; WHILE ~Delimiter(ch) DO t[j] := ch; INC(j); INC(i); ch := s[i] END;
						t[j] := 0X;
						IF ch = "/" THEN loc := loc.This(t) END
					UNTIL ch # "/";
					name := t$; MakeDocName(name);
					RETURN Files.dir.Old(loc, name, Files.shared) = NIL	(* file not found *)
				ELSE RETURN TRUE	(* wrong syntax *)
				END
			ELSE RETURN FALSE	(* unknown kind of command *)
			END
		ELSE RETURN FALSE	(* unknown kind of command *)
		END
	END Stale;

	PROCEDURE GetCmd (IN path, file: Files.Name; pos: INTEGER; OUT cmd: ARRAY OF CHAR);
		VAR p, bug0, bug1, bug2: ARRAY 128 OF CHAR;
	BEGIN
		Strings.IntToString(pos, p);
		bug0 :=  linkCommand + path + "', '";
		bug1 :=  bug0 + file + "', ";
		bug2 := bug1 + p + ")";
		cmd := bug2$
	END GetCmd;

	PROCEDURE CheckDoc (IN path, file: Files.Name; t: TextModels.Model; VAR f: TextMappers.Formatter;
										tabs: INTEGER; check: BOOLEAN; VAR hit: BOOLEAN);
		VAR r: TextModels.Reader; v: Views.View; leftSide, done: BOOLEAN; cmd, cmd0: ARRAY 1024 OF CHAR;
			i: INTEGER;
	BEGIN
		r := t.NewReader(NIL);
		r.ReadView(v);
		WHILE v # NIL DO
			WITH v: StdLinks.Link DO
				IF v.leftSide THEN
					v.GetCmd(cmd);
					IF (cmd # "") & (~check OR Stale(cmd)) THEN
						hit := TRUE;
						i := 0; WHILE i # tabs DO f.WriteTab; INC(i) END;
						GetCmd(path, file, r.Pos() - 1, cmd0);
						f.WriteView(StdLinks.dir.NewLink(cmd0));
						f.rider.SetAttr(link);
						f.WriteString(cmd);
						f.rider.SetAttr(default);
						f.WriteView(StdLinks.dir.NewLink(""));
						f.WriteLn
					END
				END
			ELSE
			END;
			r.ReadView(v)
		END
	END CheckDoc;

	PROCEDURE CheckLoc (
		IN path: Files.Name; loc: Files.Locator; name: Files.Name; VAR f: TextMappers.Formatter;
		check: BOOLEAN; OUT hit: BOOLEAN
	);
		VAR i: Iterator; fname: Files.Name; v: Views.View; conv: Converters.Converter;
			label, label0: INTEGER; hit0: BOOLEAN;
	BEGIN
		label := f.Pos(); hit := FALSE;
		loc := loc.This(name);
		i := New(loc);
		ReadFile(i, fname);
		IF fname # "" THEN f.WriteTab; f.WriteString(name); f.WriteLn END;
		hit := FALSE;
		WHILE fname # "" DO
			label0 := f.Pos(); hit0 := FALSE;
			MakeDocName(fname);
			v := Views.Old(Views.dontAsk, loc, fname, conv);
			IF (v # NIL) & (v IS TextViews.View) THEN
				f.WriteTab; f.WriteTab; f.WriteString(fname); f.WriteLn;
				CheckDoc(path, fname, v(TextViews.View).ThisModel(), f, 3, check, hit0);
				IF ~hit0 THEN f.SetPos(label0) END;
				hit := hit OR hit0
			END;
			ReadFile(i, fname)
		END;
		IF ~hit THEN f.SetPos(label) END
	END CheckLoc;

	PROCEDURE Equal (IN a, b: ARRAY OF CHAR): BOOLEAN;
		VAR i: INTEGER; ai, bi: CHAR;

		PROCEDURE Eq (a, b: CHAR): BOOLEAN;
		BEGIN
			RETURN (a = b) OR (a >= "A") & (a <= "Z") & (a = CAP(b)) OR (b >= "A") & (b <= "Z") & (b = CAP(a))
		END Eq;

	BEGIN
		i := 0; ai := a[0]; bi := b[0];
		WHILE Eq(ai, bi) & (ai # 0X) DO INC(i); ai := a[i]; bi := b[i] END;
		RETURN ai = bi
	END Equal;

	PROCEDURE Check* (subsystem: ARRAY OF CHAR; scope: INTEGER; check: BOOLEAN);
		VAR i: Iterator; root, loc: Files.Locator; name: Files.Name; hit0, hit1: BOOLEAN;
			out: TextModels.Model; f: TextMappers.Formatter; label: INTEGER; title: Views.Title;
			v: TextViews.View; c: Containers.Controller; path: Files.Name;
	BEGIN
		out := TextModels.dir.New(); f.ConnectTo(out);
		IF check THEN
			Dialog.MapString("#Dev:InconsistentLinks", title)
		ELSE
			Dialog.MapString("#Dev:Links", title)
		END;
		v := TextViews.dir.New(out);
		(* set Browser mode: *)
		c := v.ThisController();
		c.SetOpts(c.opts + {Containers.noCaret} - {Containers.noSelection, Containers.noFocus});
		Views.OpenAux(v, title);
		default := TextModels.dir.attr;
		link := TextModels.NewColor(default, Ports.blue); link := TextModels.NewStyle(link, {Fonts.underline});
		root := Files.dir.This("");
		IF scope IN {globalSubsystem, allSubsystems} THEN
			label := f.Pos();
			Dialog.ShowStatus(".");
			f.WriteString("."); f.WriteLn;
			path := "Docu";
			CheckLoc(path, root, "Docu", f, check, hit0);
			path := "Mod";
			CheckLoc(path, root, "Mod", f, check, hit1);
			IF ~hit0 & ~hit1 THEN f.SetPos(label) END
		END;
		i := New(root);
		ReadLoc(i, loc, name);
		WHILE loc # NIL DO
			IF (scope = allSubsystems) OR (scope = oneSubsystem) & Equal(name, subsystem) THEN
				label := f.Pos();
				Dialog.ShowStatus(name);
				f.WriteString(name); f.WriteLn;
				path := name + "/" + "Docu";
				CheckLoc(path, loc, "Docu", f, check, hit0);
				path := name + "/" + "Mod";
				CheckLoc(path, loc, "Mod", f, check, hit1);
				IF ~hit0 & ~hit1 THEN f.SetPos(label) END
			END;
			ReadLoc(i, loc, name)
		END;
		out.Delete(f.Pos(), out.Length());
		Dialog.ShowStatus(""); default := NIL; link := NIL
	END Check;

	PROCEDURE PathToLoc (IN path: ARRAY OF CHAR; OUT loc: Files.Locator);
		VAR i, j: INTEGER; ch: CHAR; name: ARRAY 256 OF CHAR;
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

	PROCEDURE Open* (path, file: ARRAY OF CHAR; pos: INTEGER);
		VAR loc: Files.Locator; v: Views.View; bug: Files.Name; c: TextControllers.Controller;
	BEGIN
		ASSERT(file # "", 20); ASSERT(pos >= 0, 21);
		PathToLoc(path, loc); ASSERT(loc # NIL, 22);
		bug := file$;
		v := Views.OldView(loc, bug);
		IF v # NIL THEN
			WITH v: TextViews.View DO
(*
				v.DisplayMarks(TextViews.show);
*)
				Views.Open(v, loc, bug, NIL);
				c := v.ThisController()(TextControllers.Controller);
				c.SetCaret(pos);
				v.ShowRange(pos, pos + 1, TextViews.focusOnly)
			ELSE
				HALT(23)
			END
		END
	END Open;

	PROCEDURE ListLinks*;
	(** Guard: CommandGuard **)
	BEGIN
		StdCmds.CloseDialog; Check(par.subsystem, par.scope, FALSE)
	END ListLinks;

	PROCEDURE CheckLinks*;
	(** Guard: CommandGuard **)
	BEGIN
		StdCmds.CloseDialog; Check(par.subsystem, par.scope, TRUE)
	END CheckLinks;

	PROCEDURE SubsystemGuard* (VAR p: Dialog.Par);
	BEGIN
		p.readOnly := par.scope # oneSubsystem
	END SubsystemGuard;

	PROCEDURE CommandGuard* (VAR p: Dialog.Par);
	BEGIN
		p.disabled := (par.scope = oneSubsystem) & ~par.legal
	END CommandGuard;

	PROCEDURE SyntaxOK (VAR s: ARRAY OF CHAR): BOOLEAN;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := s[0];
		WHILE (ch >= "A") & (ch <= "Z") DO INC(i); ch := s[i] END;
		WHILE (i # 0) & (ch >= "a") & (ch <= "z") OR (ch >= "0") & (ch <= "9") DO INC(i); ch := s[i] END;
		RETURN (i >= 3) & (i <= 8) & (ch = 0X)
	END SyntaxOK;

	PROCEDURE SubsystemNotifier* (op, from, to: INTEGER);
	BEGIN
		IF par.scope = oneSubsystem THEN
			par.legal := SyntaxOK(par.subsystem)
		ELSE
			par.subsystem := ""; par.legal := FALSE
		END
	END SubsystemNotifier;

END DevLinkChk.
