MODULE StdMenuTool;
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
		Kernel, Files, Fonts, Ports, Models, Views, Dialog, Properties, Containers, Documents,
		HostMenus, StdLinks, StdCmds,
		TextModels, TextMappers, TextViews, TextControllers;

	CONST
		char = TextMappers.char; string = TextMappers.string; keyword = 100;
		menuFile = "Menus"; rsrcDir = "Rsrc"; sysDir = "System";
		
	TYPE		
		LangNotifier = POINTER TO RECORD (Dialog.LangNotifier) END;

	VAR
		noerr, showerr, gen: BOOLEAN;
		includes: Files.LocInfo;
		langNotifier: LangNotifier;
		
	PROCEDURE Scan (VAR s: TextMappers.Scanner);
		VAR ch: CHAR; p: INTEGER;
	BEGIN
		s.Scan;
		IF s.type = string THEN
			p := s.rider.Pos() - 1;
			IF ~s.rider.eot THEN DEC(p) END;
			s.rider.SetPos(p); s.rider.ReadChar(ch); s.rider.Read;
			IF ch # '"' THEN s.type := keyword END
		END
	END Scan;
	
	PROCEDURE Comp (IN s1, s2: ARRAY OF CHAR): INTEGER;
		VAR i: INTEGER; a, b: CHAR;
	BEGIN
		i := 0; a := s1[0]; b := s2[0];
		WHILE (a # 0X) & ((a = b) OR (a >= "A") & (a <= "Z") & (a = CAP(b)) OR (b >= "A") & (b <= "Z") & (b = CAP(a))) DO
			INC(i); a := s1[i]; b := s2[i]
		END;
		RETURN ORD(a) - ORD(b)
	END Comp;
	
	PROCEDURE Sort (VAR list: Files.LocInfo);
		VAR inc, last, i1, i2: Files.LocInfo;
	BEGIN
		inc := list; list := NIL;
		WHILE inc # NIL DO
			i1 := inc; inc := inc.next;
			i2 := list; last := NIL;
			WHILE (i2 # NIL) & (Comp(i1.name, i2.name) > 0) DO last := i2; i2 := i2.next END;
			IF last = NIL THEN i1.next := list; list := i1 ELSE i1.next := last.next; last.next := i1 END
		END
	END Sort;


	PROCEDURE ParseMenus (VAR s: TextMappers.Scanner; view: Views.View; loc: Files.Locator; name: Files.Name);
		VAR menu, category: Dialog.String; n: INTEGER;

		PROCEDURE Error (VAR s: TextMappers.Scanner; err: ARRAY OF CHAR);
			VAR end: INTEGER;
		BEGIN
			IF noerr & showerr THEN
				IF loc # NIL THEN Views.Open(view, loc, name, NIL) END;
				end := MAX(s.rider.Pos() - 1, s.start + 1);
				end := MIN(end, s.rider.Base().Length());
				TextControllers.SetSelection(s.rider.Base(), s.start, end);
				TextViews.ShowRange(s.rider.Base(), s.start, end, TextViews.focusOnly);
				Dialog.ShowMsg(err)
			END; 
			noerr := FALSE
		END Error;

		PROCEDURE Category (VAR s: TextMappers.Scanner; VAR c: ARRAY OF CHAR);
		BEGIN
			IF (s.type = char) & (s.char = "(") THEN
				Scan(s); IF s.type # string THEN Error(s, "string expected") END;
				c := s.string$;
				Scan(s);
				IF (s.type # char) OR (s.char # ")") THEN Error(s, ") expected") END;
				Scan(s)
			ELSE c := ""
			END
		END Category;

		PROCEDURE Item (VAR s: TextMappers.Scanner);
			VAR item, str, shortcut, filter: Dialog.String;
		BEGIN
			IF s.type = keyword THEN
				IF gen THEN HostMenus.AddSeparator END;
				Scan(s)
			ELSE
				IF s.len < LEN(item) THEN item := s.string$
				ELSE item := ""; Error(s, "string too long")
				END;
				IF item = "" THEN Error(s, "nonempty string expected") END;
				Scan(s);
				shortcut := "";
				IF s.type = string THEN
					IF s.len < 8 THEN shortcut := s.string$
					ELSE Error(s, "string too long")
					END
				ELSE Error(s, "string expected")
				END;
				Scan(s); IF s.type # string THEN Error(s, "string expected") END;
				IF s.len < LEN(str) THEN str := s.string$
				ELSE str := ""; Error(s, "string too long")
				END;
				IF str = "" THEN Error(s, "nonempty string expected") END;
				Scan(s); IF s.type # string THEN Error(s, "string expected") END;
				IF s.len < LEN(str) THEN filter := s.string$
				ELSE filter := ""; Error(s, "string too long")
				END;
				IF gen THEN HostMenus.AddItem(item, str, shortcut, filter) END;
				Scan(s)
			END
		END Item;
		
		PROCEDURE IncludeSub (sub: ARRAY OF CHAR);
			VAR loc: Files.Locator; view: Views.View; t: TextModels.Model; s: TextMappers.Scanner;
		BEGIN
			loc := Files.dir.This(sub); IF loc = NIL THEN RETURN END;
			loc := loc.This(rsrcDir); IF loc = NIL THEN RETURN END;
			view := Views.OldView(loc, menuFile);
			IF (view # NIL) & (view IS TextViews.View) THEN
				t := view(TextViews.View).ThisModel();
				IF t # NIL THEN s.ConnectTo(t); Scan(s); ParseMenus(s, view, loc, menuFile) END
			END
		END IncludeSub;
		
		PROCEDURE Include (sub: ARRAY OF CHAR);
			VAR inc, last: Files.LocInfo;
		BEGIN
			IF sub = "*" THEN	(* wildcard include *)
				IF ~gen THEN	(* first pass: generate complete list *)
					IF includes # NIL THEN Error(s, "only one wildcard include allowed") END;
					includes := Files.dir.LocList(Files.dir.This(""))					
				ELSE	(* second pass: sort reduced list *)
					Sort(includes)
				END;
				inc := includes;
				WHILE (inc # NIL) & noerr DO
					IF Comp(inc.name, sysDir) # 0 THEN IncludeSub(inc.name) END;
					inc := inc.next
				END
			ELSE	(* spedific includes *)
				IncludeSub(sub);
				inc := includes; last := NIL;
				WHILE (inc # NIL) & (Comp(inc.name, sub) # 0) DO last := inc; inc := inc.next END;
				IF inc # NIL THEN	(* remove from wilcard list *)
					IF last = NIL THEN includes := inc.next ELSE last.next := inc.next END
				END
			END
		END Include;
		
	BEGIN
		n := 0;
		WHILE noerr & (s.type = keyword) & ((s.string = "MENU") OR (s.string = "INCLUDE")) DO
			IF s.string = "INCLUDE" THEN
				Scan(s);
				IF s.type # string THEN Error(s, "string expected") END;
				Include(s.string);
				Scan(s);
				INC(n)
			ELSE
				INC(n); Scan(s);
				IF s.type # string THEN Error(s, "string expected") END;
				menu := s.string$;
				IF menu = "" THEN Error(s, "nonempty string expected") END;
				Scan(s);
				Category(s, category);
				IF gen THEN HostMenus.Open(menu, category) END;
				WHILE noerr & ((s.type = string) OR (s.type = keyword) & (s.string = "SEPARATOR")) DO
					Item(s)
				END;
				IF (s.type # keyword) OR (s.string # "END") THEN Error(s, "END expected") END;
				IF gen THEN HostMenus.Close END;
				Scan(s)
			END
		END;
		IF (s.type # TextMappers.eot) OR (n = 0) THEN Error(s, "MENU expected") END;
	END ParseMenus;
	
	
	PROCEDURE InitNotifier;
	BEGIN
		IF langNotifier = NIL THEN
			NEW(langNotifier); Dialog.RegisterLangNotifier(langNotifier)
		END
	END InitNotifier;

	PROCEDURE UpdateFromText* (text: TextModels.Model);
		VAR s: TextMappers.Scanner;
	BEGIN
		InitNotifier;
		ASSERT(text # NIL, 20);
		s.ConnectTo(text); s.SetPos(0);
		Scan(s); 
		noerr := TRUE; showerr := FALSE; gen := FALSE; ParseMenus(s, NIL, NIL, "");
		IF noerr THEN
			s.SetPos(0); Scan(s); gen := TRUE;
			HostMenus.DeleteAll; ParseMenus(s, NIL, NIL, ""); HostMenus.InitMenus
		END;
		includes := NIL
	END UpdateFromText;

	PROCEDURE UpdateMenus*;
		VAR t: TextModels.Model; s: TextMappers.Scanner;
	BEGIN
		InitNotifier;
		t := TextViews.FocusText();
		IF t # NIL THEN
			s.ConnectTo(t); s.SetPos(0); Scan(s); 
			noerr := TRUE; showerr := TRUE; gen := FALSE; ParseMenus(s, NIL, NIL, "");
			IF noerr THEN
				s.SetPos(0); Scan(s); gen := TRUE;
				HostMenus.DeleteAll; ParseMenus(s, NIL, NIL, ""); HostMenus.InitMenus
			END
		END;
		includes := NIL
	END UpdateMenus;

	PROCEDURE UpdateAllMenus*;
		VAR view: Views.View; t: TextModels.Model; s: TextMappers.Scanner;
			loc: Files.Locator;
	BEGIN
		InitNotifier;
		loc := Files.dir.This(sysDir); IF loc = NIL THEN RETURN END;
		loc := loc.This(rsrcDir); IF loc = NIL THEN RETURN END;
		view := Views.OldView(loc, menuFile);
		IF (view # NIL) & (view IS TextViews.View) THEN
			t := view(TextViews.View).ThisModel();
			IF t # NIL THEN
				s.ConnectTo(t); Scan(s);
				noerr := TRUE; showerr := TRUE; gen := FALSE; ParseMenus(s, view, loc, menuFile);
				IF noerr THEN
					s.SetPos(0); Scan(s); gen := TRUE;
					HostMenus.DeleteAll; ParseMenus(s, NIL, NIL, ""); HostMenus.InitMenus
				ELSE
					Dialog.ShowMsg("errors detected in menu file");
				END
			END
		ELSE Dialog.ShowMsg("cannot open menu file")
		END;
		includes := NIL
	END UpdateAllMenus;


	PROCEDURE InsertLink (VAR w: TextMappers.Formatter; path: ARRAY OF CHAR);
		VAR a0: TextModels.Attributes; cmd: ARRAY 256 OF CHAR;
	BEGIN
		a0 := w.rider.attr;
		w.rider.SetAttr(TextModels.NewStyle(w.rider.attr, {Fonts.underline}));
		w.rider.SetAttr(TextModels.NewColor(w.rider.attr, Ports.blue));
		cmd := "StdCmds.OpenDoc('" + path + "')";
		w.WriteView(StdLinks.dir.NewLink(cmd));
		w.WriteString(path);
		w.WriteView(StdLinks.dir.NewLink(""));
		w.rider.SetAttr(a0);
	END InsertLink;
	
	PROCEDURE ListAllMenus*;
		VAR sub: Files.LocInfo; loc: Files.Locator; f: Files.File; t: TextModels.Model; w: TextMappers.Formatter;
			path: Files.Name; v: Views.View; c: Containers.Controller; p: Properties.BoundsPref;
	BEGIN
		t := TextModels.dir.New(); w.ConnectTo(t);
		w.WriteString("Menu Files:"); w.WriteLn; w.WriteLn;
		path := sysDir + "/" + rsrcDir + "/" + menuFile;
		InsertLink(w, path); w.WriteLn; w.WriteLn;
		sub := Files.dir.LocList(Files.dir.This(""));
		Sort(sub);
		WHILE sub # NIL DO
			IF Comp(sub.name, sysDir) # 0 THEN
				loc := Files.dir.This(sub.name);
				loc := loc.This(rsrcDir);
				IF loc # NIL THEN
					path := menuFile;
					Kernel.MakeFileName(path, "");
					f := Files.dir.Old(loc, path, Files.shared);
					IF f # NIL THEN
						path := sub.name + "/" + rsrcDir + "/" + menuFile;
						InsertLink(w, path); w.WriteLn;
					END
				END
			END;
			sub := sub.next
		END;
		v := TextViews.dir.New(t);
		c := v(Containers.View).ThisController();
		c.SetOpts(c.opts + {Containers.noCaret});
		p.w := Views.undefined; p.h := Views.undefined; Views.HandlePropMsg(v, p);
		v := Documents.dir.New(v, p.w, p.h);
		Views.OpenAux(v, "All Menus")
	END ListAllMenus;
	
	PROCEDURE ThisMenu*;
		VAR s: TextMappers.Scanner; c: Models.Context; v: Views.View; name: ARRAY 256 OF CHAR;
	BEGIN
		IF StdLinks.par # NIL THEN
			c := StdLinks.par.context;
			WITH c: TextModels.Context DO
				s.ConnectTo(c.ThisModel()); s.SetPos(c.Pos() + 1);
				s.rider.ReadView(v);	(* right link view *)
				s.Scan;
				IF s.type = string THEN
					IF s.string = "*" THEN ListAllMenus
					ELSE
						name := s.string + "/" + rsrcDir + "/" + menuFile;
						StdCmds.OpenDoc(name)
					END
				END
			ELSE
			END
		END
	END ThisMenu;	

	PROCEDURE (n: LangNotifier) Notify;
	BEGIN
		UpdateAllMenus
	END Notify;

END StdMenuTool.
