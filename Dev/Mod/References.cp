MODULE DevReferences;
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
		Kernel, Files, Fonts, Models, Views, Dialog, Containers,
		StdDialog,
		TextModels, TextMappers, TextViews, TextControllers;
	
	CONST
		(* additional scanner types *)
		import = 100; module = 101; semicolon = 102; becomes  = 103; comEnd = 104;


	PROCEDURE SpecialFont (r: TextModels.Reader): BOOLEAN;
		CONST max = 14 * Fonts.point;
	BEGIN
		RETURN (r.attr.font.weight > Fonts.normal) & (r.attr.font.size <= max)
	END SpecialFont;
	
	PROCEDURE SearchIdent (
		t: TextModels.Model; ident: TextMappers.String; VAR beg, end: INTEGER; bold: BOOLEAN
	);
		VAR r: TextModels.Reader; found: BOOLEAN; ch: CHAR; len, i: INTEGER;
	BEGIN
		end := 0;
		IF ident # "" THEN
			len := 0; WHILE ident[len] # 0X DO INC(len) END;
			r := t.NewReader(NIL); found := FALSE;
			beg := 0; r.SetPos(0); r.ReadChar(ch);
			WHILE ~r.eot & ~found DO
				(* search legal start symbol *)
				WHILE ~r.eot & ((ch < "A") OR (ch > "Z") & (ch # "_") & (ch < "a") OR (ch > "z")) DO
					INC(beg); r.ReadChar(ch)
				END;
				i := 0;
				WHILE ~r.eot & (~bold OR SpecialFont(r)) & (i # len) & (ch = ident[i]) DO
					r.ReadChar(ch); INC(i)
				END;
				found := (i = len) &
								((ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "Z") & (ch # "_") & (ch < "a") OR (ch > "z"));
				IF ~r.eot & ~found THEN	(* skip rest of identifier *)
					beg := r.Pos() - 1;
					WHILE ~r.eot &
						~((ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "Z") & (ch # "_") & (ch < "a") OR (ch > "z")) DO
						INC(beg); r.ReadChar(ch)
					END
				END
			END;
			IF found THEN end := r.Pos() - 1 END
		END
	END SearchIdent;

	PROCEDURE ShowText* (module, ident: TextMappers.String; category: Files.Name);
		VAR title: Views.Title; loc: Files.Locator; name: Files.Name;
			v: Views.View; m: Models.Model; t: TextModels.Model; beg, end: INTEGER; c: Containers.Controller;
	BEGIN
		StdDialog.GetSubLoc(module, category, loc, name); title := name$;
		Kernel.MakeFileName(name, "");
		IF category = "Docu" THEN
			loc.res := 77; v := Views.OldView(loc, name); loc.res := 0;
			IF v # NIL THEN
				WITH v: Containers.View DO
					c := v.ThisController();
					IF c # NIL THEN
						c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
					END
				ELSE
				END;
				Views.OpenAux(v, title)
			END
		ELSE
			v := Views.OldView(loc, name);
			IF v # NIL THEN Views.Open(v, loc, name, NIL) END
		END;
		IF v # NIL THEN
			m := v.ThisModel();
			IF (m # NIL) & (m IS TextModels.Model) THEN
				t := m(TextModels.Model);
				SearchIdent(t, ident, beg, end, TRUE);
				IF end <= 0 THEN SearchIdent(t, ident, beg, end, FALSE) END;
				IF end > 0 THEN
					TextViews.ShowRange(t, beg, end, TextViews.any);
					TextControllers.SetSelection(t, beg, end)
				ELSIF ident # "" THEN Dialog.Beep
				END
			ELSE Dialog.ShowMsg("#System:WrongSelection")
			END
		ELSE Dialog.ShowParamMsg("#System:FileNotFound", name, "", "")
		END
	END ShowText;
	
	PROCEDURE Scan (VAR s: TextMappers.Scanner);
	BEGIN
		s.Scan;
		IF s.type = TextMappers.string THEN
			IF s.string = "IMPORT" THEN s.type := import
			ELSIF s.string = "MODULE" THEN s.type := module
			END
		ELSIF s.type = TextMappers.char THEN
			IF s.char = ";" THEN s.type := semicolon
			ELSIF s.char = ":" THEN
				IF s.rider.char = "=" THEN s.rider.Read; s.type := becomes END
			ELSIF s.char = "(" THEN
				IF s.rider.char = "*" THEN
					s.rider.Read;
					REPEAT Scan(s) UNTIL (s.type = TextMappers.eot) OR (s.type = comEnd);
					Scan(s)
				END
			ELSIF s.char = "*" THEN
				IF s.rider.char = ")" THEN s.rider.Read; s.type := comEnd END
			END
		END	
	END Scan;
	
	PROCEDURE CheckModName (VAR mod: TextMappers.String; t: TextModels.Model);
		VAR s: TextMappers.Scanner;
	BEGIN
		s.ConnectTo(t); s.SetPos(0); Scan(s);
		IF s.type = module THEN
			Scan(s);
			WHILE (s.type # TextMappers.eot) & (s.type # import) DO Scan(s) END;
			WHILE (s.type # TextMappers.eot) & (s.type # semicolon)
					& ((s.type # TextMappers.string) OR (s.string # mod)) DO Scan(s) END;
			IF s.type = TextMappers.string THEN
				Scan(s);
				IF s.type = becomes THEN
					Scan(s);
					IF s.type = TextMappers.string THEN
						mod := SHORT(s.string$)
					END
				END
			END
		END
	END CheckModName;

	PROCEDURE Show (category: Files.Name);
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end: INTEGER;
			module, ident: TextMappers.String;
	BEGIN
		c := TextControllers.Focus();
		IF c # NIL THEN
			IF c.HasSelection() THEN
				c.GetSelection(beg, end);
				s.ConnectTo(c.text); s.SetOpts({7});	(* acceptUnderscores *)
				s.SetPos(beg); s.Scan;
				IF s.type = TextMappers.string THEN
					module := s.string; ident := "";
					CheckModName(module, c.text);
					s.Scan;
					IF (s.type = TextMappers.char) & (s.char = ".") THEN
						s.Scan;
						IF s.type = TextMappers.string THEN ident := s.string END
					END;
					ShowText(module, ident, category)
				ELSE Dialog.ShowMsg("#System:WrongSelection")
				END
			ELSE Dialog.ShowMsg("#System:NoSelection")
			END
		ELSE Dialog.ShowMsg("#System:NoFocus")
		END
	END Show;

	PROCEDURE ShowSource*;
	BEGIN
		Show("Mod")
	END ShowSource;

	PROCEDURE ShowDocu*;
	BEGIN
		Show("Docu")
	END ShowDocu;

END DevReferences.

 DevReferences.Show

Files.Directory
