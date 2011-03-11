MODULE ObxLinks;
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
		Files, Stores, Converters, Fonts, Ports, Views,
		TextModels, TextMappers, TextViews, StdLinks;

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

	PROCEDURE Directory* (path: ARRAY OF CHAR);
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: Views.View; tv: TextViews.View;
			old, new: TextModels.Attributes; conv: Converters.Converter;
			loc: Files.Locator; li: Files.LocInfo; fi: Files.FileInfo; str: ARRAY 256 OF CHAR; title: Views.Title;
	BEGIN
		t := TextModels.dir.New();
		f.ConnectTo(t);
		f.WriteString("Directories"); f.WriteLn;
		old := f.rider.attr;	(* save old text attributes for later use *)
		new := TextModels.NewStyle(old, old.font.style + {Fonts.underline});	(* use underline style *)
		new := TextModels.NewColor(new, Ports.blue);	(* use blue color *)
		f.rider.SetAttr(new);	(* change current attributes of formatter *)
		(* generate list of all locations *)
		PathToLoc(path, loc);
		li := Files.dir.LocList(loc);
		WHILE li # NIL DO	(* no particular sorting order is guaranteed *)
			str := "ObxLinks.Directory('";
			IF path # "" THEN str := str + path + "/" END;
			str := str + li.name + "')";
			v := StdLinks.dir.NewLink(str);
			f.WriteView(v);	(* insert left link view in text *)
			f.WriteString(li.name);
			v := StdLinks.dir.NewLink("");
			f.WriteView(v);	(* insert right link view in text *)
			f.WriteLn;
			li := li.next
		END;
		f.rider.SetAttr(old);	(* reset current attributes of formatter *)
		f.WriteLn;
		f.WriteString("Files"); f.WriteLn;
		f.rider.SetAttr(new);	(* change current attributes of formatter *)
		(* generate a list of all files *)
		fi := Files.dir.FileList(loc);
		WHILE fi # NIL DO	(* no particular sorting order is guaranteed *)
			conv := Converters.list; WHILE (conv # NIL) & (conv.fileType # fi.type) DO conv := conv.next END;
			IF conv # NIL THEN	(* there is a converter for this file type *)
				str := "ObxLinks.Open('"; str := str + path + "', '";
				str := str + fi.name + "')";
				v := StdLinks.dir.NewLink(str);
				f.WriteView(v);	(* insert left link view in text *)
				f.WriteString(fi.name);
				v := StdLinks.dir.NewLink("");
				f.WriteView(v);	(* insert right link view in text *)
				f.WriteLn
			END;
			fi := fi.next
		END;
		tv := TextViews.dir.New(t);
		(* set Browser mode: *)
		title := "Directory of " + path;
		Views.OpenAux(tv, title)
	END Directory;

	PROCEDURE Open* (path, name: ARRAY OF CHAR);
		VAR loc: Files.Locator; f: Files.File; c: Converters.Converter; n: Files.Name; s: Stores.Store;
	BEGIN
		PathToLoc(path, loc); n := name$;
		IF loc # NIL THEN
			f := Files.dir.Old(loc, n, Files.shared);
			IF f # NIL THEN
				(* search in converter list for a converter that can import a file of this type *)
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
	END Open;

END ObxLinks.
