MODULE ObxAscii;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	purpose	= "formatted I/O from/to ASCII text files"
	changes	= ""
	issues	= ""

**)

	IMPORT Files, Stores, Converters, TextModels, TextMappers, TextViews;

	TYPE
		Text* = POINTER TO RECORD
			done-: BOOLEAN;	(* last operation was successful *)
			reading: BOOLEAN;
			form: TextMappers.Formatter;
			scan: TextMappers.Scanner
		END;

	VAR conv: Converters.Converter;

	PROCEDURE PathToFileSpec (IN path: ARRAY OF CHAR; OUT loc: Files.Locator; OUT name: Files.Name);
		VAR i, j: INTEGER; ch: CHAR;
	BEGIN
		i := 0; j := 0; loc := Files.dir.This("");
		WHILE (loc.res = 0) & (i < LEN(path) - 1) & (j < LEN(name) - 1) & (path[i] # 0X) DO
			ch := path[i]; INC(i);
			IF (j > 0) & (ch = "/") THEN name[j] := 0X; j := 0; loc := loc.This(name)
			ELSE name[j] := ch; INC(j)
			END
		END;
		IF path[i] = 0X THEN name[j] := 0X ELSE loc.res := 1; name := "" END
	END PathToFileSpec;

	PROCEDURE Open* (loc: Files.Locator; IN name: ARRAY OF CHAR): Text;
		VAR s: Stores.Store; fname: Files.Name; text: Text;
	BEGIN
		IF loc = NIL THEN PathToFileSpec(name, loc, fname) ELSE fname := name$ END;
		IF loc.res = 0 THEN
			Converters.Import(loc, fname, conv, s);
			IF (s # NIL) & (s IS TextViews.View) THEN
				NEW(text); text.reading := TRUE; text.scan.ConnectTo(s(TextViews.View).ThisModel());
				RETURN text
			ELSE RETURN NIL
			END
		ELSE RETURN NIL
		END
	END Open;

	PROCEDURE NewText* (): Text;
		VAR text: Text;
	BEGIN
		NEW(text); text.reading := FALSE; text.form.ConnectTo(TextModels.dir.New());
		RETURN text
	END NewText;
	
	PROCEDURE Register* (t: Text; loc: Files.Locator; IN name: ARRAY OF CHAR);
		VAR fname: Files.Name; v: TextViews.View;
	BEGIN
		IF ~t.reading & (name # "") THEN
			IF loc = NIL THEN PathToFileSpec(name, loc, fname) ELSE fname := name$ END;
			IF loc.res = 0 THEN v := TextViews.dir.New(t.form.rider.Base());
				Converters.Export(loc, fname, conv, v); t.done := TRUE
			ELSE t.done := FALSE
			END
		ELSE t.done := FALSE
		END
	END Register;

	PROCEDURE Eot* (t: Text): BOOLEAN;
	BEGIN
		RETURN t.reading & t.scan.rider.eot
	END Eot;

	PROCEDURE ReadChar* (t: Text; OUT c: CHAR);
	BEGIN
		ASSERT(t.reading, 20);
		t.scan.rider.ReadChar(c); t.done := ~t.scan.rider.eot
	END ReadChar;

	PROCEDURE ReadString* (t: Text; OUT s: ARRAY OF CHAR);
	BEGIN
		ASSERT(t.reading, 20);
		t.scan.Scan; t.done :=  t.scan.type = TextMappers.string; s := t.scan.string$
	END ReadString;

	PROCEDURE ReadInt* (t: Text; OUT i: INTEGER);
	BEGIN
		ASSERT(t.reading, 20);
		t.scan.Scan; t.done := t.scan.type = TextMappers.int; i := t.scan.int
	END ReadInt;

	PROCEDURE WriteString* (t: Text; IN s: ARRAY OF CHAR);
	BEGIN
		ASSERT(~t.reading, 20);
		t.form.WriteString(s); t.done := TRUE
	END WriteString;

	PROCEDURE WriteLn* (t: Text);
	BEGIN
		ASSERT(~t.reading, 20);
		t.form.WriteLn; t.done := TRUE
	END WriteLn;

	PROCEDURE WriteInt* (t: Text; i: INTEGER);
	BEGIN
		ASSERT(~t.reading, 20);
		t.form.WriteInt(i); t.done := TRUE
	END WriteInt;

BEGIN
	conv := Converters.list;
	WHILE (conv # NIL) & (conv.imp # "HostTextConv.ImportText") DO
		conv := conv.next
	END
END ObxAscii.
