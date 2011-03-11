MODULE ObxConv;
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

	IMPORT Files, Stores, TextModels, TextViews;

	PROCEDURE ImportText* (f: Files.File; OUT s: Stores.Store);
		VAR r: Stores.Reader; t: TextModels.Model; w: TextModels.Writer;
			byte: SHORTCHAR; ch: CHAR; len: INTEGER;
	BEGIN
		ASSERT(f # NIL, 20); ASSERT(s = NIL, 21);
		(* ASSERT(f.type = "TEXT", 22);	(* the type is platform specific*) *)
		r.ConnectTo(f); r.SetPos(0);
		len := f.Length();
		t := TextModels.dir.New(); w := t.NewWriter(NIL);
		WHILE len # 0 DO
			r.ReadSChar(byte);
			ch := byte;	(* should translate character set here *)
			w.WriteChar(ch); DEC(len)
		END;
		s := TextViews.dir.New(t)
	END ImportText;

	PROCEDURE ExportText* (s: Stores.Store; f: Files.File);
		VAR w: Stores.Writer; t: TextModels.Model; r: TextModels.Reader;
			len: INTEGER; ch: CHAR; byte: SHORTCHAR;
	BEGIN
		ASSERT(s # NIL, 20); ASSERT(f # NIL, 21); ASSERT(f.Length() = 0, 22);
		ASSERT(s IS TextViews.View, 23);
		w.ConnectTo(f); w.SetPos(0);
		t := s(TextViews.View).ThisModel();
		len := t.Length();
		r := t.NewReader(NIL);
		WHILE len # 0 DO
			r.ReadChar(ch);
			byte := SHORT(ch);	(* should translate character set here *)
			w.WriteSChar(byte);
			DEC(len)
		END
	END ExportText;

END ObxConv.
