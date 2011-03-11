MODULE ObxMMerge;
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

	(* note that as in the other sample programs, no error handling is performed *)

	IMPORT Files, Dialog, Views, TextModels, TextViews, TextControllers;

	CONST tab = 09X;

	TYPE
		Field = POINTER TO RECORD
			prev: Field;							(* field list is sorted in reverse order *)
			name: ARRAY 24 OF CHAR;	(* name of placeholder *)
			tmplFrom, tmplTo: INTEGER;	(* character range used by placeholder in template *)
			index: INTEGER;	(* column index of this field *)
			dataFrom, dataTo: INTEGER	(* character range used by actual data in database *)
		END;

	PROCEDURE TmplFields (t: TextModels.Model): Field;
	(* returns a list of placeholder fields, in reverse order *)
	(* each field defines a text range and name of a placeholder *)
	(* the placeholder has the form "...<NameOfPlaceholder>..." *)
		VAR l, f: Field; r: TextModels.Reader; ch: CHAR; i: INTEGER;
	BEGIN
		l := NIL; r := t.NewReader(NIL); r.ReadChar(ch);
		WHILE ~r.eot DO
			IF ch = "<" THEN
				NEW(f); f.tmplFrom := r.Pos() - 1;
				r.ReadChar(ch); i := 0;
				WHILE ch # ">" DO
					f.name[i] := ch; INC(i);
					r.ReadChar(ch)
				END;
				f.name[i] := 0X; f.tmplTo := r.Pos();
				f.dataFrom := -1; f.dataTo := -1;
				f.prev := l; l := f
			END;
			r.ReadChar(ch)
		END;
		RETURN l
	END TmplFields;

	PROCEDURE ThisDatabase (): TextModels.Model;
		VAR loc: Files.Locator; name: Files.Name; v: Views.View;
			t: TextModels.Model;
	BEGIN
		t := NIL; loc := NIL; name := "";
		Dialog.GetIntSpec("", loc, name);
		IF loc # NIL THEN
			v := Views.OldView(loc, name);
			IF (v # NIL) & (v IS TextViews.View) THEN
				t := v(TextViews.View).ThisModel()
			END
		END;
		RETURN t
	END ThisDatabase;

	PROCEDURE MergeFields (f: Field; t: TextModels.Model);
	(* determine every template field's index in the data text's row of fields *)
		VAR r: TextModels.Reader; index, i: INTEGER; ch: CHAR;
	BEGIN
		r := t.NewReader(NIL);
		WHILE f # NIL DO	(* iterate over all fields in the template *)
			f.index := -1;
			r.SetPos(0); index := 0; ch := tab;
			WHILE (ch = tab) & (f.index = -1) DO	(* compare names of the fields *)
				REPEAT r.ReadChar(ch) UNTIL ch >= " ";
				i := 0; WHILE ch = f.name[i] DO r.ReadChar(ch); INC(i) END;
				IF (ch < " ") & (f.name[i] = 0X) THEN	(* names match *)
					f.index := index
				ELSE	(* no match; proceed to next data field *)
					WHILE ch >= " " DO r.ReadChar(ch) END
				END;
				INC(index)
			END;
			f := f.prev
		END
	END MergeFields;

	PROCEDURE ReadTuple (f: Field; r: TextModels.Reader);
	(* read tuple in data, and assign ranges to corresponding fields *)
		VAR index: INTEGER; from, to: INTEGER; ch: CHAR; g: Field;
	BEGIN
		index := 0; ch := tab;
		WHILE ch = tab DO
			REPEAT r.ReadChar(ch) UNTIL (ch = 0X) OR (ch >= " ") OR (ch = tab) OR (ch = 0DX);
			from := r.Pos() - 1;
			WHILE ch >= " " DO r.ReadChar(ch) END;
			to := r.Pos(); IF ~r.eot THEN DEC(to) END;
			g := f;
			WHILE g # NIL DO
				IF g.index = index THEN g.dataFrom := from; g.dataTo := to END;
				g := g.prev
			END;
			INC(index)
		END
	END ReadTuple;

	PROCEDURE AppendInstance (f: Field; data, tmpl, out: TextModels.Model);
		VAR start, from: INTEGER; r: TextModels.Reader; attr: TextModels.Attributes;
	BEGIN
		start := out.Length();
		r := out.NewReader(NIL);
		out.InsertCopy(start, tmpl, 0, tmpl.Length());	(* append new copy of template *)
		WHILE f # NIL DO	(* substitute placeholders, from end to beginning of template *)
			from := start + f.tmplFrom;
			r.SetPos(from); r.ReadRun(attr);	(* save attributes *)
			out.Delete(from, from + f.tmplTo - f.tmplFrom);	(* delete placeholder *)
			out.InsertCopy(from, data, f.dataFrom, f.dataTo);	(* insert actual data *)
			out.SetAttr(from, from + f.dataTo - f.dataFrom, attr);	(* set attributes *)
			f := f.prev
		END
	END AppendInstance;

	PROCEDURE Merge*;
		VAR c: TextControllers.Controller; tmpl, data, out: TextModels.Model;
			tmplFields: Field; r: TextModels.Reader; v: TextViews.View;
	BEGIN
		c := TextControllers.Focus();
		IF c # NIL THEN
			tmpl := c.text;	(* text template used for mail merge *)
			tmplFields := TmplFields(tmpl);	(* determine fields in template *)
			data := ThisDatabase();	(* get text database for mail merge *)
			IF data # NIL THEN
				MergeFields(tmplFields, data);	(* determine every template field's column in database *)
				out := TextModels.dir.New();	(* create output text *)
				r := data.NewReader(NIL); r.SetPos(0);
				ReadTuple(tmplFields, r);	(* skip meta data *)
				REPEAT
					ReadTuple(tmplFields, r);	(* read next data row *)
					AppendInstance(tmplFields, data, tmpl, out)	(* append new instance of template *)
				UNTIL r.eot;
				v := TextViews.dir.New(out);
				Views.OpenView(v)	(* open text view in window *)
			END
		END
	END Merge;

END ObxMMerge.
