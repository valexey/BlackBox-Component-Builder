MODULE ObxAddress2;
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

	IMPORT Files, Converters, Views, Dialog, TextModels, TextMappers, TextViews;

	VAR
		adr*: RECORD
			name*:	ARRAY 64 OF CHAR;
			city*:		ARRAY 24 OF CHAR;
			country*:	ARRAY 16 OF CHAR;
			customer*:	INTEGER;
			update*:	BOOLEAN
		END;

	PROCEDURE OpenText*;
		VAR loc: Files.Locator; name: Files.Name; conv: Converters.Converter;
			v: Views.View; t: TextModels.Model; f: TextMappers.Formatter;
	BEGIN
		loc := NIL; name := ""; conv := NIL;
		v := Views.Old(Views.ask, loc, name, conv);
		IF (v # NIL) & (v IS TextViews.View) THEN
			t := v(TextViews.View).ThisModel();
			f.ConnectTo(t);
			f.SetPos(t.Length());
			f.WriteString(adr.name); f.WriteTab;
			f.WriteString(adr.city); f.WriteTab;
			f.WriteString(adr.country); f.WriteTab;
			f.WriteInt(adr.customer); f.WriteTab;
			f.WriteBool(adr.update); f.WriteLn;
			Views.OpenView(v);
			adr.name := ""; adr.city := ""; adr.country := ""; adr.customer := 0; adr.update := FALSE;
			Dialog.Update(adr)	(* update all controls for adr *)
		END
	END OpenText;

END ObxAddress2.
