MODULE ObxPDBRep2;
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

	IMPORT Ports, Views, TextModels, TextMappers, TextViews, TextRulers, ObxPhoneDB;

	PROCEDURE WriteRuler (VAR f: TextMappers.Formatter);
		CONST cm = 10 * Ports.mm;	(* universal units *)
		VAR ruler: TextRulers.Ruler;
	BEGIN
		ruler := TextRulers.dir.New(NIL);
		TextRulers.AddTab(ruler, 4 * cm);	(* define a tab stop, 4 cm from the left margin *)
		TextRulers.SetRight(ruler, 12 * cm);	(* set right margin *)
		f.WriteView(ruler)							(* a ruler is a view, thus can be written to the text *)
	END WriteRuler;

	PROCEDURE GenReport*;
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: TextViews.View;
			i: INTEGER; name, number: ObxPhoneDB.String;
	BEGIN
		t := TextModels.dir.New();	(* create empty text carrier *)
		f.ConnectTo(t);					(* connect a formatter to the text *)
		WriteRuler(f);
		i := 0;
		ObxPhoneDB.LookupByIndex(i, name, number);
		WHILE name # "" DO
			f.WriteString(name);		(* first string *)
			f.WriteTab;					(* tab character *)
			f.WriteString(number);	(* second string *)
			f.WriteLn;						(* carriage return *)
			INC(i);
			ObxPhoneDB.LookupByIndex(i, name, number)
		END;
		v := TextViews.dir.New(t);	(* create a text view for the text generated above *)
		Views.OpenView(v)			(* open the text view in its own window *)
	END GenReport;

END ObxPDBRep2.
