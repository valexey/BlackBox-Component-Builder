MODULE ObxPDBRep0;
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

	IMPORT Views, TextModels, TextMappers, TextViews, ObxPhoneDB;

	PROCEDURE GenReport*;
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: TextViews.View;
			i: INTEGER; name, number: ObxPhoneDB.String;
	BEGIN
		t := TextModels.dir.New();	(* create empty text carrier *)
		f.ConnectTo(t);					(* connect a formatter to the text *)
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

END ObxPDBRep0.
