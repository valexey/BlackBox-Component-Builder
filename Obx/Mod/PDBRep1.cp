MODULE ObxPDBRep1;
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

	IMPORT Ports, Views, TextModels, TextMappers, TextViews, ObxPhoneDB;

	PROCEDURE GenReport*;
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: TextViews.View;
			i: INTEGER; name, number: ObxPhoneDB.String;
			default, green: TextModels.Attributes;
	BEGIN
		t := TextModels.dir.New();	(* create empty text carrier *)
		f.ConnectTo(t);					(* connect a formatter to the text *)
		default := f.rider.attr;	(* save old text attributes for later use *)
		green := TextModels.NewColor(default, Ports.green);	(* use green color *)
		i := 0;
		ObxPhoneDB.LookupByIndex(i, name, number);
		WHILE name # "" DO
			f.rider.SetAttr(green);	(* change current attributes of formatter's rider *)
			f.WriteString(name);		(* first string *)
			f.rider.SetAttr(default);	(* change current attributes of formatter's rider *)
			f.WriteTab;					(* tab character *)
			f.WriteString(number);	(* second string *)
			f.WriteLn;						(* carriage return *)
			INC(i);
			ObxPhoneDB.LookupByIndex(i, name, number)
		END;
		v := TextViews.dir.New(t);	(* create a text view for the text generated above *)
		Views.OpenView(v)			(* open the text view in its own window *)
	END GenReport;

END ObxPDBRep1.
