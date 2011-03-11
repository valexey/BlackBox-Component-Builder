MODULE ObxPDBRep4;
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

	IMPORT Views, TextModels, TextMappers, TextViews, StdLinks, StdLog, ObxPhoneDB;

	CONST cmdStart = "ObxPDBRep4.Log('"; cmdEnd = "')";

	PROCEDURE GenReport*;
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: TextViews.View;
			i: INTEGER; name, number: ObxPhoneDB.String; link: StdLinks.Link;
			cmd: ARRAY 128 OF CHAR;
	BEGIN
		t := TextModels.dir.New();	(* create empty text carrier *)
		f.ConnectTo(t);					(* connect a formatter to the text *)
		i := 0;
		ObxPhoneDB.LookupByIndex(i, name, number);
		WHILE name # "" DO
			cmd := cmdStart + name + "    " + number + cmdEnd;
			link := StdLinks.dir.NewLink(cmd);
			f.WriteView(link);
			f.WriteString(name);		(* the string shown between the pair of link views *)
			link := StdLinks.dir.NewLink("");
			f.WriteView(link);
			f.WriteLn;						(* carriage return *)
			INC(i);
			ObxPhoneDB.LookupByIndex(i, name, number)
		END;
		v := TextViews.dir.New(t);	(* create a text view for the text generated above *)
		Views.OpenView(v)			(* open the text view in its own window *)
	END GenReport;

	PROCEDURE Log* (param: ARRAY OF CHAR);
	BEGIN
		StdLog.String(param); StdLog.Ln
	END Log;

END ObxPDBRep4.
