MODULE ObxPDBRep3;
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

	IMPORT Views, TextModels, TextMappers, TextViews, StdFolds, ObxPhoneDB;					

	PROCEDURE WriteOpenFold (VAR f: TextMappers.Formatter;
															IN shortForm: ARRAY OF CHAR);
		VAR fold: StdFolds.Fold; t: TextModels.Model;
	BEGIN
		t := TextModels.dir.NewFromString(shortForm);	(* convert a string into a text model *)
		fold := StdFolds.dir.New(StdFolds.expanded, "", t);
		f.WriteView(fold)
	END WriteOpenFold;

	PROCEDURE WriteCloseFold (VAR f: TextMappers.Formatter);
		VAR fold: StdFolds.Fold; len: INTEGER;
	BEGIN
		fold := StdFolds.dir.New(StdFolds.expanded, "", NIL);
		f.WriteView(fold);
		fold.Flip;	(* swap long-form text, now between the two fold views, with hidden short-form text *)
		len := f.rider.Base().Length();	(* determine the text carrier's new length *)
		f.SetPos(len)	(* position the formatter to the end of the text *)
	END WriteCloseFold;

	PROCEDURE GenReport*;
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: TextViews.View;
			i: INTEGER; name, number: ObxPhoneDB.String;
	BEGIN
		t := TextModels.dir.New();	(* create empty text carrier *)
		f.ConnectTo(t);					(* connect a formatter to the text *)
		i := 0;
		ObxPhoneDB.LookupByIndex(i, name, number);
		WHILE name # "" DO
			WriteOpenFold(f, name$);	(* write left fold view into text, with name as its short-form text *)
			(* now write the long-form text *)
			f.WriteString(name);		(* first string *)
			f.WriteTab;					(* tab character *)
			f.WriteString(number);	(* second string *)
			WriteCloseFold(f);			(* write closing fold, and swap short- and long-form texts *)
			f.WriteLn;						(* carriage return *)
			INC(i);
			ObxPhoneDB.LookupByIndex(i, name, number)
		END;
		v := TextViews.dir.New(t);	(* create a text view for the text generated above *)
		Views.OpenView(v)			(* open the text view in its own window *)
	END GenReport;

END ObxPDBRep3.
