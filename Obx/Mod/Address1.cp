MODULE ObxAddress1;
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

	IMPORT Views, TextModels, TextMappers, TextViews;

	VAR
		adr*: RECORD
			name*:	ARRAY 64 OF CHAR;
			city*:		ARRAY 24 OF CHAR;
			country*:	ARRAY 16 OF CHAR;
			customer*:	INTEGER;
			update*:	BOOLEAN
		END;

	PROCEDURE OpenText*;
		VAR t: TextModels.Model; f: TextMappers.Formatter; v: Views.View;
	BEGIN
		t := TextModels.dir.New();	(* create a new text editor object *)
		f.ConnectTo(t);		(* connect a formatter to this object *)
		f.WriteString(adr.name); f.WriteTab;
		f.WriteString(adr.city); f.WriteTab;
		f.WriteString(adr.country); f.WriteTab;
		f.WriteInt(adr.customer); f.WriteTab;
		f.WriteBool(adr.update); f.WriteLn;
		v := TextViews.dir.New(t);	(* create a visual component for the text object *)
		Views.OpenView(v)	(* open the visual component in its own window *)
	END OpenText;

END ObxAddress1.
