MODULE ObxOpen1;
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

	IMPORT Converters, Files, Views, TextModels, TextMappers, TextViews;

	PROCEDURE Do*;
		VAR loc: Files.Locator; name: Files.Name; conv: Converters.Converter;
			v: Views.View; t: TextModels.Model; f: TextMappers.Formatter;
			res: INTEGER;
	BEGIN
		loc := NIL; name := ""; conv := NIL;
		v := Views.Old(Views.ask, loc, name, conv);
		IF (v # NIL) & (v IS TextViews.View) THEN
			t := v(TextViews.View).ThisModel();
			f.ConnectTo(t);
			f.SetPos(t.Length());
			f.WriteString("appendix");
			Views.Register(v, Views.ask, loc, name, conv, res)	(* ask the user where to save the document *)
		END
	END Do;

END ObxOpen1.