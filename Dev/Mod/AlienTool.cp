MODULE DevAlienTool;
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

	IMPORT
		Services, Ports, Stores, Models, Views, Controllers, Properties, Dialog, Containers, Documents,
		TextModels, TextMappers, TextViews, StdFolds;

	PROCEDURE Indent (VAR f: TextMappers.Formatter; level: INTEGER);
	BEGIN
		WHILE level > 0 DO f.WriteTab; DEC(level) END
	END Indent;

	PROCEDURE WriteCause (VAR f: TextMappers.Formatter; cause: INTEGER);
	BEGIN
		f.rider.SetAttr(TextModels.NewColor(f.rider.attr, Ports.red));
		CASE cause OF
		| Stores.typeNotFound: f.WriteString("type in module not found")
		| Stores.inconsModuleVersion: f.WriteString("inconsistent module version")
		| Stores.invalidModuleFile: f.WriteString("invalid module file")
		| Stores.moduleFileNotFound: f.WriteString("module not found")
		| Stores.inconsistentType: f.WriteString("type path in module inconsistent with stored version")
		| Stores.inconsistentVersion: f.WriteString("inconsistent version / program error")
		| Stores.alienVersion: f.WriteString("alien version - outdated program")
		| Stores.alienComponent: f.WriteString("alien component - required sub-part failed to internalize")
		ELSE f.WriteString("unknown (code"); f.WriteInt(cause); f.WriteChar(")")
		END;
		f.rider.SetAttr(TextModels.NewColor(f.rider.attr, Ports.black));
	END WriteCause;

	PROCEDURE Out (VAR f: TextMappers.Formatter; level: INTEGER; st: Stores.Store);
		VAR t: Stores.TypeName;

		PROCEDURE OutAlien (VAR f: TextMappers.Formatter; 
											path: Stores.TypePath; cause: INTEGER; c: Stores.AlienComp);
			VAR i: INTEGER; t: TextModels.Model; form: TextMappers.Formatter;
		BEGIN
			f.WriteString(" ");
			t := TextModels.dir.New(); form.ConnectTo(t);

			form.rider.SetAttr(TextModels.NewColor(form.rider.attr, Ports.blue));
			form.WriteString(path[0]); 
			form.rider.SetAttr(TextModels.NewColor(form.rider.attr, Ports.black));
			form.WriteLn;
			INC(level);
			IF path[1] # "" THEN
				Indent(form, level); form.WriteString("path: (");
				i := 1;
				WHILE path[i] # "" DO
					form.WriteString(path[i]);
					INC(i);
					IF path[i] # "" THEN form.WriteString(", ") END
				END;
				form.WriteChar(")"); form.WriteLn
			END;
			Indent(form, level); form.WriteString("cause: "); WriteCause(form, cause); form.WriteLn;
			Indent(form, level); form.WriteString("comps: "); form.WriteLn;
			INC(level);
			WHILE c # NIL DO
				WITH c: Stores.AlienPiece DO
					Indent(form, level); form.WriteInt(c.len); form.WriteString(" bytes data"); form.WriteLn
				| c: Stores.AlienPart DO
					IF c.store # NIL THEN
						Out(form, level, c.store)
					ELSE Indent(form, level); form.WriteString("NIL reference"); form.WriteLn
					END
				END;
				c := c.next
			END;
			DEC(level, 2);
			Indent(form, level);
			
			f.WriteView(StdFolds.dir.New(StdFolds.collapsed, "", t));
			f.rider.SetAttr(TextModels.NewColor(f.rider.attr, Ports.blue));
			f.WriteString(path[0]); 
			f.rider.SetAttr(TextModels.NewColor(f.rider.attr, Ports.black));
			f.WriteView(StdFolds.dir.New(StdFolds.collapsed, "", NIL));
			f.WriteLn;
		END OutAlien;

	BEGIN
		Indent(f, level);
		WITH st: Stores.Alien DO
			f.WriteString("Alien Store"); OutAlien(f, st.path, st.cause, st.comps)
		ELSE
			Services.GetTypeName(st, t);
			WITH st: Documents.Document DO f.WriteString("Document")
			| st: Containers.Controller DO f.WriteString("Container Controller")
			| st: Containers.View DO f.WriteString("Container View")
			| st: Containers.Model DO f.WriteString("Container Model")
			| st: Controllers.Controller DO f.WriteString("Controller")
			| st: Views.View DO f.WriteString("View")
			| st: Models.Model DO f.WriteString("Model")
			ELSE f.WriteString("Store")
			END;
			f.WriteString(' "'); f.WriteString(t); f.WriteChar('"'); f.WriteLn
		END
	END Out;

	PROCEDURE Analyze*;
		VAR v: Views.View; f: TextMappers.Formatter; d: Documents.Document;
			ops: Controllers.PollOpsMsg; bp: Properties.BoundsPref; t: TextModels.Model;
	BEGIN
		Controllers.PollOps(ops); v := ops.singleton;
		IF v # NIL THEN
			IF v IS Views.Alien THEN
				t := TextModels.dir.New();
				f.ConnectTo(t);
				Out(f, 0, v(Views.Alien).store);
				StdFolds.ExpandFolds(t, FALSE, "");
				v := TextViews.dir.New(t);
				Views.OpenAux(v, "Alien Info");
(*
				bp.w := Views.undefined; bp.h := Views.undefined; 
				Views.HandlePropMsg(v, bp);
				d := Documents.dir.New(v, bp.w, bp.h);
				Views.OpenAux(d, "Alien Info")
*)
			ELSE Dialog.ShowMsg("#Dev:NoAlienView")
			END
		ELSE Dialog.ShowMsg("#Dev:NoSingletonFound")
		END
	END Analyze;

END DevAlienTool.

Strings
NoAlienView	no alien view
NoSingletonFound	no singleton found

Info
SEPARATOR
"&Aliens" "" "DevAlienTool.Analyze" "DevAlienTool.SingletonGuard"
END
C