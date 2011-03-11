MODULE FormGen;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	purpose	= ""
	changes	= ""
	issues	= ""

**)
	
	IMPORT
		Strings, Meta, Dates, Files, Ports, Views, Properties, Dialog, Containers, Documents,
		Controls, StdDialog, StdCmds, FormModels, FormViews, FormControllers;

	CONST
		defaultDelta = 4 * Ports.point;
		minW = 10 * Ports.mm; minH = 4 * Ports.mm;
		maxW = 200 * Ports.mm; maxH = 50 * Ports.mm;
		
	TYPE
		ProcVal = RECORD (Meta.Value)
			p: PROCEDURE
		END;

	VAR
		new*: RECORD
			link*: Dialog.String
		END;

		list: RECORD (Meta.Value) p*: POINTER TO Dialog.List END;
		select: RECORD (Meta.Value) p*: POINTER TO Dialog.Selection END;
		combo: RECORD (Meta.Value) p*: POINTER TO Dialog.Combo END;
		date: RECORD (Meta.Value) p*: POINTER TO Dates.Date END;
		time: RECORD (Meta.Value) p*: POINTER TO Dates.Time END;
		color: RECORD (Meta.Value) p*: POINTER TO Dialog.Color END;
		currency: RECORD (Meta.Value) p*: POINTER TO Dialog.Currency END;
		tree: RECORD (Meta.Value) p*: POINTER TO Dialog.Tree END;


	PROCEDURE GetSize (v: Views.View; VAR w, h: INTEGER);
	BEGIN
		w := Views.undefined; h := Views.undefined;
		Properties.PreferredSize(v, minW, maxW, minH, maxH, minW, minH, w, h)
	END GetSize;

	PROCEDURE Gen (f: FormModels.Model; fv: FormViews.View; VAR path, name: ARRAY OF CHAR;
					wr: FormModels.Writer; var: Meta.Item; VAR x, y, max: INTEGER; VAR first: BOOLEAN);
		VAR v, str: Views.View; dummy, delta, x0, y0, m, w, h, i: INTEGER; n: Meta.Name;
			p: Controls.Prop; s: Meta.Scanner; elem: Meta.Item; ok, back: BOOLEAN; pv: ProcVal;
	BEGIN
		dummy := 0; delta := defaultDelta; FormViews.RoundToGrid(fv, dummy, delta);
		NEW(p); p.guard := ""; p.notifier := ""; p.label := ""; p.level := 0;
		p.opt[0] := FALSE; p.opt[1] := FALSE; p.opt[2] := FALSE; p.opt[3] := FALSE; p.opt[4] := FALSE;
		back := FALSE;
		FormViews.RoundToGrid(fv, x, y);
		v := NIL; w := Views.undefined; x0 := x; y0 := y;
		IF (name[0] >= "0") & (name[0] <= "9") THEN p.link := path + "[" + name + "]"
		ELSIF name # "" THEN p.link := path + "." + name
		ELSE p.link := path$
		END;
		IF var.obj = Meta.modObj THEN	(* traverse module *)
			s.ConnectTo(var); s.Scan; m := 0;
			WHILE ~ s.eos DO
				s.GetObjName(n);
				Gen(f, fv, p.link, n, wr, s.this, x, y, m, first);
				s.Scan
			END;
			IF m > max THEN max := m END;
		ELSIF var.obj = Meta.procObj THEN
			var.GetVal(pv, ok);
			IF ok THEN	(* command *)
				IF first THEN p.opt[Controls.default] := TRUE END;
				p.label := name$; v := Controls.dir.NewPushButton(p);
				p.opt[Controls.default] := FALSE; first := FALSE
			END
		ELSIF var.obj = Meta.varObj THEN
			IF (var.typ IN {Meta.byteTyp, Meta.sIntTyp, Meta.longTyp, Meta.intTyp, Meta.sRealTyp, Meta.realTyp})
				OR (var.typ = Meta.arrTyp) & (var.BaseTyp() = Meta.charTyp) THEN
			p.opt[Controls.left] := TRUE; v := Controls.dir.NewField(p)
			ELSIF var.typ = Meta.boolTyp THEN
				p.label := name$; v := Controls.dir.NewCheckBox(p)
			ELSIF var.typ = Meta.procTyp THEN
				var.GetVal(pv, ok);
				IF ok THEN	(* command *)
					IF first THEN p.opt[Controls.default] := TRUE END;
					p.label := name$; v := Controls.dir.NewPushButton(p);
					p.opt[Controls.default] := FALSE; first := FALSE
				END
			ELSIF var.typ = Meta.recTyp THEN
				IF var.Is(list) THEN v := Controls.dir.NewListBox(p)
				ELSIF var.Is(select) THEN v := Controls.dir.NewSelectionBox(p)
				ELSIF var.Is(combo) THEN v := Controls.dir.NewComboBox(p)
				ELSIF var.Is(date) THEN v := Controls.dir.NewDateField(p)
				ELSIF var.Is(time) THEN v := Controls.dir.NewTimeField(p)
				ELSIF var.Is(color) THEN v := Controls.dir.NewColorField(p)
				ELSIF var.Is(currency) THEN p.opt[Controls.left] := TRUE; v := Controls.dir.NewField(p)
				ELSIF var.Is(tree) THEN 
					p.opt[Controls.haslines] := TRUE; p.opt[Controls.hasbuttons] := TRUE; 
					p.opt[Controls.atroot] := TRUE; p.opt[Controls.foldericons] := TRUE;
					v := Controls.dir.NewTreeControl(p)
				ELSE	(* traverse record *)
					s.ConnectTo(var); s.Scan; m := 0;
					IF name # "" THEN INC(x, delta); INC(y, 4 * delta) END;
					WHILE ~ s.eos DO
						s.GetObjName(n);
						Gen(f, fv, p.link, n, wr, s.this, x, y, m, first);
						s.Scan
					END;
					IF m > max THEN max := m END;
					IF (name # "") & (m > 0) THEN	(* insert group *)
						p.label := name$; v := Controls.dir.NewGroup(p);
						w := m; x := x0; h := y + delta - y0; y := y0; back := TRUE
					END
				END
			ELSIF var.typ = Meta.arrTyp THEN	(* traverse array *)
				i := 0; m := 0;
				IF name # "" THEN INC(x, delta); INC(y, 4 * delta) END;
				WHILE i < var.Len() DO
					var.Index(i, elem);
					Strings.IntToString(i, n); 
					Gen(f, fv, p.link, n, wr, elem, x, y, m, first);
					INC(i)
				END;
				IF m > max THEN max := m END;
				IF (name # "") & (m > 0) THEN	(* insert group *)
					p.label := name$; v := Controls.dir.NewGroup(p);
					w := m; x := x0; h := y + delta - y0; y := y0; back := TRUE
				END
			END
		END;
		IF v # NIL THEN
			IF (name # "") & (p.label = "") THEN
				p.label := name$;
				str := Controls.dir.NewCaption(p); GetSize(str, w, h);
				wr.WriteView(str, x, y, x + w, y + h); INC(x, w + delta);
				FormViews.RoundToGrid(fv, x, y)
			END;
			IF ~back THEN GetSize(v, w, h) END;
			wr.WriteView(v, x, y, x + w, y + h); INC(x, w + delta);
			IF back THEN f.PutAbove(v, NIL) END;
			y := y + h + delta
		END;
		IF x > max THEN max := x END;
		x := x0
	END Gen;

	PROCEDURE NewDialog (name: ARRAY OF CHAR): Views.View;
		VAR var: Meta.Item;  x, y, max: INTEGER; wr: FormModels.Writer; d: Documents.Document;
			f: FormModels.Model; v: FormViews.View; first: BOOLEAN; n: ARRAY 4 OF CHAR;
			c: Containers.Controller;
	BEGIN
		f := NIL; v := NIL;
		Meta.LookupPath(name, var);
		IF (var.obj = Meta.varObj) OR (var.obj = Meta.modObj) THEN
			x := defaultDelta; y := defaultDelta; max := 50 * Ports.point; first := TRUE;
			f := FormModels.dir.New();
			v := FormViews.dir.New(f); c := v.ThisController();
			IF c # NIL THEN
				c.SetOpts(Containers.layout) 
			ELSE
				v.SetController(FormControllers.dir.NewController(Containers.layout)) 
			END;
			wr := f.NewWriter(NIL); wr.Set(NIL); n := "";
			Gen(f, v, name, n, wr, var, x, y, max, first);
			d := Documents.dir.New(v, max, y)
		ELSE Dialog.ShowParamMsg("#Form:VariableNotFound", name, "", "")
		END;
		RETURN v
	END NewDialog;

	PROCEDURE Create*;
		VAR v: Views.View; i, j: INTEGER; mod, name: Files.Name; loc: Files.Locator; 
			w: FormViews.View; c: Containers.Controller;
	BEGIN
		v := NIL;
		IF new.link = "" THEN
			w := FormViews.dir.New(FormModels.dir.New()); c := w.ThisController();
			IF c # NIL THEN
				c.SetOpts({FormControllers.noFocus}) 
			ELSE
				w.SetController(FormControllers.dir.NewController({FormControllers.noFocus})) 
			END;
			v := w
		ELSE
			v := NewDialog(new.link)
		END;
		IF v # NIL THEN
			mod := new.link$; new.link := ""; name := "";
			StdCmds.CloseDialog;
			IF mod # "" THEN
				i := 0; WHILE (mod[i] # 0X) & (mod[i] # ".") DO INC(i) END;
				IF mod[i] # 0X THEN	(* module.variable *)
					mod[i] := 0X; INC(i); j := 0;
					WHILE mod[i] # 0X DO name[j] := mod[i]; INC(i); INC(j) END;
					name[j] := 0X
				END;
				StdDialog.GetSubLoc(mod, "Rsrc", loc, mod);
				IF name = "" THEN name := mod$ END;
				loc.res := 77
			ELSE
				loc := NIL; name := ""
			END;
			Views.Open(v, loc, name, NIL);
			Views.BeginModification(Views.notUndoable, v);
			Views.EndModification(Views.notUndoable, v)
		END
	END Create;
	
	PROCEDURE Empty*;
	BEGIN
		new.link := ""; Create
	END Empty;
	
	PROCEDURE CreateGuard* (VAR par: Dialog.Par);
	BEGIN
		IF new.link = "" THEN par.disabled := TRUE END
	END CreateGuard;

END FormGen.
