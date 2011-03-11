MODULE FormCmds;
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
		Strings, Dialog, Ports, Stores, Models, Views, Properties, Controls, Containers,
		FormModels, FormViews, FormControllers, StdCmds; 

	CONST
		mm = Ports.mm; inch16th = Ports.inch DIV 16;
		left = 0; top = 1; right = 2; bottom = 3; row = 4; column = 5;

	VAR
		grid*: RECORD
			resolution*: INTEGER;	(* resolution > 0 *)
			metricSystem*: BOOLEAN
		END;

		find*: RECORD
			from*, to*: ARRAY 256 OF CHAR;
			link*, label*, guard*, notifier*: BOOLEAN;
		END;


	PROCEDURE FindExtrema (c: FormControllers.Controller; OUT l, t, r, b, h, v: INTEGER;
											OUT s: FormControllers.List);
		VAR n: INTEGER; p: FormControllers. List; q: FormModels.Reader; w: Views.View;
	BEGIN	(* there is at least one view selected *)
		l := MAX(INTEGER); t := MAX(INTEGER); r := MIN(INTEGER); b := MIN(INTEGER);
		h := 0; v := 0; n := 0; s := NIL;
		q := c.form.NewReader(NIL); q.Set(NIL); q.ReadView(w);
		WHILE w # NIL DO
			IF c.IsSelected(w) THEN
				INC(h, q.l); INC(h, q.r); INC(v, q.t); INC(v, q.b); INC(n, 2);
				NEW(p); p.next := s; s := p; p.view := w;
				IF q.l < l THEN l := q.l END;
				IF q.t < t THEN t := q.t END;
				IF q.r > r THEN r := q.r END;
				IF q.b > b THEN b := q.b END
			END;
			q.ReadView(w)
		END;
		h := h DIV n; v := v DIV n
	END FindExtrema;

	PROCEDURE Move (c: FormControllers.Controller; s: FormControllers. List; p, side: INTEGER;
									name: Stores.OpName);
		VAR script: Stores.Operation; w, h: INTEGER; l, t, r, b: INTEGER; s0: FormControllers.List;
	BEGIN
		s0 := s;
		Models.BeginScript(c.form, name, script);
		WHILE s # NIL DO
			s.view.context(FormModels.Context).GetRect(l, t, r, b); w := r - l; h := b - t;
			IF side = left THEN
				l := p; r := l + w
			ELSIF side = top THEN
				t := p; b := t + h
			ELSIF side = right THEN
				r := p; l := r - w
			ELSIF side = bottom THEN
				b := p; t := b - h
			ELSIF side = row THEN
				t := p - h DIV 2; b := t + h
			ELSIF side = column THEN
				l := p - w DIV 2; r := l + w
			END;
			c.form.Resize(s.view, l, t, r, b);
			s := s.next
		END;
		Models.EndScript(c.form, script);
		WHILE s0 # NIL DO c.Select(s0.view); s0 := s0.next END
	END Move;

	PROCEDURE AlignLeft*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller; l, t, r, b, h, v: INTEGER; s: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			FindExtrema(c, l, t, r, b, h, v, s); Move(c, s, l, left, "#Form:AlignLeft")
		END
	END AlignLeft;

	PROCEDURE AlignRight*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller; l, t, r, b, h, v: INTEGER; s: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			FindExtrema(c, l, t, r, b, h, v, s); Move(c, s, r, right, "#Form:AlignRight")
		END
	END AlignRight;

	PROCEDURE AlignTop*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller; l, t, r, b, h, v: INTEGER; s: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			FindExtrema(c, l, t, r, b, h, v, s); Move(c, s, t, top, "#Form:AlignTop")
		END
	END AlignTop;

	PROCEDURE AlignBottom*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller; l, t, r, b, h, v: INTEGER; s: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			FindExtrema(c, l, t, r, b, h, v, s); Move(c, s, b, bottom, "#Form:AlignBottom")
		END
	END AlignBottom;

	PROCEDURE AlignToRow*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller; l, t, r, b, h, v: INTEGER; s: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			FindExtrema(c, l, t, r, b, h, v, s); Move(c, s, v, row, "#Form:AlignHorizontal")
		END
	END AlignToRow;

	PROCEDURE AlignToColumn*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller; l, t, r, b, h, v: INTEGER; s: FormControllers.List;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			FindExtrema(c, l, t, r, b, h, v, s); Move(c, s, h, column, "#Form:AlignVertical")
		END
	END AlignToColumn;


	PROCEDURE SelectOffGridViews*;
		VAR c: FormControllers.Controller; v: Views.View; h: FormModels.Reader;
			l, t, r, b, l0, t0, r0, b0: INTEGER;
	BEGIN
		c := FormControllers.Focus();
		IF c # NIL THEN
			c.SelectAll(FALSE);
			h := c.form.NewReader(NIL);
			h.ReadView(v);
			WHILE v # NIL DO
				v.context(FormModels.Context).GetRect(l, t, r, b);
				l0 := l; t0 := t; r0 := r; b0 := b;
				FormViews.RoundToGrid(c.view, l0, t0);
				FormViews.RoundToGrid(c.view, r0, b0);
				IF (l0 # l) OR (t0 # t) OR (r0 # r) OR (b0 # b) THEN c.Select(v) END;
				h.ReadView(v)
			END
		END
	END SelectOffGridViews;


	PROCEDURE Selection (c: FormControllers.Controller): FormControllers.List;
		VAR p, q: FormControllers. List; h: FormModels.Reader; v: Views.View;
	BEGIN
		p := NIL;
		h := c.form.NewReader(NIL); h.Set(NIL); h.ReadView(v);
		WHILE v # NIL DO
			IF c.IsSelected(v) THEN NEW(q); q.next := p; p := q; q.view := v END;
			h.ReadView(v)
		END;
		RETURN p
	END Selection;

	PROCEDURE ForceToGrid*;
	(** Guard: SelectionGuard **)
		VAR c: FormControllers.Controller;
			l, t, r, b: INTEGER; sel, sel0: FormControllers.List; s: Stores.Operation;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			Models.BeginScript(c.form, "#Form:ForceToGrid", s);
			sel := Selection(c); sel0 := sel;
			WHILE sel # NIL DO
				sel.view.context(FormModels.Context).GetRect(l, t, r, b);
				FormViews.RoundToGrid(c.view, l, t);
				FormViews.RoundToGrid(c.view, r, b);
				c.form.Resize(sel.view, l, t, r, b);
				sel := sel.next
			END;
			Models.EndScript(c.form, s);
			WHILE sel0 # NIL DO c.Select(sel0.view); sel0 := sel0.next END
		END
	END ForceToGrid;

	PROCEDURE SortViews*;
	(** Guard: FocusGuard **)
		VAR c: FormControllers.Controller; script: Stores.Operation;
			r: FormModels.Reader; p, q: Views.View;

		PROCEDURE IsAbove (p, q: Views.View): BOOLEAN;
			VAR l0, l1, t0, t1, r, b: INTEGER;
		BEGIN
			p.context(FormModels.Context).GetRect(l0, t0, r, b);
			q.context(FormModels.Context).GetRect(l1, t1, r, b);
			RETURN (t0 < t1) OR (t0 = t1) & (l0 <= l1)
		END IsAbove;

	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & (c.form.NofViews() > 1) THEN
			c.SelectAll(FALSE);
			Models.BeginScript(c.form, "#Form:ChangeZOrder", script);
			r := c.form.NewReader(NIL);
			REPEAT	(* bubble sort *)
				r.Set(NIL); r.ReadView(p); r.ReadView(q);
				WHILE (q # NIL) & IsAbove(p, q) DO p := q; r.ReadView(q) END;
				IF q # NIL THEN c.form.PutAbove(p, q) END
			UNTIL q = NIL;
			Models.EndScript(c.form, script);
			c.SelectAll(TRUE)
		END
	END SortViews;

	PROCEDURE SetAsFirst*;
	(** Guard: SingletonGuard **)
	(** send to back **)
		VAR c: FormControllers.Controller; v: Views.View;
	BEGIN
		c := FormControllers.Focus();
		IF c # NIL THEN
			v := c.Singleton();
			IF v # NIL THEN
				c.form.PutAbove(v, NIL);
				c.SetSingleton(v)
			END
		END
	END SetAsFirst;

	PROCEDURE SetAsLast*;
	(** Guard: SingletonGuard **)
	(** bring to front **)
		VAR c: FormControllers.Controller; v, p, q: Views.View; r: FormModels.Reader;
	BEGIN
		c := FormControllers.Focus();
		IF c # NIL THEN
			v := c.Singleton();
			IF v # NIL THEN
				r := c.form.NewReader(NIL); r.Set(v); p := v;
				REPEAT q := p; r.ReadView(p) UNTIL p = NIL;
				c.form.PutAbove(v, q);
				c.SetSingleton(v)
			END
		END
	END SetAsLast;

	PROCEDURE InsertAround*;
	(** Guard: SelectionGuard **)
		CONST d = 3 * Ports.mm;
		VAR c: FormControllers.Controller; v: Views.View; rd: FormModels.Reader; l, t, r, b: INTEGER;
			s: Stores.Operation;
	BEGIN
		ASSERT(Views.Available() = 1, 20);
		c := FormControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			l := MAX(INTEGER); t := MAX(INTEGER); r := MIN(INTEGER); b := MIN(INTEGER);
			rd := c.form.NewReader(NIL); rd.ReadView(v);
			WHILE v # NIL DO
				IF c.IsSelected(v) THEN
					IF rd.l < l THEN l := rd.l END;
					IF rd.t < t THEN t := rd.t END;
					IF rd.r > r THEN r := rd.r END;
					IF rd.b > b THEN b := rd.b END
				END;
				rd.ReadView(v)
			END;
			ASSERT(l < r, 100);
			DEC(l, d); DEC(t, 3 * d); INC(r, d); INC(b, d);
			FormViews.RoundToGrid(c.view, l, t);
			FormViews.RoundToGrid(c.view, r, b);
			Views.Fetch(v);
			Models.BeginScript(c.form, "#Form:InsertAround", s);
			c.form.Insert(v, l, t, r, b);
			c.form.PutAbove(v, NIL);
			Models.EndScript(c.form, s);
			c.SetSingleton(v)
		ELSE
			StdCmds.PasteView
		END
	END InsertAround;

	PROCEDURE InitGridDialog*;
	(** Guard: FocusGuard **)
		VAR c: FormControllers.Controller; g: INTEGER;
	BEGIN
		c := FormControllers.Focus();
		IF c # NIL THEN
			g := c.view.Grid();
			IF g MOD mm = 0 THEN
				grid.resolution := g DIV mm;
				grid.metricSystem := TRUE
			ELSIF g MOD inch16th = 0 THEN
				grid.resolution := g DIV inch16th;
				grid.metricSystem := FALSE
			ELSIF Dialog.metricSystem THEN
				grid.resolution := g DIV mm;
				grid.metricSystem := TRUE
			ELSE
				grid.resolution := g  DIV inch16th;
				grid.metricSystem := FALSE
			END;
			Dialog.Update(grid)
		END
	END InitGridDialog;

	PROCEDURE SetGrid*;
	(** Guard: FocusGuard **)
		VAR c: FormControllers.Controller; n: INTEGER;
	BEGIN
		c := FormControllers.Focus();
		IF (c # NIL) & (grid.resolution > 0) THEN
			IF grid.metricSystem THEN
				n := 10 DIV grid.resolution;
				IF n < 1 THEN n := 1 END;
				c.view.SetGrid(grid.resolution * mm, n)
			ELSE
				n := 8 DIV grid.resolution;
				IF n < 1 THEN n := 1 END;
				c.view.SetGrid(grid.resolution * inch16th, n)
			END
		END
	END SetGrid;

	(** renaming of controls **)

	PROCEDURE Rename*;
	(** Guard: StdCmds.ContainerGuard **)
		VAR c: Containers.Controller; v: Views.View;
			msg: Properties.PollMsg; p, q: Properties.Property;
			setMsg: Properties.SetMsg;

		PROCEDURE Replace(VAR s: ARRAY OF CHAR; IN from, to: ARRAY OF CHAR);
			VAR pos: INTEGER;
		BEGIN
			Strings.Find(s, from, 0, pos);
			WHILE pos >= 0 DO
				Strings.Replace(s, pos, LEN(from$), to);
				Strings.Find(s, from, pos + LEN(to$), pos);
			END
		END Replace;

	BEGIN
		c := Containers.Focus();
		IF c # NIL THEN
			c.GetFirstView(Containers.any, v);
			WHILE v # NIL DO
				msg.prop := NIL; Views.HandlePropMsg(v, msg);
				p := msg.prop; WHILE (p # NIL) & ~(p IS Controls.Prop) DO p := p.next END;
				IF p # NIL THEN
					q := Properties.CopyOf(p);
					WITH q: Controls.Prop DO
						IF find.label & (Controls.label IN q.valid) THEN Replace(q.label, find.from, find.to) END;
						IF find.link & (Controls.link IN q.valid) THEN Replace(q.link, find.from, find.to) END;
						IF find.guard & (Controls.guard IN q.valid) THEN Replace(q.guard, find.from, find.to) END;
						IF find.notifier & (Controls.notifier IN q.valid) THEN Replace(q.notifier, find.from, find.to) END
					END;
					setMsg.prop := q;
					Views.HandlePropMsg(v, setMsg);
				END;
				c.GetNextView(Containers.any, v)
			END
		END
	END Rename;


	(** standard form-related guards **)

	PROCEDURE FocusGuard* (VAR par: Dialog.Par);
	(** in non-FormView menus; otherwise implied by menu type **)
	BEGIN
		par.disabled := FormViews.Focus() = NIL
	END FocusGuard;

	PROCEDURE SelectionGuard* (VAR par: Dialog.Par);
	(** in non-FormView menus; otherwise use "SelectionGuard" **)
		VAR c: FormControllers.Controller;
	BEGIN
		c := FormControllers.Focus();
		par.disabled := (c = NIL) OR ~c.HasSelection()
	END SelectionGuard;

	PROCEDURE SingletonGuard* (VAR par: Dialog.Par);
	(** in non-FormView menus; otherwise use "SingletonGuard" **)
		VAR c: FormControllers.Controller;
	BEGIN
		c := FormControllers.Focus();
		par.disabled := (c = NIL) OR (c.Singleton() = NIL)
	END SingletonGuard;

BEGIN
	find.link := TRUE; find.label := TRUE; find.guard := TRUE; find.notifier := TRUE
END FormCmds.
