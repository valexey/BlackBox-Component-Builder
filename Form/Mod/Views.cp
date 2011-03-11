MODULE FormViews;
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

	IMPORT Dialog, Ports, Stores, Models, Views, Controllers, Properties, Containers, FormModels;

	CONST
		(** minimal border between form view and any embedded view: **)
		minBorder* = 4 * Ports.point; maxBorder* = 100 * Ports.mm;
		maxSize = 600 * Ports.mm;
		(* range of currently supported versions *)
		minVersion = 0; maxBaseVersion = 2; maxStdVersion = 1;

	TYPE
		View* = POINTER TO ABSTRACT RECORD (Containers.View) END;

		Directory* = POINTER TO ABSTRACT RECORD END;


		StdView = POINTER TO RECORD (View)
			border: INTEGER;
			grid: INTEGER;	(* grid > 0 *)
			gridFactor: INTEGER;	(* gridFactor > 0 *)
			background: Ports.Color;
			cache: FormModels.Reader	(* reuse form reader *)
		END;

		StdDirectory = POINTER TO RECORD (Directory) END;


		ViewOp = POINTER TO RECORD (Stores.Operation)
			view: StdView;	(* view # NIL *)
			border: INTEGER;	(* border >= minBorder *)
			grid: INTEGER;	(* grid > 0 *)
			gridFactor: INTEGER;	(* gridFactor > 0 *)
			background: Ports.Color
		END;


	VAR
		dir-, stdDir-: Directory;
		ctrldir-: Containers.Directory;


	(** View **)

	PROCEDURE (v: View) Internalize2- (VAR rd: Stores.Reader), EXTENSIBLE;
		VAR thisVersion: INTEGER;
	BEGIN
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, maxBaseVersion, thisVersion);
			IF~ rd.cancelled THEN
				IF thisVersion IN {0, 1} THEN
					WITH v: StdView DO	(* backward compatibility with Rel. 1.3 *)
						rd.ReadInt(v.border);
						rd.ReadInt(v.grid);
						rd.ReadXInt(v.gridFactor);
						IF thisVersion = 1 THEN
							rd.ReadInt(v.background)
						ELSE
							v.background := Ports.defaultColor
						END
					END
				END
			END
		END
	END Internalize2;

	PROCEDURE (v: View) Externalize2- (VAR wr: Stores.Writer), EXTENSIBLE;
	BEGIN
		wr.WriteVersion(maxBaseVersion)
	END Externalize2;

	PROCEDURE (v: View) ThisModel* (): FormModels.Model, EXTENSIBLE;
		VAR m: Containers.Model;
	BEGIN
		m := v.ThisModel^();
		IF m = NIL THEN RETURN NIL ELSE RETURN m(FormModels.Model) END
	END ThisModel;

	PROCEDURE (v: View) SetBorder* (border: INTEGER), NEW, ABSTRACT;
	(** border >= 0	20 **)

	PROCEDURE (v: View) Border* (): INTEGER, NEW, ABSTRACT;

	PROCEDURE (v: View) SetGrid* (grid, gridFactor: INTEGER), NEW, ABSTRACT;
	(**
		grid > 0	20
		gridFactor > 0	21
	**)

	PROCEDURE (v: View) Grid* (): INTEGER, NEW, ABSTRACT;

	PROCEDURE (v: View) GridFactor* (): INTEGER, NEW, ABSTRACT;

	PROCEDURE (v: View) SetBackground* (background: Ports.Color), NEW, ABSTRACT;


	(** Directory **)

	PROCEDURE (d: Directory) New* (f: FormModels.Model): View, NEW, ABSTRACT;
	(**
		f # NIL	20
		f.init	21
	**)


	(* ViewOp *)

	PROCEDURE (op: ViewOp) Do;
		VAR border, grid, gridFactor: INTEGER; background: Ports.Color;
	BEGIN
		(* save old state of view *)
		border := op.view.border; grid := op.view.grid; gridFactor := op.view.gridFactor;
		background := op.view.background;
		(* set new state of view *)
		op.view.border := op.border; op.view.grid := op.grid; op.view.gridFactor := op.gridFactor;
		op.view.background := op.background;
		Views.Update(op.view, Views.keepFrames);
		(* old state is new undo state *)
		op.border := border; op.grid := grid; op.gridFactor := gridFactor;
		op.background := background
	END Do;


	(* StdView *)

	PROCEDURE (v: StdView) Internalize2 (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER;
	BEGIN
		v.Internalize2^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, maxStdVersion, thisVersion);
			IF thisVersion # 0 THEN
				rd.ReadInt(v.border);
				rd.ReadInt(v.grid);
				rd.ReadInt(v.gridFactor);
				rd.ReadInt(v.background)
			END
		END
	END Internalize2;

	PROCEDURE (v: StdView) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		v.Externalize2^(wr);
		wr.WriteVersion(maxStdVersion);
		wr.WriteInt(v.border);
		wr.WriteInt(v.grid);
		wr.WriteInt(v.gridFactor);
		wr.WriteInt(v.background)
	END Externalize2;

	PROCEDURE (v: StdView) CopyFromModelView2 (source: Views.View; model: Models.Model);
	BEGIN
		WITH source: StdView DO
			v.border := source.border;
			v.grid := source.grid;
			v.gridFactor := source.gridFactor;
			v.background := source.background
		END
	END CopyFromModelView2;

	PROCEDURE (d: StdView) AcceptableModel (m: Containers.Model): BOOLEAN;
	BEGIN
		RETURN m IS FormModels.Model
	END AcceptableModel;
	
	PROCEDURE (v: StdView) InitModel2 (m: Containers.Model);
	BEGIN
		ASSERT(m IS FormModels.Model, 23)
	END InitModel2;

	PROCEDURE (v: StdView) GetRect (f: Views.Frame; view: Views.View; OUT l, t, r, b: INTEGER);
	BEGIN
		view.context(FormModels.Context).GetRect(l, t, r, b)
	END GetRect;

	PROCEDURE (v: StdView) SetBorder (border: INTEGER);
		VAR op: ViewOp;
	BEGIN
		ASSERT(border >= 0, 20);
		IF border < minBorder THEN
			border := minBorder
		ELSIF border > maxBorder THEN
			border := maxBorder
		END;
		NEW(op); op.view := v; op.border := border;
		op.grid := v.grid; op.gridFactor := v.gridFactor;
		op.background := v.background;
		Views.Do(v, "#Form:BorderChange", op)
	END SetBorder;

	PROCEDURE (v: StdView) Border (): INTEGER;
	BEGIN
		RETURN v.border
	END Border;

	PROCEDURE (v: StdView) SetGrid (grid, gridFactor: INTEGER);
		VAR op: ViewOp;
	BEGIN
		ASSERT(grid > 0, 20); ASSERT(gridFactor > 0, 21);
		NEW(op); op.view := v; op.border := v.border;
		op.grid := grid; op.gridFactor := gridFactor;
		op.background := v.background;
		Views.Do(v, "#Form:GridChange", op)
	END SetGrid;

	PROCEDURE (v: StdView) Grid (): INTEGER;
	BEGIN
		RETURN v.grid
	END Grid;

	PROCEDURE (v: StdView) GridFactor (): INTEGER;
	BEGIN
		RETURN v.gridFactor
	END GridFactor;

	PROCEDURE (v: StdView) SetBackground (background: Ports.Color);
		VAR op: ViewOp;
	BEGIN
		NEW(op); op.view := v; op.border := v.border;
		op.grid := v.grid; op.gridFactor := v.gridFactor;
		op.background := background;
		Views.Do(v, "#Form:BackgroundChange", op)
	END SetBackground;

	PROCEDURE (v: StdView) GetBackground (VAR color: Ports.Color);
	BEGIN
		IF v.background = Ports.defaultColor THEN
			color := Ports.dialogBackground
		ELSE
			color := v.background
		END
	END GetBackground;

	PROCEDURE (v: StdView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR form: FormModels.Model; ctrl: Containers.Controller;
			focus, q: Views.View; k, w, h, x, y: INTEGER; s: FormModels.Reader;
	BEGIN
		form := v.ThisModel();
		IF form # NIL THEN
			ctrl := v.ThisController();
			IF ctrl # NIL THEN focus := ctrl.ThisFocus() ELSE focus := NIL END;
			s := form.NewReader(v.cache); v.cache := s;
			s.Set(NIL); s.ReadView(q); k := 0;
			WHILE q # NIL DO
				IF (s.r >= l) & (s.b >= t) & (s.l < r) & (s.t < b) THEN
					Views.InstallFrame(f, q, s.l, s.t, k, q = focus)
				END;
				s.ReadView(q); INC(k)
			END
		ELSE
			f.DrawRect(l, t, r, b, Ports.fill, Ports.grey12)
		END;
		IF (ctrl # NIL) & ~(Containers.noCaret IN ctrl.opts) THEN
			k := v.grid * v.gridFactor; ASSERT(k > 0, 100);
			v.context.GetSize(w, h);
			IF w > maxSize THEN w := maxSize END;
			IF h > maxSize THEN h := maxSize END;
			x := l - l MOD k;
			WHILE x <= w DO
				f.MarkRect(x, 0, x + f.unit, h, Ports.fill, Ports.dim50, Ports.show);
				INC(x, k)
			END;
			y := t - t MOD k;
			WHILE y <= h DO
				f.MarkRect(0, y, w, y + f.unit, Ports.fill, Ports.dim50, Ports.show);
				INC(y, k)
			END
		END
	END Restore;

	PROCEDURE (v: StdView) HandleModelMsg2 (VAR msg: Models.Message);
	BEGIN
		WITH msg: Models.UpdateMsg DO
			WITH msg: FormModels.UpdateMsg DO
				Views.UpdateIn(v, msg.l, msg.t, msg.r, msg.b, Views.rebuildFrames)
			ELSE
				Views.Update(v, Views.rebuildFrames)	(* catch all update messages *)
			END
		ELSE
		END
	END HandleModelMsg2;

	PROCEDURE GetBounds (v: StdView; VAR w, h: INTEGER);
		VAR form: FormModels.Model; r, b: INTEGER; p: FormModels.Reader; q: Views.View;
	BEGIN
		form := v.ThisModel();
		IF form # NIL THEN
			p := form.NewReader(v.cache); v.cache := p;
			p.Set(NIL);	(* set reader to bottom of view list *)
			p.ReadView(q);	(* read bottom-most view *)
			IF q # NIL THEN
				r := 0; b := 0;
				WHILE q # NIL DO
					IF p.r > r THEN r := p.r END;
					IF p.b > b THEN b := p.b END;
					p.ReadView(q)
				END;
				w := r + v.border; h := b + v.border
			ELSE
				w := 0; h := 0
			END
		ELSE
			w := 0; h := 0
		END
	END GetBounds;

	PROCEDURE AssertRange (border: INTEGER; VAR w, h: INTEGER);
		VAR min: INTEGER;
	BEGIN	(* prevent illegal values *)
		min := 2 * border + FormModels.minViewSize;
		IF w = Views.undefined THEN w := 100 * Ports.mm
		ELSIF w < min THEN w := min
		ELSIF w > maxSize THEN w := maxSize
		END;
		IF h = Views.undefined THEN h := 70 * Ports.mm
		ELSIF h < min THEN h := min
		ELSIF h > maxSize THEN h := maxSize
		END
	END AssertRange;

	PROCEDURE (v: StdView) HandlePropMsg2 (VAR p: Properties.Message);
		VAR sp: Properties.StdProp; q: Properties.Property;
	BEGIN
		WITH p: Properties.BoundsPref DO
			GetBounds(v, p.w, p.h)
		| p: Properties.SizePref DO
			AssertRange(v.border, p.w, p.h)
		| p: Properties.PollMsg DO
			NEW(sp); sp.valid := {Properties.color}; sp.known := sp.valid;
			sp.color.val := v.background; p.prop := sp
		| p: Properties.SetMsg DO
			q := p.prop;
			WHILE q # NIL DO
				WITH q: Properties.StdProp DO
					IF (Properties.color IN q.valid) & (q.color.val # v.background) THEN
						v.SetBackground(q.color.val)
					END;
				ELSE
				END;
				q :=q.next
			END
		| p: Containers.DropPref DO
			p.okToDrop := TRUE
		ELSE
		END
	END HandlePropMsg2;


	(* StdDirectory *)

	PROCEDURE (d: StdDirectory) New (f: FormModels.Model): View;
		VAR v: StdView; grid, gridFactor: INTEGER;
	BEGIN
		ASSERT(f # NIL, 20);
		NEW(v); v.InitModel(f);
		IF ctrldir # NIL THEN v.SetController(ctrldir.New()) END;
		v.SetBorder(minBorder);
		IF Dialog.metricSystem THEN
			grid := 2 * Ports.mm; gridFactor := 5	(* place at 2 mm resolution *)
		ELSE
			grid := Ports.inch DIV 16; gridFactor := 8	(* place at 1/16 inch resolution *)
		END;
		v.SetGrid(grid, gridFactor);
		v.background := Ports.defaultColor;
		RETURN v
	END New;


	(** miscellaneous **)

	PROCEDURE Focus* (): View;
		VAR v: Views.View;
	BEGIN
		v := Controllers.FocusView();
		IF (v # NIL) & (v IS View) THEN RETURN v(View) ELSE RETURN NIL END
	END Focus;

	PROCEDURE FocusModel* (): FormModels.Model;
		VAR v: View;
	BEGIN
		v := Focus();
		IF v # NIL THEN RETURN v.ThisModel() ELSE RETURN NIL END
	END FocusModel;

	PROCEDURE RoundToGrid* (v: View; VAR x, y: INTEGER);
		VAR grid: INTEGER;
	BEGIN
		grid := v.Grid();
		x := x + grid DIV 2;
		y := y + grid DIV 2;
		x := x - x MOD grid;
		y := y - y MOD grid
	END RoundToGrid;

	PROCEDURE New* (): View;
	BEGIN
		RETURN dir.New(FormModels.dir.New())
	END New;

	PROCEDURE Deposit*;
	BEGIN
		Views.Deposit(New())
	END Deposit;

	PROCEDURE SetDir* (d: Directory);
	BEGIN
		ASSERT(d # NIL, 20); dir := d
	END SetDir;

	PROCEDURE SetCtrlDir* (d: Containers.Directory);
	BEGIN
		ASSERT(d # NIL, 20); ctrldir := d
	END SetCtrlDir;

	PROCEDURE Init;
		VAR d: StdDirectory; res: INTEGER;
	BEGIN
		Dialog.Call("FormControllers.Install", "#Form:CntrlInstallFailed", res);
		NEW(d); dir := d; stdDir := d
	END Init;

BEGIN
	Init
END FormViews.
