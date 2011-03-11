MODULE FormControllers;
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
		Services, Ports, Stores, Models, Views, Controllers, Properties, Containers,
		FormModels, FormViews;

	CONST
		noSelection* = Containers.noSelection; noFocus* = Containers.noFocus;
		rdel = 7X; ldel = 8X;
		arrowLeft = 1CX; arrowRight = 1DX; arrowUp = 1EX; arrowDown = 1FX;
		(* range of currently supported versions *)
		minVersion = 0; maxBaseVersion = 0; maxStdVersion = 0;

	TYPE
		Controller* = POINTER TO ABSTRACT RECORD (Containers.Controller)
			form-: FormModels.Model;
			view-: FormViews.View
		END;

		Directory* = POINTER TO ABSTRACT RECORD (Containers.Directory) END;

		List* = POINTER TO RECORD
			next*: List;
			view*: Views.View
		END;


		StdController = POINTER TO RECORD (Controller)
			sel: List;	(* (sel = NIL) OR (c.ThisFocus() = NIL) *)
			reader: FormModels.Reader;
			lastX, lastY: INTEGER
		END;

		StdDirectory = POINTER TO RECORD (Directory) END;

		MarkMsg = RECORD (Views.Message)
			list: List;
			show: BOOLEAN
		END;

		PollFocusMsg = RECORD (Controllers.PollFocusMsg)
			c: Controller
		END;


	VAR dir-, stdDir-: Directory;


	(** Controller **)

	PROCEDURE (c: Controller) Internalize2- (VAR rd: Stores.Reader), EXTENSIBLE;
		VAR thisVersion: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxBaseVersion, thisVersion)
	END Internalize2;

	PROCEDURE (c: Controller) Externalize2- (VAR wr: Stores.Writer), EXTENSIBLE;
	BEGIN
		wr.WriteVersion(maxBaseVersion)
	END Externalize2;

	PROCEDURE (c: Controller) InitView2* (view: Views.View), EXTENSIBLE;
	BEGIN
		IF view # NIL THEN
			ASSERT(view IS FormViews.View, 25);
			c.view := view(FormViews.View); c.form := c.view.ThisModel()
		ELSE
			c.form := NIL; c.view := NIL
		END
	END InitView2;

	PROCEDURE (c: Controller) ThisView* (): FormViews.View, EXTENSIBLE;
	BEGIN
		RETURN c.view
	END ThisView;

	PROCEDURE (c: Controller) Select* (view: Views.View), NEW, ABSTRACT;

	PROCEDURE (c: Controller) Deselect* (view: Views.View), NEW, ABSTRACT;

	PROCEDURE (c: Controller) IsSelected* (view: Views.View): BOOLEAN, NEW, ABSTRACT;

	PROCEDURE (c: Controller) GetSelection* (): List, NEW, ABSTRACT;

	PROCEDURE (c: Controller) SetSelection* (l: List), NEW, ABSTRACT;


	(** Directory **)

	PROCEDURE (d: Directory) NewController* (opts: SET): Controller, ABSTRACT;

	PROCEDURE (d: Directory) New* (): Controller, EXTENSIBLE;
	BEGIN
		RETURN d.NewController({})
	END New;


	(* auxiliary procedures *)

	PROCEDURE MarkList (c: StdController; f: Views.Frame; h: List; show: BOOLEAN);
		VAR l, t, r, b, s: INTEGER;
	BEGIN
		IF ~(Containers.noSelection IN c.opts) THEN
			s := 2 * f.unit;
			WHILE h # NIL DO
				c.view.GetRect(f, h.view, l, t, r, b);
				f.MarkRect(l - s, t - s, r + s, b + s, s, Ports.hilite, show);
				h := h.next
			END
		END
	END MarkList;

	PROCEDURE Toggle (c: StdController; view: Views.View);
	BEGIN
		IF c.IsSelected(view) THEN c.Deselect(view) ELSE c.Select(view) END
	END Toggle;


	(* StdController *)

	PROCEDURE (c: StdController) Internalize2 (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER;
	BEGIN
		c.Internalize2^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, maxStdVersion, thisVersion);
			c.sel := NIL; c.lastX := -1
		END
	END Internalize2;

	PROCEDURE (c: StdController) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		c.Externalize2^(wr);
		wr.WriteVersion(maxStdVersion)
	END Externalize2;

	PROCEDURE (c: StdController) CopyFrom (source: Stores.Store);
	BEGIN
		c.CopyFrom^(source);
		c.sel := NIL; c.lastX := -1
	END CopyFrom;

	PROCEDURE (c: StdController) RestoreMarks2 (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		IF (c.lastX <= f.l ) OR (c.lastX >= f.r) OR (c.lastY <= f.t) OR (c.lastY >= f.b) THEN
			c.lastX := (f.l + f.r) DIV 2; c.lastY := (f.t + f.b) DIV 2
		END
	END RestoreMarks2;

	PROCEDURE (c: StdController) HandleViewMsg (f: Views.Frame; VAR msg: Views.Message);
	BEGIN
		WITH msg: MarkMsg DO
			MarkList(c, f, msg.list, msg.show)
		ELSE
			c.HandleViewMsg^(f, msg)
		END
	END HandleViewMsg;

	PROCEDURE (c: StdController) HandleCtrlMsg (f: Views.Frame; VAR msg: Views.CtrlMessage;
																			VAR focus: Views.View);
	BEGIN
		WITH msg: PollFocusMsg DO
			c.HandleCtrlMsg^(f, msg, focus);
			msg.c := c
		ELSE
			c.HandleCtrlMsg^(f, msg, focus)
		END
	END HandleCtrlMsg;


	(* subclass hooks *)

	PROCEDURE (c: StdController) GetContextType (OUT type: Stores.TypeName);
	BEGIN
		type := "FormViews.View"
	END GetContextType;

	PROCEDURE (c: StdController) GetValidOps (OUT valid: SET);
	BEGIN
		valid := {Controllers.pasteChar, Controllers.paste};
		IF c.sel # NIL THEN valid := valid + {Controllers.cut, Controllers.copy} END
	END GetValidOps;

	PROCEDURE (c: StdController) NativeModel (model: Models.Model): BOOLEAN;
	BEGIN
		ASSERT(model # NIL, 20);
		RETURN model IS FormModels.Model
	END NativeModel;

	PROCEDURE (c: StdController) NativeView (view: Views.View): BOOLEAN;
	BEGIN
		ASSERT(view # NIL, 20);
		RETURN view IS FormViews.View
	END NativeView;

	PROCEDURE (c: StdController) NativeCursorAt (f: Views.Frame; x, y: INTEGER): INTEGER;
	BEGIN
		ASSERT(f # NIL, 20);
		RETURN Ports.graphicsCursor
	END NativeCursorAt;

	PROCEDURE (c: StdController) MakeViewVisible (v: Views.View);
		VAR l, t, r, b: INTEGER;
	BEGIN
		ASSERT(v # NIL, 20);
		ASSERT(v.context # NIL, 21);
		ASSERT(v.context.ThisModel() = c.form, 22);
		v.context(FormModels.Context).GetRect(l, t, r, b);
		c.view.context.MakeVisible(l, t, r, b)
	END MakeViewVisible;

	PROCEDURE (c: StdController) GetFirstView (selection: BOOLEAN; OUT v: Views.View);
		VAR rd: FormModels.Reader;
	BEGIN
		IF selection THEN
			IF c.sel # NIL THEN v := c.sel.view ELSE v := NIL END
		ELSE
			rd := c.form.NewReader(c.reader); c.reader := rd;
			rd.ReadView(v)
		END
	END GetFirstView;

	PROCEDURE (c: StdController) GetNextView (selection: BOOLEAN; VAR v: Views.View);
		VAR h: List; rd: FormModels.Reader;
	BEGIN	(* could be optimized *)
		ASSERT(v # NIL, 20);
		IF selection THEN
			h := c.sel; WHILE (h # NIL) & (h.view # v) DO h := h.next END; ASSERT(h # NIL, 21);
			IF h.next # NIL THEN v := h.next.view ELSE v := NIL END
		ELSE
			rd := c.form.NewReader(c.reader); c.reader := rd;
			rd.Set(v); rd.ReadView(v)
		END
	END GetNextView;

	PROCEDURE (c: StdController) GetSelectionBounds (f: Views.Frame; OUT x, y, w, h: INTEGER);
		VAR g: Views.Frame; sel: List; l, t, r, b, gw, gh, border: INTEGER;
	BEGIN
		IF c.Singleton() # NIL THEN
			c.GetSelectionBounds^(f, x, y, w, h)
		ELSE
			l := MAX(INTEGER); t := MAX(INTEGER); r := MIN(INTEGER); b := MIN(INTEGER);
			sel := c.sel;
			WHILE sel # NIL DO
				g := Views.ThisFrame(f, sel.view);
				IF g # NIL THEN
					sel.view.context.GetSize(gw, gh);
					IF g.gx < l THEN l := g.gx END;
					IF g.gy < t THEN t := g.gy END;
					IF g.gx + gw > r THEN r := g.gx + gw END;
					IF g.gy + gh > b THEN b := g.gy + gh END;
				END;
				sel := sel.next
			END;
			IF (l < r) & (t < b) THEN
				border := c.view.Border();
				x := l - f.gx - border; y := t - f.gy - border;
				w := r - l + 2 * border; h := b - t + 2 * border
			ELSE
				x := 0; y := 0; w := 0; h := 0
			END
		END
	END GetSelectionBounds;
	
	PROCEDURE (c: StdController) MarkDropTarget (src, dst: Views.Frame;
																			sx, sy, dx, dy, w, h, rx, ry: INTEGER;
																			type: Stores.TypeName;
																			isSingle, show: BOOLEAN);
		CONST dm = 4 * Ports.point; dp = 18 * Ports.point;
		VAR vx, vy, l, t, r, b: INTEGER; sc: Containers.Controller; s: Views.View;
	BEGIN	(* cf. Drop *)
		IF ~isSingle & (src # NIL) & (src.view IS FormViews.View) THEN	(* mark local form selection *)
			vx := dx - sx; vy := dy - sy;
			sc := src.view(FormViews.View).ThisController();
			IF sc # NIL THEN
				WITH sc: Controller DO
					sc.GetFirstView(Containers.selection, s);
					WHILE s # NIL DO
						s.context(FormModels.Context).GetRect(l, t, r, b); w := r - l; h := b - t;
						INC(l, vx); INC(t, vy); FormViews.RoundToGrid(c.view, l, t);
						dst.MarkRect(l, t, l + w, t + h, 0, Ports.invert, show);
						sc.GetNextView(Containers.selection, s)
					END
				END
			END
		ELSIF (w > 0) & (h > 0) THEN	(* mark non-local form or singleton selection *)
			vx := dx - rx; vy := dy - ry;
			FormViews.RoundToGrid(c.view, vx, vy);
			IF ~isSingle & Services.Extends(type, "FormViews.View") THEN	(* general form selection *)
				dst.MarkRect(vx - dm, vy, vx + dp, vy + dst.unit, 0, Ports.invert, show);
				dst.MarkRect(vx, vy - dm, vx + dst.unit, vy + dp, 0, Ports.invert, show);
				INC(vx, w); INC(vy, h);
				dst.MarkRect(vx - dp, vy, vx + dm, vy + dst.unit, 0, Ports.invert, show);
				dst.MarkRect(vx, vy - dp, vx + dst.unit, vy + dm, 0, Ports.invert, show)
			ELSE	(* singleton selection *)
				dst.MarkRect(vx, vy, vx + w, vy + h, 0, Ports.invert, show)
			END
		ELSE	(* cross-hair mark for non-form, non-singleton selections *)
			FormViews.RoundToGrid(c.view, dx, dy);
			dst.MarkRect(dx - dm, dy, dx + dp, dy + dst.unit, 0, Ports.invert, show);
			dst.MarkRect(dx, dy - dm, dx + dst.unit, dy + dp, 0, Ports.invert, show)
		END
	END MarkDropTarget;

	PROCEDURE (c: StdController) TrackMarks (f: Views.Frame; x, y: INTEGER;
																			units, extend, add: BOOLEAN);
		VAR dx, dy, x0, y0, dx0, dy0: INTEGER; isDown: BOOLEAN; h: Views.View; m: SET;

		PROCEDURE InvertRect (f: Views.Frame; x, y, dx, dy: INTEGER);
			VAR l, t, r, b: INTEGER;
		BEGIN
			IF dx >= 0 THEN l := x; r := x + dx ELSE l := x + dx; r := x END;
			IF dy >= 0 THEN t := y; b := y + dy ELSE t := y + dy; b := y END;
			f.MarkRect(l, t, r, b, 0, Ports.dim50, TRUE)
		END InvertRect;

		PROCEDURE SelectArea (c: StdController; l, t, r, b: INTEGER; toggle: BOOLEAN);
			VAR h: INTEGER; rd: FormModels.Reader; v: Views.View; p, q: List; empty: BOOLEAN;
		BEGIN
			IF l > r THEN h := l; l := r; r := h END;
			IF t > b THEN h := t; t := b; b := h END;
			rd := c.form.NewReader(c.reader); c.reader := rd;
			rd.ReadView(v); p := NIL; empty := c.sel = NIL;
			WHILE v # NIL DO
				IF (rd.l < r) & (rd.t < b) & (rd.r > l) & (rd.b > t) THEN
					IF toggle THEN Toggle(c, v)
					ELSIF ~empty THEN c.Select(v)
					ELSE NEW(q); q.next := p; p := q; q.view := v
					END
				END;
				rd.ReadView(v)
			END;
			(* this optimization prevents the appearance of a temporary singleton *)
			IF ~toggle & empty THEN c.SetSelection(p) END
		END SelectArea;

	BEGIN
		dx := 0; dy := 0;	(* vector from (x, y) *)
		InvertRect(f, x, y, dx, dy);
		REPEAT
			f.Input(x0, y0, m, isDown);
			dx0 := x0 - x; dy0 := y0 - y;
			IF (dx0 # dx) OR (dy0 # dy) THEN
				InvertRect(f, x, y, dx, dy);
				dx := dx0; dy := dy0;
				InvertRect(f, x, y, dx, dy)
			END
		UNTIL ~isDown;
		InvertRect(f, x, y, dx, dy);
		c.lastX := x0; c.lastY := y0;
		IF (dx # 0) OR (dy # 0) THEN
			SelectArea(c, x, y, x + dx, y + dy, extend OR add)
		ELSE
			h := c.form.ViewAt(x, y);
			IF h # NIL THEN
				IF extend OR add THEN Toggle(c, h) ELSE c.Select(h) END
			END
		END
	END TrackMarks;

	PROCEDURE (c: StdController) Resize (view: Views.View; l, t, r, b: INTEGER);
	BEGIN
		c.form.Resize(view, l, t, r, b)
	END Resize;

	PROCEDURE (c: StdController) DeleteSelection;
		VAR script: Stores.Operation; h: List;
	BEGIN
		Models.BeginScript(c.form, "#System:Deletion", script);
		h := c.sel; WHILE h # NIL DO c.form.Delete(h.view); h := h.next END;
		Models.EndScript(c.form, script)
	END DeleteSelection;

	PROCEDURE MoveRelativeLocalSel (c: StdController; src, dst: Views.Frame;
															dx, dy: INTEGER; grid: BOOLEAN);
		VAR script: Stores.Operation; sel, h: List; l, t, r, b, newl, newt: INTEGER;
	BEGIN
		IF (dx # 0) OR (dy # 0) THEN
			sel := c.GetSelection();
			Models.BeginScript(c.form, "#System:Moving", script);
			h := sel;
			WHILE h # NIL DO
				h.view.context(FormModels.Context).GetRect(l, t, r, b);
				newl := l + dx; newt := t + dy;
				IF grid THEN FormViews.RoundToGrid(c.view, newl, newt) END;
				c.form.Move(h.view, newl - l, newt - t);
				h := h.next
			END;
			Models.EndScript(c.form, script);
			c.SetSelection(sel)
		END
	END MoveRelativeLocalSel;

	PROCEDURE (c: StdController) MoveLocalSelection (src, dst: Views.Frame; sx, sy,
																									dx, dy: INTEGER);
	BEGIN
		MoveRelativeLocalSel(c, src, dst,  dx - sx, dy - sy, TRUE)
	END MoveLocalSelection;

	PROCEDURE (c: StdController) CopyLocalSelection (src, dst: Views.Frame; sx, sy,
																						dx, dy: INTEGER);
		VAR script: Stores.Operation; q: Views.View; h, s, t: List;
	BEGIN
		dx := dx - sx; dy := dy - sy;
		IF (dx # 0) OR (dy # 0) THEN
			FormViews.RoundToGrid(c.view, dx, dy);
			Models.BeginScript(c.form, "#System:Copying", script);
			h := c.GetSelection(); s := NIL;
			WHILE h # NIL DO
				q := h.view; c.form.Copy(q, dx, dy);
				NEW(t); t.next := s; s := t; t.view := q;
				h := h.next
			END;
			Models.EndScript(c.form, script);
			c.SetSelection(s)
		END
	END CopyLocalSelection;

	PROCEDURE (c: StdController) SelectionCopy (): Containers.Model;
		VAR f: FormModels.Model; rd: FormModels.Reader; wr: FormModels.Writer;
			v: Views.View; dx, dy: INTEGER;

		PROCEDURE GetOffset (p: List; border: INTEGER; OUT dx, dy: INTEGER);
			VAR l, t, vl, vt, vr, vb: INTEGER;
		BEGIN
			IF p # NIL THEN
				l := MAX(INTEGER); t := MAX(INTEGER);
				WHILE p # NIL DO
					p.view.context(FormModels.Context).GetRect(vl, vt, vr, vb);
					IF vl < l THEN l := vl END;
					IF vt < t THEN t := vt END;
					p := p.next
				END;
				dx := l - border; dy := t - border
			ELSE
				dx := 0; dy := 0
			END
		END GetOffset;

	BEGIN
		f := FormModels.CloneOf(c.form);
		GetOffset(c.sel, c.view.Border(), dx, dy);
		rd := c.form.NewReader(NIL); wr := f.NewWriter(NIL);
		rd.ReadView(v);
		WHILE v # NIL DO
			IF c.IsSelected(v) THEN
				wr.WriteView(Views.CopyOf(v, Views.deep), rd.l - dx, rd.t - dy, rd.r - dx, rd.b - dy)
			END;
			rd.ReadView(v)
		END;
		RETURN f
	END SelectionCopy;

	PROCEDURE (c: StdController) NativePaste (m: Models.Model; f: Views.Frame);
		VAR x, y, cw, ch: INTEGER; v: Views.View; rd: FormModels.Reader;
			wr: FormModels.Writer; n, i: INTEGER; script: Stores.Operation;
	BEGIN
		x := c.lastX; y := c.lastY;
		c.view.context.GetSize(cw, ch);
		IF (x <= f.l) OR (x >= f.r) OR (y <= f.t) OR (y >= f.b) THEN
			x := (f.l + f.r) DIV 2; y := (f.r + f.b) DIV 2
		END;
		c.lastX := x; c.lastY := y;
		FormViews.RoundToGrid(c.view, x, y);
		WITH m: FormModels.Model DO
			Models.BeginScript(c.form, "#System:Insertion", script);
			rd := m.NewReader(NIL); wr := c.form.NewWriter(NIL); wr.Set(c.form.Top());
			rd.ReadView(v); n := 0;
			WHILE v # NIL DO
				wr.WriteView(Views.CopyOf(v, Views.shallow), x + rd.l, y + rd.t, x + rd.r, y + rd.b);
				INC(n);
				rd.ReadView(v)
			END;
			Models.EndScript(c.form, script);
			(* n views have been inserted at the top => select them *)
			c.SelectAll(Containers.deselect);
			i := c.form.NofViews() - n; ASSERT(i >= 0, 100);
			rd := c.form.NewReader(rd);
			WHILE i # 0 DO rd.ReadView(v); DEC(i) END;	(* skip old views *)
			WHILE n # 0 DO rd.ReadView(v); c.Select(v); DEC(n) END
		END
	END NativePaste;

	PROCEDURE (c: StdController) ArrowChar (f: Views.Frame; ch: CHAR; units,  select: BOOLEAN);
		VAR d: INTEGER;
	BEGIN
		IF units THEN d := c.view.Grid() ELSE d := f.unit END;
		IF ch = arrowLeft THEN
			MoveRelativeLocalSel(c, f, f, -d, 0, units)
		ELSIF ch = arrowRight THEN
			MoveRelativeLocalSel(c, f, f, +d, 0, units)
		ELSIF ch = arrowUp THEN
			MoveRelativeLocalSel(c, f, f, 0, -d, units)
		ELSIF ch = arrowDown THEN
			MoveRelativeLocalSel(c, f, f, 0, +d, units)
		END
	END ArrowChar;

	PROCEDURE (c: StdController) ControlChar (f: Views.Frame; ch: CHAR);
	BEGIN
		IF (ch = ldel) OR (ch = rdel) THEN c.DeleteSelection END
	END ControlChar;

	PROCEDURE (c: StdController) PasteChar (ch: CHAR);
	END PasteChar;

	PROCEDURE (c: StdController) PasteView (f: Views.Frame; v: Views.View; w, h: INTEGER);
		VAR minW, maxW, minH, maxH, x, y: INTEGER;
	BEGIN
		x := c.lastX; y := c.lastY;
		IF (x <= f.l) OR (x >= f.r) OR (y <= f.t) OR (y >= f.b) THEN
			x := (f.l + f.r) DIV 2; y := (f.t + f.b) DIV 2
		END;
		c.lastX := x; c.lastY := y;
		FormViews.RoundToGrid(c.view, x, y);
		c.form.GetEmbeddingLimits(minW, maxW, minH, maxH);
		Properties.PreferredSize(v, minW, maxW, minH, maxH, minW, minH, w, h);
		c.form.Insert(v, x, y, x + w, y + h); c.Select(v)
	END PasteView;

	PROCEDURE (c: StdController) Drop (src, dst: Views.Frame; sx, sy, dx, dy,
															w, h, rx, ry: INTEGER; v: Views.View; isSingle: BOOLEAN);
		VAR minW, maxW, minH, maxH, l, t, sw, sh: INTEGER;
			s: Views.View; p, q: List;
			m: FormModels.Model; rd: FormModels.Reader; wr: FormModels.Writer;
	BEGIN	(* cf. MarkDropTarget *)
		DEC(dx, rx); DEC(dy, ry);
		IF ~isSingle & (v IS FormViews.View) THEN
			m := v(FormViews.View).ThisModel();
			rd := m.NewReader(NIL); wr := c.form.NewWriter(NIL);
			rd.ReadView(s); p := NIL;
			WHILE s # NIL DO
				l := rd.l + dx; t := rd.t + dy; sw := rd.r - rd.l; sh := rd.b - rd.t;
				FormViews.RoundToGrid(c.view, l, t);
				s := Views.CopyOf(s, Views.shallow);
				wr.WriteView(s, l, t, l + sw, t + sh);
				NEW(q); q.next := p; p := q; q.view := s;
				rd.ReadView(s)
			END;
			c.SetSelection(p)
		ELSE
			FormViews.RoundToGrid(c.view, dx, dy);
			c.form.GetEmbeddingLimits(minW, maxW, minH, maxH);
			Properties.PreferredSize(v, minW, maxW, minH, maxH, minW, minH, w, h);
			c.form.Insert(v, dx, dy, dx + w, dy + h); c.Select(v)
		END
	END Drop;


	(* selection *)

	PROCEDURE (c: StdController) HasSelection (): BOOLEAN;
	BEGIN
		RETURN c.sel # NIL
	END HasSelection;

	PROCEDURE (c: StdController) Selectable (): BOOLEAN;
	BEGIN
		RETURN c.form.NofViews() # 0
	END Selectable;

	PROCEDURE (c: StdController) SetSingleton (s: Views.View);
		VAR l: List;
	BEGIN
		c.SetSingleton^(s);
		IF s # NIL THEN NEW(l); l.view := s; c.sel := l ELSE c.sel := NIL END
	END SetSingleton;

	PROCEDURE (c: StdController) SelectAll (select: BOOLEAN);
		VAR s: FormModels.Reader; v: Views.View; l, h: List; msg: MarkMsg;
	BEGIN
		IF select THEN
			ASSERT(~(Containers.noSelection IN c.opts), 20);
			c.SetFocus(NIL);
			s := c.form.NewReader(c.reader); c.reader := s;
			s.Set(NIL); s.ReadView(v);
			IF c.form.NofViews() = 1 THEN
				ASSERT(v # NIL, 100);
				c.SetSingleton(v)
			ELSE
				IF (c.sel # NIL) & (c.sel.next = NIL) THEN c.SetSingleton(NIL) END;
				l := NIL;
				WHILE v # NIL DO
					IF ~c.IsSelected(v) THEN NEW(h); h.next := l; l := h; h.view := v END;
					s.ReadView(v)
				END;
				msg.list := l;
				h := c.sel; WHILE (h # NIL) & (h.next # NIL) DO h := h.next END;
				IF h = NIL THEN c.sel := l ELSE h.next := l END;
				IF msg.list # NIL THEN msg.show := TRUE; Views.Broadcast(c.view, msg) END
			END
		ELSIF c.sel # NIL THEN
			IF c.sel.next = NIL THEN	(* singleton *)
				c.SetSingleton(NIL)
			ELSE
				msg.list := c.sel; c.sel := NIL; 
				msg.show := FALSE; Views.Broadcast(c.view, msg)
			END
		END
	END SelectAll;

	PROCEDURE (c: StdController) InSelection (f: Views.Frame; x, y: INTEGER): BOOLEAN;
		VAR g: Views.Frame;
	BEGIN
		g := Views.FrameAt(f, x, y);
		IF g # NIL THEN
			RETURN c.IsSelected(g.view)
		ELSE
			RETURN FALSE
		END
	END InSelection;

	PROCEDURE (c: StdController) MarkSelection (f: Views.Frame; show: BOOLEAN);
	BEGIN
		IF c.sel = NIL THEN	(* skip *)
		ELSIF c.sel.next = NIL THEN
			Containers.MarkSingleton(c, f, show)
		ELSE
			MarkList(c, f, c.sel, show)
		END
	END MarkSelection;


	(* caret *)

	PROCEDURE (c: StdController) HasCaret (): BOOLEAN;
	BEGIN
		RETURN TRUE
	END HasCaret;

	PROCEDURE (c: StdController) MarkCaret (f: Views.Frame; show: BOOLEAN);
	END MarkCaret;


	(* FormController protocol *)

	PROCEDURE (c: StdController) Select (view: Views.View);
		VAR l, h, sel: List; msg: MarkMsg;
	BEGIN
		ASSERT(view # NIL, 20); ASSERT(view.context.ThisModel() = c.form, 21);
		ASSERT(~(Containers.noSelection IN c.opts), 22);
		l := c.sel; WHILE (l # NIL) & (l.view # view) DO l := l.next END;
		IF l = NIL THEN	(* view is not yet selected *)
			sel := c.sel;
			IF sel = NIL THEN
				c.SetSingleton(view)
			ELSE
				NEW(l); l.view := view;
				IF sel.next = NIL THEN
					c.SetSingleton(NIL); ASSERT(c.sel = NIL, 100);
					l.next := sel; c.sel := l;
					msg.list := l
				ELSE
					l.next := sel; c.sel := l;
					NEW(h); h.view := view;
					msg.list := h
				END;
				msg.show := TRUE; Views.Broadcast(c.view, msg)
			END
		END
	END Select;

	PROCEDURE (c: StdController) Deselect (view: Views.View);
		VAR l, h: List; msg: MarkMsg;
	BEGIN
		ASSERT(view # NIL, 20); ASSERT(view.context.ThisModel() = c.form, 21);
		l := c.sel; h := NIL; WHILE (l # NIL) & (l.view # view) DO h := l; l := l.next END;
		IF l # NIL THEN	(* l is selection node of view, h its predecessor *)
			IF (h = NIL) & (l.next = NIL) THEN	(* singleton *)
				c.SetSingleton(NIL)
			ELSE
				IF h = NIL THEN c.sel := l.next ELSE h.next := l.next END;
				msg.list:= l; l.next := NIL; msg.show := FALSE; Views.Broadcast(c.view, msg);
				IF (c.sel # NIL) & (c.sel.next = NIL) THEN	(* singleton *)
					view := c.sel.view;
					msg.list := c.sel; c.sel := NIL; 
					msg.show := TRUE; Views.Broadcast(c.view, msg);
					c.SetSingleton(view)
				END
			END
		END
	END Deselect;

	PROCEDURE (c: StdController) IsSelected (view: Views.View): BOOLEAN;
		VAR l: List;
	BEGIN
		IF view # NIL THEN
			ASSERT(view.context.ThisModel() = c.form, 20);
			l := c.sel; WHILE (l # NIL) & (l.view # view) DO l := l.next END;
			RETURN l # NIL
		ELSE
			RETURN FALSE
		END
	END IsSelected;

	PROCEDURE (c: StdController) GetSelection (): List;
		VAR l, h, s: List;
	BEGIN
		l := NIL; s := c.sel;
		WHILE s # NIL DO NEW(h); h.next := l; l := h; h.view := s.view; s := s.next END;
		RETURN l
	END GetSelection;

	PROCEDURE (c: StdController) SetSelection (l: List);
		VAR msg: MarkMsg;
	BEGIN
		c.SelectAll(FALSE); ASSERT(c.sel = NIL, 100);
		IF l = NIL THEN	(* skip *)
		ELSIF l.next = NIL THEN
			c.SetSingleton(l.view)
		ELSE
			msg.list := l; c.sel := l; 
			msg.show := TRUE; Views.Broadcast(c.view, msg)
		END
	END SetSelection;


	(* StdDirectory *)

	PROCEDURE (d: StdDirectory) NewController (opts: SET): Controller;
		VAR c: StdController;
	BEGIN
		NEW(c); c.SetOpts(opts); RETURN c
	END NewController;


	(** miscellaneous **)

	PROCEDURE Focus* (): Controller;
		VAR msg: PollFocusMsg; 
	BEGIN
		msg.c := NIL;
		Controllers.Forward(msg);
		RETURN msg.c
	END Focus;
	
	PROCEDURE Insert* (c: Controller; view: Views.View; l, t, r, b: INTEGER);
		VAR w, h: INTEGER;
	BEGIN
		w := r - l; h := b - t;
		FormViews.RoundToGrid(c.view, l, t);
		c.form.Insert(view, l, t, l + w, t + h)
	END Insert;

	PROCEDURE SetDir* (d: Directory);
	BEGIN
		ASSERT(d # NIL, 20); dir := d
	END SetDir;

	PROCEDURE Install*;
	BEGIN
		FormViews.SetCtrlDir(dir)
	END Install;

	PROCEDURE Init;
		VAR d: StdDirectory;
	BEGIN
		NEW(d); SetDir(d); stdDir := d
	END Init;

BEGIN
	Init
END FormControllers.
