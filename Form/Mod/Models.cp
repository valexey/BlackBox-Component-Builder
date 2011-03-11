MODULE FormModels;
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

	IMPORT Ports, Stores, Models, Views, Properties, Containers;

	CONST
		minViewSize* = 4 * Ports.point;	(** size of smallest embedded view **)
		maxViewSize* = 1000 * Ports.mm;	(** size of largest embedded view **)
		(* range of currently supported versions *)
		minVersion = 0; maxBaseVersion = 0; maxStdVersion = 0;

	TYPE
		(* interface types *)

		Model* = POINTER TO ABSTRACT RECORD (Containers.Model) END;

		Directory* = POINTER TO ABSTRACT RECORD END;

		Context* = POINTER TO ABSTRACT RECORD (Models.Context) END;
		
		
		Reader* = POINTER TO ABSTRACT RECORD
			view*: Views.View;	(** most recently read view **)
			l*, t*, r*, b*: INTEGER	(** bounding box of most recently read view **)
		END;

		Writer* = POINTER TO ABSTRACT RECORD END;


		UpdateMsg* = RECORD (Models.UpdateMsg)
			(** the receiver of this message must not switch on any marks **)
			l*, t*, r*, b*: INTEGER	(** (l < r) & (b < t) **)
		END;


		(* concrete types *)

		StdDirectory = POINTER TO RECORD (Directory) END;

		StdModel = POINTER TO RECORD (Model)
			contexts: StdContext	(* list of views in form, ordered from bottom to top *)
		END;

		StdContext = POINTER TO RECORD (Context)
			next: StdContext;	(* next upper view *)
			form: StdModel;	(* form # NIL *)
			view: Views.View;	(* view # NIL *)
			l, t, r, b: INTEGER	(* (r - l >= minViewSize) & (b - t >= minViewSize) *)
		END;

		StdReader = POINTER TO RECORD (Reader)
			form: StdModel;	(* form # NIL *)
			pos: StdContext	(* next ReadView: read view above pos *)
		END;

		StdWriter = POINTER TO RECORD (Writer)
			form: StdModel;	(* form # NIL *)
			pos: StdContext	(* next WriteView: insert view above pos *)
		END;


		FormOp = POINTER TO RECORD (Stores.Operation)
			del, ins: StdContext;	(* ((del = NIL) # (ins = NIL)) OR (del = ins) *)
			pos: StdContext	(* ins # NIL => next Do: insert ins above pos *)
		END;

		ResizeOp = POINTER TO RECORD (Stores.Operation)
			context: StdContext;	(* context # NIL *)
			l, t, r, b: INTEGER	(* (r - l >= minViewSize) & (b - t >= minViewSize) *)
		END;

		ReplaceViewOp = POINTER TO RECORD (Stores.Operation)
			context: StdContext;	(* context # NIL *)
			view: Views.View	(* view # NIL *)
		END;


	VAR dir-, stdDir-: Directory;	(** (dir # NIL) & (stdDir # NIL) **)


	(** Model **)

	PROCEDURE (f: Model) GetEmbeddingLimits* (OUT minW, maxW,
																				minH, maxH: INTEGER), EXTENSIBLE;
	BEGIN
		minH := minViewSize; minW := minViewSize;
		maxH := maxViewSize; maxW := maxViewSize
	END GetEmbeddingLimits;

	PROCEDURE (f: Model) Insert* (v: Views.View; l, t, r, b: INTEGER), NEW, ABSTRACT;
	(**
		v # NIL	20
		v.init	21
		v.context = NIL	22
		l <= r	23
		t <= b	24
	**)

	PROCEDURE (f: Model) Delete* (v: Views.View), NEW, ABSTRACT;
	(** v in f	20 **)

	PROCEDURE (f: Model) Resize* (v: Views.View; l, t, r, b: INTEGER), NEW, ABSTRACT;
	(**
		v in f	20
		l <= r	21
		t <= b	22
	**)

	PROCEDURE (f: Model) Top* (): Views.View, NEW, ABSTRACT;

	PROCEDURE (f: Model) PutAbove* (v, pos: Views.View), NEW, ABSTRACT;
	(**
		v in f	20
		(pos = NIL) OR (pos in f)	21
	**)

	PROCEDURE (f: Model) Move* (v: Views.View; dx, dy: INTEGER), NEW, ABSTRACT;
	(** v in f	20 **)

	PROCEDURE (f: Model) Copy* (VAR v: Views.View; dx, dy: INTEGER), NEW, ABSTRACT;
	(** v in f	20 **)

	PROCEDURE (f: Model) NewReader* (old: Reader): Reader, NEW, ABSTRACT;

	PROCEDURE (f: Model) NewWriter* (old: Writer): Writer, NEW, ABSTRACT;

	PROCEDURE (f: Model) ViewAt* (x, y: INTEGER): Views.View, NEW, ABSTRACT;

	PROCEDURE (f: Model) NofViews* (): INTEGER, NEW, ABSTRACT;


	(** Directory **)

	PROCEDURE (d: Directory) New* (): Model, NEW, ABSTRACT;


	(** Context **)

	PROCEDURE (c: Context) ThisModel* (): Model, ABSTRACT;

	PROCEDURE (c: Context) GetRect* (OUT l, t, r, b: INTEGER), NEW, ABSTRACT;


	(** Reader **)

	PROCEDURE (rd: Reader) Set* (pos: Views.View), NEW, ABSTRACT;
	(** (pos = NIL) OR (pos in r's form)	20 **)

	PROCEDURE (rd: Reader) ReadView* (OUT v: Views.View), NEW, ABSTRACT;


	(** Writer **)

	PROCEDURE (wr: Writer) Set* (pos: Views.View), NEW, ABSTRACT;
	(** (pos = NIL) OR (pos in w's form)	20 **)

	PROCEDURE (wr: Writer) WriteView* (v: Views.View; l, t, r, b: INTEGER), NEW, ABSTRACT;
	(**
		v # NIL	20
		v.init	21
		v.context = NIL	22
		l <= r	23
		t <= b	24
	**)


	(* StdModel *)

	PROCEDURE ThisContext (f: StdModel; view: Views.View): StdContext;
		VAR c: StdContext;
	BEGIN
		c := f.contexts; WHILE (c # NIL) & (c.view # view) DO c := c.next END;
		RETURN c
	END ThisContext;

	PROCEDURE NewContext (form: StdModel; view: Views.View; l, t, r, b: INTEGER): StdContext;
		VAR c: StdContext;
	BEGIN
		ASSERT(form # NIL, 100); ASSERT(view.context = NIL, 101);
		IF r - l < minViewSize THEN r := l + minViewSize END;
		IF b - t < minViewSize THEN b := t + minViewSize END;
		NEW(c); c.form := form; c.view := view; c.l := l; c.t := t; c.r := r; c.b := b;
		Stores.Join(form, view);
		view.InitContext(c);
		RETURN c
	END NewContext;

	PROCEDURE InsertAbove (c, pos: StdContext);
	BEGIN
		IF pos = NIL THEN
			c.next := NIL; c.form.contexts := c
		ELSE
			c.next := pos.next; pos.next := c
		END
	END InsertAbove;

	PROCEDURE (f: StdModel) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion, l, t, r, b, x: INTEGER; top, h: StdContext; v: Views.View;
	BEGIN
		rd.ReadVersion(minVersion, maxStdVersion, thisVersion);
		IF ~rd.cancelled THEN
			rd.ReadVersion(0, 0, thisVersion); rd.ReadInt(x);	(* backward compatibility with Rel. 1.3 *)
			Views.ReadView(rd, v); top := NIL;
			WHILE v # NIL DO
				rd.ReadInt(l); rd.ReadInt(t); rd.ReadInt(r); rd.ReadInt(b);
				h := NewContext(f, v, l, t, r, b);
				InsertAbove(h, top); top := h;
				Views.ReadView(rd, v)
			END
		END
	END Internalize;

	PROCEDURE (f: StdModel) Externalize (VAR wr: Stores.Writer);
		VAR c: StdContext;
	BEGIN
		wr.WriteVersion(maxStdVersion);
		wr.WriteVersion(0); wr.WriteInt(0);	(* backward compatibility with Rel. 1.3 *)
		c := f.contexts;
		WHILE c # NIL DO
			IF Stores.ExternalizeProxy(c.view) # NIL THEN
				Views.WriteView(wr, c.view);
				wr.WriteInt(c.l); wr.WriteInt(c.t);
				wr.WriteInt(c.r); wr.WriteInt(c.b)
			END;
			c := c.next
		END;
		wr.WriteStore(NIL)
	END Externalize;

	PROCEDURE (f: StdModel) CopyFrom (source: Stores.Store);
		VAR c, top, h: StdContext;
	BEGIN
		WITH source: StdModel DO
			c := source.contexts; top := NIL;
			WHILE c # NIL DO
				h := NewContext(f, Views.CopyOf(c.view, Views.deep), c.l, c.t, c.r, c.b);
				InsertAbove(h, top); top := h;
				c := c.next
			END
		END
	END CopyFrom;

	PROCEDURE (f: StdModel) InitFrom (source: Containers.Model);
	BEGIN
		f.contexts := NIL
	END InitFrom;

	PROCEDURE (f: StdModel) ReplaceView (old, new: Views.View);
		VAR op: ReplaceViewOp; c: StdContext;
	BEGIN
		c := ThisContext(f, old); ASSERT(c # NIL, 20);
		ASSERT(new # NIL, 21); ASSERT((new.context = NIL) OR (new.context = c), 23);
		IF old # new THEN
			(* Stores.InitDomain(new, f.domain); *) Stores.Join(f, new);
			new.InitContext(c);	(* both views share same context *)
			NEW(op); op.context := c; op.view := new;
			Models.Do(f, "#System:ReplaceView", op)
		END
	END ReplaceView;

	PROCEDURE (f: StdModel) Insert (v: Views.View; l, t, r, b: INTEGER);
		VAR op: FormOp; c, h, top: StdContext;
	BEGIN
		ASSERT(v # NIL, 20); ASSERT(v.context = NIL, 22);
		ASSERT(l <= r, 23); ASSERT(t <= b, 24);
		h := f.contexts; top := NIL; WHILE h # NIL DO top := h; h := h.next END;
		c := NewContext(f, v, l, t, r, b);
		NEW(op); op.del := NIL; op.ins := c; op.pos := top;
		Models.Do(f, "#System:Insertion", op)
	END Insert;

	PROCEDURE (f: StdModel) Delete (v: Views.View);
		VAR op: FormOp; c: StdContext;
	BEGIN
		c := ThisContext(f, v); ASSERT(c # NIL, 20);
		NEW(op); op.del := c; op.ins := NIL; op.pos := NIL;
		Models.Do(f, "#System:Deletion", op)
	END Delete;

	PROCEDURE (f: StdModel) Resize (v: Views.View; l, t, r, b: INTEGER);
		VAR op: ResizeOp; c: StdContext;
	BEGIN
		c := ThisContext(f, v); ASSERT(c # NIL, 20);
		ASSERT(r >= l, 21); ASSERT(b >= t, 22);
		IF r - l < minViewSize THEN r := l + minViewSize END;
		IF b - t < minViewSize THEN b := t + minViewSize END;
		NEW(op); op.context := c; op.l := l; op.t := t; op.r := r; op.b := b;
		Models.Do(f, "#System:Resizing", op)
	END Resize;

	PROCEDURE (f: StdModel) Top (): Views.View;
		VAR h, top: StdContext;
	BEGIN
		top := NIL; h := f.contexts;
		WHILE h # NIL DO top := h; h := h.next END;
		IF top # NIL THEN RETURN top.view ELSE RETURN NIL END
	END Top;

	PROCEDURE (f: StdModel) PutAbove (v, pos: Views.View);
		VAR op: FormOp; c, d: StdContext;
	BEGIN
		c := ThisContext(f, v); ASSERT(c # NIL, 20);
		d := ThisContext(f, pos); ASSERT((pos = NIL) OR (d # NIL), 21);
		IF v # pos THEN
			NEW(op); op.del := c; op.ins := c; op.pos := d;
			Models.Do(f, "#Form:ChangeZOrder", op)
		END
	END PutAbove;

	PROCEDURE (f: StdModel) Move (v: Views.View; dx, dy: INTEGER);
		VAR op: ResizeOp; c: StdContext;
	BEGIN
		c := ThisContext(f, v); ASSERT(c # NIL, 20);
		IF (dx # 0) OR (dy # 0) THEN
			NEW(op); op.context := c;
			op.l := c.l + dx; op.t := c.t + dy; op.r := c.r + dx; op.b := c.b + dy;
			Models.Do(f, "#System:Moving", op)
		END
	END Move;

	PROCEDURE (f: StdModel) Copy (VAR v: Views.View; dx, dy: INTEGER);
		VAR op: FormOp; c, h, top: StdContext;
	BEGIN
		c := ThisContext(f, v); ASSERT(c # NIL, 20);
		h := f.contexts; top := NIL; WHILE h # NIL DO top := h; h := h.next END;
		h := NewContext(f, Views.CopyOf(v, Views.deep), c.l + dx, c.t + dy, c.r + dx, c.b + dy);
		NEW(op); op.del := NIL; op.ins := h; op.pos := top;
		Models.Do(f, "#System:Copying", op);
		v := h.view
	END Copy;

	PROCEDURE (f: StdModel) NewReader (old: Reader): Reader;
		VAR r: StdReader;
	BEGIN
		IF (old = NIL) OR ~(old IS StdReader) THEN NEW(r) ELSE r := old(StdReader) END;
		r.view := NIL; r.l := 0; r.t := 0; r.r := 0; r.b := 0;
		r.form := f; r.pos := NIL;
		RETURN r
	END NewReader;

	PROCEDURE (f: StdModel) NewWriter (old: Writer): Writer;
		VAR w: StdWriter;
	BEGIN
		IF (old = NIL) OR ~(old IS StdWriter) THEN NEW(w) ELSE w := old(StdWriter) END;
		w.form := f; w.pos := NIL;
		RETURN w
	END NewWriter;

	PROCEDURE (f: StdModel) ViewAt (x, y: INTEGER): Views.View;
		VAR c, top: StdContext;
	BEGIN
		c := f.contexts; top := NIL;
		WHILE c # NIL DO
			IF (x >= c.l) & (y >= c.t) & (x < c.r) & (y < c.b) THEN top := c END;
			c := c.next
		END;
		IF top = NIL THEN RETURN NIL ELSE RETURN top.view END
	END ViewAt;

	PROCEDURE (f: StdModel) NofViews (): INTEGER;
		VAR c: StdContext; n: INTEGER;
	BEGIN
		n := 0; c := f.contexts; WHILE c # NIL DO INC(n); c := c.next END;
		RETURN n
	END NofViews;


	(* StdContext *)

	PROCEDURE (c: StdContext) ThisModel (): Model;
	BEGIN
		RETURN c.form
	END ThisModel;

	PROCEDURE (c: StdContext) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.r - c.l; h := c.b - c.t
	END GetSize;

	PROCEDURE (c: StdContext) SetSize (w, h: INTEGER);
		VAR w0, h0: INTEGER;
	BEGIN
		w0 := c.r - c.l; h0 := c.b - c.t; ASSERT(w0 > 0, 100); ASSERT(h0 > 0, 101);
		Properties.PreferredSize(
				c.view, minViewSize, maxViewSize, minViewSize, maxViewSize, w0, h0, w, h);
		IF (w # w0) OR (h # h0) THEN
			c.form.Resize(c.view, c.l, c.t, c.l + w, c.t + h)
		END
	END SetSize;

	PROCEDURE (c: StdContext) Normalize (): BOOLEAN;
	BEGIN
		RETURN FALSE
	END Normalize;

	PROCEDURE (c: StdContext) GetRect (OUT l, t, r, b: INTEGER);
	BEGIN
		l := c.l; t := c.t; r := c.r; b := c.b
	END GetRect;


	(* StdDirectory *)

	PROCEDURE (d: StdDirectory) New (): Model;
		VAR f: StdModel;
	BEGIN
		NEW(f); RETURN f
	END New;


	(* StdReader *)

	PROCEDURE (rd: StdReader) Set (pos: Views.View);
		VAR c: StdContext;
	BEGIN
		IF pos = NIL THEN c := NIL ELSE c := ThisContext(rd.form, pos); ASSERT(c # NIL, 20) END;
		rd.view := NIL; rd.l := 0; rd.t := 0; rd.r := 0; rd.b := 0;
		rd.pos := c
	END Set;

	PROCEDURE (rd: StdReader) ReadView (OUT v: Views.View);
		VAR c: StdContext;
	BEGIN
		c := rd.pos;
		IF c = NIL THEN c := rd.form.contexts ELSE c := c.next END;
		IF c # NIL THEN
			rd.view := c.view; rd.l := c.l; rd.t := c.t; rd.r := c.r; rd.b := c.b;
			rd.pos := c
		ELSE
			rd.view := NIL; rd.l := 0; rd.t := 0; rd.r := 0; rd.b := 0
		END;
		v := rd.view
	END ReadView;


	(* StdWriter *)

	PROCEDURE (wr: StdWriter) Set (pos: Views.View);
		VAR c: StdContext;
	BEGIN
		IF pos = NIL THEN c := NIL ELSE c := ThisContext(wr.form, pos); ASSERT(c # NIL, 20) END;
		wr.pos := c
	END Set;

	PROCEDURE (wr: StdWriter) WriteView (v: Views.View; l, t, r, b: INTEGER);
		VAR op: FormOp; c: StdContext;
	BEGIN
		ASSERT(v # NIL, 20); ASSERT(v.context = NIL, 22);
		ASSERT(l <= r, 23); ASSERT(t <= b, 24);
		c := NewContext(wr.form, v, l, t, r, b);
		NEW(op); op.del := NIL; op.ins := c; op.pos := wr.pos;
		wr.pos := c;
		Models.Do(wr.form, "#System:Insertion", op)
	END WriteView;


	(* operations *)

	PROCEDURE Update (c: StdContext);
		VAR msg: UpdateMsg;
	BEGIN
		msg.l := c.l; msg.t := c.t; msg.r := c.r; msg.b := c.b; Models.Broadcast(c.form, msg)
	END Update;

	PROCEDURE (op: FormOp) Do;
		VAR f: StdModel; c, p, pos: StdContext;
	BEGIN
		(* delete *)
		pos := NIL;
		c := op.del;
		IF c # NIL THEN
			f := c.form; ASSERT(f # NIL, 100);
			p := f.contexts; ASSERT(p # NIL, 101);
			IF p = c THEN
				f.contexts := c.next
			ELSE
				WHILE p.next # c DO p := p.next; ASSERT(p # NIL, 102) END;
				pos := p; p.next := c.next
			END;
			c.next := NIL;
			Update(c)
		END;
		(* insert *)
		c := op.ins;
		IF c # NIL THEN
			f := c.form; ASSERT(f # NIL, 103);
			p := f.contexts;
			IF op.pos = NIL THEN
				c.next := f.contexts; f.contexts := c
			ELSE
				c.next := op.pos.next; op.pos.next := c
			END;
			Update(c)
		END;
		(* swap ins and del for undo *)
		p := op.del; op.del := op.ins; op.ins := p; op.pos := pos
	END Do;

	PROCEDURE (op: ResizeOp) Do;
		VAR c: StdContext; l, t, r, b: INTEGER;
	BEGIN
		c := op.context;
		(* save old state of context *)
		l := c.l; t := c.t; r := c.r; b := c.b;
		Update(c);
		(* set new state of context *)
		c.l := op.l; c.t := op.t; c.r := op.r; c.b := op.b;
		Update(c);
		(* old state is new undo state *)
		op.l := l; op.t := t; op.r := r; op.b := b
	END Do;

	PROCEDURE (op: ReplaceViewOp) Do;
		VAR c: StdContext; view: Views.View;
	BEGIN
		c := op.context;
		(* save old state of context *)
		view := c.view;
		(* set new state of context *)
		c.view := op.view;
		Update(c);
		(* old state is new undo state *)
		op.view := view
	END Do;


	(** miscellaneous **)

	PROCEDURE New* (): Model;
	BEGIN
		RETURN dir.New()
	END New;

	PROCEDURE CloneOf* (source: Model): Model;
	BEGIN
		ASSERT(source # NIL, 20);
		RETURN Containers.CloneOf(source)(Model)
	END CloneOf;

	PROCEDURE Copy* (source: Model): Model;
	BEGIN
		ASSERT(source # NIL, 20);
		RETURN Stores.CopyOf(source)(Model)
	END Copy;

	PROCEDURE SetDir* (d: Directory);
	(** d # NIL	20 **)
	BEGIN
		ASSERT(d # NIL, 20); dir := d
	END SetDir;

	PROCEDURE Init;
		VAR d: StdDirectory;
	BEGIN
		NEW(d); dir := d; stdDir := d
	END Init;

BEGIN
	Init
END FormModels.
