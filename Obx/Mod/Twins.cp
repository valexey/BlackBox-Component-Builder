MODULE ObxTwins;
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

	IMPORT Ports, Services, Stores, Models, Views, Controllers, Properties, TextViews;

	CONST
		minVersion = 3; maxVersion = 3;	(* old versions 0 .. 2 of ObxTwin views cannot be read anymore *)
		border = Ports.mm;

	TYPE
		Identity = POINTER TO RECORD (Models.Model) END;	(* dummy model *)

		Context = POINTER TO RECORD (Models.Context)
			base: View;			(* container view *)
			view: Views.View;	(* contained view *)
			l, t, r, b: INTEGER	(* cached bounding box of contained view *)
		END;

		View = POINTER TO RECORD (Views.View)
			top, bottom: Context;
			ident: Identity;	(* temporary dummy model *)
			focus: Context	(* current focus; either top or bottom *)
		END;


	(* Context *)

	PROCEDURE (c: Context) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL	(* don't give the embedded views information about the dummy twin model *)
	END ThisModel;

	PROCEDURE (c: Context) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.r - c.l;
		h := c.b - c.t
	END GetSize;

	PROCEDURE (c: Context) MakeVisible (l, t, r, b: INTEGER);
		VAR w, h, sep: INTEGER;
	BEGIN
		IF c.base.top = c THEN	(* top view *)
			c.base.context.MakeVisible(l + border, t + border, r + border, b + border)
		ELSE	(* bottom view *)
			c.base.context.GetSize(w, h); sep := h DIV 3;
			c.base.context.MakeVisible(l + border, t + sep + border, r + border, b + sep + border)
		END
	END MakeVisible;

	PROCEDURE (c: Context) Consider (VAR p: Models.Proposal);
	BEGIN
		c.base.context.Consider(p)
	END Consider;

	PROCEDURE (c: Context) Normalize (): BOOLEAN;
	BEGIN
		RETURN c.base.context.Normalize()
	END Normalize;

	PROCEDURE NewContext (v: Views.View; base: View): Context;
		VAR c: Context;
	BEGIN
		NEW(c);
		c.view := v; c.base := base;
		v.InitContext(c); Stores.Join(v, base);
		RETURN c
	END NewContext;

	PROCEDURE CopyOf (source: Context; shallow: BOOLEAN; base: View): Context;
		VAR v: Views.View;
	BEGIN
		v := Views.CopyOf(source.view,  shallow );
		RETURN NewContext(v, base)
	END CopyOf;


	(* View *)

	PROCEDURE RecalcLayout (v: View);
		VAR w, h, sep: INTEGER; c: Context;
	BEGIN
		v.context.GetSize(w, h);
		sep := h DIV 3;	(* separate the two views at 1/3 of the container's height *)
		c := v.top; c.l := border; c.t := border; c.r := w - border; c.b := sep - border;
		c := v.bottom; c.l := border; c.t := sep + border; c.r := w - border; c.b := h - border
	END RecalcLayout;

	PROCEDURE SetFocus (v: Views.View; x, y: INTEGER): BOOLEAN;
		VAR p: Properties.FocusPref;
	BEGIN	(* determine whether v should be focused when the mouse is clicked at (x, y) in v *)
		p.hotFocus := FALSE;
		p.atLocation := TRUE; p.x := x; p.y := y;
		p.setFocus := FALSE;
		Views.HandlePropMsg(v, p);
		RETURN p.setFocus
	END SetFocus;

	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
		VAR shallow: BOOLEAN;
	BEGIN
		WITH source: View DO
			shallow := model = source.ident;	(* shallow copy if both views share the same model *)
			Stores.Join(v, model);
			v.top := CopyOf(source.top, shallow, v);
			v.bottom := CopyOf(source.bottom, shallow, v);
			v.ident := model(Identity);
			v.focus := v.bottom
		END
	END CopyFromModelView;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; h: Views.View;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			Views.ReadView(rd, h); v.top := NewContext(h, v);
			Views.ReadView(rd, h); v.bottom := NewContext(h, v);
			NEW(v.ident); Stores.Join(v, v.ident);
			v.focus := v.bottom
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		Views.WriteView(wr, v.top.view);
		Views.WriteView(wr, v.bottom.view)
	END Externalize;

	PROCEDURE (v: View) Neutralize;
	BEGIN
		v.focus.view.Neutralize
	END Neutralize;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.ident
	END ThisModel;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR d: INTEGER; c: Context;
	BEGIN
		RecalcLayout(v);
		(* install the subframes for the subviews *)
		c := v.top; Views.InstallFrame(f, c.view, c.l, c.t, 0, v.focus = c);
		c := v.bottom; Views.InstallFrame(f, c.view, c.l, c.t, 1, v.focus = c);
		(* draw frame around the subviews *)
		d := 2 * f.dot;
		c := v.top;
		IF (c.t - d < c.b + d) & (c.l - d < c.r + d) THEN f.DrawRect(c.l - d, c.t - d, c.r + d, c.b + d, f.dot, Ports.black) END;
		c := v.bottom; 
		IF (c.t - d < c.b + d) & (c.l - d < c.r + d) THEN f.DrawRect(c.l - d, c.t - d, c.r + d, c.b + d, f.dot, Ports.black) END
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Views.CtrlMessage;
															VAR focus: Views.View);
		VAR g: Views.Frame; newFocus: Context; mMsg: Controllers.MarkMsg;
			w, h, sep: INTEGER;
	BEGIN
		WITH msg: Controllers.CursorMessage DO
			v.context.GetSize(w, h);
			sep := h DIV 3;
			IF msg.y >= sep THEN newFocus := v.bottom ELSE newFocus := v.top END;
			focus := newFocus.view;
			IF (newFocus # v.focus) & ((msg IS Controllers.TrackMsg) OR (msg IS Controllers.DropMsg)) &
				SetFocus(focus, msg.x, msg.y) THEN
				(* remove marks in old focus *)
				mMsg.show := FALSE; mMsg.focus := TRUE;
				g := Views.ThisFrame(f, v.focus.view); IF g # NIL THEN Views.ForwardCtrlMsg(g, mMsg) END;
				v.focus := newFocus;	(* set new focus *)
				(* set marks in new focus *)
				mMsg.show := TRUE; mMsg.focus := TRUE;
				g := Views.ThisFrame(f, v.focus.view); IF g # NIL THEN Views.ForwardCtrlMsg(g, mMsg) END
			END
		(* the following scrolling-oriented messages are always sent to bottom view, independent of focus *)
		| msg: Controllers.PollSectionMsg DO
			g := Views.ThisFrame(f, v.bottom.view); IF g # NIL THEN Views.ForwardCtrlMsg(g, msg) END;
			IF msg.vertical & ~msg.done THEN
				msg.valid := FALSE; msg.done := TRUE	(* disable default-scrolling *)
			END
		| msg: Controllers.ScrollMsg DO
			g := Views.ThisFrame(f, v.bottom.view); IF g # NIL THEN Views.ForwardCtrlMsg(g, msg) END;
			IF msg.vertical THEN msg.done := TRUE END
		| msg: Controllers.PageMsg DO
			focus := v.bottom.view
		ELSE	(* all other messages are sent to the focus, however *)
			focus := v.focus.view
		END
		(* the assignment to focus signals that the view v wants to forward the message to the
		corresponding embedded view *)
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Views.PropMessage);
		CONST defW = 80 * Ports.mm; defH = 60 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO
			IF msg.w = Views.undefined THEN msg.w := defW END;
			IF msg.h = Views.undefined THEN msg.h := defH END
		| msg: Properties.ResizePref DO
			msg.verFitToWin := TRUE	(* make view always as high as window *)
		ELSE
			Views.HandlePropMsg(v.bottom.view, msg)
		END
	END HandlePropMsg;

	PROCEDURE (v: View) ConsiderFocusRequestBy (view: Views.View);
	BEGIN
		IF view = v.top.view THEN
			v.focus := v.top
		ELSIF view = v.bottom.view THEN
			v.focus := v.bottom
		ELSE
			HALT(20)
		END
	END ConsiderFocusRequestBy;

	PROCEDURE NewTwin* (top, bottom: Views.View): Views.View;
		VAR v: View;
	BEGIN
		NEW(v);
		v.top := NewContext(top, v); v.bottom := NewContext(bottom, v);
		NEW(v.ident); Stores.Join(v, v.ident); v.focus := v.bottom;
		RETURN v
	END NewTwin;

	(* example twin view with two embedded text views *)

	PROCEDURE New* (): Views.View;
	BEGIN
		RETURN NewTwin(TextViews.dir.New(NIL), TextViews.dir.New(NIL))
	END New;

	PROCEDURE Deposit*;
	BEGIN
		Views.Deposit(New())
	END Deposit;

END ObxTwins.
