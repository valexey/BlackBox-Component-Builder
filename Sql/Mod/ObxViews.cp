MODULE SqlObxViews;
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
		Mechanisms, Fonts, Ports, Views, Controllers, Properties,
		TextModels, TextMappers, TextViews, SqlObxNets;

	CONST d = 3 * Ports.mm; w = 40 * Ports.mm; h = 6 * Ports.mm;

	TYPE
		View = POINTER TO RECORD (Views.View)
			g: SqlObxNets.Net;
			c: SqlObxNets.Company;
			w, h: INTEGER
		END;


	(* drawing *)

	PROCEDURE DrawCompany (f: Views.Frame; c: SqlObxNets.Company; first: BOOLEAN);
		VAR s, sw, x, y, asc, dsc, fw: INTEGER; col: Ports.Color; fnt: Fonts.Font;
	BEGIN
		IF first THEN s := 2 * f.unit; col := Ports.red ELSE s := f.unit; col := Ports.black END;
		x := c.x; y := c.y;
		f.DrawRect(x, y, x + w, y + h, s, col);
		fnt := Fonts.dir.Default();
		sw := fnt.StringWidth(c.name);
		fnt.GetBounds(asc, dsc,fw);
		x := x + (w - sw) DIV 2; y := y + h DIV 2 + (asc + dsc) DIV 3;
		f.DrawString(x, y, col, c.name, fnt)
	END DrawCompany;

	PROCEDURE Draw (c: SqlObxNets.Company; time: INTEGER; f: Views.Frame);
		VAR p: SqlObxNets.Node; c0: SqlObxNets.Company;
	BEGIN	(* c.x and c.y must be set up *)
		c.time := time;
		p := c.owns;
		WHILE p # NIL DO
			c0 := p.company;
			IF c0.time < time THEN
				DrawCompany(f, c0, FALSE);
				Draw(c0, time, f)
			END;
			f.DrawLine(c.x + w DIV 2, c.y + h, c0.x + w DIV 2, c0.y, f.unit, Ports.black);
			p := p.next
		END
	END Draw;


	(* mouse handling *)

	PROCEDURE TestHit (v: View; x, y: INTEGER; VAR l, t, r, b: INTEGER; VAR inside: BOOLEAN;
									VAR p: SqlObxNets.Company);
		VAR c: SqlObxNets.Company;
	BEGIN
		c := SqlObxNets.CompanyAt(v.g, x, y, w, h);
		IF c # NIL THEN
			IF p = NIL THEN p := c; l := c.x; t := c.y; r := l + w; b := t + h END;
			inside := p = c
		ELSE
			inside := FALSE
		END
	END TestHit;

	PROCEDURE Text (s: ARRAY OF CHAR): TextViews.View;
		VAR t: TextModels.Model; f: TextMappers.Formatter;
	BEGIN
		t := TextModels.dir.New();
		f.ConnectTo(t); f.WriteString(s);
		RETURN TextViews.dir.New(t)
	END Text;


	(* placement *)

	PROCEDURE ShiftDown (c: SqlObxNets.Company; time: INTEGER; dy: INTEGER; VAR b: INTEGER);
		VAR p: SqlObxNets.Node; c0: SqlObxNets.Company;
	BEGIN
		c.time := time; INC(c.y, dy);
		IF c.y + h + d > b THEN b := c.y + h + d END;
		p := c.owns;
		WHILE p # NIL DO
			c0 := p.company;
			IF c0.time < time THEN ShiftDown(c0, time, dy, b) END;
			p := p.next
		END
	END ShiftDown;

	PROCEDURE Place (c: SqlObxNets.Company; x, y, t0: INTEGER; VAR time, r, b: INTEGER);
		VAR p: SqlObxNets.Node; c0: SqlObxNets.Company;
	BEGIN
		(* place company c *)
		c.time := time; c.x := x; c.y := y;
		r := x + w + d;
		y := y + h + d; IF y > b THEN b := y END;
		(* handle companies owned by c *)
		p := c.owns;
		WHILE p # NIL DO
			c0 := p.company;
			IF c0.time < t0 THEN	(* placement is not up-to-date *)
				Place(c0, x, y, t0, time, r, b);
				x := r	(* placement above may have produced a block several companies wide *)
			ELSIF c0.y < y THEN
				INC(time); ShiftDown(c0, time, y - c0.y, b)
			END;
			p := p.next
		END
	END Place;


	(* View *)

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: View DO
			v.g := source.g;
			v.c := source.c;
			v.w := source.w;
			v.h := source.h
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR time: INTEGER;
	BEGIN
		IF v.g # NIL THEN
			DrawCompany(f, v.c, TRUE);
			time := v.c.time + 1; Draw(v.c, time, f)
		END
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR x, y, l, t, r, b, dx, dy: INTEGER; inside: BOOLEAN;
			c: SqlObxNets.Company; destX, destY: INTEGER; dest: Views.Frame; op: INTEGER; buttons: SET;
			tv: TextViews.View;
	BEGIN
		(* When the mouse is dragged from within one company to be dropped somewhere else,
			the name of the company is  dropped there as a text. This demonstrates how a drop
			source of a drag & drop operation is implemented. *)
		WITH msg: Controllers.PollOpsMsg DO
			msg.type := "TextViews.StdView"
		| msg: Controllers.TrackMsg DO
			c := NIL; x := msg.x; y := msg.y;
			TestHit(v, x, y, l, t, r, b, inside, c);
			IF inside THEN
				dx := x - c.x; dy := y - c.y ;
				op := Mechanisms.copy;
				tv := Text(c.name);
				Mechanisms.TrackToDrop(f, tv, FALSE, w, h, dx, dy, dest, destX, destY, op, x, y, buttons);
				IF op # Mechanisms.cancelDrop THEN
					Controllers.Drop(x, y, f, msg.x, msg.y, tv, FALSE, w, h, dx, dy)
				END
			END
		| msg: Controllers.PollDropMsg DO
			IF msg.mark THEN
				f.MarkRect(msg.x - msg.rx, msg.y - msg.ry, msg.x + msg.w - msg.rx,
								msg.y + msg.h - msg.ry, f.dot, Ports.invert, msg.show)
			END
		ELSE	(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF msg.w = Views.undefined THEN msg.w := v.w END;
			IF msg.h = Views.undefined THEN msg.h := v.h END
		ELSE
		END
	END HandlePropMsg;


	(* miscellaneous *)

	PROCEDURE New* (g: SqlObxNets.Net; c: SqlObxNets.Company): Views.View;
		VAR v: View;
	BEGIN
		ASSERT(g # NIL, 20); ASSERT(c # NIL, 21);
		NEW(v); v.g := g; v.c := c;
		c.time := 1;
		Place(c, d, d, c.time, c.time, v.w, v.h);
		RETURN v
	END New;

END SqlObxViews.
