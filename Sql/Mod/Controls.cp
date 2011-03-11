MODULE SqlControls;
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

	IMPORT SYSTEM,	(* SYSTEM.ADR only *)
		Stores, Sequencers, Views, Properties, Dialog, Meta,
		Fonts, Ports, Controllers, Containers, Controls, SqlDB, TextModels, TextViews;

	CONST
		nil* = -1;	(** navigator pseudo position **)
		minVersion = 0; maxBaseVersion = 0;
		anchVersion = 0; navVersion = 0; tabVersion = 0;
		w = 24;
		maxCol = 40;
		defColW = 30 * Ports.mm;
		left = -1; right = -2; center = -3;	(* adjustment modes *)
		move = 1; adjust = 2; field = 3;	(* cursor modes *)
		update = 2;								(* notify options *)
		
	TYPE
		Directory* = POINTER TO ABSTRACT RECORD END;

		Control = POINTER TO ABSTRACT RECORD (Views.View)
			item: Meta.Item;
			prop: Controls.Prop
		END;

		Anchor = POINTER TO RECORD (Control) 
			hasNotifier: BOOLEAN
		END;

		Navigator = POINTER TO RECORD (Control) END;
		
		Table = POINTER TO RECORD (Control)
			(* persistent *)
			sprop: Properties.StdProp;	(* font attributes *)
			columns: INTEGER;
			width: ARRAY maxCol OF INTEGER;
			mode: ARRAY maxCol OF INTEGER;
			(* not persistent *)
			table: SqlDB.Table;
			fldFont, titFont: Fonts.Font;
			rowH, baseOff: INTEGER;
			orgRow, orgX: INTEGER;	(* scroll state *)
			selRow, selCol: INTEGER;	(* selected field *)
			selected: BOOLEAN
		END;
		
		StdDirectory = POINTER TO RECORD (Directory) END;

		SelMsg = RECORD (Views.Message)
			show: BOOLEAN
		END;
		
		Op = POINTER TO RECORD (Stores.Operation)
			ctrl: Control;
			prop: Controls.Prop;
			sprop: Properties.StdProp
		END;
		
		FormatOp = POINTER TO RECORD (Stores.Operation)
			tab: Table;
			col, width, mode: INTEGER;
		END;

		CloseNotifier = POINTER TO RECORD (Sequencers.Notifier)
			control: Anchor
		END;

		TableValue = RECORD (Meta.Value)
			ptr: SqlDB.Table
		END;
		
		TableNotifier = PROCEDURE (t: SqlDB.Table; row, column: INTEGER; modifiers: SET);
		
		NotifierValue = RECORD (Meta.Value)
			p: TableNotifier
		END;

		NavProc = PROCEDURE (pos: INTEGER; VAR resPos, count: INTEGER);
		GuardProcVal = RECORD (Meta.Value) p*: Dialog.GuardProc END;
		NavProcVal = RECORD (Meta.Value) p*: NavProc END;


	VAR dir-, stdDir-: Directory;


	(* auxiliary procedures *)

	PROCEDURE CallGuard (c: Control; VAR disabled: BOOLEAN; VAR string: ARRAY OF CHAR);
		VAR ok: BOOLEAN; dpar: Dialog.Par; v: GuardProcVal; i: Meta.Item;
	BEGIN
		dpar.disabled := FALSE; dpar.undef := FALSE; dpar.readOnly := FALSE;
		dpar.checked := FALSE; dpar.label := "";
		Meta.LookupPath(c.prop.guard, i);
		IF (i.obj = Meta.procObj) OR (i.obj = Meta.varObj) & (i.typ = Meta.procTyp) THEN
			i.GetVal(v, ok);
			IF ok THEN v.p(dpar) END
		END;
		disabled := dpar.disabled; string := dpar.label$
	END CallGuard;

	PROCEDURE CallLink (c: Navigator; pos: INTEGER; VAR newPos, count: INTEGER);
		VAR ok: BOOLEAN; v: NavProcVal;
	BEGIN
		newPos := nil; count := nil; ok := FALSE;
		IF (c.item.obj = Meta.procObj) OR (c.item.obj = Meta.varObj) & (c.item.typ = Meta.procTyp) THEN
			c.item.GetVal(v, ok);
			IF ok THEN v.p(pos, newPos, count) END
		END;
		IF ~ok & (pos # nil) & (c.prop.label # "") THEN
			Dialog.ShowParamMsg("#Sql:HasWrongType", c.prop.link, "", "")
		END
	END CallLink;

	PROCEDURE CallNotifier (c: Anchor);
		VAR res: INTEGER;
	BEGIN
		IF c.prop.notifier # "" THEN
			Dialog.Call(c.prop.notifier, " ", res)
		END
	END CallNotifier;

	PROCEDURE CallTableNotifier (t: Table; row, column: INTEGER; modifiers: SET );
		VAR item: Meta.Item; nv: NotifierValue; ok: BOOLEAN;
	BEGIN
		IF t.prop.notifier # "" THEN
			Meta.LookupPath(t.prop.notifier, item);
			IF item.Valid() THEN
				item.GetVal(nv, ok);
				IF ok THEN
					nv.p(t.table, row, column, modifiers)
				ELSE
					Dialog.ShowParamMsg("#Sql:HasWrongType", t.prop.notifier, "", "")
				END
			ELSE
				Dialog.ShowParamMsg("#Sql:NotFound", t.prop.notifier, "", "")
			END
		END
	END CallTableNotifier;

	PROCEDURE OpenLink (c: Control; p: Controls.Prop);
		VAR mod, type: Meta.Name; tv: TableValue; ok: BOOLEAN;
	BEGIN
		c.prop := Properties.CopyOf(p)(Controls.Prop);
		IF c.prop.link # "" THEN
			Meta.LookupPath(c.prop.link, c.item);
			IF c.item.Valid() & (c.item.obj = Meta.varObj) THEN
				WITH c: Anchor DO
					IF c.item.typ # Meta.ptrTyp THEN
						Dialog.ShowParamMsg("#Sql:HasWrongType", c.prop.link, "", "")
					END
				| c: Table DO
					c.item.GetTypeName(mod, type); 
					IF (mod = "SqlDB") & (type = "Table") THEN
						c.item.GetVal(tv, ok); ASSERT(ok, 100);
						c.table := tv.ptr
					ELSE
						Dialog.ShowParamMsg("#Sql:HasWrongType", c.prop.link, "", "")
					END
				ELSE
				END
			ELSE
				Dialog.ShowParamMsg("#Sql:NotFound", c.prop.link, "", "")
			END
		END
	END OpenLink;
	
	
	PROCEDURE SetupTable (t: Table);
		VAR i, asc, dsc, w: INTEGER;
	BEGIN
		t.fldFont := Fonts.dir.This(t.sprop.typeface, t.sprop.size, t.sprop.style.val, Fonts.normal);
		t.titFont := Fonts.dir.This(t.sprop.typeface, t.sprop.size, t.sprop.style.val, Fonts.bold);
		t.rowH := 3 * t.sprop.size DIV 2;
		t.fldFont.GetBounds(asc, dsc, w);
		t.baseOff := (t.rowH -asc - dsc) DIV 2 + asc;
		i := t.columns;
		WHILE i < maxCol DO t.width[i] := defColW; t.mode[i] := center; INC(i) END
	END SetupTable;


	(** Directory **)

	PROCEDURE (d: Directory) NewAnchor* (p: Controls.Prop): Views.View, NEW, ABSTRACT;
	PROCEDURE (d: Directory) NewNavigator* (p: Controls.Prop): Views.View, NEW, ABSTRACT;
	PROCEDURE (d: Directory) NewTable* (p: Controls.Prop): Views.View, NEW, ABSTRACT;
	PROCEDURE (d: Directory) NewTableOn* (tab: SqlDB.Table): Views.View, NEW, ABSTRACT;
	
	
	(* Control *)

	PROCEDURE (c: Control) Internalize (VAR rd: Stores.Reader), EXTENSIBLE;
		VAR thisVersion: INTEGER; x: BOOLEAN;
	BEGIN
		c.Internalize^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, maxBaseVersion, thisVersion);
			IF ~rd.cancelled THEN
				NEW(c.prop);
				rd.ReadXString(c.prop.link);
				rd.ReadXString(c.prop.label);
				rd.ReadXString(c.prop.guard);
				rd.ReadXString(c.prop.notifier);
				rd.ReadBool(c.prop.opt[Controls.sorted]);
				rd.ReadBool(c.prop.opt[Controls.default]); 
				rd.ReadBool(c.prop.opt[Controls.cancel]);
				rd.ReadXInt(c.prop.level);
				rd.ReadBool(x);
				OpenLink(c, c.prop)
			END
		END
	END Internalize;

	PROCEDURE (c: Control) Externalize (VAR wr: Stores.Writer), EXTENSIBLE;
	BEGIN
		c.Externalize^(wr);
		wr.WriteVersion(maxBaseVersion);
		wr.WriteXString(c.prop.link);
		wr.WriteXString(c.prop.label);
		wr.WriteXString(c.prop.guard);
		wr.WriteXString(c.prop.notifier);
		wr.WriteBool(c.prop.opt[Controls.sorted]);
		wr.WriteBool(c.prop.opt[Controls.default]);
		wr.WriteBool(c.prop.opt[Controls.cancel]);
		wr.WriteXInt(c.prop.level);
		wr.WriteBool(FALSE)
	END Externalize;

	PROCEDURE (c: Control) CopyFromSimpleView (source: Views.View), EXTENSIBLE;
	BEGIN
		(* c.CopyFrom^(source); *)
		WITH source: Control DO
			NEW(c.prop); c.prop^ := source.prop^;
			c.item := source.item
		END
	END CopyFromSimpleView;


	(* Op *)

	PROCEDURE (op: Op) Do;
		VAR c: Control; prop: Controls.Prop; sprop: Properties.StdProp;
	BEGIN
		c := op.ctrl; 
		IF op.prop # NIL THEN
			NEW(prop);
			prop^ := c.prop^; prop.valid := op.prop.valid;
			IF Controls.link IN op.prop.valid THEN c.prop.link := op.prop.link END;
			IF Controls.label IN op.prop.valid THEN c.prop.label := op.prop.label END;
			IF Controls.guard IN op.prop.valid THEN c.prop.guard := op.prop.guard END;
			IF Controls.notifier IN op.prop.valid THEN c.prop.notifier := op.prop.notifier END;
			IF Controls.default IN op.prop.valid THEN c.prop.opt[Controls.default] := op.prop.opt[Controls.default] END;
			IF Controls.cancel IN op.prop.valid THEN c.prop.opt[Controls.cancel] := op.prop.opt[Controls.cancel] END;
			IF Controls.level IN op.prop.valid THEN c.prop.level := op.prop.level END;
			IF Controls.sorted IN op.prop.valid THEN c.prop.opt[Controls.sorted] := op.prop.opt[Controls.sorted] END;
			IF c.prop.link # prop.link THEN OpenLink(c, c.prop) END;
			op.prop := prop
		END;
		WITH c: Table DO
			IF op.sprop # NIL THEN
				NEW(sprop);
				sprop^ := c.sprop^; sprop.valid := op.sprop.valid;
				IF Properties.typeface IN op.sprop.valid THEN c.sprop.typeface := op.sprop.typeface END;
				IF Properties.size IN op.sprop.valid THEN c.sprop.size := op.sprop.size END;
				IF Properties.style IN op.sprop.valid THEN c.sprop.style := op.sprop.style END;
				IF Properties.weight IN op.sprop.valid THEN c.sprop.weight := op.sprop.weight END;
				IF sprop.valid # {} THEN SetupTable(c) END;
				op.sprop := sprop
			END
		ELSE
		END;
		Views.Update(c, Views.rebuildFrames);
	END Do;
	
	
	(* FormatOp *)
	
	PROCEDURE (op: FormatOp) Do;
		VAR t: Table; c, w, m: INTEGER;
	BEGIN
		t := op.tab; c := op.col; w := op.width; m := op.mode;
		op.width := t.width[c]; op.mode := t.mode[c];
		t.width[c] := w; t.mode[c] := m;
		IF c >= t.columns THEN t.columns := c + 1 END;
		Views.Update(t, Views.keepFrames);
	END Do;
	
	
	(* properties *)

	PROCEDURE PollProp (c: Control; VAR list: Properties.Property);
		VAR p: Controls.Prop; sp: Properties.StdProp;
	BEGIN
		NEW(p); p^ := c.prop;
		WITH c: Anchor DO
			p.valid := {Controls.link, Controls.label, Controls.guard, Controls.notifier}
		| c: Navigator DO
			p.valid := {Controls.link}
		| c: Table DO
			p.valid := {Controls.link, Controls.notifier};
			NEW(sp); sp^ := c.sprop^;
			sp.valid := {Properties.typeface, Properties.size, Properties.style, Properties.weight};
			sp.known := sp.valid; sp.readOnly := {Properties.weight};
			Properties.Insert(list, sp)
		ELSE
		END;
		p.known := p.valid;
		Properties.Insert(list, p)
	END PollProp;

	PROCEDURE SetProp (c: Control; p: Properties.Property);
		VAR op: Op; valid: SET;
	BEGIN
		op := NIL;
		WHILE p # NIL DO
			WITH p: Controls.Prop DO
				WITH c: Anchor DO
					valid := {Controls.link, Controls.label, Controls.guard, Controls.notifier}
				| c: Navigator DO
					valid := {Controls.link}
				| c: Table DO
					valid := {Controls.link, Controls.notifier}
				ELSE
				END;
				valid := p.valid * valid;
				IF valid # {} THEN
					IF op = NIL THEN NEW(op); op.ctrl := c END;
					NEW(op.prop); op.prop^ := p^; op.prop.valid := valid
				END
			| p: Properties.StdProp DO
				WITH c: Table DO
					valid := p.valid * {Properties.typeface, Properties.size, Properties.style};
					IF valid # {} THEN
						IF op = NIL THEN NEW(op); op.ctrl := c END;
						NEW(op.sprop); op.sprop^ := p^; op.sprop.valid := valid
					END
				ELSE
				END
			ELSE
			END;
			p := p.next
		END;
		IF op # NIL THEN Views.Do(c, "#System:SetProp", op) END
	END SetProp;


	(* CloseNotifier *)

	PROCEDURE (n: CloseNotifier) Notify (VAR msg: Sequencers.Message);
		VAR disabled: BOOLEAN; string: Dialog.String; res: INTEGER;
	BEGIN
		WITH msg: Sequencers.CloseMsg DO
			IF ~msg.sticky & (~n.control.item.Valid() OR (n.control.item.PtrVal() # NIL)) THEN
				CallGuard(n.control, disabled, string);
				IF disabled THEN
					IF string = "" THEN string := n.control.prop.label$ END;
					IF string = "" THEN string := "#Sql:IsCloseOk" END;
					Dialog.GetOK(string, "", "", "", {Dialog.yes, Dialog.no}, res);
					msg.sticky := res = Dialog.no
				END
			END
		| msg: Sequencers.RemoveMsg DO
			IF n.control.item.Valid() THEN n.control.item.PutPtrVal(NIL) END;
			CallNotifier(n.control)	(* Dialog.CheckGuards is called by HostWindows *)
		ELSE
		END
	END Notify;


	(* Anchor *)

	PROCEDURE (a: Anchor) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER;
	BEGIN
		a.Internalize^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, anchVersion, thisVersion)
		END
	END Internalize;

	PROCEDURE (a: Anchor) Externalize (VAR wr: Stores.Writer);
	BEGIN
		a.Externalize^(wr);
		wr.WriteVersion(anchVersion)
	END Externalize;

	PROCEDURE Visible (v: Views.View; f: Views.Frame): BOOLEAN;
		VAR visible: BOOLEAN; g: Views.Frame; ctrl: Containers.Controller;
	BEGIN
		g := Views.HostOf(f);
		IF (g = NIL) OR ~(g.view IS Containers.View) THEN
			visible := TRUE
		ELSE
			ctrl := g.view(Containers.View).ThisController();
			visible := Containers.mask * ctrl.opts # Containers.mask
		END;
		RETURN visible
	END Visible;

	PROCEDURE (a: Anchor) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR u: INTEGER; col: Ports.Color; n: CloseNotifier;
	BEGIN
		IF ~a.hasNotifier & (a.Domain() # NIL) & (a.Domain().GetSequencer() # NIL) THEN
			NEW(n); n.control := a;
			a.Domain().GetSequencer()(Sequencers.Sequencer).InstallNotifier(n);
			a.hasNotifier := TRUE
		END;
		IF Visible(a, f) THEN
			u := Ports.point;
			f.DrawRect(0, 0, 20 * u, 16 * u, u, Ports.defaultColor);
			f.DrawLine(u, 3 * u, 19 * u, 3 * u, u, Ports.defaultColor);
			IF (a.prop.link # "") OR (a.prop.label # "") OR (a.prop.guard # "") THEN
				col := Ports.red ELSE col := Ports.green
			END;
			f.DrawLine(2 * u, 5 * u, 17 * u, 13 * u, u, col);
			f.DrawLine(2 * u, 13 * u, 17 * u, 5 * u, u, col)
		END
	END Restore;

	PROCEDURE (a: Anchor) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.ResizePref DO
			msg.fixed := TRUE
		| msg: Properties.PollMsg DO
			PollProp(a, msg.prop)
		| msg: Properties.SetMsg DO
			SetProp(a, msg.prop)
		| msg: Properties.SizePref DO
			IF msg.w = Views.undefined THEN msg.w := 20 * Ports.point END;
			IF msg.h = Views.undefined THEN msg.h := 16 * Ports.point END
		ELSE
		END
	END HandlePropMsg;


	(* Navigator *)

	PROCEDURE (n: Navigator) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER;
	BEGIN
		n.Internalize^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, navVersion, thisVersion)
		END
	END Internalize;

	PROCEDURE (n: Navigator) Externalize (VAR wr: Stores.Writer);
	BEGIN
		n.Externalize^(wr);
		wr.WriteVersion(navVersion)
	END Externalize;

	PROCEDURE DrawArrow (f: Views.Frame; VAR y: INTEGER; w, u: INTEGER; bar, up, guard: BOOLEAN);
		VAR d, y0, y1: INTEGER; col: Ports.Color;
	BEGIN
		d := w DIV 2 - 2;
		IF guard THEN col := Ports.defaultColor ELSE col := Ports.grey50 END;
		IF bar  & up THEN f.DrawLine(2 * u, y * u, (w - 3) * u, y * u, u, col); INC(y) END;
		IF up THEN y0 := (y + d - 1) * u; y1 := y * u ELSE y0 := y * u; y1 := (y + d - 1) * u END;
		f.DrawLine(2 * u, y0, (w DIV 2 - 1) * u, y1, u, col);	(* left diagonal *)
		f.DrawLine(w DIV 2 * u, y1, (w - 3) * u, y0, u, col);	(* right diagonal *)
		INC(y, d);
		IF bar & ~up THEN f.DrawLine(2 * u, y * u, (w - 3) * u, y * u, u, col); INC(y) END
	END DrawArrow;

	PROCEDURE (n: Navigator) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR newPos, count, u, y: INTEGER; col: Ports.Color;
	BEGIN
		CallLink(n, nil, newPos, count);
		u := f.dot; y := 2;
		DrawArrow(f, y, w, u, TRUE, TRUE, newPos > 0); INC(y, 2);
		DrawArrow(f, y, w, u, FALSE, TRUE, newPos > 0); INC(y, 2);
		DrawArrow(f, y, w, u, FALSE, FALSE, newPos < count - 1); INC(y, 2);
		DrawArrow(f, y, w, u, TRUE, FALSE, newPos < count - 1); INC(y, 2);
		IF n.prop.link # "" THEN col := Ports.defaultColor ELSE col := Ports.grey50 END;
		f.DrawRect(0, 0, w * u, y * u, u, col)
	END Restore;

	PROCEDURE HitArrow (f: Views.Frame; VAR y: INTEGER; w, u, xm, ym: INTEGER; bar, up, guard: BOOLEAN;
										VAR hit: BOOLEAN);
		VAR d, y0, xd, yd: INTEGER; m: SET; isDown: BOOLEAN;
	BEGIN
		hit := FALSE; xm := xm DIV u; ym := ym DIV u; y0 := y - 1;
		IF (xm >= 0) & (xm < w) & (ym >= y) THEN
			d := w DIV 2 - 2;
			IF bar  & up THEN INC(y) END;
			INC(y, d);
			IF bar & ~up THEN INC(y) END;
			IF ym < y THEN
				IF guard THEN
					f.MarkRect(u, y0 * u, (w - 1) * u, (y + 1) * u, Ports.fill, Ports.hilite, Ports.show);
					REPEAT f.Input(xd, yd, m, isDown) UNTIL ~isDown;
					f.MarkRect(u, y0 * u, (w - 1) * u, (y + 1) * u, Ports.fill, Ports.hilite, Ports.hide);
					hit := TRUE
				ELSE Dialog.Beep
				END
			END
		END
	END HitArrow;

	PROCEDURE TrackMsg (v: Navigator; f: Views.Frame; w, u, xm, ym: INTEGER);
		VAR newPos, count, y: INTEGER; hit: BOOLEAN;
	BEGIN
		CallLink(v, nil, newPos, count);
		u := f.dot; y := 2;
		HitArrow(f, y, w, u, xm, ym, TRUE, TRUE, newPos > 0, hit); INC(y, 2);
		IF hit THEN CallLink(v, 0, newPos, count) END;
		HitArrow(f, y, w, u, xm, ym, FALSE, TRUE, newPos > 0, hit); INC(y, 2);
		IF hit THEN CallLink(v, newPos - 1, newPos, count) END;
		HitArrow(f, y, w, u, xm, ym, FALSE, FALSE, newPos < count - 1, hit); INC(y, 2);
		IF hit THEN CallLink(v, newPos + 1, newPos, count) END;
		HitArrow(f, y, w, u, xm, ym, TRUE, FALSE, newPos < count - 1, hit);
		IF hit THEN CallLink(v, count - 1, newPos, count) END;
		Views.Update(v, Views.keepFrames)
	END TrackMsg;

	PROCEDURE (n: Navigator) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																				VAR focus: Views.View);
	BEGIN
		ASSERT(focus = NIL, 23);
		WITH msg: Controllers.TrackMsg DO
			TrackMsg(n, f, w, f.dot, msg.x, msg.y)
		ELSE	(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE (n: Navigator) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			msg.hotFocus := TRUE
		| msg: Properties.SizePref DO
			IF msg.w = Views.undefined THEN msg.w := w * Ports.point END;
			IF msg.h = Views.undefined THEN msg.h := (12 + 4 * (w DIV 2 - 2)) * Ports.point END
		| msg: Properties.PollMsg DO
			PollProp(n, msg.prop)
		| msg: Properties.SetMsg DO
			SetProp(n, msg.prop)
		ELSE
		END
	END HandlePropMsg;


	(* Table *)

	PROCEDURE DrawField (f: Views.Frame; x, y, w, mode: INTEGER; VAR s: ARRAY OF CHAR; font: Fonts.Font);
		VAR i, sw, rw: INTEGER;
	BEGIN
		DEC(w, 4 * f.dot); INC(x, 2 * f.dot);
		sw := font.StringWidth(s);
		IF sw > w THEN
			rw := w - font.StringWidth("...");
			IF (rw >= 0) & (LEN(s) >= 4) THEN
				i := f.CharIndex(0, rw, s, font);
				IF i > 0 THEN DEC(i) END;
				IF i > LEN(s) - 4 THEN i := LEN(s) - 4 END;
				s[i] := "."; s[i+1] := "."; s[i+2] := "."; s[i+3] := 0X;
				sw := font.StringWidth(s)
			ELSE sw := 0
			END
		END;
		IF sw > 0 THEN
			IF mode = center THEN INC(x, (w - sw) DIV 2)
			ELSIF mode = right THEN INC(x, w - sw)
			END;
			f.DrawString(x, y, Ports.defaultColor, s, font)
		END
	END DrawField;
	
	PROCEDURE DrawSelection (t: Table; f: Views.Frame; show: BOOLEAN);
		VAR c, x, y: INTEGER;
	BEGIN
		c := 0; x := 2 * f.dot - t.orgX;
		WHILE c < t.selCol DO INC(x, t.width[c]); INC(c) END;
		IF t.selRow >= t.orgRow THEN
			y := (t.selRow + 1 - t.orgRow) * t.rowH + 3 * f.dot;
			f.MarkRect(x + f.dot, y + f.dot, x + t.width[t.selCol] - 2 * f.dot, y + t.rowH - 2 * f.dot,
							Ports.fill, Ports.hilite, show)
		END
	END DrawSelection;
	
	PROCEDURE Select (t: Table; on: BOOLEAN);
		VAR msg: SelMsg;
	BEGIN
		IF on # t.selected THEN
			msg.show := on; Views.Broadcast(t, msg); t.selected := on
		END
	END Select;
	
	PROCEDURE ViewFromSelection (t: Table): Views.View;
		VAR m: TextModels.Model; w: TextModels.Writer; data: SqlDB.Row; i: INTEGER;
	BEGIN
		IF t.selected THEN
			t.table.Read(t.selRow, data);
			m := TextModels.dir.New(); w := m.NewWriter(NIL); i := 0;
			WHILE data.fields[t.selCol][i] # 0X DO w.WriteChar(data.fields[t.selCol][i]); INC(i) END;
			RETURN TextViews.dir.New(m)
		ELSE RETURN NIL
		END
	END ViewFromSelection;
	
	PROCEDURE GetTableSize (t: Table; dot: INTEGER; VAR w, h: INTEGER);
		VAR c: INTEGER;
	BEGIN
		w := 0; h := 0;
		IF (t.table # NIL) & t.table.Available() & (t.table.columns > 0) THEN
			c := 0;
			WHILE (c < t.table.columns) & (c < maxCol) DO INC(w, t.width[c]); INC(c) END;
			INC(w, 3 * dot);
			IF t.table.rows = MAX(INTEGER) THEN h := 10000 * Ports.mm
			ELSE h := (t.table.rows + 1) * t.rowH + 4 * dot
			END
		END
	END GetTableSize;
	
	PROCEDURE CheckPos (t: Table; x, y, dot: INTEGER; VAR col, type, p: INTEGER);
		VAR c, w, h, a: INTEGER;
	BEGIN
		GetTableSize(t, dot, w, h);
		INC(x, t.orgX);
		IF (x >= 0) & (x <= w) & (y >= 0) & (y <= h) THEN
			c := 0; w := 0; type := 0; INC(x, dot);
			WHILE (c < maxCol) & (x >= w + t.width[c]) DO INC(w, t.width[c]); INC(c) END;
			IF (x <= w + 3 * dot) & (c > 0) THEN
				col := c - 1; p := w + dot - t.orgX; type := move
			ELSIF y - dot < t.rowH THEN
				type := adjust;
				col := c; a := t.width[c] DIV 3;
				IF x < w + a THEN p := left
				ELSIF x > w + a * 2 THEN p := right
				ELSE p := center
				END
			ELSE
				col := c; p := (y - dot) DIV t.rowH + t.orgRow - 1; type := field
			END
		ELSE type := 0
		END
	END CheckPos;
	
	PROCEDURE MoveLine (t: Table; f: Views.Frame; col, x0: INTEGER);
		VAR w, h, x, y, x1, limit: INTEGER; m: SET; isDown: BOOLEAN; op: FormatOp;
	BEGIN
		GetTableSize(t, f.dot, w, h);
		x := x0; limit := x0 - t.width[col] + 2 * f.dot;
		REPEAT
			f.Input(x1, y, m, isDown);
			IF x1 < limit THEN x1 := limit END;
			IF x1 # x THEN
				f.MarkRect(x, 0, x + f.dot, h, Ports.fill, Ports.invert, FALSE);
				x := x1;
				f.MarkRect(x, 0, x + f.dot, h, Ports.fill, Ports.invert, TRUE)
			END
		UNTIL ~isDown;
		NEW(op); op.tab := t; op.col := col;
		op.width := t.width[col] + x - x0; op.mode := t.mode[col];
		Views.Do(t, "#System:SetLayout", op)
	END MoveLine;
	
	PROCEDURE ChangeAdjust (t: Table; col, mode: INTEGER);
		VAR op: FormatOp;
	BEGIN
		NEW(op); op.tab := t; op.col := col;
		op.width := t.width[col]; op.mode := mode;
		Views.Do(t, "#System:SetLayout", op)
	END ChangeAdjust;
	
	PROCEDURE TableSections (t: Table; f: Views.Frame; vertical: BOOLEAN; VAR size, part, pos: INTEGER);
		VAR c, w, max: INTEGER;
	BEGIN
		size := 0; part := 0; pos := 0;
		IF (t.table # NIL) & t.table.Available() & (t.table.columns > 0) THEN
			IF vertical THEN
				size := t.table.rows;
				part := (f.b - f.t) DIV t.rowH - 1;
				pos := t.orgRow
			ELSE
				c := 0; w := 0; max := t.table.columns;
				IF max > maxCol THEN max := maxCol END;
				WHILE (c < max) DO INC(w, t.width[c]); INC(c) END;
				size := w + 3 * f.dot;
				part := f.r - f.l;
				pos := t.orgX
			END
		END
	END TableSections;
	
	PROCEDURE ScrollTable (t: Table; f: Views.Frame; op, pos: INTEGER; vertical: BOOLEAN);
		VAR size, part, p, delta, l, t0, r, b: INTEGER;
	BEGIN
		IF vertical THEN
			TableSections(t, f, TRUE, size, part, p);
			delta := part - 1;
			IF delta < 1 THEN delta := 1 END;
			CASE op OF
			| Controllers.decLine: DEC(p)
			| Controllers.incLine: INC(p)
			| Controllers.decPage: DEC(p, delta)
			| Controllers.incPage: INC(p, delta)
			| Controllers.gotoPos: p := pos
			END;
			IF p > size - part THEN p := size - part END;
			IF p < 0 THEN p := 0 END;
			delta := (f.gx + t.rowH + 4 * f.dot) DIV f.unit;
			f.rider.GetRect(l, t0, r, b);
			IF b > delta THEN
				IF t0 < delta THEN f.rider.SetRect(l, delta, r, b) END;
				Views.Scroll(t, 0, (t.orgRow - p) * t.rowH);
				f.rider.SetRect(l, t0, r, b)
			END;
			t.orgRow := p
		ELSE
			TableSections(t, f, FALSE, size, part, p);
			delta := part - defColW;
			IF delta < defColW THEN delta := defColW END;
			CASE op OF
			| Controllers.decLine: DEC(p, defColW)
			| Controllers.incLine: INC(p, defColW)
			| Controllers.decPage: DEC(p, delta)
			| Controllers.incPage: INC(p, delta)
			| Controllers.gotoPos: p := pos
			END;
			IF p >= size - part THEN p := size - part END;
			IF p < 0 THEN p := 0 END;
			Views.Scroll(t, t.orgX - p, 0);
			t.orgX := p
		END
	END ScrollTable;
	
	PROCEDURE HandleChar (t: Table; f: Views.Frame; ch: CHAR);
	BEGIN
		CASE ch OF
		| 10X: ScrollTable(t, f, Controllers.decPage, 0, FALSE)
		| 11X: ScrollTable(t, f, Controllers.incPage, 0, FALSE)
		| 12X: ScrollTable(t, f, Controllers.decPage, 0, TRUE)
		| 13X: ScrollTable(t, f, Controllers.incPage, 0, TRUE)
		| 14X: ScrollTable(t, f, Controllers.gotoPos, 0, FALSE)
		| 15X: ScrollTable(t, f, Controllers.gotoPos, MAX(INTEGER), FALSE)
		| 16X: ScrollTable(t, f, Controllers.gotoPos, 0, TRUE)
		| 17X: ScrollTable(t, f, Controllers.gotoPos, MAX(INTEGER), TRUE)
		| 1CX: ScrollTable(t, f, Controllers.decLine, 0, FALSE)
		| 1DX: ScrollTable(t, f, Controllers.incLine, 0, FALSE)
		| 1EX: ScrollTable(t, f, Controllers.decLine, 0, TRUE)
		| 1FX: ScrollTable(t, f, Controllers.incLine, 0, TRUE)
		| 07X, 08X, 1BX: Select(t, FALSE)
		ELSE
		END
	END HandleChar;
	

	PROCEDURE (t: Table) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER; i: INTEGER;
	BEGIN
		t.Internalize^(rd);
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, tabVersion, thisVersion);
			IF ~rd.cancelled THEN
				NEW(t.sprop);
				rd.ReadXString(t.sprop.typeface);
				rd.ReadInt(t.sprop.size);
				rd.ReadSet(t.sprop.style.val);
				t.sprop.style.mask := {Fonts.italic, Fonts.underline, Fonts.strikeout};
				t.sprop.weight := Fonts.normal;
				rd.ReadInt(t.columns); i := 0;
				WHILE i < t.columns DO
					rd.ReadInt(t.width[i]);
					rd.ReadInt(t.mode[i]);
					INC(i)
				END;
				SetupTable(t)
			END
		END
	END Internalize;

	PROCEDURE (t: Table) Externalize (VAR wr: Stores.Writer);
		VAR i: INTEGER;
	BEGIN
		t.Externalize^(wr);
		wr.WriteVersion(tabVersion);
		wr.WriteXString(t.sprop.typeface);
		wr.WriteInt(t.sprop.size);
		wr.WriteSet(t.sprop.style.val);
		wr.WriteInt(t.columns); i := 0;
		WHILE i < t.columns DO
			wr.WriteInt(t.width[i]);
			wr.WriteInt(t.mode[i]);
			INC(i)
		END
	END Externalize;

	PROCEDURE (t: Table) CopyFromSimpleView (source: Views.View);
	BEGIN
		t.CopyFromSimpleView^(source);
		WITH source: Table DO
			NEW(t.sprop); t.sprop^ := source.sprop^;
			t.columns := source.columns;
			t.width := source.width;
			t.mode := source.mode;
			t.table := source.table;
			SetupTable(t)
		END
	END CopyFromSimpleView;
	
	PROCEDURE (t: Table) Neutralize;
	BEGIN
		Select(t, FALSE)
	END Neutralize;
	
	PROCEDURE (t: Table) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;

	PROCEDURE (t: Table) Restore (f: Views.Frame; l, t1, r, b: INTEGER);
		VAR row, col, x, y, w: INTEGER; data: SqlDB.Row; font: Fonts.Font;
	BEGIN
		IF (t.table # NIL) & t.table.Available() & (t.table.columns > 0) THEN
			DEC(t.rowH, t.rowH MOD f.unit);
			row := SqlDB.names; y := 2 * f.dot; font := t.titFont;
			WHILE (row < t.table.rows) & (y < b) DO
				IF ~Views.IsPrinterFrame(f) OR (y + t.rowH < b) THEN
					t.table.Read(row, data);
					IF t.table.res # SqlDB.noData THEN 
						col := 0; x := 2 * f.dot - t.orgX;
						WHILE (col < t.table.columns) & (col < maxCol) & (x < r) DO
							w := t.width[col];
							IF x + w >= 0 THEN
								f.DrawRect(x - f.dot, y, x, y + t.rowH, Ports.fill, Ports.defaultColor);
									(* left vertical separation line *)
								IF ~Views.IsPrinterFrame(f) OR (x + w < r) THEN
									DrawField(f, x, y + t.baseOff, w - f.dot, t.mode[col], data.fields[col]^, font)
								END
							END;
							INC(col); INC(x, w)
						END;
						f.DrawRect(x - f.dot, y, x, y + t.rowH, Ports.fill, Ports.defaultColor);	(* right vertical separator line *)
						IF Views.IsPrinterFrame(f) & (x >= r) THEN DEC(x, w) END;
						
						IF row = SqlDB.names THEN row := t.orgRow; font := t.fldFont; INC(y, f.dot)
						ELSE INC(row)
						END;
						
						INC(y, t.rowH);
						f.DrawRect(f.dot - t.orgX, y - f.dot, x, y, Ports.fill, Ports.defaultColor)	(* bottom line *)
					ELSE y := b
					END
				END
			END;
			f.DrawRect(f.dot - t.orgX, f.dot, x, 2 * f.dot, Ports.fill, Ports.defaultColor);
			f.DrawRect(f.dot - t.orgX, t.rowH + f.dot, x, t.rowH + 2 * f.dot, Ports.fill, Ports.defaultColor)
		END;
	END Restore;
	
	PROCEDURE (t: Table) RestoreMarks (f: Views.Frame; l, t1, r, b: INTEGER);
	BEGIN
		IF (t.table # NIL) & t.table.Available() & (t.table.columns > 0) & t.selected THEN
			DrawSelection(t, f, TRUE)
		END
	END RestoreMarks;
	
	PROCEDURE (t: Table) HandleViewMsg (f: Views.Frame; VAR msg: Views.Message);
	BEGIN
		WITH msg: Views.NotifyMsg DO
			IF update IN msg.opts THEN
				IF (t.table # NIL) & (msg.id0 = SYSTEM.ADR(t.table^)) THEN
					Views.Update(t, Views.keepFrames)
				END
			END
		| msg: SelMsg DO
			DrawSelection(t, f, msg.show)
		ELSE
		END
	END HandleViewMsg;

	PROCEDURE (t: Table) HandleCtrlMsg (f: Views.Frame;
																	VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR p, col, type: INTEGER; c, w, size, part, pos: INTEGER;
	BEGIN
		WITH msg: Properties.CollectMsg DO
			Views.HandlePropMsg(t, msg.poll)
		| msg: Properties.EmitMsg DO
			Views.HandlePropMsg(t, msg.set)
		| msg: Controllers.PollOpsMsg DO
			IF t.selected THEN msg.valid := {Controllers.copy} END
		| msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN
				HandleChar(t, f, msg.char)
			ELSIF msg.op = Controllers.copy THEN
				msg.view := ViewFromSelection(t);
				msg.w := 0; msg.h := 0; msg.isSingle := FALSE
			END
		| msg: Controllers.TrackMsg DO
			CheckPos(t, msg.x, msg.y, f.dot, col, type, p);
			IF type = move THEN MoveLine(t, f, col, p)
			ELSIF type = adjust THEN ChangeAdjust(t, col, p)
			ELSIF type = field THEN
				CallTableNotifier(t, p, col, msg.modifiers);
				Select(t, FALSE);
				t.selRow := p; t.selCol := col; Select(t, TRUE)
			ELSE
				Select(t, FALSE)
			END
		| msg: Controllers.PollCursorMsg DO
			CheckPos(t, msg.x, msg.y, f.dot, col, type, p);
			IF type = move THEN msg.cursor := 16	(* Ports.ResizeHCursor *)
			ELSIF type = adjust THEN msg.cursor := Ports.refCursor
			ELSIF type = field THEN msg.cursor := Ports.tableCursor
			END
		| msg: Controllers.SelectMsg DO
			IF ~msg.set THEN
				Select(t, FALSE);
			END
		| msg: Controllers.PollSectionMsg DO
			TableSections(t, f, msg.vertical, msg.wholeSize, msg.partSize, msg.partPos);
			IF (msg.partPos > 0) & (msg.partPos > msg.wholeSize - msg.partSize) THEN
				ScrollTable(t, f, Controllers.gotoPos, msg.wholeSize - msg.partSize, msg.vertical);
				TableSections(t, f, msg.vertical, msg.wholeSize, msg.partSize, msg.partPos)
			END;
			msg.valid := msg.partSize < msg.wholeSize;
			msg.done := TRUE
		| msg: Controllers.ScrollMsg DO
			ScrollTable(t, f, msg.op, msg.pos, msg.vertical);
			msg.done := TRUE
		| msg: Controllers.PageMsg DO
			IF msg.op IN {Controllers.nextPageY, Controllers.gotoPageY} THEN	(* vertical *)
				TableSections(t, f, TRUE, size, part, pos);
				IF msg.op = Controllers.nextPageY THEN
					t.orgRow := pos + part
				ELSE
					t.orgRow := msg.pageY * part;
				END;
				msg.done := TRUE;
				msg.eoy :=t.orgRow >= size
			ELSE	(* horizontal *)
				TableSections(t, f, FALSE, size, part, pos);
				IF msg.op = Controllers.nextPageX THEN
					t.orgX := pos + part
				ELSE
					t.orgX := msg.pageX * part;
				END;
				IF (t.orgX > 0) & (t.orgX < size) THEN
					c := 0; w := 0;
					WHILE w < t.orgX DO INC(w, t.width[c]); INC(c) END;
					t.orgX := w - t.width[c-1] + 1*f.dot
				END;
				msg.done := TRUE;
				msg.eox :=t.orgX >= size;
			END
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (t: Table) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF t.table # NIL THEN msg.setFocus := TRUE END
		| msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) & (msg.h = Views.undefined) THEN
				GetTableSize(t, Ports.point, msg.w, msg.h)
			END;
			IF msg.w = Views.undefined THEN msg.w := 80 * Ports.mm END;
			IF msg.h = Views.undefined THEN msg.h := 30 * Ports.mm END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE;
			msg.verFitToWin := TRUE
		| msg: Properties.PollMsg DO
			PollProp(t, msg.prop)
		| msg: Properties.SetMsg DO
			SetProp(t, msg.prop)
		ELSE
		END
	END HandlePropMsg;
	

	(* StdDirectory *)

	PROCEDURE InitProp (VAR p: Controls.Prop);
	BEGIN
		NEW(p);
		p.link := ""; p.label := ""; p.guard := ""; p.notifier := "";
		p.opt[Controls.default] := FALSE; p.opt[Controls.cancel] := FALSE;
		p.level := 0; p.opt[Controls.sorted] := FALSE
	END InitProp;
	
	PROCEDURE InitStdProp (VAR p: Properties.StdProp);
		VAR f: Fonts.Font;
	BEGIN
		NEW(p);
		f := Fonts.dir.Default();
		p.typeface := f.typeface; 
		p.size := f.size;
		p.style.val := f.style;
		p.style.mask := {Fonts.italic, Fonts.underline, Fonts.strikeout};
		p.weight := Fonts.normal;
	END InitStdProp;

	PROCEDURE (d: StdDirectory) NewAnchor (p: Controls.Prop): Views.View;
		VAR c: Anchor;
	BEGIN
		NEW(c); OpenLink(c, p); RETURN c
	END NewAnchor;

	PROCEDURE (d: StdDirectory) NewNavigator (p: Controls.Prop): Views.View;
		VAR c: Navigator;
	BEGIN
		NEW(c); OpenLink(c, p); RETURN c
	END NewNavigator;

	PROCEDURE (d: StdDirectory) NewTable (p: Controls.Prop): Views.View;
		VAR c: Table;
	BEGIN
		NEW(c); OpenLink(c, p); InitStdProp(c.sprop); SetupTable(c); RETURN c
	END NewTable;

	PROCEDURE (d: StdDirectory) NewTableOn (tab: SqlDB.Table): Views.View;
		VAR c: Table;
	BEGIN
		NEW(c); InitProp(c.prop); c.table := tab; InitStdProp(c.sprop); SetupTable(c); RETURN c
	END NewTableOn;


	PROCEDURE SetDir* (d: Directory);
	BEGIN
		ASSERT(d # NIL, 20); dir := d
	END SetDir;

	PROCEDURE DepositAnchor*;
		VAR p: Controls.Prop;
	BEGIN
		InitProp(p); Views.Deposit(dir.NewAnchor(p))
	END DepositAnchor;

	PROCEDURE DepositNavigator*;
		VAR p: Controls.Prop;
	BEGIN
		InitProp(p); Views.Deposit(dir.NewNavigator(p))
	END DepositNavigator;

	PROCEDURE DepositTable*;
		VAR p: Controls.Prop;
	BEGIN
		InitProp(p); Views.Deposit(dir.NewTable(p))
	END DepositTable;

	PROCEDURE Init;
		VAR d: StdDirectory;
	BEGIN
		NEW(d); stdDir := d; dir := d
	END Init;

BEGIN
	Init
END SqlControls.
