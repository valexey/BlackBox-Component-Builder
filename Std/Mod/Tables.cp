MODULE StdTables;
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
		Services, Fonts, Ports, Dialog, Meta, Stores, Models, Views, Controllers, Containers, Properties, Controls,
		TextModels, TextViews, HostPorts;

	CONST
		minVersion = 0;
		tabVersion = 2;
		maxCol = 256;
		defColW = 30 * Ports.mm;
		left = -1; right = -2; center = -3;	(* adjustment modes *)
		move = 1; adjust = 2; field = 3;	(* cursor modes *)

		tab = 09X; ltab = 0AX; lineChar = 0DX; esc = 1BX;

		line* = 0DX;			(* line feed characher for labels *)
		deselect* = -1; select* = -2; changed* = -3; (* notifier op-values *)
		layoutEditable* = 0; dataEditable* = 1; selectionStyle* = 2; (* values for property validity in Prop *)
		noSelect* = 0; cellSelect* = 1; rowSelect* = 2; colSelect* = 3; crossSelect* = 4; (* selection style values *)

	TYPE
		Table* = RECORD
			rows-, cols-: INTEGER;
			selection: Selection;
			labels: POINTER TO ARRAY OF Dialog.String;
			data: POINTER TO ARRAY OF ARRAY OF Dialog.String;
			weights: POINTER TO ARRAY OF ARRAY OF INTEGER;
			styles: POINTER TO ARRAY OF ARRAY OF SET;
			colors: POINTER TO ARRAY OF ARRAY OF Ports.Color
		END;

		Selection = POINTER TO RECORD
			on: BOOLEAN;
			row, col: INTEGER	(** only valid if on = TRUE **)
		END;

		Prop* = POINTER TO RECORD (Properties.Property)
			layoutEditable*, dataEditable*: BOOLEAN;
			selectionStyle*: INTEGER
		END;

		Control = POINTER TO RECORD (Controls.Control)
			(* persistent *)
			sprop: Properties.StdProp;	(* font attributes *)
			tprop: Prop;	(* table attributes *)
			columns: INTEGER;	(* width[0..columns-1] and mode[0..columns-1] are defined *)
			width: ARRAY maxCol OF INTEGER;
			mode: ARRAY maxCol OF INTEGER;

			(* not persistent *)
			fldFont, titFont: Fonts.Font;	(* cell fonts *)
			rowHeight, labelHeight, baseOff: INTEGER;	(* height of rows, height of the label row and offset for font *)
			orgRow, orgX: INTEGER;	(* scroll state, orgX in coordinates, orgRow in rows *)

			selRow, selCol: INTEGER;	(* selected field *)
			showSelection: BOOLEAN;
			hasSelection: BOOLEAN;	(* control has a selected field *)

			x, y, w, h: INTEGER;
			field: Views.View	(* textfield used to enter cell content *)
		END;

		Directory* = POINTER TO ABSTRACT RECORD END;
		StdDirectory = POINTER TO RECORD (Directory) END;

		PropOp = POINTER TO RECORD (Stores.Operation)	(* typeface, style and size *)
			tab: Control;
			sprop: Properties.StdProp;
			tprop: Prop
		END;

		FormatOp = POINTER TO RECORD (Stores.Operation)	(* cell format *)
			tab: Control;
			col, width, mode: INTEGER
		END;

		TableValue = RECORD (Meta.Value)
			t: Table
		END;

		Context = POINTER TO RECORD (Models.Context)
			w, h: INTEGER;
			base: Views.View
		END;

		Action = POINTER TO RECORD (Services.Action) END;

	VAR
		dir-, stdDir-: Directory;
		text*: Dialog.String;	(* used to edit cell. Only one cell can be active at once *)
		dlg*: RECORD
			layoutEditable*, dataEditable*: BOOLEAN;
			selectionStyle*: Dialog.List;
			fingerprint: INTEGER;
			known, valid: SET
		END;
		action: Action;
		
	PROCEDURE CountLines (IN s: Dialog.String): INTEGER;
		VAR i, r, l: INTEGER;
	BEGIN
		r := 1; l := LEN(s$);
		FOR i := 0 TO l - 1 DO IF s[i] = line THEN INC(r) END END;
		RETURN r
	END CountLines;


	(* Prop *)

	PROCEDURE (p: Prop) IntersectWith* (q: Properties.Property; OUT equal: BOOLEAN);
		VAR valid: SET; p0: Prop;
	BEGIN
		p0 := q(Prop);
		valid := p.valid * p0.valid; equal := TRUE;
		IF p.layoutEditable # p0.layoutEditable THEN EXCL(valid, layoutEditable) END;
		IF p.dataEditable # p0.dataEditable THEN EXCL(valid, dataEditable) END;
		IF p.selectionStyle # p0.selectionStyle THEN EXCL(valid, selectionStyle) END;
		IF valid # p.valid THEN p.valid := valid; equal := FALSE END
	END IntersectWith;

	(* Table *)

	PROCEDURE (VAR tab: Table) SetSize* (rows, cols: INTEGER), NEW;
		VAR i, j, fr, fc: INTEGER;
			labels: POINTER TO ARRAY OF Dialog.String;
			data: POINTER TO ARRAY OF ARRAY OF Dialog.String;
			weights: POINTER TO ARRAY OF ARRAY OF INTEGER;
			styles: POINTER TO ARRAY OF ARRAY OF SET;
			colors: POINTER TO ARRAY OF ARRAY OF Ports.Color;
	BEGIN
		ASSERT((rows >= 0) & (cols >= 0), 20); 
		ASSERT((cols > 0) OR ((cols = 0) & (rows = 0)), 21); 
		tab.rows := rows; tab.cols := cols;
		IF rows > 0 THEN
			data := tab.data; NEW(tab.data, rows, cols); weights := tab.weights; NEW(tab.weights, rows, cols);
			styles := tab.styles; NEW(tab.styles, rows, cols); colors := tab.colors; NEW(tab.colors, rows, cols);
			IF data # NIL THEN
				FOR i := 0 TO MIN(rows, LEN(data, 0)) - 1 DO
					FOR j := 0 TO MIN(cols, LEN(data, 1)) - 1 DO
						tab.data[i, j] := data[i, j]; tab.weights[i, j] := weights[i, j]; tab.styles[i, j] := styles[i, j];
						tab.colors[i, j] := colors[i, j]
					END
				END
			END;
			(* set defaults *)
			IF data = NIL THEN fr := 0; fc := 0 ELSE fr := LEN(data, 0); fc := LEN(data, 1) END; 
			FOR i := fr TO LEN(tab.data, 0) - 1 DO
				FOR j := fc TO LEN(tab.data, 1) - 1 DO
					tab.weights[i, j] := Fonts.normal; tab.styles[i, j] := {}; tab.colors[i, j] := Ports.black 
				END
			END
		ELSE
			tab.data := NIL
		END;
		IF cols > 0 THEN
			labels := tab.labels; NEW(tab.labels, cols);
			IF labels # NIL THEN
				FOR i := 0 TO MIN(cols, LEN(labels)) - 1 DO
					tab.labels[i] := labels[i]
				END
			END
		ELSE
			tab.labels := NIL
		END;
		IF tab.selection = NIL THEN
			NEW(tab.selection)
		ELSE
			tab.selection.on := FALSE
		END
	END SetSize;

	PROCEDURE (VAR tab: Table) SetItem* (row, col: INTEGER; (*IN*) item: Dialog.String), NEW;
	BEGIN
		ASSERT(tab.data # NIL, 20);
		tab.data[row, col] := item
	END SetItem;

	PROCEDURE (VAR tab: Table) GetItem* (row, col: INTEGER; OUT item: Dialog.String), NEW;
	BEGIN
		ASSERT(tab.data # NIL, 20);
		item := tab.data[row, col]
	END GetItem;

	PROCEDURE (VAR tab: Table) SetLabel* (col: INTEGER; (*IN*)label: Dialog.String), NEW;
	BEGIN
		ASSERT(tab.labels # NIL, 20);
		tab.labels[col] := label
	END SetLabel;

	PROCEDURE (VAR tab: Table) GetLabel* (col: INTEGER; OUT label: Dialog.String), NEW;
	BEGIN
		ASSERT(tab.labels # NIL, 20);
		label := tab.labels[col]
	END GetLabel;

	PROCEDURE (VAR tab: Table) HasSelection* (): BOOLEAN, NEW;
	BEGIN
		RETURN (tab.selection # NIL) & (tab.selection.on)
	END HasSelection;

	PROCEDURE (VAR tab: Table) GetSelection* (OUT row, col: INTEGER), NEW;
	BEGIN
		ASSERT(tab.selection # NIL, 20);
		ASSERT(tab.selection.on, 21);
		row := tab.selection.row; col := tab.selection.col
	END GetSelection;

	PROCEDURE (VAR tab: Table) Select* (row, col: INTEGER), NEW;
	BEGIN
		ASSERT(tab.selection # NIL, 20);
		tab.selection.on := TRUE; tab.selection.row := row; tab.selection.col := col
	END Select;

	PROCEDURE (VAR tab: Table) Deselect*, NEW;
	BEGIN
		ASSERT(tab.selection # NIL, 20);
		tab.selection.on := FALSE		
	END Deselect;

	PROCEDURE (VAR tab: Table) SetAttr* (l, t, r, b: INTEGER; style: SET; weight: INTEGER; color: Ports.Color), NEW;
		VAR i, j: INTEGER;
	BEGIN
		ASSERT(tab.data # NIL, 20);
		FOR i := t TO b DO
			FOR j := l TO r DO
				tab.weights[i, j] := weight; tab.styles[i, j] := style; tab.colors[i, j] := color
			END
		END
	END SetAttr;

	PROCEDURE (VAR tab: Table) GetAttr* (row, col: INTEGER; OUT style: SET; OUT weight: INTEGER;
																OUT color: Ports.Color), NEW;
	BEGIN
		ASSERT(tab.data # NIL, 20);
		weight := tab.weights[row, col]; style := tab.styles[row, col]; color := tab.colors[row, col]
	END GetAttr;


	(* Context *)

	PROCEDURE (c: Context) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.w; h := c.h
	END GetSize;

	PROCEDURE (c: Context) Normalize (): BOOLEAN;
	BEGIN
		RETURN c.base.context.Normalize()
	END Normalize;

	PROCEDURE (c: Context) Consider (VAR p: Models.Proposal);
	BEGIN
		c.base.context.Consider(p)
	END Consider;

	PROCEDURE (c: Context) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL
	END ThisModel;

	PROCEDURE NewField(t: Control; col, w, h: INTEGER): Views.View;
		VAR c: Context; p: Controls.Prop; v: Views.View; prop: Properties.StdProp; setMsg: Properties.SetMsg;
	BEGIN
		NEW(p); p.link := "StdTables.text";
		IF t.mode[col] = left THEN p.opt[Controls.left] := TRUE
		ELSIF t.mode[col] = right THEN p.opt[Controls.right] := TRUE
		END;
		(* bug in controls, thus adjusting has to be set to left mode *)
		p.opt[Controls.left] := TRUE;
		p.opt[Controls.right] := FALSE;
		v := Controls.dir.NewField(p);

		NEW(c); c.w := w; c.h := h; c.base := t;
		v.InitContext(c);

		NEW(prop);
		prop.typeface := t.fldFont.typeface;
		prop.size := t.fldFont.size - Fonts.point;
		prop.style.val := t.fldFont.style;
		prop.style.mask := t.fldFont.style;
		prop.weight := t.fldFont.weight;
		prop.valid := {Properties.typeface..Properties.weight};
		prop.known := prop.valid;
		setMsg.prop := prop;
		Views.HandlePropMsg(v, setMsg);
		RETURN v
	END NewField;

	PROCEDURE SendNotifyMsg (c: Control);
		VAR msg: Views.NotifyMsg;
	BEGIN
		msg.id0 := c.item.adr; msg.id1 := msg.id0 + c.item.Size();
		msg.opts := {2, 4}; (* update, guardcheck *)
		Views.Omnicast(msg)
	END SendNotifyMsg;

	PROCEDURE GetSize(c: Control; OUT rows, cols: INTEGER);
		VAR item: Meta.Item;
	BEGIN
		IF c.item.Valid() THEN
			item := c.item;
			c.item.Lookup("rows", item); rows := item.IntVal();
			c.item.Lookup("cols", item); cols := item.IntVal()
		ELSE
			rows := 0; cols := 0
		END
	END GetSize;

	PROCEDURE GetLabel(c: Control; col: INTEGER; OUT val: Dialog.String; OUT ok: BOOLEAN);
		VAR t: TableValue;
	BEGIN
		ASSERT(c.item.Valid(), 20);
		c.item.GetVal(t, ok); t.t.GetLabel(col, val)
	END GetLabel;

	PROCEDURE GetAttr(c: Control; row, col: INTEGER; 
								OUT style: SET; OUT weight: INTEGER; OUT color: Ports.Color; OUT ok: BOOLEAN);
		VAR t: TableValue;
	BEGIN
		ASSERT(c.item.Valid(), 20);
		c.item.GetVal(t, ok); t.t.GetAttr(row, col, style, weight, color)
	END GetAttr;

	PROCEDURE GetText(c: Control; row, col: INTEGER; OUT val: Dialog.String; OUT ok: BOOLEAN);
		VAR t: TableValue;
	BEGIN
		ASSERT(c.item.Valid(), 20);
		c.item.GetVal(t, ok); t.t.GetItem(row, col, val)
	END GetText;

	PROCEDURE SetText(c: Control; f: Views.Frame; row, col: INTEGER; IN val: Dialog.String; OUT ok: BOOLEAN);
		VAR t: TableValue;
	BEGIN
		ASSERT(c.item.Valid(), 20);
		c.item.GetVal(t, ok);
		IF ok THEN
			t.t.SetItem(row, col, val);
			(* Notify(c, row, col, {}, changed); *)
			SendNotifyMsg(c);
			Controls.Notify(c, f, changed, row, col)
		END
	END SetText;

	PROCEDURE GetSelection(c: Control; OUT on: BOOLEAN; OUT row, col: INTEGER; OUT ok: BOOLEAN);
		VAR t: TableValue;
	BEGIN
		ASSERT(c.item.Valid(), 20);
		c.item.GetVal(t, ok);
		IF ok THEN
			on := t.t.HasSelection();
			IF on THEN t.t.GetSelection(row, col) ELSE row := -1; col := -1 END
		END
	END GetSelection;

	PROCEDURE SetSelection(c: Control; f: Views.Frame; on: BOOLEAN; row, col: INTEGER; OUT ok: BOOLEAN);
		VAR t: TableValue;
	BEGIN
		ASSERT(c.item.Valid(), 20);
		c.item.GetVal(t, ok);
		IF ok THEN
			IF on THEN t.t.Select(row, col); Controls.Notify(c, f, select, row, col)
			ELSE t.t.Deselect; Controls.Notify(c, f, deselect, row, col)
			END;
			SendNotifyMsg(c)
		END
	END SetSelection;


	PROCEDURE SetupControl (t: Control);
		VAR i, asc, dsc, w: INTEGER;
	BEGIN
		t.fldFont := Fonts.dir.This(t.sprop.typeface, t.sprop.size, t.sprop.style.val, Fonts.normal);
		t.titFont := Fonts.dir.This(t.sprop.typeface, t.sprop.size, t.sprop.style.val, Fonts.bold);
		t.rowHeight := 3 * t.sprop.size DIV 2;
		t.fldFont.GetBounds(asc, dsc, w);
		t.rowHeight := asc + dsc + 4 * Ports.point;
		t.baseOff := (t.rowHeight - asc - dsc) DIV 2 + asc;
		i := t.columns;
		WHILE i < maxCol DO t.width[i] := defColW; t.mode[i] := center; INC(i) END;
		IF t.field # NIL THEN
			t.field.context(Context).h := t.rowHeight
		END;
		t.labelHeight := t.rowHeight 
	END SetupControl;


	(** Directory **)

	PROCEDURE (d: Directory) NewControl* (p: Controls.Prop): Views.View, NEW, ABSTRACT;


	(* PropOp *)

	PROCEDURE (op: PropOp) Do;
		VAR c: Control; sprop: Properties.StdProp; tprop: Prop;
	BEGIN
		ASSERT((op.sprop # NIL) OR (op.tprop # NIL), 20);
		c := op.tab;
		IF op.sprop # NIL THEN
			sprop := Properties.CopyOf(c.sprop)(Properties.StdProp);
			sprop.valid := op.sprop.valid;	(* fields to be restored *)
			IF Properties.typeface IN sprop.valid THEN c.sprop.typeface := op.sprop.typeface END;
			IF Properties.size IN sprop.valid THEN c.sprop.size := op.sprop.size END;
			IF Properties.style IN sprop.valid THEN
				c.sprop.style.mask := c.sprop.style.mask + op.sprop.style.mask;
				c.sprop.style.val := c.sprop.style.val - op.sprop.style.mask + op.sprop.style.val
			END;
			IF sprop.valid # {} THEN SetupControl(c) END;
			op.sprop := sprop
		END;
		IF op.tprop # NIL THEN
			tprop := Properties.CopyOf(c.tprop)(Prop);
			tprop.valid := op.tprop.valid;	(* fields to be restored *)
			IF layoutEditable IN tprop.valid THEN c.tprop.layoutEditable := op.tprop.layoutEditable END;
			IF dataEditable IN tprop.valid THEN c.tprop.dataEditable := op.tprop.dataEditable END;
			IF selectionStyle IN tprop.valid THEN c.tprop.selectionStyle := op.tprop.selectionStyle END;
			op.tprop := tprop
		END;
		Views.Update(c, Views.rebuildFrames)
	END Do;


	(* FormatOp *)

	PROCEDURE (op: FormatOp) Do;
		VAR t: Control; c, w, m: INTEGER;
	BEGIN
		t := op.tab; c := op.col; w := op.width; m := op.mode;
		op.width := t.width[c]; op.mode := t.mode[c];
		t.width[c] := w; t.mode[c] := m;
		IF c >= t.columns THEN t.columns := c + 1 END;
		Views.Update(t, Views.keepFrames)
	END Do;


	(* properties *)

	PROCEDURE PollProp (c: Control; VAR list: Properties.Property);
		VAR p: Properties.Property;
	BEGIN
		p := Properties.CopyOf(c.sprop);
		p.valid := {Properties.typeface, Properties.size, Properties.style, Properties.weight};
		p.known := p.valid; p.readOnly := {Properties.weight};
		Properties.Insert(list, p);
		p := Properties.CopyOf(c.tprop);
		p.valid := {layoutEditable, dataEditable, selectionStyle}; p.known := p.valid; p.readOnly := {};
		Properties.Insert(list, p)
	END PollProp;

	PROCEDURE SetProp (c: Control; p: Properties.Property);
		VAR op: PropOp; valid: SET;
	BEGIN
		op := NIL;
		WHILE p # NIL DO
			WITH p: Properties.StdProp DO
				valid := p.valid * {Properties.typeface, Properties.size, Properties.style};
				IF valid # {} THEN
					IF op = NIL THEN NEW(op); op.tab := c END;
					op.sprop := Properties.CopyOf(p)(Properties.StdProp);
					op.sprop.valid := valid
				END
			| p: Prop DO
				valid := p.valid * {layoutEditable, dataEditable, selectionStyle};
				IF valid # {} THEN
					IF op = NIL THEN NEW(op); op.tab := c END;
					op.tprop := Properties.CopyOf(p)(Prop);
					op.tprop.valid := valid
				END
			ELSE
			END;
			p := p.next
		END;
		IF op # NIL THEN Views.Do(c, "#System:SetProp", op) END
	END SetProp;


	(* Control *)

	PROCEDURE DrawBorder (f: Views.Frame; x, y, w, h: INTEGER);
	BEGIN
		f.DrawRect(x, 	y, 	x+f.dot, 	y+h, Ports.fill, Ports.white);
		f.DrawRect(x+f.dot, 	y+0, 	x+2 * f.dot,	y+ h, Ports.fill, Ports.grey25);
		f.DrawRect(x+0, 	y+0, 	x+w, 	y+f.dot, Ports.fill, Ports.white);
		f.DrawRect(x+f.dot, 	y+f.dot, 	x+w, 	y+2 * f.dot, Ports.fill, Ports.grey25);
		f.DrawRect(x+w - f.dot, 	y+0, 	x+w, 	y+h, Ports.fill, Ports.grey50);
		f.DrawRect(x+w - 2 * f.dot, 	y+f.dot, 	x+w - f.dot, 	y+h - f.dot, Ports.fill, Ports.black);
		f.DrawRect(x+0, 	y+h - f.dot, 	x+w, 	y+h, Ports.fill, Ports.grey50);
		f.DrawRect(x+f.dot, 	y+h - 2 * f.dot, 	x+w - f.dot, 	y+h - f.dot, Ports.fill, Ports.black)
	END DrawBorder;

	PROCEDURE DrawLabel (f: Views.Frame; x, y, w, x0, y0, w0, h0, mode: INTEGER; 
									VAR is: ARRAY OF CHAR; font: Fonts.Font; rowHeight: INTEGER);
		VAR dx, i, j, si, sw, rw: INTEGER; s: Dialog.String;
	BEGIN
		DEC(w, 4 * f.dot); INC(x, 2 * f.dot);
		j := 0; y := y - rowHeight;
		WHILE is[j] # 0X DO
			si := 0;
			WHILE (is[j] # 0X) & (is[j] # line) DO s[si] := is[j]; INC(si); INC(j) END;
			IF is[j] = line THEN INC(j) END;
			s[si] := 0X; y := y + rowHeight;
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
				dx := x;
				IF mode = center THEN dx := x +  (w - sw) DIV 2
				ELSIF mode = right THEN dx := x + w - sw
				END;
				f.DrawString(dx, y, Ports.black, s, font)
			END
		END;
		DrawBorder(f, x0, y0, w0, h0)
	END DrawLabel;

	PROCEDURE DrawField (f: Views.Frame; x, y, w, mode: INTEGER; VAR s: ARRAY OF CHAR; font: Fonts.Font;
											color: Ports.Color);
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
			f.DrawString(x, y, color, s, font)
		END
	END DrawField;

	PROCEDURE GetRect(t: Control; f: Views.Frame; row, col: INTEGER; OUT x, y, w, h: INTEGER;
										OUT visible: BOOLEAN);
		VAR c: INTEGER;
	BEGIN
		c := 0; x := 2 * f.dot - t.orgX;
		WHILE c < col DO INC(x, t.width[c]); INC(c) END;
		IF row >= t.orgRow THEN
			visible := TRUE;
			IF row = -1 THEN
				h := t.labelHeight
			ELSE
				h := t.rowHeight
			END;
			y := (row - t.orgRow) * t.rowHeight + t.labelHeight + 3 * f.dot;
			w := t.width[col] 
		ELSE
			visible := FALSE
		END
	END GetRect;


	PROCEDURE DrawSelection (tab: Control; f: Views.Frame; selRow, selCol: INTEGER; show: BOOLEAN);
		VAR c, l, t, r, b, cols, rows: INTEGER;
	BEGIN
		IF (selRow >= tab.orgRow) OR (tab.tprop.selectionStyle IN {colSelect, crossSelect}) THEN
			GetSize(tab, rows, cols);
			IF (0 <= selRow) & (selRow < rows) & (0 <= selCol) & (selCol < cols) THEN
				IF tab.tprop.selectionStyle = cellSelect THEN	(* mark selected cell *)
					l := 2 * f.dot - tab.orgX; c := 0;
					WHILE c < selCol DO INC(l, tab.width[c]); INC(c) END;
					r := l + tab.width[selCol];					
					t := (selRow - tab.orgRow) * tab.rowHeight + tab.labelHeight + 3 * f.dot;
					b := t + tab.rowHeight;
					f.MarkRect(l + f.dot, t + f.dot, r - 2 * f.dot, b - 2 * f.dot, Ports.fill, Ports.hilite, show)
				ELSIF tab.tprop.selectionStyle = rowSelect THEN	(* mark selected row *)
					l := 2 * f.dot - tab.orgX; r := l; c := 0;
					WHILE c < cols DO INC(r, tab.width[c]); INC(c) END;
					t := (selRow - tab.orgRow) * tab.rowHeight + tab.labelHeight + 3 * f.dot;
					b := t + tab.rowHeight;
					f.MarkRect(l + f.dot, t + f.dot, r - 2 * f.dot, b - 2 * f.dot, Ports.fill, Ports.hilite, show)
				ELSIF tab.tprop.selectionStyle = colSelect THEN	(* mark selected column *)
					l := 2 * f.dot - tab.orgX; c := 0;
					WHILE c < selCol DO INC(l, tab.width[c]); INC(c) END;
					r := l + tab.width[selCol];
					t := tab.labelHeight + 3 * f.dot;
					b := t + tab.rowHeight * (rows - tab.orgRow);
					f.MarkRect(l + f.dot, t + f.dot, r - 2 * f.dot, b - 2 * f.dot, Ports.fill, Ports.hilite, show)
				ELSIF tab.tprop.selectionStyle = crossSelect THEN	(* mark both the row and column *)
					IF selRow >= tab.orgRow THEN
						l := 2 * f.dot - tab.orgX; r := l; c := 0;
						t := (selRow - tab.orgRow) * tab.rowHeight + tab.labelHeight + 3 * f.dot;
						b := t + tab.rowHeight;
						IF selCol > 0 THEN
							WHILE c < selCol DO INC(r, tab.width[c]); INC(c) END;
							f.MarkRect(l + f.dot, t + f.dot, r - 2 * f.dot, b - 2 * f.dot, Ports.fill, Ports.hilite, show)
						END;
						IF selCol + 1 < cols THEN
							r := r + tab.width[selCol]; l := r; c := selCol + 1;
							WHILE c < cols DO INC(r, tab.width[c]); INC(c) END;
							f.MarkRect(l + f.dot, t + f.dot, r - 2 * f.dot, b - 2 * f.dot, Ports.fill, Ports.hilite, show)
						END
					END;
					l := 2 * f.dot - tab.orgX; c := 0;
					WHILE c < selCol DO INC(l, tab.width[c]); INC(c) END;
					r := l + tab.width[selCol];
					t := tab.labelHeight + 3 * f.dot;
					b := t + tab.rowHeight * (rows - tab.orgRow);
					f.MarkRect(l + f.dot, t + f.dot, r - 2 * f.dot, b - 2 * f.dot, Ports.fill, Ports.hilite, show)
				END
			END
		END
	END DrawSelection;

	PROCEDURE Select (c: Control; f: Views.Frame; on: BOOLEAN);
		VAR ok: BOOLEAN;
	BEGIN
		IF on # c.hasSelection THEN
			c.hasSelection := on; c.showSelection := on;
			SetSelection(c, f, on, c.selRow, c.selCol, ok)
		END
	END Select;

	PROCEDURE ViewFromSelection (t: Control): Views.View;
		VAR str: Dialog.String; ok: BOOLEAN;
	BEGIN
		IF t.hasSelection & t.showSelection THEN
			GetText(t, t.selRow, t.selCol, str, ok);
			IF ok THEN
				RETURN TextViews.dir.New(TextModels.dir.NewFromString(str))
			ELSE
				RETURN NIL
			END
		ELSE RETURN NIL
		END
	END ViewFromSelection;

	PROCEDURE GetControlSize (t: Control; dot: INTEGER; OUT w, h: LONGINT);
		VAR r, c, i: INTEGER;
	BEGIN
		IF ~t.disabled & t.item.Valid() THEN
			w := 0; h := 0;
			GetSize(t, r, c);
			i := 0;
			WHILE (i < c) & (i < maxCol) DO INC(w, t.width[i]); INC(i) END;
			INC(w, 3 * dot);
			h := LONG(r - t.orgRow) * t.rowHeight + t.labelHeight
		ELSE
			w := Views.undefined; h := Views.undefined
		END
	END GetControlSize;

	PROCEDURE CheckPos (t: Control; x, y, dot: INTEGER; VAR col, type, p: INTEGER);
		VAR c, a: INTEGER; w, h: LONGINT;
	BEGIN
		GetControlSize(t, dot, w, h);
		INC(x, t.orgX);
		IF (x >= 0) & (x <= w) & (y >= 0) & (y <= h) THEN
			c := 0; w := 0; type := 0; INC(x, dot);
			WHILE (c < maxCol) & (x >= w + t.width[c]) DO INC(w, t.width[c]); INC(c) END;
			IF (x <= w + 3 * dot) & (c > 0) THEN
				col := c - 1; p := SHORT(w) + dot - t.orgX; type := move
			ELSIF y - dot < t.labelHeight THEN
				type := adjust;
				col := c; a := t.width[c] DIV 3;
				IF x < w + a THEN p := left
				ELSIF x > w + a * 2 THEN p := right
				ELSE p := center
				END
			ELSE
				col := c; p := (y - t.labelHeight - dot) DIV t.rowHeight + t.orgRow - 1 + 1; type := field
			END
		ELSE type := 0
		END
	END CheckPos;

	PROCEDURE MoveLine (t: Control; f: Views.Frame; col, x0: INTEGER);
		VAR w, h, x, y, x1, limit: INTEGER; m: SET; isDown: BOOLEAN; op: FormatOp;
	BEGIN
		t.context.GetSize(w, h);
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

	PROCEDURE ChangeAdjust (t: Control; col, mode: INTEGER);
		VAR op: FormatOp;
	BEGIN
		NEW(op); op.tab := t; op.col := col;
		op.width := t.width[col]; op.mode := mode;
		Views.Do(t, "#System:SetLayout", op)
	END ChangeAdjust;

	PROCEDURE ControlSections (t: Control; f: Views.Frame; vertical: BOOLEAN; OUT size, part, pos: INTEGER);
		VAR r, c, w, max: INTEGER;
	BEGIN
		size := 0; part := 0; pos := 0; GetSize(t, r, c);
		IF vertical THEN
			size := r;
			part := (f.b - (f.t + t.labelHeight + 3*f.dot)) DIV t.rowHeight;
			pos := t.orgRow
		ELSE
			w := 0; max := MIN(c, maxCol);
			c := 0;
			WHILE (c < max) DO INC(w, t.width[c]); INC(c) END;
			size := w + 3 * f.dot;
			part := f.r - f.l;
			pos := t.orgX
		END
	END ControlSections;

	PROCEDURE ScrollControl (t: Control; f: Views.Frame; op, pos: INTEGER; vertical: BOOLEAN);
		VAR size, part, p, delta, l, t0, r, b: INTEGER;
	BEGIN
		IF vertical THEN
			ControlSections(t, f, TRUE, size, part, p);
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
			delta := (f.gy + t.labelHeight + 3 * f.dot) DIV f.unit;
			f.rider.GetRect(l, t0, r, b);
			IF b > delta THEN
				IF t0 < delta THEN f.rider.SetRect(l, delta, r, b) END;
				Views.Scroll(t, 0, (t.orgRow - p) * t.rowHeight);
				IF f.rider # NIL THEN
					f.rider.SetRect(l, t0, r, b)
				END
			END;
			t.orgRow := p
		ELSE
			ControlSections(t, f, FALSE, size, part, p);
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
	END ScrollControl;

	PROCEDURE HandleChar (t: Control; f: Views.Frame; ch: CHAR);
	BEGIN
		CASE ch OF
		| 10X: ScrollControl(t, f, Controllers.decPage, 0, FALSE)
		| 11X: ScrollControl(t, f, Controllers.incPage, 0, FALSE)
		| 12X: ScrollControl(t, f, Controllers.decPage, 0, TRUE)
		| 13X: ScrollControl(t, f, Controllers.incPage, 0, TRUE)
		| 14X: ScrollControl(t, f, Controllers.gotoPos, 0, FALSE)
		| 15X: ScrollControl(t, f, Controllers.gotoPos, MAX(INTEGER), FALSE)
		| 16X: ScrollControl(t, f, Controllers.gotoPos, 0, TRUE)
		| 17X: ScrollControl(t, f, Controllers.gotoPos, MAX(INTEGER), TRUE)
		| 1CX: ScrollControl(t, f, Controllers.decLine, 0, FALSE)
		| 1DX: ScrollControl(t, f, Controllers.incLine, 0, FALSE)
		| 1EX: ScrollControl(t, f, Controllers.decLine, 0, TRUE)
		| 1FX: ScrollControl(t, f, Controllers.incLine, 0, TRUE)
		| 07X, 08X, 1BX: Select(t, f, FALSE)	(* rdel, del, esc *)
		ELSE
		END
	END HandleChar;


	PROCEDURE (t: Control) Internalize2 (VAR rd: Stores.Reader);
		VAR thisVersion, i: INTEGER; lockedLayout, lockedData, markRow, markCol: BOOLEAN;
	BEGIN
		rd.ReadVersion(minVersion, tabVersion, thisVersion);
		IF ~rd.cancelled THEN
			IF thisVersion = tabVersion THEN (* current table version *)
				NEW(t.tprop);
				rd.ReadBool(t.tprop.layoutEditable);
				rd.ReadBool(t.tprop.dataEditable);
				rd.ReadInt(t.tprop.selectionStyle)
			ELSIF thisVersion = 1 THEN (* intermediate Table version (after 1.4 Beta, before 1.4 final) *)
				rd.ReadBool(lockedLayout);
				rd.ReadBool(lockedData);
				rd.ReadBool(markRow);
				rd.ReadBool(markCol);
				NEW(t.tprop);
				t.tprop.layoutEditable := ~lockedLayout; t.tprop.dataEditable := ~lockedData;
				IF markRow THEN
					IF markCol THEN t.tprop.selectionStyle := noSelect ELSE t.tprop.selectionStyle := rowSelect END
				ELSE
					IF markCol THEN t.tprop.selectionStyle := colSelect ELSE t.tprop.selectionStyle := cellSelect END
				END
			ELSE (* old version, 1.4 Beta *)
				t.tprop.layoutEditable := TRUE; t.tprop.dataEditable := TRUE; t.tprop.selectionStyle := cellSelect
			END;
			NEW(t.sprop);
			rd.ReadString(t.sprop.typeface);
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
			SetupControl(t)
		END
	END Internalize2;

	PROCEDURE (t: Control) Externalize2 (VAR wr: Stores.Writer);
		VAR i: INTEGER;
	BEGIN
		wr.WriteVersion(tabVersion);
		wr.WriteBool(t.tprop.layoutEditable);
		wr.WriteBool(t.tprop.dataEditable);
		wr.WriteInt(t.tprop.selectionStyle);
		wr.WriteString(t.sprop.typeface);
		wr.WriteInt(t.sprop.size);
		wr.WriteSet(t.sprop.style.val);
		wr.WriteInt(t.columns); i := 0;
		WHILE i < t.columns DO
			wr.WriteInt(t.width[i]);
			wr.WriteInt(t.mode[i]);
			INC(i)
		END
	END Externalize2;

	PROCEDURE (t: Control) CopyFromSimpleView2 (source: Controls.Control);
	BEGIN
		WITH source: Control DO
			t.sprop := Properties.CopyOf(source.sprop)(Properties.StdProp);
			t.tprop := Properties.CopyOf(source.tprop)(Prop);
			t.columns := source.columns;
			t.width := source.width;
			t.mode := source.mode;
			SetupControl(t)
		END
	END CopyFromSimpleView2;

	PROCEDURE (t: Control) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;

	PROCEDURE Paint (c: Control; f: Views.Frame; l, t, r, b: INTEGER);
		VAR width, w, h, rows, cols, row, col, x, y, rowHeight, dl, weight: INTEGER; font: Fonts.Font;
			str: Dialog.String; ok: BOOLEAN;style: SET; color: Ports.Color;
	BEGIN
		c.context.GetSize(w, h);
		IF ~c.disabled & c.item.Valid() THEN
			GetSize(c, rows, cols); 
			DEC(c.rowHeight, c.rowHeight MOD f.unit);
			row := -1; y := 2 * f.dot; font := c.titFont;
			WHILE (row < rows) & (y < b) DO
				IF row = - 1 THEN
					rowHeight := 1;
					FOR col := 0 TO cols - 1 DO
						GetLabel(c, col, str, ok); dl := CountLines(str);
						IF dl > rowHeight THEN rowHeight := dl END
					END;
					rowHeight := rowHeight * c.rowHeight;
					c.labelHeight := rowHeight
				ELSE 
					rowHeight := c.rowHeight
				END;
				IF ~Views.IsPrinterFrame(f) OR (y + rowHeight < b) THEN
					col := 0; x := 2 * f.dot - c.orgX;
					WHILE (col < cols) & (col < maxCol) & (x < r) DO
						width := c.width[col];
						IF x + width >= 0 THEN
							f.DrawRect(x - f.dot, y, x, y + rowHeight, Ports.fill, Ports.black);	(* left vertical separation line *)
							IF ~Views.IsPrinterFrame(f) OR (x + width < r) THEN
								IF row = -1 THEN
									GetLabel(c, col, str, ok);
									f.DrawRect(x, y, x + width - f.dot, y + rowHeight - f.dot, Ports.fill, Ports.grey25);
										(* background *)
									DrawLabel(f, x, y + c.baseOff, width - f.dot, x, y, width, rowHeight,
														c.mode[col], str, font, c.rowHeight)
								ELSE
									GetText(c, row, col, str, ok); GetAttr(c, row, col, style, weight, color, ok);
									f.DrawRect(x, y, x + width - f.dot, y + rowHeight - f.dot, Ports.fill, Ports.white);
										(* background *)
									DrawField(f, x, y + c.baseOff, width - f.dot, c.mode[col], str, 
													Fonts.dir.This(font.typeface, font.size, style, weight), color)
								END
							END
						END;
						INC(col); INC(x, width)
					END;
					f.DrawRect(x - f.dot, y, x, y + rowHeight, Ports.fill, Ports.black);	(* right vertical separator line *)
					IF Views.IsPrinterFrame(f) & (x >= r) THEN DEC(x, width) END;

					IF row = -1 THEN row := c.orgRow; font := c.fldFont; INC(y, f.dot)
					ELSE INC(row)
					END
				END;
				INC(y, rowHeight);
				f.DrawRect(f.dot - c.orgX, y - f.dot, x, y, Ports.fill, Ports.black)	(* bottom line *)
			END;
			IF f.dot - c.orgX < x THEN
				f.DrawRect(f.dot - c.orgX, f.dot, x, 2 * f.dot, Ports.fill, Ports.black);
				f.DrawRect(f.dot - c.orgX, c.labelHeight + f.dot, x, c.labelHeight + 2 * f.dot, Ports.fill, Ports.black)
			END
		ELSE
			f.DrawRect(f.dot, f.dot, w - f.dot, h - f.dot, Ports.fill, Ports.grey25);
			f.DrawRect(0, 0, w, h, f.dot, Ports.black)
		END
	END Paint;

	PROCEDURE (c: Control) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR visible: BOOLEAN; g: Views.Frame;
	BEGIN
		Paint(c, f, l, t, r, b);
		IF c.field # NIL THEN
			GetRect(c, f, c.selRow, c.selCol, c.x, c.y, c.w, c.h, visible);
			IF visible THEN Views.InstallFrame(f, c.field, c.x, c.y, 0, TRUE)
			ELSE
				g := Views.ThisFrame(f, c.field); IF g # NIL THEN Views.RemoveFrame(f, g) END
			END
		END
	END Restore;
	
	PROCEDURE (c: Control) Update (f: Views.Frame; op, from, to: INTEGER);
	BEGIN
		Views.Update(c, Views.keepFrames)
	END Update;

	PROCEDURE (c: Control) RestoreMarks (f: Views.Frame; l, t, r, b: INTEGER);
		VAR row, col: INTEGER; ok, on: BOOLEAN;
	BEGIN
		IF c.item.Valid() THEN
			GetSelection(c, on, row, col, ok);
			IF ok THEN
				c.hasSelection := on;
				IF on THEN
					c.selRow := row; c.selCol := col;
					c.showSelection := c.field = NIL;
					IF c.showSelection THEN DrawSelection(c, f, c.selRow, c.selCol, TRUE) END
				END
			END
		END
	END RestoreMarks;

	PROCEDURE AddEditField (t: Control; f: Views.Frame; row, col: INTEGER);
		VAR g: Views.Frame; mMsg: Controllers.MarkMsg; ok, visible: BOOLEAN;
	BEGIN
		GetRect(t, f, row, col, t.x, t.y, t.w, t.h, visible);
		IF visible THEN
			GetText(t, row, col, text, ok);
			t.field := NewField(t, col, t.w, t.h); Stores.Join(t, t.field);

			Views.InstallFrame(f, t.field, t.x, t.y, 0, TRUE);
			mMsg.show := TRUE; mMsg.focus := TRUE;
			g := Views.ThisFrame(f, t.field);
			IF g # NIL THEN Views.ForwardCtrlMsg(g, mMsg) END
		END
	END AddEditField;

	PROCEDURE DisableEditField (t: Control; f: Views.Frame; update: BOOLEAN);
		VAR g: Views.Frame; mMsg: Controllers.MarkMsg; ok: BOOLEAN;
	BEGIN
		IF t.field # NIL THEN
			mMsg.show := FALSE; mMsg.focus := FALSE;
			g := Views.ThisFrame(f, t.field);
			IF g # NIL THEN Views.ForwardCtrlMsg(g, mMsg); Views.RemoveFrame(f, g) END;
			t.field := NIL;
			IF update THEN SetText(t, f, t.selRow, t.selCol, text, ok) END;
			t.showSelection := TRUE
		END
	END DisableEditField;

	PROCEDURE MoveEditField (t: Control; f: Views.Frame; right: BOOLEAN);
		VAR rows, cols: INTEGER; ok: BOOLEAN;
	BEGIN
		DisableEditField(t, f, TRUE);
		GetSize(t, rows, cols);
		Select(t, f, FALSE);
		IF right THEN
			t.selCol := (t.selCol + 1) MOD cols;
			IF t.selCol = 0 THEN t.selRow := (t.selRow + 1) MOD rows END
		ELSE
			t.selCol := (t.selCol - 1) MOD cols;
			IF t.selCol = cols - 1 THEN t.selRow := (t.selRow - 1) MOD rows END
		END;
		t.hasSelection := TRUE; t.showSelection := FALSE;
		SetSelection(t, f, TRUE, t.selRow, t.selCol, ok);
		AddEditField(t, f, t.selRow, t.selCol)
	END MoveEditField;

	PROCEDURE TrackMouse (t: Control; f: Views.Frame; VAR r, c, type: INTEGER);
		VAR x, y, selr, selc: INTEGER; m: SET; isDown, sel: BOOLEAN;
	BEGIN
		selr := t.selRow; selc := t.selCol; sel := t.hasSelection;
		REPEAT
			f.Input(x, y, m, isDown);
			CheckPos(t, x, y, f.dot, c, type, r);
			IF sel & ((type # field) OR (selr # r) OR (selc # c)) THEN
				DrawSelection(t, f, selr, selc, FALSE)
			END;
			IF (type = field) & (~sel OR (selr # r) OR (selc # c)) THEN
				DrawSelection(t, f, r, c, TRUE)
			END;
			sel := type = field; selr := r; selc := c
		UNTIL ~isDown;
		IF ~sel & t.hasSelection THEN
			DrawSelection(t, f, t.selRow, t.selCol, TRUE)
		END
	END TrackMouse;

	PROCEDURE (t: Control) HandleCtrlMsg2 (f: Views.Frame; VAR msg: Controllers.Message;
																	VAR focus: Views.View);
		VAR p, col, type: INTEGER; x, y: INTEGER; m: SET; isDown: BOOLEAN;
			ok, rebuild: BOOLEAN; c, w, size, part, pos: INTEGER;
	BEGIN
		IF ~t.disabled & t.item.Valid() THEN
			WITH msg: Controllers.PollOpsMsg DO
				IF t.field # NIL THEN focus := t.field
				ELSIF t.hasSelection THEN msg.valid := {Controllers.copy}
				END
			| msg: Controllers.PollCursorMsg DO
				IF (t.field # NIL) & (msg.x >= t.x) & (msg.x <= t.x + t.w) & (msg.y >= t.y) & (msg.y <= t.y + t.h) THEN
					focus := t.field
				ELSE
					CheckPos(t, msg.x, msg.y, f.dot, col, type, p);
					IF (type = move) & t.tprop.layoutEditable THEN msg.cursor := HostPorts.resizeHCursor
					ELSIF (type = adjust) & t.tprop.layoutEditable THEN msg.cursor := Ports.refCursor
					ELSIF  ~t.readOnly & (type = field) THEN msg.cursor := Ports.tableCursor
					END
				END
			| msg: Controllers.MarkMsg DO
				IF t.field # NIL THEN DisableEditField(t, f, TRUE); Views.Update(t, Views.rebuildFrames) END
			| msg: Controllers.SelectMsg DO
				IF t.field # NIL THEN focus := t.field
				ELSIF ~msg.set THEN Select(t, f, FALSE)
				END
			| msg: Controllers.EditMsg DO
				IF t.field # NIL THEN
					IF msg.op = Controllers.pasteChar THEN
						IF (msg.char = lineChar) OR (msg.char = esc) THEN
							DisableEditField(t, f, msg.char # esc); Views.Update(t, Views.rebuildFrames)
						ELSIF (msg.char = tab) OR (msg.char = ltab) THEN
							MoveEditField(t, f, msg.char = tab); Views.Update(t, Views.rebuildFrames)
						ELSE
							focus := t.field
						END	
					ELSE
						focus := t.field
					END
				ELSE
					IF (msg.op = Controllers.pasteChar) & (msg.char = lineChar) &
						(t.tprop.selectionStyle = cellSelect) & t.hasSelection THEN
						t.showSelection := FALSE;
						SetSelection(t, f, TRUE, t.selRow, t.selCol, ok);
						AddEditField(t, f, t.selRow, t.selCol); Views.Update(t, Views.keepFrames)
					ELSIF msg.op = Controllers.pasteChar THEN
						HandleChar(t, f, msg.char)
					ELSIF msg.op = Controllers.copy THEN
						msg.view := ViewFromSelection(t);
						msg.w := 0; msg.h := 0; msg.isSingle := FALSE
					END
				END
			| msg: Controllers.TrackMsg DO
				IF (t.field # NIL) & (msg.x >= t.x) & (msg.x <= t.x + t.w) & (msg.y >= t.y) & (msg.y <= t.y + t.h) THEN
						focus := t.field
				ELSE
					rebuild := t.field # NIL; DisableEditField(t, f, TRUE);	(* update below after notifications *)
					CheckPos(t, msg.x, msg.y, f.dot, col, type, p);
					IF type = move THEN
						IF t.tprop.layoutEditable THEN
							MoveLine(t, f, col, p)
						END
					ELSIF type = adjust THEN
						IF t.tprop.layoutEditable THEN
							ChangeAdjust(t, col, p);
							REPEAT f.Input(x, y, m, isDown) UNTIL ~isDown
						END
					ELSIF ~t.readOnly & (type = field) THEN
						TrackMouse(t, f, p, col, type);
						IF type = field THEN
							IF ~t.readOnly & t.tprop.dataEditable
								& ((Controllers.doubleClick IN msg.modifiers) OR (t.tprop.selectionStyle = noSelect))
							THEN
								Select(t, f, FALSE);
								t.selRow := p; t.selCol := col; t.hasSelection := TRUE; t.showSelection := FALSE;
								SetSelection(t, f, TRUE, t.selRow, t.selCol, ok);
								AddEditField(t, f, p, col); Views.Update(t, Views.keepFrames)
							ELSE
								IF (t.selRow # p) OR (t.selCol # col) OR ~t.hasSelection THEN
									Select(t, f, FALSE);
									t.selRow := p; t.selCol := col;
									Select(t, f, TRUE)
								ELSIF t.tprop.selectionStyle = cellSelect THEN Select(t, f, FALSE)
								END
							END
						END
					END;
					IF rebuild THEN Views.Update(t, Views.rebuildFrames) END
				END
			| msg: Controllers.PollSectionMsg DO
				ControlSections(t, f, msg.vertical, msg.wholeSize, msg.partSize, msg.partPos);
				IF (msg.partPos > 0) & (msg.partPos > msg.wholeSize - msg.partSize) THEN
					ScrollControl(t, f, Controllers.gotoPos, msg.wholeSize - msg.partSize, msg.vertical);
					ControlSections(t, f, msg.vertical, msg.wholeSize, msg.partSize, msg.partPos)
				END;
				msg.valid := msg.partSize < msg.wholeSize;
				msg.done := TRUE
			| msg: Controllers.ScrollMsg DO
				ScrollControl(t, f, msg.op, msg.pos, msg.vertical);
				msg.done := TRUE
			| msg: Controllers.PageMsg DO
				IF msg.op IN {Controllers.nextPageY, Controllers.gotoPageY} THEN	(* vertical *)
					ControlSections(t, f, TRUE, size, part, pos);
					IF msg.op = Controllers.nextPageY THEN
						t.orgRow := pos + part
					ELSE
						t.orgRow := msg.pageY * part
					END;
					msg.done := TRUE;
					msg.eoy :=t.orgRow >= size
				ELSE (* horizontal *)
					ControlSections(t, f, FALSE, size, part, pos);
					IF msg.op = Controllers.nextPageX THEN
						t.orgX := pos + part
					ELSE
						t.orgX := msg.pageX * part
					END;
					IF (t.orgX > 0) & (t.orgX < size) THEN
						c := 0; w := 0;
						WHILE w < t.orgX DO INC(w, t.width[c]); INC(c) END;
						t.orgX := w - t.width[c-1] + 1*f.dot
					END;
					msg.done := TRUE;
					msg.eox :=t.orgX >= size
				END
			ELSE
			END
		END
	END HandleCtrlMsg2;

	PROCEDURE (t: Control) HandlePropMsg2 (VAR msg: Properties.Message);
		VAR w, h: LONGINT;
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF ~t.disabled THEN msg.setFocus := TRUE END
		| msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) & (msg.h = Views.undefined) THEN
				GetControlSize(t, Ports.point, w, h);
				msg.w := SHORT(MIN(w, MAX(INTEGER)));
				msg.h := SHORT(MIN(h, MAX(INTEGER)))
			END;
			IF msg.w = Views.undefined THEN msg.w := 80 * Ports.mm END;
			IF msg.h = Views.undefined THEN msg.h := 30 * Ports.mm END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE;
			msg.verFitToWin := TRUE
		| msg: Controls.PropPref DO
			msg.valid := {Controls.link, Controls.label, Controls.guard, Controls.notifier}
		| msg: Properties.PollMsg DO
			PollProp(t, msg.prop)
		| msg: Properties.SetMsg DO
			SetProp(t, msg.prop)
		| msg: Properties.ControlPref DO
			IF (t.field # NIL) & ((msg.char = lineChar) OR (msg.char = esc) OR (msg.char = tab) OR (msg.char = ltab))
				OR (t.field = NIL) & (t.tprop.selectionStyle = cellSelect) & t.hasSelection & (msg.char = lineChar)
			THEN
				msg.accepts := TRUE
			END
		ELSE
			IF t.field # NIL THEN Views.HandlePropMsg(t.field, msg) END
		END
	END HandlePropMsg2;

	PROCEDURE (c: Control) CheckLink (VAR ok: BOOLEAN);
		VAR item: Meta.Item; mod: Meta.Name; name: Meta.Name;
	BEGIN
		item := c.item;
		IF (item.typ = Meta.recTyp) THEN
			item.GetTypeName(mod, name);
			IF (mod = "StdTables") & (name = "Table") THEN
				ok := TRUE
			ELSE
				ok := FALSE
			END
		ELSE ok := FALSE
		END
	END CheckLink;


	(* Property Dialog *)


	PROCEDURE PollPropForDlg;
		VAR q: Properties.Property; p: Prop;
	BEGIN
		dlg.layoutEditable := TRUE; dlg.dataEditable := TRUE; dlg.selectionStyle.index := cellSelect;
		dlg.known := {}; dlg.valid := {};
		Properties.CollectProp(q);
		WHILE (q # NIL) & ~(q IS Prop) DO q := q.next END;
		IF q # NIL THEN
			p := q(Prop);
			dlg.known := p.known; dlg.valid := p.valid;
			IF layoutEditable IN p.valid THEN dlg.layoutEditable := p.layoutEditable END;
			IF dataEditable IN p.valid THEN dlg.dataEditable := p.dataEditable END;
			IF selectionStyle IN p.valid THEN dlg.selectionStyle.index := p.selectionStyle END
		END;
		Dialog.Update(dlg)
	END PollPropForDlg;

	PROCEDURE SetPropFromDlg;
		VAR p: Prop;
	BEGIN
		NEW(p);
		p.known := dlg.known;
		p.valid := dlg.valid;
		p.layoutEditable := dlg.layoutEditable;
		p.dataEditable := dlg.dataEditable;
		p.selectionStyle := dlg.selectionStyle.index;
		Properties.EmitProp(NIL, p)
	END SetPropFromDlg;

	PROCEDURE (a: Action) Do;
		VAR c: Containers.Controller; v: Views.View; fp: INTEGER;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		c := Containers.Focus(); fp := 0;
		IF c # NIL THEN
			c.GetFirstView(Containers.selection, v);
			WHILE v # NIL DO fp := fp + Services.AdrOf(v); c.GetNextView(Containers.selection, v) END
		END;
		IF fp # dlg.fingerprint THEN PollPropForDlg; dlg.fingerprint := fp END;
		Controllers.ResetCurrentPath();
		Services.DoLater(a, Services.Ticks() + Services.resolution DIV 2)
	END Do;

	PROCEDURE InitDialog*;
	BEGIN
		IF action = NIL THEN NEW(action); Services.DoLater(action, Services.now) END;
		Controllers.SetCurrentPath(Controllers.targetPath);
		PollPropForDlg;
		Controllers.ResetCurrentPath()
	END InitDialog;

	PROCEDURE Set*;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		SetPropFromDlg; PollPropForDlg;
		Controllers.ResetCurrentPath()
	END Set;

	PROCEDURE Guard* (idx: INTEGER; VAR par: Dialog.Par);
	BEGIN
		IF ~(idx IN dlg.known) THEN par.disabled := TRUE
		ELSIF ~(idx IN dlg.valid) THEN par.undef := TRUE
		END
	END Guard;

	PROCEDURE Notifier* (idx, op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.changed THEN INCL(dlg.valid, idx) END
	END Notifier;


	(* StdDirectory *)

	PROCEDURE InitStdProp (OUT p: Properties.StdProp);
		VAR f: Fonts.Font;
	BEGIN
		NEW(p);
		f := Fonts.dir.Default();
		p.typeface := f.typeface;
		p.size := f.size;
		p.style.val := f.style;
		p.style.mask := {Fonts.italic, Fonts.underline, Fonts.strikeout};
		p.weight := Fonts.normal
	END InitStdProp;

	PROCEDURE InitTableProp (OUT p: Prop);
	BEGIN
		NEW(p);
		p.layoutEditable := TRUE; p.dataEditable := TRUE; p.selectionStyle := cellSelect
	END InitTableProp;

	PROCEDURE (d: StdDirectory) NewControl (p: Controls.Prop): Views.View;
		VAR c: Control;
	BEGIN
		NEW(c); Controls.OpenLink(c, p); InitStdProp(c.sprop); InitTableProp(c.tprop); SetupControl(c); RETURN c
	END NewControl;


	PROCEDURE SetDir* (d: Directory);
	BEGIN
		ASSERT(d # NIL, 20); dir := d
	END SetDir;

	PROCEDURE DepositControl*;
		VAR p: Controls.Prop;
	BEGIN
		NEW(p);
		p.link := ""; p.label := ""; p.guard := ""; p.notifier := "";
		p.level := 0; p.opt[Controls.sorted] := FALSE;
		Views.Deposit(dir.NewControl(p))
	END DepositControl;

	PROCEDURE SetupSelectionStyleList (VAR l: Dialog.List);
	BEGIN
		l.SetLen(5);
		l.SetItem(noSelect, "No Selection");
		l.SetItem(cellSelect, "Select Cell");
		l.SetItem(rowSelect, "Select Row");
		l.SetItem(colSelect, "Select Column");
		l.SetItem(crossSelect, "Select Row & Column")
	END SetupSelectionStyleList;

	PROCEDURE Init;
		VAR d: StdDirectory;
	BEGIN
		NEW(d); stdDir := d; dir := d;
		SetupSelectionStyleList(dlg.selectionStyle)
	END Init;

BEGIN
	Init
CLOSE
	Services.RemoveAction(action)
END StdTables.
