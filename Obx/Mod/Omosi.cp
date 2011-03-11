MODULE ObxOmosi;
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

	IMPORT Ports, Stores, Views, Controllers, Properties, Dialog;

	CONST
		outside = -1; white = 0; top = 1; left = 2; right = 3;	(* Kind *)
		gridDefault = FALSE;
		minVersion = 0; maxVersion = 1;

	TYPE
		Palette = ARRAY 4 OF Ports.Color;

		Kind = INTEGER;
		Field = RECORD
			kind: Kind;
			sel: BOOLEAN
		END;
		Row = ARRAY 8 OF Field;
		Model = ARRAY 15 OF Row;

		StdView = POINTER TO RECORD (Views.View)
			(* persistent state *)
			pal: Palette;
			mod: Model;
			(* non-persistent state *)
			sel: INTEGER;
			grid: BOOLEAN
		END;

		FieldPath = ARRAY 3 OF Ports.Point;

		FieldOp = POINTER TO RECORD (Stores.Operation)
			v: StdView; i, j: INTEGER; kind: Kind
		END;

		ColorOp = POINTER TO ColorOpDesc;
		ColorOpDesc = RECORD (Stores.Operation)
			v: StdView; n: INTEGER; col: Ports.Color
		END;

		UpdateMsg = RECORD (Views.Message)
			i, j: INTEGER
		END;

	PROCEDURE InitRow (VAR row: Row; k: INTEGER);
		VAR i, l, r: INTEGER;
	BEGIN
		l := (8 - k) DIV 2; r := 8 - l;
		i := 0; WHILE i # l DO row[i].kind := outside; INC(i) END;
		WHILE i # r DO row[i].kind := white; INC(i) END;
		WHILE i # 8 DO row[i].kind := outside; INC(i) END;
		i := 0; WHILE i # 8 DO row[i].sel := FALSE; INC(i) END
	END InitRow;

	PROCEDURE InitPalette (VAR p: Palette);
	BEGIN
		p[white] := Ports.white;
		p[top] := 0080FFH;
		p[left] := 004080H; 
		p[right] := 000040H
	END InitPalette;

	PROCEDURE InitModel (VAR m: Model);
		VAR j: INTEGER;
	BEGIN
		InitRow(m[0], 2); InitRow(m[1], 4); InitRow(m[2], 6);
		j := 3; WHILE j # 12 DO InitRow(m[j], 8); INC(j) END;
		InitRow(m[12], 6); InitRow(m[13], 4); InitRow(m[14], 2)
	END InitModel;

	PROCEDURE H (s: INTEGER): INTEGER;
	BEGIN
		RETURN s * 500 DIV 866
	END H;

	PROCEDURE GetFieldPath (v: StdView; f: Ports.Frame; i, j: INTEGER; OUT p: FieldPath);
		VAR w, h, s: INTEGER; 
	BEGIN
		v.context.GetSize(w, h); s := (w - f.unit) DIV 8; h := H(s);
		IF ODD(i + j) THEN
			p[0].x := i * s; p[0].y := (j + 1) * h;
			p[1].x := (i + 1) * s; p[1].y := j * h;
			p[2].x := (i + 1) * s; p[2].y := (j + 2) * h
		ELSE
			p[0].x := i * s; p[0].y := j * h;
			p[1].x := (i + 1) * s; p[1].y := (j + 1) * h;
			p[2].x := i * s; p[2].y := (j + 2) * h
		END
	END GetFieldPath;

	PROCEDURE AdjustPath (f: Ports.Frame; i, j: INTEGER; VAR p: FieldPath);
		VAR d, e: INTEGER;
	BEGIN
		d := 2 * f.dot; e := 3 * f.dot;
		IF ODD(i + j) THEN
			INC(p[0].x, e);
			DEC(p[1].x, d); INC(p[1].y, e);
			DEC(p[2].x, d); DEC(p[2].y, e)
		ELSE
			INC(p[0].x, d); INC(p[0].y, e);
			DEC(p[1].x, e);
			INC(p[2].x, d); DEC(p[2].y, e)
		END
	END AdjustPath;

	PROCEDURE ValidField (v: StdView; i, j: INTEGER): BOOLEAN;
	BEGIN
		RETURN (0 <= i) & (i < 8) & (0 <= j) & (j < 15) & (v.mod[j, i].kind > outside)
	END ValidField;

	PROCEDURE DrawField (v: StdView; f: Ports.Frame; i, j: INTEGER);
		VAR col: Ports.Color; p: FieldPath;
	BEGIN
		IF ValidField(v, i, j) THEN
			col := v.pal[v.mod[j, i].kind]; GetFieldPath(v, f, i, j, p);
			f.DrawPath(p, 3, Ports.fill, col, Ports.closedPoly);

			IF v.grid THEN
				f.DrawPath(p, 3, 0, Ports.grey25, Ports.closedPoly)
			END;
			IF v.mod[j, i].sel THEN
				AdjustPath(f, i, j, p);
				f.DrawPath(p, 3, 0, 800000H, Ports.closedPoly)
			END
		END
	END DrawField;

	PROCEDURE SelectField (v: StdView; f: Ports.Frame; i, j: INTEGER; sel: BOOLEAN);
	BEGIN
		IF ValidField(v, i, j) & (v.mod[j, i].sel # sel) THEN
			v.mod[j, i].sel := sel;
			IF sel THEN INC(v.sel) ELSE DEC(v.sel) END;
			DrawField(v, f, i, j)
		END
	END SelectField;

	PROCEDURE LocateField (v: StdView; f: Views.Frame; x, y: INTEGER; OUT i, j: INTEGER);
		VAR u, w, h, s, sx, sy: INTEGER;
	BEGIN
		v.context.GetSize(w, h); s := (w - f.unit) DIV 8;
		u := f.unit; h := H(s);
		sx := x DIV s; sy := y DIV h;
		IF (0 <= sx) & (sx < 9) & (0 <= sy) & (sy < 16) THEN
			i := SHORT(sx); j := SHORT(sy);
			IF ODD(i + j) THEN
				IF (s - x) MOD s * (h DIV u) >= y MOD h * (s DIV u) THEN DEC(j) END
			ELSE
				IF x MOD s * (h DIV u) >= y MOD h * (s DIV u) THEN DEC(j) END
			END;
			IF (i = 8) OR (j = 15) OR (j >= 0) & (v.mod[j, i].kind = outside) THEN j := -1 END
		ELSE j := -1
		END
	END LocateField;

	PROCEDURE Select (v: StdView; set: BOOLEAN);
		VAR i, j, sel: INTEGER;
	BEGIN
		j := 0;
		WHILE j # 15 DO
			i := 0; WHILE i # 8 DO v.mod[j, i].sel := set; INC(i) END;
			INC(j)
		END;
		IF set THEN sel := 64 ELSE sel := 0 END;
		IF v.sel # sel THEN v.sel := sel; Views.Update(v, Views.keepFrames) END
	END Select;

	PROCEDURE Track (v: StdView; f: Views.Frame; x, y: INTEGER; buttons: SET);
		VAR script: Stores.Operation; op: FieldOp; cop: ColorOp; col: Ports.Color;
			i, j, i0, j0, i1, j1: INTEGER; isDown, prevSel, setCol: BOOLEAN; m: SET;
	BEGIN
		LocateField(v, f, x, y, i, j);
		i0 := i; j0 := j; prevSel := ValidField(v, i, j) & v.mod[j, i].sel;
		IF ~prevSel & ~(Controllers.extend IN buttons) & (v.sel > 0) THEN
			j := 0;
			WHILE j # 15 DO
				i := 0;
				WHILE i # 8 DO
					IF v.mod[j, i].sel THEN SelectField(v, f, i, j, FALSE) END;
					INC(i)
				END;
				INC(j)
			END;
			v.sel := 0; i := i0; j := j0
		END;
		SelectField(v, f, i, j, ~prevSel OR ~(Controllers.extend IN buttons));
		REPEAT
			f.Input(x, y, m, isDown);
			LocateField(v, f, x, y, i1, j1); 
			IF (i1 # i) OR (j1 # j) THEN
				IF ~(Controllers.extend IN buttons) THEN SelectField(v, f, i, j, FALSE) END;
				i := i1; j := j1;
				SelectField(v, f, i, j, ~prevSel OR ~(Controllers.extend IN buttons))
			END
		UNTIL ~isDown;
		IF ~(Controllers.extend IN buttons) & ((i # i0) OR (j # j0) OR ~prevSel) THEN
			SelectField(v, f, i, j, FALSE)
		END;
		IF ValidField(v, i, j) THEN
			IF Controllers.modify IN buttons THEN
				Dialog.GetColor(v.pal[v.mod[j, i].kind], col, setCol);
				IF setCol THEN
					NEW(cop); cop.v := v; cop.n := v.mod[j, i].kind; cop.col := col;
					Views.Do(v, "Color Change", cop)
				END
			ELSIF ~(Controllers.extend IN buttons) THEN
				Views.BeginScript(v, "Omosi Change", script);
				j := 0;
				WHILE j # 15 DO
					i := 0;
					WHILE i # 8 DO
						IF (v.mod[j, i].sel OR (i = i1) & (j = j1)) & (v.mod[j, i].kind > outside) THEN
							NEW(op); op.v := v; op.i := i; op.j := j;
							op.kind := (v.mod[j, i].kind + 1) MOD 4;
							Views.Do(v, "", op)
						END;
						INC(i)
					END;
					INC(j)
				END;
				Views.EndScript(v, script)
			END
		END
	END Track;


	(* FieldOp *)

	PROCEDURE (op: FieldOp) Do;
		VAR k: Kind; msg: UpdateMsg;
	BEGIN
		k := op.v.mod[op.j, op.i].kind;
		op.v.mod[op.j, op.i].kind := op.kind;
		op.kind := k;
		msg.i := op.i; msg.j := op.j; Views.Broadcast(op.v, msg)
	END Do;


	(* ColorOp *)

	PROCEDURE (op: ColorOp) Do;
		VAR c: Ports.Color;
	BEGIN
		c := op.v.pal[op.n]; op.v.pal[op.n] := op.col; op.col := c;
		Views.Update(op.v, Views.keepFrames)
	END Do;


	(* View *)

	PROCEDURE (v: StdView) Externalize (VAR wr: Stores.Writer);
		VAR i, j: INTEGER;
	BEGIN
		wr.WriteVersion(maxVersion);
		i := 0; WHILE i # 4 DO wr.WriteInt(v.pal[i]); INC(i) END;
		j := 0;
		WHILE j # 15 DO
			i := 0; WHILE i # 8 DO wr.WriteInt(v.mod[j, i].kind); INC(i) END;
			INC(j)
		END
	END Externalize;

	PROCEDURE (v: StdView) Internalize (VAR rd: Stores.Reader);
		VAR i, j: INTEGER; version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			i := 0; WHILE i # 4 DO rd.ReadInt(v.pal[i]); INC(i) END;
			j := 0;
			WHILE j # 15 DO
				i := 0;
				WHILE i # 8 DO rd.ReadInt(v.mod[j, i].kind); v.mod[j, i].sel := FALSE; INC(i) END;
				INC(j)
			END;
			v.grid := FALSE
		END
	END Internalize;

	PROCEDURE (v: StdView) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: StdView DO
			v.pal := source.pal; v.mod := source.mod;
			v.sel := source.sel; v.grid := gridDefault
		END
	END CopyFromSimpleView;

	PROCEDURE (v: StdView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR i, j: INTEGER;
	BEGIN
		j := 0;
		WHILE j # 15 DO
			i := 0; WHILE i # 8 DO DrawField(v, f, i, j); INC(i) END;
			INC(j)
		END
	END Restore;

	PROCEDURE (v: StdView) HandleViewMsg (f: Views.Frame; VAR msg: Views.Message);
	BEGIN
		WITH msg: UpdateMsg DO
			DrawField(v, f, msg.i, msg.j)
		ELSE
		END
	END HandleViewMsg;

	PROCEDURE (v: StdView) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			Track(v, f, msg.x, msg.y, msg.modifiers)
		| msg: Controllers.PollOpsMsg DO
			msg.selectable := TRUE
		| msg: Controllers.SelectMsg DO
			Select(v, msg.set)
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: StdView) HandlePropMsg (VAR msg: Properties.Message);
		CONST minW = 3 * Ports.mm; stdW = 7 * Ports.mm;	(* per field *)
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w > Views.undefined) & (msg.h > Views.undefined) THEN
				DEC(msg.h, 1 * Ports.mm);
				Properties.ProportionalConstraint(1000, 2 * H(1000), msg.fixedW, msg.fixedH, msg.w, msg.h);
				IF msg.w < 8 * minW THEN
					msg.w := 8 * minW; msg.h := 16 * H(minW)
				END
			ELSE
				msg.w := 8 * stdW; msg.h := 16 * H(stdW)
			END;
			INC(msg.h, 1 * Ports.mm)
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;


	(* commands *)

	PROCEDURE Deposit*;
		VAR v: StdView;
	BEGIN
		NEW(v); InitPalette(v.pal); InitModel(v.mod); v.sel := 0; v.grid := FALSE; Views.Deposit(v)
	END Deposit;

	PROCEDURE ToggleGrid*;
		VAR v: Views.View;
	BEGIN
		v := Controllers.FocusView();
		IF v # NIL THEN
			WITH v: StdView DO
				v.grid := ~v.grid; Views.Update(v, Views.keepFrames)
			ELSE
			END
		END 
	END ToggleGrid;

	PROCEDURE ResetColors*;
		VAR v: Views.View; p0: Palette; script: Stores.Operation; cop: ColorOp; i: INTEGER;
	BEGIN
		v := Controllers.FocusView();
		IF v # NIL THEN
			WITH v: StdView DO
				Views.BeginScript(v, "Reset Colors", script);
				InitPalette(p0);
				i := 0;
				WHILE i # 4 DO
					NEW(cop); cop.v := v; cop.n := i; cop.col := p0[i]; Views.Do(v, "", cop); INC(i)
				END;
				Views.EndScript(v, script)
			ELSE
			END
		END 
	END ResetColors;

END ObxOmosi.
