MODULE ObxCalc;
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

	IMPORT Stores, Ports, Views, Properties, Controllers, Dialog, Fonts, Strings;

	CONST minVersion = 0; maxVersion = 0; mm = Ports.mm; CR = 0DX;

	TYPE
		Stack = POINTER TO RECORD
			next: Stack;
			val: INTEGER
		END;

		View = POINTER TO RECORD (Views.View)
			stack: Stack;
			editMode, enterMode: BOOLEAN
		END;

	VAR
		font: Fonts.Font;
		labels: ARRAY 21 OF CHAR;

	PROCEDURE LocateField (v: View; f: Views.Frame; x, y: INTEGER; VAR i, j: INTEGER;
											VAR valid: BOOLEAN);
	BEGIN
		x := x DIV mm - 3; y := y DIV mm - 12; i := SHORT(x DIV 9); j := SHORT(y DIV 9);
		valid := (i >= 0) & (i < 4) & (j >= 0) & (j < 5) & (x MOD 9 < 7) & (y MOD 9 < 7)
	END LocateField;

	PROCEDURE SelectField (v: View; f: Ports.Frame; i, j: INTEGER);
		CONST point = Ports.point;
		VAR x, y: INTEGER;
	BEGIN
		x := (3 + i * 9) * mm; y := (12 + j * 9) * mm;
		f.MarkRect(x + point, y + point, x + 6 * mm - point, y + 6 * mm - point, Ports.fill, Ports.hilite, TRUE)
	END SelectField;

	PROCEDURE HandleKey (v: View; i, j: INTEGER);
		VAR k, n: INTEGER; s: Stack;
	BEGIN
		k := j*4 + i; s := v.stack;
		IF k IN {0, 1, 2, 3, 7, 11, 15} THEN
			IF s.next # NIL THEN
				IF k = 0 THEN (* swap *) s := s.next; v.stack.next := s.next; s.next := v.stack; v.stack := s
				ELSIF k = 1 THEN v.stack := s.next
				ELSIF k IN {2, 3} THEN
					IF s.val = 0 THEN Dialog.Beep
					ELSIF k = 2 THEN s.next.val := s.next.val MOD s.val; v.stack := s.next
					ELSE s.next.val := s.next.val DIV s.val; v.stack := s.next
					END
				ELSIF k = 7 THEN s.next.val := s.next.val * s.val; v.stack := s.next
				ELSIF k = 11 THEN s.next.val := s.next.val - s.val; v.stack := s.next
				ELSIF k = 15 THEN s.next.val := s.next.val + s.val; v.stack := s.next
				END
			ELSE
				IF k = 0 THEN NEW(s); s.val := 0; s.next := v.stack; v.stack := s
				ELSIF k = 11 THEN s.val := -s.val
				ELSIF k = 15 THEN (* skip *)
				ELSE s.val := 0
				END
			END;
			v.editMode := FALSE
		ELSIF k = 18 THEN  (* ± *) 
			s.val := - s.val
		ELSIF k = 16THEN (* delete *)
			IF v.editMode THEN s.val := s.val DIV 10
			ELSE s.val := 0; v.editMode := TRUE
			END
		ELSIF k = 19 THEN (* enter *)
			NEW(s); s.val := v.stack.val; s.next := v.stack; v.stack := s;
			v.editMode := FALSE
		ELSE (* edit operation *)
			IF k = 17 THEN (* 0 *) n := 0 ELSE n := (3-j)*3 + 1 + i END;
			IF ~v.editMode & ~v.enterMode THEN
				NEW(s); s.val := n; s.next := v.stack; v.stack := s; v.editMode := TRUE
			ELSIF ~v.editMode THEN s.val := n; v.editMode := TRUE
			ELSIF s.val >= 0 THEN
				IF s.val > (MAX(INTEGER) - n) DIV 10 THEN Dialog.Beep ELSE s.val := 10*s.val + n END
			ELSE 
				IF s.val < (MIN(INTEGER) + n) DIV 10 THEN Dialog.Beep ELSE s.val := 10*s.val - n END
			END
		END;
		v.enterMode := k = 19;
		Views.Update(v, Views.keepFrames)
	END HandleKey;

	PROCEDURE Track (v: View; f: Views.Frame; x, y: INTEGER; buttons: SET);
		VAR i, j, i1, j1: INTEGER; isDown, valid, sel: BOOLEAN; m: SET;
	BEGIN
		LocateField(v, f, x, y, i, j, sel);
		IF sel THEN
			SelectField(v, f, i, j);
			REPEAT f.Input(x, y, m, isDown);
				LocateField(v, f, x, y, i1, j1, valid); 
				IF ~valid OR (i1 # i) OR (j1 # j) THEN
					IF sel THEN sel := FALSE; SelectField(v, f, i, j) END
				ELSE
					IF ~sel THEN sel := TRUE; SelectField(v, f, i, j) END
				END
			UNTIL ~isDown;
			IF sel THEN HandleKey(v, i, j); SelectField(v, f, i, j) END
		END
	END Track;

	PROCEDURE Init (v: View);
	BEGIN
		NEW(v.stack); v.stack.val := 0; v.editMode := TRUE; v.enterMode := FALSE
	END Init;

	(* View *)

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
	END Externalize;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN Init(v) END
	END Internalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		Init(v)
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR i, j: INTEGER; x, y, asc, dsc, w: INTEGER; s: ARRAY 2 OF CHAR;
			display: ARRAY 12 OF CHAR;
	BEGIN
		Strings.IntToStringForm(v.stack.val, Strings.decimal, 11, " ", FALSE, display);
		f.DrawRect(0, 0, 40 * mm, 58 * mm, 0, Ports.black);
		f.DrawRect(3 * mm, 3 * mm, 37 * mm, 10 * mm, 0, Ports.black);
		f.DrawString(6 * mm, 8 * mm, Ports.black, display, font);
		j := 0;
		WHILE j # 5 DO
			i := 0; y := (12 + j * 9) * mm;
			WHILE i # 4 DO
				x := (3 + i * 9) * mm;
				f.DrawRect(x, y, x + 6 * mm, y + 6 * mm, 0, Ports.black);
				f.DrawRect(x + mm, y + 6 * mm, x + 7 * mm, y + 7 * mm, Ports.fill, Ports.black);
				f.DrawRect(x + 6 * mm, y + Ports.mm, x + 7 * mm, y + 7 * mm, Ports.fill, Ports.black);
				s[0] := labels[j * 4 + i]; s[1] := 0X;
				font.GetBounds(asc, dsc, w);
				f.DrawString(x + 3 * mm - w DIV 2, y + 3 * mm + asc DIV 2, Ports.black, s, font);
				INC(i)
			END;
			INC(j)
		END
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																					VAR focus: Views.View);
		VAR i, j, k: INTEGER;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			Track(v, f, msg.x, msg.y, msg.modifiers)
		| msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN
				IF msg.char = CR THEN k := 19
				ELSIF msg.char = 08X THEN k := 16
				ELSE
					k := 0; WHILE (k # 20) & (CAP(labels[k]) # CAP(msg.char)) DO INC(k) END
				END;
				IF k < 20 THEN
					i := k MOD 4; j := k DIV 4;
					SelectField(v, f, i, j); HandleKey(v, i, j); SelectField(v, f, i, j)
				END
			END
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.ResizePref DO
			msg.fixed := TRUE
		| msg: Properties.SizePref DO
			msg.w := 40 * mm; msg.h := 58 * mm
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;

	(* commands *)

	PROCEDURE New* (): View;
		VAR v: View;
	BEGIN
		NEW(v); Init(v); RETURN v
	END New;

	PROCEDURE Deposit*;
	BEGIN
		Views.Deposit(New())
	END Deposit;

BEGIN 
	font := Fonts.dir.This("Courier", 11 * Fonts.point, {}, Fonts.normal);
	labels := "sp÷/789*456-123+C0±^"
END  ObxCalc.
