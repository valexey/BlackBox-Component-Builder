MODULE ObxRatCalc;
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

	IMPORT Stores, Models, Dialog, TextModels, TextControllers, TextMappers, Integers;

	CONST
		(* scanner classes *)
		stop = 0; int = 1; openPar = 2; closePar = 3; powOp = 4; mulOp = 5; addOp = 6;
		approximationLength = 40;

	TYPE
		Scanner = RECORD
			r: TextModels.Reader;
			nextCh: CHAR;
			end, level: INTEGER;
			pos: INTEGER;
			class: INTEGER;
			num, den: Integers.Integer;
			op: CHAR;
			error: BOOLEAN
		END;

		Expression = POINTER TO RECORD
			op: CHAR;
			sub1, sub2: Expression;
			int: Integers.Integer
		END;

	VAR zero, one, ten, hundred, maxExponent, minusOne: Integers.Integer;


	(* scanning *)

	PROCEDURE ReadInteger (r: TextModels.Reader; OUT nextCh: CHAR; OUT num, den: Integers.Integer);
		VAR i, j, l1, l2, beg: INTEGER; ch: CHAR; buf: POINTER TO ARRAY OF CHAR;
	BEGIN
		beg := r.Pos() - 1; l1 := 0; l2 := 0;
		REPEAT INC(l1); r.ReadChar(ch) UNTIL r.eot OR (ch < "0") OR (ch > "9");
		IF ch = "." THEN
			r.ReadChar(ch);
			WHILE (ch >= "0") & (ch <= "9") DO INC(l2); r.ReadChar(ch) END
		END;
		NEW(buf, l1 + l2 + 1);
		i := 0; r.SetPos(beg);
		REPEAT r.ReadChar(buf[i]); INC(i) UNTIL i = l1;
		IF l2 # 0 THEN
			j := l2; r.ReadChar(ch);
			REPEAT r.ReadChar(buf[i]); INC(i); DEC(j) UNTIL j = 0
		END;
		buf[i] := 0X;
		Integers.ConvertFromString(buf^, num);
		IF l2 # 0 THEN
			buf[0] := "1"; i := 1;
			REPEAT buf[i] := "0"; INC(i) UNTIL i = l2 + 1;
			buf[i] := 0X;
			Integers.ConvertFromString(buf^, den)
		ELSE den := NIL
		END;
		r.ReadChar(nextCh)
	END ReadInteger;

	PROCEDURE (VAR s: Scanner) Read, NEW;
		VAR ch: CHAR;
	BEGIN
		IF ~s.error THEN
			ch := s.nextCh;
			IF s.r.eot THEN s.pos := s.r.Pos() ELSE s.pos := s.r.Pos() - 1 END;
			WHILE ~s.r.eot & (s.r.Pos() <= s.end) & (ch <= " ") DO s.r.ReadChar(ch) END;
			IF ~s.r.eot & (s.r.Pos() <= s.end) THEN
				IF (ch >= "0") & (ch <= "9") THEN s.class := int; ReadInteger(s.r, ch, s.num, s.den)
				ELSIF (ch = "+") OR (ch = "-") THEN s.class := addOp; s.op := ch; s.r.ReadChar(ch)
				ELSIF (ch = "*") OR (ch = "/") THEN s.class := mulOp; s.op := ch; s.r.ReadChar(ch)
				ELSIF ch = "^" THEN s.class := powOp; s.op := ch; s.r.ReadChar(ch)
				ELSIF ch = "(" THEN s.class := openPar; INC(s.level); s.r.ReadChar(ch)
				ELSIF ch = ")" THEN s.class := closePar; DEC(s.level); s.r.ReadChar(ch)
				ELSE s.error := TRUE
				END
			ELSE s.class := stop
			END;
			s.nextCh := ch
		ELSE s.class := stop
		END
	END Read;

	PROCEDURE (VAR s: Scanner) ConnectTo (t: TextModels.Model; beg, end: INTEGER), NEW;
		VAR ch: CHAR;
	BEGIN
		s.r := t.NewReader(NIL); s.r.SetPos(beg); s.r.ReadChar(ch);
		WHILE ~s.r.eot & (beg < end) & (ch <= " ") DO s.r.ReadChar(ch); INC(beg) END;
		s.nextCh := ch; s.pos := beg; s.end := end;
		s.level := 0; s.error := FALSE
	END ConnectTo;


	(* parsing *)

	PROCEDURE^ ReadExpression (VAR s: Scanner; OUT exp: Expression);

	PROCEDURE ReadFactor (VAR s: Scanner; OUT exp: Expression);
		VAR e: Expression;
	BEGIN
		IF s.class = openPar THEN
			s.Read;
			ReadExpression(s, exp);
			s.error := s.error OR (s.class # closePar); s.Read
		ELSIF s.class = int THEN
			IF s.den = NIL THEN
				NEW(exp); exp.op := "i"; exp.int := s.num
			ELSE
				NEW(exp); exp.op := "/";
				NEW(e); e.op := "i"; e.int := s.num; exp.sub1 := e;
				NEW(e); e.op := "i"; e.int := s.den; exp.sub2 := e
			END;
			s.Read
		ELSE s.error := TRUE
		END;
		IF ~s.error & (s.class = powOp) THEN
			NEW(e); e.op := s.op; e.sub1 := exp; exp := e;
			s.Read; ReadFactor(s, e.sub2)
		END
	END ReadFactor;

	PROCEDURE ReadTerm (VAR s: Scanner; OUT exp: Expression);
		VAR e: Expression;
	BEGIN
		ReadFactor(s, exp);
		WHILE ~s.error & (s.class = mulOp) DO
			NEW(e); e.op := s.op; e.sub1 := exp; exp := e;
			s.Read; ReadFactor(s, exp.sub2)
		END
	END ReadTerm;

	PROCEDURE ReadExpression (VAR s: Scanner; OUT exp: Expression);
		VAR e: Expression;
	BEGIN
		IF (s.class = addOp) & (s.op = "-") THEN
			s.Read;
			NEW(e); e.op := "i"; e.int := zero;
			NEW(exp); exp.op := "-"; exp.sub1 := e;
			ReadTerm(s, exp.sub2)
		ELSE ReadTerm(s, exp)
		END;
		WHILE ~s.error & (s.class = addOp) DO
			NEW(e); e.op := s.op; e.sub1 := exp; exp := e;
			s.Read; ReadTerm(s, exp.sub2)
		END
	END ReadExpression;


	(* evaluation *)

	PROCEDURE Normalize (VAR num, den: Integers.Integer);
		VAR g: Integers.Integer;
	BEGIN
		IF Integers.Sign(num) # 0 THEN
			g := Integers.GCD(num, den);
			num := Integers.Quotient(num, g); den := Integers.Quotient(den, g);
			IF Integers.Sign(den) < 0 THEN
				num := Integers.Product(num, minusOne); den := Integers.Abs(den)
			END
		ELSE den := one
		END
	END Normalize;

	PROCEDURE Evaluate (exp: Expression; OUT num, den: Integers.Integer; VAR error: INTEGER);
		VAR exponent: INTEGER; op: CHAR; n1, d1, n2, d2, g, h: Integers.Integer;
	BEGIN
		error := 0; op := exp.op;
		IF op = "i" THEN num := exp.int; den := one
		ELSE
			Evaluate(exp.sub1, n1, d1, error);
			IF error = 0 THEN Evaluate(exp.sub2, n2, d2, error);
				IF error = 0 THEN
					IF (op = "+") OR (op = "-") THEN
						g := Integers.GCD(d1, d2); h := Integers.Quotient(d2, g);
						IF op = "+" THEN
							num := Integers.Sum(
								Integers.Product(n1, h), Integers.Product(n2, Integers.Quotient(d1, g)))
						ELSE
							num := Integers.Difference(
								Integers.Product(n1, h), Integers.Product(n2, Integers.Quotient(d1, g)))
						END;
						den := Integers.Product(d1, h);
						Normalize(num, den)
					ELSIF op = "*" THEN
						num := Integers.Product(n1, n2); den := Integers.Product(d1, d2);
						Normalize(num, den)
					ELSIF op = "/" THEN
						IF Integers.Sign(n2) # 0 THEN
							num := Integers.Product(n1, d2); den := Integers.Product(d1, n2);
							Normalize(num, den)
						ELSE error := 1
						END
					ELSIF op = "^" THEN
						IF Integers.Sign(n1) = 0 THEN num := n1; den := d1
						ELSE
							IF Integers.Compare(d2, one) = 0 THEN
								IF Integers.Sign(n2) = 0 THEN num := one; den := one
								ELSE
									IF Integers.Sign(n2) < 0 THEN
										g := n1; n1 := d1; d1 := g; n2 := Integers.Abs(n2)
									END;
									IF Integers.Compare(n2, maxExponent) <= 0 THEN
										exponent := SHORT(Integers.Short(n2));
										num := Integers.Power(n1, exponent); den := Integers.Power(d1, exponent);
										Normalize(num, den)
									ELSE error := 3
									END
								END
							ELSE error := 2
							END
						END
					ELSE HALT(99)
					END
				END
			END
		END
	END Evaluate;


	(* output *)

	PROCEDURE WriteInteger (w: TextModels.Writer; x: Integers.Integer);
		VAR i: INTEGER;
	BEGIN
		IF Integers.Sign(x) # 0 THEN
			IF Integers.Sign(x) < 0 THEN w.WriteChar("-") END;
			i := Integers.Digits10Of(x);
			REPEAT DEC(i); w.WriteChar(Integers.ThisDigit10(x, i)) UNTIL i = 0
		ELSE w.WriteChar("0")
		END
	END WriteInteger;

	PROCEDURE Replace (t: TextModels.Model; VAR beg, end: INTEGER; n, d: Integers.Integer;
									a: TextModels.Attributes);
		VAR s: Stores.Operation; w: TextMappers.Formatter;
	BEGIN
		Models.BeginScript(t, "computation", s);
		t.Delete(beg, end);
		w.ConnectTo(t); w.SetPos(beg); w.rider.SetAttr(a);
		WriteInteger(w.rider, n);
		IF (Integers.Sign(n) # 0) & (Integers.Compare(d, one) # 0) THEN
			w.WriteString(" / "); WriteInteger(w.rider, d)
		END;
		Models.EndScript(t, s);
		end := w.Pos()
	END Replace;

	PROCEDURE ReplaceReal (t: TextModels.Model; VAR beg, end: INTEGER; n, d: Integers.Integer;
											a: TextModels.Attributes);
		VAR i, k, e: INTEGER; q, r: Integers.Integer; s: Stores.Operation; w: TextMappers.Formatter;
	BEGIN
		Models.BeginScript(t, "computation", s);
		t.Delete(beg, end);
		w.ConnectTo(t); w.SetPos(beg); w.rider.SetAttr(a);
		IF Integers.Sign(n) < 0 THEN w.WriteChar("-"); n := Integers.Abs(n) END;
		Integers.QuoRem(n, d, q, r);
		k := Integers.Digits10Of(q);
		IF k > approximationLength THEN
			DEC(k); e := k;
			w.WriteChar(Integers.ThisDigit10(q, k)); w.WriteChar(".");
			i := 1;
			REPEAT DEC(k); w.WriteChar(Integers.ThisDigit10(q, k)); INC(i) UNTIL i = approximationLength;
			w.WriteString("...*10^"); w.WriteInt(e)
		ELSE
			e := 0;
			IF (k = 0) & (Integers.Sign(r) # 0) & (Integers.Compare(Integers.Quotient(d, r), hundred) > 0)
			THEN
				REPEAT
					Integers.QuoRem(Integers.Product(ten, r), d, q, r); INC(e)
				UNTIL Integers.Sign(q) # 0
			ELSIF k = 0 THEN k := 1
			END;
			WriteInteger(w.rider, q);
			IF Integers.Sign(r) # 0 THEN
				w.WriteChar(".");
				REPEAT
					Integers.QuoRem(Integers.Product(ten, r), d, q, r);
					WriteInteger(w.rider, q); INC(k)
				UNTIL (Integers.Sign(r) = 0) OR (k = approximationLength);
				IF Integers.Sign(r) # 0 THEN w.WriteString("...") END
			END;
			IF e # 0 THEN w.WriteString("*10^-"); w.WriteInt(e) END
		END;
		Models.EndScript(t, s);
		end := w.Pos()
	END ReplaceReal;


	(* commands *)

	PROCEDURE Compute (approx: BOOLEAN);
		VAR beg, end, error: INTEGER; exp: Expression; s: Scanner;
			attr: TextModels.Attributes; c: TextControllers.Controller; num, den: Integers.Integer;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end); s.ConnectTo(c.text, beg, end); attr := s.r.attr; beg := s.pos;
			s.Read; ReadExpression(s, exp); end := s.pos;
			IF ~s.error & (s.class = stop) THEN
				Evaluate(exp, num, den, error);
				IF error = 0 THEN
					IF approx THEN ReplaceReal(c.text, beg, end, num, den, attr)
					ELSE Replace(c.text, beg, end, num, den, attr)
					END;
					c.SetSelection(beg, end)
				ELSIF error = 1 THEN Dialog.ShowMsg("division by zero.")
				ELSIF error = 2 THEN Dialog.ShowMsg("non-integer exponent.")
				ELSIF error = 3 THEN Dialog.ShowMsg("exponent too large.")
				ELSE HALT(99)
				END
			ELSE
				Dialog.ShowMsg("syntax error.");
				c.SetCaret(s.pos)
			END
		END
	END Compute;

	PROCEDURE Simplify*;
	BEGIN
		Compute(FALSE)
	END Simplify;

	PROCEDURE Approximate*;
	BEGIN
		Compute(TRUE)
	END Approximate;


BEGIN
	zero := Integers.Long(0); one := Integers.Long(1); ten := Integers.Long(10);
	hundred := Integers.Long(100); maxExponent := Integers.Long(1000000);
	minusOne := Integers.Long(-1)
END ObxRatCalc.
