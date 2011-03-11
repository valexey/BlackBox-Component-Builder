MODULE ObxFern;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	references	= "Martin Reiser, Niklaus Wirth, Programming In Oberon, ISBN 0201565439"
	changes	= ""
	issues	= ""

**)

	IMPORT ObxRandom, In, Out, XYplane;

	VAR
		a1, b1, c1, d1, e1, f1, p1: REAL;
		a2, b2, c2, d2, e2, f2, p2: REAL;
		a3, b3, c3, d3, e3, f3, p3: REAL;
		a4, b4, c4, d4, e4, f4, p4: REAL;
		X, Y: REAL;
		x0: INTEGER;
		y0: INTEGER;
		e: INTEGER;
		initialized: BOOLEAN;

	PROCEDURE Draw*;
		VAR x, y: REAL; xi, eta: INTEGER; rn: REAL;
	BEGIN
		IF initialized THEN
			REPEAT
				rn := ObxRandom.Uniform();
				IF rn < p1 THEN
					x := a1 * X + b1 * Y + e1; y := c1 * X + d1 * Y + f1
				ELSIF rn < (p1 + p2) THEN
					x := a2 *X + b2 * Y + e2; y := c2 * X + d2 * Y + f2
				ELSIF rn < (p1 + p2 + p3) THEN
					x := a3 * X + b3 * Y + e3; y := c3 * X + d3 * Y + f3
				ELSE
					x := a4 * X + b4 * Y + e4; y := c4 * X + d4 * Y + f4
				END;
				X := x; xi := x0 + SHORT(ENTIER(X * e));
				Y := y; eta := y0 + SHORT(ENTIER(Y * e));
				XYplane.Dot(xi, eta, XYplane.draw)
			UNTIL "s" = XYplane.ReadKey()
		END
	END Draw;

	PROCEDURE Init*;
	BEGIN
		X := 0; Y := 0;
		initialized := FALSE;
		In.Open;
		In.Int(x0); In.Int(y0); In.Int(e);
		In.Real(a1); In.Real(a2); In.Real(a3); In.Real(a4);
		In.Real(b1); In.Real(b2); In.Real(b3); In.Real(b4);
		In.Real(c1); In.Real(c2); In.Real(c3); In.Real(c4);
		In.Real(d1); In.Real(d2); In.Real(d3); In.Real(d4);
		In.Real(e1); In.Real(e2); In.Real(e3); In.Real(e4);
		In.Real(f1); In.Real(f2); In.Real(f3); In.Real(f4);
		In.Real(p1); In.Real(p2); In.Real(p3); In.Real(p4);
		IF In.Done THEN XYplane.Open; initialized := TRUE
		ELSE Out.String("Parameter error: select a list of 31 numbers"); Out.Ln
		END
	END Init;

BEGIN
	initialized := FALSE
END ObxFern.
