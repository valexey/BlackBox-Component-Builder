MODULE ObxPatterns;
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

	IMPORT Ports, Stores, Views, Properties;

	CONST minVersion = 0; maxVersion = 0;

	TYPE View = POINTER TO RECORD (Views.View) END;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version)
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion)
	END Externalize;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR w, h, d: INTEGER; col: INTEGER; colors: ARRAY 3 OF Ports.Color;
	BEGIN
		colors[0] := Ports.red; colors[1] := Ports.green; colors[2] := Ports.blue;
		v.context.GetSize(w, h);
		d := 4 * f.dot; col := 0;
		l := 0; t := 0; r := w; b := h;
		WHILE (r> l) & (b > t) DO
			f.DrawRect(l, t, r, b, f.dot, colors[col]);
			INC(l, d); INC(t, d); DEC(r, d); DEC(b, d); col := (col + 1) MOD 3
		END
	END Restore;

	PROCEDURE (v: View) HandlePropMsg (VAR p: Properties.Message);
		CONST min = 10 * Ports.mm; max = 160 * Ports.mm; pref = 90 * Ports.mm;
	BEGIN
		WITH p: Properties.SizePref DO	(* prevent illegal sizes *)
			IF p.w = Views.undefined THEN	(* no preference for width -> skip *)
			ELSIF p.w < min THEN p.w := min
			ELSIF p.w > max THEN p.w := max
			END;
			IF p.h = Views.undefined THEN p.h := pref
			ELSIF p.h < min THEN p.h := min
			ELSIF p.h > max THEN p.h := max
			END
		ELSE
		END
	END HandlePropMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); Views.Deposit(v)
	END Deposit;

END ObxPatterns.
