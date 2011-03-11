MODULE ObxTickers;
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

	IMPORT Views, Ports, Properties, Services, Stores, Models;

	CONST minVersion = 0; maxVersion = 0;

	TYPE
		Ticks = POINTER TO ARRAY OF INTEGER;

		View = POINTER TO RECORD (Views.View)
			ticks: Ticks;
			last: INTEGER;
			offset: INTEGER
		END;

		Action = POINTER TO RECORD (Services.Action) END;

		Msg = RECORD (Models.Message)
			consumed: BOOLEAN
		END;

	VAR
		action: Action;
		actionIsActive: BOOLEAN;
		seed: INTEGER;

	PROCEDURE UniRand (): REAL;
		CONST a = 16807; m = 2147483647; q = m DIV a; r = m MOD a;
	BEGIN
		seed := a * (seed MOD q) - r * (seed DIV q);
		IF seed <= 0 THEN seed := seed + m END;
		RETURN seed * (1.0 / m)
	END UniRand;

	PROCEDURE (a: Action) Do;
		VAR msg: Msg;
	BEGIN
		msg.consumed := FALSE;
		Views.Omnicast(msg);
		IF msg.consumed THEN
			Services.DoLater(action, Services.Ticks() + Services.resolution DIV 5)
		ELSE
			actionIsActive := FALSE
		END
	END Do;

	(* view *)

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
		VAR i, len: INTEGER;
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteInt(v.offset);
		wr.WriteInt(v.last);
		len := LEN(v.ticks); wr.WriteInt(len);
		FOR i := 0 TO len - 1 DO wr.WriteInt(v.ticks[i]) END
	END Externalize;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version, i, len: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.offset);
			rd.ReadInt(v.last);
			rd.ReadInt(len); NEW(v.ticks, len);
			FOR i := 0 TO len - 1 DO rd.ReadInt(v.ticks[i]) END
		END
	END Internalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
		VAR i, len: INTEGER;
	BEGIN
		WITH source: View DO
			v.offset := source.offset;
			v.last := source.last;
			len := LEN(source.ticks);
			NEW(v.ticks, len);
			FOR i := 0 TO len - 1 DO v.ticks[i] := source.ticks[i] END
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				msg.w := 50 * Ports.mm; msg.h := 10 * Ports.mm
			END
		ELSE
		END
	END HandlePropMsg;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
		VAR len, m: INTEGER;
	BEGIN
		WITH msg: Msg DO
			len := LEN(v.ticks);
			m := v.ticks[v.last];
			IF UniRand() > 0.5 THEN INC(m) ELSE DEC(m) END;
			v.last := (v.last + 1) MOD len;
			v.ticks[v.last] := m;
			msg.consumed := TRUE;
			Views.Update(v, Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR w, h, x, y0, ylast, p, p1, len, len2, i, j: INTEGER; ticks: Ticks;
	BEGIN
		IF ~actionIsActive THEN
			Services.DoLater(action, Services.now);
			actionIsActive := TRUE
		END;
		v.context.GetSize(w, h);
		len := LEN(v.ticks); len2 := 1 + w DIV f.dot;
		IF len2 > len THEN	(* allocate new array and copy data *)
			NEW(ticks, len2);
			i := 0; WHILE i <= v.last DO ticks[i] := v.ticks[i]; INC(i) END;
			j := i + len2 - len; WHILE i < len DO ticks[j] := v.ticks[i]; INC(i); INC(j) END;
			v.ticks := ticks; len := len2
		END;

		(* adjust offset so that the last point is visible *)
		ylast := (v.ticks[v.last] + v.offset) * f.dot;
		IF ylast > h DIV 2 THEN DEC(v.offset)
		ELSIF ylast < -h DIV 2 THEN INC(v.offset)
		END;

		p := (v.last - (len2 - 1)) MOD len;	(* index of first entry *)
		x := 0; y0 := h DIV 2 + v.offset * f.dot;
		WHILE p # v.last DO
			p1 := (p + 1) MOD len;
			f.DrawLine(x, y0 + v.ticks[p] * f.dot, x + f.dot, y0 + v.ticks[p1] * f.dot, 0, Ports.red);
			x := x + f.dot; p := p1
		END;
		f.DrawLine(l, y0, r, y0, 0, Ports.black)
	END Restore;

	(* commands *)

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); NEW(v.ticks, 142);
		v.last := 0; v.offset := 0;
		Views.Deposit(v)
	END Deposit;

BEGIN
	NEW(action); actionIsActive := FALSE; seed := SHORT(Services.Ticks())
END ObxTickers.
