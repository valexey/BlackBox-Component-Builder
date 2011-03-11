MODULE ObxCubes;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	references	= "Adopted from a program written in C in 1986 by Roland Karlsson,
		    Swedish Institute for Computer Science (SICS), roland@sics.se"
	changes	= ""
	issues	= ""

**)

	IMPORT Views, Ports, Properties, Services, Stores, Models, Math, Controllers, StdCmds, Containers, Dialog;

	CONST
		minVersion = 0; maxVersion = 1;
		pi2 = 255;
		invisible = Ports.white;

	TYPE
		Colors = ARRAY 6 OF Ports.Color;

		View = POINTER TO RECORD (Views.View)
			fi1, fi2: INTEGER;	(* rotation angles *)
			colors: Colors	(* colors of the six sides of the cube *)
		END;

		Action = POINTER TO RECORD (Services.Action) END;

		Msg = RECORD (Models.Message) 
			consumed: BOOLEAN
		END;

	VAR
		para*: RECORD
			colors*: Colors
		END;

		action: Action;
		actionIsAlive: BOOLEAN;
		actual: View;
		sinus: ARRAY 256 OF INTEGER;


	(* property dialog *)

	PROCEDURE Singleton (): View;
		VAR v: Views.View;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		v := Containers.FocusSingleton();
		Controllers.ResetCurrentPath();
		IF (v # NIL) & (v IS View) THEN RETURN v(View) ELSE RETURN NIL END
	END Singleton;

	PROCEDURE Notify* (op, from, to: INTEGER);
		VAR v: View;
	BEGIN
		v := Singleton();
		IF v # NIL THEN v.colors := para.colors END
	END Notify;
	
	(* Action *)
	
	PROCEDURE (a: Action) Do;
		VAR msg: Msg; v: View;
	BEGIN
		msg.consumed := FALSE;
		Views.Omnicast(msg);
		IF msg.consumed THEN (* update Color Property Editor *)
			v := Singleton();
			IF (v # NIL) & (actual # v) THEN
				para.colors := v.colors; Dialog.Update(para);
				actual := v
			END;
			Services.DoLater(a, Services.Ticks() + Services.resolution DIV 10)
				(* i.e. perform a full rotation through all 256 states in 25.6 seconds *)
		ELSE
			actionIsAlive := FALSE
		END
	END Do;

	(* View *)

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
		VAR i: INTEGER;
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteInt(v.fi1); wr.WriteInt(v.fi2);
		FOR i := 0 TO 5 DO wr.WriteInt(v.colors[i]) END
	END Externalize;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; i: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.fi1); rd.ReadInt(v.fi2);
			IF version = maxVersion THEN
				FOR i := 0 TO 5 DO rd.ReadInt(v.colors[i]) END
			ELSE
				FOR i := 0 TO 5 DO v.colors[i] := invisible END
			END
		END
	END Internalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: View DO
			v.fi1 := source.fi1; v.fi2 := source.fi2; 
			v.colors := source.colors
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w > Views.undefined) & (msg.h > Views.undefined) THEN
				Properties.ProportionalConstraint(1, 1, msg.fixedW, msg.fixedH, msg.w, msg.h);
				IF msg.w < 10 * Ports.mm THEN
					msg.w := 10 * Ports.mm; msg.h := msg.w
				END
			ELSE
				msg.w := 40*Ports.mm; msg.h := msg.w;
			END
		| msg: Properties.FocusPref DO
			msg.hotFocus := TRUE
		ELSE
		END
	END HandlePropMsg;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																VAR focus: Views.View);
		VAR c: Containers.Controller;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			IF Controllers.modify IN msg.modifiers THEN
				c := Containers.Focus();
				IF c.opts # Containers.mask THEN
					para.colors := v.colors;
					StdCmds.OpenToolDialog('Obx/Rsrc/Cubes', 'Cube Colors');
					c.SetSingleton(v)
				END
			END
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
	BEGIN
		WITH msg: Msg DO
			v.fi1 := (v.fi1 + 7) MOD pi2;
			v.fi2 := (v.fi2 + 1) MOD pi2;
			msg.consumed := TRUE;
			Views.Update(v, Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR
			fi1, fi2, a, c: INTEGER;
			p0h, p0v, p1h, p1v, p2h, p2v, p3h, p3v: INTEGER;
			w, h: INTEGER;
			e01, e12, e23, e30,
			e45, e56, e67, e74,
			e04, e15, e26, e37: BOOLEAN;
			p: ARRAY 4 OF Ports.Point;

		PROCEDURE DrawSides(visible: BOOLEAN);
		BEGIN
			IF (e01 & e12 & e23 & e30 = visible) & (v.colors[0] # invisible) THEN
				p[0].x := (p0h - c) * w; p[0].y := p0v * w;
				p[1].x := (p1h - c) * w; p[1].y := p1v * w;
				p[2].x := (p2h - c) * w; p[2].y := p2v * w;
				p[3].x := (p3h - c) * w; p[3].y := p3v * w;
				f.DrawPath(p, 4, Ports.fill, v.colors[0], Ports.closedPoly)
			END;
			IF (e45 & e56 & e67 & e74 = visible) & (v.colors[1] # invisible)  THEN
				p[0].x := (p0h + c) * w; p[0].y := p0v * w;
				p[1].x := (p1h + c) * w; p[1].y := p1v * w;
				p[2].x := (p2h + c) * w; p[2].y := p2v * w;
				p[3].x := (p3h + c) * w; p[3].y := p3v * w;
				f.DrawPath(p, 4, Ports.fill, v.colors[1], Ports.closedPoly)
			END;
			IF (e01 & e15 & e45 & e04 = visible) & (v.colors[2] # invisible)  THEN
				p[0].x := (p0h - c) * w; p[0].y := p0v * w;
				p[1].x := (p1h - c) * w; p[1].y := p1v * w;
				p[2].x := (p1h + c) * w; p[2].y := p1v * w;
				p[3].x := (p0h + c) * w; p[3].y := p0v * w;
				f.DrawPath(p, 4, Ports.fill, v.colors[2], Ports.closedPoly)
			END;
			IF (e12 & e26 & e56 & e15 = visible) & (v.colors[3] # invisible)  THEN
				p[0].x := (p1h - c) * w; p[0].y := p1v * w;
				p[1].x := (p2h - c) * w; p[1].y := p2v * w;
				p[2].x := (p2h + c) * w; p[2].y := p2v * w;
				p[3].x := (p1h + c) * w; p[3].y := p1v * w;
				f.DrawPath(p, 4, Ports.fill, v.colors[3], Ports.closedPoly)
			END;
			IF (e23 & e37 & e67 & e26 = visible) & (v.colors[4] # invisible)  THEN
				p[0].x := (p2h - c) * w; p[0].y := p2v * w;
				p[1].x := (p3h - c) * w; p[1].y := p3v * w;
				p[2].x := (p3h + c) * w; p[2].y := p3v * w;
				p[3].x := (p2h + c) * w; p[3].y := p2v * w;
				f.DrawPath(p, 4, Ports.fill, v.colors[4], Ports.closedPoly)
			END;
			IF (e30 & e04 & e74 & e37 = visible) & (v.colors[5] # invisible)  THEN
				p[0].x := (p3h - c) * w; p[0].y := p3v * w;
				p[1].x := (p0h - c) * w; p[1].y := p0v * w;
				p[2].x := (p0h + c) * w; p[2].y := p0v * w;
				p[3].x := (p3h + c) * w; p[3].y := p3v * w;
				f.DrawPath(p, 4, Ports.fill, v.colors[5], Ports.closedPoly)
			END;
			IF e01 = visible THEN
				f.DrawLine((p0h - c) * w, p0v * w, (p1h - c) * w, p1v * w, 0, Ports.black)
			END;
			IF e12 = visible THEN
				f.DrawLine((p1h - c) * w, p1v * w, (p2h - c) * w, p2v * w, 0, Ports.black)
			END;
			IF e23 = visible THEN
				f.DrawLine((p2h - c) * w, p2v * w, (p3h - c) * w, p3v * w, 0, Ports.black)
			END;
			IF e30 = visible THEN
				f.DrawLine((p3h - c) * w, p3v * w, (p0h - c) * w, p0v * w, 0, Ports.black)
			END;
			IF e45 = visible THEN
				f.DrawLine((p0h + c) * w, p0v * w, (p1h + c) * w, p1v * w, 0, Ports.black)
			END;
			IF e56 = visible THEN
				f.DrawLine((p1h + c) * w, p1v * w, (p2h + c) * w, p2v * w, 0, Ports.black)
			END;
			IF e67 = visible THEN
				f.DrawLine((p2h + c) * w, p2v * w, (p3h + c) * w, p3v * w, 0, Ports.black)
			END;
			IF e74 = visible THEN
				f.DrawLine((p3h + c) * w, p3v * w, (p0h + c) * w, p0v * w, 0, Ports.black)
			END;
			IF e04 = visible THEN
				f.DrawLine((p0h + c) * w, p0v * w, (p0h - c) * w, p0v * w, 0, Ports.black)
			END;
			IF e15 = visible THEN
				f.DrawLine((p1h + c) * w, p1v * w, (p1h - c) * w, p1v * w, 0, Ports.black)
			END;
			IF e26 = visible THEN
				f.DrawLine((p2h + c) * w, p2v * w, (p2h - c) * w, p2v * w, 0, Ports.black)
			END;
			IF e37 = visible THEN
				f.DrawLine((p3h + c) * w, p3v * w, (p3h - c) * w, p3v * w, 0, Ports.black)
			END
		END DrawSides;

	BEGIN
		IF ~actionIsAlive THEN
			 actionIsAlive := TRUE; Services.DoLater(action, Services.now)
		END;

		v.context.GetSize(w, h); w := (w  DIV 170);

		fi1 := v.fi1;
		fi2 := v.fi2;

		a := sinus[fi2];
		c := (sinus[(64 - fi2) MOD pi2] * 91) DIV 128; (* 91/128 := sqrt(2)  *)

		p0v := 85 + sinus[fi1];
		p0h := 85 + (a * sinus[(64 - fi1) MOD pi2]) DIV 64;

		p1v := 85 + sinus[(64 + fi1) MOD pi2];
		p1h := 85 + (a * sinus[(-fi1) MOD pi2]) DIV 64;

		p2v := 85 + sinus[(128 + fi1) MOD pi2];
		p2h := 85 + (a * sinus[(-64 - fi1) MOD pi2]) DIV 64;

		p3v := 85 + sinus[(192 + fi1) MOD pi2];
		p3h := 85 + (a * sinus[(-128 - fi1) MOD pi2]) DIV 64;

		(* determine visibility of the twelve edges *)

		e01 :=  ~((((fi2 - 192) MOD pi2 < 64) & ((fi1 - 32) MOD pi2 < 128))
				OR (((fi2 - 128) MOD pi2 < 64) & ((fi1 - 160) MOD pi2 < 128)));
		e12 := ~((((fi2 - 192) MOD pi2 < 64) & ((fi1 - 224) MOD pi2 < 128))
				OR (((fi2 - 128) MOD pi2 < 64) & ((fi1 - 96) MOD pi2 < 128)));
		e23 := ~((((fi2 - 192) MOD pi2 < 64) & ((fi1 - 160) MOD pi2 < 128))
				OR (((fi2 - 128) MOD pi2 < 64) & ((fi1 - 32) MOD pi2 < 128)));
		e30 := ~((((fi2 - 192) MOD pi2 < 64) & ((fi1 - 96) MOD pi2 < 128))
				OR (((fi2 - 128) MOD pi2 < 64) & ((fi1 - 224) MOD pi2 < 128)));
		e45 := ~((((fi2) MOD pi2 < 64) & ((fi1 - 32) MOD pi2 < 128))
				OR (((fi2 - 64) MOD pi2 < 64) & ((fi1 - 160) MOD pi2 < 128)));
		e56 := ~((((fi2) MOD pi2 < 64) & ((fi1 - 224) MOD pi2 < 128))
				OR (((fi2 - 64) MOD pi2 < 64) & ((fi1 - 96) MOD pi2 < 128)));
		e67 := ~((((fi2) MOD pi2 < 64) & ((fi1 - 160) MOD pi2 < 128))
				OR (((fi2 - 64) MOD pi2 < 64) & ((fi1 - 32) MOD pi2 < 128)));
		e74 := ~((((fi2) MOD pi2 < 64) & ((fi1 - 96) MOD pi2 < 128))
				OR (((fi2 - 64) MOD pi2 < 64) & ((fi1 - 224) MOD pi2 < 128)));
		e04 := ~((((fi2 - 64) MOD pi2 < 128) & ((fi1 - 224) MOD pi2 < 64))
				OR (((fi2 - 192) MOD pi2 < 128) & ((fi1 - 96) MOD pi2 < 64)));
		e15 := ~((((fi2 - 64) MOD pi2 < 128) & ((fi1 - 160) MOD pi2 < 64))
				OR (((fi2 - 192) MOD pi2 < 128) & ((fi1 - 32) MOD pi2 < 64)));
		e26 := ~((((fi2 - 64) MOD pi2 < 128) & ((fi1 - 96) MOD pi2 < 64))
				OR (((fi2 - 192) MOD pi2 < 128) & ((fi1 - 224) MOD pi2 < 64)));
		e37 := ~((((fi2 - 64) MOD pi2 < 128) & ((fi1 - 32) MOD pi2 < 64))
				OR (((fi2 - 192) MOD pi2 < 128) & ((fi1 - 160) MOD pi2 < 64)));

		DrawSides(FALSE);	(* draw hidden sides and edges *)
		DrawSides(TRUE)	(* draw visible sides and edges *)
	END Restore;

	(* commands *)

	PROCEDURE New* (): View;
		VAR v: View;
	BEGIN
		NEW(v);
		v.fi1 := 0; v.fi2 := 0;
		v.colors := para.colors;
		RETURN v
	END New;

	PROCEDURE Deposit*;
	BEGIN
		Views.Deposit(New())
	END Deposit;

	PROCEDURE InitData;
		VAR i: INTEGER;
	BEGIN (* Pi = 128 *)
		FOR i := 0 TO 255 DO
			sinus[i] := SHORT(ENTIER(0.5 + 64 *  Math.Sin(i * 2 * Math.Pi() / 256)))
		END;
		para.colors[0] := Ports.green;
		para.colors[1] := Ports.blue;
		para.colors[2] := invisible;
		para.colors[3] := Ports.red;
		para.colors[4] := invisible;
		para.colors[5] := Ports.red + Ports.green	(* yellow *)
	END InitData;

BEGIN
	InitData; NEW(action); actionIsAlive := FALSE
CLOSE
	IF actionIsAlive THEN Services.RemoveAction(action) END
END ObxCubes.
