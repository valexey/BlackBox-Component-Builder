MODULE ObxBlackBox;
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

	IMPORT Ports, Stores, Models, Views, Controllers, Properties, Fonts, Dialog, Services, Strings;

	CONST 
		minVersion = 0; maxVersion = 0;
		minded = -3; marked = -4; markedAndMinded = -7;	(* inside marks *)
		absorbed = -1; reflected = -2;	(* outside marks *)

	TYPE
		Model = POINTER TO RECORD (Models.Model)
			board : POINTER TO ARRAY OF ARRAY OF BYTE;
			m, (* size of board *)
			p, (* number of atoms *)
			n, (* number of actual guess *)
			score: INTEGER; 
			showsol: BOOLEAN
		END;

		Path = POINTER TO RECORD
			i, j: INTEGER; next: Path
		END;

		View = POINTER TO RECORD (Views.View)
			model: Model;
			i, j: INTEGER;
			d: INTEGER;
			font: Fonts.Font
		END;

		UpdateMsg = RECORD (Models.UpdateMsg) END;

	VAR 
		para*: RECORD
			nrOfAtoms*, boardSize*: INTEGER
		END;

		seed: INTEGER;

	PROCEDURE UniRand (): REAL;
		CONST a = 16807; m = 2147483647; q = m DIV a; r = m MOD a;
	BEGIN
		seed := a*(seed MOD q) - r*(seed DIV q);
		IF seed <= 0 THEN seed := seed + m END;
		RETURN seed * (1.0/m)
	END UniRand;

	(* problem-specific part *)

	PROCEDURE Atom (m: Model; i,j: INTEGER): BOOLEAN;
		VAR b: BYTE;
	BEGIN
		b := m.board[i,j]; RETURN (b = minded) OR (b = markedAndMinded)
	END Atom;

	PROCEDURE Marked (m: Model; i,j: INTEGER): BOOLEAN;
		VAR b: BYTE;
	BEGIN
		b := m.board[i,j]; RETURN (b = marked) OR (b = markedAndMinded)
	END Marked;

	PROCEDURE Shoot (m: Model; i1, j1: INTEGER);
		VAR i, j, d, di, dj : INTEGER;
	BEGIN
		IF j1 = 0 THEN di := 0; dj := 1
		ELSIF j1 = m.m+1 THEN di := 0; dj := -1
		ELSIF i1 = 0 THEN di := 1; dj := 0
		ELSIF i1 = m.m+1 THEN di := -1; dj := 0
		END; 
		i := i1; j := j1;
		IF ~Atom(m, i+di, j+dj) THEN
			REPEAT
				IF Atom(m, i+di+dj, j+di+dj) THEN d := di; di := -dj; dj := -d
				ELSIF Atom(m,i+di-dj, j-di+dj) THEN d := di; di := dj; dj := d
				ELSE i := i+di; j := j+dj
				END
			UNTIL (i=0) OR (i=m.m+1) OR (j=0) OR (j=m.m+1) OR Atom(m, i+di, j+dj);
			IF (i=0) OR (i=m.m+1) OR (j=0) OR (j=m.m+1) THEN
				IF (i = i1) & (j = j1) THEN m.board[i1, j1] := reflected
				ELSE INC(m.n); m.board[i,j] := SHORT(SHORT(m.n)); m.board[i1,j1] := SHORT(SHORT(m.n))
				END
			ELSE m.board[i1,j1] := absorbed
			END
		ELSE m.board[i1,j1] := absorbed
		END
	END Shoot;

	PROCEDURE GetPath (m: Model; i, j: INTEGER; VAR p: Path);
		VAR d, di, dj : INTEGER;

		PROCEDURE AddPoint(i, j: INTEGER); 
			VAR q: Path;
		BEGIN
			IF (p = NIL) OR (p.i # i) OR (p.j # j) THEN NEW(q); q.i := i; q.j := j; q.next := p; p := q END
		END AddPoint;

	BEGIN
		IF j = 0 THEN di := 0; dj := 1
		ELSIF j = m.m+1 THEN di := 0; dj := -1
		ELSIF i = 0 THEN di := 1; dj := 0
		ELSIF i = m.m+1 THEN di := -1; dj := 0
		END; 
		IF ~Atom(m, i+di, j+dj) THEN AddPoint(i, j);
			REPEAT
				IF Atom(m, i+di+dj, j+di+dj) THEN d := di; di := -dj; dj := -d; AddPoint(i, j)
				ELSIF Atom(m, i+di-dj, j-di+dj) THEN d := di; di := dj; dj := d; AddPoint(i, j)
				ELSE i := i+di; j := j+dj
				END;
			UNTIL (i = 0) OR (i = m.m+1) OR (j = 0) OR (j = m.m+1) OR Atom(m, i+di, j+dj);
			IF ~((i = 0) OR (i = m.m+1) OR (j = 0) OR (j = m.m+1)) THEN i := i+di; j := j+dj END;
			AddPoint(i, j)
		END
	END GetPath;

	PROCEDURE NewPuzzle (m: Model);
		VAR i, j, k: INTEGER;
	BEGIN
		FOR i := 0 TO m.m+1 DO FOR j := 0 TO m.m+1 DO m.board[i,j] := 0 END END;		
		k := 0;
		WHILE k < m.p DO
			i := 1 + SHORT(SHORT(ENTIER(UniRand()*m.m)));
			j := 1 + SHORT(SHORT(ENTIER(UniRand()*m.m)));
			IF ~Atom(m, i, j) THEN m.board[i,j] := minded; INC(k) END
		END
	END NewPuzzle;

	PROCEDURE Score (m: Model): INTEGER;
		VAR i, j, score, n: INTEGER;
	BEGIN
		score := 0; n := 0;
		FOR i := 0 TO m.m + 1 DO
			FOR j := 0 TO m.m + 1 DO
				IF (i = 0) OR (j = 0) OR (i = m.m+1) OR (j = m.m+1) THEN
					IF m.board[i,j] # 0 THEN INC(score) END
				ELSE
					IF Marked(m, i, j) THEN INC(n);
						IF ~Atom(m, i, j) THEN INC(score, 5) END
					END
				END
			END
		END;
		IF n < m.p THEN INC(score, 5 * (m.p - n)) END;
		RETURN score
	END Score;

	(* graphics part *)

	PROCEDURE DrawStringCentered (v: View; f: Ports.Frame; x, y: INTEGER; s: ARRAY OF CHAR);
		VAR asc, dsc, w: INTEGER;
	BEGIN
		v.font.GetBounds(asc, dsc, w);
		f.DrawString(x - v.font.StringWidth(s) DIV 2, y + asc DIV 2, Ports.black, s, v.font)
	END DrawStringCentered;

	PROCEDURE GetCoord (v: View; i, j: INTEGER; VAR x, y: INTEGER);
	BEGIN
		y := j * v.d + v.d DIV 2 + 1;
		x := i * v.d + v.d DIV 2 + 1;
		IF i = 0 THEN INC(x, v.d DIV 2)
		ELSIF i = v.model.m+1 THEN DEC(x, v.d DIV 2)
		ELSIF j = 0 THEN INC(y, v.d DIV 2)
		ELSIF j = v.model.m+1 THEN DEC(y, v.d DIV 2)
		END
	END GetCoord;

	(* Model *)

	PROCEDURE Init (m: Model);
	BEGIN
		m.m := para.boardSize; m.p := para.nrOfAtoms;
		NEW(m.board, m.m+2, m.m+2); NewPuzzle(m);
		m.n := 0; m.score := 0; m.showsol := FALSE
	END Init;

	PROCEDURE (m: Model) Externalize (VAR wr: Stores.Writer);
		VAR i, j: INTEGER;
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteInt(m.m);
		wr.WriteInt(m.p);
		wr.WriteInt(m.n);
		wr.WriteInt(m.score);
		wr.WriteBool(m.showsol);
		FOR i := 0 TO m.m+1 DO
			FOR j := 0 TO m.m+1 DO
				wr.WriteByte(m.board[i,j])
			END
		END
	END Externalize;

	PROCEDURE (m: Model) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; i, j: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(m.m);
			rd.ReadInt(m.p);
			rd.ReadInt(m.n);
			rd.ReadInt(m.score);
			rd.ReadBool(m.showsol);
			NEW(m.board, m.m+2, m.m+2);
			FOR i := 0 TO m.m+1 DO
				FOR j := 0 TO m.m+1 DO
					rd.ReadByte(m.board[i,j])
				END
			END
		END
	END Internalize;

	PROCEDURE (m: Model) CopyFrom (source: Stores.Store);
		VAR i, j: INTEGER;
	BEGIN
		WITH source: Model DO
			Init(m);
			m.m := source.m; NEW(m.board, m.m+2, m.m+2);
			m.n := source.n; m.p := source.p;
			m.score := source.score; m.showsol := source.showsol;
			FOR i := 0 TO m.m+1 DO
				FOR j := 0 TO m.m+1 DO m.board[i,j] := source.board[i,j] END
			END
		END
	END CopyFrom;

	(* View *)

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteInt(v.i);
		wr.WriteInt(v.j);
		wr.WriteStore(v.model)		
	END Externalize;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER; st: Stores.Store;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.i);
			rd.ReadInt(v.j);
			rd.ReadStore(st);
			v.model := st(Model);
			v.d := 0; 
			v.font := NIL
		END
	END Internalize;

	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		ASSERT(model IS Model, 20);
		WITH source: View DO
			v.model := model(Model);
			v.i := source.i; v.j := source.j; v.d := source.d; v.font := source.font
		END
	END CopyFromModelView;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.model
	END ThisModel;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR w, h, d, x, y, x1, y1, asc, dsc, fw, i, j: INTEGER; p: Path; s: ARRAY 16 OF CHAR;
	BEGIN
		v.context.GetSize(w, h); d := w DIV (v.model.m + 2);
		IF (v.font = NIL) OR (v.d # d) THEN
			v.d := d; v.font := Fonts.dir.This("Chicago", d * 2 DIV 3, {}, Fonts.normal)
		END;

		FOR i := 1 TO v.model.m + 1 DO
			f.DrawLine(d, i * d,w - d, i * d, f.unit, 0);
			f.DrawLine(i * d, d, i * d,w - d, f.unit, 0)
		END;
		FOR i := 0 TO v.model.m + 1 DO
			FOR j := 0 TO v.model.m + 1 DO
				x := i * d + d DIV 2; y := j * d + d DIV 2;

				IF (i = 0) OR (i = v.model.m + 1) OR (j = 0) OR (j = v.model.m + 1) THEN
					IF v.model.board[i , j] = absorbed THEN DrawStringCentered(v, f, x, y, "A")
					ELSIF v.model.board[i , j] = reflected THEN DrawStringCentered(v, f, x, y, "R")
					ELSIF v.model.board[i, j] > 0 THEN
						Strings.IntToString(v.model.board[i, j], s); DrawStringCentered(v, f, x, y, s)
					END
				ELSE
					IF Marked(v.model, i, j) THEN r := (9 * d) DIV 20;
						f.DrawOval(x - r, y - r, x + r, y + r, Ports.fill, Ports.black)
					END;
					IF v.model.showsol & Atom(v.model, i, j) THEN r := d DIV 3;
						IF Marked(v.model, i, j) THEN f.DrawOval(x - r, y - r, x + r, y + r, Ports.fill, Ports.white)
						ELSE f.DrawOval(x - r, y - r, x + r, y + r, Ports.fill, Ports.black)
						END
					END
				END
			END
		END;
		IF (v.i > 0) OR (v.j > 0) THEN
			GetPath(v.model, v.i, v.j, p);
			IF p # NIL THEN
				GetCoord(v, p.i, p.j, x, y); p := p.next;
				WHILE p # NIL DO
					GetCoord(v, p.i, p.j, x1, y1);
					f.DrawLine(x, y, x1, y1, 2 * f.unit, 0); x := x1; y := y1; p := p.next
				END
			END
		END;
		Strings.IntToString(v.model.p, s);
		v.font.GetBounds(asc, dsc, fw);
		x := d; y := (v.model.m + 2) * d + (d + asc) DIV 2;
		f.DrawString(x, y, Ports.black, "Atoms: ", v.font);  x := x + v.font.StringWidth("Atoms: ");
		f.DrawString(x, y, Ports.black, s, v.font); 
		IF v.model.showsol THEN x := x + v.font.StringWidth(s);
			f.DrawString(x, y, Ports.black, "  Score: ", v.font); x := x + v.font.StringWidth("  Score: ");
			Strings.IntToString(v.model.score, s); f.DrawString(x, y, Ports.black, s, v.font)
		END
	END Restore;

	PROCEDURE Track (v: View; f: Views.Frame; x, y: INTEGER; buttons: SET);
		VAR i, j: INTEGER; msg: UpdateMsg;
	BEGIN
		i := SHORT(x DIV v.d); j := SHORT(y DIV v.d);
		IF (i > 0) & (i <= v.model.m) & (j > 0) & (j <= v.model.m) THEN	(* inside *)
			IF Marked(v.model, i, j) THEN INC(v.model.board[i, j], 4)
			ELSE DEC(v.model.board[i, j], 4) 
			END
		ELSIF ((i = 0) OR (i = v.model.m + 1)) & (j > 0) & (j <= v.model.m)
		OR ((j = 0) OR (j  = v.model.m + 1)) & (i > 0) & (i <= v.model.m) THEN
			IF v.model.board[i, j] = 0 THEN Shoot(v.model, i, j) END;
			IF v.model.showsol THEN
				IF Controllers.modify IN buttons THEN v.i := i; v.j := j ELSE v.i := 0; v.j := 0 END
			END
		END;
		Models.Broadcast(v.model, msg)
	END Track;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
		VAR w, h: INTEGER;
	BEGIN
		WITH msg: UpdateMsg DO
			IF ~v.model.showsol THEN v.i := 0; v.j := 0 END;	(* adjust view to change of model *)
			v.context.GetSize(w, h); Views.UpdateIn(v, 0, 0, w, h,  Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																				VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			Track(v, f, msg.x, msg.y, msg.modifiers); Views.SetDirty(v)
		| msg: Controllers.PollOpsMsg DO
			msg.type := "ObxBlackBox.View"
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w > Views.undefined) & (msg.h > Views.undefined) THEN
				Properties.ProportionalConstraint(v.model.m, v.model.m + 1, msg.fixedW, msg.fixedH, msg.w, msg.h)
			ELSE
				msg.w := 100 * Ports.mm; msg.h := msg.w * (v.model.m + 1) DIV v.model.m;
			END;
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;

	(* commands *)

	PROCEDURE Deposit*;
		VAR v: View; m: Model;
	BEGIN
		NEW(m); Init(m);
		NEW(v); v.model := m; Stores.Join(v, m);
		Views.Deposit(v)
	END Deposit;

	PROCEDURE ShowSolution*;
		VAR v : Views.View; msg: UpdateMsg;
	BEGIN
		v := Controllers.FocusView();
		IF v # NIL THEN
			WITH v: View DO
				v.model.showsol := TRUE; v.model.score := Score(v.model);
				Models.Broadcast(v.model, msg)
			END
		END
	END ShowSolution;

	PROCEDURE ShowSolutionGuard* (VAR par: Dialog.Par);
		VAR v: Views.View;
	BEGIN
		v := Controllers.FocusView();
		par.disabled := (v = NIL) OR ~(v IS View) OR v(View).model.showsol
	END ShowSolutionGuard;

	PROCEDURE New*;
		VAR v: Views.View; msg: UpdateMsg;
	BEGIN
		v := Controllers.FocusView();
		IF v # NIL THEN
			WITH v: View DO
				NewPuzzle(v.model);
				v.model.n := 0; v.model.score := 0; v.model.showsol := FALSE;
				v.i := 0; v.j := 0;
				Models.Broadcast(v.model, msg)
			END
		END		
	END New;

	PROCEDURE Set*;
		VAR v : Views.View; msg: UpdateMsg; i, j: INTEGER;
	BEGIN
		v := Controllers.FocusView();
		IF v # NIL THEN
			WITH v: View DO
				v.model.p := 0;
				FOR i := 0 TO v.model.m + 1 DO
					FOR j := 0 TO v.model.m + 1 DO
						IF Marked(v.model, i, j) THEN INC(v.model.p); v.model.board[i,j] := minded
						ELSE v.model.board[i,j] := 0
						END
					END
				END;
				v.model.n := 0; v.model.score := 0; v.model.showsol := FALSE;
				v.i := 0; v.j := 0;
				Models.Broadcast(v.model, msg)
			END
		END		
	END Set;

BEGIN
	seed := SHORT(Services.Ticks()); para.boardSize := 8; para.nrOfAtoms := 4
END ObxBlackBox.
