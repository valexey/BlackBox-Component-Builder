MODULE ObxScroll;
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

	IMPORT Stores, Fonts, Ports, Views, Controllers, Properties;

	CONST
		minVersion = 0; maxVersion = 0;
		cellSize = 7 * Ports.mm;
		boardSize = 10;

	TYPE
		View = POINTER TO RECORD (Views.View)
			x, y: INTEGER
		END;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		wr.WriteInt(v.x); wr.WriteInt(v.y)
	END Externalize;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.x); rd.ReadInt(v.y)
		END
	END Internalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: View DO
			v.x := source.x; v.y := source.y
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR x, y, asc, dsc, w: INTEGER; color: Ports.Color; str: ARRAY 3 OF CHAR; font: Fonts.Font;
	BEGIN
		str := "00"; font := Fonts.dir.Default();
		x := l DIV cellSize;
		IF Views.IsPrinterFrame(f) THEN r := r - r MOD cellSize END;
		WHILE (x * cellSize < r) & (v.x + x < boardSize) DO
			y := t DIV cellSize;
			IF Views.IsPrinterFrame(f) THEN b := b - b MOD cellSize END;
			WHILE (y * cellSize < b) & (v.y + y < boardSize) DO
				IF ODD(x + y + v.x + v.y) THEN color := Ports.black ELSE color := Ports.white END;
				f.DrawRect(x * cellSize, y * cellSize, (x + 1) * cellSize, (y + 1) * cellSize, Ports.fill, color);
				str[0] := CHR(ORD("0") + x + v.x); str[1] := CHR(ORD("0") + y + v.y);
				font.GetBounds(asc, dsc, w);
				f.DrawString(
					(x * cellSize + cellSize DIV 2) - font.StringWidth(str) DIV 2,
					(y * cellSize + cellSize DIV 2) + asc DIV 2, Ports.red, str, font);
				INC(y)
			END;
			INC(x)
		END
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Views.CtrlMessage; VAR focus: Views.View);
		VAR val, vis, w, h: INTEGER; changed: BOOLEAN;
	BEGIN
		WITH msg: Controllers.PollSectionMsg DO
			v.context.GetSize(w, h);
			msg.focus := FALSE;	(* v is not a container *)
			IF msg.vertical THEN
				msg.partSize := h DIV cellSize;
				msg.wholeSize := boardSize + MAX(0, msg.partSize +  v.y - boardSize);
				msg.partPos := v.y
			ELSE
				msg.partSize := w DIV cellSize;
				msg.wholeSize := boardSize + MAX(0, msg.partSize + v.x - boardSize);
				msg.partPos := v.x
			END;
			msg.valid := (msg.partSize < msg.wholeSize);
			msg.done := TRUE
		| msg: Controllers.ScrollMsg DO
			v.context.GetSize(w, h); changed := FALSE;
			msg.focus := FALSE;	(* v is not a container *)
			IF msg.vertical THEN
				val := v.y; vis := h DIV cellSize
			ELSE
				val := v.x; vis := w DIV cellSize
			END;
			CASE msg.op OF
			Controllers.decLine:
				IF val > 0 THEN DEC(val); changed := TRUE END
			| Controllers.incLine:
				IF val < boardSize - vis THEN INC(val); changed := TRUE END
			| Controllers.decPage:
				DEC(val, vis); changed := TRUE;
				IF val < 0THEN val := 0 END
			| Controllers.incPage:
				INC(val, vis); changed := TRUE;
				IF val > boardSize - vis THEN val := boardSize - vis END
			| Controllers.gotoPos:
				val := msg.pos; changed := TRUE
			END;
			IF msg.vertical THEN v.y := val ELSE v.x := val END;
			msg.done := TRUE;
			IF changed THEN Views.Update(v, Views.keepFrames) END
		| msg: Controllers.PageMsg DO
			v.context.GetSize(w, h);
			IF msg.op IN {Controllers.nextPageY, Controllers.gotoPageY} THEN
				vis := h DIV cellSize
			ELSE
				vis := w DIV cellSize
			END;
			CASE msg.op OF
			Controllers.nextPageX:
				INC(v.x, vis)
			| Controllers.nextPageY:
				INC(v.y, vis)
			| Controllers.gotoPageX:
				v.x := msg.pageX * vis
			| Controllers.gotoPageY:
				v.y := msg.pageY * vis
			END;
			msg.done := TRUE;
			msg.eox := v.x >= boardSize;
			msg.eoy := v.y >= boardSize
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR p: Properties.Message);
	BEGIN
		WITH p: Properties.SizePref DO
			IF p.w = Views.undefined THEN p.w := (boardSize - v.x) * cellSize END;
			IF p.h = Views.undefined THEN p.h := (boardSize - v.y) * cellSize END
		| p: Properties.ResizePref DO
			p.horFitToWin := TRUE;
			p.verFitToWin := TRUE
		| p: Properties.FocusPref DO
			p.setFocus := TRUE
		ELSE	(* ignore other messages *)
		END
	END HandlePropMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); v.x := 0; v.y := 0; Views.Deposit(v)
	END Deposit;

	PROCEDURE DepositAt* (x, y: INTEGER);
		VAR v: View;
	BEGIN
		NEW(v); v.x := 0; v.y := 0;
		IF (x > 0) & (x < boardSize) THEN v.x := x END;
		IF (y > 0) & (y < boardSize) THEN v.y := y END;
		Views.Deposit(v)
	END DepositAt;

END ObxScroll.
