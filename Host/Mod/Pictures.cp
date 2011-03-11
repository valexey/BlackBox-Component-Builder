MODULE HostPictures;
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
		SYSTEM, COM, WinApi, WinOle,
		Files, Fonts, Ports, Stores, Models, Views, Controllers, Properties, Dialog, Converters,
		HostPorts, HostWindows, HostClipboard;

	CONST
		clip = 0X; fit = 1X; approximate = 2X;	(* modes *)
		macPict = 0; winPict = 1;	(* types *)
		minVersion = 0; maxVersion = 0;

	TYPE
		Model = POINTER TO RECORD
			file: Files.File;
			pos, len: INTEGER;
			ref: WinApi.HANDLE;
			data: POINTER TO ARRAY OF BYTE
		END;

		StdView = POINTER TO RECORD (Views.View)
			model: Model;
			unit: INTEGER;
			w, h: INTEGER;
			mode: SHORTCHAR;
			type: BYTE
		END;
		
	
	(* Mac Pictures *)

	PROCEDURE ReadWord (rd: Files.Reader; VAR x: INTEGER);
		VAR b: BYTE;
	BEGIN
		rd.ReadByte(b); x := 256 * (b MOD 256);
		rd.ReadByte(b); x := x + b MOD 256
	END ReadWord;
	
	PROCEDURE ReadByte (rd: Files.Reader; VAR x: INTEGER);
		VAR b: BYTE;
	BEGIN
		rd.ReadByte(b); x := b;
	END ReadByte;
	
	PROCEDURE Skip (rd: Files.Reader; n: INTEGER);
	BEGIN
		rd.SetPos(rd.Pos() + n)
	END Skip;
	
	PROCEDURE DrawMacPicture (v: StdView; f: Views.Frame; w, h: INTEGER);
		VAR rd: Files.Reader; res, end, len, x, y, z, l, t, r, b, u: INTEGER; op: INTEGER; v2, clipped: BOOLEAN;
			fgnd, bgnd: Ports.Color; str: ARRAY 256 OF CHAR; size, style: INTEGER;
			p: HostPorts.Port; dc: WinApi.HANDLE; poly: ARRAY 512 OF WinApi.POINT; bt: BYTE;
			rl, rt, rr, rb, dl, dt, dr, db, ol, ot, or, ob, al, at, ar, ab, as, aa, gl, gt, gr, gb, ow, oh, lx, ly, tx, ty, plen: INTEGER;
			actPen, actBrush, nullPen, nullBrush, fillBrush, bgndBrush, bitmap, font: WinApi.HANDLE;
		
		PROCEDURE Point (VAR x, y: INTEGER);
		BEGIN
			ReadWord(rd, y); y := (y + 32768) MOD 65536 - 32768;
			ReadWord(rd, x); x := (x + 32768) MOD 65536 - 32768
		END Point;
		
		PROCEDURE Data (n: INTEGER);
			VAR b: BYTE;
		BEGIN
			WHILE n > 0 DO rd.ReadByte(b); DEC(n) END
		END Data;
		
		PROCEDURE Region (VAR l, t, r, b: INTEGER);
			VAR len: INTEGER;
		BEGIN
			ReadWord(rd, len); ReadWord(rd, t); ReadWord(rd, l);
			ReadWord(rd, b); ReadWord(rd, r); Data(len - 10)
		END Region;
		
		PROCEDURE Poly (VAR poly: ARRAY OF WinApi.POINT; VAR len: INTEGER);
			VAR i: INTEGER;
		BEGIN
			ReadWord(rd, len); len := (len - 10) DIV 4;
			ReadWord(rd, i); ReadWord(rd, i);
			ReadWord(rd, i); ReadWord(rd, i); i := 0;
			WHILE i < len DO Point(poly[i].x, poly[i].y); INC(i) END
		END Poly;
		
		PROCEDURE Pattern (VAR brush: WinApi.HANDLE);
			VAR pat: ARRAY 8 OF SHORTINT; i: INTEGER; b: BYTE;
		BEGIN
			res := WinApi.SelectObject(dc, nullBrush);
			res := WinApi.DeleteObject(brush);
			res := WinApi.DeleteObject(bitmap);
			i := 0;
			WHILE i < 8 DO rd.ReadByte(b); pat[i] := SHORT(255 - b MOD 256); INC(i) END;
			bitmap := WinApi.CreateBitmap(8, 8, 1, 1, SYSTEM.ADR(pat));
			brush := WinApi.CreatePatternBrush(bitmap)
		END Pattern;
		
		PROCEDURE Pen (x, y: INTEGER);
		BEGIN
			IF x < y THEN x := y END;
			res := WinApi.SelectObject(dc, nullPen);
			res := WinApi.DeleteObject(actPen);
			IF x = 0 THEN actPen := nullPen ELSE actPen := WinApi.CreatePen(6, x, Ports.black) END
		END Pen;
		
		PROCEDURE String (VAR s: ARRAY OF CHAR; VAR len: INTEGER);
			VAR b: BYTE; i: INTEGER;
		BEGIN
			rd.ReadByte(b); len := b MOD 256; i := 0;
			WHILE i < len DO
				rd.ReadByte(b);
				(* mac to windows *)
				IF b >= ORD(" ") THEN s[i] := CHR(b MOD 256); INC(i) ELSE DEC(len) END
			END;
			s[i] := 0X;
		END String;
		
		PROCEDURE PixMap (VAR rowBytes, height: INTEGER; VAR v2: BOOLEAN);
			VAR x: INTEGER;
		BEGIN
			ReadWord(rd, rowBytes); v2 := rowBytes >= 32768;
			ReadWord(rd, x); height := x;
			ReadWord(rd, x);
			ReadWord(rd, x); height := x - height;
			ReadWord(rd, x);
			IF v2 THEN
				DEC(rowBytes, 32768);
				ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x);
				Point(x, y); Point(x, y);
				Point(x, y); Point(x, y);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x)
			END
		END PixMap;
		
		PROCEDURE ColorTab;
			VAR x, n: INTEGER;
		BEGIN
			ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, n);
			WHILE n >= 0 DO
				ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x);
				DEC(n)
			END
		END ColorTab;
		
		PROCEDURE PixData (rowBytes, height: INTEGER; v2: BOOLEAN);
			VAR n, m: INTEGER; b: BYTE;
		BEGIN
			IF rowBytes < 8 THEN
				Skip(rd, rowBytes * height)
			ELSIF v2 THEN
				WHILE height > 0 DO
					IF rowBytes > 250 THEN ReadWord(rd, n) ELSE rd.ReadByte(b); n := b MOD 256 END;
					REPEAT
						ReadByte(rd, m); DEC(n);
						IF m >= 0 THEN
							WHILE m >= 0 DO rd.ReadByte(b); DEC(n); DEC(m) END
						ELSE
							ASSERT(m # -128, 100);
							rd.ReadByte(b); DEC(n)
						END
					UNTIL n <= 0;
					DEC(height)
				END
			ELSE
				WHILE height > 0 DO
					IF rowBytes > 250 THEN ReadWord(rd, n) ELSE rd.ReadByte(b); n := b MOD 256 END;
					Skip(rd, n); DEC(height)
				END
			END
		END PixData;
		
		PROCEDURE BitMap (isRegion: BOOLEAN);
			VAR x, y, w, h, l, t, r, b: INTEGER; v2: BOOLEAN;
		BEGIN
			PixMap(w, h, v2);
			IF v2 THEN ColorTab END;
			Point(x, y); Point(x, y); Point(x, y); Point(x, y); ReadWord(rd, x);
			IF isRegion THEN Region(l, t, r, b) END;
			PixData(w, h, v2)
		END BitMap;
		
		PROCEDURE PixPattern;
			VAR type, x, w, h: INTEGER; v2: BOOLEAN;
		BEGIN
			ReadWord(rd, type); Data(8);
			IF type = 1 THEN
				PixMap(w, h, v2); ColorTab; PixData(w, h, v2)
			ELSIF type = 2 THEN
				ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x)
			ELSE HALT(100)
			END
		END PixPattern;
		
		PROCEDURE Setup;
		BEGIN
			CASE op MOD 8 OF
			| 0: res := WinApi.SelectObject(dc, nullBrush); res := WinApi.SelectObject(dc, actPen)
			| 1: res := WinApi.SelectObject(dc, actBrush); res := WinApi.SelectObject(dc, nullPen)
			| 2: res := WinApi.SelectObject(dc, bgndBrush); res := WinApi.SelectObject(dc, nullPen)
			| 4: res := WinApi.SelectObject(dc, fillBrush); res := WinApi.SelectObject(dc, nullPen)
			| 3, 5..7: res := WinApi.SelectObject(dc, nullBrush); res := WinApi.SelectObject(dc, nullPen)
			END
		END Setup;
		
		PROCEDURE SetLn;
		BEGIN
			res := WinApi.SelectObject(dc, actPen)
		END SetLn;
		
		PROCEDURE NewFont;
			VAR res, weight: INTEGER; old: WinApi.HANDLE;
		BEGIN
			IF ODD(style) THEN weight := 700 ELSE weight := 400 END;
			old := font;
			font := WinApi.CreateFontW(
				-size, 0, 0, 0, weight, style DIV 2 MOD 2, style DIV 4 MOD 2, 0, 0, 3, 2, 1, 6, "Arial");
			res := WinApi.SelectObject(dc, font);
			res := WinApi.DeleteObject(old)
		END NewFont;
		
		PROCEDURE LineTo (x, y: INTEGER);
			VAR res: INTEGER;
		BEGIN
			IF ~clipped THEN res := WinApi.LineTo(dc, lx, ly)
			ELSE res := WinApi.MoveToEx(dc, lx, ly, NIL)
			END
		END LineTo;
		
		PROCEDURE Text (x, y: INTEGER);
			VAR res, len: INTEGER; str: ARRAY 256 OF CHAR;
		BEGIN
			String(str, len);
			IF ~clipped THEN res := WinApi.TextOutW(dc, x, y, str, len) END
		END Text;
		
		PROCEDURE Rectangle (l, t, r, b: INTEGER);
			VAR res: INTEGER;
		BEGIN
			IF ~clipped THEN res := WinApi.Rectangle(dc, l, t, r, b) END
		END Rectangle;
		
		PROCEDURE RoundRect (l, t, r, b, w, h: INTEGER);
			VAR res: INTEGER;
		BEGIN
			IF ~clipped THEN res := WinApi.RoundRect(dc, l, t, r, b, w, h) END
		END RoundRect;
		
		PROCEDURE Ellipse (l, t, r, b: INTEGER);
			VAR res: INTEGER;
		BEGIN
			IF ~clipped THEN res := WinApi.Ellipse(dc, l, t, r, b) END
		END Ellipse;
		
		PROCEDURE Polyline (VAR poly: ARRAY OF WinApi.POINT; len: INTEGER);
			VAR res: INTEGER;
		BEGIN
			IF ~clipped THEN res := WinApi.Polyline(dc, poly[0], len) END
		END Polyline;
		
		PROCEDURE Polygon (VAR poly: ARRAY OF WinApi.POINT; len: INTEGER);
			VAR res: INTEGER;
		BEGIN
			IF ~clipped THEN res := WinApi.Polygon(dc, poly[0], len) END
		END Polygon;
		
	BEGIN
		rd := v.model.file.NewReader(NIL);
		rd.SetPos(v.model.pos); end := v.model.pos + v.model.len;
		ReadWord(rd, x); Point(l, t); Point(r, b); v.w := r - l; v.h := b - t; v2 := FALSE;
		p := f.rider(HostPorts.Rider).port;
		p.CloseBuffer;
		dc := p.dc; u := p.unit;
		res := WinApi.SaveDC(dc);
		res := WinApi.SelectClipRgn(dc, 0);
		f.rider.GetRect(rl, rt, rr, rb);
		res := WinApi.IntersectClipRect(dc, rl, rt, rr, rb);
		res := WinApi.SetMapMode(dc, 8);	(* anisotropic *)
		res := WinApi.SetViewportOrgEx(dc, f.gx DIV u, f.gy DIV u, NIL);
		res := WinApi.SetViewportExtEx(dc, w DIV u, h DIV u, NIL);
		res := WinApi.SetWindowOrgEx(dc, l, t, NIL);
		res := WinApi.SetWindowExtEx(dc, r - l, b - t, NIL);
		res := WinApi.SetBkColor(dc, Ports.white);
		res := WinApi.SetTextColor(dc, Ports.black);
		f.rider(HostPorts.Rider).FixOrigin;
		nullPen := WinApi.GetStockObject(WinApi.NULL_PEN);
		nullBrush := WinApi.GetStockObject(WinApi.NULL_BRUSH);
		actPen := WinApi.CreatePen(6, 1, Ports.black);
		actBrush := WinApi.GetStockObject(WinApi.BLACK_BRUSH);
		fillBrush := WinApi.GetStockObject(WinApi.BLACK_BRUSH);
		bgndBrush := WinApi.GetStockObject(WinApi.WHITE_BRUSH);
		bitmap := 0; font := 0;
		tx := 0; ty := 0; lx := 0; ly := 0; size := 10; style := 0; NewFont;
		clipped := FALSE;
		REPEAT
			rd.ReadByte(bt); op := bt MOD 256;
			IF v2 THEN rd.ReadByte(bt); op := 256 * op + bt MOD 256 END;
			CASE op OF
			| 0: (* nop *)
			| 1: (* clip *) Region(l, t, r, b); clipped := (l = 0) & (t = 0) & (r = 0) & (b = 0)
			| 2: (* bgnd pattern *) Pattern(bgndBrush)
			| 3: (* text font *) ReadWord(rd, x)
			| 4: (* text face *) rd.ReadByte(bt); style := bt; NewFont
			| 5: (* text mode *) ReadWord(rd, x)
			| 6: (* space extra *) Point(x, y)
			| 7: (* pen size *) Point(x, y); Pen(x, y);
			| 8: (* pen mode *) ReadWord(rd, x)
			| 9: (* pen pattern *) Pattern(actBrush)
			| 0AH: (* fill pattern *) Pattern(fillBrush)
			| 0BH: (* oval size *) Point(ow, oh)
			| 0CH: (* origin *) Point(x, y)
			| 0DH: (* text size *)  ReadWord(rd, size); NewFont
			| 0EH: (* foreground *) ReadWord(rd, x); ReadWord(rd, y); fgnd := x * 65536 + y
			| 0FH: (* background *) ReadWord(rd, x); ReadWord(rd, y); bgnd := x * 65536 + y
			| 10H: (* text ratio *) Point(x, y); Point(x, y)
			| 11H: (* version *) rd.ReadByte(bt); v2 := bt = 2
			| 12H: (* bg pix pattern *) PixPattern
			| 13H: (* pen pix pattern *) PixPattern
			| 14H: (* fill pix pattern *) PixPattern
			| 15H: (* fract pen pos *) ReadWord(rd, x)
			| 16H: (* char extra *) ReadWord(rd, x)
			| 17H..19H: (* ??? *)
			| 1AH: (* rgb fg col *) ReadWord(rd, x); ReadWord(rd, y); ReadWord(rd, z)
			| 1BH: (* rgb bg col *) ReadWord(rd, x); ReadWord(rd, y); ReadWord(rd, z)
			| 1CH: (* hilite mode *)
			| 1DH: (* rgb hl col *) ReadWord(rd, x); ReadWord(rd, y); ReadWord(rd, z)
			| 1EH: (* def hilite *)
			| 1FH: (* rgb op col *) ReadWord(rd, x); ReadWord(rd, y); ReadWord(rd, z)
			| 20H: (* line *) Point(lx, ly); res := WinApi.MoveToEx(dc, lx, ly, NIL); Point(lx, ly); SetLn; LineTo(lx, ly)
			| 21H: (* line from *) Point(lx, ly); SetLn; LineTo(lx, ly)
			| 22H: (* short line *) Point(lx, ly); res := WinApi.MoveToEx(dc, lx, ly, NIL);
						ReadByte(rd, z); INC(lx, z); ReadByte(rd, z); INC(ly, z); SetLn; LineTo(lx, ly)
			| 23H: (* short line from *) ReadByte(rd, z); INC(lx, z); ReadByte(rd, z); INC(ly, z); SetLn; LineTo(lx, ly)
			| 24H..27H: (* ??? *) ReadWord(rd, len); Data(len)
			| 28H: (* long text *) Point(tx, ty); Text(tx, ty)
			| 29H: (* dh text *) rd.ReadByte(bt); INC(tx, bt MOD 256); Text(tx, ty)
			| 2AH: (* dv text *) rd.ReadByte(bt); INC(ty, bt MOD 256); Text(tx, ty)
			| 2BH: (* dhv text *) rd.ReadByte(bt); INC(tx, bt MOD 256); rd.ReadByte(bt); INC(ty, bt MOD 256); Text(tx, ty)
			| 2CH: (* font typeface ? *) ReadWord(rd, len); ReadWord(rd, x); String(str, z)
			| 2DH..2FH: (* ??? *) ReadWord(rd, len); Data(len)
			| 30H..37H: (* rect *) Point(rl, rt); Point(rr, rb); Setup; Rectangle(rl, rt, rr, rb)
			| 38H..3FH: (* same rect *); Setup; Rectangle(rl, rt, rr, rb)
			| 40H..47H: (* rrect *) Point(dl, dt); Point(dr, db); Setup; RoundRect(dl, dt, dr, db, ow, oh)
			| 48H..4FH: (* same rrect *); Setup; RoundRect(dl, dt, dr, db, ow, oh)
			| 50H..57H: (* oval *) Point(ol, ot); Point(or, ob); Setup; Ellipse(ol, ot, or, ob)
			| 58H..5FH: (* same oval *); Setup; Ellipse(ol, ot, or, ob)
			| 60H..67H: (* arc *) Point(al, at); Point(ar, ab); Point(aa, as);
			| 68H..6FH: (* same arc *) Point(aa, as);
			| 70H: (* poly *) Poly(poly, plen); SetLn; Polyline(poly, plen)
			| 71H..77H: (* poly *) Poly(poly, plen); Setup; Polygon(poly, plen)
			| 78H: (* same poly *) SetLn; Polyline(poly, plen)
			| 79H..7FH: (* same poly *) Setup; Polygon(poly, plen)
			| 80H..87H: (* rgn *) Region(gl, gt, gr, gb)
			| 88H..8FH: (* same rgn *)
			| 90H: (* bits rect *) BitMap(FALSE)
			| 91H: (* bits rgn *) BitMap(TRUE)
			| 92H..97H: (* ??? *) ReadWord(rd, len); Data(len)
			| 98H: (* packed bits rect *) BitMap(FALSE)
			| 99H: (* packed bits rgn *) BitMap(TRUE)
			| 9AH..9FH: (* ??? *) ReadWord(rd, len); Data(len)
			| 0A0H: (* short comment *) ReadWord(rd, x);
			| 0A1H: (* long comment *) ReadWord(rd, x); ReadWord(rd, len); Data(len)
			| 0A2H..0AFH: (* ??? *) ReadWord(rd, len); Data(len)
			| 0B0H..0CFH: (* ??? *)
			| 0D0H..0FEH: (* ??? *) ReadWord(rd, x); ReadWord(rd, len); Data(x * 65536 + len)
			| 0FFH: (* end *)
			ELSE
				IF op < 8000H THEN (* ??? *) Data(op DIV 256 * 2)
				ELSIF op < 8100H THEN (* ??? *)
				ELSE (* ??? *) ReadWord(rd, x); ReadWord(rd, len); Data(x * 65536 + len)
				END
			END;
			IF v2 & ODD(rd.Pos() - v.model.pos) THEN rd.ReadByte(bt) END;
		UNTIL (op = 0FFH) OR (rd.Pos() >= end);
		res := WinApi.RestoreDC(dc, -1);
		res := WinApi.DeleteObject(actPen);
		res := WinApi.DeleteObject(actBrush);
		res := WinApi.DeleteObject(fillBrush);
		res := WinApi.DeleteObject(bgndBrush);
		res := WinApi.DeleteObject(bitmap);
		res := WinApi.DeleteObject(font)
	END DrawMacPicture;
	
(*
	PROCEDURE Dump (v: StdView);
		VAR rd: Files.Reader; op, end, len, x, y: INTEGER; v2: BOOLEAN; b: BYTE;
		
		PROCEDURE Point;
			VAR x: INTEGER;
		BEGIN
			ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x)
		END Point;
		
		PROCEDURE Data (n: INTEGER);
		BEGIN
			WHILE n > 0 DO
				rd.ReadByte(b); DEC(n);
				Sub.Char(" "); Sub.IntForm(b, 16, 2, "0", FALSE);
			END
		END Data;
		
		PROCEDURE Region;
			VAR len: INTEGER;
		BEGIN
			ReadWord(rd, len);
			Point; Point;
			Data(len - 10)
		END Region;
		
		PROCEDURE Text;
			VAR b: BYTE; len: INTEGER;
		BEGIN
			rd.ReadByte(b); len := b MOD 256;
			Sub.String(' "');
			WHILE len > 0 DO rd.ReadByte(b); Sub.Char(CHR(b MOD 256)); DEC(len) END;
			Sub.Char('"')
		END Text;
		
		PROCEDURE PixMap (VAR rowBytes, height: INTEGER; VAR v2: BOOLEAN);
			VAR x: INTEGER;
		BEGIN
			ReadWord(rd, rowBytes); v2 := rowBytes >= 32768;
			Sub.Int(rowBytes);
			ReadWord(rd, x); Sub.Int(x); height := x;
			ReadWord(rd, x); Sub.Int(x);
			ReadWord(rd, x); Sub.Int(x); height := x - height;
			ReadWord(rd, x); Sub.Int(x);
			IF v2 THEN
				DEC(rowBytes, 32768);
				ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x);
				Point; Point;
				Point; Point;
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x);
				ReadWord(rd, x)
			END
		END PixMap;
		
		PROCEDURE ColorTab;
			VAR x, n: INTEGER;
		BEGIN
			ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, n); Sub.Int(n);
			WHILE n >= 0 DO
				ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x); ReadWord(rd, x);
				DEC(n)
			END
		END ColorTab;
		
		PROCEDURE PixData (rowBytes, height: INTEGER; v2: BOOLEAN);
			VAR n, m, k: INTEGER; b: BYTE;
		BEGIN
			IF rowBytes < 8 THEN
				Skip(rd, rowBytes * height)
			ELSIF v2 THEN
				WHILE height > 0 DO
					IF rowBytes > 250 THEN ReadWord(rd, n) ELSE rd.ReadByte(b); n := b MOD 256 END;
					k := 0;
					REPEAT
						rd.ReadByte(b); DEC(n); m := b;
						IF m >= 0 THEN
							WHILE m >= 0 DO rd.ReadByte(b); DEC(n); DEC(m); INC(k) END
						ELSE
							ASSERT(m # -128, 100);
							rd.ReadByte(b); DEC(n); INC(k, 1 - m)
						END
					UNTIL n <= 0;
					ASSERT(n = 0, 101);
					ASSERT(k = rowBytes, 102);
					(* Skip(rd, n); *) DEC(height)
				END
			ELSE
				WHILE height > 0 DO
					IF rowBytes > 250 THEN ReadWord(rd, n) ELSE rd.ReadByte(b); n := b MOD 256 END;
					Skip(rd, n); DEC(height)
				END
			END
		END PixData;
		
		PROCEDURE PixPattern;
			VAR type, w, h: INTEGER; v2: BOOLEAN;
		BEGIN
			ReadWord(rd, type); Data(8);
			IF type = 1 THEN
				PixMap(w, h, v2); ColorTab; PixData(w, h, v2)
			ELSIF type = 2 THEN
				ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x)
			ELSE HALT(100)
			END
		END PixPattern;
		
		PROCEDURE Bits (region: BOOLEAN);
			VAR w, h: INTEGER; v2: BOOLEAN;
		BEGIN
			PixMap(w, h, v2);
			IF v2 THEN ColorTab END;
			Point; Point; Point; Point; ReadWord(rd, x); Sub.Int(x);
			IF region THEN Region END;
			PixData(w, h, v2)
		END Bits;
		
	BEGIN
		rd := v.model.file.NewReader(NIL);
		rd.SetPos(v.model.pos); end := v.model.pos + v.model.len;
		ReadWord(rd, x); Sub.Int(x); Point; Point; Sub.Ln; v2 := FALSE;
		REPEAT
			Sub.Int(rd.Pos()); Sub.Char(" ");
			rd.ReadByte(b); op := b MOD 256;
			IF v2 THEN rd.ReadByte(b); op := 256 * op + b MOD 256 END;
			Sub.IntForm(op, 16, 4, "0", FALSE); Sub.Char(" ");
			CASE op OF
			| 0: Sub.String("nop")
			| 1: Sub.String("clip "); Region
			| 2: Sub.String("bkd pattern "); Data(8)
			| 3: Sub.String("text font "); ReadWord(rd, x); Sub.Int(x)
			| 4: Sub.String("text face "); rd.ReadByte(b); Sub.Int(b MOD 256)
			| 5: Sub.String("text mode "); ReadWord(rd, x);  Sub.Int(x)
			| 6: Sub.String("space extra "); Point
			| 7: Sub.String("pen size "); Point
			| 8: Sub.String("pen mode "); ReadWord(rd, x); Sub.Int(x)
			| 9: Sub.String("pen pattern "); Data(8)
			| 0AH: Sub.String("pen pattern "); Data(8)
			| 0BH: Sub.String("oval size "); Point
			| 0CH: Sub.String("origin "); Point
			| 0DH: Sub.String("text size "); ReadWord(rd, x); Sub.Int(x)
			| 0EH: Sub.String("foreground "); ReadWord(rd, x); ReadWord(rd, y); Sub.Int(x * 65536 + y)
			| 0FH: Sub.String("background "); ReadWord(rd, x); ReadWord(rd, y); Sub.Int(x * 65536 + y)
			| 10H: Sub.String("text ratio "); Point; Point
			| 11H: Sub.String("version "); rd.ReadByte(b); Sub.Int(b MOD 256); v2 := b = 2
			| 12H: Sub.String("bg pix pattern "); PixPattern
			| 13H: Sub.String("pen pix pattern "); PixPattern
			| 14H: Sub.String("fill pix pattern "); PixPattern
			| 15H: Sub.String("fract pen pos "); ReadWord(rd, x); Sub.Int(x)
			| 16H: Sub.String("char extra "); ReadWord(rd, x); Sub.Int(x)
			| 17H..19H: Sub.String("???")
			| 1AH: Sub.String("rgb fg col ");
				ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x)
			| 1BH: Sub.String("rgb bg col ");
				ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x)
			| 1CH: Sub.String("hilite mode")
			| 1DH: Sub.String("rgb hl col ");
				ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x)
			| 1EH: Sub.String("def hilite")
			| 1FH: Sub.String("rgb op col ");
				ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x); ReadWord(rd, x); Sub.Int(x)
			| 20H: Sub.String("line "); Point; Point
			| 21H: Sub.String("line from "); Point
			| 22H: Sub.String("short line ");
				Point; rd.ReadByte(b); Sub.Int(b MOD 256); rd.ReadByte(b); Sub.Int(b MOD 256)
			| 23H: Sub.String("short line from ");
				rd.ReadByte(b); Sub.Int(b MOD 256); rd.ReadByte(b); Sub.Int(b MOD 256)
			| 24H..27H: Sub.String("??? "); ReadWord(rd, len); Data(len)
			| 28H: Sub.String("long text "); Point; Text
			| 29H: Sub.String("dh text "); rd.ReadByte(b); Sub.Int(b MOD 256); Text
			| 2AH: Sub.String("dv text "); rd.ReadByte(b); Sub.Int(b MOD 256); Text
			| 2BH: Sub.String("dhv text ");
				rd.ReadByte(b); Sub.Int(b MOD 256); rd.ReadByte(b); Sub.Int(b MOD 256); Text
			| 2CH..2FH: Sub.String("??? "); ReadWord(rd, len); Data(len)
			| 30H..37H: Sub.String("rect "); Point; Point
			| 38H..3FH: Sub.String("same rect")
			| 40H..47H: Sub.String("rrect "); Point; Point
			| 48H..4FH: Sub.String("same rrect")
			| 50H..57H: Sub.String("oval "); Point; Point
			| 58H..5FH: Sub.String("same oval")
			| 60H..67H: Sub.String("arc "); Point; Point; Point
			| 68H..6FH: Sub.String("same arc "); Point
			| 70H..77H: Sub.String("poly "); Region
			| 78H..7FH: Sub.String("same poly")
			| 80H..87H: Sub.String("rgn "); Region
			| 88H..8FH: Sub.String("same rgn")
			| 90H: Sub.String("bits rect "); Bits(FALSE)
			| 91H: Sub.String("bits rgn "); Bits(TRUE)
			| 92H..97H: Sub.String("??? "); ReadWord(rd, len); Data(len)
			| 98H: Sub.String("packed bits rect "); Bits(FALSE)
			| 99H: Sub.String("packed bits rgn "); Bits(TRUE)
			| 9AH..9FH: Sub.String("??? "); ReadWord(rd, len); Data(len)
			| 0A0H: Sub.String("short comment "); ReadWord(rd, x); Sub.Int(x)
			| 0A1H: Sub.String("long comment "); ReadWord(rd, x); Sub.Int(x); ReadWord(rd, len); Data(len)
			| 0A2H..0AFH: Sub.String("??? "); ReadWord(rd, len); Data(len)
			| 0B0H..0CFH: Sub.String("???")
			| 0D0H..0FEH: Sub.String("??? "); ReadWord(rd, x); ReadWord(rd, len); Data(x * 65536 + len)
			| 0FFH: Sub.String("end")
			ELSE
				IF op < 8000H THEN Sub.String("??? "); Data(op DIV 256 * 2)
				ELSIF op < 8100H THEN Sub.String("???")
				ELSE Sub.String("??? "); ReadWord(rd, x); ReadWord(rd, len); Data(x * 65536 + len)
				END
			END;
			IF v2 & ODD(rd.Pos() - v.model.pos) THEN rd.ReadByte(b) END;
			Sub.Ln
		UNTIL (op = 0FFH) OR (rd.Pos() >= end)
	END Dump;
*)	
	
	PROCEDURE Evaluate (v: StdView);
		VAR len, adr: INTEGER; rd: Files.Reader;
	BEGIN
		len := v.model.len;
		IF v.model.data = NIL THEN
			rd := v.model.file.NewReader(NIL);
			rd.SetPos(v.model.pos);
			NEW(v.model.data, len);
			rd.ReadBytes(v.model.data^, 0, len)
		END;
		v.model.ref := WinApi.SetMetaFileBitsEx(len, v.model.data);
		ASSERT(v.model.ref # 0, 100);
	END Evaluate;


	(* Model *)
	
	PROCEDURE (m: Model) FINALIZE;
		VAR res: INTEGER;
	BEGIN
		IF m.ref # 0 THEN
			res := WinApi.DeleteMetaFile(m.ref);
			m.ref := 0
		END
	END FINALIZE;
	

	(* View *)

	PROCEDURE (v: StdView) Internalize (VAR rd: Stores.Reader);
		VAR m: Model; thisVersion: INTEGER;
	BEGIN
		v.Internalize^(rd);
		IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(minVersion, maxVersion, thisVersion);
		IF rd.cancelled THEN RETURN END;
		rd.ReadByte(v.type);
		IF (v.type # winPict) & (v.type # macPict) THEN rd.TurnIntoAlien(Stores.alienComponent); RETURN END;
		rd.ReadInt(v.unit);
		rd.ReadInt(v.w);
		rd.ReadInt(v.h);
		rd.ReadSChar(v.mode);
		NEW(m); m.file := rd.rider.Base();
		rd.ReadInt(m.len);
		m.pos := rd.Pos();
		m.ref := 0;	(* lazy allocation of metafile *)
		v.model := m;
		rd.SetPos(m.pos + m.len)
	END Internalize;

	PROCEDURE (v: StdView) Externalize (VAR wr: Stores.Writer);
		VAR len, res: INTEGER; r: Files.Reader; b: BYTE;
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(maxVersion);
		wr.WriteByte(v.type);
		wr.WriteInt(v.unit);
		wr.WriteInt(v.w);
		wr.WriteInt(v.h);
		wr.WriteSChar(v.mode);
		len := v.model.len;
		wr.WriteInt(len);
		IF v.model.data # NIL THEN
			wr.rider.WriteBytes(v.model.data^, 0, len)
		ELSIF v.model.file # NIL THEN
			r := v.model.file.NewReader(NIL); r.SetPos(v.model.pos);
			WHILE len # 0 DO r.ReadByte(b); wr.WriteSChar(SHORT(CHR(b))); DEC(len) END
		ELSE
			ASSERT(v.model.ref # 0, 100);
			NEW(v.model.data, len);
			res := WinApi.GetMetaFileBitsEx(v.model.ref, len, v.model.data);
			wr.rider.WriteBytes(v.model.data^, 0, len)
		END
	END Externalize;

	PROCEDURE (v: StdView) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL
	END ThisModel;
	
	PROCEDURE (v: StdView) CopyFromSimpleView (source: Views.View);
	BEGIN
		(* v.CopyFrom^(source); *)
		WITH source: StdView DO
			v.model := source.model;
			v.type := source.type;
			v.mode := source.mode;
			v.w := source.w;
			v.h := source.h;
			v.unit := source.unit
		END
	END CopyFromSimpleView;

	PROCEDURE (v: StdView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR rd: HostPorts.Rider; res, u, w, h: INTEGER;
	BEGIN
		ASSERT(v.model # NIL, 20);
		v.context.GetSize(w, h);
		IF v.type = winPict THEN
			IF v.model.ref = 0 THEN Evaluate(v) END;
			f.rider(HostPorts.Rider).DrawMetafile(v.model.ref, ORD(v.mode), f.gx, f.gy, w, h)
		ELSIF v.type = macPict THEN
			DrawMacPicture(v, f, w, h)
		END
	END Restore;

	PROCEDURE (v: StdView) GetBkgndFor (
		f: Views.Frame; VAR col: Ports.Color; VAR homogeneous: BOOLEAN
	), NEW;
	BEGIN
		col := Ports.background
	END GetBkgndFor;

	PROCEDURE (v: StdView) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w > Views.undefined) & (msg.h > Views.undefined) THEN
				IF (v.type = macPict) & (v.w # 0) & (v.h # 0) OR (v.mode = 7X) THEN	(* isotropic mode *)
					Properties.ProportionalConstraint(ABS(v.w), ABS(v.h), msg.fixedW, msg.fixedH, msg.w, msg.h)
				END
			ELSE
				IF (v.w > 0) & (v.h > 0) THEN	(* default sizes *)
					msg.w := ABS(v.w); msg.h := ABS(v.h)
				END
			END
		ELSE
		END
	END HandlePropMsg;

(*
	PROCEDURE (v: StdView) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;
																		VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			IF v.type = macPict THEN Dump(v) END
		ELSE
		END
	END HandleCtrlMsg;
*)

	PROCEDURE GenMetafileMedium (
		mf: WinApi.HMETAFILEPICT; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM
	);
	BEGIN
		sm.tymed := WinOle.TYMED_MFPICT;
		sm.u.hMetaFilePict := mf;
		sm.pUnkForRelease := unk
	END GenMetafileMedium;
	
	PROCEDURE MediumMetafile (VAR sm: WinOle.STGMEDIUM): WinApi.HMETAFILEPICT;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_MFPICT, 20);
		RETURN sm.u.hMetaFilePict
	END MediumMetafile;
	
	
	PROCEDURE ImportDPict* (
		VAR med: WinOle.STGMEDIUM; OUT v: Views.View; OUT w, h: INTEGER; OUT isSingle: BOOLEAN
	);
		VAR sv: StdView; res, len, sw, sh, u, mode: INTEGER; mf: WinApi.HMETAFILE;
			hm: WinApi.HMETAFILEPICT; mp: WinApi.PtrMETAFILEPICT;
	BEGIN
		hm := MediumMetafile(med);
		ASSERT(hm # 0, 20);
		mp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(hm));
		NEW(sv); sv.type := winPict;
		mode := mp.mm;
		CASE mode OF
		| 1: u := HostWindows.unit
		| 2: u := Ports.point DIV 20
		| 3: u := Ports.mm DIV 100
		| 4: u := Ports.inch DIV 100
		| 5: u := Ports.inch DIV 1000
		| 6: u := Ports.mm DIV 10
		| 7: u := Ports.mm DIV 100
		| 8: u := Ports.mm DIV 100
		END;
		sw := mp.xExt;
		sh := mp.yExt;
		mf := mp.hMF;
		sv.w := sw * u; sv.h := sh * u; sv.unit := u;
		sv.mode := SHORT(CHR(mode));
		NEW(sv.model);
		len := WinApi.GetMetaFileBitsEx(mf, 0, NIL);
		NEW(sv.model.data, len);
		res := WinApi.GetMetaFileBitsEx(mf, len, sv.model.data^);
		sv.model.len := len;
		res := WinApi.GlobalUnlock(hm);
		v := sv; w := ABS(sv.w); h := ABS(sv.h); isSingle := FALSE
	END ImportDPict;

	PROCEDURE ExportDPict* (
		v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
	);
		VAR res, u: INTEGER; mf: WinApi.HMETAFILE;
			hm: WinApi.HMETAFILEPICT; mp: WinApi.PtrMETAFILEPICT;
	BEGIN
		ASSERT(v # NIL, 20);
		WITH v: StdView DO
			IF v.type = winPict THEN
				IF v.model.ref = 0 THEN Evaluate(v) END;
				hm := WinApi.GlobalAlloc({1, 13}, SIZE(WinApi.METAFILEPICT));	(* movable, sharable *)
				IF hm # 0 THEN
					mp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(hm));
					CASE ORD(v.mode) OF
					| 1: u := HostWindows.unit
					| 2: u := Ports.point DIV 20
					| 3: u := Ports.mm DIV 100
					| 4: u := Ports.inch DIV 100
					| 5: u := Ports.inch DIV 1000
					| 6: u := Ports.mm DIV 10
					| 7: u := Ports.mm DIV 100
					| 8: u := Ports.mm DIV 100
					END;
					mp.mm := ORD(v.mode);
					mp.xExt := v.w DIV u;
					mp.yExt := v.h DIV u;
					mp.hMF := WinApi.SetMetaFileBitsEx(v.model.len, v.model.data^);
					res := WinApi.GlobalUnlock(hm);
					GenMetafileMedium(hm, NIL, med)
				END
			END
		ELSE
		END
	END ExportDPict;

(*
	PROCEDURE ImportCBPict*;
		VAR v: StdView; res, len, adr, w, h, u, mode: LONGINT; ref, hnd: WinApi.Handle;
	BEGIN
		hnd := WinApi.GetClipboardData(HostClipboard.metafile);
		ASSERT(hnd # 0, 20);
		adr := WinApi.GlobalLock(hnd);
		NEW(v); v.type := winPict;
		SYSTEM.GET(adr, mode); INC(adr, 4);
		SYSTEM.GET(adr, w); INC(adr, 4);
		SYSTEM.GET(adr, h); INC(adr, 4);
		SYSTEM.GET(adr, ref);
		CASE mode OF
		| 1: u := HostWindows.unit
		| 2: u := Ports.point DIV 20
		| 3: u := Ports.mm DIV 100
		| 4: u := Ports.inch DIV 100
		| 5: u := Ports.inch DIV 1000
		| 6: u := Ports.mm DIV 10
		| 7: u := Ports.mm DIV 100
		| 8: u := Ports.mm DIV 100
		END;
		v.w := w * u; v.h := h * u;
		v.mode := CHR(mode);
		NEW(v.model);
		len := WinApi.GetMetaFileBitsEx(ref, 0, NIL);
		NEW(v.model.data, len);
		res := WinApi.GetMetaFileBitsEx(ref, len, v.model.data^);
		v.model.len := len;
		res := WinApi.GlobalUnlock(hnd);
		HostClipboard.RegisterImp(v)
	END ImportCBPict;

	PROCEDURE ImportC*;
		VAR v: StdView; pic: MemoryMgr.Handle; s, d, len: LONGINT; ch: CHAR;
	BEGIN
		len := HostClipboard.len;
		pic := MemoryMgr.NewHandle(len); MemoryMgr.HLock(pic);
		s := HostClipboard.adr; d := SYSTEM.ADR(pic^);
		WHILE len # 0 DO
			SYSTEM.GET(s, ch); SYSTEM.PUT(d, ch); INC(s); INC(d); DEC(len)
		END;
		NEW(v); v.Init;
		v.model := NewModel(SYSTEM.VAL(QuickDraw.PicHandle, pic));
		HostClipboard.view := v
	END ImportC;

	PROCEDURE ExportC*;
		VAR v: StdView; s, d, len: LONGINT; scrap: MemoryMgr.Ptr; ch: CHAR;
	BEGIN
		IF HostClipboard.view IS StdView THEN
			v := HostClipboard.view(StdView);
			ASSERT(v.model.picture # NIL, 20);
			len := v.model.len;
			scrap := MemoryMgr.NewPtr(len);
			IF MemoryMgr.err = 0 THEN
				HostClipboard.adr := SYSTEM.VAL(LONGINT, scrap);
				HostClipboard.len := len;
				s := SYSTEM.ADR(v.model.picture^); d := HostClipboard.adr;
				WHILE len # 0 DO
					SYSTEM.GET(s, ch); SYSTEM.PUT(d, ch); INC(s); INC(d); DEC(len)
				END
			END
		END
	END ExportC;

	PROCEDURE ImportF*;
		VAR v: StdView; pic: MemoryMgr.Handle; r: Files.Reader; d, len: LONGINT; ch: CHAR;
	BEGIN
		ASSERT(Converters.file # NIL, 20);
		len := Converters.file.Length() - 512;
		pic := MemoryMgr.NewHandle(len); ASSERT(pic # NIL, 21);
		MemoryMgr.HLock(pic);
		d := SYSTEM.ADR(pic^);
		r := Converters.file.NewReader(NIL); r.SetPos(512);
		WHILE len # 0 DO
			r.ReadByte(ch); SYSTEM.PUT(d, ch); INC(d); DEC(len)
		END;
		NEW(v); v.Init;
		v.model := NewModel(SYSTEM.VAL(QuickDraw.PicHandle, pic));
		Converters.store := v
	END ImportF;
*)

	PROCEDURE PictType* (v: Views.View): INTEGER;
	BEGIN
		WITH v: StdView DO RETURN v.type
		ELSE RETURN -1
		END
	END PictType;
	
END HostPictures.
