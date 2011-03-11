MODULE DevDebug;
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

	IMPORT SYSTEM,
		Kernel, Strings, Dates, Files, Fonts, Services, Ports, Stores, Converters,
		Models, Views, Controllers, Properties, Dialog, Containers, Controls,
		HostFonts, Windows, HostFiles, StdDialog, StdFolds, StdLinks,
		TextModels, TextMappers, TextControllers, TextViews, TextRulers, StdLog,
		DevCommanders;
	
	CONST
		mm = Ports.mm; pt = Ports.point;
		mProc = 4;
		refViewSize = 9 * Ports.point;
		
		heap = 1; source = 2; module = 3; modules = 4;	(* RefView types *)
		open = 1; undo = 2; update = 3;	(* RefView commands *)
		
		(* additional scanner types *)
		import = 100; smodule = 101; semicolon = 102; becomes  = 103; stop = 104; comEnd = 105; 

	TYPE
		Name = Kernel.Name;
		ArrayPtr = POINTER TO EXTENSIBLE RECORD
			last, t, first: INTEGER;	(* gc header *)
			len: ARRAY 16 OF INTEGER	(* dynamic array length table *)
		END;
		RefView = POINTER TO RECORD (Views.View)
			type: SHORTINT;
			command: SHORTINT;
			back: RefView;
			adr: INTEGER;
			desc: Kernel.Type;
			ptr: ArrayPtr;
			name: Name
		END;
		
		Action = POINTER TO RECORD (Services.Action)
			text: TextModels.Model
		END;
		
		Cluster = POINTER TO RECORD [untagged]	(* must correspond to Kernel.Cluster *)
			size: INTEGER;
			next: Cluster
		END;
		
	
	VAR
		out: TextMappers.Formatter;
		loadErrors: ARRAY 10, 64 OF SHORTCHAR;
		path: ARRAY 4 OF Ports.Point;
		empty: Name;


	PROCEDURE NewRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR r: TextRulers.Ruler;
	BEGIN
		r := TextRulers.dir.New(NIL);
		TextRulers.SetRight(r, 140 * mm);
		TextRulers.AddTab(r, 4 * mm); TextRulers.AddTab(r, 34 * mm); TextRulers.AddTab(r, 80 * mm);
		RETURN r
	END NewRuler;

	PROCEDURE NewModRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR r: TextRulers.Ruler;
	BEGIN
		r := TextRulers.dir.New(NIL);
		IF Dialog.platform DIV 10 = 2 THEN	(* mac *)
			TextRulers.SetRight(r, 154 * mm);
			TextRulers.AddTab(r, 48 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 64 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 76 * mm); TextRulers.AddTab(r, 115 * mm)
		ELSE
			TextRulers.SetRight(r, 144 * mm);
			TextRulers.AddTab(r, 48 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 64 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 76 * mm); TextRulers.AddTab(r, 110 * mm)
		END;
		RETURN r
	END NewModRuler;

	PROCEDURE OpenViewer (t: TextModels.Model; title: Views.Title; ruler:TextRulers.Ruler);
		VAR v: TextViews.View; c: Containers.Controller;
	BEGIN
		Dialog.MapString(title, title);
		v := TextViews.dir.New(t);
		v.SetDefaults(ruler, TextViews.dir.defAttr);
		c := v.ThisController();
		IF c # NIL THEN
			c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
		END;
		Views.OpenAux(v, title)
	END OpenViewer;
	
	PROCEDURE OpenFold (hidden: ARRAY OF CHAR);
		VAR fold: StdFolds.Fold; t: TextModels.Model; w: TextMappers.Formatter;
	BEGIN
		Dialog.MapString(hidden, hidden);
		t := TextModels.CloneOf(StdLog.buf);
		w.ConnectTo(t); w.WriteString(hidden);
		fold := StdFolds.dir.New(StdFolds.expanded, "", t);
		out.WriteView(fold)
	END OpenFold;
	
	PROCEDURE CloseFold (collaps: BOOLEAN);
		VAR fold: StdFolds.Fold; m: TextModels.Model;
	BEGIN
		fold := StdFolds.dir.New(StdFolds.expanded, "", NIL);
		out.WriteView(fold);
		IF collaps THEN fold.Flip(); m := out.rider.Base(); out.SetPos(m.Length()) END
	END CloseFold;
	
	PROCEDURE WriteHex (n: INTEGER);
	BEGIN
		out.WriteIntForm(n, TextMappers.hexadecimal, 9, "0", TextMappers.showBase)
	END WriteHex;
	
	PROCEDURE WriteString (adr, len, base: INTEGER; zterm, unicode: BOOLEAN);
		CONST beg = 0; char = 1; code = 2;
		VAR ch: CHAR; sc: SHORTCHAR; val, mode: INTEGER; str: ARRAY 16 OF CHAR;
	BEGIN
		mode := beg;
		IF base = 2 THEN SYSTEM.GET(adr, ch); val := ORD(ch) ELSE SYSTEM.GET(adr, sc); val := ORD(sc) END;
		IF zterm & (val = 0) THEN out.WriteSString('""')
		ELSE
			REPEAT
				IF (val >= ORD(" ")) & (val < 7FH) OR (val > 0A0H) & (val < 100H) OR unicode & (val >= 100H) THEN
					IF mode # char THEN
						IF mode = code THEN out.WriteSString(", ") END;
						out.WriteChar(22X); mode := char
					END;
					out.WriteChar(CHR(val))
				ELSE
					IF mode = char THEN out.WriteChar(22X) END;
					IF mode # beg THEN out.WriteSString(", ") END;
					mode := code; Strings.IntToStringForm(val, Strings.hexadecimal, 1, "0", FALSE, str);
					IF str[0] > "9" THEN out.WriteChar("0") END;
					out.WriteString(str); out.WriteChar("X")
				END;
				INC(adr, base); DEC(len);
				IF base = 2 THEN SYSTEM.GET(adr, ch); val := ORD(ch) ELSE SYSTEM.GET(adr, sc); val := ORD(sc) END
			UNTIL (len = 0) OR zterm & (val = 0)
		END;
		IF mode = char THEN out.WriteChar(22X) END
	END WriteString;
	
	PROCEDURE IsIdent (s: ARRAY OF CHAR): BOOLEAN;
		VAR i: SHORTINT; ch: CHAR;
	BEGIN
		ch := s[0];
		IF ("A" <= CAP(ch)) & (CAP(ch) <= "Z") OR (ch = "_") OR (ch >= 0C0X) & (ch < 100X) & (ch # "×") & (ch # "÷") THEN
			i := 1; ch := s[1];
			WHILE ("A" <= CAP(ch)) & (CAP(ch) <= "Z") OR ("0" <= ch) & (ch <= "9")
					OR (ch = "_") OR (ch >= 0C0X) & (ch < 100X) & (ch # "×") & (ch # "÷") DO
				INC(i); ch := s[i]
			END;
			RETURN (s[i] = 0X) & (i < 256)
		ELSE RETURN FALSE
		END
	END IsIdent;
	
	PROCEDURE OutString (s: ARRAY OF CHAR);
		VAR str: Dialog.String;
	BEGIN
		Dialog.MapString(s, str);
		out.WriteString(str);
	END OutString;

	(* -------------------  variable display ------------------- *)
	
	PROCEDURE FormOf (t: Kernel.Type): SHORTCHAR;
	BEGIN
		IF SYSTEM.VAL(INTEGER, t) DIV 256 = 0 THEN
			RETURN SHORT(CHR(SYSTEM.VAL(INTEGER, t)))
		ELSE
			RETURN SHORT(CHR(16 + t.id MOD 4))
		END
	END FormOf;
	
	PROCEDURE LenOf (t: Kernel.Type; ptr: ArrayPtr): INTEGER;
	BEGIN
		IF t.size # 0 THEN RETURN t.size
		ELSIF ptr # NIL THEN RETURN ptr.len[t.id DIV 16 MOD 16 - 1]
		ELSE RETURN 0
		END
	END LenOf;
	
	PROCEDURE SizeOf (t: Kernel.Type; ptr: ArrayPtr): INTEGER;
	BEGIN
		CASE FormOf(t) OF
		| 0BX: RETURN 0
		| 1X, 2X, 4X: RETURN 1
		| 3X, 5X: RETURN 2
		| 8X, 0AX: RETURN 8
		| 11X: RETURN t.size
		| 12X: RETURN LenOf(t, ptr) * SizeOf(t.base[0], ptr)
		ELSE RETURN 4
		END
	END SizeOf;

	PROCEDURE WriteName (t: Kernel.Type; ptr: ArrayPtr);
		VAR name: Kernel.Name; f: SHORTCHAR;
	BEGIN
		f := FormOf(t);
		CASE f OF
		| 0X: OutString("#Dev:Unknown")
		| 1X: out.WriteSString("BOOLEAN")
		| 2X: out.WriteSString("SHORTCHAR")
		| 3X: out.WriteSString("CHAR")
		| 4X: out.WriteSString("BYTE")
		| 5X: out.WriteSString("SHORTINT")
		| 6X: out.WriteSString("INTEGER")
		| 7X: out.WriteSString("SHORTREAL")
		| 8X: out.WriteSString("REAL")
		| 9X: out.WriteSString("SET")
		| 0AX: out.WriteSString("LONGINT")
		| 0BX: out.WriteSString("ANYREC")
		| 0CX: out.WriteSString("ANYPTR")
		| 0DX: out.WriteSString("POINTER")
		| 0EX: out.WriteSString("PROCEDURE")
		| 0FX: out.WriteSString("STRING")
		| 10X..13X:
			IF (t.mod # NIL) & (t.id DIV 256 # 0) & (t.mod.refcnt >= 0) THEN
				Kernel.GetTypeName(t, name);
				IF name = "!" THEN
					IF f = 11X THEN out.WriteSString("RECORD")
					ELSIF f = 12X THEN out.WriteSString("ARRAY")
					ELSE OutString("#Dev:Unknown")
					END
				ELSE
					out.WriteSString(t.mod.name); out.WriteChar("."); out.WriteSString(name)
				END
			ELSIF f = 11X THEN
				IF t.mod # NIL THEN out.WriteSString(t.mod.name); out.WriteChar(".") END;
				out.WriteSString("RECORD"); 
			ELSIF f = 12X THEN
				out.WriteSString("ARRAY "); out.WriteInt(LenOf(t, ptr)); t := t.base[0];
				WHILE (FormOf(t) = 12X) & ((t.id DIV 256 = 0) OR (t.mod = NIL) OR (t.mod.refcnt < 0)) DO
					out.WriteSString(", "); out.WriteInt(LenOf(t, ptr)); t := t.base[0]
				END;
				out.WriteSString(" OF "); WriteName(t, ptr)
			ELSIF f = 13X THEN
				out.WriteSString("POINTER")
			ELSE
				out.WriteSString("PROCEDURE")
			END
		| 20X: out.WriteSString("COM.IUnknown")
		| 21X: out.WriteSString("COM.GUID")
		| 22X: out.WriteSString("COM.RESULT")
		ELSE OutString("#Dev:UnknownFormat"); out.WriteInt(ORD(f))
		END
	END WriteName;
	
	PROCEDURE WriteGuid (a: INTEGER);
	
		PROCEDURE Hex (a: INTEGER);
			VAR x: SHORTCHAR;
		BEGIN
			SYSTEM.GET(a, x);
			out.WriteIntForm(ORD(x), TextMappers.hexadecimal, 2, "0", FALSE)
		END Hex;
		
	BEGIN
		out.WriteChar("{");
		Hex(a + 3); Hex(a + 2); Hex(a + 1); Hex(a);
		out.WriteChar("-");
		Hex(a + 5); Hex(a + 4);
		out.WriteChar("-");
		Hex(a + 7); Hex(a + 6);
		out.WriteChar("-");
		Hex(a + 8);
		Hex(a + 9);
		out.WriteChar("-");
		Hex(a + 10);
		Hex(a + 11);
		Hex(a + 12);
		Hex(a + 13);
		Hex(a + 14);
		Hex(a + 15);
		out.WriteChar("}")
	END WriteGuid;
	
	PROCEDURE^ ShowVar (
		ad, ind: INTEGER; f, c: SHORTCHAR; desc: Kernel.Type; ptr: ArrayPtr; back: RefView; VAR name, sel: Name);
	
	PROCEDURE^ NewRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Kernel.Type; ptr: ArrayPtr; name: Name): RefView;

	PROCEDURE^ InsertRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Kernel.Type; ptr: ArrayPtr; name: Name);

	PROCEDURE ShowRecord (a, ind: INTEGER; desc: Kernel.Type; back: RefView; VAR sel: Name);
		VAR dir: Kernel.Directory; obj: Kernel.Object; name: Kernel.Name; i, j, n: INTEGER; base: Kernel.Type;
	BEGIN
		WriteName(desc, NIL); out.WriteTab;
		IF desc.mod.refcnt >= 0 THEN
			OpenFold("#Dev:Fields");
			n := desc.id DIV 16 MOD 16; j := 0;
			WHILE j <= n DO
				base := desc.base[j];
				IF base # NIL THEN
					dir := base.fields; i := 0;
					WHILE i < dir.num DO
						obj := SYSTEM.VAL(Kernel.Object, SYSTEM.ADR(dir.obj[i]));
						Kernel.GetObjName(base.mod, obj, name);
						ShowVar(a + obj.offs, ind, FormOf(obj.struct), 1X, obj.struct, NIL, back, name, sel);
						INC(i)
					END
				END;
				INC(j)
			END;
			out.WriteSString("   "); CloseFold((ind > 1) OR (sel # ""))
		ELSE
			OutString("#Dev:Unloaded")
		END
	END ShowRecord;
	
	PROCEDURE ShowArray (a, ind: INTEGER; desc: Kernel.Type; ptr: ArrayPtr; back: RefView; VAR sel: Name);
		VAR f: SHORTCHAR; i, n, m, size, len: INTEGER; name: Kernel.Name; eltyp, t: Kernel.Type;
			vi: SHORTINT; vs: BYTE; str: Dialog.String; high: BOOLEAN;
	BEGIN
		WriteName(desc, ptr); out.WriteTab;
		len := LenOf(desc, ptr); eltyp := desc.base[0]; f := FormOf(eltyp); size := SizeOf(eltyp, ptr);
		IF (f = 2X) OR (f = 3X) THEN	(* string *)
			n := 0; m := len; high := FALSE;
			IF f = 2X THEN
				REPEAT SYSTEM.GET(a + n, vs); INC(n) UNTIL (n = 32) OR (n = len) OR (vs = 0);
				REPEAT DEC(m); SYSTEM.GET(a + m, vs) UNTIL (m = 0) OR (vs # 0)
			ELSE
				REPEAT
					SYSTEM.GET(a + n * 2, vi); INC(n);
					IF vi DIV 256 # 0 THEN high := TRUE END
				UNTIL (n = len) OR (vi = 0);
				n := MIN(n, 32);
				REPEAT DEC(m); SYSTEM.GET(a + m * 2, vi) UNTIL (m = 0) OR (vi # 0)
			END;
			WriteString(a, n, size, TRUE, TRUE);
			INC(m, 2);
			IF m > len THEN m := len END;
			IF high OR (m > n) THEN
				out.WriteSString("   "); OpenFold("...");
				out.WriteLn;
				IF high & (n = 32) THEN
					WriteString(a, m, size, TRUE, TRUE);
					out.WriteLn; out.WriteLn
				END;
				WriteString(a, m, size, FALSE, FALSE);
				IF m < len THEN out.WriteSString(", ..., 0X") END;
				out.WriteSString("   "); CloseFold(TRUE)
			END
		ELSE
			t := eltyp;
			WHILE FormOf(t) = 12X DO t := t.base[0] END;
			IF FormOf(t) # 0X THEN
				OpenFold("#Dev:Elements");
				i := 0;
				WHILE i < len DO
					Strings.IntToString(i, str);
					name := "[" + SHORT(str$) + "]";
					ShowVar(a, ind, f, 1X, eltyp, ptr, back, name, sel);
					INC(i); INC(a, size)
				END;
				out.WriteSString("   "); CloseFold(TRUE)
			END
		END
	END ShowArray;
	
	PROCEDURE ShowProcVar (a: INTEGER);
		VAR vli, n, ref: INTEGER; m: Kernel.Module; name: Kernel.Name;
	BEGIN
		SYSTEM.GET(a, vli);
		Kernel.SearchProcVar(vli, m, vli);
		IF m = NIL THEN
			IF vli = 0 THEN out.WriteSString("NIL")
			ELSE WriteHex(vli)
			END
		ELSE
			IF m.refcnt >= 0 THEN
				out.WriteSString(m.name); ref := m.refs;
				REPEAT Kernel.GetRefProc(ref, n, name) UNTIL (n = 0) OR (vli < n);
				IF vli < n THEN out.WriteChar("."); out.WriteSString(name) END
			ELSE
				OutString("#Dev:ProcInUnloadedMod");
				out.WriteSString(m.name); out.WriteSString(" !!!")
			END
		END
	END ShowProcVar;

	PROCEDURE ShowPointer (a: INTEGER; f: SHORTCHAR; desc: Kernel.Type; back: RefView; VAR sel: Name);
		VAR adr, x: INTEGER; ptr: ArrayPtr; c: Cluster; btyp: Kernel.Type;
	BEGIN
		SYSTEM.GET(a, adr);
		IF f = 13X THEN btyp := desc.base[0] ELSE btyp := NIL END;
		IF adr = 0 THEN out.WriteSString("NIL")
		ELSIF f = 20X THEN
			out.WriteChar("["); WriteHex(adr); out.WriteChar("]");
			out.WriteChar(" "); c := SYSTEM.VAL(Cluster, Kernel.Root());
			WHILE (c # NIL) & ((adr < SYSTEM.VAL(INTEGER, c)) OR (adr >= SYSTEM.VAL(INTEGER, c) + c.size)) DO c := c.next END;
			IF c # NIL THEN
				ptr := SYSTEM.VAL(ArrayPtr, adr);
				InsertRefView(heap, open, adr, back, btyp, ptr, sel)
			END
		ELSE
			IF (f = 13X) OR (f = 0CX) THEN x := adr - 4 ELSE x := adr END;
			IF ((adr < -4) OR (adr >= 65536)) & Kernel.IsReadable(x, adr + 16) THEN
				out.WriteChar("["); WriteHex(adr); out.WriteChar("]");
				IF (f = 13X) OR (f = 0CX) THEN
					out.WriteChar(" "); c := SYSTEM.VAL(Cluster, Kernel.Root());
					WHILE (c # NIL) & ((adr < SYSTEM.VAL(INTEGER, c)) OR (adr >= SYSTEM.VAL(INTEGER, c) + c.size)) DO
						c := c.next
					END;
					IF c # NIL THEN
						ptr := SYSTEM.VAL(ArrayPtr, adr);
						IF (f = 13X) & (FormOf(btyp) = 12X) THEN	(* array *)
							adr := SYSTEM.ADR(ptr.len[btyp.id DIV 16 MOD 16]) 
						END;
						InsertRefView(heap, open, adr, back, btyp, ptr, sel)
					ELSE OutString("#Dev:IllegalPointer");
					END
				END
			ELSE OutString("#Dev:IllegalAddress"); WriteHex(adr)
			END
		END
	END ShowPointer;
	
	PROCEDURE ShowSelector (ref: RefView);
		VAR b: RefView; n: SHORTINT; a, a0: TextModels.Attributes;
	BEGIN
		b := ref.back; n := 1;
		IF b # NIL THEN
			WHILE (b.name = ref.name) & (b.back # NIL) DO INC(n); b := b.back END;
			ShowSelector(b);
			IF n > 1 THEN out.WriteChar("(") END;
			out.WriteChar(".")
		END;
		out.WriteSString(ref.name);
		IF ref.type = heap THEN out.WriteChar("^") END;
		IF n > 1 THEN
			out.WriteChar(")");
			a0 := out.rider.attr; a := TextModels.NewOffset(a0, 2 * Ports.point);
			out.rider.SetAttr(a);
			out.WriteInt(n); out.rider.SetAttr(a0)
		END;
	END ShowSelector;
	
	PROCEDURE ShowVar (
		ad, ind: INTEGER; f, c: SHORTCHAR; desc: Kernel.Type; ptr: ArrayPtr; back: RefView; VAR name, sel: Name
	);
		VAR i, j, vli, a, ref: INTEGER; tsel: Name; a0: TextModels.Attributes;
			vc: SHORTCHAR; vsi: BYTE; vi: SHORTINT; vr: SHORTREAL; vlr: REAL; vs: SET;
	BEGIN
		out.WriteLn; out.WriteTab; i := 0;
		WHILE i < ind DO out.WriteSString("  "); INC(i) END;
		a := ad; i := 0; j := 0;
		IF sel # "" THEN
			WHILE sel[i] # 0X DO tsel[i] := sel[i]; INC(i) END;
			IF (tsel[i-1] # ":") & (name[0] # "[") THEN tsel[i] := "."; INC(i) END
		END;
		WHILE name[j] # 0X DO tsel[i] := name[j]; INC(i); INC(j) END;
		tsel[i] := 0X;
		a0 := out.rider.attr;
		IF c = 3X THEN	(* varpar *)
			SYSTEM.GET(ad, a);
			out.rider.SetAttr(TextModels.NewStyle(a0, {Fonts.italic}))
		END;
		IF name[0] # "[" THEN out.WriteChar(".") END;
		out.WriteSString(name);
		out.rider.SetAttr(a0); out.WriteTab;
		IF (c = 3X) & (a >= 0) & (a < 65536) THEN 
			out.WriteTab; out.WriteSString("NIL VARPAR");
		ELSIF f = 11X THEN
			Kernel.GetTypeName(desc, name);
			IF (c = 3X) & (name[0] # "!") THEN SYSTEM.GET(ad + 4, desc) END;	(* dynamic type *)
			ShowRecord(a, ind + 1, desc, back, tsel)
		ELSIF (c = 3X) & (f = 0BX) THEN	(* VAR anyrecord *)
			SYSTEM.GET(ad + 4, desc);
			ShowRecord(a, ind + 1, desc, back, tsel)
		ELSIF f = 12X THEN
			IF (desc.size = 0) & (ptr = NIL) THEN SYSTEM.GET(ad, a) END;	(* dyn array val par *)
			IF ptr = NIL THEN ptr := SYSTEM.VAL(ArrayPtr, ad - 8) END;
			ShowArray(a, ind + 1, desc, ptr, back, tsel)
		ELSE
			IF desc = NIL THEN desc := SYSTEM.VAL(Kernel.Type, ORD(f)) END;
			WriteName(desc, NIL); out.WriteTab;
			CASE f OF
			| 0X: (* SYSTEM.GET(a, vli); WriteHex(vli) *)
			| 1X: SYSTEM.GET(a, vc); 
				IF vc = 0X THEN out.WriteSString("FALSE")
				ELSIF vc = 1X THEN out.WriteSString("TRUE")
				ELSE OutString("#Dev:Undefined"); out.WriteInt(ORD(vc))
				END
			| 2X: WriteString(a, 1, 1, FALSE, FALSE)
			| 3X: WriteString(a, 1, 2, FALSE, TRUE);
					SYSTEM.GET(a, vi);
					IF vi DIV 256 # 0 THEN out.WriteString("  "); WriteString(a, 1, 2, FALSE, FALSE) END
			| 4X: SYSTEM.GET(a, vsi); out.WriteInt(vsi)
			| 5X: SYSTEM.GET(a, vi); out.WriteInt(vi)
			| 6X: SYSTEM.GET(a, vli); out.WriteInt(vli)
			| 7X: SYSTEM.GET(a, vli);
					IF BITS(vli) * {23..30} = {23..30} THEN
						IF BITS(vli) = {23..30} THEN out.WriteString("inf")
						ELSIF BITS(vli) = {23..31} THEN out.WriteString("-inf")
						ELSE out.WriteString("nan  ("); WriteHex(vli); out.WriteString(")")
						END
					ELSE
						SYSTEM.GET(a, vr); out.WriteReal(vr)
					END
			| 8X: IF Kernel.littleEndian THEN SYSTEM.GET(a, vli); SYSTEM.GET(a + 4, i)
					ELSE SYSTEM.GET(a + 4, vli); SYSTEM.GET(a, i)
					END;
					IF BITS(i) * {20..30} = {20..30} THEN
						IF (BITS(i) = {20..30}) & (vli = 0) THEN out.WriteString("inf")
						ELSIF (BITS(i) = {20..31}) & (vli = 0) THEN out.WriteString("-inf")
						ELSE out.WriteString("nan  ("); out.WriteIntForm(i, TextMappers.hexadecimal, 8, "0", TextMappers.hideBase);
							WriteHex(vli); out.WriteString(")")
						END
					ELSE
						SYSTEM.GET(a, vlr); out.WriteReal(vlr)
					END
			| 9X: SYSTEM.GET(a, vs); out.WriteSet(vs)
			| 0AX: IF Kernel.littleEndian THEN SYSTEM.GET(a, vli); SYSTEM.GET(a + 4, i)
					ELSE SYSTEM.GET(a + 4, vli); SYSTEM.GET(a, i)
					END;
					IF (vli >= 0) & (i = 0) OR (vli < 0) & (i = -1) THEN out.WriteInt(vli)
					ELSE out.WriteIntForm(i, TextMappers.hexadecimal, 8, "0", TextMappers.hideBase); WriteHex(vli)
					END
			| 0CX, 0DX, 13X, 20X: ShowPointer(a, f, desc, back, tsel)
			| 0EX, 10X: ShowProcVar(a)
			| 0FX: WriteString(a, 256, 1, TRUE, FALSE)
			| 21X: WriteGuid(a)
			| 22X: SYSTEM.GET(a, vli); WriteHex(vli)
			ELSE 
			END
		END
	END ShowVar;
	
	PROCEDURE WriteTimeStamp (ts: ARRAY OF SHORTINT);
		VAR d: Dates.Date; t: Dates.Time; str: ARRAY 64 OF CHAR;
	BEGIN
		IF ts[0] = 0 THEN
			out.WriteSString("      "); OutString("#Dev:Linked")
		ELSE
			d.year := ts[0]; d.month := ts[1]; d.day := ts[2];
			t.hour := ts[3]; t.minute := ts[4]; t.second := ts[5];
			Dates.DateToString(d, Dates.short, str);
			out.WriteString(str); out.WriteString("  ");
			Dates.TimeToString(t, str);
			out.WriteString(str); 
		END
	END WriteTimeStamp;

	PROCEDURE ShowModules;
		VAR m, m1: Kernel.Module; a0: TextModels.Attributes; n, h, t, h1: ARRAY 256 OF CHAR;
	BEGIN	
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewStyle(a0, {Fonts.italic}));
		OutString("#Dev:ModuleName"); out.WriteTab;
		OutString("#Dev:BytesUsed"); out.WriteTab;
		OutString("#Dev:Clients"); out.WriteTab;
		OutString("#Dev:Compiled"); out.WriteTab;
		OutString("#Dev:Loaded");
		out.rider.SetAttr(a0); out.WriteTab; out.WriteTab;
		out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {Fonts.underline}));
		out.rider.SetAttr(TextModels.NewColor(out.rider.attr, Ports.blue));
		out.WriteView(StdLinks.dir.NewLink("DevDebug.UpdateModules"));
		OutString("#Dev:Update");
		out.WriteView(StdLinks.dir.NewLink(""));
		out.rider.SetAttr(a0); out.WriteLn;
		m := Kernel.modList;
		WHILE m # NIL DO
			IF m.refcnt >= 0 THEN
				n := m.name$; Kernel.SplitName(n, h, t);
				m1 := Kernel.modList; h1 := "*";
				WHILE (m1 # m) & (h1 # h) DO
					IF m1.refcnt >= 0 THEN n := m1.name$; Kernel.SplitName(n, h1, t) END;
					m1 := m1.next
				END;
				IF h1 # h THEN
					out.WriteLn;
					m1 := m;
					WHILE m1 # NIL DO
						n := m1.name$; Kernel.SplitName(n, h1, t);
						IF (h1 = h) & (m1.refcnt >= 0) THEN
							out.WriteSString(m1.name); out.WriteTab;
							out.WriteIntForm(m1.csize + m1.dsize + m1.rsize, 10, 6, TextModels.digitspace, TextMappers.hideBase);
							out.WriteTab;
							out.WriteIntForm(m1.refcnt, 10, 3, TextModels.digitspace, TextMappers.hideBase);
							out.WriteTab;
							WriteTimeStamp(m1.compTime);
							out.WriteTab;
							WriteTimeStamp(m1.loadTime);
							out.WriteLn
						END;
						m1 := m1.next
					END
				END
			END;
			m := m.next
		END
	END ShowModules;
	
	PROCEDURE ShowGlobals (mod: Kernel.Module);
		VAR ref, x: INTEGER; m, f: SHORTCHAR; name: ARRAY 256 OF CHAR; mname: Kernel.Name; 
			d: Kernel.Type; v: RefView; a0: TextModels.Attributes;
	BEGIN
		IF mod # NIL THEN
			out.WriteSString(mod.name);
			out.WriteTab; out.WriteTab; out.WriteTab;
			a0 := out.rider.attr;
			out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {Fonts.underline}));
			out.rider.SetAttr(TextModels.NewColor(out.rider.attr, Ports.blue));
			name := "DevDebug.UpdateGlobals('" + mod.name + "')";
			out.WriteView(StdLinks.dir.NewLink(name));
			OutString("#Dev:Update");
			out.WriteView(StdLinks.dir.NewLink(""));
			out.rider.SetAttr(a0); out.WriteLn;
			ref := mod.refs; Kernel.GetRefProc(ref, x, mname);	(* get body *)
			IF x # 0 THEN
				v := NewRefView (module, open, 0, NIL, NIL, NIL, mod.name);
				Kernel.GetRefVar(ref, m, f, d, x, mname);
				WHILE m = 1X DO
					ShowVar(mod.data + x, 0, f, m, d, NIL, v, mname, empty);
					Kernel.GetRefVar(ref, m, f, d, x, mname)
				END
			END;
			out.WriteLn
		END
	END ShowGlobals;
	
	PROCEDURE ShowObject (adr: INTEGER);
		VAR eltyp: Kernel.Type; ptr: ArrayPtr; desc: ARRAY 64 OF INTEGER; i, n, lev, elsize: INTEGER;
	BEGIN
		SYSTEM.GET(adr - 4, eltyp);
		IF ODD(SYSTEM.VAL(INTEGER, eltyp) DIV 2) THEN
			DEC(SYSTEM.VAL(INTEGER, eltyp), 2);
			ptr := SYSTEM.VAL(ArrayPtr, adr);
			elsize := eltyp.size;
			IF (eltyp.mod.name = "Kernel") & (eltyp.fields.num = 1) THEN
				eltyp := eltyp.fields.obj[0].struct
			END;
			n := (ptr.last - ptr.first) DIV elsize + 1;
			lev := (ptr.first - adr - 12) DIV 4; i := 0;
			WHILE lev > 0 DO	(* dynamic levels *)
				DEC(lev);
				desc[i] := ptr.len[lev]; n := n DIV desc[i]; INC(i);	(* size *)
				desc[i] := 0; INC(i);	(* module *)
				desc[i] := 2; INC(i);	(* id *)
				desc[i] := SYSTEM.ADR(desc[i+1]); INC(i)	(* desc *)
			END;
			IF n > 1 THEN	(* static level *)
				desc[i] := n; INC(i);	(* size *)
				desc[i] := 0; INC(i);	(* module *)
				desc[i] := 2; INC(i);	(* id *)
			ELSE DEC(i)
			END;
			desc[i] := SYSTEM.VAL(INTEGER, eltyp);	(* desc *)
			ShowArray(ptr.first, 1, SYSTEM.VAL(Kernel.Type, SYSTEM.ADR(desc)), ptr, NIL, empty);
			out.WriteLn
		ELSE ShowRecord(adr, 1, eltyp, NIL, empty)
		END;
	END ShowObject;
	
	PROCEDURE ShowPtrDeref (ref: RefView);
		VAR b: RefView;
	BEGIN
		ShowSelector(ref); b := ref.back;
		IF b # NIL THEN
			out.WriteChar(" ");
			InsertRefView(b.type, undo, b.adr, b.back, b.desc, b.ptr, b.name)
		END;
		out.WriteLn; out.WriteLn;
		out.WriteChar("["); WriteHex(ref.adr); out.WriteChar("]"); out.WriteTab;
		IF ref.desc = NIL THEN
			ShowObject(ref.adr)
		ELSIF FormOf(ref.desc) = 12X THEN
			ShowArray(ref.adr, 1, ref.desc, ref.ptr, ref, empty)
		ELSE
			ShowRecord(ref.adr, 1, Kernel.TypeOf(SYSTEM.VAL(ANYPTR, ref.ptr)), ref, empty)
		END;
		out.WriteLn
	END ShowPtrDeref;
	
	PROCEDURE RefCh (VAR ref: INTEGER; VAR ch: SHORTCHAR);
	BEGIN
		SYSTEM.GET(ref, ch); INC(ref)
	END RefCh;
	
	PROCEDURE RefNum (VAR ref: INTEGER; VAR x: INTEGER);
		VAR s, n: INTEGER; ch: SHORTCHAR;
	BEGIN
		s := 0; n := 0; RefCh(ref, ch);
		WHILE ORD(ch) >= 128 DO INC(n, ASH(ORD(ch) - 128, s) ); INC(s, 7); RefCh(ref, ch) END;
		x := n + ASH(ORD(ch) MOD 64 - ORD(ch) DIV 64 * 64, s)
	END RefNum;
	
	PROCEDURE RefName (VAR ref: INTEGER; VAR n: Kernel.Name);
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; RefCh(ref, ch);
		WHILE ch # 0X DO n[i] := ch; INC(i); RefCh(ref, ch) END;
		n[i] := 0X
	END RefName;
	
	PROCEDURE SourcePos* (mod: Kernel.Module; codePos: INTEGER): INTEGER;
		VAR ref, pos, ad, d: INTEGER; ch: SHORTCHAR; name: Kernel.Name;
	BEGIN
		IF mod # NIL THEN	(* mf, 12.02.04 *)
			ref := mod.refs; pos := 0; ad := 0; SYSTEM.GET(ref, ch);
			WHILE ch # 0X DO
				WHILE (ch > 0X) & (ch < 0FCX) DO
					INC(ad, LONG(ORD(ch))); INC(ref); RefNum(ref, d);
					IF ad > codePos THEN RETURN pos END;
					INC(pos, d); SYSTEM.GET(ref, ch) 
				END;
				IF ch = 0FCX THEN
					INC(ref); RefNum(ref, d); RefName(ref, name); SYSTEM.GET(ref, ch);
					IF (d > codePos) & (pos > 0) THEN RETURN pos END
				END;
				WHILE ch >= 0FDX DO	(* skip variables *)
					INC(ref); RefCh(ref, ch);
					IF ch = 10X THEN INC(ref, 4) END;
					RefNum(ref, d); RefName(ref, name); SYSTEM.GET(ref, ch)
				END
			END
		END;
		RETURN -1
	END SourcePos;
	
	PROCEDURE Scan (VAR s: TextMappers.Scanner);
	BEGIN
		s.Scan;
		IF s.type = TextMappers.string THEN
			IF s.string = "IMPORT" THEN s.type := import
			ELSIF s.string = "MODULE" THEN s.type := smodule
			ELSIF s.string = "THEN" THEN s.type := stop
			ELSIF s.string = "OF" THEN s.type := stop
			ELSIF s.string = "DO" THEN s.type := stop
			ELSIF s.string = "END" THEN s.type := stop
			ELSIF s.string = "ELSE" THEN s.type := stop
			ELSIF s.string = "ELSIF" THEN s.type := stop
			ELSIF s.string = "UNTIL" THEN s.type := stop
			ELSIF s.string = "TO" THEN s.type := stop
			ELSIF s.string = "BY" THEN s.type := stop
			END
		ELSIF s.type = TextMappers.char THEN
			IF s.char = ";" THEN s.type := semicolon
			ELSIF s.char = "|" THEN s.type := stop
			ELSIF s.char = ":" THEN
				IF s.rider.char = "=" THEN s.rider.Read; s.type := becomes END
			ELSIF s.char = "(" THEN
				IF s.rider.char = "*" THEN
					s.rider.Read;
					REPEAT Scan(s) UNTIL (s.type = TextMappers.eot) OR (s.type = comEnd);
					Scan(s)
				END
			ELSIF s.char = "*" THEN
				IF s.rider.char = ")" THEN s.rider.Read; s.type := comEnd END
			END
		END	
	END Scan;
	
	PROCEDURE ShowSourcePos (name: Name; adr: INTEGER);
		VAR loc: Files.Locator; fname: Files.Name; v: Views.View; m: Models.Model; conv: Converters.Converter;
			c: Containers.Controller; beg, end, p: INTEGER; s: TextMappers.Scanner; w: Windows.Window;
			n: ARRAY 256 OF CHAR;
	BEGIN
		(* search source by name heuristic *)
		n := name$; StdDialog.GetSubLoc(n, "Mod", loc, fname);
		v := Views.OldView(loc, fname); m := NIL;
		IF v # NIL THEN
			Views.Open(v, loc, fname, NIL);
			m := v.ThisModel();
			IF ~(m IS TextModels.Model) THEN m := NIL END
		END;
		IF m = NIL THEN
			(* search in open windows *)
			w := Windows.dir.First();
			WHILE (w # NIL) & (m = NIL) DO
				v := w.doc.ThisView();
				m := v.ThisModel();
				IF m # NIL THEN
					WITH m: TextModels.Model DO
						s.ConnectTo(m); s.SetPos(0);
						REPEAT
							REPEAT s.Scan UNTIL s.rider.eot OR (s.type = TextMappers.string) & (s.string = "MODULE");
							s.Scan;
						UNTIL s.rider.eot OR (s.type = TextMappers.string) & (s.string = name);
						IF ~s.rider.eot THEN Windows.dir.Select(w, Windows.eager)
						ELSE m := NIL
						END
					ELSE m := NIL
					END
				END;
				w := Windows.dir.Next(w)
			END
		END;
		IF m = NIL THEN
			(* ask user for source file *)
			conv := NIL; v := Views.Old(Views.ask, loc, fname, conv);
			IF v # NIL THEN
				Views.Open(v, loc, fname, conv);
				m := v.ThisModel();
				IF ~(m IS TextModels.Model) THEN m := NIL END
			END
		END;
		IF m # NIL THEN
			(* mark error position in text *)
			WITH m: TextModels.Model DO
				beg := SourcePos(Kernel.ThisMod(n), adr);
				IF beg >= 0 THEN
					IF beg > m.Length() THEN beg := m.Length() - 10 END;
					s.ConnectTo(m); s.SetPos(beg);
					Scan(s); beg := s.start; end := beg + 3;
					IF s.type = stop THEN end := s.Pos() - 1
					ELSE
						WHILE (s.type # TextMappers.eot) & (s.type # stop) & (s.type # semicolon) DO
							end := s.Pos() - 1; Scan(s)
						END
					END;
					c := v(TextViews.View).ThisController();
					v(TextViews.View).ShowRange(beg, end, TextViews.any);
					c(TextControllers.Controller).SetSelection(beg, end)
				END
			END
		ELSE Dialog.ShowParamMsg("#Dev:SourcefileNotFound", n, "", "")
		END
	END ShowSourcePos;
			
	(* -------------------  RefView ------------------- *)
	
	PROCEDURE (v: RefView) Internalize (VAR rd: Stores.Reader);
		VAR s: Stores.Store; thisVersion: INTEGER;
	BEGIN
		v.Internalize^(rd); IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(0, 0, thisVersion); IF rd.cancelled THEN RETURN END;
		v.command := open;
		rd.ReadSInt(v.type);
		IF v.type = source THEN
			rd.ReadInt(v.adr);
			rd.ReadSString(v.name)
		ELSIF v.type = module THEN
			rd.ReadSString(v.name)
		ELSIF v.type # modules THEN
			v.type := 0
		END
	END Internalize;

	PROCEDURE (v: RefView) Externalize (VAR wr: Stores.Writer);
		VAR t: SHORTINT;
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(0);
		t := v.type;
		IF v.command # open THEN t := 0 END;
		wr.WriteSInt(t);
		IF t = source THEN
			wr.WriteInt(v.adr);
			wr.WriteSString(v.name)
		ELSIF t = module THEN
			wr.WriteSString(v.name)
		END
	END Externalize;

	PROCEDURE (v: RefView) CopyFromSimpleView (source: Views.View);
	BEGIN
		(* v.CopyFrom^(source); *)
		WITH source: RefView DO
			v.type := source.type; v.command := source.command; v.adr := source.adr; v.back := source.back;
			v.desc := source.desc; v.ptr := source.ptr; v.name := source.name$;
		END
	END CopyFromSimpleView;

	PROCEDURE (v: RefView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawPath(path, 4, Ports.fill, Ports.blue, Ports.closedPoly)
	END Restore;
	
	PROCEDURE (v: RefView) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;

	PROCEDURE (v: RefView) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR t, t0: TextModels.Model; m: Models.Model; x, y: INTEGER;
			isDown, new: BOOLEAN; mo: SET; script: Stores.Operation;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			IF v.type > 0 THEN
				REPEAT
					f.MarkRect(0, 0, refViewSize, refViewSize, Ports.fill, Ports.hilite, Ports.show);
					IF v.command = undo THEN Dialog.ShowStatus("#Dev:ShowPrecedingObject")
					ELSIF v.command = update THEN Dialog.ShowStatus("#Dev:UpdateWindow")
					ELSIF v.type = module THEN Dialog.ShowStatus("#Dev:ShowGlobalVariables")
					ELSIF v.type = source THEN Dialog.ShowStatus("#Dev:ShowSourcePosition")
					ELSIF v.type = heap THEN Dialog.ShowStatus("#Dev:ShowReferencedObject")
					END;
					REPEAT
						f.Input(x, y, mo, isDown)
					UNTIL (x < 0) OR (x > refViewSize) OR (y < 0) OR (y > refViewSize) OR ~isDown;
					f.MarkRect(0, 0, refViewSize, refViewSize, Ports.fill, Ports.hilite, Ports.hide);
					Dialog.ShowStatus("");
					WHILE isDown & ((x < 0) OR (x > refViewSize) OR (y < 0) OR (y > refViewSize)) DO
						f.Input(x, y, mo, isDown)
					END
				UNTIL ~isDown;
				IF (x >= 0) & (x <= refViewSize) & (y >= 0) & (y <= refViewSize) THEN
					IF v.type = source THEN ShowSourcePos(v.name, v.adr)
					ELSE
						m := v.context.ThisModel();
						new := (v.command = open) & (v.back = NIL)
							OR (Controllers.modify IN msg.modifiers) & (v.command # update)
							OR ~(m IS TextModels.Model) ;
						IF new THEN
							t := TextModels.CloneOf(StdLog.buf); t0 := NIL
						ELSE
							t0 := m(TextModels.Model); t := TextModels.CloneOf(t0);
						END;
						out.ConnectTo(t);
						IF v.type = heap THEN  ShowPtrDeref(v)
						ELSIF v.type = module THEN ShowGlobals(Kernel.ThisLoadedMod(v.name))
						ELSIF v.type = modules THEN ShowModules
						END;
						out.ConnectTo(NIL);
						IF new THEN
							OpenViewer(t, "#Dev:Variables", NewRuler())
						ELSE
							Models.BeginScript(t0, "#Dev:Change", script);
							t0.Delete(0, t0.Length()); t0.Insert(0, t, 0, t.Length());
							Models.EndScript(t0, script)
						END
					END
				END
			END
		| msg: Controllers.PollCursorMsg DO
			msg.cursor := Ports.refCursor
		ELSE
		END
	END HandleCtrlMsg;
	
	PROCEDURE (v: RefView) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.Preference DO
			WITH msg: Properties.ResizePref DO msg.fixed := TRUE
			| msg: Properties.SizePref DO msg.w := refViewSize; msg.h := refViewSize
			| msg: Properties.FocusPref DO msg.hotFocus := TRUE
			ELSE
			END
		ELSE
		END
	END HandlePropMsg;
	
	PROCEDURE NewRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Kernel.Type; ptr: ArrayPtr; name: Name): RefView;
		VAR v: RefView;
	BEGIN
		NEW(v); v.type := type; v.command := command; v.adr := adr; v.back := back;
		v.desc := desc; v.ptr := ptr; v.name := name$;
		RETURN v
	END NewRefView;

	PROCEDURE InsertRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Kernel.Type; ptr: ArrayPtr; name: Name);
		VAR v: RefView; a0: TextModels.Attributes;
	BEGIN
		v := NewRefView(type, command, adr, back, desc, ptr, name);
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewOffset(a0, Ports.point));
		out.WriteView(v);
		out.rider.SetAttr(a0)
	END InsertRefView;
	
	PROCEDURE HeapRefView* (adr: INTEGER; name: ARRAY OF CHAR): Views.View;
		VAR n: Name; ptr: ArrayPtr;
	BEGIN
		n := SHORT(name$);
		ptr := SYSTEM.VAL(ArrayPtr, adr);
		RETURN NewRefView(heap, open, adr, NIL, NIL, ptr, n)
	END HeapRefView;
	
	(* ----------------------------------------- *)

	PROCEDURE GetMod (VAR mod: Kernel.Module);
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end: INTEGER;
	BEGIN
		mod := NIL;
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			s.ConnectTo(c.text); s.SetPos(beg); s.Scan;
			IF s.type = TextMappers.string THEN
				mod := Kernel.ThisMod(s.string);
				IF mod = NIL THEN
					Dialog.ShowParamMsg("#Dev:ModuleNotFound", s.string, "", "")
				END				
			ELSE Dialog.ShowMsg("#Dev:NoModuleNameSelected")
			END
		ELSE Dialog.ShowMsg("#Dev:NoSelectionFound")
		END
	END GetMod;

	PROCEDURE ShowLoadedModules*;
	BEGIN
		out.ConnectTo(TextModels.CloneOf(StdLog.buf));
		ShowModules;
		OpenViewer(out.rider.Base(), "#Dev:LoadedModules", NewModRuler());
		out.ConnectTo(NIL)
	END ShowLoadedModules;
	
	PROCEDURE UpdateModules*;
		VAR t, t0: TextModels.Model; script: Stores.Operation;
	BEGIN
		t0 := TextViews.FocusText();
		Models.BeginScript(t0, "#Dev:Change", script);
		t := TextModels.CloneOf(t0);
		out.ConnectTo(t);
		ShowModules;
		(*Stores.InitDomain(t, t0.domain);*) Stores.Join(t, t0);	(* not efficient to init domain before writing *)
		t0.Delete(0, t0.Length()); t0.Insert(0, t, 0, t.Length());
		Models.EndScript(t0, script);
		out.ConnectTo(NIL)
	END UpdateModules;

	PROCEDURE ShowGlobalVariables*;
		VAR mod: Kernel.Module;
	BEGIN
		GetMod(mod);
		IF mod # NIL THEN
			out.ConnectTo(TextModels.CloneOf(StdLog.buf));
			ShowGlobals(mod);
			OpenViewer(out.rider.Base(), "#Dev:Variables", NewRuler());
			out.ConnectTo(NIL)
		END
	END ShowGlobalVariables;
	
	PROCEDURE UpdateGlobals* (name: ARRAY OF CHAR);
		VAR t, t0: TextModels.Model; script: Stores.Operation; mod: Kernel.Module; n: Kernel.Name;
	BEGIN
		n := SHORT(name$); mod := Kernel.ThisLoadedMod(n);
		IF mod # NIL THEN
			t0 := TextViews.FocusText();
			Models.BeginScript(t0, "#Dev:Change", script);
			t := TextModels.CloneOf(t0);
			out.ConnectTo(t);
			ShowGlobals(mod);
			(*Stores.InitDomain(t, t0.domain);*) Stores.Join(t, t0);	(* not efficient to init domain before writing *)
			t0.Delete(0, t0.Length()); t0.Insert(0, t, 0, t.Length());
			Models.EndScript(t0, script);
			out.ConnectTo(NIL)
		END
	END UpdateGlobals;

	PROCEDURE ShowHeapObject* (adr: INTEGER; title: ARRAY OF CHAR);
	BEGIN
		out.ConnectTo(TextModels.CloneOf(StdLog.buf));
		IF title # "" THEN
			out.WriteString(title); out.WriteLn; out.WriteLn
		END;
		out.WriteChar("["); WriteHex(adr); out.WriteChar("]"); out.WriteTab;
		ShowObject(adr);
		out.WriteLn;
		OpenViewer(out.rider.Base(), "#Dev:HeapObject", NewRuler());
		out.ConnectTo(NIL)
	END ShowHeapObject;
	
	PROCEDURE ShowViewState*;
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		Controllers.PollOps(ops);
		IF ops.singleton # NIL THEN
			ShowHeapObject(SYSTEM.VAL(INTEGER, ops.singleton), "")
		END
	END ShowViewState;
	
	
	PROCEDURE UnloadMod (name: TextMappers.String; VAR ok: BOOLEAN);
		VAR mod: Kernel.Module; str: Dialog.String; n: Kernel.Name;
	BEGIN
		n := SHORT(name$); mod := Kernel.ThisLoadedMod(n);
		IF mod # NIL THEN
			Dialog.ShowParamStatus("#Dev:Unloading", name, "", "");
			Kernel.UnloadMod(mod);
			IF mod.refcnt < 0 THEN
				Dialog.MapParamString("#Dev:Unloaded", name, "", "", str);
				StdLog.String(str); StdLog.Ln
			ELSE
				Dialog.ShowParamMsg("#Dev:UnloadingFailed", name, "", "");
				ok := FALSE
			END
		ELSE
			Dialog.ShowParamMsg("#Dev:NotFound", name, "", "");
			ok := FALSE;
		END
	END UnloadMod;
	
	PROCEDURE Unload*;
		VAR t: TextModels.Model; s: TextMappers.Scanner; ok: BOOLEAN;
	BEGIN
		t := TextViews.FocusText();
		IF t # NIL THEN
			s.ConnectTo(t); s.SetPos(0);
			REPEAT Scan(s) UNTIL (s.type = smodule) OR s.rider.eot;
			IF (s.type = smodule) THEN
				Scan(s);
				IF (s.type = TextMappers.string) & IsIdent(s.string) THEN
					ok := TRUE; UnloadMod(s.string, ok);
					IF ok THEN Dialog.ShowStatus("#Dev:Ok") END;
					Controls.Relink
				ELSE
					Dialog.ShowMsg("#Dev:NoModNameFound");
				END
			ELSE
				Dialog.ShowMsg("#Dev:NoModNameFound");
			END
		END
	END Unload;
	
	PROCEDURE UnloadList(beg, end: INTEGER; c: TextControllers.Controller);
		VAR s: TextMappers.Scanner; res: INTEGER; ok, num: BOOLEAN; linked: ARRAY 16 OF CHAR;
	BEGIN
		s.ConnectTo(c.text); s.SetPos(beg); s.Scan; ok := TRUE; num := FALSE;
		WHILE (s.start < end) & (s.type # TextMappers.invalid) DO
			Dialog.MapString("#Dev:Linked", linked);
			IF (s.type = TextMappers.string) & (s.string # linked) THEN
				IF num & ((s.string = "AM") OR (s.string = "PM")) THEN s.Scan (* skip am & pm *)
				ELSIF IsIdent(s.string) THEN UnloadMod(s.string, ok); s.Scan
				ELSE s.type := TextMappers.invalid
				END
			ELSE
				(* skip numbers to allow selection of list of loaded modules *)
				num := TRUE; s.Scan
			END
		END;
		IF ok THEN Dialog.ShowStatus("#Dev:Ok") END;
		Controls.Relink
	END UnloadList;

	PROCEDURE UnloadModuleList*;
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; res, beg, end: INTEGER;
			ok, num: BOOLEAN; linked: ARRAY 16 OF CHAR;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			UnloadList(beg, end, c)
		END
	END UnloadModuleList;

	PROCEDURE UnloadThis*;
		VAR p: DevCommanders.Par; beg, end: INTEGER; c: TextControllers.Controller;
	BEGIN
		p := DevCommanders.par;
		IF p # NIL THEN
			DevCommanders.par := NIL;
			beg := p.beg; end := p.end;
			c := TextControllers.Focus();
			IF c # NIL THEN UnloadList(beg, end, c) END
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END
	END UnloadThis;

	PROCEDURE Execute*;
		VAR beg, end, start, res, i: INTEGER; done: BOOLEAN;
			c: TextControllers.Controller; s: TextMappers.Scanner; cmd: Dialog.String;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			s.ConnectTo(c.text); s.SetPos(beg); s.Scan; TextMappers.ScanQualIdent(s, cmd, done);
			IF done THEN
				Dialog.Call(cmd, " ", res)
			ELSIF s.type = TextMappers.string THEN
				Dialog.Call(s.string, " ", res)
			ELSE
				Dialog.ShowMsg("#Dev:StringExpected")
			END
		ELSE Dialog.ShowMsg("#Dev:NoSelectionFound")
		END
	END Execute;

	PROCEDURE ShowStack;
		VAR ref, end, i, j, x, a, b, c: INTEGER; m, f: SHORTCHAR; mod: Kernel.Module; name, sel: Kernel.Name;
			d: Kernel.Type;
	BEGIN
		a := Kernel.pc; b := Kernel.fp; c := 100;
		REPEAT
			mod := Kernel.modList;
			WHILE (mod # NIL) & ((a < mod.code) OR (a >= mod.code + mod.csize)) DO mod := mod.next END;
			IF mod # NIL THEN
				DEC(a, mod.code);
				IF mod.refcnt >= 0 THEN
					InsertRefView(module, open, 0, NIL, NIL, NIL, mod.name);
					out.WriteChar(" "); out.WriteSString(mod.name); ref := mod.refs;
					REPEAT Kernel.GetRefProc(ref, end, name) UNTIL (end = 0) OR (a < end);
					IF a < end THEN
						out.WriteChar("."); out.WriteSString(name);
						sel := mod.name$; i := 0;
						WHILE sel[i] # 0X DO INC(i) END;
						sel[i] := "."; INC(i); j := 0;
						WHILE name[j] # 0X DO sel[i] := name[j]; INC(i); INC(j) END;
						sel[i] := ":"; sel[i+1] := 0X;
						out.WriteSString("   ["); WriteHex(a);
						out.WriteSString("] ");
						i := SourcePos(mod, 0);
						IF i >= 0 THEN
							InsertRefView(source, open, a, NIL, NIL, NIL, mod.name);
						END;
						IF name # "$$" THEN
							Kernel.GetRefVar(ref, m, f, d, x, name);
							WHILE m # 0X DO
								ShowVar(b + x, 0, f, m, d, NIL, NIL, name, sel);
								Kernel.GetRefVar(ref, m, f, d, x, name);
							END
						END;
						out.WriteLn
					ELSE out.WriteSString(".???"); out.WriteLn
					END
				ELSE
					out.WriteChar("("); out.WriteSString(mod.name);
					out.WriteSString(")   (pc="); WriteHex(a);
					out.WriteSString(",  fp="); WriteHex(b); out.WriteChar(")");
					out.WriteLn
				END
			ELSE
				out.WriteSString("<system>   (pc="); WriteHex(a);
				out.WriteSString(",  fp="); WriteHex(b); out.WriteChar(")");
				out.WriteLn
			END;
			IF (b >= Kernel.fp) & (b < Kernel.stack) THEN
				SYSTEM.GET(b+4, a);	(* stacked pc *)
				SYSTEM.GET(b, b);	(* dynamic link *)
				DEC(a); DEC(c)
			ELSE c := 0
			END
		UNTIL c = 0
	END ShowStack;

	PROCEDURE (a: Action) Do;	(* delayed trap window open *)
	BEGIN
		Kernel.SetTrapGuard(TRUE);
		OpenViewer(a.text, "#Dev:Trap", NewRuler());
		Kernel.SetTrapGuard(FALSE);
	END Do;
	
	PROCEDURE GetTrapMsg(OUT msg: ARRAY OF CHAR);
		VAR ref, end, a: INTEGER; mod: Kernel.Module; name: Kernel.Name; head, tail, errstr: ARRAY 32 OF CHAR;
			key: ARRAY 128 OF CHAR;
	BEGIN
		a := Kernel.pc; mod := Kernel.modList;
		WHILE (mod # NIL) & ((a < mod.code) OR (a >= mod.code + mod.csize)) DO mod := mod.next END;
		IF mod # NIL THEN
			DEC(a, mod.code); ref := mod.refs;
			REPEAT Kernel.GetRefProc(ref, end, name) UNTIL (end = 0) OR (a < end);
			IF a < end THEN
				Kernel.SplitName (mod.name$, head, tail);
				IF head = "" THEN head := "System" END;
				Strings.IntToString(Kernel.err, errstr);
				key := tail + "." + name + "." + errstr;
				Dialog.MapString("#" + head + ":" + key, msg);
				(* IF key # msg THEN out.WriteString(" " + msg) END; *)
				IF key = msg THEN msg := "" END;
			END
		END
	END GetTrapMsg;

	PROCEDURE Trap;
		VAR a0: TextModels.Attributes; prop: Properties.StdProp; action: Action;
		 	msg: ARRAY 512 OF CHAR;
	BEGIN
		out.ConnectTo(TextModels.CloneOf(StdLog.buf));
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewWeight(a0, Fonts.bold));
		IF Kernel.err = 129 THEN out.WriteSString("invalid WITH")
		ELSIF Kernel.err = 130 THEN out.WriteSString("invalid CASE")
		ELSIF Kernel.err = 131 THEN out.WriteSString("function without RETURN")
		ELSIF Kernel.err = 132 THEN out.WriteSString("type guard")
		ELSIF Kernel.err = 133 THEN out.WriteSString("implied type guard")
		ELSIF Kernel.err = 134 THEN out.WriteSString("value out of range")
		ELSIF Kernel.err = 135 THEN out.WriteSString("index out of range")
		ELSIF Kernel.err = 136 THEN out.WriteSString("string too long")
		ELSIF Kernel.err = 137 THEN out.WriteSString("stack overflow")
		ELSIF Kernel.err = 138 THEN out.WriteSString("integer overflow")
		ELSIF Kernel.err = 139 THEN out.WriteSString("division by zero")
		ELSIF Kernel.err = 140 THEN out.WriteSString("infinite real result")
		ELSIF Kernel.err = 141 THEN out.WriteSString("real underflow")
		ELSIF Kernel.err = 142 THEN out.WriteSString("real overflow")
		ELSIF Kernel.err = 143 THEN
			out.WriteSString("undefined real result  (");
			out.WriteIntForm(Kernel.val MOD 10000H, TextMappers.hexadecimal, 4, "0", TextMappers.hideBase); out.WriteSString(", ");
			out.WriteIntForm(Kernel.val DIV 10000H, TextMappers.hexadecimal, 3, "0", TextMappers.hideBase); out.WriteChar(")")
		ELSIF Kernel.err = 144 THEN out.WriteSString("not a number")
		ELSIF Kernel.err = 200 THEN out.WriteSString("keyboard interrupt")
		ELSIF Kernel.err = 201 THEN
			out.WriteSString("NIL dereference")
		ELSIF Kernel.err = 202 THEN
			out.WriteSString("illegal instruction: ");
			out.WriteIntForm(Kernel.val, TextMappers.hexadecimal, 5, "0", TextMappers.showBase)
		ELSIF Kernel.err = 203 THEN
			IF (Kernel.val >= -4) & (Kernel.val < 65536) THEN out.WriteSString("NIL dereference (read)")
			ELSE out.WriteSString("illegal memory read (ad = "); WriteHex(Kernel.val); out.WriteChar(")")
			END
		ELSIF Kernel.err = 204 THEN
			IF (Kernel.val >= -4) & (Kernel.val < 65536) THEN out.WriteSString("NIL dereference (write)")
			ELSE out.WriteSString("illegal memory write (ad = "); WriteHex(Kernel.val); out.WriteChar(")")
			END
		ELSIF Kernel.err = 205 THEN
			IF (Kernel.val >= -4) & (Kernel.val < 65536) THEN out.WriteSString("NIL procedure call")
			ELSE out.WriteSString("illegal execution (ad = "); WriteHex(Kernel.val); out.WriteChar(")")
			END
		ELSIF Kernel.err = 257 THEN out.WriteSString("out of memory")
		ELSIF Kernel.err = 10001H THEN out.WriteSString("bus error")
		ELSIF Kernel.err = 10002H THEN out.WriteSString("address error")
		ELSIF Kernel.err = 10007H THEN out.WriteSString("fpu error")
		ELSIF Kernel.err < 0 THEN
			out.WriteSString("Exception "); out.WriteIntForm(-Kernel.err, TextMappers.hexadecimal, 3, "0", TextMappers.showBase)
		ELSE
			out.WriteSString("TRAP "); out.WriteInt(Kernel.err);
			IF Kernel.err = 126 THEN out.WriteSString("  (not yet implemented)")
			ELSIF Kernel.err = 125 THEN out.WriteSString("  (call of obsolete procedure)")
			ELSIF Kernel.err >= 100 THEN out.WriteSString("  (invariant violated)")
			ELSIF Kernel.err >= 60 THEN out.WriteSString("  (postcondition violated)")
			ELSIF Kernel.err >= 20 THEN out.WriteSString("  (precondition violated)")
			END
		END;
		GetTrapMsg(msg);
		IF msg # "" THEN out.WriteLn; out.WriteString(msg) END;

		out.WriteLn; out.rider.SetAttr(a0);
		out.WriteLn; ShowStack;
		NEW(action); action.text := out.rider.Base();
		Services.DoLater(action, Services.now);
		out.ConnectTo(NIL)
	END Trap;

BEGIN
	Kernel.InstallTrapViewer(Trap);
	empty := "";
	path[0].x := refViewSize DIV 2; path[0].y := 0;
	path[1].x := refViewSize; path[1].y := refViewSize DIV 2;
	path[2].x := refViewSize DIV 2; path[2].y := refViewSize;
	path[3].x := 0; path[3].y := refViewSize DIV 2
END DevDebug.
