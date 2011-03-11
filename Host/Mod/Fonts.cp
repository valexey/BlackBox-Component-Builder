MODULE HostFonts;
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

	IMPORT SYSTEM, WinApi, Kernel, Fonts, HostRegistry;

	CONST
		defSize = 8 * Fonts.point;	(* size of default font *)
(*
		grid = 16384;	(* true type design grid *)
*)
		grid = 4096;
		figureSpace = 8FX;

	TYPE
		WTab = ARRAY 256 OF INTEGER;
		DevFont* = POINTER TO RECORD
			unit-: INTEGER;
			id-: WinApi.HANDLE;
			next-: DevFont;
			noGap-: BOOLEAN;
			wtab-: WTab	(* rastered width in pixels *)
		END;
		Font* = POINTER TO RECORD (Fonts.Font)
			asc-, dsc-, w-: INTEGER;
			dev-: DevFont;	(* rastered fonts *)
			wtab-, ftab-, ttab-: WTab;	(* univeral width in units *)
			id-: WinApi.HANDLE;	(* font used for metric*)
			alias-: Fonts.Typeface;	(* alias # typeface & typeface # "*" == alien font *)
			a, b: INTEGER	(* coefficients for metric *)
		END;

		Directory = POINTER TO RECORD (Fonts.Directory) END;
		
		Identifier = RECORD (Kernel.Identifier)
			tface: Fonts.Typeface;
			size: INTEGER;
			style: SET;
			weight: INTEGER
		END;

		Counter = RECORD (Kernel.Identifier)
			count: INTEGER
		END;
		
		Traverser = RECORD (Kernel.Identifier)
		END;
		
		Par = RECORD [untagged]
			first, last: Fonts.TypefaceInfo
		END;
		ParPtr = POINTER TO Par;

	VAR
		sysFont-, defFont-, dlgFont-, dlgBoldFont-: Font;
		isUnicode-, useTTMetric-: BOOLEAN;
		dfName, dgName: Fonts.Typeface;
		dfSize, dgSize, dgWght: INTEGER;
		dgStyle: SET;
		dir: Directory;
		defUnit: INTEGER;	(* screen resolution *)
		dc: WinApi.HANDLE;

	(* width tab setup *)
	
	PROCEDURE NewDevFont (
		typeface: ARRAY OF CHAR; size, unit: INTEGER; style: SET; weight: INTEGER
	): DevFont;
		VAR df: DevFont; it, ul, so: INTEGER;
	BEGIN
		IF size = 8 * Fonts.point THEN INC(size, Fonts.point DIV 4) END;
		it := 0; ul := 0; so := 0;
		IF Fonts.italic IN style THEN it := 1 END;
		IF Fonts.underline IN style THEN ul := 1 END;
		IF Fonts.strikeout IN style THEN so := 1 END;
		NEW(df); df.unit := unit; df.next := NIL;
		df.id := WinApi.CreateFontW(-((size + unit DIV 2) DIV unit), 0, 0, 0, weight, it, ul, so, 1, 0, 2, 1, 4, typeface);
		RETURN df
	END NewDevFont;
	
	PROCEDURE GetRasterWidth (dc: WinApi.HANDLE; VAR wtab: WTab);
		VAR res, i, x: INTEGER; str: ARRAY 4 OF CHAR; s: WinApi.SIZE;
	BEGIN
		res := WinApi.GetTextExtentPoint32W(dc, "x", 1, s);
		i := 0; str := " x"; x := s.cx;
		WHILE i < 256 DO
			str[0] := CHR(i);
			res := WinApi.GetTextExtentPoint32W(dc, str, 2, s);
			wtab[i] := s.cx - x; INC(i)
		END
	END GetRasterWidth;
	
	PROCEDURE SetupWTabs (f: Font);
		VAR res, a, b, max, x, i: INTEGER; tm: WinApi.TEXTMETRICW;
			df: DevFont; abc: ARRAY 256 OF WinApi.ABC; dc, old: WinApi.HANDLE;
	BEGIN
		dc := WinApi.GetDC(0);
		old := WinApi.SelectObject(dc, f.dev.id);
		res := WinApi.GetTextMetricsW(dc, tm);
		IF useTTMetric & ODD(ORD(tm.tmPitchAndFamily) DIV 4) THEN	(* use true type metric *)
			df := NewDevFont(f.alias, grid, 1, f.style, f.weight);
			res := WinApi.SelectObject(dc, df.id);
			res := WinApi.GetTextMetricsW(dc, tm);
			a := f.size MOD grid; b := f.size DIV grid; f.id := df.id;
			res := WinApi.GetCharABCWidthsW(dc, 0, 255, abc[0]);
			IF res # 0 THEN
				i := 0; max := 0;
				WHILE i < 256 DO
					x := -abc[i].abcA;
					IF x > 0 THEN f.ftab[i] := x * a DIV grid + x * b END;
					x := -abc[i].abcC;
					IF x > 0 THEN f.ttab[i] := x * a DIV grid + x * b END;
					x := abc[i].abcA + abc[i].abcB + abc[i].abcC; x := x * a DIV grid + x * b;
					IF x > max THEN max := x END;
					f.wtab[i] := x; INC(i)
				END
			ELSE
				max := f.w
			END
		ELSE	(* use screen metric *)
			a := 0; b := defUnit; f.id := f.dev.id;
			GetRasterWidth(dc, f.wtab);
(*
			res := WinApi.GetCharWidth32W(dc, 0, 255, f.wtab);
*)
			i := 0; max := 0;
			WHILE i < 256 DO
				x := f.wtab[i] * b;
				IF x > max THEN max := x END;
				f.wtab[i] := x; INC(i)
			END
		END;
		f.wtab[ORD(figureSpace)] := f.wtab[ORD("0")];
		f.ftab[ORD(figureSpace)] := f.ftab[ORD("0")];
		f.ttab[ORD(figureSpace)] := f.ttab[ORD("0")];
		x := tm.tmAscent + tm.tmExternalLeading; f.asc := x * a DIV grid + x * b;
		f.dsc := tm.tmDescent * a DIV grid + tm.tmDescent * b;
		f.w := max; f.a := a; f.b := b;
		res := WinApi.SelectObject(dc, old);
		res := WinApi.ReleaseDC(0, dc)
	END SetupWTabs;
	
	PROCEDURE Cleanup (f: Font);
		VAR res: INTEGER; df: DevFont;
	BEGIN
		df := f.dev;
		IF f.id # df.id THEN res := WinApi.DeleteObject(f.id) END;
		WHILE df # NIL DO
			res := WinApi.DeleteObject(df.id);
			df := df.next
		END;
		f.id := 0; f.dev := NIL
	END Cleanup;
	

	(* width methods for unicode *)
	
	PROCEDURE (f: Font) wTab* (dc: WinApi.HANDLE; ch: CHAR): INTEGER, NEW;
		VAR res, w: INTEGER; abc: ARRAY 1 OF WinApi.ABC; wt: ARRAY 1 OF INTEGER;
	BEGIN
		IF ch < 100X THEN RETURN f.wtab[ORD(ch)] END;
		res := WinApi.GetCharABCWidthsW(dc, ORD(ch), ORD(ch), abc[0]);
		IF res # 0 THEN
			w := abc[0].abcA + abc[0].abcB + abc[0].abcC;
			w := w * f.a DIV grid + w * f.b
		ELSE
			res := WinApi.GetCharWidth32W(dc, ORD(ch), ORD(ch), wt[0]);
			IF res # 0 THEN w := wt[0] * f.a DIV grid + wt[0] * f.b
			ELSE
				res := WinApi.GetCharWidthW(dc, ORD(ch), ORD(ch), wt[0]);
				IF res # 0 THEN w := wt[0] * f.a DIV grid + wt[0] * f.b
				ELSE w := f.wtab[1]
				END
			END
		END;
		RETURN w
	END wTab;

	PROCEDURE (f: Font) fTab* (dc: WinApi.HANDLE; ch: CHAR): INTEGER, NEW;
		VAR res, w: INTEGER; abc: ARRAY 1 OF WinApi.ABC;
	BEGIN
		IF ch < 100X THEN RETURN f.ftab[ORD(ch)] END;
		res := WinApi.GetCharABCWidthsW(dc, ORD(ch), ORD(ch), abc[0]);
		IF (res # 0) & (abc[0].abcA < 0) THEN
			w := -abc[0].abcA;
			w := w * f.a DIV grid + w * f.b
		ELSE w := 0
		END;
		RETURN w
	END fTab;

	PROCEDURE (f: Font) tTab* (dc: WinApi.HANDLE; ch: CHAR): INTEGER, NEW;
		VAR res, w: INTEGER; abc: ARRAY 1 OF WinApi.ABC;
	BEGIN
		IF ch < 100X THEN RETURN f.ttab[ORD(ch)] END;
		res := WinApi.GetCharABCWidthsW(dc, ORD(ch), ORD(ch), abc[0]);
		IF (res # 0) & (abc[0].abcC < 0) THEN
			w := -abc[0].abcC;
			w := w * f.a DIV grid + w * f.b
		ELSE w := 0
		END;
		RETURN w
	END tTab;
	
	PROCEDURE (df: DevFont) wTab* (dc: WinApi.HANDLE; ch: CHAR): INTEGER, NEW;
		VAR res, w: INTEGER; wt: ARRAY 1 OF INTEGER;
	BEGIN
		IF ch < 100X THEN RETURN df.wtab[ORD(ch)] END;
		res := WinApi.GetCharWidth32W(dc, ORD(ch), ORD(ch), wt[0]);
		IF res = 0 THEN res := WinApi.GetCharWidthW(dc, ORD(ch), ORD(ch), wt[0]) END;
		IF res # 0 THEN w := wt[0] ELSE w := df.wtab[1] END;
		RETURN w
	END wTab;
	

	(** Font **)

	PROCEDURE (f: Font) GetBounds* (OUT asc, dsc, w: INTEGER);
	BEGIN
		asc := f.asc; dsc := f.dsc; w := f.w
	END GetBounds;
	
	PROCEDURE (f: Font) SStringWidth* (IN s: ARRAY OF SHORTCHAR): INTEGER;
		VAR i, w: INTEGER; ch: CHAR;
	BEGIN
		w := 0;
		IF s # "" THEN
			i := 0; ch := s[0];
			WHILE ch # 0X DO INC(w, f.wtab[ORD(ch)]); INC(i); ch := s[i] END;
			w := w + f.ftab[ORD(s[0])] + f.ttab[ORD(s[i-1])]
		END;
		RETURN w
	END SStringWidth;

	PROCEDURE (f: Font) StringWidth* (IN s: ARRAY OF CHAR): INTEGER;
		VAR res, i, w: INTEGER; lc: CHAR; dc, old: WinApi.HANDLE;
	BEGIN
		dc := WinApi.GetDC(0);
		old := WinApi.SelectObject(dc, f.id);
		w := 0;
		IF s[0] # 0X THEN
			i := 0; lc := s[0];
			WHILE lc # 0X DO INC(w, f.wTab(dc, lc)); INC(i); lc := s[i] END;
			w := w + f.fTab(dc, s[0]) + f.tTab(dc, s[i-1])
		END;
		res := WinApi.SelectObject(dc, old);
		res := WinApi.ReleaseDC(0, dc);
		RETURN w
	END StringWidth;

	PROCEDURE (f: Font) IsAlien* (): BOOLEAN;
	BEGIN
		RETURN (f.typeface # Fonts.default) & (f.alias # f.typeface)
	END IsAlien;
	
	PROCEDURE (f: Font) FINALIZE-;
	BEGIN
		Cleanup(f)
	END FINALIZE;
	


	(* Directory *)
	
	
	PROCEDURE SetupDevFont (dc: WinApi.HANDLE; df: DevFont);
		VAR res: INTEGER; abc: ARRAY 1 OF WinApi.ABC;
	BEGIN
		res := WinApi.GetCharABCWidthsW(dc, ORD("H"), ORD("H"), abc[0]);
		IF res # 0 THEN	(* true type *)
			df.noGap := (res # 0) & (abc[0].abcA <= 0);
			res := WinApi.GetCharWidth32W(dc, 0, 255, df.wtab[0]);
		ELSE	(* raster *)
			df.noGap := FALSE;
			GetRasterWidth(dc, df.wtab)
		END;
		df.wtab[ORD(figureSpace)] := df.wtab[ORD("0")]
	END SetupDevFont;
	
	PROCEDURE InsertDevFont* (dc: WinApi.HANDLE; font: Font; VAR df: DevFont; unit: INTEGER);
		VAR res: INTEGER;
	BEGIN
		df := NewDevFont(font.alias, font.size, unit, font.style, font.weight);
		res := WinApi.SelectObject(dc, df.id);
		SetupDevFont(dc, df);
		df.next := font.dev.next; font.dev.next := df	(* screen font remains at list head *)
	END InsertDevFont;
	
	PROCEDURE Setup (f: Font; typeface: ARRAY OF CHAR; size: INTEGER; style: SET; weight: INTEGER);
		VAR res: INTEGER; tm: WinApi.TEXTMETRICW; name: Fonts.Typeface; dc, old: WinApi.HANDLE;
	BEGIN
		dc := WinApi.GetDC(0);
		old := WinApi.SelectObject(dc, f.dev.id);
		res := WinApi.GetTextFaceW(dc, LEN(name), name);
		res := WinApi.GetTextMetricsW(dc, tm);
		f.alias := name$;
		IF typeface = Fonts.default THEN
			name := Fonts.default
		ELSIF (typeface = "") OR (typeface = ".") THEN
			size := ((tm.tmHeight - tm.tmInternalLeading) * defUnit + (Fonts.point DIV 2)) DIV Fonts.point * Fonts.point;
			(* IF size = 8 * Fonts.point THEN INC(size, Fonts.point DIV 4) END; *)
			weight := tm.tmWeight;
			IF typeface = "." THEN name := Fonts.default END;
			IF tm.tmItalic # 0X THEN INCL(style, Fonts.italic) END;
			IF tm.tmUnderlined # 0X THEN INCL(style, Fonts.underline) END;
			IF tm.tmStruckOut # 0X THEN INCL(style, Fonts.strikeout) END
		ELSIF name # typeface THEN
			f.dev := NewDevFont(dfName, size, defUnit, style, weight);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, f.dev.id));
			f.alias := dfName$;
			name := typeface$
		END;
		IF size # 0 THEN
			SetupDevFont(dc, f.dev);
			IF f.size = 0 THEN f.Init(name, size, style, weight) END;
			res := WinApi.SelectObject(dc, old);
			res := WinApi.ReleaseDC(0, dc);
			SetupWTabs(f)
		END;
		ASSERT(f.size > 0)
	END Setup;
	
	PROCEDURE (VAR id: Identifier) Identified (): BOOLEAN;
		VAR f: Font;
	BEGIN
		f := id.obj(Font);
		RETURN (f.typeface = id.tface) & (f.size = id.size) & (f.style = id.style) & (f.weight = id.weight)
	END Identified;

	PROCEDURE (d: Directory) This (typeface: Fonts.Typeface; size: INTEGER; style: SET; weight: INTEGER): Font;
		VAR f: Font; i: Identifier; p: ANYPTR;
	BEGIN
		ASSERT(size > 0, 20);
		style := style * {Fonts.italic, Fonts.underline, Fonts.strikeout};
		size := size - size MOD Fonts.point;
		(* IF size = 8 * Fonts.point THEN INC(size, Fonts.point DIV 4) END; *)
		IF typeface = "L Frutiger Light" THEN typeface := "Frutiger 45 Light" 
		ELSIF typeface = "R Frutiger Roman" THEN typeface := "Frutiger 55 Roman"
		ELSIF typeface = "B Frutiger Black" THEN typeface := "Frutiger 55 Roman"; weight := Fonts.bold
		END;
		i.tface := typeface$; i.size := size; i.style := style; i.weight := weight;
		i.typ := SYSTEM.TYP(Font);
		p := Kernel.ThisFinObj(i);
		IF p # NIL THEN f := p(Font)
		ELSE	(* not found in cache, search Windows fonts *)
			IF typeface = "" THEN
				f := sysFont
			ELSE
				NEW(f);
				IF typeface = Fonts.default THEN
					f.dev := NewDevFont(dfName, size, defUnit, style, weight)
				ELSE
					f.dev := NewDevFont(typeface, size, defUnit, style, weight)
				END;
				Setup(f, typeface, size, style, weight)
			END
		END;
		RETURN f
	END This;

	PROCEDURE (d: Directory) Default (): Fonts.Font;
	BEGIN
		RETURN defFont
	END Default;

	PROCEDURE CallBack (
		VAR [nil] elf: WinApi.ENUMLOGFONTW; VAR [nil] ntm: WinApi.NEWTEXTMETRICW; type, par: INTEGER
	): INTEGER;
		VAR p: ParPtr; info: Fonts.TypefaceInfo;
	BEGIN
		p := SYSTEM.VAL(ParPtr, par);
		NEW(info);
		info.typeface := elf.elfLogFont.lfFaceName$;
		IF p.last = NIL THEN p.first := info ELSE p.last.next := info END;
		p.last := info;
		RETURN 1
	END CallBack;

	PROCEDURE (d: Directory) TypefaceList* (): Fonts.TypefaceInfo;
		VAR res: INTEGER; dc: WinApi.HANDLE; par: Par;
	BEGIN
		dc := WinApi.GetDC(0);
		par.first := NIL; par.last := NIL;
		res := WinApi.EnumFontFamiliesW(dc, NIL, CallBack, SYSTEM.ADR(par));
		res := WinApi.ReleaseDC(0, dc);
		RETURN par.first
	END TypefaceList;





	(** miscellaneous **)

	PROCEDURE (VAR id: Counter) Identified (): BOOLEAN;
	BEGIN
		INC(id.count); RETURN FALSE
	END Identified;

	PROCEDURE NofFonts* (): INTEGER;
		VAR p: ANYPTR; cnt: Counter;
	BEGIN
		cnt.typ := SYSTEM.TYP(Font); cnt.count := 0; p := Kernel.ThisFinObj(cnt);
		RETURN cnt.count
	END NofFonts;

	PROCEDURE InstallDir*;
	BEGIN
		Fonts.SetDir(dir)
	END InstallDir;
	

	PROCEDURE (VAR id: Traverser) Identified (): BOOLEAN;
		VAR f: Font;
	BEGIN
		f := id.obj(Font);
		IF (f.typeface = Fonts.default) & (f.alias # dfName) THEN
			Cleanup(f);
			f.dev := NewDevFont(dfName, f.size, defUnit, f.style, f.weight);
			Setup(f, Fonts.default, f.size, f.style, f.weight)
		ELSE
			SetupWTabs(f)
		END;
		RETURN FALSE
	END Identified;

	PROCEDURE SetTTMetric* (on: BOOLEAN);
		VAR t: Traverser; p: ANYPTR;
	BEGIN
		IF useTTMetric # on THEN
			useTTMetric := on;
			t.typ := SYSTEM.TYP(Font); p := Kernel.ThisFinObj(t);
			HostRegistry.WriteBool("FontTTMetric", useTTMetric)
		END
	END SetTTMetric;
	
	PROCEDURE SetDefaultFont* (tf: Fonts.Typeface; size: INTEGER);
		VAR t: Traverser; p: ANYPTR;
	BEGIN
		ASSERT(tf # "", 20); ASSERT(size > 0, 21);
		IF tf = Fonts.default THEN tf := dfName$ END;
		IF (dfName # tf) OR (dfSize # size) THEN
			dfName := tf$; dfSize := size;
			t.typ := SYSTEM.TYP(Font); p := Kernel.ThisFinObj(t);
			defFont := dir.This(Fonts.default, dfSize, {}, Fonts.normal);
			HostRegistry.WriteString("DefFontName", dfName);
			HostRegistry.WriteInt("DefFontSize", dfSize)
		END
	END SetDefaultFont;

	PROCEDURE SetDialogFont* (tf: Fonts.Typeface; size: INTEGER; style: SET; weight: INTEGER);
		VAR i: INTEGER;
	BEGIN
		ASSERT(tf # "", 20); ASSERT(size > 0, 21);
		IF (dgName # tf) OR (dgSize # size) OR (dgStyle # style) OR (dgWght # weight) THEN
			dgName := tf$; dgSize := size; dgStyle := style; dgWght := weight;
			dlgFont := dir.This(dgName, dgSize, dgStyle, dgWght);
			dlgBoldFont := dir.This(dgName, dgSize, dgStyle, Fonts.bold);
			HostRegistry.WriteString("DlgFontName", dgName);
			HostRegistry.WriteInt("DlgFontSize", dgSize);
			i := 0;
			IF Fonts.italic IN dgStyle THEN INC(i, 1) END;
			IF Fonts.underline IN dgStyle THEN INC(i, 2) END;
			IF Fonts.strikeout IN dgStyle THEN INC(i, 4) END;
			IF dgWght > Fonts.normal THEN INC(i, 8) END;
			HostRegistry.WriteInt("DlgFontStyle", i)
		END
	END SetDialogFont;


	PROCEDURE Init;
		VAR res, i: INTEGER; dc, old, f: WinApi.HANDLE; tm: WinApi.TEXTMETRICW;
	BEGIN
		dfName := ""; dgName := ""; dfSize := 0; dgSize := 0; dgStyle := {}; dgWght := Fonts.normal; i := 0;
		HostRegistry.ReadString("DefFontName", dfName, res);
		HostRegistry.ReadInt("DefFontSize", dfSize, res);
		HostRegistry.ReadString("DlgFontName", dgName, res);
		HostRegistry.ReadInt("DlgFontSize", dgSize, res);
		HostRegistry.ReadInt("DlgFontStyle", i, res);
		IF ODD(i) THEN INCL(dgStyle, Fonts.italic) END;
		IF ODD(i DIV 2) THEN INCL(dgStyle, Fonts.underline) END;
		IF ODD(i DIV 4) THEN INCL(dgStyle, Fonts.strikeout) END;
		IF ODD(i DIV 8) THEN dgWght := Fonts.bold END;
		HostRegistry.ReadBool("FontTTMetric", useTTMetric, res);
		NEW(dir); Fonts.SetDir(dir);
		dc := WinApi.GetDC(0);
		defUnit := 72 * Fonts.point DIV WinApi.GetDeviceCaps(dc, WinApi.LOGPIXELSY);
		isUnicode := TRUE;
		res := WinApi.ReleaseDC(0, dc);
		NEW(sysFont); NEW(sysFont.dev); sysFont.dev.unit := defUnit; sysFont.dev.next := NIL;
		sysFont.dev.id := WinApi.GetStockObject(WinApi.SYSTEM_FONT);
		Setup(sysFont, "", 0, {}, 0);
		NEW(defFont); NEW(defFont.dev); defFont.dev.unit := defUnit; defFont.dev.next := NIL;
		IF (dfName # "") & (dfSize > 5 * Fonts.point) & (dfSize < 100 * Fonts.point) THEN
			defFont := dir.This(Fonts.default, dfSize, {}, Fonts.normal)
		ELSE
			i := (defSize + defUnit DIV 2) DIV defUnit;
			IF i < 11 THEN i := 11 END;
			defFont.dev.id := WinApi.CreateFontW(-i, 0, 0, 0, Fonts.normal, 0, 0, 0, 0, 7, 2, 1, 38, "");
			Setup(defFont, ".", 0, {}, 0);
			dfName := defFont.alias$
		END;
		NEW(dlgFont); NEW(dlgFont.dev); dlgFont.dev.unit := defUnit; dlgFont.dev.next := NIL;
		IF (dgName # "") & (dgSize > 5 * Fonts.point) & (dgSize < 100 * Fonts.point) THEN
			dlgFont := dir.This(dgName, dgSize, dgStyle, dgWght);
			dlgBoldFont := dir.This(dgName, dgSize, dgStyle, Fonts.bold)
		ELSE
			dlgFont.dev.id := WinApi.GetStockObject(WinApi.ANSI_VAR_FONT);
			(* ANSI_VAR_FONT is not a Unicode font *)
			(* try to use the "Tahoma" font of same size *)
			dc := WinApi.GetDC(0);
			old := WinApi.SelectObject(dc, dlgFont.dev.id);
			res := WinApi.GetTextMetricsW(dc, tm);
			f := WinApi.CreateFontW(tm.tmHeight, 0, 0, 0, 400, 0, 0, 0, 0, 0, 0, 0, 0, "Tahoma");
			res := WinApi.SelectObject(dc, old);
			res := WinApi.ReleaseDC(0, dc);
			IF f # 0 THEN
				res := WinApi.DeleteObject(dlgFont.dev.id);
				dlgFont.dev.id := f
			END;
			Setup(dlgFont, "", 0, {}, 0);
			dgName := dlgFont.alias$;
			dlgBoldFont := dir.This(dlgFont.typeface, dlgFont.size, dlgFont.style, Fonts.bold);
			IF WinApi.GetVersion() MOD 256 < 4 THEN dlgFont := dlgBoldFont END
		END
	END Init;

BEGIN
	Init
END HostFonts.
