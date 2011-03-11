MODULE OleData;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Alexander Iljin"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)


	IMPORT
		SYSTEM, COM, WinOle, WinApi, Log,
		Files, Strings, Meta, Dialog, Services, Ports, Stores, Models, Views, Properties, Containers,
		HostPorts;
		
		
	CONST
		debug = FALSE;
		(* opts *)
		stream* = 0;
		storage* = 1;
		file* = 2;
		info* = 16;
		single* = 17;
		select* = 18;
		
		obfTag = 6F4F4443H;
		
	
	TYPE
		Exporter* = PROCEDURE (
			v: Views.View; w, h, rx, ry: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM);
		Importer* = PROCEDURE (
			VAR med: WinOle.STGMEDIUM; OUT v: Views.View; OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
		InfoImporter* = PROCEDURE (
			VAR med: WinOle.STGMEDIUM; VAR type: Stores.TypeName;
			OUT w, h, rx, ry: INTEGER; OUT isSingle: BOOLEAN);
		
		IDataObject* = POINTER TO RECORD (WinOle.IDataObject)
			view-: Views.View;
			w-, h-, rx, ry: INTEGER;
			isSingle, useSel: BOOLEAN;
			type: Stores.TypeName
		END;
		
		Converter* = POINTER TO RECORD
			next-: Converter;
			imp-, exp-: Dialog.String;
			type-: Stores.TypeName;
			format-: WinOle.FORMATETC;
			opts-: SET
		END;
		
		
		ExpVal = RECORD (Meta.Value) p: Exporter END;
		ImpVal = RECORD (Meta.Value) p: Importer END;
		InfoVal = RECORD (Meta.Value) p: InfoImporter END;

		
		MetaFileContext = POINTER TO RECORD (Models.Context)
			(*domain: Stores.Domain;*)
			w, h: INTEGER
		END;
		
		MetaFileView = POINTER TO RECORD (Views.View)
			view: Views.View;
		END;
		
		
		IEnumFORMATETC = POINTER TO RECORD (WinOle.IEnumFORMATETC)
			cur: INTEGER;
			num: INTEGER;
			data: POINTER TO ARRAY OF RECORD c: WinOle.FORMATETC END;
		END;
		
		
		Info = POINTER TO InfoDesc;
		InfoDesc = RECORD [untagged]
			type: Stores.TypeName;
			w, h, rx, ry: INTEGER;
			isSingle: BOOLEAN;
		END;
		
		
		MemFile = POINTER TO RECORD (Files.File)
			mem: WinApi.HGLOBAL;
			len: INTEGER;
			owner: BOOLEAN
		END;
		
		MemReader = POINTER TO RECORD (Files.Reader)
			base: MemFile;
			pos: INTEGER
		END;
		
		MemWriter = POINTER TO RECORD (Files.Writer)
			base: MemFile;
			pos: INTEGER;
		END;
		
		MemPtr = POINTER TO ARRAY [untagged] OF BYTE;
	

	VAR
		dataObj-: WinOle.IDataObject;	(* for importers *)
		convList-: Converter;
		unit: INTEGER;	(* screen resolution *)
		
		
	(* Auxiliary procedures *)
	
	PROCEDURE GenFormatEtc (format: SHORTINT; aspect: SET; tymed: SET; VAR f: WinOle.FORMATETC);
	BEGIN
		f.cfFormat := format;
		f.ptd := NIL;
		f.dwAspect := aspect;
		f.lindex := -1;
		f.tymed := tymed
	END GenFormatEtc;
	
	PROCEDURE GenMetafileMedium (
		mf: WinApi.HMETAFILEPICT; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM
	);
	BEGIN
		sm.tymed := WinOle.TYMED_MFPICT;
		sm.u.hMetaFilePict := mf;
		sm.pUnkForRelease := unk
	END GenMetafileMedium;
	
	PROCEDURE GenStreamMedium (stm: WinOle.IStream; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		sm.tymed := WinOle.TYMED_ISTREAM;
		sm.u.pstm := stm;
		sm.pUnkForRelease := unk
	END GenStreamMedium;
	
	PROCEDURE GenGlobalMedium (hg: WinApi.HGLOBAL; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		sm.tymed := WinOle.TYMED_HGLOBAL;
		sm.u.hGlobal := hg;
		sm.pUnkForRelease := unk
	END GenGlobalMedium;
	
	PROCEDURE MediumStream (VAR sm: WinOle.STGMEDIUM): WinOle.IStream;
	BEGIN
		IF sm.tymed = WinOle.TYMED_ISTREAM THEN RETURN sm.u.pstm
		ELSE RETURN NIL
		END
	END MediumStream;
	
	PROCEDURE MediumGlobal (VAR sm: WinOle.STGMEDIUM): WinApi.HGLOBAL;
	BEGIN
		IF sm.tymed = WinOle.TYMED_HGLOBAL THEN RETURN sm.u.hGlobal
		ELSE RETURN 0
		END
	END MediumGlobal;
	
	PROCEDURE GetCommand (name: Dialog.String; VAR val: Meta.Value; VAR ok: BOOLEAN);
		VAR i: Meta.Item;
	BEGIN
		Meta.LookupPath(name, i);
		IF (i.obj = Meta.procObj) OR (i.obj = Meta.varObj) & (i.typ = Meta.procTyp) THEN
			i.GetVal(val, ok)
		ELSE ok := FALSE
		END
	END GetCommand;
	

	PROCEDURE Eql (IN f, g: WinOle.FORMATETC): BOOLEAN;
	BEGIN
		RETURN (f.cfFormat = g.cfFormat) & (f.ptd = g.ptd) & (f.dwAspect = g.dwAspect)
					& (f.lindex = g.lindex) & (f.tymed * g.tymed # {})
	END  Eql;
	
	PROCEDURE Equiv (IN f, g: WinOle.FORMATETC): BOOLEAN;
	BEGIN
		RETURN (f.cfFormat = g.cfFormat) & (f.ptd = g.ptd) & (f.dwAspect = g.dwAspect)
					& (f.lindex = g.lindex)
	END Equiv;
	
	PROCEDURE Compatible (c: Converter; VAR view: Views.View; isSingle: BOOLEAN): BOOLEAN;
	BEGIN
		RETURN ((c.type = "") OR Services.Is(view, c.type))
					& (~(single IN c.opts) OR isSingle)
					& (~(select IN c.opts) OR ~isSingle)
	END Compatible;
	
	
	PROCEDURE Setup (data: IDataObject);
		VAR v: Views.View; c: Containers.Controller; m: Containers.Model;
			p: Properties.BoundsPref; dx, dy: INTEGER;
	BEGIN
		c := data.view(Containers.View).ThisController();
		m := c.SelectionCopy();
(*
		v := Services.Clone(data.view)(Views.View);
		v.InitModel(m); v.CopyFrom(data.view);
*)
		v := Views.CopyWithNewModel(data.view, m);
		p.w := Views.undefined; p.h := Views.undefined; Views.HandlePropMsg(v, p);
		data.view := v; data.w := p.w; data.h := p.h; data.useSel := FALSE
	END Setup;
	
	
	(* IEnumFORMATETC  *)
	
	PROCEDURE CreateIEnumFORMATETC (num: INTEGER; VAR data: ARRAY OF WinOle.FORMATETC;
														VAR enum: WinOle.IEnumFORMATETC);
		VAR i, n: INTEGER; new: IEnumFORMATETC;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			NEW(new.data, num);
			i := 0;
			WHILE i < num DO new.data[i].c := data[i]; INC(i) END;
			enum := new
		END
	END CreateIEnumFORMATETC;
	
	PROCEDURE (this: IEnumFORMATETC) Next (num: INTEGER;
		OUT elem: ARRAY [untagged] OF WinOle.FORMATETC; OUT [nil] fetched: INTEGER
	): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.E_POINTER
		END;
		IF this.cur < this.num THEN
			WHILE (this.cur < this.num) & (num > 0) DO
				elem[n] := this.data[this.cur].c;
				INC(this.cur); INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: IEnumFORMATETC) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: IEnumFORMATETC) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: IEnumFORMATETC) Clone (OUT enum: WinOle.IEnumFORMATETC): COM.RESULT;
		VAR new: IEnumFORMATETC;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.num := this.num;
			new.cur := this.cur;
			new.data := this.data;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	(* Metafile Pictures *)
	
(*
	PROCEDURE (c: MetaFileContext) ThisDomain (): Stores.Domain;
	BEGIN
		RETURN c.domain
	END ThisDomain;
*)
	PROCEDURE (c: MetaFileContext) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL
	END ThisModel;

	PROCEDURE (c: MetaFileContext) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.w; h := c.h
	END GetSize;
		
	PROCEDURE (c: MetaFileContext) SetSize (w, h: INTEGER);
	END SetSize;
		
	PROCEDURE (c: MetaFileContext) Normalize (): BOOLEAN;
	BEGIN
		RETURN TRUE
	END Normalize;
	
	
	PROCEDURE (d: MetaFileView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Views.InstallFrame(f, d.view, 0, 0, 0, FALSE)
	END Restore;
	
	PROCEDURE (d: MetaFileView) GetNewFrame (VAR frame: Views.Frame);
		VAR f: Views.RootFrame;
	BEGIN
		NEW(f); frame := f
	END GetNewFrame;

	PROCEDURE (d: MetaFileView) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;
	
(*
	PROCEDURE (d: MetaFileView) PropagateDomain;
	BEGIN
		Stores.InitDomain(d.view, d.domain)
	END PropagateDomain;
*)

	PROCEDURE Paint (dc: WinApi.HDC; v: Views.View; w, h, unit: INTEGER);
		VAR d: MetaFileView; c: MetaFileContext; p: HostPorts.Port; f: Views.RootFrame; g: Views.Frame;
		(* m: Models.Model; *)
	BEGIN
		NEW(p);
		p.Init(unit, Ports.screen);
		p.SetSize(w, h);
		p.SetDC(dc, 0);
		NEW(c);
(*
		m := v.ThisModel();
		IF (m # NIL) & (m.domain # NIL) THEN
			c.domain := m.domain
		ELSE
			c.domain := Models.NewDomain()
		END;
*)
(*
		IF v.domain # NIL THEN
			c.domain := v.domain
		ELSE
			c.domain := Models.NewDomain()
		END;
*)
		c.w := w * p.unit;
		c.h := h * p.unit;
		NEW(d);
		d.view := Views.CopyOf(v, Views.shallow);
		Stores.Join(d, d.view);
		d.InitContext(c);
		d.view.InitContext(c);
		(* Stores.InitDomain(d, c.domain); *)
		(* Stores.InitDomain(d.view, c.domain); *)
		Stores.InitDomain(d);
		d.GetNewFrame(g); f := g(Views.RootFrame); f.ConnectTo(p);
		Views.SetRoot(f, d, FALSE, {});
		Views.AdaptRoot(f);
		Views.RestoreRoot(f, 0, 0, c.w, c.h);
	END Paint;
	
	
	(* ---------- MemFiles ---------- *)
	
	PROCEDURE (r: MemReader) Base (): Files.File;
	BEGIN
		RETURN r.base
	END Base;
	
	PROCEDURE (r: MemReader) Pos (): INTEGER;
	BEGIN
		RETURN r.pos
	END Pos;
	
	PROCEDURE (r: MemReader) SetPos (pos: INTEGER);
	BEGIN
		ASSERT(pos >= 0, 22); ASSERT(pos <= r.base.len, 21);
		r.pos := pos; r.eof := FALSE
	END SetPos;
	
	PROCEDURE (r: MemReader) ReadByte (OUT x: BYTE);
		VAR res: INTEGER; p: MemPtr;
	BEGIN
		ASSERT(r.base.mem # 0, 20);
		IF r.pos < r.base.len THEN
			p := SYSTEM.VAL(MemPtr, WinApi.GlobalLock(r.base.mem));
			x := p[r.pos]; INC(r.pos);
			res := WinApi.GlobalUnlock(r.base.mem)
		ELSE
			x := 0; r.eof := TRUE
		END
	END ReadByte;
	
	PROCEDURE (r: MemReader) ReadBytes (VAR x: ARRAY OF BYTE; beg, len: INTEGER);
		VAR res: INTEGER; p: MemPtr;
	BEGIN
		ASSERT(r.base.mem # 0, 20);
		ASSERT(beg >= 0, 21);
		IF len > 0 THEN
			ASSERT(beg + len <= LEN(x), 23);
			IF r.pos + len <= r.base.len THEN
				p := SYSTEM.VAL(MemPtr, WinApi.GlobalLock(r.base.mem));
				SYSTEM.MOVE(SYSTEM.ADR(p[r.pos]), SYSTEM.ADR(x[beg]), len);
				INC(r.pos, len);
				res := WinApi.GlobalUnlock(r.base.mem)
			ELSE
				r.eof := TRUE
			END
		ELSE ASSERT(len = 0, 22)
		END
	END ReadBytes;
	
	PROCEDURE (w: MemWriter) Base (): Files.File;
	BEGIN
		RETURN w.base
	END Base;
	
	PROCEDURE (w: MemWriter) Pos (): INTEGER;
	BEGIN
		RETURN w.pos
	END Pos;
	
	PROCEDURE (w: MemWriter) SetPos (pos: INTEGER);
	BEGIN
		ASSERT(pos >= 0, 22); ASSERT(pos <= w.base.len, 21);
		w.pos := pos
	END SetPos;
	
	PROCEDURE (w: MemWriter) WriteByte (x: BYTE);
		VAR res, size: INTEGER; p: MemPtr;
	BEGIN
		ASSERT(w.base.mem # 0, 20);
		IF w.pos >= w.base.len THEN
			w.base.len := w.pos + 1;
			size := WinApi.GlobalSize(w.base.mem);
			IF size < w.base.len THEN
				w.base.mem := WinApi.GlobalReAlloc(w.base.mem, w.base.len + 1024,
										WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE)
			END
		END;
		p := SYSTEM.VAL(MemPtr, WinApi.GlobalLock(w.base.mem));
		p[w.pos] := x; INC(w.pos);
		res := WinApi.GlobalUnlock(w.base.mem)
	END WriteByte;
	
	PROCEDURE (w: MemWriter) WriteBytes (IN x: ARRAY OF BYTE; beg, len: INTEGER);
		VAR res, size: INTEGER; p: MemPtr;
	BEGIN
		ASSERT(w.base.mem # 0, 20);
		ASSERT(beg >= 0, 21);
		IF len > 0 THEN
			ASSERT(beg + len <= LEN(x), 23);
			IF w.pos + len > w.base.len THEN
				w.base.len := w.pos + len;
				size := WinApi.GlobalSize(w.base.mem);
				IF size < w.base.len THEN
					w.base.mem := WinApi.GlobalReAlloc(w.base.mem, w.base.len + 1024,
											WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE)
				END
			END;
			p := SYSTEM.VAL(MemPtr, WinApi.GlobalLock(w.base.mem));
			SYSTEM.MOVE(SYSTEM.ADR(x[beg]), SYSTEM.ADR(p[w.pos]), len);
			INC(w.pos, len);
			res := WinApi.GlobalUnlock(w.base.mem)
		ELSE ASSERT(len = 0, 22)
		END
	END WriteBytes;
	
	PROCEDURE (f: MemFile) Length (): INTEGER;
	BEGIN
		RETURN f.len
	END Length;
	
	PROCEDURE (f: MemFile) NewReader (old: Files.Reader): Files.Reader;
		VAR r: MemReader;
	BEGIN
		ASSERT(f.mem # 0, 20);
		IF (old = NIL) OR ~(old IS MemReader) THEN NEW(r) 
		ELSE r := old(MemReader) 
		END;
		r.base := f;
		r.pos := 0; r.eof := FALSE;
		RETURN r
	END NewReader;
	
	PROCEDURE (f: MemFile) NewWriter (old: Files.Writer): Files.Writer;
		VAR w: MemWriter;
	BEGIN
		ASSERT(f.mem # 0, 20);
		IF (old = NIL) OR ~(old IS MemWriter) THEN NEW(w) 
		ELSE w := old(MemWriter) 
		END;
		w.base := f;
		w.pos := f.len;
		RETURN w
	END NewWriter;
	
	PROCEDURE (f: MemFile) Register (name: Files.Name; type: Files.Type; ask: BOOLEAN; OUT res: INTEGER);
	BEGIN
		res := -1
	END Register;
	
	PROCEDURE (f: MemFile) Close;
	END Close;
	
	PROCEDURE (f: MemFile) Flush;
	END Flush;
	
	PROCEDURE (f: MemFile) FINALIZE;
	BEGIN
		IF f.owner THEN f.mem := WinApi.GlobalFree(f.mem) END
	END FINALIZE;
	
	PROCEDURE NewMemFile (mem: WinApi.HGLOBAL; owner: BOOLEAN): Files.File;
		VAR f: MemFile;
	BEGIN
		NEW(f);
		f.mem := mem;
		f.len := WinApi.GlobalSize(mem);
		f.owner := owner;
		RETURN f
	END NewMemFile;
	
	
	(* standard exporters *)
	
	PROCEDURE ExportNative* (
		v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
	);
		VAR hnd: WinApi.HGLOBAL; f: Files.File; wr: Stores.Writer; res: COM.RESULT;
	BEGIN
		ASSERT(med.tymed = {}, 20);
		hnd := WinApi.GlobalAlloc(WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE, 1024);
		f := NewMemFile(hnd, FALSE);
		wr.ConnectTo(f);
		wr.SetPos(0);
		wr.WriteInt(obfTag);
		wr.WriteInt(0);
		wr.WriteInt(w);
		wr.WriteInt(h);
		IF isSingle THEN wr.WriteSChar(1X) ELSE wr.WriteSChar(0X) END;
		wr.WriteStore(v);
		GenGlobalMedium(hnd, NIL, med)
	END ExportNative;
	
	PROCEDURE ExportInfo* (
		v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
	);
		VAR hnd: WinApi.HGLOBAL; p: Info; res: INTEGER;
	BEGIN
		ASSERT(med.tymed = {}, 20);
		hnd := WinApi.GlobalAlloc(WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE, SIZE(InfoDesc));
		IF hnd # 0 THEN
			p := SYSTEM.VAL(Info, WinApi.GlobalLock(hnd));
			Services.GetTypeName(v, p.type);
			p.w := w; p.h := h; p.rx := x; p.ry := y;
			p.isSingle := isSingle;
			res := WinApi.GlobalUnlock(hnd);
			GenGlobalMedium(hnd, NIL, med)
		END
	END ExportInfo;
	
	PROCEDURE ExportPicture* (
		v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
	);
		VAR dc: WinApi.HDC; mf: WinApi.HMETAFILE; mp: WinApi.PtrMETAFILEPICT;
			rc: WinApi.RECT; res: INTEGER; hm: WinApi.HMETAFILEPICT;
			bp: Properties.BoundsPref; sp: Properties.SizePref;
	BEGIN
		ASSERT(med.tymed = {}, 20);
		IF (w = Views.undefined) OR (h = Views.undefined) THEN
			bp.w := Views.undefined; bp.h := Views.undefined;
			Views.HandlePropMsg(v, bp);
			w := bp.w; h := bp.h
		END;
		IF (w = Views.undefined) OR (h = Views.undefined) THEN
			sp.w := Views.undefined; sp.h := Views.undefined;
			sp.fixedW := FALSE; sp.fixedH := FALSE;
			Views.HandlePropMsg(v, sp);
			w := sp.w; h := sp.h
		END;
(*
		w := w DIV (Ports.mm DIV 100); h := h DIV (Ports.mm DIV 100);	(* w, h in mm/100 *)
*)
		dc := WinApi.CreateMetaFileW(NIL);
		IF dc # 0 THEN
			res := WinApi.SetMapMode(dc, WinApi.MM_ANISOTROPIC);
			res := WinApi.SetWindowOrgEx(dc, 0, 0, NIL);
			res := WinApi.SetWindowExtEx(dc, w DIV unit, h DIV unit, NIL);
			IF v # NIL THEN Paint(dc, v, w DIV unit, h DIV unit, unit) END;
			mf := WinApi.CloseMetaFile(dc);
			IF mf # 0 THEN
				hm := WinApi.GlobalAlloc(
					WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE, SIZE(WinApi.METAFILEPICT));
				IF hm # 0 THEN
					mp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(hm));
					mp.hMF := mf;
					mp.mm := WinApi.MM_ANISOTROPIC;
					mp.xExt := w DIV (Ports.mm DIV 100);	(* himetric units *)
					mp.yExt := h DIV (Ports.mm DIV 100);
					res := WinApi.GlobalUnlock(hm);
					GenMetafileMedium(hm, NIL, med)
				ELSE res := WinApi.DeleteMetaFile(mf)
				END
			END
		END
	END ExportPicture;
	
	
	(* standard importers *)
	
	PROCEDURE ImportNative* (VAR med: WinOle.STGMEDIUM; OUT v: Views.View;
											OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
		VAR hnd: WinApi.HGLOBAL; f: Files.File; r: Stores.Reader; s: Stores.Store;
			tag, version, res: COM.RESULT; ch: SHORTCHAR;
	BEGIN
		v := NIL;
		hnd := MediumGlobal(med);
		med.u.hGlobal := 0;
		f := NewMemFile(hnd, TRUE);
		r.ConnectTo(f); r.SetPos(0);
		r.ReadInt(tag);
		IF tag = obfTag THEN
			r.ReadInt(version);
			r.ReadInt(w);
			r.ReadInt(h);
			r.ReadSChar(ch); isSingle := ch # 0X;
			r.ReadStore(s);
			v := s(Views.View)
		END
	END ImportNative;
	
	PROCEDURE ImportInfo* (VAR med: WinOle.STGMEDIUM; VAR type: Stores.TypeName;
										OUT w, h, rx, ry: INTEGER; OUT isSingle: BOOLEAN);
		VAR s: Stores.Store; hnd: WinApi.HANDLE; p: Info; res: INTEGER;
	BEGIN
		hnd := MediumGlobal(med);
		IF hnd # 0 THEN
			p := SYSTEM.VAL(Info, WinApi.GlobalLock(hnd));
			type := p.type;
			w := p.w; h := p.h; rx := p.rx; ry := p.ry;
			isSingle := p.isSingle;
			res := WinApi.GlobalUnlock(hnd)
		END
	END ImportInfo;
	
	
	(* IDataObject *)
	
	PROCEDURE (this: IDataObject) GetData* (IN format: WinOle.FORMATETC;
															OUT medium: WinOle.STGMEDIUM): COM.RESULT;
		VAR c: Converter; val: ExpVal; ok: BOOLEAN;
	BEGIN
		c := convList;
		WHILE (c # NIL) & ((c.exp = "")
								OR ~Compatible(c, this.view, this.isSingle)
								OR ~Eql(c.format, format)) DO c := c.next END;
		IF c # NIL THEN
			GetCommand(c.exp, val, ok);
			medium.tymed := {};
			IF ok THEN
				IF this.useSel & ~(info IN c.opts) THEN Setup(this) END;
				val.p(this.view, this.w, this.h, this.rx, this.ry, this.isSingle, medium);
				RETURN  WinApi.S_OK
			ELSE
				IF debug THEN Log.String(c.exp); Log.String(" failed"); Log.Ln END;
				RETURN WinApi.E_UNEXPECTED
			END
		ELSE RETURN WinApi.DV_E_FORMATETC
		END
	END GetData;
	
	PROCEDURE (this: IDataObject) GetDataHere* (IN format: WinOle.FORMATETC;
																	VAR medium: WinOle.STGMEDIUM): COM.RESULT;
		VAR c: Converter; val: ExpVal; ok: BOOLEAN;
	BEGIN
		IF format.tymed * (WinOle.TYMED_ISTORAGE + WinOle.TYMED_ISTREAM) # {} THEN
			c := convList;
			WHILE (c # NIL) & ((c.exp = "")
									OR ~Compatible(c, this.view, this.isSingle)
									OR ~Eql(c.format, format)) DO c := c.next END;
			IF (c # NIL) & (medium.tymed = c.format.tymed) THEN
				GetCommand(c.exp, val, ok);
				IF ok THEN
					IF this.useSel & ~(info IN c.opts) THEN Setup(this) END;
					val.p(this.view, this.w, this.h, this.rx, this.ry, this.isSingle, medium);
					RETURN WinApi.S_OK
				ELSE
					IF debug THEN Log.String(c.exp); Log.String(" failed"); Log.Ln END;
					RETURN WinApi.E_UNEXPECTED
				END
			ELSE RETURN WinApi.DV_E_FORMATETC
			END
		ELSE RETURN WinApi.DV_E_FORMATETC
		END
	END GetDataHere;
	
	PROCEDURE (this: IDataObject) QueryGetData* (IN format: WinOle.FORMATETC): COM.RESULT;
		VAR c: Converter;
	BEGIN
		c := convList;
		WHILE (c # NIL) & ((c.exp = "")
									OR ~Compatible(c, this.view, this.isSingle)
									OR ~Eql(c.format, format)) DO c := c.next END;
		IF c # NIL THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.DV_E_FORMATETC
		END
	END QueryGetData;
	
	PROCEDURE (this: IDataObject) GetCanonicalFormatEtc* (IN formatIn: WinOle.FORMATETC;
																			OUT formatOut: WinOle.FORMATETC): COM.RESULT;
	BEGIN
		RETURN WinApi.DATA_S_SAMEFORMATETC
	END GetCanonicalFormatEtc;
	
	PROCEDURE (this: IDataObject) SetData* (IN format: WinOle.FORMATETC;
															IN medium: WinOle.STGMEDIUM; release: WinApi.BOOL): COM.RESULT;
	BEGIN
		RETURN WinApi.DV_E_FORMATETC
	END SetData;
	
	PROCEDURE (this: IDataObject) EnumFormatEtc* (direction: SET;
																	OUT enum: WinOle.IEnumFORMATETC): COM.RESULT;
		VAR format: ARRAY 32 OF WinOle.FORMATETC; p: WinOle.IEnumFORMATETC;
			n, i: INTEGER; c: Converter;
	BEGIN
		IF direction = WinOle.DATADIR_GET THEN
			n := 0; c := convList;
			WHILE (n < LEN(format)) & (c # NIL) DO
				IF (c.exp # "") & Compatible(c, this.view, this.isSingle) THEN
					format[n] := c.format; i := 0;
					WHILE ~Equiv(format[i], c.format) DO INC(i) END;
					IF i = n THEN INC(n)
					ELSE format[i].tymed := format[i].tymed + c.format.tymed
					END
				END;
				c := c.next
			END;
			IF n > 0 THEN
				CreateIEnumFORMATETC(n, format, p); 
				IF p # NIL THEN enum := p; RETURN WinApi.S_OK
				ELSE RETURN WinApi.E_OUTOFMEMORY
				END
			ELSE RETURN WinApi.E_FAIL
			END
		ELSE
			RETURN WinApi.E_NOTIMPL
		END
	END EnumFormatEtc;
	
	PROCEDURE (this: IDataObject) DAdvise* (IN format: WinOle.FORMATETC; flags: SET;
															advSink: WinOle.IAdviseSink; OUT connection: INTEGER): COM.RESULT;
	BEGIN
		RETURN WinApi.E_FAIL
	END DAdvise;
	
	PROCEDURE (this: IDataObject) DUnadvise* (connection: INTEGER): COM.RESULT;
	BEGIN
		RETURN WinApi.E_FAIL
	END DUnadvise;
	
	PROCEDURE (this: IDataObject) EnumDAdvise* (OUT enum: WinOle.IEnumSTATDATA): COM.RESULT;
	BEGIN
		RETURN WinApi.E_FAIL
	END EnumDAdvise;
	
	
	(* import/export *)
	
	PROCEDURE DataConvTo* (data: WinOle.IDataObject; type: Stores.TypeName): BOOLEAN;
		VAR t: Stores.TypeName; c: Converter; v: Views.View; res: COM.RESULT; ok, s: BOOLEAN;
			med: WinOle.STGMEDIUM; ival: InfoVal; w, h, x, y: INTEGER;
	BEGIN
		v := NIL; c := convList;
		WHILE c # NIL DO
			IF c.imp # "" THEN
				IF (type = "") OR (c.type # "") & Services.Extends(c.type, type) THEN
					IF data.QueryGetData(c.format) = WinApi.S_OK THEN RETURN TRUE END
				ELSIF info IN c.opts THEN
					res := data.GetData(c.format, med);
					IF res >= 0 THEN
						GetCommand(c.imp, ival, ok); t := "";
						IF ok THEN ival.p(med, t, w, h, x, y, s) 
						ELSIF debug THEN Log.String(c.imp); Log.String(" failed (c)"); Log.Ln
						END;
						WinOle.ReleaseStgMedium(med);
						IF t = type THEN RETURN TRUE END
					END
				END
			END;
			c := c.next
		END;
		RETURN FALSE
	END DataConvTo;
	
	PROCEDURE GetDataView* (data: WinOle.IDataObject; type: Stores.TypeName; OUT v: Views.View;
											OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
		VAR t: Stores.TypeName; c: Converter; val: ImpVal; ival: InfoVal; ok, s: BOOLEAN;
			res: COM.RESULT; med: WinOle.STGMEDIUM; x, y: INTEGER;
	BEGIN
		v := NIL; t := ""; c := convList;
		WHILE (c # NIL) & (v = NIL) DO
			IF c.imp # "" THEN
				IF info IN c.opts THEN
					IF type # "" THEN
						res := data.GetData(c.format, med); t := "";
						IF debug THEN Log.String("Get Data "); Log.String(c.imp); Log.String(c.exp); Log.Int(res); Log.Ln
						END;
						IF res >= 0 THEN
							GetCommand(c.imp, ival, ok);
							IF ok THEN ival.p(med, t, w, h, x, y, s)
							ELSIF debug THEN Log.String(c.imp); Log.String(" failed (i)"); Log.Ln
							END;
							WinOle.ReleaseStgMedium(med)
						END
					END
				ELSIF (type = "") OR (c.type # "") & Services.Extends(c.type, type)
									OR (c.type = "") & (t # "") & Services.Extends(t, type) THEN
					IF debug THEN Log.String("query"); Log.Int(data.QueryGetData(c.format)); Log.Ln END;
					res := data.GetData(c.format, med);
					IF debug THEN Log.String("Get Data "); Log.String(c.imp); Log.String(c.exp); Log.Int(res); Log.Ln END;
					IF res >= 0 THEN
						GetCommand(c.imp, val, ok);
						IF ok THEN
							dataObj := data; val.p(med, v, w, h, isSingle); dataObj := NIL
						ELSIF debug THEN Log.String(c.imp); Log.String(" failed (g)"); Log.Ln
						END;
						WinOle.ReleaseStgMedium(med)
					END
				END
			END;
			c := c.next
		END
	END GetDataView;
	
	PROCEDURE GetDataViewUsing* (data: WinOle.IDataObject; c: Converter; OUT v: Views.View;
													OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
		VAR val: ImpVal; ok: BOOLEAN; res: COM.RESULT; med: WinOle.STGMEDIUM;
	BEGIN
		ASSERT(c # NIL, 20);
		ASSERT(c.imp # "", 21);
		ASSERT(~(info IN c.opts), 22);
		v := NIL;
		res := data.GetData(c.format, med);
		IF debug THEN Log.String("Get Data "); Log.String(c.imp); Log.String(c.exp); Log.Int(res); Log.Ln END;
		IF res >= 0 THEN
			GetCommand(c.imp, val, ok);
			IF ok THEN
				dataObj := data; val.p(med, v, w, h, isSingle); dataObj := NIL
			ELSIF debug THEN Log.String(c.imp); Log.String(" failed (g)"); Log.Ln
			END;
			WinOle.ReleaseStgMedium(med)
		END
	END GetDataViewUsing;
	
	PROCEDURE GetTextDataView* (data: WinOle.IDataObject; OUT v: Views.View;
													OUT w, h: INTEGER; OUT isSingle: BOOLEAN);
		VAR c: Converter;
	BEGIN
		v := NIL; c := convList;
		WHILE (c # NIL) & (c.imp # "OleData.ImportNative") DO c := c.next END;
		IF c # NIL THEN GetDataViewUsing(data, c, v, w, h, isSingle) END;
		IF v = NIL THEN
			c := convList;
			WHILE (c # NIL) & (c.imp # "HostTextConv.ImportDRichText") DO c := c.next END;
			IF c # NIL THEN GetDataViewUsing(data, c, v, w, h, isSingle) END;
		END;
		IF v = NIL THEN
			GetDataView(data, "", v, w, h, isSingle)
		END
	END GetTextDataView;
	
	PROCEDURE GetDataType* (data: WinOle.IDataObject; OUT type: Stores.TypeName;
												OUT w, h, x, y: INTEGER; OUT isSingle: BOOLEAN);
		VAR t: Stores.TypeName; c: Converter; res: COM.RESULT; med: WinOle.STGMEDIUM;
			ival: InfoVal; ok: BOOLEAN;
	BEGIN
		type := ""; c := convList;
		WHILE (c # NIL) & (type = "") DO
			IF c.imp # "" THEN
				IF info IN c.opts THEN
					res := data.GetData(c.format, med);
					IF res >= 0 THEN
						GetCommand(c.imp, ival, ok);
						IF ok THEN ival.p(med, type, w, h, x, y, isSingle)
						ELSIF debug THEN Log.String(c.imp); Log.String(" failed (t)"); Log.Ln
						END;
						WinOle.ReleaseStgMedium(med)
					END
				ELSIF c.type # "" THEN
					IF data.QueryGetData(c.format) = WinApi.S_OK THEN
						type := c.type; isSingle := FALSE;
						w := 0; h := 0; x := 0; y := 0
					END
				END
			END;
			c := c.next
		END
	END GetDataType;
	
	
	(* creation *)
	
	PROCEDURE ViewData* (v: Views.View; w, h: INTEGER; isSingle: BOOLEAN): IDataObject;
		VAR new: IDataObject;
	BEGIN
		NEW(new); new.view := v; new.w := w; new.h := h; new.rx := 0; new.ry := 0;
		new.isSingle := isSingle; new.useSel := FALSE;
		IF v # NIL THEN Services.GetTypeName(v, new.type) ELSE new.type := "*" END;
		RETURN new
	END ViewData;
	
	PROCEDURE ViewDropData* (v: Views.View; w, h, rx, ry: INTEGER; isSingle, useSel: BOOLEAN): IDataObject;
		VAR new: IDataObject;
	BEGIN
		IF useSel THEN
			ASSERT(~isSingle, 20);
			ASSERT(v IS Containers.View, 21);
			ASSERT(v(Containers.View).ThisController() # NIL, 22)
		END;
		NEW(new); new.view := v; new.w := w; new.h := h; new.rx := rx; new.ry := ry;
		new.isSingle := isSingle; new.useSel := useSel;
		IF v # NIL THEN Services.GetTypeName(v, new.type) ELSE new.type := "*" END;
		RETURN new
	END ViewDropData;
	
	PROCEDURE SetView* (data: IDataObject; v: Views.View; w, h: INTEGER);
	BEGIN
		data.view := v; data.w := w; data.h := h;
		IF v # NIL THEN Services.GetTypeName(v, data.type) ELSE data.type := "*" END;
	END SetView;
	
	
	(* registration *)
	
	PROCEDURE Register* (imp, exp, format: Dialog.String; type: Stores.TypeName; opts: SET);
		VAR c, f: Converter; tymed: SET; cbf: SHORTINT;
	BEGIN
		tymed := WinOle.TYMED_HGLOBAL;
		IF format = "TEXT" THEN cbf := WinApi.CF_TEXT
		ELSIF format = "BITMAP" THEN cbf := WinApi.CF_BITMAP; tymed := WinOle.TYMED_GDI
		ELSIF format = "METAFILEPICT" THEN cbf := WinApi.CF_METAFILEPICT; tymed := WinOle.TYMED_MFPICT
		ELSIF format = "SYLK" THEN cbf := WinApi.CF_SYLK
		ELSIF format = "DIF" THEN cbf := WinApi.CF_DIF
		ELSIF format = "TIFF" THEN cbf := WinApi.CF_TIFF
		ELSIF format = "OEMTEXT" THEN cbf := WinApi.CF_OEMTEXT
		ELSIF format = "DIB" THEN cbf := WinApi.CF_DIB; tymed := WinOle.TYMED_GDI
		ELSIF format = "PALETTE" THEN cbf := WinApi.CF_PALETTE
		ELSIF format = "PENDATA" THEN cbf := WinApi.CF_PENDATA
		ELSIF format = "RIFF" THEN cbf := WinApi.CF_RIFF
		ELSIF format = "WAVE" THEN cbf := WinApi.CF_WAVE
		ELSIF format = "UNICODETEXT" THEN cbf := WinApi.CF_UNICODETEXT
		ELSIF format = "ENHMETAFILE" THEN cbf := WinApi.CF_ENHMETAFILE; tymed := WinOle.TYMED_ENHMF
		ELSIF format = "HDROP" THEN cbf := WinApi.CF_HDROP
		ELSIF format = "LOCALE" THEN cbf := WinApi.CF_LOCALE
		ELSE cbf := SHORT(WinApi.RegisterClipboardFormatW(format))
		END;
		IF stream IN opts THEN tymed := WinOle.TYMED_ISTREAM
		ELSIF storage IN opts THEN tymed := WinOle.TYMED_ISTORAGE
		ELSIF file IN opts THEN tymed := WinOle.TYMED_FILE
		END;
		NEW(c); c.imp := imp; c.exp := exp; c.type := type; c.opts := opts;
		GenFormatEtc(cbf, WinOle.DVASPECT_CONTENT, tymed, c.format);
		IF convList = NIL THEN convList := c
		ELSE f := convList;
			WHILE f.next # NIL DO f := f.next END;
			f.next := c
		END
	END Register;


	(* debug *)
	
	PROCEDURE DumpData* (data: WinOle.IDataObject);
		VAR type: Stores.TypeName; c: Converter; val: ImpVal; ival: InfoVal; ok, s: BOOLEAN;
			res: COM.RESULT; med: WinOle.STGMEDIUM; w, h, x, y: INTEGER; v: Views.View;
	BEGIN
		c := convList;
		WHILE c # NIL DO
			IF c.imp # "" THEN
				res := data.QueryGetData(c.format);
				IF res >= 0 THEN
					Log.String(c.imp); Log.Char(" ");
					res := data.GetData(c.format, med); 
					IF res >= 0 THEN
						Log.String(" read ");
						IF info IN c.opts THEN
							Log.String("(i) ");
							GetCommand(c.imp, ival, ok);
							IF ok THEN
								type := "";
								ival.p(med, type, w, h, x, y, s);
								Log.String(type);
								Log.Int(w); Log.Int(h); Log.Int(x); Log.Int(y);
								IF s THEN Log.String(" singleton") END
							ELSE
								Log.String("failed");
							END
						ELSE
							GetCommand(c.imp, val, ok);
							IF ok THEN
								v := NIL;
								dataObj := data; val.p(med, v, w, h, s); dataObj := NIL;
								IF v # NIL THEN
									Services.GetTypeName(v, type);
									Log.String(type);
									Log.Int(w); Log.Int(h);
									IF s THEN Log.String(" singleton") END
								ELSE
									Log.String("NIL")
								END
							ELSE
								Log.String("failed");
							END
						END;
						WinOle.ReleaseStgMedium(med);
						Log.Ln
					END
				END
			END;
			c := c.next
		END
	END DumpData;
	
	
	PROCEDURE Init;
		VAR res: INTEGER; dc: WinApi.HDC;
	BEGIN
		dc := WinApi.GetDC(0);
		unit := 914400 DIV WinApi.GetDeviceCaps(dc, 90);
		res := WinApi.ReleaseDC(0, dc)
	END Init;

BEGIN
	Init
END OleData.
