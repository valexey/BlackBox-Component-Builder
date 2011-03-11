MODULE HostBitmaps;
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
		SYSTEM, WinApi, WinOle,
		Files, Fonts, Ports, Stores, Models, Views, Controllers, Containers, Properties, Dialog, Converters,
		HostPorts, HostWindows;

	CONST
		minVersion = 0; maxVersion = 1;

	TYPE
		Model = POINTER TO RECORD
			file: Files.File;
			pos, len: INTEGER;
			ref: WinApi.HBITMAP
		END;

		StdView = POINTER TO RECORD (Views.View)
			model: Model;
			w, h: INTEGER;	(* in pixels *)
			bits: INTEGER;	(* bit per pixel *)
		END;
		
		BITMAPINFO8 = RECORD [untagged]
			header: WinApi.BITMAPINFOHEADER;
			colors: ARRAY 256 OF INTEGER
		END;
		
		RootContext = POINTER TO RECORD (Models.Context)
			w, h: INTEGER
		END;
		
		RootView = POINTER TO RECORD (Views.View)
			view: Views.View;
		END;
		
	
	(* helpers for painting to bitmap *)	
	
	PROCEDURE (c: RootContext) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL
	END ThisModel;

	PROCEDURE (c: RootContext) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.w; h := c.h
	END GetSize;
		
	PROCEDURE (c: RootContext) Normalize (): BOOLEAN;
	BEGIN
		RETURN TRUE
	END Normalize;
	
	PROCEDURE (d: RootView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Views.InstallFrame(f, d.view, 0, 0, 0, FALSE)
	END Restore;
	
	PROCEDURE (d: RootView) GetNewFrame (VAR frame: Views.Frame);
		VAR f: Views.RootFrame;
	BEGIN
		NEW(f); frame := f
	END GetNewFrame;

	PROCEDURE (d: RootView) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;
	
	PROCEDURE Paint (dc: WinApi.HDC; v: Views.View; w, h, unit: INTEGER);
		VAR d: RootView; c: RootContext; p: HostPorts.Port; f: Views.RootFrame; g: Views.Frame;
	BEGIN
		NEW(p);
		p.Init(unit, Ports.screen);
		p.SetSize(w, h);
		p.SetDC(dc, 0);
		NEW(c);
		c.w := w * p.unit;
		c.h := h * p.unit;
		NEW(d);
		d.view := Views.CopyOf(v, Views.shallow);
		Stores.Join(d, d.view);
		d.InitContext(c);
		d.view.InitContext(c);
		Stores.InitDomain(d);
		d.GetNewFrame(g); f := g(Views.RootFrame); f.ConnectTo(p);
		Views.SetRoot(f, d, FALSE, {});
		Views.AdaptRoot(f);
		Views.RestoreRoot(f, 0, 0, c.w, c.h);
	END Paint;
	

	
	PROCEDURE GetHeader (v: StdView; OUT hdr: WinApi.BITMAPINFOHEADER; OUT colors: INTEGER);
	BEGIN
		hdr.biSize := SIZE(WinApi.BITMAPINFOHEADER);
		hdr.biWidth := v.w;
		hdr.biHeight := v.h;
		hdr.biPlanes := 1;
		hdr.biBitCount := SHORT(v.bits);
		IF v.bits = 8 THEN	(* use 8 bit & run length encoding *)
			hdr.biCompression := WinApi.BI_RLE8;
			colors := 256
		ELSIF v.bits = 24 THEN	(* use 24 bit true color *)
			hdr.biCompression := WinApi.BI_RGB;
			colors := 0
		ELSIF v.bits = 0 THEN	(* use jpeg *)
			hdr.biCompression := 4 (* WinApi.BI_JPEG *);
			colors := 0
		ELSE
			HALT(100)	(* unsupported format *)
		END;
		hdr.biSizeImage := 0;
		hdr.biXPelsPerMeter := 0;
		hdr.biYPelsPerMeter := 0;
		hdr.biClrUsed := 0;
		hdr.biClrImportant := 0
	END GetHeader;
	
	PROCEDURE Evaluate (v: StdView; dc: WinApi.HDC);
		VAR len, adr, res, colors, i: INTEGER;
			rd: Stores.Reader; info: BITMAPINFO8; data: POINTER TO ARRAY OF BYTE;
	BEGIN
		rd.ConnectTo(v.model.file);
		rd.SetPos(v.model.pos);
		len := v.model.len;
		GetHeader(v, info.header, colors);
		i := 0; WHILE i < colors DO rd.ReadInt(info.colors[i]); INC(i); DEC(len, 4) END;
		NEW(data, len);
		rd.rider.ReadBytes(data^, 0, len);
		v.model.ref := WinApi.CreateCompatibleBitmap(dc, v.w, v.h);
		info.header.biSizeImage := len;
		res := WinApi.SetDIBits(dc, v.model.ref, 0, v.h, SYSTEM.ADR(data[0]),
			SYSTEM.VAL(WinApi.BITMAPINFO, info), WinApi.DIB_RGB_COLORS);
		IF res = 0 THEN
			res := WinApi.GetLastError();
			IF res = WinApi.ERROR_NOT_ENOUGH_MEMORY THEN HALT(101) ELSE HALT(100) END
		END;
		ASSERT(v.model.ref # 0, 102)
	END Evaluate;


	(* Model *)
	
	PROCEDURE (m: Model) FINALIZE;
		VAR res: INTEGER;
	BEGIN
		IF m.ref # 0 THEN
			res := WinApi.DeleteObject(m.ref);
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
		rd.ReadInt(v.w);
		rd.ReadInt(v.h);
		IF thisVersion > 0 THEN rd.ReadInt(v.bits) ELSE v.bits := 24 END;
		NEW(m); m.file := rd.rider.Base();
		rd.ReadInt(m.len);
		m.pos := rd.Pos();
		m.ref := 0;	(* lazy allocation of bitmap data *)
		v.model := m;
		rd.SetPos(m.pos + m.len)
	END Internalize;

	PROCEDURE (v: StdView) Externalize (VAR wr: Stores.Writer);
		VAR len, res, colors, i: INTEGER;
			r: Files.Reader; b: BYTE; info: BITMAPINFO8; data: POINTER TO ARRAY OF BYTE;
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(maxVersion);
		wr.WriteInt(v.w);
		wr.WriteInt(v.h);
		wr.WriteInt(v.bits);
		IF v.model.file # NIL THEN
			len := v.model.len;
			wr.WriteInt(len);
			r := v.model.file.NewReader(NIL); r.SetPos(v.model.pos);
			WHILE len # 0 DO r.ReadByte(b); wr.WriteSChar(SHORT(CHR(b))); DEC(len) END
		ELSE
			ASSERT(v.model.ref # 0, 100);
			GetHeader(v, info.header, colors);
			res := WinApi.GetDIBits(WinApi.GetDC(HostWindows.main), v.model.ref, 0, v.h, 0,
				SYSTEM.VAL(WinApi.BITMAPINFO, info), WinApi.DIB_RGB_COLORS);
			IF res = 0 THEN res := WinApi.GetLastError(); HALT(100) END;
			len := info.header.biSizeImage;
			NEW(data, len);
			res := WinApi.GetDIBits(WinApi.GetDC(HostWindows.main), v.model.ref, 0, v.h,
				SYSTEM.ADR(data[0]),
				SYSTEM.VAL(WinApi.BITMAPINFO, info), WinApi.DIB_RGB_COLORS);
			IF res = 0 THEN res := WinApi.GetLastError(); HALT(100) END;
			INC(len, 4 * colors);
			wr.WriteInt(len);
			i := 0; WHILE i < colors DO wr.WriteInt(info.colors[i]); INC(i) END;
			wr.rider.WriteBytes(data^, 0, LEN(data));
			v.model.len := len
		END;
	END Externalize;

	PROCEDURE (v: StdView) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: StdView DO
			v.model := source.model;
			v.w := source.w;
			v.h := source.h;
			v.bits := source.bits;
		END
	END CopyFromSimpleView;

	PROCEDURE (v: StdView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR rd: HostPorts.Rider; res, w, h: INTEGER; dc, pdc, bdc, bdc1: WinApi.HDC; memBM: WinApi.HBITMAP;
	BEGIN
		ASSERT(v.model # NIL, 20);
		v.context.GetSize(w, h);
		dc := f.rider(HostPorts.Rider).port.homedc;
		IF WinApi.WindowFromDC(dc) = 0 THEN dc := WinApi.GetDC(HostWindows.main) END;
		IF v.model.ref = 0 THEN Evaluate(v, dc) END;
		IF Views.IsPrinterFrame(f) THEN (* need to make a print copy of the existing bitmap *)
			bdc1 := WinApi.CreateCompatibleDC(dc);
			res := WinApi.SelectObject(bdc1, v.model.ref);
			pdc := f.rider(HostPorts.Rider).port.dc;
			bdc := WinApi.CreateCompatibleDC(pdc);
			memBM := WinApi.CreateCompatibleBitmap(pdc, v.w, v.h);
			res := WinApi.SelectObject(bdc, memBM);
			res := WinApi.BitBlt(bdc, 0, 0, v.w, v.h, bdc1, 0, 0, 00CC0020H);	(* copy *)
			res := WinApi.DeleteDC(bdc1)
		ELSE
			bdc := WinApi.CreateCompatibleDC(dc);
			res := WinApi.SelectObject(bdc, v.model.ref)
		END;
		f.rider(HostPorts.Rider).DrawBitmap(bdc, v.w, v.h, f.gx, f.gy, w, h);
		res := WinApi.DeleteDC(bdc)
	END Restore;

	PROCEDURE (v: StdView) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w > Views.undefined) & (msg.h > Views.undefined) THEN
				Properties.ProportionalConstraint(v.w, v.h, msg.fixedW, msg.fixedH, msg.w, msg.h)
			ELSE
				IF (v.w > 0) & (v.h > 0) THEN	(* default sizes *)
					msg.w := v.w * HostWindows.unit; msg.h := v.h * HostWindows.unit
				END
			END
		ELSE
		END
	END HandlePropMsg;
	

	PROCEDURE TurnToBitmap* (bits: INTEGER);
		VAR v: Views.View; f: Views.Frame; rd: HostPorts.Rider; dc, bdc: WinApi.HDC; bm: WinApi.HBITMAP;
			res, w, h: INTEGER; obj: StdView;
	BEGIN
		ASSERT((bits = 0) OR (bits = 8) OR (bits = 24), 20);
		v := Containers.FocusSingleton();
		IF v # NIL THEN
			f := Controllers.FocusFrame();
			f := Views.ThisFrame(f, v);
			rd := f.rider(HostPorts.Rider);
			dc := rd.port.homedc;
			bdc := WinApi.CreateCompatibleDC(dc);
			res := WinApi.SetBkMode(bdc, WinApi.TRANSPARENT);
			v.context.GetSize(w, h);
			w := w DIV f.unit; h := h DIV f.unit;
			bm := WinApi.CreateCompatibleBitmap(dc, w, h);
			res := WinApi.SelectObject(bdc, bm);
			Paint(bdc, v, w, h, f.unit);
			res := WinApi.DeleteDC(bdc);
			NEW(obj); obj.w := w; obj.h := h; obj.bits := bits;
			NEW(obj.model); obj.model.ref := bm;
			Containers.Focus().ThisView().ThisModel().ReplaceView(v, obj)
		END
	END TurnToBitmap;
(*
	PROCEDURE TurnThisToBitmap* (v: Views.View; owner: Containers.Model; bits: INTEGER);
		VAR dc, bdc: WinApi.HDC; bm: WinApi.HBITMAP; res, w, h: INTEGER; obj: StdView;
	BEGIN
		dc := WinApi.GetDC(HostWindows.main);
		bdc := WinApi.CreateCompatibleDC(dc);
		res := WinApi.SetBkMode(bdc, WinApi.TRANSPARENT);
		v.context.GetSize(w, h);
		w := w DIV HostWindows.unit; h := h DIV HostWindows.unit;
		bm := WinApi.CreateCompatibleBitmap(dc, w, h);
		res := WinApi.SelectObject(bdc, bm);
		Paint(bdc, v, w, h, HostWindows.unit);
		res := WinApi.DeleteDC(bdc);
		NEW(obj); obj.w := w; obj.h := h; obj.bits := bits;
		NEW(obj.model); obj.model.ref := bm;
		owner.ReplaceView(v, obj)
	END TurnThisToBitmap;
*)
	PROCEDURE ViewToBitmap* (v: Views.View; bits: INTEGER): Views.View;
		VAR dc, bdc: WinApi.HDC; bm: WinApi.HBITMAP; res, w, h: INTEGER; obj: StdView;
	BEGIN
		ASSERT(v # NIL, 20);
		IF v IS StdView THEN
			RETURN v
		ELSE
			ASSERT((bits = 0) OR (bits = 8) OR (bits = 24), 21);
			dc := WinApi.GetDC(HostWindows.main);
			bdc := WinApi.CreateCompatibleDC(dc);
			res := WinApi.SetBkMode(bdc, WinApi.TRANSPARENT);
			v.context.GetSize(w, h);
			w := w DIV HostWindows.unit; h := h DIV HostWindows.unit;
			bm := WinApi.CreateCompatibleBitmap(dc, w, h);
			res := WinApi.SelectObject(bdc, bm);
			Paint(bdc, v, w, h, HostWindows.unit);
			res := WinApi.DeleteDC(bdc);
			NEW(obj); obj.w := w; obj.h := h; obj.bits := bits;
			NEW(obj.model); obj.model.ref := bm;
			RETURN obj
		END
	END ViewToBitmap;


	PROCEDURE ImportBitmap* (f: Files.File; OUT s: Stores.Store);
		TYPE Str = POINTER TO ARRAY [untagged] OF CHAR;
		VAR name: Str; bm: WinApi.HBITMAP; obj: StdView; res: INTEGER;
			info: WinApi.BITMAP;
	BEGIN
		name := SYSTEM.VAL(Str, SYSTEM.VAL(INTEGER, f) + 40);	(* f(HostFiles.File).name *)
		bm := WinApi.LoadImageW(0, name, WinApi.IMAGE_BITMAP, 0, 0, ORD(WinApi.LR_LOADFROMFILE));
		IF bm = 0 THEN res := WinApi.GetLastError(); HALT(100) END;
		res := WinApi.GetObjectW(bm, SIZE(WinApi.BITMAP), SYSTEM.ADR(info));
		NEW(obj); obj.w := info.bmWidth; obj.h := info.bmHeight;
		IF info.bmBitsPixel > 8 THEN obj.bits := 24 ELSE obj.bits := 8 END;
		NEW(obj.model); obj.model.ref := bm;
		s := obj
	END ImportBitmap;

	PROCEDURE ExportBitmap* (v: Stores.Store; f: Files.File);
		VAR w: Stores.Writer; info: BITMAPINFO8; i, col, res: INTEGER; data: POINTER TO ARRAY OF BYTE;
	BEGIN
		ASSERT(v # NIL, 20); ASSERT(f # NIL, 21);
		WITH v: StdView DO
			GetHeader(v, info.header, col);
			res := WinApi.GetDIBits(WinApi.GetDC(HostWindows.main), v.model.ref, 0, v.h, 0,
				SYSTEM.VAL(WinApi.BITMAPINFO, info), WinApi.DIB_RGB_COLORS);
			IF res = 0 THEN res := WinApi.GetLastError(); HALT(100) END;
			NEW(data, info.header.biSizeImage);
			res := WinApi.GetDIBits(WinApi.GetDC(HostWindows.main), v.model.ref, 0, v.h,
				SYSTEM.ADR(data[0]),
				SYSTEM.VAL(WinApi.BITMAPINFO, info), WinApi.DIB_RGB_COLORS);
			IF res = 0 THEN res := WinApi.GetLastError(); HALT(100) END;
			w.ConnectTo(f);
			w.SetPos(0);
			(* file header *)
			w.WriteSInt(4D42H);	(* type *)
			w.WriteInt(14 + 40 + 4 * col + LEN(data));	(* size *)
			w.WriteInt(0);
			w.WriteInt(14 + 40 + 4 * col);	(* offset *)
			(* bitmap header *)
			w.WriteInt(info.header.biSize);
			w.WriteInt(info.header.biWidth);
			w.WriteInt(info.header.biHeight);
			w.WriteSInt(info.header.biPlanes);
			w.WriteSInt(info.header.biBitCount);
			w.WriteInt(info.header.biCompression);
			w.WriteInt(info.header.biSizeImage);
			w.WriteInt(info.header.biXPelsPerMeter);
			w.WriteInt(info.header.biYPelsPerMeter);
			w.WriteInt(info.header.biClrUsed);
			w.WriteInt(info.header.biClrImportant);
			(* colors *)
			i := 0; WHILE i < col DO w.WriteInt(info.colors[i]); INC(i) END;
			(* bits *)
			w.rider.WriteBytes(data^, 0, LEN(data))
		ELSE
		END
	END ExportBitmap;


	PROCEDURE ImportDPictAsBitmap* (VAR med: WinOle.STGMEDIUM;
		OUT v: Views.View; OUT w, h: INTEGER; OUT isSingle: BOOLEAN
	);
		VAR hm: WinApi.HMETAFILEPICT; mp: WinApi.PtrMETAFILEPICT;
			dc, bdc: WinApi.HDC; res, u: INTEGER; bm: WinApi.HBITMAP; obj: StdView; s: SET;
	BEGIN
		hm := med.u.hMetaFilePict;
		mp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(hm));
		CASE mp.mm OF
		| 1: u := HostWindows.unit
		| 2: u := Ports.point DIV 20
		| 3: u := Ports.mm DIV 100
		| 4: u := Ports.inch DIV 100
		| 5: u := Ports.inch DIV 1000
		| 6: u := Ports.mm DIV 10
		| 7: u := Ports.mm DIV 100
		| 8: u := Ports.mm DIV 100
		END;
		w := mp.xExt * u; h := mp.yExt * u;
		NEW(obj); obj.bits := 24;
		obj.w := w DIV HostWindows.unit;
		obj.h := h DIV HostWindows.unit;
		dc := WinApi.GetDC(HostWindows.main);
		bdc := WinApi.CreateCompatibleDC(dc);
		res := WinApi.SetBkMode(bdc, WinApi.TRANSPARENT);
		bm := WinApi.CreateCompatibleBitmap(dc, obj.w, obj.h);
		res := WinApi.SelectObject(bdc, bm);
		res := WinApi.SetMapMode(bdc, mp.mm);
		res := WinApi.SetViewportOrgEx(bdc, 0, 0, NIL);
		res := WinApi.SetViewportExtEx(bdc, obj.w, obj.h, NIL);
		s := WinApi.SetTextAlign(bdc, {});
		res := WinApi.PlayMetaFile(bdc, mp.hMF);
		res := WinApi.GlobalUnlock(hm);
		res := WinApi.DeleteDC(bdc);
		WinOle.ReleaseStgMedium(med);
		NEW(obj.model); obj.model.ref := bm;
		v := obj; isSingle := FALSE
	END ImportDPictAsBitmap;

	PROCEDURE ImportDBitmap* (VAR med: WinOle.STGMEDIUM;
		OUT v: Views.View; OUT w, h: INTEGER; OUT isSingle: BOOLEAN
	);
		VAR obj: StdView; res: INTEGER;
			bm, bm0: WinApi.HBITMAP; info: WinApi.BITMAP; dc, bdc, bdc0: WinApi.HDC;
	BEGIN
		ASSERT(med.tymed = WinOle.TYMED_GDI, 20);
		bm0 := med.u.hBitmap;
		ASSERT(bm0 # 0, 20);
		res := WinApi.GetObjectW(bm0, SIZE(WinApi.BITMAP), SYSTEM.ADR(info));
		NEW(obj);
		obj.w := info.bmWidth;
		obj.h := info.bmHeight;
		IF info.bmBitsPixel > 8 THEN obj.bits := 24 ELSE obj.bits := 8 END;
		dc := WinApi.GetDC(HostWindows.main);
		bdc0 := WinApi.CreateCompatibleDC(dc);
		res := WinApi.SelectObject(bdc0, bm0);
		bdc := WinApi.CreateCompatibleDC(dc);
		bm := WinApi.CreateCompatibleBitmap(dc, obj.w, obj.h);
		res := WinApi.SelectObject(bdc, bm);
		res := WinApi.BitBlt(bdc, 0, 0, obj.w, obj.h, bdc0, 0, 0, 00CC0020H);	(* copy *)
		IF res = 0 THEN res := WinApi.GetLastError(); HALT(100) END;
		res := WinApi.DeleteDC(bdc0);
		res := WinApi.DeleteDC(bdc);
		WinOle.ReleaseStgMedium(med);
		NEW(obj.model); obj.model.ref := bm;
		v := obj; w := obj.w * HostWindows.unit; h := obj.h * HostWindows.unit; isSingle := FALSE
	END ImportDBitmap;

	PROCEDURE ExportDBitmap* (v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN;
		VAR med: WinOle.STGMEDIUM
	);
	BEGIN
		ASSERT(v # NIL, 20);
		WITH v: StdView DO
			IF v.model.ref = 0 THEN Evaluate(v, WinApi.GetDC(HostWindows.main)) END;
			med.tymed := WinOle.TYMED_GDI;
			med.u.hBitmap := v.model.ref;
			med.pUnkForRelease := NIL
		ELSE
		END
	END ExportDBitmap;

END HostBitmaps.
