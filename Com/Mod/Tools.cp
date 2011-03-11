MODULE ComTools;
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


	IMPORT SYSTEM, COM, WinApi, WinOle;
	
	
	(* ole string handling *)
	
	PROCEDURE NewString* (IN str: ARRAY [untagged] OF CHAR): WinApi.PtrWSTR;
		VAR p: WinApi.PtrWSTR; n: INTEGER;
	BEGIN
		n := 0; WHILE str[n] # 0X DO INC(n) END;
		p := SYSTEM.VAL(WinApi.PtrWSTR, WinOle.CoTaskMemAlloc(SIZE(CHAR) * (n + 1)));
		p^ := str$;
		RETURN p
	END NewString;
	
	PROCEDURE NewEmptyString* (length: INTEGER): WinApi.PtrWSTR;
	BEGIN
		RETURN SYSTEM.VAL(WinApi.PtrWSTR, WinOle.CoTaskMemAlloc(SIZE(CHAR) * length))
	END NewEmptyString;
	
	PROCEDURE FreeString* (VAR p: WinApi.PtrWSTR);
	BEGIN
		WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, p));
		p := NIL
	END FreeString;
	
	PROCEDURE NewSString* (IN str: ARRAY [untagged] OF SHORTCHAR): WinApi.PtrSTR;
		VAR p: WinApi.PtrSTR; n: INTEGER;
	BEGIN
		n := 0; WHILE str[n] # 0X DO INC(n) END;
		p := SYSTEM.VAL(WinApi.PtrSTR, WinOle.CoTaskMemAlloc(SIZE(SHORTCHAR) * (n + 1)));
		p^ := str$;
		RETURN p
	END NewSString;
	
	PROCEDURE NewEmptySString* (length: INTEGER): WinApi.PtrSTR;
	BEGIN
		RETURN SYSTEM.VAL(WinApi.PtrSTR, WinOle.CoTaskMemAlloc(SIZE(CHAR) * length))
	END NewEmptySString;
	
	PROCEDURE FreeSString* (VAR p: WinApi.PtrSTR);
	BEGIN
		WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, p));
		p := NIL
	END FreeSString;

	
	(* FORMATETC generation *)
	
	PROCEDURE GenFormatEtc* (format: SHORTINT; aspect: SET; tymed: SET; OUT f: WinOle.FORMATETC);
	BEGIN
		f.cfFormat := format;
		f.ptd := NIL;
		f.dwAspect := aspect;
		f.lindex := -1;
		f.tymed := tymed
	END GenFormatEtc;
	
	
	(* STGMEDIUM generation *)

	PROCEDURE GenBitmapMedium* (
		bitmap: WinApi.HBITMAP; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_GDI;
		sm.u.hBitmap := bitmap;
		sm.pUnkForRelease := unk
	END GenBitmapMedium;
	
	PROCEDURE GenMetafileMedium* (
		mf: WinApi.HMETAFILEPICT; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_MFPICT;
		sm.u.hMetaFilePict := mf;
		sm.pUnkForRelease := unk
	END GenMetafileMedium;
	
	PROCEDURE GenEMetafileMedium* (
		emf: WinApi.HENHMETAFILE; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_ENHMF;
		sm.u.hEnhMetaFile := emf;
		sm.pUnkForRelease := unk
	END GenEMetafileMedium;
	
	PROCEDURE GenGlobalMedium* (hg: WinApi.HGLOBAL; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_HGLOBAL;
		sm.u.hGlobal := hg;
		sm.pUnkForRelease := unk
	END GenGlobalMedium;
	
	PROCEDURE GenFileMedium* (name: ARRAY OF CHAR; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_FILE;
		sm.u.lpszFileName := NewString(name);
		sm.pUnkForRelease := unk
	END GenFileMedium;
	
	PROCEDURE GenStreamMedium* (stm: WinOle.IStream; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_ISTREAM;
		sm.u.pstm := stm;
		sm.pUnkForRelease := unk
	END GenStreamMedium;
	
	PROCEDURE GenStorageMedium* (stg: WinOle.IStorage; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		IF sm.tymed # WinOle.TYMED_NULL THEN WinOle.ReleaseStgMedium(sm) END;
		sm.tymed := WinOle.TYMED_ISTORAGE;
		sm.u.pstg := stg;
		sm.pUnkForRelease := unk
	END GenStorageMedium;
	
	
	(* STGMEDIUM access *)
	
	PROCEDURE MediumBitmap* (IN sm: WinOle.STGMEDIUM): WinApi.HBITMAP;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_GDI, 20);
		RETURN sm.u.hBitmap
	END MediumBitmap;
	
	PROCEDURE MediumMetafile* (IN sm: WinOle.STGMEDIUM): WinApi.HMETAFILEPICT;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_MFPICT, 20);
		RETURN sm.u.hMetaFilePict
	END MediumMetafile;
	
	PROCEDURE MediumEnhMetafile* (IN sm: WinOle.STGMEDIUM): WinApi.HENHMETAFILE;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_ENHMF, 20);
		RETURN sm.u.hEnhMetaFile
	END MediumEnhMetafile;
	
	PROCEDURE MediumGlobal* (IN sm: WinOle.STGMEDIUM): WinApi.HGLOBAL;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_HGLOBAL, 20);
		RETURN sm.u.hGlobal
	END MediumGlobal;
	
	PROCEDURE MediumFile* (IN sm: WinOle.STGMEDIUM): WinApi.PtrWSTR;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_FILE, 20);
		RETURN sm.u.lpszFileName
	END MediumFile;
	
	PROCEDURE MediumStream* (IN sm: WinOle.STGMEDIUM): WinOle.IStream;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_ISTREAM, 20);
		RETURN sm.u.pstm
	END MediumStream;
	
	PROCEDURE MediumStorage* (IN sm: WinOle.STGMEDIUM): WinOle.IStorage;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_ISTORAGE, 20);
		RETURN sm.u.pstg
	END MediumStorage;
	
END ComTools.
