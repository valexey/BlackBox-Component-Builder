MODULE ComObject;
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
		SYSTEM, COM, WinApi, WinOle, ComTools;
	
	CONST
		ObjectID = "{00010001-1000-11cf-adf0-444553540000}";
		streamStr = "CONTENTS";
		cbFormat = 200H;
	
	TYPE
		IClassFactory = POINTER TO RECORD (WinOle.IClassFactory) END;
		
		Object = POINTER TO RECORD (COM.IUnknown)
			ioo: IOleObject;
			ido: IDataObject;
			ips: IPersistStorage;
			ics: WinOle.IOleClientSite;
			idah: WinOle.IDataAdviseHolder;
			ioah: WinOle.IOleAdviseHolder;
			isg: WinOle.IStorage;
			ism: WinOle.IStream;
			w, h: INTEGER
		END;
		IOleObject = POINTER TO RECORD (WinOle.IOleObject)
			obj: Object
		END;
		IDataObject = POINTER TO RECORD (WinOle.IDataObject)
			obj: Object
		END;
		IPersistStorage = POINTER TO RECORD (WinOle.IPersistStorage)
			obj: Object
		END;
		
		
	VAR
		locks: INTEGER;
		token: INTEGER;
		
		
	PROCEDURE PictureOf (obj: Object): WinApi.HMETAFILEPICT;
		VAR dc: WinApi.HDC; mf: WinApi.HMETAFILE; mp: WinApi.PtrMETAFILEPICT;
			rc: WinApi.RECT; res: INTEGER; h: WinApi.HMETAFILEPICT; oldb, oldp: WinApi.HGDIOBJ;
	BEGIN
		dc := WinApi.CreateMetaFileW(NIL);
		IF dc # 0 THEN
			res := WinApi.SetMapMode(dc, WinApi.MM_ANISOTROPIC);
			res := WinApi.SetWindowOrgEx(dc, 0, 0, NIL);
			res := WinApi.SetWindowExtEx(dc, 20, 20, NIL);
			oldb := WinApi.SelectObject(dc, WinApi.GetStockObject(WinApi.NULL_BRUSH));
			oldp := WinApi.SelectObject(dc, WinApi.CreatePen(WinApi.PS_SOLID, 1, 0));
			res := WinApi.Ellipse(dc, 2, 2, 18, 18);
			res := WinApi.Ellipse(dc, 6, 6, 8, 8);
			res := WinApi.Ellipse(dc, 12, 6, 14, 8);
			res := WinApi.Ellipse(dc, 8, 8, 12, 12);
			res := WinApi.Ellipse(dc, 6, 14, 14, 16);
			res := WinApi.SelectObject(dc, oldb);
			res := WinApi.DeleteObject(WinApi.SelectObject(dc, oldp));
			mf := WinApi.CloseMetaFile(dc);
			IF mf # 0 THEN
				h := WinApi.GlobalAlloc(WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE,
													SIZE(WinApi.METAFILEPICT));
				IF h # 0 THEN
					mp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(h));
					mp.hMF := mf;
					mp.mm := WinApi.MM_ANISOTROPIC;
					mp.xExt := obj.w; mp.yExt := obj.h;
					res := WinApi.GlobalUnlock(h);
					RETURN h
				ELSE res := WinApi.DeleteMetaFile(mf)
				END
			END
		END;
		RETURN 0
	END PictureOf;
	
	
	(* ---------- IClassFactory ---------- *)
	
	PROCEDURE (this: IClassFactory) CreateInstance (outer: COM.IUnknown; IN iid: COM.GUID;
																				OUT int: COM.IUnknown): COM.RESULT;
		VAR res: COM.RESULT; new: Object;
	BEGIN
		IF outer = NIL THEN
			NEW(new);
			IF new # NIL THEN
				NEW(new.ioo, new); NEW(new.ido, new); NEW(new.ips, new);
				IF (new.ioo # NIL) & (new.ido # NIL) & (new.ips # NIL) THEN
					new.ioo.obj := new;
					new.ido.obj := new;
					new.ips.obj := new;
					res := new.QueryInterface(iid, int)
				ELSE res := WinApi.E_OUTOFMEMORY
				END
			ELSE res := WinApi.E_OUTOFMEMORY
			END
		ELSE res := WinApi.CLASS_E_NOAGGREGATION
		END;
		RETURN res
	END CreateInstance;

	PROCEDURE (this: IClassFactory) LockServer (lock: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF lock # 0 THEN INC(locks) ELSE DEC(locks) END;
		RETURN WinApi.S_OK
	END LockServer;
	
	
	(* ---------- Object ---------- *)
	
	PROCEDURE (this: Object) QueryInterface (IN iid: COM.GUID; OUT int: COM.IUnknown): COM.RESULT;
	BEGIN
		IF COM.QUERY(this, iid, int)
			OR COM.QUERY(this.ioo, iid, int)
			OR COM.QUERY(this.ido, iid, int)
			OR COM.QUERY(this.ips, iid, int) THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_NOINTERFACE
		END
	END QueryInterface;
	
	
	(* ---------- IOleObject ---------- *)
	
	PROCEDURE (this: IOleObject) SetClientSite (site: WinOle.IOleClientSite): COM.RESULT;
	BEGIN
		this.obj.ics := site;
		RETURN WinApi.S_OK
	END SetClientSite;
	
	PROCEDURE (this: IOleObject) GetClientSite (OUT site: WinOle.IOleClientSite): COM.RESULT;
	BEGIN
		site := this.obj.ics;
		RETURN WinApi.S_OK
	END GetClientSite;
	
	PROCEDURE (this: IOleObject) SetHostNames (app, obj: WinApi.PtrWSTR): COM.RESULT;
	BEGIN
		RETURN WinApi.S_OK
	END SetHostNames;
	
	PROCEDURE (this: IOleObject) Close (saveOption: INTEGER): COM.RESULT;
	BEGIN
		RETURN WinApi.S_OK
	END Close;
	
	PROCEDURE (this: IOleObject) SetMoniker (which: INTEGER; mk: WinOle.IMoniker): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END SetMoniker;
	
	PROCEDURE (this: IOleObject) GetMoniker (assign, which: INTEGER; OUT mk: WinOle.IMoniker): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END GetMoniker;
	
	PROCEDURE (this: IOleObject) InitFromData (obj: WinOle.IDataObject; creation: WinApi.BOOL;
																reserved: INTEGER): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END InitFromData;
	
	PROCEDURE (this: IOleObject) GetClipboardData (reserved: INTEGER; OUT  obj: WinOle.IDataObject):
																COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END GetClipboardData;
	
	PROCEDURE (this: IOleObject) DoVerb (verb: INTEGER; IN msg: WinApi.MSG; activeSite: WinOle.IOleClientSite;
													index: INTEGER; parent: WinApi.HWND; IN posRect: WinApi.RECT):
													COM.RESULT;
	BEGIN
		IF verb < 0 THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.OLEOBJ_E_NOVERBS
		END
	END DoVerb;
	
	PROCEDURE (this: IOleObject) EnumVerbs (OUT enum: WinOle.IEnumOLEVERB): COM.RESULT;
	BEGIN
		RETURN WinApi.OLEOBJ_E_NOVERBS
	END EnumVerbs;
	
	PROCEDURE (this: IOleObject) Update (): COM.RESULT;
	BEGIN
		RETURN WinApi.S_OK
	END Update;
	
	PROCEDURE (this: IOleObject) IsUpToDate (): COM.RESULT;
	BEGIN
		RETURN WinApi.S_OK
	END IsUpToDate;
	
	PROCEDURE (this: IOleObject) GetUserClassID (OUT id: COM.GUID): COM.RESULT;
	BEGIN
		id := ObjectID;
		RETURN WinApi.S_OK
	END GetUserClassID;
	
	PROCEDURE (this: IOleObject) GetUserType (form: INTEGER; OUT type: WinApi.PtrWSTR): COM.RESULT;
	BEGIN
		RETURN WinApi.OLE_S_USEREG
	END GetUserType;
	
	PROCEDURE (this: IOleObject) SetExtent (aspect: SET; IN size: WinApi.SIZE): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF aspect * WinOle.DVASPECT_CONTENT # {} THEN
			this.obj.w := size.cx; this.obj.h := size.cy;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_FAIL
		END
	END SetExtent;
	
	PROCEDURE (this: IOleObject) GetExtent (aspect: SET; OUT size: WinApi.SIZE): COM.RESULT;
	BEGIN
		IF aspect * WinOle.DVASPECT_CONTENT # {} THEN
			size.cx := this.obj.w; size.cy := this.obj.h;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_FAIL
		END
	END GetExtent;
	
	PROCEDURE (this: IOleObject) Advise (sink: WinOle.IAdviseSink; OUT connection: INTEGER): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF this.obj.ioah = NIL THEN
			res := WinOle.CreateOleAdviseHolder(this.obj.ioah);
			IF res < 0 THEN RETURN res END
		END;
		RETURN this.obj.ioah.Advise(sink, connection)
	END Advise;
	
	PROCEDURE (this: IOleObject) Unadvise (connection: INTEGER): COM.RESULT;
	BEGIN
		IF this.obj.ioah # NIL THEN
			RETURN this.obj.ioah.Unadvise(connection)
		ELSE RETURN WinApi.E_FAIL
		END
	END Unadvise;
	
	PROCEDURE (this: IOleObject) EnumAdvise (OUT enum: WinOle.IEnumSTATDATA): COM.RESULT;
	BEGIN
		IF this.obj.ioah # NIL THEN
			RETURN this.obj.ioah.EnumAdvise(enum)
		ELSE RETURN WinApi.E_FAIL
		END
	END EnumAdvise;
	
	PROCEDURE (this: IOleObject) GetMiscStatus (aspect: SET; OUT status: SET): COM.RESULT;
	BEGIN
		RETURN WinApi.OLE_S_USEREG
	END GetMiscStatus;
	
	PROCEDURE (this: IOleObject) SetColorScheme (IN pal: WinApi.LOGPALETTE): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END SetColorScheme;

	
	(* ---------- IDataObject ---------- *)
	
	PROCEDURE (this: IDataObject) GetData (IN format: WinOle.FORMATETC;
															OUT medium: WinOle.STGMEDIUM): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		res := this.QueryGetData(format);
		IF res = WinApi.S_OK THEN
			ComTools.GenMetafileMedium(PictureOf(this.obj), NIL, medium);
			RETURN WinApi.S_OK
		ELSE RETURN res
		END
	END GetData;
	
	PROCEDURE (this: IDataObject) GetDataHere (IN format: WinOle.FORMATETC;
																	VAR medium: WinOle.STGMEDIUM): COM.RESULT;
	BEGIN
		RETURN WinApi.DV_E_FORMATETC
	END GetDataHere;
	
	PROCEDURE (this: IDataObject) QueryGetData (IN format: WinOle.FORMATETC): COM.RESULT;
	BEGIN
		IF format.dwAspect * WinOle.DVASPECT_CONTENT = {} THEN RETURN WinApi.DV_E_DVASPECT
		ELSIF format.cfFormat # WinApi.CF_METAFILEPICT THEN RETURN WinApi.DV_E_FORMATETC
		ELSIF format.tymed * WinOle.TYMED_MFPICT = {} THEN RETURN WinApi.DV_E_TYMED
		ELSE RETURN WinApi.S_OK
		END
	END QueryGetData;
	
	PROCEDURE (this: IDataObject) GetCanonicalFormatEtc (IN formatIn: WinOle.FORMATETC;
																			OUT formatOut: WinOle.FORMATETC): COM.RESULT;
	BEGIN
		RETURN WinApi.DATA_S_SAMEFORMATETC
	END GetCanonicalFormatEtc;
	
	PROCEDURE (this: IDataObject) SetData (IN format: WinOle.FORMATETC;
															IN medium: WinOle.STGMEDIUM; release: WinApi.BOOL): COM.RESULT;
	BEGIN
		RETURN WinApi.DV_E_FORMATETC
	END SetData;
	
	PROCEDURE (this: IDataObject) EnumFormatEtc (direction: SET; OUT enum: WinOle.IEnumFORMATETC):
															COM.RESULT;
	BEGIN
		RETURN WinApi.OLE_S_USEREG
	END EnumFormatEtc;
	
	PROCEDURE (this: IDataObject) DAdvise (IN format: WinOle.FORMATETC; flags: SET;
															advSink: WinOle.IAdviseSink; OUT connection: INTEGER): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF this.obj.idah = NIL THEN
			res := WinOle.CreateDataAdviseHolder(this.obj.idah);
			IF res < 0 THEN RETURN res END
		END;
		RETURN this.obj.idah.Advise(this, format, flags, advSink, connection)
	END DAdvise;
	
	PROCEDURE (this: IDataObject) DUnadvise (connection: INTEGER): COM.RESULT;
	BEGIN
		IF this.obj.idah # NIL THEN
			RETURN this.obj.idah.Unadvise(connection)
		ELSE RETURN WinApi.E_FAIL
		END
	END DUnadvise;
	
	PROCEDURE (this: IDataObject) EnumDAdvise (OUT  enum: WinOle.IEnumSTATDATA): COM.RESULT;
	BEGIN
		IF this.obj.idah # NIL THEN
			RETURN this.obj.idah.EnumAdvise(enum)
		ELSE RETURN WinApi.E_FAIL
		END
	END EnumDAdvise;
	
	
	(* ---------- IPersistStorage ---------- *)
	
	PROCEDURE (this: IPersistStorage) GetClassID (OUT id: COM.GUID): COM.RESULT;
	BEGIN
		id := ObjectID;
		RETURN WinApi.S_OK
	END GetClassID;

	PROCEDURE (this: IPersistStorage) IsDirty (): COM.RESULT;
	BEGIN
		RETURN WinApi.S_FALSE
	END IsDirty;
	
	PROCEDURE (this: IPersistStorage) InitNew (stg: WinOle.IStorage): COM.RESULT;
		VAR res: COM.RESULT; ps: WinApi.PtrWSTR;
	BEGIN
		IF stg # NIL THEN
			res := stg.CreateStream(streamStr,
											WinOle.STGM_DIRECT + WinOle.STGM_CREATE
											+ WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE,
											0, 0, this.obj.ism);
			IF res >= 0 THEN
				res := WinOle.OleRegGetUserType(ObjectID, WinOle.USERCLASSTYPE_SHORT, ps);
				res := WinOle.WriteFmtUserTypeStg(stg, cbFormat, ps);
				ComTools.FreeString(ps);
				this.obj.isg := stg;
				this.obj.w := 5000;
				this.obj.h := 5000;
				RETURN WinApi.S_OK
			ELSE RETURN res
			END
		ELSE RETURN WinApi.E_POINTER
		END
	END InitNew;
	
	PROCEDURE (this: IPersistStorage) Load (stg: WinOle.IStorage): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF stg # NIL THEN
			res := stg.OpenStream(streamStr, 0,
										WinOle.STGM_DIRECT + WinOle.STGM_READWRITE
										+ WinOle.STGM_SHARE_EXCLUSIVE,
										0, this.obj.ism);
			IF res >= 0 THEN
				res := this.obj.ism.Read(SYSTEM.ADR(this.obj.w), 4, NIL);
				res := this.obj.ism.Read(SYSTEM.ADR(this.obj.h), 4, NIL);
				IF res >= 0 THEN
					this.obj.isg := stg;
					RETURN WinApi.S_OK
				END
			END;
			RETURN WinApi.STG_E_READFAULT
		ELSE RETURN WinApi.E_POINTER
		END
	END Load;
	
	PROCEDURE (this: IPersistStorage) Save (stg: WinOle.IStorage; sameAsLoad: WinApi.BOOL): COM.RESULT;
		VAR stm: WinOle.IStream; res: COM.RESULT; ps: WinApi.PtrWSTR;
	BEGIN
		IF sameAsLoad # 0 THEN
			stm := this.obj.ism;
			res := stm.Seek(0, WinOle.STREAM_SEEK_SET, NIL)
		ELSIF stg # NIL THEN
			res := stg.CreateStream(streamStr,
											WinOle.STGM_DIRECT + WinOle.STGM_CREATE
											+ WinOle.STGM_WRITE + WinOle.STGM_SHARE_EXCLUSIVE,
											0, 0, stm);
			IF res < 0 THEN RETURN res END;
			res := WinOle.OleRegGetUserType(ObjectID, WinOle.USERCLASSTYPE_SHORT, ps);
			res := WinOle.WriteFmtUserTypeStg(stg, cbFormat, ps);
			ComTools.FreeString(ps);
		ELSE RETURN WinApi.E_POINTER
		END;
		res := stm.Write(SYSTEM.ADR(this.obj.w), 4, NIL);
		res := stm.Write(SYSTEM.ADR(this.obj.h), 4, NIL);
		IF res < 0 THEN RETURN WinApi.STG_E_WRITEFAULT
		ELSE RETURN WinApi.S_OK
		END
	END Save;
	
	PROCEDURE (this: IPersistStorage) SaveCompleted (new: WinOle.IStorage): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF new # NIL THEN
			res := new.OpenStream(streamStr, 0,
										WinOle.STGM_DIRECT + WinOle.STGM_READWRITE
										+ WinOle.STGM_SHARE_EXCLUSIVE,
										0, this.obj.ism);
			IF res >= 0 THEN this.obj.isg := new;
			ELSE RETURN res
			END
		END;
		IF this.obj.ioah # NIL THEN res := this.obj.ioah.SendOnSave() END;
		RETURN WinApi.S_OK
	END SaveCompleted;
	
	PROCEDURE (this: IPersistStorage) HandsOffStorage (): COM.RESULT;
	BEGIN
		this.obj.ism := NIL; this.obj.isg := NIL;
		RETURN WinApi.S_OK
	END HandsOffStorage;
	

	(* ---------- commands ---------- *)
	
	PROCEDURE Register*;
		VAR res: COM.RESULT; factory: IClassFactory;
	BEGIN
		NEW(factory);
		res := WinOle.CoRegisterClassObject(ObjectID, factory,
												WinOle.CLSCTX_LOCAL_SERVER, WinOle.REGCLS_MULTIPLEUSE, token);
	END Register;
	
	PROCEDURE Unregister*;
		VAR res: COM.RESULT;
	BEGIN
		IF (token # 0) & (locks = 0) THEN res := WinOle.CoRevokeClassObject(token) END
	END Unregister;
	
END ComObject.

ComObject.Register
ComObject.Unregister


-----------------------------------------------------------------------------------------------------------------
REGEDIT

HKEY_CLASSES_ROOT\BlackBox.Object = BlackBox Object
HKEY_CLASSES_ROOT\BlackBox.Object\CLSID = {00010001-1000-11cf-adf0-444553540000}
HKEY_CLASSES_ROOT\BlackBox.Object\Insertable

HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000} = BlackBox Object
HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\ProgID = BlackBox.Object

HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\LocalServer32 = C:\BlackBox\BlackBox.exe
HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\InProcHandler32 = ole32.dll

HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\Insertable
HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\DefaultIcon = C:\BlackBox\BlackBox.exe,0

HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\DataFormats\GetSet\0 = 3,1,32,1
HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\MiscStatus = 16
HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\AuxUserType\2 = BlackBox Object
HKEY_CLASSES_ROOT\CLSID\{00010001-1000-11cf-adf0-444553540000}\AuxUserType\3 = BlackBox
-----------------------------------------------------------------------------------------------------------------
