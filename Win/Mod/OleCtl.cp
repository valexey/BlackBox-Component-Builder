MODULE WinOleCtl ["OC30.dll"];
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

	IMPORT WinOleAut, COM, WinOle, WinApi;

	CONST (* macros *)
		CTL_E_ILLEGALFUNCTIONCALL* = -2146828283;
		CTL_E_OVERFLOW* = -2146828282;
		CTL_E_OUTOFMEMORY* = -2146828281;
		CTL_E_DIVISIONBYZERO* = -2146828277;
		CTL_E_OUTOFSTRINGSPACE* = -2146828274;
		CTL_E_OUTOFSTACKSPACE* = -2146828260;
		CTL_E_BADFILENAMEORNUMBER* = -2146828236;
		CTL_E_FILENOTFOUND* = -2146828235;
		CTL_E_BADFILEMODE* = -2146828234;
		CTL_E_FILEALREADYOPEN* = -2146828233;
		CTL_E_DEVICEIOERROR* = -2146828231;
		CTL_E_FILEALREADYEXISTS* = -2146828230;
		CTL_E_BADRECORDLENGTH* = -2146828229;
		CTL_E_DISKFULL* = -2146828227;
		CTL_E_BADRECORDNUMBER* = -2146828225;
		CTL_E_BADFILENAME* = -2146828224;
		CTL_E_TOOMANYFILES* = -2146828221;
		CTL_E_DEVICEUNAVAILABLE* = -2146828220;
		CTL_E_PERMISSIONDENIED* = -2146828218;
		CTL_E_DISKNOTREADY* = -2146828217;
		CTL_E_PATHFILEACCESSERROR* = -2146828213;
		CTL_E_PATHNOTFOUND* = -2146828212;
		CTL_E_INVALIDPATTERNSTRING* = -2146828195;
		CTL_E_INVALIDUSEOFNULL* = -2146828194;
		CTL_E_INVALIDFILEFORMAT* = -2146827967;
		CTL_E_INVALIDPROPERTYVALUE* = -2146827908;
		CTL_E_INVALIDPROPERTYARRAYINDEX* = -2146827907;
		CTL_E_SETNOTSUPPORTEDATRUNTIME* = -2146827906;
		CTL_E_SETNOTSUPPORTED* = -2146827905;
		CTL_E_NEEDPROPERTYARRAYINDEX* = -2146827903;
		CTL_E_SETNOTPERMITTED* = -2146827901;
		CTL_E_GETNOTSUPPORTEDATRUNTIME* = -2146827895;
		CTL_E_GETNOTSUPPORTED* = -2146827894;
		CTL_E_PROPERTYNOTFOUND* = -2146827866;
		CTL_E_INVALIDCLIPBOARDFORMAT* = -2146827828;
		CTL_E_INVALIDPICTURE* = -2146827807;
		CTL_E_PRINTERERROR* = -2146827806;
		CTL_E_CANTSAVEFILETOTEMP* = -2146827553;
		CTL_E_SEARCHTEXTNOTFOUND* = -2146827544;
		CTL_E_REPLACEMENTSTOOLONG* = -2146827542;
		CTL_E_CUSTOM_FIRST* = -2146827688;
		CLASS_E_NOTLICENSED* = -2147221230;
		CONNECT_E_FIRST* = -2147220992;
		CONNECT_E_LAST* = -2147220977;
		CONNECT_S_FIRST* = 262656;
		CONNECT_S_LAST* = 262671;
		CONNECT_E_NOCONNECTION* = -2147220992;
		CONNECT_E_ADVISELIMIT* = -2147220991;
		CONNECT_E_CANNOTCONNECT* = -2147220990;
		CONNECT_E_OVERRIDDEN* = -2147220989;
		SELFREG_E_FIRST* = -2147220992;
		SELFREG_E_LAST* = -2147220977;
		SELFREG_S_FIRST* = 262656;
		SELFREG_S_LAST* = 262671;
		SELFREG_E_TYPELIB* = -2147220992;
		SELFREG_E_CLASS* = -2147220991;
		PERPROP_E_FIRST* = -2147220992;
		PERPROP_E_LAST* = -2147220977;
		PERPROP_S_FIRST* = 262656;
		PERPROP_S_LAST* = 262671;
		PERPROP_E_NOPAGEAVAILABLE* = -2147220992;
		OLEMISC_INVISIBLEATRUNTIME* = {10};
		OLEMISC_ALWAYSRUN* = {11};
		OLEMISC_ACTSLIKEBUTTON* = {12};
		OLEMISC_ACTSLIKELABEL* = {13};
		OLEMISC_NOUIACTIVATE* = {14};
		OLEMISC_ALIGNABLE* = {15};
		OLEMISC_SIMPLEFRAME* = {16};
		OLEMISC_SETCLIENTSITEFIRST* = {17};
		OLEMISC_IMEMODE* = {18};
		OLEMISC_WANTSTOMENUMERGE* = {19};
		OLEIVERB_PROPERTIES* = -7;
		VT_STREAMED_PROPSET* = 73;
		VT_STORED_PROPSET* = 74;
		VT_BLOB_PROPSET* = 75;
		VT_VERBOSE_ENUM* = 76;
		VT_COLOR* = 3;
		VT_XPOS_PIXELS* = 3;
		VT_YPOS_PIXELS* = 3;
		VT_XSIZE_PIXELS* = 3;
		VT_YSIZE_PIXELS* = 3;
		VT_XPOS_HIMETRIC* = 3;
		VT_YPOS_HIMETRIC* = 3;
		VT_XSIZE_HIMETRIC* = 3;
		VT_YSIZE_HIMETRIC* = 3;
		VT_TRISTATE* = 2;
		VT_OPTEXCLUSIVE* = 11;
		VT_FONT* = 9;
		VT_PICTURE* = 9;
		VT_HANDLE* = 3;
		OCM__BASE* = 8192;
		OCM_COMMAND* = 8465;
		OCM_CTLCOLORBTN* = 8501;
		OCM_CTLCOLOREDIT* = 8499;
		OCM_CTLCOLORDLG* = 8502;
		OCM_CTLCOLORLISTBOX* = 8500;
		OCM_CTLCOLORMSGBOX* = 8498;
		OCM_CTLCOLORSCROLLBAR* = 8503;
		OCM_CTLCOLORSTATIC* = 8504;
		OCM_DRAWITEM* = 8235;
		OCM_MEASUREITEM* = 8236;
		OCM_DELETEITEM* = 8237;
		OCM_VKEYTOITEM* = 8238;
		OCM_CHARTOITEM* = 8239;
		OCM_COMPAREITEM* = 8249;
		OCM_HSCROLL* = 8468;
		OCM_VSCROLL* = 8469;
		OCM_PARENTNOTIFY* = 8720;
		OCM_NOTIFY* = 8270;
		CTRLINFO_EATS_RETURN* = 1;
		CTRLINFO_EATS_ESCAPE* = 2;
		XFORMCOORDS_POSITION* = 1;
		XFORMCOORDS_SIZE* = 2;
		XFORMCOORDS_HIMETRICTOCONTAINER* = 4;
		XFORMCOORDS_CONTAINERTOHIMETRIC* = 8;
		GUIDKIND_DEFAULT_SOURCE_DISP_IID* = 1;
		PROPPAGESTATUS_DIRTY* = 1;
		PROPPAGESTATUS_VALIDATE* = 2;
		PICTURE_SCALABLE* = 1;
		PICTURE_TRANSPARENT* = 2;
		PICTYPE_UNINITIALIZED* = -1;
		PICTYPE_NONE* = 0;
		PICTYPE_BITMAP* = 1;
		PICTYPE_METAFILE* = 2;
		PICTYPE_ICON* = 3;
		PICTYPE_ENHMETAFILE* = 4;
		DISPID_AUTOSIZE* = -500;
		DISPID_BACKCOLOR* = -501;
		DISPID_BACKSTYLE* = -502;
		DISPID_BORDERCOLOR* = -503;
		DISPID_BORDERSTYLE* = -504;
		DISPID_BORDERWIDTH* = -505;
		DISPID_DRAWMODE* = -507;
		DISPID_DRAWSTYLE* = -508;
		DISPID_DRAWWIDTH* = -509;
		DISPID_FILLCOLOR* = -510;
		DISPID_FILLSTYLE* = -511;
		DISPID_FONT* = -512;
		DISPID_FORECOLOR* = -513;
		DISPID_ENABLED* = -514;
		DISPID_HWND* = -515;
		DISPID_TABSTOP* = -516;
		DISPID_TEXT* = -517;
		DISPID_CAPTION* = -518;
		DISPID_BORDERVISIBLE* = -519;
		DISPID_APPEARANCE* = -520;
		DISPID_REFRESH* = -550;
		DISPID_DOCLICK* = -551;
		DISPID_ABOUTBOX* = -552;
		DISPID_CLICK* = -600;
		DISPID_DBLCLICK* = -601;
		DISPID_KEYDOWN* = -602;
		DISPID_KEYPRESS* = -603;
		DISPID_KEYUP* = -604;
		DISPID_MOUSEDOWN* = -605;
		DISPID_MOUSEMOVE* = -606;
		DISPID_MOUSEUP* = -607;
		DISPID_ERROREVENT* = -608;
		DISPID_AMBIENT_BACKCOLOR* = -701;
		DISPID_AMBIENT_DISPLAYNAME* = -702;
		DISPID_AMBIENT_FONT* = -703;
		DISPID_AMBIENT_FORECOLOR* = -704;
		DISPID_AMBIENT_LOCALEID* = -705;
		DISPID_AMBIENT_MESSAGEREFLECT* = -706;
		DISPID_AMBIENT_SCALEUNITS* = -707;
		DISPID_AMBIENT_TEXTALIGN* = -708;
		DISPID_AMBIENT_USERMODE* = -709;
		DISPID_AMBIENT_UIDEAD* = -710;
		DISPID_AMBIENT_SHOWGRABHANDLES* = -711;
		DISPID_AMBIENT_SHOWHATCHING* = -712;
		DISPID_AMBIENT_DISPLAYASDEFAULT* = -713;
		DISPID_AMBIENT_SUPPORTSMNEMONICS* = -714;
		DISPID_AMBIENT_AUTOCLIP* = -715;
		DISPID_AMBIENT_APPEARANCE* = -716;
		DISPID_FONT_NAME* = 0;
		DISPID_FONT_SIZE* = 2;
		DISPID_FONT_BOLD* = 3;
		DISPID_FONT_ITALIC* = 4;
		DISPID_FONT_UNDER* = 5;
		DISPID_FONT_STRIKE* = 6;
		DISPID_FONT_WEIGHT* = 7;
		DISPID_FONT_CHARSET* = 8;
		DISPID_PICT_HANDLE* = 0;
		DISPID_PICT_HPAL* = 2;
		DISPID_PICT_TYPE* = 3;
		DISPID_PICT_WIDTH* = 4;
		DISPID_PICT_HEIGHT* = 5;
		DISPID_PICT_RENDER* = 6;

	CONST (* enumerations *)
		CLSID_CFontPropPage* = "{2542F180-3532-1069-A2CD-00AA0034B50B}";
		CLSID_CColorPropPage* = "{DDF5A600-B9C0-101A-AF1A-00AA0034B50B}";
		CLSID_CPicturePropPage* = "{FC7AF71D-FC74-101A-84ED-08002B2EC713}";
		CLSID_PersistPropset* = "{FB8F0821-0164-101B-84ED-08002B2EC713}";
		CLSID_ConvertVBX* = "{FB8F0822-0164-101B-84ED-08002B2EC713}";
		CLSID_StdFont* = "{FB8F0823-0164-101B-84ED-08002B2EC713}";
		CLSID_StdPicture* = "{FB8F0824-0164-101B-84ED-08002B2EC713}";
		GUID_HIMETRIC* = "{66504300-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_COLOR* = "{66504301-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_XPOSPIXEL* = "{66504302-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_YPOSPIXEL* = "{66504303-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_XSIZEPIXEL* = "{66504304-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_YSIZEPIXEL* = "{66504305-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_XPOS* = "{66504306-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_YPOS* = "{66504307-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_XSIZE* = "{66504308-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_YSIZE* = "{66504309-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_TRISTATE* = "{6650430A-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_OPTIONVALUEEXCLUSIVE* = "{6650430B-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_CHECKVALUEEXCLUSIVE* = "{6650430C-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_FONTNAME* = "{6650430D-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_FONTSIZE* = "{6650430E-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_FONTBOLD* = "{6650430F-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_FONTITALIC* = "{66504310-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_FONTUNDERSCORE* = "{66504311-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_FONTSTRIKETHROUGH* = "{66504312-BE0F-101A-8BBB-00AA00300CAB}";
		GUID_HANDLE* = "{66504313-BE0F-101A-8BBB-00AA00300CAB}";
		triUnchecked* = 0;
		triChecked* = 1;
		triGray* = 2;

	TYPE
		TEXTMETRICOLE* = WinApi.TEXTMETRICW;
		PtrTEXTMETRICOLE* = WinApi.PtrTEXTMETRICW;
		OLE_COLOR* = INTEGER;
		OLE_XPOS_PIXELS* = INTEGER;
		OLE_YPOS_PIXELS* = INTEGER;
		OLE_XSIZE_PIXELS* = INTEGER;
		OLE_YSIZE_PIXELS* = INTEGER;
		OLE_XPOS_HIMETRIC* = INTEGER;
		OLE_YPOS_HIMETRIC* = INTEGER;
		OLE_XSIZE_HIMETRIC* = INTEGER;
		OLE_YSIZE_HIMETRIC* = INTEGER;
		OLE_XPOS_CONTAINER* = SHORTREAL;
		OLE_YPOS_CONTAINER* = SHORTREAL;
		OLE_XSIZE_CONTAINER* = SHORTREAL;
		OLE_YSIZE_CONTAINER* = SHORTREAL;
		OLE_TRISTATE* = INTEGER;
		OLE_OPTEXCLUSIVE* = WinOle.VARIANT_BOOL;
		OLE_CANCELBOOL* = WinOle.VARIANT_BOOL;
		OLE_ENABLEDEFAULTBOOL* = WinOle.VARIANT_BOOL;
		OLE_HANDLE* = INTEGER;
		POINTF* = RECORD [untagged]
			x*: SHORTREAL;
			y*: SHORTREAL;
		END;
		PtrPOINTF* = POINTER TO POINTF;
		CONTROLINFO* = RECORD [untagged]
			cb*: INTEGER;
			hAccel*: WinApi.HACCEL;
			cAccel*: SHORTINT;
			dwFlags*: SET;
		END;
		PtrCONTROLINFO* = POINTER TO CONTROLINFO;
		IOleControl* = POINTER TO ABSTRACT RECORD ["{B196B288-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IOleControlSite* = POINTER TO ABSTRACT RECORD ["{B196B289-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		ISimpleFrameSite* = POINTER TO ABSTRACT RECORD ["{742B0E01-14E6-101B-914E-00AA00300CAB}"] (COM.IUnknown)
		END;
		IErrorLog* = POINTER TO ABSTRACT RECORD [interface] (COM.IUnknown)
		END;
		IPropertyBag* = POINTER TO ABSTRACT RECORD [interface] (COM.IUnknown)
		END;
		IPersistPropertyBag* = POINTER TO ABSTRACT RECORD [interface] (WinOle.IPersist)
		END;
		IPersistStreamInit* = POINTER TO ABSTRACT RECORD ["{7FD52380-4E07-101B-AE2D-08002B2EC713}"] (WinOle.IPersist)
		END;
		IPersistMemory* = POINTER TO ABSTRACT RECORD [interface] (WinOle.IPersist)
		END;
		IPropertyNotifySink* = POINTER TO ABSTRACT RECORD ["{9BFBBC02-EFF1-101A-84ED-00AA00341D07}"] (COM.IUnknown)
		END;
		IProvideClassInfo* = POINTER TO ABSTRACT RECORD ["{B196B283-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IProvideClassInfo2* = POINTER TO ABSTRACT RECORD ["{A6BC3AC0-DBAA-11CE-9DE3-00AA004BB851}"] (IProvideClassInfo)
		END;
		LICINFO* = RECORD [untagged]
			cbLicInfo*: INTEGER;
			fRuntimeKeyAvail*: WinApi.BOOL;
			fLicVerified*: WinApi.BOOL;
		END;
		PtrLICINFO* = POINTER TO LICINFO;
		IClassFactory2* = POINTER TO ABSTRACT RECORD ["{B196B28F-BAB4-101A-B69C-00AA00341D07}"] (WinOle.IClassFactory)
		END;
		CAUUID* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF COM.GUID;
		END;
		PtrCAUUID* = POINTER TO CAUUID;
		CALPOLESTR* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrWSTR;
		END;
		PtrCALPOLESTR* = POINTER TO CALPOLESTR;
		CADWORD* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
		END;
		PtrCADWORD* = POINTER TO CADWORD;
		OCPFIPARAMS* = RECORD [untagged]
			cbStructSize*: INTEGER;
			hWndOwner*: WinApi.HWND;
			x*: INTEGER;
			y*: INTEGER;
			lpszCaption*: WinApi.PtrWSTR;
			cObjects*: INTEGER;
			lplpUnk*: POINTER TO (*?*) ARRAY [untagged] OF COM.IUnknown;
			cPages*: INTEGER;
			lpPages*: POINTER TO (*?*) ARRAY [untagged] OF COM.GUID;
			lcid*: WinOle.LCID;
			dispidInitialProperty*: WinOleAut.DISPID;
		END;
		PtrOCPFIPARAMS* = POINTER TO OCPFIPARAMS;
		PROPPAGEINFO* = RECORD [untagged]
			cb*: INTEGER;
			pszTitle*: WinApi.PtrWSTR;
			size*: WinApi.SIZE;
			pszDocString*: WinApi.PtrWSTR;
			pszHelpFile*: WinApi.PtrWSTR;
			dwHelpContext*: INTEGER;
		END;
		PtrPROPPAGEINFO* = POINTER TO PROPPAGEINFO;
		ISpecifyPropertyPages* = POINTER TO ABSTRACT RECORD ["{B196B28B-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IPerPropertyBrowsing* = POINTER TO ABSTRACT RECORD ["{376BD3AA-3845-101B-84ED-08002B2EC713}"] (COM.IUnknown)
		END;
		IPropertyPageSite* = POINTER TO ABSTRACT RECORD ["{B196B28C-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IPropertyPage* = POINTER TO ABSTRACT RECORD ["{B196B28D-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IPropertyPage2* = POINTER TO ABSTRACT RECORD ["{01E44665-24AC-101B-84ED-08002B2EC713}"] (IPropertyPage)
		END;
		IFont* = POINTER TO ABSTRACT RECORD ["{BEF6E002-A874-101A-8BBA-00AA00300CAB}"] (COM.IUnknown)
		END;
		IFontDisp* = POINTER TO ABSTRACT RECORD ["{BEF6E003-A874-101A-8BBA-00AA00300CAB}"] (WinOleAut.IDispatch)
		END;
		FONTDESC* = RECORD [align8]
			cbSizeofstruct*: INTEGER;
			lpstrName*: WinApi.PtrWSTR;
			cySize*: WinApi.CY;
			sWeight*: SHORTINT;
			sCharset*: SHORTINT;
			fItalic*: WinApi.BOOL;
			fUnderline*: WinApi.BOOL;
			fStrikethrough*: WinApi.BOOL;
		END;
		PtrFONTDESC* = POINTER TO FONTDESC;
		IPicture* = POINTER TO ABSTRACT RECORD ["{7BF80980-BF32-101A-8BBB-00AA00300CAB}"] (COM.IUnknown)
		END;
		IPictureDisp* = POINTER TO ABSTRACT RECORD ["{7BF80981-BF32-101A-8BBB-00AA00300CAB}"] (WinOleAut.IDispatch)
		END;
		INTERFACE* = IPictureDisp;
		PICTDESC* = RECORD [untagged]
			cbSizeofstruct*: INTEGER;
			picType*: INTEGER;
			u*: RECORD [union]
				bmp*: RECORD [untagged]
					hbitmap*: WinApi.HBITMAP;
					hpal*: WinApi.HPALETTE;
				END;
				wmf*: RECORD [untagged]
					hmeta*: WinApi.HMETAFILE;
					xExt*: INTEGER;
					yExt*: INTEGER;
				END;
				icon*: RECORD [untagged]
					hicon*: WinApi.HICON;
				END;
				emf*: RECORD [untagged]
					hemf*: WinApi.HENHMETAFILE;
				END;
			END;
		END;
		PtrPICTDESC* = POINTER TO PICTDESC;

	PROCEDURE (this: IOleControl) GetControlInfo* (VAR [nil] pCI: CONTROLINFO): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControl) OnMnemonic* (VAR [nil] pMsg: WinApi.MSG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControl) OnAmbientPropertyChange* (dispid: WinOleAut.DISPID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControl) FreezeEvents* (bFreeze: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) OnControlInfoChanged* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) LockInPlaceActive* (fLock: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) GetExtendedControl* (VAR [nil] ppDisp: WinOleAut.IDispatch): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) TransformCoords* (VAR [nil] lpptlHimetric: WinApi.POINT; VAR [nil] lpptfContainer: POINTF; flags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) TranslateAccelerator* (VAR [nil] lpMsg: WinApi.MSG; grfModifiers: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) OnFocus* (fGotFocus: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleControlSite) ShowPropertyFrame* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ISimpleFrameSite) PreMessageFilter* (hwnd: WinApi.HWND; msg: INTEGER; wp: WinApi.WPARAM; lp: WinApi.LPARAM; VAR [nil] lplResult: WinApi.LRESULT; VAR [nil] lpdwCookie: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ISimpleFrameSite) PostMessageFilter* (hwnd: WinApi.HWND; msg: INTEGER; wp: WinApi.WPARAM; lp: WinApi.LPARAM; VAR [nil] lplResult: WinApi.LRESULT; dwCookie: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IErrorLog) AddError* (pszPropName: WinApi.PtrWSTR; VAR [nil] pExcepInfo: WinOleAut.EXCEPINFO): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyBag) Read* (pszPropName: WinApi.PtrWSTR; VAR [nil] pVar: WinOleAut.VARIANT; pErrorLog: IErrorLog): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyBag) Write* (pszPropName: WinApi.PtrWSTR; VAR [nil] pVar: WinOleAut.VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistPropertyBag) InitNew* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistPropertyBag) Load* (pPropBag: IPropertyBag; pErrorLog: IErrorLog): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistPropertyBag) Save* (pPropBag: IPropertyBag; fClearDirty: WinApi.BOOL; fSaveAllProperties: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStreamInit) IsDirty* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStreamInit) Load* (pStm: WinOle.IStream): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStreamInit) Save* (pStm: WinOle.IStream; fClearDirty: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStreamInit) GetSizeMax* (VAR [nil] pcbSize: LONGINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStreamInit) InitNew* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistMemory) IsDirty* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistMemory) Load* (lpStream: WinApi.PtrVoid; cbSize: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistMemory) Save* (lpStream: WinApi.PtrVoid; fClearDirty: WinApi.BOOL; cbSize: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistMemory) GetSizeMax* (VAR [nil] pcbSize: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistMemory) InitNew* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyNotifySink) OnChanged* (dispid: WinOleAut.DISPID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyNotifySink) OnRequestEdit* (dispid: WinOleAut.DISPID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IProvideClassInfo) GetClassInfoA* (VAR [nil] ppTI: WinOleAut.ITypeInfo): COM.RESULT, NEW, ABSTRACT;
	
	PROCEDURE (this: IProvideClassInfo2) GetGUID* (dwGuidKind: INTEGER; VAR [nil] pGUID: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClassFactory2) GetLicInfo* (VAR [nil] pLicInfo: LICINFO): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClassFactory2) RequestLicKey* (dwResrved: INTEGER; VAR [nil] pbstrKey: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClassFactory2) CreateInstanceLic* (pUnkOuter: COM.IUnknown; pUnkReserved: COM.IUnknown; IN [iid] riid: COM.GUID; bstrKey: WinOle.BSTR; OUT [new] ppvObject: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ISpecifyPropertyPages) GetPages* (VAR [nil] pPages: CAUUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPerPropertyBrowsing) GetDisplayString* (dispid: WinOleAut.DISPID; VAR [nil] lpbstr: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPerPropertyBrowsing) MapPropertyToPage* (dispid: WinOleAut.DISPID; VAR [nil] lpclsid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPerPropertyBrowsing) GetPredefinedStrings* (dispid: WinOleAut.DISPID; VAR [nil] lpcaStringsOut: CALPOLESTR; VAR [nil] lpcaCookiesOut: CADWORD): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPerPropertyBrowsing) GetPredefinedValue* (dispid: WinOleAut.DISPID; dwCookie: INTEGER; VAR [nil] lpvarOut: WinOleAut.VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPageSite) OnStatusChange* (flags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPageSite) GetLocaleID* (VAR [nil] pLocaleID: WinOle.LCID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPageSite) GetPageContainer* (VAR [nil] ppUnk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPageSite) TranslateAccelerator* (VAR [nil] lpMsg: WinApi.MSG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) SetPageSite* (pPageSite: IPropertyPageSite): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) Activate* (hwndParent: WinApi.HWND; VAR [nil] lprc: WinApi.RECT; bModal: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) Deactivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) GetPageInfo* (VAR [nil] pPageInfo: PROPPAGEINFO): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) SetObjects* (cObjects: INTEGER; VAR [nil] ppunk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) Show* (nCmdShow: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) Move* (VAR [nil] prect: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) IsPageDirty* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) Apply* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) Help* (lpszHelpDir: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage) TranslateAccelerator* (VAR [nil] lpMsg: WinApi.MSG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyPage2) EditProperty* (dispid: WinOleAut.DISPID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Name* (VAR [nil] pname: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Name* (name: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Size* (VAR [nil] psize: WinApi.CY): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Size* (size: WinApi.CY): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Bold* (VAR [nil] pbold: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Bold* (bold: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Italic* (VAR [nil] pitalic: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Italic* (italic: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Underline* (VAR [nil] punderline: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Underline* (underline: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Strikethrough* (VAR [nil] pstrikethrough: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Strikethrough* (strikethrough: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Weight* (VAR [nil] pweight: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Weight* (weight: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_Charset* (VAR [nil] pcharset: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) put_Charset* (charset: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) get_hFont* (VAR [nil] phfont: WinApi.HFONT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) Clone* (VAR [nil] lplpfont: IFont): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) IsEqual* (lpFontOther: IFont): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) SetRatio* (cyLogical: INTEGER; cyHimetric: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) QueryTextMetrics* (VAR [nil] lptm: TEXTMETRICOLE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) AddRefHfont* (hfont: WinApi.HFONT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) ReleaseHfont* (hfont: WinApi.HFONT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IFont) SetHdc* (hdc: WinApi.HDC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_Handle* (VAR [nil] phandle: OLE_HANDLE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_hPal* (VAR [nil] phpal: OLE_HANDLE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_Type* (VAR [nil] ptype: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_Width* (VAR [nil] pwidth: OLE_XSIZE_HIMETRIC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_Height* (VAR [nil] pheight: OLE_YSIZE_HIMETRIC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) Render* (hdc: WinApi.HDC; x: INTEGER; y: INTEGER; cx: INTEGER; cy: INTEGER; xSrc: OLE_XPOS_HIMETRIC; ySrc: OLE_YPOS_HIMETRIC; cxSrc: OLE_XSIZE_HIMETRIC; cySrc: OLE_YSIZE_HIMETRIC; VAR [nil] lprcWBounds: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) set_hPal* (hpal: OLE_HANDLE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_CurDC* (VAR [nil] phdcOut: WinApi.HDC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) SelectPicture* (hdcIn: WinApi.HDC; VAR [nil] phdcOut: WinApi.HDC; VAR [nil] phbmpOut: OLE_HANDLE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_KeepOriginalFormat* (VAR [nil] pfkeep: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) put_KeepOriginalFormat* (fkeep: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) PictureChanged* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) SaveAsFile* (lpstream: WinOle.IStream; fSaveMemCopy: WinApi.BOOL; VAR [nil] lpcbSize: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPicture) get_Attributes* (VAR [nil] lpdwAttr: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE OleCreatePropertyFrame* (hwndOwner: WinApi.HWND; x: INTEGER; y: INTEGER; lpszCaption: WinApi.PtrWSTR; cObjects: INTEGER; VAR [nil] ppUnk: COM.IUnknown; cPages: INTEGER; VAR [nil] pPageClsID: COM.GUID; lcid: WinOle.LCID; dwReserved: INTEGER; pvReserved: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE OleCreatePropertyFrameIndirect* (VAR [nil] lpParams: OCPFIPARAMS): COM.RESULT;

	PROCEDURE OleTranslateColor* (clr: OLE_COLOR; hpal: WinApi.HPALETTE; VAR [nil] lpcolorref: WinApi.COLORREF): COM.RESULT;

	PROCEDURE OleCreateFontIndirect* (VAR [nil] lpFontDesc: FONTDESC; IN [iid] riid: COM.GUID; OUT [new] lplpvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreatePictureIndirect* (VAR [nil] lpPictDesc: PICTDESC; IN [iid] riid: COM.GUID; fOwn: WinApi.BOOL; OUT [new] lplpvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleLoadPicture* (lpstream: WinOle.IStream; lSize: INTEGER; fRunmode: WinApi.BOOL; IN [iid] riid: COM.GUID; OUT [new] lplpvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleIconToCursor* (hinstExe: WinApi.HINSTANCE; hIcon: WinApi.HICON): WinApi.HCURSOR;

END WinOleCtl.
