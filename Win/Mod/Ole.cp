MODULE WinOle ["OLE32.dll"];
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

	IMPORT COM, WinApi;

	CONST
		GUID_NULL* = "{00000000-0000-0000-0000-000000000000}";

	CONST (* macros *)
		WIN32* = 100;
		CLSCTX_ALL* = {0..2, 4};
		CLSCTX_INPROC* = {0, 1};
		CLSCTX_SERVER* = {0, 2, 4};
		MARSHALINTERFACE_MIN* = 500;
		CWCSTORAGENAME* = 32;
		STGM_DIRECT* = {};
		STGM_TRANSACTED* = {16};
		STGM_SIMPLE* = {27};
		STGM_READ* = {};
		STGM_WRITE* = {0};
		STGM_READWRITE* = {1};
		STGM_SHARE_DENY_NONE* = {6};
		STGM_SHARE_DENY_READ* = {4, 5};
		STGM_SHARE_DENY_WRITE* = {5};
		STGM_SHARE_EXCLUSIVE* = {4};
		STGM_PRIORITY* = {18};
		STGM_DELETEONRELEASE* = {26};
		STGM_NOSCRATCH* = {20};
		STGM_CREATE* = {12};
		STGM_CONVERT* = {17};
		STGM_FAILIFTHERE* = {};
		ROTFLAGS_REGISTRATIONKEEPSALIVE* = {0};
		ROT_COMPARE_MAX* = 2048;
		WDT_INPROC_CALL* = 1232364631;
		WDT_REMOTE_CALL* = 1383359575;
		DECIMAL_NEG* = 128;
		PROPSETFLAG_DEFAULT* = 0;
		PROPSETFLAG_NONSIMPLE* = 1;
		PROPSETFLAG_ANSI* = 2;
		PID_DICTIONARY* = 0;
		PID_CODEPAGE* = 1;
		PID_FIRST_NAME_DEFAULT* = 4095;
		PID_LOCALE* = 80000000H;
		PID_MODIFY_TIME* = -2147483647;
		PID_SECURITY* = -2147483646;
		PID_ILLEGAL* = -1;
		PRSPEC_INVALID* = -1;
		PRSPEC_LPWSTR* = 0;
		PRSPEC_PROPID* = 1;
		COM_RIGHTS_EXECUTE* = 1;
		E_DRAW* = -2147221184;
		DATA_E_FORMATETC* = -2147221404;
		OLEIVERB_PRIMARY* = 0;
		OLEIVERB_SHOW* = -1;
		OLEIVERB_OPEN* = -2;
		OLEIVERB_HIDE* = -3;
		OLEIVERB_UIACTIVATE* = -4;
		OLEIVERB_INPLACEACTIVATE* = -5;
		OLEIVERB_DISCARDUNDOSTATE* = -6;
		EMBDHLP_INPROC_HANDLER* = {};
		EMBDHLP_INPROC_SERVER* = {0};
		EMBDHLP_CREATENOW* = {};
		EMBDHLP_DELAYCREATE* = {16};
		UPDFCACHE_NODATACACHE* = {0};
		UPDFCACHE_ONSAVECACHE* = {1};
		UPDFCACHE_ONSTOPCACHE* = {2};
		UPDFCACHE_NORMALCACHE* = {3};
		UPDFCACHE_IFBLANK* = {4};
		UPDFCACHE_ONLYIFBLANK* = {31};
		UPDFCACHE_IFBLANKORONSAVECACHE* = {1, 4};
		UPDFCACHE_ALL* = {0..30};
		UPDFCACHE_ALLBUTNODATACACHE* = {1..30};
		MK_ALT* = {5};
		DROPEFFECT_NONE* = {};
		DROPEFFECT_COPY* = {0};
		DROPEFFECT_MOVE* = {1};
		DROPEFFECT_LINK* = {2};
		DROPEFFECT_SCROLL* = {31};
		DD_DEFSCROLLINSET* = 11;
		DD_DEFSCROLLDELAY* = 50;
		DD_DEFSCROLLINTERVAL* = 50;
		DD_DEFDRAGDELAY* = 200;
		DD_DEFDRAGMINDIST* = 2;

	CONST (* enumerations *)
		REGCLS_SINGLEUSE* = {};
		REGCLS_MULTIPLEUSE* = {0};
		REGCLS_MULTI_SEPARATE* = {1};
		MEMCTX_TASK* = 1;
		MEMCTX_SHARED* = 2;
		MEMCTX_MACSYSTEM* = 3;
		MEMCTX_UNKNOWN* = -1;
		MEMCTX_SAME* = -2;
		CLSCTX_INPROC_SERVER* = {0};
		CLSCTX_INPROC_HANDLER* = {1};
		CLSCTX_LOCAL_SERVER* = {2};
		CLSCTX_INPROC_SERVER16* = {3};
		CLSCTX_REMOTE_SERVER* = {4};
		CLSCTX_INPROC_HANDLER16* = {5};
		CLSCTX_INPROC_SERVERX86* = {6};
		CLSCTX_INPROC_HANDLERX86* = {7};
		MSHLFLAGS_NORMAL* = {};
		MSHLFLAGS_TABLESTRONG* = {0};
		MSHLFLAGS_TABLEWEAK* = {1};
		MSHCTX_LOCAL* = 0;
		MSHCTX_NOSHAREDMEM* = 1;
		MSHCTX_DIFFERENTMACHINE* = 2;
		MSHCTX_INPROC* = 3;
		DVASPECT_CONTENT* = {0};
		DVASPECT_THUMBNAIL* = {1};
		DVASPECT_ICON* = {2};
		DVASPECT_DOCPRINT* = {3};
		STGC_DEFAULT* = {};
		STGC_OVERWRITE* = {0};
		STGC_ONLYIFCURRENT* = {1};
		STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE* = {2};
		STGMOVE_MOVE* = 0;
		STGMOVE_COPY* = 1;
		STGMOVE_SHALLOWCOPY* = 2;
		STATFLAG_DEFAULT* = 0;
		STATFLAG_NONAME* = 1;
		STATFLAG_NOOPEN* = 2;
		VT_EMPTY* = 0;
		VT_NULL* = 1;
		VT_I2* = 2;
		VT_I4* = 3;
		VT_R4* = 4;
		VT_R8* = 5;
		VT_CY* = 6;
		VT_DATE* = 7;
		VT_BSTR* = 8;
		VT_DISPATCH* = 9;
		VT_ERROR* = 10;
		VT_BOOL* = 11;
		VT_VARIANT* = 12;
		VT_UNKNOWN* = 13;
		VT_DECIMAL* = 14;
		VT_I1* = 16;
		VT_UI1* = 17;
		VT_UI2* = 18;
		VT_UI4* = 19;
		VT_I8* = 20;
		VT_UI8* = 21;
		VT_INT* = 22;
		VT_UINT* = 23;
		VT_VOID* = 24;
		VT_HRESULT* = 25;
		VT_PTR* = 26;
		VT_SAFEARRAY* = 27;
		VT_CARRAY* = 28;
		VT_USERDEFINED* = 29;
		VT_LPSTR* = 30;
		VT_LPWSTR* = 31;
		VT_FILETIME* = 64;
		VT_BLOB* = 65;
		VT_STREAM* = 66;
		VT_STORAGE* = 67;
		VT_STREAMED_OBJECT* = 68;
		VT_STORED_OBJECT* = 69;
		VT_BLOB_OBJECT* = 70;
		VT_CF* = 71;
		VT_CLSID* = 72;
		VT_VECTOR* = 4096;
		VT_ARRAY* = 8192;
		VT_BYREF* = 16384;
		VT_RESERVED* = 32768;
		VT_ILLEGAL* = 65535;
		VT_ILLEGALMASKED* = 4095;
		VT_TYPEMASK* = 4095;
		EXTCONN_STRONG* = {0};
		EXTCONN_WEAK* = {1};
		EXTCONN_CALLABLE* = {2};
		BIND_MAYBOTHERUSER* = {0};
		BIND_JUSTTESTEXISTENCE* = {1};
		MKSYS_NONE* = 0;
		MKSYS_GENERICCOMPOSITE* = 1;
		MKSYS_FILEMONIKER* = 2;
		MKSYS_ANTIMONIKER* = 3;
		MKSYS_ITEMMONIKER* = 4;
		MKSYS_POINTERMONIKER* = 5;
		MKRREDUCE_ONE* = 196608;
		MKRREDUCE_TOUSER* = 131072;
		MKRREDUCE_THROUGHUSER* = 65536;
		MKRREDUCE_ALL* = 0;
		STGTY_STORAGE* = 1;
		STGTY_STREAM* = 2;
		STGTY_LOCKBYTES* = 3;
		STGTY_PROPERTY* = 4;
		STREAM_SEEK_SET* = 0;
		STREAM_SEEK_CUR* = 1;
		STREAM_SEEK_END* = 2;
		LOCK_WRITE* = {0};
		LOCK_EXCLUSIVE* = {1};
		LOCK_ONLYONCE* = {2};
		ADVF_NODATA* = {0};
		ADVF_PRIMEFIRST* = {1};
		ADVF_ONLYONCE* = {2};
		ADVF_DATAONSTOP* = {6};
		ADVFCACHE_NOHANDLER* = {3};
		ADVFCACHE_FORCEBUILTIN* = {4};
		ADVFCACHE_ONSAVE* = {5};
		TYMED_HGLOBAL* = {0};
		TYMED_FILE* = {1};
		TYMED_ISTREAM* = {2};
		TYMED_ISTORAGE* = {3};
		TYMED_GDI* = {4};
		TYMED_MFPICT* = {5};
		TYMED_ENHMF* = {6};
		TYMED_NULL* = {};
		DATADIR_GET* = {0};
		DATADIR_SET* = {1};
		CALLTYPE_TOPLEVEL* = 1;
		CALLTYPE_NESTED* = 2;
		CALLTYPE_ASYNC* = 3;
		CALLTYPE_TOPLEVEL_CALLPENDING* = 4;
		CALLTYPE_ASYNC_CALLPENDING* = 5;
		SERVERCALL_ISHANDLED* = 0;
		SERVERCALL_REJECTED* = 1;
		SERVERCALL_RETRYLATER* = 2;
		PENDINGTYPE_TOPLEVEL* = 1;
		PENDINGTYPE_NESTED* = 2;
		PENDINGMSG_CANCELCALL* = 0;
		PENDINGMSG_WAITNOPROCESS* = 1;
		PENDINGMSG_WAITDEFPROCESS* = 2;
		EOAC_NONE* = 0;
		EOAC_MUTUAL_AUTH* = 1;
		COINIT_MULTITHREADED* = 0;
		COINIT_APARTMENTTHREADED* = 2;
		COINIT_DISABLE_OLE1DDE* = 4;
		DISCARDCACHE_SAVEIFDIRTY* = 0;
		DISCARDCACHE_NOSAVE* = 1;
		OLEGETMONIKER_ONLYIFTHERE* = 1;
		OLEGETMONIKER_FORCEASSIGN* = 2;
		OLEGETMONIKER_UNASSIGN* = 3;
		OLEGETMONIKER_TEMPFORUSER* = 4;
		OLEWHICHMK_CONTAINER* = 1;
		OLEWHICHMK_OBJREL* = 2;
		OLEWHICHMK_OBJFULL* = 3;
		USERCLASSTYPE_FULL* = 1;
		USERCLASSTYPE_SHORT* = 2;
		USERCLASSTYPE_APPNAME* = 3;
		OLEMISC_RECOMPOSEONRESIZE* = {0};
		OLEMISC_ONLYICONIC* = {1};
		OLEMISC_INSERTNOTREPLACE* = {2};
		OLEMISC_STATIC* = {3};
		OLEMISC_CANTLINKINSIDE* = {4};
		OLEMISC_CANLINKBYOLE1* = {5};
		OLEMISC_ISLINKOBJECT* = {6};
		OLEMISC_INSIDEOUT* = {7};
		OLEMISC_ACTIVATEWHENVISIBLE* = {8};
		OLEMISC_RENDERINGISDEVICEINDEPENDENT* = {9};
		OLECLOSE_SAVEIFDIRTY* = 0;
		OLECLOSE_NOSAVE* = 1;
		OLECLOSE_PROMPTSAVE* = 2;
		OLERENDER_NONE* = 0;
		OLERENDER_DRAW* = 1;
		OLERENDER_FORMAT* = 2;
		OLERENDER_ASIS* = 3;
		OLEUPDATE_ALWAYS* = 1;
		OLEUPDATE_ONCALL* = 3;
		OLELINKBIND_EVENIFCLASSDIFF* = {0};
		BINDSPEED_INDEFINITE* = 1;
		BINDSPEED_MODERATE* = 2;
		BINDSPEED_IMMEDIATE* = 3;
		OLECONTF_EMBEDDINGS* = {0};
		OLECONTF_LINKS* = {1};
		OLECONTF_OTHERS* = {2};
		OLECONTF_ONLYUSER* = {3};
		OLECONTF_ONLYIFRUNNING* = {4};
		OLEVERBATTRIB_NEVERDIRTIES* = {0};
		OLEVERBATTRIB_ONCONTAINERMENU* = {1};
		
	CONST (* additional *)
		STGFMT_DOCUMENT*  = 0;
		STGFMT_DIRECTORY* = 1;
		STGFMT_CATALOG*   = 2;
		STGFMT_FILE*      = 3;


	TYPE
		BSTR* = POINTER TO ARRAY [untagged] OF CHAR;
		PtrWSTR* = WinApi.PtrWSTR;
		REGCLS* = INTEGER;
		RemHGLOBAL* = RECORD [untagged]
			fNullHGlobal*: INTEGER;
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		RemHMETAFILEPICT* = RECORD [untagged]
			mm*: INTEGER;
			xExt*: INTEGER;
			yExt*: INTEGER;
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		RemHENHMETAFILE* = RECORD [untagged]
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		RemHBITMAP* = RECORD [untagged]
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		RemHPALETTE* = RECORD [untagged]
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		RemHBRUSH* = RECORD [untagged]
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		MEMCTX* = INTEGER;
		CLSCTX* = INTEGER;
		MSHLFLAGS* = INTEGER;
		MSHCTX* = INTEGER;
		DVASPECT* = INTEGER;
		STGC* = INTEGER;
		STGMOVE* = INTEGER;
		STATFLAG* = INTEGER;
		LCID* = INTEGER;
		BYTE_BLOB* = RECORD [untagged]
			clSize*: INTEGER;
			abData*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		PtrBYTE_BLOB* = POINTER TO BYTE_BLOB;
		WORD_BLOB* = RECORD [untagged]
			clSize*: INTEGER;
			asData*: ARRAY [untagged] 1 OF SHORTINT;
		END;
		PtrWORD_BLOB* = POINTER TO WORD_BLOB;
		DWORD_BLOB* = RECORD [untagged]
			clSize*: INTEGER;
			alData*: ARRAY [untagged] 1 OF INTEGER;
		END;
		PtrDWORD_BLOB* = POINTER TO DWORD_BLOB;
		FLAGGED_BYTE_BLOB* = RECORD [untagged]
			fFlags*: INTEGER;
			clSize*: INTEGER;
			abData*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		PtrFLAGGED_BYTE_BLOB* = POINTER TO FLAGGED_BYTE_BLOB;
		FLAGGED_WORD_BLOB* = RECORD [untagged]
			fFlags*: INTEGER;
			clSize*: INTEGER;
			asData*: ARRAY [untagged] 1 OF SHORTINT;
		END;
		PtrFLAGGED_WORD_BLOB* = POINTER TO FLAGGED_WORD_BLOB;
		BYTE_SIZEDARR* = RECORD [untagged]
			clSize*: INTEGER;
			pData*: POINTER TO (*?*) ARRAY [untagged] OF SHORTCHAR;
		END;
		WORD_SIZEDARR* = RECORD [untagged]
			clSize*: INTEGER;
			pData*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
		END;
		DWORD_SIZEDARR* = RECORD [untagged]
			clSize*: INTEGER;
			pData*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
		END;
		HYPER_SIZEDARR* = RECORD [untagged]
			clSize*: INTEGER;
			pData*: POINTER TO (*?*) ARRAY [untagged] OF LONGINT;
		END;
		GDI_NONREMOTE* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtrDWORD_BLOB;
				hGlobal*: INTEGER;
			END;
		END;
		userHGLOBAL* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtrFLAGGED_BYTE_BLOB;
				hGlobal*: INTEGER;
			END;
		END;
		PtruserHGLOBAL* = POINTER TO userHGLOBAL;
		userHMETAFILE* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtrBYTE_BLOB;
				hGlobal*: INTEGER;
			END;
		END;
		PtruserHMETAFILE* = POINTER TO userHMETAFILE;
		remoteMETAFILEPICT* = RECORD [untagged]
			mm*: INTEGER;
			xExt*: INTEGER;
			yExt*: INTEGER;
			hMF*: PtruserHMETAFILE;
		END;
		PtrremoteMETAFILEPICT* = POINTER TO remoteMETAFILEPICT;
		userHMETAFILEPICT* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtrremoteMETAFILEPICT;
				hGlobal*: INTEGER;
			END;
		END;
		PtruserHMETAFILEPICT* = POINTER TO userHMETAFILEPICT;
		userHENHMETAFILE* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtrBYTE_BLOB;
				hGlobal*: INTEGER;
			END;
		END;
		PtruserHENHMETAFILE* = POINTER TO userHENHMETAFILE;
		userBITMAP* = RECORD [untagged]
			bmType*: INTEGER;
			bmWidth*: INTEGER;
			bmHeight*: INTEGER;
			bmWidthBytes*: INTEGER;
			bmPlanes*: SHORTINT;
			bmBitsPixel*: SHORTINT;
			cbSize*: INTEGER;
			pBuffer*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		PtruserBITMAP* = POINTER TO userBITMAP;
		userHBITMAP* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtruserBITMAP;
				hGlobal*: INTEGER;
			END;
		END;
		PtruserHBITMAP* = POINTER TO userHBITMAP;
		rpcLOGPALETTE* = RECORD [untagged]
			palVersion*: SHORTINT;
			palNumEntries*: SHORTINT;
			palPalEntry*: ARRAY [untagged] 1 OF WinApi.PALETTEENTRY;
		END;
		PtrrpcLOGPALETTE* = POINTER TO rpcLOGPALETTE;
		userHPALETTE* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: PtrrpcLOGPALETTE;
				hGlobal*: INTEGER;
			END;
		END;
		PtruserHPALETTE* = POINTER TO userHPALETTE;
		RemotableHandle* = RECORD [untagged]
			fContext*: INTEGER;
			u*: RECORD [union]
				hInproc*: INTEGER;
				hRemote*: INTEGER;
				hGlobal*: INTEGER;
			END;
		END;
		PtrRemotableHandle* = POINTER TO RemotableHandle;
		tagCY* = RECORD [union]
			r*: RECORD [untagged]
				Lo*: INTEGER;
				Hi*: INTEGER;
			END;
			int64*: LONGINT;
		END;
		DECIMAL* = RECORD [align8]
			wReserved*: SHORTINT;
			u*: RECORD [union]
				r*: RECORD [untagged]
					scale*: SHORTCHAR;
					sign*: SHORTCHAR;
				END;
				signscale*: SHORTINT;
			END;
			Hi32*: INTEGER;
			u1*: RECORD [union]
				r*: RECORD [untagged]
					Lo32*: INTEGER;
					Mid32*: INTEGER;
				END;
				Lo64*: LONGINT;
			END;
		END;
		PtrDECIMAL* = POINTER TO DECIMAL;
		VARIANT_BOOL* = SHORTINT;
		BLOB* = RECORD [untagged]
			cbSize*: INTEGER;
			pBlobData*: POINTER TO (*?*) ARRAY [untagged] OF SHORTCHAR;
		END;
		PtrBLOB* = POINTER TO BLOB;
		CLIPDATA* = RECORD [untagged]
			cbSize*: INTEGER;
			ulClipFmt*: INTEGER;
			pClipData*: POINTER TO (*?*) ARRAY [untagged] OF SHORTCHAR;
		END;
		PtrCLIPDATA* = POINTER TO CLIPDATA;
		VARTYPE* = SHORTINT;
		VARENUM* = INTEGER;
		PROPID* = INTEGER;
		IClassFactory* = POINTER TO ABSTRACT RECORD ["{00000001-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IMarshal* = POINTER TO ABSTRACT RECORD ["{00000003-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IMalloc* = POINTER TO ABSTRACT RECORD ["{00000002-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IMallocSpy* = POINTER TO ABSTRACT RECORD ["{0000001d-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IStdMarshalInfo* = POINTER TO ABSTRACT RECORD ["{00000018-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		EXTCONN* = INTEGER;
		IExternalConnection* = POINTER TO ABSTRACT RECORD ["{00000019-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IEnumUnknown* = POINTER TO ABSTRACT RECORD ["{00000100-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		BIND_OPTS* = RECORD [untagged]
			cbStruct*: INTEGER;
			grfFlags*: SET;
			grfMode*: SET;
			dwTickCountDeadline*: INTEGER;
		END;
		PtrBIND_OPTS* = POINTER TO BIND_OPTS;
		BIND_FLAGS* = INTEGER;
		IBindCtx* = POINTER TO ABSTRACT RECORD ["{0000000e-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IEnumMoniker* = POINTER TO ABSTRACT RECORD ["{00000102-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IRunnableObject* = POINTER TO ABSTRACT RECORD ["{00000126-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IRunningObjectTable* = POINTER TO ABSTRACT RECORD ["{00000010-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IPersist* = POINTER TO ABSTRACT RECORD ["{0000010c-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IPersistStream* = POINTER TO ABSTRACT RECORD ["{00000109-0000-0000-C000-000000000046}"] (IPersist)
		END;
		MKSYS* = INTEGER;
		MKRREDUCE* = INTEGER;
		IMoniker* = POINTER TO ABSTRACT RECORD ["{0000000f-0000-0000-C000-000000000046}"] (IPersistStream)
		END;
		IROTData* = POINTER TO ABSTRACT RECORD ["{f29f6bc0-5021-11ce-aa15-00006901293f}"] (COM.IUnknown)
		END;
		IEnumString* = POINTER TO ABSTRACT RECORD ["{00000101-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		ISequentialStream* = POINTER TO ABSTRACT RECORD ["{0c733a30-2a1c-11ce-ade5-00aa0044773d}"] (COM.IUnknown)
		END;
		STATSTG* = RECORD [align8]
			pwcsName*: WinApi.PtrWSTR;
			type*: INTEGER;
			cbSize*: LONGINT;
			mtime*: WinApi.FILETIME;
			ctime*: WinApi.FILETIME;
			atime*: WinApi.FILETIME;
			grfMode*: SET;
			grfLocksSupported*: SET;
			clsid*: COM.GUID;
			grfStateBits*: SET;
			dwStgFmt*: INTEGER;
		END;
		PtrSTATSTG* = POINTER TO STATSTG;
		STGTY* = INTEGER;
		STREAM_SEEK* = INTEGER;
		LOCKTYPE* = INTEGER;
		IStream* = POINTER TO ABSTRACT RECORD ["{0000000c-0000-0000-C000-000000000046}"] (ISequentialStream)
		END;
		IEnumSTATSTG* = POINTER TO ABSTRACT RECORD ["{0000000d-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		RemSNB* = RECORD [untagged]
			ulCntStr*: INTEGER;
			ulCntChar*: INTEGER;
			rgString*: ARRAY [untagged] 1 OF CHAR;
		END;
		PtrRemSNB* = POINTER TO RemSNB;
		SNB* = POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrWSTR;
		IStorage* = POINTER TO ABSTRACT RECORD ["{0000000b-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IPersistFile* = POINTER TO ABSTRACT RECORD ["{0000010b-0000-0000-C000-000000000046}"] (IPersist)
		END;
		IPersistStorage* = POINTER TO ABSTRACT RECORD ["{0000010a-0000-0000-C000-000000000046}"] (IPersist)
		END;
		ILockBytes* = POINTER TO ABSTRACT RECORD ["{0000000a-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		DVTARGETDEVICE* = RECORD [untagged]
			tdSize*: INTEGER;
			tdDriverNameOffset*: SHORTINT;
			tdDeviceNameOffset*: SHORTINT;
			tdPortNameOffset*: SHORTINT;
			tdExtDevmodeOffset*: SHORTINT;
			tdData*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		PtrDVTARGETDEVICE* = POINTER TO DVTARGETDEVICE;
		CLIPFORMAT* = SHORTINT;
		FORMATETC* = RECORD [untagged]
			cfFormat*: CLIPFORMAT;
			ptd*: PtrDVTARGETDEVICE;
			dwAspect*: SET;
			lindex*: INTEGER;
			tymed*: SET;
		END;
		PtrFORMATETC* = POINTER TO FORMATETC;
		IEnumFORMATETC* = POINTER TO ABSTRACT RECORD ["{00000103-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		ADVF* = INTEGER;
		STATDATA* = RECORD [untagged]
			formatetc*: FORMATETC;
			advf*: SET;
			pAdvSink*: IAdviseSink;
			dwConnection*: INTEGER;
		END;
		PtrSTATDATA* = POINTER TO STATDATA;
		IEnumSTATDATA* = POINTER TO ABSTRACT RECORD ["{00000105-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IRootStorage* = POINTER TO ABSTRACT RECORD ["{00000012-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		TYMED* = INTEGER;
		RemSTGMEDIUM* = RECORD [untagged]
			tymed*: SET;
			dwHandleType*: INTEGER;
			pData*: INTEGER;
			pUnkForRelease*: INTEGER;
			cbData*: INTEGER;
			data*: ARRAY [untagged] 1 OF SHORTCHAR;
		END;
		STGMEDIUM* = RECORD [untagged]
			tymed*: SET;
			u*: RECORD [union]
				hBitmap*: WinApi.HBITMAP;
				hMetaFilePict*: WinApi.HMETAFILEPICT;
				hEnhMetaFile*: WinApi.HENHMETAFILE;
				hGlobal*: WinApi.HGLOBAL;
				lpszFileName*: WinApi.PtrWSTR;
				pstm*: IStream;
				pstg*: IStorage;
			END;
			pUnkForRelease*: COM.IUnknown;
		END;
		PtrSTGMEDIUM* = POINTER TO STGMEDIUM;
		GDI_OBJECT* = RECORD [untagged]
			ObjectType*: INTEGER;
			u*: RECORD [union]
				hBitmap*: PtruserHBITMAP;
				hPalette*: PtruserHPALETTE;
				hGeneric*: PtruserHGLOBAL;
			END;
		END;
		PtrGDI_OBJECT* = POINTER TO GDI_OBJECT;
		_STGMEDIUM_UNION* = RECORD [untagged]
			tymed*: SET;
			u*: RECORD [union]
				hMetaFilePict*: PtruserHMETAFILEPICT;
				hHEnhMetaFile*: PtruserHENHMETAFILE;
				hGdiHandle*: PtrGDI_OBJECT;
				hGlobal*: PtruserHGLOBAL;
				lpszFileName*: WinApi.PtrWSTR;
				pstm*: PtrBYTE_BLOB;
				pstg*: PtrBYTE_BLOB;
			END;
		END;
		userSTGMEDIUM* = RECORD [untagged]
			r*: _STGMEDIUM_UNION;
			pUnkForRelease*: COM.IUnknown;
		END;
		PtruserSTGMEDIUM* = POINTER TO userSTGMEDIUM;
		userFLAG_STGMEDIUM* = RECORD [untagged]
			ContextFlags*: SET;
			fPassOwnership*: INTEGER;
			Stgmed*: userSTGMEDIUM;
		END;
		PtruserFLAG_STGMEDIUM* = POINTER TO userFLAG_STGMEDIUM;
		FLAG_STGMEDIUM* = RECORD [untagged]
			ContextFlags*: SET;
			fPassOwnership*: INTEGER;
			Stgmed*: STGMEDIUM;
		END;
		PtrFLAG_STGMEDIUM* = POINTER TO FLAG_STGMEDIUM;
		IAdviseSink* = POINTER TO ABSTRACT RECORD ["{0000010f-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IAdviseSink2* = POINTER TO ABSTRACT RECORD ["{00000125-0000-0000-C000-000000000046}"] (IAdviseSink)
		END;
		DATADIR* = INTEGER;
		IDataObject* = POINTER TO ABSTRACT RECORD ["{0000010e-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IDataAdviseHolder* = POINTER TO ABSTRACT RECORD ["{00000110-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		CALLTYPE* = INTEGER;
		SERVERCALL* = INTEGER;
		PENDINGTYPE* = INTEGER;
		PENDINGMSG* = INTEGER;
		INTERFACEINFO* = RECORD [untagged]
			pUnk*: COM.IUnknown;
			iid*: COM.GUID;
			wMethod*: SHORTINT;
		END;
		PtrINTERFACEINFO* = POINTER TO INTERFACEINFO;
		IMessageFilter* = POINTER TO ABSTRACT RECORD ["{00000016-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		RPCOLEDATAREP* = INTEGER;
		RPCOLEMESSAGE* = RECORD [untagged]
			reserved1*: WinApi.PtrVoid;
			dataRepresentation*: RPCOLEDATAREP;
			Buffer*: WinApi.PtrVoid;
			cbBuffer*: INTEGER;
			iMethod*: INTEGER;
			reserved2*: ARRAY [untagged] 5 OF WinApi.PtrVoid;
			rpcFlags*: INTEGER;
		END;
		PtrRPCOLEMESSAGE* = POINTER TO RPCOLEMESSAGE;
		IRpcChannelBuffer* = POINTER TO ABSTRACT RECORD ["{D5F56B60-593B-101A-B569-08002B2DBF7A}"] (COM.IUnknown)
		END;
		IRpcProxyBuffer* = POINTER TO ABSTRACT RECORD ["{D5F56A34-593B-101A-B569-08002B2DBF7A}"] (COM.IUnknown)
		END;
		IRpcStubBuffer* = POINTER TO ABSTRACT RECORD ["{D5F56AFC-593B-101A-B569-08002B2DBF7A}"] (COM.IUnknown)
		END;
		IPSFactoryBuffer* = POINTER TO ABSTRACT RECORD ["{D5F569D0-593B-101A-B569-08002B2DBF7A}"] (COM.IUnknown)
		END;
		FMTID* = COM.GUID;
		CAUB* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: WinApi.PtrSTR;
		END;
		CAI* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
		END;
		CAUI* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
		END;
		CAL* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
		END;
		CAUL* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
		END;
		CAFLT* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF SHORTREAL;
		END;
		CADBL* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF REAL;
		END;
		CACY* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.CY;
		END;
		CADATE* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.DATE;
		END;
		CABSTR* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF BSTR;
		END;
		CABOOL* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF VARIANT_BOOL;
		END;
		CASCODE* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF COM.RESULT;
		END;
		PtrPROPVARIANT* = POINTER TO PROPVARIANT;
		CAPROPVARIANT* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: PtrPROPVARIANT;
		END;
		CAH* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF LONGINT;
		END;
		CAUH* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF LONGINT;
		END;
		CALPSTR* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrSTR;
		END;
		CALPWSTR* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrWSTR;
		END;
		CAFILETIME* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.FILETIME;
		END;
		CACLIPDATA* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: PtrCLIPDATA;
		END;
		CACLSID* = RECORD [untagged]
			cElems*: INTEGER;
			pElems*: POINTER TO (*?*) ARRAY [untagged] OF COM.GUID;
		END;
		PROPVARIANT* = RECORD [align8]
			vt*: VARTYPE;
			wReserved1*: SHORTINT;
			wReserved2*: SHORTINT;
			wReserved3*: SHORTINT;
			u*: RECORD [union]
				bVal*: SHORTCHAR;
				iVal*: SHORTINT;
				uiVal*: SHORTINT;
				bool*: VARIANT_BOOL;
				lVal*: INTEGER;
				ulVal*: INTEGER;
				fltVal*: SHORTREAL;
				scode*: COM.RESULT;
				hVal*: LONGINT;
				uhVal*: LONGINT;
				dblVal*: REAL;
				cyVal*: WinApi.CY;
				date*: WinApi.DATE;
				filetime*: WinApi.FILETIME;
				puuid*: POINTER TO (*?*) ARRAY [untagged] OF COM.GUID;
				blob*: BLOB;
				pclipdata*: PtrCLIPDATA;
				pStream*: IStream;
				pStorage*: IStorage;
				bstrVal*: BSTR;
				pszVal*: WinApi.PtrSTR;
				pwszVal*: WinApi.PtrWSTR;
				caub*: CAUB;
				cai*: CAI;
				caui*: CAUI;
				cabool*: CABOOL;
				cal*: CAL;
				caul*: CAUL;
				caflt*: CAFLT;
				cascode*: CASCODE;
				cah*: CAH;
				cauh*: CAUH;
				cadbl*: CADBL;
				cacy*: CACY;
				cadate*: CADATE;
				cafiletime*: CAFILETIME;
				cauuid*: CACLSID;
				caclipdata*: CACLIPDATA;
				cabstr*: CABSTR;
				calpstr*: CALPSTR;
				calpwstr*: CALPWSTR;
				capropvar*: CAPROPVARIANT;
			END;
		END;
		PROPSPEC* = RECORD [untagged]
			ulKind*: INTEGER;
			u*: RECORD [union]
				propid*: PROPID;
				lpwstr*: WinApi.PtrWSTR;
			END;
		END;
		STATPROPSTG* = RECORD [untagged]
			lpwstrName*: WinApi.PtrWSTR;
			propid*: PROPID;
			vt*: VARTYPE;
		END;
		PtrSTATPROPSTG* = POINTER TO STATPROPSTG;
		STATPROPSETSTG* = RECORD [align8]
			fmtid*: FMTID;
			clsid*: COM.GUID;
			grfFlags*: SET;
			mtime*: WinApi.FILETIME;
			ctime*: WinApi.FILETIME;
			atime*: WinApi.FILETIME;
		END;
		PtrSTATPROPSETSTG* = POINTER TO STATPROPSETSTG;
		IPropertyStorage* = POINTER TO ABSTRACT RECORD ["{00000138-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IPropertySetStorage* = POINTER TO ABSTRACT RECORD ["{0000013A-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IEnumSTATPROPSTG* = POINTER TO ABSTRACT RECORD ["{00000139-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IEnumSTATPROPSETSTG* = POINTER TO ABSTRACT RECORD ["{0000013B-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IConnectionPoint* = POINTER TO ABSTRACT RECORD ["{B196B286-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IConnectionPointContainer* = POINTER TO ABSTRACT RECORD ["{B196B284-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		CONNECTDATA* = RECORD [untagged]
			pUnk*: COM.IUnknown;
			dwCookie*: INTEGER;
		END;
		PtrCONNECTDATA* = POINTER TO CONNECTDATA;
		IEnumConnections* = POINTER TO ABSTRACT RECORD ["{B196B287-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		IEnumConnectionPoints* = POINTER TO ABSTRACT RECORD ["{B196B285-BAB4-101A-B69C-00AA00341D07}"] (COM.IUnknown)
		END;
		SOLE_AUTHENTICATION_SERVICE* = RECORD [untagged]
			dwAuthnSvc*: INTEGER;
			dwAuthzSvc*: INTEGER;
			pPrincipalName*: WinApi.PtrWSTR;
			hr*: COM.RESULT;
		END;
		PtrSOLE_AUTHENTICATION_SERVICE* = POINTER TO SOLE_AUTHENTICATION_SERVICE;
		EOLE_AUTHENTICATION_CAPABILITES* = INTEGER;
		IClientSecurity* = POINTER TO ABSTRACT RECORD ["{0000013D-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IServerSecurity* = POINTER TO ABSTRACT RECORD ["{0000013E-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		COINIT* = INTEGER;
		COSERVERINFO* = RECORD [untagged]
			dwSize*: INTEGER;
			pszName*: WinApi.PtrWSTR;
		END;
		PtrCOSERVERINFO* = POINTER TO COSERVERINFO;
		MULTI_QI* = RECORD [untagged]
			pIID*: POINTER TO (*?*) ARRAY [untagged] OF COM.GUID;
			pItf*: COM.IUnknown;
			hr*: COM.RESULT;
		END;
		PtrMULTI_QI* = POINTER TO MULTI_QI;
		FNGETCLASSOBJECT* = PROCEDURE (IN [nil] p0: COM.GUID; IN [iid] p1: COM.GUID; OUT [new] p2: COM.IUnknown): COM.RESULT;
		FNCANUNLOADNOW* = PROCEDURE (): COM.RESULT;
		IOleAdviseHolder* = POINTER TO ABSTRACT RECORD ["{00000111-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IOleCache* = POINTER TO ABSTRACT RECORD ["{0000011e-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		DISCARDCACHE* = INTEGER;
		IOleCache2* = POINTER TO ABSTRACT RECORD ["{00000128-0000-0000-C000-000000000046}"] (IOleCache)
		END;
		IOleCacheControl* = POINTER TO ABSTRACT RECORD ["{00000129-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IParseDisplayName* = POINTER TO ABSTRACT RECORD ["{0000011a-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IOleContainer* = POINTER TO ABSTRACT RECORD ["{0000011b-0000-0000-C000-000000000046}"] (IParseDisplayName)
		END;
		IOleClientSite* = POINTER TO ABSTRACT RECORD ["{00000118-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		OLEGETMONIKER* = INTEGER;
		OLEWHICHMK* = INTEGER;
		USERCLASSTYPE* = INTEGER;
		OLEMISC* = INTEGER;
		OLECLOSE* = INTEGER;
		IOleObject* = POINTER TO ABSTRACT RECORD ["{00000112-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		OLERENDER* = INTEGER;
		OBJECTDESCRIPTOR* = RECORD [untagged]
			cbSize*: INTEGER;
			clsid*: COM.GUID;
			dwDrawAspect*: SET;
			sizel*: WinApi.SIZE;
			pointl*: WinApi.POINT;
			dwStatus*: SET;
			dwFullUserTypeName*: INTEGER;
			dwSrcOfCopy*: INTEGER;
		END;
		PtrOBJECTDESCRIPTOR* = POINTER TO OBJECTDESCRIPTOR;
		LINKSRCDESCRIPTOR* = OBJECTDESCRIPTOR;
		PtrLINKSRCDESCRIPTOR* = PtrOBJECTDESCRIPTOR;
		IOleWindow* = POINTER TO ABSTRACT RECORD ["{00000114-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		OLEUPDATE* = INTEGER;
		OLELINKBIND* = INTEGER;
		IOleLink* = POINTER TO ABSTRACT RECORD ["{0000011d-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		BINDSPEED* = INTEGER;
		OLECONTF* = INTEGER;
		IOleItemContainer* = POINTER TO ABSTRACT RECORD ["{0000011c-0000-0000-C000-000000000046}"] (IOleContainer)
		END;
		BORDERWIDTHS* = WinApi.RECT;
		PtrBORDERWIDTHS* = WinApi.PtrRECT;
		IOleInPlaceUIWindow* = POINTER TO ABSTRACT RECORD ["{00000115-0000-0000-C000-000000000046}"] (IOleWindow)
		END;
		IOleInPlaceActiveObject* = POINTER TO ABSTRACT RECORD ["{00000117-0000-0000-C000-000000000046}"] (IOleWindow)
		END;
		OLEINPLACEFRAMEINFO* = RECORD [untagged]
			cb*: INTEGER;
			fMDIApp*: WinApi.BOOL;
			hwndFrame*: WinApi.HWND;
			haccel*: WinApi.HACCEL;
			cAccelEntries*: INTEGER;
		END;
		PtrOLEINPLACEFRAMEINFO* = POINTER TO OLEINPLACEFRAMEINFO;
		OLEMENUGROUPWIDTHS* = RECORD [untagged]
			width*: ARRAY [untagged] 6 OF INTEGER;
		END;
		PtrOLEMENUGROUPWIDTHS* = POINTER TO OLEMENUGROUPWIDTHS;
		HOLEMENU* = WinApi.HGLOBAL;
		IOleInPlaceFrame* = POINTER TO ABSTRACT RECORD ["{00000116-0000-0000-C000-000000000046}"] (IOleInPlaceUIWindow)
		END;
		IOleInPlaceObject* = POINTER TO ABSTRACT RECORD ["{00000113-0000-0000-C000-000000000046}"] (IOleWindow)
		END;
		IOleInPlaceSite* = POINTER TO ABSTRACT RECORD ["{00000119-0000-0000-C000-000000000046}"] (IOleWindow)
		END;
		IViewObject* = POINTER TO ABSTRACT RECORD ["{0000010d-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IViewObject2* = POINTER TO ABSTRACT RECORD ["{00000127-0000-0000-C000-000000000046}"] (IViewObject)
		END;
		IDropSource* = POINTER TO ABSTRACT RECORD ["{00000121-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IDropTarget* = POINTER TO ABSTRACT RECORD ["{00000122-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		OLEVERB* = RECORD [untagged]
			lVerb*: INTEGER;
			lpszVerbName*: WinApi.PtrWSTR;
			fuFlags*: SET;
			grfAttribs*: SET;
		END;
		PtrOLEVERB* = POINTER TO OLEVERB;
		OLEVERBATTRIB* = INTEGER;
		IEnumOLEVERB* = POINTER TO ABSTRACT RECORD ["{00000104-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		PtrOLESTREAM* = POINTER TO OLESTREAM;
		OLESTREAMVTBL* = RECORD [untagged]
			Get*: PROCEDURE (p0: PtrOLESTREAM; p1: WinApi.PtrVoid; p2: INTEGER): INTEGER;
			Put*: PROCEDURE (p0: PtrOLESTREAM; p1: WinApi.PtrVoid; p2: INTEGER): INTEGER;
		END;
		PtrOLESTREAMVTBL* = POINTER TO OLESTREAMVTBL;
		OLESTREAM* = RECORD [untagged]
			lpstbl*: PtrOLESTREAMVTBL;
		END;

	PROCEDURE (this: IClassFactory) CreateInstance* (pUnkOuter: COM.IUnknown; IN [iid] riid: COM.GUID; OUT [new] ppvObject: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClassFactory) LockServer* (fLock: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMarshal) GetUnmarshalClass* (IN [nil] riid: COM.GUID; pv: WinApi.PtrVoid; dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; mshlflags: SET; OUT [nil] pCid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMarshal) GetMarshalSizeMax* (IN [nil] riid: COM.GUID; pv: WinApi.PtrVoid; dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; mshlflags: SET; OUT [nil] pSize: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMarshal) MarshalInterface* (pStm: IStream; IN [nil] riid: COM.GUID; pv: WinApi.PtrVoid; dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; mshlflags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMarshal) UnmarshalInterface* (pStm: IStream; IN [iid] riid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMarshal) ReleaseMarshalData* (pStm: IStream): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMarshal) DisconnectObject* (dwReserved: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMalloc) Alloc* (cb: INTEGER): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMalloc) Realloc* (pv: WinApi.PtrVoid; cb: INTEGER): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMalloc) Free* (pv: WinApi.PtrVoid), NEW, ABSTRACT;

	PROCEDURE (this: IMalloc) GetSize* (pv: WinApi.PtrVoid): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IMalloc) DidAlloc* (pv: WinApi.PtrVoid): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IMalloc) HeapMinimize* (), NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PreAlloc* (cbRequest: INTEGER): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PostAlloc* (pActual: WinApi.PtrVoid): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PreFree* (pRequest: WinApi.PtrVoid; fSpyed: WinApi.BOOL): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PostFree* (fSpyed: WinApi.BOOL), NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PreRealloc* (pRequest: WinApi.PtrVoid; cbRequest: INTEGER; OUT [nil] ppNewRequest: WinApi.PtrVoid; fSpyed: WinApi.BOOL): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PostRealloc* (pActual: WinApi.PtrVoid; fSpyed: WinApi.BOOL): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PreGetSize* (pRequest: WinApi.PtrVoid; fSpyed: WinApi.BOOL): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PostGetSize* (cbActual: INTEGER; fSpyed: WinApi.BOOL): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PreDidAlloc* (pRequest: WinApi.PtrVoid; fSpyed: WinApi.BOOL): WinApi.PtrVoid, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PostDidAlloc* (pRequest: WinApi.PtrVoid; fSpyed: WinApi.BOOL; fActual: INTEGER): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PreHeapMinimize* (), NEW, ABSTRACT;

	PROCEDURE (this: IMallocSpy) PostHeapMinimize* (), NEW, ABSTRACT;

	PROCEDURE (this: IStdMarshalInfo) GetClassForHandler* (dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; OUT [nil] pClsid: COM.GUID): COM.RESULT, NEW, ABSTRACT;
	
	PROCEDURE (this: IExternalConnection) AddConnection* (extconn: SET; reserved: INTEGER): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IExternalConnection) ReleaseConnection* (extconn: SET; reserved: INTEGER; fLastReleaseCloses: WinApi.BOOL): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IEnumUnknown) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF COM.IUnknown; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumUnknown) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumUnknown) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumUnknown) Clone* (OUT [nil] ppenum: IEnumUnknown): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IBindCtx) RegisterObjectBound* (punk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IBindCtx) RevokeObjectBound* (punk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IBindCtx) ReleaseBoundObjects* (): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IBindCtx) SetBindOptions* (IN [nil] pbindopts: BIND_OPTS): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IBindCtx) GetBindOptions* (VAR [nil] pbindopts: BIND_OPTS): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IBindCtx) GetRunningObjectTable* (OUT [nil] pprot: IRunningObjectTable): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IBindCtx) RegisterObjectParam* (pszKey: WinApi.PtrWSTR; punk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IBindCtx) GetObjectParam* (pszKey: WinApi.PtrWSTR; OUT [nil] ppunk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IBindCtx) EnumObjectParam* (OUT [nil] ppenum: IEnumString): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IBindCtx) RevokeObjectParam* (pszKey: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumMoniker) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF IMoniker; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumMoniker) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumMoniker) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumMoniker) Clone* (OUT [nil] ppenum: IEnumMoniker): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRunnableObject) GetRunningClass* (OUT [nil] lpClsid: COM.GUID): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRunnableObject) Run* (pbc: IBindCtx): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunnableObject) IsRunning* (): WinApi.BOOL, NEW, ABSTRACT;

	PROCEDURE (this: IRunnableObject) LockRunning* (fLock: WinApi.BOOL; fLastUnlockCloses: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunnableObject) SetContainedObject* (fContained: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunningObjectTable) Register* (grfFlags: SET; punkObject: COM.IUnknown; pmkObjectName: IMoniker; OUT [nil] pdwRegister: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunningObjectTable) Revoke* (dwRegister: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunningObjectTable) IsRunning* (pmkObjectName: IMoniker): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRunningObjectTable) GetObjectA* (pmkObjectName: IMoniker; OUT [nil] ppunkObject: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunningObjectTable) NoteChangeTime* (dwRegister: INTEGER;VAR [nil] pfiletime: WinApi.FILETIME): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunningObjectTable) GetTimeOfLastChange* (pmkObjectName: IMoniker; OUT [nil] pfiletime: WinApi.FILETIME): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRunningObjectTable) EnumRunning* (OUT [nil] ppenumMoniker: IEnumMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersist) GetClassID* (OUT [nil] pClassID: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStream) IsDirty* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStream) Load* (pStm: IStream): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistStream) Save* (pStm: IStream; fClearDirty: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStream) GetSizeMax* (OUT [nil] pcbSize: LONGINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) BindToObject* (pbc: IBindCtx; pmkToLeft: IMoniker; IN [iid] riidResult: COM.GUID; OUT [new] ppvResult: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) BindToStorage* (pbc: IBindCtx; pmkToLeft: IMoniker; IN [iid] riid: COM.GUID; OUT [new] ppvObj: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) Reduce* (pbc: IBindCtx; dwReduceHowFar: INTEGER; VAR [nil] ppmkToLeft: IMoniker; OUT [nil] ppmkReduced: IMoniker): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IMoniker) ComposeWith* (pmkRight: IMoniker; fOnlyIfNotGeneric: WinApi.BOOL; OUT [nil] ppmkComposite: IMoniker): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IMoniker) Enum* (fForward: WinApi.BOOL; OUT [nil] ppenumMoniker: IEnumMoniker): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IMoniker) IsEqual* (pmkOtherMoniker: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) Hash* (OUT [nil] pdwHash: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) IsRunning* (pbc: IBindCtx; pmkToLeft: IMoniker; pmkNewlyRunning: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) GetTimeOfLastChange* (pbc: IBindCtx; pmkToLeft: IMoniker; OUT [nil] pFileTime: WinApi.FILETIME): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) Inverse* (OUT [nil] ppmk: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) CommonPrefixWith* (pmkOther: IMoniker; OUT [nil] ppmkPrefix: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) RelativePathTo* (pmkOther: IMoniker; OUT [nil] ppmkRelPath: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) GetDisplayName* (pbc: IBindCtx; pmkToLeft: IMoniker; OUT [nil] ppszDisplayName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) ParseDisplayName* (pbc: IBindCtx; pmkToLeft: IMoniker; pszDisplayName: WinApi.PtrWSTR; OUT [nil] pchEaten: INTEGER; OUT [nil] ppmkOut: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IMoniker) IsSystemMoniker* (OUT [nil] pdwMksys: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IROTData) GetComparisonData* (OUT [nil] pbData: SHORTCHAR; cbMax: INTEGER; OUT [nil] pcbData: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumString) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF WinApi.PtrWSTR; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumString) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumString) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumString) Clone* (OUT [nil] ppenum: IEnumString): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ISequentialStream) Read* (pv: WinApi.PtrVoid; cb: INTEGER; OUT [nil] pcbRead: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ISequentialStream) Write* (pv: WinApi.PtrVoid; cb: INTEGER; OUT [nil] pcbWritten: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) Seek* (dlibMove: LONGINT; dwOrigin: INTEGER; OUT [nil] plibNewPosition: LONGINT): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStream) SetSize* (libNewSize: LONGINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) CopyTo* (pstm: IStream; cb: LONGINT; OUT [nil] pcbRead: LONGINT; OUT [nil] pcbWritten: LONGINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) Commit* (grfCommitFlags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) Revert* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) LockRegion* (libOffset: LONGINT; cb: LONGINT; dwLockType: SET): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStream) UnlockRegion* (libOffset: LONGINT; cb: LONGINT; dwLockType: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) Stat* (OUT [nil] pstatstg: STATSTG; grfStatFlag: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStream) Clone* (OUT [nil] ppstm: IStream): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATSTG) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF STATSTG; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATSTG) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumSTATSTG) Reset* (): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumSTATSTG) Clone* (OUT [nil] ppenum: IEnumSTATSTG): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) CreateStream* (pwcsName: WinApi.PtrWSTR; grfMode: SET; reserved1: INTEGER; reserved2: INTEGER; OUT [nil] ppstm: IStream): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) OpenStream* (pwcsName: WinApi.PtrWSTR; reserved1: WinApi.PtrVoid; grfMode: SET; reserved2: INTEGER; OUT [nil] ppstm: IStream): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) CreateStorage* (pwcsName: WinApi.PtrWSTR; grfMode: SET; dwStgFmt: INTEGER; reserved2: INTEGER; OUT [nil] ppstg: IStorage): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) OpenStorage* (pwcsName: WinApi.PtrWSTR; pstgPriority: IStorage; grfMode: SET; snbExclude: SNB; reserved: INTEGER; OUT [nil] ppstg: IStorage): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStorage) CopyTo* (ciidExclude: INTEGER; IN [nil] rgiidExclude: COM.GUID; snbExclude: SNB; pstgDest: IStorage): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) MoveElementTo* (pwcsName: WinApi.PtrWSTR; pstgDest: IStorage; pwcsNewName: WinApi.PtrWSTR; grfFlags: SET): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) Commit* (grfCommitFlags: SET): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) Revert* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStorage) EnumElements* (reserved1: INTEGER; reserved2: WinApi.PtrVoid; reserved3: INTEGER; OUT [nil] ppenum: IEnumSTATSTG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStorage) DestroyElement* (pwcsName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) RenameElement* (pwcsOldName: WinApi.PtrWSTR; pwcsNewName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) SetElementTimes* (pwcsName: WinApi.PtrWSTR; VAR [nil] pctime: WinApi.FILETIME; VAR [nil] patime: WinApi.FILETIME; VAR [nil] pmtime: WinApi.FILETIME): COM.RESULT, NEW, ABSTRACT; 
	
	PROCEDURE (this: IStorage) SetClass* (IN [nil] clsid: COM.GUID): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IStorage) SetStateBits* (grfStateBits: SET; grfMask: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IStorage) Stat* (OUT [nil] pstatstg: STATSTG; grfStatFlag: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistFile) IsDirty* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistFile) Load* (pszFileName: WinApi.PtrWSTR; dwMode: SET): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistFile) Save* (pszFileName: WinApi.PtrWSTR; fRemember: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistFile) SaveCompleted* (pszFileName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistFile) GetCurFile* (OUT [nil] ppszFileName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistStorage) IsDirty* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStorage) InitNew* (pStg: IStorage): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStorage) Load* (pStg: IStorage): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistStorage) Save* (pStgSave: IStorage; fSameAsLoad: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPersistStorage) SaveCompleted* (pStgNew: IStorage): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPersistStorage) HandsOffStorage* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) ReadAt* (ulOffset: LONGINT; pv: WinApi.PtrVoid; cb: INTEGER; OUT [nil] pcbRead: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) WriteAt* (ulOffset: LONGINT; pv: WinApi.PtrVoid; cb: INTEGER; OUT [nil] pcbWritten: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) Flush* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) SetSize* (cb: LONGINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) LockRegion* (libOffset: LONGINT; cb: LONGINT; dwLockType: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) UnlockRegion* (libOffset: LONGINT; cb: LONGINT; dwLockType: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ILockBytes) Stat* (OUT [nil] pstatstg: STATSTG; grfStatFlag: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IEnumFORMATETC) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF FORMATETC; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumFORMATETC) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumFORMATETC) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumFORMATETC) Clone* (OUT [nil] ppenum: IEnumFORMATETC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATDATA) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF STATDATA; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATDATA) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATDATA) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATDATA) Clone* (OUT [nil] ppenum: IEnumSTATDATA): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRootStorage) SwitchToFile* (pszFile: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IAdviseSink) OnDataChange* (IN [nil] pFormatetc: FORMATETC; IN [nil] pStgmed: STGMEDIUM), NEW, ABSTRACT; 

	PROCEDURE (this: IAdviseSink) OnViewChange* (dwAspect: SET; lindex: INTEGER), NEW, ABSTRACT; 

	PROCEDURE (this: IAdviseSink) OnRename* (pmk: IMoniker), NEW, ABSTRACT; 

	PROCEDURE (this: IAdviseSink) OnSave* (), NEW, ABSTRACT;

	PROCEDURE (this: IAdviseSink) OnClose* (), NEW, ABSTRACT; 

	PROCEDURE (this: IAdviseSink2) OnLinkSrcChange* (pmk: IMoniker), NEW, ABSTRACT;

	PROCEDURE (this: IDataObject) GetData* (IN [nil] pformatetcIn: FORMATETC; OUT [nil] pmedium: STGMEDIUM): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) GetDataHere* (IN [nil] pformatetc: FORMATETC; VAR [nil] pmedium: STGMEDIUM): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) QueryGetData* (IN [nil] pformatetc: FORMATETC): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) GetCanonicalFormatEtc* (IN [nil] pformatectIn: FORMATETC; OUT [nil] pformatetcOut: FORMATETC): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) SetData* (IN [nil] pformatetc: FORMATETC; IN [nil] pmedium: STGMEDIUM; fRelease: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) EnumFormatEtc* (dwDirection: SET; OUT [nil] ppenumFormatEtc: IEnumFORMATETC): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) DAdvise* (IN [nil] pformatetc: FORMATETC; advf: SET; pAdvSink: IAdviseSink; OUT [nil] pdwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) DUnadvise* (dwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataObject) EnumDAdvise* (OUT [nil] ppenumAdvise: IEnumSTATDATA): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDataAdviseHolder) Advise* (pDataObject: IDataObject; IN [nil] pFetc: FORMATETC; advf: SET; pAdvise: IAdviseSink; OUT [nil] pdwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataAdviseHolder) Unadvise* (dwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDataAdviseHolder) EnumAdvise* (OUT [nil] ppenumAdvise: IEnumSTATDATA): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IDataAdviseHolder) SendOnDataChange* (pDataObject: IDataObject; dwReserved: INTEGER; advf: SET): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IMessageFilter) HandleInComingCall* (dwCallType: INTEGER; htaskCaller: WinApi.HTASK; dwTickCount: INTEGER; IN [nil] lpInterfaceInfo: INTERFACEINFO): INTEGER, NEW, ABSTRACT; 

	PROCEDURE (this: IMessageFilter) RetryRejectedCall* (htaskCallee: WinApi.HTASK; dwTickCount: INTEGER; dwRejectType: INTEGER): INTEGER, NEW, ABSTRACT; 

	PROCEDURE (this: IMessageFilter) MessagePending* (htaskCallee: WinApi.HTASK; dwTickCount: INTEGER; dwPendingType: INTEGER): INTEGER, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcChannelBuffer) GetBuffer* (IN [nil] pMessage: RPCOLEMESSAGE; IN [nil] riid: COM.GUID): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcChannelBuffer) SendReceive* (VAR [nil] pMessage: RPCOLEMESSAGE; OUT [nil] pStatus: INTEGER): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcChannelBuffer) FreeBuffer* (IN [nil] pMessage: RPCOLEMESSAGE): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcChannelBuffer) GetDestCtx* (OUT [nil] pdwDestContext: INTEGER; OUT [nil] ppvDestContext: WinApi.PtrVoid): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcChannelBuffer) IsConnected* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRpcProxyBuffer) Connect* (pRpcChannelBuffer: IRpcChannelBuffer): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcProxyBuffer) Disconnect* (), NEW, ABSTRACT; 

	PROCEDURE (this: IRpcStubBuffer) Connect* (pUnkServer: COM.IUnknown): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcStubBuffer) Disconnect* (), NEW, ABSTRACT; 

	PROCEDURE (this: IRpcStubBuffer) Invoke* (IN [nil] _prpcmsg: RPCOLEMESSAGE; _pRpcChannelBuffer: IRpcChannelBuffer): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IRpcStubBuffer) IsIIDSupported* (IN [nil] riid: COM.GUID): IRpcStubBuffer, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcStubBuffer) CountRefs* (): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: IRpcStubBuffer) DebugServerQueryInterface* (VAR [nil] ppv: WinApi.PtrVoid): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IRpcStubBuffer) DebugServerRelease* (pv: WinApi.PtrVoid), NEW, ABSTRACT;

	PROCEDURE (this: IPSFactoryBuffer) CreateProxy* (pUnkOuter: COM.IUnknown; IN [iid] riid: COM.GUID; OUT [nil] ppProxy: IRpcProxyBuffer; OUT [new] ppv: COM.IUnknown): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPSFactoryBuffer) CreateStub* (IN [nil] riid: COM.GUID; pUnkServer: COM.IUnknown; OUT [nil] ppStub: IRpcStubBuffer): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPropertyStorage) ReadMultiple* (cpspec: INTEGER; IN [nil] rgpspec: ARRAY [untagged]  OF PROPSPEC; IN [nil] rgpropvar: ARRAY [untagged]  OF PROPVARIANT): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPropertyStorage) WriteMultiple* (cpspec: INTEGER; IN [nil] rgpspec: ARRAY [untagged]  OF PROPSPEC; IN [nil] rgpropvar: ARRAY [untagged]  OF PROPVARIANT; propidNameFirst: PROPID): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPropertyStorage) DeleteMultiple* (cpspec: INTEGER; IN [nil] rgpspec: ARRAY [untagged]  OF PROPSPEC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyStorage) ReadPropertyNames* (cpropid: INTEGER; IN [nil] rgpropid: ARRAY [untagged]  OF PROPID; IN [nil] rglpwstrName: ARRAY [untagged]  OF WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyStorage) WritePropertyNames* (cpropid: INTEGER; IN [nil] rgpropid: ARRAY [untagged]  OF PROPID; IN [nil] rglpwstrName: ARRAY [untagged]  OF WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPropertyStorage) DeletePropertyNames* (cpropid: INTEGER; IN [nil] rgpropid: ARRAY [untagged]  OF PROPID): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPropertyStorage) Commit* (grfCommitFlags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyStorage) Revert* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyStorage) Enum* (OUT [nil] ppenum: IEnumSTATPROPSTG): COM.RESULT, NEW, ABSTRACT; 

	PROCEDURE (this: IPropertyStorage) SetTimes* (VAR [nil] pctime: WinApi.FILETIME; VAR [nil] patime: WinApi.FILETIME; VAR [nil] pmtime: WinApi.FILETIME): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyStorage) SetClass* (IN [nil] clsid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertyStorage) Stat* (OUT [nil] pstatpsstg: STATPROPSETSTG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertySetStorage) Create* (IN [nil] rfmtid: FMTID; IN [nil] pclsid: COM.GUID; grfFlags: SET; grfMode: SET; OUT [nil] ppprstg: IPropertyStorage): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertySetStorage) Open* (IN [nil] rfmtid: FMTID; grfMode: SET; OUT [nil] ppprstg: IPropertyStorage): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertySetStorage) Delete* (IN [nil] rfmtid: FMTID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IPropertySetStorage) Enum* (OUT [nil] ppenum: IEnumSTATPROPSETSTG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSTG) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF STATPROPSTG; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSTG) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSTG) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSTG) Clone* (OUT [nil] ppenum: IEnumSTATPROPSTG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSETSTG) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF STATPROPSETSTG; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSETSTG) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSETSTG) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumSTATPROPSETSTG) Clone* (OUT [nil] ppenum: IEnumSTATPROPSETSTG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPoint) GetConnectionInterface* (OUT [nil] piid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPoint) GetConnectionPointContainer* (OUT [nil] ppCPC: IConnectionPointContainer): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPoint) Advise* (pUnkSink: COM.IUnknown; OUT [nil] pdwCookie: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPoint) Unadvise* (dwCookie: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPoint) EnumConnections* (OUT [nil] ppEnum: IEnumConnections): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPointContainer) EnumConnectionPoints* (OUT [nil] ppEnum: IEnumConnectionPoints): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IConnectionPointContainer) FindConnectionPoint* (IN [nil] riid: COM.GUID; OUT [nil] ppCP: IConnectionPoint): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnections) Next* (cConnections: INTEGER; OUT rgcd: ARRAY [untagged] OF CONNECTDATA; OUT [nil] lpcFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnections) Skip* (cConnections: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnections) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnections) Clone* (OUT [nil] ppEnum: IEnumConnections): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnectionPoints) Next* (cConnections: INTEGER; OUT rgpcn: ARRAY [untagged] OF IConnectionPoint; OUT [nil] lpcFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnectionPoints) Skip* (cConnections: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnectionPoints) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumConnectionPoints) Clone* (OUT [nil] ppEnum: IEnumConnectionPoints): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClientSecurity) QueryBlanket* (pProxy: COM.IUnknown; OUT [nil] pAuthnSvc: INTEGER; OUT [nil] pAuthzSvc: INTEGER; OUT [nil] pServerPrincName: WinApi.PtrWSTR; OUT [nil] pAuthnLevel: INTEGER; OUT [nil] pImpLevel: INTEGER; OUT [nil] pAuthInfo: WinApi.PtrVoid; OUT [nil] pCapabilites: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClientSecurity) SetBlanket* (pProxy: COM.IUnknown; AuthnSvc: INTEGER; AuthzSvc: INTEGER; pServerPrincName: WinApi.PtrWSTR; AuthnLevel: INTEGER; ImpLevel: INTEGER; pAuthInfo: WinApi.PtrVoid; Capabilities: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IClientSecurity) CopyProxy* (pProxy: COM.IUnknown; OUT [nil] ppCopy: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IServerSecurity) QueryBlanket* (OUT [nil] pAuthnSvc: INTEGER; OUT [nil] pAuthzSvc: INTEGER; OUT [nil] pServerPrincName: WinApi.PtrWSTR; OUT [nil] pAuthnLevel: INTEGER; OUT [nil] pImpLevel: INTEGER; OUT [nil] pPrivs: WinApi.PtrVoid; OUT [nil] pCapabilities: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IServerSecurity) ImpersonateClient* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IServerSecurity) RevertToSelf* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IServerSecurity) IsImpersonating* (): WinApi.BOOL, NEW, ABSTRACT;

	PROCEDURE (this: IOleAdviseHolder) Advise* (pAdvise: IAdviseSink; OUT [nil] pdwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleAdviseHolder) Unadvise* (dwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleAdviseHolder) EnumAdvise* (OUT [nil] ppenumAdvise: IEnumSTATDATA): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleAdviseHolder) SendOnRename* (pmk: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleAdviseHolder) SendOnSave* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleAdviseHolder) SendOnClose* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache) Cache* (IN [nil] pformatetc: FORMATETC; advf: SET; OUT [nil] pdwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache) Uncache* (dwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache) EnumCache* (OUT [nil] ppenumSTATDATA: IEnumSTATDATA): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache) InitCache* (pDataObject: IDataObject): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache) SetData* (IN [nil] pformatetc: FORMATETC; IN [nil] pmedium: STGMEDIUM; fRelease: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache2) UpdateCache* (pDataObject: IDataObject; grfUpdf: SET; pReserved: WinApi.PtrVoid): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCache2) DiscardCache* (dwDiscardOptions: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCacheControl) OnRun* (pDataObject: IDataObject): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleCacheControl) OnStop* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IParseDisplayName) ParseDisplayName* (pbc: IBindCtx; pszDisplayName: WinApi.PtrWSTR; OUT [nil] pchEaten: INTEGER; OUT [nil] ppmkOut: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleContainer) EnumObjects* (grfFlags: SET; OUT [nil] ppenum: IEnumUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleContainer) LockContainer* (fLock: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleClientSite) SaveObject* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleClientSite) GetMoniker* (dwAssign: INTEGER; dwWhichMoniker: INTEGER; OUT [nil] ppmk: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleClientSite) GetContainer* (OUT [nil] ppContainer: IOleContainer): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleClientSite) ShowObject* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleClientSite) OnShowWindow* (fShow: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleClientSite) RequestNewObjectLayout* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) SetClientSite* (pClientSite: IOleClientSite): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetClientSite* (OUT [nil] ppClientSite: IOleClientSite): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) SetHostNames* (szContainerApp: WinApi.PtrWSTR; szContainerObj: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) Close* (dwSaveOption: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) SetMoniker* (dwWhichMoniker: INTEGER; pmk: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetMoniker* (dwAssign: INTEGER; dwWhichMoniker: INTEGER; OUT [nil] ppmk: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) InitFromData* (pDataObject: IDataObject; fCreation: WinApi.BOOL; dwReserved: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetClipboardData* (dwReserved: INTEGER; OUT [nil] ppDataObject: IDataObject): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) DoVerb* (iVerb: INTEGER; IN [nil] lpmsg: WinApi.MSG; pActiveSite: IOleClientSite; lindex: INTEGER; hwndParent: WinApi.HWND; IN [nil] lprcPosRect: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) EnumVerbs* (OUT [nil] ppEnumOleVerb: IEnumOLEVERB): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) Update* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) IsUpToDate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetUserClassID* (OUT [nil] pClsid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetUserType* (dwFormOfType: INTEGER; OUT [nil] pszUserType: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) SetExtent* (dwDrawAspect: SET; IN [nil] psizel: WinApi.SIZE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetExtent* (dwDrawAspect: SET; OUT [nil] psizel: WinApi.SIZE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) Advise* (pAdvSink: IAdviseSink; OUT [nil] pdwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) Unadvise* (dwConnection: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) EnumAdvise* (OUT [nil] ppenumAdvise: IEnumSTATDATA): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) GetMiscStatus* (dwAspect: SET; OUT [nil] pdwStatus: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleObject) SetColorScheme* (IN [nil] pLogpal: WinApi.LOGPALETTE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleWindow) GetWindow* (OUT [nil] phwnd: WinApi.HWND): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleWindow) ContextSensitiveHelp* (fEnterMode: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) SetUpdateOptions* (dwUpdateOpt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) GetUpdateOptions* (OUT [nil] pdwUpdateOpt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) SetSourceMoniker* (pmk: IMoniker; IN [nil] rclsid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) GetSourceMoniker* (OUT [nil] ppmk: IMoniker): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) SetSourceDisplayName* (pszStatusText: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) GetSourceDisplayName* (OUT [nil] ppszDisplayName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) BindToSource* (bindflags: SET; pbc: IBindCtx): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) BindIfRunning* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) GetBoundSource* (OUT [nil] ppunk: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) UnbindSource* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleLink) Update* (pbc: IBindCtx): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleItemContainer) GetObjectA* (pszItem: WinApi.PtrWSTR; dwSpeedNeeded: INTEGER; pbc: IBindCtx; IN [iid] riid: COM.GUID; OUT [new] ppvObject: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleItemContainer) GetObjectStorage* (pszItem: WinApi.PtrWSTR; pbc: IBindCtx; IN [iid] riid: COM.GUID; OUT [new] ppvStorage: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleItemContainer) IsRunning* (pszItem: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceUIWindow) GetBorder* (OUT [nil] lprectBorder: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceUIWindow) RequestBorderSpace* (IN [nil] pborderwidths: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceUIWindow) SetBorderSpace* (IN [nil] pborderwidths: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceUIWindow) SetActiveObject* (pActiveObject: IOleInPlaceActiveObject; pszObjName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceActiveObject) TranslateAccelerator* (IN [nil] lpmsg: WinApi.MSG): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceActiveObject) OnFrameWindowActivate* (fActivate: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceActiveObject) OnDocWindowActivate* (fActivate: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceActiveObject) ResizeBorder* (IN [nil] prcBorder: WinApi.RECT; pUIWindow: IOleInPlaceUIWindow; fFrameWindow: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceActiveObject) EnableModeless* (fEnable: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceFrame) InsertMenus* (hmenuShared: WinApi.HMENU; VAR [nil] lpMenuWidths: OLEMENUGROUPWIDTHS): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceFrame) SetMenu* (hmenuShared: WinApi.HMENU; holemenu: HOLEMENU; hwndActiveObject: WinApi.HWND): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceFrame) RemoveMenus* (hmenuShared: WinApi.HMENU): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceFrame) SetStatusText* (pszStatusText: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceFrame) EnableModeless* (fEnable: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceFrame) TranslateAccelerator* (IN [nil] lpmsg: WinApi.MSG; wID: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceObject) InPlaceDeactivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceObject) UIDeactivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceObject) SetObjectRects* (IN [nil] lprcPosRect: WinApi.RECT; IN [nil] lprcClipRect: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceObject) ReactivateAndUndo* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) CanInPlaceActivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) OnInPlaceActivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) OnUIActivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) GetWindowContext* (OUT [nil] ppFrame: IOleInPlaceFrame; OUT [nil] ppDoc: IOleInPlaceUIWindow; OUT [nil] lprcPosRect: WinApi.RECT; OUT [nil] lprcClipRect: WinApi.RECT; VAR [nil] lpFrameInfo: OLEINPLACEFRAMEINFO): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) Scroll* (scrollExtant: WinApi.SIZE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) OnUIDeactivate* (fUndoable: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) OnInPlaceDeactivate* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) DiscardUndoState* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) DeactivateAndUndo* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IOleInPlaceSite) OnPosRectChange* (IN [nil] lprcPosRect: WinApi.RECT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject) Draw* (dwDrawAspect: SET; lindex: INTEGER; pvAspect: WinApi.PtrVoid; IN [nil] ptd: DVTARGETDEVICE; hdcTargetDev: WinApi.HDC; hdcDraw: WinApi.HDC; IN [nil] lprcBounds: WinApi.RECT; IN [nil] lprcWBounds: WinApi.RECT; pfnContinue: PROCEDURE (dwContinue: INTEGER): WinApi.BOOL; dwContinue: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject) GetColorSet* (dwDrawAspect: SET; lindex: INTEGER; pvAspect: WinApi.PtrVoid; IN [nil] ptd: DVTARGETDEVICE; hicTargetDev: WinApi.HDC; OUT [nil] ppColorSet: WinApi.PtrLOGPALETTE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject) Freeze* (dwDrawAspect: SET; lindex: INTEGER; pvAspect: WinApi.PtrVoid; OUT [nil] pdwFreeze: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject) Unfreeze* (dwFreeze: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject) SetAdvise* (aspects: SET; advf: SET; pAdvSink: IAdviseSink): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject) GetAdvise* (OUT [nil] pAspects: SET; OUT [nil] pAdvf: INTEGER; OUT [nil] ppAdvSink: IAdviseSink): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IViewObject2) GetExtent* (dwDrawAspect: SET; lindex: INTEGER; IN [nil] ptd: DVTARGETDEVICE; OUT [nil] lpsizel: WinApi.SIZE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDropSource) QueryContinueDrag* (fEscapePressed: WinApi.BOOL; grfKeyState: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDropSource) GiveFeedback* (dwEffect: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDropTarget) DragEnter* (pDataObj: IDataObject; grfKeyState: SET; pt: WinApi.POINT; VAR [nil] pdwEffect: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDropTarget) DragOver* (grfKeyState: SET; pt: WinApi.POINT; VAR [nil] pdwEffect: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDropTarget) DragLeave* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDropTarget) Drop* (pDataObj: IDataObject; grfKeyState: SET; pt: WinApi.POINT; VAR [nil] pdwEffect: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumOLEVERB) Next* (celt: INTEGER; OUT rgelt: ARRAY [untagged] OF OLEVERB; OUT [nil] pceltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumOLEVERB) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IEnumOLEVERB) Reset* (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IEnumOLEVERB) Clone* (OUT [nil] ppenum: IEnumOLEVERB): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE PropVariantCopy* (VAR [nil] pvarDest: PROPVARIANT; VAR [nil] pvarSrc: PROPVARIANT): COM.RESULT;
	PROCEDURE PropVariantClear* (VAR [nil] pvar: PROPVARIANT): COM.RESULT;
	PROCEDURE FreePropVariantArray* (cVariants: INTEGER; VAR [nil] rgvars: PROPVARIANT): COM.RESULT;
	PROCEDURE SNB_UserSize* (VAR [nil] p0: INTEGER; p1: INTEGER; VAR [nil] p2: SNB): INTEGER;
	PROCEDURE SNB_UserMarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: SNB): WinApi.PtrSTR;
	PROCEDURE SNB_UserUnmarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: SNB): WinApi.PtrSTR;
	PROCEDURE SNB_UserFree* (VAR [nil] p0: INTEGER; VAR [nil] p1: SNB);
	PROCEDURE STGMEDIUM_UserSize* (VAR [nil] p0: INTEGER; p1: INTEGER; VAR [nil] p2: STGMEDIUM): INTEGER;
	PROCEDURE STGMEDIUM_UserMarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: STGMEDIUM): WinApi.PtrSTR;
	PROCEDURE STGMEDIUM_UserUnmarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: STGMEDIUM): WinApi.PtrSTR; 	PROCEDURE STGMEDIUM_UserFree* (VAR [nil] p0: INTEGER; VAR [nil] p1: STGMEDIUM);
	PROCEDURE CoBuildVersion* (): INTEGER;
	PROCEDURE CoInitialize* (pvReserved: WinApi.PtrVoid): COM.RESULT;
	PROCEDURE CoInitializeEx* (pvReserved: WinApi.PtrVoid; dwCoInit: INTEGER): COM.RESULT;
	PROCEDURE CoUninitialize* ();
	PROCEDURE CoGetMalloc* (dwMemContext: INTEGER; VAR [nil] ppMalloc: IMalloc): COM.RESULT;
	PROCEDURE CoGetCurrentProcess* (): INTEGER;
	PROCEDURE CoRegisterMallocSpy* (pMallocSpy: IMallocSpy): COM.RESULT;
	PROCEDURE CoRevokeMallocSpy* (): COM.RESULT;
	PROCEDURE CoGetClassObject* (IN [nil] rclsid: COM.GUID; dwClsContext: SET; pvReserved: WinApi.PtrVoid; IN [iid] riid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT;
	PROCEDURE CoRegisterClassObject* (IN [nil] rclsid: COM.GUID; pUnk: COM.IUnknown; dwClsContext: SET; flags: SET; VAR [nil] lpdwRegister: INTEGER): COM.RESULT;
	PROCEDURE CoRevokeClassObject* (dwRegister: INTEGER): COM.RESULT;

	PROCEDURE CoGetMarshalSizeMax* (VAR [nil] pulSize: INTEGER; IN [nil] riid: COM.GUID; pUnk: COM.IUnknown; dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; mshlflags: SET): COM.RESULT;

	PROCEDURE CoMarshalInterface* (pStm: IStream; IN [nil] riid: COM.GUID; pUnk: COM.IUnknown; dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; mshlflags: SET): COM.RESULT;

	PROCEDURE CoUnmarshalInterface* (pStm: IStream; IN [iid] riid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT;

	PROCEDURE CoMarshalHresult* (pstm: IStream; hresult: COM.RESULT): COM.RESULT;

	PROCEDURE CoUnmarshalHresult* (pstm: IStream; VAR [nil] phresult: COM.RESULT): COM.RESULT;

	PROCEDURE CoReleaseMarshalData* (pStm: IStream): COM.RESULT;

	PROCEDURE CoDisconnectObject* (pUnk: COM.IUnknown; dwReserved: INTEGER): COM.RESULT;

	PROCEDURE CoLockObjectExternal* (pUnk: COM.IUnknown; fLock: WinApi.BOOL; fLastUnlockReleases: WinApi.BOOL): COM.RESULT;

	PROCEDURE CoGetStandardMarshal* (IN [nil] riid: COM.GUID; pUnk: COM.IUnknown; dwDestContext: INTEGER; pvDestContext: WinApi.PtrVoid; mshlflags: SET; VAR [nil] ppMarshal: IMarshal): COM.RESULT;

	PROCEDURE CoIsHandlerConnected* (pUnk: COM.IUnknown): WinApi.BOOL;

	PROCEDURE CoMarshalInterThreadInterfaceInStream* (IN [nil] riid: COM.GUID; pUnk: COM.IUnknown; VAR [nil] ppStm: IStream): COM.RESULT;

	PROCEDURE CoGetInterfaceAndReleaseStream* (pStm: IStream; IN [iid] iid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT;

	PROCEDURE CoCreateFreeThreadedMarshaler* (punkOuter: COM.IUnknown; VAR [nil] ppunkMarshal: COM.IUnknown): COM.RESULT;

	PROCEDURE CoLoadLibrary* (lpszLibName: WinApi.PtrWSTR; bAutoFree: WinApi.BOOL): WinApi.HINSTANCE;

	PROCEDURE CoFreeLibrary* (hInst: WinApi.HINSTANCE);

	PROCEDURE CoFreeAllLibraries* ();

	PROCEDURE CoFreeUnusedLibraries* ();

	PROCEDURE CoInitializeSecurity* (VAR [nil] pSecDesc: WinApi.SECURITY_DESCRIPTOR; dwAuthnLevel: INTEGER; dwImpLevel: INTEGER; pReserved: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE CoRegisterAuthenticationServices* (cbAuthSvc: INTEGER; VAR [nil] asAuthSvc: SOLE_AUTHENTICATION_SERVICE): COM.RESULT;

	PROCEDURE CoGetCallContext* (IN [iid] riid: COM.GUID; OUT [new] ppInterface: COM.IUnknown): COM.RESULT;

	PROCEDURE CoQueryProxyBlanket* (pProxy: COM.IUnknown; VAR [nil] pwAuthnSvc: INTEGER; VAR [nil] pAuthzSvc: INTEGER; VAR [nil] pServerPrincName: WinApi.PtrWSTR; VAR [nil] pAuthnLevel: INTEGER; VAR [nil] pImpLevel: INTEGER; VAR [nil] pAuthInfo: WinApi.PtrVoid; VAR [nil] pCapabilites: INTEGER): COM.RESULT;

	PROCEDURE CoSetProxyBlanket* (pProxy: COM.IUnknown; dwAuthnSvc: INTEGER; dwAuthzSvc: INTEGER; pServerPrincName: WinApi.PtrWSTR; dwAuthnLevel: INTEGER; dwImpLevel: INTEGER; VAR [nil] pAuthInfo: WinApi.PtrVoid; dwCapabilities: INTEGER): COM.RESULT;

	PROCEDURE CoCopyProxy* (pProxy: COM.IUnknown; VAR [nil] ppCopy: COM.IUnknown): COM.RESULT;

	PROCEDURE CoQueryClientBlanket* (VAR [nil] pAuthnSvc: INTEGER; VAR [nil] pAuthzSvc: INTEGER; VAR [nil] pServerPrincName: WinApi.PtrWSTR; VAR [nil] pAuthnLevel: INTEGER; VAR [nil] pImpLevel: INTEGER; VAR [nil] pPrivs: WinApi.PtrVoid; VAR [nil] pCapabilities: INTEGER): COM.RESULT;

	PROCEDURE CoImpersonateClient* (): COM.RESULT;

	PROCEDURE CoRevertToSelf* (): COM.RESULT;

	PROCEDURE CoCreateInstance* (IN [nil] rclsid: COM.GUID; pUnkOuter: COM.IUnknown; dwClsContext: SET; IN [iid] riid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT;

	PROCEDURE CoGetInstanceFromFile* (VAR [nil] pServerInfo: COSERVERINFO; VAR [nil] pClsid: COM.GUID; punkOuter: COM.IUnknown; dwClsCtx: INTEGER; grfMode: SET; pwszName: WinApi.PtrWSTR; dwCount: INTEGER; VAR [nil] pResults: MULTI_QI): COM.RESULT;

	PROCEDURE CoGetInstanceFromIStorage* (VAR [nil] pServerInfo: COSERVERINFO; VAR [nil] pClsid: COM.GUID; punkOuter: COM.IUnknown; dwClsCtx: INTEGER; pstg: IStorage; dwCount: INTEGER; VAR [nil] pResults: MULTI_QI): COM.RESULT;

	PROCEDURE CoCreateInstanceEx* (IN [nil] Clsid: COM.GUID; punkOuter: COM.IUnknown; dwClsCtx: INTEGER; VAR [nil] pServerInfo: COSERVERINFO; dwCount: INTEGER; VAR [nil] pResults: MULTI_QI): COM.RESULT;

	PROCEDURE StringFromCLSID* (IN [nil] rclsid: COM.GUID; VAR [nil] lplpsz: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE CLSIDFromString* (lpsz: WinApi.PtrWSTR; VAR [nil] pclsid: COM.GUID): COM.RESULT;

	PROCEDURE StringFromIID* (IN [nil] rclsid: COM.GUID; VAR [nil] lplpsz: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE IIDFromString* (lpsz: WinApi.PtrWSTR; VAR [nil] lpiid: COM.GUID): COM.RESULT;

	PROCEDURE CoIsOle1Class* (IN [nil] rclsid: COM.GUID): WinApi.BOOL;

	PROCEDURE ProgIDFromCLSID* (IN [nil] clsid: COM.GUID; VAR [nil] lplpszProgID: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE CLSIDFromProgID* (lpszProgID: WinApi.PtrWSTR; VAR [nil] lpclsid: COM.GUID): COM.RESULT;

	PROCEDURE StringFromGUID2* (IN [nil] rguid: COM.GUID; lpsz: WinApi.PtrWSTR; cbMax: INTEGER): INTEGER;

	PROCEDURE CoCreateGuid* (VAR [nil] pguid: COM.GUID): COM.RESULT;

	PROCEDURE CoFileTimeToDosDateTime* (VAR [nil] lpFileTime: WinApi.FILETIME; VAR [nil] lpDosDate: SHORTINT; VAR [nil] lpDosTime: SHORTINT): WinApi.BOOL;

	PROCEDURE CoDosDateTimeToFileTime* (nDosDate: SHORTINT; nDosTime: SHORTINT; VAR [nil] lpFileTime: WinApi.FILETIME): WinApi.BOOL;

	PROCEDURE CoFileTimeNow* (VAR [nil] lpFileTime: WinApi.FILETIME): COM.RESULT;

	PROCEDURE CoRegisterMessageFilter* (lpMessageFilter: IMessageFilter; VAR [nil] lplpMessageFilter: IMessageFilter): COM.RESULT;

	PROCEDURE CoGetTreatAsClass* (IN [nil] clsidOld: COM.GUID; VAR [nil] pClsidNew: COM.GUID): COM.RESULT;

	PROCEDURE CoTreatAsClass* (IN [nil] clsidOld: COM.GUID; IN [nil] clsidNew: COM.GUID): COM.RESULT;

	PROCEDURE DllGetClassObject* ["OLEAUT32.dll", ""] (IN [nil] rclsid: COM.GUID; IN [iid] riid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT;

	PROCEDURE DllCanUnloadNow* ["OLEAUT32.dll", ""] (): COM.RESULT;

	PROCEDURE CoTaskMemAlloc* (cb: INTEGER): WinApi.PtrVoid;

	PROCEDURE CoTaskMemRealloc* (pv: WinApi.PtrVoid; cb: INTEGER): WinApi.PtrVoid;

	PROCEDURE CoTaskMemFree* (pv: WinApi.PtrVoid);

	PROCEDURE CreateDataAdviseHolder* (VAR [nil] ppDAHolder: IDataAdviseHolder): COM.RESULT;

	PROCEDURE CreateDataCache* (pUnkOuter: COM.IUnknown; IN [nil] rclsid: COM.GUID; IN [iid] iid: COM.GUID; OUT [new] ppv: COM.IUnknown): COM.RESULT;

	PROCEDURE StgCreateDocfile* (pwcsName: WinApi.PtrWSTR; grfMode: SET; reserved: INTEGER; VAR [nil] ppstgOpen: IStorage): COM.RESULT;

	PROCEDURE StgCreateDocfileOnILockBytes* (plkbyt: ILockBytes; grfMode: SET; reserved: INTEGER; VAR [nil] ppstgOpen: IStorage): COM.RESULT;

	PROCEDURE StgOpenStorage* (pwcsName: WinApi.PtrWSTR; pstgPriority: IStorage; grfMode: SET; snbExclude: SNB; reserved: INTEGER; VAR [nil] ppstgOpen: IStorage): COM.RESULT;

	PROCEDURE StgOpenStorageOnILockBytes* (plkbyt: ILockBytes; pstgPriority: IStorage; grfMode: SET; snbExclude: SNB; reserved: INTEGER; VAR [nil] ppstgOpen: IStorage): COM.RESULT;

	PROCEDURE StgIsStorageFile* (pwcsName: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE StgIsStorageILockBytes* (plkbyt: ILockBytes): COM.RESULT;

	PROCEDURE StgSetTimes* (lpszName: WinApi.PtrWSTR; VAR [nil] pctime: WinApi.FILETIME; VAR [nil] patime: WinApi.FILETIME; VAR [nil] pmtime: WinApi.FILETIME): COM.RESULT;

	PROCEDURE BindMoniker* (pmk: IMoniker; grfOpt: INTEGER; IN [iid] iidResult: COM.GUID; OUT [new] ppvResult: COM.IUnknown): COM.RESULT;

	PROCEDURE MkParseDisplayName* (pbc: IBindCtx; szUserName: WinApi.PtrWSTR; VAR [nil] pchEaten: INTEGER; VAR [nil] ppmk: IMoniker): COM.RESULT;

	PROCEDURE MonikerRelativePathTo* (pmkSrc: IMoniker; pmkDest: IMoniker; VAR [nil] ppmkRelPath: IMoniker; dwReserved: WinApi.BOOL): COM.RESULT;

	PROCEDURE MonikerCommonPrefixWith* (pmkThis: IMoniker; pmkOther: IMoniker; VAR [nil] ppmkCommon: IMoniker): COM.RESULT;

	PROCEDURE CreateBindCtx* (reserved: INTEGER; VAR [nil] ppbc: IBindCtx): COM.RESULT;

	PROCEDURE CreateGenericComposite* (pmkFirst: IMoniker; pmkRest: IMoniker; VAR [nil] ppmkComposite: IMoniker): COM.RESULT;

	PROCEDURE GetClassFile* (szFilename: WinApi.PtrWSTR; VAR [nil] pclsid: COM.GUID): COM.RESULT;

	PROCEDURE CreateFileMoniker* (lpszPathName: WinApi.PtrWSTR; VAR [nil] ppmk: IMoniker): COM.RESULT;

	PROCEDURE CreateItemMoniker* (lpszDelim: WinApi.PtrWSTR; lpszItem: WinApi.PtrWSTR; VAR [nil] ppmk: IMoniker): COM.RESULT;

	PROCEDURE CreateAntiMoniker* (VAR [nil] ppmk: IMoniker): COM.RESULT;

	PROCEDURE CreatePointerMoniker* (punk: COM.IUnknown; VAR [nil] ppmk: IMoniker): COM.RESULT;

	PROCEDURE GetRunningObjectTable* (reserved: INTEGER; VAR [nil] pprot: IRunningObjectTable): COM.RESULT;

	PROCEDURE HACCEL_UserSize* (VAR [nil] p0: INTEGER; p1: INTEGER; VAR [nil] p2: WinApi.HACCEL): INTEGER;

	PROCEDURE HACCEL_UserMarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HACCEL): WinApi.PtrSTR;

	PROCEDURE HACCEL_UserUnmarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HACCEL): WinApi.PtrSTR;

	PROCEDURE HACCEL_UserFree* (VAR [nil] p0: INTEGER; VAR [nil] p1: WinApi.HACCEL);

	PROCEDURE HGLOBAL_UserSize* (VAR [nil] p0: INTEGER; p1: INTEGER; VAR [nil] p2: WinApi.HGLOBAL): INTEGER;

	PROCEDURE HGLOBAL_UserMarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HGLOBAL): WinApi.PtrSTR;

	PROCEDURE HGLOBAL_UserUnmarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HGLOBAL): WinApi.PtrSTR;

	PROCEDURE HGLOBAL_UserFree* (VAR [nil] p0: INTEGER; VAR [nil] p1: WinApi.HGLOBAL);

	PROCEDURE HMENU_UserSize* (VAR [nil] p0: INTEGER; p1: INTEGER; VAR [nil] p2: WinApi.HMENU): INTEGER;

	PROCEDURE HMENU_UserMarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HMENU): WinApi.PtrSTR;

	PROCEDURE HMENU_UserUnmarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HMENU): WinApi.PtrSTR;

	PROCEDURE HMENU_UserFree* (VAR [nil] p0: INTEGER; VAR [nil] p1: WinApi.HMENU);

	PROCEDURE HWND_UserSize* (VAR [nil] p0: INTEGER; p1: INTEGER; VAR [nil] p2: WinApi.HWND): INTEGER;

	PROCEDURE HWND_UserMarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HWND): WinApi.PtrSTR;

	PROCEDURE HWND_UserUnmarshal* (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; VAR [nil] p2: WinApi.HWND): WinApi.PtrSTR;

	PROCEDURE HWND_UserFree* (VAR [nil] p0: INTEGER; VAR [nil] p1: WinApi.HWND);

	PROCEDURE OleBuildVersion* (): INTEGER;

	PROCEDURE ReadClassStg* (pStg: IStorage; VAR [nil] pclsid: COM.GUID): COM.RESULT;

	PROCEDURE WriteClassStg* (pStg: IStorage; IN [nil] rclsid: COM.GUID): COM.RESULT;

	PROCEDURE ReadClassStm* (pStm: IStream; VAR [nil] pclsid: COM.GUID): COM.RESULT;

	PROCEDURE WriteClassStm* (pStm: IStream; IN [nil] rclsid: COM.GUID): COM.RESULT;

	PROCEDURE WriteFmtUserTypeStg* (pstg: IStorage; cf: CLIPFORMAT; lpszUserType: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE ReadFmtUserTypeStg* (pstg: IStorage; VAR [nil] pcf: CLIPFORMAT; VAR [nil] lplpszUserType: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE OleInitialize* (pvReserved: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE OleUninitialize* ();

	PROCEDURE OleQueryLinkFromData* (pSrcDataObject: IDataObject): COM.RESULT;

	PROCEDURE OleQueryCreateFromData* (pSrcDataObject: IDataObject): COM.RESULT;

	PROCEDURE OleCreate* (IN [nil] rclsid: COM.GUID; IN [iid] riid: COM.GUID; renderopt: INTEGER; VAR [nil] pFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateFromData* (pSrcDataObj: IDataObject; IN [iid] riid: COM.GUID; renderopt: INTEGER; VAR [nil] pFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateLinkFromData* (pSrcDataObj: IDataObject; IN [iid] riid: COM.GUID; renderopt: INTEGER; VAR [nil] pFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateStaticFromData* (pSrcDataObj: IDataObject; IN [iid] iid: COM.GUID; renderopt: INTEGER; VAR [nil] pFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateLink* (pmkLinkSrc: IMoniker; IN [iid] riid: COM.GUID; renderopt: INTEGER; VAR [nil] lpFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateLinkToFile* (lpszFileName: WinApi.PtrWSTR; IN [iid] riid: COM.GUID; renderopt: INTEGER; VAR [nil] lpFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateFromFile* (IN [nil] rclsid: COM.GUID; lpszFileName: WinApi.PtrWSTR; IN [iid] riid: COM.GUID; renderopt: INTEGER; VAR [nil] lpFormatEtc: FORMATETC; pClientSite: IOleClientSite; pStg: IStorage; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleLoad* (pStg: IStorage; IN [iid] riid: COM.GUID; pClientSite: IOleClientSite; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleSave* (pPS: IPersistStorage; pStg: IStorage; fSameAsLoad: WinApi.BOOL): COM.RESULT;

	PROCEDURE OleLoadFromStream* (pStm: IStream; IN [iid] iidInterface: COM.GUID; OUT [new] ppvObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleSaveToStream* (pPStm: IPersistStream; pStm: IStream): COM.RESULT;

	PROCEDURE OleSetContainedObject* (pUnknown: COM.IUnknown; fContained: WinApi.BOOL): COM.RESULT;

	PROCEDURE OleNoteObjectVisible* (pUnknown: COM.IUnknown; fVisible: WinApi.BOOL): COM.RESULT;

	PROCEDURE RegisterDragDrop* (hwnd: WinApi.HWND; pDropTarget: IDropTarget): COM.RESULT;

	PROCEDURE RevokeDragDrop* (hwnd: WinApi.HWND): COM.RESULT;

	PROCEDURE DoDragDrop* (pDataObj: IDataObject; pDropSource: IDropSource; dwOKEffects: SET; VAR [nil] pdwEffect: SET): COM.RESULT;

	PROCEDURE OleSetClipboard* (pDataObj: IDataObject): COM.RESULT;

	PROCEDURE OleGetClipboard* (VAR [nil] ppDataObj: IDataObject): COM.RESULT;

	PROCEDURE OleFlushClipboard* (): COM.RESULT;

	PROCEDURE OleIsCurrentClipboard* (pDataObj: IDataObject): COM.RESULT;

	PROCEDURE OleCreateMenuDescriptor* (hmenuCombined: WinApi.HMENU; VAR [nil] lpMenuWidths: OLEMENUGROUPWIDTHS): HOLEMENU;

	PROCEDURE OleSetMenuDescriptor* (holemenu: HOLEMENU; hwndFrame: WinApi.HWND; hwndActiveObject: WinApi.HWND; lpFrame: IOleInPlaceFrame; lpActiveObj: IOleInPlaceActiveObject): COM.RESULT;

	PROCEDURE OleDestroyMenuDescriptor* (holemenu: HOLEMENU): COM.RESULT;

	PROCEDURE OleTranslateAccelerator* (lpFrame: IOleInPlaceFrame; VAR [nil] lpFrameInfo: OLEINPLACEFRAMEINFO; VAR [nil] lpmsg: WinApi.MSG): COM.RESULT;

	PROCEDURE OleDuplicateData* (hSrc: WinApi.HANDLE; cfFormat: CLIPFORMAT; uiFlags: SET): WinApi.HANDLE;

	PROCEDURE OleDraw* (pUnknown: COM.IUnknown; dwAspect: SET; hdcDraw: WinApi.HDC; VAR [nil] lprcBounds: WinApi.RECT): COM.RESULT;

	PROCEDURE OleRun* (pUnknown: COM.IUnknown): COM.RESULT;

	PROCEDURE OleIsRunning* (pObject: IOleObject): WinApi.BOOL;

	PROCEDURE OleLockRunning* (pUnknown: COM.IUnknown; fLock: WinApi.BOOL; fLastUnlockCloses: WinApi.BOOL): COM.RESULT;

	PROCEDURE ReleaseStgMedium* (VAR [nil] p0: STGMEDIUM);

	PROCEDURE CreateOleAdviseHolder* (VAR [nil] ppOAHolder: IOleAdviseHolder): COM.RESULT;

	PROCEDURE OleCreateDefaultHandler* (IN [nil] clsid: COM.GUID; pUnkOuter: COM.IUnknown; IN [iid] riid: COM.GUID; OUT [new] lplpObj: COM.IUnknown): COM.RESULT;

	PROCEDURE OleCreateEmbeddingHelper* (IN [nil] clsid: COM.GUID; pUnkOuter: COM.IUnknown; flags: SET; pCF: IClassFactory; IN [iid] riid: COM.GUID; OUT [new] lplpObj: COM.IUnknown): COM.RESULT;

	PROCEDURE IsAccelerator* (hAccel: WinApi.HACCEL; cAccelEntries: INTEGER; VAR [nil] lpMsg: WinApi.MSG; VAR [nil] lpwCmd: SHORTINT): WinApi.BOOL;

	PROCEDURE OleGetIconOfFile* (lpszPath: WinApi.PtrWSTR; fUseFileAsLabel: WinApi.BOOL): WinApi.HGLOBAL;

	PROCEDURE OleGetIconOfClass* (IN [nil] rclsid: COM.GUID; lpszLabel: WinApi.PtrWSTR; fUseTypeAsLabel: WinApi.BOOL): WinApi.HGLOBAL;

	PROCEDURE OleMetafilePictFromIconAndLabel* (hIcon: WinApi.HICON; lpszLabel: WinApi.PtrWSTR; lpszSourceFile: WinApi.PtrWSTR; iIconIndex: INTEGER): WinApi.HGLOBAL;

	PROCEDURE OleRegGetUserType* (IN [nil] clsid: COM.GUID; dwFormOfType: INTEGER; VAR [nil] pszUserType: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE OleRegGetMiscStatus* (IN [nil] clsid: COM.GUID; dwAspect: SET; VAR [nil] pdwStatus: SET): COM.RESULT;

	PROCEDURE OleRegEnumFormatEtc* (IN [nil] clsid: COM.GUID; dwDirection: SET; VAR [nil] ppenum: IEnumFORMATETC): COM.RESULT;

	PROCEDURE OleRegEnumVerbs* (IN [nil] clsid: COM.GUID; VAR [nil] ppenum: IEnumOLEVERB): COM.RESULT;

	PROCEDURE OleConvertOLESTREAMToIStorage* (VAR [nil] lpolestream: OLESTREAM; pstg: IStorage; VAR [nil] ptd: DVTARGETDEVICE): COM.RESULT;

	PROCEDURE OleConvertIStorageToOLESTREAM* (pstg: IStorage; VAR [nil] lpolestream: OLESTREAM): COM.RESULT;

	PROCEDURE GetHGlobalFromILockBytes* (plkbyt: ILockBytes; VAR [nil] phglobal: WinApi.HGLOBAL): COM.RESULT;

	PROCEDURE CreateILockBytesOnHGlobal* (hGlobal: WinApi.HGLOBAL; fDeleteOnRelease: WinApi.BOOL; VAR [nil] pplkbyt: ILockBytes): COM.RESULT;

	PROCEDURE GetHGlobalFromStream* (pstm: IStream; VAR [nil] phglobal: WinApi.HGLOBAL): COM.RESULT;

	PROCEDURE CreateStreamOnHGlobal* (hGlobal: WinApi.HGLOBAL; fDeleteOnRelease: WinApi.BOOL; VAR [nil] ppstm: IStream): COM.RESULT;

	PROCEDURE OleDoAutoConvert* (pStg: IStorage; VAR [nil] pClsidNew: COM.GUID): COM.RESULT;

	PROCEDURE OleGetAutoConvert* (IN [nil] clsidOld: COM.GUID; VAR [nil] pClsidNew: COM.GUID): COM.RESULT;

	PROCEDURE OleSetAutoConvert* (IN [nil] clsidOld: COM.GUID; IN [nil] clsidNew: COM.GUID): COM.RESULT;

	PROCEDURE GetConvertStg* (pStg: IStorage): COM.RESULT;

	PROCEDURE SetConvertStg* (pStg: IStorage; fConvert: WinApi.BOOL): COM.RESULT;

	PROCEDURE OleConvertIStorageToOLESTREAMEx* (pstg: IStorage; cfFormat: CLIPFORMAT; lWidth: INTEGER; lHeight: INTEGER; dwSize: INTEGER; VAR [nil] pmedium: STGMEDIUM; VAR [nil] polestm: OLESTREAM): COM.RESULT;

	PROCEDURE OleConvertOLESTREAMToIStorageEx* (VAR [nil] polestm: OLESTREAM; pstg: IStorage; VAR [nil] pcfFormat: CLIPFORMAT; VAR [nil] plwWidth: INTEGER; VAR [nil] plHeight: INTEGER; VAR [nil] pdwSize: INTEGER; VAR [nil] pmedium: STGMEDIUM): COM.RESULT;

END WinOle.
