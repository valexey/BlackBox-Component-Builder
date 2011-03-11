MODULE WinOleAut ["OLEAUT32.dll"];
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

	IMPORT COM, WinOle, WinApi;

	CONST (* macros *)
		STDOLE_MAJORVERNUM* = 1;
		STDOLE_MINORVERNUM* = 0;
		STDOLE_LCID* = 0;
		STDOLE2_MAJORVERNUM* = 2;
		STDOLE2_MINORVERNUM* = 0;
		STDOLE2_LCID* = 0;
		FADF_AUTO* = 1;
		FADF_STATIC* = 2;
		FADF_EMBEDDED* = 4;
		FADF_FIXEDSIZE* = 16;
		FADF_BSTR* = 256;
		FADF_UNKNOWN* = 512;
		FADF_DISPATCH* = 1024;
		FADF_VARIANT* = 2048;
		FADF_RESERVED* = 61672;
		PARAMFLAG_NONE* = 0;
		PARAMFLAG_FIN* = 1;
		PARAMFLAG_FOUT* = 2;
		PARAMFLAG_FLCID* = 4;
		PARAMFLAG_FRETVAL* = 8;
		PARAMFLAG_FOPT* = 16;
		PARAMFLAG_FHASDEFAULT* = 32;
		IDLFLAG_NONE* = 0;
		IDLFLAG_FIN* = 1;
		IDLFLAG_FOUT* = 2;
		IDLFLAG_FLCID* = 4;
		IDLFLAG_FRETVAL* = 8;
		IMPLTYPEFLAG_FDEFAULT* = {0};
		IMPLTYPEFLAG_FSOURCE* = {1};
		IMPLTYPEFLAG_FRESTRICTED* = {2};
		IMPLTYPEFLAG_FDEFAULTVTABLE* = {3};
		DISPID_UNKNOWN* = -1;
		DISPID_VALUE* = 0;
		DISPID_PROPERTYPUT* = -3;
		DISPID_NEWENUM* = -4;
		DISPID_EVALUATE* = -5;
		DISPID_CONSTRUCTOR* = -6;
		DISPID_DESTRUCTOR* = -7;
		DISPID_COLLECT* = -8;
		DISPID_Name* = -800;
		DISPID_Delete* = -801;
		DISPID_Object* = -802;
		DISPID_Parent* = -803;
		VARIANT_NOVALUEPROP* = 1;
		VARIANT_ALPHABOOL* = 2;
		VAR_TIMEVALUEONLY* = 1;
		VAR_DATEVALUEONLY* = 2;
		NUMPRS_LEADING_WHITE* = {0};
		NUMPRS_TRAILING_WHITE* = {1};
		NUMPRS_LEADING_PLUS* = {2};
		NUMPRS_TRAILING_PLUS* = {3};
		NUMPRS_LEADING_MINUS* = {4};
		NUMPRS_TRAILING_MINUS* = {5};
		NUMPRS_HEX_OCT* = {6};
		NUMPRS_PARENS* = {7};
		NUMPRS_DECIMAL* = {8};
		NUMPRS_THOUSANDS* = {9};
		NUMPRS_CURRENCY* = {10};
		NUMPRS_EXPONENT* = {11};
		NUMPRS_USE_ALL* = {12};
		NUMPRS_STD* = {0..12};
		NUMPRS_NEG* = {16};
		NUMPRS_INEXACT* = {17};
		VTBIT_I1* = {16};
		VTBIT_UI1* = {17};
		VTBIT_I2* = {2};
		VTBIT_UI2* = {18};
		VTBIT_I4* = {3};
		VTBIT_UI4* = {19};
		VTBIT_R4* = {4};
		VTBIT_R8* = {5};
		VTBIT_CY* = {6};
		VTBIT_DECIMAL* = {14};
		VAR_VALIDDATE* = 4;
		VAR_CALENDAR_HIJRI* = 8;
		VARIANT_CALENDAR_HIJRI* = 8;
		MEMBERID_NIL* = -1;
		ID_DEFAULTINST* = -2;
		DISPATCH_METHOD* = 1;
		DISPATCH_PROPERTYGET* = 2;
		DISPATCH_PROPERTYPUT* = 4;
		DISPATCH_PROPERTYPUTREF* = 8;
		ACTIVEOBJECT_STRONG* = 0;
		ACTIVEOBJECT_WEAK* = 1;

	CONST (* enumerations *)
		SF_ERROR* = 10;
		SF_I1* = 16;
		SF_I2* = 2;
		SF_I4* = 3;
		SF_I8* = 20;
		SF_BSTR* = 8;
		SF_UNKNOWN* = 13;
		SF_DISPATCH* = 9;
		SF_VARIANT* = 12;
		TKIND_ENUM* = 0;
		TKIND_RECORD* = 1;
		TKIND_MODULE* = 2;
		TKIND_INTERFACE* = 3;
		TKIND_DISPATCH* = 4;
		TKIND_COCLASS* = 5;
		TKIND_ALIAS* = 6;
		TKIND_UNION* = 7;
		TKIND_MAX* = 8;
		CC_CDECL* = 1;
		CC_MSCPASCAL* = 2;
		CC_PASCAL* = 2;
		CC_MACPASCAL* = 3;
		CC_STDCALL* = 4;
		CC_RESERVED* = 5;
		CC_SYSCALL* = 6;
		CC_MPWCDECL* = 7;
		CC_MPWPASCAL* = 8;
		CC_MAX* = 9;
		FUNC_VIRTUAL* = 0;
		FUNC_PUREVIRTUAL* = 1;
		FUNC_NONVIRTUAL* = 2;
		FUNC_STATIC* = 3;
		FUNC_DISPATCH* = 4;
		INVOKE_FUNC* = 1;
		INVOKE_PROPERTYGET* = 2;
		INVOKE_PROPERTYPUT* = 4;
		INVOKE_PROPERTYPUTREF* = 8;
		VAR_PERINSTANCE* = 0;
		VAR_STATIC* = 1;
		VAR_CONST* = 2;
		VAR_DISPATCH* = 3;
		TYPEFLAG_FAPPOBJECT* = 1;
		TYPEFLAG_FCANCREATE* = 2;
		TYPEFLAG_FLICENSED* = 4;
		TYPEFLAG_FPREDECLID* = 8;
		TYPEFLAG_FHIDDEN* = 16;
		TYPEFLAG_FCONTROL* = 32;
		TYPEFLAG_FDUAL* = 64;
		TYPEFLAG_FNONEXTENSIBLE* = 128;
		TYPEFLAG_FOLEAUTOMATION* = 256;
		TYPEFLAG_FRESTRICTED* = 512;
		TYPEFLAG_FAGGREGATABLE* = 1024;
		TYPEFLAG_FREPLACEABLE* = 2048;
		TYPEFLAG_FDISPATCHABLE* = 4096;
		FUNCFLAG_FRESTRICTED* = 1;
		FUNCFLAG_FSOURCE* = 2;
		FUNCFLAG_FBINDABLE* = 4;
		FUNCFLAG_FREQUESTEDIT* = 8;
		FUNCFLAG_FDISPLAYBIND* = 16;
		FUNCFLAG_FDEFAULTBIND* = 32;
		FUNCFLAG_FHIDDEN* = 64;
		FUNCFLAG_FUSESGETLASTERROR* = 128;
		FUNCFLAG_FDEFAULTCOLLELEM* = 256;
		FUNCFLAG_FUIDEFAULT* = 512;
		FUNCFLAG_FNONBROWSABLE* = 1024;
		FUNCFLAG_FREPLACEABLE* = 2048;
		FUNCFLAG_FIMMEDIATEBIND* = 4096;
		VARFLAG_FREADONLY* = 1;
		VARFLAG_FSOURCE* = 2;
		VARFLAG_FBINDABLE* = 4;
		VARFLAG_FREQUESTEDIT* = 8;
		VARFLAG_FDISPLAYBIND* = 16;
		VARFLAG_FDEFAULTBIND* = 32;
		VARFLAG_FHIDDEN* = 64;
		VARFLAG_FRESTRICTED* = 128;
		VARFLAG_FDEFAULTCOLLELEM* = 256;
		VARFLAG_FUIDEFAULT* = 512;
		VARFLAG_FNONBROWSABLE* = 1024;
		VARFLAG_FREPLACEABLE* = 2048;
		VARFLAG_FIMMEDIATEBIND* = 4096;
		DESCKIND_NONE* = 0;
		DESCKIND_FUNCDESC* = 1;
		DESCKIND_VARDESC* = 2;
		DESCKIND_TYPECOMP* = 3;
		DESCKIND_IMPLICITAPPOBJ* = 4;
		DESCKIND_MAX* = 5;
		SYS_WIN16* = 0;
		SYS_WIN32* = 1;
		SYS_MAC* = 2;
		LIBFLAG_FRESTRICTED* = 1;
		LIBFLAG_FCONTROL* = 2;
		LIBFLAG_FHIDDEN* = 4;
		CHANGEKIND_ADDMEMBER* = 0;
		CHANGEKIND_DELETEMEMBER* = 1;
		CHANGEKIND_SETNAMES* = 2;
		CHANGEKIND_SETDOCUMENTATION* = 3;
		CHANGEKIND_GENERAL* = 4;
		CHANGEKIND_MAX* = 5;
		REGKIND_DEFAULT* = 0;
		REGKIND_REGISTER* = 1;
		REGKIND_NONE* = 2;

	TYPE
		CURRENCY* = WinApi.CY;
		SAFEARR_BSTR* = RECORD [untagged]
			Size*: INTEGER;
			aBstr*: POINTER TO (*?*) ARRAY [untagged] OF WinOle.PtrFLAGGED_WORD_BLOB;
		END;
		SAFEARR_UNKNOWN* = RECORD [untagged]
			Size*: INTEGER;
			apUnknown*: POINTER TO (*?*) ARRAY [untagged] OF COM.IUnknown;
		END;
		SAFEARR_DISPATCH* = RECORD [untagged]
			Size*: INTEGER;
			apDispatch*: POINTER TO (*?*) ARRAY [untagged] OF IDispatch;
		END;
		Ptr_wireVARIANT* = POINTER TO _wireVARIANT;
		SAFEARR_VARIANT* = RECORD [untagged]
			Size*: INTEGER;
			aVariant*: POINTER TO (*?*) ARRAY [untagged] OF Ptr_wireVARIANT;
		END;
		SF_TYPE* = INTEGER;
		SAFEARRAYBOUND* = RECORD [untagged]
			cElements*: INTEGER;
			lLbound*: INTEGER;
		END;
		PtrSAFEARRAYBOUND* = POINTER TO SAFEARRAYBOUND;
		SAFEARRAYUNION* = RECORD [untagged]
			sfType*: INTEGER;
			u*: RECORD [union]
				BstrStr*: SAFEARR_BSTR;
				UnknownStr*: SAFEARR_UNKNOWN;
				DispatchStr*: SAFEARR_DISPATCH;
				VariantStr*: SAFEARR_VARIANT;
				ByteStr*: WinOle.BYTE_SIZEDARR;
				WordStr*: WinOle.WORD_SIZEDARR;
				LongStr*: WinOle.DWORD_SIZEDARR;
				HyperStr*: WinOle.HYPER_SIZEDARR;
			END;
		END;
		_wireSAFEARRAY* = RECORD [untagged]
			cDims*: SHORTINT;
			fFeatures*: SHORTINT;
			cbElements*: INTEGER;
			cLocks*: INTEGER;
			uArrayStructs*: SAFEARRAYUNION;
			rgsabound*: ARRAY [untagged] 16 OF SAFEARRAYBOUND;
		END;
		Ptr_wireSAFEARRAY* = POINTER TO _wireSAFEARRAY;
		SAFEARRAY* = RECORD [untagged]
			cDims*: SHORTINT;
			fFeatures*: SHORTINT;
			cbElements*: INTEGER;
			cLocks*: INTEGER;
			pvData*: WinApi.PtrVoid;
			rgsabound*: ARRAY [untagged] 16 OF SAFEARRAYBOUND;
		END;
		PtrSAFEARRAY* = POINTER TO SAFEARRAY;
		_VARIANT_BOOL* = WinOle.VARIANT_BOOL;
		PtrVARIANT* = POINTER TO VARIANT;
		VARIANT* = RECORD [align8]
			vt*: WinOle.VARTYPE;
			scale*: SHORTCHAR;
			sign*: SHORTCHAR;
			Hi32*: INTEGER;
			u*: RECORD [union]
				lVal*: INTEGER;
				bVal*: SHORTCHAR;
				iVal*: SHORTINT;
				fltVal*: SHORTREAL;
				dblVal*: REAL;
				boolVal*: WinOle.VARIANT_BOOL;
				bool*: _VARIANT_BOOL;
				scode*: COM.RESULT;
				cyVal*: WinApi.CY;
				date*: WinApi.DATE;
				bstrVal*: WinOle.BSTR;
				punkVal*: COM.IUnknown;
				pdispVal*: IDispatch;
				parray*: PtrSAFEARRAY;
				pbVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTCHAR;
				piVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
				plVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				pfltVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTREAL;
				pdblVal*: POINTER TO (*?*) ARRAY [untagged] OF REAL;
				pboolVal*: POINTER TO (*?*) ARRAY [untagged] OF WinOle.VARIANT_BOOL;
				pbool*: POINTER TO (*?*) ARRAY [untagged] OF _VARIANT_BOOL;
				pscode*: POINTER TO (*?*) ARRAY [untagged] OF COM.RESULT;
				pcyVal*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.CY;
				pdate*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.DATE;
				pbstrVal*: POINTER TO (*?*) ARRAY [untagged] OF WinOle.BSTR;
				ppunkVal*: POINTER TO (*?*) ARRAY [untagged] OF COM.IUnknown;
				ppdispVal*: POINTER TO (*?*) ARRAY [untagged] OF IDispatch;
				pparray*: POINTER TO (*?*) ARRAY [untagged] OF PtrSAFEARRAY;
				pvarVal*: PtrVARIANT;
				byref*: WinApi.PtrVoid;
				cVal*: SHORTCHAR;
				uiVal*: SHORTINT;
				ulVal*: INTEGER;
				intVal*: INTEGER;
				uintVal*: INTEGER;
				pdecVal*: WinOle.PtrDECIMAL;
				pcVal*: WinApi.PtrSTR;
				puiVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
				pulVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				pintVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				puintVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				Lo64*: LONGINT;
			END
		END;
		VARIANTARG* = VARIANT;
		PtrVARIANTARG* = PtrVARIANT;
		_wireVARIANT* = RECORD [align8]
			vt*: SHORTINT;
			wReserved1*: SHORTINT;
			wReserved2*: SHORTINT;
			wReserved3*: SHORTINT;
			u*: RECORD [union]
				lVal*: INTEGER;
				bVal*: SHORTCHAR;
				iVal*: SHORTINT;
				fltVal*: SHORTREAL;
				dblVal*: REAL;
				boolVal*: WinOle.VARIANT_BOOL;
				scode*: COM.RESULT;
				cyVal*: WinApi.CY;
				date*: WinApi.DATE;
				bstrVal*: WinOle.PtrFLAGGED_WORD_BLOB;
				punkVal*: COM.IUnknown;
				pdispVal*: IDispatch;
				parray*: Ptr_wireSAFEARRAY;
				pbVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTCHAR;
				piVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
				plVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				pfltVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTREAL;
				pdblVal*: POINTER TO (*?*) ARRAY [untagged] OF REAL;
				pboolVal*: POINTER TO (*?*) ARRAY [untagged] OF WinOle.VARIANT_BOOL;
				pscode*: POINTER TO (*?*) ARRAY [untagged] OF COM.RESULT;
				pcyVal*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.CY;
				pdate*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.DATE;
				pbstrVal*: POINTER TO (*?*) ARRAY [untagged] OF WinOle.PtrFLAGGED_WORD_BLOB;
				ppunkVal*: POINTER TO (*?*) ARRAY [untagged] OF COM.IUnknown;
				ppdispVal*: POINTER TO (*?*) ARRAY [untagged] OF IDispatch;
				pparray*: POINTER TO (*?*) ARRAY [untagged] OF Ptr_wireSAFEARRAY;
				pvarVal*: POINTER TO (*?*) ARRAY [untagged] OF Ptr_wireVARIANT;
				byref*: WinApi.PtrVoid;
				cVal*: SHORTCHAR;
				uiVal*: SHORTINT;
				ulVal*: INTEGER;
				intVal*: INTEGER;
				uintVal*: INTEGER;
				pdecVal*: WinOle.PtrDECIMAL;
				pcVal*: WinApi.PtrSTR;
				puiVal*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
				pulVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				pintVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
				puintVal*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			END;
		END;
		DISPID* = INTEGER;
		MEMBERID* = DISPID;
		HREFTYPE* = INTEGER;
		TYPEKIND* = INTEGER;
		PtrTYPEDESC* = POINTER TO TYPEDESC;
		PtrARRAYDESC* = POINTER TO ARRAYDESC;
		TYPEDESC* = RECORD [untagged]
			u*: RECORD [union]
				lptdesc*: PtrTYPEDESC;
				lpadesc*: PtrARRAYDESC;
				hreftype*: HREFTYPE;
			END;
			vt*: WinOle.VARTYPE;
		END;
		ARRAYDESC* = RECORD [untagged]
			tdescElem*: TYPEDESC;
			cDims*: SHORTINT;
			rgbounds*: ARRAY [untagged] 16 OF SAFEARRAYBOUND;
		END;
		PARAMDESCEX* = RECORD [align8]
			cBytes*: INTEGER;
			varDefaultValue*: VARIANTARG;
		END;
		PtrPARAMDESCEX* = POINTER TO PARAMDESCEX;
		PARAMDESC* = RECORD [untagged]
			pparamdescex*: PtrPARAMDESCEX;
			wParamFlags*: SHORTINT;
		END;
		PtrPARAMDESC* = POINTER TO PARAMDESC;
		IDLDESC* = RECORD [untagged]
			dwReserved*: INTEGER;
			wIDLFlags*: SHORTINT;
		END;
		PtrIDLDESC* = POINTER TO IDLDESC;
		ELEMDESC* = RECORD [untagged]
			tdesc*: TYPEDESC;
			u*: RECORD [union]
				idldesc*: IDLDESC;
				paramdesc*: PARAMDESC;
			END;
		END;
		PtrELEMDESC* = POINTER TO ELEMDESC;
		TYPEATTR* = RECORD [untagged]
			guid*: COM.GUID;
			lcid*: WinOle.LCID;
			dwReserved*: INTEGER;
			memidConstructor*: MEMBERID;
			memidDestructor*: MEMBERID;
			lpstrSchema*: WinApi.PtrWSTR;
			cbSizeInstance*: INTEGER;
			typekind*: TYPEKIND;
			cFuncs*: SHORTINT;
			cVars*: SHORTINT;
			cImplTypes*: SHORTINT;
			cbSizeVft*: SHORTINT;
			cbAlignment*: SHORTINT;
			wTypeFlags*: SHORTINT;
			wMajorVerNum*: SHORTINT;
			wMinorVerNum*: SHORTINT;
			tdescAlias*: TYPEDESC;
			idldescType*: IDLDESC;
		END;
		PtrTYPEATTR* = POINTER TO TYPEATTR;
		_wireDISPPARAMS* = RECORD [untagged]
			rgvarg*: POINTER TO (*?*) ARRAY [untagged] OF Ptr_wireVARIANT;
			rgdispidNamedArgs*: POINTER TO (*?*) ARRAY [untagged] OF DISPID;
			cArgs*: INTEGER;
			cNamedArgs*: INTEGER;
		END;
		Ptr_wireDISPPARAMS* = POINTER TO _wireDISPPARAMS;
		DISPPARAMS* = RECORD [untagged]
			rgvarg*: POINTER TO ARRAY [untagged] OF VARIANTARG;
			rgdispidNamedArgs*: POINTER TO ARRAY [untagged] OF DISPID;
			cArgs*: INTEGER;
			cNamedArgs*: INTEGER;
		END;
		PtrDISPPARAMS* = POINTER TO DISPPARAMS;
		_wireEXCEPINFO* = RECORD [untagged]
			wCode*: SHORTINT;
			wReserved*: SHORTINT;
			bstrSource*: WinOle.BSTR;
			bstrDescription*: WinOle.BSTR;
			bstrHelpFile*: WinOle.BSTR;
			dwHelpContext*: INTEGER;
			pvReserved*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			pfnDeferredFillIn*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			scode*: COM.RESULT;
		END;
		Ptr_wireEXCEPINFO* = POINTER TO _wireEXCEPINFO;
		PtrEXCEPINFO* = POINTER TO EXCEPINFO;
		EXCEPINFO* = RECORD [untagged]
			wCode*: SHORTINT;
			wReserved*: SHORTINT;
			bstrSource*: WinOle.BSTR;
			bstrDescription*: WinOle.BSTR;
			bstrHelpFile*: WinOle.BSTR;
			dwHelpContext*: INTEGER;
			pvReserved*: WinApi.PtrVoid;
			pfnDeferredFillIn*: PROCEDURE (VAR __MIDL_0012: EXCEPINFO): COM.RESULT;
			scode*: COM.RESULT;
		END;
		CALLCONV* = INTEGER;
		FUNCKIND* = INTEGER;
		INVOKEKIND* = INTEGER;
		FUNCDESC* = RECORD [untagged]
			memid*: MEMBERID;
			lprgscode*: POINTER TO (*?*) ARRAY [untagged] OF COM.RESULT;
			lprgelemdescParam*: POINTER TO ARRAY [untagged] OF ELEMDESC;
			funckind*: FUNCKIND;
			invkind*: INVOKEKIND;
			callconv*: CALLCONV;
			cParams*: SHORTINT;
			cParamsOpt*: SHORTINT;
			oVft*: SHORTINT;
			cScodes*: SHORTINT;
			elemdescFunc*: ELEMDESC;
			wFuncFlags*: SHORTINT;
		END;
		PtrFUNCDESC* = POINTER TO FUNCDESC;
		VARKIND* = INTEGER;
		VARDESC* = RECORD [untagged]
			memid*: MEMBERID;
			lpstrSchema*: WinApi.PtrWSTR;
			u*: RECORD [union]
				oInst*: INTEGER;
				lpvarValue*: PtrVARIANT;
			END;
			elemdescVar*: ELEMDESC;
			wVarFlags*: SHORTINT;
			varkind*: VARKIND;
		END;
		PtrVARDESC* = POINTER TO VARDESC;
		TYPEFLAGS* = INTEGER;
		FUNCFLAGS* = INTEGER;
		VARFLAGS* = INTEGER;
		ICreateTypeInfo* = POINTER TO ABSTRACT RECORD ["{00020405-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		ICreateTypeInfo2* = POINTER TO ABSTRACT RECORD ["{0002040E-0000-0000-C000-000000000046}"] (ICreateTypeInfo)
		END;
		ICreateTypeLib* = POINTER TO ABSTRACT RECORD ["{00020406-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		ICreateTypeLib2* = POINTER TO ABSTRACT RECORD ["{0002040F-0000-0000-C000-000000000046}"] (ICreateTypeLib)
		END;
		IDispatch* = POINTER TO ABSTRACT RECORD ["{00020400-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IEnumVARIANT* = POINTER TO ABSTRACT RECORD ["{00020404-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		DESCKIND* = INTEGER;
		ITypeComp* = POINTER TO ABSTRACT RECORD ["{00020403-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		tagBINDPTR* = RECORD [union]
			lpfuncdesc*: PtrFUNCDESC;
			lpvardesc*: PtrVARDESC;
			lptcomp*: ITypeComp;
		END;
		PtrtagBINDPTR* = POINTER TO tagBINDPTR;
		BINDPTR* = tagBINDPTR;
		PtrBINDPTR* = PtrtagBINDPTR;
		ITypeInfo* = POINTER TO ABSTRACT RECORD ["{00020401-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		ITypeInfo2* = POINTER TO ABSTRACT RECORD ["{00020412-0000-0000-C000-000000000046}"] (ITypeInfo)
		END;
		SYSKIND* = INTEGER;
		LIBFLAGS* = INTEGER;
		TLIBATTR* = RECORD [untagged]
			guid*: COM.GUID;
			lcid*: WinOle.LCID;
			syskind*: SYSKIND;
			wMajorVerNum*: SHORTINT;
			wMinorVerNum*: SHORTINT;
			wLibFlags*: SHORTINT;
		END;
		PtrTLIBATTR* = POINTER TO TLIBATTR;
		ITypeLib* = POINTER TO ABSTRACT RECORD ["{00020402-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		ITypeLib2* = POINTER TO ABSTRACT RECORD ["{00020411-0000-0000-C000-000000000046}"] (ITypeLib)
		END;
		CHANGEKIND* = INTEGER;
		ITypeChangeEvents* = POINTER TO ABSTRACT RECORD ["{00020410-0000-0000-C000-000000000046}"] (COM.IUnknown)
		END;
		IErrorInfo* = POINTER TO ABSTRACT RECORD ["{1CF2B120-547D-101B-8E65-08002B2BD119}"] (COM.IUnknown)
		END;
		ICreateErrorInfo* = POINTER TO ABSTRACT RECORD ["{22F03340-547D-101B-8E65-08002B2BD119}"] (COM.IUnknown)
		END;
		ISupportErrorInfo* = POINTER TO ABSTRACT RECORD ["{DF0B3D60-548F-101B-8E65-08002B2BD119}"] (COM.IUnknown)
		END;
		NUMPARSE* = RECORD [untagged]
			cDig*: INTEGER;
			dwInFlags*: SET;
			dwOutFlags*: SET;
			cchUsed*: INTEGER;
			nBaseShift*: INTEGER;
			nPwr10*: INTEGER;
		END;
		PtrNUMPARSE* = POINTER TO NUMPARSE;
		UDATE* = RECORD [untagged]
			st*: WinApi.SYSTEMTIME;
			wDayOfYear*: SHORTINT;
		END;
		PtrUDATE* = POINTER TO UDATE;
		REGKIND* = INTEGER;
		PARAMDATA* = RECORD [untagged]
			szName*: WinApi.PtrWSTR;
			vt*: WinOle.VARTYPE;
		END;
		PtrPARAMDATA* = POINTER TO PARAMDATA;
		METHODDATA* = RECORD [untagged]
			szName*: WinApi.PtrWSTR;
			ppdata*: PtrPARAMDATA;
			dispid*: DISPID;
			iMeth*: INTEGER;
			cc*: CALLCONV;
			cArgs*: INTEGER;
			wFlags*: SHORTINT;
			vtReturn*: WinOle.VARTYPE;
		END;
		PtrMETHODDATA* = POINTER TO METHODDATA;
		INTERFACEDATA* = RECORD [untagged]
			pmethdata*: PtrMETHODDATA;
			cMembers*: INTEGER;
		END;
		PtrINTERFACEDATA* = POINTER TO INTERFACEDATA;

	PROCEDURE (this: ICreateTypeInfo) SetGuid* (IN [nil] guid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetTypeFlags* (uTypeFlags: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetDocString* (pStrDoc: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetHelpContext* (dwHelpContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetVersion* (wMajorVerNum: SHORTINT; wMinorVerNum: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) AddRefTypeInfo* (pTInfo: ITypeInfo; VAR [nil] phRefType: HREFTYPE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) AddFuncDesc* (index: INTEGER; IN [nil] pFuncDesc: FUNCDESC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) AddImplType* (index: INTEGER; hRefType: HREFTYPE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetImplTypeFlags* (index: INTEGER; implTypeFlags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetAlignment* (cbAlignment: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetSchema* (pStrSchema: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) AddVarDesc* (index: INTEGER; IN [nil] pVarDesc: VARDESC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetFuncAndParamNames* (index: INTEGER; IN [nil] rgszNames: WinApi.PtrWSTR; cNames: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetVarName* (index: INTEGER; szName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetTypeDescAlias* (IN [nil] pTDescAlias: TYPEDESC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) DefineFuncAsDllEntry* (index: INTEGER; szDllName: WinApi.PtrWSTR; szProcName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetFuncDocString* (index: INTEGER; szDocString: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetVarDocString* (index: INTEGER; szDocString: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetFuncHelpContext* (index: INTEGER; dwHelpContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetVarHelpContext* (index: INTEGER; dwHelpContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetMops* (index: INTEGER; bstrMops: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) SetTypeIdldesc* (IN [nil] pIdlDesc: IDLDESC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo) LayOut* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) DeleteFuncDesc* (index: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) DeleteFuncDescByMemId* (memid: MEMBERID; invKind: INVOKEKIND): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) DeleteVarDesc* (index: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) DeleteVarDescByMemId* (memid: MEMBERID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) DeleteImplType* (index: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetCustData* (IN [nil] guid: COM.GUID; IN [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetFuncCustData* (index: INTEGER; IN [nil] guid: COM.GUID; IN [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetParamCustData* (indexFunc: INTEGER; indexParam: INTEGER; IN [nil] guid: COM.GUID; IN [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetVarCustData* (index: INTEGER; IN [nil] guid: COM.GUID; IN [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetImplTypeCustData* (index: INTEGER; IN [nil] guid: COM.GUID; IN [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetHelpStringContext* (dwHelpStringContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetFuncHelpStringContext* (index: INTEGER; dwHelpStringContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeInfo2) SetVarHelpStringContext* (index: INTEGER; dwHelpStringContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) CreateTypeInfo* (szName: WinApi.PtrWSTR; tkind: TYPEKIND; OUT [nil] ppCTInfo: ICreateTypeInfo): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetName* (szName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetVersion* (wMajorVerNum: SHORTINT; wMinorVerNum: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetGuid* (IN [nil] guid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetDocString* (szDoc: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetHelpFileName* (szHelpFileName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetHelpContext* (dwHelpContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetLcid* (lcid: WinOle.LCID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SetLibFlags* (uLibFlags: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib) SaveAllChanges* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib2) DeleteTypeInfo* (szName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib2) SetCustData* (IN [nil] guid: COM.GUID; IN [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateTypeLib2) SetHelpStringContext* (dwHelpStringContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDispatch) GetTypeInfoCount* (OUT [nil] pctinfo: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDispatch) GetTypeInfo* (iTInfo: INTEGER; lcid: WinOle.LCID; OUT [nil] ppTInfo: ITypeInfo): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDispatch) GetIDsOfNames* (IN [nil] riid: COM.GUID; IN [nil] rgszNames: WinApi.PtrWSTR; cNames: INTEGER; lcid: WinOle.LCID; OUT [nil] rgDispId: DISPID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IDispatch) Invoke* (dispIdMember: DISPID; IN riid: COM.GUID; lcid: WinOle.LCID; wFlags: SHORTINT; VAR [nil] pDispParams: DISPPARAMS; OUT [nil] pVarResult: VARIANT; OUT [nil] pExcepInfo: EXCEPINFO; OUT [nil] puArgErr: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumVARIANT) Next* (celt: INTEGER; OUT rgVar: ARRAY [untagged]  OF VARIANT; OUT [nil] pCeltFetched: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumVARIANT) Skip* (celt: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumVARIANT) Reset* (): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IEnumVARIANT) Clone* (OUT [nil] ppEnum: IEnumVARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeComp) Bind* (szName: WinApi.PtrWSTR; lHashVal: INTEGER; fFlags: SHORTINT; OUT [nil] ppTInfo: ITypeInfo; OUT [nil] pDescKind: DESCKIND; OUT [nil] pBindPtr: BINDPTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeComp) BindType* (szName: WinApi.PtrWSTR; lHashVal: INTEGER; OUT [nil] ppTInfo: ITypeInfo; OUT [nil] ppTComp: ITypeComp): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetTypeAttr* (OUT [nil] ppTypeAttr: PtrTYPEATTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetTypeComp* (OUT [nil] ppTComp: ITypeComp): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetFuncDesc* (index: INTEGER; OUT [nil] ppFuncDesc: PtrFUNCDESC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetVarDesc* (index: INTEGER; OUT [nil] ppVarDesc: PtrVARDESC): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetNames* (memid: MEMBERID; OUT [nil] rgBstrNames: WinOle.BSTR; cMaxNames: INTEGER; OUT [nil] pcNames: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetRefTypeOfImplType* (index: INTEGER; OUT [nil] pRefType: HREFTYPE): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetImplTypeFlags* (index: INTEGER; OUT [nil] pImplTypeFlags: SET): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetIDsOfNames* (IN [nil] ppNames: WinApi.PtrWSTR; cNames: INTEGER; OUT [nil] pMemId: MEMBERID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) Invoke* (pvInstance: WinApi.PtrVoid; memid: MEMBERID; wFlags: SHORTINT; IN [nil] pDispParams: DISPPARAMS; OUT [nil] pVarResult: VARIANT; OUT [nil] pExcepInfo: EXCEPINFO; OUT [nil] puArgErr: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetDocumentation* (memid: MEMBERID; OUT [nil] pBstrName: WinOle.BSTR; OUT [nil] pBstrDocString: WinOle.BSTR; OUT [nil] pdwHelpContext: INTEGER; OUT [nil] pBstrHelpFile: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetDllEntry* (memid: MEMBERID; invKind: INVOKEKIND; OUT [nil] pBstrDllName: WinOle.BSTR; OUT [nil] pBstrName: WinOle.BSTR; OUT [nil] pwOrdinal: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetRefTypeInfo* (hRefType: HREFTYPE; OUT [nil] ppTInfo: ITypeInfo): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) AddressOfMember* (memid: MEMBERID; invKind: INVOKEKIND; OUT [nil] ppv: WinApi.PtrVoid): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) CreateInstance* (pUnkOuter: COM.IUnknown; IN [iid] riid: COM.GUID; OUT [new] ppvObj: COM.IUnknown): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetMops* (memid: MEMBERID; OUT [nil] pBstrMops: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) GetContainingTypeLib* (OUT [nil] ppTLib: ITypeLib; OUT [nil] pIndex: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) ReleaseTypeAttr* (pTypeAttr: PtrTYPEATTR), NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) ReleaseFuncDesc* (pFuncDesc: PtrFUNCDESC), NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo) ReleaseVarDesc* (pVarDesc: PtrVARDESC), NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetTypeKind* (OUT [nil] pTypeKind: TYPEKIND): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetTypeFlags* (OUT [nil] pTypeFlags: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetFuncIndexOfMemId* (memid: MEMBERID; invKind: INVOKEKIND; OUT [nil] pFuncIndex: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetVarIndexOfMemId* (memid: MEMBERID; OUT [nil] pVarIndex: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetCustData* (IN [nil] guid: COM.GUID; OUT [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetFuncCustData* (index: INTEGER; IN [nil] guid: COM.GUID; OUT [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetParamCustData* (indexFunc: INTEGER; indexParam: INTEGER; IN [nil] guid: COM.GUID; OUT [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetVarCustData* (index: INTEGER; IN [nil] guid: COM.GUID; OUT [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetImplTypeCustData* (index: INTEGER; IN [nil] guid: COM.GUID; OUT [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeInfo2) GetHelpStringContext* (memid: MEMBERID; OUT [nil] pHelpStringContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetTypeInfoCount* (): INTEGER, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetTypeInfo* (index: INTEGER; OUT [nil] ppTInfo: ITypeInfo): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetTypeInfoType* (index: INTEGER; OUT [nil] pTKind: TYPEKIND): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetTypeInfoOfGuid* (IN [nil] guid: COM.GUID; OUT [nil] ppTinfo: ITypeInfo): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetLibAttr* (OUT [nil] ppTLibAttr: PtrTLIBATTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetTypeComp* (OUT [nil] ppTComp: ITypeComp): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) GetDocumentation* (index: INTEGER; OUT [nil] pBstrName: WinOle.BSTR; OUT [nil] pBstrDocString: WinOle.BSTR; OUT [nil] pdwHelpContext: INTEGER; OUT [nil] pBstrHelpFile: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) IsName* (szNameBuf: WinApi.PtrWSTR; lHashVal: INTEGER; OUT [nil] pfName: WinApi.BOOL): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) FindName* (szNameBuf: WinApi.PtrWSTR; lHashVal: INTEGER; OUT [nil] ppTInfo: ITypeInfo; OUT [nil] rgMemId: MEMBERID; VAR [nil] pcFound: SHORTINT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib) ReleaseTLibAttr* (pTLibAttr: PtrTLIBATTR), NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib2) GetCustData* (IN [nil] guid: COM.GUID; OUT [nil] pVarVal: VARIANT): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib2) GetLibStatistics* (OUT [nil] pcUniqueNames: INTEGER; OUT [nil] pcchUniqueNames: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeLib2) GetHelpStringContext* (index: INTEGER; OUT [nil] pHelpStringContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeChangeEvents) RequestTypeChange* (changeKind: CHANGEKIND; pTInfoBefore: ITypeInfo; pStrName: WinApi.PtrWSTR; OUT [nil] pfCancel: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ITypeChangeEvents) AfterTypeChange* (changeKind: CHANGEKIND; pTInfoAfter: ITypeInfo; pStrName: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IErrorInfo) GetGUID* (OUT [nil] pGUID: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IErrorInfo) GetSource* (OUT [nil] pBstrSource: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IErrorInfo) GetDescription* (OUT [nil] pBstrDescription: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IErrorInfo) GetHelpFile* (OUT [nil] pBstrHelpFile: WinOle.BSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: IErrorInfo) GetHelpContext* (OUT [nil] pdwHelpContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateErrorInfo) SetGUID* (IN [nil] rguid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateErrorInfo) SetSource* (szSource: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateErrorInfo) SetDescription* (szDescription: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateErrorInfo) SetHelpFile* (szHelpFile: WinApi.PtrWSTR): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ICreateErrorInfo) SetHelpContext* (dwHelpContext: INTEGER): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE (this: ISupportErrorInfo) InterfaceSupportsErrorInfo* (IN [nil] riid: COM.GUID): COM.RESULT, NEW, ABSTRACT;

	PROCEDURE SysAllocString* (p0: WinApi.PtrWSTR): WinOle.BSTR;

	PROCEDURE SysReAllocString* (VAR [nil] p0: WinOle.BSTR; p1: WinApi.PtrWSTR): INTEGER;

	PROCEDURE SysAllocStringLen* (p0: WinApi.PtrWSTR; p1: INTEGER): WinOle.BSTR;

	PROCEDURE SysReAllocStringLen* (VAR [nil] p0: WinOle.BSTR; p1: WinApi.PtrWSTR; p2: INTEGER): INTEGER;

	PROCEDURE SysFreeString* (p0: WinOle.BSTR);

	PROCEDURE SysStringLen* (p0: WinOle.BSTR): INTEGER;

	PROCEDURE SysStringByteLen* (bstr: WinOle.BSTR): INTEGER;

	PROCEDURE SysAllocStringByteLen* (psz: WinApi.PtrSTR; len: INTEGER): WinOle.BSTR;

	PROCEDURE DosDateTimeToVariantTime* (wDosDate: SHORTINT; wDosTime: SHORTINT; VAR [nil] pvtime: REAL): INTEGER;

	PROCEDURE VariantTimeToDosDateTime* (vtime: REAL; VAR [nil] pwDosDate: SHORTINT; VAR [nil] pwDosTime: SHORTINT): INTEGER;

	PROCEDURE SafeArrayAllocDescriptor* (cDims: INTEGER; VAR [nil] ppsaOut: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayAllocData* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayCreate* (vt: WinOle.VARTYPE; cDims: INTEGER; VAR [nil] rgsabound: SAFEARRAYBOUND): PtrSAFEARRAY;

	PROCEDURE SafeArrayDestroyDescriptor* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayDestroyData* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayDestroy* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayRedim* (psa: PtrSAFEARRAY; VAR [nil] psaboundNew: SAFEARRAYBOUND): COM.RESULT;

	PROCEDURE SafeArrayGetDim* (psa: PtrSAFEARRAY): INTEGER;

	PROCEDURE SafeArrayGetElemsize* (psa: PtrSAFEARRAY): INTEGER;

	PROCEDURE SafeArrayGetUBound* (psa: PtrSAFEARRAY; nDim: INTEGER; VAR [nil] plUbound: INTEGER): COM.RESULT;

	PROCEDURE SafeArrayGetLBound* (psa: PtrSAFEARRAY; nDim: INTEGER; VAR [nil] plLbound: INTEGER): COM.RESULT;

	PROCEDURE SafeArrayLock* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayUnlock* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayAccessData* (psa: PtrSAFEARRAY; VAR [nil] ppvData: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE SafeArrayUnaccessData* (psa: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayGetElement* (psa: PtrSAFEARRAY; VAR [nil] rgIndices: INTEGER; pv: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE SafeArrayPutElement* (psa: PtrSAFEARRAY; VAR [nil] rgIndices: INTEGER; pv: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE SafeArrayCopy* (psa: PtrSAFEARRAY; VAR [nil] ppsaOut: PtrSAFEARRAY): COM.RESULT;

	PROCEDURE SafeArrayPtrOfIndex* (psa: PtrSAFEARRAY; VAR [nil] rgIndices: INTEGER; VAR [nil] ppvData: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE VariantInit* (VAR [nil] pvarg: VARIANTARG);

	PROCEDURE VariantClear* (VAR [nil] pvarg: VARIANTARG): COM.RESULT;

	PROCEDURE VariantCopy* (VAR [nil] pvargDest: VARIANTARG; VAR [nil] pvargSrc: VARIANTARG): COM.RESULT;

	PROCEDURE VariantCopyInd* (VAR [nil] pvarDest: VARIANT; VAR [nil] pvargSrc: VARIANTARG): COM.RESULT;

	PROCEDURE VariantChangeType* (VAR [nil] pvargDest: VARIANTARG; VAR [nil] pvarSrc: VARIANTARG; wFlags: SHORTINT; vt: WinOle.VARTYPE): COM.RESULT;

	PROCEDURE VariantChangeTypeEx* (VAR [nil] pvargDest: VARIANTARG; VAR [nil] pvarSrc: VARIANTARG; lcid: WinOle.LCID; wFlags: SHORTINT; vt: WinOle.VARTYPE): COM.RESULT;

	PROCEDURE VarUI1FromI2* (sIn: SHORTINT; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromI4* (lIn: INTEGER; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromR4* (fltIn: SHORTREAL; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromR8* (dblIn: REAL; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromCy* (cyIn: WinApi.CY; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromDate* (dateIn: WinApi.DATE; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarUI1FromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarI2FromUI1* (bIn: SHORTCHAR; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromI4* (lIn: INTEGER; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromR4* (fltIn: SHORTREAL; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromR8* (dblIn: REAL; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromCy* (cyIn: WinApi.CY; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromDate* (dateIn: WinApi.DATE; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI2FromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarI4FromUI1* (bIn: SHORTCHAR; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromI2* (sIn: SHORTINT; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromR4* (fltIn: SHORTREAL; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromR8* (dblIn: REAL; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromCy* (cyIn: WinApi.CY; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromDate* (dateIn: WinApi.DATE; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarI4FromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarR4FromUI1* (bIn: SHORTCHAR; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromI2* (sIn: SHORTINT; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromI4* (lIn: INTEGER; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromR8* (dblIn: REAL; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromCy* (cyIn: WinApi.CY; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromDate* (dateIn: WinApi.DATE; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR4FromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR8FromUI1* (bIn: SHORTCHAR; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromI2* (sIn: SHORTINT; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromI4* (lIn: INTEGER; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromR4* (fltIn: SHORTREAL; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromCy* (cyIn: WinApi.CY; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromDate* (dateIn: WinApi.DATE; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarR8FromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarDateFromUI1* (bIn: SHORTCHAR; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromI2* (sIn: SHORTINT; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromI4* (lIn: INTEGER; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromR4* (fltIn: SHORTREAL; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromR8* (dblIn: REAL; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromCy* (cyIn: WinApi.CY; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarDateFromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarCyFromUI1* (bIn: SHORTCHAR; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromI2* (sIn: SHORTINT; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromI4* (lIn: INTEGER; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromR4* (fltIn: SHORTREAL; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromR8* (dblIn: REAL; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromDate* (dateIn: WinApi.DATE; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarCyFromBool* (boolIn: WinOle.VARIANT_BOOL; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarBstrFromUI1* (bVal: SHORTCHAR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromI2* (iVal: SHORTINT; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromI4* (lIn: INTEGER; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromR4* (fltIn: SHORTREAL; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromR8* (dblIn: REAL; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromCy* (cyIn: WinApi.CY; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromDate* (dateIn: WinApi.DATE; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBstrFromBool* (boolIn: WinOle.VARIANT_BOOL; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBoolFromUI1* (bIn: SHORTCHAR; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromI2* (sIn: SHORTINT; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromI4* (lIn: INTEGER; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromR4* (fltIn: SHORTREAL; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromR8* (dblIn: REAL; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromDate* (dateIn: WinApi.DATE; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromCy* (cyIn: WinApi.CY; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromStr* (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarBoolFromDisp* (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarUI1FromInt* ["VarUI1FromI4"] (lIn: INTEGER; VAR [nil] pbOut: SHORTCHAR): COM.RESULT;

	PROCEDURE VarI2FromInt* ["VarI2FromI4"] (lIn: INTEGER; VAR [nil] psOut: SHORTINT): COM.RESULT;

	PROCEDURE VarR4FromInt* ["VarR4FromI4"] (lIn: INTEGER; VAR [nil] pfltOut: SHORTREAL): COM.RESULT;

	PROCEDURE VarR8FromInt* ["VarR8FromI4"] (lIn: INTEGER; VAR [nil] pdblOut: REAL): COM.RESULT;

	PROCEDURE VarDateFromInt* ["VarDateFromI4"] (lIn: INTEGER; VAR [nil] pdateOut: WinApi.DATE): COM.RESULT;

	PROCEDURE VarCyFromInt* ["VarCyFromI4"] (lIn: INTEGER; VAR [nil] pcyOut: WinApi.CY): COM.RESULT;

	PROCEDURE VarBstrFromInt* ["VarBstrFromI4"] (lIn: INTEGER; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] pbstrOut: WinOle.BSTR): COM.RESULT;

	PROCEDURE VarBoolFromInt* ["VarBoolFromI4"] (lIn: INTEGER; VAR [nil] pboolOut: WinOle.VARIANT_BOOL): COM.RESULT;

	PROCEDURE VarIntFromUI1* ["VarI4FromUI1"] (bIn: SHORTCHAR; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromI2* ["VarI4FromI2"] (sIn: SHORTINT; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromR4* ["VarI4FromR4"] (fltIn: SHORTREAL; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromR8* ["VarI4FromR8"] (dblIn: REAL; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromDate* ["VarI4FromDate"] (dateIn: WinApi.DATE; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromCy* ["VarI4FromCy"] (cyIn: WinApi.CY; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromStr* ["VarI4FromStr"] (strIn: WinApi.PtrWSTR; lcid: WinOle.LCID; dwFlags: SET; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromDisp* ["VarI4FromDisp"] (pdispIn: IDispatch; lcid: WinOle.LCID; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE VarIntFromBool* ["VarI4FromBool"] (boolIn: WinOle.VARIANT_BOOL; VAR [nil] plOut: INTEGER): COM.RESULT;

	PROCEDURE LHashValOfNameSysA* (syskind: SYSKIND; lcid: WinOle.LCID; szName: WinApi.PtrSTR): INTEGER;

	PROCEDURE LHashValOfNameSys* (syskind: SYSKIND; lcid: WinOle.LCID; szName: WinApi.PtrWSTR): INTEGER;

	PROCEDURE LoadTypeLib* (szFile: WinApi.PtrWSTR; VAR [nil] pptlib: ITypeLib): COM.RESULT;

	PROCEDURE LoadRegTypeLib* (IN [nil] rguid: COM.GUID; wVerMajor: SHORTINT; wVerMinor: SHORTINT; lcid: WinOle.LCID; VAR [nil] pptlib: ITypeLib): COM.RESULT;

	PROCEDURE QueryPathOfRegTypeLib* (IN [nil] guid: COM.GUID; wMaj: SHORTINT; wMin: SHORTINT; lcid: WinOle.LCID; VAR [nil] lpbstrPathName: WinOle.BSTR): COM.RESULT;

	PROCEDURE RegisterTypeLib* (ptlib: ITypeLib; szFullPath: WinApi.PtrWSTR; szHelpDir: WinApi.PtrWSTR): COM.RESULT;

	PROCEDURE CreateTypeLib* (syskind: SYSKIND; szFile: WinApi.PtrWSTR; VAR [nil] ppctlib: ICreateTypeLib): COM.RESULT;

	PROCEDURE DispGetParam* (VAR [nil] pdispparams: DISPPARAMS; position: INTEGER; vtTarg: WinOle.VARTYPE; VAR [nil] pvarResult: VARIANT; VAR [nil] puArgErr: INTEGER): COM.RESULT;

	PROCEDURE DispGetIDsOfNames* (ptinfo: ITypeInfo; VAR [nil] rgszNames: WinApi.PtrWSTR; cNames: INTEGER; VAR [nil] rgdispid: DISPID): COM.RESULT;

	PROCEDURE DispInvoke* (_this: WinApi.PtrVoid; ptinfo: ITypeInfo; dispidMember: DISPID; wFlags: SHORTINT; VAR [nil] pparams: DISPPARAMS; VAR [nil] pvarResult: VARIANT; VAR [nil] pexcepinfo: EXCEPINFO; VAR [nil] puArgErr: INTEGER): COM.RESULT;

	PROCEDURE CreateDispTypeInfo* (VAR [nil] pidata: INTERFACEDATA; lcid: WinOle.LCID; VAR [nil] pptinfo: ITypeInfo): COM.RESULT;

	PROCEDURE CreateStdDispatch* (punkOuter: COM.IUnknown; pvThis: WinApi.PtrVoid; ptinfo: ITypeInfo; VAR [nil] ppunkStdDisp: COM.IUnknown): COM.RESULT;

	PROCEDURE RegisterActiveObject* (punk: COM.IUnknown; IN [nil] rclsid: COM.GUID; dwFlags: SET; VAR [nil] pdwRegister: INTEGER): COM.RESULT;

	PROCEDURE RevokeActiveObject* (dwRegister: INTEGER; pvReserved: WinApi.PtrVoid): COM.RESULT;

	PROCEDURE GetActiveObject* (IN [nil] rclsid: COM.GUID; pvReserved: WinApi.PtrVoid; VAR [nil] ppunk: COM.IUnknown): COM.RESULT;

	PROCEDURE SetErrorInfo* (dwReserved: INTEGER; perrinfo: IErrorInfo): COM.RESULT;

	PROCEDURE GetErrorInfo* (dwReserved: INTEGER; VAR [nil] pperrinfo: IErrorInfo): COM.RESULT;

	PROCEDURE CreateErrorInfo* (VAR [nil] pperrinfo: ICreateErrorInfo): COM.RESULT;

	PROCEDURE OaBuildVersion* (): INTEGER;


	(* manually added procedures *)
	
	PROCEDURE VariantTimeToSystemTime* (variantTime: REAL; VAR [nil] systemTime: WinApi.SYSTEMTIME): INTEGER;

	PROCEDURE SystemTimeToVariantTime* (VAR [nil] systemTime: WinApi.SYSTEMTIME; VAR [nil] variantTime: REAL): INTEGER;
	
END WinOleAut.
