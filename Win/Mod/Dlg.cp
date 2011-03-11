MODULE WinDlg ["COMDLG32.dll"];
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

	IMPORT WinApi;

	CONST (* macros *)
		OFN_READONLY* = {0};
		OFN_OVERWRITEPROMPT* = {1};
		OFN_HIDEREADONLY* = {2};
		OFN_NOCHANGEDIR* = {3};
		OFN_SHOWHELP* = {4};
		OFN_ENABLEHOOK* = {5};
		OFN_ENABLETEMPLATE* = {6};
		OFN_ENABLETEMPLATEHANDLE* = {7};
		OFN_NOVALIDATE* = {8};
		OFN_ALLOWMULTISELECT* = {9};
		OFN_EXTENSIONDIFFERENT* = {10};
		OFN_PATHMUSTEXIST* = {11};
		OFN_FILEMUSTEXIST* = {12};
		OFN_CREATEPROMPT* = {13};
		OFN_SHAREAWARE* = {14};
		OFN_NOREADONLYRETURN* = {15};
		OFN_NOTESTFILECREATE* = {16};
		OFN_NONETWORKBUTTON* = {17};
		OFN_NOLONGNAMES* = {18};
		OFN_EXPLORER* = {19};
		OFN_NODEREFERENCELINKS* = {20};
		OFN_LONGNAMES* = {21};
		OFN_SHAREFALLTHROUGH* = 2;
		OFN_SHARENOWARN* = 1;
		OFN_SHAREWARN* = 0;
		CDN_INITDONE* = -601;
		CDN_SELCHANGE* = -602;
		CDN_FOLDERCHANGE* = -603;
		CDN_SHAREVIOLATION* = -604;
		CDN_HELP* = -605;
		CDN_FILEOK* = -606;
		CDN_TYPECHANGE* = -607;
		CDM_FIRST* = 1124;
		CDM_LAST* = 1224;
		CDM_GETSPEC* = 1124;
		CDM_GETFILEPATH* = 1125;
		CDM_GETFOLDERPATH* = 1126;
		CDM_GETFOLDERIDLIST* = 1127;
		CDM_SETCONTROLTEXT* = 1128;
		CDM_HIDECONTROL* = 1129;
		CDM_SETDEFEXT* = 1130;
		CC_RGBINIT* = {0};
		CC_FULLOPEN* = {1};
		CC_PREVENTFULLOPEN* = {2};
		CC_SHOWHELP* = {3};
		CC_ENABLEHOOK* = {4};
		CC_ENABLETEMPLATE* = {5};
		CC_ENABLETEMPLATEHANDLE* = {6};
		CC_SOLIDCOLOR* = {7};
		CC_ANYCOLOR* = {8};
		FR_DOWN* = {0};
		FR_WHOLEWORD* = {1};
		FR_MATCHCASE* = {2};
		FR_FINDNEXT* = {3};
		FR_REPLACE* = {4};
		FR_REPLACEALL* = {5};
		FR_DIALOGTERM* = {6};
		FR_SHOWHELP* = {7};
		FR_ENABLEHOOK* = {8};
		FR_ENABLETEMPLATE* = {9};
		FR_NOUPDOWN* = {10};
		FR_NOMATCHCASE* = {11};
		FR_NOWHOLEWORD* = {12};
		FR_ENABLETEMPLATEHANDLE* = {13};
		FR_HIDEUPDOWN* = {14};
		FR_HIDEMATCHCASE* = {15};
		FR_HIDEWHOLEWORD* = {16};
		CF_SCREENFONTS* = {0};
		CF_PRINTERFONTS* = {1};
		CF_BOTH* = {0, 1};
		CF_SHOWHELP* = {2};
		CF_ENABLEHOOK* = {3};
		CF_ENABLETEMPLATE* = {4};
		CF_ENABLETEMPLATEHANDLE* = {5};
		CF_INITTOLOGFONTSTRUCT* = {6};
		CF_USESTYLE* = {7};
		CF_EFFECTS* = {8};
		CF_APPLY* = {9};
		CF_ANSIONLY* = {10};
		CF_SCRIPTSONLY* = {10};
		CF_NOVECTORFONTS* = {11};
		CF_NOOEMFONTS* = {11};
		CF_NOSIMULATIONS* = {12};
		CF_LIMITSIZE* = {13};
		CF_FIXEDPITCHONLY* = {14};
		CF_WYSIWYG* = {15};
		CF_FORCEFONTEXIST* = {16};
		CF_SCALABLEONLY* = {17};
		CF_TTONLY* = {18};
		CF_NOFACESEL* = {19};
		CF_NOSTYLESEL* = {20};
		CF_NOSIZESEL* = {21};
		CF_SELECTSCRIPT* = {22};
		CF_NOSCRIPTSEL* = {23};
		CF_NOVERTFONTS* = {24};
		SIMULATED_FONTTYPE* = 32768;
		PRINTER_FONTTYPE* = 16384;
		SCREEN_FONTTYPE* = 8192;
		BOLD_FONTTYPE* = 256;
		ITALIC_FONTTYPE* = 512;
		REGULAR_FONTTYPE* = 1024;
		WM_CHOOSEFONT_GETLOGFONT* = 1025;
		LBSELCHSTRINGA* = "commdlg_LBSelChangedNotify";
		SHAREVISTRINGA* = "commdlg_ShareViolation";
		FILEOKSTRINGA* = "commdlg_FileNameOK";
		COLOROKSTRINGA* = "commdlg_ColorOK";
		SETRGBSTRINGA* = "commdlg_SetRGBColor";
		HELPMSGSTRINGA* = "commdlg_help";
		FINDMSGSTRINGA* = "commdlg_FindReplace";
		LBSELCHSTRINGW* = "commdlg_LBSelChangedNotify";
		SHAREVISTRINGW* = "commdlg_ShareViolation";
		FILEOKSTRINGW* = "commdlg_FileNameOK";
		COLOROKSTRINGW* = "commdlg_ColorOK";
		SETRGBSTRINGW* = "commdlg_SetRGBColor";
		HELPMSGSTRINGW* = "commdlg_help";
		FINDMSGSTRINGW* = "commdlg_FindReplace";
		LBSELCHSTRING* = "commdlg_LBSelChangedNotify";
		SHAREVISTRING* = "commdlg_ShareViolation";
		FILEOKSTRING* = "commdlg_FileNameOK";
		COLOROKSTRING* = "commdlg_ColorOK";
		SETRGBSTRING* = "commdlg_SetRGBColor";
		HELPMSGSTRING* = "commdlg_help";
		FINDMSGSTRING* = "commdlg_FindReplace";
		CD_LBSELNOITEMS* = -1;
		CD_LBSELCHANGE* = 0;
		CD_LBSELSUB* = 1;
		CD_LBSELADD* = 2;
		PD_ALLPAGES* = {};
		PD_SELECTION* = {0};
		PD_PAGENUMS* = {1};
		PD_NOSELECTION* = {2};
		PD_NOPAGENUMS* = {3};
		PD_COLLATE* = {4};
		PD_PRINTTOFILE* = {5};
		PD_PRINTSETUP* = {6};
		PD_NOWARNING* = {7};
		PD_RETURNDC* = {8};
		PD_RETURNIC* = {9};
		PD_RETURNDEFAULT* = {10};
		PD_SHOWHELP* = {11};
		PD_ENABLEPRINTHOOK* = {12};
		PD_ENABLESETUPHOOK* = {13};
		PD_ENABLEPRINTTEMPLATE* = {14};
		PD_ENABLESETUPTEMPLATE* = {15};
		PD_ENABLEPRINTTEMPLATEHANDLE* = {16};
		PD_ENABLESETUPTEMPLATEHANDLE* = {17};
		PD_USEDEVMODECOPIES* = {18};
		PD_USEDEVMODECOPIESANDCOLLATE* = {18};
		PD_DISABLEPRINTTOFILE* = {19};
		PD_HIDEPRINTTOFILE* = {20};
		PD_NONETWORKBUTTON* = {21};
		DN_DEFAULTPRN* = 1;
		WM_PSD_PAGESETUPDLG* = 1024;
		WM_PSD_FULLPAGERECT* = 1025;
		WM_PSD_MINMARGINRECT* = 1026;
		WM_PSD_MARGINRECT* = 1027;
		WM_PSD_GREEKTEXTRECT* = 1028;
		WM_PSD_ENVSTAMPRECT* = 1029;
		WM_PSD_YAFULLPAGERECT* = 1030;
		PSD_DEFAULTMINMARGINS* = {};
		PSD_INWININIINTLMEASURE* = {};
		PSD_MINMARGINS* = {0};
		PSD_MARGINS* = {1};
		PSD_INTHOUSANDTHSOFINCHES* = {2};
		PSD_INHUNDREDTHSOFMILLIMETERS* = {3};
		PSD_DISABLEMARGINS* = {4};
		PSD_DISABLEPRINTER* = {5};
		PSD_NOWARNING* = {7};
		PSD_DISABLEORIENTATION* = {8};
		PSD_RETURNDEFAULT* = {10};
		PSD_DISABLEPAPER* = {9};
		PSD_SHOWHELP* = {11};
		PSD_ENABLEPAGESETUPHOOK* = {13};
		PSD_ENABLEPAGESETUPTEMPLATE* = {15};
		PSD_ENABLEPAGESETUPTEMPLATEHANDLE* = {17};
		PSD_ENABLEPAGEPAINTHOOK* = {18};
		PSD_DISABLEPAGEPAINTING* = {19};
		PSD_NONETWORKBUTTON* = {21};

	CONST (* enumerations *)

	TYPE
		OFNHOOKPROC* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		OPENFILENAMEA* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hInstance*: WinApi.HINSTANCE;
			lpstrFilter*: WinApi.PtrSTR;
			lpstrCustomFilter*: WinApi.PtrSTR;
			nMaxCustFilter*: INTEGER;
			nFilterIndex*: INTEGER;
			lpstrFile*: WinApi.PtrSTR;
			nMaxFile*: INTEGER;
			lpstrFileTitle*: WinApi.PtrSTR;
			nMaxFileTitle*: INTEGER;
			lpstrInitialDir*: WinApi.PtrSTR;
			lpstrTitle*: WinApi.PtrSTR;
			Flags*: SET;
			nFileOffset*: SHORTINT;
			nFileExtension*: SHORTINT;
			lpstrDefExt*: WinApi.PtrSTR;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: OFNHOOKPROC;
			lpTemplateName*: WinApi.PtrSTR;
		END;
		PtrOPENFILENAMEA* = POINTER TO OPENFILENAMEA;
		OPENFILENAMEW* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hInstance*: WinApi.HINSTANCE;
			lpstrFilter*: WinApi.PtrWSTR;
			lpstrCustomFilter*: WinApi.PtrWSTR;
			nMaxCustFilter*: INTEGER;
			nFilterIndex*: INTEGER;
			lpstrFile*: WinApi.PtrWSTR;
			nMaxFile*: INTEGER;
			lpstrFileTitle*: WinApi.PtrWSTR;
			nMaxFileTitle*: INTEGER;
			lpstrInitialDir*: WinApi.PtrWSTR;
			lpstrTitle*: WinApi.PtrWSTR;
			Flags*: SET;
			nFileOffset*: SHORTINT;
			nFileExtension*: SHORTINT;
			lpstrDefExt*: WinApi.PtrWSTR;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: OFNHOOKPROC;
			lpTemplateName*: WinApi.PtrWSTR;
		END;
		PtrOPENFILENAMEW* = POINTER TO OPENFILENAMEW;
		OPENFILENAME* = OPENFILENAMEA;
		PtrOPENFILENAME* = PtrOPENFILENAMEA;
		CCHOOKPROC* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		OFNOTIFYA* = RECORD [noalign]
			hdr*: WinApi.NMHDR;
			lpOFN*: PtrOPENFILENAMEA;
			pszFile*: WinApi.PtrSTR;
		END;
		PtrOFNOTIFYA* = POINTER TO OFNOTIFYA;
		OFNOTIFYW* = RECORD [noalign]
			hdr*: WinApi.NMHDR;
			lpOFN*: PtrOPENFILENAMEW;
			pszFile*: WinApi.PtrWSTR;
		END;
		PtrOFNOTIFYW* = POINTER TO OFNOTIFYW;
		OFNOTIFY* = OFNOTIFYA;
		PtrOFNOTIFY* = PtrOFNOTIFYA;
		CHOOSECOLORA* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hInstance*: WinApi.HWND;
			rgbResult*: WinApi.COLORREF;
			lpCustColors*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.COLORREF;
			Flags*: SET;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: CCHOOKPROC;
			lpTemplateName*: WinApi.PtrSTR;
		END;
		PtrCHOOSECOLORA* = POINTER TO CHOOSECOLORA;
		CHOOSECOLORW* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hInstance*: WinApi.HWND;
			rgbResult*: WinApi.COLORREF;
			lpCustColors*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.COLORREF;
			Flags*: SET;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: CCHOOKPROC;
			lpTemplateName*: WinApi.PtrWSTR;
		END;
		PtrCHOOSECOLORW* = POINTER TO CHOOSECOLORW;
		CHOOSECOLOR* = CHOOSECOLORA;
		PtrCHOOSECOLOR* = PtrCHOOSECOLORA;
		FRHOOKPROC* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		FINDREPLACEA* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hInstance*: WinApi.HINSTANCE;
			Flags*: SET;
			lpstrFindWhat*: WinApi.PtrSTR;
			lpstrReplaceWith*: WinApi.PtrSTR;
			wFindWhatLen*: SHORTINT;
			wReplaceWithLen*: SHORTINT;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: FRHOOKPROC;
			lpTemplateName*: WinApi.PtrSTR;
		END;
		PtrFINDREPLACEA* = POINTER TO FINDREPLACEA;
		FINDREPLACEW* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hInstance*: WinApi.HINSTANCE;
			Flags*: SET;
			lpstrFindWhat*: WinApi.PtrWSTR;
			lpstrReplaceWith*: WinApi.PtrWSTR;
			wFindWhatLen*: SHORTINT;
			wReplaceWithLen*: SHORTINT;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: FRHOOKPROC;
			lpTemplateName*: WinApi.PtrWSTR;
		END;
		PtrFINDREPLACEW* = POINTER TO FINDREPLACEW;
		FINDREPLACE* = FINDREPLACEA;
		PtrFINDREPLACE* = PtrFINDREPLACEA;
		CFHOOKPROC* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		CHOOSEFONTA* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hDC*: WinApi.HDC;
			lpLogFont*: WinApi.PtrLOGFONTA;
			iPointSize*: INTEGER;
			Flags*: SET;
			rgbColors*: WinApi.COLORREF;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: CFHOOKPROC;
			lpTemplateName*: WinApi.PtrSTR;
			hInstance*: WinApi.HINSTANCE;
			lpszStyle*: WinApi.PtrSTR;
			nFontType*: SHORTINT;
			___MISSING_ALIGNMENT__*: SHORTINT;
			nSizeMin*: INTEGER;
			nSizeMax*: INTEGER;
		END;
		PtrCHOOSEFONTA* = POINTER TO CHOOSEFONTA;
		CHOOSEFONTW* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hDC*: WinApi.HDC;
			lpLogFont*: WinApi.PtrLOGFONTW;
			iPointSize*: INTEGER;
			Flags*: SET;
			rgbColors*: WinApi.COLORREF;
			lCustData*: WinApi.LPARAM;
			lpfnHook*: CFHOOKPROC;
			lpTemplateName*: WinApi.PtrWSTR;
			hInstance*: WinApi.HINSTANCE;
			lpszStyle*: WinApi.PtrWSTR;
			nFontType*: SHORTINT;
			___MISSING_ALIGNMENT__*: SHORTINT;
			nSizeMin*: INTEGER;
			nSizeMax*: INTEGER;
		END;
		PtrCHOOSEFONTW* = POINTER TO CHOOSEFONTW;
		CHOOSEFONT* = CHOOSEFONTA;
		PtrCHOOSEFONT* = PtrCHOOSEFONTA;
		PRINTHOOKPROC* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		SETUPHOOKPROC* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		PRINTDLGA* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hDevMode*: WinApi.HGLOBAL;
			hDevNames*: WinApi.HGLOBAL;
			hDC*: WinApi.HDC;
			Flags*: SET;
			nFromPage*: SHORTINT;
			nToPage*: SHORTINT;
			nMinPage*: SHORTINT;
			nMaxPage*: SHORTINT;
			nCopies*: SHORTINT;
			hInstance*: WinApi.HINSTANCE;
			lCustData*: WinApi.LPARAM;
			lpfnPrintHook*: PRINTHOOKPROC;
			lpfnSetupHook*: SETUPHOOKPROC;
			lpPrintTemplateName*: WinApi.PtrSTR;
			lpSetupTemplateName*: WinApi.PtrSTR;
			hPrintTemplate*: WinApi.HGLOBAL;
			hSetupTemplate*: WinApi.HGLOBAL;
		END;
		PtrPRINTDLGA* = POINTER TO PRINTDLGA;
		PRINTDLGW* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hDevMode*: WinApi.HGLOBAL;
			hDevNames*: WinApi.HGLOBAL;
			hDC*: WinApi.HDC;
			Flags*: SET;
			nFromPage*: SHORTINT;
			nToPage*: SHORTINT;
			nMinPage*: SHORTINT;
			nMaxPage*: SHORTINT;
			nCopies*: SHORTINT;
			hInstance*: WinApi.HINSTANCE;
			lCustData*: WinApi.LPARAM;
			lpfnPrintHook*: PRINTHOOKPROC;
			lpfnSetupHook*: SETUPHOOKPROC;
			lpPrintTemplateName*: WinApi.PtrWSTR;
			lpSetupTemplateName*: WinApi.PtrWSTR;
			hPrintTemplate*: WinApi.HGLOBAL;
			hSetupTemplate*: WinApi.HGLOBAL;
		END;
		PtrPRINTDLGW* = POINTER TO PRINTDLGW;
		PRINTDLG* = PRINTDLGA;
		PtrPRINTDLG* = PtrPRINTDLGA;
		DEVNAMES* = RECORD [noalign]
			wDriverOffset*: SHORTINT;
			wDeviceOffset*: SHORTINT;
			wOutputOffset*: SHORTINT;
			wDefault*: SHORTINT;
		END;
		PtrDEVNAMES* = POINTER TO DEVNAMES;
		PAGEPAINTHOOK* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		PAGESETUPHOOK* = PROCEDURE (p0: WinApi.HWND; p1: INTEGER; p2: WinApi.WPARAM; p3: WinApi.LPARAM): INTEGER;
		PAGESETUPDLGA* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hDevMode*: WinApi.HGLOBAL;
			hDevNames*: WinApi.HGLOBAL;
			Flags*: SET;
			ptPaperSize*: WinApi.POINT;
			rtMinMargin*: WinApi.RECT;
			rtMargin*: WinApi.RECT;
			hInstance*: WinApi.HINSTANCE;
			lCustData*: WinApi.LPARAM;
			lpfnPageSetupHook*: PAGESETUPHOOK;
			lpfnPagePaintHook*: PAGEPAINTHOOK;
			lpPageSetupTemplateName*: WinApi.PtrSTR;
			hPageSetupTemplate*: WinApi.HGLOBAL;
		END;
		PtrPAGESETUPDLGA* = POINTER TO PAGESETUPDLGA;
		PAGESETUPDLGW* = RECORD [noalign]
			lStructSize*: INTEGER;
			hwndOwner*: WinApi.HWND;
			hDevMode*: WinApi.HGLOBAL;
			hDevNames*: WinApi.HGLOBAL;
			Flags*: SET;
			ptPaperSize*: WinApi.POINT;
			rtMinMargin*: WinApi.RECT;
			rtMargin*: WinApi.RECT;
			hInstance*: WinApi.HINSTANCE;
			lCustData*: WinApi.LPARAM;
			lpfnPageSetupHook*: PAGESETUPHOOK;
			lpfnPagePaintHook*: PAGEPAINTHOOK;
			lpPageSetupTemplateName*: WinApi.PtrWSTR;
			hPageSetupTemplate*: WinApi.HGLOBAL;
		END;
		PtrPAGESETUPDLGW* = POINTER TO PAGESETUPDLGW;
		PAGESETUPDLG* = PAGESETUPDLGA;
		PtrPAGESETUPDLG* = PtrPAGESETUPDLGA;

	PROCEDURE GetOpenFileNameA* (VAR [nil] p0: OPENFILENAMEA): WinApi.BOOL;
	(*END GetOpenFileNameA;*)

	PROCEDURE GetOpenFileNameW* (VAR [nil] p0: OPENFILENAMEW): WinApi.BOOL;
	(*END GetOpenFileNameW;*)

	PROCEDURE GetOpenFileName* ["GetOpenFileNameA"] (VAR [nil] p0: OPENFILENAMEA): WinApi.BOOL;
	(*END GetOpenFileName;*)

	PROCEDURE GetSaveFileNameA* (VAR [nil] p0: OPENFILENAMEA): WinApi.BOOL;
	(*END GetSaveFileNameA;*)

	PROCEDURE GetSaveFileNameW* (VAR [nil] p0: OPENFILENAMEW): WinApi.BOOL;
	(*END GetSaveFileNameW;*)

	PROCEDURE GetSaveFileName* ["GetSaveFileNameA"] (VAR [nil] p0: OPENFILENAMEA): WinApi.BOOL;
	(*END GetSaveFileName;*)

	PROCEDURE GetFileTitleA* (p0: WinApi.PtrSTR; p1: WinApi.PtrSTR; p2: SHORTINT): SHORTINT;
	(*END GetFileTitleA;*)

	PROCEDURE GetFileTitleW* (p0: WinApi.PtrWSTR; p1: WinApi.PtrWSTR; p2: SHORTINT): SHORTINT;
	(*END GetFileTitleW;*)

	PROCEDURE GetFileTitle* ["GetFileTitleA"] (p0: WinApi.PtrSTR; p1: WinApi.PtrSTR; p2: SHORTINT): SHORTINT;
	(*END GetFileTitle;*)

	PROCEDURE ChooseColorA* (VAR [nil] p0: CHOOSECOLORA): WinApi.BOOL;
	(*END ChooseColorA;*)

	PROCEDURE ChooseColorW* (VAR [nil] p0: CHOOSECOLORW): WinApi.BOOL;
	(*END ChooseColorW;*)

	PROCEDURE ChooseColor* ["ChooseColorA"] (VAR [nil] p0: CHOOSECOLORA): WinApi.BOOL;
	(*END ChooseColor;*)

	PROCEDURE FindTextA* (VAR [nil] p0: FINDREPLACEA): WinApi.HWND;
	(*END FindTextA;*)

	PROCEDURE FindTextW* (VAR [nil] p0: FINDREPLACEW): WinApi.HWND;
	(*END FindTextW;*)

	PROCEDURE FindText* ["FindTextA"] (VAR [nil] p0: FINDREPLACEA): WinApi.HWND;
	(*END FindText;*)

	PROCEDURE ReplaceTextA* (VAR [nil] p0: FINDREPLACEA): WinApi.HWND;
	(*END ReplaceTextA;*)

	PROCEDURE ReplaceTextW* (VAR [nil] p0: FINDREPLACEW): WinApi.HWND;
	(*END ReplaceTextW;*)

	PROCEDURE ReplaceText* ["ReplaceTextA"] (VAR [nil] p0: FINDREPLACEA): WinApi.HWND;
	(*END ReplaceText;*)

	PROCEDURE ChooseFontA* (VAR [nil] p0: CHOOSEFONTA): WinApi.BOOL;
	(*END ChooseFontA;*)

	PROCEDURE ChooseFontW* (VAR [nil] p0: CHOOSEFONTW): WinApi.BOOL;
	(*END ChooseFontW;*)

	PROCEDURE ChooseFont* ["ChooseFontA"] (VAR [nil] p0: CHOOSEFONTA): WinApi.BOOL;
	(*END ChooseFont;*)

	PROCEDURE PrintDlgA* (VAR [nil] p0: PRINTDLGA): WinApi.BOOL;
	(*END PrintDlgA;*)

	PROCEDURE PrintDlgW* (VAR [nil] p0: PRINTDLGW): WinApi.BOOL;
	(*END PrintDlgW;*)

	PROCEDURE PrintDlg* ["PrintDlgA"] (VAR [nil] p0: PRINTDLGA): WinApi.BOOL;
	(*END PrintDlg;*)

	PROCEDURE CommDlgExtendedError* (): INTEGER;
	(*END CommDlgExtendedError;*)

	PROCEDURE PageSetupDlgA* (VAR [nil] p0: PAGESETUPDLGA): WinApi.BOOL;
	(*END PageSetupDlgA;*)

	PROCEDURE PageSetupDlgW* (VAR [nil] p0: PAGESETUPDLGW): WinApi.BOOL;
	(*END PageSetupDlgW;*)

	PROCEDURE PageSetupDlg* ["PageSetupDlgA"] (VAR [nil] p0: PAGESETUPDLGA): WinApi.BOOL;
	(*END PageSetupDlg;*)

END WinDlg.
