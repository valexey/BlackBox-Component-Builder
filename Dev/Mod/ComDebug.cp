MODULE DevComDebug;
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
		SYSTEM, COM, WinApi, WinOle,
		Kernel, Strings,  Dialog, Fonts, Ports, Stores, Models, Views, Properties, Containers,
		Documents, Windows, Sequencers,
		TextModels, TextRulers, TextViews, TextControllers, TextMappers,
		DevDebug, DevHeapSpy, StdLog, StdLinks;
	
	
	TYPE
		Block = POINTER TO RECORD [untagged]
			tag: Kernel.Type;
			size: INTEGER;		(* size of free blocks *)
			ref: INTEGER;
			unk: INTEGER
		END;
		Cluster = POINTER TO RECORD [untagged]
			size: INTEGER;	(* total size *)
			next: Cluster;
		END;


	VAR
		all: BOOLEAN;
		headerLen: INTEGER;
		
	
	PROCEDURE NewRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR p: TextRulers.Prop;
	BEGIN
		NEW(p);
		p.valid := {TextRulers.right, TextRulers.tabs, TextRulers.opts};
		p.opts.val := {TextRulers.rightFixed}; p.opts.mask := p.opts.val;
		p.right := 130 * mm;
		p.tabs.len := 4;
		p.tabs.tab[0].stop := 15 * mm; p.tabs.tab[1].stop := 70 * mm;
		p.tabs.tab[2].stop := 85 * mm; p.tabs.tab[3].stop := 95 * mm;
		RETURN TextRulers.dir.NewFromProp(p)
	END NewRuler;

	PROCEDURE OpenViewer (t: TextModels.Model; title: Views.Title; ruler:TextRulers.Ruler);
		VAR v: TextViews.View; c: Containers.Controller;
	BEGIN
		Dialog.MapString(title, title);
		v := TextViews.dir.New(t);
		IF ruler # NIL THEN v.SetDefaults(ruler, TextViews.dir.defAttr) END;
		c := v.ThisController();
		IF c # NIL THEN
			c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
		END;
		Views.OpenAux(v, title)
	END OpenViewer;
	
	PROCEDURE OpenInfoViewer (t: TextModels.Model; title: Views.Title);
		VAR v: TextViews.View; c: Containers.Controller; p: Properties.BoundsPref;
	BEGIN
		Dialog.MapString(title, title);
		v := TextViews.dir.New(t);
		c := v.ThisController();
		IF c # NIL THEN
			c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
		END;
		p.w := Views.undefined; p.h := Views.undefined; Views.HandlePropMsg(v, p);
		Views.OpenAux(Documents.dir.New(v, p.w, p.h), title)
	END OpenInfoViewer;
	
	
	PROCEDURE Next (b: Block): Block;	(* next block in same cluster *)
		VAR size: INTEGER; tag: Kernel.Type;
	BEGIN
		tag := SYSTEM.VAL(Kernel.Type, SYSTEM.VAL(INTEGER, b.tag) DIV 4 * 4);
		size := tag.size + 4;
		IF ODD(SYSTEM.VAL(INTEGER, b.tag) DIV 2) THEN size := b.size - SYSTEM.ADR(b.size) + size END;
		size := (size + 15) DIV 16 * 16;
		RETURN SYSTEM.VAL(Block, SYSTEM.VAL(INTEGER, b) + size)
	END Next;

	PROCEDURE ShowInterfaces (showHeader: BOOLEAN; VAR out: TextMappers.Formatter);
		VAR adr, end: INTEGER; name: Kernel.Name; anchor: ARRAY 256 OF CHAR;
			a0: TextModels.Attributes; cluster: Cluster; blk: Block;
	BEGIN
		IF showHeader THEN
			out.WriteSString("Referenced Interface Records:");
			out.WriteTab; out.WriteTab;
			a0 := out.rider.attr;
			out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {Fonts.underline}));
			out.rider.SetAttr(TextModels.NewColor(out.rider.attr, Ports.blue));
			out.WriteView(StdLinks.dir.NewLink("DevComDebug.ToggleAllInterfaces"));
			IF all THEN out.WriteSString("New") ELSE out.WriteSString("All") END;
			out.WriteView(StdLinks.dir.NewLink(""));
			out.WriteTab;
			out.WriteView(StdLinks.dir.NewLink("DevComDebug.UpdateInterfaceRecords"));
			out.WriteSString("Update");
			out.WriteView(StdLinks.dir.NewLink(""));
			out.rider.SetAttr(a0);
			out.WriteLn; out.WriteLn;
			headerLen := out.Pos()
		END;
		cluster := SYSTEM.VAL(Cluster, Kernel.Root());
		WHILE cluster # NIL DO
			blk := SYSTEM.VAL(Block, SYSTEM.VAL(INTEGER, cluster) + 12);
			end := SYSTEM.VAL(INTEGER, blk) + (cluster.size - 12) DIV 16 * 16;
			WHILE SYSTEM.VAL(INTEGER, blk) < end DO
				IF ~(1 IN SYSTEM.VAL(SET, blk.tag))
					& (SYSTEM.VAL(INTEGER, blk.tag) # SYSTEM.ADR(blk.size))
					& (blk.tag.base[0] = NIL)
					& (all OR (blk.tag.mod.name # "HostMechanisms")
							& (blk.tag.mod.name # "OleServer")
							& (blk.tag.mod.name # "OleClient")
							& (blk.tag.mod.name # "OleStorage")
							& (blk.tag.mod.name # "OleData")) THEN
					adr := SYSTEM.ADR(blk.size);
					out.WriteSString("ref: ");
					out.WriteInt(blk.ref); out.WriteTab;
					out.WriteSString(blk.tag.mod.name); out.WriteChar(".");
					IF (blk.tag.id DIV 256 # 0) & (blk.tag.mod.refcnt >= 0) THEN
						Kernel.GetTypeName(blk.tag, name); out.WriteSString(name)
					ELSE
						out.WriteSString("RECORD"); 
					END;
					out.WriteTab;
					out.WriteChar("["); out.WriteIntForm(adr, TextMappers.hexadecimal, 9, "0", TextMappers.showBase);
					out.WriteChar("]"); out.WriteChar(" ");
					out.WriteView(DevDebug.HeapRefView(adr, "Interface"));
					DevHeapSpy.GetAnchor(adr, anchor);
					IF anchor # "" THEN
						out.WriteTab; out.WriteTab; out.WriteChar("(");
						out.WriteString(anchor); out.WriteChar(")")
					END;
					out.WriteLn
				END;
				blk := Next(blk)
			END;
			cluster := cluster.next
		END
	END ShowInterfaces;
	
	PROCEDURE ShowInterfaceRecords*;
		VAR out: TextMappers.Formatter;
	BEGIN
		out.ConnectTo(TextModels.CloneOf(StdLog.buf));
		ShowInterfaces(TRUE, out);
		OpenViewer(out.rider.Base(), "Interfaces", NewRuler());
		out.ConnectTo(NIL)
	END ShowInterfaceRecords;
	
	PROCEDURE UpdateInterfaceRecords*;
		VAR t, t0: TextModels.Model; out: TextMappers.Formatter;
	BEGIN
		t0 := TextViews.FocusText();
		Models.BeginModification(Models.notUndoable, t0);
		t0.Delete(headerLen, t0.Length());	(* removes old object references from text *)
		Views.Update(TextViews.Focus(), Views.rebuildFrames);
		Windows.dir.Update(Windows.dir.First());	(* remove frame references *)
		Kernel.Collect;
		t := TextModels.CloneOf(t0); (*Stores.InitDomain(t, t0.domain);*) Stores.Join(t, t0);
		out.ConnectTo(t);
		ShowInterfaces(FALSE, out);
		t0.Insert(headerLen, t, 0, t.Length());
		Models.EndModification(Models.notUndoable, t0);
		out.ConnectTo(NIL)
	END UpdateInterfaceRecords;
	
	PROCEDURE ToggleAllInterfaces*;
	BEGIN
		all := ~all;
		UpdateInterfaceRecords
	END ToggleAllInterfaces;
	
	PROCEDURE ShowError*;
		VAR res: INTEGER; c: TextControllers.Controller; r: TextModels.Reader; f: TextMappers.Formatter;
			beg, end, i: INTEGER; str: ARRAY 1024 OF CHAR; ch: CHAR; s: ARRAY 64 OF CHAR;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			r := c.text.NewReader(NIL);
			r.SetPos(beg); i := 0; r.ReadChar(ch);
			WHILE (beg + i < end) & (i < LEN(s) - 1) & (ch >= " ") DO s[i] := ch; INC(i); r.ReadChar(ch) END;
			s[i] := 0X;
			Strings.StringToInt(s, i, res);
			IF res = 0 THEN
				f.ConnectTo(TextModels.CloneOf(StdLog.buf));
				f.WriteSString("Error Code: ");
				f.WriteIntForm(i, TextMappers.hexadecimal, 9, "0", TRUE);
				f.WriteLn;
				f.WriteSString("(Facility: ");
				CASE i DIV 10000H MOD 2000H OF
				| 0: f.WriteSString("NULL, ")
				| 1: f.WriteSString("RPC, ")
				| 2: f.WriteSString("DISPATCH, ")
				| 3: f.WriteSString("STORAGE, ")
				| 4: f.WriteSString("ITF, ")
				| 7: f.WriteSString("WIN32, ")
				| 8: f.WriteSString("WINDOWS, ")
				| 10: f.WriteSString("CONTROL, ")
				ELSE f.WriteSString("unknown, ")
				END;
				f.WriteSString("Severity: ");
				IF i < 0 THEN f.WriteSString("Error, ") ELSE f.WriteSString("Success, ") END;
				f.WriteSString("Code: ");
				f.WriteInt(i MOD 10000H);
				f.WriteChar(")"); f.WriteLn;
				f.WriteSString("Description:");
				f.WriteLn;
				i := WinApi.FormatMessageW({12}, 0, i, 0, str, LEN(str), NIL);
				IF i > 0 THEN
					REPEAT DEC(i) UNTIL (i < 0) OR (str[i] >= " ");
					str[i + 1] := 0X;
				ELSE str := ""
				END;
				f.WriteString(str);
				f.WriteLn;
				OpenInfoViewer(f.rider.Base(), "Show Error");
				f.ConnectTo(NIL)
			END
		END
	END ShowError;
	
	PROCEDURE Hex (VAR f: TextMappers.Formatter; x, n: INTEGER);
	BEGIN
		IF n > 1 THEN Hex(f, x DIV 16, n - 1) END;
		x := x MOD 16;
		IF x >= 10 THEN f.WriteChar(CHR(x + ORD("A") - 10))
		ELSE f.WriteChar(CHR(x + ORD("0")))
		END
	END Hex;

	PROCEDURE NewGuid*;
		VAR f: TextMappers.Formatter; g: COM.GUID; res: COM.RESULT; n: INTEGER;
	BEGIN
		f.ConnectTo(TextModels.CloneOf(StdLog.buf)); n := 10;
		WHILE n > 0 DO
			res := WinOle.CoCreateGuid(g);
			f.WriteChar("{");
			Hex(f, g[2] MOD 256 + 256 * g[3] MOD 256, 4);
			Hex(f, g[0] MOD 256 + 256 * g[1] MOD 256, 4);
			f.WriteChar("-");
			Hex(f, g[4] MOD 256 + 256 * g[5] MOD 256, 4);
			f.WriteChar("-");
			Hex(f, g[6] MOD 256 + 256 * g[7] MOD 256, 4);
			f.WriteChar("-");
			Hex(f, g[8] MOD 256, 2);
			Hex(f, g[9] MOD 256, 2);
			f.WriteChar("-");
			Hex(f, g[10] MOD 256, 2);
			Hex(f, g[11] MOD 256, 2);
			Hex(f, g[12] MOD 256, 2);
			Hex(f, g[13] MOD 256, 2);
			Hex(f, g[14] MOD 256, 2);
			Hex(f, g[15] MOD 256, 2);
			f.WriteChar("}");
			f.WriteLn; DEC(n)
		END;
		OpenInfoViewer(f.rider.Base(), "Guids");
		f.ConnectTo(NIL)
	END NewGuid;

END DevComDebug.

		S_OK* = 0;
		S_FALSE* = 1;
		E_UNEXPECTED* = 8000FFFFH;
		E_NOTIMPL* = 80004001H;
		E_OUTOFMEMORY* = 8007000EH;
		E_INVALIDARG* = 80070057H;
		E_NOINTERFACE* = 80004002H;
		E_POINTER* = 80004003H;
		E_HANDLE* = 80070006H;
		E_ABORT* = 80004004H;
		E_FAIL* = 80004005H;
		E_ACCESSDENIED* = 80070005H;
		CO_E_INIT_TLS* = 80004006H;
		CO_E_INIT_SHARED_ALLOCATOR* = 80004007H;
		CO_E_INIT_MEMORY_ALLOCATOR* = 80004008H;
		CO_E_INIT_CLASS_CACHE* = 80004009H;
		CO_E_INIT_RPC_CHANNEL* = 8000400AH;
		CO_E_INIT_TLS_SET_CHANNEL_CONTROL* = 8000400BH;
		CO_E_INIT_TLS_CHANNEL_CONTROL* = 8000400CH;
		CO_E_INIT_UNACCEPTED_USER_ALLOCATOR* = 8000400DH;
		CO_E_INIT_SCM_MUTEX_EXISTS* = 8000400EH;
		CO_E_INIT_SCM_FILE_MAPPING_EXISTS* = 8000400FH;
		CO_E_INIT_SCM_MAP_VIEW_OF_FILE* = 80004010H;
		CO_E_INIT_SCM_EXEC_FAILURE* = 80004011H;
		CO_E_INIT_ONLY_SINGLE_THREADED* = 80004012H;
		OLE_E_OLEVERB* = 80040000H;
		OLE_E_ADVF* = 80040001H;
		OLE_E_ENUM_NOMORE* = 80040002H;
		OLE_E_ADVISENOTSUPPORTED* = 80040003H;
		OLE_E_NOCONNECTION* = 80040004H;
		OLE_E_NOTRUNNING* = 80040005H;
		OLE_E_NOCACHE* = 80040006H;
		OLE_E_BLANK* = 80040007H;
		OLE_E_CLASSDIFF* = 80040008H;
		OLE_E_CANT_GETMONIKER* = 80040009H;
		OLE_E_CANT_BINDTOSOURCE* = 8004000AH;
		OLE_E_STATIC* = 8004000BH;
		OLE_E_PROMPTSAVECANCELLED* = 8004000CH;
		OLE_E_INVALIDRECT* = 8004000DH;
		OLE_E_WRONGCOMPOBJ* = 8004000EH;
		OLE_E_INVALIDHWND* = 8004000FH;
		OLE_E_NOT_INPLACEACTIVE* = 80040010H;
		OLE_E_CANTCONVERT* = 80040011H;
		OLE_E_NOSTORAGE* = 80040012H;
		DV_E_FORMATETC* = 80040064H;
		DV_E_DVTARGETDEVICE* = 80040065H;
		DV_E_STGMEDIUM* = 80040066H;
		DV_E_STATDATA* = 80040067H;
		DV_E_LINDEX* = 80040068H;
		DV_E_TYMED* = 80040069H;
		DV_E_CLIPFORMAT* = 8004006AH;
		DV_E_DVASPECT* = 8004006BH;
		DV_E_DVTARGETDEVICE_SIZE* = 8004006CH;
		DV_E_NOIVIEWOBJECT* = 8004006DH;
		DRAGDROP_E_NOTREGISTERED* = 80040100H;
		DRAGDROP_E_ALREADYREGISTERED* = 80040101H;
		DRAGDROP_E_INVALIDHWND* = 80040102H;
		CLASS_E_NOAGGREGATION* = 80040110H;
		CLASS_E_CLASSNOTAVAILABLE* = 80040111H;
		VIEW_E_DRAW* = 80040140H;
		REGDB_E_READREGDB* = 80040150H;
		REGDB_E_WRITEREGDB* = 80040151H;
		REGDB_E_KEYMISSING* = 80040152H;
		REGDB_E_INVALIDVALUE* = 80040153H;
		REGDB_E_CLASSNOTREG* = 80040154H;
		REGDB_E_IIDNOTREG* = 80040155H;
		CACHE_E_NOCACHE_UPDATED* = 80040170H;
		OLEOBJ_E_NOVERBS* = 80040180H;
		OLEOBJ_E_INVALIDVERB* = 80040181H;
		INPLACE_E_NOTUNDOABLE* = 800401A0H;
		INPLACE_E_NOTOOLSPACE* = 800401A1H;
		CONVERT10_E_OLESTREAM_GET* = 800401C0H;
		CONVERT10_E_OLESTREAM_PUT* = 800401C1H;
		CONVERT10_E_OLESTREAM_FMT* = 800401C2H;
		CONVERT10_E_OLESTREAM_BITMAP_TO_DIB* = 800401C3H;
		CONVERT10_E_STG_FMT* = 800401C4H;
		CONVERT10_E_STG_NO_STD_STREAM* = 800401C5H;
		CONVERT10_E_STG_DIB_TO_BITMAP* = 800401C6H;
		CLIPBRD_E_CANT_OPEN* = 800401D0H;
		CLIPBRD_E_CANT_EMPTY* = 800401D1H;
		CLIPBRD_E_CANT_SET* = 800401D2H;
		CLIPBRD_E_BAD_DATA* = 800401D3H;
		CLIPBRD_E_CANT_CLOSE* = 800401D4H;
		MK_E_CONNECTMANUALLY* = 800401E0H;
		MK_E_EXCEEDEDDEADLINE* = 800401E1H;
		MK_E_NEEDGENERIC* = 800401E2H;
		MK_E_UNAVAILABLE* = 800401E3H;
		MK_E_SYNTAX* = 800401E4H;
		MK_E_NOOBJECT* = 800401E5H;
		MK_E_INVALIDEXTENSION* = 800401E6H;
		MK_E_INTERMEDIATEINTERFACENOTSUPPORTED* = 800401E7H;
		MK_E_NOTBINDABLE* = 800401E8H;
		MK_E_NOTBOUND* = 800401E9H;
		MK_E_CANTOPENFILE* = 800401EAH;
		MK_E_MUSTBOTHERUSER* = 800401EBH;
		MK_E_NOINVERSE* = 800401ECH;
		MK_E_NOSTORAGE* = 800401EDH;
		MK_E_NOPREFIX* = 800401EEH;
		MK_E_ENUMERATION_FAILED* = 800401EFH;
		CO_E_NOTINITIALIZED* = 800401F0H;
		CO_E_ALREADYINITIALIZED* = 800401F1H;
		CO_E_CANTDETERMINECLASS* = 800401F2H;
		CO_E_CLASSSTRING* = 800401F3H;
		CO_E_IIDSTRING* = 800401F4H;
		CO_E_APPNOTFOUND* = 800401F5H;
		CO_E_APPSINGLEUSE* = 800401F6H;
		CO_E_ERRORINAPP* = 800401F7H;
		CO_E_DLLNOTFOUND* = 800401F8H;
		CO_E_ERRORINDLL* = 800401F9H;
		CO_E_WRONGOSFORAPP* = 800401FAH;
		CO_E_OBJNOTREG* = 800401FBH;
		CO_E_OBJISREG* = 800401FCH;
		CO_E_OBJNOTCONNECTED* = 800401FDH;
		CO_E_APPDIDNTREG* = 800401FEH;
		CO_E_RELEASED* = 800401FFH;
		OLE_S_USEREG* = 00040000H;
		OLE_S_STATIC* = 00040001H;
		OLE_S_MAC_CLIPFORMAT* = 00040002H;
		DRAGDROP_S_DROP* = 00040100H;
		DRAGDROP_S_CANCEL* = 00040101H;
		DRAGDROP_S_USEDEFAULTCURSORS* = 00040102H;
		DATA_S_SAMEFORMATETC* = 00040130H;
		VIEW_S_ALREADY_FROZEN* = 00040140H;
		CACHE_S_FORMATETC_NOTSUPPORTED* = 00040170H;
		CACHE_S_SAMECACHE* = 00040171H;
		CACHE_S_SOMECACHES_NOTUPDATED* = 00040172H;
		OLEOBJ_S_INVALIDVERB* = 00040180H;
		OLEOBJ_S_CANNOT_DOVERB_NOW* = 00040181H;
		OLEOBJ_S_INVALIDHWND* = 00040182H;
		INPLACE_S_TRUNCATED* = 000401A0H;
		CONVERT10_S_NO_PRESENTATION* = 000401C0H;
		MK_S_REDUCED_TO_SELF* = 000401E2H;
		MK_S_ME* = 000401E4H;
		MK_S_HIM* = 000401E5H;
		MK_S_US* = 000401E6H;
		MK_S_MONIKERALREADYREGISTERED* = 000401E7H;
		CO_E_CLASS_CREATE_FAILED* = 80080001H;
		CO_E_SCM_ERROR* = 80080002H;
		CO_E_SCM_RPC_FAILURE* = 80080003H;
		CO_E_BAD_PATH* = 80080004H;
		CO_E_SERVER_EXEC_FAILURE* = 80080005H;
		CO_E_OBJSRV_RPC_FAILURE* = 80080006H;
		MK_E_NO_NORMALIZED* = 80080007H;
		CO_E_SERVER_STOPPING* = 80080008H;
		MEM_E_INVALID_ROOT* = 80080009H;
		MEM_E_INVALID_LINK* = 80080010H;
		MEM_E_INVALID_SIZE* = 80080011H;
		DISP_E_UNKNOWNINTERFACE* = 80020001H;
		DISP_E_MEMBERNOTFOUND* = 80020003H;
		DISP_E_PARAMNOTFOUND* = 80020004H;
		DISP_E_TYPEMISMATCH* = 80020005H;
		DISP_E_UNKNOWNNAME* = 80020006H;
		DISP_E_NONAMEDARGS* = 80020007H;
		DISP_E_BADVARTYPE* = 80020008H;
		DISP_E_EXCEPTION* = 80020009H;
		DISP_E_OVERFLOW* = 8002000AH;
		DISP_E_BADINDEX* = 8002000BH;
		DISP_E_UNKNOWNLCID* = 8002000CH;
		DISP_E_ARRAYISLOCKED* = 8002000DH;
		DISP_E_BADPARAMCOUNT* = 8002000EH;
		DISP_E_PARAMNOTOPTIONAL* = 8002000FH;
		DISP_E_BADCALLEE* = 80020010H;
		DISP_E_NOTACOLLECTION* = 80020011H;
		TYPE_E_BUFFERTOOSMALL* = 80028016H;
		TYPE_E_INVDATAREAD* = 80028018H;
		TYPE_E_UNSUPFORMAT* = 80028019H;
		TYPE_E_REGISTRYACCESS* = 8002801CH;
		TYPE_E_LIBNOTREGISTERED* = 8002801DH;
		TYPE_E_UNDEFINEDTYPE* = 80028027H;
		TYPE_E_QUALIFIEDNAMEDISALLOWED* = 80028028H;
		TYPE_E_INVALIDSTATE* = 80028029H;
		TYPE_E_WRONGTYPEKIND* = 8002802AH;
		TYPE_E_ELEMENTNOTFOUND* = 8002802BH;
		TYPE_E_AMBIGUOUSNAME* = 8002802CH;
		TYPE_E_NAMECONFLICT* = 8002802DH;
		TYPE_E_UNKNOWNLCID* = 8002802EH;
		TYPE_E_DLLFUNCTIONNOTFOUND* = 8002802FH;
		TYPE_E_BADMODULEKIND* = 800288BDH;
		TYPE_E_SIZETOOBIG* = 800288C5H;
		TYPE_E_DUPLICATEID* = 800288C6H;
		TYPE_E_INVALIDID* = 800288CFH;
		TYPE_E_TYPEMISMATCH* = 80028CA0H;
		TYPE_E_OUTOFBOUNDS* = 80028CA1H;
		TYPE_E_IOERROR* = 80028CA2H;
		TYPE_E_CANTCREATETMPFILE* = 80028CA3H;
		TYPE_E_CANTLOADLIBRARY* = 80029C4AH;
		TYPE_E_INCONSISTENTPROPFUNCS* = 80029C83H;
		TYPE_E_CIRCULARTYPE* = 80029C84H;
		STG_E_INVALIDFUNCTION* = 80030001H;
		STG_E_FILENOTFOUND* = 80030002H;
		STG_E_PATHNOTFOUND* = 80030003H;
		STG_E_TOOMANYOPENFILES* = 80030004H;
		STG_E_ACCESSDENIED* = 80030005H;
		STG_E_INVALIDHANDLE* = 80030006H;
		STG_E_INSUFFICIENTMEMORY* = 80030008H;
		STG_E_INVALIDPOINTER* = 80030009H;
		STG_E_NOMOREFILES* = 80030012H;
		STG_E_DISKISWRITEPROTECTED* = 80030013H;
		STG_E_SEEKERROR* = 80030019H;
		STG_E_WRITEFAULT* = 8003001DH;
		STG_E_READFAULT* = 8003001EH;
		STG_E_SHAREVIOLATION* = 80030020H;
		STG_E_LOCKVIOLATION* = 80030021H;
		STG_E_FILEALREADYEXISTS* = 80030050H;
		STG_E_INVALIDPARAMETER* = 80030057H;
		STG_E_MEDIUMFULL* = 80030070H;
		STG_E_ABNORMALAPIEXIT* = 800300FAH;
		STG_E_INVALIDHEADER* = 800300FBH;
		STG_E_INVALIDNAME* = 800300FCH;
		STG_E_UNKNOWN* = 800300FDH;
		STG_E_UNIMPLEMENTEDFUNCTION* = 800300FEH;
		STG_E_INVALIDFLAG* = 800300FFH;
		STG_E_INUSE* = 80030100H;
		STG_E_NOTCURRENT* = 80030101H;
		STG_E_REVERTED* = 80030102H;
		STG_E_CANTSAVE* = 80030103H;
		STG_E_OLDFORMAT* = 80030104H;
		STG_E_OLDDLL* = 80030105H;
		STG_E_SHAREREQUIRED* = 80030106H;
		STG_E_NOTFILEBASEDSTORAGE* = 80030107H;
		STG_E_EXTANTMARSHALLINGS* = 80030108H;
		STG_S_CONVERTED* = 00030200H;
		RPC_E_CALL_REJECTED* = 80010001H;
		RPC_E_CALL_CANCELED* = 80010002H;
		RPC_E_CANTPOST_INSENDCALL* = 80010003H;
		RPC_E_CANTCALLOUT_INASYNCCALL* = 80010004H;
		RPC_E_CANTCALLOUT_INEXTERNALCALL* = 80010005H;
		RPC_E_CONNECTION_TERMINATED* = 80010006H;
		RPC_E_SERVER_DIED* = 80010007H;
		RPC_E_CLIENT_DIED* = 80010008H;
		RPC_E_INVALID_DATAPACKET* = 80010009H;
		RPC_E_CANTTRANSMIT_CALL* = 8001000AH;
		RPC_E_CLIENT_CANTMARSHAL_DATA* = 8001000BH;
		RPC_E_CLIENT_CANTUNMARSHAL_DATA* = 8001000CH;
		RPC_E_SERVER_CANTMARSHAL_DATA* = 8001000DH;
		RPC_E_SERVER_CANTUNMARSHAL_DATA* = 8001000EH;
		RPC_E_INVALID_DATA* = 8001000FH;
		RPC_E_INVALID_PARAMETER* = 80010010H;
		RPC_E_CANTCALLOUT_AGAIN* = 80010011H;
		RPC_E_SERVER_DIED_DNE* = 80010012H;
		RPC_E_SYS_CALL_FAILED* = 80010100H;
		RPC_E_OUT_OF_RESOURCES* = 80010101H;
		RPC_E_ATTEMPTED_MULTITHREAD* = 80010102H;
		RPC_E_NOT_REGISTERED* = 80010103H;
		RPC_E_FAULT* = 80010104H;
		RPC_E_SERVERFAULT* = 80010105H;
		RPC_E_CHANGED_MODE* = 80010106H;
		RPC_E_INVALIDMETHOD* = 80010107H;
		RPC_E_DISCONNECTED* = 80010108H;
		RPC_E_RETRY* = 80010109H;
		RPC_E_SERVERCALL_RETRYLATER* = 8001010AH;
		RPC_E_SERVERCALL_REJECTED* = 8001010BH;
		RPC_E_INVALID_CALLDATA* = 8001010CH;
		RPC_E_CANTCALLOUT_ININPUTSYNCCALL* = 8001010DH;
		RPC_E_WRONG_THREAD* = 8001010EH;
		RPC_E_THREAD_NOT_INIT* = 8001010FH;
		RPC_E_UNEXPECTED* = 8001FFFFH;
