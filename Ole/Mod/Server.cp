MODULE OleServer;
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
		SYSTEM, COM, WinOle, WinApi,
		OleStorage, OleData, StdDialog,
		Kernel, Files, Services, Ports, Dialog, Stores, Sequencers, Views,
		Controllers, Properties, Converters, Containers, Documents, Windows,
		TextViews, Log, HostPorts, HostDialog, HostWindows, HostMenus;
	
	CONST
		debug = FALSE;
		ObjectID = "{00000001-1000-11cf-adf0-444553540000}";
		streamStr = "CONTENTS";
		cbFormat = 200H;
		obfTag = 6F4F4443H;
		miscStatus = WinOle.OLEMISC_RECOMPOSEONRESIZE
			+ WinOle.OLEMISC_CANTLINKINSIDE
			+ WinOle.OLEMISC_RENDERINGISDEVICEINDEPENDENT;
		oleUnit = Ports.mm DIV 100;
		borderW = 5 * Ports.point;
		fixed = 31;	(* controller option *)
	
	TYPE
		IClassFactory = POINTER TO RECORD (WinOle.IClassFactory) END;
		
		Object = POINTER TO RECORD (COM.IUnknown)
			ioo: IOleObject;
			ido: IDataObject;
			ips: IPersistStorage;
			iipo: IOleInPlaceObject;
			iipao: IOleInPlaceActiveObject;
			ics: WinOle.IOleClientSite;
			iips: WinOle.IOleInPlaceSite;	(* # NIL => in place open *)
			iipf: WinOle.IOleInPlaceFrame;
			iipw: WinOle.IOleInPlaceUIWindow;
			fInfo: WinOle.OLEINPLACEFRAMEINFO;
			idah: WinOle.IDataAdviseHolder;
			ioah: WinOle.IOleAdviseHolder;
			isg: WinOle.IStorage;
			ism: WinOle.IStream;
			rsm: WinOle.IStream;
			menu: WinApi.HMENU;
			oleMenu: WinOle.HOLEMENU;
			menuType: Stores.TypeName;
			w, h: INTEGER;
			view: Views.View;
			seq: Sequencers.Sequencer;
			win: HostWindows.Window;	(* # NIL => open *)
			embedded: BOOLEAN;
			update: BOOLEAN;
			uiActive: BOOLEAN;
			useMenu: BOOLEAN;
			unit: INTEGER	(* scaling when in place open *)
		END;
		IOleObject = POINTER TO RECORD (WinOle.IOleObject)
			obj: Object
		END;
		IDataObject = POINTER TO RECORD (WinOle.IDataObject)
			obj: Object;
			data: OleData.IDataObject
		END;
		IPersistStorage = POINTER TO EXTENSIBLE RECORD (WinOle.IPersistStorage)
			obj: Object
		END;
		IOleInPlaceObject = POINTER TO RECORD (WinOle.IOleInPlaceObject);
			obj: Object
		END;
		IOleInPlaceActiveObject = POINTER TO RECORD (WinOle.IOleInPlaceActiveObject)
			obj: Object
		END;
		
		Window = POINTER TO RECORD (HostWindows.Window)
			obj: Object
		END;
		
		WinDir = POINTER TO RECORD (HostWindows.Directory)
			obj: Object;
			host: WinApi.HWND;
			pos, clip: WinApi.RECT;
		END;
		
		Action = POINTER TO RECORD (Services.Action)
			w: Window
		END;
		
		Verb = POINTER TO RECORD
			verb: INTEGER;
			name: ARRAY 64 OF CHAR;
			disabled, checked: BOOLEAN;
			next: Verb
		END;
		
		IEnumOLEVERB = POINTER TO RECORD (WinOle.IEnumOLEVERB)
			first, cur: Verb
		END;
		
		
	VAR
		factory: IClassFactory;
		token: INTEGER;
		winDir: WinDir;
		
		
	(* ---------- auxiliary ---------- *)
	
	PROCEDURE Max (x, y: INTEGER): INTEGER;
	BEGIN
		IF x > y THEN RETURN x ELSE RETURN y END
	END Max;
	
	PROCEDURE Min (x, y: INTEGER): INTEGER;
	BEGIN
		IF x < y THEN RETURN x ELSE RETURN y END
	END Min;
	
	PROCEDURE GenGlobalMedium (hg: WinApi.HGLOBAL; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		sm.tymed := WinOle.TYMED_HGLOBAL;
		sm.u.hGlobal := hg;
		sm.pUnkForRelease := unk
	END GenGlobalMedium;
	
	PROCEDURE MediumGlobal (VAR sm: WinOle.STGMEDIUM): WinApi.HGLOBAL;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_HGLOBAL, 20);
		RETURN sm.u.hGlobal
	END MediumGlobal;
	
	PROCEDURE GenStorageMedium (stg: WinOle.IStorage; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
		TYPE PS = POINTER TO RECORD t: SET; s: WinOle.IStorage; u: COM.IUnknown END;
		VAR ps: PS;
	BEGIN
		sm.u.hGlobal := 0;
		sm.tymed := WinOle.TYMED_ISTORAGE;
		sm.u.pstg := stg;
		sm.pUnkForRelease := unk
	END GenStorageMedium;
	
	PROCEDURE MediumStorage (VAR sm: WinOle.STGMEDIUM): WinOle.IStorage;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_ISTORAGE, 20);
		RETURN sm.u.pstg
	END MediumStorage;
	
	PROCEDURE NewString (VAR str: ARRAY OF CHAR): WinApi.PtrWSTR;
		VAR n: INTEGER; p: WinApi.PtrWSTR;
	BEGIN
		n := 0; WHILE str[n] # 0X DO INC(n) END;
		p := SYSTEM.VAL(WinApi.PtrWSTR, WinOle.CoTaskMemAlloc(SIZE(CHAR) * (n + 1)));
		p^ := str$;
		RETURN p
	END NewString;
	
	PROCEDURE CheckVerb (v: Views.View; n: INTEGER; VAR pvm: Properties.PollVerbMsg);
	BEGIN
		pvm.verb := n;
		pvm.label := "";
		pvm.disabled := FALSE; pvm.checked := FALSE;
		IF v # NIL THEN Views.HandlePropMsg(v, pvm) END
	END CheckVerb;
	

	(* ---------- IClassFactory ---------- *)
	
	PROCEDURE (this: IClassFactory) CreateInstance (outer: COM.IUnknown; IN [iid] iid: COM.GUID;
																				OUT [new] int: COM.IUnknown): COM.RESULT;
		VAR res: COM.RESULT; new: Object;
	BEGIN
		IF debug THEN Log.String("create instance"); Log.Ln END;
		IF outer = NIL THEN
			NEW(new);
			IF new # NIL THEN
				NEW(new.ioo, new); NEW(new.ido, new); NEW(new.ips, new); NEW(new.iipo, new);
				NEW(new.iipao);	(* separate component *)
				IF (new.ioo # NIL) & (new.ido # NIL) & (new.ips # NIL) & (new.iipo # NIL) & (new.iipao # NIL) THEN
					new.ioo.obj := new;
					new.ido.obj := new;
					new.ido.data := OleData.ViewData(NIL, 0, 0, TRUE);
					new.ips.obj := new;
					new.iipo.obj := new;
					new.iipao.obj := new;
					new.embedded := FALSE;
					res := new.QueryInterface(iid, int);
					IF res >= 0 THEN HostMenus.Lock END;
					IF debug THEN Log.String("c lock "); Log.Int(HostMenus.locks); Log.Ln END;
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
		IF lock # 0 THEN HostMenus.Lock ELSE HostMenus.Unlock END;
		IF debug THEN Log.String("lock server "); Log.Int(lock); Log.Int(HostMenus.locks); Log.Ln END;
		RETURN WinApi.S_OK
	END LockServer;
	
	
	(* IEnumOLEVERB  *)
	
	PROCEDURE (this: IEnumOLEVERB) Next (num: INTEGER; OUT elem: ARRAY [untagged] OF WinOle.OLEVERB;
																								OUT [nil] fetched: INTEGER): COM.RESULT;
		VAR n: INTEGER; flags: SET;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.E_POINTER
		END;
		IF this.cur # NIL THEN
			WHILE (this.cur # NIL) & (num > 0) DO
				elem[n].lVerb := this.cur.verb;
				elem[n].lpszVerbName := NewString(this.cur.name);
				flags := {};
				IF this.cur.disabled THEN INCL(flags, 0) END;
				IF this.cur.checked THEN INCL(flags, 3) END;
				elem[n].fuFlags := flags;
				IF this.cur.verb >= 0 THEN
					elem[n].grfAttribs := WinOle.OLEVERBATTRIB_ONCONTAINERMENU
				ELSE
					elem[n].grfAttribs := {}
				END;
				this.cur := this.cur.next; INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: IEnumOLEVERB) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		WHILE (num > 0) & (this.cur # NIL) DO this.cur := this.cur.next; DEC(num) END;
		IF this.cur # NIL THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: IEnumOLEVERB) Reset (): COM.RESULT;
	BEGIN
		this.cur := this.first;
		RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: IEnumOLEVERB) Clone (OUT enum: WinOle.IEnumOLEVERB): COM.RESULT;
		VAR new: IEnumOLEVERB;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.first := this.first;
			new.cur := this.cur;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
(*	
	(* ---------- Object Release Patch ---------- *)	(* VERY BIG HACK !!!!!!!!! *)
	
	PROCEDURE^ (this: Object) InPlaceDeactivate ();
	
	PROCEDURE ObjectRelease (this: Object): LONGINT;
		VAR n: LONGINT; msg: Sequencers.RemoveMsg;
	BEGIN
		n := Kernel.Release(SYSTEM.VAL(LONGINT, this));
		IF debug THEN Log.String("release "); Log.Int(n); Log.Ln END;
		IF n = 0 THEN
			IF this.iips # NIL THEN this.InPlaceDeactivate END;
			this.ics := NIL; this.idah := NIL; this.ioah := NIL;
			IF this.seq # NIL THEN this.seq.Notify(msg) END;
			HostMenus.Unlock;
			IF debug THEN Log.String("r unlock "); Log.Int(HostMenus.locks); Log.Ln END;
			Kernel.Cleanup
		END;
		RETURN n
	END ObjectRelease;
	
	PROCEDURE PatchObjectRelease;
		CONST releaseMethOff = -4;
	BEGIN
		SYSTEM.PUT(SYSTEM.ADR(Object) + releaseMethOff, SYSTEM.ADR(ObjectRelease))
	END PatchObjectRelease;
*)	
	
	(* ---------- Object ---------- *)
	
	PROCEDURE^ (this: Object) InPlaceDeactivate (), NEW;
	
	PROCEDURE (this: Object) RELEASE;
		VAR msg: Sequencers.RemoveMsg;
	BEGIN
		IF this.iips # NIL THEN this.InPlaceDeactivate END;
		this.ics := NIL; this.idah := NIL; this.ioah := NIL;
		IF this.seq # NIL THEN this.seq.Notify(msg) END;
		HostMenus.Unlock;
		IF debug THEN Log.String("r unlock "); Log.Int(HostMenus.locks); Log.Ln END;
		Kernel.Cleanup
	END RELEASE;
	
	PROCEDURE (this: Object) QueryInterface (IN iid: COM.GUID; OUT int: COM.IUnknown): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("query interface"); Log.Ln END;
		IF COM.QUERY(this, iid, int)
			OR COM.QUERY(this.ioo, iid, int)
			OR COM.QUERY(this.ido, iid, int)
			OR COM.QUERY(this.ips, iid, int)
			OR COM.QUERY(this.iipo, iid, int) THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_NOINTERFACE
		END
	END QueryInterface;
	
	PROCEDURE (this: Object) GetMenuType (VAR changed: BOOLEAN), NEW;
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		ops.type := ""; this.win.ForwardCtrlMsg(ops);
		IF ops.type # this.menuType THEN
			changed := TRUE;
			this.menuType := ops.type$
		ELSE
			changed := FALSE
		END
	END GetMenuType;
	
	PROCEDURE (this: Object) InPlaceMenuCreate (), NEW;
		VAR menu: WinApi.HMENU; res, i, p, n: INTEGER; widths: WinOle.OLEMENUGROUPWIDTHS;
			m: HostMenus.Menu;
	BEGIN
		IF debug THEN Log.String("create menu"); Log.Ln END;
		i := 0; WHILE i < 6 DO widths.width[i] := 0; INC(i) END;
		menu := WinApi.CreateMenu();
		res := this.iipf.InsertMenus(menu, widths);
		m := HostMenus.menus; p := 0; i := 0;
		WHILE p < 6 DO
			WHILE (m # NIL) & (m.class <= p) DO m := m.next END;
			INC(i, widths.width[p]); INC(p); n := 0;
			WHILE (m # NIL) & (m.class = p) DO
				IF (m.type = "") OR (m.type = this.menuType) THEN
					res := WinApi.InsertMenuW(menu, i, WinApi.MF_POPUP + WinApi.MF_BYPOSITION,
															m.menuH, m.menu);
					INC(n); INC(i)
				END;
				m := m.next;
			END;
			widths.width[p] := n; INC(p)
		END;
		this.menu := menu;
		this.oleMenu := WinOle.OleCreateMenuDescriptor(menu, widths);
		IF debug THEN Log.String("menu created"); Log.Ln END;
	END InPlaceMenuCreate;
	
	PROCEDURE (this: Object) InPlaceMenuDestroy (), NEW;
		VAR res: COM.RESULT; i: INTEGER; m: HostMenus.Menu; sm: WinApi.HMENU;
	BEGIN
		IF debug THEN Log.String("destroy menu"); Log.Ln END;
		res := WinOle.OleDestroyMenuDescriptor(this.oleMenu);
		this.oleMenu := 0;
		i := WinApi.GetMenuItemCount(this.menu);
		WHILE i > 0 DO
			DEC(i); sm := WinApi.GetSubMenu(this.menu, i);
			m := HostMenus.menus;
			WHILE (m # NIL) & (m.menuH # sm) DO m := m.next END;
			IF m # NIL THEN res := WinApi.RemoveMenu(this.menu, i, WinApi.MF_BYPOSITION) END
		END;
		IF this.iipf # NIL THEN res := this.iipf.RemoveMenus(this.menu) END;
		res := WinApi.DestroyMenu(this.menu);
		this.menu := 0;
		IF debug THEN Log.String("menu destroyed"); Log.Ln END;
	END InPlaceMenuDestroy;
	
	PROCEDURE (this: Object) UIActivate (): COM.RESULT, NEW;
		VAR res: COM.RESULT; dumy: BOOLEAN; rect: WinApi.RECT;
	BEGIN
		IF debug THEN Log.String("ui activate"); Log.Ln END;
		IF this.iips # NIL THEN res := this.iips.OnUIActivate() END;
		res := WinApi.SetFocus(this.win.wnd);
		HostWindows.ActivateWindow(this.win, TRUE);
		IF this.iipf # NIL THEN res := this.iipf.SetActiveObject(this.iipao, "") END;
		IF this.iipw # NIL THEN res := this.iipw.SetActiveObject(this.iipao, "") END;
		this.GetMenuType(dumy);
		this.InPlaceMenuCreate;
		res := this.iipf.SetMenu(this.menu, this.oleMenu, HostWindows.main);
		this.useMenu := TRUE;
		HostMenus.isObj := TRUE;
		rect.left := 0; rect.top := 0; rect.right := 0; rect.bottom := 0;
		IF this.iipf # NIL THEN res := this.iipf.SetBorderSpace(rect) END;
		IF this.iipw # NIL THEN res := this.iipw.SetBorderSpace(rect) END;
		this.uiActive := TRUE;
		IF debug THEN Log.String("ui active"); Log.Ln END;
		RETURN WinApi.S_OK
	END UIActivate;
	
	PROCEDURE (this: Object) UIDeactivate (), NEW;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ui deactivate"); Log.Ln END;
		IF this.iipf # NIL THEN res := this.iipf.SetMenu(0, 0, 0) END;
		this.useMenu := FALSE;
		this.InPlaceMenuDestroy;
		IF this.win # NIL THEN HostWindows.ActivateWindow(this.win, FALSE) END;
		IF this.iipf # NIL THEN res := this.iipf.SetActiveObject(NIL, NIL) END;
		IF this.iipw # NIL THEN res := this.iipw.SetActiveObject(NIL, NIL) END;
		IF this.iips # NIL THEN res := this.iips.OnUIDeactivate(0) END;
		HostMenus.isObj := FALSE;
		this.uiActive := FALSE;
		IF debug THEN Log.String("ui deactive"); Log.Ln END;
	END UIDeactivate;
	
	PROCEDURE (this: Object) InPlaceActivate (site: WinOle.IOleClientSite; ui: BOOLEAN): COM.RESULT, NEW;
		VAR res: COM.RESULT; host: WinApi.HWND;
			d: Documents.Document; c: Containers.Controller; w: Windows.Window;
	BEGIN
		IF site # NIL THEN
			IF this.iips = NIL THEN
				IF debug THEN Log.String("try in place"); Log.Ln END;
				res := site.QueryInterface(COM.ID(this.iips), this.iips);
				IF res < 0 THEN RETURN res END;
				res := this.iips.CanInPlaceActivate();
				IF res = WinApi.S_OK THEN
					IF debug THEN Log.String("in place activate"); Log.Ln END;
					res := this.iips.OnInPlaceActivate();
					(* undodeactivates := TRUE *)
					res := this.iips.GetWindow(host);
					this.fInfo.cb := SIZE(WinOle.OLEINPLACEFRAMEINFO);
					res := this.iips.GetWindowContext(this.iipf, this.iipw, winDir.pos, winDir.clip, this.fInfo);
					(* open window *)
					this.view := Views.CopyOf(this.view, Views.shallow);
					winDir.obj := this; winDir.host := host;
					Windows.SetDir(winDir);
					d := Documents.dir.New(this.view, this.w, this.h);
					c := d.ThisController();
					c.SetOpts(c.opts - {Documents.pageWidth..Documents.winHeight} + {31});
					StdDialog.Open(d, "", NIL, "", NIL, TRUE, FALSE, FALSE, FALSE, TRUE);
					w := Windows.dir.First();
					this.seq := w.seq;
					this.win := w(Window);
					OleData.SetView(this.ido.data, this.view, this.w, this.h);
					Windows.SetDir(Windows.stdDir);
					IF this.ics # NIL THEN res := this.ics.ShowObject() END;
					IF debug THEN Log.String("in place active"); Log.Ln END;
				ELSE
					this.iips := NIL;
					RETURN WinApi.E_FAIL
				END
			END;
			IF ui THEN res := this.UIActivate() END;
			IF this.iips # NIL THEN	(* minimal undo support *)
				res := this.iips.DiscardUndoState()
			END
		ELSE RETURN WinApi.E_INVALIDARG
		END;
		RETURN WinApi.S_OK
	END InPlaceActivate;
	
	PROCEDURE (this: Object) CheckViewUpdate (), NEW;
		VAR res: COM.RESULT; w, h: INTEGER; v: Views.View; changed: BOOLEAN;
	BEGIN
		IF (this.win # NIL) & (this.win.doc # NIL) THEN
			v := this.win.doc.ThisView();
			v.context.GetSize(w, h);
			IF (v # this.view) OR (w # this.w) OR (h # this.h) THEN
				this.view := v; this.w := w; this.h := h;
				OleData.SetView(this.ido.data, v, w, h);
				this.update := TRUE
			END
		END;
		IF this.update & (this.idah # NIL) & (this.iips = NIL) THEN
			IF debug THEN Log.String("on data change"); Log.Ln END;
			res := this.idah.SendOnDataChange(this.ido, 0, {});
			this.update := FALSE
		END;
		(* check menus *)
		IF (this.win # NIL) & (this.iipf # NIL) & (this.menu # 0) & (Windows.dir.Focus(Controllers.targetPath) = this.win)
		THEN
			this.GetMenuType(changed);
			IF changed THEN
				this.InPlaceMenuDestroy(); this.InPlaceMenuCreate;
				IF this.useMenu THEN
					res := this.iipf.SetMenu(this.menu, this.oleMenu, HostWindows.main)
				END
			END
		END
	END CheckViewUpdate;
	
	PROCEDURE (this: Object) InPlaceDeactivate (), NEW;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("in place deactivate"); Log.Ln END;
		this.UIDeactivate;
		IF this.win # NIL THEN this.win.Close(); this.win := NIL END;
		IF this.iips # NIL THEN res := this.iips.OnInPlaceDeactivate() END;
		this.iips := NIL; this.iipf := NIL; this.iipw := NIL;
		this.CheckViewUpdate;
		Kernel.Cleanup;
		IF debug THEN Log.String("in place deactive"); Log.Ln END;
	END InPlaceDeactivate;
	
	PROCEDURE (this: Object) Open (), NEW;
		VAR res: COM.RESULT; v: Views.View; d: Documents.Document; c: Containers.Controller; w: Windows.Window;
	BEGIN
		IF this.win = NIL THEN
			IF this.view = NIL THEN
				IF debug THEN Log.String("show new"); Log.Ln END;
				v := TextViews.dir.New(NIL)
			ELSE
				IF debug THEN Log.String("show old"); Log.Ln END;
				v := Views.CopyOf(this.view, Views.shallow);
			END;
			winDir.obj := this;
			Windows.SetDir(winDir);
			d := Documents.dir.New(v, this.w, this.h);
			c := d.ThisController();
			c.SetOpts(c.opts - {Documents.pageWidth..Documents.winHeight});
			Views.OpenAux(d, "BlackBox Object");
			w := Windows.dir.First();
			this.view := v;
			this.seq := w.seq;
			this.win := w(Window);
			OleData.SetView(this.ido.data, this.view, this.w, this.h);
			Windows.SetDir(Windows.stdDir)
		END;
		Windows.dir.Select(this.win, Windows.lazy);
		IF this.ics # NIL THEN res := this.ics.ShowObject() END;
		IF this.ics # NIL THEN res := this.ics.OnShowWindow(1) END;
	END Open;
	
	PROCEDURE (this: Object) CustomVerb (
		verb: INTEGER; IN msg: WinApi.MSG; activeSite: WinOle.IOleClientSite;
		index: INTEGER; parent: WinApi.HWND; IN posRect: WinApi.RECT
	): COM.RESULT, NEW;
		VAR res: COM.RESULT; pvm: Properties.PollVerbMsg; dvm: Properties.DoVerbMsg;
	BEGIN
		CheckVerb(this.view, 0, pvm);
		IF pvm.label = "" THEN
			IF verb = 0 THEN
				RETURN this.ioo.DoVerb(WinOle.OLEIVERB_SHOW, msg, activeSite, index, parent, posRect)
			ELSIF verb = 1 THEN
				RETURN this.ioo.DoVerb(WinOle.OLEIVERB_OPEN, msg, activeSite, index, parent, posRect)
			ELSE
				DEC(verb)
			END
		END;
		CheckVerb(this.view, verb, pvm);
		IF pvm.label = "" THEN
			res := this.ioo.DoVerb(0, msg, activeSite, index, parent, posRect);
			RETURN WinApi.OLEOBJ_S_INVALIDVERB
		END;
		dvm.frame := NIL;
		dvm.verb := verb;
		Views.HandlePropMsg(this.view, dvm);
		RETURN WinApi.S_OK
	END CustomVerb;
	
	
	(* ---------- IOleObject ---------- *)
	
	PROCEDURE (this: IOleObject) SetClientSite (site: WinOle.IOleClientSite): COM.RESULT;
	BEGIN
		this.obj.ics := site;
		IF site = NIL THEN Kernel.Cleanup END;
		RETURN WinApi.S_OK
	END SetClientSite;
	
	PROCEDURE (this: IOleObject) GetClientSite (OUT site: WinOle.IOleClientSite): COM.RESULT;
	BEGIN
		site := this.obj.ics;
		RETURN WinApi.S_OK
	END GetClientSite;
	
	PROCEDURE (this: IOleObject) SetHostNames (app, obj: WinApi.PtrWSTR): COM.RESULT;
	BEGIN
		this.obj.embedded := TRUE;
		RETURN WinApi.S_OK
	END SetHostNames;
	
	PROCEDURE (this: IOleObject) Close (saveOption: INTEGER): COM.RESULT;
		VAR res: COM.RESULT; dirty: BOOLEAN; r: INTEGER; q: BOOLEAN;
	BEGIN
		IF (this.obj.view = NIL) OR (this.obj.seq # NIL) & this.obj.seq.Dirty() THEN
			IF saveOption = WinOle.OLECLOSE_SAVEIFDIRTY THEN
				IF this.obj.ics # NIL THEN res := this.obj.ics.SaveObject() END;
				IF this.obj.ioah # NIL THEN res := this.obj.ioah.SendOnSave() END
			ELSIF saveOption = WinOle.OLECLOSE_PROMPTSAVE THEN
				IF this.obj.win # NIL THEN HostDialog.CloseDialog(this.obj.win, q, r)
				ELSE r := HostDialog.save
				END;
				IF r = HostDialog.save THEN
					IF this.obj.ics # NIL THEN res := this.obj.ics.SaveObject() END;
					IF this.obj.ioah # NIL THEN res := this.obj.ioah.SendOnSave() END
				ELSIF r = HostDialog.cancel THEN
					RETURN WinApi.OLE_E_PROMPTSAVECANCELLED
				END
			END
		END;
		IF this.obj.win # NIL THEN Windows.dir.Close(this.obj.win) END;
		Kernel.Cleanup;
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
	
	PROCEDURE (this: IOleObject) GetClipboardData (reserved: INTEGER; OUT  obj: WinOle.IDataObject
	): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END GetClipboardData;
	
	PROCEDURE (this: IOleObject) DoVerb (
		verb: INTEGER; IN msg: WinApi.MSG; activeSite: WinOle.IOleClientSite;
		index: INTEGER; parent: WinApi.HWND; IN posRect: WinApi.RECT
	): COM.RESULT;
		VAR res: COM.RESULT; v: Views.View; d: Documents.Document; pvm: Properties.PollVerbMsg;
			c: Containers.Controller; w: Windows.Window; vw, vh: INTEGER; dvm: Properties.DoVerbMsg;
	BEGIN
		CASE verb OF
		| WinOle.OLEIVERB_HIDE:
			IF this.obj.iips # NIL THEN
				this.obj.InPlaceDeactivate()
			ELSE
				IF this.obj.win # NIL THEN Windows.dir.Close(this.obj.win) END;
				IF this.obj.ics # NIL THEN res := this.obj.ics.OnShowWindow(0) END;
				Kernel.Cleanup
			END
		| WinOle.OLEIVERB_PRIMARY, WinOle.OLEIVERB_SHOW:
			IF this.obj.win = NIL THEN
				res := this.obj.InPlaceActivate(activeSite, TRUE);
				IF res # WinApi.S_OK THEN this.obj.Open END
			END
		| WinOle.OLEIVERB_OPEN:
			IF this.obj.iips # NIL THEN this.obj.InPlaceDeactivate END;
			this.obj.Open
		| WinOle.OLEIVERB_INPLACEACTIVATE:
			RETURN this.obj.InPlaceActivate(activeSite, FALSE)
		| WinOle.OLEIVERB_UIACTIVATE:
			RETURN this.obj.InPlaceActivate(activeSite, TRUE)
		| WinOle.OLEIVERB_DISCARDUNDOSTATE:
			(* discard undo *)
		ELSE
			IF verb >= 0 THEN RETURN this.obj.CustomVerb(verb, msg, activeSite, index, parent, posRect)
			ELSE RETURN WinApi.E_NOTIMPL
			END
		END;
		RETURN WinApi.S_OK
	END DoVerb;
	
	PROCEDURE (this: IOleObject) EnumVerbs (OUT enum: WinOle.IEnumOLEVERB): COM.RESULT;
		VAR e: IEnumOLEVERB; v, last: Verb; pvm: Properties.PollVerbMsg; i, j: INTEGER;
	BEGIN
		NEW(e);
		IF e # NIL THEN
			NEW(v); v.verb := -3; v.name := "Hide"; e.first := v; last := v;
			NEW(v); v.verb := -2; v.name := "Open"; last.next := v; last := v;
			NEW(v); v.verb := -1; v.name := "Show"; last.next := v; last := v;
			i := 0; j := 0;
			CheckVerb(this.obj.view, 0, pvm);
			IF pvm.label = "" THEN
				NEW(v); v.verb := i; INC(i); last.next := v; last := v;
				Dialog.MapString("#Host:Edit", v.name);
				NEW(v); v.verb := i; INC(i); last.next := v; last := v;
				Dialog.MapString("#Host:Open", v.name);
				INC(j);
				CheckVerb(this.obj.view, j, pvm)
			END;
			WHILE pvm.label # "" DO
				NEW(v); v.verb := i; INC(i); last.next := v; last := v;
				Dialog.MapString(pvm.label, v.name);
				v.disabled := pvm.disabled;
				v.checked := pvm.checked;
				INC(j);
				CheckVerb(this.obj.view, j, pvm)
			END;
			e.cur := e.first; enum := e;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
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
			IF debug THEN Log.String("set extent"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
			this.obj.w := size.cx * oleUnit; this.obj.h := size.cy * oleUnit;
			OleData.SetView(this.obj.ido.data, this.obj.view, this.obj.w, this.obj.h);
			IF this.obj.win # NIL THEN
				this.obj.view.context.SetSize(this.obj.w, this.obj.h)
			END;
			IF debug THEN Log.String("on data change (se)"); Log.Ln END;
			IF this.obj.idah # NIL THEN res := this.obj.idah.SendOnDataChange(this.obj.ido, 0, {}) END;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_FAIL
		END
	END SetExtent;
	
	PROCEDURE (this: IOleObject) GetExtent (aspect: SET; OUT size: WinApi.SIZE): COM.RESULT;
	BEGIN
		IF aspect * WinOle.DVASPECT_CONTENT # {} THEN
			this.obj.CheckViewUpdate;
			IF (this.obj.w # 0) & (this.obj.h # 0) THEN
				size.cx := this.obj.w DIV oleUnit; size.cy := this.obj.h DIV oleUnit;
				IF debug THEN Log.String("get extent"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
				RETURN WinApi.S_OK
			END
		END;
		RETURN WinApi.E_FAIL
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
		status := miscStatus;
		RETURN WinApi.S_OK
	END GetMiscStatus;
	
	PROCEDURE (this: IOleObject) SetColorScheme (IN pal: WinApi.LOGPALETTE): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END SetColorScheme;

	
	(* ---------- IDataObject ---------- *)
	
	PROCEDURE (this: IDataObject) GetData (IN format: WinOle.FORMATETC;
															OUT medium: WinOle.STGMEDIUM): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("get data"); Log.Ln END;
		RETURN this.data.GetData(format, medium)
	END GetData;
	
	PROCEDURE (this: IDataObject) GetDataHere (IN format: WinOle.FORMATETC;
																	VAR medium: WinOle.STGMEDIUM): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("get data here"); Log.Ln END;
		RETURN this.data.GetDataHere(format, medium)
	END GetDataHere;
	
	PROCEDURE (this: IDataObject) QueryGetData (IN format: WinOle.FORMATETC): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("query get data"); Log.Ln END;
		RETURN this.data.QueryGetData(format)
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
	
	PROCEDURE (this: IDataObject) EnumFormatEtc (
		direction: SET; OUT enum: WinOle.IEnumFORMATETC
	): COM.RESULT;
	BEGIN
		RETURN this.data.EnumFormatEtc(direction, enum)
	END EnumFormatEtc;
	
	PROCEDURE (this: IDataObject) DAdvise (
		IN format: WinOle.FORMATETC; flags: SET; advSink: WinOle.IAdviseSink; OUT connection: INTEGER
	): COM.RESULT;
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
	
	PROCEDURE (this: IPersistStorage) GetClassID* (OUT id: COM.GUID): COM.RESULT, EXTENSIBLE;
	BEGIN
		id := ObjectID;
		RETURN WinApi.S_OK
	END GetClassID;

	PROCEDURE (this: IPersistStorage) IsDirty (): COM.RESULT;
	BEGIN
		IF (this.obj.view = NIL) OR (this.obj.seq # NIL) & this.obj.seq.Dirty() THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END IsDirty;
	
	PROCEDURE (this: IPersistStorage) InitNew (stg: WinOle.IStorage): COM.RESULT;
		VAR res: COM.RESULT; ps: WinApi.PtrWSTR;
	BEGIN
		IF debug THEN Log.String("init new"); Log.Ln END;
		IF stg # NIL THEN
			res := stg.CreateStream(streamStr,
											WinOle.STGM_DIRECT + WinOle.STGM_CREATE
											+ WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE,
											0, 0, this.obj.ism);
			IF res >= 0 THEN
				res := WinOle.OleRegGetUserType(ObjectID, WinOle.USERCLASSTYPE_SHORT, ps);
				res := WinOle.WriteFmtUserTypeStg(stg, cbFormat, ps);
				WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, ps));
				this.obj.isg := stg;
				this.obj.w := 60 * Ports.mm;
				this.obj.h := 60 * Ports.mm;
				this.obj.rsm := NIL;
				this.obj.view := TextViews.dir.New(NIL);
				this.obj.seq := NIL;
				this.obj.win := NIL;
				OleData.SetView(this.obj.ido.data, this.obj.view, this.obj.w, this.obj.h);
				RETURN WinApi.S_OK
			ELSE RETURN res
			END
		ELSE RETURN WinApi.E_POINTER
		END
	END InitNew;
	
	PROCEDURE (this: IPersistStorage) Load (stg: WinOle.IStorage): COM.RESULT;
		VAR res: COM.RESULT; tag, version: INTEGER; is: BOOLEAN;
	BEGIN
		IF debug THEN Log.String("load"); Log.Ln END;
		IF stg # NIL THEN
			res := stg.OpenStream(streamStr, 0,
				WinOle.STGM_DIRECT + WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE,
				0, this.obj.ism);
			IF res >= 0 THEN
				res := WinOle.CreateStreamOnHGlobal(0, 1, this.obj.rsm);
				res := this.obj.ism.CopyTo(this.obj.rsm, MAX(LONGINT), NIL, NIL);
				OleStorage.ImportFromStream(this.obj.rsm, this.obj.view, this.obj.w, this.obj.h, is);
				IF this.obj.view # NIL THEN
					this.obj.seq := NIL;
					this.obj.win := NIL;
					this.obj.isg := stg;
					OleData.SetView(this.obj.ido.data, this.obj.view, this.obj.w, this.obj.h);
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
			IF debug THEN Log.String("save same"); Log.Ln END;
			stm := this.obj.ism;
		ELSIF stg # NIL THEN
			IF debug THEN Log.String("save"); Log.Ln END;
			res := stg.CreateStream(streamStr,
											WinOle.STGM_DIRECT + WinOle.STGM_CREATE
											+ WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE,
											0, 0, stm);
			IF res < 0 THEN RETURN res END;
			res := WinOle.OleRegGetUserType(ObjectID, WinOle.USERCLASSTYPE_SHORT, ps);
			res := WinOle.WriteFmtUserTypeStg(stg, cbFormat, ps);
			WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, ps));
		ELSE RETURN WinApi.E_POINTER
		END;
		this.obj.CheckViewUpdate;
		OleStorage.ExportToStream(stm, this.obj.view, this.obj.w, this.obj.h, TRUE);
		IF this.obj.seq # NIL THEN
			this.obj.seq.BeginModification(Sequencers.notUndoable, NIL);
			this.obj.seq.EndModification(Sequencers.notUndoable, NIL);	(* clear sequencer *)
			this.obj.seq.SetDirty(FALSE)
		END;
		RETURN WinApi.S_OK
	END Save;
	
	PROCEDURE (this: IPersistStorage) SaveCompleted (new: WinOle.IStorage): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("save completed"); Log.Ln END;
		IF new # NIL THEN
			res := new.OpenStream(streamStr, 0,
				WinOle.STGM_DIRECT + WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE,
				0, this.obj.ism);
			IF res >= 0 THEN
				this.obj.isg := new;
			ELSE RETURN res
			END
		END;
		IF this.obj.ioah # NIL THEN res := this.obj.ioah.SendOnSave() END;
		RETURN WinApi.S_OK
	END SaveCompleted;
	
	PROCEDURE (this: IPersistStorage) HandsOffStorage (): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		this.obj.ism := NIL; this.obj.isg := NIL;
		RETURN WinApi.S_OK
	END HandsOffStorage;
	

	(* ---------- IOleInPlaceObject ---------- *)
	
	PROCEDURE (this: IOleInPlaceObject) GetWindow (OUT wnd: WinApi.HWND): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipo: get window"); Log.Ln END;
		wnd := this.obj.win.wnd;
		ASSERT(wnd # 0, 100);
		RETURN WinApi.S_OK
	END GetWindow;
	
	PROCEDURE (this: IOleInPlaceObject) ContextSensitiveHelp (enter: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipo: context help"); Log.Ln END;
		RETURN WinApi.S_OK
	END ContextSensitiveHelp;
	
	PROCEDURE (this: IOleInPlaceObject) InPlaceDeactivate (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipo: ip deactivate"); Log.Ln END;
		this.obj.InPlaceDeactivate;
		RETURN WinApi.S_OK
	END InPlaceDeactivate;
	
	PROCEDURE (this: IOleInPlaceObject) UIDeactivate (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipo: ui deactivate"); Log.Ln END;
		this.obj.UIDeactivate;
		RETURN WinApi.S_OK
	END UIDeactivate;
	
	PROCEDURE (this: IOleInPlaceObject) SetObjectRects (IN pos, clip: WinApi.RECT): COM.RESULT;
		VAR l, t, r, b, bw, u, res: INTEGER;
	BEGIN
		IF debug THEN Log.String("iipo: set rect"); Log.Ln END;
		u := this.obj.unit;
		bw := borderW DIV HostWindows.unit;
		l := Max(pos.left - bw, clip.left);
		t := Max(pos.top - bw, clip.top);
		r := Min(pos.right + bw, clip.right);
		b := Min(pos.bottom + bw, clip.bottom);
		res := WinApi.SetWindowPos(this.obj.win.wnd, 0, l, t, r - l, b - t, WinApi.SWP_NOZORDER);
		this.obj.win.SetSize(r - l, b - t);
		l := Min(bw, pos.left - clip.left) * u;
		t := Min(bw, pos.top - clip.top) * u;
(*
		r := l + (pos.right - pos.left) * u;
		b := t + (pos.bottom - pos.top) * u;
*)
		r := l + this.obj.w;
		b := t + this.obj.h;
		this.obj.win.doc.SetRect(l, t, r, b);
		RETURN WinApi.S_OK
	END SetObjectRects;
	
	PROCEDURE (this: IOleInPlaceObject) ReactivateAndUndo (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipo: reactivate & undo"); Log.Ln END;
		RETURN WinApi.S_OK
	END ReactivateAndUndo;
	
	
	(* ---------- IOleInPlaceActiveObject ---------- *)
	
	PROCEDURE (this: IOleInPlaceActiveObject) GetWindow (OUT wnd: WinApi.HWND): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: get window"); Log.Ln END;
		wnd := this.obj.win.wnd;
		RETURN WinApi.S_OK
	END GetWindow;
	
	PROCEDURE (this: IOleInPlaceActiveObject) ContextSensitiveHelp (enter: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: context help"); Log.Ln END;
		RETURN WinApi.S_OK
	END ContextSensitiveHelp;
	
	PROCEDURE (this: IOleInPlaceActiveObject) TranslateAccelerator (IN msg: WinApi.MSG): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: translate acc"); Log.Ln END;
		RETURN WinApi.S_FALSE
	END TranslateAccelerator;
	
	PROCEDURE (this: IOleInPlaceActiveObject) OnFrameWindowActivate (activate: WinApi.BOOL): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: on frame active "); Log.Int(activate); Log.Ln END;
		IF (this.obj.iipf # NIL) & (this.obj.iipw = NIL) THEN
			IF activate # 0 THEN
				HostWindows.ActivateWindow(this.obj.win, TRUE);
				HostMenus.isObj := TRUE
			ELSE
				HostWindows.ActivateWindow(this.obj.win, FALSE);
				HostMenus.isObj := FALSE
			END
		END;
		RETURN WinApi.S_OK
	END OnFrameWindowActivate;
	
	PROCEDURE (this: IOleInPlaceActiveObject) OnDocWindowActivate (activate: WinApi.BOOL): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: on win active"); Log.Int(activate); Log.Ln END;
		IF this.obj.iipf # NIL THEN
			IF activate # 0 THEN
				IF this.obj.uiActive THEN res := this.obj.iipf.SetActiveObject(this, "") END;
				HostWindows.ActivateWindow(this.obj.win, TRUE);
				HostMenus.isObj := TRUE;
				IF this.obj.menu # 0 THEN
					res := this.obj.iipf.SetMenu(this.obj.menu, this.obj.oleMenu, HostWindows.main);
					this.obj.useMenu := TRUE
				END
			ELSE
				IF this.obj.menu # 0 THEN
					res := this.obj.iipf.SetMenu(0, 0, 0);
					this.obj.useMenu := FALSE
				END;
				HostWindows.ActivateWindow(this.obj.win, FALSE);
				HostMenus.isObj := FALSE;
				IF this.obj.uiActive THEN res := this.obj.iipf.SetActiveObject(NIL, NIL) END
			END
		END;
		RETURN WinApi.S_OK
	END OnDocWindowActivate;
	
	PROCEDURE (this: IOleInPlaceActiveObject) ResizeBorder (IN border: WinApi.RECT;
														win: WinOle.IOleInPlaceUIWindow; frameWin: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: resize border"); Log.Ln END;
		RETURN WinApi.S_OK
	END ResizeBorder;
	
	PROCEDURE (this: IOleInPlaceActiveObject) EnableModeless (enable: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("iipao: enable modeless"); Log.Ln END;
		RETURN WinApi.S_OK
	END EnableModeless;


	(* ---------- Window ---------- *)
	
	PROCEDURE (w: Window) BroadcastViewMsg (VAR msg: Views.Message);
		VAR res: COM.RESULT;
	BEGIN
		w.BroadcastViewMsg^(msg);
		IF msg.view = w.obj.view THEN w.obj.update := TRUE END
	END BroadcastViewMsg;
	
	PROCEDURE (w: Window) ForwardCtrlMsg (VAR msg: Views.CtrlMessage);
		VAR c, dc: Containers.Controller;
	BEGIN
		w.ForwardCtrlMsg^(msg);
		WITH msg: Controllers.EditMsg DO
			IF (msg.op = Controllers.pasteChar) & (msg.char = 1BX) & (31 IN msg.modifiers) & w.obj.uiActive THEN
				w.obj.InPlaceDeactivate()
			END
		ELSE
		END
	END ForwardCtrlMsg;
	
	PROCEDURE (w: Window) Close;
		VAR res: COM.RESULT;
	BEGIN
		IF w.obj.uiActive THEN
			IF debug THEN Log.String("close -> deactivate"); Log.Ln END;
			w.obj.InPlaceDeactivate()
		ELSE
			IF debug THEN Log.String("close window"); Log.Ln END;
			w.obj.CheckViewUpdate;
			SYSTEM.PUT(SYSTEM.ADR(w.sub), TRUE);	(* don't send remove msg *)
			w.Close^;
			IF (w.obj.ics # NIL) & (w.obj.iips = NIL) THEN res := w.obj.ics.OnShowWindow(0) END;
			IF debug THEN Log.String("close window c"); Log.Ln END;
			w.obj.win := NIL
		END
	END Close;
	

	(* ---------- Action ---------- *)
	
	PROCEDURE (a: Action) Do;
		VAR res: COM.RESULT; w, h: INTEGER;
	BEGIN
		IF a.w.frame # NIL THEN
			a.w.obj.CheckViewUpdate;
			Services.DoLater(a, Services.Ticks() + Services.resolution)
		END
	END Do;
	

	(* ---------- WinDir ---------- *)
	
	PROCEDURE (d: WinDir) New (): HostWindows.Window;
		VAR w: Windows.Window; sw: Window; a: Action;
	BEGIN
		IF d.obj # NIL THEN
			NEW(sw); sw.obj := d.obj;
			NEW(a); a.w := sw; Services.DoLater(a, Services.now);
			RETURN sw
		ELSE
			w := Windows.stdDir.New(); RETURN w(HostWindows.Window)
		END
	END New;
	
	PROCEDURE (d: WinDir) Open (w: Windows.Window; doc: Documents.Document; flags: SET; name: Views.Title;
											loc: Files.Locator; fname: Files.Name; conv: Converters.Converter);
		VAR l, t, r, b, bw, u, u1, res: INTEGER; style: SET; wnd: WinApi.HWND; c: Containers.Controller; dc: WinApi.HDC;
	BEGIN
		IF (d.obj # NIL) & (d.obj.iips # NIL) THEN	(* open in place *)
			WITH w: Window DO
				IF debug THEN Log.String("open window in place"); Log.Ln END;
				flags := flags + {Windows.noHScroll, Windows.noVScroll, Windows.noResize, HostWindows.inPlace};
				u := d.obj.w DIV (d.pos.right - d.pos.left);
				u1 := d.obj.h DIV (d.pos.bottom - d.pos.top);
				IF u1 > u THEN u := u1 END;
				d.unit := u;
				d.Open^(w, doc, flags, name, loc, fname, conv);
(*
				u := HostWindows.unit;
*)
				bw := borderW DIV HostWindows.unit;
				l := Max(d.pos.left - bw, d.clip.left);
				t := Max(d.pos.top - bw, d.clip.top);
				r := Min(d.pos.right + bw, d.clip.right);
				b := Min(d.pos.bottom + bw, d.clip.bottom);
				style := {30};	(* child *)
				wnd := WinApi.CreateWindowExW({}, "Oberon Dlg", "", style, l, t, r - l, b - t,
															d.host, 0, WinApi.GetModuleHandleW(NIL), SYSTEM.VAL(INTEGER, w));
				ASSERT(wnd # 0, 100);
				dc := WinApi.GetDC(wnd);
				w.port(HostPorts.Port).SetDC(dc, wnd);
				w.SetSize(r - l, b - t);
				l := Min(bw, d.pos.left - d.clip.left) * u;
				t := Min(bw, d.pos.top - d.clip.top) * u;
(*
				r := l + (d.pos.right - d.pos.left) * u;
				b := t + (d.pos.bottom - d.pos.top) * u;
*)
				IF debug THEN Log.Int(d.obj.w); Log.Int(d.obj.h); Log.Ln END;
				r := l + d.obj.w;
				b := t + d.obj.h;
				w.doc.SetRect(l, t, r, b);
				res := WinApi.ShowWindow(wnd, 1);
				d.obj.unit := u;
				IF debug THEN
					style := SYSTEM.VAL(SET, WinApi.GetWindowLongW(wnd, -16));
					Log.Set(style);
					Log.Ln
				END;				
				c := w.doc.ThisController();
				c.SetFocus(w.doc.ThisView())
			END
		ELSE
			d.Open^(w, doc, flags, name, loc, fname, conv)
		END;
		d.obj := NIL
	END Open;
	
	
	(* ---------- import / export ---------- *)
	
	PROCEDURE Export* (v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM);
		VAR stg: WinOle.IStorage; stm: WinOle.IStream; res: COM.RESULT; ilb: WinOle.ILockBytes; ps: WinApi.PtrWSTR;
	BEGIN
		IF debug THEN Log.String("export"); Log.Ln END;
		IF med.tymed = WinOle.TYMED_ISTORAGE THEN
			stg := MediumStorage(med)
		ELSE
			res := WinOle.CreateILockBytesOnHGlobal(0, 1, ilb);
			ASSERT(res >= 0, 110);
			res := WinOle.StgCreateDocfileOnILockBytes(ilb, WinOle.STGM_CREATE
											+ WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE, 0, stg);
			ASSERT(res >= 0, 111);
			GenStorageMedium(stg, NIL, med)
		END;
		res := WinOle.WriteClassStg(stg, ObjectID);
		ASSERT(res >= 0, 112);
		res := stg.CreateStream(streamStr, WinOle.STGM_CREATE
										+ WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE, 0, 0, stm);
		ASSERT(res >= 0, 113);
		res := WinOle.OleRegGetUserType(ObjectID, WinOle.USERCLASSTYPE_SHORT, ps);
		res := WinOle.WriteFmtUserTypeStg(stg, cbFormat, ps);
		WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, ps));
		ASSERT(res >= 0, 114);
		OleStorage.ExportToStream(stm, v, w, h, isSingle);
		res := stg.Commit({});
		ASSERT(res >= 0, 115)
	END Export;
	
	PROCEDURE Import* (
		VAR med: WinOle.STGMEDIUM; VAR v: Views.View; VAR w, h: INTEGER; VAR isSingle: BOOLEAN
	);
		VAR stg: WinOle.IStorage; res: COM.RESULT; id: COM.GUID; stm, stm2: WinOle.IStream;
	BEGIN
		IF debug THEN Log.String("import"); Log.Ln END;
		stg := MediumStorage(med);
		res := WinOle.ReadClassStg(stg, id);
		IF (res = WinApi.S_OK) & (id = ObjectID) THEN
			res := stg.OpenStream(streamStr, 0,
				WinOle.STGM_DIRECT + WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE,
				0, stm);
			ASSERT(res >= 0, 110);
			res := WinOle.CreateStreamOnHGlobal(0, 1, stm2);
			ASSERT(res >= 0, 111);
			res := stm.CopyTo(stm2, MAX(LONGINT), NIL, NIL);
			ASSERT(res >= 0, 112);
			OleStorage.ImportFromStream(stm2, v, w, h, isSingle);
		ELSE
			v := NIL
		END
	END Import;

	(* ---------- commands ---------- *)
	
	PROCEDURE ContextOf* (w: Windows.Window): WinOle.IOleInPlaceSite;
	BEGIN
		WITH w: Window DO
			IF w.obj # NIL THEN RETURN w.obj.iips END
		ELSE RETURN NIL
		END
	END ContextOf;
	
	PROCEDURE RemoveUI* (w: Windows.Window);
		VAR res: COM.RESULT;
	BEGIN
		WITH w: Window DO
			IF (w.obj # NIL) & (w.obj.iips # NIL) & w.obj.uiActive THEN
				IF debug THEN Log.String("remove ui"); Log.Ln END;
				IF w.obj.iipf # NIL THEN res := w.obj.iipf.SetActiveObject(NIL, NIL) END;
				IF w.obj.iipw # NIL THEN res := w.obj.iipw.SetActiveObject(NIL, NIL) END;
				IF (w.obj.iipf # NIL) & (w.obj.menu # 0) THEN
					res := w.obj.iipf.SetMenu(0, 0, 0);
					w.obj.useMenu := FALSE
				END;
(*
				HostWindows.ActivateWindow(w, FALSE);
*)
			END
		ELSE
		END
	END RemoveUI;
	
	PROCEDURE ResetUI* (w: Windows.Window; VAR done: BOOLEAN);
		VAR res: COM.RESULT; rect: WinApi.RECT;
	BEGIN
		done := FALSE;
		WITH w: Window DO
			IF (w.obj # NIL) & (w.obj.iips # NIL) & w.obj.uiActive THEN
				IF debug THEN Log.String("reset ui"); Log.Ln END;
				IF w.obj.iipf # NIL THEN res := w.obj.iipf.SetActiveObject(w.obj.iipao, "") END;
				IF w.obj.iipw # NIL THEN res := w.obj.iipw.SetActiveObject(w.obj.iipao, "") END;
				IF (w.obj.iipf # NIL) & (w.obj.menu # 0) THEN
					res := w.obj.iipf.SetMenu(w.obj.menu, w.obj.oleMenu, HostWindows.main);
					w.obj.useMenu := TRUE
				END;
				rect.left := 0; rect.top := 0; rect.right := 0; rect.bottom := 0;
				IF w.obj.iipf # NIL THEN res := w.obj.iipf.SetBorderSpace(rect) END;
				IF w.obj.iipw # NIL THEN res := w.obj.iipw.SetBorderSpace(rect) END;
(*
				HostWindows.ActivateWindow(w, TRUE);
*)
				done := TRUE
			END
		ELSE
		END
	END ResetUI;
	
	PROCEDURE ShowOleStatus (w: Windows.Window; s: ARRAY OF CHAR);
		VAR res: COM.RESULT;
	BEGIN
		WITH w: Window DO
			IF w.obj.iipf # NIL THEN
				res := w.obj.iipf.SetStatusText(s)
			END
		ELSE
		END
	END ShowOleStatus;
	
	PROCEDURE UpdateOleMenus ();
		VAR w: Windows.Window; res: COM.RESULT;
	BEGIN
		w := Windows.dir.First();
		WHILE w # NIL DO
			WITH w: Window DO
				IF (w.obj.iipf # NIL) & (w.obj.menu # 0) THEN
					w.obj.InPlaceMenuDestroy(); w.obj.InPlaceMenuCreate();
					IF w.obj.useMenu THEN
						res := w.obj.iipf.SetMenu(w.obj.menu, w.obj.oleMenu, HostWindows.main)
					END
				END
			ELSE
			END;
			w := Windows.dir.Next(w)
		END
	END UpdateOleMenus;
	
	PROCEDURE TranslateOleKeys (VAR msg: WinApi.MSG; VAR done: BOOLEAN);
		VAR w: Windows.Window; res: COM.RESULT;
	BEGIN
		w := Windows.dir.First(); done := FALSE;
		IF w # NIL THEN
			WITH w: Window DO
				IF w.obj.iipf # NIL THEN
					res := WinOle.OleTranslateAccelerator(w.obj.iipf, w.obj.fInfo, msg);
					IF res = WinApi.S_OK THEN done := TRUE END
				END
			ELSE
			END
		END
	END TranslateOleKeys;
	
	PROCEDURE Register;
		VAR res: COM.RESULT;
	BEGIN
		NEW(factory);
		res := WinOle.CoRegisterClassObject(ObjectID, factory,
												WinOle.CLSCTX_LOCAL_SERVER, WinOle.REGCLS_MULTIPLEUSE, token);
	END Register;
	
	PROCEDURE Unregister;
		VAR res: COM.RESULT; n: INTEGER; p: Object;
	BEGIN
		IF token # 0 THEN
			res := WinOle.CoRevokeClassObject(token)
		END
	END Unregister;

BEGIN
	NEW(winDir);
	HostDialog.ShowOleStatus := ShowOleStatus;
	HostMenus.UpdateOleMenus := UpdateOleMenus;
	HostMenus.TranslateOleKeys2 := TranslateOleKeys;
	Register
CLOSE
	Unregister
END OleServer.

OleServer.xy	OleServer.Close

DevDecoder.Decode OleServer
