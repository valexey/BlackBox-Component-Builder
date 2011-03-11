MODULE OleClient;
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
		SYSTEM, COM, WinApi, WinOle, WinOleCtl, WinOleAut, WinOleDlg,
		OleStorage, OleData, OleServer,
		Meta, Dialog, Services, Ports, Stores, Sequencers, Models, Views, Controllers,
		Properties, Containers, Controls,
		StdDialog, Log, HostPorts, HostWindows, HostMenus;

	CONST
		debug = FALSE;
		minModelVersion = 0; maxModelVersion = 1;
		minViewVersion = 0; maxViewVersion = 0;
		ObjectID = "{00000001-1000-11cf-adf0-444553540000}";	(* BlackBox views *)
		miscStatus = WinOle.OLEMISC_RECOMPOSEONRESIZE
			+ WinOle.OLEMISC_CANTLINKINSIDE
			+ WinOle.OLEMISC_RENDERINGISDEVICEINDEPENDENT;
		oleUnit = Ports.mm DIV 100;

	TYPE
		IOleClientSite = POINTER TO RECORD (WinOle.IOleClientSite)
			ias: IAdviseSink;
			iips: IOleInPlaceSite;
			obj: Model;
			frame: Views.Frame;
			wnd: WinApi.HWND
		END;
		IAdviseSink = POINTER TO RECORD (WinOle.IAdviseSink)
			site: IOleClientSite;
			obj: Model
		END;
		IOleInPlaceSite = POINTER TO RECORD (WinOle.IOleInPlaceSite)
			site: IOleClientSite;
			obj: Model
		END;

		IOleInPlaceUIWindow = POINTER TO RECORD (WinOle.IOleInPlaceUIWindow)
			wnd: WinApi.HWND;
			iipao: WinOle.IOleInPlaceActiveObject
		END;
		WinHook = POINTER TO RECORD (HostWindows.Hook)
			iipw: IOleInPlaceUIWindow
		END;

		IOleInPlaceFrame = POINTER TO RECORD (WinOle.IOleInPlaceFrame)
			objWnd: WinApi.HWND;	(* for menu handling *)
			iipao: WinOle.IOleInPlaceActiveObject
		END;
		FrameHook = POINTER TO RECORD (HostWindows.Hook) END;

		Sink = POINTER TO RECORD
			next: Sink;
(*
			obj: CtlT.OutObject;
*)
			point: WinOle.IConnectionPoint;
			cookie: INTEGER
		END;

		View = POINTER TO RECORD (Views.View)
			model: Model;
			hasNotifier: BOOLEAN
		END;
		Model = POINTER TO RECORD (Models.Model)
			site: IOleClientSite;
			advConn: INTEGER;
			open: BOOLEAN;
			objUnk: COM.IUnknown;
			objView: WinOle.IViewObject2;
			objObj: WinOle.IOleObject;
			objIPObj: WinOle.IOleInPlaceObject;	(* # NIL: in place open *)
			stg: WinOle.IStorage;
			flags: SET;
			w, h: INTEGER;	(* actual site size (units) *)
			rw, rh: INTEGER;	(* actual pos rect size (pixels) *)
			guard: BOOLEAN;
			focusGuard: BOOLEAN;
			onServer: BOOLEAN;
			view: View;
			link: ARRAY 256 OF CHAR;
			sinks: Sink
		END;
		Frame = POINTER TO RECORD (Views.Frame) END;

		Notifier = POINTER TO RECORD (Sequencers.Notifier)
			model: Model
		END;

		Deactivator = POINTER TO RECORD (Services.Action)
			obj: Model
		END;

		UpdateMsg = RECORD (Models.UpdateMsg)
			checkSize: BOOLEAN
		END;

(*
		ObjectValue = RECORD (Meta.Value)
			obj*: CtlT.OutObject
		END;
*)
		ProcValue = RECORD (Meta.Value)
			open*: PROCEDURE(v: Views.View)
		END;

		Op = POINTER TO RECORD (Stores.Operation)
			model: Model;
			link: ARRAY 256 OF CHAR
		END;


	VAR
		appFrame: IOleInPlaceFrame;
		winMenu: WinApi.HMENU;
		hAccel: WinApi.HACCEL;
		nAccel: INTEGER;
		menuBar: WinApi.HMENU;


	(* ----------callback linking ---------- *)

	PROCEDURE Connect* (v: Views.View; iid: COM.GUID; disp: WinOleAut.IDispatch);
		VAR res: COM.RESULT; cont: WinOle.IConnectionPointContainer; point: WinOle.IConnectionPoint; sink: Sink;
	BEGIN
		WITH v: View DO
			res := v.model.objUnk.QueryInterface(COM.ID(cont), cont);
			IF res >= 0 THEN
				res := cont.FindConnectionPoint(iid, point);
				IF res >= 0 THEN
					NEW(sink); sink.point := point;
					res := point.Advise(disp, sink.cookie);
					IF res >= 0 THEN
						sink.next := v.model.sinks; v.model.sinks := sink
					END
				END
			END
		ELSE
		END
	END Connect;

	PROCEDURE Disconnect (model: Model);
		VAR res: COM.RESULT; sink: Sink;
	BEGIN
		sink := model.sinks;
		WHILE sink # NIL DO
			res := sink.point.Unadvise(sink.cookie);
			sink := sink.next
		END;
		model.sinks := NIL
	END Disconnect;

	PROCEDURE OpenLink (model: Model);
		VAR item: Meta.Item; pv: ProcValue; ok: BOOLEAN;
	BEGIN
		IF model.link # "" THEN
			Meta.LookupPath(model.link, item);
			IF item.obj = Meta.procObj THEN
				item.GetVal(pv, ok);
				IF ok THEN pv.open(model.view)
				ELSE Dialog.ShowParamMsg("#System:HasWrongType", model.link, "", "")
				END
			ELSE Dialog.ShowParamMsg("#System:NotFound", model.link, "", "")
			END
		END
	END OpenLink;
(*
	PROCEDURE OpenLinks (model: Model);
		VAR item: Meta.Item; ov: ObjectValue; ok: BOOLEAN; iid: COM.GUID; res: COM.RESULT;
			cont: WinOle.IConnectionPointContainer; disp: WinOleAut.IDispatch; point: WinOle.IConnectionPoint;
			i, j: INTEGER; name: ARRAY 256 OF CHAR; sink: Sink;
	BEGIN
		i := 0;
		WHILE model.link[i] # 0X DO
			WHILE (model.link[i] # 0X) & (model.link[i] <= ",") DO INC(i) END;
			j := 0;
			WHILE model.link[i] > "," DO name[j] := model.link[i]; INC(j); INC(i) END;
			name[j] := 0X;
			IF name # "" THEN
				Meta.LookupPath(name, item);
				IF item.obj # Meta.undef THEN
					IF (item.obj = Meta.typObj) & (item.typ = Meta.ptrTyp) THEN item.GetBaseType(item) END;
					IF (item.obj = Meta.typObj) & (item.typ = Meta.recTyp) & item.Is(ov) THEN
						(* item.Allocate(ov); *) ov.obj := item.New()(CtlT.OutObject);
						res := model.objUnk.QueryInterface(COM.ID(cont), cont);
						IF res >= 0 THEN
							ov.obj.GetIID(iid);
							res := cont.FindConnectionPoint(iid, point);
							IF res >= 0 THEN
								disp := CtlT.Disp(ov.obj); ASSERT(disp # NIL);
								NEW(sink); sink.obj := ov.obj; sink.point := point;
								res := point.Advise(disp, sink.cookie);
								IF res >= 0 THEN
									sink.next := model.sinks; model.sinks := sink;
									res := model.objUnk.QueryInterface(COM.ID(disp), disp);
									IF res >= 0 THEN sink.obj.source := CtlT.Obj(disp) END
								END
							ELSE Dialog.ShowParamMsg("#System:HasWrongType", name, "", "")
							END
						ELSE HALT(100)
						END
					ELSE Dialog.ShowParamMsg("#System:HasWrongType", name, "", "")
					END
				ELSE Dialog.ShowParamMsg("#System:NotFound", name, "", "")
				END
			END
		END
	END OpenLinks;

	PROCEDURE CloseLinks (model: Model);
		VAR res: COM.RESULT; sink: Sink; iid: COM.GUID;
	BEGIN
		sink := model.sinks;
		WHILE sink # NIL DO
			res := sink.point.Unadvise(sink.cookie);
			sink.obj.source := NIL;
			sink := sink.next
		END;
		model.sinks := NIL
	END CloseLinks;
*)
	PROCEDURE PollProp (model: Model; VAR list: Properties.Property);
		VAR p: Controls.Prop; res: COM.RESULT; cont: WinOle.IConnectionPointContainer;
	BEGIN
		res := model.objUnk.QueryInterface(COM.ID(cont), cont);
		IF res >= 0 THEN
			NEW(p);
			p.valid := {Controls.link}; p.known := p.valid;
			p.link := model.link$;
			Properties.Insert(list, p)
		END
	END PollProp;

	PROCEDURE SetProp (model: Model; p: Properties.Property);
		VAR res: COM.RESULT; cont: WinOle.IConnectionPointContainer; op: Op;
	BEGIN
		res := model.objUnk.QueryInterface(COM.ID(cont), cont);
		IF res >= 0 THEN
			WHILE p # NIL DO
				WITH p: Controls.Prop DO
					IF Controls.link IN p.valid THEN
						NEW(op); op.model := model; op.link := p.link$;
						Models.Do(model, "#System:SetProp", op)
					END
				ELSE
				END;
				p := p.next
			END
		END
	END SetProp;

	PROCEDURE (op: Op) Do;
		VAR link: ARRAY 256 OF CHAR;
	BEGIN
(*
		Disconnect(op.model);
*)
		link := op.model.link$; op.model.link := op.link$; op.link := link$;
		OpenLink(op.model)
	END Do;


	(* ---------- auxiliary ---------- *)

	PROCEDURE Visible (m: Model; f: Views.Frame): BOOLEAN;
		VAR g: Views.Frame; ctrl: Containers.Controller;
	BEGIN
		IF m.flags * WinOleCtl.OLEMISC_INVISIBLEATRUNTIME = {} THEN RETURN TRUE
		ELSE
			g := Views.HostOf(f);
			IF (g = NIL) OR ~(g.view IS Containers.View) THEN RETURN TRUE
			ELSE
				ctrl := g.view(Containers.View).ThisController();
				RETURN Containers.mask * ctrl.opts # Containers.mask
			END
		END
	END Visible;

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

	PROCEDURE DoVerb (obj: Model; f: Views.Frame; verb: INTEGER);
		VAR res: COM.RESULT; w, h: INTEGER; rect: WinApi.RECT;
	BEGIN
		obj.site.frame := f;
		IF f # NIL THEN
			f.view.context.GetSize(w, h);
			rect.left := f.gx DIV f.unit;
			rect.top := f.gy DIV f.unit;
			rect.right := (f.gx + w) DIV f.unit;
			rect.bottom := (f.gy + h) DIV f.unit;
			obj.rw := w DIV f.unit;
			obj.rh := h DIV f.unit;
			IF debug THEN Log.Int(rect.left); Log.Int(rect.top); Log.Int(rect.right); Log.Int(rect.bottom); Log.Ln END;
			res := obj.objObj.DoVerb(verb, NIL, obj.site, 0, f.rider(HostPorts.Rider).port.wnd, rect)
		ELSE
			rect.left := 0; rect.top := 0; rect.right := 0; rect.bottom := 0;
			res := obj.objObj.DoVerb(verb, NIL, obj.site, 0, 0, rect)
		END
	END DoVerb;

	PROCEDURE UpdateSizes (v: View; checkSize, setRect: BOOLEAN);
		VAR obj: Model; f: Views.Frame; host: Views.Frame; p: HostPorts.Port; res: COM.RESULT;
			pos, clip: WinApi.RECT; size: WinApi.SIZE; w, h, ow, oh: INTEGER;
			c: Containers.Controller; s: Views.View; g: BOOLEAN;
	BEGIN
		obj := v.model; f := obj.site.frame;
		IF (f # NIL) & ((f.rider = NIL) OR (obj.objIPObj = NIL) OR (f.view # v)) THEN f := NIL END;
		IF checkSize THEN
			IF debug THEN Log.String("check sizes"); Log.Ln END;
			res := obj.objObj.GetExtent(WinOle.DVASPECT_CONTENT, size);
			IF res = WinApi.S_OK THEN
				ow := size.cx * oleUnit; oh := size.cy * oleUnit;
				v.context.GetSize(w, h);
				IF (w # ow) OR (h # oh) THEN
					IF debug THEN Log.String("  set size"); Log.Int(ow); Log.Int(oh); Log.Ln END;
					Controllers.SetCurrentPath(Controllers.targetPath);
					c := Containers.Focus();
					Controllers.ResetCurrentPath();
					s := c.Singleton();
					IF f # NIL THEN host := Views.HostOf(f) END;
					g := obj.focusGuard; obj.focusGuard := TRUE;
					v.context.SetSize(ow, oh);
					v.model.w := ow; v.model.h := oh;
					IF f # NIL THEN
						IF host # NIL THEN	(* restore saved frame *)
							IF debug THEN Log.String("  restore frame"); Log.Ln END;
							Views.ValidateRoot(Views.RootOf(host));
							f := Views.ThisFrame(host, v);
							obj.site.frame := f
						END;
						c.SetFocus(v)	(* restore focus *)
					ELSIF (s # NIL) & (s # c.Singleton()) THEN c.SetSingleton(s)	(* restore selection *)
					END;
					obj.focusGuard := g
				END
			END
		END;
		IF f # NIL THEN
			v.context.GetSize(w, h); w := w DIV f.unit; h := h DIV f.unit;
			IF w > obj.rw THEN obj.rw := w; setRect := TRUE END;
			IF h > obj.rh THEN obj.rh := h; setRect := TRUE END;
			IF setRect THEN
				pos.left := f.gx DIV f.unit; pos.top := f.gy DIV f.unit;
				pos.right := pos.left + obj.rw; pos.bottom := pos.top + obj.rh;
				p := f.rider(HostPorts.Rider).port;
				clip.left := 0; clip.top := 0; clip.right := p.w; clip.bottom := p.h;
				IF debug THEN Log.String("set object rects"); Log.Int(obj.rw); Log.Int(obj.rh); Log.Ln END;
				res := obj.objIPObj.SetObjectRects(pos, clip)
			END
		END
	END UpdateSizes;

	PROCEDURE GetAccelTable* (VAR a: WinApi.HACCEL; VAR n: INTEGER);
		VAR res: INTEGER; m: HostMenus.Menu; data: ARRAY 128 OF WinApi.ACCEL; f: BYTE; i: StdDialog.Item;
	BEGIN
		IF menuBar # HostMenus.menuBar THEN	(* update table *)
			IF hAccel # 0 THEN res := WinApi.DestroyAcceleratorTable(hAccel) END;
			nAccel := 0; m := HostMenus.menus;
			WHILE m # NIL DO
				IF m.class IN {0, 2, 4} THEN
					i := m.firstItem;
					WHILE (i # NIL) & (nAccel < LEN(data)) DO
						WITH i: HostMenus.Item DO
							IF i.code # 0 THEN
								f := WinApi.FVIRTKEY;
								IF i.shift THEN INC(f, WinApi.FSHIFT) END;
								IF i.ctrl THEN INC(f, WinApi.FCONTROL) END;
								data[nAccel].key := SHORT(i.code);
								data[nAccel].cmd := SHORT(i.id);
								data[nAccel].fVirt := SHORT(CHR(f));
								INC(nAccel)
							END
						END;
						i := i.next
					END
				END;
				m := m.next
			END;
			hAccel := WinApi.CreateAcceleratorTableW(data[0], nAccel);
			IF debug THEN Log.String("new accel table"); Log.Int(nAccel); Log.Int(hAccel); Log.Ln END;
			IF hAccel = 0 THEN nAccel := 0 END;
			menuBar := HostMenus.menuBar
		END;
		a := hAccel; n := nAccel
	END GetAccelTable;


	(* ---------- IOleClientSite ---------- *)

	PROCEDURE (this: IOleClientSite) QueryInterface (IN iid: COM.GUID; OUT int: COM.IUnknown): COM.RESULT;
	BEGIN
		IF COM.QUERY(this, iid, int)
			OR COM.QUERY(this.ias, iid, int)
			OR COM.QUERY(this.iips, iid, int) THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_NOINTERFACE
		END
	END QueryInterface;

	PROCEDURE (this: IOleClientSite) SaveObject (): COM.RESULT;
		VAR res: COM.RESULT; ips: WinOle.IPersistStorage;
	BEGIN
		IF debug THEN Log.String("do save object"); Log.Ln END;
		IF (this.obj # NIL) & (this.obj.objUnk # NIL) THEN
			res := this.obj.objUnk.QueryInterface(COM.ID(ips), ips);
			ASSERT(res >= 0, 100);
			res := WinOle.OleSave(ips, this.obj.stg, 1);
			ASSERT(res >= 0, 101);
			res := ips.SaveCompleted(NIL);
			ASSERT(res >= 0, 102);
			RETURN WinApi.S_OK
		ELSE
			RETURN WinApi.E_FAIL
		END
	END SaveObject;

	PROCEDURE (this: IOleClientSite) GetMoniker (
		assign, which: INTEGER; OUT mk: WinOle.IMoniker
	): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END GetMoniker;

	PROCEDURE (this: IOleClientSite) GetContainer (OUT container: WinOle.IOleContainer): COM.RESULT;
	BEGIN
		RETURN WinApi.E_NOTIMPL
	END GetContainer;

	PROCEDURE (this: IOleClientSite) ShowObject (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("do show object"); Log.Ln END;
		(* Container.MakeVisible(object) *)
		RETURN WinApi.S_OK
	END ShowObject;

	PROCEDURE (this: IOleClientSite) OnShowWindow (show: WinApi.BOOL): COM.RESULT;
		VAR msg: UpdateMsg;
	BEGIN
		IF debug THEN Log.String("do show window"); Log.Ln END;
		this.obj.open := show # 0; msg.checkSize := FALSE;
		Models.Broadcast(this.obj, msg);
		RETURN WinApi.S_OK
	END OnShowWindow;

	PROCEDURE (this: IOleClientSite) RequestNewObjectLayout (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("request new layout"); Log.Ln END;
		RETURN WinApi.E_NOTIMPL
	END RequestNewObjectLayout;


	(* ---------- IAdviseSink ---------- *)

	PROCEDURE (this: IAdviseSink) OnDataChange (
		IN format: WinOle.FORMATETC; IN medium: WinOle.STGMEDIUM
	);
	BEGIN
		IF debug THEN Log.String("on data change"); Log.Ln END
	END OnDataChange;

	PROCEDURE (this: IAdviseSink) OnViewChange (aspect: SET; index: INTEGER);
		VAR msg: UpdateMsg; ips: WinOle.IPersistStorage; res: COM.RESULT; seq: ANYPTR;
	BEGIN
		IF debug THEN Log.String("on view change"); Log.Ln END;
		msg.checkSize := TRUE;
		IF ~this.obj.guard THEN Models.Broadcast(this.obj, msg) END;
		IF WinOle.OleIsRunning(this.obj.objObj) # 0 THEN	(* check dirty *)
			res := this.obj.objUnk.QueryInterface(COM.ID(ips), ips);
			IF (res >= 0) & (ips.IsDirty() # WinApi.S_FALSE) THEN
			IF debug THEN Log.String("set dirty"); Log.Int(ips.IsDirty()); Log.Ln END;
				IF this.obj.Domain() # NIL THEN
					seq := this.obj.Domain().GetSequencer();
					IF seq # NIL THEN
						seq(Sequencers.Sequencer).SetDirty(TRUE)
					END
				ELSIF debug THEN Log.String("nil domain !!!"); Log.Ln
				END
			END
		END
	END OnViewChange;

	PROCEDURE (this: IAdviseSink) OnRename (moniker: WinOle.IMoniker);
	BEGIN
		IF debug THEN Log.String("on rename"); Log.Ln END
	END OnRename;

	PROCEDURE (this: IAdviseSink) OnSave ();
	BEGIN
		IF debug THEN Log.String("on save"); Log.Ln END
	END OnSave;

	PROCEDURE (this: IAdviseSink) OnClose ();
	BEGIN
		IF debug THEN Log.String("on close"); Log.Ln END;
		this.obj.open := FALSE
	END OnClose;


	(* ---------- IOleInPlaceSite ---------- *)

	PROCEDURE (this: IOleInPlaceSite) GetWindow (OUT wnd: WinApi.HWND): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip site: get window"); Log.Ln END;
		IF this.site.frame # NIL THEN
			wnd := this.site.frame.rider(HostPorts.Rider).port.wnd;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_FAIL
		END
	END GetWindow;

	PROCEDURE (this: IOleInPlaceSite) ContextSensitiveHelp (enter: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip site: context help"); Log.Ln END;
		RETURN WinApi.S_OK
	END ContextSensitiveHelp;

	PROCEDURE (this: IOleInPlaceSite) CanInPlaceActivate (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip site: can activate"); Log.Ln END;
		IF this.site.frame # NIL THEN RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END CanInPlaceActivate;

	PROCEDURE (this: IOleInPlaceSite) OnInPlaceActivate (): COM.RESULT;
		VAR res: COM.RESULT; host: Views.Frame; c: Containers.Controller; style: SET;
	BEGIN
		IF debug THEN Log.String("ip site: on activate"); Log.Ln END;
		res := this.obj.objUnk.QueryInterface(COM.ID(this.obj.objIPObj), this.obj.objIPObj);
		IF this.site.frame # NIL THEN
			this.site.wnd := this.site.frame.rider(HostPorts.Rider).port.wnd;
			style := SYSTEM.VAL(SET, WinApi.GetWindowLongW(this.site.wnd, -16));
			res := WinApi.SetWindowLongW(this.site.wnd, -16, SYSTEM.VAL(INTEGER, style + {25}));
			host := Views.HostOf(this.site.frame);
			IF (host # NIL) & (host.view IS Containers.View) THEN
				c := host.view(Containers.View).ThisController();
				c.SetFocus(this.site.frame.view)
			END
		END;
		res := this.site.OnShowWindow(1);
		RETURN WinApi.S_OK
	END OnInPlaceActivate;

	PROCEDURE (this: IOleInPlaceSite) OnUIActivate (): COM.RESULT;
		VAR f: Views.Frame; p: HostPorts.Port; win: HostWindows.Window;
	BEGIN
		IF debug THEN Log.String("ip site: on ui activate"); Log.Ln END;
		(* remove user interface *)
		f := this.site.frame; p := f.rider(HostPorts.Rider).port;
		win := SYSTEM.VAL(HostWindows.Window, WinApi.GetWindowLongW(p.wnd, 0));
		OleServer.RemoveUI(win);
		RETURN WinApi.S_OK
	END OnUIActivate;

	PROCEDURE (this: IOleInPlaceSite) GetWindowContext (OUT frame: WinOle.IOleInPlaceFrame;
																				OUT doc: WinOle.IOleInPlaceUIWindow;
																				OUT pos, clip: WinApi.RECT;
																				VAR info: WinOle.OLEINPLACEFRAMEINFO): COM.RESULT;
		VAR pwin: IOleInPlaceUIWindow; whk: WinHook; fhk: FrameHook; p: HostPorts.Port; f: Views.Frame;
			w, h: INTEGER; win: HostWindows.Window; outerSite: WinOle.IOleInPlaceSite; r1, r2: WinApi.RECT;
	BEGIN
		IF debug THEN Log.String("ip site: get context"); Log.Ln END;
		IF this.site.frame = NIL THEN RETURN WinApi.E_FAIL END;
		f := this.site.frame; p := f.rider(HostPorts.Rider).port;
		pos.left := f.gx DIV f.unit; pos.top := f.gy DIV f.unit;
(*
		f.view.context.GetSize(w, h);
*)
		pos.right := pos.left + this.obj.rw; pos.bottom := pos.top + this.obj.rh;
		clip.left := 0; clip.top := 0; clip.right := p.w; clip.bottom := p.h;
		win := SYSTEM.VAL(HostWindows.Window, WinApi.GetWindowLongW(p.wnd, 0));
		outerSite := OleServer.ContextOf(win);
		IF outerSite # NIL THEN
			RETURN outerSite.GetWindowContext(frame, doc, r1, r2, info)
		ELSE
			IF appFrame = NIL THEN
				NEW(appFrame);
				NEW(fhk); HostWindows.mainHook := fhk
			END;
			frame := appFrame;
			NEW(pwin); doc := pwin; pwin.wnd := p.wnd;
			NEW(whk); win.hook := whk; whk.iipw := pwin;
			info.fMDIApp := 1;
			info.hwndFrame := HostWindows.main;
			GetAccelTable(info.haccel, info.cAccelEntries);
			RETURN WinApi.S_OK
		END
	END GetWindowContext;

	PROCEDURE (this: IOleInPlaceSite) Scroll (scrollExtent: WinApi.SIZE): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip site: scroll"); Log.Ln END;
		(* scroll document *)
		RETURN WinApi.S_OK
	END Scroll;

	PROCEDURE (d: Deactivator) Do;
		VAR res: COM.RESULT;
	BEGIN
		IF d.obj.objIPObj # NIL THEN
			res := d.obj.objIPObj.InPlaceDeactivate()
		END
	END Do;

	PROCEDURE (this: IOleInPlaceSite) OnUIDeactivate (undoable: WinApi.BOOL): COM.RESULT;
		VAR res: COM.RESULT; win: HostWindows.Window; done: BOOLEAN; d: Deactivator;
	BEGIN
		IF debug THEN Log.String("ip site: ui deactivate"); Log.Ln END;
		(* restore user interface *)
		win := SYSTEM.VAL(HostWindows.Window, WinApi.GetWindowLongW(this.site.wnd, 0));
		OleServer.ResetUI(win, done);
		IF ~done & (appFrame # NIL) THEN
			IF debug THEN Log.String("reset oberon/f user interface"); Log.Ln END;
			res := appFrame.SetMenu(0, 0, 0);
			res := appFrame.SetBorderSpace(NIL)
		END;
		NEW(d); d.obj := this.obj;
		Services.DoLater(d, Services.now);
		RETURN WinApi.S_OK
	END OnUIDeactivate;

	PROCEDURE (this: IOleInPlaceSite) OnInPlaceDeactivate (): COM.RESULT;
		VAR host: Views.Frame; c: Containers.Controller; style: SET; res: INTEGER;
	BEGIN
		res := this.site.OnShowWindow(0);
		IF debug THEN Log.String("ip site: deactivate"); Log.Ln END;
		IF this.site.frame # NIL THEN
			IF ~this.obj.guard THEN
				host := Views.HostOf(this.site.frame);
				IF (host # NIL) & (host.view IS Containers.View) THEN
					c := host.view(Containers.View).ThisController();
					IF debug THEN Log.String("remove focus"); Log.Ln END;
					c.SetFocus(NIL);
					c.SetSingleton(this.site.frame.view)
				END
			END;
			style := SYSTEM.VAL(SET, WinApi.GetWindowLongW(this.site.wnd, -16));
			res := WinApi.SetWindowLongW(this.site.wnd, -16, SYSTEM.VAL(INTEGER, style - {25}))
		END;
		(* minimal undo support *)
		DoVerb(this.obj, NIL, WinOle.OLEIVERB_DISCARDUNDOSTATE);
		this.obj.objIPObj := NIL;
		RETURN WinApi.S_OK
	END OnInPlaceDeactivate;

	PROCEDURE (this: IOleInPlaceSite) DiscardUndoState (): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip site: discard undo"); Log.Ln END;
		(* no undo state *)
		RETURN WinApi.E_NOTIMPL
	END DiscardUndoState;

	PROCEDURE (this: IOleInPlaceSite) DeactivateAndUndo (): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip site: deactivate & undo"); Log.Ln END;
		res := this.obj.objIPObj.InPlaceDeactivate();
		RETURN WinApi.S_OK
	END DeactivateAndUndo;

	PROCEDURE (this: IOleInPlaceSite) OnPosRectChange (IN posRect: WinApi.RECT): COM.RESULT;
		VAR pos, clip: WinApi.RECT; res: COM.RESULT; p: HostPorts.Port; f: Views.Frame; w, h: INTEGER;
			host: Views.Frame; c: Containers.Controller; g: BOOLEAN; v: Views.View; size: WinApi.SIZE;
	BEGIN
		f := this.site.frame;
		IF (f = NIL) OR (f.rider = NIL) THEN RETURN WinApi.E_FAIL END;
		host := Views.HostOf(f); v := f.view;
		p := f.rider(HostPorts.Rider).port;
		IF debug THEN
			Log.String("ip site: on pos rect change");
			Log.Int((posRect.right - posRect.left) * f.unit DIV oleUnit);
			Log.Int((posRect.bottom - posRect.top) * f.unit DIV oleUnit);
			Log.Ln;
			res := this.obj.objObj.GetExtent(WinOle.DVASPECT_CONTENT, size);
			IF res = WinApi.S_OK THEN
				Log.String("   get extent");
				Log.Int(size.cx); Log.Int(size.cy)
			ELSE
				Log.String("   get extent failed")
			END;
			Log.Ln;
			v.context.GetSize(w, h);
			Log.String("   get size");
			Log.Int(w DIV oleUnit); Log.Int(h DIV oleUnit);
			Log.Ln
		END;
		this.obj.rw := posRect.right - posRect.left;
		this.obj.rh := posRect.bottom - posRect.top;
		UpdateSizes(this.site.frame.view(View), TRUE, TRUE);
(*
		g := this.obj.focusGuard; this.obj.focusGuard := TRUE;
		v.context.SetSize((posRect.right - posRect.left) * f.unit, (posRect.bottom - posRect.top) * f.unit);
		IF host # NIL THEN	(* update saved frame *)
			Views.ValidateRoot(Views.RootOf(host));
			f := Views.ThisFrame(host, v);
			this.site.frame := f;
		END;
		IF this.obj.objIPObj # NIL THEN
			pos.left := f.gx DIV f.unit; pos.top := f.gy DIV f.unit;
			v.context.GetSize(w, h);
			pos.right := pos.left + w DIV f.unit; pos.bottom := pos.top + h DIV f.unit;
			clip.left := 0; clip.top := 0; clip.right := p.w; clip.bottom := p.h;
			IF debug THEN Log.String("set object rects"); Log.Ln END;
			res := this.obj.objIPObj.SetObjectRects(pos, clip);
			IF (host # NIL) & (host.view IS Containers.View) THEN
				c := host.view(Containers.View).ThisController();
				c.SetFocus(v)
			END
		END;
		this.obj.focusGuard := g;
*)
		RETURN WinApi.S_OK
	END OnPosRectChange;


	(* ---------- IOleInPlaceUIWindow ---------- *)

	PROCEDURE (this: IOleInPlaceUIWindow) GetWindow (OUT wnd: WinApi.HWND): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip win: get window"); Log.Ln END;
		wnd := this.wnd;
		RETURN WinApi.S_OK
	END GetWindow;

	PROCEDURE (this: IOleInPlaceUIWindow) ContextSensitiveHelp (enter: WinApi.BOOL): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip win: context help"); Log.Ln END;
		IF this.iipao # NIL THEN res := this.iipao.ContextSensitiveHelp(enter) END;
		RETURN WinApi.S_OK
	END ContextSensitiveHelp;

	PROCEDURE (this: IOleInPlaceUIWindow) GetBorder (OUT border: WinApi.RECT): COM.RESULT;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.GetClientRect(this.wnd, border);
		IF debug THEN
			Log.String("ip win: get border");
			Log.Int(border.left); Log.Int(border.top);
			Log.Int(border.right); Log.Int(border.bottom);
			Log.Ln
		END;
		RETURN WinApi.S_OK
	END GetBorder;

	PROCEDURE (this: IOleInPlaceUIWindow) RequestBorderSpace (
		IN width: WinOle.BORDERWIDTHS
	): COM.RESULT;
	BEGIN
		IF debug THEN
			Log.String("ip win: request space");
			Log.Int(width.left); Log.Int(width.top);
			Log.Int(width.right); Log.Int(width.bottom);
			Log.Ln
		END;
		RETURN WinApi.INPLACE_E_NOTOOLSPACE
	END RequestBorderSpace;

	PROCEDURE (this: IOleInPlaceUIWindow) SetBorderSpace (
		IN [nil] width: WinOle.BORDERWIDTHS
	): COM.RESULT;
	BEGIN
		IF debug THEN
			Log.String("ip win: set space");
			IF VALID(width) THEN
				Log.Int(width.left); Log.Int(width.top);
				Log.Int(width.right); Log.Int(width.bottom)
			ELSE Log.String(" nil")
			END;
			Log.Ln
		END;
		IF ~VALID(width) OR (width.left = 0) & (width.top = 0) & (width.right = 0) & (width.bottom = 0) THEN
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.OLE_E_INVALIDRECT
		END
	END SetBorderSpace;

	PROCEDURE (this: IOleInPlaceUIWindow) SetActiveObject (
		obj: WinOle.IOleInPlaceActiveObject; name: WinApi.PtrWSTR
	): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip win: set active obj"); Log.Ln END;
		this.iipao := obj;
		RETURN WinApi.S_OK
	END SetActiveObject;


	(* ---------- WinHook ---------- *)

	PROCEDURE (hk: WinHook) Activate (do: BOOLEAN);
		VAR res: COM.RESULT;
	BEGIN
		IF (hk.iipw # NIL) & (hk.iipw.iipao # NIL) THEN
			IF do THEN res := hk.iipw.iipao.OnDocWindowActivate(1)
			ELSE
				res := hk.iipw.iipao.OnDocWindowActivate(0);
				res := WinApi.SendMessageW(HostWindows.client, 560, HostMenus.menuBar, winMenu);
				res := WinApi.DrawMenuBar(HostWindows.main);
				HostWindows.SetMainBorderWidth(0, 0, 0, 0);
				HostMenus.isCont := FALSE
			END
		END
	END Activate;

	PROCEDURE (hk: WinHook) Focus (do: BOOLEAN);
		VAR res: COM.RESULT; wnd: WinApi.HWND;
	BEGIN
		IF do & (hk.iipw # NIL) & (hk.iipw.iipao # NIL) THEN
			res := hk.iipw.iipao.GetWindow(wnd);
			res := WinApi.SetFocus(wnd)
		END
	END Focus;

	PROCEDURE (hk: WinHook) Resize (w, h: INTEGER);
		VAR res: COM.RESULT; rect: WinApi.RECT;
	BEGIN
		IF (hk.iipw # NIL) & (hk.iipw.iipao # NIL) THEN
			res := WinApi.GetClientRect(hk.iipw.wnd, rect);
			res := hk.iipw.iipao.ResizeBorder(rect, hk.iipw, 0)
		END
	END Resize;


	(* ---------- IOleInPlaceFrame ---------- *)

	PROCEDURE (this: IOleInPlaceFrame) GetWindow (OUT wnd: WinApi.HWND): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip frame: get window"); Log.Ln END;
		wnd := HostWindows.main;
		RETURN WinApi.S_OK
	END GetWindow;

	PROCEDURE (this: IOleInPlaceFrame) ContextSensitiveHelp (enter: WinApi.BOOL): COM.RESULT;
		VAR res: COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip frame: context help"); Log.Ln END;
		IF this.iipao # NIL THEN res := this.iipao.ContextSensitiveHelp(enter) END;
		RETURN WinApi.S_OK
	END ContextSensitiveHelp;

	PROCEDURE (this: IOleInPlaceFrame) GetBorder (OUT border: WinApi.RECT): COM.RESULT;
	BEGIN
		HostWindows.GetMainBorder(border.left, border.top, border.right, border.bottom);
		IF debug THEN
			Log.String("ip frame: get border");
			Log.Int(border.left); Log.Int(border.top);
			Log.Int(border.right); Log.Int(border.bottom);
			Log.Ln
		END;
		RETURN WinApi.S_OK
	END GetBorder;

	PROCEDURE (this: IOleInPlaceFrame) RequestBorderSpace (IN width: WinOle.BORDERWIDTHS): COM.RESULT;
	BEGIN
		IF debug THEN
			Log.String("ip frame: request space");
			Log.Int(width.left); Log.Int(width.top);
			Log.Int(width.right); Log.Int(width.bottom);
			Log.Ln
		END;
		RETURN WinApi.S_OK
	END RequestBorderSpace;

	PROCEDURE (this: IOleInPlaceFrame) SetBorderSpace (IN [nil] width: WinOle.BORDERWIDTHS): COM.RESULT;
	BEGIN
		IF debug THEN
			Log.String("ip frame: set space");
			IF VALID(width) THEN
				Log.Int(width.left); Log.Int(width.top);
				Log.Int(width.right); Log.Int(width.bottom)
			ELSE Log.String(" nil")
			END;
			Log.Ln
		END;
		IF VALID(width) THEN
			HostWindows.SetMainBorderWidth(width.left, width.top, width.right, width.bottom)
		ELSE
			HostWindows.SetMainBorderWidth(0, 0, 0, 0)
		END;
		RETURN WinApi.S_OK
	END SetBorderSpace;

	PROCEDURE (this: IOleInPlaceFrame) SetActiveObject (obj: WinOle.IOleInPlaceActiveObject;
																					name: WinApi.PtrWSTR): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip frame: set active obj"); Log.Ln END;
		this.iipao := obj;
		RETURN WinApi.S_OK
	END SetActiveObject;

	PROCEDURE (this: IOleInPlaceFrame) InsertMenus (menu: WinApi.HMENU;
																	VAR widths: WinOle.OLEMENUGROUPWIDTHS): COM.RESULT;
		VAR res, n, p: INTEGER; m: HostMenus.Menu;
	BEGIN
		IF debug THEN Log.String("ip frame: insert menus"); Log.Ln END;
		m := HostMenus.menus; p := 0;
		WHILE p < 6 DO
			WHILE (m # NIL) & (m.class < p) DO m := m.next END;
			n := 0;
			WHILE (m # NIL) & (m.class = p) DO
				res := WinApi.AppendMenuW(menu, WinApi.MF_POPUP, m.menuH, m.menu);
				m := m.next; INC(n)
			END;
			widths.width[p] := n;
			INC(p, 2)
		END;
		m := HostMenus.menus;
		WHILE (m # NIL) & ~m.isWinMenu DO m := m.next END;
		IF m # NIL THEN winMenu := m.menuH ELSE winMenu := 0 END;
		RETURN WinApi.S_OK
	END InsertMenus;

	PROCEDURE (this: IOleInPlaceFrame) SetMenu (menu: WinApi.HMENU; oleMenu: WinOle.HOLEMENU;
																	activeObj: WinApi.HWND): COM.RESULT;
		VAR res: INTEGER;
	BEGIN
		IF debug THEN Log.String("ip frame: set menu"); Log.Int(menu); Log.Ln END;
		IF menu # 0 THEN
			this.objWnd := activeObj;
			res := WinApi.SendMessageW(HostWindows.client, 560, menu, winMenu);
			HostMenus.isCont := TRUE
		ELSIF this.objWnd # 0 THEN
			this.objWnd := 0;
			res := WinApi.SendMessageW(HostWindows.client, 560, HostMenus.menuBar, winMenu);
			HostMenus.isCont := FALSE
		END;
		res := WinApi.DrawMenuBar(HostWindows.main);
		RETURN WinOle.OleSetMenuDescriptor(oleMenu, HostWindows.main, activeObj, this, this.iipao)
	END SetMenu;

	PROCEDURE (this: IOleInPlaceFrame) RemoveMenus (menu: WinApi.HMENU): COM.RESULT;
		VAR m: HostMenus.Menu; i: INTEGER; sm: WinApi.HMENU; res: INTEGER;
	BEGIN
		IF debug THEN Log.String("ip frame: remove menus"); Log.Ln END;
		i := WinApi.GetMenuItemCount(menu);
		WHILE i > 0 DO
			DEC(i); sm := WinApi.GetSubMenu(menu, i);
			m := HostMenus.menus;
			WHILE (m # NIL) & (m.menuH # sm) DO m := m.next END;
			IF m # NIL THEN res := WinApi.RemoveMenu(menu, i, WinApi.MF_BYPOSITION) END
		END;
		RETURN WinApi.S_OK
	END RemoveMenus;

	PROCEDURE (this: IOleInPlaceFrame) SetStatusText (text: WinApi.PtrWSTR): COM.RESULT;
		VAR res, i: INTEGER; str: ARRAY 256 OF CHAR;
	BEGIN
		IF debug THEN Log.String("ip frame: set status"); Log.Ln END;
		IF Dialog.showsStatus THEN
			IF text # NIL THEN
				i := 0;
				WHILE (i < LEN(str) - 1) & (text[i] # 0X) DO str[i] := text[i]; INC(i) END;
				str[i] := 0X;
				res := WinApi.SetWindowTextW(HostWindows.status, str);
				res := WinApi.UpdateWindow(HostWindows.status)
			END;
			RETURN WinApi.S_OK
		ELSE
			RETURN WinApi.E_FAIL
		END
	END SetStatusText;

	PROCEDURE (this: IOleInPlaceFrame) EnableModeless (enable: WinApi.BOOL): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("ip frame: enable modeless"); Log.Ln END;
		(* enable/disable modeless dialogs *)
		RETURN WinApi.S_OK
	END EnableModeless;

	PROCEDURE (this: IOleInPlaceFrame) TranslateAccelerator (IN msg: WinApi.MSG; id: SHORTINT): COM.RESULT;
		VAR res: COM.RESULT; r: INTEGER; m: WinApi.MSG;
	BEGIN
		IF debug THEN Log.String("ip frame: translate accelerator"); Log.Int(id); Log.Ln END;
		res := WinApi.S_OK;
		IF (id >= 100) & (id < HostMenus.lastId) THEN
			r := WinApi.SendMessageW(HostWindows.main, 273, id + 65536, 0)
		ELSE
			m := msg;
			r := WinApi.TranslateMDISysAccel(HostWindows.client, m);
			IF r = 0 THEN res := WinApi.S_FALSE END
		END;
		RETURN res
	END TranslateAccelerator;


	(* ---------- FrameHook ---------- *)

	PROCEDURE (hk: FrameHook) Activate (do: BOOLEAN);
		VAR res: COM.RESULT;
	BEGIN
		IF (appFrame # NIL) & (appFrame.iipao # NIL) THEN
			IF do THEN res := appFrame.iipao.OnFrameWindowActivate(1)
			ELSE res := appFrame.iipao.OnFrameWindowActivate(0)
			END
		END
	END Activate;

	PROCEDURE (hk: FrameHook) Resize (w, h: INTEGER);
		VAR res: COM.RESULT; rect: WinApi.RECT;
	BEGIN
		IF (appFrame # NIL) & (appFrame.iipao # NIL) THEN
			HostWindows.GetMainBorder(rect.left, rect.top, rect.right, rect.bottom);
			res := appFrame.iipao.ResizeBorder(rect, appFrame, 1)
		END
	END Resize;

	PROCEDURE (hk: FrameHook) Focus (on: BOOLEAN), EMPTY;


	(* ---------- View ---------- *)

	PROCEDURE (v: View) GetNewFrame (VAR frame: Views.Frame);
		VAR f: Frame;
	BEGIN
		NEW(f); frame := f
	END GetNewFrame;

	PROCEDURE InitModel(v: View; m: Model);
	BEGIN
		v.model := m; Stores.Join(v, m);
		IF m.view = NIL THEN m.view := v END;
		OpenLink(m)
	END InitModel;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER; s: Stores.Store;
	BEGIN
		v.Internalize^(rd); IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(minViewVersion, 1 (* maxViewVersion *), thisVersion); IF rd.cancelled THEN RETURN END;
		rd.ReadStore(s); ASSERT(s # NIL, 100); ASSERT(s IS Model, 101);
		InitModel(v, s(Model))
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(maxViewVersion);
		wr.WriteStore(v.model)
	END Externalize;

(*
	PROCEDURE (v: View) PropagateDomain;
	BEGIN
		Stores.InitDomain(v.model, v.Domain())
	END PropagateDomain;
*)

	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		InitModel(v, model(Model))
	END CopyFromModelView;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.model
	END ThisModel;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR res: COM.RESULT; dc: WinApi.HDC; rect, wrect: WinApi.RECT; w, h: INTEGER;
			size: WinApi.SIZE; g: BOOLEAN; p: HostPorts.Port; s: SET; fl, ft, fr, fb: INTEGER;
			n: Notifier;
	BEGIN
		IF ~v.hasNotifier & (v.Domain() # NIL)
			& (v.Domain().GetSequencer() # NIL) & (v.Domain().GetSequencer() IS Sequencers.Sequencer) THEN
			NEW(n); n.model := v.model;
			v.Domain().GetSequencer()(Sequencers.Sequencer).InstallNotifier(n);
			v.hasNotifier := TRUE
		END;
		IF (v.model.objObj # NIL) & (v.model.objView # NIL) & Visible(v.model, f) THEN
			v.context.GetSize(w, h);
			IF (w # v.model.w) OR (h # v.model.h) THEN
				size.cx := w DIV oleUnit; size.cy := h DIV oleUnit;
				g := v.model.guard; v.model.guard := TRUE;
				IF WinOle.OleIsRunning(v.model.objObj) # 0 THEN
					IF debug THEN Log.String("set size (running)"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
					res := v.model.objObj.SetExtent(WinOle.DVASPECT_CONTENT, size)
				ELSIF v.model.flags * WinOle.OLEMISC_RECOMPOSEONRESIZE # {} THEN
					res := WinOle.OleRun(v.model.objUnk);
					IF debug THEN Log.String("set size (recomp)"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
					res := v.model.objObj.SetExtent(WinOle.DVASPECT_CONTENT, size);
					res := v.model.objObj.Update();
					res := v.model.objObj.Close(WinOle.OLECLOSE_SAVEIFDIRTY)
				END;
				v.model.w := w; v.model.h := h;
				v.model.guard := g
			END;
			p := f.rider(HostPorts.Rider).port; dc := p.dc;
			rect.left := f.gx DIV f.unit;
			rect.top := f.gy DIV f.unit;
			rect.right := (f.gx + w) DIV f.unit;
			rect.bottom := (f.gy + h) DIV f.unit;
			wrect.left := 0;
			wrect.top := 0;
			wrect.right := p.w;
			wrect.bottom := p.h;
			res := WinApi.SaveDC(dc);
			IF p.wnd # 0 THEN res := WinApi.SelectClipRgn(dc, 0) END;
			f.rider.GetRect(fl, ft, fr, fb);
			res := WinApi.IntersectClipRect(dc, fl, ft, fr, fb);
			s := WinApi.SetTextAlign(dc, {});
			res := v.model.objView.Draw(WinOle.DVASPECT_CONTENT, -1, 0, NIL, 0, dc, rect, wrect, NIL, 0);
			h := WinApi.RestoreDC(dc, -1);
			IF debug THEN
				IF p.wnd # 0 THEN
					Log.String("draw"); Log.Int(res);
					Log.Int(SYSTEM.VAL(INTEGER, p));
					Log.Int(p.wnd); Log.Int(dc);
					Log.Int(rect.left); Log.Int(rect.top); Log.Int(rect.right); Log.Int(rect.bottom);
					Log.Int(wrect.left); Log.Int(wrect.top); Log.Int(wrect.right); Log.Int(wrect.bottom);
					Log.Ln
				ELSE
					Log.String("draw (p)"); Log.Int(res);
					Log.Int(SYSTEM.VAL(INTEGER, p));
					Log.Int(p.wnd); Log.Int(dc);
					Log.Int(rect.left); Log.Int(rect.top); Log.Int(rect.right); Log.Int(rect.bottom);
					Log.Int(wrect.left); Log.Int(wrect.top); Log.Int(wrect.right); Log.Int(wrect.bottom);
					Log.Ln
				END
			END
		END
	END Restore;

	PROCEDURE (v: View) RestoreMarks (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		IF v.model.open & ((v.model.objIPObj = NIL) OR (v.model.site.frame # f))
				& ~v.model.focusGuard & ~Views.IsPrinterFrame(f) THEN
			f.MarkRect(l, t, r, b, Ports.fill, HostPorts.focusPat, TRUE)
		END
	END RestoreMarks;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
		VAR res: COM.RESULT; size: WinApi.SIZE; ow, oh, w, h: INTEGER;
			c: Containers.Controller; s: Views.View; path: BOOLEAN;
	BEGIN
		WITH msg: UpdateMsg DO
			IF debug THEN Log.String("update"); Log.Ln END;
			UpdateSizes(v, msg.checkSize, FALSE);
(*
			res := v.model.objView.GetExtent(WinOle.DVASPECT_CONTENT, -1, NIL, size);
			IF debug THEN Log.String("update"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
			IF res = WinApi.S_OK THEN
				ow := size.cx * oleUnit; oh := size.cy * oleUnit;
				v.context.GetSize(w, h);
				IF (w # ow) OR (h # oh) THEN
					IF debug THEN Log.String("set size"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
					path := Controllers.path;
					Controllers.SetCurrentPath(Controllers.targetPath);
					c := Containers.Focus();
					Controllers.SetCurrentPath(path);
					s := c.Singleton();
					v.context.SetSize(ow, oh);
					IF c.Singleton() # s THEN c.SetSingleton(s) END;
					v.model.w := ow; v.model.h := oh
				END;
			END;
*)
			Views.Update(v, Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR res: COM.RESULT; g: BOOLEAN;
	BEGIN
		WITH msg: Controllers.MarkMsg DO
			IF msg.focus & ~v.model.focusGuard THEN
				g := v.model.guard; v.model.guard := TRUE;
				IF ~msg.show THEN	(* defocus *)
					IF debug THEN Log.String("defocus"); Log.Ln END;
					IF v.model.objIPObj # NIL THEN
						res := v.model.objIPObj.InPlaceDeactivate()
					END;
(*
				ELSIF msg.show & (v.model.flags * WinOle.OLEMISC_INSIDEOUT # {}) THEN
					DoVerb(v.model, f, WinOle.OLEIVERB_SHOW)
*)
				END;
				v.model.guard := g
			END
		| msg: Controllers.TrackMsg DO
			IF v.model.flags * WinOle.OLEMISC_INSIDEOUT # {} THEN
				DoVerb(v.model, f, WinOle.OLEIVERB_SHOW)
			END
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		VAR size: WinApi.SIZE; res: COM.RESULT; i: INTEGER;
			iev: WinOle.IEnumOLEVERB; ov: ARRAY 1 OF WinOle.OLEVERB; pstr: WinApi.PtrWSTR;
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				res := v.model.objView.GetExtent(WinOle.DVASPECT_CONTENT, -1, NIL, size);
				IF res >= 0 THEN
					IF debug THEN Log.String("get size (prefs)"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
					msg.w := size.cx * (Ports.mm DIV 100);
					msg.h := size.cy * (Ports.mm DIV 100)
				ELSE
					IF debug THEN Log.String("get size (prefs) (no size)"); Log.Ln END;
					msg.w := 60 * Ports.mm;
					msg.h := 60 * Ports.mm
				END
			END
		| msg: Properties.FocusPref DO
			IF v.model.flags * WinOle.OLEMISC_INSIDEOUT # {} THEN
				msg.hotFocus := TRUE
			END
		| msg: Properties.PollMsg DO
			PollProp(v.model, msg.prop)
		| msg: Properties.SetMsg DO
			SetProp(v.model, msg.prop)
		| msg: Properties.PollVerbMsg DO
			res := v.model.objObj.EnumVerbs(iev);
			IF res >= 0 THEN
				REPEAT
					res := iev.Next(1, ov, NIL)
				UNTIL (res # WinApi.S_OK) OR (ov[0].lVerb = msg.verb);
				IF res = WinApi.S_OK THEN
					i := 0;
					IF ov[0].lpszVerbName # NIL THEN
						WHILE ov[0].lpszVerbName[i] # 0X DO msg.label[i] := ov[0].lpszVerbName[i]; INC(i) END
					END;
					msg.label[i] := 0X;
					msg.disabled := 0 IN ov[0].fuFlags;
					msg.checked := 3 IN ov[0].fuFlags
				END
			END
		| msg: Properties.DoVerbMsg DO
			DoVerb(v.model, msg.frame, msg.verb)
		| msg: HostMenus.TypeMsg DO
			res := v.model.objObj.GetUserType(WinOle.USERCLASSTYPE_SHORT, pstr);
			IF res >= 0 THEN
				i := 0;
				WHILE pstr[i] # 0X DO msg.type[i] := pstr[i]; INC(i) END;
				msg.type[i] := 0X;
				WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, pstr))
			END
		ELSE
		END
	END HandlePropMsg;


	(* ---------- Frame ---------- *)

	PROCEDURE (f: Frame) SetOffset (gx, gy: INTEGER);
		VAR obj: Model; pos, clip: WinApi.RECT; w, h: INTEGER; p: HostPorts.Port; res: COM.RESULT;
	BEGIN
		f.SetOffset^(gx, gy);
		obj := f.view(View).model;
		IF (obj.site.frame = f) & (obj.objIPObj # NIL) THEN
			IF debug THEN Log.String("set offset"); Log.Ln END;
			UpdateSizes(f.view(View), FALSE, TRUE)
(*
			IF debug THEN Log.String("set object rects"); Log.Ln END;
			p := f.rider(HostPorts.Rider).port;
			pos.left := f.gx DIV f.unit; pos.top := f.gy DIV f.unit;
			f.view.context.GetSize(w, h);
			pos.right := pos.left + w DIV f.unit; pos.bottom := pos.top + h DIV f.unit;
			clip.left := 0; clip.top := 0; clip.right := p.w; clip.bottom := p.h;
			res := obj.objIPObj.SetObjectRects(pos, clip);
*)
		END
	END SetOffset;

	PROCEDURE (f: Frame) Close;
	BEGIN
		(*f.Close^;*)
		IF f = f.view(View).model.site.frame THEN
			f.view(View).model.site.frame := NIL
		END
	END Close;

	(* ---------- Model ---------- *)

	PROCEDURE Init (model: Model);
		VAR res: COM.RESULT; ilb: WinOle.ILockBytes;
	BEGIN
		NEW(model.site); model.site.obj := model;
		NEW(model.site.ias, model.site); model.site.ias.obj := model; model.site.ias.site := model.site;
		NEW(model.site.iips, model.site); model.site.iips.obj := model; model.site.iips.site := model.site;
		res := WinOle.CreateILockBytesOnHGlobal(0, 1, ilb);
		ASSERT(res >= 0, 100);
		res := WinOle.StgCreateDocfileOnILockBytes(ilb,
			WinOle.STGM_CREATE + WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE, 0, model.stg);
		ASSERT(res >= 0, 101)
	END Init;

	PROCEDURE Setup (model: Model; object: COM.IUnknown);
		VAR res: COM.RESULT; size: WinApi.SIZE; a, b: ARRAY 64 OF CHAR;
	BEGIN
		model.objUnk := object;
		res := object.QueryInterface(COM.ID(model.objView), model.objView);
		ASSERT(res >= 0, 102);
		res := model.objView.SetAdvise(WinOle.DVASPECT_CONTENT, {}, model.site.ias);
		ASSERT(res >= 0, 103);
		res := object.QueryInterface(COM.ID(model.objObj), model.objObj);
		ASSERT(res >= 0, 104);
		res := model.objObj.SetClientSite(model.site);
		ASSERT(res >= 0, 105);
		res := model.objObj.Advise(model.site.ias, model.advConn);
		IF debug THEN Log.String("setup "); Log.Int(model.advConn); Log.Ln END;
(*
		res := WinOle.OleSetContainedObject(object, 1);
		ASSERT(res >= 0, 107);
*)
		a := "BlackBox"; b := "BlackBox Document";
		res := model.objObj.SetHostNames(a, b);
		IF debug & (res < 0) THEN Log.String("set host names"); Log.Int(res); Log.Ln END;
		model.open := FALSE;
		res := model.objObj.GetMiscStatus(WinOle.DVASPECT_CONTENT, model.flags);
		res := model.objView.GetExtent(WinOle.DVASPECT_CONTENT, -1, NIL, size);
		IF debug THEN Log.String("get size (setup)"); Log.Int(size.cx); Log.Int(size.cy); Log.Ln END;
		model.w := size.cx * oleUnit; model.h := size.cy * oleUnit;
(*
		OpenLinks(model)
*)
	END Setup;

	PROCEDURE (n: Notifier) Notify (VAR msg: Sequencers.Message);
		VAR model: Model; res: COM.RESULT;
	BEGIN
		IF msg IS Sequencers.RemoveMsg THEN
			model := n.model;
			Disconnect(model);
			IF debug THEN Log.String("release "); Log.Int(model.advConn); Log.Ln END;
			IF model.objView # NIL THEN
				res := model.objView.SetAdvise(WinOle.DVASPECT_CONTENT, {}, NIL);
				model.objView := NIL
			END;
			IF model.objObj # NIL THEN
				res := model.objObj.Close(WinOle.OLECLOSE_SAVEIFDIRTY);
				res := model.objObj.SetClientSite(NIL);
				res := model.objObj.Unadvise(model.advConn);
				model.objObj := NIL
			END;
			model.objUnk := NIL
		END
	END Notify;

(*
	PROCEDURE (model: Model) PropagateDomain;
		VAR n: Notifier; d: Stores.Domain;
	BEGIN
		OpenLink(model);
		NEW(n); n.model := model; d := model.Domain();
		WITH d: Sequencers.Sequencer DO d.InstallNotifier(n) ELSE END
	END PropagateDomain;
*)

	PROCEDURE (model: Model) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER; stg: WinOle.IStorage; ilb: WinOle.ILockBytes;
			res: COM.RESULT; object: COM.IUnknown;
	BEGIN
		model.Internalize^(rd); IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(minModelVersion, maxModelVersion, thisVersion); IF rd.cancelled THEN RETURN END;
		Init(model);
		IF thisVersion = 1 THEN rd.ReadXString(model.link) END;
		ilb := OleStorage.NewReadILockBytes(rd.rider);
		ASSERT(ilb # NIL);
		res := WinOle.StgOpenStorageOnILockBytes(ilb, NIL,
			WinOle.STGM_DIRECT + WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE, NIL, 0, stg);
		ASSERT(res >= 0, 100);
		res := stg.CopyTo(0, NIL, NIL, model.stg);
		res := WinOle.OleLoad(model.stg, COM.ID(object), model.site, object);
		ASSERT(res >= 0, 101);
		res := ilb.Flush();	(* resynchronise reader *)
		Setup(model, object)
	END Internalize;

	PROCEDURE (model: Model) Externalize (VAR wr: Stores.Writer);
		VAR stg: WinOle.IStorage; res: COM.RESULT; ips: WinOle.IPersistStorage; ilb: WinOle.ILockBytes; g: BOOLEAN;
	BEGIN
		g := model.guard; model.guard := TRUE;
		IF debug THEN Log.String("externalize (m)"); Log.Ln END;
		model.Externalize^(wr);
		IF model.link # "" THEN wr.WriteVersion(1); wr.WriteXString(model.link)
		ELSE wr.WriteVersion(0)
		END;
		ilb := OleStorage.NewWriteILockBytes(wr.rider);
		res := WinOle.StgCreateDocfileOnILockBytes(ilb, WinOle.STGM_CREATE
														+ WinOle.STGM_READWRITE + WinOle.STGM_SHARE_EXCLUSIVE, 0, stg);
		ASSERT(res >= 0, 100);
		IF debug THEN Log.String("externalize (1)"); Log.Ln END;
		res := model.objUnk.QueryInterface(COM.ID(ips), ips);
		ASSERT(res >= 0, 101);
		IF debug THEN Log.String("externalize (2)"); Log.Ln END;
		res := WinOle.OleSave(ips, stg, 0);
		ASSERT(res >= 0, 102);
		IF debug THEN Log.String("externalize (3)"); Log.Ln END;
		res := ilb.Flush();	(* resynchronise writer *)
		IF debug THEN Log.String("externalize (4)"); Log.Ln END;
		res := ips.SaveCompleted(NIL);
		ASSERT(res >= 0, 103);
		model.guard := g;
		IF debug THEN Log.String("externalized (m)"); Log.Ln END
	END Externalize;

	PROCEDURE (model: Model) CopyFrom (source: Stores.Store);
		VAR res: COM.RESULT; ips: WinOle.IPersistStorage; object: COM.IUnknown;
	BEGIN
		WITH source: Model DO
			Init(model);
			res := source.objUnk.QueryInterface(COM.ID(ips), ips);
			ASSERT(res >= 0, 100);
			res := WinOle.OleSave(ips, model.stg, 0);
			ASSERT(res >= 0, 101);
			res := ips.SaveCompleted(NIL);
			ASSERT(res >= 0, 102);
			res := WinOle.OleLoad(model.stg, COM.ID(object), model.site, object);
			model.link := source.link$;
			Setup(model, object)
		END
	END CopyFrom;

(*
	PROCEDURE (model: Model) InitFrom (source: Models.Model);
		VAR res: COM.RESULT; ips: WinOle.IPersistStorage; object: COM.IUnknown;
	BEGIN
		WITH source: Model DO
			Init(model);
			res := source.objUnk.QueryInterface(COM.ID(ips), ips);
			ASSERT(res >= 0, 100);
			res := WinOle.OleSave(ips, model.stg, 0);
			ASSERT(res >= 0, 101);
			res := ips.SaveCompleted(NIL);
			ASSERT(res >= 0, 102);
			res := WinOle.OleLoad(model.stg, COM.ID(object), model.site, object);
			model.link := source.link$;
			Setup(model, object)
		END
	END InitFrom;
*)

	(* ---------- import / export ---------- *)

	PROCEDURE ImportInfo* (VAR med: WinOle.STGMEDIUM; VAR type: Stores.TypeName;
										OUT w, h, rx, ry: INTEGER; OUT isSingle: BOOLEAN);
		VAR hnd: WinApi.HGLOBAL; p: WinOle.PtrOBJECTDESCRIPTOR; res: INTEGER;
	BEGIN
		hnd := MediumGlobal(med);
		p := SYSTEM.VAL(WinOle.PtrOBJECTDESCRIPTOR, WinApi.GlobalLock(hnd));
		type := "OleClient.View";
		w := p.sizel.cx * oleUnit;
		h := p.sizel.cy * oleUnit;
		rx := p.pointl.x * oleUnit;
		ry := p.pointl.x * oleUnit;
		isSingle := TRUE;
		res := WinApi.GlobalUnlock(hnd)
	END ImportInfo;

	PROCEDURE Import* (
		VAR med: WinOle.STGMEDIUM; OUT v: Views.View; OUT w, h: INTEGER; OUT isSingle: BOOLEAN
	);
		VAR cv: View; model: Model; res: COM.RESULT; object: COM.IUnknown;
	BEGIN
		OleServer.Import(med, v, w, h, isSingle);
		IF v = NIL THEN	(* no BlackBox object *)
			NEW(model); Init(model);
			res := WinOle.OleCreateFromData(OleData.dataObj, COM.ID(object),
														WinOle.OLERENDER_DRAW, NIL, model.site, model.stg, object);
			ASSERT(res >= 0, 100);
			Setup(model, object);
			NEW(cv); InitModel(cv, model);
			v := cv; w := 0; h := 0; isSingle := TRUE
		END
	END Import;

	PROCEDURE ExportInfo* (
		v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
	);
		VAR hnd: WinApi.HGLOBAL; p: WinOle.PtrOBJECTDESCRIPTOR; id: COM.GUID; stat: SET;
			res, size, nlen, slen: INTEGER; name, source: ARRAY 256 OF CHAR; sp: WinApi.PtrWSTR;
	BEGIN
		ASSERT(med.tymed = {}, 20);
		WITH v: View DO
			res := v.model.objObj.GetUserClassID(id);
			res := v.model.objObj.GetMiscStatus(WinOle.DVASPECT_CONTENT, stat);
			res := v.model.objObj.GetUserType(WinOle.USERCLASSTYPE_FULL, sp)
		ELSE
			id := ObjectID;
			stat := miscStatus;
			res := WinOle.OleRegGetUserType(ObjectID, WinOle.USERCLASSTYPE_FULL, sp)
		END;
		IF sp # NIL THEN
			name := sp^$;
			WinOle.CoTaskMemFree(SYSTEM.VAL(WinApi.PtrVoid, sp))
		ELSE name := ""
		END;
		nlen := 0; slen := 0;
		WHILE name[nlen] # 0X DO INC(nlen) END;
		nlen := 2 * (nlen + 1);
		WHILE Dialog.appName[slen] # 0X DO source[slen] := Dialog.appName[slen]; INC(slen) END;
		source[slen] := 0X;
		slen := 2 * (slen + 1);
		size := 52 + nlen + slen;
		hnd := WinApi.GlobalAlloc(WinApi.GMEM_DDESHARE + WinApi.GMEM_MOVEABLE, size);
		IF hnd # 0 THEN
			p := SYSTEM.VAL(WinOle.PtrOBJECTDESCRIPTOR, WinApi.GlobalLock(hnd));
			p.cbSize := size;
			p.clsid := id;
			p.dwDrawAspect := WinOle.DVASPECT_CONTENT;
			p.sizel.cx := w DIV oleUnit;
			p.sizel.cy := h DIV oleUnit;
			p.pointl.x := x DIV oleUnit;
			p.pointl.y := y DIV oleUnit;
			p.dwStatus := stat;
			p.dwFullUserTypeName := 52;
			p.dwSrcOfCopy := 52 + nlen;
			SYSTEM.MOVE(SYSTEM.ADR(name), SYSTEM.ADR(p^) + 52, nlen);
			SYSTEM.MOVE(SYSTEM.ADR(source), SYSTEM.ADR(p^) + 52 + nlen, slen);
			res := WinApi.GlobalUnlock(hnd);
			GenGlobalMedium(hnd, NIL, med)
		END
	END ExportInfo;

	PROCEDURE Export* (v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM);
		VAR stg: WinOle.IStorage; res: COM.RESULT; ilb: WinOle.ILockBytes; ips: WinOle.IPersistStorage;
	BEGIN
		WITH v: View DO
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
			res := v.model.objUnk.QueryInterface(COM.ID(ips), ips);
			ASSERT(res >= 0, 112);
			res := WinOle.OleSave(ips, stg, 0);
			ASSERT(res >= 0, 113);
			res := ips.SaveCompleted(NIL);
			ASSERT(res >= 0, 114)
		END
	END Export;


	(* ---------- commands ---------- *)

	PROCEDURE InsertObject*;
		VAR v: View; model: Model; res: COM.RESULT; pmfp: WinApi.PtrMETAFILEPICT;
			p: WinOleDlg.OLEUIINSERTOBJECTW; w: HostWindows.Window;
			fname: ARRAY 260 OF CHAR; guids: ARRAY 1 OF COM.GUID; object: COM.IUnknown;
			c: Containers.Controller; f: Views.Frame; v1: Views.View;
	BEGIN
		NEW(model); Init(model);
		w := HostWindows.dir.First();
		guids[0] := ObjectID;
		p.cbStruct := SIZE(WinOleDlg.OLEUIINSERTOBJECTW);
		p.dwFlags := WinOleDlg.IOF_DISABLELINK + WinOleDlg.IOF_SELECTCREATENEW
						+ WinOleDlg.IOF_CREATENEWOBJECT + WinOleDlg.IOF_CREATEFILEOBJECT
						+ WinOleDlg.IOF_DISABLEDISPLAYASICON;
(*
						+ WinOleDlg.IOF_SHOWINSERTCONTROL;
*)
		p.hWndOwner := w.wnd;
		p.lpszCaption := NIL;
		p.lpfnHook := NIL;
		p.lCustData := 0;
		p.hInstance := 0;
		p.lpszTemplate := NIL;
		p.hResource := 0;
		p.lpszFile := fname;
		p.cchFile := LEN(fname);
		p.cClsidExclude := LEN(guids);
		p.lpClsidExclude := guids;
		p.iid := COM.ID(object);
		p.oleRender := WinOle.OLERENDER_DRAW;
		p.lpFormatEtc := NIL;
		p.lpIOleClientSite := model.site;
		p.lpIStorage := model.stg;
		p.ppvObj := SYSTEM.ADR(object);
		p.sc := WinApi.S_OK;
		p.hMetaPict := 0;
		res := WinOleDlg.OleUIInsertObjectW(p);
		IF res = WinOleDlg.OLEUI_OK THEN
			IF p.hMetaPict # 0 THEN
				pmfp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(p.hMetaPict));
				res := WinApi.DeleteMetaFile(pmfp.hMF);
				res := WinApi.GlobalUnlock(p.hMetaPict);
				res := WinApi.GlobalFree(p.hMetaPict)
			END;
			Setup(model, object);
			NEW(v); InitModel(v, model);
			Controllers.PasteView(v, Views.undefined, Views.undefined, FALSE);
(*
			Windows.dir.Update(w);
*)
			c := Containers.Focus();
			c.GetFirstView(FALSE, v1);
			WHILE (v1 # NIL) & (~(v1 IS View) OR (v1(View).model # model)) DO c.GetNextView(FALSE, v1) END;
			IF v1 # NIL THEN
				v := v1(View);
				c.SetSingleton(v);
				f := Controllers.FocusFrame();
				Views.ValidateRoot(Views.RootOf(f));
				f := Views.ThisFrame(f, v);
				IF debug THEN Log.String("Object created ("); Log.Int(SYSTEM.VAL(INTEGER, f)); Log.Char(")"); Log.Ln
				END;
				DoVerb(model, f, WinOle.OLEIVERB_SHOW)
			END
		ELSIF res # WinOleDlg.OLEUI_CANCEL THEN
		IF debug THEN
			Log.String("Object creation failed ("); Log.Int(res); Log.Char(")");
			IF res = WinOleDlg.OLEUI_IOERR_SCODEHASERROR THEN Log.String(" ("); Log.Int(p.sc); Log.Char(")")
			END;
			Log.Ln
		END
		END
	END InsertObject;

	PROCEDURE PasteSpecial*;
		VAR res: INTEGER; p: WinOleDlg.OLEUIPASTESPECIALW; win: HostWindows.Window;
			guids: ARRAY 1 OF COM.GUID; pmfp: WinApi.PtrMETAFILEPICT;
			entries: ARRAY 16 OF WinOleDlg.OLEUIPASTEENTRYW; key: Dialog.String;
			conv: ARRAY 16 OF OleData.Converter; str: ARRAY 16, 2, 64 OF CHAR;
			c: OleData.Converter; n: INTEGER; msg: Controllers.EditMsg;
	BEGIN
		win := HostWindows.dir.First();
		guids[0] := ObjectID;
		n := 0; c := OleData.convList;
		WHILE (c # NIL) & (n < LEN(entries)) DO
			IF (c.imp # "") & ~(OleData.info IN c.opts) THEN
				entries[n].fmtetc := c.format;
				IF c.type # "" THEN
					key := "#Host:" + c.imp; Dialog.MapString(key, key); str[n, 0] := key$;
					key := "#Host:" + c.type; Dialog.MapString(key, key); str[n, 1] := key$;
					entries[n].lpstrFormatName := str[n, 0];
					entries[n].lpstrResultText := str[n, 1]
				ELSE
					entries[n].lpstrFormatName := "%s";
					entries[n].lpstrResultText := "%s"
				END;
				entries[n].dwFlags := WinOleDlg.OLEUIPASTE_PASTEONLY;
				conv[n] := c; INC(n)
			END;
			c := c.next
		END;
		p.cbStruct := SIZE(WinOleDlg.OLEUIPASTESPECIALW);
		p.dwFlags := WinOleDlg.PSF_DISABLEDISPLAYASICON;
		p.hWndOwner := win.wnd;
		p.lpszCaption := NIL;
		p.lpfnHook := NIL;
		p.lCustData := 0;
		p.hInstance := 0;
		p.lpszTemplate := NIL;
		p.hResource := 0;
		p.lpSrcDataObj := NIL;
		p.arrPasteEntries := SYSTEM.ADR(entries[0]);
		p.cPasteEntries := n;
		p.arrLinkTypes := NIL;
		p.cLinkTypes := 0;
		p.cClsidExclude := LEN(guids);
		p.lpClsidExclude := guids;
		p.hMetaPict := 0;
		res := WinOleDlg.OleUIPasteSpecialW(p);
		IF res # WinOleDlg.OLEUI_CANCEL THEN
			ASSERT(res = WinOleDlg.OLEUI_OK, 100);
			ASSERT((p.nSelectedIndex >= 0) & (p.nSelectedIndex < n), 101);
			OleData.GetDataViewUsing(
				p.lpSrcDataObj, conv[p.nSelectedIndex], msg.view, msg.w, msg.h, msg.isSingle);
			IF (msg.view = NIL) & (p.nSelectedIndex + 1 < n)
				& (conv[p.nSelectedIndex].imp = conv[p.nSelectedIndex + 1].imp)
			THEN
				OleData.GetDataViewUsing(
					p.lpSrcDataObj, conv[p.nSelectedIndex + 1], msg.view, msg.w, msg.h, msg.isSingle)
			END;
			IF msg.view # NIL THEN
				msg.op := Controllers.paste; msg.clipboard := TRUE;
				Controllers.Forward(msg)
			END;
			IF p.hMetaPict # 0 THEN
				pmfp := SYSTEM.VAL(WinApi.PtrMETAFILEPICT, WinApi.GlobalLock(p.hMetaPict));
				res := WinApi.DeleteMetaFile(pmfp.hMF);
				res := WinApi.GlobalUnlock(p.hMetaPict);
				res := WinApi.GlobalFree(p.hMetaPict)
			END
		END
	END PasteSpecial;

	PROCEDURE NewView* (clsid: COM.GUID): Views.View;
		VAR unk: COM.IUnknown; res: COM.RESULT; m: Model; v: View;
	BEGIN
		NEW(m); Init(m);
		res := WinOle.OleCreate(clsid, COM.ID(unk), WinOle.OLERENDER_DRAW, NIL, m.site, m.stg, unk);
		IF res = WinApi.S_OK THEN
			Setup(m, unk);
			NEW(v); InitModel(v, m); RETURN v
		ELSIF debug THEN Log.String("NewView: "); Log.Int(res); Log.Ln
		END;
		RETURN NIL
	END NewView;
(*
	PROCEDURE NewViewFrom* (unk: COM.IUnknown): Views.View;
		VAR res: COM.RESULT; ips: WinOle.IPersistStorage; new: COM.IUnknown; m: Model; v: View;
	BEGIN
		res := unk.QueryInterface(COM.ID(ips), ips);
		IF res = WinApi.S_OK THEN
			NEW(m); Init(m);
			res := WinOle.OleSave(ips, m.stg, 0);
			ASSERT(res >= 0, 100);
			res := ips.SaveCompleted(NIL);
			ASSERT(res >= 0, 101);
			res := WinOle.OleLoad(m.stg, COM.ID(new), m.site, new);
			Setup(m, new);
			NEW(v); InitModel(v, m); RETURN v
		END;
		RETURN NIL
	END NewViewFrom;
*)
	PROCEDURE NewViewFrom* (unk: COM.IUnknown): Views.View;
		VAR res: COM.RESULT; dobj: WinOle.IDataObject; new: COM.IUnknown; m: Model; v: View;
	BEGIN
		res := unk.QueryInterface(COM.ID(dobj), dobj);
		IF res = WinApi.S_OK THEN
			NEW(m); Init(m);
			res := WinOle.OleCreateFromData(
				dobj, COM.ID(new), WinOle.OLERENDER_DRAW, NIL, m.site, m.stg, new);
			IF res >= 0 THEN
				Setup(m, new);
				NEW(v); InitModel(v, m); RETURN v
			END
		END;
		RETURN NIL
	END NewViewFrom;

	PROCEDURE NewViewFromCB* (): Views.View;
		VAR res: COM.RESULT; dobj: WinOle.IDataObject; new: COM.IUnknown; m: Model; v: View;
	BEGIN
		res := WinOle.OleGetClipboard(dobj);
		IF res >= 0 THEN
			NEW(m); Init(m);
			res := WinOle.OleCreateFromData(
				dobj, COM.ID(new), WinOle.OLERENDER_DRAW, NIL, m.site, m.stg, new);
			IF res >= 0 THEN
				Setup(m, new);
				NEW(v); InitModel(v, m); RETURN v
			END
		END;
		RETURN NIL
	END NewViewFromCB;

	PROCEDURE IUnknown* (v: Views.View): COM.IUnknown;
	BEGIN
		IF v IS View THEN RETURN v(View).model.objUnk
		ELSE RETURN NIL
		END
	END IUnknown;


	PROCEDURE TranslateOleKeys (VAR msg: WinApi.MSG; VAR done: BOOLEAN);
		VAR res: COM.RESULT;
	BEGIN
		IF (appFrame # NIL) & (appFrame.iipao # NIL) THEN
			res := appFrame.iipao.TranslateAccelerator(msg);
			IF res = WinApi.S_OK THEN done := TRUE END
		END
	END TranslateOleKeys;

BEGIN
	HostMenus.TranslateOleKeys1 := TranslateOleKeys
END OleClient.
