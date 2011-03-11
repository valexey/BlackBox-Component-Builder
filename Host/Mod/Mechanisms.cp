MODULE HostMechanisms;
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

	IMPORT COM,
		WinApi, WinOle, OleData, 
		Services, Ports, Stores, Views, Controllers, Properties,
		Dialog, Mechanisms, Containers, Documents, Windows,
		HostPorts;


	CONST
		handleSize = Ports.point * 11 DIV 2;
		clipMiddleHandle = handleSize DIV 2 + 2 * Ports.point;

		escape = -2;
		
		fixed = 31;	(* controller option *)


	TYPE
		IDropSource = POINTER TO RECORD (WinOle.IDropSource)
			key: SET
		END;
		
		IDropTarget = POINTER TO RECORD (WinOle.IDropTarget)
			win: Windows.Window;
			wnd: WinApi.HWND;
			x, y: INTEGER;
			key, effect: SET;
			source: Views.Frame;
			srcX, srcY: INTEGER;
			type: Stores.TypeName;
			isSingle: BOOLEAN;
			w, h, rx, ry: INTEGER
		END;
		
		Hook = POINTER TO RECORD (Mechanisms.Hook) END;
		
	
	VAR
		sourceFrame: Views.Frame;	(* source of drag & drop *)
		sourceX, sourceY: INTEGER;
		targetFrame: Views.Frame;	(* target of drag & drop *)
		targetX, targetY: INTEGER;
		dropView: Views.View;	(* view to drop *)
		isSingleton: BOOLEAN;
		dropW, dropH: INTEGER;
		relX, relY: INTEGER;
		
	
	(** focus borders **)

	PROCEDURE Fixed (host: Views.Frame; v: Views.View): BOOLEAN;
		VAR sp: Properties.ResizePref; c: Containers.Controller;
	BEGIN
		c := host.view(Containers.View).ThisController();
		IF c.opts * {Containers.noCaret, Documents.pageWidth..Documents.winHeight, fixed} # {} THEN
			RETURN TRUE
		END;
		sp.fixed := FALSE; Views.HandlePropMsg(v, sp); RETURN sp.fixed
	END Fixed;

	PROCEDURE PaintFocusBorder (f: Views.Frame; focus: Views.View; l, t, r, b: INTEGER);
		VAR u, s,  w, h,  mx, my,  l0, t0, r0, b0: INTEGER;
		
		PROCEDURE PaintHandle (x, y: INTEGER; l, t, r, b: BOOLEAN);
		BEGIN
			IF l THEN f.DrawRect(x - u, y, x, y + s, Ports.fill, Ports.background) END;
			IF t THEN f.DrawRect(x, y - u, x + s, y, Ports.fill, Ports.background) END;
			IF r THEN f.DrawRect(x + s, y, x + s + u, y + s, Ports.fill, Ports.background) END;
			IF b THEN f.DrawRect(x, y + s, x + s, y + s + u, Ports.fill, Ports.background) END;
			f.DrawRect(x, y, x + s, y + s, Ports.fill, Ports.defaultColor)
		END PaintHandle;
		
	BEGIN
		f.rider.GetRect(l0, t0, r0, b0);
		s := (handleSize - f.dot) DIV f.unit;
		f.rider.SetRect(l0 - s, t0 - s, r0 + s, b0 + s);
		u := f.dot; s := s * f.unit;
		w := r - l; h := b - t;
		f.DrawRect(l, t - s, r, t, Ports.fill, Ports.background);
		f.DrawRect(l, b, r, b + s, Ports.fill, Ports.background);
		f.DrawRect(l - s, t - s, l, b + s, Ports.fill, Ports.background);
		f.DrawRect(r, t - s, r + s, b + s, Ports.fill, Ports.background);
		DEC(s, u);
		f.MarkRect(l, t - s, r, t, Ports.fill, HostPorts.focusPat, Ports.show);
		f.MarkRect(l, b, r, b + s, Ports.fill, HostPorts.focusPat, Ports.show);
		f.MarkRect(l - s, t - s, l, b + s, Ports.fill, HostPorts.focusPat, Ports.show);
		f.MarkRect(r, t - s, r + s, b + s, Ports.fill, HostPorts.focusPat, Ports.show);
		IF ~Fixed(f, focus) THEN
			PaintHandle(l - s, t - s, FALSE, FALSE, TRUE, TRUE);
			PaintHandle(r, t - s, TRUE, FALSE, FALSE, TRUE);
			PaintHandle(l - s, b, FALSE, TRUE, TRUE, FALSE);
			PaintHandle(r, b, TRUE, TRUE, FALSE, FALSE);
			IF w > 2 * clipMiddleHandle THEN
				mx := (l + r - s) DIV 2;
				PaintHandle(mx, t - s, TRUE, FALSE, TRUE, FALSE);
				PaintHandle(mx, b, TRUE, FALSE, TRUE, FALSE)
			END;
			IF h > 2 * clipMiddleHandle THEN
				my := (t + b - s) DIV 2;
				PaintHandle(l - s, my, FALSE, TRUE, FALSE, TRUE);
				PaintHandle(r, my, FALSE, TRUE, FALSE, TRUE)
			END
		END;
		f.DrawRect(l - u, t - u, r + u, b + u, u, Ports.defaultColor);
		f.rider.SetRect(l0, t0, r0, b0)
	END PaintFocusBorder;

	PROCEDURE (hook: Hook) FocusBorderCursor* (f: Views.Frame; view: Views.View; l, t, r, b: INTEGER;
									x, y: INTEGER): INTEGER;
		VAR s, u, w, h, mx, my, cursor: INTEGER;
		
		PROCEDURE CheckHandle (x0, y0: INTEGER; c: INTEGER);
		BEGIN
			IF (x >= x0 - u) & (x <= x0 + s) & (y >= y0 - u) & (y <= y0 + s) THEN
				cursor := c
			END
		END CheckHandle;
		
	BEGIN
		u := f.dot; s := handleSize - 2 * u;
		IF (x < l - s) OR (x > r + s) OR (y < t - s) OR (y > b + s) THEN
			cursor := Mechanisms.outside
		ELSE
			cursor := Mechanisms.inside
		END;
		w := r - l; h := b - t;
		IF ~Fixed(f, view) THEN
			CheckHandle(l - s, t - s, HostPorts.resizeLCursor);
			CheckHandle(r, t - s, HostPorts.resizeRCursor);
			CheckHandle(l - s, b, HostPorts.resizeRCursor);
			CheckHandle(r, b, HostPorts.resizeLCursor);
			IF w > 2 * clipMiddleHandle THEN
				mx := (l + r - s) DIV 2;
				CheckHandle(mx, t - s, HostPorts.resizeVCursor);
				CheckHandle(mx, b, HostPorts.resizeVCursor)
			END;
			IF h > 2 * clipMiddleHandle THEN
				my := (t + b - s) DIV 2;
				CheckHandle(l - s, my, HostPorts.resizeHCursor);
				CheckHandle(r, my, HostPorts.resizeHCursor)
			END
		END;
		RETURN cursor
	END FocusBorderCursor;

	PROCEDURE RestoreBorderArea (f: Views.Frame; l, t, r, b: INTEGER);
	(* restore area under destructive border mark *)
		VAR g: Views.RootFrame; s, dx, dy: INTEGER;
	BEGIN
		g := Views.RootOf(f);
		dx := f.gx - g.gx; dy := f.gy - g.gy;
		s := (handleSize - f.dot) DIV f.unit * f.unit;
		INC(l, dx); INC(t, dy); INC(r, dx); INC(b, dy);
(*
		Views.UpdateRoot(g, l - s, t - s, r + s, b + s, FALSE);
*)
		Views.ValidateRoot(g);
		Views.RestoreRoot(g, l - s, t - s, r + s, b + s);
(*
		Views.RestoreRoot(g, l - s, t - s, r + s, t);
		Views.RestoreRoot(g, l - s, t, l, b);
		Views.RestoreRoot(g, r, t, r + s, b);
		Views.RestoreRoot(g, l - s, b, r + s, b + s)
*)
	END RestoreBorderArea;
	
	PROCEDURE (hook: Hook) MarkFocusBorder* (
		host: Views.Frame; focus: Views.View; l, t, r, b: INTEGER; show: BOOLEAN
	);
	BEGIN
		IF focus # NIL THEN
			IF show THEN
				PaintFocusBorder(host, focus, l, t, r, b)
			ELSE
				RestoreBorderArea(host, l, t, r, b)
			END
		END
	END MarkFocusBorder;
	

	(** selection borders **)

	PROCEDURE PaintSelBorder (f: Views.Frame; view: Views.View; l, t, r, b: INTEGER);
		VAR u, d, w, h,  mx, my, l0, t0, r0, b0: INTEGER; sizeable: BOOLEAN;
		
		PROCEDURE PaintHandle (x, y: INTEGER);
			VAR s: INTEGER; ci, co: Ports.Color;
		BEGIN
			DEC(x, d); DEC(y, d); s := d * 2 + u;
			IF sizeable THEN ci := HostPorts.selBackground; co := HostPorts.selTextCol
			ELSE ci := HostPorts.selTextCol; co := HostPorts.selBackground
			END;
			f.DrawRect(x, y, x + s, y + s, Ports.fill, co);
			INC(x, u); INC(y, u); DEC(s, 2 * u);
			f.DrawRect(x, y, x + s, y + s, Ports.fill, ci);
(*			
			f.DrawRect(x, y, x + s, y + s, Ports.fill, ci);
			f.DrawRect(x, y, x + s, y + s, 0, co)
*)
		END PaintHandle;
		
	BEGIN
		d := (handleSize - f.dot) DIV f.unit DIV 2;
		f.rider.GetRect(l0, t0, r0, b0);
		f.rider.SetRect(l0 - d - 1, t0 - d - 1, r0 + d + 1, b0 + d + 1);
		d := d * f.unit; u := f.dot;
		w := r - l; h := b - t; sizeable := ~Fixed(f, view);
		DEC(l, u); DEC(t, u);
(*
		f.SaveRect(l - d, t - d, r + u + d, b + u + d, res);
*)
		f.DrawRect(l, t, r + u, b + u, u, HostPorts.selBackground);
		IF f.front THEN
			IF (w > clipMiddleHandle) & (h > clipMiddleHandle) THEN
				PaintHandle(l, t);
				PaintHandle(r, t);
				PaintHandle(l, b);
				PaintHandle(r, b);
				IF w > 2 * clipMiddleHandle THEN
					mx := (l + r) DIV 2;
					PaintHandle(mx, t);
					PaintHandle(mx, b)
				END;
				IF h > 2 * clipMiddleHandle THEN
					my := (t + b) DIV 2;
					PaintHandle(l, my);
					PaintHandle(r, my)
				END
			ELSIF sizeable THEN
				PaintHandle(r, b)
			END
		END;
		f.rider.SetRect(l0, t0, r0, b0)
	END PaintSelBorder;

	PROCEDURE (hook: Hook) SelBorderCursor* (f: Views.Frame; view: Views.View; l, t, r, b: INTEGER;
									x, y: INTEGER): INTEGER;
		VAR d, u, w, h, mx, my, cursor: INTEGER;
		
		PROCEDURE CheckHandle (x0, y0: INTEGER; c: INTEGER);
		BEGIN
			IF (x >= x0 - d) & (x <= x0 + d) & (y >= y0 - d) & (y <= y0 + d) THEN
				cursor := c
			END
		END CheckHandle;
		
	BEGIN
		IF (x < l) OR (x > r) OR (y < t) OR (y > b) THEN cursor := Mechanisms.outside
		ELSE cursor := Mechanisms.inside
		END;
		IF (view # NIL) & ~Fixed(f, view) THEN
			d := (handleSize - f.dot) DIV f.unit DIV 2 * f.unit;
			w := r - l; h := b - t; u := f.dot;
			DEC(l, u); DEC(t, u);
			IF (w > clipMiddleHandle) & (h > clipMiddleHandle) THEN
				CheckHandle(l, t, HostPorts.resizeLCursor);
				CheckHandle(r, t, HostPorts.resizeRCursor);
				CheckHandle(l, b, HostPorts.resizeRCursor);
				CheckHandle(r, b, HostPorts.resizeLCursor);
				IF w > 2 * clipMiddleHandle THEN
					mx := (l + r) DIV 2;
					CheckHandle(mx, t, HostPorts.resizeVCursor);
					CheckHandle(mx, b, HostPorts.resizeVCursor)
				END;
				IF h > 2 * clipMiddleHandle THEN
					my := (t + b) DIV 2;
					CheckHandle(l, my, HostPorts.resizeHCursor);
					CheckHandle(r, my, HostPorts.resizeHCursor)
				END
			ELSE
				CheckHandle(r, b, HostPorts.resizeLCursor)
			END
		END;
		RETURN cursor
	END SelBorderCursor;

	PROCEDURE RestoreViewArea (f: Views.Frame; l, t, r, b: INTEGER);
	(* restore area under destructive selection mark *)
		VAR g: Views.RootFrame; d, dx, dy: INTEGER;
	BEGIN
(*
		d := (handleSize - f.dot) DIV f.unit DIV 2 * f.unit + f.dot;
		f.RestoreRect(l - d, t - d, r + d, b + d, TRUE)
*)
		g := Views.RootOf(f);
		dx := f.gx - g.gx; dy := f.gy - g.gy;
		d := (handleSize - f.dot) DIV f.unit DIV 2 * f.unit + f.dot;
		INC(l, dx); INC(t, dy); INC(r, dx); INC(b, dy);
		Views.ValidateRoot(g);
		Views.RestoreRoot(g, l - d, t - d, r + d, b + d)
	END RestoreViewArea;

	PROCEDURE (hook: Hook) MarkSingletonBorder* (
		host: Views.Frame; view: Views.View; l, t, r, b: INTEGER; show: BOOLEAN
	);
	BEGIN
		IF view # NIL THEN
			IF show THEN
				PaintSelBorder(host, view, l, t, r, b)
			ELSE
				RestoreViewArea(host, l, t, r, b)
			END
		END
(*		
		IF view # NIL THEN InvertSelBorder(host, view, l, t, r, b, show) END
*)
	END MarkSingletonBorder;


(*
	PROCEDURE MarkBorder* (host: Ports.Frame; view: Stores.Store; l, t, r, b: INTEGER);
		VAR s: INTEGER;
	BEGIN
		IF view # NIL THEN 
			s := markBorderSize * host.dot;
			host.MarkRect(l - s, t - s, r + s, b + s, s, Ports.dim50, Ports.show)
		END
	END MarkBorder;
*)

	PROCEDURE (hook: Hook) TrackToResize* (host: Views.Frame; view: Views.View;
										minW, maxW, minH, maxH: INTEGER;
										VAR l, t, r, b: INTEGER; VAR op: INTEGER;
										VAR x, y: INTEGER; VAR buttons: SET);
		VAR isDown: BOOLEAN; m: SET; p: Properties.SizePref;
			x1, y1,  dx, dy,  dl, dt, dr, db,  l0, t0, r0, b0,  l1, t1, r1, b1,  w, h,  dw, dh: INTEGER;
	BEGIN
		l0 := l; t0 := t; r0 := r; b0 := b;  dl := 0; dt := 0; dr := 0; db := 0;
		x1 := (l + r) DIV 2; y1 := (t + b) DIV 2;
		IF (r - l <= 2 * clipMiddleHandle) OR (ABS(x - x1) > handleSize DIV 2) THEN
			IF x < x1 THEN dl := 1 ELSE dr := 1 END
		END;
		IF (b - t <= 2 * clipMiddleHandle) OR (ABS(y - y1) > handleSize DIV 2) THEN
			IF y < y1 THEN dt := 1 ELSE db := 1 END
		END;
		IF (Controllers.extend IN buttons) & (dl # dr) THEN dl := 1; dr := 1 END;
		IF (Controllers.extend IN buttons) & (dt # db) THEN dt := 1; db := 1 END;
		host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.show);
		REPEAT
			host.Input(x1, y1, m, isDown);
			IF x1 < host.l THEN x1 := host.l ELSIF x1 > host.r THEN x1 := host.r END;
			IF y1 < host.t THEN y1 := host.t ELSIF y1 > host.b THEN y1 := host.b END;
			dx := x1 - x; dy := y1 - y;
			l1 := l0 + dl * dx; t1 := t0 + dt * dy; r1 := r0 + dr * dx; b1 := b0 + db * dy;
			w := r1 - l1; h := b1 - t1;
			IF (w > 0) & (h > 0) THEN
				p.fixedH := (dl = 0) & (dr = 0); p.fixedW := (dt = 0) & (db = 0);
				p.w := w; p.h := h; Views.HandlePropMsg(view, p); w := p.w; h := p.h;
				IF w < minW THEN w := minW ELSIF w > maxW THEN w := maxW END;
				IF h < minH THEN h := minH ELSIF h > maxH THEN h := maxH END;
				dw := w - (r1 - l1); dh := h - (b1 - t1);
				DEC(l1, dl * dw); DEC(t1, dt * dh);
				IF (dl + dr = 0) & (dw # 0) THEN INC(r1, dw) ELSE INC(r1, dr * dw) END;
				IF (dt + db = 0) & (dh # 0) THEN INC(b1, dh) ELSE INC(b1, db * dh) END;
				IF (l1 # l) OR (t1 # t) OR (r1 # r) OR (b1 # b) THEN
					host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.hide);
					l := l1; t := t1; r := r1; b := b1;
					host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.show)
				END
			END
		UNTIL ~isDown;
		host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.hide);
		x := x1; y := y1; buttons := {};
		IF (l # l0) OR (t # t0) OR (r # r0) OR (b # b0) THEN op := Mechanisms.resize
		ELSE op := Mechanisms.cancelResize
		END
	END TrackToResize;

	
	(* IDropSource *)
	
	PROCEDURE (this: IDropSource) QueryContinueDrag (
		escapePressed: WinApi.BOOL; keyState: SET
	): COM.RESULT;
	BEGIN
		IF this.key = {} THEN
			this.key := keyState * (WinApi.MK_LBUTTON + WinApi.MK_MBUTTON + WinApi.MK_RBUTTON)
		END;
		IF escapePressed # 0 THEN RETURN WinApi.DRAGDROP_S_CANCEL
		ELSIF keyState * this.key = {} THEN RETURN WinApi.DRAGDROP_S_DROP
		ELSE RETURN WinApi.S_OK
		END
	END QueryContinueDrag;
	
	PROCEDURE (this: IDropSource) GiveFeedback (effect: SET): COM.RESULT;
	BEGIN
		RETURN WinApi.DRAGDROP_S_USEDEFAULTCURSORS
	END GiveFeedback;
	

	(* IDropTarget *)

	PROCEDURE InstallDropTarget* (wnd: WinApi.HWND; win: Windows.Window);
		VAR drop: IDropTarget; res: COM.RESULT;
	BEGIN
		NEW(drop); drop.win := win; drop.wnd := wnd;
		res := WinOle.RegisterDragDrop(wnd, drop)
	END InstallDropTarget;
	
	PROCEDURE RemoveDropTarget* (wnd: WinApi.HWND);
		VAR res: COM.RESULT;
	BEGIN
		res := WinOle.RevokeDragDrop(wnd)
	END RemoveDropTarget;
	
	PROCEDURE PollDrop (d: IDropTarget; show: BOOLEAN);
		VAR msg: Controllers.PollDropMsg; w, h: INTEGER;
	BEGIN
		(* x, y in port coordinates of w *)
		d.win.port.GetSize(w, h);
		msg.x := d.x * d.win.port.unit;
		msg.y := d.y * d.win.port.unit;
		(* msg.x, msg.y in coordinates of target root frame *)
		msg.source := d.source; msg.sourceX := d.srcX; msg.sourceY := d.srcY;
		msg.mark := Controllers.mark; msg.show := show;
		msg.type := d.type; msg.isSingle := d.isSingle;
		msg.w := d.w; msg.h := d.h; msg.rx := d.rx; msg.ry := d.ry;
		msg.dest := NIL;
		d.win.ForwardCtrlMsg(msg);
		(* msg.x, msg.y in coordinates of target frame *)
		targetFrame := msg.dest; targetX := msg.x; targetY := msg.y
	END PollDrop;

	PROCEDURE Drop (d: IDropTarget; v: Views.View; w, h: INTEGER; isSingle: BOOLEAN);
		VAR msg: Controllers.DropMsg; pw, ph: INTEGER;
	BEGIN
		(* x, y in port coordinates of w *)
		d.win.port.GetSize(pw, ph);
		msg.x := d.x * d.win.port.unit;
		msg.y := d.y * d.win.port.unit;
		(* msg.x, msg.y in coordinates of target root frame *)
		msg.source := d.source; msg.sourceX := d.srcX; msg.sourceY := d.srcY;
		msg.view := v; msg.isSingle := isSingle;
		msg.w := w; msg.h := h; msg.rx := d.rx; msg.ry := d.ry;
		d.win.ForwardCtrlMsg(msg)
	END Drop;

	PROCEDURE AppendMenu (menu: WinApi.HANDLE; id: INTEGER; name: Dialog.String);
		VAR res: INTEGER;
	BEGIN
		Dialog.MapString(name, name);
		res := WinApi.AppendMenuW(menu, {}, id, name)
	END AppendMenu;

	PROCEDURE ShowPopup (f: Views.Frame; x, y: INTEGER; VAR effect: SET);
		VAR res: INTEGER; menu, wnd: WinApi.HANDLE; msg: WinApi.MSG; pt: WinApi.POINT;
	BEGIN
		menu := WinApi.CreatePopupMenu();
		AppendMenu(menu, 32000, "#Host:MoveHere");
		AppendMenu(menu, 32001, "#Host:CopyHere");
		AppendMenu(menu, 32002, "#Host:LinkHere");
		IF effect * WinOle.DROPEFFECT_MOVE = {} THEN
			res := WinApi.EnableMenuItem(menu, 32000, WinApi.MF_GRAYED)
		END;
		IF effect * WinOle.DROPEFFECT_COPY = {} THEN
			res := WinApi.EnableMenuItem(menu, 32001, WinApi.MF_GRAYED)
		END;
		IF effect * WinOle.DROPEFFECT_LINK = {} THEN
			res := WinApi.EnableMenuItem(menu, 32002, WinApi.MF_GRAYED)
		END;
		res := WinApi.AppendMenuW(menu, WinApi.MF_SEPARATOR, 0, NIL);
		AppendMenu(menu, 32003, "#System:Cancel");
		wnd := f.rider(HostPorts.Rider).port.wnd;
		pt.x := (x + f.gx) DIV f.unit; pt.y := (y + f.gy) DIV f.unit;
		res := WinApi.ClientToScreen(wnd, pt);
		res := WinApi.TrackPopupMenu(menu, {1}, pt.x, pt.y, 0, wnd, NIL);
		res := WinApi.DestroyMenu(menu);
		effect := WinOle.DROPEFFECT_NONE;
		IF WinApi.PeekMessageW(msg, wnd, WinApi.WM_COMMAND, WinApi.WM_COMMAND, 1) # 0 THEN
			IF msg.wParam = 32000 THEN effect := WinOle.DROPEFFECT_MOVE
			ELSIF msg.wParam = 32001 THEN effect := WinOle.DROPEFFECT_COPY
			ELSIF msg.wParam = 32002 THEN effect := WinOle.DROPEFFECT_LINK
			END
		END
	END ShowPopup;
	
	PROCEDURE Effect (mask, keyState: SET): SET;
		VAR effect: SET;
	BEGIN
		IF (mask * WinOle.DROPEFFECT_LINK # {})
			& (((WinApi.MK_SHIFT + WinApi.MK_CONTROL) - keyState = {})
				OR (mask * (WinOle.DROPEFFECT_MOVE + WinOle.DROPEFFECT_COPY) = {})) THEN
					effect := WinOle.DROPEFFECT_LINK
		ELSIF (mask * WinOle.DROPEFFECT_COPY # {})
			& ((keyState * WinApi.MK_CONTROL # {})
				OR ((WinApi.MK_LBUTTON + WinApi.MK_RBUTTON) - keyState = {})
				OR (mask * WinOle.DROPEFFECT_MOVE = {})) THEN effect := WinOle.DROPEFFECT_COPY
		ELSIF mask * WinOle.DROPEFFECT_MOVE # {} THEN effect := WinOle.DROPEFFECT_MOVE
		ELSE effect := WinOle.DROPEFFECT_NONE
		END;
		RETURN effect
	END Effect;
	
	PROCEDURE (this: IDropTarget) DragEnter (dataObj: WinOle.IDataObject; keyState: SET; pt: WinApi.POINT;
																VAR effect: SET): COM.RESULT;
		VAR res: INTEGER; s: BOOLEAN;
	BEGIN
		IF sourceFrame # NIL THEN
			this.source := sourceFrame;
			this.srcX := sourceX; this.srcY := sourceY;
			Services.GetTypeName(dropView, this.type);
			this.isSingle := isSingleton;
			this.w := dropW; this.h := dropH;
			this.rx := relX; this.ry := relY
		ELSE
			OleData.GetDataType(dataObj, this.type, this.w, this.h, this.rx, this.ry, this.isSingle);
			this.source := NIL
		END;
		IF (this.type # "") & (this.win # NIL) THEN
			res := WinApi.ScreenToClient(this.wnd, pt);
			this.x := pt.x; this.y := pt.y; this.key := keyState;
			PollDrop(this, Controllers.show)
		END;
		IF (this.type # "") & (this.win = NIL) OR (targetFrame # NIL) THEN
			effect := Effect(effect, keyState)
		ELSE effect := WinOle.DROPEFFECT_NONE
		END;
		 this.effect := effect;
		RETURN WinApi.S_OK
	END DragEnter;
	
	PROCEDURE (this: IDropTarget) DragOver (keyState: SET; pt: WinApi.POINT; VAR effect: SET): COM.RESULT;
		VAR res: INTEGER;
	BEGIN
		IF (this.type # "") & (this.win # NIL) THEN
			res := WinApi.ScreenToClient(this.wnd, pt);
			IF (pt.x # this.x) OR (pt.y # this.y) THEN
				PollDrop(this, Controllers.hide);
				this.x := pt.x; this.y := pt.y;
				PollDrop(this, Controllers.show)
			END
		END;
		IF (this.type # "") & (this.win = NIL) OR (targetFrame # NIL) THEN
			effect := Effect(effect, keyState)
		ELSE effect := WinOle.DROPEFFECT_NONE
		END;
		this.effect := effect;
		RETURN WinApi.S_OK
	END DragOver;
	
	PROCEDURE (this: IDropTarget) DragLeave (): COM.RESULT;
	BEGIN
		IF (this.type # "") & (this.win # NIL) THEN
			PollDrop(this, Controllers.hide)
		END;
		targetFrame := NIL; this.source := NIL;
		RETURN WinApi.S_OK
	END DragLeave;
	
	PROCEDURE (this: IDropTarget) Drop (dataObj: WinOle.IDataObject; keyState: SET; pt: WinApi.POINT;
														VAR effect: SET): COM.RESULT;
		VAR res, w, h: INTEGER; v: Views.View; s: BOOLEAN;
			c: Containers.Controller; m: Containers.Model; p: Properties.BoundsPref;
	BEGIN
		IF this.effect # WinOle.DROPEFFECT_NONE THEN
			IF this.win # NIL THEN
				res := WinApi.ScreenToClient(this.wnd, pt);
				PollDrop(this, Controllers.hide);
				IF targetFrame # NIL THEN
					Windows.dir.Select(this.win, Windows.eager);
					IF WinApi.MK_LBUTTON * this.key = {} THEN	(* nonstandard drag *)
						ShowPopup(targetFrame, targetX, targetY, effect)
					ELSE effect := this.effect
					END;
					IF (effect # WinOle.DROPEFFECT_NONE) & (sourceFrame = NIL) THEN
						IF Services.Is(targetFrame.view, "TextViews.View") THEN
							OleData.GetTextDataView(dataObj, v, w, h, s)
						ELSE
							OleData.GetDataView(dataObj, "", v, w, h, s)
						END;
						IF v # NIL THEN Drop(this, v, w, h, s)
						ELSE effect := WinOle.DROPEFFECT_NONE
						END
					END
				ELSE effect := WinOle.DROPEFFECT_NONE
				END
			ELSE	(* drop to new window *)
				effect := this.effect;
				IF sourceFrame # NIL THEN
					w := dropW; h := dropH;
					IF isSingleton THEN
						v := Views.CopyOf(dropView, Views.deep)
					ELSE
						c := dropView(Containers.View).ThisController();
						m := c.SelectionCopy();
						v := Views.CopyWithNewModel(dropView, m);
						p.w := w; p.h := h; Views.HandlePropMsg(v, p); w := p.w; h := p.h
					END
				ELSE OleData.GetDataView(dataObj, "", v, w, h, s)
				END;
				IF v # NIL THEN Views.OpenView(Documents.dir.New(v, w, h));
				Views.BeginModification(Views.notUndoable, v);
				Views.EndModification(Views.notUndoable, v)
				ELSE effect := WinOle.DROPEFFECT_NONE
				END
			END
		ELSE effect := WinOle.DROPEFFECT_NONE
		END;
		RETURN WinApi.S_OK
	END Drop;
	
	
	(* drag & drop *)

	PROCEDURE (hook: Hook) TrackToDrop* (f: Views.Frame; view: Views.View;
									isSingle: BOOLEAN; w, h, rx, ry: INTEGER;
									VAR dest: Views.Frame; VAR destX, destY: INTEGER; VAR op: INTEGER;
									VAR x, y: INTEGER; VAR buttons: SET);
		VAR res: COM.RESULT; mask, mode: SET; p: HostPorts.Port; pt: WinApi.POINT;
			dsrc: IDropSource; data: WinOle.IDataObject;
	BEGIN
		sourceFrame := f; sourceX := x; sourceY := y;
		dropView := view; isSingleton := isSingle;
		dropW := w; dropH := h; relX := rx; relY := ry;
		data := OleData.ViewDropData(view, w, h, rx, ry, isSingle, ~isSingle);
		NEW(dsrc); dsrc.key := {};
		mask := WinOle.DROPEFFECT_COPY;
		IF op # Mechanisms.copy THEN mask := mask + WinOle.DROPEFFECT_MOVE END;
		res := WinOle.DoDragDrop(data, dsrc, mask, mode);
		op := Mechanisms.cancelDrop;
		IF res = WinApi.DRAGDROP_S_DROP THEN
			IF mode * WinOle.DROPEFFECT_MOVE # {} THEN op := Mechanisms.move
			ELSIF mode * WinOle.DROPEFFECT_COPY # {} THEN op := Mechanisms.copy
			END
		END;
		IF targetFrame # NIL THEN	(* reconstruct final mouse coordinates in f *)
			p := targetFrame.rider(HostPorts.Rider).port;
			pt.x := (targetX + targetFrame.gx) DIV p.unit;
			pt.y := (targetY + targetFrame.gy) DIV p.unit;
			res := WinApi.ClientToScreen(p.wnd, pt);
			p := f.rider(HostPorts.Rider).port;
			res := WinApi.ScreenToClient(p.wnd, pt);
			x := pt.x * p.unit - f.gx;
			y := pt.y * p.unit - f.gy
		END;
		dest := targetFrame; destX := targetX; destY := targetY;
		sourceFrame := NIL; targetFrame := NIL; dropView := NIL
	END TrackToDrop;

	PROCEDURE PickMode (f, dest: Views.Frame; x, y: INTEGER): INTEGER;
		VAR mode, cursor: INTEGER;
	BEGIN
		IF WinApi.GetAsyncKeyState(1BH) < 0 THEN mode := escape; cursor := Ports.arrowCursor
(*
		ELSIF Home(f, x, y) THEN mode := Mechanisms.cancelPick; cursor := Ports.arrowCursor
*)
		ELSIF dest = NIL THEN mode := Mechanisms.cancelPick; cursor := HostPorts.stopCursor
		ELSE
			cursor := HostPorts.pickCursor;
			IF Services.SameType(dest.view, f.view) THEN
				mode := Mechanisms.pick
			ELSE mode := Mechanisms.pickForeign
			END
		END;
		f.SetCursor(cursor);
		RETURN mode
	END PickMode;

	PROCEDURE (hook: Hook) TrackToPick* (f: Views.Frame;
									VAR dest: Views.Frame; VAR destX, destY: INTEGER; VAR op: INTEGER;
									VAR x, y: INTEGER; VAR buttons: SET);
		VAR d, d0: Views.Frame;
			dx, dy,  x0, y0,  x1, y1: INTEGER; isDown: BOOLEAN; m: SET;
	BEGIN
		x0 := x; y0 := y;
		Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.show, dest, destX, destY);
		(* MarkTarget(dest, dest # f); *)
		op := PickMode(f, dest, x, y);
		REPEAT
			(* CheckWindow(TRUE); *)
			f.Input(x1, y1, m, isDown);
			IF (x1 # x) OR (y1 # y) THEN
				Properties.PollPick(x1, y1, f, x0, y0, Properties.noMark, Properties.show, d, dx, dy);
				IF (d # dest) OR (dx # destX) OR (dy # destY) THEN
					d0 := dest;
					(* MarkTarget(dest, (dest # f) & (d # d0)); *)
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.hide, dest, destX, destY);
					x := x1; y := y1;
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.show, d, dx, dy);
					dest := d; destX := dx; destY := dy;
					(* MarkTarget(dest, (dest # f) (* ~home *) & (d # d0)); *)
				ELSE
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.hide, d, dx, dy);
					x := x1; y := y1;
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.show, d, dx, dy)
				END
			END;
			op := PickMode(f, dest, x, y)
		UNTIL ~isDown OR (op = escape);
		Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.hide, d, dx, dy);
		IF op = escape THEN
			REPEAT f.Input(x, y, m, isDown) UNTIL ~isDown;
			op := Mechanisms.cancelPick
		END;
		(* MarkTarget(dest, dest # f); *)
		(* CheckWindow(FALSE) *)
		buttons := {}
	END TrackToPick;


	PROCEDURE (hook: Hook) PopUpAndSelect* (f: Views.Frame;
										n, this: INTEGER;
										string: ARRAY OF ARRAY OF CHAR;
										enabled, checked: ARRAY OF BOOLEAN;
										VAR i: INTEGER;
										VAR x, y: INTEGER; VAR buttons: SET);
		VAR res, j: INTEGER; menu, wnd: WinApi.HANDLE; pt: WinApi.POINT;
			t: ARRAY 256 OF CHAR; s: SET;
	BEGIN
		ASSERT(0 < n, 20); ASSERT(n <= LEN(string), 21);
		ASSERT(LEN(enabled) = LEN(string), 22);
		ASSERT(LEN(checked) = LEN(string), 23);
		wnd := f.rider(HostPorts.Rider).port.wnd;
		ASSERT(wnd # 0, 100);
		menu := WinApi.CreatePopupMenu(); j := 0;
		WHILE j < n DO
			IF string[j] = "-" THEN
				res := WinApi.AppendMenuW(menu, WinApi.MF_SEPARATOR, 0, NIL)
			ELSE
				Dialog.MapString(string[j], t);
				res := WinApi.AppendMenuW(menu, {}, 32000 + j, t);
				IF ~enabled[j] THEN
					res := WinApi.EnableMenuItem(menu, 32000 + j, WinApi.MF_GRAYED)
				ELSIF checked[j] THEN
					res := WinApi.CheckMenuItem(menu, 32000 + j, WinApi.MF_GRAYED)
				END
			END;
			INC(j)
		END;
		pt.x := (x + f.gx) DIV f.unit; pt.y := (y + f.gy) DIV f.unit;
		res := WinApi.ClientToScreen(wnd, pt);
		s := {1, 2};	(* track right, center align *)
		res := WinApi.TrackPopupMenu(menu, s, pt.x, pt.y + 2, 0, wnd, NIL);
		res := WinApi.DestroyMenu(menu)
	END PopUpAndSelect;


	PROCEDURE Init*;
		VAR h: Hook;
	BEGIN
		NEW(h); Mechanisms.SetHook(h)
	END Init;

BEGIN
	Init
END HostMechanisms.
