MODULE StdTabViews;
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
		Kernel, Dialog, FormViews, StdCFrames, Stores, Views, Controllers, Ports,
		Properties, Containers, Services, Models, Fonts, Meta;

	CONST
		minVersion = 1; maxVersion = 1;
		noTab* = -1;

	TYPE
		Directory* = POINTER TO ABSTRACT RECORD END;

		StdDirectory = POINTER TO RECORD (Directory) END;

		TabInfo = RECORD
			label: Dialog.String;
			view: Views.View
		END;

		Tabs = POINTER TO ARRAY OF TabInfo;

		View* = POINTER TO LIMITED RECORD (Views.View)
			dispX, dispY, dispW, dispH: INTEGER;
			deposited: BOOLEAN;
			tabs: Tabs;
			font: Fonts.Font;
			index, nofTabs, scriptLevel: INTEGER;
			notifier: Dialog.String;
			ba: BalanceAction; tc: TrapCleaner
		END;

		TabContext = POINTER TO RECORD (Models.Context)
			w, h: INTEGER
		END;

		Frame* = POINTER TO ABSTRACT RECORD (StdCFrames.Frame) END;

		FrameDirectory* = POINTER TO ABSTRACT RECORD END;

		FocusMsg = RECORD (Controllers.Message) view: View END;

		SetItemOp = POINTER TO RECORD (Stores.Operation)
			tv: View;
			label: Dialog.String;
			v: Views.View;
			i, oldLen: INTEGER;
			set: BOOLEAN
		END;

		DeleteOp = POINTER TO RECORD (Stores.Operation)
			tv: View;
			label: Dialog.String;
			v: Views.View;
			i: INTEGER;
			set: BOOLEAN
		END;

		FontOp = POINTER TO RECORD (Stores.Operation)
			v: View;
			font: Fonts.Font
		END;

		SetNotifierOp = POINTER TO RECORD (Stores.Operation)
			notifier: Dialog.String;
			v: View
		END;

		NotifyProc = RECORD (Meta.Value) p*: PROCEDURE (tv: View; from, to: INTEGER) END;

		BalanceAction = POINTER TO RECORD (Services.Action) v: View END;

		TrapCleaner = POINTER TO RECORD (Kernel.TrapCleaner) v: View END;

		NotifierProc* = PROCEDURE (tv: View; from, to: INTEGER);

	VAR
		dir-, stdDir-: Directory;
		d: StdDirectory;
		setFocus*: BOOLEAN;
		frameDir-, frameStdDir-: FrameDirectory;
		dlg*: RECORD
			name*, notifier*: Dialog.String;
			opt*, index: INTEGER;
			tab, ntab: View
		END;
		nv: View;

	PROCEDURE^ (tv: View) SetIndex* (i : INTEGER), NEW;

	(* Auxiliary procedures *)

	PROCEDURE LayoutMode(view: Views.View);
		VAR c: Containers.Controller;
	BEGIN
		WITH view: Containers.View DO
			c := view.ThisController();
			(* set view in layout mode *)
			c.SetOpts(c.opts - {Containers.noSelection, Containers.noCaret} + {Containers.noFocus} )
		ELSE
		END
	END LayoutMode;

	PROCEDURE MaskMode(view: Views.View);
		VAR c: Containers.Controller;
	BEGIN
		WITH view: Containers.View DO
			c := view.ThisController();
			(* set view in mask mode *)
			c.SetOpts(c.opts - {Containers.noFocus} + {Containers.noSelection, Containers.noCaret})
		ELSE
		END
	END MaskMode;

	PROCEDURE ExecNotifier (from, to, nop: INTEGER);
		VAR i: Meta.Item; p: NotifyProc; ok: BOOLEAN;
	BEGIN
		ASSERT(nv # NIL, 20);
		Meta.LookupPath(nv.notifier, i);
		IF i.obj # Meta.undef THEN
			i.GetVal(p, ok);
			IF ok THEN
				p.p(nv, from, to)
			ELSE
				Dialog.ShowParamMsg("#System:HasWrongType", nv.notifier, "", "")
			END
		ELSE
			Dialog.ShowMsg(nv.notifier + ' is not a valid notifier for StdTabViews.View')
		END
	END ExecNotifier;

	PROCEDURE CallNotifier (tv: View; from, to: INTEGER);
	BEGIN
		IF tv.notifier # "" THEN
			nv := tv; Kernel.Try(ExecNotifier, from, to, 0); nv := NIL
		END
	END CallNotifier;


	(* Frame *)

	PROCEDURE (f: Frame) InDispArea* (x, y: INTEGER): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (f: Frame) GetDispSize* (OUT x, y, w, h: INTEGER), NEW, ABSTRACT;

	PROCEDURE (f: Frame) SetIndex* (i: INTEGER), NEW;
		VAR from: INTEGER; tv: View;
	BEGIN
		tv := f.view(View); from := tv.index;
		tv.SetIndex(i);
		CallNotifier(tv, from, i)
	END SetIndex;


	(** FrameDirectory **)

	PROCEDURE (d: FrameDirectory) GetTabSize* (VAR w, h: INTEGER), NEW, ABSTRACT;
	PROCEDURE (d: FrameDirectory) New* (): Frame, NEW, ABSTRACT;

	PROCEDURE SetFrameDir* (d: FrameDirectory);
	BEGIN
		ASSERT(d # NIL, 20); frameDir := d;
		IF frameStdDir = NIL THEN frameStdDir := d END
	END SetFrameDir;


	(* View *)

	PROCEDURE (tv: View) Update (), NEW;
	BEGIN
		IF tv.scriptLevel = 0 THEN Views.Update(tv, Views.rebuildFrames) END
	END Update;

	PROCEDURE (tv: View) Reset (), NEW;
	BEGIN
		tv.scriptLevel := 0;
		IF tv.tc # NIL THEN Kernel.PopTrapCleaner(tv.tc); tv.tc := NIL END;
		IF tv.ba # NIL THEN Services.RemoveAction(tv.ba); tv.ba := NIL END;
		tv.Update
	END Reset;

	PROCEDURE (tv: View) SetNofTabs* (nofTabs: INTEGER), NEW;
		VAR i: INTEGER; nt: Tabs;
	BEGIN
		ASSERT(nofTabs >= 0, 20);
		IF nofTabs = 0 THEN
			tv.nofTabs := 0; tv.tabs := NIL; tv.index := 0
		ELSE
			NEW(nt, nofTabs);
			IF tv.tabs # NIL THEN
				FOR i := 0 TO MIN(nofTabs, tv.nofTabs) - 1 DO
					nt[i].label := tv.tabs[i].label; nt[i].view := tv.tabs[i].view
				END
			END;
			tv.tabs := nt; tv.nofTabs := nofTabs; tv.index := MIN(tv.index, tv.nofTabs - 1)
		END;
		tv.Update
	END SetNofTabs;

	PROCEDURE (tv: View) NofTabs* (): INTEGER, NEW;
	BEGIN
		IF tv.tabs = NIL THEN RETURN 0 END;
		RETURN MAX(0, tv.nofTabs)
	END NofTabs;

	PROCEDURE (tv: View) Index* (): INTEGER, NEW;
	BEGIN
		RETURN MAX(0, MIN(tv.index, tv.NofTabs() - 1))
	END Index;

	PROCEDURE (tv: View) SetItem* (i: INTEGER; label: Dialog.String; v: Views.View), NEW;
		VAR sop: SetItemOp;
	BEGIN
		ASSERT(i >= 0, 20); ASSERT(label # "", 21); ASSERT(v # NIL, 22);
		NEW(sop);
		sop.tv := tv; sop.i := i; sop.v := v; sop.label := label; sop.set := TRUE;
		Views.Do(tv, "#Std:TabSetItem", sop)
	END SetItem;

	PROCEDURE (tv: View) GetItem* (i: INTEGER; OUT label: Dialog.String; OUT v: Views.View), NEW;
	BEGIN
		ASSERT((i >= 0) & (i < tv.NofTabs()), 21);
		label := tv.tabs[i].label;
		v := tv.tabs[i].view
	END GetItem;

	PROCEDURE (tv: View) View (i: INTEGER): Views.View, NEW;
	BEGIN
		ASSERT((i >= 0) & (i < tv.NofTabs()), 20);
		RETURN tv.tabs[i].view
	END View;

	PROCEDURE (tv: View) SetIndex* (i : INTEGER), NEW;
	BEGIN
		ASSERT((i >= 0) & (i < tv.NofTabs()), 20);
		tv.index := i;
		tv.Update
	END SetIndex;

	PROCEDURE (tv: View) SetNotifier* (IN notifier: ARRAY OF CHAR), NEW;
		VAR o: SetNotifierOp;
	BEGIN
		NEW(o);
		o.v := tv; o.notifier := notifier$;
		Views.Do(tv, "Set Notifier", o)
	END SetNotifier;

	PROCEDURE (tv: View) GetNotifier* (OUT notifier: Dialog.String), NEW;
	BEGIN
		notifier := tv.notifier
	END GetNotifier;

	PROCEDURE (tv: View) Externalize- (VAR wr: Stores.Writer);
		VAR l: Dialog.String; i: INTEGER; v: Views.View;
	BEGIN
		tv.Externalize^(wr);
		wr.WriteVersion(maxVersion);
		wr.WriteInt(tv.NofTabs());
		wr.WriteInt(tv.Index());
		wr.WriteString(tv.notifier);
		FOR i := 0 TO tv.NofTabs() - 1 DO
			tv.GetItem(i, l, v);
			wr.WriteString(l); Views.WriteView(wr, v)
		END;
		wr.WriteString(tv.font.typeface$); wr.WriteInt(tv.font.size); wr.WriteSet(tv.font.style); wr.WriteInt(tv.font.weight)
	END Externalize;

	PROCEDURE (tv: View) Internalize- (VAR rd: Stores.Reader);
		VAR thisVersion, i, size, weight: INTEGER; l: Dialog.String; v: Views.View; face: Fonts.Typeface; style: SET;
	BEGIN
		ASSERT(frameStdDir # NIL, 20);
		tv.Internalize^(rd);
		rd.ReadVersion(minVersion, maxVersion, thisVersion);
		IF (thisVersion >= minVersion) & (thisVersion <= maxVersion) THEN
			rd.ReadInt(i); tv.SetNofTabs(i);
			rd.ReadInt(tv.index);
			rd.ReadString(tv.notifier);
			FOR i := 0 TO tv.NofTabs() -1 DO
				rd.ReadString(l); Views.ReadView(rd, v);
				tv.SetItem(i, l, v)
			END;
			rd.ReadString(face); rd.ReadInt(size); rd.ReadSet(style); rd.ReadInt(weight);
			tv.font := Fonts.dir.This(face, size, style, weight);
			CallNotifier(tv, noTab, tv.index)
		END
	END Internalize;

	PROCEDURE (tv: View) CopyFromSimpleView- (source: Views.View);
		VAR l: Dialog.String; v: Views.View; i: INTEGER;
	BEGIN
		WITH source: View DO
			tv.notifier := source.notifier;
			tv.SetNofTabs(source.NofTabs());
			FOR i := 0 TO tv.NofTabs() - 1 DO
				source.GetItem(i, l, v);
				v := Views.CopyOf(v, Views.deep);
				tv.SetItem(i, l, v)
			END;
			tv.font := Fonts.dir.This(source.font.typeface, source.font.size, source.font.style, source.font.weight);
			tv.SetIndex(source.Index())
		END
	END CopyFromSimpleView;

	PROCEDURE (tv: View) GetNewFrame* (VAR frame: Views.Frame);
		VAR f: Frame;
	BEGIN
		f := frameDir.New();
		f.font := tv.font;
		frame := f
	END GetNewFrame;

	PROCEDURE (tv: View) Restore* (f: Views.Frame; l, t, r, b: INTEGER);
		VAR v: Views.View;
	BEGIN
		WITH f: Frame DO
			IF (~f.InDispArea(l,t) OR ~f.InDispArea(r,b)) THEN
				(* redraw is not completely within the display area so the control itself also needs to be restored *)
				f.UpdateList(); f.Restore(l , t , r, b)
			END;
			f.GetDispSize(tv.dispX, tv.dispY, tv.dispW, tv.dispH);
			IF tv.NofTabs() > 0 THEN
				v := tv.View(tv.Index());
				v.context.SetSize(tv.dispW, tv.dispH);
				Views.InstallFrame(f, v, tv.dispX, tv.dispY, 0, TRUE)
			END
		END
	END Restore;

	PROCEDURE (tv: View) HandleCtrlMsg* (f: Views.Frame; VAR msg: Controllers.Message;
																		VAR focus: Views.View);
		VAR sp: Properties.SizeProp;
	BEGIN
		WITH f: Frame DO
			WITH msg: Controllers.TrackMsg DO
				IF (tv.NofTabs() > 0) & (f.InDispArea(msg.x, msg.y)) THEN
					focus := tv.View(tv.Index())
				ELSE
					f.MouseDown(msg.x, msg.y, msg.modifiers)
				END
			| msg: FocusMsg DO
				msg.view := tv
			| msg: Properties.PollPickMsg DO
				msg.dest := f;
				IF (tv.NofTabs() > 0) & (f.InDispArea(msg.x, msg.y)) THEN
					focus :=  tv.View(tv.Index())
				END
			| msg: Properties.PickMsg DO
				NEW(sp); sp.known := {Properties.width, Properties.height}; sp.valid := sp.known;
				tv.context.GetSize(sp.width, sp.height);
				Properties.Insert(msg.prop, sp);
				IF (tv.NofTabs() > 0) & (f.InDispArea(msg.x, msg.y)) THEN
					focus :=  tv.View(tv.Index())
				END
			|msg: Controllers.PollCursorMsg DO
				IF (tv.NofTabs() > 0) & (f.InDispArea(msg.x, msg.y)) THEN
					focus := tv.View(tv.Index())
				ELSE
					f.GetCursor(msg.x, msg.y, msg.modifiers, msg.cursor)
				END
			ELSE
				IF tv.NofTabs() > 0 THEN
					focus := tv.View(tv.Index())
				ELSE
					WITH msg: Controllers.PollOpsMsg DO
						msg.valid := {Controllers.pasteChar}
					| msg: Controllers.MarkMsg DO
						f.Mark(msg.show, msg.focus)
					ELSE
					END
				END
			END
		END
	END HandleCtrlMsg;

	PROCEDURE BeginChanges* (tv: View);
	BEGIN
		ASSERT(tv.scriptLevel >= 0, 20);
		IF tv.tc = NIL THEN NEW(tv.tc); tv.tc.v := tv; Kernel.PushTrapCleaner(tv.tc) END;
		IF tv.ba = NIL THEN NEW(tv.ba); tv.ba.v := tv; Services.DoLater(tv.ba, Services.now) END;
		INC(tv.scriptLevel)
	END BeginChanges;

	PROCEDURE EndChanges* (tv: View);
	BEGIN
		ASSERT(tv.scriptLevel > 0, 20);
		DEC(tv.scriptLevel);
		IF tv.scriptLevel = 0 THEN tv.Reset END
	END EndChanges;


	(* TrapCleaner *)

	PROCEDURE (t: TrapCleaner) Cleanup;
	BEGIN
		t.v.Reset
	END Cleanup;


	(* Actions and Operations *)

	PROCEDURE (a: BalanceAction) Do-;
	BEGIN
		a.v.Reset();
		HALT(100) (* Unbalanced call to BeginChanges/EndChanges *)
	END Do;

	PROCEDURE (o: SetNotifierOp) Do;
		VAR old: Dialog.String;
	BEGIN
		old := o.v.notifier;
		o.v.notifier := o.notifier;
		o.notifier := old
	END Do;

	PROCEDURE (o: FontOp) Do;
		VAR f: Fonts.Font;
	BEGIN
		f := o.v.font; o.v.font := o.font; o.font := f;
		o.v.Update
	END Do;

	PROCEDURE (o: SetItemOp) Do;
		VAR tc: TabContext; label: Dialog.String; view: Views.View;
	BEGIN
		IF o.set THEN
			o.set := FALSE; o.oldLen := o.tv.NofTabs();
			view := Views.CopyOf(o.v, Views.deep); Stores.Join(o.tv, view);
			label := o.label;
			IF o.i >= o.oldLen THEN
				o.tv.SetNofTabs(o.i + 1)
			ELSE
				o.tv.GetItem(o.i, o.label, o.v)
			END;
			NEW(tc); view.InitContext(tc);
			o.tv.tabs[o.i].label := label; o.tv.tabs[o.i].view := view
		ELSE
			o.set := TRUE;
			IF o.i >= o.oldLen THEN
				o.tv.SetNofTabs(o.oldLen)
			ELSE
				view := o.v; label := o.label;
				o.tv.GetItem(o.i, o.label, o.v);
				o.tv.tabs[o.i].label := label; o.tv.tabs[o.i].view := view
			END
		END;
		o.tv.Update
	END Do;

	PROCEDURE (o: DeleteOp) Do;
		VAR j: INTEGER;
	BEGIN
		IF o.set THEN
			ASSERT(o.i < o.tv.NofTabs(), 20);
			o.set := FALSE;
			o.tv.GetItem(o.i, o.label, o.v);
			FOR j := o.i TO o.tv.NofTabs() - 2 DO
				o.tv.tabs[j].label := o.tv.tabs[j + 1].label;
				o.tv.tabs[j].view := o.tv.tabs[j + 1].view
			END;
			o.tv.SetNofTabs(o.tv.NofTabs() - 1)
		ELSE
			ASSERT(o.i <= o.tv.NofTabs(), 21);
			o.set := TRUE;
			o.tv.SetNofTabs(o.tv.NofTabs() + 1);
			FOR j := o.tv.NofTabs() - 1 TO o.i + 1 BY - 1 DO
				o.tv.tabs[j].label := o.tv.tabs[j - 1].label;
				o.tv.tabs[j].view := o.tv.tabs[j - 1].view
			END;
			o.tv.tabs[o.i].label := o.label;
			o.tv.tabs[o.i].view := o.v
		END;
		o.tv.Update
	END Do;

	PROCEDURE (tv: View) HandlePropMsg- (VAR msg: Properties.Message);
		VAR stp: Properties.StdProp; p: Properties.Property;
			face: Fonts.Typeface; size, weight, i, w, h, asc, dsc: INTEGER; style: SET;
			smsg: Properties.SizePref; fo: FontOp;
	BEGIN
		WITH msg: Properties.ControlPref DO
			IF tv.NofTabs() = 0 THEN
				IF msg.getFocus THEN msg.getFocus := StdCFrames.setFocus END
			ELSE
				msg.focus := tv.View(tv.Index());
				Views.HandlePropMsg(msg.focus, msg)
			END
		| msg: Properties.FocusPref DO
			IF (tv.NofTabs() = 0) OR (msg.atLocation & (msg.y < tv.dispY)) THEN
				msg.hotFocus := TRUE
			ELSE
				IF msg.atLocation THEN msg.x := msg.x - tv.dispX; msg.y := msg.y - tv.dispY END;
				Views.HandlePropMsg(tv.View(tv.Index()), msg)
			END
		| msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				IF tv.deposited OR (tv.NofTabs() = 0) THEN
					frameDir.GetTabSize(msg.w, msg.h)
				ELSE
					w := 0; h:= 0;
					FOR i := 0 TO tv.NofTabs() - 1 DO
						smsg.w := Views.undefined; smsg.h := Views.undefined;
						Views.HandlePropMsg(tv.View(i), smsg); w := MAX(smsg.w, w); h := MAX(smsg.h, h)
					END;
					IF (w = 0) OR (h = 0) THEN
						frameDir.GetTabSize(msg.w, msg.h)
					ELSE
						(* add some constants to compensate for the tab view itself. *)
						msg.w := w + 2 * Ports.mm;
						tv.font.GetBounds(asc, dsc, w); msg.h := h + asc + dsc + 4 * Ports.mm
					END
				END
			END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE; msg.verFitToWin := TRUE
		| msg: Properties.PollMsg DO
			NEW(stp);
			stp.valid := {Properties.typeface..Properties.weight};
			stp.known := stp.valid;
			stp.typeface := tv.font.typeface;
			stp.size := tv.font.size; stp.style.val := tv.font.style;
			stp.weight := tv.font.weight;
			stp.style.mask := {Fonts.italic, Fonts.strikeout, Fonts.underline};
			Properties.Insert(msg.prop, stp)
		| msg: Properties.SetMsg DO
			p := msg.prop;
			WHILE (p # NIL) DO
				WITH p: Properties.StdProp DO
					IF (p.valid * {Properties.typeface..Properties.weight}) # {} THEN
						face := tv.font.typeface$; size := tv.font.size;
						style := tv.font.style; weight := tv.font.weight;
						IF Properties.typeface IN p.valid THEN face := p.typeface$;
							IF face = Fonts.default THEN face := StdCFrames.defaultFont.typeface END
						END;
						IF Properties.size IN p.valid THEN size := p.size END;
						IF Properties.style IN p.valid THEN
							style := (p.style.val * p.style.mask) + (style - p.style.mask)
						END;
						IF Properties.weight IN p.valid THEN weight := p.weight END;
						NEW(fo); fo.v := tv; fo.font := Fonts.dir.This(face, size, style, weight);
						Views.Do(tv, "#System:SetProp", fo)
					END
				ELSE
				END;
				p := p.next
			END
		| msg: Containers.DropPref DO
			msg.okToDrop := TRUE
		ELSE
			IF tv.NofTabs() > 0 THEN
				Views.HandlePropMsg(tv.View(tv.Index()), msg)
			END
		END
	END HandlePropMsg;

	PROCEDURE (tv: View) Neutralize*;
	BEGIN
		IF tv.NofTabs() > 0 THEN
			tv.View(tv.Index()).Neutralize
		END
	END Neutralize;

	(* Tab context *)

	PROCEDURE (c: TabContext) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL
	END ThisModel;

	PROCEDURE (c: TabContext) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.w; h := c.h
	END GetSize;

	PROCEDURE (c: TabContext) SetSize (w, h: INTEGER);
	BEGIN
		c.w := w; c.h := h
	END SetSize;

	PROCEDURE (c: TabContext) Normalize (): BOOLEAN;
	BEGIN
		RETURN FALSE
	END Normalize;


	(* Directory *)

	PROCEDURE (d: Directory) New* (): View, NEW, ABSTRACT;

	PROCEDURE SetDir* (d: Directory);
	BEGIN
		ASSERT(d # NIL, 20); dir := d
	END SetDir;


	(* StdDirectory *)

	PROCEDURE (d: StdDirectory) New (): View;
		VAR tv: View;
	BEGIN
		ASSERT(frameStdDir # NIL, 20);
		NEW(tv); tv.deposited := FALSE; tv.scriptLevel := 0; tv.nofTabs := 0; tv.index :=0; tv.notifier := "";
		tv.ba := NIL; tv.tc := NIL;
		tv.font := StdCFrames.defaultFont;
		RETURN tv
	END New;

	PROCEDURE Deposit*;
		VAR tv: View; fv: FormViews.View;
	BEGIN
		tv := dir.New(); tv.deposited := TRUE;
		fv := FormViews.New(); LayoutMode(fv); tv.SetItem(0, "Tab1", fv);
		fv := FormViews.New(); LayoutMode(fv); tv.SetItem(1, "Tab2", fv);
		Views.Deposit(tv)
	END Deposit;


	(* Procedures to find tab views *)

	PROCEDURE This* (v: Views.View): View;
		VAR c: Containers.Controller; v0: Views.View; v1: View;
	BEGIN
		ASSERT (v # NIL, 20);
		IF v IS View THEN RETURN v(View)
		ELSIF v IS Containers.View THEN
			c := v(Containers.View).ThisController();
			ASSERT(c # NIL, 100);
			c.GetFirstView(Containers.any, v0);
			IF v0 # NIL THEN v1 := This(v0) END;
			WHILE (v0 # NIL) & (v1 = NIL) DO
				c.GetNextView(Containers.any, v0);
				IF v0 # NIL THEN v1 := This(v0) END
			END;
			RETURN v1
		ELSE RETURN NIL
		END
	END This;

	PROCEDURE Focus* (): View;
		VAR msg: FocusMsg;
	BEGIN
		msg.view := NIL;
		Controllers.Forward(msg);
		RETURN msg.view
	END Focus;


	(* The property editor *)

	PROCEDURE SingleView() : View;
		VAR v: Views.View;
	BEGIN
		v := Containers.FocusSingleton();
		IF v = NIL THEN v := Focus() END;
		IF (v # NIL) & (v IS View) THEN RETURN v(View) ELSE RETURN NIL END
	END SingleView;

	PROCEDURE Right*;
		VAR label1, label2: Dialog.String; v1, v2: Views.View; iv: View; script: Stores.Operation;
	BEGIN
		iv := SingleView();
		IF (iv # NIL) & (iv.Index() < (iv.NofTabs() - 1)) THEN
			Views.BeginScript(iv, "Shift tab right", script);
			iv.GetItem(iv.Index() , label1, v1);
			iv.GetItem(iv.Index() + 1 , label2, v2);
			iv.SetItem(iv.Index(), label2, v2);
			iv.SetItem(iv.Index() + 1, label1, v1);
			Views.EndScript(iv, script);
			iv.SetIndex(iv.Index() + 1);
			Dialog.Update(iv)
		END
	END Right;

	PROCEDURE Left*;
		VAR label1, label2: Dialog.String; v1, v2: Views.View; iv: View;
	BEGIN
		iv := SingleView();
		IF (iv # NIL) & (iv.Index() > 0) THEN
			iv.GetItem(iv.Index() , label1, v1);
			iv.GetItem(iv.Index() - 1 , label2, v2);
			iv.SetItem(iv.Index(), label2, v2);
			iv.SetItem(iv.Index() - 1, label1, v1);
			iv.SetIndex(iv.Index() - 1);
			Dialog.Update(iv)
		END
	END Left;

	PROCEDURE AddTab*;
		VAR v: View; fv: FormViews.View;
	BEGIN
		ASSERT(dlg.name # "", 20);
		v := SingleView();
		IF v # NIL THEN
			fv := FormViews.New(); LayoutMode(fv);
			v.SetItem(v.NofTabs(), dlg.name, fv)
		END
	END AddTab;

	PROCEDURE Rename*;
		VAR v: View;
	BEGIN
		ASSERT(dlg.name # "", 20);
		v := SingleView();
		IF (v # NIL) & (v.NofTabs() > 0) THEN
			v.SetItem(v.Index(), dlg.name, v.View(v.Index()))
		END
	END Rename;

	PROCEDURE Delete*;
		VAR v: View; dop: DeleteOp;
	BEGIN
		v := SingleView();
		IF (v # NIL) & (v.NofTabs() > 0) THEN
			NEW(dop); dop.tv := v; dop.i := v.Index(); dop.set := TRUE;
			Views.Do(v, "#Std:TabDeleteItem", dop)
		END
	END Delete;

	PROCEDURE ModeNotifier* (op, from, to: INTEGER);
		VAR tv: View; i: INTEGER;
	BEGIN
		IF op IN {Dialog.changed} THEN
			tv := SingleView();
			IF (tv # NIL) & (tv.NofTabs() > 0) THEN
				IF to = 0 THEN
					FOR i := 0 TO tv.NofTabs() - 1 DO LayoutMode(tv.View(i)) END
				ELSE
					FOR i := 0 TO tv.NofTabs() - 1 DO MaskMode(tv.View(i)) END
				END
			END
		END
	END ModeNotifier;

	PROCEDURE SetNotifier*;
		VAR v: View; o: SetNotifierOp;
	BEGIN
		v := SingleView();
		IF v # NIL THEN
			NEW(o); o.v := v; o.notifier := dlg.notifier;
			Views.Do(v, "Set Notifier", o)
		END
	END SetNotifier;

	PROCEDURE NewGuard* (VAR par: Dialog.Par);
		VAR v: View;
	BEGIN
		v := SingleView();
		par.disabled := (v = NIL) OR (dlg.name = "")
	END NewGuard;

	PROCEDURE RenameGuard* (VAR par: Dialog.Par);
		VAR v: View;
	BEGIN
		v := SingleView();
		par.disabled := (v = NIL) OR (dlg.name = "")
	END RenameGuard;

	PROCEDURE DeleteGuard* (VAR par: Dialog.Par);
		VAR v: View;
	BEGIN
		v := SingleView();
		par.disabled := (v = NIL) OR (v.NofTabs() < 1)
	END DeleteGuard;

	PROCEDURE LabelGuard* (VAR par: Dialog.Par);
		VAR v: View; vv: Views.View;
	BEGIN
		v := SingleView();
		IF v # NIL THEN
			IF (v # dlg.tab) OR (v.Index() # dlg.index) THEN
				par.disabled := FALSE;
				v.GetItem(v.Index(), dlg.name, vv);
				dlg.tab := v; dlg.index := v.Index();
				Dialog.Update(dlg)
			END
		ELSE
			dlg.tab := NIL; dlg.index := -1;
			par.label := ""; par.disabled := TRUE
		END
	END LabelGuard;

	PROCEDURE NotifierGuard* (VAR par: Dialog.Par);
		VAR v: View;
	BEGIN
		v := SingleView();
		IF v # NIL THEN
			IF v # dlg.ntab THEN
				par.disabled := FALSE;
				dlg.notifier := v.notifier;
				dlg.ntab := v;
				Dialog.Update(dlg)
			END
		ELSE
			dlg.ntab := NIL;
			par.label := ""; par.disabled := TRUE
		END
	END NotifierGuard;

	PROCEDURE SetGuard* (VAR par: Dialog.Par);
		VAR v: View;
	BEGIN
		v := SingleView();
		par.disabled := v = NIL
	END SetGuard;

	PROCEDURE ModeGuard (VAR par: Dialog.Par; layout: BOOLEAN);
		CONST mode = {Containers.noSelection, Containers.noFocus, Containers.noCaret};
		VAR tv: View; i, l, m: INTEGER; c: Containers.Controller;
	BEGIN
		tv := SingleView();
		IF (tv # NIL) & (tv.NofTabs() > 0) THEN
			l := 0; m := 0;
			FOR i := 0 TO tv.NofTabs() - 1 DO
				IF tv.View(i) IS Containers.View THEN
					c := tv.View(i)(Containers.View).ThisController();
					IF c.opts * mode = {Containers.noFocus} THEN INC(l)
					ELSIF c.opts * mode = {Containers.noSelection, Containers.noCaret} THEN INC(m)
					END
				END
			END;
			IF ((m # 0) & (l # 0)) OR ((m + l) < tv.NofTabs()) THEN
				par.undef := TRUE;
				dlg.opt := -1
			ELSIF layout & (m = 0) THEN dlg.opt := 0
			ELSIF ~layout & (l = 0) THEN dlg.opt := 1
			END
		ELSE
			par.disabled := TRUE
		END
	END ModeGuard;

	PROCEDURE LayoutModeGuard*(VAR par: Dialog.Par);
	BEGIN
		ModeGuard(par, TRUE)
	END LayoutModeGuard;

	PROCEDURE MaskModeGuard*(VAR par: Dialog.Par);
	BEGIN
		ModeGuard(par, FALSE)
	END MaskModeGuard;

	PROCEDURE InitDialog*;
	BEGIN
		dlg.tab := NIL; dlg.index := -1
	END InitDialog;


	PROCEDURE Init;
		VAR res: INTEGER;
	BEGIN
		setFocus := FALSE; dlg.tab := NIL; dlg.index := -1;
		NEW(d); stdDir := d; dir := d;
		Dialog.Call('HostTabFrames.Init', 'Could not load HostTabFrames', res)
	END Init;

BEGIN
	Init
END StdTabViews.
