MODULE DevMsgSpy;
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

	IMPORT SYSTEM, 
		DevDebug, Dialog, Stores, Models, Views, Controllers, Properties, Kernel, 
		Ports, TextModels, TextViews, TextMappers, StdDialog,
		Files, Converters, FormViews, FormModels, Windows, Containers, Services;

	CONST
		refViewSize = 9 * Ports.point;

	TYPE
		View = POINTER TO RECORD (Views.View)
			inner: Views.View
		END;
		
		RefView = POINTER TO RECORD (Views.View)
			msg: ANYPTR
		END;

		Msgs = POINTER TO RECORD
			left, right: Msgs;
			mod, ident: Kernel.Name;
			index: INTEGER;
		END;
		
	VAR
		para* : RECORD
			sel*: Dialog.Selection;
			selectNewMessages*: BOOLEAN;
			showMessages*: BOOLEAN
		END;
		text*: TextModels.Model;
		
		msgs: Msgs;
		size: INTEGER;
		path: ARRAY 4 OF Ports.Point;

	(* -------------------  Selection  ------------------- *)

	PROCEDURE Find(mod, ident: Kernel.Name): Msgs;
		VAR m: Msgs;
	BEGIN m := msgs;
		WHILE m # NIL DO
			IF mod < m.mod THEN m := m.left
			ELSIF mod > m.mod THEN m := m.right
			ELSIF ident < m.ident THEN m := m.left
			ELSIF ident > m.ident THEN m := m.right
			ELSE RETURN m
			END
		END;
		RETURN NIL
	END Find;
	
	PROCEDURE Insert(mod, ident: Kernel.Name);
		PROCEDURE InsertTree(VAR m: Msgs);
			VAR name: ARRAY 64 OF CHAR;
		BEGIN
			IF m = NIL THEN
				NEW(m); m.mod := mod; m.ident := ident; m.index := size-1;
				name := m.mod + "." + m.ident;
				para.sel.SetItem(m.index, name);
				IF para.selectNewMessages THEN para.sel.Incl(m.index, m.index) END
			ELSE
				IF mod < m.mod THEN InsertTree(m.left)
				ELSIF mod > m.mod THEN InsertTree(m.right)
				ELSIF ident < m.ident THEN InsertTree(m.left)
				ELSIF ident > m.ident THEN InsertTree(m.right)
				END
			END
		END InsertTree;
	BEGIN
		INC(size);  para.sel.SetLen(size);	(* increase size and reset all items *)
		InsertTree(msgs);						(* insert the new item *)
		Dialog.UpdateList(para.sel)
	END Insert;

	(* -------------------  RefView ------------------- *)
	
	PROCEDURE (v: RefView) Internalize (VAR rd: Stores.Reader);
		VAR thisVersion: INTEGER;
	BEGIN
		v.Internalize^(rd); IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(0, 0, thisVersion); IF rd.cancelled THEN RETURN END;
	END Internalize;

	PROCEDURE (v: RefView) Externalize (VAR wr: Stores.Writer);
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(0);
	END Externalize;

	PROCEDURE (v: RefView) CopyFromSimpleView (source: Views.View);
	BEGIN
		(* v.CopyFrom^(source); *)
		v.msg := source(RefView).msg
	END CopyFromSimpleView;

	PROCEDURE (v: RefView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawPath(path, 4, Ports.fill, Ports.blue, Ports.closedPoly)
	END Restore;
	
	PROCEDURE (v: RefView) HandleCtrlMsg (
		f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View
	);
		TYPE ControllersMessage = POINTER TO Controllers.Message;
			ViewsMessage = POINTER TO Views.Message;
			ModelsMessage = POINTER TO Models.Message;
			PropMessage = POINTER TO Properties.Message;
		VAR x, y: INTEGER; isDown: BOOLEAN; mo: SET;name: ARRAY 32 OF CHAR;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			IF v.msg # NIL THEN
				REPEAT
					f.MarkRect(0, 0, refViewSize, refViewSize, Ports.fill, Ports.hilite, Ports.show);
					REPEAT
						f.Input(x, y, mo, isDown)
					UNTIL (x < 0) OR (x > refViewSize) OR (y < 0) OR (y > refViewSize) OR ~isDown;
					f.MarkRect(0, 0, refViewSize, refViewSize, Ports.fill, Ports.hilite, Ports.hide);
					WHILE isDown & ((x < 0) OR (x > refViewSize) OR (y < 0) OR (y > refViewSize)) DO
						f.Input(x, y, mo, isDown)
					END
				UNTIL ~isDown;
				IF (x >= 0) & (x <= refViewSize) & (y >= 0) & (y <= refViewSize) THEN
					IF v.msg IS ControllersMessage THEN name := "Controllers.Message"
					ELSIF v.msg IS PropMessage THEN name := "Properties.Message"
					ELSIF v.msg IS ViewsMessage THEN name := "Views.Message"
					ELSIF v.msg IS ModelsMessage THEN name := "Models.Message"
					ELSE name := "Message Record"
					END;
					DevDebug.ShowHeapObject(SYSTEM.ADR(v.msg^), name)
				END
			END
		| msg: Controllers.PollCursorMsg DO
			msg.cursor := Ports.refCursor
		ELSE
		END
	END HandleCtrlMsg;
	
	PROCEDURE (v: RefView) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.Preference DO
			WITH msg: Properties.ResizePref DO msg.fixed := TRUE
			| msg: Properties.SizePref DO msg.w := refViewSize; msg.h := refViewSize
			| msg: Properties.FocusPref DO msg.hotFocus := TRUE
			ELSE
			END
		ELSE
		END
	END HandlePropMsg;
	
	(* -------------------  Wrapper   ------------------- *)
	
	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		WITH source: View DO
			IF model = NIL THEN v.inner := Views.CopyOf(source.inner, Views.shallow)
			ELSE v.inner := Views.CopyWithNewModel(source.inner, model)
			END;
			Stores.Join(v, v.inner)
		END
	END CopyFromModelView;
	
	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.inner.ThisModel()
	END ThisModel;

	PROCEDURE (v: View) InitContext (context: Models.Context);
	BEGIN
		v.InitContext^(context);
		v.inner.InitContext(context)	(* wrapper and wrapped view share the same context *)
	END InitContext;
	
(*
	PROCEDURE (v: View) PropagateDomain;
	BEGIN
		Stores.InitDomain(v.inner, v.domain)
	END PropagateDomain;
*)

	PROCEDURE (v: View) Neutralize;
	BEGIN
		v.inner.Neutralize
	END Neutralize;

	(* NewFrame: wrapper uses standard frame *)
	(* Background: wrapper has no intrinsic background color *)

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Views.InstallFrame(f, v.inner, 0, 0, 0, TRUE)
	END Restore;

	(* RestoreMarks: wrapper has no intrinsic marks, wrapped view's RestoreMarks is called by framework *)
	
	PROCEDURE DescOfMsg (VAR x: ANYREC): Kernel.Type;
		VAR desc: Kernel.Type;
	BEGIN
		desc := SYSTEM.VAL(Kernel.Type, SYSTEM.TYP(x));	(* tdesc of x *)
		RETURN desc
	END DescOfMsg;
	
	PROCEDURE WriteLog(t: Kernel.Type; VAR msg: ANYREC; name: ARRAY OF SHORTCHAR);
		VAR link: RefView; p: ANYPTR; name2 : ARRAY 256 OF CHAR; f: TextMappers.Formatter;
	BEGIN
		Kernel.NewObj(p, t);
		SYSTEM.MOVE(SYSTEM.ADR(msg), p, t.size);
		NEW(link); link.msg := p;
		name2 := t.mod.name$ + "." + name$;
		
		f.ConnectTo(text);
		f.WriteString(name2 + " ");
		f.WriteView(link);
		f.WriteLn;
		TextViews.ShowRange(text, text.Length(), text.Length(), ~TextViews.focusOnly)
	END WriteLog;
	
	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR name: Kernel.Name; t: Kernel.Type; m: Msgs;
	BEGIN
		t := DescOfMsg(msg);
		Kernel.GetTypeName (t, name);
		m := Find(t.mod.name, name);
		IF m = NIL THEN Insert(t.mod.name, name)
		ELSIF para.sel.In(m.index) & para.showMessages THEN WriteLog(t, msg, name);
		END;
		focus := v.inner	(* forward all controller messages to wrapped view *)
	END HandleCtrlMsg;
	
	PROCEDURE (v: View) ExternalizeAs (VAR s: Stores.Store);
	BEGIN
		s := v.inner
	END ExternalizeAs;
	
	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		VAR name: Kernel.Name; t: Kernel.Type; m: Msgs;
	BEGIN
		t := DescOfMsg(msg);
		Kernel.GetTypeName (t, name);
		m := Find(t.mod.name, name);
		IF m = NIL THEN Insert(t.mod.name, name)
		ELSIF para.sel.In(m.index) & para.showMessages THEN WriteLog(t, msg, name);
		END;			
		Views.HandlePropMsg(v.inner, msg)
	END HandlePropMsg;
	
	PROCEDURE (v: View) HandleViewMsg (f: Views.Frame; VAR msg: Views.Message);
		VAR name: Kernel.Name; t: Kernel.Type; m: Msgs;
	BEGIN
		t := DescOfMsg(msg);
		Kernel.GetTypeName (t, name);
		m := Find(t.mod.name, name);
		IF m = NIL THEN Insert(t.mod.name, name)
		ELSIF para.sel.In(m.index) & para.showMessages THEN WriteLog(t, msg, name);
		END;

		WITH msg: Views.ScrollClassMsg DO
			msg.allowBitmapScrolling := TRUE
		ELSE
		END
		(* framework performs message propagation *)
	END HandleViewMsg;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
		VAR name: Kernel.Name; t: Kernel.Type; m: Msgs;
	BEGIN
		t := DescOfMsg(msg);
		Kernel.GetTypeName (t, name);
		m := Find(t.mod.name, name);
		IF m = NIL THEN Insert(t.mod.name, name)
		ELSIF para.sel.In(m.index) & para.showMessages THEN WriteLog(t, msg, name);
		END;
		(* framework performs message propagation *)
	END HandleModelMsg;

	
	PROCEDURE Toggle*;
		VAR v: Views.View; w: View; replace: Controllers.ReplaceViewMsg;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		v := Containers.FocusSingleton();
		Controllers.ResetCurrentPath();
		IF v # NIL THEN
			WITH v: View DO
				replace.old := v; replace.new := v.inner;
			ELSE
				NEW(w); w.inner := v;
				replace.old := v; replace.new := w;
			END;
			Controllers.Forward(replace)
		ELSE Dialog.Beep
		END
	END Toggle;
	
	PROCEDURE ToggleGuard* (VAR par: Dialog.Par);
		VAR v : Views.View;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		v := Containers.FocusSingleton();
		Controllers.ResetCurrentPath();
		IF (v = NIL) OR ~(v IS View) THEN
			par.label := "#Dev:AddView";
			par.disabled := v = NIL
		ELSE
			par.label := "#Dev:RemoveView"
		END
	END ToggleGuard;
	
	PROCEDURE Reset*;
	BEGIN
		msgs := NIL; size := 0; para.sel.SetLen(size);
		Dialog.UpdateList(para.sel)
	END Reset;
	
	PROCEDURE Clear*;
	BEGIN
		text.Delete(0, text.Length())
	END Clear;
	
	PROCEDURE PathToSpec (VAR path: ARRAY OF CHAR; VAR loc: Files.Locator; VAR name: Files.Name);
		VAR i, j: INTEGER; ch: CHAR;
	BEGIN
		i := 0; j := 0; loc := Files.dir.This("");
		WHILE (loc.res = 0) & (i < LEN(path) - 1) & (j < LEN(name) - 1) & (path[i] # 0X) DO
			ch := path[i]; INC(i);
			IF (j > 0) & ((ch = "/") OR (ch = "\")) THEN
				name[j] := 0X; j := 0; loc := loc.This(name)
			ELSE
				name[j] := ch; INC(j)
			END
		END;
		IF path[i] = 0X THEN name[j] := 0X
		ELSE loc.res := 1; name := ""
		END
	END PathToSpec;

	PROCEDURE OpenDialog*(file, title: ARRAY OF CHAR);
		VAR loc: Files.Locator; fname: Files.Name; conv: Converters.Converter; view, v, t: Views.View;
			r: FormModels.Reader; t0: Views.Title; done: BOOLEAN; c: Containers.Controller;
	BEGIN
		Dialog.MapString(title, t0);
		Windows.SelectByTitle(NIL, {Windows.isTool}, t0, done);
		IF ~ done THEN
			PathToSpec(file, loc, fname);
			IF loc.res = 0 THEN
				conv := NIL;
				view := Views.Old(Views.dontAsk, loc, fname, conv);
				IF view IS FormViews.View THEN t := NIL;
					r := view(FormViews.View).ThisModel().NewReader(NIL);
					r.ReadView(v);
					WHILE (v # NIL) & (t = NIL) DO
						t := Properties.ThisType(v, "TextViews.View");
						r.ReadView(v)
					END;
					IF t # NIL THEN
						text := t(TextViews.View).ThisModel();
						text.Delete(0, text.Length())
					END;
				END;
				IF view # NIL THEN
					WITH view: Containers.View DO
						c := view.ThisController();
						IF c # NIL THEN
							c.SetOpts(c.opts - {Containers.noFocus} + {Containers.noCaret, Containers.noSelection})
						ELSE Dialog.ShowMsg("#System:NotEditable")
						END
					ELSE Dialog.ShowMsg("#System:ContainerExpected")
					END;
					IF text # NIL THEN
						StdDialog.Open(view, title, NIL, "", NIL, TRUE, FALSE, TRUE, FALSE, TRUE)
					ELSE
						Dialog.ShowMsg("#Dev:TextInDialogExpected")
					END
				END
			ELSE Dialog.ShowParamMsg("#System:FileNotFound", file, "", "")
			END;
		END
	END OpenDialog;

BEGIN
(*  NEW(action); action.Do; *)
	para.showMessages := TRUE;
	size := 0;
	path[0].x := refViewSize DIV 2; path[0].y := 0;
	path[1].x := refViewSize; path[1].y := refViewSize DIV 2;
	path[2].x := refViewSize DIV 2; path[2].y := refViewSize;
	path[3].x := 0; path[3].y := refViewSize DIV 2;
END DevMsgSpy.

 "DevMsgSpy.OpenDialog('Dev/Rsrc/MsgSpy', 'Message Spy')"

Menu:
"Message Spy..."	""	"DevMsgSpy.OpenDialog('Dev/Rsrc/MsgSpy', 'Message Spy')"	""

Strings: (Dev)
AddView	Add View
RemoveView	Remove View
TextInDialogExpected	Text in Dialog expected

Dialog:
StdCoder.Decode ..,, ..qX....3Qw7uP5PRPPNR9Rbf9b8R79FTvMf1uZlbcjRAktYcjRgp,
 xW1xhiZiVBhihgmRiioedhgrp9XjJHPNjvQRdJHXS7wb8RTfQ9vQRtIdvPZHjU..D.JA06.Css
 HorC4sQqorGqmmKjAST1.PuP.PuP7PNCLLq2o9ZD,6.ciDU7Umy.0E.0.3IXyKtqKfaqmQCb8R
 7fJHPNjfL4TXyKt.bHfEjIy0.,.s,cMD.,6.5IXyKtgdjZACbHlayKmKaSNwB0UnNHEjIy4.,U
 uq.e.8P16.,6.cUCoruamxhgRiiQeZZhZRgoBhjph0xhsp9XzE.QcjphoVSdw5.0.Z00.p,0E0
 GYZdRqYntNCZkNSuWkNM8bVd9CbZ7P6..k.E.0ke59,BXgevQ4,TCbE,5zV.C5FPN5vO3uPlfL
 8z4U.EEF.E2F.Y....05.C5MNCaodHKarNHKantQ4abNNCb.MH4amtO0WC,0WBNNsQ.66aai76
 mYdtQGb.6...YiVA.1hBFlSzs9gh6Z33gwRE.6.YF0E.Eu0.,E0Uq,S4.05MMsQWajtRq2sQ.6
 .CZc,0WB,C5.0WddPUjtN0E.0E...2cNA.fCKNl2Ts9Aoe343Qw.6IfvQFfEf9RdvPRfL8z4U.
 EjU.EXU.Y..sNC3U17PKaVV.2U17P.00U.E...AYJ51f9APldCmGghh443gwT.0.,X.2.272.7
 .O5UH,cIKanNNG5C2MM.aan7R6...A2guy23bu69lajvJIkmz,6.o96.o66.7sIGbYtEqaYtQu
 W1VjtQKa2NO4agtPSa.sEm4C50.,.U,E.Yyvp2188PFHeQNAbZ443Qwb8R7vI5fQT9PNPNZvQR
 dJ.5.,6.Yo0k1E49.,E.8cIhgsZiKBhZtQC3h0xmsHpmW5sQO3uZmL.0U..w.oa0.,sUGpmWbB
 xhYltQeoNHEjAyI,ktg7cL8T1U.kk2.T.n0,6.Eb2.86.QC18RdfQHfMf9R9vQxGtH.0..c4E.
 k.UesFnQ.E.0t.U,UzjV0CyIhACoruKuEqUHZC58RZ9Pxms,.U1xB.uZlT6AA.cQ...b1.o9ZD
 ,6.636.M00U.2..AU0CyIVGhighgmRCbWGhigFjAyI,ktIepVSdw5.,6.IIw.IH2U.sU.ktQ8C
 JuaLqKKjAyI,.AjgVmz.2U..2,w86.IE.U,ZC..2..EGE.4E.E.EECOhU.wcNC.zwPA.A.2U.E
 ,9z8U...p.0.4.I3E.6.VQ.E..2eHJ.6...Z1Jsp0E9mr72YKQ4hO8BF,9z5U.E41.,.d1,E0G
 2O5sNC3a5GZjtNSagNN0U,7NGaUMHC5C36.GYZdR..u0Ub7PsFKbVdQG40..EulEPHIoY8L4nw
 gLF.3U..8ssP2Coru4UntIGqVyKr.o9XjF..Cb1,USdwl.,..A,,E.0U06.,6.,c.16QBaywPW
 .0.161lbA2TmNeHXGBnmcUXDF.sET1.4zVkk.Um,..QC.Ej2yHZCQCwBu32..W.0..0F6Cb.yy
 W8Utj00My7Yb6OQzL0yl1...
 --- end of encoding ---