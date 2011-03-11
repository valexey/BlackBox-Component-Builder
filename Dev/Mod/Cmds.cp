MODULE DevCmds; 
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
		Services, Files, Fonts, Ports, Stores, Models, Views, Controllers, Dialog,
		Containers, Controls, Properties, Documents,
		TextModels, TextMappers, TextRulers, TextViews, TextControllers,
		StdDialog, StdCmds,
		HostMenus;


	PROCEDURE OpenModuleList*;
		VAR v: Views.View;
			c: TextControllers.Controller; s: TextMappers.Scanner;
			loc: Files.Locator; name: Files.Name;
			beg, end: INTEGER; error, one: BOOLEAN;
	BEGIN
		c := TextControllers.Focus();
		IF c # NIL THEN
			s.ConnectTo(c.text);
			IF c.HasSelection() THEN c.GetSelection(beg, end)
			ELSE beg := 0; end := c.text.Length()
			END;
			s.SetPos(beg); s.Scan; one := FALSE;
			WHILE (s.start < end) & (s.type = TextMappers.string) & (s.len < LEN(name)) DO
				s.Scan; one := TRUE
			END;
			IF one & ((s.start >= end) OR (s.type = TextMappers.eot)) THEN
				s.SetPos(beg); s.Scan; error := FALSE;
				WHILE (s.start < end) & (s.type = TextMappers.string) & ~error DO
					StdDialog.GetSubLoc(s.string, "Mod", loc, name);
					IF loc # NIL THEN
						v := Views.OldView(loc, name);
						IF v # NIL THEN
							Views.Open(v, loc, name, NIL)
						ELSE
							Dialog.ShowParamMsg("#System:CannotOpenFile", name, "", "");
							error := TRUE
						END
					ELSE
						Dialog.ShowParamMsg("#Dev:ModuleNotFound", s.string, "", "");
						error := TRUE
					END;
					s.Scan
				END
			ELSE Dialog.ShowMsg("#Dev:NotOnlyModNames")
			END;
			s.ConnectTo(NIL)
		ELSE Dialog.ShowMsg("#Text:FocusShouldBeText")
		END
	END OpenModuleList;
	
	PROCEDURE Scan (VAR s: TextMappers.Scanner);
		VAR ch: CHAR; i, max: INTEGER;
	BEGIN
		s.Skip(ch);
		IF s.type # TextMappers.eot THEN
			IF ch = '"' THEN s.Scan;
			ELSE i := 0; max := LEN(s.string)-1;
				WHILE (i < max) & ~s.rider.eot & (ch > " ") DO
					s.string[i] := ch; s.rider.ReadChar(ch); INC(i);
				END;
				s.string[i] := 0X; s.len := SHORT(i);
				IF s.len > 0 THEN s.type := TextMappers.string
				ELSE s.type := TextMappers.invalid
				END
			END
		END
	END Scan;
	
	PROCEDURE OpenFileList*;	(* Pre: focus is text and has a selection *)
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end: INTEGER;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN 
			c.GetSelection(beg, end);
			s.ConnectTo(c.text);
			s.SetPos(beg); Scan(s);
			WHILE (s.type # TextMappers.eot) & (s.start < end) DO
				IF s.type = TextMappers.string THEN StdCmds.OpenDoc(s.string) END;
				Scan(s);
			END
		ELSE Dialog.ShowMsg("#Text:FocusShouldBeText")
		END
	END OpenFileList;

	PROCEDURE FlushResources*;
		VAR m: HostMenus.Menu; i: StdDialog.Item;
	BEGIN
		(* flush menu guards *)
		m := HostMenus.FirstMenu();
		WHILE m # NIL DO
			i := m.firstItem; WHILE i # NIL DO StdDialog.ClearGuards(i); i := i.next END;
			m := m.next
		END;
		(* flush string cache *)
		Dialog.FlushMappings
	END FlushResources;

	PROCEDURE RevalidateView*;
		VAR ops: Controllers.PollOpsMsg; v: Views.View;
	BEGIN
		Controllers.PollOps(ops);
		IF ops.singleton # NIL THEN v := ops.singleton
		ELSE v := Controllers.FocusView();
		END;
		IF v # NIL THEN Views.RevalidateView(v) END
	END RevalidateView;
	
	PROCEDURE RevalidateViewGuard* (VAR par: Dialog.Par);
		VAR ops: Controllers.PollOpsMsg; v: Views.View;
	BEGIN
		Controllers.PollOps(ops);
		IF ops.singleton # NIL THEN v := ops.singleton
		ELSE v := Controllers.FocusView();
		END;
		IF (v = NIL) OR ~Views.IsInvalid(v) THEN par.disabled := TRUE END
	END RevalidateViewGuard;

	PROCEDURE SetDefault (c: Views.View; default: BOOLEAN);
		VAR p: Controls.Prop; msg: Properties.SetMsg;
	BEGIN
		NEW(p); p.valid := {Controls.opt0}; p.opt[Controls.default] := default;
		msg.prop := p; Views.HandlePropMsg(c, msg)
	END SetDefault;

	PROCEDURE SetDefaultButton*;
		VAR s, v: Views.View; c: Containers.Controller; script: Stores.Operation;
	BEGIN
		c := Containers.Focus();
		IF c # NIL THEN
			s := c.Singleton();
			c.SelectAll(FALSE);
			Views.BeginScript(c.ThisView(), "#Dev:SetDefault", script);
			c.GetFirstView(Containers.any, v);
			WHILE v # NIL DO SetDefault(v, FALSE); c.GetNextView(Containers.any, v) END;
			IF s # NIL THEN SetDefault(s, TRUE); c.SetSingleton(s) END;
			Views.EndScript(c.ThisView(), script)
		ELSE Dialog.Beep
		END
	END SetDefaultButton;

	PROCEDURE SetCancel (c: Views.View; cancel: BOOLEAN);
		VAR p: Controls.Prop; msg: Properties.SetMsg;
	BEGIN
		NEW(p); p.valid := {Controls.opt1}; p.opt[Controls.cancel] := cancel;
		msg.prop := p; Views.HandlePropMsg(c, msg)
	END SetCancel;

	PROCEDURE SetCancelButton*;
		VAR s, v: Views.View; c: Containers.Controller; script: Stores.Operation;
	BEGIN
		c := Containers.Focus();
		IF c # NIL THEN
			s := c.Singleton();
			c.SelectAll(FALSE);
			Views.BeginScript(c.ThisView(), "#Dev:SetCancel", script);
			c.GetFirstView(Containers.any, v);
			WHILE v # NIL DO SetCancel(v, FALSE); c.GetNextView(Containers.any, v) END;
			IF s # NIL THEN SetCancel(s, TRUE); c.SetSingleton(s) END;
			Views.EndScript(c.ThisView(), script)
		ELSE Dialog.Beep
		END
	END SetCancelButton;

	PROCEDURE NewRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR p: TextRulers.Prop;
	BEGIN
		NEW(p);
		p.valid := {TextRulers.right, TextRulers.tabs, TextRulers.opts};
		p.opts.val := {TextRulers.rightFixed}; p.opts.mask := p.opts.val;
		p.tabs.len := 7;
		p.right := 200 * mm;
		p.tabs.tab[0].stop := 40 * mm; p.tabs.tab[0].type := {TextRulers.rightTab};
		p.tabs.tab[1].stop := 50 * mm; p.tabs.tab[1].type := {TextRulers.rightTab};
		p.tabs.tab[2].stop := 55 * mm;
		p.tabs.tab[3].stop := 90 * mm;
		p.tabs.tab[4].stop := 100 * mm;
		p.tabs.tab[5].stop := 140 * mm;
		p.tabs.tab[6].stop := 180 * mm; p.tabs.tab[6].type := {TextRulers.rightTab};
		RETURN TextRulers.dir.NewFromProp(p)
	END NewRuler;

	PROCEDURE ShowControlList*;
	(** Guard: StdCmds.ContainerGuard **)
		VAR c: Containers.Controller; v: Views.View; s, t: Stores.TypeName;
			msg: Properties.PollMsg; p: Properties.Property;
			out: TextMappers.Formatter; str: Dialog.String; w, h: INTEGER;

		PROCEDURE WriteEntry(name, value: Dialog.String);
		BEGIN
			out.WriteTab; out.WriteString(name); out.WriteTab; out.WriteString(value); out.WriteLn
		END WriteEntry;

	BEGIN
		c := Containers.Focus();
		IF c # NIL THEN
			out.ConnectTo(TextModels.dir.New());out.WriteView(NewRuler());
			out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {Fonts.italic}));
			out.WriteString("type"); out.WriteTab;
			out.WriteString("width"); out.WriteTab;
			out.WriteString("height"); out.WriteTab;
			out.WriteString("label"); out.WriteTab;
			out.WriteString("link"); out.WriteTab;
			out.WriteString("guard"); out.WriteTab;
			out.WriteString("notifier"); out.WriteTab;
			out.WriteString("level"); out.WriteTab;
			out.WriteLn; out.WriteLn;
			out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {}));
			c.GetFirstView(Containers.any, v);
			WHILE v # NIL DO
				Services.GetTypeName(v, t);
				s := "#Dev:"; s := s + t;
				Dialog.MapString(s, s);
				out.WriteString(s); out.WriteTab;
				v.context.GetSize(w, h);
				out.WriteInt(w DIV Ports.point); out.WriteTab;
				out.WriteInt(h DIV Ports.point); out.WriteTab;
				msg.prop := NIL; Views.HandlePropMsg(v, msg);
				p := msg.prop; WHILE (p # NIL) & ~(p IS Controls.Prop) DO p := p.next END;
				IF p # NIL THEN
					WITH p: Controls.Prop DO
						IF Controls.label IN p.valid THEN out.WriteString(p.label) END; out.WriteTab;
						IF Controls.link IN p.valid THEN out.WriteString(p.link) END; out.WriteTab;
						IF Controls.level IN p.valid THEN out.WriteTab; out.WriteTab; out.WriteInt(p.level) END;
						IF (Controls.guard IN p.valid) & (p.guard # "") OR
							(Controls.notifier IN p.valid) & (p.notifier # "") THEN
							out.WriteLn; out.WriteTab; out.WriteTab; out.WriteTab; out.WriteTab; out.WriteTab;
							IF Controls.guard IN p.valid THEN out.WriteString(p.guard) END; out.WriteTab;
							IF Controls.notifier IN p.valid THEN out.WriteString(p.notifier) END
						END
					END
				END;
				out.WriteLn;
				c.GetNextView(Containers.any, v)
			END;
			Views.OpenAux(Documents.dir.New(TextViews.dir.New(out.rider.Base()), 200*Ports.mm, -1), "Controls");
		END
	END ShowControlList;

END DevCmds.
