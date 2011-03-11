MODULE DevInspector;
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

	IMPORT Kernel, Services, Stores, Views, Controllers, Properties, Containers, Dialog, Controls;
	
	CONST
		multiView = TRUE;
			
	TYPE
		Action = POINTER TO RECORD (Services.Action) END;

	VAR
		inspect*: RECORD
			control-: Dialog.String;
			label*: Dialog.String; (* ARRAY 40 OF CHAR; *)
			link*, guard*, notifier*: Dialog.String;
			level*: INTEGER;
			opt0*, opt1*, opt2*, opt3*, opt4*: BOOLEAN;
			known, valid: SET;
			type: Stores.TypeName
		END;
		fingerprint: INTEGER;
		action: Action;
	
	PROCEDURE GetTypeName (v: Views.View; VAR t, s: ARRAY OF CHAR);
		VAR c: Containers.Controller; w: Views.View; subs, cntr: Dialog.String;
	BEGIN
		s := ""; t := "";
		IF multiView THEN
			c := Containers.Focus();
			IF c # NIL THEN
				c.GetFirstView(Containers.selection, v); w := v;
				WHILE (w # NIL) & Services.SameType(w, v) DO c.GetNextView(Containers.selection, w) END;
				IF w # NIL THEN v := NIL END
			END
		END;
		IF v # NIL THEN
			Services.GetTypeName(v, t); Kernel.SplitName(t, subs, cntr);
			Dialog.MapString("#" + subs + ":" + cntr, s);
			IF s = cntr THEN Dialog.MapString("#Dev:" + t, s) END
		END
	END GetTypeName;

	PROCEDURE PollProp (v: Views.View);
		VAR msg: Properties.PollMsg; q: Properties.Property; p: Controls.Prop;
	BEGIN
		inspect.control := ""; inspect.link := ""; inspect.label := '';
		inspect.guard := ""; inspect.notifier := ""; inspect.level := 0;
		inspect.opt0 := FALSE; inspect.opt1 := FALSE;
		inspect.opt2 := FALSE; inspect.opt3 := FALSE; inspect.opt4 := FALSE;
		inspect.known := {}; inspect.valid := {};
		IF multiView OR (v # NIL) THEN
			GetTypeName(v, inspect.type, inspect.control);
			IF multiView THEN
				Properties.CollectProp(q)
			ELSE
				msg.prop := NIL; Views.HandlePropMsg(v, msg); q := msg.prop
			END;
			WHILE (q # NIL) & ~(q IS Controls.Prop) DO q := q.next END;
			IF q # NIL THEN
				p := q(Controls.Prop);
				inspect.known := p.known; inspect.valid := p.valid;
				IF Controls.link IN p.valid THEN inspect.link := p.link$ END;
				IF Controls.label IN p.valid THEN inspect.label := p.label$ END;
				IF Controls.guard IN p.valid THEN inspect.guard := p.guard$ END;
				IF Controls.notifier IN p.valid THEN inspect.notifier := p.notifier$ END;
				IF Controls.level IN p.valid THEN inspect.level := p.level END;
				IF Controls.opt0 IN p.valid THEN inspect.opt0 := p.opt[0] END;
				IF Controls.opt1 IN p.valid THEN inspect.opt1 := p.opt[1] END;
				IF Controls.opt2 IN p.valid THEN inspect.opt2 := p.opt[2] END;
				IF Controls.opt3 IN p.valid THEN inspect.opt3 := p.opt[3] END;
				IF Controls.opt4 IN p.valid THEN inspect.opt4 := p.opt[4] END
			END
		END;
		Dialog.Update(inspect)
	END PollProp;

	PROCEDURE SetProp (v: Views.View);
		VAR p: Controls.Prop; msg: Properties.SetMsg;
	BEGIN
		IF multiView OR (v # NIL) THEN
			NEW(p);
			p.valid := inspect.valid;
			p.link := inspect.link$; p.label := inspect.label$;
			p.guard := inspect.guard$; p.notifier := inspect.notifier$;
			p.level := inspect.level;
			p.opt[0] := inspect.opt0;
			p.opt[1] := inspect.opt1;
			p.opt[2] := inspect.opt2;
			p.opt[3] := inspect.opt3;
			p.opt[4] := inspect.opt4;
			IF multiView THEN Properties.EmitProp(NIL, p)
			ELSE msg.old := NIL; msg.prop := p; Views.HandlePropMsg(v, msg)
			END
		END
	END SetProp;

	PROCEDURE Singleton (): Views.View;
		VAR v: Views.View;
	BEGIN
		v := Containers.FocusSingleton();
		RETURN v
	END Singleton;

	PROCEDURE (a: Action) Do;
		VAR c: Containers.Controller; v: Views.View; fp: INTEGER;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		IF multiView THEN
			c := Containers.Focus(); fp := 0;
			IF c # NIL THEN
				c.GetFirstView(TRUE, v);
				WHILE v # NIL DO fp := fp + Services.AdrOf(v); c.GetNextView(TRUE, v) END
			END
		ELSE
			v := Singleton();
			IF v = NIL THEN fp := 0 ELSE fp := Services.AdrOf(v) END
		END;
		IF fp # fingerprint THEN PollProp(v); fingerprint := fp END;
		Controllers.ResetCurrentPath();
		Services.DoLater(action, Services.Ticks() + Services.resolution DIV 2)
	END Do;

	PROCEDURE InitDialog*;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		PollProp(Singleton());
		Controllers.ResetCurrentPath()
	END InitDialog;

	PROCEDURE GetNext*;
		VAR c: Containers.Controller; v: Views.View;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		c := Containers.Focus();
		IF c # NIL THEN
			IF c.HasSelection() THEN v := c.Singleton() ELSE v := NIL END;
			IF v = NIL THEN
				c.GetFirstView(Containers.any, v)
			ELSE
				c.GetNextView(Containers.any, v);
				IF v = NIL THEN c.GetFirstView(Containers.any, v) END
			END;
			c.SelectAll(FALSE);
			IF v # NIL THEN c.SetSingleton(v); c.MakeViewVisible(v) END;
			PollProp(v)
		ELSE Dialog.ShowMsg("#Dev:NoTargetFocusFound")
		END;
		Controllers.ResetCurrentPath()
	END GetNext;

	PROCEDURE Set*;
		VAR v: Views.View;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		v := Singleton();
		IF multiView OR (v # NIL) THEN SetProp(v); PollProp(v) END;
		Controllers.ResetCurrentPath()
	END Set;

	PROCEDURE MapLabel (
		IN control: Stores.TypeName; IN type: Dialog.String; i: INTEGER; VAR label: Dialog.String
	);
		VAR l, k, subs, cntr: Dialog.String; si: ARRAY 2 OF CHAR;
	BEGIN
		Kernel.SplitName(control, subs, cntr);
		si[0] := CHR(i + ORD("0")); si[1] := 0X;
		k := cntr + "." + type + si;
		Dialog.MapString("#" + subs + ":" + k, l);
		IF l$ # k$ THEN label := l$ END
	END MapLabel;

	PROCEDURE OptGuard* (opt: INTEGER; VAR par: Dialog.Par);
		VAR name: ARRAY 64 OF CHAR; num: ARRAY 2 OF CHAR;
	BEGIN
		ASSERT((opt >= 0) & (opt <= 9), 20);
		IF ~(opt IN inspect.known) OR (inspect.type = "") THEN
			par.disabled := TRUE
		ELSE
			num[0] := CHR(opt + ORD("0")); num[1] := 0X;
			name := inspect.type + ".Opt" + num;
			par.label := "#Dev:" + name;
			Dialog.MapString(par.label, par.label);
			IF par.label = name THEN par.label := "" END;
			IF ~(opt IN inspect.valid) THEN par.undef := TRUE END;
			MapLabel(inspect.type, "Opt", opt, par.label)
		END
	END OptGuard;

	PROCEDURE LevelGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~(Controls.level IN inspect.known) THEN par.disabled := TRUE
		ELSIF ~(Controls.level IN inspect.valid) THEN par.undef := TRUE
		END;
		IF inspect.type # "" THEN MapLabel(inspect.type, "Field", 5, par.label) END
	END LevelGuard;

	PROCEDURE NotifierGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~(Controls.notifier IN inspect.known) THEN par.disabled := TRUE
		ELSIF ~(Controls.notifier IN inspect.valid) THEN par.undef := TRUE
		END;
		IF inspect.type # "" THEN MapLabel(inspect.type, "Field", 4, par.label) END
	END NotifierGuard;
	
	PROCEDURE GuardGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~(Controls.guard IN inspect.known) THEN par.disabled := TRUE
		ELSIF ~(Controls.guard IN inspect.valid) THEN par.undef := TRUE
		END;
		IF inspect.type # "" THEN MapLabel(inspect.type, "Field", 3, par.label) END
	END GuardGuard;
	
	PROCEDURE LabelGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~(Controls.label IN inspect.known) THEN par.disabled := TRUE
		ELSIF ~(Controls.label IN inspect.valid) THEN par.undef := TRUE
		END;
		IF inspect.type # "" THEN MapLabel(inspect.type, "Field", 2, par.label) END
	END LabelGuard;
	
	PROCEDURE LinkGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~(Controls.link IN inspect.known) THEN par.disabled := TRUE
		ELSIF ~(Controls.link IN inspect.valid) THEN par.undef := TRUE
		END;
		IF inspect.type # "" THEN MapLabel(inspect.type, "Field", 1, par.label) END
	END LinkGuard;
	
	PROCEDURE ControlGuard* (VAR par: Dialog.Par);
	BEGIN
		IF inspect.known = {} THEN par.disabled := TRUE END
	END ControlGuard;

	PROCEDURE Notifier* (idx, op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.changed THEN INCL(inspect.valid, idx) END
	END Notifier;

BEGIN
	NEW(action); Services.DoLater(action, Services.now)
END DevInspector.




 DevCompiler.Compile

 " DevInspector.InitDialog; OpenAuxDialog('#Dev:inspect', 'Inspect') "

 " Unload('DevInspector'); DevInspector.InitDialog; OpenAuxDialog('DevInspector.inspect', 'Inspect') "
