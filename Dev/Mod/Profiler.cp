MODULE DevProfiler;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Ilya E. Ermakov"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= ""
	issues	= ""

**)

	IMPORT
		WinApi, WinMM,
		Kernel, Dialog, Fonts, Ports, Views,
		TextModels, TextRulers, TextViews, TextControllers, TextMappers, StdLog;
	
	CONST
		maxMod = 256;
		
	TYPE
		Module = RECORD
			start, end: INTEGER;	(* address range *)
			first, last: INTEGER;	(* procedures *)
			mod: Kernel.Module;
			count: INTEGER;
		END;
		
		Proc = POINTER TO RECORD
			name: Kernel.Name;
			p: INTEGER;
			next: Proc
		END;
		
	VAR
		mod: ARRAY maxMod OF Module;
		pEnd, pCount: POINTER TO ARRAY OF INTEGER;
		numMod, numProc, listLen: INTEGER;
		otherCount, totalCount: INTEGER;
		out: TextMappers.Formatter;
		started: BOOLEAN;
		
	(* ------------- non-portable part ------------ *)
	
	VAR id, periode: INTEGER;
	
	PROCEDURE^ Count (pc: INTEGER);
	
	PROCEDURE Recorder (id, msg, user, dw1, dw2: INTEGER);
		VAR res: INTEGER; context: WinApi.CONTEXT;
	BEGIN
		context.ContextFlags := WinApi.CONTEXT_CONTROL;
		res := WinApi.GetThreadContext(user, context);
		Count(context.Eip)	(* current pc of main thread *)
	END Recorder;
	
	PROCEDURE StartRecording;
		(* Start calling Count periodically by an interrupt handler *)
		VAR res: INTEGER; main: WinApi.HANDLE; tc: WinMM.TIMECAPS;
	BEGIN
		res := WinApi.DuplicateHandle(WinApi.GetCurrentProcess(), WinApi.GetCurrentThread(),
					WinApi.GetCurrentProcess(), main, {1, 3, 4, 16..19}, 0, {});
		res := WinMM.timeGetDevCaps(tc, SIZE(WinMM.TIMECAPS));
		periode := tc.wPeriodMin * 3;
		res := WinMM.timeBeginPeriod(periode);
		id := WinMM.timeSetEvent(periode, 0, Recorder, main, WinMM.TIME_PERIODIC);
		ASSERT(id # 0, 100)
	END StartRecording;
	
	PROCEDURE StopRecording;
		(* stop calling Count *)
		VAR res: INTEGER;
	BEGIN
		res := WinMM.timeKillEvent(id);
		res := WinMM.timeEndPeriod(periode)
	END StopRecording;
	
	PROCEDURE Init;
	BEGIN
	END Init;
	
	(* ------------- portable part ------------- *)

	PROCEDURE Count (pc: INTEGER);
		VAR l, r, m: INTEGER;
	BEGIN
		l := 0; r := numMod;
		WHILE l < r DO	(* binary search on modules *)
			m := (l + r) DIV 2;
			IF pc >= mod[m].end THEN l := m + 1 ELSE r := m END
		END;
		IF (r < numMod) & (pc >= mod[r].start) & (pc < mod[r].end) THEN
			INC(mod[r].count); INC(totalCount);
			l := mod[r].first; r := mod[r].last + 1;
			WHILE l < r DO	(* binary search on procedures *)
				m := (l + r) DIV 2;
				IF pc >= pEnd[m] THEN l := m + 1 ELSE r := m END
			END;
			INC(pCount[r])
		ELSE INC(otherCount)
		END
	END Count;
	
	
	(* ---------- programming interface ---------- *)
	
	PROCEDURE InsertModule (m: Kernel.Module; VAR done: BOOLEAN);
		VAR i: INTEGER;
	BEGIN
		IF (m # NIL) & (numMod < maxMod) THEN
			i := numMod; INC(numMod);
			WHILE (i > 0) & (m.code < mod[i-1].mod.code) DO mod[i] := mod[i-1]; DEC(i) END;
			mod[i].mod := m;
			done := TRUE
		ELSE
			done := FALSE
		END
	END InsertModule;
	
	PROCEDURE Start*;
		VAR ref, end, i, j: INTEGER; n: Kernel.Name; m: Kernel.Module; ok: BOOLEAN;
	BEGIN
		IF ~started THEN
			IF listLen = 0 THEN	(* all modules *)
				m := Kernel.modList;
				WHILE m # NIL DO
					IF m.refcnt >= 0 THEN InsertModule(m, ok) END;
					m := m.next
				END
			END;
			otherCount := 0; totalCount := 0; numProc := 0; i := 0;
			WHILE i < numMod DO
				m := mod[i].mod;
				mod[i].start := m.code;
				mod[i].first := numProc;
				ref := m.refs;
				Kernel.GetRefProc(ref, end, n);
				WHILE end # 0 DO INC(numProc); Kernel.GetRefProc(ref, end, n) END;
				mod[i].last := numProc - 1;
				INC(i)
			END;
			NEW(pEnd, numProc);
			NEW(pCount, numProc);
			i := 0; j := 0;
			WHILE i < numMod DO
				m := mod[i].mod; ref := m.refs;
				Kernel.GetRefProc(ref, end, n);
				WHILE end # 0 DO
					pEnd[j] := m.code + end; pCount[j] := 0; INC(j);
					Kernel.GetRefProc(ref, end, n)
				END;
				mod[i].end := pEnd[mod[i].last];
				mod[i].count := 0;
				INC(i)
			END;
			IF numMod > 0 THEN StartRecording END;
			started := TRUE
		END
	END Start;
	
	PROCEDURE Stop*;
	BEGIN
		IF started THEN
			StopRecording;
			started := FALSE
		END
	END Stop;
	
	PROCEDURE Reset*;
	BEGIN
		Stop;
		numMod := 0; numProc := 0; listLen := 0;
		pEnd := NIL; pCount := NIL
	END Reset;
	
	PROCEDURE SetModuleList* (list: ARRAY OF CHAR);
		VAR i, j: INTEGER; name: ARRAY 256 OF CHAR; ch: CHAR; done: BOOLEAN;
	BEGIN
		Stop;
		Reset;
		i := 0;
		ch := list[i];
		WHILE (i < LEN(list)) & (ch # 0X) DO
			WHILE (ch # 0X) & (ch < "0") OR (ch > "9") & (CAP(ch) < "A") OR (CAP(ch) > "Z") DO INC(i); ch := list[i] END;
			IF ch # 0X THEN
				j := 0;
				WHILE (ch >= "0") & (ch <= "9") OR (CAP(ch) >= "A") & (CAP(ch) <= "Z") DO
					name[j] := ch; INC(j); INC(i); ch := list[i]
				END;
				name[j] := 0X;
				InsertModule(Kernel.ThisMod(name), done);
				IF done THEN INC(listLen) END
			END
		END
	END SetModuleList;
	
(*
	PROCEDURE GetModuleCount* (name: ARRAY OF CHAR; VAR count: LONGINT);
		VAR i: LONGINT;
	BEGIN
		i := 0;
		WHILE (i < numMod) & (mod[i].mod.name # name) DO INC(i) END;
		IF i < numMod THEN count := mod[i].count
		ELSE count := -1
		END
	END GetModuleCount;
	
	PROCEDURE GetProcedureCount* (modName, procName: ARRAY OF CHAR; VAR count: LONGINT);
		VAR i, j, last, ref, end: LONGINT; name: Kernel.Name;
	BEGIN
		i := 0;
		WHILE (i < numMod) & (mod[i].mod.name # modName) DO INC(i) END;
		IF i < numMod THEN 
			ref := mod[i].mod.refs; j := mod[i].first - 1; last := mod[i].last;
			REPEAT INC(j); Kernel.GetRefProc(ref, end, name); UNTIL (j > last) OR (name = procName);
			IF j <= last THEN count := pCount[j]
			ELSE count := -1
			END
		ELSE count := -1
		END
	END GetProcedureCount;
	
	PROCEDURE GetTotalCount* (VAR inModules, outside: LONGINT);
	BEGIN
		inModules := totalCount;
		outside := otherCount
	END GetTotalCount;
*)	
	(* ---------- menu commands ---------- *)
	
	PROCEDURE Execute*;
		VAR t0: LONGINT; res: INTEGER; str: Dialog.String;
	BEGIN
		t0 := Kernel.Time();
		Dialog.Call("DevDebug.Execute", "", res);
		StdLog.Int(SHORT((Kernel.Time() - t0) * 1000 DIV Kernel.timeResolution));
		Dialog.MapString("#Dev:msec", str);
		StdLog.Char(" "); StdLog.String(str);
		StdLog.Ln
	END Execute;
	
	PROCEDURE NewRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR p: TextRulers.Prop;
	BEGIN
		NEW(p);
		p.valid := {TextRulers.right, TextRulers.tabs, TextRulers.opts};
		p.opts.val := {TextRulers.rightFixed}; p.opts.mask := p.opts.val;
		p.right := 130 * mm;
		p.tabs.len := 5;
		p.tabs.tab[0].stop := 4 * mm; p.tabs.tab[1].stop := 50 * mm;
		p.tabs.tab[2].stop := 54 * mm; p.tabs.tab[3].stop := 70 * mm; 
		p.tabs.tab[4].stop := 74 * mm;
		RETURN TextRulers.dir.NewFromProp(p)
	END NewRuler;

	PROCEDURE ShowModule (VAR m: Module);
		VAR i, p, ref, end: INTEGER; proc, list, a, b: Proc;
	BEGIN
		ASSERT(totalCount > 0, 20);
		p := m.count * 100 DIV totalCount;
		IF p > 0 THEN
			out.WriteSString(m.mod.name); out.WriteTab; out.WriteTab;
			out.WriteIntForm(p, 10, 2, " ", FALSE); out.WriteLn;
			NEW(proc); list := NIL;
			ref := m.mod.refs; i := m.first;
			WHILE i <= m.last DO
				Kernel.GetRefProc(ref, end, proc.name);
				proc.p := pCount[i] * 100 DIV totalCount;
				IF proc.p > 0 THEN proc.next := list; list := proc; NEW(proc) END;
				INC(i)
			END;
			(* sort list *)
			WHILE list # NIL DO
				a := proc; b := proc.next;
				WHILE (b # NIL) & (list.p < b.p) DO a := b; b := b.next END;
				a.next := list; a := list.next; list.next := b; list := a
			END;
			list := proc.next;
			WHILE list # NIL DO
				out.WriteTab; out.WriteSString(list.name); out.WriteTab; out.WriteTab;
				out.WriteIntForm(list.p, 10, 2, " ", FALSE); out.WriteLn;
				list := list.next
			END;
			out.WriteLn;
		END
	END ShowModule;
	
	PROCEDURE ShowProfile*;
		VAR max, limit, maxi, pos, c, n, i: INTEGER; v: TextViews.View; a0: TextModels.Attributes; str: Views.Title;
	BEGIN
		Stop;
		out.ConnectTo(TextModels.dir.New());
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewStyle(a0, {Fonts.italic}));
		Dialog.MapString("#Dev:Module", str); out.WriteString(str);
		out.WriteTab; out.WriteTab;
		Dialog.MapString("#Dev:PercentPerModule", str); out.WriteString(str);
		out.WriteLn; out.WriteTab;
		Dialog.MapString("#Dev:Procedure", str); out.WriteString(str);
		out.WriteTab; out.WriteTab;
		Dialog.MapString("#Dev:PercentPerProc", str); out.WriteString(str);
		out.WriteLn; out.WriteLn;
		out.rider.SetAttr(a0);
		IF totalCount > 0 THEN
			n := numMod; limit := MAX(INTEGER);
			WHILE n > 0 DO
				i := 0; max := -1;
				WHILE i < numMod DO
					c := mod[i].count;
					IF (c > max) & ((c < limit) OR (c = limit) & (i > pos)) THEN max := c; maxi := i END;
					INC(i)
				END;
				ShowModule(mod[maxi]);
				pos := maxi; limit := max;
				DEC(n)
			END
		END;
		Dialog.MapString("#Dev:Samples", str); out.WriteString(str);
		out.WriteTab; out.WriteTab; out.WriteInt(totalCount + otherCount);
		out.WriteTab; out.WriteTab; out.WriteString("100%"); out.WriteLn;
		out.WriteTab;
		Dialog.MapString("#Dev:InProfiledModules", str); out.WriteString(str);
		out.WriteTab; out.WriteTab; 
		out.WriteInt(totalCount); out.WriteTab; out.WriteTab;
		IF totalCount + otherCount > 0 THEN
			n := totalCount * 100 DIV (totalCount + otherCount)
		ELSE
			n := 0
		END;
		out.WriteInt(n); out.WriteChar("%"); out.WriteLn;
		out.WriteTab;
		Dialog.MapString("#Dev:Other", str); out.WriteString(str);
		out.WriteTab; out.WriteTab; 
		out.WriteInt(otherCount); out.WriteTab; out.WriteTab;
		out.WriteInt(100 - n); out.WriteChar("%"); out.WriteLn;
		v := TextViews.dir.New(out.rider.Base());
		v.SetDefaults(NewRuler(), TextViews.dir.defAttr);
		Dialog.MapString("#Dev:Profile", str);
		Views.OpenAux(v, str);
		out.ConnectTo(NIL)
	END ShowProfile;
	
	PROCEDURE SetProfileList*;
		VAR beg, end: INTEGER; s: TextMappers.Scanner;
			c: TextControllers.Controller; done: BOOLEAN;
	BEGIN
		IF ~started THEN
			Reset;
			c := TextControllers.Focus();
			IF c # NIL THEN
				s.ConnectTo(c.text);
				IF c.HasSelection() THEN c.GetSelection(beg, end)
				ELSE beg := 0; end := c.text.Length()
				END;
				Reset;
				s.SetPos(beg); s.Scan;
				WHILE (s.start < end) & (s.type # TextMappers.eot) DO
					IF s.type = TextMappers.string THEN
						IF numMod < maxMod THEN
							InsertModule(Kernel.ThisMod(s.string), done);
							IF done THEN INC(listLen)
							ELSE Dialog.ShowParamMsg("module ^0 not found", s.string, "", "")
							END
						ELSE
							Dialog.ShowMsg("too many modules");
							RETURN
						END
					END;
					s.Scan
				END
			END
		END
	END SetProfileList;
	
	PROCEDURE StartGuard* (VAR par: Dialog.Par);
	BEGIN
		IF started THEN par.disabled := TRUE END
	END StartGuard;

	PROCEDURE StopGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~started THEN par.disabled := TRUE END
	END StopGuard;

BEGIN
	Init
END DevProfiler.


STRINGS

msec	msec
Module	Module
PercentPerModule	% per module
Procedure	Procedure
PercentPerProc	% per procedure
Samples	samples:
InProfiledModules	in profiled modules
Other	other
Profile	Profile


Menus

	"Set Profile List"	""	"DevProfiler.SetProfileList"	"DevProfiler.StartGuard"
	"Start Profiler"	""	"DevProfiler.Start"	"DevProfiler.StartGuard"
	"Stop Profiler"	""	"DevProfiler.Stop; DevProfiler.ShowProfile"	"DevProfiler.StopGuard"
	"Execute"	""	"DevProfiler.Execute"	"TextCmds.SelectionGuard"


