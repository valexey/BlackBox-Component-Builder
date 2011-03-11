MODULE DevDependencies;
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
		Kernel, Files, Dialog, TextMappers, TextControllers, StdDialog, StdFolds, Strings, Views, Ports, 
		Fonts, Properties, Controllers, Stores, HostMenus, HostPorts, StdCmds, Math, 
		TextModels, TextViews, Dates, DevCommanders;
	
	CONST 
		bold = 0; italic = 1;	(* used with SetStyle *)
		full = TRUE;			(* used with DependencyList *)
		implicit = TRUE; 	   (* used with GetNode *)
		
	TYPE 
		String = ARRAY 256 OF CHAR;
		
		(** Data structure types **)
		
		Subgraph = POINTER TO RECORD (Stores.Store)
			node: List;
			isImplicit: BOOLEAN;
			next: Subgraph
		END;
		SubsystemList = POINTER TO RECORD (Stores.Store)
			name:  String;
			isExpanded, isLayedout, isHidden, isBasic, isStart, isSelected: BOOLEAN;
			l, b, w, h, level: INTEGER;
			next: SubsystemList
		END;
		List = POINTER TO RECORD (Stores.Store)
			name: String;
			imports: Subgraph;
			subsystem: SubsystemList;
			l, b, w, h: INTEGER;
			isLayedout, isHidden, isStart, isSelected, isImported, onlyImplicit: BOOLEAN;
			next: List
		END;
		NameList = POINTER TO RECORD (Stores.Store)
			name: String; 
			next: NameList
		END;
		
		(** View type **)
		
		View = POINTER TO RECORD (Views.View)
			list: List;
			startList: NameList;
			subsystems: SubsystemList;
			maxLevel, nofMods: INTEGER;
			selfont, normfont: Fonts.Font;
			showBasic: BOOLEAN
		END;
		
		(** Meta types **)
		
		MetaList = POINTER TO RECORD list: List; next: MetaList END;
		MetaSSList = POINTER TO RECORD sslist: SubsystemList; next: MetaSSList END;
		
		(** Operation types **)
		
		DepOp = POINTER TO ABSTRACT RECORD (Stores.Operation)
			view: View;
			mods: MetaList; subs: MetaSSList
		END;
		ExpandOp = POINTER TO RECORD (DepOp) expand: BOOLEAN END;
		MoveOp = POINTER TO RECORD (DepOp) dx, dy, l, t, r, b: INTEGER END;
		HideOp = POINTER TO RECORD (DepOp) END;
		HideBasicOp = POINTER TO RECORD (Stores.Operation) view: View END;
		ShowAllOp = POINTER TO RECORD (DepOp) END;	
		ExpandAllOp = POINTER TO RECORD (DepOp) expand: BOOLEAN END;	
			
	(** Forward declaragions **)		

	PROCEDURE^ StartAnalysis (startList: NameList; font: Fonts.Font; incImpl: BOOLEAN; OUT v: View);
	
	(** Auxiliary procedures **)
	
	PROCEDURE RWord (reader: Files.Reader; OUT x: INTEGER);
		VAR b: BYTE; y: INTEGER;
	BEGIN
		reader.ReadByte(b); y := b MOD 256;
		reader.ReadByte(b); y := y + 100H * (b MOD 256);
		reader.ReadByte(b); y := y + 10000H * (b MOD 256);
		reader.ReadByte(b); x := y + 1000000H * b
	END RWord;
	
	PROCEDURE RNum (reader: Files.Reader; OUT x: INTEGER);
		VAR b: BYTE; s, y: INTEGER;
	BEGIN
		s := 0; y := 0; reader.ReadByte(b);
		WHILE b < 0 DO INC(y, ASH(b + 128, s)); INC(s, 7); reader.ReadByte(b) END;
		x := ASH((b + 64) MOD 128 - 64, s) + y
	END RNum;
	
	PROCEDURE RName (reader: Files.Reader; OUT name: ARRAY OF CHAR);
		VAR b: BYTE; i, n: INTEGER;
	BEGIN
		i := 0; n := LEN(name) - 1; reader.ReadByte(b);
		WHILE (i < n) & (b # 0) DO name[i] := SHORT(CHR(b)); INC(i); reader.ReadByte(b) END;
		WHILE b # 0 DO reader.ReadByte(b) END;
		name[i] := 0X
	END RName;
				
	PROCEDURE GetImports(IN name: String; 
									OUT imps: POINTER TO ARRAY OF String; OUT nofImps: INTEGER; OUT found: BOOLEAN);
		VAR 
			file: Files.File; loc: Files.Locator; fileName: Files.Name; reader: Files.Reader;
			n, i, j, tmpWord: INTEGER; 
			impName, dollarStr: String; tmpImps: POINTER TO ARRAY OF String;
	BEGIN
		found := TRUE;
		StdDialog.GetSubLoc(name, "Code", loc, fileName);
		file := Files.dir.Old(loc, fileName, Files.shared);
		IF file # NIL THEN
			reader := file.NewReader(NIL); reader.SetPos(0); RWord(reader, n);
			IF n = 6F4F4346H THEN
				RWord(reader, tmpWord); RWord(reader, tmpWord); RWord(reader, tmpWord); RWord(reader, tmpWord);
				RWord(reader, tmpWord); RWord(reader, tmpWord); RNum(reader, n); RName(reader, impName); 
				i := 0; nofImps := n; IF nofImps > 0 THEN NEW(imps, nofImps) END;
				FOR i :=0 TO n - 1 DO
					RName(reader, impName); Strings.Extract(impName$, 0, 1, dollarStr);
					IF dollarStr = "$" THEN (* To avoid analysis of windows platform imports *)
						imps[i] := ""; DEC(nofImps)
					ELSE
						imps[i] := impName$
					END
				END;
				IF nofImps # n THEN (* some names are empty, delete them *)
					IF nofImps = 0 THEN 
						imps := NIL
					ELSE
						NEW(tmpImps, nofImps); j := 0;
						FOR i := 0 TO n-1 DO IF imps[i] # "" THEN tmpImps[j] := imps[i]; INC(j) END END;
						imps := tmpImps
					END
				END
			END
		ELSE 
			found := FALSE
		END
	END GetImports;
	
	PROCEDURE GetImplicitDependencies(
		IN name: String; OUT imps: POINTER TO ARRAY OF String; OUT nofImps: INTEGER
	);
		VAR deps: String; t: TextModels.Model; s: TextMappers.Scanner; i: INTEGER;
	BEGIN
		Dialog.MapString("#Dev:Implicit." + name, deps);
		IF deps # "Implicit." + name THEN
			t := TextModels.dir.NewFromString(deps); s.ConnectTo(t); s.SetPos(0); s.Scan; i := 0;
			WHILE s.type # TextMappers.eot DO
				IF s.type = TextMappers.string THEN INC(i) END;
				s.Scan
			END;
			nofImps := i; NEW(imps, nofImps); 
			s.SetPos(0); s.Scan; i := 0;
			WHILE s.type # TextMappers.eot DO
				IF s.type = TextMappers.string THEN imps[i] := s.string$; INC(i) END;
				s.Scan
			END
		ELSE
			imps := NIL; nofImps := 0
		END
	END GetImplicitDependencies;
	
	PROCEDURE GetModuleList (OUT modList: NameList);
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end: INTEGER; l: NameList;
	BEGIN
		modList := NIL;
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			s.ConnectTo(c.text); s.SetPos(beg); s.Scan;
			WHILE (s.start < end) & (s.type # TextMappers.invalid) DO
				IF (s.type = TextMappers.string) THEN
					NEW(l); IF modList # NIL THEN Stores.Join(modList, l) END;
					l.next := modList; l.name := s.string$; modList := l; s.Scan
				ELSE
					s.Scan
				END
			END
		END
	END GetModuleList;
	
	(** List **)
	
	PROCEDURE (l: List) GetExtent(OUT w, h: INTEGER), NEW;
	BEGIN
		IF l.subsystem.isExpanded THEN w := l.w; h:= l.h
		ELSE w := l.subsystem.w; h := l.subsystem.h END
	END GetExtent;

	PROCEDURE (l: List) GetPosition(OUT x, y: INTEGER), NEW;
	BEGIN
		IF l.subsystem.isExpanded THEN x := l.l; y:= l.b
		ELSE x := l.subsystem.l; y := l.subsystem.b END
	END GetPosition;
	
	PROCEDURE (l: List) SetPosition(x, y: INTEGER), NEW;
	BEGIN
		IF x < 1 THEN x := 1 END; IF y < 1 THEN y := 1 END; (* To keep objects in the second quadrant *)
		IF l.subsystem.isExpanded THEN  l.l := x; l.b:= y
		ELSE l.subsystem.l := x; l.subsystem.b := y END
	END SetPosition;
	
	PROCEDURE (l: List) Selected(): BOOLEAN, NEW;
	BEGIN
		RETURN l.subsystem.isExpanded & l.isSelected
	END Selected;
	
	PROCEDURE (l: List) Visible(showBasic: BOOLEAN): BOOLEAN, NEW;
	BEGIN
		RETURN l.subsystem.isExpanded
			& ~l.isHidden & ~l.subsystem.isHidden & (~l.subsystem.isBasic OR showBasic)
	END Visible;
	
	
	(** SubsystemList **)
	
	PROCEDURE (s: SubsystemList) Selected(): BOOLEAN, NEW;
	BEGIN
		RETURN ~s.isExpanded & s.isSelected
	END Selected;
	
	PROCEDURE (s: SubsystemList) Visible(showBasic: BOOLEAN): BOOLEAN, NEW;
	BEGIN
		RETURN ~s.isExpanded & ~s.isHidden & (~s.isBasic OR showBasic)
	END Visible;
	
	
	(** View **)
	
	PROCEDURE (v: View) GetNode (name: String; incImpl: BOOLEAN; VAR node: List), NEW;
		VAR 
			tmpList, tmpNode: List; sl: NameList;
			tmpGraph, prevGraph: Subgraph;
			nofImps, i: INTEGER; found: BOOLEAN;
			head, tail: String;
			sslist: SubsystemList;
			imps: POINTER TO ARRAY OF String;
	BEGIN
		(* First check if a node with the sought name exists in the nodeList. 
		   Otherwise build a new subgraph from objectfiles. *)
		tmpList := v.list;
		WHILE (tmpList # NIL) & (tmpList.name # name) DO tmpList := tmpList.next END;
		IF tmpList # NIL THEN
			node := tmpList
		ELSE
			(* Create a new node from an objectfile. *)
			GetImports(name, imps, nofImps, found); 
			IF found THEN
				NEW(tmpList); Stores.Join(v, tmpList); INC(v.nofMods);
				tmpList.name := name; 
				(* if subsystem exists then use it otherwise create a new one *) 
				Kernel.SplitName(name, head, tail); IF head = "" THEN head := "System" END;
				sslist := v.subsystems;
				WHILE (sslist # NIL) & (sslist.name # head) DO sslist := sslist.next END;
				IF sslist = NIL THEN
					NEW (sslist); Stores.Join(v, sslist); sslist.name := head; sslist.next := v.subsystems;
					sslist.level := v.maxLevel; sslist.isExpanded := FALSE;
					v.subsystems := sslist 
				END;
				sl := v.startList; (* If it is one of the start modules it should be expanded *)
				WHILE (sl # NIL) & (sl.name # name) DO sl := sl.next END; 
				IF sl # NIL THEN
					sslist.isExpanded := TRUE; sslist.level := 1; sslist.isStart := TRUE; tmpList.isStart := TRUE
				END;
				tmpList.subsystem := sslist; tmpList.imports := NIL; 
				tmpList.next := v.list; v.list := tmpList; tmpGraph := NIL;
				FOR i := 0 TO nofImps - 1 DO
					prevGraph := tmpGraph; NEW(tmpGraph); tmpGraph.isImplicit := FALSE; Stores.Join(v, tmpGraph);
					v.GetNode(imps[i], incImpl, tmpNode);
					tmpNode.isImported := TRUE; (* to be able to identify root modules *)
					tmpGraph.node := tmpNode; tmpGraph.next := prevGraph
				END;
				IF incImpl THEN
					GetImplicitDependencies(name, imps, nofImps);
					FOR i := 0 TO nofImps - 1 DO
						prevGraph := tmpGraph; NEW(tmpGraph); tmpGraph.isImplicit := TRUE; Stores.Join(v, tmpGraph);
						v.GetNode(imps[i], incImpl, tmpNode);
						tmpGraph.node := tmpNode; tmpGraph.next := prevGraph
					END
				END;
				tmpList.imports := tmpGraph; node := tmpList
			ELSE 
				Dialog.ShowParamMsg("#Dev:ModuleNotFound", name, "", "")
			END		
		END
	END GetNode;
	
	PROCEDURE (v: View) OnSelectedObject(x, y: INTEGER): BOOLEAN, NEW;
		VAR 
			l: List; s: SubsystemList;
	BEGIN
		l := v.list; 
		WHILE l # NIL DO
			IF l.Visible(v.showBasic) &  l.Selected() & (x > l.l) & (x < l.l + l.w) & (y < l.b) & (y > l.b - l.h) THEN
				RETURN TRUE
			END;
			l := l.next
		END;
		s := v.subsystems;
		WHILE s # NIL DO
			IF s.Visible(v.showBasic) & s.Selected() & (x > s.l) & (x < s.l + s.w) & (y < s.b) & (y > s.b - s.h) THEN
				RETURN TRUE
			END;
			s := s.next
		END;
		RETURN FALSE
	END OnSelectedObject;
	
	PROCEDURE (v: View) OnObject(x, y: INTEGER): BOOLEAN, NEW;
		VAR 
			l: List; s: SubsystemList;
	BEGIN
		l := v.list; 
		WHILE l # NIL DO
			IF l.Visible(v.showBasic) & (x > l.l) & (x < l.l + l.w) & (y < l.b) & (y > l.b - l.h) THEN
				RETURN TRUE
			END;
			l := l.next
		END;
		s := v.subsystems;
		WHILE s # NIL DO
			IF s.Visible(v.showBasic) & (x > s.l) & (x < s.l + s.w) & (y < s.b) & (y > s.b - s.h) THEN
				RETURN TRUE
			END;
			s := s.next
		END;
		RETURN FALSE
	END OnObject;
	
	PROCEDURE OverLap(l1, t1, r1, b1, l2, t2, r2, b2: INTEGER): BOOLEAN;
	BEGIN
		RETURN  ~((l1 > r2) OR (l2 > r1) OR (t1 > b2) OR (t2 > b1))
	END OverLap;
	
	PROCEDURE (v: View) Select(x1, y1, x2, y2: INTEGER; modifiers: SET), NEW;
		VAR l: List; s: SubsystemList;
	BEGIN
		l := v.list; 
		WHILE l # NIL DO
			IF l.Visible(v.showBasic) & OverLap(l.l, l.b - l.h, l.l + l.w, l.b, x1, y1, x2, y2) THEN 
				IF (Controllers.extend IN modifiers) OR (Controllers.modify IN modifiers) THEN
					l.isSelected := ~l.isSelected
				ELSE
					l.isSelected := TRUE
				END
			ELSE
				IF ~(Controllers.extend IN modifiers) & ~(Controllers.modify IN modifiers) THEN l.isSelected := FALSE END
			END;
			l := l.next
		END;
		s := v.subsystems;
		WHILE s # NIL DO
			IF s.Visible(v.showBasic) & OverLap(s.l, s.b - s.h, s.l + s.w, s.b, x1, y1, x2, y2)  THEN 
				IF (Controllers.extend IN modifiers) OR (Controllers.modify IN modifiers) THEN
					s.isSelected := ~s.isSelected
				ELSE
					s.isSelected := TRUE
				END
			ELSE
				IF ~(Controllers.extend IN modifiers) & ~(Controllers.modify IN modifiers) THEN
					s.isSelected := FALSE
				END
			END;
			s := s.next
		END
	END Select;
	
	PROCEDURE (v: View) SelectAll(set: BOOLEAN), NEW;
		VAR l: List; s: SubsystemList;
	BEGIN
		l := v.list; 
		WHILE l # NIL DO l.isSelected := set & l.Visible(v.showBasic); l := l.next END;
		s := v.subsystems;
		WHILE s # NIL DO s.isSelected := set & s.Visible(v.showBasic); s := s.next END;
		Views.Update(v, Views.keepFrames)
	END SelectAll;
		

	(** Operations on the View **)
	
	PROCEDURE (dop: DepOp) AddSelection(), NEW;
		VAR ml: MetaList; ms: MetaSSList; l: List; s: SubsystemList;
	BEGIN
		ASSERT(dop.view # NIL, 20);
		l := dop.view.list; 
		WHILE l # NIL DO
			IF l.Selected() THEN NEW(ml); ml.list := l; ml.next := dop.mods; dop.mods := ml END; l := l.next
		END;
		s := dop.view.subsystems;
		WHILE s # NIL DO
			IF s.Selected() THEN NEW(ms); ms.sslist := s; ms.next := dop.subs; dop.subs := ms END; s := s.next
		END
	END AddSelection;
	
	PROCEDURE (e: ExpandOp) Do;
		VAR l: MetaList; s: MetaSSList;
	BEGIN
		l := e.mods;
		WHILE l # NIL DO l.list.subsystem.isExpanded := e.expand; l := l.next END;
		s := e.subs;
		WHILE s # NIL DO s.sslist.isExpanded := e.expand; s := s.next END;
		e.expand := ~e.expand;
		Views.Update(e.view, Views.keepFrames)
	END Do;
	
	PROCEDURE (m: MoveOp) BoundingBox, NEW;
		VAR l: MetaList; s: MetaSSList; h, d, w: INTEGER;
	BEGIN
		m.l := MAX(INTEGER); m.t := m.l; m.b := 0; m.r := 0;
		l := m.mods;
		WHILE l # NIL DO
			IF (l.list.l < m.l) THEN m.l := l.list.l END;
			IF (l.list.b - l.list.h < m.t) THEN m.t := l.list.b - l.list.h END;
			IF (l.list.l + l.list.w >  m.r) THEN m.r := l.list.l + l.list.w END;
			IF (l.list.b > m.b) THEN m.b := l.list.b END;
			l := l.next 
		END;
		s := m.subs;
		WHILE s # NIL DO
			IF (s.sslist.l < m.l) THEN m.l := s.sslist.l END;
			IF (s.sslist.b - s.sslist.h < m.t) THEN m.t := s.sslist.b - s.sslist.h END;
			IF (s.sslist.l + s.sslist.w >  m.r) THEN m.r := s.sslist.l + s.sslist.w END;
			IF (s.sslist.b > m.b) THEN m.b := s.sslist.b END;
			s := s.next 
		END;
		m.view.selfont.GetBounds(h, d, w);
		m.b := m.b + d; (* correction for descent *)
		m.r := m.r + w; m.l := m.l - w (* correction for incorrect values by italic fonts *)
	END BoundingBox;
	
	PROCEDURE (m: MoveOp) Do;
		VAR l: MetaList; s: MetaSSList;
	BEGIN
		l := m.mods;
		WHILE l # NIL DO l.list.l := l.list.l + m.dx; l.list.b := l.list.b + m.dy; l := l.next END;
		s := m.subs;
		WHILE s # NIL DO s.sslist.l := s.sslist.l + m.dx; s.sslist.b := s.sslist.b + m.dy; s := s.next END;
		m.dx := -m.dx; m.dy := -m.dy;
		Views.Update(m.view, Views.keepFrames)
	END Do;
	
	PROCEDURE (h: HideOp) Do;
		VAR l: MetaList; s: MetaSSList;
	BEGIN
		l := h.mods;
		WHILE l # NIL DO l.list.isHidden := ~l.list.isHidden; l := l.next END;
		s := h.subs;
		WHILE s # NIL DO s.sslist.isHidden := ~s.sslist.isHidden; s := s.next END;
		Views.Update(h.view, Views.keepFrames)
	END Do;
	
	PROCEDURE (h: HideBasicOp) Do;
	BEGIN
		h.view.showBasic := ~h.view.showBasic;
		Views.Update(h.view, Views.keepFrames)
	END Do;
	
	PROCEDURE (s: ShowAllOp) Do;
		VAR ml: MetaList; mssl: MetaSSList;
	BEGIN
		ml := s.mods; mssl := s.subs;
		WHILE ml # NIL DO ml.list.isHidden := ~ml.list.isHidden; ml := ml.next END;
		WHILE mssl # NIL DO mssl.sslist.isHidden := ~mssl.sslist.isHidden; mssl := mssl.next END;
		Views.Update(s.view, Views.keepFrames)
	END Do;
	
	PROCEDURE (e: ExpandAllOp) Do;
		VAR mssl: MetaSSList;
	BEGIN
		mssl := e.subs;
		WHILE mssl # NIL DO mssl.sslist.isExpanded := e.expand; mssl := mssl.next END;
		e.expand := ~e.expand;
		Views.Update(e.view, Views.keepFrames)
	END Do;
	
	PROCEDURE (v: View) ExpandCollapse(expand: BOOLEAN), NEW;
		VAR e: ExpandOp;
	BEGIN
		NEW(e); e.view := v; e.AddSelection; e.expand := expand;
		IF e.expand THEN e.mods := NIL ELSE e.subs := NIL END;
		Views.Do(v, "Toggle Expand/Collapse", e)
	END ExpandCollapse;
	
	
	(** Redrawing the view **)
	
	PROCEDURE (v: View) SetExtents, NEW;
		VAR tmpList: List; ssList: SubsystemList; h, d, w: INTEGER;
	BEGIN
		tmpList := v.list; v.selfont.GetBounds(h, d, w);
		WHILE tmpList # NIL DO
			tmpList.w := v.selfont.StringWidth(tmpList.name); tmpList.h := h; tmpList := tmpList.next
		END;
		ssList := v.subsystems;
		WHILE ssList # NIL DO
			ssList.w := v.selfont.StringWidth("[" + ssList.name + "]"); ssList.h := h; ssList := ssList.next
		END
	END SetExtents;
	
	PROCEDURE (v: View)  Layout, NEW;
		CONST space = 5 * Ports.mm;
		VAR
			width, height, xindent, yindent, level: INTEGER;
			levelIndent: POINTER TO ARRAY OF INTEGER;
			tmpList: List; 
	BEGIN
		NEW(levelIndent, v.maxLevel + 1); v.context.GetSize(width, height);
		tmpList := v.list;
		WHILE tmpList # NIL DO
			IF tmpList.isStart THEN 
				yindent := space; level := 0 
			ELSE 
				level := tmpList.subsystem.level;
				yindent := space + (15 * Ports.mm * level)
			END;
			xindent := space + levelIndent[level];
			WHILE xindent + v.normfont.StringWidth(tmpList.name) > width DO
				xindent := xindent - width;
				yindent := yindent + (4 * Ports.mm)
			END;
			IF  ~tmpList.subsystem.isBasic OR v.showBasic THEN
				IF tmpList.subsystem.isExpanded THEN
					IF ~tmpList.isHidden & ~tmpList.isLayedout THEN
						levelIndent[level] := levelIndent[level] + tmpList.w + space;
						tmpList.SetPosition(xindent, yindent); tmpList.isLayedout := TRUE
					END
				ELSIF ~ tmpList.subsystem.isHidden & ~tmpList.subsystem.isLayedout THEN
					levelIndent[level] := levelIndent[level] + tmpList.subsystem.w + space;
					tmpList.SetPosition(xindent, yindent); tmpList.subsystem.isLayedout := TRUE
				END
			END;
			tmpList := tmpList.next
		END
	END Layout;
	
	PROCEDURE (v: View) DrawArrow(f: Views.Frame; ax, ay, bx, by, s: INTEGER), NEW;
		CONST ArrowLen = 1 * Ports.mm;
		TYPE Vector = RECORD x, y: REAL END;
		VAR a, b, c, d, e, v1, v2: Vector; lenc, m, x, y: REAL; p: ARRAY 3 OF Ports.Point;
	BEGIN
		a.x := ax; a.y := ay; b.x := bx; b.y := by; c.x := b.x - a.x; c.y := b.y - a.y;
		m := MAX(ABS(c.x), ABS(c.y)); x := c.x / m; y := c.y / m;
		lenc := m * Math.Sqrt(x * x + y * y); s := MAX(1, SHORT(ENTIER(1.5 * s)) DIV Ports.point); (* scaling factor *)
		e.x := c.x * s / lenc; e.y := c.y * s / lenc; 
		d.x := e.y; d.y := - e.x; (* first orthogonal vector *)
		v1.x := (e.x + d.x) * ArrowLen; v1.y := (e.y + d.y) * ArrowLen; 
		v1.x := -v1.x; v1.y := -v1.y; (* mirror vector *)
		d.x := -d.x; d.y := -d.y; (* second orthogonal vector *)
		v2.x := (e.x + d.x) * ArrowLen; v2.y := (e.y + d.y) * ArrowLen; 
		v2.x := -v2.x; v2.y := -v2.y; (* mirror vector *)
		p[0].x := SHORT(ENTIER(b.x));p[0].y := SHORT(ENTIER(b.y));
		p[1].x := SHORT(ENTIER(b.x + v1.x)); p[1].y := SHORT(ENTIER(b.y + v1.y));
		p[2].x := SHORT(ENTIER(b.x + v2.x)); p[2].y := SHORT(ENTIER(b.y + v2.y));
		f.DrawPath(p, 3, Ports.fill, Ports.black, Ports.closedPoly)
	END DrawArrow;
	
	PROCEDURE (v: View)  DrawDependencies (f: Views.Frame; l, t, r, b: INTEGER), NEW;
		VAR
			xindent, yindent, startx, starty, middlex, middley, sx, sy, thickness: INTEGER;
			subgraph: Subgraph; tmpList: List; s: SubsystemList;
	BEGIN
		thickness := MAX(2, (v.normfont.size DIV 8));
		f.DrawRect(l, t, r, b, Ports.fill, Ports.white);
		tmpList := v.list;
		WHILE tmpList # NIL DO
			IF tmpList.Visible(v.showBasic) OR tmpList.subsystem.Visible(v.showBasic) THEN
				subgraph := tmpList.imports;
				tmpList.GetPosition(startx, starty);
				WHILE subgraph # NIL DO
					IF subgraph.node.Visible(v.showBasic) OR subgraph.node.subsystem.Visible(v.showBasic) THEN
						subgraph.node.GetPosition(sx, sy);
						middlex := ABS(startx - ((startx - sx) DIV 2));
						middley := ABS(starty - ((starty - sy) DIV 2));
						IF ~subgraph.isImplicit THEN
							f.DrawLine(startx, starty, middlex, middley, thickness, Ports.red);
							f.DrawLine(middlex, middley, sx, sy, thickness, Ports.blue)
						ELSE
							f.DrawLine(startx, starty, middlex, middley, thickness, Ports.grey25);
							f.DrawLine(middlex, middley, sx, sy, thickness, Ports.grey25)
						END;
						IF (middlex # startx) OR (middley # starty) THEN
							v.DrawArrow(f, startx, starty, middlex, middley, thickness)
						END
					END;
					subgraph := subgraph.next
				END
			END;
			tmpList := tmpList.next
		END;
		(* Draw the names of the modules last, so they end up on top *)
		tmpList := v.list;
		WHILE tmpList # NIL DO
			IF tmpList.Visible(v.showBasic) THEN
				tmpList.GetPosition(xindent, yindent);
				IF tmpList.Selected() THEN
					f.DrawString(xindent, yindent, Ports.black, tmpList.name, v.selfont)
				ELSE
					f.DrawString(xindent, yindent, Ports.black, tmpList.name, v.normfont)
				END;
				f.DrawOval(xindent - (Ports.mm DIV 2), yindent - (Ports.mm DIV 2), xindent + (Ports.mm DIV 2), yindent + 
					(Ports.mm DIV 2), Ports.fill, Ports.black)
			END;
			tmpList := tmpList.next
		END;
		s := v.subsystems;
		WHILE s # NIL DO
			IF s.Visible(v.showBasic) THEN
				xindent := s.l; yindent := s.b;
				IF s.Selected() THEN
					f.DrawString(xindent, yindent, Ports.black, "[" + s.name + "]", v.selfont)
				ELSE
					f.DrawString(xindent, yindent, Ports.black, "[" + s.name + "]", v.normfont)
				END;
				f.DrawOval(xindent - (Ports.mm DIV 2), yindent - (Ports.mm DIV 2), xindent + (Ports.mm DIV 2), yindent + 
					(Ports.mm DIV 2), Ports.fill, Ports.black)
			END;
			s := s.next
		END
	END DrawDependencies;
	
	PROCEDURE (v: View)  Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		v.Layout();
		v.DrawDependencies(f, l, t, r, b)
	END Restore;
	
	
	(** Handle view messages **)
	
	PROCEDURE (v: View) SetProps(p: Properties.Property), NEW;
		VAR 
			size: INTEGER;
			typeface: Fonts.Typeface;
			prop: Properties.StdProp;
			style: SET;
	BEGIN
		WHILE (p # NIL) & ~(p IS Properties.StdProp) DO p := p.next END;
		IF p # NIL THEN
			prop := p(Properties.StdProp); size := v.normfont.size; 
			typeface := v.normfont.typeface; style := v.normfont.style;
			IF Properties.size IN prop.valid THEN size := prop.size END;
			IF Properties.typeface IN prop.valid THEN typeface := prop.typeface END;
			IF Properties.style IN prop.valid THEN 
				IF Fonts.underline IN prop.style.mask THEN
					IF Fonts.underline IN prop.style.val THEN style := style + {Fonts.underline} 
					ELSE style := style - {Fonts.underline} END
				END;
				IF Fonts.italic IN prop.style.mask THEN
					IF Fonts.italic IN prop.style.val THEN style := style + {Fonts.italic}
					ELSE style := style - {Fonts.italic} END
				END;
				IF Fonts.strikeout IN prop.style.mask THEN
					IF Fonts.strikeout IN prop.style.val THEN style := style + {Fonts.strikeout}
					ELSE style := style - {Fonts.strikeout} END
				END
			END;
			v.normfont := Fonts.dir.This(typeface, size, style, Fonts.normal);
			v.selfont := Fonts.dir.This(typeface, size, style, Fonts.bold); v.SetExtents();
			Views.Update(v, Views.keepFrames)
		END
	END SetProps;
	
	PROCEDURE (v: View) GetProps(OUT p: Properties.Property), NEW;
		VAR
			prop: Properties.StdProp;
	BEGIN
			NEW(prop); prop.known := {Properties.weight, Properties.size, Properties.typeface, Properties.style};
			prop.valid := {Properties.weight, Properties.size, Properties.typeface, Properties.style};
			prop.typeface := v.normfont.typeface; prop.size := v.normfont.size; prop.weight := v.normfont.weight; 
			prop.style.val := v.normfont.style; prop.style.mask := v.normfont.style;
			p := prop
	END GetProps;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		CONST min = 50 * Ports.mm; max = 500 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				 msg.w := 200 * Ports.mm; msg.h := 100 * Ports.mm
			ELSE
				Properties.ProportionalConstraint(2, 1, msg.fixedW, msg.fixedH,
										msg.w, msg.h);
				IF msg.h < min THEN
					msg.h := min; msg.w := 2 * min
				ELSIF msg.h > max THEN
					msg.h := max; msg.w := 2 * max
				END
			END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE; msg.verFitToWin := TRUE
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		| msg: Properties.PollMsg DO
			v.GetProps(msg.prop)
		| msg: Properties.SetMsg DO
			v.SetProps(msg.prop)
		ELSE	(* ignore other messages *)
		END
	END HandlePropMsg;
	
	PROCEDURE (v: View) HandleMultiSelect (f: Views.Frame; VAR msg: Controllers.TrackMsg), NEW;
		VAR
			x, y, x1, y1: INTEGER; 
			isDown, first: BOOLEAN; m: SET;
	BEGIN
		first := TRUE; x1 := -1; y1 := -1;
		REPEAT 
			f.Input(x, y, m, isDown); 
			IF (x1 # x) OR (y1 # y) THEN
				IF first THEN
					first := FALSE
				ELSE
					f.MarkRect(MIN(x1, msg.x), MIN(y1, msg.y), MAX(x1, msg.x), MAX(y1, msg.y), 0, Ports.dim50, FALSE)
				END;
				x1 := x; y1 := y;
				f.MarkRect(MIN(x1, msg.x), MIN(y1, msg.y), MAX(x1, msg.x), MAX(y1, msg.y), 0, Ports.dim50, TRUE)
			END
		UNTIL ~isDown;
		f.MarkRect(MIN(x1, msg.x), MIN(y1, msg.y), MAX(x1, msg.x), MAX(y1, msg.y), 0, Ports.dim50, FALSE);
		v.Select(MIN(x1, msg.x), MIN(y1, msg.y), MAX(x1, msg.x), MAX(y1, msg.y), msg.modifiers);
		Views.Update(v, Views.keepFrames)
	END HandleMultiSelect;
	
	PROCEDURE (v: View) HandleMove (f: Views.Frame; VAR msg: Controllers.TrackMsg), NEW;
		VAR
			x1, y1, x, y, res: INTEGER; mo: MoveOp; m: SET; isDown: BOOLEAN;
			ml: MetaList; ms: MetaSSList;
	BEGIN
		NEW(mo); mo.view := v; mo.AddSelection; mo.BoundingBox;
		x1 := msg.x; y1 := msg.y;
		f.SaveRect(f.l, f.t, f.r, f.b, res);
		REPEAT 
			f.Input(x, y, m, isDown); 
			IF (res = 0) & ((x # x1) OR (y # y1)) THEN
				x1 := x; y1 := y;
				f.RestoreRect(mo.l + mo.dx, mo.t + mo.dy, mo.r + mo.dx, mo.b + mo.dy, Ports.keepBuffer);
				mo.dx := x - msg.x; mo.dy := y - msg.y; 
				IF mo.l + mo.dx < f.l THEN mo.dx := f.l - mo.l END; IF mo.r + mo.dx > f.r THEN mo.dx := f.r - mo.r END;
				IF mo.t + mo.dy < f.t THEN mo.dy := f.t - mo.t END; IF mo.b + mo.dy > f.b THEN mo.dy := f.b - mo.b END;
				ml := mo.mods;
				WHILE ml # NIL DO
					f.DrawString(ml.list.l + mo.dx, ml.list.b + mo.dy, Ports.grey50, ml.list.name, v.selfont); ml := ml.next
				END;
				ms := mo.subs;
				WHILE ms # NIL DO
					f.DrawString(ms.sslist.l + mo.dx, ms.sslist.b + mo.dy, Ports.grey50, '[' + ms.sslist.name + ']', v.selfont); 
					ms := ms.next
				END
			END
		UNTIL ~isDown;
		f.RestoreRect(f.l, f.t, f.r, f.b, Ports.disposeBuffer);
		Views.Do(v, "Move", mo)
	END HandleMove;
	
	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message;  VAR focus: Views.View);
		CONST movedist = Ports.mm * Ports.mm; arrowdist = Ports.mm;
		VAR
			w, h, x, y, delta: INTEGER; m: SET; 
			isDown, isMove: BOOLEAN;
			mo: MoveOp; e: ExpandOp;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			IF HostPorts.right IN msg.modifiers THEN
				HostMenus.PopupMenu
			ELSIF (Controllers.doubleClick IN msg.modifiers) THEN
				IF v.OnSelectedObject(msg.x, msg.y) THEN
					NEW(e); e.view := v; e.AddSelection;
					IF (e.mods # NIL) & (e.subs = NIL) & (e.mods.next = NIL) THEN
						e.expand := FALSE; Views.Do(v, "Collapse " + e.mods.list.name, e)
					ELSIF (e.mods = NIL) & (e.subs # NIL) & (e.subs.next = NIL) THEN
						e.expand := TRUE; Views.Do(v, "Expand " + e.subs.sslist.name, e)
					END
				END
			ELSE
				isMove := FALSE;
				REPEAT 
					f.Input(x, y, m, isDown); 	
					IF ~isMove & ((ABS(x - msg.x) * ABS(x - msg.x) + ABS(y - msg.y) * ABS(y - msg.y)) > movedist) THEN
						isMove := TRUE
					END	
				UNTIL ~isDown OR isMove;
				IF isMove THEN
					IF v.OnSelectedObject(msg.x, msg.y) THEN 
						v.HandleMove(f, msg) 
					ELSIF v.OnObject(msg.x, msg.y) THEN
						v.Select(msg.x, msg.y, msg.x, msg.y, {}); v.HandleMove(f, msg)
					ELSE v.HandleMultiSelect(f, msg) END
				ELSE
					v.Select(msg.x, msg.y, msg.x, msg.y, msg.modifiers); Views.Update(v, Views.keepFrames)
				END
			END
		| msg: Controllers.PollOpsMsg DO
			msg.valid := {Controllers.copy}; msg.selectable := TRUE; msg.type := "DevDependencies.View"
		| msg: Properties.CollectMsg DO
			v.GetProps(msg.poll.prop)
		| msg: Properties.EmitMsg DO
			v.SetProps(msg.set.prop); v.context.GetSize(w, h); v.Restore(f, 0, 0, w, h)
		| msg: Controllers.SelectMsg DO
			v.SelectAll(msg.set)
		| msg: Controllers.EditMsg DO
			IF (msg.op = Controllers.pasteChar) & (ORD(msg.char) >= 28) & (ORD(msg.char) <= 31) THEN
				NEW(mo); mo.view := v; mo.AddSelection; mo.BoundingBox; mo.dx := 0; mo.dy := 0;
				IF Controllers.modify IN msg.modifiers THEN delta := f.unit ELSE delta := arrowdist END;
				CASE msg.char OF
				| CHR(28): mo.dx := -delta | CHR(29): mo.dx := delta 
				| CHR(30): mo.dy := -delta | CHR(31): mo.dy := delta
				END;
				IF mo.l + mo.dx < f.l THEN mo.dx := f.l - mo.l END; IF mo.r + mo.dx > f.r THEN mo.dx := f.r - mo.r END;
				IF mo.t + mo.dy < f.t THEN mo.dy := f.t - mo.t END; IF mo.b + mo.dy > f.b THEN mo.dy := f.b - mo.b END;
				IF (mo.dy # 0) OR (mo.dx # 0) THEN Views.Do(v, "Move", mo) END
			END
		ELSE (* Ignore other messages *)
		END
	END HandleCtrlMsg;

	
	(** Internalize/Externalize and Copy **)
	
	PROCEDURE (s: SubsystemList) Internalize (VAR rd: Stores.Reader);
		VAR 
			store: Stores.Store;
	BEGIN
		rd.ReadString(s.name); rd.ReadBool(s.isExpanded);
		rd.ReadBool(s.isLayedout); rd.ReadBool(s.isHidden); rd.ReadBool(s.isBasic); rd.ReadBool(s.isStart);
		rd.ReadInt(s.l); rd.ReadInt(s.b); rd.ReadInt(s.w); rd.ReadInt(s.h); rd.ReadInt(s.level);
		rd.ReadStore(store); 
		IF store # NIL THEN
			s.next := store(SubsystemList)
		ELSE
			s.next := NIL
		END
	END Internalize;

	PROCEDURE (s: SubsystemList) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteString(s.name); wr.WriteBool(s.isExpanded);
		wr.WriteBool(s.isLayedout);wr.WriteBool(s.isHidden); wr.WriteBool(s.isBasic); wr.WriteBool(s.isStart);
		wr.WriteInt(s.l); wr.WriteInt(s.b); wr.WriteInt(s.w); wr.WriteInt(s.h); wr.WriteInt(s.level);
		wr.WriteStore(s.next)
	END Externalize;
	
	PROCEDURE (l: List) Internalize (VAR rd: Stores.Reader);
		VAR 
			store: Stores.Store;
	BEGIN
		rd.ReadString(l.name); rd.ReadInt(l.l); rd.ReadInt(l.b); rd.ReadInt(l.w); rd.ReadInt(l.h);
		rd.ReadBool(l.isLayedout); rd.ReadBool(l.isHidden); rd.ReadBool(l.isStart);
		rd.ReadStore(store); l.subsystem := store(SubsystemList);
		rd.ReadStore(store); 
		IF store # NIL THEN l.imports := store(Subgraph) ELSE l.imports := NIL END;
		rd.ReadStore(store); 
		IF store # NIL THEN l.next := store(List) ELSE l.next := NIL END
	END Internalize;

	PROCEDURE (l: List) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteString(l.name); wr.WriteInt(l.l); wr.WriteInt(l.b); wr.WriteInt(l.w); wr.WriteInt(l.h);
		wr.WriteBool(l.isLayedout);wr.WriteBool(l.isHidden); wr.WriteBool(l.isStart);
		wr.WriteStore(l.subsystem); wr.WriteStore(l.imports); 
		wr.WriteStore(l.next)
	END Externalize;
	
	PROCEDURE (l: NameList) Internalize (VAR rd: Stores.Reader);
		VAR 
			store: Stores.Store;
	BEGIN
		rd.ReadString(l.name); 
		rd.ReadStore(store); 
		IF store # NIL THEN l.next := store(NameList) ELSE l.next := NIL END
	END Internalize;

	PROCEDURE (l: NameList) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteString(l.name); wr.WriteStore(l.next)
	END Externalize;
	
	PROCEDURE (g: Subgraph) Internalize (VAR rd: Stores.Reader);
		VAR 
			store: Stores.Store;
	BEGIN
		rd.ReadBool(g.isImplicit);
		rd.ReadStore(store); g.node := store(List); 
		rd.ReadStore(store);
		IF store # NIL THEN g.next := store(Subgraph) ELSE g.next := NIL END
	END Internalize;

	PROCEDURE (g: Subgraph) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteBool(g.isImplicit); wr.WriteStore(g.node); wr.WriteStore(g.next) 
	END Externalize;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR 
			version, size: INTEGER;
			store: Stores.Store;
			typeface: Fonts.Typeface;
			style: SET;
	BEGIN
		rd.ReadVersion(0, 0, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.maxLevel); rd.ReadBool(v.showBasic);
			rd.ReadString(typeface); rd.ReadInt(size); rd.ReadSet(style);
			v.normfont := Fonts.dir.This(typeface, size, style, Fonts.normal);
			v.selfont := Fonts.dir.This(typeface, size, style, Fonts.bold);
			rd.ReadStore(store); v.startList := store(NameList); 
			rd.ReadStore(store); v.subsystems := store(SubsystemList);
			rd.ReadStore(store); v.list := store(List)
		END
	END Internalize;
		
	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(0);
		wr.WriteInt(v.maxLevel); wr.WriteBool(v.showBasic);
		wr.WriteString(v.normfont.typeface); wr.WriteInt(v.normfont.size); wr.WriteSet(v.normfont.style);
		wr.WriteStore(v.startList); wr.WriteStore(v.subsystems); wr.WriteStore(v.list)
	END Externalize;
	
	PROCEDURE (s: SubsystemList) CopyFrom(source: Stores.Store);
	BEGIN
		WITH source: SubsystemList DO
			s.name := source.name;
			s.isExpanded := source.isExpanded; 
			s.isLayedout := source.isLayedout;
			s.isHidden := source.isHidden;
			s.isBasic := source.isBasic;
			s.isStart := source.isStart;
			s.l := source.l; s.b := source.b; s.w := source.w; s.h := source.h; s.level := source.level;
			IF source.next # NIL THEN s.next := Stores.CopyOf(source.next)(SubsystemList) END
		END
	END CopyFrom;
	
	PROCEDURE (l: List) CopyFrom(source: Stores.Store);
	BEGIN
		WITH source: List DO
			l.name := source.name; l.l := source.l; l.b := source.b; l.w := source.w; l.h := source.h;  
			l.isLayedout := source.isLayedout; l.isHidden := source.isHidden; l.isStart := source.isStart;
			IF source.imports # NIL THEN l.imports := Stores.CopyOf(source.imports)(Subgraph) END;
			IF source.subsystem # NIL THEN
				 l.subsystem := Stores.CopyOf(source.subsystem)(SubsystemList)
			END;
			IF source.next # NIL THEN l.next := Stores.CopyOf(source.next)(List) END
		END
	END CopyFrom;

	PROCEDURE (l: NameList) CopyFrom(source: Stores.Store);
	BEGIN
		WITH source: NameList DO
			l.name := source.name;
			IF source.next # NIL THEN l.next := Stores.CopyOf(source.next)(NameList) END
		END
	END CopyFrom;
	
	PROCEDURE (g: Subgraph) CopyFrom(source: Stores.Store);
	BEGIN
		WITH source: Subgraph DO
			g.isImplicit := source.isImplicit;
			IF source.node # NIL THEN g.node := Stores.CopyOf(source.node)(List) END;
			IF source.next # NIL THEN g.next := Stores.CopyOf(source.next)(Subgraph) END
		END
	END CopyFrom;
	
	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: View DO
			v.maxLevel := source.maxLevel; 
			v.normfont := source.normfont;
			v.selfont := source.selfont;
			v.showBasic := source.showBasic;
			v.startList := Stores.CopyOf(source.startList)(NameList);
			v.subsystems := Stores.CopyOf(source.subsystems)(SubsystemList);
			v.list := Stores.CopyOf(source.list)(List)
		END
	END CopyFromSimpleView;
	
	
	(** Guards and Interactors **)
	
	PROCEDURE ExpandClick*;
		VAR v: View;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN v.ExpandCollapse(TRUE) END
	END ExpandClick;
		
	PROCEDURE CollapseClick*;
		VAR v: View;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN v.ExpandCollapse(FALSE) END
	END CollapseClick;
	
	PROCEDURE HideClick*;
		VAR v: View; hop: HideOp;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN
			NEW(hop); hop.view := v; hop.AddSelection;
			Views.Do(v, "Hide Item", hop)
		END
	END HideClick;
	
	PROCEDURE ToggleBasicSystemsClick*;
		VAR v: View; hbop: HideBasicOp;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN 
			NEW(hbop); hbop.view := v; 
			IF v.showBasic THEN Views.Do(v, "Hide Basic Systems", hbop) 
			ELSE Views.Do(v, "Show Basic Systems", hbop)
			END
		END
	END ToggleBasicSystemsClick;
	
	PROCEDURE ShowAllClick*;
		VAR v: View; l: List; sl: SubsystemList;
				so: ShowAllOp; ml: MetaList; ssl: MetaSSList;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN 
			NEW(so); l := v.list;
			WHILE l # NIL DO 
				IF l.isHidden THEN NEW(ml); ml.list := l; ml.next := so.mods; so.mods := ml END;
				l := l.next 
			END;
			sl := v.subsystems;
			WHILE sl # NIL DO 
				IF sl.isHidden THEN NEW(ssl); ssl.sslist := sl; ssl.next := so.subs; so.subs := ssl END;
				sl := sl.next
			END;
			IF (so.mods # NIL) OR (so.subs # NIL) THEN so.view := v; Views.Do(v, "Show All", so) END
		END
	END ShowAllClick;
	
	PROCEDURE ECAll(eo: ExpandAllOp);
		VAR v: View; sl: SubsystemList; ssl: MetaSSList;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN 
			sl := v.subsystems;
			WHILE sl # NIL DO 
				IF (sl.isExpanded # eo.expand) & ~sl.isHidden & (~sl.isBasic OR v.showBasic) THEN
					NEW(ssl); ssl.sslist := sl; ssl.next := eo.subs; eo.subs := ssl
				END;
				sl := sl.next
			END;
			IF eo.subs # NIL THEN eo.view := v; Views.Do(v, "Show All", eo) END
		END
	END ECAll;
	
	PROCEDURE ExpandAllClick*;
		VAR eo: ExpandAllOp; 
	BEGIN
		NEW(eo); eo.expand := TRUE; ECAll(eo)
	END ExpandAllClick;
	
	PROCEDURE CollapseAllClick*;
		VAR eo: ExpandAllOp; 
	BEGIN
		NEW(eo); eo.expand := FALSE; ECAll(eo)
	END CollapseAllClick;
	
	PROCEDURE ArrangeClick*; (* Not undoable *)
		VAR v: View; l: List; sl: SubsystemList;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN 
			Views.BeginModification(Views.notUndoable, v);
			l := v.list; 
			WHILE l # NIL DO l.isLayedout := FALSE; l := l.next END;
			sl := v.subsystems;
			WHILE sl # NIL DO sl.isLayedout := FALSE; sl := sl.next END;
			Views.Update(v, Views.keepFrames);
			Views.EndModification(Views.notUndoable, v)
		END
	END ArrangeClick;
	
	PROCEDURE DependencyList(v: View; full: BOOLEAN; VAR clist: MetaList);
		VAR l: List;
		PROCEDURE RecursiveList(node: List; VAR clist: MetaList);
			VAR imports: Subgraph; ml, prev: MetaList;
		BEGIN
			IF full OR node.subsystem.isExpanded THEN
				ml := clist; 
				WHILE (ml # NIL) & (ml.list # node) DO ml := ml.next END;
				IF ml = NIL THEN
					imports := node.imports;
					WHILE imports # NIL DO
						IF ~imports.isImplicit THEN RecursiveList(imports.node, clist) END;
						imports := imports.next
					END;
					ml := clist; prev := ml;
					WHILE (ml # NIL) & (ml.list # node)  DO
						(* could have been implicitly imported *)
						prev := ml; ml := ml.next
					END;
					IF ml = NIL THEN
						NEW(ml); ml.list := node;
						IF prev # NIL THEN prev.next := ml END
					END;
					IF clist = NIL THEN clist := ml END;
					imports := node.imports;
					WHILE imports # NIL DO
						IF imports.isImplicit THEN RecursiveList(imports.node, clist) END;
						imports := imports.next
					END
				END
			END
		END RecursiveList;
	BEGIN
		l := v.list; 
		WHILE l # NIL DO
			RecursiveList(l, clist);
			l := l.next
		END
	END DependencyList;

	PROCEDURE SortMetaList(VAR ml: MetaList);
		VAR l, p, cur, pcur: MetaList;
	BEGIN
		ASSERT(ml # NIL, 20);
		cur := ml.next; pcur := ml;
		WHILE cur # NIL DO
			p := NIL; l := ml;
			WHILE  (l # cur) & (cur.list.name$ > l.list.name$) DO p := l; l := l.next END;
			IF l = cur THEN
				pcur := cur; cur := cur.next
			ELSE
				pcur.next := cur.next; cur.next := l;
				IF p = NIL THEN ml := cur ELSE p.next := cur END;
				cur := pcur.next
			END
		END
	END SortMetaList;

	PROCEDURE MarkImplicitModules(v: View);
		VAR 
			fullList, nonImplList: MetaList; v2: View;
	BEGIN
		IF v.nofMods <= 0 THEN RETURN END;
		DependencyList(v, full, fullList);
		SortMetaList(fullList);
		StartAnalysis(v.startList, NIL, ~implicit, v2);
		DependencyList(v2, full, nonImplList);
		SortMetaList(nonImplList);
		WHILE nonImplList # NIL DO
			IF nonImplList.list.name$ = fullList.list.name$ THEN
				fullList.list.onlyImplicit := FALSE
			ELSE
				WHILE nonImplList.list.name$ # fullList.list.name$ DO
					fullList.list.onlyImplicit := TRUE; fullList := fullList.next
				END
			END;
			fullList := fullList.next;
			nonImplList := nonImplList.next
		END;
		WHILE fullList # NIL DO fullList.list.onlyImplicit := TRUE; fullList := fullList.next END
	END MarkImplicitModules;
	
	PROCEDURE SetStyle(f: TextMappers.Formatter; style: SET);
		VAR prop: Properties.StdProp;
	BEGIN
		prop := f.rider.attr.Prop()(Properties.StdProp); 
		IF bold IN style THEN prop.weight := Fonts.bold ELSE prop.weight := Fonts.normal END;
		IF italic IN style THEN INCL(prop.style.val, Fonts.italic); prop.style.mask := prop.style.val 
		ELSE EXCL(prop.style.val, Fonts.italic) END;
		f.rider.SetAttr(TextModels.ModifiedAttr(f.rider.attr, prop))
	END SetStyle;
	
	PROCEDURE SetGray(f: TextMappers.Formatter);
		VAR prop: Properties.StdProp;
	BEGIN
		prop := f.rider.attr.Prop()(Properties.StdProp);
		prop.color.val := Ports.grey25;
		f.rider.SetAttr(TextModels.ModifiedAttr(f.rider.attr, prop))
	END SetGray;
	
	PROCEDURE SetDefColor(f: TextMappers.Formatter);
		VAR prop: Properties.StdProp;
	BEGIN
		prop := f.rider.attr.Prop()(Properties.StdProp);
		prop.color.val := Ports.defaultColor;
		f.rider.SetAttr(TextModels.ModifiedAttr(f.rider.attr, prop))
	END SetDefColor;

	PROCEDURE WritePackList(f: TextMappers.Formatter; v: View);
		CONST colsPerRow = 3;
		VAR head, tail: Files.Name; l: List; col: INTEGER;
	BEGIN
		l := v.list; col := 0;
		WHILE l # NIL DO
			IF l.onlyImplicit THEN SetGray(f) ELSE SetDefColor(f) END;
			Kernel.SplitName(l.name, head, tail); IF head = "" THEN head := "System" END;
			f.WriteString(head + "/Code/" + tail + ".ocf ");
			INC(col); IF col >= colsPerRow THEN f.WriteLn; col := 0 END;
			l := l.next
		END;
		SetGray(f); f.WriteString("System/Rsrc/Menus.odc")
	END WritePackList;
	
	PROCEDURE WriteCommand (IN f: TextMappers.Formatter; title: Dialog.String; cmd: TextModels.Model);
	BEGIN
		SetStyle(f, {bold}); f.WriteString(title); f.WriteLn; 
		SetStyle(f, {});
		f.WriteView(StdFolds.dir.New(StdFolds.collapsed, "", cmd)); 
		 f.WriteString('command');
		f.WriteView(StdFolds.dir.New(StdFolds.collapsed, "", NIL)); 
		f.WriteLn; f.WriteLn
	END WriteCommand;
	
	PROCEDURE AddHeader (IN f: TextMappers.Formatter; v: View);
		VAR sl: NameList; date: Dates.Date; time: Dates.Time; ds, ts: String;
	BEGIN
		Dates.GetDate(date); Dates.DateToString(date, Dates.plainLong, ds);
		Dates.GetTime(time); Dates.TimeToString(time, ts);
		SetStyle(f, {bold});
		f.WriteString('Tool for: '); sl := v.startList;
		WHILE sl # NIL DO f.WriteString(sl.name + ' '); sl := sl.next END; f.WriteLn; 
		SetStyle(f, {italic});f.WriteString('Created: ' + ds + ', ' + ts); f.WriteLn; f.WriteLn
	END AddHeader;
	
	PROCEDURE AddCompileList (IN f: TextMappers.Formatter; v: View);
		VAR l: MetaList; m: TextModels.Model; hf:TextMappers.Formatter;
	BEGIN
		m := TextModels.dir.New(); hf.ConnectTo(m);
		SetStyle(hf, {}); hf.WriteLn;
		hf.WriteView(DevCommanders.dir.New()); hf.WriteString(' DevCompiler.CompileThis '); hf.WriteLn;
		DependencyList(v, ~full, l);
		WHILE l # NIL DO 
			IF l.list.onlyImplicit THEN SetGray(hf) ELSE SetDefColor(hf) END;
			hf.WriteString(l.list.name + ' '); 
			l := l.next
		END; 
		hf.WriteView(DevCommanders.dir.NewEnd()); hf.WriteLn; 
		WriteCommand(f, 'To compile:', m)
	END AddCompileList;

	PROCEDURE WriteUnloadList(f: TextMappers.Formatter; l: MetaList);
	BEGIN
		IF l # NIL THEN
			WriteUnloadList(f, l.next);
			IF l.list.onlyImplicit THEN SetGray(f) ELSE SetDefColor(f) END;
			f.WriteString(l.list.name + ' ')
		END
	END WriteUnloadList;
	
	PROCEDURE AddUnloadList (IN f: TextMappers.Formatter; v: View);
		VAR l: MetaList; m: TextModels.Model; hf:TextMappers.Formatter;
	BEGIN
		m := TextModels.dir.New(); hf.ConnectTo(m);
		SetStyle(hf, {}); hf.WriteLn;
		hf.WriteView(DevCommanders.dir.New()); hf.WriteString(' DevDebug.UnloadThis '); hf.WriteLn; 
		DependencyList(v, ~full, l);
		WriteUnloadList(hf, l); hf.WriteView(DevCommanders.dir.NewEnd()); hf.WriteLn;
		WriteCommand(f, 'To unload:', m)
	END AddUnloadList;
	
	PROCEDURE AddLinkList (IN f: TextMappers.Formatter);
		VAR m: TextModels.Model; hf:TextMappers.Formatter;
	BEGIN
		m := TextModels.dir.New(); hf.ConnectTo(m);
		SetStyle(hf, {}); hf.WriteLn;
		hf.WriteView(DevCommanders.dir.New()); hf.WriteString(' DevLinker.Link exefilename.exe := '); hf.WriteLn;
		hf.WriteString('Kernel$+ Files HostFiles HostPackedFiles StdLoader'); hf.WriteLn;
		hf.WriteString('1 Applogo.ico 2 Doclogo.ico 3 SFLogo.ico 4 CFLogo.ico 5 DtyLogo.ico'); hf.WriteLn;
		hf.WriteString('6 folderimg.ico 7 openimg.ico 8 leafimg.ico'); hf.WriteLn; 
		hf.WriteString('1 Move.cur 2 Copy.cur 3 Link.cur 4 Pick.cur 5 Stop.cur 6 Hand.cur 7 Table.cur');
		hf.WriteView(DevCommanders.dir.NewEnd()); hf.WriteLn;
		WriteCommand(f, 'To link (executable to which files can be packed):', m)
	END AddLinkList;
	
	PROCEDURE AddPackList (IN f: TextMappers.Formatter; v: View);
		VAR m: TextModels.Model; hf:TextMappers.Formatter;
	BEGIN
		m := TextModels.dir.New(); hf.ConnectTo(m);
		SetStyle(hf, {}); hf.WriteLn;
		hf.WriteView(DevCommanders.dir.New()); hf.WriteString(' DevPacker.PackThis exefilename.exe :='); hf.WriteLn;
		WritePackList(hf, v);
		hf.WriteView(DevCommanders.dir.NewEnd()); hf.WriteLn;
		WriteCommand(f, 'To pack:', m)
	END AddPackList;
	
	PROCEDURE AddRootList (IN f: TextMappers.Formatter; v: View);
		VAR m: TextModels.Model; hf:TextMappers.Formatter; l: List;
	BEGIN
		m := TextModels.dir.New(); hf.ConnectTo(m);
		SetStyle(hf, {italic}); hf.WriteLn;
		hf.WriteString('(modules which are not imported from any other modules)'); hf.WriteLn;
		SetStyle(hf, {});
		l := v.list;
		WHILE l # NIL DO
			IF ~l.isImported THEN
				IF l.onlyImplicit THEN SetGray(hf) ELSE SetDefColor(hf) END;
				hf.WriteString(l.name + ' ') 
			END;
			l := l.next
		END;
		hf.WriteLn;
		WriteCommand(f, 'Root modules:', m)
	END AddRootList;
	
	PROCEDURE (v: View) CreateTool, NEW;
		VAR t: TextModels.Model; f: TextMappers.Formatter; tv: TextViews.View; 
	BEGIN
		MarkImplicitModules(v);
		t := TextModels.dir.New(); f.ConnectTo(t); f.SetPos(0); SetStyle(f, {bold});
		AddHeader(f, v);
		AddCompileList(f, v);
		AddUnloadList(f, v);
		AddLinkList(f);
		AddPackList(f, v);
		AddRootList(f, v);
		tv := TextViews.dir.New(t); Views.OpenView(tv)
	END CreateTool;
	
	PROCEDURE CreateToolClick*;
		VAR v: View; 
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN v.CreateTool END
	END CreateToolClick;
	
	PROCEDURE SelGuard* (VAR par: Dialog.Par);
		VAR e: ExpandOp; v: View;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v = NIL THEN 
			par.disabled := TRUE
		ELSE
			NEW(e); e.view := v; e.AddSelection;
			par.disabled := (e.mods = NIL) & (e.subs = NIL)
		END
	END SelGuard;
	
	PROCEDURE ModsGuard* (VAR par: Dialog.Par);
		VAR e: ExpandOp; v: View;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v = NIL THEN 
			par.disabled := TRUE
		ELSE
			NEW(e); e.view := v; e.AddSelection;
			par.disabled := e.mods = NIL
		END
	END ModsGuard;
	
	PROCEDURE SubsGuard* (VAR par: Dialog.Par);
		VAR e: ExpandOp; v: View;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v = NIL THEN 
			par.disabled := TRUE
		ELSE
			NEW(e); e.view := v; e.AddSelection;
			par.disabled := e.subs = NIL
		END
	END SubsGuard;
	
	PROCEDURE ShowBasicGuard* (VAR par: Dialog.Par);
		VAR v: View; 
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN 
			par.checked := v.showBasic
		ELSE
			par.disabled := TRUE
		END
	END ShowBasicGuard;
	
	(** Initialization **)
	
	PROCEDURE SetBasic(v: View);
		VAR str: String; t: TextModels.Model; s: TextMappers.Scanner; l, n, sl: NameList; sslist: SubsystemList;
			sub, mod: String;
	BEGIN
		Dialog.MapString("#Dev:BasicSystems", str);
		l := NIL;
		IF str # "BasicSystems" THEN
			t := TextModels.dir.NewFromString(str); s.ConnectTo(t); s.SetPos(0); s.Scan;
			WHILE s.type # TextMappers.eot DO
				IF s.type = TextMappers.string THEN
					sl := v.startList;
					Kernel.SplitName(sl.name, sub, mod); IF sub = "" THEN sub := "System" END;
					WHILE (sl # NIL) & (sub # s.string) DO
						Kernel.SplitName(sl.name, sub, mod); IF sub = "" THEN sub := "System" END;
						sl := sl.next
					END;
					IF sl = NIL THEN NEW(n); n.name := s.string$; n.next := l;  l := n END
				END;
				s.Scan
			END;
			sslist := v.subsystems; INC(v.maxLevel);
			WHILE sslist # NIL DO
				n := l;
				WHILE (n # NIL) & (sslist.name # n.name) DO n := n.next END;
				IF n # NIL THEN
					sslist.isBasic := TRUE; INC(sslist.level)
				END;
				sslist := sslist.next
			END
		END
	END SetBasic;
	
	PROCEDURE StartAnalysis(startList: NameList; font: Fonts.Font; incImpl: BOOLEAN; OUT v: View);
		VAR rootNode: List; sl: NameList;
	BEGIN
		NEW(v); Stores.Join(v, startList);
		v.showBasic := FALSE; 
		IF font = NIL THEN font := Fonts.dir.Default() END;
		v.normfont := Fonts.dir.This(font.typeface, font.size, font.style, Fonts.normal);
		v.selfont := Fonts.dir.This(font.typeface, font.size, font.style, Fonts.bold);
		v.startList := startList; v.subsystems := NIL; v.list := NIL; v.maxLevel := 2; v.nofMods := 0;
		sl := startList;
		WHILE sl # NIL DO
			v.GetNode(sl.name, incImpl, rootNode);
			sl := sl.next
		END;
		SetBasic(v); v.SetExtents()
	END StartAnalysis;
	
	PROCEDURE NewAnalysisClick*;
		VAR 
			v, nv: View;
			l: List; nl, start: NameList;
	BEGIN
		v := Controllers.FocusView()(View); 
		IF v # NIL THEN 
			l := v.list; start := NIL;
			WHILE l # NIL DO
				IF l.Selected() THEN NEW(nl); nl.name := l.name; nl.next := start; start := nl END;
				l := l.next
			END;
			IF start # NIL THEN
				StartAnalysis(start, v.normfont, implicit, nv); 
				IF nv.list # NIL THEN Views.Deposit(nv); StdCmds.Open END
			END
		END
	END NewAnalysisClick;

	PROCEDURE Analyze (OUT v: View);
		VAR startList: NameList;
	BEGIN
		GetModuleList(startList);
		IF startList # NIL THEN StartAnalysis(startList, NIL, implicit, v)
		ELSE Dialog.ShowMsg("#Dev:NoModuleNameSelected")
		END
	END Analyze;
	
	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		Analyze(v); Views.Deposit(v)
	END Deposit;
	
	PROCEDURE CreateTool*;
		VAR v: View;
	BEGIN
		Analyze(v); IF v.list # NIL THEN v.CreateTool END
	END CreateTool;

END DevDependencies.

 "DevDependencies.Deposit;StdCmds.Open"
 DevDependencies.CreateTool
