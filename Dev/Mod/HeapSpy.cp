MODULE DevHeapSpy;
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
		S := SYSTEM, Kernel, Strings,
		Ports, Models, Views, Services, Stores, Properties, Dialog, Controllers, Documents, DevDebug;

	CONST
		resolution = 8; (* one pixel represents resolution bytes *)
		lineW = 512; (* one line is 512 pixels long, i.e. represents 512 * resolution bytes *)
		lineH = 5; (* one line is lineH pixels high *)
		lineVertM = 3; (* margin between two lines *)
		lineHorM = 2; (* margin between vertical cluster edge and lines *)
		cluW = lineW + 2*lineHorM;
		cluHorM = 4; (* cluHorM pixels between left view edge and vertical cluster edge *)
		cluTopM = 8; (* cluTopM pixels between clusters *)
		
		W = (2*cluHorM + cluW) * Ports.point;

	TYPE
		Block = POINTER TO RECORD [untagged]
			tag: Kernel.Type;
			size: INTEGER;		(* size of free blocks *)
			actual: INTEGER;
			first: INTEGER
		END;
		Cluster = POINTER TO RECORD [untagged]
			size: INTEGER;	(* total size *)
			next: Cluster;
		END;

		View = POINTER TO RECORD (Views.View)
			height: INTEGER
		END;
		
		Model = POINTER TO RECORD (Models.Model)
			alloc: INTEGER
		END;
		
		Action = POINTER TO RECORD (Services.Action) END;

		Msg = RECORD (Models.Message) END;
		
	VAR
		mem: Model;
		par-: RECORD
			allocated-: INTEGER; (* number of bytes currently allocated *)
			clusters-: INTEGER; (* number of clusters currently allocated *)
			heapsize-: INTEGER (* total bytes currently allocated for heap *)
		END;

	PROCEDURE Append (VAR s: ARRAY OF CHAR; t: ARRAY OF CHAR);
		VAR len, i, j: INTEGER; ch: CHAR;
	BEGIN
		len := LEN(s);
		i := 0; WHILE s[i] # 0X DO INC(i) END;
		j := 0; REPEAT ch := t[j]; s[i] := ch; INC(j); INC(i) UNTIL (ch = 0X) OR (i = len);
		s[len - 1] := 0X
	END Append;

	PROCEDURE SAppend (VAR s: ARRAY OF CHAR; t: ARRAY OF SHORTCHAR);
		VAR str: ARRAY 256 OF CHAR;
	BEGIN
		str := t$; Append(s, str)
	END SAppend;

	PROCEDURE SizeOf (f: SHORTCHAR; t: Kernel.Type): INTEGER;
		VAR x: INTEGER;
	BEGIN
		CASE f OF
		| 0BX: RETURN 0
		| 1X, 2X, 4X: RETURN 1
		| 3X, 5X: RETURN 2
		| 8X, 0AX: RETURN 8
		| 11X: RETURN t.size
		| 12X:
			x := S.VAL(INTEGER, t.base[0]);
			IF x DIV 256 # 0 THEN x := t.base[0].id MOD 4 + 16 END;
			RETURN t.size * SizeOf(SHORT(CHR(x)), t.base[0])
		ELSE RETURN 4
		END
	END SizeOf;

	PROCEDURE FormOf (t: Kernel.Type): SHORTCHAR;
	BEGIN
		IF S.VAL(INTEGER, t) DIV 256 = 0 THEN
			RETURN SHORT(CHR(S.VAL(INTEGER, t)))
		ELSE
			RETURN SHORT(CHR(16 + t.id MOD 4))
		END
	END FormOf;
		
	PROCEDURE WriteName (t: Kernel.Type; VAR s: ARRAY OF CHAR);
		VAR name: Kernel.Name; f: SHORTCHAR;
	BEGIN
		f := FormOf(t);
		CASE f OF
		| 0X: Dialog.MapString("#Dev:Undefined", s)
		| 1X: s := "BOOLEAN"
		| 2X: s := "SHORTCHAR"
		| 3X: s := "CHAR"
		| 4X: s := "BYTE"
		| 5X: s := "SHORTINT"
		| 6X: s := "INTEGER"
		| 7X: s := "SHORTREAL"
		| 8X: s := "REAL"
		| 9X: s := "SET"
		| 0AX: s := "LONGINT"
		| 0BX: s := "ANYREC"
		| 0CX: s := "ANYPTR"
		| 0DX: s := "POINTER"
		| 0EX: s := "PROCEDURE"
		| 0FX: s := "STRING"
		| 10X, 11X, 13X:
			IF (t.id DIV 256 # 0) & (t.mod.refcnt >= 0) THEN
				s := t.mod.name$; Append(s, ".");
				Kernel.GetTypeName(t, name); SAppend(s, name)
			ELSIF f = 11X THEN
				s := t.mod.name$; Append(s, ".RECORD"); 
			ELSIF f = 13X THEN
				s := "POINTER"
			ELSE
				s := "PROCEDURE"
			END
		| 20X: s := "COM.IUnknown"
		| 21X: s := "COM.GUID"
		| 22X: s := "COM.RESULT"
		ELSE Dialog.MapString("#Dev:UnknownFormat", s)
		END
	END WriteName;
		
	PROCEDURE Next (b: Block): Block;	(* next block in same cluster *)
		VAR size: INTEGER; tag: Kernel.Type;
	BEGIN
		tag := S.VAL(Kernel.Type, S.VAL(INTEGER, b.tag) DIV 4 * 4);
		size := tag.size + 4;
		IF ODD(S.VAL(INTEGER, b.tag) DIV 2) THEN size := b.size - S.ADR(b.size) + size END;
		size := (size + 15) DIV 16 * 16;
		RETURN S.VAL(Block, S.VAL(INTEGER, b) + size)
	END Next;

	PROCEDURE ClusterHeight (size: INTEGER): INTEGER;
		(* height of a cluster in pixels *)
		VAR noflines: INTEGER;
	BEGIN
		noflines := ((size DIV resolution) + (lineW-1)) DIV lineW;
		RETURN noflines * (lineH + lineVertM) + lineVertM;
	END ClusterHeight;

	PROCEDURE ViewHeight(): INTEGER;
		(* height of view in universal units *)
		VAR cluster: Cluster; h: INTEGER;
	BEGIN
		cluster := S.VAL(Cluster, Kernel.Root()); h := 0;
		WHILE cluster # NIL DO
			INC(h, (cluTopM + ClusterHeight(cluster.size)) * Ports.point);
			cluster := cluster.next;
		END;
		INC(h, cluTopM * Ports.point);
		RETURN h;
	END ViewHeight;

	PROCEDURE PaintMem (f: Views.Frame; top, bottom: INTEGER);
		VAR cluster: Cluster; b0, blk, next: Block; end: INTEGER;
			clusterTop, clusterH, x, x1, y: INTEGER; (* units *)
			dot, painted, runlen: INTEGER;
			c, c0: INTEGER;
	BEGIN
		dot := (Ports.point DIV f.dot) * f.dot;
		IF dot = 0 THEN dot := f.dot END;
		clusterTop := 0;
		cluster := S.VAL(Cluster, Kernel.Root());
		WHILE cluster # NIL DO
			INC(clusterTop, cluTopM*dot);
			clusterH := ClusterHeight(cluster.size) * dot;
			IF (clusterTop - cluTopM < bottom) OR (clusterTop + clusterH > top) THEN
				f.DrawRect(cluHorM*dot, clusterTop, (cluHorM+cluW)*dot, clusterTop + clusterH,
							Ports.fill, Ports.grey25);
				(* scan cluster and draw blocks *)
				blk := S.VAL(Block, S.VAL(INTEGER, cluster) + 12);
				end := S.VAL(INTEGER, blk) + (cluster.size - 12) DIV 16 * 16;
				b0 := blk; c0 := -1;
				y := clusterTop + lineVertM*dot;
				painted := 16 DIV resolution; (* nof pixels already painted on current line *)
				x := (cluHorM+lineHorM+painted)*dot;
				WHILE S.VAL(INTEGER, blk) < end DO
					IF S.VAL(INTEGER, blk.tag) = S.ADR(blk.size) THEN (* free block *)
						c := -1; next := S.VAL(Block, S.VAL(INTEGER, blk) + blk.size + 4)
					ELSIF 1 IN S.VAL(SET, blk.tag) THEN (* array *)
						c := Ports.blue; next := Next(blk)
					ELSE (* record *)
						c := Ports.red; next := S.VAL(Block, S.VAL(INTEGER, blk) + (blk.tag.size + 19) DIV 16 * 16)
					END;
					IF c # c0 THEN
						runlen := (S.VAL(INTEGER, blk) - S.VAL(INTEGER, b0)) DIV resolution;
						WHILE runlen > lineW-painted DO
							x1 := (cluHorM+lineHorM+lineW)*dot;
							IF (c0 # -1) & (x < x1) THEN
								f.DrawRect(x, y, x1, y + lineH * dot, Ports.fill, c0)
							END;
							DEC(runlen, lineW-painted); painted := 0;
							x := (cluHorM+lineHorM)*dot; INC(y, (lineH+lineVertM) * dot);
						END;
						IF runlen > 0 THEN
							IF c0 # -1 THEN f.DrawRect(x, y, x + runlen*dot, y + lineH * dot, Ports.fill, c0) END;
							INC(painted, runlen); INC(x, runlen*dot);
						END;
						b0 := blk; c0 := c
					END;
					blk := next
				END;
				IF c0 # -1 THEN
					runlen := (S.VAL(INTEGER, end) - S.VAL(INTEGER, b0)) DIV resolution;
					WHILE runlen > lineW-painted DO
						f.DrawRect(x, y, (cluHorM+lineHorM+lineW)*dot, y + lineH * dot, Ports.fill, c0);
						DEC(runlen, lineW-painted); painted := 0;
						x := (cluHorM+lineHorM)*dot; INC(y, (lineH+lineVertM) * dot);
					END;
					IF runlen > 0 THEN f.DrawRect(x, y, x + runlen*dot, y + lineH * dot, Ports.fill, c0) END;
				END
			END;
			cluster := cluster.next;
			INC(clusterTop, clusterH);
		END
	END PaintMem;

	PROCEDURE MarkBlock (f: Views.Frame; sel: Block; on: BOOLEAN);
		VAR cluster: Cluster; next: Block; end, offs, dot, col: INTEGER; found: BOOLEAN;
			clusterTop, x, y, r, e: INTEGER; (* units *)
	BEGIN
		dot := (Ports.point DIV f.dot) * f.dot;
		IF dot = 0 THEN dot := f.dot END;
		clusterTop := 0;
		cluster := S.VAL(Cluster, Kernel.Root()); found := FALSE;
		WHILE (cluster # NIL) & ~found DO
			end := S.VAL(INTEGER, cluster) + 12 + (cluster.size - 12) DIV 16 * 16;
			INC(clusterTop, cluTopM * dot);
			IF (S.VAL(INTEGER, cluster) <= S.VAL(INTEGER, sel)) & (S.VAL(INTEGER, sel) < end) THEN
				found := TRUE;
			ELSE
				INC(clusterTop, ClusterHeight(cluster.size) * dot);
				cluster := cluster.next;
			END
		END;
		IF found THEN (* sel is contained in cluster *)
			IF on THEN col := Ports.green
			ELSIF 1 IN S.VAL(SET, sel.tag) THEN col := Ports.blue
			ELSE col := Ports.red
			END;
			next := Next(sel);
			r := (cluHorM + lineHorM + lineW) * dot;
			offs := (S.VAL(INTEGER, sel) + 4 (* tag *) - S.VAL(INTEGER, cluster)) DIV resolution;
			y := clusterTop + ((offs DIV lineW) * (lineH+lineVertM) + lineVertM) * dot;
			x := (cluHorM + lineHorM + (offs MOD lineW)) * dot;
			e := x + (S.VAL(INTEGER, next) - S.VAL(INTEGER, sel)) DIV resolution * dot;
			WHILE e >= r DO
				f.DrawRect(x, y, r, y + lineH * dot, Ports.fill, col);
				INC(y, (lineH + lineVertM) * dot);
				x := (cluHorM + lineHorM) * dot;
				e := x + e - r;
			END;
			IF e > x THEN f.DrawRect(x, y, e, y + lineH * dot, Ports.fill, col) END;
		END
	END MarkBlock;

	PROCEDURE ThisCluster (f: Views.Frame; sx, sy: INTEGER): Cluster;
		VAR cluster: Cluster; dot, clusterTop, clusterH: INTEGER;
	BEGIN
		dot := (Ports.point DIV f.dot) * f.dot;
		IF dot = 0 THEN dot := f.dot END;
		cluster := NIL;
		IF (cluHorM * dot <= sx) & (sx < (cluHorM + 2*lineHorM + lineW) * dot) THEN
			cluster := S.VAL(Cluster, Kernel.Root());
			clusterTop := 0;
			WHILE cluster # NIL DO
				INC(clusterTop, cluTopM * dot);
				clusterH := ClusterHeight(cluster.size) * dot;
				IF (clusterTop <= sy) & (sy < clusterTop + clusterH) THEN RETURN cluster
				ELSE INC(clusterTop, clusterH); cluster := cluster.next;
				END
			END
		END;
		RETURN cluster;
	END ThisCluster;

	PROCEDURE ThisBlock (f: Views.Frame; sx, sy: INTEGER): Block;
		VAR cluster: Cluster; blk, next: Block; found: BOOLEAN;
			dot, lineno, offs, end, adr: INTEGER;
			clusterTop, clusterH: INTEGER;
	BEGIN
		dot := (Ports.point DIV f.dot) * f.dot;
		IF dot = 0 THEN dot := f.dot END;
		IF (sx < (cluHorM + lineHorM) * dot) OR ((cluHorM + lineHorM + lineW) * dot <= sx) THEN
			RETURN NIL
		END;
		cluster := S.VAL(Cluster, Kernel.Root());
		clusterTop :=0; found := FALSE;
		WHILE (cluster # NIL) & ~found DO
			INC(clusterTop, cluTopM * dot);
			clusterH := ClusterHeight(cluster.size) * dot;
			IF (clusterTop <= sy) & (sy < clusterTop + clusterH) THEN found := TRUE
			ELSE INC(clusterTop, clusterH); cluster := cluster.next;
			END
		END;
		IF found THEN (* (sx, sy) points into cluster *)
			lineno := ((sy - clusterTop) DIV dot) DIV (lineH + lineVertM);
			offs := ((sy - clusterTop) DIV dot) MOD (lineH + lineVertM); (* vertical offset in line *)
			IF (lineVertM <= offs) & (offs < lineH + lineVertM) THEN
				offs := (sx DIV dot) - cluHorM - lineHorM; (* hor offset from left into line *)
				adr := S.VAL(INTEGER, cluster) + (lineno * lineW + offs) * resolution;
				(* we're looking for the block that contains address adr *)
				blk := S.VAL(Block, S.VAL(INTEGER, cluster) + 12);
				end := S.VAL(INTEGER, blk) + (cluster.size - 12) DIV 16 * 16;
				WHILE S.VAL(INTEGER, blk) < end DO
					next := Next(blk);
					IF adr < S.VAL(INTEGER, next) THEN
						IF S.VAL(INTEGER, blk.tag) # S.ADR(blk.size) THEN RETURN blk
						ELSE RETURN NIL (* blk is a free block *)
						END
					ELSE blk := next
					END
				END;
				RETURN NIL
			ELSE (* (sx, sy) points between two lines *) RETURN NIL
			END
		ELSE RETURN NIL
		END
	END ThisBlock;

	PROCEDURE SearchPath (this, that: Block; VAR path: ARRAY OF INTEGER; VAR len: INTEGER);
		VAR father, son: Block; tag: Kernel.Type; flag, offset, actual, i: INTEGER; found: BOOLEAN;
	BEGIN
		i := 1; len := 0; found := FALSE;
		IF ~ODD(S.VAL(INTEGER, this.tag)) THEN
			father := NIL;
			LOOP
				INC(S.VAL(INTEGER, this.tag));
				flag := S.VAL(INTEGER, this.tag) MOD 4;
				tag := S.VAL(Kernel.Type, S.VAL(INTEGER, this.tag) - flag);
				IF flag >= 2 THEN actual := this.first; this.actual := actual
				ELSE actual := S.ADR(this.size)
				END;
				LOOP
					IF (this = that) & ~found THEN len := i; found := TRUE END;
					offset := tag.ptroffs[0];
					IF offset < 0 THEN
						INC(S.VAL(INTEGER, tag), offset + 4); (* restore tag *)
						IF (flag >= 2) & (actual < this.size) & (offset < -4) THEN (* next array element *)
							INC(actual, tag.size); this.actual := actual
						ELSE (* up *)
							this.tag := S.VAL(Kernel.Type, S.VAL(INTEGER, tag) + flag);
							IF father = NIL THEN RETURN END;
							son := this; this := father; DEC(i);
							flag := S.VAL(INTEGER, this.tag) MOD 4;
							tag := S.VAL(Kernel.Type, S.VAL(INTEGER, this.tag) - flag);
							offset := tag.ptroffs[0];
							IF flag >= 2 THEN actual := this.actual ELSE actual := S.ADR(this.size) END;
							S.GET(actual + offset, father); S.PUT(actual + offset, S.ADR(son.size));
							INC(S.VAL(INTEGER, tag), 4)
						END
					ELSE
						S.GET(actual + offset, son);
						IF (son # NIL) & ~found THEN
							DEC(S.VAL(INTEGER, son), 4);
							IF ~ODD(S.VAL(INTEGER, son.tag)) THEN (* down *)
								IF i < LEN(path) THEN
									IF flag < 2 THEN path[i] := offset
									ELSE path[i] := actual - S.ADR(this.size) + offset
									END
								END;
								INC(i);
								this.tag := S.VAL(Kernel.Type, S.VAL(INTEGER, tag) + flag);
								S.PUT(actual + offset, father); father := this; this := son;
								EXIT
							END
						END;
						INC(S.VAL(INTEGER, tag), 4)
					END
				END
			END
		END
	END SearchPath;

	PROCEDURE ResetMarks;
		VAR cluster: Cluster; blk: Block; end: INTEGER;
	BEGIN
		cluster := S.VAL(Cluster, Kernel.Root());
		WHILE cluster # NIL DO
			blk := S.VAL(Block, S.VAL(INTEGER, cluster) + 12);
			end := S.VAL(INTEGER, blk) + (cluster.size - 12) DIV 16 * 16;
			WHILE S.VAL(INTEGER, blk) < end DO EXCL(S.VAL(SET, blk.tag), 0); blk := Next(blk) END;
			cluster := cluster.next
		END
	END ResetMarks;

	PROCEDURE SearchAnchor (blk: Block; VAR path: ARRAY OF CHAR);
		VAR m, mod: Kernel.Module; ref, offs, i, j, k, p, x, a, n: INTEGER; offsets: ARRAY 1 OF INTEGER;
			 t, f: SHORTCHAR; desc: Kernel.Type; name, title: Kernel.Name; tag, typ: Kernel.Type;
	BEGIN
		mod := NIL; offs := 0;
		m := Kernel.ThisLoadedMod("HostWindows");
		IF m # NIL THEN
			ref := m.refs; Kernel.GetRefProc(ref, x, name);
			IF x # 0 THEN
				REPEAT
					Kernel.GetRefVar(ref, t, f, desc, a, name)
				UNTIL (t # 1X) OR (name = "winAnchor");
				IF t = 1X THEN
					S.GET(m.data + a, p);
					IF p # 0 THEN
						SearchPath(S.VAL(Block, p - 4), blk, offsets, n);
						IF n > 0 THEN offs := 1 END
					END
				END
			END
		END;
		m := Kernel.modList;
		WHILE (mod = NIL) & (offs = 0) & (m # NIL) DO
			IF m.refcnt >= 0 THEN
				i := 0;
				WHILE (mod = NIL) & (i < m.nofptrs) DO
					S.GET(m.data + m.ptrs[i], p);
					IF p # 0 THEN
						SearchPath(S.VAL(Block, p - 4), blk, offsets, n);
						IF n > 0 THEN mod := m; offs := m.ptrs[i] END
					END;
					INC(i)
				END
			END;
			m := m.next
		END;
		ResetMarks;
		IF offs # 0 THEN
			IF mod # NIL THEN
				path := mod.name$; Append(path, ".");
				ref := mod.refs; Kernel.GetRefProc(ref, x, name);
				IF x # 0 THEN
					REPEAT
						Kernel.GetRefVar(ref, t, f, desc, a, name)
					UNTIL (t # 1X) OR (offs >= a) & (offs < a + SizeOf(f, desc));
					IF t = 1X THEN SAppend(path, name)
					ELSE Append(path, "???")
					END
				END
			ELSE path := "window list"
			END;
			i := 1;
			WHILE (i < n) & (i < LEN(offsets)) DO
				S.GET(p - 4, tag);
				IF 1 IN S.VAL(SET, tag) THEN Append(path, "[]")
				ELSE
					k := 0;
					REPEAT
						typ := tag.base[k]; INC(k); j := 0;
						WHILE (j < typ.fields.num) & (typ.fields.obj[j].offs # offsets[i]) DO INC(j) END;
					UNTIL (j < typ.fields.num) OR (k > tag.id DIV 16 MOD 16);
					IF j < typ.fields.num THEN
						Kernel.GetObjName(typ.mod, S.ADR(typ.fields.obj[j]), name);
						Append(path, "."); SAppend(path, name)
					ELSE
						Append(path, ".?")
					END
				END;
				S.GET(p + offsets[i], p); INC(i)
			END
		ELSE path := ""
		END
	END SearchAnchor;

	PROCEDURE ShowBlock (blk: Block);
		VAR  title: ARRAY 1024 OF CHAR; path: ARRAY 1024 OF CHAR;
	BEGIN
		SearchAnchor(blk, path);
		IF path # "" THEN
			title := "Object anchored in ";
			Append(title, path);
		ELSE title := "Object not globally anchored"
		END;
		DevDebug.ShowHeapObject(S.ADR(blk.size), title)
	END ShowBlock;

	PROCEDURE BlockInfo (blk: Block; VAR s: ARRAY OF CHAR);
		VAR tag: Kernel.Type; str: ARRAY 256 OF CHAR;
	BEGIN
		tag := blk.tag;
		IF ODD(S.VAL(INTEGER, tag) DIV 2) THEN	(* array *)
			DEC(S.VAL(INTEGER, tag), 2);
			IF (tag.mod.name = "Kernel") & (tag.fields.num = 1) THEN
				tag := tag.fields.obj[0].struct
			END;
			WriteName(tag, str);
			s := "ARRAY OF "; Append(s, str)
		ELSE	(* record *)
			WriteName(tag, s)
		END
	END BlockInfo;

	PROCEDURE HeapInfo (VAR size, nofclusters: INTEGER);
		VAR cluster: Cluster;
	BEGIN
		nofclusters := 0; size := 0; cluster := S.VAL(Cluster, Kernel.Root());
		WHILE cluster # NIL DO
			INC(nofclusters); INC(size, cluster.size);
			cluster := cluster.next
		END;
	END HeapInfo;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN mem
	END ThisModel;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR h: INTEGER;
	BEGIN
		PaintMem(f, t, b);
		h := ViewHeight();
		IF h # v.height THEN v.context.SetSize(W, h) END;
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (
		f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View
	);
		VAR x, y: INTEGER; isDown: BOOLEAN; blk, b: Block; m: SET; c, lastC: Cluster;
			s: ARRAY 100 OF CHAR; num: ARRAY 16 OF CHAR;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			blk := NIL; lastC := NIL;
			REPEAT
				f.Input(x, y, m, isDown);
				c := ThisCluster(f, x, y); b := ThisBlock(f, x, y);
				IF (b # blk) OR (c # lastC) THEN
					IF b # NIL THEN BlockInfo(b, s);
					ELSIF c # NIL THEN s := "cluster of length ";
						IF c.size MOD 1024 = 0 THEN
							Strings.IntToString(c.size DIV 1024, num); Append(s, num); Append(s, " KB")
						ELSE Strings.IntToString(c.size, num); Append(s, num); Append(s, " bytes")
						END;
						Append(s, " at address ");
						Strings.IntToStringForm(S.VAL(INTEGER, c), Strings.hexadecimal, 9, "0", TRUE, num);
						Append(s, num)
					ELSE s := ""
					END;
					Dialog.ShowStatus(s)
				END;
				lastC := c;
				IF b # blk THEN
					IF blk # NIL THEN MarkBlock(f, blk, FALSE) END;
					blk := b;
					IF blk # NIL THEN MarkBlock(f, blk, TRUE) END
				END
			UNTIL ~isDown;
			(* IF ~Dialog.showsStatus & (s # "") THEN Dialog.ShowMsg(s) END; *)
			IF blk # NIL THEN
				MarkBlock(f, blk, FALSE);
				ShowBlock(blk)
			END;
			Dialog.ShowStatus("");
		ELSE
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandleModelMsg (VAR msg: Models.Message);
	BEGIN
		WITH msg: Msg DO Views.Update(v, Views.keepFrames)
		ELSE
		END
	END HandleModelMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.Preference DO
			WITH msg: Properties.ResizePref DO msg.fixed := TRUE
			| msg: Properties.FocusPref DO msg.hotFocus := TRUE
			| msg: Properties.SizePref DO msg.w := W; msg.h := ViewHeight();
			ELSE
			END
		ELSE
		END
	END HandlePropMsg;


	PROCEDURE (m: Model) CopyFrom (source: Stores.Store), EMPTY;


	PROCEDURE (a: Action) Do;
		VAR alloc, size, nofclusters: INTEGER; msg: Msg;
	BEGIN
		alloc := Kernel.Allocated();
		IF mem.alloc # alloc THEN
			mem.alloc := alloc;
			Models.Broadcast(mem, msg);
			par.allocated := alloc;
			HeapInfo(size, nofclusters);
			IF nofclusters # par.clusters THEN par.clusters := nofclusters END;
			IF size # par.heapsize THEN par.heapsize := size END;
			Dialog.Update(par)
		END;
		Services.DoLater(a, Services.Ticks() + Services.resolution)
	END Do;

	PROCEDURE ShowHeap*;
		VAR v: View; action: Action; d: Documents.Document;
	BEGIN
		NEW(v); v.height := ViewHeight();
		d := Documents.dir.New(v, W, v.height);
		Views.OpenAux(d, "Heap Layout")
	END ShowHeap;

	PROCEDURE GetAnchor* (adr: INTEGER; OUT anchor: ARRAY OF CHAR);
	BEGIN
		SearchAnchor(S.VAL(Block, adr - 4), anchor);
	END GetAnchor;

	PROCEDURE ShowAnchor* (adr: INTEGER);
	BEGIN
		ShowBlock(S.VAL(Block, adr - 4))
	END ShowAnchor;

	PROCEDURE Init;
		VAR action: Action;
	BEGIN
		NEW(mem); mem.alloc := 0;
		Stores.InitDomain(mem);
		NEW(action); Services.DoLater(action, Services.now)
	END Init;

BEGIN Init
END DevHeapSpy.
