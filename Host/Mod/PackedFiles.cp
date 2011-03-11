MODULE HostPackedFiles;
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

	(* This module depends on the implementation of DevPacker. *)

	IMPORT SYSTEM, Files, HostFiles (*, Dialog*);

	CONST
		packTag* = 12681268H;
		version = 1; (* same as in DevPacker *)

	TYPE
		Directory = POINTER TO RECORD (Files.Directory) END;
		PackedDirectory = POINTER TO RECORD (Files.Directory) END;
		FileList = POINTER TO RECORD
			path, name: Files.Name;
			adr, len: INTEGER;
			year, month, day, hour, minute, second: INTEGER;
			next: FileList
		END;
		Locator = POINTER TO RECORD
			name: Files.Name;
			files: FileList;
			sublocs: Locator;
			next: Locator
		END;
		File = POINTER TO RECORD (Files.File)
			l: FileList;
			f: Files.File
		END;
		Reader = POINTER TO RECORD (Files.Reader)
			r: Files.Reader;
			base: File
		END;

	VAR
		orgdir: Files.Directory;
		stdDir-, packedDir-: Files.Directory;
		roots: Locator;
		exefile: Files.File;
		curloc: HostFiles.Locator;

	(* Auxiliary procedures *)

	PROCEDURE DebugPrint(IN str: ARRAY OF CHAR);
	BEGIN
		(*
		Dialog.ShowMsg(str)
		*)
	END DebugPrint;

	PROCEDURE ReadInt (r: Files.Reader; OUT x: INTEGER);
		VAR b: ARRAY 4 OF BYTE;
	BEGIN
		r.ReadBytes(b, 0, 4);
		x := b[0] MOD 256 + 256 * (b[1] MOD 256 + 256 * (b[2] MOD 256 + 256 * (b[3] MOD 256)))
	END ReadInt;

	PROCEDURE ReadChar (r: Files.Reader; OUT x: CHAR);
		VAR b: ARRAY 2 OF BYTE;
	BEGIN
		r.ReadBytes(b, 0, 2);
		x := SYSTEM.VAL(CHAR, b)
	END ReadChar;

	PROCEDURE ReadString (r: Files.Reader; OUT x: ARRAY OF CHAR);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; REPEAT ReadChar(r, ch); x[i] := ch; INC(i) UNTIL ch = 0X
	END ReadString;

	PROCEDURE Diff (VAR a, b: ARRAY OF CHAR; caseSens: BOOLEAN): INTEGER;
		VAR i: INTEGER; cha, chb: CHAR;
	BEGIN
		i := 0;
		REPEAT
			cha := a[i]; chb := b[i]; INC(i);
			IF cha # chb THEN
				IF ~caseSens THEN
					IF (cha >= "a") & ((cha <= "z") OR (cha >= 0E0X) & (cha <= 0FEX) & (cha # 0F7X)) THEN cha := CAP(cha)
					END;
					IF (chb >= "a") & ((chb <= "z") OR (chb >= 0E0X) & (chb <= 0FEX) & (chb # 0F7X)) THEN chb := CAP(chb)
					END
				END;
				IF cha = "\" THEN cha := "/" END;
				IF chb = "\" THEN chb := "/" END;
				IF cha # chb THEN RETURN ORD(cha) - ORD(chb) END
			END
		UNTIL cha = 0X;
		RETURN 0
	END Diff;

	PROCEDURE GetType(name: Files.Name; OUT type: Files.Type);
		VAR l, i: INTEGER;
	BEGIN
		l := LEN(name$); type := "";
		WHILE (l > 0) & (name[l - 1] # '.') DO DEC(l) END;
		FOR i := 0 TO LEN(name$) - l DO type[i] := name[l + i] END
	END GetType;

	PROCEDURE GetNextSubLoc (path: Files.Name; beg: INTEGER; OUT res: Files.Name; OUT end: INTEGER);
		VAR i: INTEGER;
	BEGIN
		i := 0; res := "";
		WHILE (beg < LEN(path$)) & (path[beg] # '/') & (path[beg] # '\') & (path[beg] # ':') DO
			res[i] := path[beg];
			INC(beg); INC(i)
		END;
		res[i] := 0X; end := beg
	END GetNextSubLoc;

	PROCEDURE GetLoc(path: Files.Name; create: BOOLEAN): Locator;
		VAR end, diff: INTEGER;sp: Files.Name; loc, tl, last: Locator;
	BEGIN
		sp := "";
		IF path = '' THEN
			DebugPrint("Cannot use an empty path.");
			RETURN NIL
		ELSIF (path[0] = '/') OR (path[0] = '\') THEN (* network path *)
			IF (path[1] = '/') OR (path[1] = '\') THEN
				GetNextSubLoc(path, 2, sp, end)
			ELSE
				DebugPrint("Invalid network path.");
				RETURN NIL
			END
		ELSIF path[1] = ':' THEN (* absolute path *)
			GetNextSubLoc(path, 0, sp, end)
		ELSE
			DebugPrint("No absolute path.");
			RETURN NIL
		END;
		IF sp # "" THEN
			loc := roots; last := loc;
			IF loc # NIL THEN diff := Diff(sp, loc.name, FALSE) END;
			WHILE (loc # NIL) & (diff > 0) DO
				last := loc; loc := loc.next;
				IF loc # NIL THEN diff := Diff(sp, loc.name, FALSE) END
			END;
			IF ((loc = NIL) OR (diff # 0)) & ~create THEN RETURN NIL END;
			IF (loc = NIL) OR (diff < 0) THEN
				NEW(loc); loc.name := sp;
				IF roots = NIL THEN
					roots := loc
				ELSE
					loc.next := last.next; last.next := loc
				END
			END;
			GetNextSubLoc(path, 3, sp, end);
			WHILE sp # "" DO
				tl := loc.sublocs; last := NIL;
				IF tl # NIL THEN diff := Diff(sp, tl.name, FALSE) END;
				WHILE (tl # NIL) & (diff > 0) DO
					last := tl; tl := tl.next;
					IF tl # NIL THEN diff := Diff(sp, tl.name, FALSE) END
				END;
				IF (tl = NIL) OR (diff < 0) THEN
					IF create THEN
						NEW(tl); tl.name := sp;
						IF last = NIL THEN
							tl.next := loc.sublocs; loc.sublocs := tl
						ELSE
							tl.next := last.next; last.next := tl
						END
					ELSE
						RETURN NIL
					END
				END;
				loc := tl;
				GetNextSubLoc(path, end + 1, sp, end)
			END;
			RETURN loc
		END;
		RETURN NIL
	END GetLoc;

	PROCEDURE ReadResourceTable;
		VAR r: Files.Reader; tableadr, int, noff, i: INTEGER; str: Files.Name; l: FileList; loc: Locator;
	BEGIN
		roots := NIL; r := exefile.NewReader(NIL);
		r.SetPos(exefile.Length() - 12);
		ReadInt(r, int);
		IF int = packTag THEN
			ReadInt(r, int);
			IF int = version THEN
				ReadInt(r, tableadr);
				r.SetPos(tableadr); ReadInt(r, noff);
				NEW(roots);
				FOR i := 0 TO noff - 1 DO
					(* Files are packed in reversed alphabetical order,
						so adding files at the beginning of the list renders an alphabetically sorted list. *)
					NEW(l);
					ReadString(r, str); l.path := str; ReadString(r, str); l.name := str;
					ReadInt(r, int); l.adr := int; ReadInt(r, int); l.len := int;
					ReadInt(r, int); l.year := int; ReadInt(r, int); l.month := int; ReadInt(r, int); l.day := int;
					ReadInt(r, int); l.hour := int; ReadInt(r, int); l.minute := int; ReadInt(r, int); l.second := int;
					loc := GetLoc(curloc.path + '/' + l.path, TRUE);
					l.next := loc.files; loc.files := l
				END
			END
		END
	END ReadResourceTable;

	PROCEDURE Get (path: HostFiles.FullName; name: Files.Name): Files.File;
		VAR
			l: FileList; f: File; loc: Locator;
			type: Files.Type;
			diff: INTEGER;
	BEGIN
		loc := GetLoc(path$, FALSE);
		IF loc # NIL THEN l := loc.files ELSE RETURN NIL END;
		IF l # NIL THEN diff := Diff(l.name, name, FALSE) END;
		WHILE (l # NIL) & (diff < 0) DO
			l := l.next;
			IF l # NIL THEN diff := Diff(l.name, name, FALSE) END
		END;
		IF (l # NIL) & (diff = 0) THEN
			NEW(f); f.l := l; f.f := exefile;
			GetType(name, type); f.InitType(type);
			RETURN f
		END;
		RETURN NIL
	END Get;

	(* Files.Directory *)

	PROCEDURE (d: Directory) Delete (loc: Files.Locator; name: Files.Name);
	BEGIN
		orgdir.Delete(loc, name)
	END Delete;

	PROCEDURE (d: Directory) FileList (floc: Files.Locator): Files.FileInfo;
		VAR pi, fi, tfi, nfi, last: Files.FileInfo; diff: INTEGER; caseSens: BOOLEAN;
	BEGIN
		ASSERT(floc IS HostFiles.Locator, 20);
		fi := orgdir.FileList(floc); (* Gives an alphabetically sorted list of files. *)
		pi := packedDir.FileList(floc); (* Gives an alphabetically sorted list of files. *)
		nfi := NIL; last := NIL; tfi := NIL;
		(* Both fi and l are alphabetically sorted. And the returned list also has to be alphabetically sorted. *)
		caseSens := floc(HostFiles.Locator).caseSens;
		WHILE (pi # NIL) & (fi # NIL) DO
			diff := Diff(pi.name, fi.name, caseSens);
			IF diff >= 0 THEN
				tfi := fi;
				fi := fi.next;
				IF diff = 0 THEN pi := pi.next END
			ELSE
				tfi := pi;
				pi := pi.next
			END;
			IF nfi = NIL THEN nfi := tfi ELSE last.next := tfi END;
			last := tfi
		END;
		IF pi # NIL THEN
			IF nfi = NIL THEN nfi := pi ELSE last.next := pi END
		ELSIF fi # NIL THEN
			IF nfi = NIL THEN nfi := fi ELSE last.next := fi END
		END;
		RETURN nfi
	END FileList;

	PROCEDURE (d: Directory) GetFileName (name: Files.Name; type: Files.Type; OUT filename: Files.Name);
	BEGIN
		orgdir.GetFileName(name, type, filename)
	END GetFileName;

	PROCEDURE (d: Directory) LocList (floc: Files.Locator): Files.LocInfo;
		VAR pi, li, nli, last: Files.LocInfo; diff: INTEGER; caseSens: BOOLEAN;
	BEGIN
		li := orgdir.LocList(floc);
		pi := packedDir.LocList(floc);
		caseSens := floc(HostFiles.Locator).caseSens;
		nli := NIL;
		(* Both pi and li are alphabetically ordered. *)
		WHILE (pi # NIL) & (li # NIL) DO
			diff := Diff(pi.name, li.name, caseSens);
			IF diff >= 0 THEN
				IF nli = NIL THEN nli := li ELSE last.next := li END; last := li;
				li := li.next;
				IF diff = 0 THEN pi := pi.next END
			ELSE
				IF nli = NIL THEN nli := pi ELSE last.next := pi END; last := pi;
				pi := pi.next
			END
		END;
		IF pi = NIL THEN
			IF nli = NIL THEN nli := li ELSE last.next := li END
		ELSE
			IF nli = NIL THEN nli := pi ELSE last.next := pi END
		END;
		RETURN nli
	END LocList;

	PROCEDURE (d: Directory) New (loc: Files.Locator; ask: BOOLEAN): Files.File;
	BEGIN
		RETURN orgdir.New(loc, ask)
	END New;

	PROCEDURE (d: Directory) Old (loc: Files.Locator; name: Files.Name; shared: BOOLEAN): Files.File;
		VAR f: Files.File;
	BEGIN
		f := orgdir.Old(loc, name, shared);
		IF f = NIL THEN f := packedDir.Old(loc, name, shared) END;
		RETURN f
	END Old;

	PROCEDURE (d: Directory) Rename (loc: Files.Locator; old, new: Files.Name; ask: BOOLEAN);
	BEGIN
		orgdir.Rename(loc, old, new, ask)
	END Rename;

	PROCEDURE (d: Directory) SameFile (
		loc0: Files.Locator; name0: Files.Name; loc1: Files.Locator; name1: Files.Name
	): BOOLEAN;
	BEGIN
		RETURN orgdir.SameFile(loc0, name0, loc1, name1)
	END SameFile;

	PROCEDURE (d: Directory) Temp (): Files.File;
	BEGIN
		RETURN orgdir.Temp()
	END Temp;

	PROCEDURE (d: Directory) This (IN path: ARRAY OF CHAR): Files.Locator;
	BEGIN
		RETURN orgdir.This(path)
	END This;

	(* PackedDirectory *)

	PROCEDURE (d: PackedDirectory) Delete (loc: Files.Locator; name: Files.Name);
	BEGIN
		loc.res := 4 (* write-protection *)
	END Delete;

	PROCEDURE (d: PackedDirectory) FileList (floc: Files.Locator): Files.FileInfo;
		VAR nfi, tfi, last: Files.FileInfo; loc: Locator; l: FileList; type: Files.Type; hloc: HostFiles.Locator;
	BEGIN
		ASSERT(floc IS HostFiles.Locator, 20);
		hloc := floc(HostFiles.Locator);
		loc := GetLoc(hloc.path$, FALSE);
		nfi := NIL;
		IF loc # NIL THEN
			l := loc.files; last := NIL; tfi := NIL;
			(* l is alphabetically sorted. And the returned list also has to be alphabetically sorted. *)
			WHILE l # NIL DO
				GetType(l.name, type);
				NEW(tfi); tfi.name := l.name; tfi.type := type; tfi.attr := {Files.readOnly};
				tfi.modified.year := l.year; tfi.modified.month := l.month; tfi.modified.day := l.day;
				tfi.modified.hour := l.hour; tfi.modified.minute := l.minute; tfi.modified.second := l.second;
				IF nfi = NIL THEN nfi := tfi ELSE last.next := tfi END;
				last := tfi;
				l := l.next
			END
		END;
		RETURN nfi
	END FileList;

	PROCEDURE (d: PackedDirectory) GetFileName (name: Files.Name; type: Files.Type; OUT filename: Files.Name);
	BEGIN
		orgdir.GetFileName(name, type, filename)
	END GetFileName;

	PROCEDURE (d: PackedDirectory) LocList (floc: Files.Locator): Files.LocInfo;
		VAR nli, tli, last: Files.LocInfo; loc: Locator; hloc: HostFiles.Locator;
	BEGIN
		hloc := floc(HostFiles.Locator); nli := NIL;
		loc := GetLoc(hloc.path$, FALSE);
		IF loc # NIL THEN loc := loc.sublocs END;
		(* loc is alphabetically ordered. *)
		WHILE loc # NIL DO
			NEW(tli); tli.name := loc.name; tli.attr := {Files.readOnly};
			IF nli = NIL THEN nli := tli ELSE last.next := tli END;
			last := tli;
			loc := loc.next
		END;
		RETURN nli
	END LocList;

	PROCEDURE (d: PackedDirectory) New (loc: Files.Locator; ask: BOOLEAN): Files.File;
	BEGIN
		loc.res := 4; (* write-protection *)
		RETURN NIL
	END New;

	PROCEDURE (d: PackedDirectory) Old (loc: Files.Locator; name: Files.Name; shared: BOOLEAN): Files.File;
		VAR f: Files.File;
	BEGIN
		f := NIL;
		IF shared THEN
			WITH loc: HostFiles.Locator DO
				f := Get(loc.path, name);
				IF f # NIL THEN
					loc.res := 0
				END
			ELSE
				DebugPrint("HostPackedFiles: Directory.Old - This operation requires HostFiles. ")
			END
		END;
		RETURN f
	END Old;

	PROCEDURE (d: PackedDirectory) Rename (loc: Files.Locator; old, new: Files.Name; ask: BOOLEAN);
	BEGIN
		loc.res := 4 (* write-protection *)
	END Rename;

	PROCEDURE (d: PackedDirectory) SameFile (loc0: Files.Locator; name0: Files.Name; loc1: Files.Locator; name1: Files.Name): BOOLEAN;
	BEGIN
		RETURN orgdir.SameFile(loc0, name0, loc1, name1)
	END SameFile;

	PROCEDURE (d: PackedDirectory) Temp (): Files.File;
	BEGIN
		RETURN orgdir.Temp()
	END Temp;

	PROCEDURE (d: PackedDirectory) This (IN path: ARRAY OF CHAR): Files.Locator;
	BEGIN
		RETURN orgdir.This(path)
	END This;

	(* Files.Reader *)

	PROCEDURE (r: Reader) Base (): File;
	BEGIN
		RETURN r.base
	END Base;

	PROCEDURE (r: Reader) Pos (): INTEGER;
	BEGIN
		RETURN r.r.Pos() - r.base.l.adr
	END Pos;

	PROCEDURE (r: Reader) SetPos (pos: INTEGER);
	BEGIN
		ASSERT(pos <= r.base.l.len, 20);
		r.r.SetPos(pos + r.base.l.adr);
		r.eof := FALSE
	END SetPos;

	PROCEDURE (r: Reader) ReadByte (OUT x: BYTE);
	BEGIN
		IF (r.r.Pos() - r.base.l.adr) >= r.base.l.len THEN
			r.eof := TRUE; x := 0
		ELSE
			r.r.ReadByte(x)
		END
	END ReadByte;

	PROCEDURE (r: Reader) ReadBytes (VAR x: ARRAY OF BYTE; beg, len: INTEGER);
	BEGIN
		ASSERT(beg >= 0, 20); ASSERT(len >= 0, 21);
		ASSERT(beg + len <= LEN(x), 22);
		len := MIN(r.base.l.len, len);
		r.r.ReadBytes(x, beg, len);
		IF (r.r.Pos() - r.base.l.adr) >= r.base.l.len THEN r.eof := TRUE END
	END ReadBytes;

	(* Files.File *)

	PROCEDURE (f: File) Close;
	BEGIN
		(* Do nothing since all packed files are opened on the exe file which should stay open. *)
	END Close;

	PROCEDURE (f: File) Flush;
	BEGIN
		(* Do nothing since all packed files are read only. *)
	END Flush;

	PROCEDURE (f: File) Length(): INTEGER;
	BEGIN
		RETURN f.l.len
	END Length;

	PROCEDURE (f: File) NewReader(old: Files.Reader): Files.Reader;
		VAR r: Reader; hr: Files.Reader;
	BEGIN
		ASSERT(f.f # NIL, 20); ASSERT(f.l # NIL, 21);
		hr := f.f.NewReader(old);
		IF hr = NIL THEN RETURN NIL END;
		hr.SetPos(f.l.adr);
		NEW(r); r.base := f; r.r := hr; r.eof := FALSE;
		RETURN r
	END NewReader;

	PROCEDURE (f: File) NewWriter(old: Files.Writer): Files.Writer;
	BEGIN
		(* Return NIL since all packed files are read only. *)
		RETURN NIL
	END NewWriter;

	PROCEDURE (f: File) Register (name: Files.Name; type: Files.Type; ask: BOOLEAN; OUT res: INTEGER);
	BEGIN
		HALT(20)
		(* Do nothing since all packed files are opened using Old and only files opened using New can be registered. *)
	END Register;

	(* Inititlization and uninitialization *)

	PROCEDURE SetFilesDir*;
	BEGIN
		orgdir := Files.dir;
		IF orgdir # NIL THEN
			IF roots # NIL THEN
				curloc := Files.dir.This("")(HostFiles.Locator);
				Files.SetDir(stdDir)
			END
		ELSE
		END
	END SetFilesDir;

	PROCEDURE RestoreFilesDir*;
	BEGIN
		IF orgdir # NIL THEN Files.SetDir(orgdir) END
	END RestoreFilesDir;

	PROCEDURE IsInstalled*;
	BEGIN
		IF Files.dir IS Directory THEN
			DebugPrint("HostPackedFiles is installed")
		ELSE
			DebugPrint("HostPackedFiles is NOT installed")
		END
	END IsInstalled;

	PROCEDURE GetModDate* (f: Files.File; VAR year, month, day, hour, minute, second: INTEGER);
	BEGIN
		ASSERT(f IS File);
		WITH f: File DO
			year := f.l.year; month := f.l.month; day := f.l.day;
			hour := f.l.hour; minute := f.l.minute; second := f.l.second
		END
	END GetModDate;

	PROCEDURE Init;
		VAR loc: Files.Locator; appName: Files.Name; pDir: PackedDirectory; sDir: Directory;
	BEGIN
		loc := Files.dir.This(""); Files.dir.GetFileName(HostFiles.appName$, "EXE", appName);
		exefile := Files.dir.Old(loc, appName, Files.shared);
		IF exefile # NIL THEN
			curloc := loc(HostFiles.Locator);
			ReadResourceTable;
			NEW(pDir); packedDir := pDir; NEW(sDir); stdDir := sDir;
			SetFilesDir
		ELSE
			DebugPrint("HostPackedFiles: Could not open " + appName)
		END
	END Init;

BEGIN
	Init
CLOSE
	RestoreFilesDir;
	IF exefile # NIL THEN exefile.Close END
END HostPackedFiles.

 HostPackedFiles.SetFilesDir
 HostPackedFiles.RestoreFilesDir
 HostPackedFiles.PackStat
 HostPackedFiles.IsInstalled
