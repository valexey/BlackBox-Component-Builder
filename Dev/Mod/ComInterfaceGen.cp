MODULE DevComInterfaceGen;
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

	IMPORT COM, WinApi, WinOle, WinOleAut, 
		Files, HostFiles, Strings, Dialog, StdDialog, Views, TextModels, TextViews, DevTypeLibs; 

	TYPE
		Entry = POINTER TO RECORD	(* registry typelib entry *)
			next: Entry;
			index: INTEGER;
			guid: COM.GUID;
			major, minor: SHORTINT;
			lcid: WinApi.LCID;
			title: ARRAY 256 OF CHAR;
			file: ARRAY 256 OF CHAR
		END;

	VAR
		dialog*: RECORD
			library*: Dialog.List;
			fileName*: ARRAY 256 OF CHAR;
			modName*: ARRAY 64 OF CHAR;
			list, current: Entry
		END;

	PROCEDURE GetName (tlib: WinOleAut.ITypeLib; VAR name: ARRAY OF CHAR);
		VAR res: INTEGER; s: WinOle.BSTR;
	BEGIN
		res := tlib.GetDocumentation(-1, s, NIL, NIL, NIL);
		name := s$;
		IF (name[0] >= "a") & (name[0] <= "z") THEN name[0] := CAP(name[0]) END;
		name := "Ctl" + name;
		WinOleAut.SysFreeString(s)
	END GetName;

	PROCEDURE GenAutomationInterface*;
		VAR fn: Files.Name; loc: Files.Locator; t: TextModels.Model;
	BEGIN
		t := DevTypeLibs.AutomationInterface(dialog.fileName$, dialog.modName$);
		StdDialog.GetSubLoc(dialog.modName, "Mod", loc, fn); loc.res := 77;
		Views.Open(TextViews.dir.New(t), loc, fn, NIL)		
	END GenAutomationInterface;
	
	PROCEDURE GenCustomInterface*;
		VAR fn: Files.Name; loc: Files.Locator; t: TextModels.Model;
	BEGIN
		t := DevTypeLibs.CustomInterface(dialog.fileName$, dialog.modName$);
		StdDialog.GetSubLoc(dialog.modName, "Mod", loc, fn); loc.res := 77;
		Views.Open(TextViews.dir.New(t), loc, fn, NIL)		
	END GenCustomInterface;	
	
	PROCEDURE Browse*;
		VAR loc: Files.Locator; name: Files.Name;
			res: INTEGER; tlib: WinOleAut.ITypeLib; n: ARRAY 256 OF CHAR;	
	BEGIN
		Dialog.GetIntSpec("*", loc, name);
		IF loc # NIL THEN
			dialog.fileName := loc(HostFiles.Locator).path$ + "\" + name$;
			res := WinOleAut.LoadTypeLib(dialog.fileName, tlib);
			IF res >= 0 THEN
				GetName(tlib, n);
				dialog.modName := n$
			ELSE
				dialog.modName := ""
			END;			
			dialog.library.index := 0;
			Dialog.Update(dialog)
		END	
	END Browse;
	
	PROCEDURE TextFieldNotifier* (op, from, to: INTEGER);
		VAR res: INTEGER; tlib: WinOleAut.ITypeLib; n: ARRAY 256 OF CHAR;
	BEGIN
		res := WinOleAut.LoadTypeLib(dialog.fileName, tlib);
		IF res >= 0 THEN
			GetName(tlib, n);
			dialog.modName := n$
		ELSE
			dialog.modName := ""
		END;
		dialog.library.index := 0;
		Dialog.Update(dialog)		
	END TextFieldNotifier;

	PROCEDURE ListBoxNotifier* (op, from, to: INTEGER);
		VAR name: ARRAY 260 OF CHAR; res: INTEGER; e: Entry;
			tlib: WinOleAut.ITypeLib; n: ARRAY 256 OF CHAR;
	BEGIN
		IF op = Dialog.changed THEN
			IF dialog.library.index # dialog.current.index THEN
				e := dialog.list; WHILE e.index # dialog.library.index DO e := e.next END;
				dialog.current := e; dialog.fileName := e.file$
			END;
			res := WinOleAut.LoadTypeLib(dialog.fileName, tlib);
			IF res >= 0 THEN
				GetName(tlib, n);
				dialog.modName := n$
			ELSE
				dialog.modName := "";
				dialog.library.index := 0							
			END;
			Dialog.Update(dialog)
		END
	END ListBoxNotifier;

	PROCEDURE InitDialog*;
		VAR tlKey, gKey, vKey, lidKey, fKey: WinApi.HKEY; guid: COM.GUID; e: Entry;
			i, j, k, res, idx, lcid, len: INTEGER; ver: REAL;
			wstr: ARRAY 256 OF CHAR; nstr: ARRAY 16 OF CHAR;
	BEGIN
		NEW(dialog.list); dialog.list.next := NIL;
		dialog.list.index := 0;
		dialog.list.title := " "; dialog.list.file := "";
		dialog.current := dialog.list;
		res := WinApi.RegOpenKeyW(WinApi.HKEY_CLASSES_ROOT, "TypeLib", tlKey);
		idx := 1;
		i := 0; res := WinApi.RegEnumKeyW(tlKey, i, wstr, LEN(wstr));
		WHILE res = 0 DO
			res := WinOle.CLSIDFromString(wstr, guid);
			IF res = 0 THEN
				res := WinApi.RegOpenKeyW(tlKey, wstr, gKey);
				j := 0; res := WinApi.RegEnumKeyW(gKey, j, wstr, LEN(wstr));
				WHILE res = 0 DO
					Strings.StringToReal(wstr, ver, res);
					IF res = 0 THEN
						res := WinApi.RegOpenKeyW(gKey, wstr, vKey);
						k := 0; res := WinApi.RegEnumKeyW(vKey, k, wstr, LEN(wstr));
						WHILE res  = 0 DO
							Strings.StringToInt(wstr, lcid, res);
							IF res = 0 THEN
								res := WinApi.RegOpenKeyW(vKey, wstr, lidKey);
								res := WinApi.RegOpenKeyW(lidKey, "Win32", fKey);
								IF res # 0 THEN res := WinApi.RegOpenKeyW(lidKey, "Win16", fKey) END;
								IF res = 0 THEN
									NEW(e); e.next := dialog.list; dialog.list := e;
									e.index := idx; INC(idx);
									e.guid := guid;
									e.major := SHORT(SHORT(ENTIER(ver)));
									e.minor := SHORT(SHORT(ENTIER(ver * 100)) MOD 100);
									e.lcid := lcid;
									len := LEN(e.title);
									res := WinApi.RegQueryValueW(vKey, NIL, e.title, len);
									Strings.RealToString(ver, wstr);
									Strings.IntToString(lcid, nstr);
									e.title := e.title + " (" + wstr + ", " + nstr + ")";
									len := LEN(e.file);
									res := WinApi.RegQueryValueW(fKey, NIL, e.file, len)
								END
							END;
							INC(k); res := WinApi.RegEnumKeyW(vKey, k, wstr, LEN(wstr))
						END
					END;
					INC(j); res := WinApi.RegEnumKeyW(gKey, j, wstr, LEN(wstr))
				END
			END;
			INC(i); res := WinApi.RegEnumKeyW(tlKey, i, wstr, LEN(wstr))
		END;
		dialog.library.SetLen(idx); e := dialog.list;
		WHILE e # NIL DO dialog.library.SetItem(e.index, e.title); e := e.next END;
		IF dialog.list.next # NIL THEN dialog.library.index := 1 ELSE dialog.library.index := 0 END;
		ListBoxNotifier(Dialog.changed, 0, 0)
	END InitDialog;
		
END DevComInterfaceGen.


"DevComInterfaceGen.InitDialog; StdCmds.OpenToolDialog('Dev/Rsrc/ComInterfaceGen', 'Generate Automation Interface')"