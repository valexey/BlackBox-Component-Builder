MODULE ObxFileTree;
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

	IMPORT Files, Converters, Views, Dialog;
		
	TYPE
		Dlg* = RECORD
			path*, currentPath: Dialog.String;
			tree*: Dialog.Tree
		END;
	
	VAR 
		dlg*: Dlg;

	PROCEDURE BuildDirTree (loc: Files.Locator; parent: Dialog.TreeNode);
		VAR l: Files.LocInfo; f: Files.FileInfo; n: Dialog.TreeNode;
	BEGIN
		l := Files.dir.LocList(loc);
		WHILE l # NIL DO (* Recursively add directories *)
			n := dlg.tree.NewChild(parent, 0, l.name$); n.ViewAsFolder(TRUE);
			BuildDirTree(loc.This(l.name$), n);
			l := l.next
		END;
		f := Files.dir.FileList(loc);
		WHILE f # NIL DO n := dlg.tree.NewChild(parent, 0, f.name$); f := f.next END
	END BuildDirTree;

	PROCEDURE ThisLocator (IN path: ARRAY OF CHAR): Files.Locator;
		VAR i, j: INTEGER; ch: CHAR; loc: Files.Locator; dir: Dialog.String;
	BEGIN
		loc := Files.dir.This(""); i := 0; ch := path[0];
		WHILE ch # 0X DO
			j := 0;
			WHILE (ch # 0X) & (ch # "/") DO dir[j] := ch; INC(j); INC(i); ch := path[i] END;
			dir[j] := 0X;
			loc := loc.This(dir);
			IF ch = "/" THEN INC(i); ch := path[i] END
		END;
		RETURN loc
	END ThisLocator;

	PROCEDURE Update*;
		VAR loc: Files.Locator; 
	BEGIN
		IF (dlg.path # "") & (dlg.path[LEN(dlg.path$) - 1] # "/") & (dlg.path[LEN(dlg.path$) - 1] # "\") THEN
			dlg.path := dlg.path + "/"
		END;
		dlg.currentPath := dlg.path$;
		dlg.tree.DeleteAll();
		loc := ThisLocator(dlg.path);
		IF loc # NIL THEN BuildDirTree(loc, NIL); Dialog.UpdateList(dlg.tree) END;
		Dialog.Update(dlg)
	END Update;

	PROCEDURE Open*;
		VAR n: Dialog.TreeNode; name, path, p: Dialog.String;
			loc: Files.Locator; fname: Files.Name; conv: Converters.Converter; v: Views.View;
	BEGIN
		n := dlg.tree.Selected();
		IF n # NIL THEN
			IF ~n.IsFolder() THEN (* a leaf node is selected *)
				n.GetName(name);
				n := dlg.tree.Parent(n); path := "";
				WHILE n # NIL DO n.GetName(p); path := p + "/" + path; n := dlg.tree.Parent(n) END;
				loc := ThisLocator(dlg.currentPath + path);
				IF loc # NIL THEN
					conv := NIL; fname := name$;
					v := Views.Old(Views.dontAsk, loc, fname, conv);
					IF v # NIL THEN Views.Open(v, loc, fname, conv) END
				ELSE Dialog.ShowStatus("file not found")
				END
			ELSIF ~n.IsExpanded() THEN n.SetExpansion(TRUE); Dialog.UpdateList(dlg.tree)
			END
		ELSE Dialog.ShowStatus("no selection")
		END
	END Open;

	PROCEDURE OpenGuard* (VAR par: Dialog.Par);
		VAR n: Dialog.TreeNode;
	BEGIN
		n := dlg.tree.Selected();
		par.disabled := (n = NIL) OR (n.IsFolder() & n.IsExpanded());
		IF (n # NIL) & ~n.IsFolder() THEN par.label := "&Open File" ELSE par.label := "&Open Folder" END
	END OpenGuard;

END ObxFileTree.
