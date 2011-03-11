MODULE HostCmds;
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
		Kernel, Ports, Printers, Files,
		Stores, Views, Controllers, Dialog, Printing,
		Converters, Sequencers, Documents, Windows,
		StdDialog, StdCmds,
		HostFiles, HostWindows, HostDialog, HostClipboard;

	CONST
		(* hints *)
		impAll = 0;	(* can import all file types *)
		expDoc = 1;	(* imported view should be stored as document *)

	VAR quit*: BOOLEAN;


	(** File menu **)

	PROCEDURE Open*;
	(** open an existing document or view **)
		VAR loc: Files.Locator; name: Files.Name; v: Views.View;
			s: Stores.Store; done: BOOLEAN; w: Windows.Window; conv: Converters.Converter;
	BEGIN
		HostDialog.GetIntSpec(loc, name, conv);
		IF loc # NIL THEN
			w := Windows.dir.First();
			WHILE (w # NIL) & ((w.loc = NIL) OR (w.name = "") OR (w.loc.res = 77) OR
											~Files.dir.SameFile(loc, name, w.loc, w.name) OR (w.conv # conv)) DO
				w := Windows.dir.Next(w)
			END;
			IF w # NIL THEN s := w.doc
			ELSE
				Converters.Import(loc, name, conv, s);
				IF s # NIL THEN StdDialog.RecalcView(s(Views.View)) END
			END;
			IF s # NIL THEN
				v := s(Views.View);
				IF (conv # NIL) & (expDoc IN conv.opts) THEN conv := NIL END;
				Views.Open(v, loc, name, conv)
			ELSE Dialog.ShowParamMsg("#System:FailedToOpen", name, "", "")
			END
		END
	END Open;
	
	PROCEDURE OpenCopyOf*;
		VAR loc: Files.Locator; name: Files.Name; conv: Converters.Converter; v: Views.View;
	BEGIN
		v := Views.Old(TRUE, loc, name, conv);
		IF v # NIL THEN
			IF v.context # NIL THEN
				v := Views.CopyOf(v.context(Documents.Context).ThisDoc(), Views.deep);
				Stores.InitDomain(v)
			ELSE v := Views.CopyOf(v, Views.deep)
			END;
			Views.Open(v, NIL, "", conv)
		END
	END OpenCopyOf;

	PROCEDURE SaveWindow (w: Windows.Window; rename: BOOLEAN);
		VAR title: Views.Title; loc: Files.Locator; name: Files.Name; v: Views.View;
			conv: Converters.Converter; stat: BOOLEAN; i: INTEGER;
	BEGIN
		IF (w # NIL) & (rename OR ~(Windows.neverDirty IN w.flags)) THEN
			v := w.doc.OriginalView();
			loc := w.loc; name := w.name$; conv := w.conv;
			IF name = "" THEN Dialog.MapString("#System:untitled", name) END;
			IF (loc = NIL) OR (loc.res = 77) OR (conv # NIL) & (conv.exp = "") THEN rename := TRUE END;
			IF rename THEN HostDialog.GetExtSpec(v, loc, name, conv) END;
			IF loc # NIL THEN
				Dialog.ShowStatus("#Host:Saving");
				Converters.Export(loc, name, conv, v);
				IF loc.res = 0 THEN
					IF w.seq.Dirty() THEN
						w.seq.BeginModification(Sequencers.notUndoable, NIL);
						w.seq.EndModification(Sequencers.notUndoable, NIL);	(* clear sequencer *)
						w.seq.SetDirty(FALSE)
					END;
					IF rename THEN
						i := 0;
						WHILE (i < LEN(title) - 1) & (name[i] # 0X) DO title[i] := name[i]; INC(i) END;
						title[i] := 0X;
						w.SetTitle(title); w.SetSpec(loc, name, conv)
					END;
					Dialog.ShowStatus("#Host:Saved")
				ELSE
					Dialog.ShowStatus("#Host:Failed")
				END
			END;
			IF ~quit THEN Kernel.Cleanup END
		END
	END SaveWindow;

	PROCEDURE Save*;
	(** save document shown in target window under old name **)
		VAR w: Windows.Window;
	BEGIN
		w := Windows.dir.Focus(Controllers.targetPath);
		SaveWindow(w, FALSE)
	END Save;

	PROCEDURE SaveAs*;
	(** save document shown in target window under new name **)
		VAR w: Windows.Window;
	BEGIN
		w := Windows.dir.Focus(Controllers.targetPath);
		SaveWindow(w, TRUE)
	END SaveAs;

	PROCEDURE SaveCopyAs*;
	(** save copy of document shown in target window under new name **)
		VAR w: Windows.Window; loc: Files.Locator; name: Files.Name; v: Views.View;
			conv: Converters.Converter;
	BEGIN
		w := Windows.dir.Focus(Controllers.targetPath);
		IF (w # NIL) THEN
			v := w.doc.OriginalView();
			loc := w.loc; name := w.name$; conv := w.conv;
			IF name = "" THEN Dialog.MapString("#System:untitled", name) END;
			HostDialog.GetExtSpec(v, loc, name, conv);
			IF loc # NIL THEN
				Dialog.ShowStatus("#Host:Saving");
				Converters.Export(loc, name, conv, v);
				IF loc.res = 0 THEN Dialog.ShowStatus("#Host:Saved")
				ELSE Dialog.ShowStatus("#Host:Failed")
				END
			END;
			IF ~quit THEN Kernel.Cleanup END
		END
	END SaveCopyAs;

	PROCEDURE CloseWindow (w: Windows.Window);
		VAR res: INTEGER; msg: Sequencers.CloseMsg;
	BEGIN
		IF w # NIL THEN
			IF ~w.sub THEN
				msg.sticky := FALSE; w.seq.Notify(msg);
				IF ~msg.sticky THEN
					IF w.seq.Dirty() & ~(Windows.neverDirty IN w.flags) THEN
						HostDialog.CloseDialog(w, quit, res);
						IF res = HostDialog.save THEN
							SaveWindow(w, FALSE);	(* if saving is canceled, document remains dirty *)
							IF w.seq.Dirty() THEN quit := FALSE
							ELSE Windows.dir.Close(w)
							END
						ELSIF res = HostDialog.cancel THEN quit := FALSE
						ELSE Windows.dir.Close(w)
						END
					ELSE Windows.dir.Close(w)
					END
				ELSE quit := FALSE
				END
			ELSE Windows.dir.Close(w)
			END;
			IF ~quit THEN Kernel.Cleanup END
		END
	END CloseWindow;
	
	PROCEDURE Close*;
	(** close top window **)
	BEGIN
		CloseWindow(Windows.dir.First())
	END Close;
(*
	PROCEDURE PageSetup*;
	(** ask user for page size, margins and decoration of the front window's document **)
		VAR win: Windows.Window; d: Documents.Document;
			w, h, l, t, r, b,  dl, dt, dr, db: LONGINT; decorate: BOOLEAN; s: Stores.Operation;
	BEGIN
		win := Windows.dir.Focus(Controllers.targetPath);
		IF win # NIL THEN
			d := win.doc;
			d.PollPage(w, h, l, t, r, b, decorate);
			HostDialog.PageSetup(w, h, l, t, r, b, decorate);
			IF w > 0 THEN
				IF Windows.noResize IN win.flags THEN
					d.PollRect(dl, dt, dr, db);
					r := l + (dr - dl); b := t + (db - dt)
				END;
				d.SetPage(w, h, l, t, r, b, decorate)
			END
		END
	END PageSetup;
*)
	PROCEDURE HasSel (w: Windows.Window): BOOLEAN;
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		ops.type := ""; ops.singleton := NIL; ops.selectable := FALSE; ops.valid := {};
		w.ForwardCtrlMsg(ops);
		RETURN ops.singleton # NIL
	END HasSel;

	PROCEDURE PrintSel (w: Windows.Window; from, to, copies: INTEGER);
		VAR wt, title: Views.Title; i: INTEGER; edit: Controllers.EditMsg;
	BEGIN
		edit.op := Controllers.copy; edit.view := NIL;
		edit.clipboard := FALSE; w.ForwardCtrlMsg(edit);
		ASSERT(edit.view # NIL, 100);
		w.GetTitle(wt); title := "[";
		i := 1; WHILE (wt[i - 1] # 0X) & (i < LEN(title)) DO title[i] := wt[i - 1]; INC(i) END;
		IF i >= LEN(title) - 1 THEN i := LEN(title) - 2 END;
		title[i] := "]"; title[i + 1] := 0X;
		Printing.PrintView(edit.view, (*edit.w, edit.h,*) Printing.NewDefaultPar(title))
	END PrintSel;

	PROCEDURE PrintDoc (w: Windows.Window; from, to, copies: INTEGER);
		VAR pw, ph, l, t, r, b: INTEGER; decorate: BOOLEAN;
			msg: Controllers.ScrollMsg; page: Printing.PageInfo; header, footer: Printing.Banner;
	BEGIN
		w.doc.PollPage(pw, ph, l, t, r, b, decorate);
		page.first := 1; page.from := from; page.to := to; page.alternate := FALSE;
		w.GetTitle(page.title);
		IF decorate THEN
			header.gap := 5 * Ports.mm; header.right := "&d&;&p";
		ELSE
			header.gap := 0; header.right := "";
		END;
		footer.gap := 0; footer.right := "";
		Printing.PrintView(w.doc, Printing.NewPar(page, header, footer, copies));
(*
		msg.focus := FALSE; msg.vertical := TRUE;
		msg.op := Controllers.gotoPos; msg.pos := 0;
		msg.done := FALSE; w.ForwardCtrlMsg(msg);
		Views.UpdateRoot(w.frame, w.frame.l, w.frame.t, w.frame.r, w.frame.b, Views.rebuildFrames)
*)
	END PrintDoc;

	PROCEDURE PrintThis (w: Windows.Window; from, to, copies: INTEGER; selection: BOOLEAN);
	BEGIN
		IF copies > 0 THEN
			IF selection THEN
				PrintSel(w, from, to, copies)
			ELSE
				PrintDoc(w, from, to, copies)
			END
		END
	END PrintThis;
(*
	PROCEDURE PrintSelection*;
	(** print the front window's selection **)
		VAR win: Windows.Window; pr: Printers.Printer;
			from, to, copies: INTEGER; selection, hasSel: BOOLEAN;
	BEGIN
		win := Windows.dir.Focus(Controllers.path);
		IF win # NIL THEN
			hasSel := HasSel(win); selection := hasSel;
			HostDialog.PrintDialog(hasSel, pr, from, to, copies, selection);
			PrintThis(pr, win, from, to, copies, selection)
		END
	END PrintSelection;
*)
	PROCEDURE Print*;
	(** print the front window's document **)
		VAR win: Windows.Window;
			from, to, copies: INTEGER; selection: BOOLEAN;
	BEGIN
		IF Printers.dir.Available() THEN
			win := Windows.dir.Focus(Controllers.targetPath);
			IF win # NIL THEN
				WHILE win.sub DO win := win.link END;
				selection := FALSE;
				HostDialog.PrintDialog(HasSel(win), from, to, copies, selection);
				PrintThis(win, from, to, copies, selection)
			END;
			Kernel.Cleanup
		END
	END Print;

	PROCEDURE Quit*;
	(** stop if all windows can be closed **)
		VAR w: Windows.Window;
	BEGIN
		quit := TRUE;
		w := Windows.dir.First();
		WHILE (w # NIL) & (HostWindows.inPlace IN w.flags) DO w := Windows.dir.Next(w) END;
		WHILE (w # NIL) & quit DO
			CloseWindow(w);
			w := Windows.dir.First();
			WHILE (w # NIL) & (HostWindows.inPlace IN w.flags) DO w := Windows.dir.Next(w) END
		END
	END Quit;

	PROCEDURE SaveAll*;
		VAR w: Windows.Window; res: INTEGER;
	BEGIN
		quit := FALSE;
		w := Windows.dir.First();
		WHILE (w # NIL) & (HostWindows.inPlace IN w.flags) DO w := Windows.dir.Next(w) END;
		res := HostDialog.save;
		WHILE (w # NIL) & (res # HostDialog.cancel) DO
			IF ~w.sub & w.seq.Dirty() & ~(Windows.neverDirty IN w.flags) THEN
				HostDialog.CloseDialog(w, FALSE, res);
				IF res = HostDialog.save THEN
					SaveWindow(w, FALSE)	(* if saving is canceled, document remains dirty *)
				END;
				Kernel.Cleanup
			END;
			w := Windows.dir.Next(w)
		END
	END SaveAll;

	(** Edit menu **)

	PROCEDURE Cut*;
	(** move the focus document's selection into the clipboard **)
		VAR msg: Controllers.EditMsg;
	BEGIN
		msg.op := Controllers.cut;
		msg.clipboard := TRUE;
		msg.view := NIL; msg.w := Views.undefined; msg.h := Views.undefined;
		Controllers.Forward(msg);
		IF msg.view # NIL THEN HostClipboard.Register(msg.view, msg.w, msg.h, msg.isSingle) END
	END Cut;

	PROCEDURE Copy*;
	(** move a copy of the focus document's selection into the clipboard **)
		VAR msg: Controllers.EditMsg;
	BEGIN
		msg.op := Controllers.copy;
		msg.clipboard := TRUE;
		msg.view := NIL; msg.w := Views.undefined; msg.h := Views.undefined;
		Controllers.Forward(msg);
		IF msg.view # NIL THEN HostClipboard.Register(msg.view, msg.w, msg.h, msg.isSingle) END
	END Copy;

	PROCEDURE Paste*;
	(** let focus document insert a copy of the clipboard's contents **)
		VAR ops: Controllers.PollOpsMsg; msg: Controllers.EditMsg;
	BEGIN
		HostClipboard.cloneAttributes := TRUE;
		HostClipboard.isText := TRUE;
		Controllers.PollOps(ops);
		HostClipboard.isText := ops.type = "TextViews.View";
		IF Controllers.paste IN ops.valid THEN
			msg.clipboard := TRUE;
			HostClipboard.GetClipView(ops.pasteType, msg.view, msg.w, msg.h, msg.isSingle);
			IF msg.view # NIL THEN
				msg.op := Controllers.paste; Controllers.Forward(msg)
			END
		END;
		HostClipboard.cloneAttributes := FALSE;
		HostClipboard.isText := TRUE;
	END Paste;

	PROCEDURE PasteObject*;
	(** let focus document insert a copy of the clipboard's contents **)
		VAR ops: Controllers.PollOpsMsg; v: Views.View; w, h: INTEGER; s: BOOLEAN;
	BEGIN
		HostClipboard.cloneAttributes := FALSE;
		Controllers.PollOps(ops);
		IF Controllers.paste IN ops.valid THEN
			HostClipboard.GetClipView(ops.pasteType, v, w, h, s);
			IF v # NIL THEN
				Controllers.PasteView(v, w, h, TRUE)
			END
		END
	END PasteObject;

	PROCEDURE PasteToWindow*;
		VAR v: Views.View; w, h: INTEGER; d: Documents.Document; s: BOOLEAN;
	BEGIN
		HostClipboard.cloneAttributes := FALSE;
		HostClipboard.GetClipView("", v, w, h, s);
		IF v # NIL THEN
			d := Documents.dir.New(v, w, h);
			Views.OpenView(d)
		END
	END PasteToWindow;
	
	
	
	PROCEDURE OpenDoc* (file: ARRAY OF CHAR);
		VAR w: Windows.Window;
	BEGIN
		w := Windows.dir.Focus(FALSE);
		IF (w.loc # NIL) & (w.loc IS HostFiles.Locator) & (w.loc(HostFiles.Locator).path # "") THEN
			StdCmds.OpenDoc(w.loc(HostFiles.Locator).path + "\" + file)
		ELSE
			StdCmds.OpenDoc(file)
		END
	END OpenDoc;
	
	
	(* Guards *)

	PROCEDURE SaveGuard* (VAR par: Dialog.Par);
		VAR w: Windows.Window;
	BEGIN
		w := Windows.dir.Focus(Controllers.targetPath);
		IF (w = NIL) OR (Windows.neverDirty IN w.flags) OR ~w.seq.Dirty() THEN par.disabled := TRUE END
	END SaveGuard;
	
	PROCEDURE PrintGuard* (VAR par: Dialog.Par);
		VAR w: Windows.Window;
	BEGIN
		w := Windows.dir.Focus(Controllers.targetPath);
		IF (w = NIL) OR ~Printers.dir.Available() THEN par.disabled := TRUE END
	END PrintGuard;

	PROCEDURE PrinterGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~Printers.dir.Available() THEN par.disabled := TRUE END
	END PrinterGuard;

	PROCEDURE CutGuard* (VAR par: Dialog.Par);
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		Controllers.PollOps(ops);
		IF ~(Controllers.cut IN ops.valid) THEN par.disabled := TRUE END
	END CutGuard;
	
	PROCEDURE CopyGuard* (VAR par: Dialog.Par);
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		Controllers.PollOps(ops);
		IF ~(Controllers.copy IN ops.valid) THEN par.disabled := TRUE END
	END CopyGuard;

	PROCEDURE PasteGuard* (VAR par: Dialog.Par);
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		Controllers.PollOps(ops);
		IF ~(Controllers.paste IN ops.valid)
			OR ~HostClipboard.ConvertibleTo(ops.pasteType) THEN par.disabled := TRUE END
	END PasteGuard;

	PROCEDURE PasteObjectGuard* (VAR par: Dialog.Par);
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		Controllers.PollOps(ops);
		IF ~(Controllers.paste IN ops.valid)
			OR ~HostClipboard.ConvertibleTo("") THEN par.disabled := TRUE END
	END PasteObjectGuard;
	
	PROCEDURE PasteToWindowGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~HostClipboard.ConvertibleTo("") THEN par.disabled := TRUE END
	END PasteToWindowGuard;

BEGIN
	quit := FALSE
END HostCmds.
