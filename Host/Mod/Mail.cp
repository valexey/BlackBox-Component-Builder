MODULE HostMail;
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
		SYSTEM, WinCmc, WinApi,
		Files, Dialog, Converters, Views, Windows,
		TextModels, TextViews, TextControllers, HostFiles;
	
	CONST
		tempFile = "_mail_.odc";
	
	VAR
		tempLoc: HostFiles.Locator;
		checked: BOOLEAN;
		sendDocument: PROCEDURE(adr, subject, note: WinCmc.string; flgs: WinCmc.flags;
						paths, names, delimiter: WinCmc.string; id: WinCmc.ui_id): WinCmc.return_code;

		
	
	PROCEDURE CheckDll*;
		VAR dll: WinApi.HINSTANCE;
	BEGIN
		IF ~checked THEN
			dll := WinApi.LoadLibraryW("MAPI32");
			IF dll # 0 THEN
				SYSTEM.PUT(SYSTEM.ADR(sendDocument), WinApi.GetProcAddress(dll, "cmc_send_documents"));
			END;
			checked := TRUE
		END
	END CheckDll;

	PROCEDURE SendNote*;
		VAR c: TextControllers.Controller; r: TextModels.Reader; res: WinCmc.return_code;
			s: POINTER TO ARRAY OF SHORTCHAR; ch: CHAR; beg, end, i: INTEGER; p: WinCmc.string;
	BEGIN
		CheckDll;
		IF sendDocument # NIL THEN
			c := TextControllers.Focus(); p := NIL;
			IF (c # NIL) & c.HasSelection() THEN
				c.GetSelection(beg, end); 
				r := c.text.NewReader(NIL); r.SetPos(beg);
				NEW(s, (end - beg) * 2 + 1); r.ReadChar(ch); i := 0;
				WHILE ~r.eot & (r.Pos() <= end) DO
					IF (r.view = NIL) & (ch < 100X) THEN
						s[i] := SHORT(ch); INC(i);
						IF ch = 13X THEN s[i] := 10X; INC(i) END
					END;
					r.ReadChar(ch)
				END;
				s[i] := 0X; p := s^;
			END;
			res := sendDocument(NIL, NIL, p,
				WinCmc.ERROR_UI_ALLOWED + WinCmc.LOGON_UI_ALLOWED + WinCmc.SEND_UI_REQUESTED,
				NIL, NIL, ", ", 0)
		END
	END SendNote;
	
	PROCEDURE SendDocument*;
		VAR w: Windows.Window; res: WinCmc.return_code; name: HostFiles.FullName; title: Views.Title;
			ss: ARRAY 260 OF SHORTCHAR; st: ARRAY 64 OF SHORTCHAR;
	BEGIN
		CheckDll;
		IF sendDocument # NIL THEN
			w := Windows.dir.First();
			IF w # NIL THEN
				Converters.Export(tempLoc, tempFile, NIL, w.doc.ThisView());
				name := tempLoc.path + "\" + tempFile;
				IF w.name # "" THEN title := w.name$
				ELSE w.GetTitle(title); title := title + ".odc"
				END;
				ss := SHORT(name$); st := SHORT(title$);
				res := sendDocument(NIL, NIL, NIL,
					WinCmc.ERROR_UI_ALLOWED
					+ WinCmc.LOGON_UI_ALLOWED
					+ WinCmc.SEND_UI_REQUESTED,
					ss, st, ", ", 0);
				Files.dir.Delete(tempLoc, tempFile)
			END
		END
	END SendDocument;
	
	PROCEDURE SendNoteGuard* (VAR par: Dialog.Par);
	BEGIN
		IF checked & (sendDocument = NIL) THEN par.disabled := TRUE END
	END SendNoteGuard;
	
	PROCEDURE SendDocumentGuard* (VAR par: Dialog.Par);
	BEGIN
		IF checked & (sendDocument = NIL) OR (Windows.dir.First() = NIL) THEN par.disabled := TRUE END
	END SendDocumentGuard;
	
	PROCEDURE Init;
		VAR res: INTEGER; path: HostFiles.FullName;
	BEGIN
		res := WinApi.GetTempPathW(LEN(path), path);
		tempLoc := HostFiles.NewLocator(path);
	END Init;
	
BEGIN
	Init
END HostMail.

