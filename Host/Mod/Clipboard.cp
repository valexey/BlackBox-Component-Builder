MODULE HostClipboard;
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
		COM, WinApi, WinOle,
		Services, Stores, Views, Documents, OleData, HostWindows;
		
	
	TYPE
		EmptyView = POINTER TO RECORD (Views.View) END;
		
	
	VAR
		cloneAttributes*: BOOLEAN;	(* set by HostCmds, used by Importers *)
		isText*: BOOLEAN;	(* set by HostCmds, used by GetClipView *)

		doc: Documents.Document;	(* contents of clipboard window *)
		isSingleton: BOOLEAN;	(* contents of clipboard is singleton view *)
		width, height: INTEGER;	(* size of clipboard content *)
		empty: Views.View;	(* contents of empty clipboard *)
		cbdata: WinOle.IDataObject;	(* data object registred via OLE *)
		
	
	PROCEDURE Register* (v: Views.View; w, h: INTEGER; isSingle: BOOLEAN);
		VAR res: COM.RESULT;
	BEGIN
		doc.SetView(v, w, h);
		width := w; height := h;
		isSingleton := isSingle;
		cbdata := OleData.ViewData(v, w, h, isSingle);
		res := WinOle.OleSetClipboard(cbdata)
	END Register;
	
	PROCEDURE GetClipView* (
		type: Stores.TypeName; VAR v: Views.View; VAR w, h: INTEGER; VAR isSingle: BOOLEAN
	);
		VAR dobj: WinOle.IDataObject; res: COM.RESULT;
	BEGIN
		v := NIL;
		IF WinOle.OleIsCurrentClipboard(cbdata) = WinApi.S_OK THEN
			v := Views.CopyOf(doc.ThisView(), Views.deep);
			w := width; h := height; isSingle := isSingleton;
			IF (type # "") & ~Services.Is(v, type) THEN v := NIL END
		ELSE
			res := WinOle.OleGetClipboard(dobj);
			IF res >= 0 THEN
				IF isText THEN
					OleData.GetTextDataView(dobj, v, w, h, isSingle)
				ELSE
					OleData.GetDataView(dobj, type, v, w, h, isSingle)
				END;
				IF v # NIL THEN
					doc.SetView(Views.CopyOf(v, Views.deep), w, h);
					width := w; height := h;
					isSingleton := isSingle
				END
			END
		END
	END GetClipView;
	
	PROCEDURE ConvertibleTo* (type: Stores.TypeName): BOOLEAN;
		VAR dobj: WinOle.IDataObject; res: COM.RESULT;
	BEGIN
		IF WinOle.OleIsCurrentClipboard(cbdata) = WinApi.S_OK THEN
			RETURN (type = "") OR Services.Is(doc.ThisView(), type)
		ELSE
			res := WinOle.OleGetClipboard(dobj);
			IF res >= 0 THEN
				RETURN OleData.DataConvTo(dobj, type)
			END
		END;
		RETURN FALSE
	END ConvertibleTo;
	
	PROCEDURE Flush*;
		VAR res: COM.RESULT;
	BEGIN
		IF WinOle.OleIsCurrentClipboard(cbdata) = WinApi.S_OK THEN
			res := WinOle.OleFlushClipboard()
		END
	END Flush;
	
	PROCEDURE Dump*;
		VAR res: COM.RESULT; dobj: WinOle.IDataObject;
	BEGIN
		res := WinOle.OleGetClipboard(dobj);
		OleData.DumpData(dobj)
	END Dump;
	
	
	PROCEDURE (v: EmptyView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	END Restore;
	

	PROCEDURE Init;
		VAR e: EmptyView;
	BEGIN
		NEW(e); empty := e;
		(* init document to be used to display the clipboard *)
		doc := Documents.dir.New(empty, Views.undefined, Views.undefined);
		HostWindows.OpenClipboard(doc)
	END Init;
	
BEGIN
	Init
END HostClipboard.

	
HostClipboard.Flush

HostClipboard.Dump