MODULE OleViews;
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

	IMPORT COM, WinApi, WinOle, WinOleAut, CtlT, Properties, Views, Containers, Windows, OleServer, OleClient;
	
	(* client wrappers *)
	
	PROCEDURE NewObjectView* (name: ARRAY OF CHAR): Views.View;
		VAR clsid: COM.GUID; res: COM.RESULT;
	BEGIN
		IF name[0] = "{" THEN res := WinOle.CLSIDFromString(name, clsid)
		ELSE res := WinOle.CLSIDFromProgID(name, clsid)
		END;
		IF res = WinApi.S_OK THEN RETURN OleClient.NewView(clsid)
		ELSE RETURN NIL
		END
	END NewObjectView;
(*	
	PROCEDURE NewObjectViewFrom* (obj: CtlT.Object): Views.View;
		VAR disp: WinOleAut.IDispatch;
	BEGIN
		disp := CtlT.Disp(obj);
		RETURN OleClient.NewViewFrom(disp)
	END NewObjectViewFrom;
*)	
	PROCEDURE NewObjectViewFromClipboard* (): Views.View;
	BEGIN
		RETURN OleClient.NewViewFromCB()
	END NewObjectViewFromClipboard;
	
	PROCEDURE IsObjectView* (v: Views.View): BOOLEAN;
		VAR unk: COM.IUnknown;
	BEGIN
		unk := OleClient.IUnknown(v);
		RETURN unk # NIL
	END IsObjectView;
	
	PROCEDURE IsAutoView* (v: Views.View): BOOLEAN;
		VAR unk: COM.IUnknown; disp: WinOleAut.IDispatch; res: COM.RESULT;
	BEGIN
		unk := OleClient.IUnknown(v);
		IF unk # NIL THEN
			res := unk.QueryInterface(COM.ID(disp), disp);
			RETURN res = WinApi.S_OK
		ELSE RETURN FALSE
		END
	END IsAutoView;
	
	PROCEDURE OleObject* (v: Views.View): CtlT.Interface;
		VAR unk: COM.IUnknown;
	BEGIN
		unk := OleClient.IUnknown(v);
		IF unk # NIL THEN RETURN CtlT.Intfce(unk)
		ELSE RETURN NIL
		END
	END OleObject;
	
	PROCEDURE AutoObject* (v: Views.View): CtlT.Object;
		VAR unk: COM.IUnknown; disp: WinOleAut.IDispatch; res: COM.RESULT;
	BEGIN
		unk := OleClient.IUnknown(v);
		IF unk # NIL THEN
			res := unk.QueryInterface(COM.ID(disp), disp);
			IF res = WinApi.S_OK THEN RETURN CtlT.Obj(disp) END
		END;
		RETURN NIL
	END AutoObject;
	
	PROCEDURE Deposit* (name: ARRAY OF CHAR);
		VAR v: Views.View;
	BEGIN
		v := NewObjectView(name);
		IF v # NIL THEN Views.Deposit(v) END
	END Deposit;
(*	
	PROCEDURE Activate*;
		VAR v, dv: Views.View; msg: Properties.DoVerbMsg; w: Windows.Window; f: Views.Frame;
	BEGIN
		v := Containers.FocusSingleton();
		IF v # NIL THEN
			w := Windows.dir.Focus(TRUE);
			IF w # NIL THEN
				dv := w.doc.ThisView();
				f := Views.ThisFrame(w.frame, dv);
				msg.frame := Views.ThisFrame(f, v);
				msg.verb := WinOle.OLEIVERB_SHOW;
				Views.HandlePropMsg(v, msg)
			END
		END
	END Activate;
*)	
	PROCEDURE Connect* (sink: CtlT.OutObject; source: Views.View);
		VAR iid: COM.GUID; disp: WinOleAut.IDispatch;
	BEGIN
		sink.GetIID(iid); disp := CtlT.Disp(sink);
		OleClient.Connect(source, iid, disp)
	END Connect;
	
(*	
	(* server wrappers *)
	
	PROCEDURE NewViewObject* (v: Views.View): CtlT.Interface;
	BEGIN
		ASSERT(v.context = NIL, 20);
		
	END NewViewObject;
	
	PROCEDURE IsViewObject* (i: CtlT.Interface): BOOLEAN;
	BEGIN
	
	END IsViewObject;
	
	PROCEDURE View* (i: CtlT.Interface): Views.View;
	BEGIN
	
	END View;
*)	


END OleViews.

