MODULE ObxWrappers;
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

	IMPORT Dialog, Stores, Models, Views, Controllers, Properties, StdLog;

	CONST minVersion = 2; maxVersion = 2;

	TYPE
		View = POINTER TO RECORD (Views.View)
			inner: Views.View	(* v # NIL *)
		END;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			Views.ReadView(rd, v.inner)	(* generate Views.Alien if necessary *)
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		Views.WriteView(wr, v.inner)	(* handle Views.Alien if necessary *)
	END Externalize;

	PROCEDURE (v: View) CopyFromModelView (source: Views.View; model: Models.Model);
	BEGIN
		WITH source: View DO
			IF model = NIL THEN
				v.inner := Views.CopyOf(source.inner, Views.deep)
			ELSE
				v.inner := Views.CopyWithNewModel(source.inner, model)
			END
		END
	END CopyFromModelView;

	PROCEDURE (v: View) ThisModel (): Models.Model;
	BEGIN
		RETURN v.inner.ThisModel()
	END ThisModel;

	PROCEDURE (v: View) InitContext (context: Models.Context);
	BEGIN
		v.InitContext^(context);
		v.inner.InitContext(context)	(* wrapper and wrapped view share the same context *)
	END InitContext;

	PROCEDURE (v: View) Neutralize;
	BEGIN
		v.inner.Neutralize
	END Neutralize;

	(* GetNewFrame: wrapper uses standard frame *)
	(* GetBackground: wrapper has no intrinsic background color *)

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Views.InstallFrame(f, v.inner, 0, 0, 0, TRUE)	(* create and install wrapped view's frame *)
	END Restore;

	(* RestoreMarks: wrapper has no intrinsic marks, wrapped view's RestoreMarks is called by framework *)
	(* HandleModelMsg: framework performs message propagation *)
	(* HandleViewMsg: framework performs message propagation *)

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
	BEGIN
		(* here comes the behavior which is specific to this wrapper -
			it modifies the wrapped view's behavior *)
		WITH msg: Controllers.EditMsg DO
			(* sample special behavior: copy typing into log *)
			IF msg.op = Controllers.pasteChar THEN StdLog.Char(msg.char); StdLog.Ln END	
		ELSE
		END;
		focus := v.inner	(* forward all controller messages to wrapped view *)
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		Views.HandlePropMsg(v.inner, msg)	(* forward all property messages to wrapped view *)
	END HandlePropMsg;


	PROCEDURE Wrap*;
		VAR poll: Controllers.PollOpsMsg; w: View; replace: Controllers.ReplaceViewMsg;
	BEGIN
		Controllers.PollOps(poll);
		IF (poll.singleton # NIL) & ~(poll.singleton IS View) THEN
			NEW(w); w.inner := poll.singleton; Stores.Join(w, w.inner);
			replace.old := poll.singleton; replace.new := w;
			Controllers.Forward(replace)
		ELSE Dialog.Beep
		END
	END Wrap;

	PROCEDURE Unwrap*;
		VAR poll: Controllers.PollOpsMsg; replace: Controllers.ReplaceViewMsg;
	BEGIN
		Controllers.PollOps(poll);
		IF (poll.singleton # NIL) & (poll.singleton IS View) THEN
			replace.old := poll.singleton; replace.new := poll.singleton(View).inner;
			Controllers.Forward(replace)
		ELSE Dialog.Beep
		END
	END Unwrap;

END ObxWrappers.
