MODULE XhtmlTextTableMarkers;
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

	IMPORT Ports, Stores, Views, Controllers, Properties, TextModels;

	CONST minVersion = 0; maxVersion = 1;

	TYPE
		View = POINTER TO RECORD (Views.View)
			openMark: BOOLEAN
		END;


	(* View *)

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		(* v is not initialized *)
		(* v.Domain() = NIL *)
		IF ~rd.cancelled THEN
			rd.ReadVersion(minVersion, maxVersion, version);
			IF ~rd.cancelled THEN
				rd.ReadBool(v.openMark)
			END
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		(* v is initialized *)
		wr.WriteVersion(maxVersion);
		wr.WriteBool(v.openMark)
	END Externalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		(* v is not initialized *)
		(* v.Domain() = NIL *)
		(* source # NIL *)
		(* source is initialized *)
		(* TYP(v) = TYP(source) *)
		WITH source: View DO
			v.openMark := source.openMark
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR w, h, s: INTEGER;
	BEGIN
		(* f # NIL *)
		IF ~Views.IsPrinterFrame(f) THEN
			v.context.GetSize(w, h); w := f.r - f.l;
			s := 2 * f.dot;
			f.DrawRect(0, 0, s, h, Ports.fill, Ports.grey25);	(* left border *)
			f.DrawRect(w - s, 0, w, h, Ports.fill, Ports.grey25);	(* right border *)
			IF v.openMark THEN
				f.DrawRect(s, 0, w - s, s, Ports.fill, Ports.grey25);	(* top border *)
				f.DrawRect(s, s, w - s, h, Ports.fill, Ports.grey12)	(* interior *)
			ELSE
				f.DrawRect(s, h - s, w - s, h, Ports.fill, Ports.grey25);	(* bottom border *)
				f.DrawRect(s, 0, w - s, h - s, Ports.fill, Ports.grey12)	(* interior *)
			END
		END
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
	BEGIN
		(* f # NIL *)
		(* focus = NIL *)
		WITH msg: Controllers.PollOpsMsg DO
			(* specify which editing operations are supported *)
		| msg: Controllers.TrackMsg DO
			(* implement mouse tracking *)
		| msg: Controllers.EditMsg DO
			(* implement editing operations *)
		ELSE	(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		CONST viewHeight = 2 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO
			msg.w := 10000 * Ports.mm; msg.h := viewHeight
		| msg: Properties.ResizePref DO
			msg.fixed := TRUE
		| msg: TextModels.Pref DO
			(*msg.opts := {TextModels.maskChar};
			msg.mask := TextModels.para*)
		ELSE	(* ignore other messages *)
		END
	END HandlePropMsg;


	(** miscellaneous **)

	PROCEDURE IsOpenMark* (v: Views.View): BOOLEAN;
	BEGIN
		RETURN (v # NIL) & (v IS View) & v(View).openMark
	END IsOpenMark;

	PROCEDURE IsCloseMark* (v: Views.View): BOOLEAN;
	BEGIN
		RETURN (v # NIL) & (v IS View) & ~v(View).openMark
	END IsCloseMark;

	PROCEDURE New* (openMark: BOOLEAN): Views.View;
		VAR v: View;
	BEGIN
		NEW(v); v.openMark := openMark; RETURN v
	END New;

	PROCEDURE DepositOpenMark*;
	BEGIN
		Views.Deposit(New(TRUE))
	END DepositOpenMark;

	PROCEDURE DepositCloseMark*;
	BEGIN
		Views.Deposit(New(FALSE))
	END DepositCloseMark;

END XhtmlTextTableMarkers.
