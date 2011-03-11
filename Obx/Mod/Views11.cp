MODULE ObxViews11;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	purpose	= "Same as ObxViews10, but the view's string can be stored and copied"
	changes	= ""
	issues	= ""

**)

	IMPORT Fonts, Ports, Stores, Views, Controllers, Properties;

	CONST d = 20 * Ports.point;

	TYPE
		View = POINTER TO RECORD (Views.View)
			i: INTEGER;					(* position of next free slot in string *)
			s: ARRAY 256 OF CHAR		(* string *)
		END;

	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(0, 0, version);
		IF ~rd.cancelled THEN
			rd.ReadInt(v.i); rd.ReadString(v.s)
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(0);
		wr.WriteInt(v.i); wr.WriteString(v.s)
	END Externalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: View DO
			v.i := source.i; v.s := source.s
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawString(d, d, Ports.black, v.s, Fonts.dir.Default())
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (f: Views.Frame;
											VAR msg: Controllers.Message;
											VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.EditMsg DO
			IF msg.op = Controllers.pasteChar THEN	(* accept typing *)
				v.s[v.i] := msg.char; INC(v.i); v.s[v.i] := 0X;	(* append character to string *)
				Views.SetDirty(v);	(* mark view's document as dirty *)
				Views.Update(v, Views.keepFrames)	(* restore v in any frame that displays it *)
			END
		ELSE							(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE (v:View) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.SizePref DO
			IF (msg.w = Views.undefined) OR (msg.h = Views.undefined) THEN
				msg.w := 10 * d; msg.h := 2 * d
			END
		| msg: Properties.FocusPref DO
			msg.setFocus := TRUE
		ELSE
		END
	END HandlePropMsg;

	PROCEDURE Deposit*;
		VAR v: View;
	BEGIN
		NEW(v); v.s := ""; v.i := 0;
		Views.Deposit(v)
	END Deposit;

END ObxViews11.
