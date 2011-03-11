MODULE Out;
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

	IMPORT Views, TextModels, TextMappers, TextViews, StdLog;

	CONST digitspace = 08FX;

	VAR
		buf: TextModels.Model;
		out: TextMappers.Formatter;

	PROCEDURE Open*;
	BEGIN
		StdLog.Open
	END Open;

	PROCEDURE Char* (ch: CHAR);
	BEGIN
		out.WriteChar(ch);
		StdLog.text.Append(buf); Views.RestoreDomain(StdLog.text.Domain())
	END Char;

	PROCEDURE Ln*;
	BEGIN
		out.WriteLn;
		StdLog.text.Append(buf); Views.RestoreDomain(StdLog.text.Domain());
		TextViews.ShowRange(StdLog.text, StdLog.text.Length(), StdLog.text.Length(), TextViews.any)
	END Ln;

	PROCEDURE String* (str: ARRAY OF CHAR);
	BEGIN
		out.WriteString(str);
		StdLog.text.Append(buf); Views.RestoreDomain(StdLog.text.Domain())
	END String;

	PROCEDURE Int* (i: LONGINT; n: INTEGER);
	BEGIN
		out.WriteIntForm(i, 10, n, digitspace, FALSE);
		StdLog.text.Append(buf); Views.RestoreDomain(StdLog.text.Domain())
	END Int;

	PROCEDURE Real* (x: REAL; n: INTEGER);
	BEGIN
		out.WriteRealForm(x, 16, n, 0, digitspace);
		StdLog.text.Append(buf); Views.RestoreDomain(StdLog.text.Domain())
	END Real;

BEGIN
	buf := TextModels.CloneOf(StdLog.buf); out.ConnectTo(buf)
END Out.
