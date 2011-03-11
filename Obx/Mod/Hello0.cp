MODULE ObxHello0;
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

	IMPORT StdLog;

	PROCEDURE Do*;
	BEGIN
		StdLog.String("Hello World"); StdLog.Ln	(* write string and 0DX into log *)
	END Do;

END ObxHello0.
