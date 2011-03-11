MODULE Init;
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

	IMPORT Kernel, Dialog, Converters, HostMenus;

	PROCEDURE Init;
		VAR res: INTEGER; m: Kernel.Module;
	BEGIN
		HostMenus.OpenApp;
		m := Kernel.ThisMod("DevDebug");
		IF m = NIL THEN Kernel.LoadMod("StdDebug") END;
		Converters.Register("Documents.ImportDocument", "Documents.ExportDocument", "", "odc", {});
		Dialog.Call("StdMenuTool.UpdateAllMenus", "", res);
		Kernel.LoadMod("OleServer");
		Dialog.Call("Config.Setup", "", res);
		HostMenus.Run
	END Init;

BEGIN
	Init
END Init.
