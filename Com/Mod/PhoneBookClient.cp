MODULE ComPhoneBookClient;
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

	IMPORT COM, WinOle, WinApi, ComTools, StdLog, ComPhoneBook, Dialog;
	
	VAR
		para*: RECORD
			name*: Dialog.Combo;
			number*: ARRAY 32 OF CHAR
		END;
	
	PROCEDURE GetNumber*;
		VAR phoneBook: ComPhoneBook.ILookup; res: INTEGER; str: WinApi.PtrWSTR;
	BEGIN
		res := WinOle.CoCreateInstance(ComPhoneBook.CLSID, NIL, WinOle.CLSCTX_SERVER,
															COM.ID(phoneBook), phoneBook);
		IF res = WinApi.S_OK THEN
			res := phoneBook.LookupByName(para.name.item, str);
			IF res = WinApi.S_OK THEN
				para.number := str$; ComTools.FreeString(str)
			ELSE StdLog.String("GetNumber failed, error = "); StdLog.Int(res); StdLog.Ln;
				para.number := ""
			END;
			Dialog.Update(para)
		ELSE StdLog.String("Instance creation failed, error = "); StdLog.Int(res); StdLog.Ln
		END;
	END GetNumber;
	
BEGIN
	para.name.SetLen(5);
	para.name.SetItem(0, "Daffy Duck");
	para.name.SetItem(1, "Wile E. Coyote");
	para.name.SetItem(2, "Scrogge McDuck");
	para.name.SetItem(3, "Huey Lewis");
	para.name.SetItem(4, "Thomas Dewey");
END ComPhoneBookClient.


ComPhoneBook.Register
ComPhoneBook.Unregister
"StdCmds.OpenToolDialog('Com/Rsrc/PhoneBook', 'PhoneBook')"
