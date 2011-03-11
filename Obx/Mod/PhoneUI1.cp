MODULE ObxPhoneUI1;
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

	IMPORT Dialog, ObxPhoneDB;

	VAR
		phone*: RECORD
			name*, number*: ObxPhoneDB.String
		END;

	PROCEDURE NameNotifier* (op, from, to: INTEGER);
	BEGIN
		ObxPhoneDB.LookupByName(phone.name, phone.number);
		Dialog.Update(phone)
	END NameNotifier;

	PROCEDURE NumberNotifier* (op, from, to: INTEGER);
	BEGIN
		ObxPhoneDB.LookupByNumber(phone.number, phone.name);
		Dialog.Update(phone)
	END NumberNotifier;

END ObxPhoneUI1.
