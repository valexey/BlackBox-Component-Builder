MODULE ObxPhoneUI;
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
			name*, number*: ObxPhoneDB.String;
			lookupByName*: BOOLEAN
		END;

	PROCEDURE Lookup*;
	BEGIN
		IF phone.lookupByName THEN
			ObxPhoneDB.LookupByName(phone.name, phone.number);
			IF phone.number = "" THEN phone.number := "not found" END
		ELSE
			ObxPhoneDB.LookupByNumber(phone.number, phone.name);
			IF phone.name = "" THEN phone.name := "not found" END
		END;
		Dialog.Update(phone)
	END Lookup;

	PROCEDURE LookupGuard* (VAR par: Dialog.Par);
	BEGIN	(* disable if input string is empty *)
		par.disabled := phone.lookupByName & (phone.name = "") OR
							~phone.lookupByName & (phone.number = "")
	END LookupGuard;

	PROCEDURE NameGuard* (VAR par: Dialog.Par);
	BEGIN	(* make read-only if lookup is by number *)
		par.readOnly := ~phone.lookupByName
	END NameGuard;

	PROCEDURE NumberGuard* (VAR par: Dialog.Par);
	BEGIN	(* make read-only if lookup is by name *)
		par.readOnly := phone.lookupByName
	END NumberGuard;

END ObxPhoneUI.
