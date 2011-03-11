MODULE ObxPhoneDB;
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

	CONST
		maxLen = 32;	(* maximum length of name/number strings *)
		maxEntries = 5;	(* maximum number of entries in the database *)

	TYPE
		String* = ARRAY maxLen OF CHAR;

		Entry = RECORD
			name, number: String
		END;

	VAR db: ARRAY maxEntries OF Entry;

	PROCEDURE LookupByIndex* (index: INTEGER; OUT name, number: String);
	BEGIN	(* given an index, return the corresponding <name, number> pair *)
		ASSERT(index >= 0);
		IF index < maxEntries THEN
			name := db[index].name; number := db[index].number
		ELSE
			name := ""; number := ""
		END
	END LookupByIndex;

	PROCEDURE LookupByName* (name: String; OUT number: String);
		VAR i: INTEGER;
	BEGIN	(* given a name, find the corresponding phone number *)
		i := 0; WHILE (i # maxEntries) & (db[i].name # name) DO INC(i) END;
		IF i # maxEntries THEN	(* name found in db[i] *)
			number := db[i].number
		ELSE	(* name not found in db[0..maxEntries-1] *)
			number := ""
		END
	END LookupByName;

	PROCEDURE LookupByNumber* (number: String; OUT name: String);
		VAR i: INTEGER;
	BEGIN	(* given a phone number, find the corresponding name *)
		i := 0; WHILE (i # maxEntries) & (db[i].number # number) DO INC(i) END;
		IF i # maxEntries THEN	(* number found in db[i] *)
			name := db[i].name
		ELSE	(* number not found in db[0..maxEntries-1] *)
			name := ""
		END
	END LookupByNumber;

BEGIN	(* initialization of database contents *)
	db[0].name := "Daffy Duck"; db[0].number := "310-555-1212";
	db[1].name := "Wile E. Coyote"; db[1].number := "408-555-1212";
	db[2].name := "Scrooge McDuck"; db[2].number := "206-555-1212";
	db[3].name := "Huey Lewis"; db[3].number := "415-555-1212";
	db[4].name := "Thomas Dewey"; db[4].number := "617-555-1212"
END ObxPhoneDB.
