MODULE ObxAddress0;
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

	VAR
		adr*: RECORD
			name*:	ARRAY 64 OF CHAR;
			city*:	ARRAY 24 OF CHAR;
			country*:	ARRAY 16 OF CHAR;
			customer*:	INTEGER;
			update*:	BOOLEAN
		END;

	PROCEDURE OpenText*;
	BEGIN
		
	END OpenText;
	
END ObxAddress0.
