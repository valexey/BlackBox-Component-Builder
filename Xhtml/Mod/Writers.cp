MODULE XhtmlWriters;
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

	CONST prettyPrint* = FALSE; preserve* = TRUE;

	TYPE
		Error* = POINTER TO RECORD 
			pos*: INTEGER;
			msg*: ARRAY 80 OF CHAR
		END;

		Writer* = POINTER TO ABSTRACT RECORD END;

	PROCEDURE (wr: Writer) Error* (): Error, NEW, ABSTRACT;
	PROCEDURE (wr: Writer) Ln*, NEW, ABSTRACT;
	PROCEDURE (wr: Writer) Comment* (IN comment: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (wr: Writer) Instruction* (IN piTarget, instruction: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (wr: Writer) DocType* (IN rootName, pubidLiteral, sysidLiteral: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (wr: Writer) StartTag* (IN elem: ARRAY OF CHAR; preserve: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (wr: Writer) Attr* (IN name, value: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (wr: Writer) Data* (IN data: ARRAY OF CHAR), NEW, ABSTRACT;
	PROCEDURE (wr: Writer) EndTag*, NEW, ABSTRACT;

END XhtmlWriters.
