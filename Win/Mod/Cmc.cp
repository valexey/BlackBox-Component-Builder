MODULE WinCmc ["MAPI32"];
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
		NO_TIMEZONE* = MIN(SHORTINT);
		
		EXT_REQUIRED* = {16};
		EXT_OUTPUT* =  {17};
		EXT_LAST_ELEMENT* = {31};
		EXT_RSV_FLAG_MASK* = {16..31};
		EXT_ITEM_FLAG_MASK* = {0..15};
		
		ATT_APP_OWNS_FILE* = {0};
		ATT_LAST_ELEMENT* = {31};

		ATT_OID_BINARY* = "? ? ? ? ? ?";
		ATT_OID_TEXT* = "? ? ? ? ? ?";
		
		TYPE_UNKNOWN* = 0;
		TYPE_INDIVIDUAL* = 1;
		TYPE_GROUP* = 2;

		ROLE_TO* = 0;
		ROLE_CC* = 1;
		ROLE_BCC* = 2;
		ROLE_ORIGINATOR* = 3;
		ROLE_AUTHORIZING_USER* = 4;

		RECIP_IGNORE* = {0};
		RECIP_LIST_TRUNCATED* = {1};
		RECIP_LAST_ELEMENT* = {31};

		MSG_READ* = {0};
		MSG_TEXT_NOTE_AS_FILE* = {1};
		MSG_UNSENT* = {2};
		MSG_LAST_ELEMENT* = {31};

		SUM_READ* = {0};
		SUM_UNSENT* = {1};
		SUM_LAST_ELEMENT* = {31};

		ERROR_UI_ALLOWED* = {24};
		LOGON_UI_ALLOWED* = {25};
		COUNTED_STRING_TYPE* = {26};

		SEND_UI_REQUESTED* = {0};
		FIRST_ATTACH_AS_TEXT_NOTE* = {1};

		ACT_ON_EXTENDED* = 0;
		ACT_ON_DELETE* = 1;

		LIST_UNREAD_ONLY* = {0};
		LIST_MSG_REFS_ONLY* = {1};
		LIST_COUNT_ONLY* = {2};

		LENGTH_UNKNOWN* = 0FFFFFFFFH;

		DO_NOT_MARK_AS_READ* = {0};
		MSG_AND_ATT_HDRS_ONLY* = {1};
		READ_FIRST_UNREAD_MESSAGE* = {2};

		LOOKUP_RESOLVE_PREFIX_SEARCH* = {0};
		LOOKUP_RESOLVE_IDENTITY* = {1};
		LOOKUP_RESOLVE_UI* = {2};
		LOOKUP_DETAILS_UI* = {3};
		LOOKUP_ADDRESSING_UI* = {4};

		LOGOFF_UI_ALLOWED* = {0};

		VERSION* = 100;

		CONFIG_CHARACTER_SET* = 1;
		CONFIG_LINE_TERM* = 2;
		CONFIG_DEFAULT_SERVICE* = 3;
		CONFIG_DEFAULT_USER* = 4;
		CONFIG_REQ_PASSWORD* = 5;
		CONFIG_REQ_SERVICE* = 6;
		CONFIG_REQ_USER* = 7;
		CONFIG_UI_AVAIL* = 8;
		CONFIG_SUP_NOMKMSGREAD* = 9;
		CONFIG_SUP_COUNTED_STR* = 10;
		CONFIG_VER_IMPLEM* = 11;
		CONFIG_VER_SPEC* = 12;

		LINE_TERM_CRLF* = 0;
		LINE_TERM_CR* = 1;
		LINE_TERM_LF* = 2;

		REQUIRED_NO* = 0;
		REQUIRED_YES* = 1;
		REQUIRED_OPT* = 2;

		CHAR_CP437* = "1 2 840 113556 3 2 437";
		CHAR_CP850* = "1 2 840 113556 3 2 850";
		CHAR_CP1252* = "1 2 840 113556 3 2 1252";
		CHAR_ISTRING* = "1 2 840 113556 3 2 0";
		CHAR_UNICODE* = "1 2 840 113556 3 2 1";

		ERROR_DISPLAYED* = 00008000H;
		ERROR_RSV_MASK* = 0000FFFFH;
		ERROR_IMPL_MASK* = 0FFFF0000H;

		SUCCESS* = 0;

		E_AMBIGUOUS_RECIPIENT* = 1;
		E_ATTACHMENT_NOT_FOUND* = 2;
		E_ATTACHMENT_OPEN_FAILURE* = 3;
		E_ATTACHMENT_READ_FAILURE* = 4;
		E_ATTACHMENT_WRITE_FAILURE* = 5;
		E_COUNTED_STRING_UNSUPPORTED* = 6;
		E_DISK_FULL* = 7;
		E_FAILURE* = 8;
		E_INSUFFICIENT_MEMORY* = 9;
		E_INVALID_CONFIGURATION* = 10;
		E_INVALID_ENUM* = 11;
		E_INVALID_FLAG* = 12;
		E_INVALID_MEMORY* = 13;
		E_INVALID_MESSAGE_PARAMETER* = 14;
		E_INVALID_MESSAGE_REFERENCE* = 15;
		E_INVALID_PARAMETER* = 16;
		E_INVALID_SESSION_ID* = 17;
		E_INVALID_UI_ID* = 18;
		E_LOGON_FAILURE* = 19;
		E_MESSAGE_IN_USE* = 20;
		E_NOT_SUPPORTED* = 21;
		E_PASSWORD_REQUIRED* = 22;
		E_RECIPIENT_NOT_FOUND* = 23;
		E_SERVICE_UNAVAILABLE* = 24;
		E_TEXT_TOO_LARGE* = 25;
		E_TOO_MANY_FILES* = 26;
		E_TOO_MANY_RECIPIENTS* = 27;
		E_UNABLE_TO_NOT_MARK_AS_READ* = 28;
		E_UNRECOGNIZED_MESSAGE_TYPE* = 29;
		E_UNSUPPORTED_ACTION* = 30;
		E_UNSUPPORTED_CHARACTER_SET* = 31;
		E_UNSUPPORTED_DATA_EXT* = 32;
		E_UNSUPPORTED_FLAG* = 33;
		E_UNSUPPORTED_FUNCTION_EXT* = 34;
		E_UNSUPPORTED_VERSION* = 35;
		E_USER_CANCEL* = 36;
		E_USER_NOT_LOGGED_ON* = 37;
	
	TYPE
		sint8* = BYTE;
		sint16* = SHORTINT;
		sint32* = INTEGER;
		uint16* = SHORTINT;
		uint32* = INTEGER;
		buffer* = INTEGER;	(* POINTER TO RECORD [untagged] END *)
		string* = POINTER TO ARRAY [untagged] OF SHORTCHAR;
		boolean* = uint16;
		enum* = sint32;
		return_code* = uint32;
		flags* = SET;
		object_identifier* = string;
		session_id* = uint32;
		ui_id* = uint32;
		
		counted_string* = RECORD [untagged]
			length*: uint32;
			string*: ARRAY 32000 OF SHORTCHAR;
		END;
		
		message_reference* = counted_string;
		
		time* = RECORD [untagged]
			second*, minute*, hour*: sint8;
			day*, month*, year*: sint8;
			isdst*: sint8;
			tmzone*: sint16
		END;
		
		extension* = RECORD [untagged]
			item_code*: uint32;
			item_data*: uint32;
			item_reference*: buffer;
			extension_flags*: flags
		END;
		
		attachment* = RECORD [untagged]
			attach_title*: string;
			attach_type*: object_identifier;
			attach_filename*: string;
			attach_flags*: flags;
			attach_extensions*: POINTER TO extension
		END;
		
		recipient* = RECORD [untagged]
			name*: string;
			name_type*: enum;
			address*: string;
			role*: enum;
			recip_flags*: flags;
			recip_extensions*: POINTER TO extension
		END;
	
		message* = RECORD [untagged]
			message_ref*: POINTER TO message_reference;
			message_type*: string;
			subject*: string;
			time_sent*: time;
			text_note*: string;
			recipients*: POINTER TO recipient;
			attachments*: POINTER TO attachment;
			message_flags*: flags;
			message_extensions*: POINTER TO extension
		END;
		
		message_summary* = RECORD [untagged]
			message_ref*: POINTER TO message_reference;
			message_type*: string;
			subject*: string;
			time_sent*: time;
			byte_length*: uint32;
			originator*: POINTER TO recipient;
			summary_flags*: flags;
			message_summary_extensions*: POINTER TO extension
		END;

	
	PROCEDURE send* ["cmc_send"] (session: session_id; VAR msg: message; flgs: flags; id: ui_id; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END send;*)
	
	PROCEDURE send_documents* ["cmc_send_documents"] (adr, subject, note: string; flgs: flags; paths, names, delimiter: string; id: ui_id): return_code;
	(*END send_documents;*)

	PROCEDURE act_on* ["cmc_act_on"] (session: session_id; VAR ref: message_reference; operation: enum; flgs: flags; id: ui_id; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END act_on;*)
	
	PROCEDURE list* ["cmc_list"] (session: session_id; type: string; flgs: flags; VAR [nil] seed: message_reference; VAR count: uint32; id: ui_id; VAR result: POINTER TO ARRAY [untagged] OF message_summary; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END list;*)

	PROCEDURE read* ["cmc_read"] (session: session_id; VAR ref: message_reference; flgs: flags; VAR msg: POINTER TO message; id: ui_id; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END read;*)

	PROCEDURE look_up* ["cmc_look_up"] (session: session_id; VAR rin: ARRAY [untagged] OF recipient; flgs: flags; id: ui_id; VAR count: uint32; VAR rout: POINTER TO ARRAY [untagged] OF recipient; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END look_up;*)

	PROCEDURE logoff* ["cmc_logoff"] (session: session_id; id: ui_id; flgs: flags; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END logoff;*)

	PROCEDURE logon* ["cmc_logon"] (service, user, password: string; chset: object_identifier; id: ui_id; version: uint16; flgs: flags; VAR session: session_id; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END logon;*)

	PROCEDURE query_configuration* ["cmc_query_configuration"] (session: session_id; item: enum; ref: buffer; VAR [nil] ext: ARRAY [untagged] OF extension): return_code;
	(*END query_configuration;*)
	
	PROCEDURE free* ["cmc_free"] (mem: buffer): return_code;
	(*END free;*)

END WinCmc.
