MODULE WinRpc ["RPCRT4.dll"];
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

	IMPORT SYSTEM, WinOle, COM, WinApi;

	CONST (* macros *)
		RPC_C_BINDING_INFINITE_TIMEOUT* = 10;
		RPC_C_BINDING_MIN_TIMEOUT* = 0;
		RPC_C_BINDING_DEFAULT_TIMEOUT* = 5;
		RPC_C_BINDING_MAX_TIMEOUT* = 9;
		RPC_C_CANCEL_INFINITE_TIMEOUT* = -1;
		RPC_C_LISTEN_MAX_CALLS_DEFAULT* = 1234;
		RPC_C_PROTSEQ_MAX_REQS_DEFAULT* = 10;
		RPC_C_STATS_CALLS_IN* = 0;
		RPC_C_STATS_CALLS_OUT* = 1;
		RPC_C_STATS_PKTS_IN* = 2;
		RPC_C_STATS_PKTS_OUT* = 3;
		RPC_C_AUTHN_LEVEL_DEFAULT* = 0;
		RPC_C_AUTHN_LEVEL_NONE* = 1;
		RPC_C_AUTHN_LEVEL_CONNECT* = 2;
		RPC_C_AUTHN_LEVEL_CALL* = 3;
		RPC_C_AUTHN_LEVEL_PKT* = 4;
		RPC_C_AUTHN_LEVEL_PKT_INTEGRITY* = 5;
		RPC_C_AUTHN_LEVEL_PKT_PRIVACY* = 6;
		RPC_C_IMP_LEVEL_ANONYMOUS* = 1;
		RPC_C_IMP_LEVEL_IDENTIFY* = 2;
		RPC_C_IMP_LEVEL_IMPERSONATE* = 3;
		RPC_C_IMP_LEVEL_DELEGATE* = 4;
		RPC_C_PROTECT_LEVEL_DEFAULT* = 0;
		RPC_C_PROTECT_LEVEL_NONE* = 1;
		RPC_C_PROTECT_LEVEL_CONNECT* = 2;
		RPC_C_PROTECT_LEVEL_CALL* = 3;
		RPC_C_PROTECT_LEVEL_PKT* = 4;
		RPC_C_PROTECT_LEVEL_PKT_INTEGRITY* = 5;
		RPC_C_PROTECT_LEVEL_PKT_PRIVACY* = 6;
		RPC_C_AUTHN_NONE* = 0;
		RPC_C_AUTHN_DCE_PRIVATE* = 1;
		RPC_C_AUTHN_DCE_PUBLIC* = 2;
		RPC_C_AUTHN_DEC_PUBLIC* = 4;
		RPC_C_AUTHN_WINNT* = 10;
		RPC_C_AUTHN_DEFAULT* = -1;
		SEC_WINNT_AUTH_IDENTITY_ANSI* = 1;
		SEC_WINNT_AUTH_IDENTITY_UNICODE* = 2;
		RPC_C_AUTHZ_NONE* = 0;
		RPC_C_AUTHZ_NAME* = 1;
		RPC_C_AUTHZ_DCE* = 2;
		DCE_C_ERROR_STRING_LEN* = 256;
		RPC_C_EP_ALL_ELTS* = 0;
		RPC_C_EP_MATCH_BY_IF* = 1;
		RPC_C_EP_MATCH_BY_OBJ* = 2;
		RPC_C_EP_MATCH_BY_BOTH* = 3;
		RPC_C_VERS_ALL* = 1;
		RPC_C_VERS_COMPATIBLE* = 2;
		RPC_C_VERS_EXACT* = 3;
		RPC_C_VERS_MAJOR_ONLY* = 4;
		RPC_C_VERS_UPTO* = 5;
		RPC_C_MGMT_INQ_IF_IDS* = 0;
		RPC_C_MGMT_INQ_PRINC_NAME* = 1;
		RPC_C_MGMT_INQ_STATS* = 2;
		RPC_C_MGMT_IS_SERVER_LISTEN* = 3;
		RPC_C_MGMT_STOP_SERVER_LISTEN* = 4;
		RPC_C_PARM_MAX_PACKET_LENGTH* = 1;
		RPC_C_PARM_BUFFER_LENGTH* = 2;
		RPC_IF_AUTOLISTEN* = 1;
		RPC_IF_OLE* = 2;
		RPC_NCA_FLAGS_DEFAULT* = {};
		RPC_NCA_FLAGS_IDEMPOTENT* = {0};
		RPC_NCA_FLAGS_BROADCAST* = {1};
		RPC_NCA_FLAGS_MAYBE* = {2};
		RPCFLG_ASYNCHRONOUS* = {30};
		RPCFLG_INPUT_SYNCHRONOUS* = {29};
		RPC_FLAGS_VALID_BIT* = {15};
		TRANSPORT_TYPE_CN* = 1;
		TRANSPORT_TYPE_DG* = 2;
		TRANSPORT_TYPE_LPC* = 4;
		TRANSPORT_TYPE_WMSG* = 8;
		RPC_C_NS_SYNTAX_DEFAULT* = 0;
		RPC_C_NS_SYNTAX_DCE* = 3;
		RPC_C_PROFILE_DEFAULT_ELT* = 0;
		RPC_C_PROFILE_ALL_ELT* = 1;
		RPC_C_PROFILE_MATCH_BY_IF* = 2;
		RPC_C_PROFILE_MATCH_BY_MBR* = 3;
		RPC_C_PROFILE_MATCH_BY_BOTH* = 4;
		RPC_C_NS_DEFAULT_EXP_AGE* = -1;
		RPC_S_OK* = 0;
		RPC_S_INVALID_ARG* = 87;
		RPC_S_OUT_OF_MEMORY* = 14;
		RPC_S_OUT_OF_THREADS* = 164;
		RPC_S_INVALID_LEVEL* = 87;
		RPC_S_BUFFER_TOO_SMALL* = 122;
		RPC_S_INVALID_SECURITY_DESC* = 1338;
		RPC_S_ACCESS_DENIED* = 5;
		RPC_S_SERVER_OUT_OF_MEMORY* = 1130;
		RPC_X_NO_MEMORY* = 14;
		RPC_X_INVALID_BOUND* = 1734;
		RPC_X_INVALID_TAG* = 1733;
		RPC_X_ENUM_VALUE_TOO_LARGE* = 1781;
		RPC_X_SS_CONTEXT_MISMATCH* = 6;
		RPC_X_INVALID_BUFFER* = 1784;
		NDR_CHAR_REP_MASK* = 15;
		NDR_INT_REP_MASK* = 240;
		NDR_FLOAT_REP_MASK* = 65280;
		NDR_LITTLE_ENDIAN* = 16;
		NDR_BIG_ENDIAN* = 0;
		NDR_IEEE_FLOAT* = 0;
		NDR_VAX_FLOAT* = 256;
		NDR_ASCII_CHAR* = 0;
		NDR_EBCDIC_CHAR* = 1;
		NDR_LOCAL_DATA_REPRESENTATION* = 16;
		NDR_LOCAL_ENDIAN* = 16;
		cbNDRContext* = 20;
		USER_MARSHAL_FC_BYTE* = 1;
		USER_MARSHAL_FC_CHAR* = 2;
		USER_MARSHAL_FC_SMALL* = 3;
		USER_MARSHAL_FC_USMALL* = 4;
		USER_MARSHAL_FC_WCHAR* = 5;
		USER_MARSHAL_FC_SHORT* = 6;
		USER_MARSHAL_FC_USHORT* = 7;
		USER_MARSHAL_FC_LONG* = 8;
		USER_MARSHAL_FC_ULONG* = 9;
		USER_MARSHAL_FC_FLOAT* = 10;
		USER_MARSHAL_FC_HYPER* = 11;
		USER_MARSHAL_FC_DOUBLE* = 12;

	CONST (* enumerations *)
		XLAT_SERVER* = 1;
		XLAT_CLIENT* = 2;
		STUB_UNMARSHAL* = 0;
		STUB_CALL_SERVER* = 1;
		STUB_MARSHAL* = 2;
		STUB_CALL_SERVER_NO_HRESULT* = 3;
		PROXY_CALCSIZE* = 0;
		PROXY_GETBUFFER* = 1;
		PROXY_MARSHAL* = 2;
		PROXY_SENDRECEIVE* = 3;
		PROXY_UNMARSHAL* = 4;

	TYPE
		RPC_STATUS* = INTEGER;
		RPC_BINDING_VECTOR* = RECORD [untagged]
			Count*: INTEGER;
			BindingH*: ARRAY [untagged] 1 OF WinApi.PtrVoid;
		END;
		PtrRPC_BINDING_VECTOR* = POINTER TO RPC_BINDING_VECTOR;
		rpc_binding_vector_t* = RPC_BINDING_VECTOR; (*m*)
		Ptrrpc_binding_vector_t* = PtrRPC_BINDING_VECTOR;
		UUID_VECTOR* = RECORD [untagged]
			Count*: INTEGER;
			Uuid*: ARRAY [untagged] 1 OF POINTER TO (*?*) ARRAY [untagged] OF COM.GUID;
		END;
		PtrUUID_VECTOR* = POINTER TO UUID_VECTOR;
		uuid_vector_t* = UUID_VECTOR; (*m*)
		Ptruuid_vector_t* = PtrUUID_VECTOR;
		RPC_IF_ID* = RECORD [untagged]
			Uuid*: COM.GUID;
			VersMajor*: SHORTINT;
			VersMinor*: SHORTINT;
		END;
		PtrRPC_IF_ID* = POINTER TO RPC_IF_ID;
		RPC_PROTSEQ_VECTORA* = RECORD [untagged]
			Count*: INTEGER;
			Protseq*: ARRAY [untagged] 1 OF WinApi.PtrSTR;
		END;
		PtrRPC_PROTSEQ_VECTORA* = POINTER TO RPC_PROTSEQ_VECTORA;
		RPC_PROTSEQ_VECTORW* = RECORD [untagged]
			Count*: INTEGER;
			Protseq*: ARRAY [untagged] 1 OF POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
		END;
		PtrRPC_PROTSEQ_VECTORW* = POINTER TO RPC_PROTSEQ_VECTORW;
		RPC_PROTSEQ_VECTOR* = RPC_PROTSEQ_VECTORA; (*m*)
		PtrRPC_PROTSEQ_VECTOR* = PtrRPC_PROTSEQ_VECTORA;
		RPC_OBJECT_INQ_FN* = PROCEDURE(VAR [nil] ObjectUuid: COM.GUID; VAR [nil] TypeUuid: COM.GUID; VAR [nil] Status: RPC_STATUS);
		RPC_IF_CALLBACK_FN* = PROCEDURE(InterfaceUuid: WinApi.PtrVoid; Context: WinApi.PtrVoid): RPC_STATUS;
		RPC_STATS_VECTOR* = RECORD [untagged]
			Count*: INTEGER;
			Stats*: ARRAY [untagged] 1 OF INTEGER;
		END;
		PtrRPC_STATS_VECTOR* = POINTER TO RPC_STATS_VECTOR;
		RPC_IF_ID_VECTOR* = RECORD [untagged]
			Count*: INTEGER;
			IfId*: ARRAY [untagged] 1 OF PtrRPC_IF_ID;
		END;
		PtrRPC_IF_ID_VECTOR* = POINTER TO RPC_IF_ID_VECTOR;
		SEC_WINNT_AUTH_IDENTITY_W* = RECORD [untagged]
			User*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
			UserLength*: INTEGER;
			Domain*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
			DomainLength*: INTEGER;
			Password*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
			PasswordLength*: INTEGER;
			Flags*: SET;
		END;
		PtrSEC_WINNT_AUTH_IDENTITY_W* = POINTER TO SEC_WINNT_AUTH_IDENTITY_W;
		SEC_WINNT_AUTH_IDENTITY_A* = RECORD [untagged]
			User*: WinApi.PtrSTR;
			UserLength*: INTEGER;
			Domain*: WinApi.PtrSTR;
			DomainLength*: INTEGER;
			Password*: WinApi.PtrSTR;
			PasswordLength*: INTEGER;
			Flags*: SET;
		END;
		PtrSEC_WINNT_AUTH_IDENTITY_A* = POINTER TO SEC_WINNT_AUTH_IDENTITY_A;
		SEC_WINNT_AUTH_IDENTITY* = SEC_WINNT_AUTH_IDENTITY_A; (*m*)
		PtrSEC_WINNT_AUTH_IDENTITY* = PtrSEC_WINNT_AUTH_IDENTITY_A;
		_SEC_WINNT_AUTH_IDENTITY* = SEC_WINNT_AUTH_IDENTITY_A; (*m*)
		Ptr_SEC_WINNT_AUTH_IDENTITY* = PtrSEC_WINNT_AUTH_IDENTITY_A;
		RPC_AUTH_KEY_RETRIEVAL_FN* = PROCEDURE (Arg: WinApi.PtrVoid; VAR [nil] ServerPrincName: SHORTINT; KeyVer: INTEGER; VAR [nil] Key: WinApi.PtrVoid; VAR [nil] Status: RPC_STATUS);
		RPC_CLIENT_INFORMATION1* = RECORD [untagged]
			UserName*: WinApi.PtrSTR;
			ComputerName*: WinApi.PtrSTR;
			Privilege*: SHORTINT;
			AuthFlags*: INTEGER;
		END;
		PtrRPC_CLIENT_INFORMATION1* = POINTER TO RPC_CLIENT_INFORMATION1;
		RPC_MGMT_AUTHORIZATION_FN* = PROCEDURE (ClientBinding: WinApi.PtrVoid; RequestedMgmtOperation: INTEGER; VAR [nil] Status: RPC_STATUS): INTEGER;
		RPC_VERSION* = RECORD [untagged]
			MajorVersion*: SHORTINT;
			MinorVersion*: SHORTINT;
		END;
		PtrRPC_VERSION* = POINTER TO RPC_VERSION;
		RPC_SYNTAX_IDENTIFIER* = RECORD [untagged]
			SyntaxGUID*: COM.GUID;
			SyntaxVersion*: RPC_VERSION;
		END;
		PtrRPC_SYNTAX_IDENTIFIER* = POINTER TO RPC_SYNTAX_IDENTIFIER;
		RPC_MESSAGE* = RECORD [untagged]
			Handle*: WinApi.PtrVoid;
			DataRepresentation*: INTEGER;
			Buffer*: WinApi.PtrVoid;
			BufferLength*: INTEGER;
			ProcNum*: INTEGER;
			TransferSyntax*: PtrRPC_SYNTAX_IDENTIFIER;
			RpcInterfaceInformation*: WinApi.PtrVoid;
			ReservedForRuntime*: WinApi.PtrVoid;
			ManagerEpv*: WinApi.PtrVoid;
			ImportContext*: WinApi.PtrVoid;
			RpcFlags*: INTEGER;
		END;
		PtrRPC_MESSAGE* = POINTER TO RPC_MESSAGE;
		RPC_FORWARD_FUNCTION* = PROCEDURE(VAR [nil] InterfaceId: COM.GUID; VAR [nil] InterfaceVersion: RPC_VERSION; VAR [nil] ObjectId: COM.GUID; Rpcpro: WinApi.PtrSTR; VAR [nil] ppDestEndpoint: WinApi.PtrVoid): RPC_STATUS;
		RPC_DISPATCH_FUNCTION* = PROCEDURE (VAR [nil] Message: RPC_MESSAGE);
		RPC_DISPATCH_TABLE* = RECORD [untagged]
			DispatchTableCount*: INTEGER;
			DispatchTable*: POINTER TO (*?*) ARRAY [untagged] OF RPC_DISPATCH_FUNCTION;
			Reserved*: INTEGER;
		END;
		PtrRPC_DISPATCH_TABLE* = POINTER TO RPC_DISPATCH_TABLE;
		RPC_PROTSEQ_ENDPOINT* = RECORD [untagged]
			RpcProtocolSequence*: WinApi.PtrSTR;
			Endpoint*: WinApi.PtrSTR;
		END;
		PtrRPC_PROTSEQ_ENDPOINT* = POINTER TO RPC_PROTSEQ_ENDPOINT;
		RPC_SERVER_INTERFACE* = RECORD [untagged]
			Length*: INTEGER;
			InterfaceId*: RPC_SYNTAX_IDENTIFIER;
			TransferSyntax*: RPC_SYNTAX_IDENTIFIER;
			DispatchTable*: PtrRPC_DISPATCH_TABLE;
			RpcProtseqEndpointCount*: INTEGER;
			RpcProtseqEndpoint*: PtrRPC_PROTSEQ_ENDPOINT;
			DefaultManagerEpv*: WinApi.PtrVoid;
			InterpreterInfo*: WinApi.PtrVoid;
		END;
		PtrRPC_SERVER_INTERFACE* = POINTER TO RPC_SERVER_INTERFACE;
		RPC_CLIENT_INTERFACE* = RECORD [untagged]
			Length*: INTEGER;
			InterfaceId*: RPC_SYNTAX_IDENTIFIER;
			TransferSyntax*: RPC_SYNTAX_IDENTIFIER;
			DispatchTable*: PtrRPC_DISPATCH_TABLE;
			RpcProtseqEndpointCount*: INTEGER;
			RpcProtseqEndpoint*: PtrRPC_PROTSEQ_ENDPOINT;
			Reserved*: INTEGER;
			InterpreterInfo*: WinApi.PtrVoid;
		END;
		PtrRPC_CLIENT_INTERFACE* = POINTER TO RPC_CLIENT_INTERFACE;
		RPC_RUNDOWN* = PROCEDURE (AssociationContext: WinApi.PtrVoid);
		RPC_TRANSFER_SYNTAX* = RECORD [untagged]
			Uuid*: COM.GUID;
			VersMajor*: SHORTINT;
			VersMinor*: SHORTINT;
		END;
		PtrRPC_TRANSFER_SYNTAX* = POINTER TO RPC_TRANSFER_SYNTAX;
		RPC_IMPORT_CONTEXT_P* = RECORD [untagged]
			LookupContext*: WinApi.PtrVoid;
			ProposedHandle*: WinApi.PtrVoid;
			Bindings*: PtrRPC_BINDING_VECTOR;
		END;
		PtrRPC_IMPORT_CONTEXT_P* = POINTER TO RPC_IMPORT_CONTEXT_P;
		boolean* = SHORTCHAR;
		NDR_SCONTEXT* = POINTER TO RECORD [untagged]
			pad*: ARRAY [untagged] 2 OF WinApi.PtrVoid;
			userContext*: WinApi.PtrVoid;
		END;
		NDR_RUNDOWN* = PROCEDURE (context: WinApi.PtrVoid);
		SCONTEXT_QUEUE* = RECORD [untagged]
			NumberOfObjects*: INTEGER;
			ArrayOfObjects*: POINTER TO (*?*) ARRAY [untagged] OF NDR_SCONTEXT;
		END;
		PtrSCONTEXT_QUEUE* = POINTER TO SCONTEXT_QUEUE;
		error_status_t* = INTEGER;
		RPC_LENGTH* = INTEGER;
		PtrMIDL_STUB_MESSAGE* = POINTER TO MIDL_STUB_MESSAGE;
		EXPR_EVAL* = PROCEDURE (p0: PtrMIDL_STUB_MESSAGE);
		ARRAY_INFO* = RECORD [untagged]
			Dimension*: INTEGER;
			BufferConformanceMark*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			BufferVarianceMark*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			MaxCountArray*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			OffsetArray*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			ActualCountArray*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
		END;
		PtrARRAY_INFO* = POINTER TO ARRAY_INFO;
		PtrMIDL_STUB_DESC* = POINTER TO MIDL_STUB_DESC;
		PtrFULL_PTR_XLAT_TABLES* = POINTER TO FULL_PTR_XLAT_TABLES;
		MIDL_STUB_MESSAGE* = RECORD [untagged]
			RpcMsg*: PtrRPC_MESSAGE;
			Buffer*: WinApi.PtrSTR;
			BufferStart*: WinApi.PtrSTR;
			BufferEnd*: WinApi.PtrSTR;
			BufferMark*: WinApi.PtrSTR;
			BufferLength*: INTEGER;
			MemorySize*: INTEGER;
			Memory*: WinApi.PtrSTR;
			IsClient*: INTEGER;
			ReuseBuffer*: INTEGER;
			AllocAllNodesMemory*: WinApi.PtrSTR;
			AllocAllNodesMemoryEnd*: WinApi.PtrSTR;
			IgnoreEmbeddedPointers*: INTEGER;
			PointerBufferMark*: WinApi.PtrSTR;
			fBufferValid*: SHORTCHAR;
			Unused*: SHORTCHAR;
			MaxCount*: INTEGER;
			Offset*: INTEGER;
			ActualCount*: INTEGER;
			pfnAllocate*: PROCEDURE (p0: INTEGER): WinApi.PtrVoid;
			pfnFree*: PROCEDURE (p0: WinApi.PtrVoid);
			StackTop*: WinApi.PtrSTR;
			pPresentedType*: WinApi.PtrSTR;
			pTransmitType*: WinApi.PtrSTR;
			SavedHandle*: WinApi.PtrVoid;
			StubDesc*: PtrMIDL_STUB_DESC;
			FullPtrXlatTables*: PtrFULL_PTR_XLAT_TABLES;
			FullPtrRefId*: INTEGER;
			fCheckBounds*: INTEGER;
			fBits0*: SET;
			(* fInDontFree*: LONGINT; (1 bits) *)
			(* fDontCallFreeInst*: LONGINT; (1 bits) *)
			(* fInOnlyParam*: LONGINT; (1 bits) *)
			(* fHasReturn*: LONGINT; (1 bits) *)
			dwDestContext*: INTEGER;
			pvDestContext*: WinApi.PtrVoid;
			SavedContextHandles*: POINTER TO (*?*) ARRAY [untagged] OF NDR_SCONTEXT;
			ParamNumber*: INTEGER;
			pRpcChannelBuffer*: WinOle.IRpcChannelBuffer;
			pArrayInfo*: PtrARRAY_INFO;
			SizePtrCountArray*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			SizePtrOffsetArray*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			SizePtrLengthArray*: POINTER TO (*?*) ARRAY [untagged] OF INTEGER;
			pArgQueue*: WinApi.PtrVoid;
			dwStubPhase*: INTEGER;
			Reserved*: ARRAY [untagged] 5 OF INTEGER;
		END;
		GENERIC_BINDING_ROUTINE* = PROCEDURE (p0: WinApi.PtrVoid): WinApi.PtrVoid;
		GENERIC_UNBIND_ROUTINE* = PROCEDURE (p0: WinApi.PtrVoid; p1: WinApi.PtrSTR);
		GENERIC_BINDING_ROUTINE_PAIR* = RECORD [untagged]
			pfnBind*: GENERIC_BINDING_ROUTINE;
			pfnUnbind*: GENERIC_UNBIND_ROUTINE;
		END;
		PtrGENERIC_BINDING_ROUTINE_PAIR* = POINTER TO GENERIC_BINDING_ROUTINE_PAIR;
		GENERIC_BINDING_INFO* = RECORD [untagged]
			pObj*: WinApi.PtrVoid;
			Size*: INTEGER;
			pfnBind*: GENERIC_BINDING_ROUTINE;
			pfnUnbind*: GENERIC_UNBIND_ROUTINE;
		END;
		PtrGENERIC_BINDING_INFO* = POINTER TO GENERIC_BINDING_INFO;
		XMIT_HELPER_ROUTINE* = PROCEDURE (VAR [nil] p0: MIDL_STUB_MESSAGE);
		XMIT_ROUTINE_QUINTUPLE* = RECORD [untagged]
			pfnTranslateToXmit*: XMIT_HELPER_ROUTINE;
			pfnTranslateFromXmit*: XMIT_HELPER_ROUTINE;
			pfnFreeXmit*: XMIT_HELPER_ROUTINE;
			pfnFreeInst*: XMIT_HELPER_ROUTINE;
		END;
		PtrXMIT_ROUTINE_QUINTUPLE* = POINTER TO XMIT_ROUTINE_QUINTUPLE;
		USER_MARSHAL_SIZING_ROUTINE* = PROCEDURE (VAR [nil] p0: INTEGER; p1: INTEGER; p2: WinApi.PtrVoid): INTEGER;
		USER_MARSHAL_MARSHALLING_ROUTINE* = PROCEDURE (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; p2: WinApi.PtrVoid): WinApi.PtrSTR;
		USER_MARSHAL_UNMARSHALLING_ROUTINE* = PROCEDURE (VAR [nil] p0: INTEGER; p1: WinApi.PtrSTR; p2: WinApi.PtrVoid): WinApi.PtrSTR;
		USER_MARSHAL_FREEING_ROUTINE* = PROCEDURE (VAR [nil] p0: INTEGER; p1: WinApi.PtrVoid);
		USER_MARSHAL_ROUTINE_QUADRUPLE* = RECORD [untagged]
			pfnBufferSize*: USER_MARSHAL_SIZING_ROUTINE;
			pfnMarshall*: USER_MARSHAL_MARSHALLING_ROUTINE;
			pfnUnmarshall*: USER_MARSHAL_UNMARSHALLING_ROUTINE;
			pfnFree*: USER_MARSHAL_FREEING_ROUTINE;
		END;
		PtrUSER_MARSHAL_ROUTINE_QUADRUPLE* = POINTER TO USER_MARSHAL_ROUTINE_QUADRUPLE;
		USER_MARSHAL_CB* = RECORD [untagged]
			Flags*: SET;
			pStubMsg*: PtrMIDL_STUB_MESSAGE;
			pReserve*: WinApi.PtrSTR;
		END;
		MALLOC_FREE_STRUCT* = RECORD [untagged]
			pfnAllocate*: PROCEDURE (p0: INTEGER): WinApi.PtrVoid;
			pfnFree*: PROCEDURE (p0: WinApi.PtrVoid);
		END;
		PtrMALLOC_FREE_STRUCT* = POINTER TO MALLOC_FREE_STRUCT;
		COMM_FAULT_OFFSETS* = RECORD [untagged]
			CommOffset*: SHORTINT;
			FaultOffset*: SHORTINT;
		END;
		PtrCOMM_FAULT_OFFSETS* = POINTER TO COMM_FAULT_OFFSETS;
		MIDL_STUB_DESC* = RECORD [untagged]
			RpcInterfaceInformation*: WinApi.PtrVoid;
			pfnAllocate*: PROCEDURE (p0: INTEGER): WinApi.PtrVoid;
			pfnFree*: PROCEDURE (p0: WinApi.PtrVoid);
			IMPLICIT_HANDLE_INFO*: RECORD [union]
				pAutoHandle*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrVoid;
				pPrimitiveHandle*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrVoid;
				pGenericBindingInfo*: PtrGENERIC_BINDING_INFO;
			END;
			apfnNdrRundownRoutines*: POINTER TO (*?*) ARRAY [untagged] OF NDR_RUNDOWN;
			aGenericBindingRoutinePairs*: PtrGENERIC_BINDING_ROUTINE_PAIR;
			apfnExprEval*: POINTER TO (*?*) ARRAY [untagged] OF EXPR_EVAL;
			aXmitQuintuple*: PtrXMIT_ROUTINE_QUINTUPLE;
			pFormatTypes*: WinApi.PtrSTR;
			fCheckBounds*: INTEGER;
			Version*: INTEGER;
			pMallocFreeStruct*: PtrMALLOC_FREE_STRUCT;
			MIDLVersion*: INTEGER;
			CommFaultOffsets*: PtrCOMM_FAULT_OFFSETS;
			aUserMarshalQuadruple*: PtrUSER_MARSHAL_ROUTINE_QUADRUPLE;
			Reserved1*: INTEGER;
			Reserved2*: INTEGER;
			Reserved3*: INTEGER;
			Reserved4*: INTEGER;
			Reserved5*: INTEGER;
		END;
		STUB_THUNK* = PROCEDURE (VAR [nil] p0: MIDL_STUB_MESSAGE);
		SERVER_ROUTINE* = PROCEDURE (): INTEGER;
		MIDL_SERVER_INFO* = RECORD [untagged]
			pStubDesc*: PtrMIDL_STUB_DESC;
			DispatchTable*: POINTER TO (*?*) ARRAY [untagged] OF SERVER_ROUTINE;
			ProcString*: WinApi.PtrSTR;
			FmtStringOffset*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
			ThunkTable*: POINTER TO (*?*) ARRAY [untagged] OF STUB_THUNK;
		END;
		PtrMIDL_SERVER_INFO* = POINTER TO MIDL_SERVER_INFO;
		MIDL_STUBLESS_PROXY_INFO* = RECORD [untagged]
			pStubDesc*: PtrMIDL_STUB_DESC;
			ProcFormatString*: WinApi.PtrSTR;
			FormatStringOffset*: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT;
		END;
		PtrMIDL_STUBLESS_PROXY_INFO* = POINTER TO MIDL_STUBLESS_PROXY_INFO;
		CLIENT_CALL_RETURN* = RECORD [union]
			Pointer*: WinApi.PtrVoid;
			Simple*: INTEGER;
		END;
		XLAT_SIDE* = INTEGER;
		PtrFULL_PTR_TO_REFID_ELEMENT* = POINTER TO FULL_PTR_TO_REFID_ELEMENT;
		FULL_PTR_TO_REFID_ELEMENT* = RECORD [untagged]
			Next*: PtrFULL_PTR_TO_REFID_ELEMENT;
			Pointer*: WinApi.PtrVoid;
			RefId*: INTEGER;
			State*: SHORTCHAR;
		END;
		FULL_PTR_XLAT_TABLES* = RECORD [untagged]
			RefIdToPointer*: RECORD [untagged]
				XlatTable*: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrVoid;
				StateTable*: WinApi.PtrSTR;
				NumberOfEntries*: INTEGER;
			END;
			PointerToRefId*: RECORD [untagged]
				XlatTable*: POINTER TO (*?*) ARRAY [untagged] OF PtrFULL_PTR_TO_REFID_ELEMENT;
				NumberOfBuckets*: INTEGER;
				HashMask*: INTEGER;
			END;
			NextRefId*: INTEGER;
			XlatSide*: XLAT_SIDE;
		END;
		STUB_PHASE* = INTEGER;
		PROXY_PHASE* = INTEGER;
		RPC_CLIENT_ALLOC* = PROCEDURE(Size: INTEGER): WinApi.PtrVoid;
		RPC_CLIENT_FREE* = PROCEDURE(Ptr: WinApi.PtrVoid);

	PROCEDURE RpcBindingCopy* (SourceBinding: WinApi.PtrVoid; VAR [nil] DestinationBinding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingCopy;*)

	PROCEDURE RpcBindingFree* (VAR [nil] Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingFree;*)

	PROCEDURE RpcBindingFromStringBindingA* (StringBinding: WinApi.PtrSTR; VAR [nil] Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingFromStringBindingA;*)

	PROCEDURE RpcBindingFromStringBindingW* (VAR [nil] StringBinding: SHORTINT; VAR [nil] Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingFromStringBindingW;*)

	PROCEDURE RpcBindingFromStringBinding* ["RpcBindingFromStringBindingA"] (StringBinding: WinApi.PtrSTR; VAR [nil] Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingFromStringBinding;*)

	PROCEDURE RpcBindingInqObject* (Binding: WinApi.PtrVoid; VAR [nil] ObjectUuid: COM.GUID): RPC_STATUS;
	(*END RpcBindingInqObject;*)

	PROCEDURE RpcBindingReset* (Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingReset;*)

	PROCEDURE RpcBindingSetObject* (Binding: WinApi.PtrVoid; VAR [nil] ObjectUuid: COM.GUID): RPC_STATUS;
	(*END RpcBindingSetObject;*)

	PROCEDURE RpcMgmtInqDefaultProtectLevel* (AuthnSvc: INTEGER; VAR [nil] AuthnLevel: INTEGER): RPC_STATUS;
	(*END RpcMgmtInqDefaultProtectLevel;*)

	PROCEDURE RpcBindingToStringBindingA* (Binding: WinApi.PtrVoid; VAR [nil] StringBinding: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcBindingToStringBindingA;*)

	PROCEDURE RpcBindingToStringBindingW* (Binding: WinApi.PtrVoid; VAR [nil] StringBinding: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcBindingToStringBindingW;*)

	PROCEDURE RpcBindingToStringBinding* ["RpcBindingToStringBindingA"] (Binding: WinApi.PtrVoid; VAR [nil] StringBinding: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcBindingToStringBinding;*)

	PROCEDURE RpcBindingVectorFree* (VAR [nil] BindingVector: PtrRPC_BINDING_VECTOR): RPC_STATUS;
	(*END RpcBindingVectorFree;*)

	PROCEDURE RpcStringBindingComposeA* (ObjUuid: WinApi.PtrSTR; Protseq: WinApi.PtrSTR; NetworkAddr: WinApi.PtrSTR; Endpoint: WinApi.PtrSTR; Options: WinApi.PtrSTR; VAR [nil] StringBinding: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcStringBindingComposeA;*)

	PROCEDURE RpcStringBindingComposeW* (VAR [nil] ObjUuid: SHORTINT; VAR [nil] Protseq: SHORTINT; VAR [nil] NetworkAddr: SHORTINT; VAR [nil] Endpoint: SHORTINT; VAR [nil] Options: SHORTINT; VAR [nil] StringBinding: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcStringBindingComposeW;*)

	PROCEDURE RpcStringBindingCompose* ["RpcStringBindingComposeA"] (ObjUuid: WinApi.PtrSTR; Protseq: WinApi.PtrSTR; NetworkAddr: WinApi.PtrSTR; Endpoint: WinApi.PtrSTR; Options: WinApi.PtrSTR; VAR [nil] StringBinding: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcStringBindingCompose;*)

	PROCEDURE RpcStringBindingParseA* (StringBinding: WinApi.PtrSTR; VAR [nil] ObjUuid: WinApi.PtrSTR; VAR [nil] Protseq: WinApi.PtrSTR; VAR [nil] NetworkAddr: WinApi.PtrSTR; VAR [nil] Endpoint: WinApi.PtrSTR; VAR [nil] NetworkOptions: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcStringBindingParseA;*)

	PROCEDURE RpcStringBindingParseW* (VAR [nil] StringBinding: SHORTINT; VAR [nil] ObjUuid: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] Protseq: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] NetworkAddr: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] Endpoint: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] NetworkOptions: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcStringBindingParseW;*)

	PROCEDURE RpcStringBindingParse* ["RpcStringBindingParseA"] (StringBinding: WinApi.PtrSTR; VAR [nil] ObjUuid: WinApi.PtrSTR; VAR [nil] Protseq: WinApi.PtrSTR; VAR [nil] NetworkAddr: WinApi.PtrSTR; VAR [nil] Endpoint: WinApi.PtrSTR; VAR [nil] NetworkOptions: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcStringBindingParse;*)

	PROCEDURE RpcStringFreeA* (VAR [nil] String: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcStringFreeA;*)

	PROCEDURE RpcStringFreeW* (VAR [nil] String: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcStringFreeW;*)

	PROCEDURE RpcStringFree* ["RpcStringFreeA"] (VAR [nil] String: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcStringFree;*)

	PROCEDURE RpcIfInqId* (RpcIfHandle: WinApi.PtrVoid; VAR [nil] RpcIfId: RPC_IF_ID): RPC_STATUS;
	(*END RpcIfInqId;*)

	PROCEDURE RpcNetworkIsProtseqValidA* (Protseq: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNetworkIsProtseqValidA;*)

	PROCEDURE RpcNetworkIsProtseqValidW* (VAR [nil] Protseq: SHORTINT): RPC_STATUS;
	(*END RpcNetworkIsProtseqValidW;*)

	PROCEDURE RpcNetworkIsProtseqValid* ["RpcNetworkIsProtseqValidA"] (Protseq: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNetworkIsProtseqValid;*)

	PROCEDURE RpcMgmtInqComTimeout* (Binding: WinApi.PtrVoid; VAR [nil] Timeout: INTEGER): RPC_STATUS;
	(*END RpcMgmtInqComTimeout;*)

	PROCEDURE RpcMgmtSetComTimeout* (Binding: WinApi.PtrVoid; Timeout: INTEGER): RPC_STATUS;
	(*END RpcMgmtSetComTimeout;*)

	PROCEDURE RpcMgmtSetCancelTimeout* (Timeout: INTEGER): RPC_STATUS;
	(*END RpcMgmtSetCancelTimeout;*)

	PROCEDURE RpcNetworkInqProtseqsA* (VAR [nil] ProtseqVector: PtrRPC_PROTSEQ_VECTORA): RPC_STATUS;
	(*END RpcNetworkInqProtseqsA;*)

	PROCEDURE RpcNetworkInqProtseqsW* (VAR [nil] ProtseqVector: PtrRPC_PROTSEQ_VECTORW): RPC_STATUS;
	(*END RpcNetworkInqProtseqsW;*)

	PROCEDURE RpcNetworkInqProtseqs* ["RpcNetworkInqProtseqsA"] (VAR [nil] ProtseqVector: PtrRPC_PROTSEQ_VECTORA): RPC_STATUS;
	(*END RpcNetworkInqProtseqs;*)

	PROCEDURE RpcObjectInqType* (VAR [nil] ObjUuid: COM.GUID; VAR [nil] TypeUuid: COM.GUID): RPC_STATUS;
	(*END RpcObjectInqType;*)

	PROCEDURE RpcObjectSetInqFn* (InquiryFn: RPC_OBJECT_INQ_FN): RPC_STATUS;
	(*END RpcObjectSetInqFn;*)

	PROCEDURE RpcObjectSetType* (VAR [nil] ObjUuid: COM.GUID; VAR [nil] TypeUuid: COM.GUID): RPC_STATUS;
	(*END RpcObjectSetType;*)

	PROCEDURE RpcProtseqVectorFreeA* (VAR [nil] ProtseqVector: PtrRPC_PROTSEQ_VECTORA): RPC_STATUS;
	(*END RpcProtseqVectorFreeA;*)

	PROCEDURE RpcProtseqVectorFreeW* (VAR [nil] ProtseqVector: PtrRPC_PROTSEQ_VECTORW): RPC_STATUS;
	(*END RpcProtseqVectorFreeW;*)

	PROCEDURE RpcProtseqVectorFree* ["RpcProtseqVectorFreeA"] (VAR [nil] ProtseqVector: PtrRPC_PROTSEQ_VECTORA): RPC_STATUS;
	(*END RpcProtseqVectorFree;*)

	PROCEDURE RpcServerInqBindings* (VAR [nil] BindingVector: PtrRPC_BINDING_VECTOR): RPC_STATUS;
	(*END RpcServerInqBindings;*)

	PROCEDURE RpcServerInqIf* (IfSpec: WinApi.PtrVoid; VAR [nil] MgrTypeUuid: COM.GUID; VAR [nil] MgrEpv: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerInqIf;*)

	PROCEDURE RpcServerListen* (MinimumCallThreads: INTEGER; MaxCalls: INTEGER; DontWait: INTEGER): RPC_STATUS;
	(*END RpcServerListen;*)

	PROCEDURE RpcServerRegisterIf* (IfSpec: WinApi.PtrVoid; VAR [nil] MgrTypeUuid: COM.GUID; MgrEpv: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerRegisterIf;*)

	PROCEDURE RpcServerUnregisterIf* (IfSpec: WinApi.PtrVoid; VAR [nil] MgrTypeUuid: COM.GUID; WaitForCallsToComplete: INTEGER): RPC_STATUS;
	(*END RpcServerUnregisterIf;*)

	PROCEDURE RpcServerUseAllProtseqs* (MaxCalls: INTEGER; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseAllProtseqs;*)

	PROCEDURE RpcServerUseAllProtseqsIf* (MaxCalls: INTEGER; IfSpec: WinApi.PtrVoid; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseAllProtseqsIf;*)

	PROCEDURE RpcServerUseProtseqA* (Protseq: WinApi.PtrSTR; MaxCalls: INTEGER; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqA;*)

	PROCEDURE RpcServerUseProtseqW* (VAR [nil] Protseq: SHORTINT; MaxCalls: INTEGER; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqW;*)

	PROCEDURE RpcServerUseProtseq* ["RpcServerUseProtseqA"] (Protseq: WinApi.PtrSTR; MaxCalls: INTEGER; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseq;*)

	PROCEDURE RpcServerUseProtseqEpA* (Protseq: WinApi.PtrSTR; MaxCalls: INTEGER; Endpoint: WinApi.PtrSTR; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqEpA;*)

	PROCEDURE RpcServerUseProtseqEpW* (VAR [nil] Protseq: SHORTINT; MaxCalls: INTEGER; VAR [nil] Endpoint: SHORTINT; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqEpW;*)

	PROCEDURE RpcServerUseProtseqEp* ["RpcServerUseProtseqEpA"] (Protseq: WinApi.PtrSTR; MaxCalls: INTEGER; Endpoint: WinApi.PtrSTR; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqEp;*)

	PROCEDURE RpcServerUseProtseqIfA* (Protseq: WinApi.PtrSTR; MaxCalls: INTEGER; IfSpec: WinApi.PtrVoid; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqIfA;*)

	PROCEDURE RpcServerUseProtseqIfW* (VAR [nil] Protseq: SHORTINT; MaxCalls: INTEGER; IfSpec: WinApi.PtrVoid; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqIfW;*)

	PROCEDURE RpcServerUseProtseqIf* ["RpcServerUseProtseqIfA"] (Protseq: WinApi.PtrSTR; MaxCalls: INTEGER; IfSpec: WinApi.PtrVoid; SecurityDescriptor: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerUseProtseqIf;*)

	PROCEDURE RpcMgmtStatsVectorFree* (VAR [nil] StatsVector: PtrRPC_STATS_VECTOR): RPC_STATUS;
	(*END RpcMgmtStatsVectorFree;*)

	PROCEDURE RpcMgmtInqStats* (Binding: WinApi.PtrVoid; VAR [nil] Statistics: PtrRPC_STATS_VECTOR): RPC_STATUS;
	(*END RpcMgmtInqStats;*)

	PROCEDURE RpcMgmtIsServerListening* (Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcMgmtIsServerListening;*)

	PROCEDURE RpcMgmtStopServerListening* (Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcMgmtStopServerListening;*)

	PROCEDURE RpcMgmtWaitServerListen* (): RPC_STATUS;
	(*END RpcMgmtWaitServerListen;*)

	PROCEDURE RpcMgmtSetServerStackSize* (ThreadStackSize: INTEGER): RPC_STATUS;
	(*END RpcMgmtSetServerStackSize;*)

	PROCEDURE RpcSsDontSerializeContext* ();
	(*END RpcSsDontSerializeContext;*)

	PROCEDURE RpcMgmtEnableIdleCleanup* (): RPC_STATUS;
	(*END RpcMgmtEnableIdleCleanup;*)

	PROCEDURE RpcMgmtInqIfIds* (Binding: WinApi.PtrVoid; VAR [nil] IfIdVector: PtrRPC_IF_ID_VECTOR): RPC_STATUS;
	(*END RpcMgmtInqIfIds;*)

	PROCEDURE RpcIfIdVectorFree* (VAR [nil] IfIdVector: PtrRPC_IF_ID_VECTOR): RPC_STATUS;
	(*END RpcIfIdVectorFree;*)

	PROCEDURE RpcMgmtInqServerPrincNameA* (Binding: WinApi.PtrVoid; AuthnSvc: INTEGER; VAR [nil] ServerPrincName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcMgmtInqServerPrincNameA;*)

	PROCEDURE RpcMgmtInqServerPrincNameW* (Binding: WinApi.PtrVoid; AuthnSvc: INTEGER; VAR [nil] ServerPrincName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcMgmtInqServerPrincNameW;*)

	PROCEDURE RpcMgmtInqServerPrincName* ["RpcMgmtInqServerPrincNameA"] (Binding: WinApi.PtrVoid; AuthnSvc: INTEGER; VAR [nil] ServerPrincName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcMgmtInqServerPrincName;*)

	PROCEDURE RpcServerInqDefaultPrincNameA* (AuthnSvc: INTEGER; VAR [nil] PrincName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcServerInqDefaultPrincNameA;*)

	PROCEDURE RpcServerInqDefaultPrincNameW* (AuthnSvc: INTEGER; VAR [nil] PrincName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcServerInqDefaultPrincNameW;*)

	PROCEDURE RpcServerInqDefaultPrincName* ["RpcServerInqDefaultPrincNameA"] (AuthnSvc: INTEGER; VAR [nil] PrincName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcServerInqDefaultPrincName;*)

	PROCEDURE RpcEpResolveBinding* (Binding: WinApi.PtrVoid; IfSpec: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcEpResolveBinding;*)

	PROCEDURE RpcNsBindingInqEntryNameA* (Binding: WinApi.PtrVoid; EntryNameSyntax: INTEGER; VAR [nil] EntryName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsBindingInqEntryNameA;*)

	PROCEDURE RpcNsBindingInqEntryNameW* (Binding: WinApi.PtrVoid; EntryNameSyntax: INTEGER; VAR [nil] EntryName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcNsBindingInqEntryNameW;*)

	PROCEDURE RpcNsBindingInqEntryName* ["RpcNsBindingInqEntryNameA"] (Binding: WinApi.PtrVoid; EntryNameSyntax: INTEGER; VAR [nil] EntryName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsBindingInqEntryName;*)

	PROCEDURE RpcBindingInqAuthClientA* (ClientBinding: WinApi.PtrVoid; VAR [nil] Privs: WinApi.PtrVoid; VAR [nil] ServerPrincName: WinApi.PtrSTR; VAR [nil] AuthnLevel: INTEGER; VAR [nil] AuthnSvc: INTEGER; VAR [nil] AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingInqAuthClientA;*)

	PROCEDURE RpcBindingInqAuthClientW* (ClientBinding: WinApi.PtrVoid; VAR [nil] Privs: WinApi.PtrVoid; VAR [nil] ServerPrincName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] AuthnLevel: INTEGER; VAR [nil] AuthnSvc: INTEGER; VAR [nil] AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingInqAuthClientW;*)

	PROCEDURE RpcBindingInqAuthInfoA* (Binding: WinApi.PtrVoid; VAR [nil] ServerPrincName: WinApi.PtrSTR; VAR [nil] AuthnLevel: INTEGER; VAR [nil] AuthnSvc: INTEGER; VAR [nil] AuthIdentity: WinApi.PtrVoid; VAR [nil] AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingInqAuthInfoA;*)

	PROCEDURE RpcBindingInqAuthInfoW* (Binding: WinApi.PtrVoid; VAR [nil] ServerPrincName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] AuthnLevel: INTEGER; VAR [nil] AuthnSvc: INTEGER; VAR [nil] AuthIdentity: WinApi.PtrVoid; VAR [nil] AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingInqAuthInfoW;*)

	PROCEDURE RpcBindingSetAuthInfoA* (Binding: WinApi.PtrVoid; ServerPrincName: WinApi.PtrSTR; AuthnLevel: INTEGER; AuthnSvc: INTEGER; AuthIdentity: WinApi.PtrVoid; AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingSetAuthInfoA;*)

	PROCEDURE RpcBindingSetAuthInfoW* (Binding: WinApi.PtrVoid; VAR [nil] ServerPrincName: SHORTINT; AuthnLevel: INTEGER; AuthnSvc: INTEGER; AuthIdentity: WinApi.PtrVoid; AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingSetAuthInfoW;*)

	PROCEDURE RpcServerRegisterAuthInfoA* (ServerPrincName: WinApi.PtrSTR; AuthnSvc: INTEGER; GetKeyFn: RPC_AUTH_KEY_RETRIEVAL_FN; Arg: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerRegisterAuthInfoA;*)

	PROCEDURE RpcServerRegisterAuthInfoW* (VAR [nil] ServerPrincName: SHORTINT; AuthnSvc: INTEGER; GetKeyFn: RPC_AUTH_KEY_RETRIEVAL_FN; Arg: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerRegisterAuthInfoW;*)

	PROCEDURE RpcBindingInqAuthClient* ["RpcBindingInqAuthClientA"] (ClientBinding: WinApi.PtrVoid; VAR [nil] Privs: WinApi.PtrVoid; VAR [nil] ServerPrincName: WinApi.PtrSTR; VAR [nil] AuthnLevel: INTEGER; VAR [nil] AuthnSvc: INTEGER; VAR [nil] AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingInqAuthClient;*)

	PROCEDURE RpcBindingInqAuthInfo* ["RpcBindingInqAuthInfoA"] (Binding: WinApi.PtrVoid; VAR [nil] ServerPrincName: WinApi.PtrSTR; VAR [nil] AuthnLevel: INTEGER; VAR [nil] AuthnSvc: INTEGER; VAR [nil] AuthIdentity: WinApi.PtrVoid; VAR [nil] AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingInqAuthInfo;*)

	PROCEDURE RpcBindingSetAuthInfo* ["RpcBindingSetAuthInfoA"] (Binding: WinApi.PtrVoid; ServerPrincName: WinApi.PtrSTR; AuthnLevel: INTEGER; AuthnSvc: INTEGER; AuthIdentity: WinApi.PtrVoid; AuthzSvc: INTEGER): RPC_STATUS;
	(*END RpcBindingSetAuthInfo;*)

	PROCEDURE RpcServerRegisterAuthInfo* ["RpcServerRegisterAuthInfoA"] (ServerPrincName: WinApi.PtrSTR; AuthnSvc: INTEGER; GetKeyFn: RPC_AUTH_KEY_RETRIEVAL_FN; Arg: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcServerRegisterAuthInfo;*)

	PROCEDURE RpcBindingServerFromClient* (ClientBinding: WinApi.PtrVoid; VAR [nil] ServerBinding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcBindingServerFromClient;*)

	PROCEDURE RpcRaiseException* (exception: RPC_STATUS);
	(*END RpcRaiseException;*)

	PROCEDURE RpcTestCancel* (): RPC_STATUS;
	(*END RpcTestCancel;*)

	PROCEDURE RpcCancelThread* (Thread: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcCancelThread;*)

	PROCEDURE UuidCreate* (VAR [nil] Uuid: COM.GUID): RPC_STATUS;
	(*END UuidCreate;*)

	PROCEDURE UuidToStringA* (VAR [nil] Uuid: COM.GUID; VAR [nil] StringUuid: WinApi.PtrSTR): RPC_STATUS;
	(*END UuidToStringA;*)

	PROCEDURE UuidFromStringA* (StringUuid: WinApi.PtrSTR; VAR [nil] Uuid: COM.GUID): RPC_STATUS;
	(*END UuidFromStringA;*)

	PROCEDURE UuidToStringW* (VAR [nil] Uuid: COM.GUID; VAR [nil] StringUuid: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END UuidToStringW;*)

	PROCEDURE UuidFromStringW* (VAR [nil] StringUuid: SHORTINT; VAR [nil] Uuid: COM.GUID): RPC_STATUS;
	(*END UuidFromStringW;*)

	PROCEDURE UuidFromString* ["UuidFromStringA"] (StringUuid: WinApi.PtrSTR; VAR [nil] Uuid: COM.GUID): RPC_STATUS;
	(*END UuidFromString;*)

	PROCEDURE UuidToString* ["UuidToStringA"] (VAR [nil] Uuid: COM.GUID; VAR [nil] StringUuid: WinApi.PtrSTR): RPC_STATUS;
	(*END UuidToString;*)

	PROCEDURE UuidCompare* (VAR [nil] Uuid1: COM.GUID; VAR [nil] Uuid2: COM.GUID; VAR [nil] Status: RPC_STATUS): INTEGER;
	(*END UuidCompare;*)

	PROCEDURE UuidCreateNil* (VAR [nil] NilUuid: COM.GUID): RPC_STATUS;
	(*END UuidCreateNil;*)

	PROCEDURE UuidEqual* (VAR [nil] Uuid1: COM.GUID; VAR [nil] Uuid2: COM.GUID; VAR [nil] Status: RPC_STATUS): INTEGER;
	(*END UuidEqual;*)

	PROCEDURE UuidHash* (VAR [nil] Uuid: COM.GUID; VAR [nil] Status: RPC_STATUS): SHORTINT;
	(*END UuidHash;*)

	PROCEDURE UuidIsNil* (VAR [nil] Uuid: COM.GUID; VAR [nil] Status: RPC_STATUS): INTEGER;
	(*END UuidIsNil;*)

	PROCEDURE RpcEpRegisterNoReplaceA* (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR; Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcEpRegisterNoReplaceA;*)

	PROCEDURE RpcEpRegisterNoReplaceW* (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR; VAR [nil] Annotation: SHORTINT): RPC_STATUS;
	(*END RpcEpRegisterNoReplaceW;*)

	PROCEDURE RpcEpRegisterA* (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR; Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcEpRegisterA;*)

	PROCEDURE RpcEpRegisterW* (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR; VAR [nil] Annotation: SHORTINT): RPC_STATUS;
	(*END RpcEpRegisterW;*)

	PROCEDURE RpcEpRegisterNoReplace* ["RpcEpRegisterNoReplaceA"] (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR; Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcEpRegisterNoReplace;*)

	PROCEDURE RpcEpRegister* ["RpcEpRegisterA"] (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR; Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcEpRegister;*)

	PROCEDURE RpcEpUnregister* (IfSpec: WinApi.PtrVoid; VAR [nil] BindingVector: RPC_BINDING_VECTOR; VAR [nil] UuidVector: UUID_VECTOR): RPC_STATUS;
	(*END RpcEpUnregister;*)

	PROCEDURE DceErrorInqTextA* (RpcStatus: RPC_STATUS; ErrorText: WinApi.PtrSTR): RPC_STATUS;
	(*END DceErrorInqTextA;*)

	PROCEDURE DceErrorInqTextW* (RpcStatus: RPC_STATUS; VAR [nil] ErrorText: SHORTINT): RPC_STATUS;
	(*END DceErrorInqTextW;*)

	PROCEDURE DceErrorInqText* ["DceErrorInqTextA"] (RpcStatus: RPC_STATUS; ErrorText: WinApi.PtrSTR): RPC_STATUS;
	(*END DceErrorInqText;*)

	PROCEDURE RpcMgmtEpEltInqBegin* (EpBinding: WinApi.PtrVoid; InquiryType: INTEGER; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; VAR [nil] ObjectUuid: COM.GUID; VAR [nil] InquiryContext: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrVoid): RPC_STATUS;
	(*END RpcMgmtEpEltInqBegin;*)

	PROCEDURE RpcMgmtEpEltInqDone* (VAR [nil] InquiryContext: POINTER TO (*?*) ARRAY [untagged] OF WinApi.PtrVoid): RPC_STATUS;
	(*END RpcMgmtEpEltInqDone;*)

	PROCEDURE RpcMgmtEpEltInqNextA* (VAR [nil] InquiryContext: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; VAR [nil] Binding: WinApi.PtrVoid; VAR [nil] ObjectUuid: COM.GUID; VAR [nil] Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcMgmtEpEltInqNextA;*)

	PROCEDURE RpcMgmtEpEltInqNextW* (VAR [nil] InquiryContext: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; VAR [nil] Binding: WinApi.PtrVoid; VAR [nil] ObjectUuid: COM.GUID; VAR [nil] Annotation: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcMgmtEpEltInqNextW;*)

	PROCEDURE RpcMgmtEpEltInqNext* ["RpcMgmtEpEltInqNextA"] (VAR [nil] InquiryContext: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; VAR [nil] Binding: WinApi.PtrVoid; VAR [nil] ObjectUuid: COM.GUID; VAR [nil] Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcMgmtEpEltInqNext;*)

	PROCEDURE RpcMgmtEpUnregister* (EpBinding: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; Binding: WinApi.PtrVoid; VAR [nil] ObjectUuid: COM.GUID): RPC_STATUS;
	(*END RpcMgmtEpUnregister;*)

	PROCEDURE RpcMgmtSetAuthorizationFn* (AuthorizationFn: RPC_MGMT_AUTHORIZATION_FN): RPC_STATUS;
	(*END RpcMgmtSetAuthorizationFn;*)

	PROCEDURE RpcMgmtInqParameter* (Parameter: INTEGER; VAR [nil] Value: INTEGER): RPC_STATUS;
	(*END RpcMgmtInqParameter;*)

	PROCEDURE RpcMgmtSetParameter* (Parameter: INTEGER; Value: INTEGER): RPC_STATUS;
	(*END RpcMgmtSetParameter;*)

	PROCEDURE RpcMgmtBindingInqParameter* (Handle: WinApi.PtrVoid; Parameter: INTEGER; VAR [nil] Value: INTEGER): RPC_STATUS;
	(*END RpcMgmtBindingInqParameter;*)

	PROCEDURE RpcMgmtBindingSetParameter* (Handle: WinApi.PtrVoid; Parameter: INTEGER; Value: INTEGER): RPC_STATUS;
	(*END RpcMgmtBindingSetParameter;*)

	PROCEDURE I_RpcGetBuffer* (VAR [nil] Message: RPC_MESSAGE): RPC_STATUS;
	(*END I_RpcGetBuffer;*)

	PROCEDURE I_RpcSendReceive* (VAR [nil] Message: RPC_MESSAGE): RPC_STATUS;
	(*END I_RpcSendReceive;*)

	PROCEDURE I_RpcFreeBuffer* (VAR [nil] Message: RPC_MESSAGE): RPC_STATUS;
	(*END I_RpcFreeBuffer;*)

	PROCEDURE I_RpcRequestMutex* (VAR [nil] Mutex: WinApi.PtrVoid);
	(*END I_RpcRequestMutex;*)

	PROCEDURE I_RpcClearMutex* (Mutex: WinApi.PtrVoid);
	(*END I_RpcClearMutex;*)

	PROCEDURE I_RpcDeleteMutex* (Mutex: WinApi.PtrVoid);
	(*END I_RpcDeleteMutex;*)

	PROCEDURE I_RpcAllocate* (Size: INTEGER): WinApi.PtrVoid;
	(*END I_RpcAllocate;*)

	PROCEDURE I_RpcFree* (Object: WinApi.PtrVoid);
	(*END I_RpcFree;*)

	PROCEDURE I_RpcPauseExecution* (Milliseconds: INTEGER);
	(*END I_RpcPauseExecution;*)

	PROCEDURE I_RpcMonitorAssociation* (Handle: WinApi.PtrVoid; RundownRoutine: RPC_RUNDOWN; Context: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcMonitorAssociation;*)

	PROCEDURE I_RpcStopMonitorAssociation* (Handle: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcStopMonitorAssociation;*)

	PROCEDURE I_RpcGetCurrentCallHandle* (): WinApi.PtrVoid;
	(*END I_RpcGetCurrentCallHandle;*)

	PROCEDURE I_RpcGetAssociationContext* (VAR [nil] AssociationContext: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcGetAssociationContext;*)

	PROCEDURE I_RpcSetAssociationContext* (AssociationContext: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcSetAssociationContext;*)

	PROCEDURE I_RpcNsBindingSetEntryName* (Binding: WinApi.PtrVoid; EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT): RPC_STATUS;
	(*END I_RpcNsBindingSetEntryName;*)

	PROCEDURE I_RpcBindingInqDynamicEndpoint* (Binding: WinApi.PtrVoid; VAR [nil] DynamicEndpoint: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END I_RpcBindingInqDynamicEndpoint;*)

	PROCEDURE I_RpcBindingInqTransportType* (Binding: WinApi.PtrVoid; VAR [nil] Type: INTEGER): RPC_STATUS;
	(*END I_RpcBindingInqTransportType;*)

	PROCEDURE I_RpcIfInqTransferSyntaxes* (RpcIfHandle: WinApi.PtrVoid; VAR [nil] TransferSyntaxes: RPC_TRANSFER_SYNTAX; TransferSyntaxSize: INTEGER; VAR [nil] TransferSyntaxCount: INTEGER): RPC_STATUS;
	(*END I_RpcIfInqTransferSyntaxes;*)

	PROCEDURE I_UuidCreate* (VAR [nil] Uuid: COM.GUID): RPC_STATUS;
	(*END I_UuidCreate;*)

	PROCEDURE I_RpcBindingCopy* (SourceBinding: WinApi.PtrVoid; VAR [nil] DestinationBinding: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcBindingCopy;*)

	PROCEDURE I_RpcBindingIsClientLocal* (BindingHandle: WinApi.PtrVoid; VAR [nil] ClientLocalFlag: INTEGER): RPC_STATUS;
	(*END I_RpcBindingIsClientLocal;*)

	PROCEDURE I_RpcSsDontSerializeContext* ();
	(*END I_RpcSsDontSerializeContext;*)

	PROCEDURE I_RpcLaunchDatagramReceiveThread* (pAddress: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcLaunchDatagramReceiveThread;*)

	PROCEDURE I_RpcServerRegisterForwardFunction* (pForwardFunction: RPC_FORWARD_FUNCTION): RPC_STATUS;
	(*END I_RpcServerRegisterForwardFunction;*)

	PROCEDURE I_RpcConnectionInqSockBuffSize* (VAR [nil] RecvBuffSize: INTEGER; VAR [nil] SendBuffSize: INTEGER): RPC_STATUS;
	(*END I_RpcConnectionInqSockBuffSize;*)

	PROCEDURE I_RpcConnectionSetSockBuffSize* (RecvBuffSize: INTEGER; SendBuffSize: INTEGER): RPC_STATUS;
	(*END I_RpcConnectionSetSockBuffSize;*)

	PROCEDURE I_RpcServerInqTransportType* (VAR [nil] Type: INTEGER): RPC_STATUS;
	(*END I_RpcServerInqTransportType;*)

	PROCEDURE RpcNsBindingExportA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] BindingVec: RPC_BINDING_VECTOR; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsBindingExportA;*)

	PROCEDURE RpcNsBindingUnexportA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsBindingUnexportA;*)

	PROCEDURE RpcNsBindingExportW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; IfSpec: WinApi.PtrVoid; VAR [nil] BindingVec: RPC_BINDING_VECTOR; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsBindingExportW;*)

	PROCEDURE RpcNsBindingUnexportW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; IfSpec: WinApi.PtrVoid; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsBindingUnexportW;*)

	PROCEDURE RpcNsBindingLookupBeginA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID; BindingMaxCount: INTEGER; VAR [nil] LookupContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingLookupBeginA;*)

	PROCEDURE RpcNsBindingLookupBeginW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; IfSpec: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID; BindingMaxCount: INTEGER; VAR [nil] LookupContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingLookupBeginW;*)

	PROCEDURE RpcNsBindingLookupNext* ["RPCNS4.dll", ""] (LookupContext: WinApi.PtrVoid; VAR [nil] BindingVec: PtrRPC_BINDING_VECTOR): RPC_STATUS;
	(*END RpcNsBindingLookupNext;*)

	PROCEDURE RpcNsBindingLookupDone* ["RPCNS4.dll", ""] (VAR [nil] LookupContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingLookupDone;*)

	PROCEDURE RpcNsGroupDeleteA* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupDeleteA;*)

	PROCEDURE RpcNsGroupMbrAddA* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupMbrAddA;*)

	PROCEDURE RpcNsGroupMbrRemoveA* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupMbrRemoveA;*)

	PROCEDURE RpcNsGroupMbrInqBeginA* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR; MemberNameSyntax: INTEGER; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsGroupMbrInqBeginA;*)

	PROCEDURE RpcNsGroupMbrInqNextA* ["RPCNS4.dll", ""] (InquiryContext: WinApi.PtrVoid; VAR [nil] MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupMbrInqNextA;*)

	PROCEDURE RpcNsGroupDeleteW* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; VAR [nil] GroupName: SHORTINT): RPC_STATUS;
	(*END RpcNsGroupDeleteW;*)

	PROCEDURE RpcNsGroupMbrAddW* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; VAR [nil] GroupName: SHORTINT; MemberNameSyntax: INTEGER; VAR [nil] MemberName: SHORTINT): RPC_STATUS;
	(*END RpcNsGroupMbrAddW;*)

	PROCEDURE RpcNsGroupMbrRemoveW* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; VAR [nil] GroupName: SHORTINT; MemberNameSyntax: INTEGER; VAR [nil] MemberName: SHORTINT): RPC_STATUS;
	(*END RpcNsGroupMbrRemoveW;*)

	PROCEDURE RpcNsGroupMbrInqBeginW* ["RPCNS4.dll", ""] (GroupNameSyntax: INTEGER; VAR [nil] GroupName: SHORTINT; MemberNameSyntax: INTEGER; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsGroupMbrInqBeginW;*)

	PROCEDURE RpcNsGroupMbrInqNextW* ["RPCNS4.dll", ""] (InquiryContext: WinApi.PtrVoid; VAR [nil] MemberName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcNsGroupMbrInqNextW;*)

	PROCEDURE RpcNsGroupMbrInqDone* ["RPCNS4.dll", ""] (VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsGroupMbrInqDone;*)

	PROCEDURE RpcNsProfileDeleteA* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileDeleteA;*)

	PROCEDURE RpcNsProfileEltAddA* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR; VAR [nil] IfId: RPC_IF_ID; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR; Priority: INTEGER; Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileEltAddA;*)

	PROCEDURE RpcNsProfileEltRemoveA* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR; VAR [nil] IfId: RPC_IF_ID; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileEltRemoveA;*)

	PROCEDURE RpcNsProfileEltInqBeginA* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR; InquiryType: INTEGER; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsProfileEltInqBeginA;*)

	PROCEDURE RpcNsProfileEltInqNextA* ["RPCNS4.dll", ""] (InquiryContext: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; VAR [nil] MemberName: WinApi.PtrSTR; VAR [nil] Priority: INTEGER; VAR [nil] Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileEltInqNextA;*)

	PROCEDURE RpcNsProfileDeleteW* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; VAR [nil] ProfileName: SHORTINT): RPC_STATUS;
	(*END RpcNsProfileDeleteW;*)

	PROCEDURE RpcNsProfileEltAddW* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; VAR [nil] ProfileName: SHORTINT; VAR [nil] IfId: RPC_IF_ID; MemberNameSyntax: INTEGER; VAR [nil] MemberName: SHORTINT; Priority: INTEGER; VAR [nil] Annotation: SHORTINT): RPC_STATUS;
	(*END RpcNsProfileEltAddW;*)

	PROCEDURE RpcNsProfileEltRemoveW* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; VAR [nil] ProfileName: SHORTINT; VAR [nil] IfId: RPC_IF_ID; MemberNameSyntax: INTEGER; VAR [nil] MemberName: SHORTINT): RPC_STATUS;
	(*END RpcNsProfileEltRemoveW;*)

	PROCEDURE RpcNsProfileEltInqBeginW* ["RPCNS4.dll", ""] (ProfileNameSyntax: INTEGER; VAR [nil] ProfileName: SHORTINT; InquiryType: INTEGER; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; MemberNameSyntax: INTEGER; VAR [nil] MemberName: SHORTINT; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsProfileEltInqBeginW;*)

	PROCEDURE RpcNsProfileEltInqNextW* ["RPCNS4.dll", ""] (InquiryContext: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; VAR [nil] MemberName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT; VAR [nil] Priority: INTEGER; VAR [nil] Annotation: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcNsProfileEltInqNextW;*)

	PROCEDURE RpcNsProfileEltInqDone* ["RPCNS4.dll", ""] (VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsProfileEltInqDone;*)

	PROCEDURE RpcNsEntryObjectInqBeginA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsEntryObjectInqBeginA;*)

	PROCEDURE RpcNsEntryObjectInqBeginW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsEntryObjectInqBeginW;*)

	PROCEDURE RpcNsEntryObjectInqNext* ["RPCNS4.dll", ""] (InquiryContext: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID): RPC_STATUS;
	(*END RpcNsEntryObjectInqNext;*)

	PROCEDURE RpcNsEntryObjectInqDone* ["RPCNS4.dll", ""] (VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsEntryObjectInqDone;*)

	PROCEDURE RpcNsEntryExpandNameA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] ExpandedName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsEntryExpandNameA;*)

	PROCEDURE RpcNsMgmtBindingUnexportA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsMgmtBindingUnexportA;*)

	PROCEDURE RpcNsMgmtEntryCreateA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsMgmtEntryCreateA;*)

	PROCEDURE RpcNsMgmtEntryDeleteA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsMgmtEntryDeleteA;*)

	PROCEDURE RpcNsMgmtEntryInqIfIdsA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] IfIdVec: PtrRPC_IF_ID_VECTOR): RPC_STATUS;
	(*END RpcNsMgmtEntryInqIfIdsA;*)

	PROCEDURE RpcNsMgmtHandleSetExpAge* ["RPCNS4.dll", ""] (NsHandle: WinApi.PtrVoid; ExpirationAge: INTEGER): RPC_STATUS;
	(*END RpcNsMgmtHandleSetExpAge;*)

	PROCEDURE RpcNsMgmtInqExpAge* ["RPCNS4.dll", ""] (VAR [nil] ExpirationAge: INTEGER): RPC_STATUS;
	(*END RpcNsMgmtInqExpAge;*)

	PROCEDURE RpcNsMgmtSetExpAge* ["RPCNS4.dll", ""] (ExpirationAge: INTEGER): RPC_STATUS;
	(*END RpcNsMgmtSetExpAge;*)

	PROCEDURE RpcNsEntryExpandNameW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; VAR [nil] ExpandedName: POINTER TO (*?*) ARRAY [untagged] OF SHORTINT): RPC_STATUS;
	(*END RpcNsEntryExpandNameW;*)

	PROCEDURE RpcNsMgmtBindingUnexportW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsMgmtBindingUnexportW;*)

	PROCEDURE RpcNsMgmtEntryCreateW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT): RPC_STATUS;
	(*END RpcNsMgmtEntryCreateW;*)

	PROCEDURE RpcNsMgmtEntryDeleteW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT): RPC_STATUS;
	(*END RpcNsMgmtEntryDeleteW;*)

	PROCEDURE RpcNsMgmtEntryInqIfIdsW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; VAR [nil] IfIdVec: PtrRPC_IF_ID_VECTOR): RPC_STATUS;
	(*END RpcNsMgmtEntryInqIfIdsW;*)

	PROCEDURE RpcNsBindingImportBeginA* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID; VAR [nil] ImportContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingImportBeginA;*)

	PROCEDURE RpcNsBindingImportBeginW* ["RPCNS4.dll", ""] (EntryNameSyntax: INTEGER; VAR [nil] EntryName: SHORTINT; IfSpec: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID; VAR [nil] ImportContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingImportBeginW;*)

	PROCEDURE RpcNsBindingImportNext* ["RPCNS4.dll", ""] (ImportContext: WinApi.PtrVoid; VAR [nil] Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingImportNext;*)

	PROCEDURE RpcNsBindingImportDone* ["RPCNS4.dll", ""] (VAR [nil] ImportContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingImportDone;*)

	PROCEDURE RpcNsBindingSelect* ["RPCNS4.dll", ""] (VAR [nil] BindingVec: RPC_BINDING_VECTOR; VAR [nil] Binding: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingSelect;*)

	PROCEDURE RpcNsBindingLookupBegin* ["RPCNS4.dll", "RpcNsBindingLookupBeginA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID; BindingMaxCount: INTEGER; VAR [nil] LookupContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingLookupBegin;*)

	PROCEDURE RpcNsBindingImportBegin* ["RPCNS4.dll", "RpcNsBindingImportBeginA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] ObjUuid: COM.GUID; VAR [nil] ImportContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsBindingImportBegin;*)

	PROCEDURE RpcNsBindingExport* ["RPCNS4.dll", "RpcNsBindingExportA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] BindingVec: RPC_BINDING_VECTOR; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsBindingExport;*)

	PROCEDURE RpcNsBindingUnexport* ["RPCNS4.dll", "RpcNsBindingUnexportA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; IfSpec: WinApi.PtrVoid; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsBindingUnexport;*)

	PROCEDURE RpcNsGroupDelete* ["RPCNS4.dll", "RpcNsGroupDeleteA"] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupDelete;*)

	PROCEDURE RpcNsGroupMbrAdd* ["RPCNS4.dll", "RpcNsGroupMbrAddA"] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupMbrAdd;*)

	PROCEDURE RpcNsGroupMbrRemove* ["RPCNS4.dll", "RpcNsGroupMbrRemoveA"] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupMbrRemove;*)

	PROCEDURE RpcNsGroupMbrInqBegin* ["RPCNS4.dll", "RpcNsGroupMbrInqBeginA"] (GroupNameSyntax: INTEGER; GroupName: WinApi.PtrSTR; MemberNameSyntax: INTEGER; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsGroupMbrInqBegin;*)

	PROCEDURE RpcNsGroupMbrInqNext* ["RPCNS4.dll", "RpcNsGroupMbrInqNextA"] (InquiryContext: WinApi.PtrVoid; VAR [nil] MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsGroupMbrInqNext;*)

	PROCEDURE RpcNsEntryExpandName* ["RPCNS4.dll", "RpcNsEntryExpandNameA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] ExpandedName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsEntryExpandName;*)

	PROCEDURE RpcNsEntryObjectInqBegin* ["RPCNS4.dll", "RpcNsEntryObjectInqBeginA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsEntryObjectInqBegin;*)

	PROCEDURE RpcNsMgmtBindingUnexport* ["RPCNS4.dll", "RpcNsMgmtBindingUnexportA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; VAR [nil] ObjectUuidVec: UUID_VECTOR): RPC_STATUS;
	(*END RpcNsMgmtBindingUnexport;*)

	PROCEDURE RpcNsMgmtEntryCreate* ["RPCNS4.dll", "RpcNsMgmtEntryCreateA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsMgmtEntryCreate;*)

	PROCEDURE RpcNsMgmtEntryDelete* ["RPCNS4.dll", "RpcNsMgmtEntryDeleteA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsMgmtEntryDelete;*)

	PROCEDURE RpcNsMgmtEntryInqIfIds* ["RPCNS4.dll", "RpcNsMgmtEntryInqIfIdsA"] (EntryNameSyntax: INTEGER; EntryName: WinApi.PtrSTR; VAR [nil] IfIdVec: PtrRPC_IF_ID_VECTOR): RPC_STATUS;
	(*END RpcNsMgmtEntryInqIfIds;*)

	PROCEDURE RpcNsProfileDelete* ["RPCNS4.dll", "RpcNsProfileDeleteA"] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileDelete;*)

	PROCEDURE RpcNsProfileEltAdd* ["RPCNS4.dll", "RpcNsProfileEltAddA"] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR; VAR [nil] IfId: RPC_IF_ID; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR; Priority: INTEGER; Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileEltAdd;*)

	PROCEDURE RpcNsProfileEltRemove* ["RPCNS4.dll", "RpcNsProfileEltRemoveA"] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR; VAR [nil] IfId: RPC_IF_ID; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileEltRemove;*)

	PROCEDURE RpcNsProfileEltInqBegin* ["RPCNS4.dll", "RpcNsProfileEltInqBeginA"] (ProfileNameSyntax: INTEGER; ProfileName: WinApi.PtrSTR; InquiryType: INTEGER; VAR [nil] IfId: RPC_IF_ID; VersOption: INTEGER; MemberNameSyntax: INTEGER; MemberName: WinApi.PtrSTR; VAR [nil] InquiryContext: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcNsProfileEltInqBegin;*)

	PROCEDURE RpcNsProfileEltInqNext* ["RPCNS4.dll", "RpcNsProfileEltInqNextA"] (InquiryContext: WinApi.PtrVoid; VAR [nil] IfId: RPC_IF_ID; VAR [nil] MemberName: WinApi.PtrSTR; VAR [nil] Priority: INTEGER; VAR [nil] Annotation: WinApi.PtrSTR): RPC_STATUS;
	(*END RpcNsProfileEltInqNext;*)

	PROCEDURE RpcImpersonateClient* (BindingHandle: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcImpersonateClient;*)

	PROCEDURE RpcRevertToSelfEx* (BindingHandle: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcRevertToSelfEx;*)

	PROCEDURE RpcRevertToSelf* (): RPC_STATUS;
	(*END RpcRevertToSelf;*)

	PROCEDURE I_RpcMapWin32Status* (Status: RPC_STATUS): INTEGER;
	(*END I_RpcMapWin32Status;*)

	PROCEDURE I_RpcNsGetBuffer* ["RPCNS4.dll", ""] (VAR [nil] Message: RPC_MESSAGE): RPC_STATUS;
	(*END I_RpcNsGetBuffer;*)

	PROCEDURE I_RpcNsSendReceive* ["RPCNS4.dll", ""] (VAR [nil] Message: RPC_MESSAGE; VAR [nil] Handle: WinApi.PtrVoid): RPC_STATUS;
	(*END I_RpcNsSendReceive;*)

	PROCEDURE I_RpcNsRaiseException* ["RPCNS4.dll", ""] (VAR [nil] Message: RPC_MESSAGE; Status: RPC_STATUS);
	(*END I_RpcNsRaiseException;*)

	PROCEDURE I_RpcReBindBuffer* ["RPCNS4.dll", ""] (VAR [nil] Message: RPC_MESSAGE): RPC_STATUS;
	(*END I_RpcReBindBuffer;*)

	PROCEDURE NDRCContextBinding* (CContext: WinApi.PtrVoid): WinApi.PtrVoid;
	(*END NDRCContextBinding;*)

	PROCEDURE NDRCContextMarshall* (CContext: WinApi.PtrVoid; pBuff: WinApi.PtrVoid);
	(*END NDRCContextMarshall;*)

	PROCEDURE NDRCContextUnmarshall* (VAR [nil] pCContext: WinApi.PtrVoid; hBinding: WinApi.PtrVoid; pBuff: WinApi.PtrVoid; DataRepresentation: INTEGER);
	(*END NDRCContextUnmarshall;*)

	PROCEDURE NDRSContextMarshall* (CContext: NDR_SCONTEXT; pBuff: WinApi.PtrVoid; userRunDownIn: NDR_RUNDOWN);
	(*END NDRSContextMarshall;*)

	PROCEDURE NDRSContextUnmarshall* (pBuff: WinApi.PtrVoid; DataRepresentation: INTEGER): NDR_SCONTEXT;
	(*END NDRSContextUnmarshall;*)

	PROCEDURE RpcSsDestroyClientContext* (VAR [nil] ContextHandle: WinApi.PtrVoid);
	(*END RpcSsDestroyClientContext;*)

	PROCEDURE NDRcopy* (pTarget: WinApi.PtrVoid; pSource: WinApi.PtrVoid; size: INTEGER);
	(*END NDRcopy;*)

	PROCEDURE MIDL_wchar_strlen* (s: WinApi.PtrWSTR): INTEGER;
	(*END MIDL_wchar_strlen;*)

	PROCEDURE MIDL_wchar_strcpy* (t: WinApi.PtrVoid; s: WinApi.PtrWSTR);
	(*END MIDL_wchar_strcpy;*)

	PROCEDURE char_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; Target: WinApi.PtrSTR);
	(*END char_from_ndr;*)

	PROCEDURE char_array_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; LowerIndex: INTEGER; UpperIndex: INTEGER; Target: WinApi.PtrSTR);
	(*END char_array_from_ndr;*)

	PROCEDURE short_from_ndr* (VAR [nil] source: RPC_MESSAGE; VAR [nil] target: SHORTINT);
	(*END short_from_ndr;*)

	PROCEDURE short_array_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; LowerIndex: INTEGER; UpperIndex: INTEGER; VAR [nil] Target: SHORTINT);
	(*END short_array_from_ndr;*)

	PROCEDURE short_from_ndr_temp* (VAR [nil] source: WinApi.PtrSTR; VAR [nil] target: SHORTINT; format: INTEGER);
	(*END short_from_ndr_temp;*)

	PROCEDURE long_from_ndr* (VAR [nil] source: RPC_MESSAGE; VAR [nil] target: INTEGER);
	(*END long_from_ndr;*)

	PROCEDURE long_array_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; LowerIndex: INTEGER; UpperIndex: INTEGER; VAR [nil] Target: INTEGER);
	(*END long_array_from_ndr;*)

	PROCEDURE long_from_ndr_temp* (VAR [nil] source: WinApi.PtrSTR; VAR [nil] target: INTEGER; format: INTEGER);
	(*END long_from_ndr_temp;*)

	PROCEDURE enum_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; VAR [nil] Target: INTEGER);
	(*END enum_from_ndr;*)

	PROCEDURE float_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; Target: WinApi.PtrVoid);
	(*END float_from_ndr;*)

	PROCEDURE float_array_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; LowerIndex: INTEGER; UpperIndex: INTEGER; Target: WinApi.PtrVoid);
	(*END float_array_from_ndr;*)

	PROCEDURE double_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; Target: WinApi.PtrVoid);
	(*END double_from_ndr;*)

	PROCEDURE double_array_from_ndr* (VAR [nil] SourceMessage: RPC_MESSAGE; LowerIndex: INTEGER; UpperIndex: INTEGER; Target: WinApi.PtrVoid);
	(*END double_array_from_ndr;*)

	PROCEDURE data_from_ndr* (VAR [nil] source: RPC_MESSAGE; target: WinApi.PtrVoid; format: WinApi.PtrSTR; MscPak: SHORTCHAR);
	(*END data_from_ndr;*)

	PROCEDURE data_into_ndr* (source: WinApi.PtrVoid; VAR [nil] target: RPC_MESSAGE; format: WinApi.PtrSTR; MscPak: SHORTCHAR);
	(*END data_into_ndr;*)

	PROCEDURE tree_into_ndr* (source: WinApi.PtrVoid; VAR [nil] target: RPC_MESSAGE; format: WinApi.PtrSTR; MscPak: SHORTCHAR);
	(*END tree_into_ndr;*)

	PROCEDURE data_size_ndr* (source: WinApi.PtrVoid; VAR [nil] target: RPC_MESSAGE; format: WinApi.PtrSTR; MscPak: SHORTCHAR);
	(*END data_size_ndr;*)

	PROCEDURE tree_size_ndr* (source: WinApi.PtrVoid; VAR [nil] target: RPC_MESSAGE; format: WinApi.PtrSTR; MscPak: SHORTCHAR);
	(*END tree_size_ndr;*)

	PROCEDURE tree_peek_ndr* (VAR [nil] source: RPC_MESSAGE; VAR [nil] buffer: WinApi.PtrSTR; format: WinApi.PtrSTR; MscPak: SHORTCHAR);
	(*END tree_peek_ndr;*)

	PROCEDURE NdrSimpleTypeMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; FormatChar: SHORTCHAR);
	(*END NdrSimpleTypeMarshall;*)

	PROCEDURE NdrPointerMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrPointerMarshall;*)

	PROCEDURE NdrSimpleStructMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrSimpleStructMarshall;*)

	PROCEDURE NdrConformantStructMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrConformantStructMarshall;*)

	PROCEDURE NdrConformantVaryingStructMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrConformantVaryingStructMarshall;*)

	PROCEDURE NdrHardStructMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrHardStructMarshall;*)

	PROCEDURE NdrComplexStructMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrComplexStructMarshall;*)

	PROCEDURE NdrFixedArrayMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrFixedArrayMarshall;*)

	PROCEDURE NdrConformantArrayMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrConformantArrayMarshall;*)

	PROCEDURE NdrConformantVaryingArrayMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrConformantVaryingArrayMarshall;*)

	PROCEDURE NdrVaryingArrayMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrVaryingArrayMarshall;*)

	PROCEDURE NdrComplexArrayMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrComplexArrayMarshall;*)

	PROCEDURE NdrNonConformantStringMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrNonConformantStringMarshall;*)

	PROCEDURE NdrConformantStringMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrConformantStringMarshall;*)

	PROCEDURE NdrEncapsulatedUnionMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrEncapsulatedUnionMarshall;*)

	PROCEDURE NdrNonEncapsulatedUnionMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrNonEncapsulatedUnionMarshall;*)

	PROCEDURE NdrByteCountPointerMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrByteCountPointerMarshall;*)

	PROCEDURE NdrXmitOrRepAsMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrXmitOrRepAsMarshall;*)

	PROCEDURE NdrUserMarshalMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrUserMarshalMarshall;*)

	PROCEDURE NdrInterfacePointerMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrInterfacePointerMarshall;*)

	PROCEDURE NdrClientContextMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; ContextHandle: WinApi.PtrVoid; fCheck: INTEGER);
	(*END NdrClientContextMarshall;*)

	PROCEDURE NdrServerContextMarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; ContextHandle: NDR_SCONTEXT; RundownRoutine: NDR_RUNDOWN);
	(*END NdrServerContextMarshall;*)

	PROCEDURE NdrSimpleTypeUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; FormatChar: SHORTCHAR);
	(*END NdrSimpleTypeUnmarshall;*)

	PROCEDURE NdrPointerUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrPointerUnmarshall;*)

	PROCEDURE NdrSimpleStructUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrSimpleStructUnmarshall;*)

	PROCEDURE NdrConformantStructUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrConformantStructUnmarshall;*)

	PROCEDURE NdrConformantVaryingStructUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrConformantVaryingStructUnmarshall;*)

	PROCEDURE NdrHardStructUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrHardStructUnmarshall;*)

	PROCEDURE NdrComplexStructUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrComplexStructUnmarshall;*)

	PROCEDURE NdrFixedArrayUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrFixedArrayUnmarshall;*)

	PROCEDURE NdrConformantArrayUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrConformantArrayUnmarshall;*)

	PROCEDURE NdrConformantVaryingArrayUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrConformantVaryingArrayUnmarshall;*)

	PROCEDURE NdrVaryingArrayUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrVaryingArrayUnmarshall;*)

	PROCEDURE NdrComplexArrayUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrComplexArrayUnmarshall;*)

	PROCEDURE NdrNonConformantStringUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrNonConformantStringUnmarshall;*)

	PROCEDURE NdrConformantStringUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrConformantStringUnmarshall;*)

	PROCEDURE NdrEncapsulatedUnionUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrEncapsulatedUnionUnmarshall;*)

	PROCEDURE NdrNonEncapsulatedUnionUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrNonEncapsulatedUnionUnmarshall;*)

	PROCEDURE NdrByteCountPointerUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrByteCountPointerUnmarshall;*)

	PROCEDURE NdrXmitOrRepAsUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrXmitOrRepAsUnmarshall;*)

	PROCEDURE NdrUserMarshalUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrUserMarshalUnmarshall;*)

	PROCEDURE NdrInterfacePointerUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] ppMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR; fMustAlloc: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrInterfacePointerUnmarshall;*)

	PROCEDURE NdrClientContextUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pContextHandle: WinApi.PtrVoid; BindHandle: WinApi.PtrVoid);
	(*END NdrClientContextUnmarshall;*)

	PROCEDURE NdrServerContextUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE): NDR_SCONTEXT;
	(*END NdrServerContextUnmarshall;*)

	PROCEDURE NdrPointerBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrPointerBufferSize;*)

	PROCEDURE NdrSimpleStructBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrSimpleStructBufferSize;*)

	PROCEDURE NdrConformantStructBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantStructBufferSize;*)

	PROCEDURE NdrConformantVaryingStructBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantVaryingStructBufferSize;*)

	PROCEDURE NdrHardStructBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrHardStructBufferSize;*)

	PROCEDURE NdrComplexStructBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrComplexStructBufferSize;*)

	PROCEDURE NdrFixedArrayBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrFixedArrayBufferSize;*)

	PROCEDURE NdrConformantArrayBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantArrayBufferSize;*)

	PROCEDURE NdrConformantVaryingArrayBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantVaryingArrayBufferSize;*)

	PROCEDURE NdrVaryingArrayBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrVaryingArrayBufferSize;*)

	PROCEDURE NdrComplexArrayBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrComplexArrayBufferSize;*)

	PROCEDURE NdrConformantStringBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantStringBufferSize;*)

	PROCEDURE NdrNonConformantStringBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrNonConformantStringBufferSize;*)

	PROCEDURE NdrEncapsulatedUnionBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrEncapsulatedUnionBufferSize;*)

	PROCEDURE NdrNonEncapsulatedUnionBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrNonEncapsulatedUnionBufferSize;*)

	PROCEDURE NdrByteCountPointerBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrByteCountPointerBufferSize;*)

	PROCEDURE NdrXmitOrRepAsBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrXmitOrRepAsBufferSize;*)

	PROCEDURE NdrUserMarshalBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrUserMarshalBufferSize;*)

	PROCEDURE NdrInterfacePointerBufferSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrInterfacePointerBufferSize;*)

	PROCEDURE NdrContextHandleSize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrContextHandleSize;*)

	PROCEDURE NdrPointerMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrPointerMemorySize;*)

	PROCEDURE NdrSimpleStructMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrSimpleStructMemorySize;*)

	PROCEDURE NdrConformantStructMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrConformantStructMemorySize;*)

	PROCEDURE NdrConformantVaryingStructMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrConformantVaryingStructMemorySize;*)

	PROCEDURE NdrHardStructMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrHardStructMemorySize;*)

	PROCEDURE NdrComplexStructMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrComplexStructMemorySize;*)

	PROCEDURE NdrFixedArrayMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrFixedArrayMemorySize;*)

	PROCEDURE NdrConformantArrayMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrConformantArrayMemorySize;*)

	PROCEDURE NdrConformantVaryingArrayMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrConformantVaryingArrayMemorySize;*)

	PROCEDURE NdrVaryingArrayMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrVaryingArrayMemorySize;*)

	PROCEDURE NdrComplexArrayMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrComplexArrayMemorySize;*)

	PROCEDURE NdrConformantStringMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrConformantStringMemorySize;*)

	PROCEDURE NdrNonConformantStringMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrNonConformantStringMemorySize;*)

	PROCEDURE NdrEncapsulatedUnionMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrEncapsulatedUnionMemorySize;*)

	PROCEDURE NdrNonEncapsulatedUnionMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrNonEncapsulatedUnionMemorySize;*)

	PROCEDURE NdrXmitOrRepAsMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrXmitOrRepAsMemorySize;*)

	PROCEDURE NdrUserMarshalMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrUserMarshalMemorySize;*)

	PROCEDURE NdrInterfacePointerMemorySize* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR): INTEGER;
	(*END NdrInterfacePointerMemorySize;*)

	PROCEDURE NdrPointerFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrPointerFree;*)

	PROCEDURE NdrSimpleStructFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrSimpleStructFree;*)

	PROCEDURE NdrConformantStructFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantStructFree;*)

	PROCEDURE NdrConformantVaryingStructFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantVaryingStructFree;*)

	PROCEDURE NdrHardStructFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrHardStructFree;*)

	PROCEDURE NdrComplexStructFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrComplexStructFree;*)

	PROCEDURE NdrFixedArrayFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrFixedArrayFree;*)

	PROCEDURE NdrConformantArrayFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantArrayFree;*)

	PROCEDURE NdrConformantVaryingArrayFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrConformantVaryingArrayFree;*)

	PROCEDURE NdrVaryingArrayFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrVaryingArrayFree;*)

	PROCEDURE NdrComplexArrayFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrComplexArrayFree;*)

	PROCEDURE NdrEncapsulatedUnionFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrEncapsulatedUnionFree;*)

	PROCEDURE NdrNonEncapsulatedUnionFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrNonEncapsulatedUnionFree;*)

	PROCEDURE NdrByteCountPointerFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrByteCountPointerFree;*)

	PROCEDURE NdrXmitOrRepAsFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrXmitOrRepAsFree;*)

	PROCEDURE NdrUserMarshalFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrUserMarshalFree;*)

	PROCEDURE NdrInterfacePointerFree* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pMemory: WinApi.PtrSTR; pFormat: WinApi.PtrSTR);
	(*END NdrInterfacePointerFree;*)

	PROCEDURE NdrConvert2* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR; NumberParams: INTEGER);
	(*END NdrConvert2;*)

	PROCEDURE NdrConvert* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR);
	(*END NdrConvert;*)

	PROCEDURE NdrUserMarshalSimpleTypeConvert* (VAR [nil] pFlags: INTEGER; pBuffer: WinApi.PtrSTR; FormatChar: SHORTCHAR): WinApi.PtrSTR;
	(*END NdrUserMarshalSimpleTypeConvert;*)

	PROCEDURE NdrClientInitializeNew* (VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pStubDescriptor: MIDL_STUB_DESC; ProcNum: INTEGER);
	(*END NdrClientInitializeNew;*)

	PROCEDURE NdrServerInitializeNew* (VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pStubDescriptor: MIDL_STUB_DESC): WinApi.PtrSTR;
	(*END NdrServerInitializeNew;*)

	PROCEDURE NdrClientInitialize* (VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pStubDescriptor: MIDL_STUB_DESC; ProcNum: INTEGER);
	(*END NdrClientInitialize;*)

	PROCEDURE NdrServerInitialize* (VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pStubDescriptor: MIDL_STUB_DESC): WinApi.PtrSTR;
	(*END NdrServerInitialize;*)

	PROCEDURE NdrServerInitializeUnmarshall* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pStubDescriptor: MIDL_STUB_DESC; VAR [nil] pRpcMsg: RPC_MESSAGE): WinApi.PtrSTR;
	(*END NdrServerInitializeUnmarshall;*)

	PROCEDURE NdrServerInitializeMarshall* (VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE);
	(*END NdrServerInitializeMarshall;*)

	PROCEDURE NdrGetBuffer* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; BufferLength: INTEGER; Handle: WinApi.PtrVoid): WinApi.PtrSTR;
	(*END NdrGetBuffer;*)

	PROCEDURE NdrNsGetBuffer* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; BufferLength: INTEGER; Handle: WinApi.PtrVoid): WinApi.PtrSTR;
	(*END NdrNsGetBuffer;*)

	PROCEDURE NdrSendReceive* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pBufferEnd: WinApi.PtrSTR): WinApi.PtrSTR;
	(*END NdrSendReceive;*)

	PROCEDURE NdrNsSendReceive* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pBufferEnd: WinApi.PtrSTR; VAR [nil] pAutoHandle: WinApi.PtrVoid): WinApi.PtrSTR;
	(*END NdrNsSendReceive;*)

	PROCEDURE NdrFreeBuffer* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE);
	(*END NdrFreeBuffer;*)

	PROCEDURE [ccall] NdrClientCall2* (VAR [nil] pStubDescriptor: MIDL_STUB_DESC; pFormat: WinApi.PtrSTR): LONGINT;
	(*END NdrClientCall2;*)

	PROCEDURE [ccall] NdrClientCall* (VAR [nil] pStubDescriptor: MIDL_STUB_DESC; pFormat: WinApi.PtrSTR): LONGINT;
	(*END NdrClientCall;*)

	PROCEDURE NdrStubCall2* (pThis: WinOle.IRpcStubBuffer; pChannel: WinOle.IRpcChannelBuffer; VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pdwStubPhase: INTEGER): INTEGER;
	(*END NdrStubCall2;*)

	PROCEDURE NdrServerCall2* (VAR [nil] pRpcMsg: RPC_MESSAGE);
	(*END NdrServerCall2;*)

	PROCEDURE NdrStubCall* (pThis: WinOle.IRpcStubBuffer; pChannel: WinOle.IRpcChannelBuffer; VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pdwStubPhase: INTEGER): INTEGER;
	(*END NdrStubCall;*)

	PROCEDURE NdrServerCall* (VAR [nil] pRpcMsg: RPC_MESSAGE);
	(*END NdrServerCall;*)

	PROCEDURE NdrServerUnmarshall* (pChannel: WinOle.IRpcChannelBuffer; VAR [nil] pRpcMsg: RPC_MESSAGE; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pStubDescriptor: MIDL_STUB_DESC; pFormat: WinApi.PtrSTR; pParamList: WinApi.PtrVoid): INTEGER;
	(*END NdrServerUnmarshall;*)

	PROCEDURE NdrServerMarshall* (pThis: WinOle.IRpcStubBuffer; pChannel: WinOle.IRpcChannelBuffer; VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR);
	(*END NdrServerMarshall;*)

	PROCEDURE NdrMapCommAndFaultStatus* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; VAR [nil] pCommStatus: INTEGER; VAR [nil] pFaultStatus: INTEGER; Status: RPC_STATUS): RPC_STATUS;
	(*END NdrMapCommAndFaultStatus;*)

	PROCEDURE RpcSsAllocate* (Size: INTEGER): WinApi.PtrVoid;
	(*END RpcSsAllocate;*)

	PROCEDURE RpcSsDisableAllocate* ();
	(*END RpcSsDisableAllocate;*)

	PROCEDURE RpcSsEnableAllocate* ();
	(*END RpcSsEnableAllocate;*)

	PROCEDURE RpcSsFree* (NodeToFree: WinApi.PtrVoid);
	(*END RpcSsFree;*)

	PROCEDURE RpcSsGetThreadHandle* (): WinApi.PtrVoid;
	(*END RpcSsGetThreadHandle;*)

	PROCEDURE RpcSsSetClientAllocFree* (ClientAlloc: RPC_CLIENT_ALLOC; ClientFree: RPC_CLIENT_FREE);
	(*END RpcSsSetClientAllocFree;*)

	PROCEDURE RpcSsSetThreadHandle* (Id: WinApi.PtrVoid);
	(*END RpcSsSetThreadHandle;*)

	PROCEDURE RpcSsSwapClientAllocFree* (ClientAlloc: RPC_CLIENT_ALLOC; ClientFree: RPC_CLIENT_FREE; VAR [nil] OldClientAlloc: RPC_CLIENT_ALLOC; VAR [nil] OldClientFree: RPC_CLIENT_FREE);
	(*END RpcSsSwapClientAllocFree;*)

	PROCEDURE RpcSmAllocate* (Size: INTEGER; VAR [nil] pStatus: RPC_STATUS): WinApi.PtrVoid;
	(*END RpcSmAllocate;*)

	PROCEDURE RpcSmClientFree* (pNodeToFree: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcSmClientFree;*)

	PROCEDURE RpcSmDestroyClientContext* (VAR [nil] ContextHandle: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcSmDestroyClientContext;*)

	PROCEDURE RpcSmDisableAllocate* (): RPC_STATUS;
	(*END RpcSmDisableAllocate;*)

	PROCEDURE RpcSmEnableAllocate* (): RPC_STATUS;
	(*END RpcSmEnableAllocate;*)

	PROCEDURE RpcSmFree* (NodeToFree: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcSmFree;*)

	PROCEDURE RpcSmGetThreadHandle* (VAR [nil] pStatus: RPC_STATUS): WinApi.PtrVoid;
	(*END RpcSmGetThreadHandle;*)

	PROCEDURE RpcSmSetClientAllocFree* (ClientAlloc: RPC_CLIENT_ALLOC; ClientFree: RPC_CLIENT_FREE): RPC_STATUS;
	(*END RpcSmSetClientAllocFree;*)

	PROCEDURE RpcSmSetThreadHandle* (Id: WinApi.PtrVoid): RPC_STATUS;
	(*END RpcSmSetThreadHandle;*)

	PROCEDURE RpcSmSwapClientAllocFree* (ClientAlloc: RPC_CLIENT_ALLOC; ClientFree: RPC_CLIENT_FREE; VAR [nil] OldClientAlloc: RPC_CLIENT_ALLOC; VAR [nil] OldClientFree: RPC_CLIENT_FREE): RPC_STATUS;
	(*END RpcSmSwapClientAllocFree;*)

	PROCEDURE NdrRpcSsEnableAllocate* (VAR [nil] pMessage: MIDL_STUB_MESSAGE);
	(*END NdrRpcSsEnableAllocate;*)

	PROCEDURE NdrRpcSsDisableAllocate* (VAR [nil] pMessage: MIDL_STUB_MESSAGE);
	(*END NdrRpcSsDisableAllocate;*)

	PROCEDURE NdrRpcSmSetClientToOsf* (VAR [nil] pMessage: MIDL_STUB_MESSAGE);
	(*END NdrRpcSmSetClientToOsf;*)

	PROCEDURE NdrRpcSmClientAllocate* (Size: INTEGER): WinApi.PtrVoid;
	(*END NdrRpcSmClientAllocate;*)

	PROCEDURE NdrRpcSmClientFree* (NodeToFree: WinApi.PtrVoid);
	(*END NdrRpcSmClientFree;*)

	PROCEDURE NdrRpcSsDefaultAllocate* (Size: INTEGER): WinApi.PtrVoid;
	(*END NdrRpcSsDefaultAllocate;*)

	PROCEDURE NdrRpcSsDefaultFree* (NodeToFree: WinApi.PtrVoid);
	(*END NdrRpcSsDefaultFree;*)

	PROCEDURE NdrFullPointerXlatInit* (NumberOfPointers: INTEGER; XlatSide: XLAT_SIDE): PtrFULL_PTR_XLAT_TABLES;
	(*END NdrFullPointerXlatInit;*)

	PROCEDURE NdrFullPointerXlatFree* (VAR [nil] pXlatTables: FULL_PTR_XLAT_TABLES);
	(*END NdrFullPointerXlatFree;*)

	PROCEDURE NdrFullPointerQueryPointer* (VAR [nil] pXlatTables: FULL_PTR_XLAT_TABLES; pPointer: WinApi.PtrVoid; QueryType: SHORTCHAR; VAR [nil] pRefId: INTEGER): INTEGER;
	(*END NdrFullPointerQueryPointer;*)

	PROCEDURE NdrFullPointerQueryRefId* (VAR [nil] pXlatTables: FULL_PTR_XLAT_TABLES; RefId: INTEGER; QueryType: SHORTCHAR; VAR [nil] ppPointer: WinApi.PtrVoid): INTEGER;
	(*END NdrFullPointerQueryRefId;*)

	PROCEDURE NdrFullPointerInsertRefId* (VAR [nil] pXlatTables: FULL_PTR_XLAT_TABLES; RefId: INTEGER; pPointer: WinApi.PtrVoid);
	(*END NdrFullPointerInsertRefId;*)

	PROCEDURE NdrFullPointerFree* (VAR [nil] pXlatTables: FULL_PTR_XLAT_TABLES; Pointer: WinApi.PtrVoid): INTEGER;
	(*END NdrFullPointerFree;*)

	PROCEDURE NdrAllocate* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; Len: INTEGER): WinApi.PtrVoid;
	(*END NdrAllocate;*)

	PROCEDURE NdrClearOutParameters* (VAR [nil] pStubMsg: MIDL_STUB_MESSAGE; pFormat: WinApi.PtrSTR; ArgAddr: WinApi.PtrVoid);
	(*END NdrClearOutParameters;*)

	PROCEDURE NdrOleAllocate* (Size: INTEGER): WinApi.PtrVoid;
	(*END NdrOleAllocate;*)

	PROCEDURE NdrOleFree* (NodeToFree: WinApi.PtrVoid);
	(*END NdrOleFree;*)

END WinRpc.
