MODULE DevTypeLibs;
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

	(* Code that is only reached with wrapper in options. Code that is not reached with wrapper in options. *)
		
	IMPORT COM, WinOle, WinOleAut, TextModels, TextMappers;

	CONST
		(* options *)
		browse = 1; interface = 2; wrapper = 3;
		inAuto = 12; inAll = 13;
		outAuto = 14; outAll = 15;
		source = 31;
		(* kind *)
		value = 1; var = 2; varin = 3; varout = 4;
		(* conv type *)
		ret = 1; get = 2; par = 3; refpar = 4;
		(* pseudo types *)
		enumerator = -1; record = -2; union = -3; module = -4; class = -5; ptrVoid = -6;

	VAR
		modules: ARRAY 16, 64 OF CHAR;
		noMod: INTEGER;
		retName: ARRAY 156 OF CHAR;

	PROCEDURE WriteBSTR (s: WinOle.BSTR; VAR out: TextMappers.Formatter);
	BEGIN
		IF s # NIL THEN out.WriteString(s$) END
	END WriteBSTR;

	PROCEDURE WriteGuid (VAR guid: COM.GUID; VAR out: TextMappers.Formatter);
	BEGIN
		out.WriteChar("{");
		out.WriteIntForm((guid[2] MOD 256 + 256 * guid[3]) MOD 65536, TextMappers.hexadecimal, 4, "0", FALSE);
		out.WriteIntForm((guid[0] MOD 256 + 256 * guid[1]) MOD 65536, TextMappers.hexadecimal, 4, "0", FALSE);
		out.WriteChar("-");
		out.WriteIntForm((guid[4] MOD 256 + 256 * guid[5]) MOD 65536, TextMappers.hexadecimal, 4, "0", FALSE);
		out.WriteChar("-");
		out.WriteIntForm((guid[6] MOD 256 + 256 * guid[7]) MOD 65536, TextMappers.hexadecimal, 4, "0", FALSE);
		out.WriteChar("-");
		out.WriteIntForm((guid[8]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteIntForm((guid[9]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteChar("-");
		out.WriteIntForm((guid[10]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteIntForm((guid[11]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteIntForm((guid[12]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteIntForm((guid[13]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteIntForm((guid[14]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteIntForm((guid[15]) MOD 256, TextMappers.hexadecimal, 2, "0", FALSE);
		out.WriteChar("}")
	END WriteGuid;

	PROCEDURE^ GetInfoType (tinfo: WinOleAut.ITypeInfo; OUT type: INTEGER; OUT info: WinOleAut.ITypeInfo);
	
	PROCEDURE GetDescType (IN desc: WinOleAut.TYPEDESC; tinfo: WinOleAut.ITypeInfo;
										OUT type: INTEGER; OUT info: WinOleAut.ITypeInfo);
		VAR res: COM.RESULT; t: INTEGER; i: WinOleAut.ITypeInfo;
	BEGIN
		type := desc.vt;
		IF type = WinOle.VT_USERDEFINED THEN
			res := tinfo.GetRefTypeInfo(desc.u.hreftype, i);
			GetInfoType(i, type, info)
		ELSIF type = WinOle.VT_PTR THEN
			GetDescType(desc.u.lptdesc, tinfo, t, i);
			IF (i # NIL) & ((t = WinOle.VT_DISPATCH) OR (t = WinOle.VT_UNKNOWN))
					OR (t = WinOle.VT_VARIANT) OR (t = WinOle.VT_SAFEARRAY) THEN type := t; info := i
			ELSIF t = WinOle.VT_VOID THEN type := ptrVoid
			END
		END
	END GetDescType;

	PROCEDURE GetInfoType (tinfo: WinOleAut.ITypeInfo; OUT type: INTEGER; OUT info: WinOleAut.ITypeInfo);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; i: INTEGER; flags: SET;
			ti: WinOleAut.ITypeInfo; t: WinOleAut.HREFTYPE;
	BEGIN
		info := tinfo; res := tinfo.GetTypeAttr(attr);
		CASE attr.typekind OF
		| WinOleAut.TKIND_ENUM: type := WinOle.VT_I4
		| WinOleAut.TKIND_RECORD: type := record
		| WinOleAut.TKIND_UNION: type := union
		| WinOleAut.TKIND_MODULE: type := module
		| WinOleAut.TKIND_INTERFACE: type := WinOle.VT_UNKNOWN
		| WinOleAut.TKIND_DISPATCH: type := WinOle.VT_DISPATCH
		| WinOleAut.TKIND_ALIAS: GetDescType(attr.tdescAlias, tinfo, type, info)
		| WinOleAut.TKIND_COCLASS: 
			type := class; i := 0;
			WHILE (i < attr.cImplTypes) & (type = class) DO
				res := tinfo.GetImplTypeFlags(i, flags);
				IF (WinOleAut.IMPLTYPEFLAG_FDEFAULT * flags # {} )
						& (WinOleAut.IMPLTYPEFLAG_FSOURCE * flags = {} ) THEN
					res := tinfo.GetRefTypeOfImplType(i, t); ASSERT(res >= 0, 101);
					res := tinfo.GetRefTypeInfo(t, ti); ASSERT(res >= 0, 102);
					GetInfoType(ti, type, info)
				END;
				INC(i)
			END
		END;
		tinfo.ReleaseTypeAttr(attr)
	END GetInfoType;

	PROCEDURE WriteVariant (VAR v: WinOleAut.VARIANT; VAR out: TextMappers.Formatter);
	BEGIN
		IF ODD(v.vt DIV WinOle.VT_ARRAY) THEN
			out.WriteSString("array ");
			(* to be completed *)
		ELSIF ODD(v.vt DIV WinOle.VT_BYREF) THEN
			out.WriteSString("ref ");
			CASE v.vt MOD 4096 OF
			| WinOle.VT_UI1: out.WriteInt(ORD(v.u.pbVal[0]))
			| WinOle.VT_I2: out.WriteInt(v.u.piVal[0])
			| WinOle.VT_I4:
				IF v.u.plVal[0] = 80000000H THEN out.WriteString("80000000H")
				ELSE out.WriteInt(v.u.plVal[0])
				END
			| WinOle.VT_R4: out.WriteReal(v.u.pfltVal[0])
			| WinOle.VT_R8: out.WriteReal(v.u.pdblVal[0])
			| WinOle.VT_BOOL:
				IF v.u.pboolVal[0] = 0 THEN out.WriteSString("FALSE") ELSE out.WriteSString("TRUE") END
			| WinOle.VT_CY: out.WriteReal(v.u.pcyVal[0] / 10000)
			| WinOle.VT_DATE: out.WriteReal(v.u.pdate[0])
			| WinOle.VT_BSTR: out.WriteChar('"'); WriteBSTR(v.u.pbstrVal[0], out); out.WriteChar('"')
			| WinOle.VT_ERROR: out.WriteIntForm(v.u.pscode[0], TextMappers.hexadecimal, 9, "0", TRUE)
			| WinOle.VT_DISPATCH: out.WriteSString("IDispatch")
			| WinOle.VT_UNKNOWN: out.WriteSString("IUnknown")
			ELSE out.WriteSString("undefined ("); out.WriteInt(v.vt MOD 4096); out.WriteChar(")")
			END
		ELSE
			CASE v.vt MOD 4096 OF
			| WinOle.VT_NULL: out.WriteSString("NIL")
			| WinOle.VT_UI1: out.WriteInt(ORD(v.u.bVal))
			| WinOle.VT_I2: out.WriteInt(v.u.iVal)
			| WinOle.VT_I4:
				IF v.u.lVal = 80000000H THEN out.WriteString("80000000H")
				ELSE out.WriteInt(v.u.lVal)
				END
			| WinOle.VT_R4: out.WriteReal(v.u.fltVal)
			| WinOle.VT_R8: out.WriteReal(v.u.dblVal)
			| WinOle.VT_BOOL:
				IF v.u.boolVal = 0 THEN out.WriteSString("FALSE") ELSE out.WriteSString("TRUE") END
			| WinOle.VT_CY: out.WriteReal(v.u.cyVal / 10000)
			| WinOle.VT_DATE: out.WriteReal(v.u.date)
			| WinOle.VT_BSTR: out.WriteChar('"'); WriteBSTR(v.u.bstrVal, out); out.WriteChar('"')
			| WinOle.VT_ERROR: out.WriteIntForm(v.u.scode, TextMappers.hexadecimal, 9, "0", TRUE)
			| WinOle.VT_DISPATCH: out.WriteSString("IDispatch")
			| WinOle.VT_UNKNOWN: out.WriteSString("IUnknown")
			ELSE out.WriteSString("undefined ("); out.WriteInt(v.vt MOD 4096); out.WriteChar(")")
			END
		END
	END WriteVariant;

	PROCEDURE WriteTypeName (
		tinfo: WinOleAut.ITypeInfo; convert, isThis: BOOLEAN; VAR out: TextMappers.Formatter
	);
		VAR ti: WinOleAut.ITypeInfo; lib: WinOleAut.ITypeLib; i, n, vt: INTEGER;
			res: COM.RESULT; s: WinOle.BSTR; mod, name: ARRAY 64 OF CHAR; flags: SET;
	BEGIN
		res := tinfo.GetContainingTypeLib(lib, i); ASSERT(res >= 0, 102);
		res := lib.GetDocumentation(-1, s, NIL, NIL, NIL); ASSERT(res >= 0, 105);
		mod := s$; WinOleAut.SysFreeString(s);
		res := tinfo.GetDocumentation(-1, s, NIL, NIL, NIL); ASSERT(res >= 0, 106);
		(* !!! *)
		name := s$; WinOleAut.SysFreeString(s);
		IF convert THEN
			IF mod = "stdole" THEN
				IF name = "IDispatch" THEN mod := "CtlT"; name := "Object"
				ELSE mod := "CtlStdType"
				END
			ELSE
				GetInfoType(tinfo, vt, ti);
				IF vt = WinOle.VT_UNKNOWN THEN mod := "CtlT"; name := "IUnknown"
				ELSE mod := "Ctl" + mod
				END
			END;
		END;
		i := 0; WHILE (i < noMod) & (modules[i] # mod) DO INC(i) END;
		IF i = noMod THEN modules[i] := mod$; INC(noMod) END;
		IF i # 0 THEN out.WriteString(mod); out.WriteChar(".") END;
		IF isThis THEN out.WriteString("This") END;
		out.WriteString(name)
	END WriteTypeName;

	PROCEDURE WriteHandleName (
		t: WinOleAut.HREFTYPE; tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter
	);
		VAR res: COM.RESULT; type: WinOleAut.ITypeInfo;
	BEGIN
		res := tinfo.GetRefTypeInfo(t, type); (* ASSERT(res >= 0, 100); *)
		IF res >= 0 THEN
			WriteTypeName(type, wrapper IN opts, FALSE, out)
		ELSE
			out.WriteSString("???("); out.WriteInt(res); out.WriteChar(")")
		END
	END WriteHandleName;

	PROCEDURE WriteType (
		VAR t: WinOleAut.TYPEDESC; tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter
	);
		VAR i, vt: INTEGER; ti: WinOleAut.ITypeInfo;
	BEGIN
		CASE t.vt OF
		| WinOle.VT_I2: out.WriteSString("SHORTINT")
		| WinOle.VT_I4: out.WriteSString("INTEGER")
		| WinOle.VT_R4: out.WriteSString("SHORTREAL")
		| WinOle.VT_R8: out.WriteSString("REAL")
		| WinOle.VT_CY:
			IF wrapper IN opts THEN out.WriteSString("CtlT.OleCy") ELSE out.WriteSString("CURRENCY") END
		| WinOle.VT_DATE:
			IF wrapper IN opts THEN out.WriteSString("CtlT.OleDate") ELSE out.WriteSString("DATE") END
		| WinOle.VT_BSTR:
			IF wrapper IN opts THEN out.WriteSString("CtlT.Strg") ELSE out.WriteSString("WinOle.BSTR") END
		| WinOle.VT_DISPATCH:
			IF wrapper IN opts THEN out.WriteSString("CtlT.Object")
			ELSE out.WriteSString("WinOleAut.IDispatch")
			END
		| WinOle.VT_ERROR: out.WriteSString("CtlT.RESULT")
		| WinOle.VT_BOOL:
			IF wrapper IN opts THEN out.WriteSString("BOOLEAN")
			ELSE out.WriteSString("WinOle.VARIANT_BOOL")
			END
		| WinOle.VT_VARIANT:
			IF wrapper IN opts THEN out.WriteSString("CtlT.Any") ELSE out.WriteSString("WinOleAut.VARIANT") END
		| WinOle.VT_UNKNOWN: out.WriteSString("CtlT.IUnknown")
		| WinOle.VT_DECIMAL: out.WriteSString("DECIMAL")
		| WinOle.VT_I1: out.WriteSString("BYTE")
		| WinOle.VT_UI1: out.WriteSString("BYTE")
		| WinOle.VT_UI2: out.WriteSString("SHORTINT")
		| WinOle.VT_UI4: out.WriteSString("INTEGER")
		| WinOle.VT_I8: out.WriteSString("LONGINT")
		| WinOle.VT_UI8: out.WriteSString("LONGINT")
		| WinOle.VT_INT: out.WriteSString("INTEGER")
		| WinOle.VT_UINT: out.WriteSString("INTEGER")
		| WinOle.VT_VOID:
			IF browse IN opts THEN out.WriteSString("VOID") ELSE out.WriteSString("RECORD END") END
		| WinOle.VT_HRESULT: out.WriteSString("CtlT.RESULT")
		| WinOle.VT_PTR:
			GetDescType(t, tinfo, vt, ti);
			IF vt = ptrVoid THEN out.WriteSString("CtlT.PtrVoid")
			ELSIF vt # WinOle.VT_PTR THEN WriteType(t.u.lptdesc, tinfo, opts, out)
			ELSE out.WriteSString("POINTER TO "); WriteType(t.u.lptdesc^, tinfo, opts, out)
			END
		| WinOle.VT_SAFEARRAY:
			IF wrapper IN opts THEN out.WriteSString("CtlT.Any")
			ELSE out.WriteSString("ARRAY [safe] OF "); WriteType(t.u.lptdesc^, tinfo, opts, out)
			END
		| WinOle.VT_CARRAY: out.WriteSString("ARRAY ");
			i := 0;
			WHILE i < t.u.lpadesc.cDims DO
				out.WriteInt(t.u.lpadesc.rgbounds[i].cElements); out.WriteChar(" "); INC(i)
			END;
			out.WriteSString("OF "); WriteType(t.u.lpadesc.tdescElem, tinfo, opts, out)
		| WinOle.VT_USERDEFINED: WriteHandleName(t.u.hreftype, tinfo, opts, out)
		| WinOle.VT_LPSTR:
			IF wrapper IN opts THEN out.WriteSString("CtlT.Strg") ELSE out.WriteSString("PtrSTR") END
		| WinOle.VT_LPWSTR:
			IF wrapper IN opts THEN out.WriteSString("CtlT.Strg") ELSE out.WriteSString("PtrWSTR") END
		| enumerator: out.WriteSString("CtlT.Enumerator")
		END
	END WriteType;

	PROCEDURE WriteShortType (VAR t: WinOleAut.TYPEDESC; tinfo: WinOleAut.ITypeInfo; VAR out: TextMappers.Formatter);
		VAR vt: INTEGER; ti: WinOleAut.ITypeInfo; res: COM.RESULT;
	BEGIN
		GetDescType(t, tinfo, vt, ti);
		CASE vt OF
		| WinOle.VT_I2: out.WriteSString("SInt")
		| WinOle.VT_I4: out.WriteSString("Int")
		| WinOle.VT_R4: out.WriteSString("SReal")
		| WinOle.VT_R8: out.WriteSString("Real")
		| WinOle.VT_CY: out.WriteSString("Cy")
		| WinOle.VT_DATE: out.WriteSString("Date")
		| WinOle.VT_BSTR: out.WriteSString("Str")
		| WinOle.VT_DISPATCH: out.WriteSString("Obj")
		| WinOle.VT_ERROR: out.WriteSString("Res")
		| WinOle.VT_BOOL: out.WriteSString("Bool")
		| WinOle.VT_VARIANT: out.WriteSString("Any")
		| WinOle.VT_UNKNOWN: out.WriteSString("Intfce")
		| WinOle.VT_DECIMAL: out.WriteSString("Decimal")
		| WinOle.VT_I1: out.WriteSString("Byte")
		| WinOle.VT_UI1: out.WriteSString("Byte")
		| WinOle.VT_UI2: out.WriteSString("SInt")
		| WinOle.VT_UI4: out.WriteSString("Int")
		| WinOle.VT_I8: out.WriteSString("LInt")
		| WinOle.VT_UI8: out.WriteSString("LInt")
		| WinOle.VT_INT: out.WriteSString("Int")
		| WinOle.VT_UINT: out.WriteSString("Int")
		| WinOle.VT_VOID: out.WriteSString("Void")
		| WinOle.VT_HRESULT: out.WriteSString("Res")
		| WinOle.VT_PTR: out.WriteSString("Pointer")
		| WinOle.VT_SAFEARRAY: out.WriteSString("Any"); (* t.u.lptdesc *)
		| WinOle.VT_CARRAY: out.WriteSString("Array");
		| WinOle.VT_USERDEFINED: HALT(100)
		| WinOle.VT_LPSTR: out.WriteSString("Str")
		| WinOle.VT_LPWSTR: out.WriteSString("Str")
		| enumerator: out.WriteSString("Enum")
		| ptrVoid: out.WriteSString("Int")
		| record: out.WriteSString("Record")
		| union: out.WriteSString("Union")
		| module: out.WriteSString("Module")
		| class: out.WriteSString("Class")
		END
	END WriteShortType;

	PROCEDURE WriteTypeFlags (flags: SHORTINT; VAR out: TextMappers.Formatter);
	BEGIN
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FAPPOBJECT) THEN out.WriteSString(", appObject") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FCANCREATE) THEN out.WriteSString(", createable") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FLICENSED) THEN out.WriteSString(", licensed") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FPREDECLID) THEN out.WriteSString(", predeclared") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FHIDDEN) THEN out.WriteSString(", hidden") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FCONTROL) THEN out.WriteSString(", control") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FDUAL) THEN out.WriteSString(", dual") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FNONEXTENSIBLE) THEN out.WriteSString(", nonextensible") END;
		IF ODD(flags DIV WinOleAut.TYPEFLAG_FOLEAUTOMATION) THEN out.WriteSString(", oleauto") END
	END WriteTypeFlags;

	PROCEDURE WriteTypeConv  (t: WinOleAut.TYPEDESC; tinfo: WinOleAut.ITypeInfo;
											type, n: INTEGER; VAR out: TextMappers.Formatter);
		VAR ti: WinOleAut.ITypeInfo; vt: INTEGER;
 	BEGIN
		GetDescType(t, tinfo, vt, ti);
		IF (vt = WinOle.VT_DISPATCH) & (ti # NIL) THEN
			WriteTypeName(ti, TRUE, TRUE, out);
			IF type = ret THEN out.WriteSString("(CtlC.VarAny("); out.WriteString(retName); out.WriteString("))")
			ELSIF type = get THEN out.WriteSString("(CtlC.GetAny(this, "); out.WriteInt(n); out.WriteSString("))")
			ELSIF type = par THEN out.WriteSString("(CtlC.VarAny(par["); out.WriteInt(n); out.WriteSString("]))")
			ELSE (* type = refpar *) out.WriteSString("(CtlC.VarRefAny(par["); out.WriteInt(n); out.WriteSString("])[0])")
			END
		ELSE
			IF type = ret THEN
				out.WriteSString("CtlC.Var"); WriteShortType(t, tinfo, out);
				out.WriteChar("("); out.WriteString(retName); out.WriteChar(")")
			ELSIF type = get THEN
				out.WriteSString("CtlC.Get"); WriteShortType(t, tinfo, out);
				out.WriteSString("(this, "); out.WriteInt(n); out.WriteChar(")")
			ELSIF type = par THEN
				out.WriteSString("CtlC.Var"); WriteShortType(t, tinfo, out);
				out.WriteSString("(par["); out.WriteInt(n); out.WriteSString("])")
			ELSE (* type = refpar *)
				out.WriteSString("CtlC.VarRef"); WriteShortType(t, tinfo, out);
				out.WriteSString("(par["); out.WriteInt(n); out.WriteSString("])[0]")
			END
		END
	END WriteTypeConv;

	PROCEDURE IsSpecial (IN type: WinOleAut.TYPEDESC; tinfo: WinOleAut.ITypeInfo): BOOLEAN;
		(* special handling needed for bool, string, dispatch & variant passed by reference *)
		VAR ti: WinOleAut.ITypeInfo; vt: INTEGER;
	BEGIN
		GetDescType(type, tinfo, vt, ti);
		RETURN (vt = WinOle.VT_BOOL) OR (vt = WinOle.VT_BSTR) 
			OR (vt = WinOle.VT_DISPATCH) OR (vt = WinOle.VT_VARIANT)
	END IsSpecial;

	PROCEDURE GetParamType (IN param: WinOleAut.ELEMDESC; tinfo: WinOleAut.ITypeInfo; opts: SET;
										OUT type: WinOleAut.TYPEDESC; OUT kind: SHORTINT);
		VAR flags: SHORTINT; vt: INTEGER; ti: WinOleAut.ITypeInfo;
	BEGIN
		type := param.tdesc; kind := value;
		GetDescType(param.tdesc, tinfo, vt, ti);
		IF (type.vt = WinOle.VT_PTR) & (vt = WinOle.VT_PTR) THEN
			flags := param.u.paramdesc.wParamFlags;
			type := type.u.lptdesc^;
			IF ODD(flags DIV WinOleAut.PARAMFLAG_FIN) THEN
				IF ~ODD(flags DIV WinOleAut.PARAMFLAG_FOUT) THEN kind := varin;
					(* correction for wrong IN attributes *)
					GetDescType(type, tinfo, vt, ti);
					IF (vt # record) & (vt # union) & (vt # WinOle.VT_CARRAY) THEN kind := var END
				ELSE kind := var
				END
			ELSIF ODD(flags DIV WinOleAut.PARAMFLAG_FOUT) THEN kind := varout
			ELSE kind := var
			END
		END
	END GetParamType;

	PROCEDURE ShowVar (VAR var: WinOleAut.PtrVARDESC; tinfo: WinOleAut.ITypeInfo;
									opts: SET; VAR out: TextMappers.Formatter);
		VAR n: INTEGER; name, s, t: WinOle.BSTR; res: COM.RESULT; e: SHORTINT;
	BEGIN
		res := tinfo.GetNames(var.memid, name, 1, n);
		out.WriteTab; out.WriteTab; out.WriteTab; WriteBSTR(name, out);
		WinOleAut.SysFreeString(name);
		IF var.varkind = WinOleAut.VAR_CONST THEN 
			out.WriteSString("* = "); WriteVariant(var.u.lpvarValue^, out)
		ELSE
			IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FREADONLY) THEN out.WriteChar("-")
			ELSE out.WriteChar("*")
			END;
			IF var.varkind = WinOleAut.VAR_STATIC THEN
				res := tinfo.GetDllEntry(var.memid, 0, s, t, e);
				IF res >= 0 THEN
					out.WriteSString(" [");
					IF s # NIL THEN
						out.WriteChar('"'); WriteBSTR(s, out); out.WriteSString('", '); WinOleAut.SysFreeString(s)
					END;
					IF t # NIL THEN out.WriteChar('"'); WriteBSTR(s, out); out.WriteChar('"'); WinOleAut.SysFreeString(t)
					ELSE out.WriteChar('"'); out.WriteInt(e); out.WriteChar('"')
					END;
					out.WriteChar("]")
				END
			END;
			out.WriteSString(": "); WriteType(var.elemdescVar.tdesc, tinfo, opts + {browse}, out)
		END;
		out.WriteChar(";");
		res := tinfo.GetDocumentation(var.memid, NIL, s, NIL, NIL);
		IF (s # NIL) OR (browse IN opts) THEN
			out.WriteSString(" (* ");
			IF s # NIL THEN
				WriteBSTR(s, out);
				IF browse IN opts THEN out.WriteSString(", ") END;
				WinOleAut.SysFreeString(s)
			END;
			IF browse IN opts THEN
				out.WriteSString("id: "); out.WriteIntForm(var.memid, TextMappers.hexadecimal, 8, "0", FALSE);
				IF var.varkind = WinOleAut.VAR_DISPATCH THEN out.WriteSString(", property") END;
				IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FREADONLY) THEN out.WriteSString(", readonly")
				END;
				IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FSOURCE) THEN out.WriteSString(", source") END;
				IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FBINDABLE) THEN out.WriteSString(", bindable")
				END;
				IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FDISPLAYBIND) THEN out.WriteSString(", display")
				END;
				IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FDEFAULTBIND) THEN out.WriteSString(", default")
				END;
				IF ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FHIDDEN) THEN out.WriteSString(", hidden") END;
				IF var.varkind = WinOleAut.VAR_PERINSTANCE THEN
					out.WriteSString(", offset: "); out.WriteInt(var.u.oInst)
				END
			END;
			out.WriteSString(" *)")
		END;
		out.WriteLn
	END ShowVar;

	PROCEDURE ShowParam (VAR param: WinOleAut.ELEMDESC; name: WinOle.BSTR;
										tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR type: WinOleAut.TYPEDESC; flags, kind: SHORTINT; ti: WinOleAut.ITypeInfo; s: WinOle.BSTR; res: COM.RESULT;
	BEGIN
		GetParamType(param, tinfo, opts, type, kind);
		IF kind = var THEN  out.WriteSString("VAR ")
		ELSIF kind = varin THEN out.WriteSString("IN ")
		ELSIF kind = varout THEN out.WriteSString("OUT ")
		END;
		flags := param.u.paramdesc.wParamFlags;
		IF ODD(flags DIV WinOleAut.PARAMFLAG_FLCID) THEN out.WriteSString("(* lcid *) ") END;
		IF ODD(flags DIV WinOleAut.PARAMFLAG_FRETVAL) THEN out.WriteSString("(* retval *) ") END;
(*		
		(* correct parameter name *)
		IF (type.vt = WinOle.VT_PTR) & (type.u.lptdesc.vt = WinOle.VT_USERDEFINED) THEN
			res := tinfo.GetRefTypeInfo(type.u.lptdesc.u.hreftype, ti);
			IF res >= 0 THEN
				res := ti.GetDocumentation(-1, s, NIL, NIL, NIL);
				IF s # NIL THEN
					IF name^ = s^ THEN
						IF name[0] < "a" THEN name[0] := CHR(ORD(name[0]) + 32)
						ELSE name[0] := CHR(ORD(name[0]) - 32)
						END
					END;
					WinOleAut.SysFreeString(s)
				END
			END
		END;
*)		
		WriteBSTR(name, out); out.WriteSString(": ");
		IF (wrapper IN opts) & (type.vt = WinOle.VT_BSTR) & (kind = value) THEN out.WriteSString("ARRAY OF CHAR")
		ELSE WriteType(type, tinfo, opts, out)
		END;
(*
		IF ODD(param.u.paramdesc.wParamFlags DIV WinOleAut.PARAMFLAG_FHASDEFAULT) THEN
			out.WriteSString(" (* := ");
			WriteVariant(param.u.paramdesc.pparamdescex.varDefaultValue, out);
			out.WriteSString(" *)")
		END;
*)
	END ShowParam;

	PROCEDURE ShowWrapper (VAR [nil] param: ARRAY [untagged] OF WinOleAut.ELEMDESC;
										VAR retTyp: WinOleAut.TYPEDESC;
										VAR names: ARRAY OF WinOle.BSTR;
										par, id, invoke: INTEGER; hasRet: BOOLEAN; opts: SET;
										tinfo: WinOleAut.ITypeInfo; VAR out: TextMappers.Formatter);
		VAR i: INTEGER; type: WinOleAut.TYPEDESC; kind: SHORTINT; hasVar: BOOLEAN;
	BEGIN
		IF (invoke = WinOleAut.INVOKE_PROPERTYPUT) & ~hasRet & (par = 1) THEN
			out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
			out.WriteTab; out.WriteTab; out.WriteSString("CtlC.Put");
			WriteShortType(param[0].tdesc, tinfo, out); out.WriteSString("(this, ");
			out.WriteInt(id); out.WriteSString(", ");
			WriteBSTR(names[1], out); out.WriteChar(")"); out.WriteLn
		ELSIF (invoke = WinOleAut.INVOKE_PROPERTYGET) & hasRet & (par = 0) THEN
			out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
			out.WriteTab; out.WriteTab; out.WriteSString("RETURN ");
			WriteTypeConv(retTyp, tinfo, get, id, out); out.WriteLn
		ELSE
			hasVar := FALSE;
			IF par > 0 THEN
				IF ~hasVar THEN out.WriteTab; out.WriteTab; out.WriteSString("VAR"); hasVar := TRUE END;
				out.WriteString(" arg: ARRAY "); out.WriteInt(par);
				out.WriteString(" OF CtlT.Variant;")
			END;
			IF hasRet THEN
				IF ~hasVar THEN out.WriteTab; out.WriteTab; out.WriteSString("VAR"); hasVar := TRUE END;
				out.WriteString(" ret: CtlT.Variant;")
			END;
			i := 0;
			WHILE i < par DO
				GetParamType(param[i], tinfo, opts, type, kind);
				IF (kind # value) & IsSpecial(type, tinfo) THEN
					IF ~hasVar THEN out.WriteTab; out.WriteTab; out.WriteSString("VAR"); hasVar := TRUE END;
					out.WriteChar(" "); WriteBSTR(names[i + 1], out); out.WriteString("_TEMP: CtlT.Variant;")
				END;
				INC(i)
			END;
			IF hasVar THEN out.WriteLn END;
			out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
			i := 0;
			WHILE i < par DO
				GetParamType(param[i], tinfo, opts, type, kind);
				IF (kind IN {var, varin}) & IsSpecial(type, tinfo) THEN
					out.WriteTab; out.WriteTab; out.WriteSString("CtlC.");
					WriteShortType(type, tinfo, out);
					out.WriteString("Var("); WriteBSTR(names[i + 1], out);
					out.WriteString(", "); WriteBSTR(names[i + 1], out);
					out.WriteString("_TEMP);"); out.WriteLn
				END;
				out.WriteTab; out.WriteTab; out.WriteSString("CtlC.");
				IF kind # value THEN out.WriteSString("Ref") END;
				WriteShortType(type, tinfo, out);
				out.WriteSString("Var("); WriteBSTR(names[i + 1], out);
				IF (kind # value) & IsSpecial(type, tinfo) THEN out.WriteString("_TEMP") END;
				out.WriteSString(", arg["); out.WriteInt(par - i - 1);
				out.WriteSString("]);"); out.WriteLn;
				INC(i)
			END;
			out.WriteTab; out.WriteTab;
			IF par = 0 THEN out.WriteSString("CtlC.CallMethod(this, ")
			ELSIF ODD(invoke DIV WinOleAut.INVOKE_PROPERTYGET) THEN
				out.WriteSString("CtlC.CallGetMethod(this, ")
			ELSIF ODD(invoke DIV WinOleAut.INVOKE_PROPERTYPUT) THEN
				out.WriteSString("CtlC.CallPutMethod(this, ")
			ELSIF ODD(invoke DIV WinOleAut.INVOKE_PROPERTYPUTREF) THEN
				out.WriteSString("CtlC.CallPutRefMethod(this, ")
			ELSE out.WriteSString("CtlC.CallParMethod(this, ")
			END;
			out.WriteInt(id);
			IF par > 0 THEN out.WriteSString(", arg") END;
			IF hasRet THEN out.WriteSString(", ret") ELSE out.WriteSString(", NIL") END;
			out.WriteSString(");"); out.WriteLn;
			i := 0;
			WHILE i < par DO
				GetParamType(param[i], tinfo, opts, type, kind);
				IF (kind IN {var, varout}) & IsSpecial(type, tinfo) THEN
					out.WriteTab; out.WriteTab; WriteBSTR(names[i + 1], out);
					out.WriteSString(" := "); retName := names[i + 1]$ + "_TEMP";
					WriteTypeConv(type, tinfo, ret, 0, out);
					out.WriteChar(";"); out.WriteLn
				END;
				INC(i)
			END;
			IF hasRet THEN
				out.WriteTab; out.WriteTab; out.WriteSString("RETURN ");
				retName := "ret";
				WriteTypeConv(retTyp, tinfo, ret, 0, out); out.WriteLn
			END
		END 
	END ShowWrapper;

	PROCEDURE ShowGenerator (tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; name: WinOle.BSTR;
	BEGIN
		res := tinfo.GetTypeAttr(attr);
		res := tinfo.GetDocumentation(-1, name, NIL, NIL, NIL);
		(* !!! *)
		out.WriteTab; out.WriteSString("PROCEDURE This"); WriteBSTR(name, out);
		out.WriteSString("* (v: CtlT.Any): ");  WriteBSTR(name, out);
		out.WriteChar(";"); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteSString("VAR new: ");
		WriteBSTR(name, out); out.WriteChar(";"); out.WriteLn;
		out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteSString("IF v # NIL THEN"); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteTab;
		out.WriteSString('NEW(new); CtlC.InitObj(new, v, "');
		WriteGuid(attr.guid, out);
		out.WriteSString('"); RETURN new'); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteSString("ELSE RETURN NIL"); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteSString("END"); out.WriteLn;
		out.WriteTab; out.WriteSString("END This");
		WriteBSTR(name, out); out.WriteChar(";"); out.WriteLn; out.WriteLn;
		out.WriteTab; out.WriteSString("PROCEDURE Is"); WriteBSTR(name, out);
		out.WriteSString("* (v: CtlT.Any): BOOLEAN;"); out.WriteLn;
		out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
		out.WriteTab; out.WriteTab;
		out.WriteSString('RETURN CtlC.IsObj(v, "');
		WriteGuid(attr.guid, out); out.WriteSString('")');
		out.WriteLn; out.WriteTab; out.WriteSString("END Is");
		WriteBSTR(name, out); out.WriteChar(";"); out.WriteLn; out.WriteLn;
		WinOleAut.SysFreeString(name)
	END ShowGenerator;

	PROCEDURE ShowClassGenerator (tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr, iattr: WinOleAut.PtrTYPEATTR; sc, si: WinOle.BSTR; i: INTEGER;
			t: WinOleAut.HREFTYPE; iinfo: WinOleAut.ITypeInfo; flags: SET;
	BEGIN
		IF (inAll IN opts) OR (inAuto IN opts) THEN
			res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100); i := 0;
			WHILE i < attr.cImplTypes DO
				res := tinfo.GetImplTypeFlags(i, flags);
				IF WinOleAut.IMPLTYPEFLAG_FSOURCE * flags = {} THEN
					res := tinfo.GetRefTypeOfImplType(i, t); ASSERT(res >= 0, 101);
					res := tinfo.GetRefTypeInfo(t, iinfo); ASSERT(res >= 0, 102);
					res := iinfo.GetTypeAttr(iattr); ASSERT(res >= 0, 103);
					IF iattr.typekind = WinOleAut.TKIND_DISPATCH THEN
						res := tinfo.GetDocumentation(-1, sc, NIL, NIL, NIL); ASSERT(res >= 0, 104);
						res := iinfo.GetDocumentation(-1, si, NIL, NIL, NIL); ASSERT(res >= 0, 105);
						(* !!! *)
						IF si # NIL THEN
							out.WriteTab; out.WriteSString("PROCEDURE New"); WriteBSTR(sc, out);
							IF WinOleAut.IMPLTYPEFLAG_FDEFAULT * flags = {}  THEN
								out.WriteChar("_"); WriteBSTR(si, out)
							END;
							out.WriteSString("* (): ");  WriteBSTR(si, out); out.WriteChar(";"); out.WriteLn;
							out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
							out.WriteTab; out.WriteTab;
							out.WriteSString("RETURN This"); WriteBSTR(si, out);
							out.WriteSString('(CtlC.NewObj("'); WriteGuid(attr.guid, out);
							out.WriteSString('"))'); out.WriteLn;
							out.WriteTab; out.WriteSString("END New"); WriteBSTR(sc, out);
							IF WinOleAut.IMPLTYPEFLAG_FDEFAULT * flags = {}  THEN
								out.WriteChar("_"); WriteBSTR(si, out)
							END;
							out.WriteChar(";"); out.WriteLn; out.WriteLn;
							WinOleAut.SysFreeString(sc); WinOleAut.SysFreeString(si)
						END
					END;
					iinfo.ReleaseTypeAttr(iattr)
				END;
				INC(i)
			END;
			tinfo.ReleaseTypeAttr(attr)
		END
	END ShowClassGenerator;

	PROCEDURE ShowProperty (VAR var: WinOleAut.PtrVARDESC; tinfo: WinOleAut.ITypeInfo; opts: SET;
										VAR attr: WinOleAut.PtrTYPEATTR; tn: WinOle.BSTR; VAR out: TextMappers.Formatter);
		VAR n, i: INTEGER; s, t: WinOle.BSTR; res: COM.RESULT; e: SHORTINT;
			names: ARRAY 2 OF WinOle.BSTR; elem: ARRAY 1 OF WinOleAut.ELEMDESC;
	BEGIN
		res := tinfo.GetNames(var.memid, names[0], 1, n);
		out.WriteTab; out.WriteSString("PROCEDURE ");
		out.WriteSString("(this: "); WriteBSTR(tn, out); out.WriteSString(") ");
		WriteBSTR(names[0], out);
		out.WriteSString("* (): "); WriteType(var.elemdescVar.tdesc, tinfo, opts, out);
		out.WriteString(", NEW");
		IF (inAll IN opts) OR (inAuto IN opts) & ~(source IN opts) THEN
			(* Something's wrong here, should choose ABSTRACT. *)
			IF (outAll IN opts) OR (outAuto IN opts) & (source IN opts) THEN out.WriteString(", EXTENSIBLE") END;
			out.WriteChar(";"); out.WriteLn;
			ShowWrapper(elem, var.elemdescVar.tdesc, names, 0, var.memid,
								WinOleAut.INVOKE_PROPERTYGET, TRUE, opts, tinfo, out);
			out.WriteTab; out.WriteSString("END ");
			WriteBSTR(names[0], out);
		ELSE
			out.WriteString(", ABSTRACT")
		END;
		out.WriteChar(";"); out.WriteLn; out.WriteLn;
		IF ~ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FREADONLY) THEN
			out.WriteTab; out.WriteSString("PROCEDURE ");
			out.WriteSString("(this: "); WriteBSTR(tn, out); out.WriteSString(") PUT");
			WriteBSTR(names[0], out);
			out.WriteSString("* (val: ");
			IF var.elemdescVar.tdesc.vt = WinOle.VT_BSTR THEN out.WriteSString("ARRAY OF CHAR")
			ELSE WriteType(var.elemdescVar.tdesc, tinfo, opts, out)
			END;
			out.WriteString("), NEW");
			IF (inAll IN opts) OR (inAuto IN opts) & ~(source IN opts) THEN
				IF (outAll IN opts) OR (outAuto IN opts) & (source IN opts) THEN out.WriteString(", EXTENSIBLE") END;
				out.WriteChar(";"); out.WriteLn;
				elem[0] := var.elemdescVar;
				names[1] := WinOleAut.SysAllocString("val");
				ShowWrapper(elem, var.elemdescVar.tdesc, names, 1, var.memid,
									WinOleAut.INVOKE_PROPERTYPUT, FALSE, opts, tinfo, out);
				WinOleAut.SysFreeString(names[1]);
				out.WriteTab; out.WriteSString("END PUT");
				WriteBSTR(names[0], out);
			ELSE
				out.WriteString(", ABSTRACT")
			END;
			out.WriteChar(";"); out.WriteLn; out.WriteLn;
		END;
		WinOleAut.SysFreeString(names[0]);
	END ShowProperty;

	PROCEDURE ShowFunc (VAR func: WinOleAut.PtrFUNCDESC; tinfo: WinOleAut.ITypeInfo; VAR attr: WinOleAut.PtrTYPEATTR;
									tn: WinOle.BSTR; opts: SET; VAR out: TextMappers.Formatter);
		VAR n, i: INTEGER; names: ARRAY 64 OF WinOle.BSTR; s, t: WinOle.BSTR; res: COM.RESULT; e: SHORTINT;
			retTyp: WinOleAut.TYPEDESC; ti: WinOleAut.ITypeInfo;
	BEGIN
		res := tinfo.GetNames(func.memid, names[0], LEN(names), n);
		IF (wrapper IN opts) & ((func.memid = WinOleAut.DISPID_NEWENUM)
				OR (names[0]^ = "_NewEnum") & (func.cParams = 0)
					& ((func.elemdescFunc.tdesc.vt = WinOle.VT_UNKNOWN)
						OR (func.elemdescFunc.tdesc.vt = WinOle.VT_VARIANT))) THEN
(*
			IF names[0]^ = "_NewEnum" THEN names[0]^ := "NewEnum" END;
*)
			retTyp.vt := enumerator
		ELSE
			retTyp := func.elemdescFunc.tdesc
		END;
		IF ~(wrapper IN opts) OR ~ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FRESTRICTED)
			OR (retTyp.vt = enumerator) THEN
(*
		IF ~(wrapper IN opts) OR (names[0]^ # "QueryInterface") & (names[0]^ # "AddRef") & (names[0]^ # "Release")
										& (names[0]^ # "GetTypeInfoCount") & (names[0]^ # "GetTypeInfo")
										& (names[0]^ # "GetIDsOfNames") & (names[0]^ # "Invoke") THEN
*)
			out.WriteTab; out.WriteSString("PROCEDURE ");
			IF ~(wrapper IN opts) & (func.callconv # WinOleAut.CC_STDCALL) THEN
				out.WriteChar("[");
				IF func.callconv = WinOleAut.CC_CDECL THEN out.WriteSString("ccall")
				ELSIF func.callconv = WinOleAut.CC_PASCAL THEN out.WriteSString("pascal")
				ELSIF func.callconv = WinOleAut.CC_MACPASCAL THEN out.WriteSString("macpascal")
				ELSIF func.callconv = WinOleAut.CC_SYSCALL THEN out.WriteSString("syscall")
				ELSIF func.callconv = WinOleAut.CC_MPWCDECL THEN out.WriteSString("mpwccall")
				ELSIF func.callconv = WinOleAut.CC_MPWPASCAL THEN out.WriteSString("mpwpascal")
				ELSE HALT(100)
				END;
				out.WriteSString("] ")
			END;
			IF (func.funckind = WinOleAut.FUNC_VIRTUAL)
				OR (func.funckind = WinOleAut.FUNC_PUREVIRTUAL)
				OR (func.funckind = WinOleAut.FUNC_DISPATCH) THEN
				out.WriteSString("(this: ");
				WriteBSTR(tn, out); out.WriteSString(") ")
			END;
			IF func.invkind = WinOleAut.INVOKE_PROPERTYPUT THEN out.WriteSString("PUT")
			ELSIF func.invkind = WinOleAut.INVOKE_PROPERTYPUTREF THEN out.WriteSString("PUTREF")
			END;
			WriteBSTR(names[0], out); out.WriteChar("*");
			IF func.funckind = WinOleAut.FUNC_STATIC THEN
				res := tinfo.GetDllEntry(func.memid, func.invkind, s, t, e);
				IF res >= 0 THEN
					out.WriteSString(" [");
					IF s # NIL THEN
						out.WriteChar('"'); WriteBSTR(s, out); out.WriteSString('", '); WinOleAut.SysFreeString(s)
					END;
					IF t # NIL THEN out.WriteChar('"'); WriteBSTR(t, out); out.WriteChar('"'); WinOleAut.SysFreeString(t)
					ELSE out.WriteChar('"'); out.WriteInt(e); out.WriteChar('"')
					END;
					out.WriteChar("]")
				END
			END;
			out.WriteSString(" (");
			WHILE n <= func.cParams DO
				IF n <= 9 THEN
					names[n] := WinOleAut.SysAllocString("p0");
					names[n][1] := SHORT(CHR(n + ORD("0")))
				ELSE
					names[n] := WinOleAut.SysAllocString("p00");
					names[n][1] := SHORT(CHR(n DIV 10 + ORD("0")));
					names[n][2] := SHORT(CHR(n MOD 10 + ORD("0")))
				END;
				INC(n)
			END;
(*		
			(* correct parameter name - return type conflict *)
			IF (n > 1) & (retTyp.vt = WinOle.VT_PTR) & (retTyp.u.lptdesc.vt = WinOle.VT_USERDEFINED) THEN
				res := tinfo.GetRefTypeInfo(retTyp.u.lptdesc.u.hreftype, ti);
				IF res >= 0 THEN
					res := ti.GetDocumentation(-1, s, NIL, NIL, NIL);
					IF s # NIL THEN
						IF names[1]^ = s^ THEN
							IF names[1, 0] < "a" THEN names[1, 0] := CHR(ORD(names[1, 0]) + 32)
							ELSE names[1, 0] := CHR(ORD(names[1, 0]) - 32)
							END
						END;
						WinOleAut.SysFreeString(s)
					END
				END
			END;
*)
			i := 0;
			WHILE i < func.cParams DO
				IF i > 0 THEN out.WriteSString("; ") END;
				IF i = func.cParams - ABS(func.cParamsOpt) THEN out.WriteSString("(* optional *) ") END;
				ShowParam(func.lprgelemdescParam[i], names[i + 1], tinfo, opts, out);
				INC(i)
			END;
			out.WriteChar(")");
			IF retTyp.vt # WinOle.VT_VOID THEN
				out.WriteSString(": "); WriteType(retTyp, tinfo, opts, out)
			END;
			IF (names[0]^ # "Date") & (names[0]^ # "Cy") & (names[0]^ # "Int") THEN
				out.WriteString(", NEW")
			END;
			IF (inAll IN opts) OR (inAuto IN opts) & ~(source IN opts) THEN
				IF (outAll IN opts) OR (outAuto IN opts) & (source IN opts) THEN out.WriteString(", EXTENSIBLE") END
			ELSE
				out.WriteString(", ABSTRACT")
			END;
			out.WriteChar(";"); out.WriteLn;
			res := tinfo.GetDocumentation(func.memid, NIL, s, NIL, NIL);
			IF (s # NIL) OR (browse IN opts) THEN
				out.WriteTab; out.WriteTab; out.WriteSString("(* ");
				IF s # NIL THEN
					WriteBSTR(s, out);
					IF browse IN opts THEN out.WriteSString(", ") END;
					WinOleAut.SysFreeString(s)
				END;
				IF browse IN opts THEN
					out.WriteSString("id: "); out.WriteIntForm(func.memid, TextMappers.hexadecimal, 8, "0", FALSE);
					IF func.memid = attr.memidConstructor THEN out.WriteSString(", contructor") END;
					IF func.memid = attr.memidDestructor THEN out.WriteSString(", destructor") END;
					IF func.funckind = WinOleAut.FUNC_DISPATCH THEN out.WriteSString(", dispatch") END;
					IF func.invkind = WinOleAut.INVOKE_PROPERTYGET THEN out.WriteSString(", get")
					ELSIF func.invkind = WinOleAut.INVOKE_PROPERTYPUT THEN out.WriteSString(", put")
					ELSIF func.invkind = WinOleAut.INVOKE_PROPERTYPUTREF THEN out.WriteSString(", putref")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FRESTRICTED) THEN
						out.WriteSString(", restricted")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FSOURCE) THEN
						out.WriteSString(", source")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FBINDABLE) THEN
						out.WriteSString(", bindable")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FREQUESTEDIT) THEN
						out.WriteSString(", request")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FDISPLAYBIND) THEN
						out.WriteSString(", display")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FDEFAULTBIND) THEN
						out.WriteSString(", default")
					END;
					IF ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FHIDDEN) THEN
						out.WriteSString(", hidden")
					END;
					IF (func.funckind = WinOleAut.FUNC_PUREVIRTUAL)
						OR (func.funckind = WinOleAut.FUNC_VIRTUAL)
					THEN
						out.WriteSString(", offset: "); out.WriteInt(func.oVft)
					END
				END;
				out.WriteSString(" *)"); out.WriteLn
			END;
			res := tinfo.GetMops(func.memid, s);
			IF s # NIL THEN
				out.WriteTab; out.WriteTab; out.WriteSString("(* mops: "); WriteBSTR(s, out);
				out.WriteSString(" *)"); out.WriteLn; WinOleAut.SysFreeString(s)
			END;
			IF func.cScodes > 0 THEN
				out.WriteTab; out.WriteTab; out.WriteSString("(* scodes:"); i := 0;
				WHILE i < func.cScodes DO
					out.WriteChar(" "); out.WriteIntForm(func.lprgscode[i], TextMappers.hexadecimal, 9, "0", TRUE); INC(i)
				END;
				out.WriteSString(" *)"); out.WriteLn
			END;
			IF (inAll IN opts) OR (inAuto IN opts) & ~(source IN opts) THEN
				IF wrapper IN opts THEN
					ShowWrapper(func.lprgelemdescParam^, retTyp, names, func.cParams,
										func.memid, func.invkind, retTyp.vt # WinOle.VT_VOID, opts, tinfo, out)
				END;
				out.WriteTab; out.WriteSString("END ");
				IF func.invkind = WinOleAut.INVOKE_PROPERTYPUT THEN out.WriteSString("PUT")
				ELSIF func.invkind = WinOleAut.INVOKE_PROPERTYPUTREF THEN out.WriteSString("PUTREF")
				END;
				WriteBSTR(names[0], out); out.WriteChar(";"); out.WriteLn
			END;
			out.WriteLn
		END;
		i := 0;
		WHILE i < n DO WinOleAut.SysFreeString(names[i]); INC(i) END
	END ShowFunc;

	PROCEDURE ShowConst (tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; var: WinOleAut.PtrVARDESC;
			i: INTEGER; s, t: WinOle.BSTR; used: BOOLEAN;
	BEGIN
		res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100);
		i := 0; used := FALSE;
		WHILE i < attr.cVars DO
			res := tinfo.GetVarDesc(i, var); ASSERT(res >= 0, 101);
			IF var.varkind = WinOleAut.VAR_CONST THEN
				IF ~used THEN
					out.WriteTab; out.WriteTab; out.WriteSString("(* ");
					res := tinfo.GetDocumentation(-1, s, t, NIL, NIL);
					IF s # NIL THEN WriteBSTR(s, out); WinOleAut.SysFreeString(s) END;
					IF t # NIL THEN out.WriteSString(": "); WriteBSTR(t, out); WinOleAut.SysFreeString(t) END;
					WriteTypeFlags(attr.wTypeFlags, out);
					out.WriteSString(" *)"); out.WriteLn; used := TRUE
				END;
				ShowVar(var, tinfo, opts, out)
			END;
			tinfo.ReleaseVarDesc(var);
			INC(i)
		END;
		tinfo.ReleaseTypeAttr(attr)
	END ShowConst;

	PROCEDURE ShowStatic (tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; var: WinOleAut.PtrVARDESC;
			i: INTEGER; s, t: WinOle.BSTR; used: BOOLEAN;
	BEGIN
		res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100);
		i := 0; used := FALSE;
		WHILE i < attr.cVars DO
			res := tinfo.GetVarDesc(i, var); ASSERT(res >= 0, 101);
			IF var.varkind # WinOleAut.VAR_CONST THEN
				IF ~used THEN
					out.WriteTab; out.WriteTab; out.WriteSString("(* ");
					res := tinfo.GetDocumentation(-1, s, t, NIL, NIL);
					IF s # NIL THEN WriteBSTR(s, out); WinOleAut.SysFreeString(s) END;
					IF t # NIL THEN out.WriteSString(": "); WriteBSTR(t, out); WinOleAut.SysFreeString(t) END;
					WriteTypeFlags(attr.wTypeFlags, out);
					out.WriteSString(" *)"); out.WriteLn; used := TRUE
				END;
				ShowVar(var, tinfo, opts, out)
			END;
			tinfo.ReleaseVarDesc(var);
			INC(i)
		END;
		tinfo.ReleaseTypeAttr(attr)
	END ShowStatic;

	PROCEDURE ShowType (tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; var: WinOleAut.PtrVARDESC; flags: SET;
			i, vt: INTEGER; func: WinOleAut.PtrFUNCDESC; s, s1: WinOle.BSTR; iinfo: WinOleAut.ITypeInfo;
			t: WinOleAut.HREFTYPE;
	BEGIN
		res := tinfo.GetDocumentation(-1, s, s1, NIL, NIL);
		(* !!! *)
		IF s1 # NIL THEN
			out.WriteTab; out.WriteTab; out.WriteSString("(* "); WriteBSTR(s1, out);
			out.WriteSString(" *)"); out.WriteLn; WinOleAut.SysFreeString(s1)
		END;
		res := tinfo.GetMops(-1, s1);
		IF s1 # NIL THEN
			out.WriteTab; out.WriteTab; out.WriteSString("(* Mops: "); WriteBSTR(s1, out);
			out.WriteSString(" *)"); out.WriteLn; WinOleAut.SysFreeString(s1)
		END;
		res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100);
		IF attr.typekind = WinOleAut.TKIND_COCLASS THEN (* !!! *)
			IF wrapper IN opts THEN
				GetInfoType(tinfo, vt, iinfo);
				IF vt = WinOle.VT_DISPATCH THEN
					res := iinfo.GetDocumentation(-1, s1, NIL, NIL, NIL); ASSERT(res >= 0, 104);
					IF s1 # NIL THEN
						out.WriteTab; out.WriteTab;
						WriteBSTR(s, out); out.WriteSString("* = "); WriteBSTR(s1, out);
						out.WriteChar(";"); out.WriteLn;
						WinOleAut.SysFreeString(s1)
					END
				END
			END
		ELSE
			out.WriteTab; out.WriteTab;
			WriteBSTR(s, out); out.WriteSString("* = ");
			IF attr.typekind = WinOleAut.TKIND_ALIAS THEN
				WriteType(attr.tdescAlias, tinfo, opts, out)
			ELSIF attr.typekind = WinOleAut.TKIND_ENUM THEN
				out.WriteString("INTEGER");
			ELSE
				out.WriteSString("POINTER TO ");
				IF wrapper IN opts THEN
					IF (inAll IN opts) OR (inAuto IN opts) & ~(source IN opts) THEN
						IF (outAll IN opts) OR (outAuto IN opts) & (source IN opts) THEN
							out.WriteString("EXTENSIBLE ")
						END
					ELSE
						out.WriteString("ABSTRACT ")
					END;
					out.WriteSString("RECORD ")
				ELSE
					out.WriteSString("RECORD ");
					IF (attr.typekind = WinOleAut.TKIND_INTERFACE) OR (attr.typekind = WinOleAut.TKIND_DISPATCH)
					THEN
						out.WriteSString('["'); WriteGuid(attr.guid, out); out.WriteSString('"] ')
					ELSIF attr.typekind = WinOleAut.TKIND_UNION THEN out.WriteSString("[union] ")
					ELSIF attr.cbAlignment = 1 THEN out.WriteSString("[noalign] ")
					ELSIF attr.cbAlignment = 2 THEN out.WriteSString("[align2] ")
					ELSIF attr.cbAlignment = 8 THEN out.WriteSString("[align8] ")
					ELSE out.WriteSString("[untagged] ")
					END
				END;
				IF attr.cImplTypes > 0 THEN
					ASSERT(attr.cImplTypes = 1, 101);
					res := tinfo.GetRefTypeOfImplType(0, t); ASSERT(res >= 0, 102);
					out.WriteChar("(");
					IF (wrapper IN opts) & ((outAll IN opts) OR (outAuto IN opts) & (source IN opts)) THEN
						out.WriteString("CtlT.OutObject")
					ELSE
						WriteHandleName(t, tinfo, opts, out)
					END;
					out.WriteSString(") ")
				END;
				IF ~(wrapper IN opts) THEN
					IF attr.typekind = WinOleAut.TKIND_DISPATCH THEN
						out.WriteSString("(* dispatch")
					ELSE
						out.WriteSString("(* size: "); out.WriteInt(attr.cbSizeInstance);
						out.WriteSString(", vtbl size: "); out.WriteInt(attr.cbSizeVft)
					END;
					WriteTypeFlags(attr.wTypeFlags, out);
					out.WriteSString(" *)"); out.WriteLn;
					i := 0;
					WHILE i < attr.cVars DO
						res := tinfo.GetVarDesc(i, var); ASSERT(res >= 0, 103);
						IF var.varkind # WinOleAut.VAR_CONST THEN
							ShowVar(var, tinfo, opts, out)
						END;
						tinfo.ReleaseVarDesc(var);
						INC(i)
					END;
					out.WriteTab; out.WriteTab
				END;
				out.WriteSString("END")
			END;
			out.WriteChar(";"); out.WriteLn
		END;
		WinOleAut.SysFreeString(s);
		tinfo.ReleaseTypeAttr(attr)
	END ShowType;

	PROCEDURE ShowInvokeCall (func: WinOleAut.PtrFUNCDESC; tinfo: WinOleAut.ITypeInfo;
											opts: SET; VAR out: TextMappers.Formatter);
		VAR p, n: INTEGER; res: COM.RESULT; name: WinOle.BSTR; type: WinOleAut.TYPEDESC; kind: SHORTINT;
	BEGIN
		res := tinfo.GetNames(func.memid, name, 1, n);
		IF func.elemdescFunc.tdesc.vt # WinOle.VT_VOID THEN	(* function *)
			out.WriteSString("CtlC.");
			IF (wrapper IN opts) & ((func.memid = WinOleAut.DISPID_NEWENUM)
				OR (name^ = "_NewEnum") & (func.cParams = 0)
					& ((func.elemdescFunc.tdesc.vt = WinOle.VT_UNKNOWN)
						OR (func.elemdescFunc.tdesc.vt = WinOle.VT_VARIANT))) THEN out.WriteString("Enum")
			ELSE WriteShortType(func.elemdescFunc.tdesc, tinfo, out)
			END;
			out.WriteSString("Var(")
		END;
		out.WriteSString("this.");
		IF ODD(func.invkind DIV WinOleAut.INVOKE_PROPERTYPUT) THEN out.WriteSString("PUT")
		ELSIF ODD(func.invkind DIV WinOleAut.INVOKE_PROPERTYPUTREF) THEN out.WriteSString("PUTREF")
		END;
		WriteBSTR(name, out); out.WriteChar("(");
		WinOleAut.SysFreeString(name);
		p := 0;
		WHILE p < func.cParams DO
			IF p > 0 THEN out.WriteSString(", ") END;
			GetParamType(func.lprgelemdescParam[p], tinfo, opts, type, kind);
			IF kind = value THEN WriteTypeConv(type, tinfo, par, func.cParams - 1 - p, out)
			ELSE WriteTypeConv(type, tinfo, refpar, func.cParams - 1 - p, out)
			END;
			INC(p)
		END;
		out.WriteChar(")");
		IF func.elemdescFunc.tdesc.vt # WinOle.VT_VOID THEN out.WriteSString(", ret)") END;
		p := 0;
		WHILE p < func.cParams DO
			GetParamType(func.lprgelemdescParam[p], tinfo, opts, type, kind);
			IF (kind # value) & IsSpecial(type, tinfo) THEN
				out.WriteSString("; CtlC.Ret");
				WriteShortType(type, tinfo, out);
				out.WriteString("(par["); out.WriteInt(func.cParams - 1 - p); out.WriteString("])")
			END;
			INC(p)
		END;
	END ShowInvokeCall;

	PROCEDURE ShowInvoke (tinfo: WinOleAut.ITypeInfo; attr: WinOleAut.PtrTYPEATTR;
										tn: WinOle.BSTR; opts: SET; VAR out: TextMappers.Formatter);
		VAR func, pfunc: WinOleAut.PtrFUNCDESC; res: COM.RESULT; i, j, n: INTEGER;
			name: WinOle.BSTR; var: WinOleAut.PtrVARDESC; ifUsed: BOOLEAN;
	BEGIN
		out.WriteTab; out.WriteSString("PROCEDURE (this: "); WriteBSTR(tn, out);
		out.WriteSString(") Invoke* (id, n: INTEGER; VAR par: CtlT.ParList; VAR ret: CtlT.Variant);");
		out.WriteLn; out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteSString("CASE id OF"); out.WriteLn;
		i := 0;
		WHILE i < attr.cVars DO	(* property access *)
			res := tinfo.GetVarDesc(i, var); ASSERT(res >= 0, 101);
			IF var.varkind # WinOleAut.VAR_CONST THEN
				res := tinfo.GetNames(var.memid, name, 1, n);
				out.WriteTab; out.WriteTab; out.WriteSString("| "); out.WriteInt(var.memid);
				out.WriteSString(": ");
				IF ~ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FREADONLY) THEN
					out.WriteSString("IF n = -1 THEN this.PUT");
					WriteBSTR(name, out); out.WriteChar("(");
					WriteTypeConv(var.elemdescVar.tdesc, tinfo, par, 0, out);
					out.WriteChar(")"); out.WriteLn;
					out.WriteTab; out.WriteTab; out.WriteTab; out.WriteSString("ELSE ")
				END;
				out.WriteSString("ASSERT(n = 0, 11); ");
				out.WriteSString("CtlC."); WriteShortType(var.elemdescVar.tdesc, tinfo, out);
				out.WriteSString("Var(this."); WriteBSTR(name, out);
				out.WriteSString("(), ret)"); out.WriteLn;
				IF ~ODD(var.wVarFlags DIV WinOleAut.VARFLAG_FREADONLY) THEN
					out.WriteTab; out.WriteTab; out.WriteTab; out.WriteSString("END"); out.WriteLn
				END;
				WinOleAut.SysFreeString(name)
			END;
			tinfo.ReleaseVarDesc(var);
			INC(i)
		END;
		i := 0;
		WHILE i < attr.cFuncs DO	(* method access *)
			res := tinfo.GetFuncDesc(i, func); ASSERT(res >= 0, 102);
			IF ~ODD(func.wFuncFlags DIV WinOleAut.FUNCFLAG_FRESTRICTED)
					& (func.invkind < WinOleAut.INVOKE_PROPERTYPUT) THEN
				out.WriteTab; out.WriteTab; out.WriteSString("| "); out.WriteInt(func.memid);
				out.WriteSString(": "); ifUsed := FALSE;
				IF func.invkind = WinOleAut.INVOKE_PROPERTYGET THEN
					j := 0;
					WHILE j < attr.cFuncs DO
						res := tinfo.GetFuncDesc(j, pfunc); ASSERT(res >= 0, 103);
						IF (pfunc.memid = func.memid)
								& ~ODD(pfunc.wFuncFlags DIV WinOleAut.FUNCFLAG_FRESTRICTED)
								& (pfunc.invkind >= WinOleAut.INVOKE_PROPERTYPUT) THEN
							IF ifUsed THEN out.WriteTab; out.WriteTab; out.WriteTab; out.WriteSString("ELS") END;
							out.WriteSString("IF n = -");
							IF ODD(pfunc.invkind DIV WinOleAut.INVOKE_PROPERTYPUT) THEN
								out.WriteInt(pfunc.cParams)
							ELSE out.WriteInt(pfunc.cParams + 100)
							END;
							out.WriteSString(" THEN ");
							ShowInvokeCall(pfunc, tinfo, opts, out); out.WriteLn;
							ifUsed := TRUE
						END;
						tinfo.ReleaseFuncDesc(pfunc);
						INC(j)
					END
				END;
				IF ifUsed THEN out.WriteTab; out.WriteTab; out.WriteTab; out.WriteSString("ELSE ") END;
				out.WriteSString("ASSERT(n = "); out.WriteInt(func.cParams); out.WriteSString(", 11); ");
				ShowInvokeCall(func, tinfo, opts, out); out.WriteLn;
				IF ifUsed THEN out.WriteTab; out.WriteTab; out.WriteTab; out.WriteSString("END"); out.WriteLn END
			END;
			tinfo.ReleaseFuncDesc(func);
			INC(i)
		END;
		out.WriteTab; out.WriteTab; out.WriteSString("END"); out.WriteLn;
		out.WriteTab; out.WriteSString("END Invoke;"); out.WriteLn; out.WriteLn;
		(* GetIID *)
		out.WriteTab; out.WriteSString("PROCEDURE (this: "); WriteBSTR(tn, out);
		out.WriteSString(") GetIID* (OUT iid: CtlT.GUID);"); out.WriteLn;
		out.WriteTab; out.WriteSString("BEGIN"); out.WriteLn;
		out.WriteTab; out.WriteTab; out.WriteSString('iid := "');
		WriteGuid(attr.guid, out); out.WriteChar('"'); out.WriteLn;
		out.WriteTab; out.WriteSString("END GetIID;"); out.WriteLn; out.WriteLn
	END ShowInvoke;

	PROCEDURE ShowProcs (tinfo: WinOleAut.ITypeInfo; opts: SET; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; s, t: WinOle.BSTR;
			i: INTEGER; func: WinOleAut.PtrFUNCDESC; var: WinOleAut.PtrVARDESC;
	BEGIN
		res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100);
		IF (wrapper IN opts) OR (attr.cFuncs > 0) THEN
			res := tinfo.GetDocumentation(-1, s, t, NIL, NIL);
			(* !!! *)
			out.WriteLn; out.WriteTab;
			out.WriteSString("(* ---------- ");
			IF s # NIL THEN WriteBSTR(s, out) END;
			IF t # NIL THEN out.WriteSString(": "); WriteBSTR(t, out) END;
			WriteTypeFlags(attr.wTypeFlags, out); out.WriteSString(" ---------- *)");
			out.WriteLn; out.WriteLn;
			IF wrapper IN opts THEN
				i := 0;
				WHILE i < attr.cVars DO
					res := tinfo.GetVarDesc(i, var); ASSERT(res >= 0, 101);
					IF var.varkind # WinOleAut.VAR_CONST THEN
						ShowProperty(var, tinfo, opts, attr, s, out)
					END;
					tinfo.ReleaseVarDesc(var);
					INC(i)
				END
			END;
			i := 0;
			WHILE i < attr.cFuncs DO
				res := tinfo.GetFuncDesc(i, func); ASSERT(res >= 0, 102);
				ShowFunc(func, tinfo, attr, s, opts, out);
				tinfo.ReleaseFuncDesc(func);
				INC(i)
			END;
			IF (wrapper IN opts) & ((outAll IN opts) OR (outAuto IN opts) & (source IN opts)) THEN
				ShowInvoke(tinfo, attr, s, opts, out)
			END;
			WinOleAut.SysFreeString(s);
			WinOleAut.SysFreeString(t)
		END;
		tinfo.ReleaseTypeAttr(attr)
	END ShowProcs;

	PROCEDURE ShowClass (tinfo: WinOleAut.ITypeInfo; VAR out: TextMappers.Formatter);
		VAR res: COM.RESULT; attr: WinOleAut.PtrTYPEATTR; flags: SET;
			i: INTEGER; s, s1: WinOle.BSTR; t: WinOleAut.HREFTYPE;
	BEGIN
		out.WriteTab; out.WriteSString("(* CLASS ");
		res := tinfo.GetDocumentation(-1, s, s1, NIL, NIL);
		IF s # NIL THEN WriteBSTR(s, out) END;
		IF s1 # NIL THEN out.WriteSString(": "); WriteBSTR(s1, out); WinOleAut.SysFreeString(s1) END;
		res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100);
		out.WriteLn; out.WriteTab; out.WriteTab;
		WriteGuid(attr.guid, out); WriteTypeFlags(attr.wTypeFlags, out);
		out.WriteLn;
		i := 0;
		WHILE i < attr.cImplTypes DO
			res := tinfo.GetImplTypeFlags(i, flags);
			res := tinfo.GetRefTypeOfImplType(i, t); ASSERT(res >= 0, 103);
			out.WriteTab; out.WriteTab; out.WriteSString("INTERFACE ");
			WriteHandleName(t, tinfo, {}, out);
			out.WriteTab; out.WriteSString("(* ");
			IF WinOleAut.IMPLTYPEFLAG_FDEFAULT * flags # {} THEN out.WriteSString("default ") END;
			IF WinOleAut.IMPLTYPEFLAG_FDEFAULTVTABLE * flags # {} THEN
				out.WriteSString("defaultVtable ")
			END;
			IF WinOleAut.IMPLTYPEFLAG_FRESTRICTED * flags # {} THEN out.WriteSString("restricted ") END;
			IF WinOleAut.IMPLTYPEFLAG_FSOURCE * flags # {} THEN out.WriteSString("source ") END;
			out.WriteSString("*)"); out.WriteLn; INC(i)
		END;
		out.WriteTab; out.WriteSString("END *)"); out.WriteLn; out.WriteLn;
		WinOleAut.SysFreeString(s);
		tinfo.ReleaseTypeAttr(attr)
	END ShowClass;

	PROCEDURE IsSource (tlib: WinOleAut.ITypeLib; tinfo: WinOleAut.ITypeInfo): BOOLEAN;
		VAR attr, ca, ta: WinOleAut.PtrTYPEATTR; ci, ti: WinOleAut.ITypeInfo; i, j, n: INTEGER;
			res: COM.RESULT; kind: WinOleAut.TYPEKIND; t: WinOleAut.HREFTYPE; flags: SET;
	BEGIN
		res := tinfo.GetTypeAttr(attr); ASSERT(res >= 0, 100);
		i := 0; n := tlib.GetTypeInfoCount();
		WHILE i < n DO
			res := tlib.GetTypeInfoType(i, kind); ASSERT(res >= 0, 101);
			IF kind = WinOleAut.TKIND_COCLASS THEN
				res := tlib.GetTypeInfo(i, ci); ASSERT(res >= 0, 102);
				res := ci.GetTypeAttr(ca); ASSERT(res >= 0, 103);
				j := 0;
				WHILE j < ca.cImplTypes DO
					res := ci.GetImplTypeFlags(j, flags); ASSERT(res >= 0, 104);
					IF WinOleAut.IMPLTYPEFLAG_FSOURCE * flags # {} THEN
						res := ci.GetRefTypeOfImplType(j, t); ASSERT(res >= 0, 105);
						res := ci.GetRefTypeInfo(t, ti); ASSERT(res >= 0, 106);
						res := ti.GetTypeAttr(ta); ASSERT(res >= 0, 107);
						IF ta.guid = attr.guid THEN i := n; j := ca.cImplTypes END;
						ti.ReleaseTypeAttr(ta)
					END;
					INC(j)
				END;
				ci.ReleaseTypeAttr(ca)
			END;
			INC(i)
		END;
		tinfo.ReleaseTypeAttr(attr);
		RETURN i > n
	END IsSource;

	PROCEDURE ShowLibrary (
		tlib: WinOleAut.ITypeLib; name: ARRAY OF CHAR; opts: SET; VAR out: TextMappers.Formatter
	);
		VAR s1, s2, s3: WinOle.BSTR; str: ARRAY 256 OF SHORTCHAR; i, n, impPos: INTEGER; res: COM.RESULT;
			attr: WinOleAut.PtrTLIBATTR; tinfo, ti: WinOleAut.ITypeInfo; kind: WinOleAut.TYPEKIND;
			t: WinOleAut.HREFTYPE;
	BEGIN
		res := tlib.GetDocumentation(-1, s1, s2, n, s3); ASSERT(res >= 0, 100);
		modules[0] := s1$; WinOleAut.SysFreeString(s1);
		modules[1] := "CtlT"; modules[2] := "CtlC"; noMod := 3;
		IF wrapper IN opts THEN modules[0] := "Ctl" + modules[0] END;
		out.WriteSString("MODULE ");
		out.WriteString(name);
		IF interface IN opts THEN out.WriteSString(' [""]') END;
		out.WriteChar(";"); out.WriteLn; out.WriteTab; out.WriteSString("(* ");
		IF s2 # NIL THEN WriteBSTR(s2, out); WinOleAut.SysFreeString(s2) END;
		out.WriteSString(", help: ");
		IF s3 # NIL THEN WriteBSTR(s3, out); WinOleAut.SysFreeString(s3) END;
		out.WriteSString(", id: "); out.WriteInt(n); out.WriteSString(" *)"); out.WriteLn;
		res := tlib.GetLibAttr(attr); ASSERT(res >= 0, 102);
		out.WriteTab; out.WriteSString("(* guid: "); WriteGuid(attr.guid, out);
		out.WriteSString(", lcid: "); out.WriteInt(attr.lcid);
		out.WriteSString(", syskind: ");
		IF attr.syskind = WinOleAut.SYS_WIN16 THEN out.WriteSString("win16")
		ELSIF attr.syskind = WinOleAut.SYS_WIN32 THEN out.WriteSString("win32")
		ELSIF attr.syskind = WinOleAut.SYS_MAC THEN out.WriteSString("mac")
		ELSE out.WriteInt(attr.syskind)
		END;
		out.WriteSString(", version: ");
		out.WriteInt(attr.wMajorVerNum);
		out.WriteChar(".");
		out.WriteInt(attr.wMinorVerNum);
		IF ODD(attr.wLibFlags DIV WinOleAut.LIBFLAG_FRESTRICTED) THEN out.WriteSString(", restricted") END;
		IF ODD(attr.wLibFlags DIV WinOleAut.LIBFLAG_FCONTROL) THEN out.WriteSString(", control") END;
		IF ODD(attr.wLibFlags DIV WinOleAut.LIBFLAG_FHIDDEN) THEN out.WriteSString(", hidden") END;
		out.WriteSString(" *)"); out.WriteLn; out.WriteLn;
		impPos := out.Pos();
		tlib.ReleaseTLibAttr(attr);
		n := tlib.GetTypeInfoCount();
		out.WriteTab; out.WriteSString("CONST"); out.WriteLn; i := 0;
		WHILE i < n DO
			res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 103);
			ShowConst(tinfo, opts, out);
			INC(i)
		END;
		out.WriteLn; out.WriteLn;
		out.WriteTab; out.WriteSString("TYPE"); out.WriteLn; i := 0;
		WHILE i < n DO
			res := tlib.GetTypeInfoType(i, kind); ASSERT(res >= 0, 104);
			IF (kind = WinOleAut.TKIND_ALIAS) OR (kind = WinOleAut.TKIND_ENUM)
					OR ~(wrapper IN opts) & (kind IN {WinOleAut.TKIND_RECORD, WinOleAut.TKIND_INTERFACE,
																WinOleAut.TKIND_ALIAS, WinOleAut.TKIND_UNION})
					OR ~(interface IN opts) & (kind IN {WinOleAut.TKIND_DISPATCH, WinOleAut.TKIND_COCLASS})
			THEN
				res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 105);
				IF (wrapper IN opts) & IsSource(tlib, tinfo) THEN
					ShowType(tinfo, opts + {source}, out)
				ELSE ShowType(tinfo, opts, out)
				END
			END;
			IF (~(wrapper IN opts)) & (kind = WinOleAut.TKIND_DISPATCH) THEN
				res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 105);
				res := tinfo.GetRefTypeOfImplType(-1, t);
				IF res >= 0 THEN	(* dual interfaced *)
					res := tinfo.GetRefTypeInfo(t, ti); ASSERT(res >= 0, 106);
					ShowType(ti, opts, out)
				END
			END;
			INC(i)
		END;
		out.WriteLn; out.WriteLn;
		IF ~(wrapper IN opts) THEN
			out.WriteTab; out.WriteSString("VAR"); out.WriteLn;
			i := 0;
			WHILE i < n DO
				res := tlib.GetTypeInfoType(i, kind); ASSERT(res >= 0, 107);
				IF kind = WinOleAut.TKIND_MODULE THEN
					res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 108);
					ShowStatic(tinfo, opts, out)
				END;
				INC(i)
			END;
			out.WriteLn; out.WriteLn
		ELSE
			i := 0;
			WHILE i < n DO
				res := tlib.GetTypeInfoType(i, kind); ASSERT(res >= 0, 109);
				IF (wrapper IN opts) & (kind = WinOleAut.TKIND_DISPATCH) THEN
					res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 110);
					IF (inAll IN opts) OR (inAuto IN opts) & ~IsSource(tlib, tinfo) THEN
						ShowGenerator(tinfo, opts, out)
					END
				END;
				INC(i)
			END;
			out.WriteLn
		END;
		i := 0;
		WHILE i < n DO
			res := tlib.GetTypeInfoType(i, kind); ASSERT(res >= 0, 109);
			IF ~(wrapper IN opts) & (kind IN {WinOleAut.TKIND_MODULE, WinOleAut.TKIND_INTERFACE})
					OR ~(interface IN opts) & (kind = WinOleAut.TKIND_DISPATCH) THEN
				res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 110);
				IF (wrapper IN opts) & IsSource(tlib, tinfo) THEN
					ShowProcs(tinfo, opts + {source}, out)
				ELSE ShowProcs(tinfo, opts, out)
				END
			END;
			IF ~(wrapper IN opts) & (kind = WinOleAut.TKIND_DISPATCH) THEN
				res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 110);
				res := tinfo.GetRefTypeOfImplType(-1, t);
				IF res >= 0 THEN	(* dual interfaced *)
					res := tinfo.GetRefTypeInfo(t, ti); ASSERT(res >= 0, 111);
					ShowProcs(ti, opts, out)
				END
			END;
			INC(i)
		END;
		out.WriteLn; i := 0;
		WHILE i < n DO
			res := tlib.GetTypeInfoType(i, kind); ASSERT(res >= 0, 112);
			IF kind = WinOleAut.TKIND_COCLASS THEN
				res := tlib.GetTypeInfo(i, tinfo); ASSERT(res >= 0, 113);
				IF wrapper IN opts THEN ShowClassGenerator(tinfo, opts, out)
				ELSE ShowClass(tinfo, out)
				END
			END;
			INC(i)
		END;
		out.WriteSString("END ");
		out.WriteString(name); out.WriteChar("."); out.WriteLn;
		IF (wrapper IN opts) & (noMod > 1) THEN
			out.SetPos(impPos);
			out.WriteTab; out.WriteSString("IMPORT "); i := 1;
			WHILE i < noMod DO
				IF i > 1 THEN out.WriteSString(", ") END;
				out.WriteString(modules[i]); INC(i)
			END;
			out.WriteChar(";"); out.WriteLn; out.WriteLn
		END;
	END ShowLibrary;

	PROCEDURE AutomationInterface* (
		fileName: ARRAY 256 OF CHAR; modName: ARRAY 64 OF CHAR
	): TextModels.Model;
		VAR t: TextModels.Model; out: TextMappers.Formatter; res: COM.RESULT; tlib: WinOleAut.ITypeLib;
	BEGIN
		t := TextModels.dir.New();
		out.ConnectTo(t);
		res := WinOleAut.LoadTypeLib(fileName, tlib);
		IF res >= 0 THEN
			ShowLibrary(tlib, modName, {wrapper, inAuto, outAuto}, out)
		END;
		out.ConnectTo(NIL);
		RETURN t
	END AutomationInterface;
	
	PROCEDURE CustomInterface* (
		fileName: ARRAY 256 OF CHAR; modName: ARRAY 64 OF CHAR
	): TextModels.Model;
	(* not yet tested *)
		VAR t: TextModels.Model; out: TextMappers.Formatter; res: COM.RESULT; tlib: WinOleAut.ITypeLib;
	BEGIN
		t := TextModels.dir.New();
		out.ConnectTo(t);
		res := WinOleAut.LoadTypeLib(fileName, tlib);
		IF res >= 0 THEN
			ShowLibrary(tlib, modName, {interface}, out)
		END;
		out.ConnectTo(NIL);
		RETURN t
	END CustomInterface;
	
	PROCEDURE Browse* (fileName: ARRAY 256 OF CHAR; modName: ARRAY 64 OF CHAR): TextModels.Model;
	(* not yet tested *)
		VAR t: TextModels.Model; out: TextMappers.Formatter; res: COM.RESULT; tlib: WinOleAut.ITypeLib;
	BEGIN
		t := TextModels.dir.New();
		out.ConnectTo(t);
		res := WinOleAut.LoadTypeLib(fileName, tlib);
		IF res >= 0 THEN
			ShowLibrary(tlib, modName, {browse}, out)
		END;
		out.ConnectTo(NIL);
		RETURN t
	END Browse;

END DevTypeLibs.

DevComInterfaceGen
DevTypeLibs

