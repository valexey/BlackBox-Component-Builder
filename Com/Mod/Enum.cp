MODULE ComEnum;
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

	IMPORT COM, WinApi, WinOle, ComTools;
	
	TYPE
		IEnumUnknown = POINTER TO RECORD (WinOle.IEnumUnknown)
			cur: INTEGER;
			num: INTEGER;
			data: POINTER TO ARRAY OF COM.IUnknown
		END;
		
		IEnumString = POINTER TO RECORD (WinOle.IEnumString)
			cur: INTEGER;
			num: INTEGER;
			data: POINTER TO ARRAY OF ARRAY OF CHAR
		END;
		
		IEnumFORMATETC = POINTER TO RECORD (WinOle.IEnumFORMATETC)
			cur: INTEGER;
			num: INTEGER;
			format: POINTER TO ARRAY OF INTEGER;
			aspect, tymed: POINTER TO ARRAY OF SET
		END;
		
		IEnumOLEVERB = POINTER TO RECORD (WinOle.IEnumOLEVERB)
			cur: INTEGER;
			num: INTEGER;
			verb: POINTER TO ARRAY OF INTEGER;
			name: POINTER TO ARRAY OF ARRAY OF CHAR;
			flags, attribs: POINTER TO ARRAY OF SET
		END;
		

	(* IEnumUnknown  *)
	
	PROCEDURE CreateIEnumUnknown* (num: INTEGER; IN data: ARRAY OF COM.IUnknown;
														OUT enum: WinOle.IEnumUnknown);
		VAR i, n: INTEGER; new: IEnumUnknown;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			NEW(new.data, num);
			i := 0;
			WHILE i < num DO new.data[i] := data[i]; INC(i) END;
			enum := new
		END
	END CreateIEnumUnknown;
	
	PROCEDURE (this: IEnumUnknown) Next (num: INTEGER; OUT elem: ARRAY [untagged] OF COM.IUnknown;
																								OUT [nil] fetched: INTEGER): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.E_POINTER
		END;
		IF this.cur < this.num THEN
			WHILE (this.cur < this.num) & (num > 0) DO
				elem[n] := this.data[this.cur];
				INC(this.cur); INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: IEnumUnknown) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: IEnumUnknown) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: IEnumUnknown) Clone (OUT enum: WinOle.IEnumUnknown): COM.RESULT;
		VAR new: IEnumUnknown;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.num := this.num;
			new.cur := this.cur;
			new.data := this.data;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	(* IEnumString  *)
	
	PROCEDURE CreateIEnumString* (num: INTEGER; IN data: ARRAY OF ARRAY OF CHAR;
															OUT enum: WinOle.IEnumString);
		VAR i, n: INTEGER; new: IEnumString;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			NEW(new.data, num, LEN(data, 1));
			i := 0;
			WHILE i < num DO new.data[i] := data[i]$; INC(i) END;
			enum := new
		END
	END CreateIEnumString;
	
	PROCEDURE (this: IEnumString) Next (num: INTEGER; OUT elem: ARRAY [untagged] OF WinApi.PtrWSTR;
																								OUT [nil] fetched: INTEGER): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.E_POINTER
		END;
		IF this.cur < this.num THEN
			WHILE (this.cur < this.num) & (num > 0) DO
				elem[n] := ComTools.NewString(this.data[this.cur]);
				INC(this.cur); INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: IEnumString) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: IEnumString) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: IEnumString) Clone (OUT enum: WinOle.IEnumString): COM.RESULT;
		VAR new: IEnumString;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.num := this.num;
			new.cur := this.cur;
			new.data := this.data;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	(* IEnumFORMATETC  *)
	
	PROCEDURE CreateIEnumFORMATETC* (num: INTEGER; IN format: ARRAY OF INTEGER;
														IN aspect, tymed: ARRAY OF SET;
														OUT enum: WinOle.IEnumFORMATETC);
		VAR i, n: INTEGER; new: IEnumFORMATETC;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			NEW(new.format, num);
			NEW(new.aspect, num);
			NEW(new.tymed, num);
			i := 0;
			WHILE i < num DO
				new.format[i] := format[i];
				new.aspect[i] := aspect[i];
				new.tymed[i] := tymed[i];
				INC(i)
			END;
			enum := new
		END
	END CreateIEnumFORMATETC;
	
	PROCEDURE (this: IEnumFORMATETC) Next (num: INTEGER;
																			OUT elem: ARRAY [untagged] OF WinOle.FORMATETC;
																			OUT [nil] fetched: INTEGER): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.E_POINTER
		END;
		IF this.cur < this.num THEN
			WHILE (this.cur < this.num) & (num > 0) DO
				ComTools.GenFormatEtc(SHORT(this.format[this.cur]), this.aspect[this.cur], this.tymed[this.cur], elem[n]);
				INC(this.cur); INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: IEnumFORMATETC) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: IEnumFORMATETC) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: IEnumFORMATETC) Clone (OUT enum: WinOle.IEnumFORMATETC): COM.RESULT;
		VAR new: IEnumFORMATETC;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.num := this.num;
			new.cur := this.cur;
			new.format := this.format;
			new.aspect := this.aspect;
			new.tymed := this.tymed;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	(* IEnumOLEVERB  *)
	
	PROCEDURE CreateIEnumOLEVERB* (num: INTEGER; IN verb: ARRAY OF INTEGER;
														IN name: ARRAY OF ARRAY OF CHAR;
														IN flags, attribs: ARRAY OF SET;
														OUT enum: WinOle.IEnumOLEVERB);
		VAR i, n: INTEGER; new: IEnumOLEVERB;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			NEW(new.verb, num);
			NEW(new.name, num, LEN(name, 1));
			NEW(new.flags, num);
			NEW(new.attribs, num);
			i := 0;
			WHILE i < num DO
				new.verb[i] := verb[i];
				new.name[i] := name[i]$;
				new.flags[i] := flags[i];
				new.attribs[i] := attribs[i];
				INC(i)
			END;
			enum := new
		END
	END CreateIEnumOLEVERB;
	
	PROCEDURE (this: IEnumOLEVERB) Next (num: INTEGER; OUT elem: ARRAY [untagged] OF WinOle.OLEVERB;
																								OUT [nil] fetched: INTEGER): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.E_POINTER
		END;
		IF this.cur < this.num THEN
			WHILE (this.cur < this.num) & (num > 0) DO
				elem[n].lVerb := this.verb[this.cur];
				elem[n].lpszVerbName := ComTools.NewString(this.name[this.cur]);
				elem[n].fuFlags := this.flags[this.cur];
				elem[n].grfAttribs := this.attribs[this.cur];
				INC(this.cur); INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: IEnumOLEVERB) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: IEnumOLEVERB) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: IEnumOLEVERB) Clone (OUT enum: WinOle.IEnumOLEVERB): COM.RESULT;
		VAR new: IEnumOLEVERB;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.num := this.num;
			new.cur := this.cur;
			new.verb := this.verb;
			new.name := this.name;
			new.flags := this.flags;
			new.attribs := this.attribs;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
END ComEnum.

