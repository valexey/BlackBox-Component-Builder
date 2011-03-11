MODULE ComEnumRect;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	references	= "adapted from EnumRect sample in "Inside OLE", 2nd ed."
	changes	= ""
	issues	= ""

**)


	IMPORT COM, WinApi;
	
	
	CONST rects = 15;
	
	
	TYPE
		IEnumRECT* = POINTER TO
								ABSTRACT RECORD ["{00021140-0000-0000-C000-000000000046}"] (COM.IUnknown) END;
		
		
		EnumRECT = POINTER TO RECORD (IEnumRECT)
			cur: INTEGER;
			data: ARRAY rects OF WinApi.RECT;
		END;
		
		
	(* ---------- abstract interface methods ---------- *)
	
	(* QueryInterface, AddRef, Release inherited from COM.IUnknown *)
		
	PROCEDURE (this: IEnumRECT) Next* (num: INTEGER; OUT elem: ARRAY [untagged] OF WinApi.RECT;
														OUT [nil] fetched: INTEGER): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IEnumRECT) Skip* (num: INTEGER): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IEnumRECT) Reset* (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IEnumRECT) Clone* (OUT enum: IEnumRECT): COM.RESULT, NEW, ABSTRACT;
	
		
	(* ---------- interface implementation ---------- *)
	
	(* use default QueryInterface implementation *)
	
	(* AddRef & Release implemented implicitly by the compiler *)
	
	PROCEDURE (this: EnumRECT) Next (num: INTEGER; OUT elem: ARRAY [untagged] OF WinApi.RECT;
														OUT [nil] fetched: INTEGER): COM.RESULT;
		VAR n: INTEGER;
	BEGIN
		n := 0;
		IF VALID(fetched) THEN fetched := 0
		ELSIF num # 1 THEN RETURN WinApi.S_FALSE
		END;
		IF this.cur < rects THEN
			WHILE (this.cur < rects) & (num > 0) DO
				elem[n] := this.data[this.cur];
				INC(this.cur); INC(n); DEC(num)
			END;
			IF VALID(fetched) THEN fetched := n END;
			RETURN WinApi.S_OK
		END;
		RETURN WinApi.S_FALSE
	END Next;
	
	PROCEDURE (this: EnumRECT) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < rects THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: EnumRECT) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: EnumRECT) Clone (OUT enum: IEnumRECT): COM.RESULT;
		VAR new: EnumRECT;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := this.cur;
			new.data := this.data;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	PROCEDURE CreateRectEnumerator* (OUT enum: IEnumRECT);
		VAR new: EnumRECT; i: INTEGER;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0; i := 0;
			WHILE i < rects DO
				new.data[i].left := i;
				new.data[i].top := i * 2;
				new.data[i].right := i * 3;
				new.data[i].bottom := i * 4;
				INC(i)
			END;
			enum := new
		END
	END CreateRectEnumerator;
		

END ComEnumRect.
