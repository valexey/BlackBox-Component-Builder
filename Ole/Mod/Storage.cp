MODULE OleStorage;
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

	IMPORT Log,
		SYSTEM, COM, WinApi, WinOle, Files, Stores, Views;
	
	CONST
		debug = FALSE;
		obfTag = 6F4F4443H;
		
	TYPE
		ILockBytes = POINTER TO RECORD (WinOle.ILockBytes)
			org: INTEGER;
			len: INTEGER;	(* org + len <= file.Length() *)
			file: Files.File;	(* file # NIL *)
			rd: Files.Reader;	(* (rd # NIL) & (rd.Base() = file) *)
			wr: Files.Writer	(* (wr = NIL) OR (wr.Base() = file) *)
		END;
	
		StreamFile = POINTER TO RECORD (Files.File)
			stream: WinOle.IStream;
			org: LONGINT;
			len: INTEGER;
			pos: INTEGER	(* actual seek pos of stream *)
		END;
		
		StreamReader = POINTER TO RECORD (Files.Reader)
			base: StreamFile;
			pos: INTEGER
		END;
		
		StreamWriter = POINTER TO RECORD (Files.Writer)
			base: StreamFile;
			pos: INTEGER;
		END;
	
	
	(* ---------- ILockBytes ---------- *)
	
	PROCEDURE (this: ILockBytes) ReadAt (offset: LONGINT; buf: WinApi.PtrVoid;
															len: INTEGER; OUT [nil] read: INTEGER): COM.RESULT;
		TYPE P = POINTER TO ARRAY [untagged] 2000000000 OF BYTE;
		VAR p: P;
	BEGIN
		IF debug THEN
			Log.String("read@"); Log.Int(SHORT(offset)); Log.Int(len);
		END;
		IF (len > 0) & (offset < this.len) THEN
			this.rd.SetPos(SHORT(this.org + offset));
			IF offset + len > this.len THEN len := SHORT(this.len - offset) END;
			p := SYSTEM.VAL(P, buf);
			this.rd.ReadBytes(p^, 0, len)
		ELSE len := 0
		END;
		IF VALID(read) THEN read := len END;
		IF debug THEN
			Log.Int(len);
			Log.Int(p[0]); Log.Int(p[1]); Log.Int(p[2]); Log.Int(p[3]); 
			Log.Ln
		END;
		RETURN WinApi.S_OK
	END ReadAt;
	
	PROCEDURE (this: ILockBytes) WriteAt (offset: LONGINT; buf: WinApi.PtrVoid;
																len: INTEGER; OUT [nil] written: INTEGER): COM.RESULT;
		TYPE P = POINTER TO ARRAY [untagged] 2000000000 OF BYTE;
		VAR res: COM.RESULT; p: P;
	BEGIN
		p := SYSTEM.VAL(P, buf);
		IF debug THEN
			Log.String("write@"); Log.Int(SHORT(offset)); Log.Int(len);
			Log.Int(p[0]); Log.Int(p[1]); Log.Int(p[2]); Log.Int(p[3]); 
			Log.Ln
		END;
		IF this.wr # NIL THEN
			IF len > 0 THEN
				IF offset > this.len THEN res := this.SetSize(offset) END;
				this.wr.SetPos(SHORT(this.org + offset));
				this.wr.WriteBytes(p^, 0, len);
				IF offset + len > this.len THEN this.len := SHORT(offset + len) END
			ELSE len := 0
			END;
			IF VALID(written) THEN written := len END;
			IF debug THEN Log.String("written"); Log.Ln END;
			RETURN WinApi.S_OK
		ELSE
			IF VALID(written) THEN written := 0 END;
			RETURN WinApi.STG_E_ACCESSDENIED
		END
	END WriteAt;
	
	PROCEDURE (this: ILockBytes) Flush (): COM.RESULT;
		TYPE A4 = ARRAY 4 OF BYTE;
	BEGIN
		IF debug THEN Log.String("flush"); Log.Ln END;
		IF this.wr # NIL THEN
			this.wr.SetPos(this.org - 4);
			this.wr.WriteBytes(SYSTEM.VAL(A4, this.len), 0, 4);
			this.file.Flush();
			this.wr.SetPos(this.org + this.len)
		ELSE
			this.rd.SetPos(this.org + this.len)
		END;
		IF debug THEN Log.String("flushed"); Log.Ln END;
		RETURN WinApi.S_OK
	END Flush;
	
	PROCEDURE (this: ILockBytes) SetSize (size: LONGINT): COM.RESULT;
		VAR len: INTEGER; buf: ARRAY 256 OF BYTE;
	BEGIN
		IF debug THEN Log.String("set size"); Log.Int(SHORT(size)); Log.Ln END;
		IF this.wr # NIL THEN
			IF size > this.len THEN	(* enlarge file *)
				this.wr.SetPos(this.org + this.len);
				len := SHORT(size - this.len);
				WHILE len > 256 DO this.wr.WriteBytes(buf, 0, 256); DEC(len, 256) END;
				this.wr.WriteBytes(buf, 0, len);
			END;
			this.len := SHORT(size);
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.STG_E_ACCESSDENIED
		END
	END SetSize;
	
	PROCEDURE (this: ILockBytes) LockRegion (offset, len: LONGINT; lockType: SET): COM.RESULT;
	BEGIN
		RETURN WinApi.STG_E_INVALIDFUNCTION
	END LockRegion;
	
	PROCEDURE (this: ILockBytes) UnlockRegion (offset, len: LONGINT; lockType: SET): COM.RESULT;
	BEGIN
		RETURN WinApi.STG_E_INVALIDFUNCTION
	END UnlockRegion;
	
	PROCEDURE (this: ILockBytes) Stat (OUT statStg: WinOle.STATSTG; statflag: INTEGER): COM.RESULT;
	BEGIN
		IF debug THEN Log.String("stat"); Log.Ln END;
		statStg.pwcsName := NIL;
		statStg.type := WinOle.STGTY_LOCKBYTES;
		statStg.cbSize := this.len;
		statStg.mtime.dwLowDateTime := 0;
		statStg.mtime.dwHighDateTime := 0;
		statStg.ctime.dwLowDateTime := 0;
		statStg.ctime.dwHighDateTime := 0;
		statStg.atime.dwLowDateTime := 0;
		statStg.atime.dwHighDateTime := 0;
		statStg.grfMode := WinOle.STGM_DIRECT + WinOle.STGM_CREATE + WinOle.STGM_SHARE_EXCLUSIVE;
		IF this.wr # NIL THEN statStg.grfMode := statStg.grfMode + WinOle.STGM_READWRITE
		ELSE statStg.grfMode := statStg.grfMode + WinOle.STGM_READ
		END;
		statStg.grfLocksSupported := {};
		statStg.clsid := WinOle.GUID_NULL;
		statStg.grfStateBits := {};
		statStg.dwStgFmt := WinOle.STGFMT_FILE;
		IF debug THEN Log.String("stat end"); Log.Ln END;
		RETURN WinApi.S_OK
	END Stat;
	
	
	(* ---------- StreamReader ---------- *)
	
	PROCEDURE (r: StreamReader) Base (): Files.File;
	BEGIN
		RETURN r.base
	END Base;
	
	PROCEDURE (r: StreamReader) Pos (): INTEGER;
	BEGIN
		RETURN r.pos
	END Pos;
	
	PROCEDURE (r: StreamReader) SetPos (pos: INTEGER);
		VAR res: COM.RESULT;
	BEGIN
		ASSERT(pos >= 0, 22); ASSERT(pos <= r.base.len, 21);
		r.pos := pos; r.eof := FALSE
	END SetPos;
	
	PROCEDURE (r: StreamReader) ReadByte (OUT x: BYTE);
		VAR res: COM.RESULT;
	BEGIN
		IF r.pos < r.base.len THEN
			IF r.pos # r.base.pos THEN
				res := r.base.stream.Seek(r.base.org + r.pos, WinOle.STREAM_SEEK_SET, NIL);
				r.base.pos := r.pos
			END;
			res := r.base.stream.Read(SYSTEM.ADR(x), 1, NIL);
			INC(r.pos); INC(r.base.pos);
		ELSE
			x := 0; r.eof := TRUE
		END
	END ReadByte;
	
	PROCEDURE (r: StreamReader) ReadBytes (VAR x: ARRAY OF BYTE; beg, len: INTEGER);
		VAR res: COM.RESULT;
	BEGIN
		ASSERT(beg >= 0, 21);
		IF len > 0 THEN
			ASSERT(beg + len <= LEN(x), 23);
			IF r.pos + len <= r.base.len THEN
				IF r.pos # r.base.pos THEN
					res := r.base.stream.Seek(r.base.org + r.pos, WinOle.STREAM_SEEK_SET, NIL);
					r.base.pos := r.pos
				END;
				res := r.base.stream.Read(SYSTEM.ADR(x[beg]), len, NIL);
				INC(r.pos, len); INC(r.base.pos, len)
			ELSE
				r.eof := TRUE
			END
		ELSE ASSERT(len = 0, 22)
		END
	END ReadBytes;
	
	
	(* ---------- StreamWriter ---------- *)
	
	PROCEDURE (w: StreamWriter) Base (): Files.File;
	BEGIN
		RETURN w.base
	END Base;
	
	PROCEDURE (w: StreamWriter) Pos (): INTEGER;
	BEGIN
		RETURN w.pos
	END Pos;
	
	PROCEDURE (w: StreamWriter) SetPos (pos: INTEGER);
		VAR res: COM.RESULT;
	BEGIN
		ASSERT(pos >= 0, 22); ASSERT(pos <= w.base.len, 21);
		w.pos := pos
	END SetPos;
	
	PROCEDURE (w: StreamWriter) WriteByte (x: BYTE);
		VAR res: COM.RESULT;
	BEGIN
		IF w.pos # w.base.pos THEN
			res := w.base.stream.Seek(w.base.org + w.pos, WinOle.STREAM_SEEK_SET, NIL);
			w.base.pos := w.pos
		END;
		res := w.base.stream.Write(SYSTEM.ADR(x), 1, NIL);
		INC(w.pos); INC(w.base.pos);
		IF w.pos > w.base.len THEN w.base.len := w.pos END
	END WriteByte;
	
	PROCEDURE (w: StreamWriter) WriteBytes (IN x: ARRAY OF BYTE; beg, len: INTEGER);
		VAR res: COM.RESULT;
	BEGIN
		ASSERT(beg >= 0, 21);
		IF len > 0 THEN
			ASSERT(beg + len <= LEN(x), 23);
			IF w.pos # w.base.pos THEN
				res := w.base.stream.Seek(w.base.org + w.pos, WinOle.STREAM_SEEK_SET, NIL);
				w.base.pos := w.pos
			END;
			res := w.base.stream.Write(SYSTEM.ADR(x[beg]), len, NIL);
			INC(w.pos, len); INC(w.base.pos, len); 
			IF w.pos > w.base.len THEN w.base.len := w.pos END
		ELSE ASSERT(len = 0, 22)
		END
	END WriteBytes;
	
	
	(* ---------- StreamFile ---------- *)
	
	PROCEDURE (f: StreamFile) Length (): INTEGER;
	BEGIN
		RETURN f.len
	END Length;
	
	PROCEDURE (f: StreamFile) NewReader (old: Files.Reader): Files.Reader;
		VAR r: StreamReader;
	BEGIN
		ASSERT(f.stream # NIL, 20);
		NEW(r); r.base := f;
		r.pos := 0; r.eof := FALSE;
		RETURN r
	END NewReader;
	
	PROCEDURE (f: StreamFile) NewWriter (old: Files.Writer): Files.Writer;
		VAR w: StreamWriter;
	BEGIN
		ASSERT(f.stream # NIL, 20);
		NEW(w); w.base := f;
		w.pos := f.len;
		RETURN w
	END NewWriter;
	
	PROCEDURE (f: StreamFile) Flush;
		VAR res: COM.RESULT;
	BEGIN
		ASSERT(f.stream # NIL, 20);
		res := f.stream.Commit(WinOle.STGC_DEFAULT)
	END Flush;
	
	PROCEDURE (f: StreamFile) Close;
	BEGIN
		IF f.stream # NIL THEN
			f.Flush;
			f.stream := NIL;
		END
	END Close;
	
	PROCEDURE (f: StreamFile) Register (name: Files.Name; type: Files.Type; ask: BOOLEAN; OUT res: INTEGER);
	BEGIN
		res := 10
	END Register;
	
	
	(* ---------- file creation ---------- *)
	
	PROCEDURE NewStreamFile* (stream: WinOle.IStream): Files.File;
		VAR f: StreamFile; res: COM.RESULT; y: LONGINT;
	BEGIN
		NEW(f);
		f.stream := stream;
		res := stream.Seek(0, WinOle.STREAM_SEEK_CUR, f.org);
		res := stream.Seek(0, WinOle.STREAM_SEEK_END, y);
		DEC(y, f.org);
		IF y > MAX(INTEGER) THEN y := MAX(INTEGER) END;
		f.len := SHORT(y);
		res := stream.Seek(f.org, WinOle.STREAM_SEEK_SET, NIL);
		f.pos := 0;
		RETURN f
	END NewStreamFile;
	
	PROCEDURE NewWriteILockBytes* (wr: Files.Writer): WinOle.ILockBytes;
		VAR new: ILockBytes;
	BEGIN
		IF debug THEN Log.String("new write"); Log.Ln END;
		NEW(new);
		IF new = NIL THEN RETURN NIL END;
		wr.WriteByte(0); wr.WriteByte(0); wr.WriteByte(0); wr.WriteByte(0); 	(* length *)
		new.len := 0;
		new.org := wr.Pos();
		new.file := wr.Base();
		new.rd := new.file.NewReader(NIL);
		new.wr := wr;
		RETURN new
	END NewWriteILockBytes;
	
	PROCEDURE NewReadILockBytes* (rd: Files.Reader): WinOle.ILockBytes;
		TYPE A4 = ARRAY 4 OF BYTE;
		VAR new: ILockBytes;
	BEGIN
		IF debug THEN Log.String("new read"); Log.Ln END;
		NEW(new);
		IF new = NIL THEN RETURN NIL END;
		rd.ReadBytes(SYSTEM.VAL(A4, new.len), 0, 4);
		new.org := rd.Pos();
		new.file := rd.Base();
		new.rd := rd;
		new.wr := NIL;
		RETURN new
	END NewReadILockBytes;
	
	
	(* stream import/export *)
	
	PROCEDURE GenStreamMedium (stm: WinOle.IStream; unk: COM.IUnknown; VAR sm: WinOle.STGMEDIUM);
	BEGIN
		sm.tymed := WinOle.TYMED_ISTREAM;
		sm.u.pstm := stm;
		sm.pUnkForRelease := unk
	END GenStreamMedium;
	
	PROCEDURE MediumStream (VAR sm: WinOle.STGMEDIUM): WinOle.IStream;
	BEGIN
		ASSERT(sm.tymed = WinOle.TYMED_ISTREAM, 20);
		RETURN SYSTEM.VAL(WinOle.IStream, sm.u.pstm)
	END MediumStream;
	
	PROCEDURE ExportToStream* (stream: WinOle.IStream; v: Views.View; w, h: INTEGER; isSingle: BOOLEAN);
		VAR f: Files.File; wr: Stores.Writer; res: COM.RESULT;
	BEGIN
		res := stream.Seek(0, WinOle.STREAM_SEEK_SET, NIL);
		f := NewStreamFile(stream);
		wr.ConnectTo(f);
		wr.SetPos(0);
		wr.WriteInt(obfTag);
		wr.WriteInt(0);
		wr.WriteInt(w);
		wr.WriteInt(h);
		IF isSingle THEN wr.WriteSChar(1X) ELSE wr.WriteSChar(0X) END;
		wr.WriteStore(v);
		f.Close
	END ExportToStream;
	
	PROCEDURE ExportOberon* (
		v: Views.View; w, h, x, y: INTEGER; isSingle: BOOLEAN; VAR med: WinOle.STGMEDIUM
	);
		VAR stream: WinOle.IStream; res: COM.RESULT;
	BEGIN
		IF med.tymed = WinOle.TYMED_ISTREAM THEN	(* use old stream *)
			stream := MediumStream(med)
		ELSE	(* create new temporary stream *)
			res := WinOle.CreateStreamOnHGlobal(0, 1, stream);
			GenStreamMedium(stream, NIL, med)
		END;
		ExportToStream(stream, v, w, h, isSingle)
	END ExportOberon;
	
	PROCEDURE ImportFromStream* (stream: WinOle.IStream;  VAR v: Views.View;
											VAR w, h: INTEGER; VAR isSingle: BOOLEAN);
		VAR f: Files.File; r: Stores.Reader; s: Stores.Store; tag, version, res: COM.RESULT; ch: SHORTCHAR;
	BEGIN
		v := NIL;
		res := stream.Seek(0, WinOle.STREAM_SEEK_SET, NIL);
		f := NewStreamFile(stream);
		r.ConnectTo(f); r.SetPos(0);
		r.ReadInt(tag);
		IF tag = obfTag THEN
			r.ReadInt(version);
			r.ReadInt(w);
			r.ReadInt(h);
			r.ReadSChar(ch); isSingle := ch # 0X;
			r.ReadStore(s);
			v := s(Views.View)
		END
	END ImportFromStream;
	
	PROCEDURE ImportOberon* (VAR med: WinOle.STGMEDIUM; VAR v: Views.View;
											VAR w, h: INTEGER; VAR isSingle: BOOLEAN);
		VAR stream: WinOle.IStream;
	BEGIN
		stream := MediumStream(med);
		ImportFromStream(stream, v, w, h, isSingle)
	END ImportOberon;

END OleStorage.


	OleStorage?
	
	DevDecoder.Decode OleStorage
	