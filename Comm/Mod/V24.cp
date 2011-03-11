MODULE CommV24;
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

	IMPORT SYSTEM, WinApi;

	CONST
		bits4* = 0; bits5* = 1; bits6* = 2; bits7* = 3; stop15* = 4; stop2* = 5; even* = 6; odd* = 7;
		inXonXoff* = 8; outXonXoff* = 9; inRTS* = 10; inDTR* = 11; outCTS* = 12; outDSR* = 13;

	TYPE
		Connection* = POINTER TO LIMITED RECORD
			hnd: WinApi.HANDLE;	(* # 0: open *)
			opts: SET
		END;

	PROCEDURE Open* (device: ARRAY OF CHAR; baud: INTEGER; opts: SET; OUT conn: Connection);
		VAR c: Connection; h: WinApi.HANDLE; res: INTEGER; dcb: WinApi.DCB; to: WinApi.COMMTIMEOUTS;
	BEGIN
		h := WinApi.CreateFileW(
			device,  WinApi.GENERIC_READ +  WinApi.GENERIC_WRITE, {}, NIL,  WinApi.OPEN_EXISTING, {}, 0);
		IF h # -1 THEN
			dcb.DCBlength := SIZE(WinApi.DCB);
			res := WinApi.GetCommState(h, dcb);
			IF res # 0 THEN
				dcb.BaudRate := baud;
				dcb.fBits0 := {0};	(* binary *)
				IF opts * {even, odd} # {} THEN INCL(dcb.fBits0, 1) END;	(* check parity *)
				IF outCTS IN opts THEN INCL(dcb.fBits0, 2) END;	(* CTS out flow control *)
				IF outDSR IN opts THEN INCL(dcb.fBits0, 3) END;	(* DSR out flow control *)
				IF inDTR IN opts THEN INCL(dcb.fBits0, 5) END;	(* DTR flow control handshake *)
				IF outXonXoff IN opts THEN INCL(dcb.fBits0, 8) END;	(* Xon/Xoff out flow control *)
				IF inXonXoff IN opts THEN INCL(dcb.fBits0, 9) END;	(* Xob/Xoff in flow control *)
				IF inRTS IN opts THEN INCL(dcb.fBits0, 13) END;	(* RTS flow control handshake *)
				IF bits4 IN opts THEN dcb.ByteSize := 4X
				ELSIF bits5 IN opts THEN dcb.ByteSize := 5X
				ELSIF bits6 IN opts THEN dcb.ByteSize := 6X
				ELSIF bits7 IN opts THEN dcb.ByteSize := 7X
				ELSE dcb.ByteSize := 8X
				END;
				IF stop15 IN opts THEN dcb.StopBits := 1X
				ELSIF stop2 IN opts THEN dcb.StopBits := 2X
				ELSE dcb.StopBits := 0X
				END;
				IF even IN opts THEN dcb.Parity := 2X
				ELSIF odd IN opts THEN dcb.Parity := 1X
				ELSE dcb.Parity := 0X
				END;
				res := WinApi.SetCommState(h, dcb);
				IF res # 0 THEN
					to.ReadIntervalTimeout := 0;
					to.ReadTotalTimeoutMultiplier := 0;
					to.ReadTotalTimeoutConstant := 0;
					to.WriteTotalTimeoutMultiplier := 0;
					to.WriteTotalTimeoutConstant := 0;
					res := WinApi.SetCommTimeouts(h, to);
					IF res # 0 THEN
						NEW(c); c.hnd := h; c.opts := opts; conn := c
					END
				END
			END
		END
	END Open;

	PROCEDURE Close* (c: Connection);
		VAR res: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.CloseHandle(c.hnd);
		c.hnd := -1
	END Close;

	PROCEDURE SendByte* (c: Connection; x: BYTE);
		VAR res, written: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.WriteFile(c.hnd, SYSTEM.ADR(x), 1, written, NIL);
		ASSERT(res # 0, 100)
	END SendByte;

	PROCEDURE SendBytes* (c: Connection; IN x: ARRAY OF BYTE; beg, len: INTEGER);
		VAR res, written: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		ASSERT(LEN(x) >= beg + len, 22);
		ASSERT(len > 0, 23);
		res := WinApi.WriteFile(c.hnd, SYSTEM.ADR(x) + beg, len, written, NIL);
		ASSERT(res # 0, 100)
	END SendBytes;

	PROCEDURE Available* (c: Connection): INTEGER;
		VAR res: INTEGER; errors: SET; status: WinApi.COMSTAT;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.ClearCommError(c.hnd, errors, status);
		ASSERT(res # 0, 100);
		RETURN status.cbInQue
	END Available;

	PROCEDURE ReceiveByte* (c: Connection; OUT x: BYTE);
		VAR res, read: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.ReadFile(c.hnd, SYSTEM.ADR(x), 1, read, NIL);
		ASSERT(res # 0, 100)
	END ReceiveByte;

	PROCEDURE ReceiveBytes* (c: Connection; OUT x: ARRAY OF BYTE; beg, len: INTEGER);
		VAR res, read: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		ASSERT(LEN(x) >= beg + len, 22);
		ASSERT(len > 0, 23);
		res := WinApi.ReadFile(c.hnd, SYSTEM.ADR(x) + beg, len, read, NIL);
		ASSERT(res # 0, 100)
	END ReceiveBytes;

	PROCEDURE SetBuffers* (c: Connection; inpBufSize, outBufSize: INTEGER);
		VAR res: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.SetupComm(c.hnd, inpBufSize, outBufSize)
	END SetBuffers;

	PROCEDURE SetDTR* (c: Connection; on: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		ASSERT(~(inDTR IN c.opts), 22);
		IF on THEN res := WinApi.EscapeCommFunction(c.hnd, WinApi.SETDTR)
		ELSE res := WinApi.EscapeCommFunction(c.hnd, WinApi.CLRDTR)
		END
	END SetDTR;

	PROCEDURE SetRTS* (c: Connection; on: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		ASSERT(~(inRTS IN c.opts), 22);
		IF on THEN res := WinApi.EscapeCommFunction(c.hnd, WinApi.SETRTS)
		ELSE res := WinApi.EscapeCommFunction(c.hnd, WinApi.CLRRTS)
		END
	END SetRTS;

	PROCEDURE SetBreak* (c: Connection; on: BOOLEAN);
		VAR res: INTEGER;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		IF on THEN res := WinApi.EscapeCommFunction(c.hnd, WinApi.SETBREAK)
		ELSE res := WinApi.EscapeCommFunction(c.hnd, WinApi.CLRBREAK)
		END
	END SetBreak;

	PROCEDURE CTSState* (c: Connection): BOOLEAN;
		VAR res: INTEGER; s: SET;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.GetCommModemStatus(c.hnd, s);
		RETURN s * WinApi.MS_CTS_ON # {}
	END CTSState;

	PROCEDURE DSRState* (c: Connection): BOOLEAN;
		VAR res: INTEGER; s: SET;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.GetCommModemStatus(c.hnd, s);
		RETURN s * WinApi.MS_DSR_ON # {}
	END DSRState;

	PROCEDURE CDState* (c: Connection): BOOLEAN;
		VAR res: INTEGER; s: SET;
	BEGIN
		ASSERT(c # NIL, 20); ASSERT(c.hnd # -1, 21);
		res := WinApi.GetCommModemStatus(c.hnd, s);
		RETURN s * WinApi.MS_RLSD_ON # {}
	END CDState;

END CommV24.
