MODULE CommObxStreamsClient;
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

	IMPORT Services, CommStreams, TextModels, TextControllers, StdLog;

	CONST
		protocol = "CommTCP";			(* driver for TCP/IP communication, over Winsock *)
		remoteAdr = "127.0.0.1:900";	(* loopback address, so you can test on your local machine;
														choose a port number that is not used yet (here: 900).
														You could also specify an IP address in the form
														"mymachine.mydomain.com:900" *)
		localAdr = "";							(* don't specify an address or port on the client side *)

	TYPE
		Sender = POINTER TO RECORD (Services.Action)
			prev, succ: Sender;	(* linked to allow stopping upon module unloading *)
			stream: CommStreams.Stream;	(* during sending, the communicates through this object *)
			idx, len: INTEGER;					(* index for start of data into the buf array, and length remaining to be sent *)
			buf: ARRAY 256 OF BYTE		(* data to be sent *)
		END;

	VAR
		senders: Sender;

	PROCEDURE (s: Sender) Do;
		VAR written: INTEGER;
	BEGIN
		IF s.stream.IsConnected() THEN
			s.stream.WriteBytes(s.buf, s.idx, s.len, written);	(* poll for outgoing data *)
			INC(s.idx, written); DEC(s.len, written);
			IF s.len > 0 THEN	(* keep action alive if there remains further data to send *)
				Services.DoLater(s, Services.now)
			ELSE
				s.stream.Close;
				IF s.prev # NIL THEN s.prev.succ := s.succ ELSE senders := s.succ END;
				IF s.succ # NIL THEN s.succ.prev := s.prev END
			END
		ELSE	(* connection was closed by server *)
			IF s.prev # NIL THEN s.prev.succ := s.succ ELSE senders := s.succ END;
			IF s.succ # NIL THEN s.succ.prev := s.prev END;
			IF s.idx = 0 THEN StdLog.String("client: connection was not accepted by server")
			ELSE StdLog.String("client: connection was closed by server")
			END;
			StdLog.Ln
		END
	END Do;


	PROCEDURE Start (s: Sender);
		VAR stream: CommStreams.Stream; res: INTEGER;
	BEGIN
		CommStreams.NewStream(protocol, localAdr, remoteAdr, stream, res);
		IF stream # NIL THEN
			s.prev := NIL; s.succ := senders; senders := s;
			IF s.succ # NIL THEN s.succ.prev := s END;
			s.stream := stream;
			Services.DoLater(s, Services.now);
			StdLog.String("client: connection opened"); StdLog.Ln
		ELSE
			StdLog.String("client: error opening the connection ("); StdLog.Int(res); StdLog.Char(")"); StdLog.Ln
		END
	END Start;

	PROCEDURE Stop;
	BEGIN
		WHILE senders # NIL DO
			senders.stream.Close; Services.RemoveAction(senders);
			senders := senders.succ
		END
	END Stop;

	PROCEDURE SendTextSelection*;
		VAR c: TextControllers.Controller; beg, end, i, len: INTEGER; rd: TextModels.Reader; ch: CHAR; s: Sender;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			NEW(s);
			c.GetSelection(beg, end);
			rd := c.text.NewReader(NIL); rd.SetPos(beg);
			i := 0; len := end - beg;
			IF len >= LEN(s.buf) - 1 THEN len := LEN(s.buf) - 1 END;	(* clip string if necessary *)
			WHILE len # 0 DO
				rd.ReadChar(ch);
				IF ch < 100X THEN	(* skip Unicode characters *)
					s.buf[i] := SHORT(SHORT(ORD(ch))); INC(i)
				END;
				DEC(len)
			END;
			s.idx := 0; s.len := i;
			Start(s)
		ELSE
			StdLog.String("client: no text selection found"); StdLog.Ln
		END
	END SendTextSelection;

CLOSE
	Stop	(* prevent the client from trapping after module unloading *)
END CommObxStreamsClient.

 CommObxStreamsClient.SendTextSelection
