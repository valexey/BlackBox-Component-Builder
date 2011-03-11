MODULE CommObxStreamsServer;
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

	IMPORT Services, CommStreams, Views, TextModels, TextViews, StdLog;

	CONST
		protocol = "CommTCP";			(* driver for TCP/IP communication, over Winsock *)
		localAdr = "900";					(* this is the port on which the server listens *)

	TYPE
		Server = POINTER TO RECORD (Services.Action)
			listener: CommStreams.Listener	(* the server listens using this object and accepts incoming connections*)
		END;

		Processor = POINTER TO RECORD (Services.Action)
			prev, succ: Processor;	(* linked to allow stopping of all active processors with Stop command *)
			stream: CommStreams.Stream;	(* for each connection, a receiver communicates through this object *)
			writer: TextModels.Writer
		END;

	VAR
		server: Server;
		processors: Processor;

	PROCEDURE (server: Server) Do;
		VAR s: CommStreams.Stream; p: Processor; t: TextModels.Model;
	BEGIN
		server.listener.Accept(s);	(* poll for a connection *)
		IF s # NIL THEN	(* upon accept create and start processor for new connection *)
			NEW(p); p.prev := NIL; p.succ := processors; processors := p;
			IF p.succ # NIL THEN p.succ.prev := p END;
			p.stream := s;
			t := TextModels.dir.New(); p.writer := t.NewWriter(NIL);
			Views.OpenAux(TextViews.dir.New(t), "Message");
			Services.DoLater(p, Services.now)
		END;
		Services.DoLater(server, Services.now)
	END Do;

	PROCEDURE (p: Processor) Do;
		VAR buf: ARRAY 256 OF BYTE; read, i: INTEGER;
	BEGIN
		p.stream.ReadBytes(buf, 0, LEN(buf), read);	(* poll for incoming data *)
		IF read > 0 THEN	(* write received data into log *)
			i := 0; WHILE i # read DO p.writer.WriteChar(CHR(buf[i])); INC(i) END;
			Services.DoLater(p, Services.now)
		ELSIF p.stream.IsConnected() THEN
			Services.DoLater(p, Services.now)
		ELSE
			IF p.prev # NIL THEN p.prev.succ := p.succ ELSE processors := p.succ END;
			IF p.succ # NIL THEN p.succ.prev := p.prev END
		END
	END Do;


	PROCEDURE Start*;
		VAR l: CommStreams.Listener; res: INTEGER;
	BEGIN
		IF server = NIL THEN
			CommStreams.NewListener(protocol, localAdr, l, res);
			IF l # NIL THEN
				NEW(server); server.listener := l; Services.DoLater(server, Services.now);
				StdLog.String("server: server started"); StdLog.Ln
			ELSE
				StdLog.String("server: error starting the server ("); StdLog.Int(res); StdLog.Char(")"); StdLog.Ln
			END
		END
	END Start;

	PROCEDURE Stop*;
	BEGIN
		WHILE processors # NIL DO
			processors.stream.Close; Services.RemoveAction(processors);
			processors := processors.succ
		END;
		IF server # NIL THEN
			Services.RemoveAction(server);
			server.listener.Close;
			server := NIL;
			StdLog.String("server: server stopped"); StdLog.Ln
		END
	END Stop;

CLOSE
	Stop	(* prevent the server from trapping after module unloading *)
END CommObxStreamsServer.

 CommObxStreamsServer.Start
 CommObxStreamsServer.Stop
