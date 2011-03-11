MODULE CommStreams; 
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

	IMPORT Meta;

	CONST
		(* portable error codes: *)
		done* = 0; noSuchProtocol* = 1; invalidLocalAdr* = 2; invalidRemoteAdr* = 3; networkDown* = 4;
		localAdrInUse* = 5; remoteAdrInUse* = 6;

	TYPE
		Adr* = POINTER TO ARRAY OF CHAR;
		
		Stream* = POINTER TO ABSTRACT RECORD END;
		
		StreamAllocator* = PROCEDURE
			(localAdr, remoteAdr: ARRAY OF CHAR; OUT s: Stream; OUT res: INTEGER);

		Listener* = POINTER TO ABSTRACT RECORD END;
		
		ListenerAllocator* = PROCEDURE
			(localAdr: ARRAY OF CHAR; OUT l: Listener; OUT res: INTEGER);


	PROCEDURE (s: Stream) RemoteAdr* (): Adr, NEW, ABSTRACT;
	PROCEDURE (s: Stream) IsConnected* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (s: Stream) WriteBytes* (
		IN x: ARRAY OF BYTE; beg, len: INTEGER; OUT written: INTEGER), NEW, ABSTRACT;
	PROCEDURE (s: Stream) ReadBytes* (
		VAR x: ARRAY OF BYTE; beg, len: INTEGER; OUT read: INTEGER), NEW, ABSTRACT;
	PROCEDURE (s: Stream) Close*, NEW, ABSTRACT;

	PROCEDURE NewStream* (protocol, localAdr, remoteAdr: ARRAY OF CHAR; OUT s: Stream; OUT res: INTEGER);
		VAR ok: BOOLEAN; m, p: Meta.Item; mod: Meta.Name;
			v: RECORD (Meta.Value)
				p: StreamAllocator
			END;
	BEGIN
		ASSERT(protocol # "", 20);
		res := noSuchProtocol;
		mod := protocol$; Meta.Lookup(mod, m);
		IF m.obj = Meta.modObj THEN
			m.Lookup("NewStream", p);
			IF p.obj = Meta.procObj THEN
				p.GetVal(v, ok);
				IF ok THEN v.p(localAdr, remoteAdr, s, res) END
			END
		END
	END NewStream;

	PROCEDURE (l: Listener) LocalAdr* (): Adr, NEW, ABSTRACT;
	PROCEDURE (l: Listener) Accept* (OUT s: Stream), NEW, ABSTRACT;
	PROCEDURE (l: Listener) Close*, NEW, ABSTRACT;

	PROCEDURE NewListener* (protocol, localAdr: ARRAY OF CHAR; OUT l: Listener; OUT res: INTEGER);
		VAR ok: BOOLEAN; m, p: Meta.Item; mod: Meta.Name;
			v: RECORD(Meta.Value)
				p: ListenerAllocator
			END;
	BEGIN
		ASSERT(protocol # "", 20);
		res := noSuchProtocol;
		mod := protocol$; Meta.Lookup(mod, m);
		IF m.obj = Meta.modObj THEN
			m.Lookup("NewListener", p);
			IF p.obj = Meta.procObj THEN
				p.GetVal(v, ok);
				IF ok THEN v.p(localAdr, l, res) END
			END
		END
	END NewListener;

END CommStreams.


