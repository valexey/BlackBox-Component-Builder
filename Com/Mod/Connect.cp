MODULE ComConnect;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	references	= "adapted from Connect sample in "Inside OLE", 2nd ed."
	changes	= ""
	issues	= ""

**)


	IMPORT COM, WinApi, WinOle, StdLog;
	
	CONST
		connPoints = 1;
		connMax = 2;
		
		eventQuack = 1; eventFlap = 2; eventPaddle = 3;
		
		SINK1 = 0; SINK2 = 1;
		
		
	TYPE
		IDuckEvents = POINTER TO 
					ABSTRACT RECORD ["{00021145-0000-0000-C000-000000000046}"] (COM.IUnknown) END;
		
		
		CEnumConnectionPoints = POINTER TO RECORD (WinOle.IEnumConnectionPoints)
			cur: INTEGER;
			num: INTEGER;
			data: ARRAY connPoints OF WinOle.IConnectionPoint
		END;
		
		CEnumConnections = POINTER TO RECORD (WinOle.IEnumConnections)
			cur: INTEGER;
			num: INTEGER;
			data: ARRAY connMax OF WinOle.CONNECTDATA
		END;
		
		CConnectionPoint = POINTER TO RECORD (WinOle.IConnectionPoint)
			obj: CConnObject;
			iid: COM.GUID;
			unk: ARRAY connMax OF COM.IUnknown;
			cookies: ARRAY connMax OF INTEGER;
			conn: INTEGER;
			next: INTEGER
		END;
		
		CConnObject = POINTER TO RECORD (WinOle.IConnectionPointContainer)
			connPt: ARRAY connPoints OF CConnectionPoint
		END;
		
		CDuckEvents = POINTER TO RECORD (IDuckEvents)
			id: INTEGER;
			cookie: INTEGER
		END;
	
	
	VAR
		sink: ARRAY 2 OF CDuckEvents;
		obj: CConnObject;
		
		
	(* ---------- IDuckEvents ---------- *)
	
	PROCEDURE (this: IDuckEvents) Quack (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IDuckEvents) Flap (): COM.RESULT, NEW, ABSTRACT;
	PROCEDURE (this: IDuckEvents) Paddle (): COM.RESULT, NEW, ABSTRACT;
	
	
	(* ---------- CDuckEvents ---------- *)
	
	PROCEDURE CreateCDuckEvents (id: INTEGER; OUT new: CDuckEvents);
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.id := id;
			new.cookie := 0
		END
	END CreateCDuckEvents;
	
	PROCEDURE (this: CDuckEvents) Quack (): COM.RESULT;
	BEGIN
		StdLog.String("Sink "); StdLog.Int(this.id + 1); StdLog.String(" received Quack"); StdLog.Ln;
		RETURN WinApi.S_OK
	END Quack;
	
	PROCEDURE (this: CDuckEvents) Flap (): COM.RESULT;
	BEGIN
		StdLog.String("Sink "); StdLog.Int(this.id + 1); StdLog.String(" received Flap"); StdLog.Ln;
		RETURN WinApi.S_OK
	END Flap;
	
	PROCEDURE (this: CDuckEvents) Paddle (): COM.RESULT;
	BEGIN
		StdLog.String("Sink "); StdLog.Int(this.id + 1); StdLog.String(" received Paddle"); StdLog.Ln;
		RETURN WinApi.S_OK
	END Paddle;
	
	
	(* ---------- CEnumConnections ---------- *)
	
	PROCEDURE CreateCEnumConnections (num: INTEGER; VAR data: ARRAY OF WinOle.CONNECTDATA;
														OUT new: CEnumConnections);
		VAR i: INTEGER;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			i := 0;
			WHILE i < num DO new.data[i] := data[i]; INC(i) END
		END
	END CreateCEnumConnections;
	
	PROCEDURE (this: CEnumConnections) Next (num: INTEGER;
																OUT elem: ARRAY [untagged] OF WinOle.CONNECTDATA;
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
	
	PROCEDURE (this: CEnumConnections) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: CEnumConnections) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: CEnumConnections) Clone (OUT enum: WinOle.IEnumConnections): COM.RESULT;
		VAR new: CEnumConnections;
	BEGIN
		CreateCEnumConnections(this.num, this.data, new);
		IF new # NIL THEN
			new.cur := this.cur;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	(* ---------- CConnectionPoint ---------- *)
	
	PROCEDURE CreateCConnectionPoint (obj: CConnObject; IN iid: COM.GUID; OUT new: CConnectionPoint);
		VAR i: INTEGER;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.iid := iid;
			new.obj := obj;
			new.conn := 0;
			new.next := 100
		END
	END CreateCConnectionPoint;
	
	PROCEDURE (this: CConnectionPoint) GetConnectionInterface (OUT iid: COM.GUID): COM.RESULT;
	BEGIN
		iid := this.iid; RETURN WinApi.S_OK
	END GetConnectionInterface;
	
	PROCEDURE (this: CConnectionPoint) GetConnectionPointContainer
															(OUT cpc: WinOle.IConnectionPointContainer): COM.RESULT;
	BEGIN
		RETURN this.obj.QueryInterface(COM.ID(cpc), cpc)
	END GetConnectionPointContainer;
	
	PROCEDURE (this: CConnectionPoint) Advise (sink: COM.IUnknown; OUT cookie: INTEGER): COM.RESULT;
		VAR res: COM.RESULT; unk: COM.IUnknown; i: INTEGER;
	BEGIN
		IF this.conn < connMax THEN
			res := sink.QueryInterface(this.iid, unk);
			IF res >= 0 THEN
				i := 0;
				WHILE this.unk[i] # NIL DO INC(i) END;
				this.unk[i] := unk;
				INC(this.next);
				this.cookies[i] := this.next;
				cookie := this.next;
				INC(this.conn);
				RETURN WinApi.S_OK
			ELSE RETURN WinApi.CONNECT_E_CANNOTCONNECT
			END
		ELSE RETURN WinApi.CONNECT_E_ADVISELIMIT
		END
	END Advise;
	
	PROCEDURE (this: CConnectionPoint) Unadvise (cookie: INTEGER): COM.RESULT;
		VAR i: INTEGER;
	BEGIN
		IF cookie # 0 THEN
			i := 0;
			WHILE (i < connMax) & (this.cookies[i] # cookie) DO INC(i) END;
			IF i < connMax THEN
				this.unk[i] := NIL;
				this.cookies[i] := 0;
				DEC(this.conn);
				RETURN WinApi.S_OK
			ELSE RETURN WinApi.CONNECT_E_NOCONNECTION
			END
		ELSE RETURN WinApi.E_INVALIDARG
		END
	END Unadvise;
	
	PROCEDURE (this: CConnectionPoint) EnumConnections (
															OUT enum: WinOle.IEnumConnections): COM.RESULT;
		VAR p: ARRAY connMax OF WinOle.CONNECTDATA; i, j: INTEGER; c: CEnumConnections;
	BEGIN
		IF this.conn > 0 THEN
			i := 0; j := 0;
			WHILE i < connMax DO
				IF this.unk[i] # NIL THEN
					p[j].pUnk := this.unk[i];
					p[j].dwCookie := this.cookies[i];
					INC(j)
				END;
				INC(i)
			END;
			CreateCEnumConnections(this.conn, p, c);
			IF c # NIL THEN
				enum := c;
				RETURN WinApi.S_OK
			ELSE RETURN WinApi.E_OUTOFMEMORY
			END
		ELSE RETURN WinApi.OLE_E_NOCONNECTION
		END
	END EnumConnections;
	
	
	(* ---------- CEnumConnectionPoints ---------- *)
	
	PROCEDURE CreateCEnumConnectionPoints (num: INTEGER; VAR data: ARRAY OF WinOle.IConnectionPoint;
																VAR new: CEnumConnectionPoints);
		VAR i: INTEGER;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			new.cur := 0;
			new.num := num;
			i := 0;
			WHILE i < num DO new.data[i] := data[i]; INC(i) END
		END
	END CreateCEnumConnectionPoints;
	
	PROCEDURE (this: CEnumConnectionPoints) Next (num: INTEGER;
																		OUT elem: ARRAY [untagged] OF WinOle.IConnectionPoint;
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
	
	PROCEDURE (this: CEnumConnectionPoints) Skip (num: INTEGER): COM.RESULT;
	BEGIN
		IF this.cur + num < this.num THEN
			INC(this.cur, num); RETURN WinApi.S_OK
		ELSE RETURN WinApi.S_FALSE
		END
	END Skip;
	
	PROCEDURE (this: CEnumConnectionPoints) Reset (): COM.RESULT;
	BEGIN
		this.cur := 0; RETURN WinApi.S_OK
	END Reset;
	
	PROCEDURE (this: CEnumConnectionPoints) Clone (
																		OUT enum: WinOle.IEnumConnectionPoints): COM.RESULT;
		VAR new: CEnumConnectionPoints;
	BEGIN
		CreateCEnumConnectionPoints(this.num, this.data, new);
		IF new # NIL THEN
			new.cur := this.cur;
			enum := new;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END Clone;
	
	
	(* ---------- CConnObject ---------- *)
	
	PROCEDURE CreateCConnObject (VAR new: CConnObject);
		VAR i, n: INTEGER;
	BEGIN
		NEW(new);
		IF new # NIL THEN
			i := 0;
			WHILE i < connPoints DO
				CreateCConnectionPoint(new, COM.ID(IDuckEvents), new.connPt[i]);
				INC(i)
			END
		END
	END CreateCConnObject;
	
	PROCEDURE (this: CConnObject) EnumConnectionPoints
															(OUT enum: WinOle.IEnumConnectionPoints): COM.RESULT;
		VAR i: INTEGER; cp: ARRAY connPoints OF WinOle.IConnectionPoint; ce: CEnumConnectionPoints;
	BEGIN
		i := 0;
		WHILE i < connPoints DO cp[i] := this.connPt[i]; INC(i) END;
		CreateCEnumConnectionPoints(connPoints, cp, ce);
		IF ce # NIL THEN
			enum := ce;
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_OUTOFMEMORY
		END
	END EnumConnectionPoints;
	
	PROCEDURE (this: CConnObject) FindConnectionPoint (IN iid: COM.GUID;
																			OUT conn: WinOle.IConnectionPoint): COM.RESULT;
		VAR
	BEGIN
		IF iid = COM.ID(IDuckEvents) THEN
			conn := this.connPt[0];
			RETURN WinApi.S_OK
		ELSE RETURN WinApi.E_NOINTERFACE
		END
	END FindConnectionPoint;
	
	PROCEDURE (this: CConnObject) TriggerEvent (event: INTEGER): BOOLEAN, NEW;
		VAR enum: WinOle.IEnumConnections; cd: ARRAY 1 OF WinOle.CONNECTDATA;
			duck: IDuckEvents; n: INTEGER;
	BEGIN
		IF this.connPt[0].EnumConnections(enum) >= 0 THEN
			WHILE enum.Next(1, cd, NIL) = WinApi.S_OK DO
				IF cd[0].pUnk.QueryInterface(COM.ID(duck), duck) >= 0 THEN
					CASE event OF
					| eventQuack: n := duck.Quack()
					| eventFlap: n := duck.Flap()
					| eventPaddle: n := duck.Paddle()
					END
				END
			END;
			RETURN TRUE
		ELSE RETURN FALSE
		END
	END TriggerEvent;

	
	(* ---------- commands ---------- *)
	
	PROCEDURE GetConnectionPoint (): WinOle.IConnectionPoint;
		VAR res: COM.RESULT; cont: WinOle.IConnectionPointContainer; cp: WinOle.IConnectionPoint;
	BEGIN
		res := obj.QueryInterface(COM.ID(cont), cont);
		IF res >= 0 THEN
			res := cont.FindConnectionPoint(COM.ID(IDuckEvents), cp);
			RETURN cp
		ELSE RETURN NIL
		END
	END GetConnectionPoint;
	
	PROCEDURE Connect (id: INTEGER);
		VAR cp: WinOle.IConnectionPoint; res: COM.RESULT;
	BEGIN
		IF obj # NIL THEN
			IF sink[id].cookie = 0 THEN
				cp := GetConnectionPoint();
				IF cp # NIL THEN
					res := cp.Advise(sink[id], sink[id].cookie);
					IF res < 0 THEN StdLog.String("Connection failed"); StdLog.Ln
					ELSE StdLog.String("Connection complete"); StdLog.Ln
					END
				ELSE
					StdLog.String("Failed to get IConnectionPoint"); StdLog.Ln
				END
			ELSE
				StdLog.String("Sink already connected"); StdLog.Ln
			END
		ELSE
			StdLog.String("No object"); StdLog.Ln
		END
	END Connect;
	
	PROCEDURE Disconnect (id: INTEGER);
		VAR cp: WinOle.IConnectionPoint; res: COM.RESULT;
	BEGIN
		IF obj # NIL THEN
			IF sink[id].cookie # 0 THEN
				cp := GetConnectionPoint();
				IF cp # NIL THEN
					res := cp.Unadvise(sink[id].cookie);
					IF res < 0 THEN StdLog.String("Disconnection failed"); StdLog.Ln
					ELSE
						StdLog.String("Disconnection complete"); StdLog.Ln;
						sink[id].cookie := 0
					END
				ELSE
					StdLog.String("Failed to get IConnectionPoint"); StdLog.Ln
				END
			ELSE
				StdLog.String("Sink not connected"); StdLog.Ln
			END
		ELSE
			StdLog.String("No object"); StdLog.Ln
		END
	END Disconnect;
	
	PROCEDURE Init*;
	BEGIN
		CreateCDuckEvents(SINK1, sink[SINK1]);
		CreateCDuckEvents(SINK2, sink[SINK2]);
	END Init;
	
	PROCEDURE Release*;
	BEGIN
		obj := NIL;
		Disconnect(SINK1);
		Disconnect(SINK2);
		sink[SINK1] := NIL;
		sink[SINK2] := NIL
	END Release;

	PROCEDURE ObjectCreate*;
	BEGIN
		CreateCConnObject(obj);
		IF obj # NIL THEN
			StdLog.String(" Object created"); StdLog.Ln
		END
	END ObjectCreate;
	
	PROCEDURE ObjectRelease*;
	BEGIN
		IF obj # NIL THEN
			obj := NIL;
			StdLog.String("Object released"); StdLog.Ln
		ELSE
			StdLog.String("No object"); StdLog.Ln
		END
	END ObjectRelease;
	
	PROCEDURE Sink1Connect*;
	BEGIN
		Connect(SINK1)
	END Sink1Connect;
	
	PROCEDURE Sink1Disconnect*;
	BEGIN
		Disconnect(SINK1)
	END Sink1Disconnect;
	
	PROCEDURE Sink2Connect*;
	BEGIN
		Connect(SINK2)
	END Sink2Connect;
	
	PROCEDURE Sink2Disconnect*;
	BEGIN
		Disconnect(SINK2)
	END Sink2Disconnect;
	
	PROCEDURE TriggerQuack*;
	BEGIN
		IF obj # NIL THEN
			IF ~obj.TriggerEvent(eventQuack) THEN
				StdLog.String("No connected sinks"); StdLog.Ln
			END
		ELSE
			StdLog.String("No object"); StdLog.Ln
		END
	END TriggerQuack;
	
	PROCEDURE TriggerFlap*;
	BEGIN
		IF obj # NIL THEN
			IF ~obj.TriggerEvent(eventFlap) THEN
				StdLog.String("No connected sinks"); StdLog.Ln
			END
		ELSE
			StdLog.String("No object"); StdLog.Ln
		END
	END TriggerFlap;
	
	PROCEDURE TriggerPaddle*;
	BEGIN
		IF obj # NIL THEN
			IF ~obj.TriggerEvent(eventPaddle) THEN
				StdLog.String("No connected sinks"); StdLog.Ln
			END
		ELSE
			StdLog.String("No object"); StdLog.Ln
		END
	END TriggerPaddle;
	
END ComConnect.


 ComConnect.Init
 ComConnect.Release

 ComConnect.ObjectCreate
 ComConnect.ObjectRelease

 ComConnect.Sink1Connect
 ComConnect.Sink1Disconnect
 ComConnect.Sink2Connect
 ComConnect.Sink2Disconnect

 ComConnect.TriggerFlap
 ComConnect.TriggerPaddle
 ComConnect.TriggerQuack
