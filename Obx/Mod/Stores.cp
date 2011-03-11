MODULE ObxStores;
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

	(*
	This example illustrates the persistency service implemented by Stores. It is shown how a graph
	of linked nodes can be externalized and internalized, maintaining invariants like objects refered to
	through multiple pointers and cycles. Stores also offeres a mechanism to just copy such a graph.
			
	To run the example, click on the commanders below:
		 ObxStores.WriteRead		 ObxStores.Copy
	*)

	IMPORT Dialog, Files, Stores;

	TYPE
		Node = POINTER TO RECORD (Stores.Store)
			a, b: Node
		END;

	(* Methods of Node *)

	PROCEDURE (n: Node) Externalize (VAR w: Stores.Writer);
	BEGIN
		w.WriteStore(n.a); w.WriteStore(n.b)
	END Externalize;

	PROCEDURE (n: Node) Internalize (VAR r: Stores.Reader);
		VAR s: Stores.Store;
	BEGIN
		r.ReadStore(s);
		IF (s # NIL) & (s IS Node) THEN n.a := s(Node) ELSE n.a := NIL END;
		r.ReadStore(s);
		IF (s # NIL) & (s IS Node) THEN n.b := s(Node) ELSE n.b := NIL END
	END Internalize;

	PROCEDURE (n: Node) CopyFrom (source: Stores.Store);
	BEGIN
		WITH source: Node DO
			IF source.a # NIL THEN n.a := Stores.CopyOf(source.a)(Node) ELSE n.a := NIL END;
			IF source.b # NIL THEN n.b := Stores.CopyOf(source.b)(Node) ELSE n.b := NIL END
		END
	END CopyFrom;

	(* Build and check a graph of Nodes *)
																						
	PROCEDURE NewGraph (): Node;
		VAR n: Node;
	BEGIN
		NEW(n);
		NEW(n.a); Stores.Join(n, n.a);
		NEW(n.b); Stores.Join(n, n.b);
		n.a.a := n.b; n.a.b := NIL; n.b.a := n.a; n.b.b := n.b;
		RETURN n
	END NewGraph;

	PROCEDURE GraphOk (n: Node): BOOLEAN;
	BEGIN
		RETURN (n # n.a) & (n # n.b) & (n.a # n.b) & (n.a.a = n.b) & (n.a.b = NIL) & (n.b.a = n.a) & (n.b.b = n.b)
			& Stores.Joined(n, n.a) & Stores.Joined(n, n.b)
	END GraphOk;


	(* Demonstrate Ex- and Internalization *)

	PROCEDURE WriteRead*;
		VAR n, m: Node; f: Files.File; w: Stores.Writer; r: Stores.Reader; s: Stores.Store;
	BEGIN
		(* allocate and check new graph *)
		n := NewGraph(); ASSERT(GraphOk(n), 1);

		(* externalize graph to a temporary file *)
		f := Files.dir.Temp();
		w.ConnectTo(f); w.WriteStore(n);

		(* read graph back from file *)
		r.ConnectTo(f); r.ReadStore(s); m := s(Node);

		(* check graph to be internalized with nodes correctly linked and joined *)
		ASSERT(GraphOk(m), 2);
		Dialog.ShowMsg("WriteRead test ok.")
	END WriteRead;


	(* Demonstrate Copying *)

	PROCEDURE Copy*;
		VAR n, m: Node;
	BEGIN
		(* allocate and check new graph *)
		n := NewGraph(); ASSERT(GraphOk(n), 1);

		(* copy entire graph *)
		m := Stores.CopyOf(n)(Node);

		(* check graph to be internalized with nodes correctly linked and joined *)
		ASSERT(GraphOk(m), 2);
		Dialog.ShowMsg("Copy test ok.")
	END Copy;

END ObxStores.
