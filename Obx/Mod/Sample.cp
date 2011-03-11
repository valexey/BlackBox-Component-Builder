MODULE ObxSample;
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

	TYPE
		File* = POINTER TO RECORD
			len: INTEGER	(* hidden instance variable *)
		END;

		Rider* = POINTER TO RECORD
			(* there may be several riders on one file *)
			file-: File;	(* read-only instance variable *)
			eof*: BOOLEAN;	(* fully exported instance variable *)
			pos: INTEGER	(* hidden instance variable *)
				(* Invariant: (pos >= 0) & (pos < file.len) *)
		END;

	PROCEDURE (f: File) GetLength* (OUT length: INTEGER), NEW;
	BEGIN
		length := f.len
	END GetLength;

	PROCEDURE (rd: Rider) SetPos* (pos: INTEGER), NEW;
	BEGIN
		(* assert invariants, so that errors may not be propagated across components *)
		ASSERT(pos >= 0); ASSERT(pos < rd.file.len);
		rd.pos := pos
	END SetPos;

	(* ... *)

END ObxSample.
