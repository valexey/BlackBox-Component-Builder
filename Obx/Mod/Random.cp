MODULE ObxRandom;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	references	= "Martin Reiser, Niklaus Wirth, Programming In Oberon, ISBN 0201565439"
	changes	= ""
	issues	= ""

**)

	VAR z: INTEGER;	(* global variable *)

	PROCEDURE Uniform* (): REAL;
		CONST a = 16807; m = 2147483647; q = m DIV a; r = m MOD a;
		VAR gamma: INTEGER;
	BEGIN
		gamma := a * (z MOD q) - r * (z DIV q);
		IF gamma > 0 THEN
			z := gamma
		ELSE
			z := gamma + m
		END;
		RETURN z * (1.0 / m)	(* value of the function *)
	END Uniform;

	PROCEDURE InitSeed* (seed: INTEGER);
	BEGIN
		z := seed
	END InitSeed;

BEGIN
	z := 314159	(* initial value of seed *)
END ObxRandom.
