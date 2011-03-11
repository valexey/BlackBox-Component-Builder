MODULE ObxActions;
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

	IMPORT Services, Views, TextModels, TextMappers, TextViews, Math;

	CONST stepSize = 1000;

	TYPE
		PrimeAction = POINTER TO RECORD (Services.Action)
			to, current, divisor: INTEGER;
			f: TextMappers.Formatter
		END;

	VAR
		primeDlg*: RECORD
			to*: INTEGER;
		END;

	PROCEDURE Step (VAR cur, divisor: INTEGER; f: TextMappers.Formatter);
		VAR end, sqrtCur: INTEGER;
	BEGIN
		end := divisor + stepSize;
		sqrtCur := SHORT(ENTIER(Math.Sqrt(cur)));
		WHILE (divisor <= sqrtCur) & (divisor < end) & (cur MOD divisor # 0) DO divisor := divisor + 2 END;
		IF divisor > sqrtCur THEN	(* cur is a prime *)
			f.WriteInt(cur); f.WriteLn;
			divisor := 3; cur := cur + 2
		ELSIF divisor < end THEN	(* cur is not a prime, test next one *)
			divisor := 3; cur := cur + 2
		ELSE	(* time exhausted, continue test next time *)
		END
	END Step;

	PROCEDURE (a: PrimeAction) Do;
	BEGIN
		Step(a.current, a.divisor, a.f);
		IF a.current <= a.to THEN
			Services.DoLater(a, Services.now)
		ELSE
			Views.OpenView(TextViews.dir.New(a.f.rider.Base()))
		END
	END Do;

	PROCEDURE StartPrimes*;
		VAR a: PrimeAction;
	BEGIN
		NEW(a);
		a.to := primeDlg.to; a.current := 3; a.divisor := 3;
		a.f.ConnectTo(TextModels.dir.New());
		a.f.WriteInt(2); a.f.WriteLn;
		Services.DoLater(a, Services.now)
	END StartPrimes;

END ObxActions.
