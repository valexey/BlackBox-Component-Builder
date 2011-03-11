MODULE ObxTabViews;
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

	IMPORT StdTabViews, Views, Strings, Dialog, StdLog,
				ObxCalc, ObxCubes;
	
	VAR 
		st: StdTabViews.View;
		calc: Views.View;

	PROCEDURE New* (): StdTabViews.View;
		VAR v: Views.View;
	BEGIN
		st := StdTabViews.dir.New();
		calc := ObxCalc.New(); v := ObxCubes.New();
		st.SetItem(0, 'Cube', v);
		st.SetItem(1, 'Calc 1', calc);
		st.SetItem(2, 'Calc 2', calc);
		st.SetNotifier("ObxTabViews.Notify");
		RETURN st
	END New;

	PROCEDURE Deposit*;
	BEGIN
		Views.Deposit(New())
	END Deposit;
	
	PROCEDURE AddNewCalc*;
		VAR s: ARRAY 4 OF CHAR;
	BEGIN
		Strings.IntToString(st.NofTabs(), s);
		st.SetItem(st.NofTabs(), 'Calc ' + s, calc)
	END AddNewCalc;
	
	PROCEDURE DeleteTab*;
		VAR i: INTEGER; s: Dialog.String; v: Views.View;
	BEGIN
		IF st.NofTabs() > 0 THEN
			FOR i := st.Index() TO st.NofTabs() - 2 DO
				st.GetItem(i + 1, s, v); st.SetItem(i, s, v)
			END;
			st.SetNofTabs(st.NofTabs() - 1)
		END
	END DeleteTab;
	
	PROCEDURE Notify* (v: StdTabViews.View; from, to: INTEGER);
	BEGIN
		StdLog.String("Notify: "); StdLog.Int(from); StdLog.Int(to); StdLog.Ln
	END Notify;
	
END ObxTabViews.
