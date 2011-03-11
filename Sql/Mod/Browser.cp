MODULE SqlBrowser;
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

	IMPORT Dialog, Views, TextModels, TextControllers, SqlDB, SqlControls;
	
	
	VAR
		dlg*: RECORD
			id*,
			password*,
			database*,
			driver*: ARRAY 32 OF CHAR;
			statement*: ARRAY 1024 OF CHAR
		END;
		table*: SqlDB.Table;	(* anchor for database *)
		
	
	PROCEDURE CheckResult (tab: SqlDB.Table; par: ANYPTR);
		VAR v: Views.View;
	BEGIN
		IF tab.res = 0 THEN
			IF tab.columns > 0 THEN
				v := SqlControls.dir.NewTableOn(tab);
				Views.OpenAux(v, "#Sql:Result")
			ELSE Dialog.ShowMsg("#Sql:StatementExecuted")
			END
		ELSE Dialog.ShowMsg("#Sql:ExecutionFailed")
		END
	END CheckResult;
	
	PROCEDURE ExecuteThis (statement: ARRAY OF CHAR);
		VAR res: INTEGER; db: SqlDB.Database; tab: SqlDB.Table;
	BEGIN
		IF table = NIL THEN
			SqlDB.OpenDatabase(dlg.driver, dlg.id, dlg.password, dlg.database,
											SqlDB.async, SqlDB.showErrors, db, res);
			IF res = 0 THEN
				table := db.NewTable()
			ELSIF res <= 3 THEN
				Dialog.ShowMsg("#Sql:CannotLoadDriver")
			ELSE
				Dialog.ShowMsg("#Sql:ConnectionFailed")
			END
		END;
		IF (statement # "") & (table # NIL) THEN
			(* allocate separate table to allow for multiple open tables *)
			tab := table.base.NewTable();
			tab.Exec(statement);
			(* separate result check from execution to allow for asynchronous operation *)
			tab.Call(CheckResult, NIL)
		END
	END ExecuteThis;
	
	
	PROCEDURE Execute*;
	BEGIN
		ExecuteThis(dlg.statement)
	END Execute;
	
	PROCEDURE ExecuteSel*;
		VAR c: TextControllers.Controller; r: TextModels.Reader; beg, end, i: INTEGER; stat: ARRAY 1024 OF CHAR;
	BEGIN
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			r := c.text.NewReader(NIL);
			r.SetPos(beg); i := 0;
			WHILE r.Pos() < end DO r.ReadChar(stat[i]); INC(i) END;
			stat[i] := 0X;
			ExecuteThis(stat)
		END
	END ExecuteSel;
	
	PROCEDURE Commit*;
	BEGIN
		IF table # NIL THEN
			table.base.Commit
		END
	END Commit;
	
	
BEGIN
	dlg.id := ""; dlg.password := ""; dlg.database := "Test Database"; dlg.driver := "SqlOdbc"
END SqlBrowser.
