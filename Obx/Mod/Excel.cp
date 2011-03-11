MODULE ObxExcel;
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

	IMPORT
		CtlT, CtlExcel := CtlExcel9, Dates, Views, Containers,
		OleViews, TextModels, TextViews, TextMappers, StdLog;

	
	PROCEDURE ShowExcel*;
		VAR ws: CtlExcel.Worksheet; wb: CtlExcel.Workbook;
			range: CtlExcel.Range;
			a: ARRAY 3 OF INTEGER;
			d: Dates.Date; t: Dates.Time;
			date: CtlT.OleDate; (* = REAL *)
			cy: CtlT.OleCy;  (* = LONGINT *)
			v: Views.View;
			text: TextModels.Model; out: TextMappers.Formatter;
	BEGIN
		v := OleViews.NewObjectView("Excel.Sheet");
		IF v # NIL THEN
			(* in excel 8.0 Excel.Sheet is a workbook object ! *)
			wb := CtlExcel.This_Workbook(OleViews.AutoObject(v));
			ws := CtlExcel.This_Worksheet(wb.Worksheets().Item(CtlT.Int(1)));

			range := ws.Range(CtlT.Str("A1"), NIL);
			range.PUTValue(CtlT.Str("Hello World"));
			range.PUTColumnWidth(CtlT.Int(12));	(* unit = width of "0" in standard font *)
			ws.Range(CtlT.Str("B1"), NIL).PUTValue(CtlT.Int(13));
			d.year := 1996; d.month := 8; d.day := 5;
			t.hour := 10; t.minute := 5; t.second := 30;
			date := CtlT.OleDateFromDateAndTime(d, t);	(* (day - 1.1.1900 + 2) + hour / 24 + minute / (24 * 60) + ... *)
			range := ws.Range(CtlT.Str("C1"), NIL);
			range.PUTValue(CtlT.Date(date));
			range.PUTColumnWidth(CtlT.Int(15));
			cy := 133500;	(* 13.35 *)
			ws.Range(CtlT.Str("D1"), NIL).PUTValue(CtlT.Cy(cy));
			a[0] := 1; a[1] := 2; a[2] := 3;
			ws.Range(CtlT.Str("A2"), CtlT.Str("C2")).PUTValue(CtlT.IntArr(a));
			ws.Range(CtlT.Str("D2"), NIL).PUTValue(CtlT.Str("=A2+B2+C2"));
			
			text := TextModels.dir.New();
			out.ConnectTo(text);
			out.WriteString("Excel Automation Example");
			out.WriteLn; out.WriteLn; out.WriteLn;
			out.WriteTab; out.WriteTab; out.WriteView(v);
			out.WriteLn; out.WriteLn; out.WriteLn;
			Views.OpenView(TextViews.dir.New(text));
		ELSE
			StdLog.String("cannot open excel object"); StdLog.Ln
		END
	END ShowExcel;
	
	PROCEDURE ReadExcel*;
		VAR v: Views.View; ws: CtlExcel.Worksheet;
			obj: CtlT.Object;
			p: POINTER TO ARRAY OF ARRAY OF CtlT.Any;
			val, cell: CtlT.Any; r, c: INTEGER;
			date: Dates.Date; time: Dates.Time;
			str: ARRAY 256 OF CHAR;
	BEGIN
		v := Containers.FocusSingleton();
		IF v # NIL THEN
			IF OleViews.IsAutoView(v) THEN
				obj:= OleViews.AutoObject(v);
				IF CtlExcel.Is_Workbook(obj) THEN
					ws := CtlExcel.This_Worksheet(CtlExcel.This_Workbook(obj).Worksheets().Item(CtlT.Int(1)));
					val := ws.Range(CtlT.Str("A1"), CtlT.Str("E5")).Value();
					ASSERT(val # NIL, 10);
					ASSERT(val.dim = 2, 11);
					ASSERT(val.typeId = CtlT.any, 12);
					p := val(CtlT.AnyArray2).p; r := 0;
					StdLog.Int(LEN(p^)); StdLog.Int(LEN(p^, 1)); StdLog.Ln;
					WHILE r < LEN(p^) DO
						c := 0;
						WHILE c < LEN(p^, 1) DO
							cell := p[r, c];
							IF cell # NIL THEN
								ASSERT(cell.dim = 0, 13);
								StdLog.String("Cell "); StdLog.Char(CHR(c + ORD("A"))); StdLog.Int(r); StdLog.String(": ");
								CASE cell.typeId OF
								| CtlT.byte: StdLog.Int(cell(CtlT.ByteT).val)
								| CtlT.shortint: StdLog.Int(cell(CtlT.ShortInt).val)
								| CtlT.integer: StdLog.Int(cell(CtlT.Integer).val)
								| CtlT.shortreal: StdLog.Real(cell(CtlT.ShortReal).val)
								| CtlT.real: StdLog.Real(cell(CtlT.RealT).val)
								| CtlT.result: StdLog.IntForm(cell(CtlT.Result).val, 16, 8, "0", FALSE)
								| CtlT.date: CtlT.OleDateToDateAndTime(cell(CtlT.DateT).val, date, time);
									Dates.DateToString(date, Dates.short, str); StdLog.String(str);
									StdLog.String("  ");
									Dates.TimeToString(time, str); StdLog.String(str)
								| CtlT.currency: StdLog.Int(SHORT(cell(CtlT.Currency).val DIV 10000));
									StdLog.Char(".");
									StdLog.IntForm(SHORT(cell(CtlT.Currency).val MOD 10000), 10, 4, "0", FALSE);
								| CtlT.boolean: StdLog.Bool(cell(CtlT.Boolean).val)
								| CtlT.string: StdLog.String(cell(CtlT.String).val^)
								| CtlT.object: StdLog.String("<object>")
								| CtlT.interface: StdLog.String("<interface>")
								END;
								StdLog.Ln
							END;
							INC(c)
						END;
						INC(r)
					END
				ELSE
					StdLog.String("not a worksheet"); StdLog.Ln
				END
			ELSE
				StdLog.String("not an OLE object"); StdLog.Ln
			END
		ELSE
			StdLog.String("no singleton"); StdLog.Ln
		END
	END ReadExcel;
	
	PROCEDURE OpenChart*;
		VAR ws: CtlExcel.Worksheet; wb: CtlExcel.Workbook; ch: CtlExcel.Chart;
			obj: CtlT.Object; v: Views.View;
			a: CtlExcel.Application;
	BEGIN
		v := Containers.FocusSingleton();
		IF v # NIL THEN
			IF OleViews.IsAutoView(v) THEN
				obj:= OleViews.AutoObject(v);
				IF CtlExcel.Is_Workbook(obj) THEN
					ws := CtlExcel.This_Worksheet(CtlExcel.This_Workbook(obj).Worksheets().Item(CtlT.Int(1)));
					a := ws.Application();
					a.PUTVisible(TRUE);
					wb := a.Workbooks().Add(NIL);
					ch := CtlExcel.This_Chart(wb.Sheets().Add(NIL, NIL, NIL, CtlT.Int(CtlExcel.xlChart)));
					ch.ChartWizard(ws.Range(CtlT.Str("A2"), CtlT.Str("E2")), CtlT.Int(CtlExcel.xlBar), NIL, NIL, NIL, NIL, NIL,
											CtlT.Str("Demo Chart"), NIL, NIL, NIL);
				ELSE
					StdLog.String("not a worksheet"); StdLog.Ln
				END
			ELSE
				StdLog.String("not an OLE object"); StdLog.Ln
			END
		ELSE
			StdLog.String("no singleton"); StdLog.Ln
		END
	END OpenChart;
	
	
END ObxExcel.
		
