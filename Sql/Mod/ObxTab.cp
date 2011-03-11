MODULE SqlObxTab;
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

	IMPORT Dialog, Ports, Views, TextModels, TextViews, TextRulers, TextMappers, SqlDB;
		
	
	CONST
		protocol = "SqlOdbc3";
		id = ""; password = "";
		datasource = "Test Database";
		
		colWidth = 30 * Ports.mm;
		minCol = 4;
		
		
	VAR
		name*: ARRAY 64 OF CHAR;
		

	PROCEDURE NewRuler (tabs: INTEGER): TextRulers.Ruler;
		VAR p: TextRulers.Prop; i, w: INTEGER;
	BEGIN
		NEW(p);
		IF tabs > LEN(p.tabs.tab) THEN tabs := LEN(p.tabs.tab)
		ELSIF tabs < minCol THEN tabs := minCol
		END;
		p.valid := {TextRulers.right, TextRulers.tabs, TextRulers.opts};
		p.opts.val := {TextRulers.rightFixed}; p.opts.mask := p.opts.val;
		p.tabs.len := tabs - 1; i := 0; w := 0;
		WHILE i < tabs - 1 DO
			INC(w, colWidth); p.tabs.tab[i].stop := w; INC(i)
		END;
		INC(w, colWidth); p.right := w;
		RETURN TextRulers.dir.NewFromProp(p)
	END NewRuler;

	PROCEDURE OpenViewer (t: TextModels.Model; title: Views.Title; ruler:TextRulers.Ruler);
		VAR v: TextViews.View;
	BEGIN
		Dialog.MapString(title, title);
		v := TextViews.dir.New(t);
		IF ruler # NIL THEN v.SetDefaults(ruler, TextViews.dir.defAttr) END;
		Views.OpenAux(v, title)
	END OpenViewer;
	
	PROCEDURE ShowTables*;
		VAR db: SqlDB.Database; t, tabs: SqlDB.Table; res, row, col, n, max: INTEGER;
			text: TextModels.Model; out: TextMappers.Formatter; data: SqlDB.Row;
	BEGIN
		text := TextModels.dir.New(); out.ConnectTo(text);
		SqlDB.OpenDatabase(protocol, id, password, datasource, SqlDB.async, SqlDB.showErrors, db, res);
		tabs := db.NewTable();
		tabs.Exec("Show Tables");
		n := 0; max := 0;
		WHILE n < tabs.rows DO
			tabs.Read(n, data);
			name := data.fields[1]^$;
			out.WriteLn;
			out.WriteString("Table "); out.WriteString(name); out.WriteLn;
			out.WriteLn;
			t := db.NewTable();
			t.Exec("select * from :!SqlObxTab.name");
			row := SqlDB.names;
			WHILE row < t.rows DO
				t.Read(row, data); col := 0;
				WHILE col < t.columns DO
					out.WriteString(data.fields[col]^); out.WriteTab;
					INC(col)
				END;
				out.WriteLn; INC(row)
			END;
			IF t.columns > max THEN max := t.columns END;
			t.Clear; INC(n)
		END;
		OpenViewer(text, "Tables", NewRuler(max))
	END ShowTables;
	
END SqlObxTab.
