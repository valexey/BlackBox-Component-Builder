MODULE Config;

	IMPORT Dialog, Converters, OleData;

	PROCEDURE Setup*;
		VAR res: INTEGER;
	BEGIN
		Converters.Register("HostTextConv.ImportText", "HostTextConv.ExportText", "TextViews.View", "txt", {Converters.importAll});
		Converters.Register("HostTextConv.ImportRichText", "HostTextConv.ExportRichText", "TextViews.View", "rtf", {});
		Converters.Register("HostTextConv.ImportUnicode", "HostTextConv.ExportUnicode", "TextViews.View", "utf", {});
		Converters.Register("HostTextConv.ImportDosText", "", "TextViews.View", "txt", {});
		Converters.Register("HostTextConv.ImportHex", "", "TextViews.View", "dat", {Converters.importAll});
		Converters.Register("HostTextConv.ImportText", "HostTextConv.ExportText", "TextViews.View", "xml", {});
		Converters.Register("HostTextConv.ImportText", "HostTextConv.ExportText", "TextViews.View", "html", {});
		Converters.Register("DevBrowser.ImportSymFile", "", "TextViews.View", "osf", {});
		Converters.Register("DevBrowser.ImportCodeFile", "", "TextViews.View", "ocf", {});
		Converters.Register("HostBitmaps.ImportBitmap", "HostBitmaps.ExportBitmap", "HostBitmaps.StdView", "bmp", {});
		Converters.Register("StdETHConv.ImportETHDoc", "", "TextViews.View", "eth", {Converters.importAll});
		Converters.Register("", "XhtmlExporter.ExportText", "TextViews.View", "html", {});

		OleData.Register("OleData.ImportInfo", "OleData.ExportInfo", "BlackBox Info", "", {OleData.info});
		OleData.Register("OleData.ImportNative", "OleData.ExportNative", "BlackBox Data", "", {});
		OleData.Register("HostTextConv.ImportDRichText", "HostTextConv.ExportDRichText", "Rich Text Format", "TextViews.View", {});
		OleData.Register("OleClient.ImportInfo", "OleClient.ExportInfo", "Object Descriptor", "", {OleData.info});
		OleData.Register("", "OleClient.Export", "Embedded Object", "OleClient.View", {OleData.storage});
		OleData.Register("OleClient.Import", "OleServer.Export", "Embed Source", "", {OleData.storage});
		OleData.Register("OleClient.Import", "", "Embedded Object", "", {OleData.storage});
		OleData.Register("HostTextConv.ImportDUnicode", "HostTextConv.ExportDUnicode", "UNICODETEXT", "TextViews.View", {});
		OleData.Register("HostTextConv.ImportDText", "HostTextConv.ExportDText", "TEXT", "TextViews.View", {});
		OleData.Register("HostPictures.ImportDPict", "HostPictures.ExportDPict", "METAFILEPICT", "HostPictures.View", {});
		OleData.Register("HostBitmaps.ImportDBitmap", "HostBitmaps.ExportDBitmap", "BITMAP", "HostBitmaps.View", {});
		OleData.Register("HostBitmaps.ImportDPictAsBitmap", "", "METAFILEPICT", "HostBitmaps.View", {});
		OleData.Register("", "OleData.ExportPicture", "METAFILEPICT", "", {});

		Dialog.Call("StdLog.Open", "", res)
	END Setup;

END Config.
