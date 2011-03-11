MODULE ObxWordEdit;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	purpose	= "Some examples of how the Word Automation Interface 9.0 can be used."
	changes	= ""
	issues	= ""

**)

	IMPORT CtlWord9, CtlT, StdLog, Fonts, Services;

	VAR
		app: CtlWord9.Application;
		connectDoc: CtlWord9.Document;

	PROCEDURE GetApplication (): CtlWord9.Application;
	BEGIN
		RETURN app
	END GetApplication;

	(* Connecting / Disconnecting *)

	PROCEDURE Connect*;
	(* Connects to a Word application. If Word is not running, it is started. *)
	BEGIN
		connectDoc := CtlWord9.NewDocument();
		connectDoc.Windows().Item(CtlT.Int(1)).PUTVisible(FALSE);
		app := connectDoc.Application();
		StdLog.String(' Connected to Word.'); StdLog.Ln()
	END Connect;

	PROCEDURE Disconnect*;
	(* Disconnects from Word. If nobody else is using the application, it is terminated. *)
	BEGIN
		connectDoc.Close(NIL, NIL, NIL);
		app := NIL;
		StdLog.String(' Disconnected to Word.'); StdLog.Ln()
	END Disconnect;

	(* Starting / Quitting *)

	PROCEDURE Start*;
	(* Starts a new Word application. *)
	BEGIN
		app := CtlWord9.NewApplication();
		app.Options().PUTSmartCutPaste(FALSE);
		app.PUTVisible(TRUE);
		StdLog.String(' Word started.'); StdLog.Ln()
	END Start;

	PROCEDURE Quit*;
	(* Quits Word. *)
	BEGIN
		app.Quit(CtlT.Bool(FALSE),NIL,NIL); app := NIL;
		StdLog.String(' Word quited.'); StdLog.Ln()
	END Quit;

	PROCEDURE Restart*;
	(* Restarts Word. *)
	BEGIN
		Quit; Start
	END Restart;

	(* File *)

	PROCEDURE NewDoc*;
	(* Creates a new document using CtlWord9.NewDocument() and makes it visible. *)
	(* The document is created in the oldest running Word. If there is no Word running, it is started. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := CtlWord9.NewDocument();
		doc.Windows().Item(CtlT.Int(1)).PUTVisible(TRUE);
		StdLog.String("New Document created. "); StdLog.Ln
	END NewDoc;

	PROCEDURE CreateDoc*;
	(* Creates a new, visible document using CtlWord9.Application.Documents().Add. *)
	(* It is only visible, if the application is visible. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().Documents().Add(NIL, NIL, NIL, CtlT.Bool(TRUE));
		StdLog.String(' Document '); StdLog.String(doc.FullName());
		StdLog.String(' created.'); StdLog.Ln
	END CreateDoc;

	PROCEDURE CreateInvisibleDoc*;
	(* Creates a new, invisible document. It is also invisible, if the application is visible. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().Documents().Add(NIL, NIL, NIL, CtlT.Bool(FALSE));
		StdLog.String(' Document '); StdLog.String(doc.FullName());
		StdLog.String(' created invisible.'); StdLog.Ln
	END CreateInvisibleDoc;

	PROCEDURE OpenDoc (filename: ARRAY OF CHAR);
	(* Opens the file filename. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().Documents().Open(CtlT.Str(filename),NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL);
		StdLog.String(' Document '); StdLog.String(doc.FullName());
		StdLog.String(' opened.'); StdLog.Ln
	END OpenDoc;

	PROCEDURE OpenDocTest*;
	(* Opens a test file. *)
	BEGIN
		OpenDoc('D:\Users\Juerg\test.doc')
	END OpenDocTest;

	PROCEDURE CloseDoc*;
	(* Closes the active document without saving. *)
		VAR doc: CtlWord9.Document; name: CtlT.Strg;
	BEGIN
		doc := GetApplication().ActiveDocument();
		name := doc.FullName();
		doc.Close(CtlT.Int(CtlWord9.wdDoNotSaveChanges),NIL,NIL);
		StdLog.String(' Document '); StdLog.String(name);
		StdLog.String(' closed.'); StdLog.Ln
	END CloseDoc;

	PROCEDURE SaveAndCloseDoc*;
	(* Saves the active document and closes it. *)
		VAR doc: CtlWord9.Document; name: CtlT.Strg;
	BEGIN
		doc := GetApplication().ActiveDocument();
		name := doc.FullName();
		doc.Close(CtlT.Int(CtlWord9.wdSaveChanges),NIL,NIL);
		StdLog.String(' Document '); StdLog.String(name);
		StdLog.String(' saved and closed.'); StdLog.Ln
	END SaveAndCloseDoc;

	PROCEDURE SaveDoc*;
	(* Saves the active document. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		doc.Save;
		StdLog.String(' Document '); StdLog.String(doc.FullName());
		StdLog.String(' saved.'); StdLog.Ln
	END SaveDoc;

	PROCEDURE Print*;
	(* Prints the active document. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		doc.PrintOut(NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL);
		StdLog.String(' Document '); StdLog.String(doc.FullName());
		StdLog.String(' printed.'); StdLog.Ln
	END Print;

	(* Application *)

	PROCEDURE DispDocs*;
	(* Displays the full names of all the open documents. *)
		VAR count, i: INTEGER; docs: CtlWord9.Documents;
	BEGIN
		docs := GetApplication().Documents();
		count := docs.Count();
		StdLog.Int(count); StdLog.String(' Documents available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(docs.Item(CtlT.Int(i)).FullName()); StdLog.Ln
		END
	END DispDocs;

	PROCEDURE DispFontNames*;
	(* Displays the names of the available fonts in Word. *)
		VAR fontNames : CtlWord9.FontNames; count, i: INTEGER;
	BEGIN
		fontNames := GetApplication().FontNames();
		count := fontNames.Count();
		StdLog.Int(count);
		StdLog.String(' FontNames available.');
		StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(fontNames.Item(i)); StdLog.Ln
		END
	END DispFontNames;

	PROCEDURE DispBBFontNames*;
	(* Displays the names of the available fonts in BlackBox. There are more than in Word. *)
		VAR t: Fonts.TypefaceInfo;
	BEGIN
		StdLog.String(' BB FontNames:');
		t := Fonts.dir.TypefaceList();
		WHILE t # NIL DO
			StdLog.String(t.typeface); StdLog.Ln;
			t := t.next
		END
	END DispBBFontNames;

	PROCEDURE DispLanguages*;
	(* Displays the languages available in Word. *)
		VAR count: INTEGER; languages: CtlWord9.Languages; lang: CtlWord9.Language; enum: CtlT.Enumerator;
	BEGIN
		languages := GetApplication().Languages();
		count := languages.Count();
		StdLog.Int(count); StdLog.String(' Languages available.'); StdLog.Ln;
		enum:= languages._NewEnum();
		lang := CtlWord9.ThisLanguage(enum.First());
		WHILE lang # NIL DO
			StdLog.String(lang.NameLocal());
			StdLog.Ln;
			lang := CtlWord9.ThisLanguage(enum.Next())
		END
	END DispLanguages;

	PROCEDURE DispLanguagesAndDictionaries*;
	(* Displays the languages available in Word and whether they have a dictionary. *)
	(* Attention: ActiveSpellingDictionary traps if there is no dictionary available... *)
		VAR count: INTEGER; languages : CtlWord9.Languages; lang: CtlWord9.Language; enum:CtlT.Enumerator;
	BEGIN
		languages := GetApplication().Languages();
		count := languages.Count();
		StdLog.Int(count); StdLog.String(' Languages available.'); StdLog.Ln;
		enum:= languages._NewEnum();
		lang := CtlWord9.ThisLanguage(enum.First());
		WHILE lang # NIL DO
			StdLog.String(lang.NameLocal());
			IF lang.ActiveSpellingDictionary() # NIL THEN
				StdLog.String(' (Dict)')
			END;
			StdLog.Ln;
			lang := CtlWord9.ThisLanguage(enum.Next())
		END
	END DispLanguagesAndDictionaries;

	PROCEDURE UseSmartCutPaste*;
	(* Sets the option SmartCutPaste. *)
	BEGIN
		GetApplication().Options().PUTSmartCutPaste(TRUE);
		StdLog.String(' SmartCutPaste turned on.'); StdLog.Ln
	END UseSmartCutPaste;

	(* Visibility *)

	PROCEDURE MakeWordVisible*;
	(* Makes all documents visible, also the ones that were created invisible. *)
	BEGIN
		GetApplication().PUTVisible(TRUE);
		StdLog.String(' Word made visible.'); StdLog.Ln
	END MakeWordVisible;

	PROCEDURE MakeWordInvisible*;
	(* Makes all documents invisible. *)
	BEGIN
		GetApplication().PUTVisible(FALSE);
		StdLog.String(' Word made invisible.'); StdLog.Ln
	END MakeWordInvisible;

	PROCEDURE MakeWinVisible*;
	(* Makes the first window of the active document visible. *)
	BEGIN
		GetApplication().ActiveDocument().Windows().Item(CtlT.Int(1)).PUTVisible(TRUE);
		StdLog.String(' Window made visible.'); StdLog.Ln
	END MakeWinVisible;

	PROCEDURE MakeWinInvisible*;
	(* Makes the first window of the active document invisible. *)
	BEGIN
		GetApplication().ActiveDocument().Windows().Item(CtlT.Int(1)).PUTVisible(FALSE);
		StdLog.String(' Window made invisible.'); StdLog.Ln
	END MakeWinInvisible;

	PROCEDURE IsWinVisible*;
	(* Displays whether the first window of the active document is visible. *)
	BEGIN
		StdLog.Bool(GetApplication().ActiveDocument().Windows().Item(CtlT.Int(1)).Visible()); StdLog.Ln
	END IsWinVisible;

	(* Document *)

	PROCEDURE Undo*;
	(* Undoes the last action. Actions, such a typing characters, can be merged to one action by Word. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		IF doc.Undo(CtlT.Int(1)) THEN
			StdLog.String(' Undone.')
		ELSE
			StdLog.String(' Undo not possible.')
		END;
		StdLog.Ln
	END Undo;

	PROCEDURE Redo*;
	(* Redoes the last undone action. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		IF doc.Redo(CtlT.Int(1)) THEN
			StdLog.String(' Redone.')
		ELSE
			StdLog.String(' Redo not possible.')
		END;
		StdLog.Ln
	END Redo;

	PROCEDURE Protect*;
	(* Protects the active document. *)
	BEGIN
		GetApplication().ActiveDocument().Protect(CtlWord9.wdAllowOnlyFormFields, NIL, NIL);
		StdLog.String('Document locked.'); StdLog.Ln
	END Protect;

	PROCEDURE Unprotect*;
	(* Unprotects the active document. *)
	BEGIN
		GetApplication().ActiveDocument().Unprotect(NIL);
		StdLog.String('Document unlocked.'); StdLog.Ln
	END Unprotect;

	(* Accessing the Content of a Document *)

	PROCEDURE DispContent*;
	(* Displays the content of the active document. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		StdLog.String('Document content: ');
		StdLog.String(doc.Content().Text()); StdLog.Ln
	END DispContent;

	PROCEDURE DispParagraphs*;
	(* Displays the paragraphs of the active document. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; paras: CtlWord9.Paragraphs;
	BEGIN
		doc := GetApplication().ActiveDocument();
		paras := doc.Paragraphs();
		count := paras.Count();
		StdLog.Int(count); StdLog.String(' Paragraphs available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(paras.Item(i).Range().Text());
			StdLog.Ln; StdLog.Ln
		END
	END DispParagraphs;

	PROCEDURE DispListParagraphs*;
	(* Displays the ListParagraphs of the active document. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; paras: CtlWord9.ListParagraphs;
	BEGIN
		doc := GetApplication().ActiveDocument();
		paras := doc.ListParagraphs();
		count := paras.Count();
		StdLog.Int(count); StdLog.String(' ListParagraphs available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(paras.Item(i).Range().Text());
			StdLog.Ln; StdLog.Ln
		END
	END DispListParagraphs;

	PROCEDURE DispLists*;
	(* Displays the Lists of the active document. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; lists: CtlWord9.Lists;
	BEGIN
		doc := GetApplication().ActiveDocument();
		lists := doc.Lists();
		count := lists.Count();
		StdLog.Int(count); StdLog.String(' Lists available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(lists.Item(i).Range().Text());
			StdLog.Ln; StdLog.Ln
		END
	END DispLists;

	PROCEDURE DispWords*;
	(* Displays the Words of the active document, using the CtlWord9.Document.Words method. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; words: CtlWord9.Words;
	BEGIN
		doc := GetApplication().ActiveDocument();
		words := doc.Words();
		count := words.Count();
		StdLog.Int(count); StdLog.String(' Words available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(words.Item(i).Text()); StdLog.Ln
		END
	END DispWords;

	PROCEDURE DispWords2*;
	(* Displays the Words of the active document, using the CtlWord9.Range.Next method. *)
		VAR len: INTEGER; doc: CtlWord9.Document; r: CtlWord9.Range;
	BEGIN
		doc := GetApplication().ActiveDocument();
		len := doc.Characters().Count();
		r := doc.Range(CtlT.Int(0),CtlT.Int(0));
		StdLog.String(' Words: '); StdLog.Ln;
		REPEAT
			r := r.Next(CtlT.Int(CtlWord9.wdWord), NIL);
			StdLog.String(r.Text()); StdLog.Ln
		UNTIL r.End() >= len
	END DispWords2;

	PROCEDURE DispWords3*;
	(* Displays the Words of the active document, using the CtlWord9.Range.MoveEnd method. *)
		VAR moved, lastEnd: INTEGER; doc: CtlWord9.Document; r: CtlWord9.Range;
	BEGIN
		doc := GetApplication().ActiveDocument();
		r := doc.Range(CtlT.Int(0),CtlT.Int(0));
		lastEnd := -1;
		StdLog.String(' Words: '); StdLog.Ln;
		REPEAT
			lastEnd := r.End();
			moved := r.MoveEnd(CtlT.Int(CtlWord9.wdWord), CtlT.Int(1));
			StdLog.String(r.Text()); StdLog.Ln;
			r.Collapse(CtlT.Int(CtlWord9.wdCollapseEnd))
		UNTIL lastEnd = r.End()
	END DispWords3;

	PROCEDURE DispCharacters*;
	(* Displays the Characters of the active document. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; chars: CtlWord9.Characters;
	BEGIN
		doc := GetApplication().ActiveDocument();
		chars := doc.Characters();
		count := chars.Count();
		StdLog.Int(count); StdLog.String(' Characters available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(chars.Item(i).Text()); StdLog.String(' ')
		END
	END DispCharacters;

	PROCEDURE DispSentences*;
	(* Displays the Sentences of the active document. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; sentences: CtlWord9.Sentences;
	BEGIN
		doc := GetApplication().ActiveDocument();
		sentences := doc.Sentences();
		count := sentences.Count();
		StdLog.Int(count); StdLog.String(' Sentences available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(sentences.Item(i).Text());
			StdLog.Ln; StdLog.Ln
		END
	END DispSentences;

	PROCEDURE DispStoryRanges*;
	(* Displays the StoryRanges of the active document. *)
		VAR count, i: INTEGER; doc: CtlWord9.Document; storyRanges: CtlWord9.StoryRanges;
	BEGIN
		doc := GetApplication().ActiveDocument();
		storyRanges := doc.StoryRanges();
		count := storyRanges.Count();
		StdLog.Int(count); StdLog.String(' StoryRanges available.'); StdLog.Ln;
		FOR i := 1 TO count DO
			StdLog.String(storyRanges.Item(i).Text());
			StdLog.Ln; StdLog.Ln
		END
	END DispStoryRanges;

	PROCEDURE DispRuns*;
	(* Should write the runs of the active document, but it does not work! How can we get the runs? *)
		VAR len: INTEGER; doc: CtlWord9.Document; r: CtlWord9.Range;
	BEGIN
		doc := GetApplication().ActiveDocument();
		len := doc.Characters().Count();
		r := doc.Range(CtlT.Int(0),CtlT.Int(0));
		StdLog.String('Runs: '); StdLog.Ln;
		REPEAT
			r := r.Next(CtlT.Int(CtlWord9.wdCharacterFormatting), NIL);
			StdLog.String(r.Text()); StdLog.Ln
		UNTIL r.End() >= len
	END DispRuns;

	(* Editing Text *)

	PROCEDURE AppendThisText*;
	(* Appends "This Text" to the end of the active document. This means that it is insert in front of the last 0DX. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		doc.Content().InsertAfter('This Text');
		StdLog.String('This Text appended.'); StdLog.Ln
	END AppendThisText;

	PROCEDURE OverwriteLastCharWithA*;
	(* Overwrites the last character, which is always a 0DX, with "A". Word then inserts a new 0DX at the end. *)
		VAR count: INTEGER; doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		count := doc.Characters().Count();
		doc.Range(CtlT.Int(count-1), CtlT.Int(count)).PUTText('A');
		StdLog.String('Last Character overwritten.'); StdLog.Ln
	END OverwriteLastCharWithA;

	PROCEDURE CopyText*;
	(* Copies and inserts the first 10 chars at the beginning of the active document. *)
		VAR doc: CtlWord9.Document; text: CtlT.Strg;
	BEGIN
		doc := GetApplication().ActiveDocument();
		text := doc.Range(CtlT.Int(0),CtlT.Int(10)).Text();
		doc.Range(CtlT.Int(0),CtlT.Int(0)).PUTText(text);
		StdLog.String('Text copied.'); StdLog.Ln
	END CopyText;

	PROCEDURE CopyFormattedText*;
	(* Copies and inserts the first 10 chars at the beginning of the active document, formatted. *)
		VAR doc: CtlWord9.Document; formattedText: CtlWord9.Range;
	BEGIN
		doc := GetApplication().ActiveDocument();
		formattedText := doc.Range(CtlT.Int(0),CtlT.Int(10)).FormattedText();
		doc.Range(CtlT.Int(0),CtlT.Int(0)).PUTFormattedText(formattedText);
		StdLog.String('Formatted text copied.'); StdLog.Ln
	END CopyFormattedText;

	PROCEDURE DeleteText*;
	(* Deletes the first 10 character of the active document. *)
		VAR doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		doc.Range(CtlT.Int(0),CtlT.Int(10)).PUTText('');
		StdLog.String('Text deleted.'); StdLog.Ln
	END DeleteText;

	(* Font *)

	PROCEDURE Disp2ndParagraphFontName*;
	(* Displays the name of the font of the 2nd paragraph of the active document, if it is defined. *)
		VAR doc: CtlWord9.Document; fontName: ARRAY 100 OF CHAR;
	BEGIN
		doc := GetApplication().ActiveDocument();
		fontName := doc.Paragraphs().Item(2).Range().Font().Name()$;
		IF fontName[0] = 0X THEN
			StdLog.String('2nd Paragraph Font Name is not defined (result = empty string)')
		ELSE
			StdLog.String('2nd Paragraph Font Name = '); StdLog.String(fontName)
		END;
		StdLog.Ln
	END Disp2ndParagraphFontName;

	PROCEDURE Is1stParagraphBold*;
	(* Displays whether the 1st paragraph of the active document is bold or not, if it is defined. *)
		VAR bold: INTEGER; doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		bold := doc.Paragraphs().Item(1).Range().Bold();
		StdLog.String('The first paragraph ');
		IF bold = 0 THEN
			StdLog.String('is Not Bold')
		ELSIF bold = -1 THEN
			StdLog.String('is Bold')
		ELSE
			StdLog.String('is neither Bold nor Not Bold')
		END;
		StdLog.String(' (result = '); StdLog.Int(bold); StdLog.String(')'); StdLog.Ln
	END Is1stParagraphBold;

	PROCEDURE Disp1stParagraphFontSize*;
	(* Displays the size of the font of the 1st paragraph of the active document, if it is defined. *)
		VAR size: SHORTREAL; doc: CtlWord9.Document;
	BEGIN
		doc := GetApplication().ActiveDocument();
		StdLog.String('The first paragraph: size =');
		size := doc.Paragraphs().Item(1).Range().Font().Size();
		IF size # CtlWord9.wdUndefined THEN
			StdLog.Real(size)
		ELSE
			StdLog.String('undefined (result = '); StdLog.Real(size); StdLog.String(' )')
		END;
		StdLog.Ln
	END Disp1stParagraphFontSize;

	(* Performance *)

	PROCEDURE Performance*;
		CONST
			wordTries = 1000;
			bbTries = 100000000;
		VAR start, counterTime, wordTime, bbTime : LONGINT; i : INTEGER; opt: CtlWord9.Options;

		PROCEDURE Put (newValue: BOOLEAN);
			VAR value: BOOLEAN;
		BEGIN
			value := newValue
		END Put;

	BEGIN
		IF app = NIL THEN Start END;
		opt := GetApplication().Options();

		start:= Services.Ticks();
		FOR i := 1 TO wordTries DO END;
		counterTime := Services.Ticks() - start;

		start:= Services.Ticks();
		FOR i := 1 TO wordTries DO opt.PUTSmartCutPaste(TRUE) END;
		wordTime := Services.Ticks() - start - counterTime;
		StdLog.Int(wordTries);
		StdLog.String(" PUTSmartCutPaste took ");
		StdLog.Real(wordTime / Services.resolution);
		StdLog.String(" sec."); StdLog.Ln;

		start:= Services.Ticks();
		FOR i := 1 TO bbTries DO END;
		counterTime := Services.Ticks() - start;

		start := Services.Ticks();
		FOR i := 1 TO bbTries DO Put(TRUE) END;
		bbTime := Services.Ticks() - start - counterTime;
		StdLog.Int(bbTries);
		StdLog.String(" BlackBox procedure calls took ");
		StdLog.Real(bbTime / Services.resolution);
		StdLog.String(" sec."); StdLog.Ln;

		StdLog.String("Properties are ");
		StdLog.Int( (wordTime * bbTries) DIV (bbTime * wordTries));
		StdLog.String(" times slower."); StdLog.Ln
	END Performance;

END ObxWordEdit.
