MODULE ObxButtons;
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

	IMPORT Dialog, Stores, Fonts, Ports, Views, Controllers, Properties, Controls;

	CONST minVersion = 0; maxVersion = 0;

	TYPE
		View = POINTER TO RECORD (Views.View)
			font: Fonts.Font;
			color: INTEGER;
			link: Dialog.String;
			label: ARRAY 256 OF CHAR
		END;


	PROCEDURE (v: View) Internalize (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN
			Views.ReadFont(rd, v.font); rd.ReadInt(v.color); rd.ReadString(v.link); rd.ReadString(v.label)
		END
	END Internalize;

	PROCEDURE (v: View) Externalize (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion);
		Views.WriteFont(wr, v.font); wr.WriteInt(v.color); wr.WriteString(v.link); wr.WriteString(v.label)
	END Externalize;

	PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
	BEGIN
		WITH source: View DO
			v.font := source.font; v.color := source.color; v.link := source.link; v.label := source.label
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR w, h, x, y, asc, dsc, fw: INTEGER;
	BEGIN	(* restore view at least in rectangle (l, t, r, b) *)
		v.context.GetSize(w, h);	(* the container context maintains the view's size *)
		f.DrawRect(0, 0, w, h, f.dot, v.color);	(* f.dot is close to one point, i.e. 1/72 inch *)
		x := (w - v.font.StringWidth(v.label)) DIV 2;
		v.font.GetBounds(asc, dsc, fw);
		y := h DIV 2 + (asc + dsc) DIV 3;
		f.DrawString(x, y, v.color, v.label, v.font)
	END Restore;

	PROCEDURE (v: View) HandleCtrlMsg (
		f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View
	);
		VAR x, y, w, h, res: INTEGER; modifiers: SET; inside, isDown: BOOLEAN;
	BEGIN
		WITH msg: Controllers.TrackMsg DO	(* mouse button was pressed *)
			v.context.GetSize(w, h);
			f.MarkRect(0, 0, w, h, Ports.fill, Ports.invert, Ports.show); inside := TRUE;
			REPEAT	(* mouse tracking loop *)
				f.Input(x, y, modifiers, isDown);
				IF inside # (x >= 0) & (y >= 0) & (x < w) & (y < h) THEN	(* toggle state *)
					inside := ~inside; f.MarkRect(0, 0, w, h, Ports.fill, Ports.invert, inside)
				END
			UNTIL ~isDown;
			IF inside THEN	(* mouse was released inside the control *)
				f.MarkRect(0, 0, w, h, Ports.fill, Ports.invert, Ports.hide);
				IF v.link # "" THEN
					Dialog.Call(v.link, "", res)	(* interpret and execute the string in v.link *)
				END
			END
		ELSE	(* ignore other messages *)
		END
	END HandleCtrlMsg;

	PROCEDURE GetStdProp (v: View): Properties.Property;
		VAR prop: Properties.StdProp;
	BEGIN
		NEW(prop);
		prop.color.val := v.color;
		prop.typeface := v.font.typeface;
		prop.size := v.font.size;
		prop.style.val := v.font.style;
		prop.weight := v.font.weight;
		prop.valid := {Properties.color..Properties.weight}; prop.known := prop.valid;
		RETURN prop
	END GetStdProp;

	PROCEDURE SetStdProp (v: View; prop: Properties.StdProp);
		VAR typeface: Fonts.Typeface; size: INTEGER; style: SET; weight: INTEGER;
	BEGIN
		IF Properties.color IN prop.valid THEN v.color := prop.color.val END;
		typeface := v.font.typeface; size := v.font.size; style := v.font.style; weight := v.font.weight;
		IF Properties.typeface IN prop.valid THEN typeface := prop.typeface END;
		IF Properties.size IN prop.valid THEN size := prop.size END;
		IF Properties.style IN prop.valid THEN style := prop.style.val END;
		IF Properties.weight IN prop.valid THEN weight := prop.weight END;
		v.font := Fonts.dir.This(typeface, size, style, weight);		
	END SetStdProp;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		CONST defaultWidth = 20 * Ports.mm; defaultHeight = 7 * Ports.mm;
		VAR p: Controls.Prop; prop: Properties.Property;
	BEGIN
		WITH msg: Properties.FocusPref DO	(* a button is a "hot focus", i.e. does not remain focused *)
			msg.hotFocus := TRUE
		| msg: Properties.SizePref DO	(* return the default size of a button *)
			IF msg.w = Views.undefined THEN msg.w := defaultWidth END;
			IF msg.h = Views.undefined THEN msg.h := defaultHeight END
		| msg: Properties.PollMsg DO	(* return the standard font, color, and control properties *)
			NEW(p);
			p.link := v.link; p.label := v.label$;
			p.valid := {Controls.link, Controls.label}; p.known := p.valid;
			Properties.Insert(msg.prop, p);
			Properties.Insert(msg.prop, GetStdProp(v))
		| msg: Properties.SetMsg DO	(* set the standard font, color, or control properties *)
			prop := msg.prop;
			WHILE prop # NIL DO
				WITH prop: Controls.Prop DO	(* standard control properties *)
					Views.BeginModification(Views.notUndoable, v);
					IF Controls.link IN prop.valid THEN v.link := prop.link; END;
					IF Controls.label IN prop.valid THEN v.label := prop.label$ END;
					Views.EndModification(Views.notUndoable, v);
					Views.Update(v, Views.keepFrames)	(* causes a delayed redrawing of the view *)
				| prop: Properties.StdProp DO	(* standard font and color properties *)
					Views.BeginModification(Views.notUndoable, v);
					SetStdProp(v, prop);
					Views.EndModification(Views.notUndoable, v);
					Views.Update(v, Views.keepFrames)	(* causes a delayed redrawing of the view *)
				ELSE
				END;
				prop := prop.next
			END
		ELSE	(* ignore other messages *)
		END
	END HandlePropMsg;


	PROCEDURE New* (): Views.View;
		VAR v: View;
	BEGIN	(* create a new button *)
		NEW(v); v.color := Ports.defaultColor; v.font := Fonts.dir.Default(); v.link := ""; v.label := ""; RETURN v
	END New;

	PROCEDURE Deposit*;
	BEGIN
		Views.Deposit(New())
	END Deposit;

END ObxButtons.
