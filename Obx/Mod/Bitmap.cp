MODULE ObxBitmap;
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

	IMPORT SYSTEM, WinApi, Views, HostPorts;

	TYPE
		View = POINTER TO RECORD (Views.View)
			bmp: WinApi.HBITMAP;	(* handle of bitmap *)
			w, h: INTEGER	(* size of bitmap *)
		END;

	PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
		VAR dc, bdc: WinApi.HDC; x, y, res: INTEGER;
	BEGIN
		dc := f.rider(HostPorts.Rider).port.dc;	(* get actual dc *)
		x := f.gx DIV f.unit;	(* get origin of view in dc *)
		y := f.gy DIV f.unit;
		bdc := WinApi.CreateCompatibleDC(dc);	(* create dc for bitmap *)
		res := WinApi.SelectObject(bdc, v.bmp);
		res := WinApi.BitBlt(dc, x, y, v.w, v.h, bdc, 0, 0, WinApi.SRCCOPY);	(* copy bitmap to screen *)
		res := WinApi.DeleteDC(bdc)
	END Restore;

	PROCEDURE Deposit*;
		VAR v: View; pat: ARRAY 32 OF INTEGER;
	BEGIN
		NEW(v);
		pat[0] := 000000000H; pat[1] := 0DCDDDD5DH; pat[2] := 0BABBBB3BH; pat[3] := 076777777H;
		pat[4] := 0EEEEEE6EH; pat[5] := 0DCDDDD5DH; pat[6] := 0BABBBB3BH; pat[7] := 076777777H;
		pat[8] := 0EEEEEE6EH; pat[9] := 0DCDDDD5DH; pat[10] := 0BABBBB3BH; pat[11] := 076777777H;
		pat[12] := 0EEEEEE6EH; pat[13] := 0DCDDDD5DH; pat[14] := 0BABBBB3BH; pat[15] := 076777777H;
		pat[16] := 0EEEEEE6EH; pat[17] := 0DCDDDD5DH; pat[18] := 0BABBBB3BH; pat[19] := 076777777H;
		pat[20] := 0EEEEEE6EH; pat[21] := 0DCDDDD5DH; pat[22] := 0BABBBB3BH; pat[23] := 076777777H;
		pat[24] := 0EEEEEE6EH; pat[25] := 0DCDDDD5DH; pat[26] := 0BABBBB3BH; pat[27] := 076777777H;
		pat[28] := 0EEEEEE6EH; pat[29] := 0DCDDDD5DH; pat[30] := 0BABBBB3BH; pat[31] := 000000000H;
		v.bmp := WinApi.CreateBitmap(32, 32, 1, 1, SYSTEM.ADR(pat));
		v.w := 32; v.h := 32;
		Views.Deposit(v)
	END Deposit;

END ObxBitmap.
