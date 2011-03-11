MODULE WinGLUtil ["Glu32.dll"];
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

	IMPORT WinGL;

	CONST (* macros *)
		GLU_VERSION_1_1* = 1;
		GLU_VERSION_1_2* = 1;
		GLU_INVALID_ENUM* = 100900;
		GLU_INVALID_VALUE* = 100901;
		GLU_OUT_OF_MEMORY* = 100902;
		GLU_INCOMPATIBLE_GL_VERSION* = 100903;
		GLU_VERSION* = 100800;
		GLU_EXTENSIONS* = 100801;
		GLU_TRUE* = 1;
		GLU_FALSE* = 0;
		GLU_SMOOTH* = 100000;
		GLU_FLAT* = 100001;
		GLU_NONE* = 100002;
		GLU_POINT* = 100010;
		GLU_LINE* = 100011;
		GLU_FILL* = 100012;
		GLU_SILHOUETTE* = 100013;
		GLU_OUTSIDE* = 100020;
		GLU_INSIDE* = 100021;
		GLU_TESS_TOLERANCE* = 100142;
		GLU_TESS_WINDING_ODD* = 100130;
		GLU_TESS_WINDING_NONZERO* = 100131;
		GLU_TESS_WINDING_POSITIVE* = 100132;
		GLU_TESS_WINDING_NEGATIVE* = 100133;
		GLU_TESS_WINDING_ABS_GEQ_TWO* = 100134;
		GLU_TESS_BEGIN* = 100100;
		GLU_TESS_VERTEX* = 100101;
		GLU_TESS_END* = 100102;
		GLU_TESS_ERROR* = 100103;
		GLU_TESS_EDGE_FLAG* = 100104;
		GLU_TESS_COMBINE* = 100105;
		GLU_TESS_BEGIN_DATA* = 100106;
		GLU_TESS_VERTEX_DATA* = 100107;
		GLU_TESS_END_DATA* = 100108;
		GLU_TESS_ERROR_DATA* = 100109;
		GLU_TESS_EDGE_FLAG_DATA* = 100110;
		GLU_TESS_COMBINE_DATA* = 100111;
		GLU_TESS_ERROR1* = 100151;
		GLU_TESS_ERROR2* = 100152;
		GLU_TESS_ERROR3* = 100153;
		GLU_TESS_ERROR4* = 100154;
		GLU_TESS_ERROR5* = 100155;
		GLU_TESS_ERROR6* = 100156;
		GLU_TESS_ERROR7* = 100157;
		GLU_TESS_ERROR8* = 100158;
		GLU_TESS_MISSING_BEGIN_POLYGON* = 100151;
		GLU_TESS_MISSING_BEGIN_CONTOUR* = 100152;
		GLU_TESS_MISSING_END_POLYGON* = 100153;
		GLU_TESS_MISSING_END_CONTOUR* = 100154;
		GLU_TESS_COORD_TOO_LARGE* = 100155;
		GLU_TESS_NEED_COMBINE_CALLBACK* = 100156;
		GLU_AUTO_LOAD_MATRIX* = 100200;
		GLU_CULLING* = 100201;
		GLU_SAMPLING_TOLERANCE* = 100203;
		GLU_DISPLAY_MODE* = 100204;
		GLU_PARAMETRIC_TOLERANCE* = 100202;
		GLU_SAMPLING_METHOD* = 100205;
		GLU_U_STEP* = 100206;
		GLU_V_STEP* = 100207;
		GLU_PATH_LENGTH* = 100215;
		GLU_PARAMETRIC_ERROR* = 100216;
		GLU_DOMAIN_DISTANCE* = 100217;
		GLU_MAP1_TRIM_2* = 100210;
		GLU_MAP1_TRIM_3* = 100211;
		GLU_OUTLINE_POLYGON* = 100240;
		GLU_OUTLINE_PATCH* = 100241;
		GLU_NURBS_ERROR1* = 100251;
		GLU_NURBS_ERROR2* = 100252;
		GLU_NURBS_ERROR3* = 100253;
		GLU_NURBS_ERROR4* = 100254;
		GLU_NURBS_ERROR5* = 100255;
		GLU_NURBS_ERROR6* = 100256;
		GLU_NURBS_ERROR7* = 100257;
		GLU_NURBS_ERROR8* = 100258;
		GLU_NURBS_ERROR9* = 100259;
		GLU_NURBS_ERROR10* = 100260;
		GLU_NURBS_ERROR11* = 100261;
		GLU_NURBS_ERROR12* = 100262;
		GLU_NURBS_ERROR13* = 100263;
		GLU_NURBS_ERROR14* = 100264;
		GLU_NURBS_ERROR15* = 100265;
		GLU_NURBS_ERROR16* = 100266;
		GLU_NURBS_ERROR17* = 100267;
		GLU_NURBS_ERROR18* = 100268;
		GLU_NURBS_ERROR19* = 100269;
		GLU_NURBS_ERROR20* = 100270;
		GLU_NURBS_ERROR21* = 100271;
		GLU_NURBS_ERROR22* = 100272;
		GLU_NURBS_ERROR23* = 100273;
		GLU_NURBS_ERROR24* = 100274;
		GLU_NURBS_ERROR25* = 100275;
		GLU_NURBS_ERROR26* = 100276;
		GLU_NURBS_ERROR27* = 100277;
		GLU_NURBS_ERROR28* = 100278;
		GLU_NURBS_ERROR29* = 100279;
		GLU_NURBS_ERROR30* = 100280;
		GLU_NURBS_ERROR31* = 100281;
		GLU_NURBS_ERROR32* = 100282;
		GLU_NURBS_ERROR33* = 100283;
		GLU_NURBS_ERROR34* = 100284;
		GLU_NURBS_ERROR35* = 100285;
		GLU_NURBS_ERROR36* = 100286;
		GLU_NURBS_ERROR37* = 100287;
		GLU_CW* = 100120;
		GLU_CCW* = 100121;
		GLU_INTERIOR* = 100122;
		GLU_EXTERIOR* = 100123;
		GLU_UNKNOWN* = 100124;
		GLU_BEGIN* = 100100;
		GLU_VERTEX* = 100101;
		GLU_END* = 100102;
		GLU_ERROR* = 100103;
		GLU_EDGE_FLAG* = 100104;

	CONST (* enumerations *)

	TYPE
		PtrVoid* = INTEGER;
		PtrSTR* = POINTER TO ARRAY [untagged] OF SHORTCHAR;
		PtrWSTR* = POINTER TO ARRAY [untagged] OF CHAR;
		PtrGLUquadric* = POINTER TO RECORD [untagged] END;
		PtrGLUtesselator* = POINTER TO RECORD [untagged] END;
		PtrGLUnurbs* = POINTER TO RECORD [untagged] END;
		GLUquadricErrorProc* = PROCEDURE (p0: WinGL.GLenum);
		GLUtessBeginProc* = PROCEDURE (p0: WinGL.GLenum);
		GLUtessEdgeFlagProc* = PROCEDURE (p0: WinGL.GLboolean);
		GLUtessVertexProc* = PROCEDURE (p0: PtrVoid);
		GLUtessEndProc* = PROCEDURE ();
		GLUtessErrorProc* = PROCEDURE (p0: WinGL.GLenum);
		GLUtessCombineProc* = PROCEDURE (p0: ARRAY [untagged] 3 OF WinGL.GLdouble; p1: ARRAY [untagged] 4 OF PtrVoid; p2: ARRAY [untagged] 4 OF WinGL.GLfloat; VAR [nil] p3: PtrVoid);
		GLUtessBeginDataProc* = PROCEDURE (p0: WinGL.GLenum; p1: PtrVoid);
		GLUtessEdgeFlagDataProc* = PROCEDURE (p0: WinGL.GLboolean; p1: PtrVoid);
		GLUtessVertexDataProc* = PROCEDURE (p0: PtrVoid; p1: PtrVoid);
		GLUtessEndDataProc* = PROCEDURE (p0: PtrVoid);
		GLUtessErrorDataProc* = PROCEDURE (p0: WinGL.GLenum; p1: PtrVoid);
		GLUtessCombineDataProc* = PROCEDURE (p0: ARRAY [untagged] 3 OF WinGL.GLdouble; p1: ARRAY [untagged] 4 OF PtrVoid; p2: ARRAY [untagged] 4 OF WinGL.GLfloat; VAR [nil] p3: PtrVoid; p4: PtrVoid);
		GLUnurbsErrorProc* = PROCEDURE (p0: WinGL.GLenum);

	PROCEDURE gluErrorString* (errCode: WinGL.GLenum): PtrSTR;

	PROCEDURE gluErrorUnicodeStringEXT* (errCode: WinGL.GLenum): PtrWSTR;

	PROCEDURE gluGetString* (name: WinGL.GLenum): PtrSTR;

	PROCEDURE gluOrtho2D* (left: WinGL.GLdouble; right: WinGL.GLdouble; bottom: WinGL.GLdouble; top: WinGL.GLdouble);

	PROCEDURE gluPerspective* (fovy: WinGL.GLdouble; aspect: WinGL.GLdouble; zNear: WinGL.GLdouble; zFar: WinGL.GLdouble);

	PROCEDURE gluPickMatrix* (x: WinGL.GLdouble; y: WinGL.GLdouble; width: WinGL.GLdouble; height: WinGL.GLdouble; viewport: ARRAY [untagged] 4 OF WinGL.GLint);

	PROCEDURE gluLookAt* (eyex: WinGL.GLdouble; eyey: WinGL.GLdouble; eyez: WinGL.GLdouble; centerx: WinGL.GLdouble; centery: WinGL.GLdouble; centerz: WinGL.GLdouble; upx: WinGL.GLdouble; upy: WinGL.GLdouble; upz: WinGL.GLdouble);

	PROCEDURE gluProject* (objx: WinGL.GLdouble; objy: WinGL.GLdouble; objz: WinGL.GLdouble; modelMatrix: ARRAY [untagged] 16 OF WinGL.GLdouble; projMatrix: ARRAY [untagged] 16 OF WinGL.GLdouble; viewport: ARRAY [untagged] 4 OF WinGL.GLint; VAR [nil] winx: WinGL.GLdouble; VAR [nil] winy: WinGL.GLdouble; VAR [nil] winz: WinGL.GLdouble): INTEGER;

	PROCEDURE gluUnProject* (winx: WinGL.GLdouble; winy: WinGL.GLdouble; winz: WinGL.GLdouble; modelMatrix: ARRAY [untagged] 16 OF WinGL.GLdouble; projMatrix: ARRAY [untagged] 16 OF WinGL.GLdouble; viewport: ARRAY [untagged] 4 OF WinGL.GLint; VAR [nil] objx: WinGL.GLdouble; VAR [nil] objy: WinGL.GLdouble; VAR [nil] objz: WinGL.GLdouble): INTEGER;

	PROCEDURE gluScaleImage* (format: WinGL.GLenum; widthin: WinGL.GLint; heightin: WinGL.GLint; typein: WinGL.GLenum; datain: PtrVoid; widthout: WinGL.GLint; heightout: WinGL.GLint; typeout: WinGL.GLenum; dataout: PtrVoid): INTEGER;

	PROCEDURE gluBuild1DMipmaps* (target: WinGL.GLenum; components: WinGL.GLint; width: WinGL.GLint; format: WinGL.GLenum; type: WinGL.GLenum; data: PtrVoid): INTEGER;

	PROCEDURE gluBuild2DMipmaps* (target: WinGL.GLenum; components: WinGL.GLint; width: WinGL.GLint; height: WinGL.GLint; format: WinGL.GLenum; type: WinGL.GLenum; data: PtrVoid): INTEGER;

	PROCEDURE gluNewQuadric* (): PtrGLUquadric;

	PROCEDURE gluDeleteQuadric* (state: PtrGLUquadric);

	PROCEDURE gluQuadricNormals* (quadObject: PtrGLUquadric; normals: WinGL.GLenum);

	PROCEDURE gluQuadricTexture* (quadObject: PtrGLUquadric; textureCoords: WinGL.GLboolean);

	PROCEDURE gluQuadricOrientation* (quadObject: PtrGLUquadric; orientation: WinGL.GLenum);

	PROCEDURE gluQuadricDrawStyle* (quadObject: PtrGLUquadric; drawStyle: WinGL.GLenum);

	PROCEDURE gluCylinder* (qobj: PtrGLUquadric; baseRadius: WinGL.GLdouble; topRadius: WinGL.GLdouble; height: WinGL.GLdouble; slices: WinGL.GLint; stacks: WinGL.GLint);

	PROCEDURE gluDisk* (qobj: PtrGLUquadric; innerRadius: WinGL.GLdouble; outerRadius: WinGL.GLdouble; slices: WinGL.GLint; loops: WinGL.GLint);

	PROCEDURE gluPartialDisk* (qobj: PtrGLUquadric; innerRadius: WinGL.GLdouble; outerRadius: WinGL.GLdouble; slices: WinGL.GLint; loops: WinGL.GLint; startAngle: WinGL.GLdouble; sweepAngle: WinGL.GLdouble);

	PROCEDURE gluSphere* (qobj: PtrGLUquadric; radius: WinGL.GLdouble; slices: WinGL.GLint; stacks: WinGL.GLint);

	PROCEDURE gluQuadricCallback* (qobj: PtrGLUquadric; which: WinGL.GLenum; fn: PROCEDURE ());

	PROCEDURE gluNewTess* (): PtrGLUtesselator;

	PROCEDURE gluDeleteTess* (tess: PtrGLUtesselator);

	PROCEDURE gluTessBeginPolygon* (tess: PtrGLUtesselator; polygon_data: PtrVoid);

	PROCEDURE gluTessBeginContour* (tess: PtrGLUtesselator);

	PROCEDURE gluTessVertex* (tess: PtrGLUtesselator; coords: ARRAY [untagged] 3 OF WinGL.GLdouble; data: PtrVoid);

	PROCEDURE gluTessEndContour* (tess: PtrGLUtesselator);

	PROCEDURE gluTessEndPolygon* (tess: PtrGLUtesselator);

	PROCEDURE gluTessProperty* (tess: PtrGLUtesselator; which: WinGL.GLenum; value: WinGL.GLdouble);

	PROCEDURE gluTessNormal* (tess: PtrGLUtesselator; x: WinGL.GLdouble; y: WinGL.GLdouble; z: WinGL.GLdouble);

	PROCEDURE gluTessCallback* (tess: PtrGLUtesselator; which: WinGL.GLenum; fn: PROCEDURE ());

	PROCEDURE gluGetTessProperty* (tess: PtrGLUtesselator; which: WinGL.GLenum; VAR [nil] value: WinGL.GLdouble);

	PROCEDURE gluNewNurbsRenderer* (): PtrGLUnurbs;

	PROCEDURE gluDeleteNurbsRenderer* (nobj: PtrGLUnurbs);

	PROCEDURE gluBeginSurface* (nobj: PtrGLUnurbs);

	PROCEDURE gluBeginCurve* (nobj: PtrGLUnurbs);

	PROCEDURE gluEndCurve* (nobj: PtrGLUnurbs);

	PROCEDURE gluEndSurface* (nobj: PtrGLUnurbs);

	PROCEDURE gluBeginTrim* (nobj: PtrGLUnurbs);

	PROCEDURE gluEndTrim* (nobj: PtrGLUnurbs);

	PROCEDURE gluPwlCurve* (nobj: PtrGLUnurbs; count: WinGL.GLint; VAR [nil] array: WinGL.GLfloat; stride: WinGL.GLint; type: WinGL.GLenum);

	PROCEDURE gluNurbsCurve* (nobj: PtrGLUnurbs; nknots: WinGL.GLint; VAR [nil] knot: WinGL.GLfloat; stride: WinGL.GLint; VAR [nil] ctlarray: WinGL.GLfloat; order: WinGL.GLint; type: WinGL.GLenum);

	PROCEDURE gluNurbsSurface* (nobj: PtrGLUnurbs; sknot_count: WinGL.GLint; VAR [nil] sknot: SHORTREAL; tknot_count: WinGL.GLint; VAR [nil] tknot: WinGL.GLfloat; s_stride: WinGL.GLint; t_stride: WinGL.GLint; VAR [nil] ctlarray: WinGL.GLfloat; sorder: WinGL.GLint; torder: WinGL.GLint; type: WinGL.GLenum);

	PROCEDURE gluLoadSamplingMatrices* (nobj: PtrGLUnurbs; modelMatrix: ARRAY [untagged] 16 OF WinGL.GLfloat; projMatrix: ARRAY [untagged] 16 OF WinGL.GLfloat; viewport: ARRAY [untagged] 4 OF WinGL.GLint);

	PROCEDURE gluNurbsProperty* (nobj: PtrGLUnurbs; property: WinGL.GLenum; value: WinGL.GLfloat);

	PROCEDURE gluGetNurbsProperty* (nobj: PtrGLUnurbs; property: WinGL.GLenum; VAR [nil] value: WinGL.GLfloat);

	PROCEDURE gluNurbsCallback* (nobj: PtrGLUnurbs; which: WinGL.GLenum; fn: PROCEDURE ());

	PROCEDURE gluBeginPolygon* (tess: PtrGLUtesselator);

	PROCEDURE gluNextContour* (tess: PtrGLUtesselator; type: WinGL.GLenum);

	PROCEDURE gluEndPolygon* (tess: PtrGLUtesselator);

END WinGLUtil.
