MODULE WinGL ["Opengl32.dll"];
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

	CONST (* macros *)
		GL_VERSION_1_1* = 1;
		GL_ACCUM* = 256;
		GL_LOAD* = 257;
		GL_RETURN* = 258;
		GL_MULT* = 259;
		GL_ADD* = 260;
		GL_NEVER* = 512;
		GL_LESS* = 513;
		GL_EQUAL* = 514;
		GL_LEQUAL* = 515;
		GL_GREATER* = 516;
		GL_NOTEQUAL* = 517;
		GL_GEQUAL* = 518;
		GL_ALWAYS* = 519;
		GL_CURRENT_BIT* = 1;
		GL_POINT_BIT* = 2;
		GL_LINE_BIT* = 4;
		GL_POLYGON_BIT* = 8;
		GL_POLYGON_STIPPLE_BIT* = 16;
		GL_PIXEL_MODE_BIT* = 32;
		GL_LIGHTING_BIT* = 64;
		GL_FOG_BIT* = 128;
		GL_DEPTH_BUFFER_BIT* = 256;
		GL_ACCUM_BUFFER_BIT* = 512;
		GL_STENCIL_BUFFER_BIT* = 1024;
		GL_VIEWPORT_BIT* = 2048;
		GL_TRANSFORM_BIT* = 4096;
		GL_ENABLE_BIT* = 8192;
		GL_COLOR_BUFFER_BIT* = 16384;
		GL_HINT_BIT* = 32768;
		GL_EVAL_BIT* = 65536;
		GL_LIST_BIT* = 131072;
		GL_TEXTURE_BIT* = 262144;
		GL_SCISSOR_BIT* = 524288;
		GL_ALL_ATTRIB_BITS* = 1048575;
		GL_POINTS* = 0;
		GL_LINES* = 1;
		GL_LINE_LOOP* = 2;
		GL_LINE_STRIP* = 3;
		GL_TRIANGLES* = 4;
		GL_TRIANGLE_STRIP* = 5;
		GL_TRIANGLE_FAN* = 6;
		GL_QUADS* = 7;
		GL_QUAD_STRIP* = 8;
		GL_POLYGON* = 9;
		GL_ZERO* = 0;
		GL_ONE* = 1;
		GL_SRC_COLOR* = 768;
		GL_ONE_MINUS_SRC_COLOR* = 769;
		GL_SRC_ALPHA* = 770;
		GL_ONE_MINUS_SRC_ALPHA* = 771;
		GL_DST_ALPHA* = 772;
		GL_ONE_MINUS_DST_ALPHA* = 773;
		GL_DST_COLOR* = 774;
		GL_ONE_MINUS_DST_COLOR* = 775;
		GL_SRC_ALPHA_SATURATE* = 776;
		GL_TRUE* = 1;
		GL_FALSE* = 0;
		GL_CLIP_PLANE0* = 12288;
		GL_CLIP_PLANE1* = 12289;
		GL_CLIP_PLANE2* = 12290;
		GL_CLIP_PLANE3* = 12291;
		GL_CLIP_PLANE4* = 12292;
		GL_CLIP_PLANE5* = 12293;
		GL_BYTE* = 5120;
		GL_UNSIGNED_BYTE* = 5121;
		GL_SHORT* = 5122;
		GL_UNSIGNED_SHORT* = 5123;
		GL_INT* = 5124;
		GL_UNSIGNED_INT* = 5125;
		GL_FLOAT* = 5126;
		GL_2_BYTES* = 5127;
		GL_3_BYTES* = 5128;
		GL_4_BYTES* = 5129;
		GL_DOUBLE* = 5130;
		GL_NONE* = 0;
		GL_FRONT_LEFT* = 1024;
		GL_FRONT_RIGHT* = 1025;
		GL_BACK_LEFT* = 1026;
		GL_BACK_RIGHT* = 1027;
		GL_FRONT* = 1028;
		GL_BACK* = 1029;
		GL_LEFT* = 1030;
		GL_RIGHT* = 1031;
		GL_FRONT_AND_BACK* = 1032;
		GL_AUX0* = 1033;
		GL_AUX1* = 1034;
		GL_AUX2* = 1035;
		GL_AUX3* = 1036;
		GL_NO_ERROR* = 0;
		GL_INVALID_ENUM* = 1280;
		GL_INVALID_VALUE* = 1281;
		GL_INVALID_OPERATION* = 1282;
		GL_STACK_OVERFLOW* = 1283;
		GL_STACK_UNDERFLOW* = 1284;
		GL_OUT_OF_MEMORY* = 1285;
		GL_2D* = 1536;
		GL_3D* = 1537;
		GL_3D_COLOR* = 1538;
		GL_3D_COLOR_TEXTURE* = 1539;
		GL_4D_COLOR_TEXTURE* = 1540;
		GL_PASS_THROUGH_TOKEN* = 1792;
		GL_POINT_TOKEN* = 1793;
		GL_LINE_TOKEN* = 1794;
		GL_POLYGON_TOKEN* = 1795;
		GL_BITMAP_TOKEN* = 1796;
		GL_DRAW_PIXEL_TOKEN* = 1797;
		GL_COPY_PIXEL_TOKEN* = 1798;
		GL_LINE_RESET_TOKEN* = 1799;
		GL_EXP* = 2048;
		GL_EXP2* = 2049;
		GL_CW* = 2304;
		GL_CCW* = 2305;
		GL_COEFF* = 2560;
		GL_ORDER* = 2561;
		GL_DOMAIN* = 2562;
		GL_CURRENT_COLOR* = 2816;
		GL_CURRENT_INDEX* = 2817;
		GL_CURRENT_NORMAL* = 2818;
		GL_CURRENT_TEXTURE_COORDS* = 2819;
		GL_CURRENT_RASTER_COLOR* = 2820;
		GL_CURRENT_RASTER_INDEX* = 2821;
		GL_CURRENT_RASTER_TEXTURE_COORDS* = 2822;
		GL_CURRENT_RASTER_POSITION* = 2823;
		GL_CURRENT_RASTER_POSITION_VALID* = 2824;
		GL_CURRENT_RASTER_DISTANCE* = 2825;
		GL_POINT_SMOOTH* = 2832;
		GL_POINT_SIZE* = 2833;
		GL_POINT_SIZE_RANGE* = 2834;
		GL_POINT_SIZE_GRANULARITY* = 2835;
		GL_LINE_SMOOTH* = 2848;
		GL_LINE_WIDTH* = 2849;
		GL_LINE_WIDTH_RANGE* = 2850;
		GL_LINE_WIDTH_GRANULARITY* = 2851;
		GL_LINE_STIPPLE* = 2852;
		GL_LINE_STIPPLE_PATTERN* = 2853;
		GL_LINE_STIPPLE_REPEAT* = 2854;
		GL_LIST_MODE* = 2864;
		GL_MAX_LIST_NESTING* = 2865;
		GL_LIST_BASE* = 2866;
		GL_LIST_INDEX* = 2867;
		GL_POLYGON_MODE* = 2880;
		GL_POLYGON_SMOOTH* = 2881;
		GL_POLYGON_STIPPLE* = 2882;
		GL_EDGE_FLAG* = 2883;
		GL_CULL_FACE* = 2884;
		GL_CULL_FACE_MODE* = 2885;
		GL_FRONT_FACE* = 2886;
		GL_LIGHTING* = 2896;
		GL_LIGHT_MODEL_LOCAL_VIEWER* = 2897;
		GL_LIGHT_MODEL_TWO_SIDE* = 2898;
		GL_LIGHT_MODEL_AMBIENT* = 2899;
		GL_SHADE_MODEL* = 2900;
		GL_COLOR_MATERIAL_FACE* = 2901;
		GL_COLOR_MATERIAL_PARAMETER* = 2902;
		GL_COLOR_MATERIAL* = 2903;
		GL_FOG* = 2912;
		GL_FOG_INDEX* = 2913;
		GL_FOG_DENSITY* = 2914;
		GL_FOG_START* = 2915;
		GL_FOG_END* = 2916;
		GL_FOG_MODE* = 2917;
		GL_FOG_COLOR* = 2918;
		GL_DEPTH_RANGE* = 2928;
		GL_DEPTH_TEST* = 2929;
		GL_DEPTH_WRITEMASK* = 2930;
		GL_DEPTH_CLEAR_VALUE* = 2931;
		GL_DEPTH_FUNC* = 2932;
		GL_ACCUM_CLEAR_VALUE* = 2944;
		GL_STENCIL_TEST* = 2960;
		GL_STENCIL_CLEAR_VALUE* = 2961;
		GL_STENCIL_FUNC* = 2962;
		GL_STENCIL_VALUE_MASK* = 2963;
		GL_STENCIL_FAIL* = 2964;
		GL_STENCIL_PASS_DEPTH_FAIL* = 2965;
		GL_STENCIL_PASS_DEPTH_PASS* = 2966;
		GL_STENCIL_REF* = 2967;
		GL_STENCIL_WRITEMASK* = 2968;
		GL_MATRIX_MODE* = 2976;
		GL_NORMALIZE* = 2977;
		GL_VIEWPORT* = 2978;
		GL_MODELVIEW_STACK_DEPTH* = 2979;
		GL_PROJECTION_STACK_DEPTH* = 2980;
		GL_TEXTURE_STACK_DEPTH* = 2981;
		GL_MODELVIEW_MATRIX* = 2982;
		GL_PROJECTION_MATRIX* = 2983;
		GL_TEXTURE_MATRIX* = 2984;
		GL_ATTRIB_STACK_DEPTH* = 2992;
		GL_CLIENT_ATTRIB_STACK_DEPTH* = 2993;
		GL_ALPHA_TEST* = 3008;
		GL_ALPHA_TEST_FUNC* = 3009;
		GL_ALPHA_TEST_REF* = 3010;
		GL_DITHER* = 3024;
		GL_BLEND_DST* = 3040;
		GL_BLEND_SRC* = 3041;
		GL_BLEND* = 3042;
		GL_LOGIC_OP_MODE* = 3056;
		GL_INDEX_LOGIC_OP* = 3057;
		GL_COLOR_LOGIC_OP* = 3058;
		GL_AUX_BUFFERS* = 3072;
		GL_DRAW_BUFFER* = 3073;
		GL_READ_BUFFER* = 3074;
		GL_SCISSOR_BOX* = 3088;
		GL_SCISSOR_TEST* = 3089;
		GL_INDEX_CLEAR_VALUE* = 3104;
		GL_INDEX_WRITEMASK* = 3105;
		GL_COLOR_CLEAR_VALUE* = 3106;
		GL_COLOR_WRITEMASK* = 3107;
		GL_INDEX_MODE* = 3120;
		GL_RGBA_MODE* = 3121;
		GL_DOUBLEBUFFER* = 3122;
		GL_STEREO* = 3123;
		GL_RENDER_MODE* = 3136;
		GL_PERSPECTIVE_CORRECTION_HINT* = 3152;
		GL_POINT_SMOOTH_HINT* = 3153;
		GL_LINE_SMOOTH_HINT* = 3154;
		GL_POLYGON_SMOOTH_HINT* = 3155;
		GL_FOG_HINT* = 3156;
		GL_TEXTURE_GEN_S* = 3168;
		GL_TEXTURE_GEN_T* = 3169;
		GL_TEXTURE_GEN_R* = 3170;
		GL_TEXTURE_GEN_Q* = 3171;
		GL_PIXEL_MAP_I_TO_I* = 3184;
		GL_PIXEL_MAP_S_TO_S* = 3185;
		GL_PIXEL_MAP_I_TO_R* = 3186;
		GL_PIXEL_MAP_I_TO_G* = 3187;
		GL_PIXEL_MAP_I_TO_B* = 3188;
		GL_PIXEL_MAP_I_TO_A* = 3189;
		GL_PIXEL_MAP_R_TO_R* = 3190;
		GL_PIXEL_MAP_G_TO_G* = 3191;
		GL_PIXEL_MAP_B_TO_B* = 3192;
		GL_PIXEL_MAP_A_TO_A* = 3193;
		GL_PIXEL_MAP_I_TO_I_SIZE* = 3248;
		GL_PIXEL_MAP_S_TO_S_SIZE* = 3249;
		GL_PIXEL_MAP_I_TO_R_SIZE* = 3250;
		GL_PIXEL_MAP_I_TO_G_SIZE* = 3251;
		GL_PIXEL_MAP_I_TO_B_SIZE* = 3252;
		GL_PIXEL_MAP_I_TO_A_SIZE* = 3253;
		GL_PIXEL_MAP_R_TO_R_SIZE* = 3254;
		GL_PIXEL_MAP_G_TO_G_SIZE* = 3255;
		GL_PIXEL_MAP_B_TO_B_SIZE* = 3256;
		GL_PIXEL_MAP_A_TO_A_SIZE* = 3257;
		GL_UNPACK_SWAP_BYTES* = 3312;
		GL_UNPACK_LSB_FIRST* = 3313;
		GL_UNPACK_ROW_LENGTH* = 3314;
		GL_UNPACK_SKIP_ROWS* = 3315;
		GL_UNPACK_SKIP_PIXELS* = 3316;
		GL_UNPACK_ALIGNMENT* = 3317;
		GL_PACK_SWAP_BYTES* = 3328;
		GL_PACK_LSB_FIRST* = 3329;
		GL_PACK_ROW_LENGTH* = 3330;
		GL_PACK_SKIP_ROWS* = 3331;
		GL_PACK_SKIP_PIXELS* = 3332;
		GL_PACK_ALIGNMENT* = 3333;
		GL_MAP_COLOR* = 3344;
		GL_MAP_STENCIL* = 3345;
		GL_INDEX_SHIFT* = 3346;
		GL_INDEX_OFFSET* = 3347;
		GL_RED_SCALE* = 3348;
		GL_RED_BIAS* = 3349;
		GL_ZOOM_X* = 3350;
		GL_ZOOM_Y* = 3351;
		GL_GREEN_SCALE* = 3352;
		GL_GREEN_BIAS* = 3353;
		GL_BLUE_SCALE* = 3354;
		GL_BLUE_BIAS* = 3355;
		GL_ALPHA_SCALE* = 3356;
		GL_ALPHA_BIAS* = 3357;
		GL_DEPTH_SCALE* = 3358;
		GL_DEPTH_BIAS* = 3359;
		GL_MAX_EVAL_ORDER* = 3376;
		GL_MAX_LIGHTS* = 3377;
		GL_MAX_CLIP_PLANES* = 3378;
		GL_MAX_TEXTURE_SIZE* = 3379;
		GL_MAX_PIXEL_MAP_TABLE* = 3380;
		GL_MAX_ATTRIB_STACK_DEPTH* = 3381;
		GL_MAX_MODELVIEW_STACK_DEPTH* = 3382;
		GL_MAX_NAME_STACK_DEPTH* = 3383;
		GL_MAX_PROJECTION_STACK_DEPTH* = 3384;
		GL_MAX_TEXTURE_STACK_DEPTH* = 3385;
		GL_MAX_VIEWPORT_DIMS* = 3386;
		GL_MAX_CLIENT_ATTRIB_STACK_DEPTH* = 3387;
		GL_SUBPIXEL_BITS* = 3408;
		GL_INDEX_BITS* = 3409;
		GL_RED_BITS* = 3410;
		GL_GREEN_BITS* = 3411;
		GL_BLUE_BITS* = 3412;
		GL_ALPHA_BITS* = 3413;
		GL_DEPTH_BITS* = 3414;
		GL_STENCIL_BITS* = 3415;
		GL_ACCUM_RED_BITS* = 3416;
		GL_ACCUM_GREEN_BITS* = 3417;
		GL_ACCUM_BLUE_BITS* = 3418;
		GL_ACCUM_ALPHA_BITS* = 3419;
		GL_NAME_STACK_DEPTH* = 3440;
		GL_AUTO_NORMAL* = 3456;
		GL_MAP1_COLOR_4* = 3472;
		GL_MAP1_INDEX* = 3473;
		GL_MAP1_NORMAL* = 3474;
		GL_MAP1_TEXTURE_COORD_1* = 3475;
		GL_MAP1_TEXTURE_COORD_2* = 3476;
		GL_MAP1_TEXTURE_COORD_3* = 3477;
		GL_MAP1_TEXTURE_COORD_4* = 3478;
		GL_MAP1_VERTEX_3* = 3479;
		GL_MAP1_VERTEX_4* = 3480;
		GL_MAP2_COLOR_4* = 3504;
		GL_MAP2_INDEX* = 3505;
		GL_MAP2_NORMAL* = 3506;
		GL_MAP2_TEXTURE_COORD_1* = 3507;
		GL_MAP2_TEXTURE_COORD_2* = 3508;
		GL_MAP2_TEXTURE_COORD_3* = 3509;
		GL_MAP2_TEXTURE_COORD_4* = 3510;
		GL_MAP2_VERTEX_3* = 3511;
		GL_MAP2_VERTEX_4* = 3512;
		GL_MAP1_GRID_DOMAIN* = 3536;
		GL_MAP1_GRID_SEGMENTS* = 3537;
		GL_MAP2_GRID_DOMAIN* = 3538;
		GL_MAP2_GRID_SEGMENTS* = 3539;
		GL_TEXTURE_1D* = 3552;
		GL_TEXTURE_2D* = 3553;
		GL_FEEDBACK_BUFFER_POINTER* = 3568;
		GL_FEEDBACK_BUFFER_SIZE* = 3569;
		GL_FEEDBACK_BUFFER_TYPE* = 3570;
		GL_SELECTION_BUFFER_POINTER* = 3571;
		GL_SELECTION_BUFFER_SIZE* = 3572;
		GL_TEXTURE_WIDTH* = 4096;
		GL_TEXTURE_HEIGHT* = 4097;
		GL_TEXTURE_INTERNAL_FORMAT* = 4099;
		GL_TEXTURE_BORDER_COLOR* = 4100;
		GL_TEXTURE_BORDER* = 4101;
		GL_DONT_CARE* = 4352;
		GL_FASTEST* = 4353;
		GL_NICEST* = 4354;
		GL_LIGHT0* = 16384;
		GL_LIGHT1* = 16385;
		GL_LIGHT2* = 16386;
		GL_LIGHT3* = 16387;
		GL_LIGHT4* = 16388;
		GL_LIGHT5* = 16389;
		GL_LIGHT6* = 16390;
		GL_LIGHT7* = 16391;
		GL_AMBIENT* = 4608;
		GL_DIFFUSE* = 4609;
		GL_SPECULAR* = 4610;
		GL_POSITION* = 4611;
		GL_SPOT_DIRECTION* = 4612;
		GL_SPOT_EXPONENT* = 4613;
		GL_SPOT_CUTOFF* = 4614;
		GL_CONSTANT_ATTENUATION* = 4615;
		GL_LINEAR_ATTENUATION* = 4616;
		GL_QUADRATIC_ATTENUATION* = 4617;
		GL_COMPILE* = 4864;
		GL_COMPILE_AND_EXECUTE* = 4865;
		GL_CLEAR* = 5376;
		GL_AND* = 5377;
		GL_AND_REVERSE* = 5378;
		GL_COPY* = 5379;
		GL_AND_INVERTED* = 5380;
		GL_NOOP* = 5381;
		GL_XOR* = 5382;
		GL_OR* = 5383;
		GL_NOR* = 5384;
		GL_EQUIV* = 5385;
		GL_INVERT* = 5386;
		GL_OR_REVERSE* = 5387;
		GL_COPY_INVERTED* = 5388;
		GL_OR_INVERTED* = 5389;
		GL_NAND* = 5390;
		GL_SET* = 5391;
		GL_EMISSION* = 5632;
		GL_SHININESS* = 5633;
		GL_AMBIENT_AND_DIFFUSE* = 5634;
		GL_COLOR_INDEXES* = 5635;
		GL_MODELVIEW* = 5888;
		GL_PROJECTION* = 5889;
		GL_TEXTURE* = 5890;
		GL_COLOR* = 6144;
		GL_DEPTH* = 6145;
		GL_STENCIL* = 6146;
		GL_COLOR_INDEX* = 6400;
		GL_STENCIL_INDEX* = 6401;
		GL_DEPTH_COMPONENT* = 6402;
		GL_RED* = 6403;
		GL_GREEN* = 6404;
		GL_BLUE* = 6405;
		GL_ALPHA* = 6406;
		GL_RGB* = 6407;
		GL_RGBA* = 6408;
		GL_LUMINANCE* = 6409;
		GL_LUMINANCE_ALPHA* = 6410;
		GL_BITMAP* = 6656;
		GL_POINT* = 6912;
		GL_LINE* = 6913;
		GL_FILL* = 6914;
		GL_RENDER* = 7168;
		GL_FEEDBACK* = 7169;
		GL_SELECT* = 7170;
		GL_FLAT* = 7424;
		GL_SMOOTH* = 7425;
		GL_KEEP* = 7680;
		GL_REPLACE* = 7681;
		GL_INCR* = 7682;
		GL_DECR* = 7683;
		GL_VENDOR* = 7936;
		GL_RENDERER* = 7937;
		GL_VERSION* = 7938;
		GL_EXTENSIONS* = 7939;
		GL_S* = 8192;
		GL_T* = 8193;
		GL_R* = 8194;
		GL_Q* = 8195;
		GL_MODULATE* = 8448;
		GL_DECAL* = 8449;
		GL_TEXTURE_ENV_MODE* = 8704;
		GL_TEXTURE_ENV_COLOR* = 8705;
		GL_TEXTURE_ENV* = 8960;
		GL_EYE_LINEAR* = 9216;
		GL_OBJECT_LINEAR* = 9217;
		GL_SPHERE_MAP* = 9218;
		GL_TEXTURE_GEN_MODE* = 9472;
		GL_OBJECT_PLANE* = 9473;
		GL_EYE_PLANE* = 9474;
		GL_NEAREST* = 9728;
		GL_LINEAR* = 9729;
		GL_NEAREST_MIPMAP_NEAREST* = 9984;
		GL_LINEAR_MIPMAP_NEAREST* = 9985;
		GL_NEAREST_MIPMAP_LINEAR* = 9986;
		GL_LINEAR_MIPMAP_LINEAR* = 9987;
		GL_TEXTURE_MAG_FILTER* = 10240;
		GL_TEXTURE_MIN_FILTER* = 10241;
		GL_TEXTURE_WRAP_S* = 10242;
		GL_TEXTURE_WRAP_T* = 10243;
		GL_CLAMP* = 10496;
		GL_REPEAT* = 10497;
		GL_CLIENT_PIXEL_STORE_BIT* = 1;
		GL_CLIENT_VERTEX_ARRAY_BIT* = 2;
		GL_CLIENT_ALL_ATTRIB_BITS* = -1;
		GL_POLYGON_OFFSET_FACTOR* = 32824;
		GL_POLYGON_OFFSET_UNITS* = 10752;
		GL_POLYGON_OFFSET_POINT* = 10753;
		GL_POLYGON_OFFSET_LINE* = 10754;
		GL_POLYGON_OFFSET_FILL* = 32823;
		GL_ALPHA4* = 32827;
		GL_ALPHA8* = 32828;
		GL_ALPHA12* = 32829;
		GL_ALPHA16* = 32830;
		GL_LUMINANCE4* = 32831;
		GL_LUMINANCE8* = 32832;
		GL_LUMINANCE12* = 32833;
		GL_LUMINANCE16* = 32834;
		GL_LUMINANCE4_ALPHA4* = 32835;
		GL_LUMINANCE6_ALPHA2* = 32836;
		GL_LUMINANCE8_ALPHA8* = 32837;
		GL_LUMINANCE12_ALPHA4* = 32838;
		GL_LUMINANCE12_ALPHA12* = 32839;
		GL_LUMINANCE16_ALPHA16* = 32840;
		GL_INTENSITY* = 32841;
		GL_INTENSITY4* = 32842;
		GL_INTENSITY8* = 32843;
		GL_INTENSITY12* = 32844;
		GL_INTENSITY16* = 32845;
		GL_R3_G3_B2* = 10768;
		GL_RGB4* = 32847;
		GL_RGB5* = 32848;
		GL_RGB8* = 32849;
		GL_RGB10* = 32850;
		GL_RGB12* = 32851;
		GL_RGB16* = 32852;
		GL_RGBA2* = 32853;
		GL_RGBA4* = 32854;
		GL_RGB5_A1* = 32855;
		GL_RGBA8* = 32856;
		GL_RGB10_A2* = 32857;
		GL_RGBA12* = 32858;
		GL_RGBA16* = 32859;
		GL_TEXTURE_RED_SIZE* = 32860;
		GL_TEXTURE_GREEN_SIZE* = 32861;
		GL_TEXTURE_BLUE_SIZE* = 32862;
		GL_TEXTURE_ALPHA_SIZE* = 32863;
		GL_TEXTURE_LUMINANCE_SIZE* = 32864;
		GL_TEXTURE_INTENSITY_SIZE* = 32865;
		GL_PROXY_TEXTURE_1D* = 32867;
		GL_PROXY_TEXTURE_2D* = 32868;
		GL_TEXTURE_PRIORITY* = 32870;
		GL_TEXTURE_RESIDENT* = 32871;
		GL_TEXTURE_BINDING_1D* = 32872;
		GL_TEXTURE_BINDING_2D* = 32873;
		GL_VERTEX_ARRAY* = 32884;
		GL_NORMAL_ARRAY* = 32885;
		GL_COLOR_ARRAY* = 32886;
		GL_INDEX_ARRAY* = 32887;
		GL_TEXTURE_COORD_ARRAY* = 32888;
		GL_EDGE_FLAG_ARRAY* = 32889;
		GL_VERTEX_ARRAY_SIZE* = 32890;
		GL_VERTEX_ARRAY_TYPE* = 32891;
		GL_VERTEX_ARRAY_STRIDE* = 32892;
		GL_NORMAL_ARRAY_TYPE* = 32894;
		GL_NORMAL_ARRAY_STRIDE* = 32895;
		GL_COLOR_ARRAY_SIZE* = 32897;
		GL_COLOR_ARRAY_TYPE* = 32898;
		GL_COLOR_ARRAY_STRIDE* = 32899;
		GL_INDEX_ARRAY_TYPE* = 32901;
		GL_INDEX_ARRAY_STRIDE* = 32902;
		GL_TEXTURE_COORD_ARRAY_SIZE* = 32904;
		GL_TEXTURE_COORD_ARRAY_TYPE* = 32905;
		GL_TEXTURE_COORD_ARRAY_STRIDE* = 32906;
		GL_EDGE_FLAG_ARRAY_STRIDE* = 32908;
		GL_VERTEX_ARRAY_POINTER* = 32910;
		GL_NORMAL_ARRAY_POINTER* = 32911;
		GL_COLOR_ARRAY_POINTER* = 32912;
		GL_INDEX_ARRAY_POINTER* = 32913;
		GL_TEXTURE_COORD_ARRAY_POINTER* = 32914;
		GL_EDGE_FLAG_ARRAY_POINTER* = 32915;
		GL_V2F* = 10784;
		GL_V3F* = 10785;
		GL_C4UB_V2F* = 10786;
		GL_C4UB_V3F* = 10787;
		GL_C3F_V3F* = 10788;
		GL_N3F_V3F* = 10789;
		GL_C4F_N3F_V3F* = 10790;
		GL_T2F_V3F* = 10791;
		GL_T4F_V4F* = 10792;
		GL_T2F_C4UB_V3F* = 10793;
		GL_T2F_C3F_V3F* = 10794;
		GL_T2F_N3F_V3F* = 10795;
		GL_T2F_C4F_N3F_V3F* = 10796;
		GL_T4F_C4F_N3F_V4F* = 10797;
		GL_EXT_vertex_array* = 1;
		GL_WIN_swap_hint* = 1;
		GL_EXT_bgra* = 1;
		GL_EXT_paletted_texture* = 1;
		GL_VERTEX_ARRAY_EXT* = 32884;
		GL_NORMAL_ARRAY_EXT* = 32885;
		GL_COLOR_ARRAY_EXT* = 32886;
		GL_INDEX_ARRAY_EXT* = 32887;
		GL_TEXTURE_COORD_ARRAY_EXT* = 32888;
		GL_EDGE_FLAG_ARRAY_EXT* = 32889;
		GL_VERTEX_ARRAY_SIZE_EXT* = 32890;
		GL_VERTEX_ARRAY_TYPE_EXT* = 32891;
		GL_VERTEX_ARRAY_STRIDE_EXT* = 32892;
		GL_VERTEX_ARRAY_COUNT_EXT* = 32893;
		GL_NORMAL_ARRAY_TYPE_EXT* = 32894;
		GL_NORMAL_ARRAY_STRIDE_EXT* = 32895;
		GL_NORMAL_ARRAY_COUNT_EXT* = 32896;
		GL_COLOR_ARRAY_SIZE_EXT* = 32897;
		GL_COLOR_ARRAY_TYPE_EXT* = 32898;
		GL_COLOR_ARRAY_STRIDE_EXT* = 32899;
		GL_COLOR_ARRAY_COUNT_EXT* = 32900;
		GL_INDEX_ARRAY_TYPE_EXT* = 32901;
		GL_INDEX_ARRAY_STRIDE_EXT* = 32902;
		GL_INDEX_ARRAY_COUNT_EXT* = 32903;
		GL_TEXTURE_COORD_ARRAY_SIZE_EXT* = 32904;
		GL_TEXTURE_COORD_ARRAY_TYPE_EXT* = 32905;
		GL_TEXTURE_COORD_ARRAY_STRIDE_EXT* = 32906;
		GL_TEXTURE_COORD_ARRAY_COUNT_EXT* = 32907;
		GL_EDGE_FLAG_ARRAY_STRIDE_EXT* = 32908;
		GL_EDGE_FLAG_ARRAY_COUNT_EXT* = 32909;
		GL_VERTEX_ARRAY_POINTER_EXT* = 32910;
		GL_NORMAL_ARRAY_POINTER_EXT* = 32911;
		GL_COLOR_ARRAY_POINTER_EXT* = 32912;
		GL_INDEX_ARRAY_POINTER_EXT* = 32913;
		GL_TEXTURE_COORD_ARRAY_POINTER_EXT* = 32914;
		GL_EDGE_FLAG_ARRAY_POINTER_EXT* = 32915;
		GL_DOUBLE_EXT* = 5130;
		GL_BGR_EXT* = 32992;
		GL_BGRA_EXT* = 32993;
		GL_COLOR_TABLE_FORMAT_EXT* = 32984;
		GL_COLOR_TABLE_WIDTH_EXT* = 32985;
		GL_COLOR_TABLE_RED_SIZE_EXT* = 32986;
		GL_COLOR_TABLE_GREEN_SIZE_EXT* = 32987;
		GL_COLOR_TABLE_BLUE_SIZE_EXT* = 32988;
		GL_COLOR_TABLE_ALPHA_SIZE_EXT* = 32989;
		GL_COLOR_TABLE_LUMINANCE_SIZE_EXT* = 32990;
		GL_COLOR_TABLE_INTENSITY_SIZE_EXT* = 32991;
		GL_COLOR_INDEX1_EXT* = 32994;
		GL_COLOR_INDEX2_EXT* = 32995;
		GL_COLOR_INDEX4_EXT* = 32996;
		GL_COLOR_INDEX8_EXT* = 32997;
		GL_COLOR_INDEX12_EXT* = 32998;
		GL_COLOR_INDEX16_EXT* = 32999;
		GL_LOGIC_OP* = 3057;
		GL_TEXTURE_COMPONENTS* = 4099;

	CONST (* enumerations *)

	TYPE
		PtrSTR* = POINTER TO ARRAY [untagged] OF SHORTCHAR;
		GLenum* = INTEGER;
		GLboolean* = BOOLEAN;
		GLbitfield* = INTEGER;
		GLbyte* = BYTE;
		GLshort* = SHORTINT;
		GLint* = INTEGER;
		GLsizei* = INTEGER;
		GLubyte* = SHORTCHAR;
		GLushort* = SHORTINT;
		GLuint* = INTEGER;
		GLfloat* = SHORTREAL;
		GLclampf* = SHORTREAL;
		GLdouble* = REAL;
		GLclampd* = REAL;
		PtrGLvoid* = INTEGER;
		FNGLARRAYELEMENTEXTPROC* = PROCEDURE (i: GLint);
		FNGLDRAWARRAYSEXTPROC* = PROCEDURE (mode: GLenum; first: GLint; count: GLsizei);
		FNGLVERTEXPOINTEREXTPROC* = PROCEDURE (size: GLint; type: GLenum; stride: GLsizei; count: GLsizei; pointer: PtrGLvoid);
		FNGLNORMALPOINTEREXTPROC* = PROCEDURE (type: GLenum; stride: GLsizei; count: GLsizei; pointer: PtrGLvoid);
		FNGLCOLORPOINTEREXTPROC* = PROCEDURE (size: GLint; type: GLenum; stride: GLsizei; count: GLsizei; pointer: PtrGLvoid);
		FNGLINDEXPOINTEREXTPROC* = PROCEDURE (type: GLenum; stride: GLsizei; count: GLsizei; pointer: PtrGLvoid);
		FNGLTEXCOORDPOINTEREXTPROC* = PROCEDURE (size: GLint; type: GLenum; stride: GLsizei; count: GLsizei; pointer: PtrGLvoid);
		FNGLEDGEFLAGPOINTEREXTPROC* = PROCEDURE (stride: GLsizei; count: GLsizei; pointer: PtrSTR);
		FNGLGETPOINTERVEXTPROC* = PROCEDURE (pname: GLenum; VAR [nil] params: PtrGLvoid);
		FNGLARRAYELEMENTARRAYEXTPROC* = PROCEDURE (mode: GLenum; count: GLsizei; pi: PtrGLvoid);
		FNGLADDSWAPHINTRECTWINPROC* = PROCEDURE (x: GLint; y: GLint; width: GLsizei; height: GLsizei);
		FNGLCOLORTABLEEXTPROC* = PROCEDURE (target: GLenum; internalFormat: GLenum; width: GLsizei; format: GLenum; type: GLenum; data: PtrGLvoid);
		FNGLCOLORSUBTABLEEXTPROC* = PROCEDURE (target: GLenum; start: GLsizei; count: GLsizei; format: GLenum; type: GLenum; data: PtrGLvoid);
		FNGLGETCOLORTABLEEXTPROC* = PROCEDURE (target: GLenum; format: GLenum; type: GLenum; data: PtrGLvoid);
		FNGLGETCOLORTABLEPARAMETERIVEXTPROC* = PROCEDURE (target: GLenum; pname: GLenum; VAR [nil] params: GLint);
		FNGLGETCOLORTABLEPARAMETERFVEXTPROC* = PROCEDURE (target: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glAccum* (op: GLenum; value: GLfloat);

	PROCEDURE glAlphaFunc* (func: GLenum; ref: GLclampf);

	PROCEDURE glAreTexturesResident* (n: GLsizei; VAR [nil] textures: GLuint; residences: PtrSTR): GLboolean;

	PROCEDURE glArrayElement* (i: GLint);

	PROCEDURE glBegin* (mode: GLenum);

	PROCEDURE glBindTexture* (target: GLenum; texture: GLuint);

	PROCEDURE glBitmap* (width: GLsizei; height: GLsizei; xorig: GLfloat; yorig: GLfloat; xmove: GLfloat; ymove: GLfloat; bitmap: PtrSTR);

	PROCEDURE glBlendFunc* (sfactor: GLenum; dfactor: GLenum);

	PROCEDURE glCallList* (list: GLuint);

	PROCEDURE glCallLists* (n: GLsizei; type: GLenum; lists: PtrGLvoid);

	PROCEDURE glClear* (mask: GLbitfield);

	PROCEDURE glClearAccum* (red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat);

	PROCEDURE glClearColor* (red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf);

	PROCEDURE glClearDepth* (depth: GLclampd);

	PROCEDURE glClearIndex* (c: GLfloat);

	PROCEDURE glClearStencil* (s: GLint);

	PROCEDURE glClipPlane* (plane: GLenum; VAR [nil] equation: GLdouble);

	PROCEDURE glColor3b* (red: GLbyte; green: GLbyte; blue: GLbyte);

	PROCEDURE glColor3bv* (VAR [nil] v: GLbyte);

	PROCEDURE glColor3d* (red: GLdouble; green: GLdouble; blue: GLdouble);

	PROCEDURE glColor3dv* (VAR [nil] v: GLdouble);

	PROCEDURE glColor3f* (red: GLfloat; green: GLfloat; blue: GLfloat);

	PROCEDURE glColor3fv* (VAR [nil] v: GLfloat);

	PROCEDURE glColor3i* (red: GLint; green: GLint; blue: GLint);

	PROCEDURE glColor3iv* (VAR [nil] v: GLint);

	PROCEDURE glColor3s* (red: GLshort; green: GLshort; blue: GLshort);

	PROCEDURE glColor3sv* (VAR [nil] v: GLshort);

	PROCEDURE glColor3ub* (red: GLubyte; green: GLubyte; blue: GLubyte);

	PROCEDURE glColor3ubv* (VAR [nil] v: GLubyte);

	PROCEDURE glColor3ui* (red: GLuint; green: GLuint; blue: GLuint);

	PROCEDURE glColor3uiv* (VAR [nil] v: GLuint);

	PROCEDURE glColor3us* (red: GLushort; green: GLushort; blue: GLushort);

	PROCEDURE glColor3usv* (VAR [nil] v: GLushort);

	PROCEDURE glColor4b* (red: GLbyte; green: GLbyte; blue: GLbyte; alpha: GLbyte);

	PROCEDURE glColor4bv* (VAR [nil] v: GLbyte);

	PROCEDURE glColor4d* (red: GLdouble; green: GLdouble; blue: GLdouble; alpha: GLdouble);

	PROCEDURE glColor4dv* (VAR [nil] v: GLdouble);

	PROCEDURE glColor4f* (red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat);

	PROCEDURE glColor4fv* (VAR [nil] v: GLfloat);

	PROCEDURE glColor4i* (red: GLint; green: GLint; blue: GLint; alpha: GLint);

	PROCEDURE glColor4iv* (VAR [nil] v: GLint);

	PROCEDURE glColor4s* (red: GLshort; green: GLshort; blue: GLshort; alpha: GLshort);

	PROCEDURE glColor4sv* (VAR [nil] v: GLshort);

	PROCEDURE glColor4ub* (red: GLubyte; green: GLubyte; blue: GLubyte; alpha: GLubyte);

	PROCEDURE glColor4ubv* (VAR [nil] v: GLubyte);

	PROCEDURE glColor4ui* (red: GLuint; green: GLuint; blue: GLuint; alpha: GLuint);

	PROCEDURE glColor4uiv* (VAR [nil] v: GLuint);

	PROCEDURE glColor4us* (red: GLushort; green: GLushort; blue: GLushort; alpha: GLushort);

	PROCEDURE glColor4usv* (VAR [nil] v: GLushort);

	PROCEDURE glColorMask* (red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean);

	PROCEDURE glColorMaterial* (face: GLenum; mode: GLenum);

	PROCEDURE glColorPointer* (size: GLint; type: GLenum; stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glCopyPixels* (x: GLint; y: GLint; width: GLsizei; height: GLsizei; type: GLenum);

	PROCEDURE glCopyTexImage1D* (target: GLenum; level: GLint; internalFormat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint);

	PROCEDURE glCopyTexImage2D* (target: GLenum; level: GLint; internalFormat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint);

	PROCEDURE glCopyTexSubImage1D* (target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei);

	PROCEDURE glCopyTexSubImage2D* (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei);

	PROCEDURE glCullFace* (mode: GLenum);

	PROCEDURE glDeleteLists* (list: GLuint; range: GLsizei);

	PROCEDURE glDeleteTextures* (n: GLsizei; VAR [nil] textures: GLuint);

	PROCEDURE glDepthFunc* (func: GLenum);

	PROCEDURE glDepthMask* (flag: GLboolean);

	PROCEDURE glDepthRange* (zNear: GLclampd; zFar: GLclampd);

	PROCEDURE glDisable* (cap: GLenum);

	PROCEDURE glDisableClientState* (array: GLenum);

	PROCEDURE glDrawArrays* (mode: GLenum; first: GLint; count: GLsizei);

	PROCEDURE glDrawBuffer* (mode: GLenum);

	PROCEDURE glDrawElements* (mode: GLenum; count: GLsizei; type: GLenum; indices: PtrGLvoid);

	PROCEDURE glDrawPixels* (width: GLsizei; height: GLsizei; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glEdgeFlag* (flag: GLboolean);

	PROCEDURE glEdgeFlagPointer* (stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glEdgeFlagv* (flag: PtrSTR);

	PROCEDURE glEnable* (cap: GLenum);

	PROCEDURE glEnableClientState* (array: GLenum);

	PROCEDURE glEnd* ();

	PROCEDURE glEndList* ();

	PROCEDURE glEvalCoord1d* (u: GLdouble);

	PROCEDURE glEvalCoord1dv* (VAR [nil] u: GLdouble);

	PROCEDURE glEvalCoord1f* (u: GLfloat);

	PROCEDURE glEvalCoord1fv* (VAR [nil] u: GLfloat);

	PROCEDURE glEvalCoord2d* (u: GLdouble; v: GLdouble);

	PROCEDURE glEvalCoord2dv* (VAR [nil] u: GLdouble);

	PROCEDURE glEvalCoord2f* (u: GLfloat; v: GLfloat);

	PROCEDURE glEvalCoord2fv* (VAR [nil] u: GLfloat);

	PROCEDURE glEvalMesh1* (mode: GLenum; i1: GLint; i2: GLint);

	PROCEDURE glEvalMesh2* (mode: GLenum; i1: GLint; i2: GLint; j1: GLint; j2: GLint);

	PROCEDURE glEvalPoint1* (i: GLint);

	PROCEDURE glEvalPoint2* (i: GLint; j: GLint);

	PROCEDURE glFeedbackBuffer* (size: GLsizei; type: GLenum; VAR [nil] buffer: GLfloat);

	PROCEDURE glFinish* ();

	PROCEDURE glFlush* ();

	PROCEDURE glFogf* (pname: GLenum; param: GLfloat);

	PROCEDURE glFogfv* (pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glFogi* (pname: GLenum; param: GLint);

	PROCEDURE glFogiv* (pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glFrontFace* (mode: GLenum);

	PROCEDURE glFrustum* (left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble);

	PROCEDURE glGenLists* (range: GLsizei): GLuint;

	PROCEDURE glGenTextures* (n: GLsizei; VAR [nil] textures: GLuint);

	PROCEDURE glGetBooleanv* (pname: GLenum; params: PtrSTR);

	PROCEDURE glGetClipPlane* (plane: GLenum; VAR [nil] equation: GLdouble);

	PROCEDURE glGetDoublev* (pname: GLenum; VAR [nil] params: GLdouble);

	PROCEDURE glGetError* (): GLenum;

	PROCEDURE glGetFloatv* (pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetIntegerv* (pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glGetLightfv* (light: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetLightiv* (light: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glGetMapdv* (target: GLenum; query: GLenum; VAR [nil] v: GLdouble);

	PROCEDURE glGetMapfv* (target: GLenum; query: GLenum; VAR [nil] v: GLfloat);

	PROCEDURE glGetMapiv* (target: GLenum; query: GLenum; VAR [nil] v: GLint);

	PROCEDURE glGetMaterialfv* (face: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetMaterialiv* (face: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glGetPixelMapfv* (map: GLenum; VAR [nil] values: GLfloat);

	PROCEDURE glGetPixelMapuiv* (map: GLenum; VAR [nil] values: GLuint);

	PROCEDURE glGetPixelMapusv* (map: GLenum; VAR [nil] values: GLushort);

	PROCEDURE glGetPointerv* (pname: GLenum; VAR [nil] params: PtrGLvoid);

	PROCEDURE glGetPolygonStipple* (mask: PtrSTR);

	PROCEDURE glGetString* (name: GLenum): PtrSTR;

	PROCEDURE glGetTexEnvfv* (target: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetTexEnviv* (target: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glGetTexGendv* (coord: GLenum; pname: GLenum; VAR [nil] params: GLdouble);

	PROCEDURE glGetTexGenfv* (coord: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetTexGeniv* (coord: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glGetTexImage* (target: GLenum; level: GLint; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glGetTexLevelParameterfv* (target: GLenum; level: GLint; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetTexLevelParameteriv* (target: GLenum; level: GLint; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glGetTexParameterfv* (target: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glGetTexParameteriv* (target: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glHint* (target: GLenum; mode: GLenum);

	PROCEDURE glIndexMask* (mask: GLuint);

	PROCEDURE glIndexPointer* (type: GLenum; stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glIndexd* (c: GLdouble);

	PROCEDURE glIndexdv* (VAR [nil] c: GLdouble);

	PROCEDURE glIndexf* (c: GLfloat);

	PROCEDURE glIndexfv* (VAR [nil] c: GLfloat);

	PROCEDURE glIndexi* (c: GLint);

	PROCEDURE glIndexiv* (VAR [nil] c: GLint);

	PROCEDURE glIndexs* (c: GLshort);

	PROCEDURE glIndexsv* (VAR [nil] c: GLshort);

	PROCEDURE glIndexub* (c: GLubyte);

	PROCEDURE glIndexubv* (VAR [nil] v: GLubyte);

	PROCEDURE glInitNames* ();

	PROCEDURE glInterleavedArrays* (format: GLenum; stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glIsEnabled* (cap: GLenum): GLboolean;

	PROCEDURE glIsList* (list: GLuint): GLboolean;

	PROCEDURE glIsTexture* (texture: GLuint): GLboolean;

	PROCEDURE glLightModelf* (pname: GLenum; param: GLfloat);

	PROCEDURE glLightModelfv* (pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glLightModeli* (pname: GLenum; param: GLint);

	PROCEDURE glLightModeliv* (pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glLightf* (light: GLenum; pname: GLenum; param: GLfloat);

	PROCEDURE glLightfv* (light: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glLighti* (light: GLenum; pname: GLenum; param: GLint);

	PROCEDURE glLightiv* (light: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glLineStipple* (factor: GLint; pattern: GLushort);

	PROCEDURE glLineWidth* (width: GLfloat);

	PROCEDURE glListBase* (base: GLuint);

	PROCEDURE glLoadIdentity* ();

	PROCEDURE glLoadMatrixd* (VAR [nil] m: GLdouble);

	PROCEDURE glLoadMatrixf* (VAR [nil] m: GLfloat);

	PROCEDURE glLoadName* (name: GLuint);

	PROCEDURE glLogicOp* (opcode: GLenum);

	PROCEDURE glMap1d* (target: GLenum; u1: GLdouble; u2: GLdouble; stride: GLint; order: GLint; VAR [nil] points: GLdouble);

	PROCEDURE glMap1f* (target: GLenum; u1: GLfloat; u2: GLfloat; stride: GLint; order: GLint; VAR [nil] points: GLfloat);

	PROCEDURE glMap2d* (target: GLenum; u1: GLdouble; u2: GLdouble; ustride: GLint; uorder: GLint; v1: GLdouble; v2: GLdouble; vstride: GLint; vorder: GLint; VAR [nil] points: GLdouble);

	PROCEDURE glMap2f* (target: GLenum; u1: GLfloat; u2: GLfloat; ustride: GLint; uorder: GLint; v1: GLfloat; v2: GLfloat; vstride: GLint; vorder: GLint; VAR [nil] points: GLfloat);

	PROCEDURE glMapGrid1d* (un: GLint; u1: GLdouble; u2: GLdouble);

	PROCEDURE glMapGrid1f* (un: GLint; u1: GLfloat; u2: GLfloat);

	PROCEDURE glMapGrid2d* (un: GLint; u1: GLdouble; u2: GLdouble; vn: GLint; v1: GLdouble; v2: GLdouble);

	PROCEDURE glMapGrid2f* (un: GLint; u1: GLfloat; u2: GLfloat; vn: GLint; v1: GLfloat; v2: GLfloat);

	PROCEDURE glMaterialf* (face: GLenum; pname: GLenum; param: GLfloat);

	PROCEDURE glMaterialfv* (face: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glMateriali* (face: GLenum; pname: GLenum; param: GLint);

	PROCEDURE glMaterialiv* (face: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glMatrixMode* (mode: GLenum);

	PROCEDURE glMultMatrixd* (VAR [nil] m: GLdouble);

	PROCEDURE glMultMatrixf* (VAR [nil] m: GLfloat);

	PROCEDURE glNewList* (list: GLuint; mode: GLenum);

	PROCEDURE glNormal3b* (nx: GLbyte; ny: GLbyte; nz: GLbyte);

	PROCEDURE glNormal3bv* (VAR [nil] v: GLbyte);

	PROCEDURE glNormal3d* (nx: GLdouble; ny: GLdouble; nz: GLdouble);

	PROCEDURE glNormal3dv* (VAR [nil] v: GLdouble);

	PROCEDURE glNormal3f* (nx: GLfloat; ny: GLfloat; nz: GLfloat);

	PROCEDURE glNormal3fv* (VAR [nil] v: GLfloat);

	PROCEDURE glNormal3i* (nx: GLint; ny: GLint; nz: GLint);

	PROCEDURE glNormal3iv* (VAR [nil] v: GLint);

	PROCEDURE glNormal3s* (nx: GLshort; ny: GLshort; nz: GLshort);

	PROCEDURE glNormal3sv* (VAR [nil] v: GLshort);

	PROCEDURE glNormalPointer* (type: GLenum; stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glOrtho* (left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble);

	PROCEDURE glPassThrough* (token: GLfloat);

	PROCEDURE glPixelMapfv* (map: GLenum; mapsize: GLsizei; VAR [nil] values: GLfloat);

	PROCEDURE glPixelMapuiv* (map: GLenum; mapsize: GLsizei; VAR [nil] values: GLuint);

	PROCEDURE glPixelMapusv* (map: GLenum; mapsize: GLsizei; VAR [nil] values: GLushort);

	PROCEDURE glPixelStoref* (pname: GLenum; param: GLfloat);

	PROCEDURE glPixelStorei* (pname: GLenum; param: GLint);

	PROCEDURE glPixelTransferf* (pname: GLenum; param: GLfloat);

	PROCEDURE glPixelTransferi* (pname: GLenum; param: GLint);

	PROCEDURE glPixelZoom* (xfactor: GLfloat; yfactor: GLfloat);

	PROCEDURE glPointSize* (size: GLfloat);

	PROCEDURE glPolygonMode* (face: GLenum; mode: GLenum);

	PROCEDURE glPolygonOffset* (factor: GLfloat; units: GLfloat);

	PROCEDURE glPolygonStipple* (mask: PtrSTR);

	PROCEDURE glPopAttrib* ();

	PROCEDURE glPopClientAttrib* ();

	PROCEDURE glPopMatrix* ();

	PROCEDURE glPopName* ();

	PROCEDURE glPrioritizeTextures* (n: GLsizei; VAR [nil] textures: GLuint; VAR [nil] priorities: GLclampf);

	PROCEDURE glPushAttrib* (mask: GLbitfield);

	PROCEDURE glPushClientAttrib* (mask: GLbitfield);

	PROCEDURE glPushMatrix* ();

	PROCEDURE glPushName* (name: GLuint);

	PROCEDURE glRasterPos2d* (x: GLdouble; y: GLdouble);

	PROCEDURE glRasterPos2dv* (VAR [nil] v: GLdouble);

	PROCEDURE glRasterPos2f* (x: GLfloat; y: GLfloat);

	PROCEDURE glRasterPos2fv* (VAR [nil] v: GLfloat);

	PROCEDURE glRasterPos2i* (x: GLint; y: GLint);

	PROCEDURE glRasterPos2iv* (VAR [nil] v: GLint);

	PROCEDURE glRasterPos2s* (x: GLshort; y: GLshort);

	PROCEDURE glRasterPos2sv* (VAR [nil] v: GLshort);

	PROCEDURE glRasterPos3d* (x: GLdouble; y: GLdouble; z: GLdouble);

	PROCEDURE glRasterPos3dv* (VAR [nil] v: GLdouble);

	PROCEDURE glRasterPos3f* (x: GLfloat; y: GLfloat; z: GLfloat);

	PROCEDURE glRasterPos3fv* (VAR [nil] v: GLfloat);

	PROCEDURE glRasterPos3i* (x: GLint; y: GLint; z: GLint);

	PROCEDURE glRasterPos3iv* (VAR [nil] v: GLint);

	PROCEDURE glRasterPos3s* (x: GLshort; y: GLshort; z: GLshort);

	PROCEDURE glRasterPos3sv* (VAR [nil] v: GLshort);

	PROCEDURE glRasterPos4d* (x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble);

	PROCEDURE glRasterPos4dv* (VAR [nil] v: GLdouble);

	PROCEDURE glRasterPos4f* (x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat);

	PROCEDURE glRasterPos4fv* (VAR [nil] v: GLfloat);

	PROCEDURE glRasterPos4i* (x: GLint; y: GLint; z: GLint; w: GLint);

	PROCEDURE glRasterPos4iv* (VAR [nil] v: GLint);

	PROCEDURE glRasterPos4s* (x: GLshort; y: GLshort; z: GLshort; w: GLshort);

	PROCEDURE glRasterPos4sv* (VAR [nil] v: GLshort);

	PROCEDURE glReadBuffer* (mode: GLenum);

	PROCEDURE glReadPixels* (x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glRectd* (x1: GLdouble; y1: GLdouble; x2: GLdouble; y2: GLdouble);

	PROCEDURE glRectdv* (VAR [nil] v1: GLdouble; VAR [nil] v2: GLdouble);

	PROCEDURE glRectf* (x1: GLfloat; y1: GLfloat; x2: GLfloat; y2: GLfloat);

	PROCEDURE glRectfv* (VAR [nil] v1: GLfloat; VAR [nil] v2: GLfloat);

	PROCEDURE glRecti* (x1: GLint; y1: GLint; x2: GLint; y2: GLint);

	PROCEDURE glRectiv* (VAR [nil] v1: GLint; VAR [nil] v2: GLint);

	PROCEDURE glRects* (x1: GLshort; y1: GLshort; x2: GLshort; y2: GLshort);

	PROCEDURE glRectsv* (VAR [nil] v1: GLshort; VAR [nil] v2: GLshort);

	PROCEDURE glRenderMode* (mode: GLenum): GLint;

	PROCEDURE glRotated* (angle: GLdouble; x: GLdouble; y: GLdouble; z: GLdouble);

	PROCEDURE glRotatef* (angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat);

	PROCEDURE glScaled* (x: GLdouble; y: GLdouble; z: GLdouble);

	PROCEDURE glScalef* (x: GLfloat; y: GLfloat; z: GLfloat);

	PROCEDURE glScissor* (x: GLint; y: GLint; width: GLsizei; height: GLsizei);

	PROCEDURE glSelectBuffer* (size: GLsizei; VAR [nil] buffer: GLuint);

	PROCEDURE glShadeModel* (mode: GLenum);

	PROCEDURE glStencilFunc* (func: GLenum; ref: GLint; mask: GLuint);

	PROCEDURE glStencilMask* (mask: GLuint);

	PROCEDURE glStencilOp* (fail: GLenum; zfail: GLenum; zpass: GLenum);

	PROCEDURE glTexCoord1d* (s: GLdouble);

	PROCEDURE glTexCoord1dv* (VAR [nil] v: GLdouble);

	PROCEDURE glTexCoord1f* (s: GLfloat);

	PROCEDURE glTexCoord1fv* (VAR [nil] v: GLfloat);

	PROCEDURE glTexCoord1i* (s: GLint);

	PROCEDURE glTexCoord1iv* (VAR [nil] v: GLint);

	PROCEDURE glTexCoord1s* (s: GLshort);

	PROCEDURE glTexCoord1sv* (VAR [nil] v: GLshort);

	PROCEDURE glTexCoord2d* (s: GLdouble; t: GLdouble);

	PROCEDURE glTexCoord2dv* (VAR [nil] v: GLdouble);

	PROCEDURE glTexCoord2f* (s: GLfloat; t: GLfloat);

	PROCEDURE glTexCoord2fv* (VAR [nil] v: GLfloat);

	PROCEDURE glTexCoord2i* (s: GLint; t: GLint);

	PROCEDURE glTexCoord2iv* (VAR [nil] v: GLint);

	PROCEDURE glTexCoord2s* (s: GLshort; t: GLshort);

	PROCEDURE glTexCoord2sv* (VAR [nil] v: GLshort);

	PROCEDURE glTexCoord3d* (s: GLdouble; t: GLdouble; r: GLdouble);

	PROCEDURE glTexCoord3dv* (VAR [nil] v: GLdouble);

	PROCEDURE glTexCoord3f* (s: GLfloat; t: GLfloat; r: GLfloat);

	PROCEDURE glTexCoord3fv* (VAR [nil] v: GLfloat);

	PROCEDURE glTexCoord3i* (s: GLint; t: GLint; r: GLint);

	PROCEDURE glTexCoord3iv* (VAR [nil] v: GLint);

	PROCEDURE glTexCoord3s* (s: GLshort; t: GLshort; r: GLshort);

	PROCEDURE glTexCoord3sv* (VAR [nil] v: GLshort);

	PROCEDURE glTexCoord4d* (s: GLdouble; t: GLdouble; r: GLdouble; q: GLdouble);

	PROCEDURE glTexCoord4dv* (VAR [nil] v: GLdouble);

	PROCEDURE glTexCoord4f* (s: GLfloat; t: GLfloat; r: GLfloat; q: GLfloat);

	PROCEDURE glTexCoord4fv* (VAR [nil] v: GLfloat);

	PROCEDURE glTexCoord4i* (s: GLint; t: GLint; r: GLint; q: GLint);

	PROCEDURE glTexCoord4iv* (VAR [nil] v: GLint);

	PROCEDURE glTexCoord4s* (s: GLshort; t: GLshort; r: GLshort; q: GLshort);

	PROCEDURE glTexCoord4sv* (VAR [nil] v: GLshort);

	PROCEDURE glTexCoordPointer* (size: GLint; type: GLenum; stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glTexEnvf* (target: GLenum; pname: GLenum; param: GLfloat);

	PROCEDURE glTexEnvfv* (target: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glTexEnvi* (target: GLenum; pname: GLenum; param: GLint);

	PROCEDURE glTexEnviv* (target: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glTexGend* (coord: GLenum; pname: GLenum; param: GLdouble);

	PROCEDURE glTexGendv* (coord: GLenum; pname: GLenum; VAR [nil] params: GLdouble);

	PROCEDURE glTexGenf* (coord: GLenum; pname: GLenum; param: GLfloat);

	PROCEDURE glTexGenfv* (coord: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glTexGeni* (coord: GLenum; pname: GLenum; param: GLint);

	PROCEDURE glTexGeniv* (coord: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glTexImage1D* (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; border: GLint; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glTexImage2D* (target: GLenum; level: GLint; internalformat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glTexParameterf* (target: GLenum; pname: GLenum; param: GLfloat);

	PROCEDURE glTexParameterfv* (target: GLenum; pname: GLenum; VAR [nil] params: GLfloat);

	PROCEDURE glTexParameteri* (target: GLenum; pname: GLenum; param: GLint);

	PROCEDURE glTexParameteriv* (target: GLenum; pname: GLenum; VAR [nil] params: GLint);

	PROCEDURE glTexSubImage1D* (target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glTexSubImage2D* (target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type: GLenum; pixels: PtrGLvoid);

	PROCEDURE glTranslated* (x: GLdouble; y: GLdouble; z: GLdouble);

	PROCEDURE glTranslatef* (x: GLfloat; y: GLfloat; z: GLfloat);

	PROCEDURE glVertex2d* (x: GLdouble; y: GLdouble);

	PROCEDURE glVertex2dv* (VAR [nil] v: GLdouble);

	PROCEDURE glVertex2f* (x: GLfloat; y: GLfloat);

	PROCEDURE glVertex2fv* (VAR [nil] v: GLfloat);

	PROCEDURE glVertex2i* (x: GLint; y: GLint);

	PROCEDURE glVertex2iv* (VAR [nil] v: GLint);

	PROCEDURE glVertex2s* (x: GLshort; y: GLshort);

	PROCEDURE glVertex2sv* (VAR [nil] v: GLshort);

	PROCEDURE glVertex3d* (x: GLdouble; y: GLdouble; z: GLdouble);

	PROCEDURE glVertex3dv* (VAR [nil] v: GLdouble);

	PROCEDURE glVertex3f* (x: GLfloat; y: GLfloat; z: GLfloat);

	PROCEDURE glVertex3fv* (VAR [nil] v: GLfloat);

	PROCEDURE glVertex3i* (x: GLint; y: GLint; z: GLint);

	PROCEDURE glVertex3iv* (VAR [nil] v: GLint);

	PROCEDURE glVertex3s* (x: GLshort; y: GLshort; z: GLshort);

	PROCEDURE glVertex3sv* (VAR [nil] v: GLshort);

	PROCEDURE glVertex4d* (x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble);

	PROCEDURE glVertex4dv* (VAR [nil] v: GLdouble);

	PROCEDURE glVertex4f* (x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat);

	PROCEDURE glVertex4fv* (VAR [nil] v: GLfloat);

	PROCEDURE glVertex4i* (x: GLint; y: GLint; z: GLint; w: GLint);

	PROCEDURE glVertex4iv* (VAR [nil] v: GLint);

	PROCEDURE glVertex4s* (x: GLshort; y: GLshort; z: GLshort; w: GLshort);

	PROCEDURE glVertex4sv* (VAR [nil] v: GLshort);

	PROCEDURE glVertexPointer* (size: GLint; type: GLenum; stride: GLsizei; pointer: PtrGLvoid);

	PROCEDURE glViewport* (x: GLint; y: GLint; width: GLsizei; height: GLsizei);

END WinGL.
