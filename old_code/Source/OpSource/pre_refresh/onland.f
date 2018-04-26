      LOGICAL FUNCTION ONLAND(RLAT,RLONG)

!-----------------------------------------------------------------------
!
! PROGRAM       : ONLAND
!
! PURPOSE       : LOGICAL FUNCTION, TRUE IF POSITION ON LAND OR BAD
!
! DESCRIPTION   : A MAP WITH 1-DEGREE RESOLUTION IS USED; A 1-DEGREE
!                 SQUARE IS COUNTED AS SEA IF THERE'S ANY SEA IN IT.
!
! CALLED BY     : SYNOB
!
! PARAMETERS    : (1) LATITUDE (DEGREES, SOUTH NEGATIVE)
!                 (2) LONGITUDE (DEGREES, WEST NEGATIVE)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:50$
! $Source: /home/us0400/mdb/op/lib/source/RCS/onland.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:50    Sheila Needham  
! $
! Revision 2.2  2001/10/03 15:10:26  usmdb
! Put lines IS=NLAT(LAT+81) and IL=NLAT(LAT+82) within a else
! block to prevent NLAT(162) = out of bounds if LAT=80 - S.Cox
!
! Revision 2.1  2001/09/05  09:02:04  09:02:04  usmdb (Generic MetDB account)
! treat -80deg latitude as land, i.e. change check from LAT.LT.-80
! to LAT.LE.-80, otherwise the check IS=NLAT(LAT+80) can be NLAT(0)
! which is illegal - S.Cox
!
! Revision 2.0  2001/07/03  10:43:42  10:43:42  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:32:04  09:32:04  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 13:06:18  uspm
! Initial revision
!
! NOV 95 - CASPIAN SEA INCLUDED, SEVERAL MISTAKES CORRECTED
!
! MADE FROM SDB ONLAND NOV 94 (INTEGER*2, LOGICAL*1 & UNUSED ENTRY
! POINTS REMOVED; REAL INPUT LAT/LONG)
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      DIMENSION NLAT(161),NLONG(1340), NLONGS(424),NLONGN(916)
      CHARACTER*132 HEAD

      EQUIVALENCE (NLONG(1),NLONGS(1))
      EQUIVALENCE (NLONG(425),NLONGN(1))
*
* THE ARRAYS NLAT & NLONG CONSTITUTE A LAND/SEA MAP OF THE GLOBE
* BETWEEN 80S & 80N WITH A 1-DEGREE RESOLUTION.
* NLAT(1) IS FOR (TRUNCATED) LATITUDE -79, I.E. THE LATITUDE BAND
* BETWEEN 80S & 79S.  FURTHER LAT BANDS FOLLOW FROM SOUTH TO NORTH.
* EACH VALUE OF NLAT GIVES THE STARTING POINT IN NLONG OF THE LIST
* OF LAND/SEA CHANGE POINTS ROUND THE BAND, STARTING ON SEA UNLESS
* NLAT IS NEGATIVE.  WHERE TWO CONSECUTIVE VALUES OF NLAT ARE THE
* SAME, THERE IS NO LAND IN THAT LATITUDE BAND.
*
* TO INSERT 2 POINTS IN A LATITUDE BAND (AS DONE FOR THE CASPIAN!)
* WITHOUT RETYPING ALL THE FOLLOWING NUMBERS, COPY THE INITIALISATION
* TO A TEMPORARY PROGRAM, REPEAT THE LINE WHERE POINTS ARE TO BE
* INSERTED, SPLIT THE 10 POINTS BETWEEN THE 2 LINES AT THE INSERTION
* POINT AND ADD THE 2 NEW POINTS, LEAVING 12 ON THE 2 LINES.  THEN
* TIDY UP BY WRITING THE ARRAY OUT WITH A FORMAT OF
*     FORMAT((5X,'&',2X,5(I5,','),2X,5(I5,',')))
* AND REINSERTING THE INITIALISATION IN THE SOURCE.
*  WHEN ADDING THE NUMBER OF POINTS INSERTED TO LATER VALUES OF NLAT
* REMEMBER THAT THE SIGN IS REALLY A FLAG, AND SO:
*       IF (NLAT(I).GE.0) NLAT(I)=NLAT(I)+NADDED
*       IF (NLAT(I).LT.0) NLAT(I)=NLAT(I)-NADDED
*
* CHANGES CAN BE TESTED BY RUNNING DBJCLLIB(MAPONLAN) TO PRINT A MAP
* OF THE AREA CONCERNED.
*
      DATA NLAT/
     &     -1,   -3,   -5,   -7,  -15,    -19,  -25,  -33,   37,   47,
     &     53,   61,   67,   77,   81,     81,   81,   81,   81,   81,
     &     81,   81,   81,   81,   81,     81,   81,   81,   81,   83,
     &     85,   87,   89,   93,   97,    101,  105,  109,  113,  115,
     &    119,  123,  127,  133,  137,    141,  147,  155,  165,  173,
     &    179,  187,  195,  203,  211,    219,  229,  239,  249,  259,
     &    267,  275,  283,  293,  303,    313,  323,  333,  341,  345,
     &    349,  355,  361,  367,  373,    379,  387,  395,  405,  417,
* NORTHERN HEMISPHERE
     &    425,  433,  441,  447,  455,    463, -471, -477, -483, -489,
     &   -499, -513, -521, -529, -539,   -549, -559, -571, -585, -597,
     &   -609, -623, -633, -645, -653,   -663, -671, -681, -695, -703,
     &   -711, -721, -731, -737, -743,    749,  759,  771,  783,  793,
     &   -805, -819, -839, -855, -877,    895, -907, -921, -931,  939,
     &    943, -947, -955,  969,  985,    995, 1003, 1015, 1025, 1039,
     &   1055, 1071, 1085, 1095, 1109,   1127, 1145, 1161, 1185, 1205,
     &   1225, 1243, 1269, 1289, 1297,   1303, 1317, 1323, 1327, 1331,
     &   1341/
*
* EACH VALUE OF NLONG INDICATES A CHANGE FROM 1-DEGREE SQUARES WITH NO
* SEA IN THEM TO SQUARES INCLUDING SEA (ROUND A CERTAIN LATITUDE BAND).
* SO NLONG(1)=160 IN A LATITUDE BAND STARTING ON LAND (NLAT(1)<0) MEANS
* THAT THE FIRST SQUARE TO INCLUDE SEA IS THAT FROM 160E TO 161E.
* (SO VALUES OF NLONG RANGE FROM 0 TO 359.)
* NLONG IS INITIALISED IN 2 DATA STATEMENTS BECAUSE THE NUMBER OF
* CONTINUATIONS IS LIMITED TO 99.
*
      DATA NLONGS/
     &    160,  200,  160,  200,  163,    215,  163,  215,  220,  222,
     &    227,  230,  300,  330,  163,    233,  300,  335,  163,  235,
     &    242,  260,  299,  337,  167,    237,  238,  260,  286,  291,
     &    299,  340,  169,  293,  299,    348,    2,  169,  287,  291,
     &    293,  299,  353,  354,  357,    358,   29,  163,  289,  291,
     &    293,  296,   32,   34,   39,     70,   77,  150,  294,  297,
     &     43,   61,   82,  145,  294,    295,   51,   59,   84,   88,
     &     90,   99,  100,  142,  294,    296,   51,   56,  111,  115,
     &    288,  290,  287,  290,  287,    291,  287,  292,  146,  147,
     &    287,  293,  147,  148,  286,    292,  168,  170,  287,  292,
* 100
     &    169,  171,  288,  293,  171,    172,  287,  294,  172,  173,
     &    288,  294,  288,  294,  286,    294,  296,  297,  175,  176,
     &    287,  297,  175,  177,  287,    297,  141,  144,  145,  147,
     &    287,  302,  140,  149,  288,    302,  140,  150,  288,  302,
     &    117,  118,  139,  150,  288,    301,   19,   25,  116,  120,
     &    138,  151,  289,  306,   19,     27,  116,  124,  135,  137,
     &    138,  151,  289,  307,   19,     29,  116,  128,  132,  152,
     &    289,  307,   18,   30,  116,    153,  289,  308,   18,   31,
     &    115,  140,  140,  153,  289,    310,   17,   32,  115,  140,
     &    140,  153,  289,  310,   16,     32,  115,  140,  140,  153,
* 200
     &    290,  311,   16,   32,  115,    140,  140,  153,  290,  311,
     &     15,   32,  115,  140,  140,    152,  290,  311,   15,   34,
     &     45,   47,  114,  140,  140,    151,  290,  312,   15,   35,
     &     45,   47,  114,  140,  140,    150,  290,  314,   15,   35,
     &     45,   47,  115,  140,  140,    149,  290,  318,   15,   35,
     &     45,   48,  117,  140,  140,    149,  290,  319,   14,   34,
     &     45,   48,  120,  148,  290,    319,   14,   34,   45,   48,
     &    122,  146,  290,  320,   13,     36,   45,   49,  123,  146,
     &    290,  320,   13,   37,   45,     49,  124,  139,  141,  146,
     &    290,  320,   12,   39,   46,     49,  125,  137,  142,  145,
* 300
     &    288,  320,   13,   40,   48,     49,  126,  135,  142,  145,
     &    286,  320,   13,   40,   48,     50,  130,  135,  142,  143,
     &    285,  320,   13,   40,   49,     50,  131,  135,  142,  143,
     &    284,  320,   14,   40,  133,    134,  142,  143,  284,  321,
     &     14,   40,  283,  322,   14,     39,  283,  322,   14,   39,
     &    148,  149,  282,  324,   14,     39,  147,  148,  282,  324,
     &     14,   39,  139,  143,  281,    325,   13,   38,  139,  147,
     &    281,  324,   13,   38,  139,    145,  280,  323,   12,   39,
     &    104,  105,  138,  145,  279,    321,   12,   39,  103,  105,
     &    136,  143,  281,  320,   11,     40,  102,  104,  112,  116,
* 400
     &    137,  139,  281,  315,   10,     41,  101,  103,  111,  116,
     &    132,  134,  280,  311,  312,    313,   10,   42,  101,  103,
     &    110,  117,  280,  310/
*
      DATA NLONGN/
     &                             10,     42,  101,  102,  110,  117,
     &    281,  309,   10,   43,   99,    101,  111,  117,  282,  309,
     &     10,   45,  113,  117,  283,    309,   10,   46,  102,  103,
     &    114,  117,  283,  309,   10,     47,  101,  103,  116,  117,
     &    283,  307,    6,   48,  101,    102,  283,  302,  351,  353,
     &      1,    5,   49,  283,  301,    350,   49,   80,   81,  283,
     &    301,  349,   50,  284,  288,    289,  299,  348,   50,   77,
     &     78,  285,  287,  289,  297,    346,  348,  349,   41,   48,
* 500
     &     50,   77,   79,  105,  106,    275,  276,  286,  288,  289,
     &    290,  346,   40,   76,   79,    104,  108,  275,  276,  345,
     &     42,   76,   79,  103,  109,    274,  276,  344,   41,   44,
     &     45,   75,   80,  102,  109,    273,  276,  344,   40,   43,
     &     46,   75,   80,   99,  109,    269,  276,  344,   39,   43,
     &     50,   74,   80,   98,  108,    268,  271,  344,   39,   43,
     &     52,   74,   81,   98,  107,    262,  264,  266,  271,  344,
     &     38,   43,   54,   74,   82,     95,   96,   98,  106,  121,
     &    122,  260,  271,  344,   37,     42,   56,   73,   83,   95,
     &    105,  258,  264,  269,  271,    344,   37,   42,   57,   73,
* 600
     &     84,   94,  105,  256,  263,    270,  272,  344,   37,   41,
     &     57,   73,   86,   94,  106,    255,  262,  270,  272,  283,
     &    284,  344,   36,   40,   58,     73,   86,   93,  107,  255,
     &    262,  344,   35,   40,   59,     71,   72,   73,   90,   92,
     &    113,  255,  262,  344,   35,     39,   57,   69,  116,  254,
     &    262,  345,   34,   39,   51,     55,   56,   68,  117,  253,
     &    261,  345,   34,   38,   50,     67,  118,  252,  261,  346,
     &     33,   37,   49,   58,  119,    251,  262,  278,  279,  347,
     &     33,   36,   48,   54,   55,     57,  120,  246,  247,  250,
     &    262,  278,  279,  347,   32,     36,   48,   52,  120,  249,
* 700
     &    262,  349,   32,   36,   47,     51,  121,  248,  264,  350,
     &     17,   21,   32,   33,  120,    248,  270,  277,  278,  351,
     &     15,   21,   24,   35,  121,    244,  245,  247,  278,  351,
     &     12,   35,  120,  244,  279,    352,   10,   36,  120,  243,
     &    280,  353,   10,   36,  119,    242,  282,  354,    0,   10,
     &     36,  119,  127,  128,  137,    138,  240,  283,    9,   10,
     &     37,   51,   54,  119,  128,    129,  138,  140,  239,  283,
     &     28,   49,   54,  118,  127,    128,  139,  140,  238,  283,
     &    354,  358,   28,   49,   54,    117,  126,  128,  237,  283,
     &    352,  359,   21,   22,   27,     49,   54,  117,  126,  127,
* 800
     &    237,  284,  351,  359,    0,     20,   22,   30,   38,   41,
     &     49,   55,  120,  123,  127,    236,  285,  352,    0,   14,
     &     15,   20,   27,   33,   35,     42,   48,   55,  121,  122,
     &    129,  236,  272,  274,  276,    280,  286,  352,    2,   12,
     &     13,   20,   27,   42,   47,     53,  130,  236,  272,  274,
     &    276,  281,  289,  352,    3,     11,   12,   18,   27,   41,
     &     47,   52,  131,  133,  134,    142,  144,  236,  272,  274,
     &    276,  279,  280,  284,  289,    359,    8,   10,   12,   16,
     &     28,   39,   47,   52,  135,    237,  272,  275,  276,  280,
     &    283,  284,  291,  359,    0,     12,   15,   29,   38,   47,
* 900
     &     55,  136,  237,  272,  280,    292,   30,   39,   49,   54,
     &    138,  237,  267,  271,  272,    278,  289,  290,  295,  359,
     &     37,   40,  138,  238,  268,    276,  288,  291,  293,  358,
     &    139,  238,  270,  274,  290,    302,  305,  359,    2,  140,
     &    238,  291,    2,  140,  236,    293,    0,    5,  140,  233,
     &    278,  282,  301,  358,    0,      6,  141,  142,  143,  157,
     &    158,  234,  277,  282,  303,    351,  352,  356,   10,   14,
     &     15,  136,  139,  140,  157,    158,  232,  277,  282,  300,
     &    351,  353,  358,  359,   22,    135,  156,  160,  231,  277,
     &    281,  301,  352,  353,   22,    135,  156,  161,  231,  272,
* 1000
     &    283,  298,   13,   14,   22,    137,  156,  162,  229,  270,
     &    284,  298,  355,  356,   13,     16,   25,  138,  158,  163,
     &    228,  267,  284,  297,   12,     16,   25,  140,  160,  162,
     &    203,  204,  226,  265,  283,    290,  294,  296,    7,    9,
     &     12,   18,   30,  142,  162,    163,  200,  206,  222,  224,
     &    225,  265,  283,  290,    6,     17,   23,   24,   30,  155,
     &    164,  165,  195,  207,  210,    211,  218,  265,  283,  289,
     &      6,   17,   22,  156,  164,    172,  195,  266,  283,  287,
     &    312,  313,  314,  316,    8,     17,   22,  163,  165,  175,
     &    197,  267,  311,  317,   11,     18,   24,   36,   38,  178,
* 1100
     &    199,  269,  289,  291,  294,    295,  310,  318,   12,   20,
     &     26,   34,   41,  178,  200,    270,  271,  272,  275,  276,
     &    288,  293,  310,  318,  339,    343,   13,   21,   26,   34,
     &     42,  180,  185,  187,  195,    196,  199,  270,  287,  292,
     &    309,  318,  342,  345,   14,     32,   48,  180,  182,  185,
     &    186,  188,  200,  250,  252,    273,  288,  292,  308,  324,
     &     17,   32,   33,   40,   44,     45,   50,  183,  197,  244,
     &    252,  256,  262,  263,  265,    272,  274,  275,  276,  278,
     &    288,  293,  310,  325,   18,     36,   61,   65,   69,  169,
     &    172,  181,  198,  222,  225,    242,  267,  269,  276,  277,
* 1200
     &    287,  291,  310,  328,   22,     23,   24,   29,   68,  160,
     &    172,  176,  199,  214,  216,    217,  232,  233,  247,  252,
     &    286,  291,  310,  334,   67,     72,   74,  152,  154,  159,
     &    249,  255,  264,  266,  276,    277,  278,  279,  283,  288,
     &    310,  332,   54,   55,   69,     72,   74,   75,   79,  128,
     &    140,  149,  245,  251,  253,    255,  273,  274,  276,  278,
     &    279,  280,  282,  286,  309,    332,  336,  337,   54,   55,
     &     70,   71,   82,  129,  236,    240,  243,  245,  253,  254,
     &    260,  262,  271,  274,  276,    278,  307,  335,   55,   56,
     &     86,  111,  236,  242,  306,    335,   56,   58,   87,  110,
* 1300
     &    305,  337,   59,   60,   99,    113,  138,  140,  251,  253,
     &    260,  262,  269,  270,  305,    339,  102,  106,  292,  297,
     &    302,  337,  277,  278,  294,    339,  275,  281,  295,  338,
     &     97,   99,  268,  271,  278,    283,  290,  291,  298,  340/
!
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/onland.F,v $
     &'//'$ $Date: 30/01/2006 20:23:50$ $Revision: 1$'

*
* WE NEED THE LATITUDE & LONGITUDE IN WHOLE DEGREES FOR USE WITH THE
* ABOVE MAP.  THE FRACTION IS TRUNCATED REGARDLESS OF SIGN, I.E -3.7
* BECOMES -3 (NOT -4, ANOTHER POSSIBLE CONVENTION!).
*
      LAT=RLAT
      LONG=RLONG
*
* GET THE START OF THE LIST OF CHANGE POINTS FROM NLAT.  SWITCH BETWEEN
* LAND AND SEA AT CHANGE POINTS UNTIL THE INPUT LONGITUDE IS REACHED.
* NOTE THAT LAT=80 IS TREATED LIKE LAT=79, EXTENDING THE MAP TO 81N.
* THE TRUNCATION ABOVE GIVES LAT=0 FOR ANY LATITUDE BETWEEN 1S & 1N,
* SO THE REAL LATITUDE IS TESTED TO SET IS TO NLAT(80) OR NLAT(81).
*
      IF (LAT.GT.80 .AND. LAT.LT.90) THEN
        ONLAND=.FALSE.        ! ARCTIC
      ELSE IF (LAT.GE.90 .OR. LAT.LE.-80) THEN                      !2.1
        ONLAND=.TRUE.         ! ANTARCTICA OR LATITUDE IMPOSSIBLE
      ELSE
        IF (RLAT.LT.0.OR.LAT.EQ.80) THEN
          IS=NLAT(LAT+80)
          IL=NLAT(LAT+81)
        ELSE                                                        !2.2
          IS=NLAT(LAT+81)       ! FIRST CHANGE POINT IN NLONG ARRAY
          IL=NLAT(LAT+82)       ! FIRST CHANGE POINT FOR NEXT LATITUDE
        ENDIF
*
        IF (IS.GE.0) THEN
          ONLAND=.FALSE.      ! START ON SEA
        ELSE
          ONLAND=.TRUE.       ! START ON LAND
          IS=-IS              ! MAKE IS POSITIVE NOW SIGN HAS BEEN USED
        ENDIF
*
        IL=IABS(IL)-1         ! LAST CHANGE POINT FOR THIS LATITUDE
        IF (IL.LE.IS) RETURN  ! RETURN IF NO LAND AT THIS LATITUDE
*
        IF (LONG.LT.-179 .OR. LONG.GT.180) THEN
          ONLAND=.TRUE.       ! LONGITUDE IMPOSSIBLE, SO BAD POSITION
          RETURN
        ELSE
          LON=LONG+1
          IF (LONG.EQ.0 .AND. RLONG.LT.0) LON=0
          IF (LONG.LT.0) LON=LONG+360
*
          DO 20 I=IS,IL
           IF (NLONG(I).GE.LON) RETURN
           ONLAND=.NOT.ONLAND ! CHANGE FROM LAND TO SEA OR BACK
   20     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END
