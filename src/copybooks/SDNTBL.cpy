      *================================================================*
      * COPYBOOK : SDNTBL                                              *
      * PURPOSE  : IN-MEMORY SANCTIONS TABLE PASSED BY REFERENCE TO    *
      *            OFACSCRN (SECOND PARAMETER). THE CALLER LOADS IT    *
      *            ONCE PER RUN FROM DD SDNLIST (SEE SDNREC):          *
      *              1. INITIALIZE SDN-TABLE                           *
      *              2. FOR EACH LIST LINE: ADD 1 TO SDNT-COUNT AND    *
      *                 MOVE ENT-NUM, TYPE, NAME, COUNTRY, PROGRAM TO  *
      *                 SDNT-ENT-NUM ETC. (SDNT-COUNT)                 *
      *              3. LEAVE SDNT-PREPARED-FLAG = 'N'                 *
      *            ON THE FIRST CALL OFACSCRN NORMALISES EVERY ENTRY   *
      *            (SDNT-NORM-NAME, TOKENS, SURNAME, SOUNDEX) AND SETS *
      *            SDNT-PREPARED-FLAG = 'Y'. IF THE TABLE IS RELOADED  *
      *            THE CALLER MUST RESET THE FLAG TO 'N'.              *
      *            ONLINE: LOADED AS CICS DATA PROGRAM 'SDNTBLD'       *
      *            (EXEC CICS LOAD ... SET(ADDRESS OF SDN-TABLE)).     *
      * CAPACITY : 500 ENTRIES (SDNT-MAX-ENTRIES)                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - 200 ENTRIES                         *
      * 2011-01-24 DPS  500 ENTRIES, PRE-NORMALISED NAME FIELDS        *
      *================================================================*
       01  SDN-TABLE.
           05  SDNT-COUNT                  PIC 9(04).
           05  SDNT-MAX-ENTRIES            PIC 9(04) VALUE 500.
           05  SDNT-PREPARED-FLAG          PIC X(01).
               88  SDNT-PREPARED                     VALUE 'Y'.
           05  SDNT-LIST-DATE              PIC 9(08).
           05  SDNT-ENTRY OCCURS 500 TIMES INDEXED BY SDNT-IDX.
      *        --- LOADED BY CALLER ---
               10  SDNT-ENT-NUM            PIC 9(06).
               10  SDNT-TYPE               PIC X(01).
               10  SDNT-NAME               PIC X(60).
               10  SDNT-COUNTRY            PIC X(02).
               10  SDNT-PROGRAM            PIC X(10).
      *        --- BUILT BY OFACSCRN ---
               10  SDNT-NORM-NAME          PIC X(60).
               10  SDNT-LAST-NAME          PIC X(30).
               10  SDNT-FIRST-NAME         PIC X(20).
               10  SDNT-LAST-SOUNDEX       PIC X(04).
               10  SDNT-FIRST-SOUNDEX      PIC X(04).
               10  SDNT-TOKEN-COUNT        PIC 9(02).
               10  SDNT-TOKEN              PIC X(20) OCCURS 8 TIMES.
