      *================================================================*
      * PROGRAM  : HOLDMNT                                             *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CB12       MAPSET HOLDMNS  MAP HOLDMNM              *
      * PURPOSE  : ACCOUNT HOLD AND LIEN MAINTENANCE                   *
      *            ENTER     - DISPLAY ACCOUNT AND ITS HOLDS (8/PAGE)  *
      *            PF7 / PF8 - PAGE BACK / FORWARD THROUGH THE HOLDS   *
      *            PF5       - PLACE A NEW HOLD                        *
      *            PF6       - RELEASE THE HOLD IN 'RELEASE SEQ'       *
      *            RULES:                                              *
      *            - HOLD TYPES THAT MAY BE PLACED ONLINE: AD, LG, LN, *
      *              FR. CK / CX (REG CC) HOLDS ARE PLACED ONLY BY     *
      *              POSTTRAN (REGCCHLD) AND MAY NOT BE KEYED.         *
      *            - WHO MAY PLACE: AD - ANY ROLE                      *
      *                             FR - SRT, SUP, MGR, OPS            *
      *                             LG, LN - OPS, MGR ONLY             *
      *            - WHO MAY RELEASE: LG, FR - OPS, MGR ONLY           *
      *                               LN     - OPS, MGR ONLY           *
      *                               CK, CX - SUP, MGR, OPS (EARLY    *
      *                                        RELEASE OF REG CC HOLD) *
      *                               AD     - ANY ROLE                *
      *            - ADMINISTRATIVE (AD) HOLD: RELEASE DATE REQUIRED,  *
      *              NO LATER THAN 10 BUSINESS DAYS AFTER TODAY, AND   *
      *              THE AMOUNT MAY NOT EXCEED THE AVAILABLE BALANCE   *
      *            - LG / LN / FR: RELEASE DATE OPTIONAL (BLANK =      *
      *              99999999, UNTIL MANUALLY RELEASED); AMOUNT MAY    *
      *              EXCEED AVAILABLE BUT NOT THE LEDGER BALANCE       *
      *            - LG HOLD REQUIRES A COURT / LEVY REFERENCE         *
      *            - NO NEW HOLDS ON CLOSED, ESCHEATED OR CHARGED-OFF  *
      *              ACCOUNTS                                          *
      *            PLACING A HOLD REDUCES ACCT-AVAIL-BAL AND RAISES    *
      *            ACCT-HOLD-AMT; RELEASING REVERSES IT. LEDGER IS     *
      *            NOT CHANGED, SO NOTHING IS QUEUED FOR POSTING.      *
      * FILES    : ACCTMST (UPDATE), HOLDMST (BROWSE/WRITE/UPDATE),    *
      *            CUSTMST, TELLMST, BANKCTL                           *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-19 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2008-02-11 BHS  LEGAL HOLD RELEASE RESTRICTED TO OPS / MGR     *
      *                 (INC12904 - LEVY RELEASED IN ERROR)            *
      * 2011-07-21 DPS  CK / CX TYPES - NOT KEYABLE, EARLY RELEASE SUP *
      * 2013-05-06 DPS  ADMIN HOLD MAX 10 BUSINESS DAYS (AUD-13-022)   *
      * 2016-02-08 DPS  RESP CHECKING - REMOVED HANDLE CONDITION       *
      * 2019-10-14 AKW  FRAUD HOLD RELEASE OPS / MGR (FRD-POL-7)       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOLDMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                      PIC X(08) VALUE 'HOLDMNT'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB12'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.

      *----------------------------------------------------------------*
      * HOLD POLICY                                                    *
      *----------------------------------------------------------------*
       01  WS-HOLD-POLICY.
           05  WS-ADMIN-MAX-BUS-DAYS       PIC S9(05) VALUE +10.
           05  WS-INDEFINITE               PIC 9(08)  VALUE 99999999.
           05  WS-LINES-PER-PAGE           PIC 9(02)  VALUE 08.

       01  WS-WORK.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-MESSAGE                  PIC X(79).
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-REL-DATE                 PIC 9(08).
           05  WS-MAX-ADMIN-DATE           PIC 9(08).
           05  WS-LAST-SEQ                 PIC 9(04).
           05  WS-REL-SEQ                  PIC 9(04).
           05  WS-LINE                     PIC 9(02).
           05  WS-BROWSE-DONE              PIC X(01).
           05  WS-AUTH-OK                  PIC X(01).
           05  WS-AMT-ED                   PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-HAMT-ED                  PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-DATE-ED                  PIC 9999/99/99.
           05  WS-HOLD-KEY.
               10  WS-HK-ACCT              PIC X(12).
               10  WS-HK-SEQ               PIC 9(04).

       01  WS-HOLD-LINE.
           05  HL-SEQ                      PIC 9(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-TYPE                     PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  HL-AMT                      PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-PLACED                   PIC 9(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-RELEASE                  PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  HL-REASON                   PIC X(30).

       COPY ACCTREC.
       COPY CUSTREC.
       COPY HOLDREC.
       COPY TLRREC.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY HOLDMNS.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-ACCT                     PIC X(12).
           05  CA-FIRST-SEQ                PIC 9(04).
           05  CA-LAST-SEQ-SHOWN           PIC 9(04).
           05  CA-PREV-FIRST               PIC 9(04) OCCURS 10 TIMES.
           05  CA-PAGE                     PIC 9(02).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(62).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           PERFORM 8100-READ-CONTROL
           PERFORM 8300-READ-OPERATOR
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               PERFORM 1000-SEND-INITIAL
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               MOVE 'N' TO WS-ERROR-FLAG
               MOVE SPACES TO WS-MESSAGE
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                       INITIALIZE WS-COMMAREA
                       PERFORM 1000-SEND-INITIAL
                   WHEN DFHENTER
                       PERFORM 2000-RECEIVE
                       IF NOT WS-ERROR
                           MOVE 0 TO CA-FIRST-SEQ CA-PAGE
                           PERFORM 2100-DISPLAY-ACCOUNT
                       END-IF
                       PERFORM 8000-SEND-DATAONLY
                   WHEN DFHPF7
                       PERFORM 2000-RECEIVE
                       IF NOT WS-ERROR
                           IF CA-PAGE > 1
                               SUBTRACT 1 FROM CA-PAGE
                               MOVE CA-PREV-FIRST(CA-PAGE)
                                 TO CA-FIRST-SEQ
                           ELSE
                               MOVE 0 TO CA-FIRST-SEQ CA-PAGE
                               MOVE 'TOP OF HOLD LIST' TO WS-MESSAGE
                           END-IF
                           PERFORM 2100-DISPLAY-ACCOUNT
                       END-IF
                       PERFORM 8000-SEND-DATAONLY
                   WHEN DFHPF8
                       PERFORM 2000-RECEIVE
                       IF NOT WS-ERROR
                           IF CA-PAGE < 10
                               ADD 1 TO CA-PAGE
                               MOVE CA-FIRST-SEQ
                                 TO CA-PREV-FIRST(CA-PAGE)
                           END-IF
                           COMPUTE CA-FIRST-SEQ = CA-LAST-SEQ-SHOWN + 1
                           PERFORM 2100-DISPLAY-ACCOUNT
                       END-IF
                       PERFORM 8000-SEND-DATAONLY
                   WHEN DFHPF5
                       PERFORM 2000-RECEIVE
                       IF NOT WS-ERROR
                           PERFORM 3000-PLACE-HOLD
                       END-IF
                       IF NOT WS-ERROR
                           MOVE 0 TO CA-FIRST-SEQ CA-PAGE
                           PERFORM 2100-DISPLAY-ACCOUNT
                       END-IF
                       PERFORM 8000-SEND-DATAONLY
                   WHEN DFHPF6
                       PERFORM 2000-RECEIVE
                       IF NOT WS-ERROR
                           PERFORM 4000-RELEASE-HOLD
                       END-IF
                       IF NOT WS-ERROR
                           PERFORM 2100-DISPLAY-ACCOUNT
                       END-IF
                       PERFORM 8000-SEND-DATAONLY
                   WHEN OTHER
                       MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                       PERFORM 8000-SEND-DATAONLY
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANID)
                COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-SEND-INITIAL.
           MOVE LOW-VALUES TO HOLDMNMO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           MOVE TLR-ROLE   TO OPROLEO
           MOVE 'ENTER ACCOUNT NUMBER AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('HOLDMNM') MAPSET('HOLDMNS')
                FROM(HOLDMNMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE.
           EXEC CICS RECEIVE MAP('HOLDMNM') MAPSET('HOLDMNS')
                INTO(HOLDMNMI) RESP(WS-RESP) END-EXEC
           IF ACCTNOL = 0 OR ACCTNOI NOT NUMERIC
               MOVE 'ACCOUNT NUMBER MUST BE 12 DIGITS' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF ACCTNOI NOT = CA-ACCT
               MOVE ACCTNOI TO CA-ACCT
               MOVE 0 TO CA-FIRST-SEQ CA-PAGE
           END-IF.

      *----------------------------------------------------------------*
      * ACCOUNT HEADER AND ONE PAGE OF HOLDS FROM CA-FIRST-SEQ         *
      *----------------------------------------------------------------*
       2100-DISPLAY-ACCOUNT.
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(CA-ACCT) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO ACCTNMO
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                RIDFLD(ACCT-PRIMARY-CUST-ID) RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CUST-LAST-NAME TO ACCTNMO
           END-IF
           EVALUATE TRUE
               WHEN ACCT-ACTIVE      MOVE 'ACTIVE'     TO ASTATO
               WHEN ACCT-DORMANT     MOVE 'DORMANT'    TO ASTATO
               WHEN ACCT-FROZEN      MOVE 'FROZEN'     TO ASTATO
               WHEN ACCT-RESTRICTED  MOVE 'RESTRICTED' TO ASTATO
               WHEN ACCT-CLOSED      MOVE 'CLOSED'     TO ASTATO
               WHEN ACCT-CHARGED-OFF MOVE 'CHARGE-OFF' TO ASTATO
               WHEN ACCT-ESCHEATED   MOVE 'ESCHEATED'  TO ASTATO
               WHEN OTHER            MOVE ACCT-STATUS  TO ASTATO
           END-EVALUATE
           MOVE ACCT-LEDGER-BAL TO WS-AMT-ED
           MOVE WS-AMT-ED       TO LEDGERO
           MOVE ACCT-AVAIL-BAL  TO WS-AMT-ED
           MOVE WS-AMT-ED       TO AVAILO
           MOVE ACCT-HOLD-AMT   TO WS-AMT-ED
           MOVE WS-AMT-ED       TO HOLDAMO
           MOVE TLR-ROLE        TO OPROLEO

           MOVE SPACES TO HLINE1O HLINE2O HLINE3O HLINE4O
                          HLINE5O HLINE6O HLINE7O HLINE8O
           MOVE 0 TO WS-LINE
           MOVE CA-FIRST-SEQ TO CA-LAST-SEQ-SHOWN
           MOVE CA-ACCT      TO WS-HK-ACCT
           MOVE CA-FIRST-SEQ TO WS-HK-SEQ
           MOVE 'N' TO WS-BROWSE-DONE
           EXEC CICS STARTBR FILE('HOLDMST') RIDFLD(WS-HOLD-KEY)
                GTEQ RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Y' TO WS-BROWSE-DONE
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE = 'Y'
               EXEC CICS READNEXT FILE('HOLDMST') INTO(HOLD-RECORD)
                    RIDFLD(WS-HOLD-KEY) RESP(WS-RESP) END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR HOLD-ACCT-NUMBER NOT = CA-ACCT
                   MOVE 'Y' TO WS-BROWSE-DONE
               ELSE
                   ADD 1 TO WS-LINE
                   PERFORM 2110-FORMAT-LINE
                   MOVE HOLD-SEQ TO CA-LAST-SEQ-SHOWN
                   IF WS-LINE NOT < WS-LINES-PER-PAGE
                       MOVE 'Y' TO WS-BROWSE-DONE
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('HOLDMST') RESP(WS-RESP) END-EXEC
           IF WS-LINE = 0 AND WS-MESSAGE = SPACES
               MOVE 'NO HOLDS ON THIS ACCOUNT' TO WS-MESSAGE
           END-IF.

       2110-FORMAT-LINE.
           MOVE HOLD-SEQ          TO HL-SEQ
           MOVE HOLD-TYPE         TO HL-TYPE
           MOVE HOLD-AMOUNT       TO HL-AMT
           MOVE HOLD-PLACED-DATE  TO HL-PLACED
           IF HOLD-RELEASE-DATE = WS-INDEFINITE
               MOVE 'INDEFIN.' TO HL-RELEASE
           ELSE
               MOVE HOLD-RELEASE-DATE TO HL-RELEASE
           END-IF
           MOVE HOLD-STATUS       TO HL-STATUS
           MOVE HOLD-REASON       TO HL-REASON
           EVALUATE WS-LINE
               WHEN 1 MOVE WS-HOLD-LINE TO HLINE1O
               WHEN 2 MOVE WS-HOLD-LINE TO HLINE2O
               WHEN 3 MOVE WS-HOLD-LINE TO HLINE3O
               WHEN 4 MOVE WS-HOLD-LINE TO HLINE4O
               WHEN 5 MOVE WS-HOLD-LINE TO HLINE5O
               WHEN 6 MOVE WS-HOLD-LINE TO HLINE6O
               WHEN 7 MOVE WS-HOLD-LINE TO HLINE7O
               WHEN 8 MOVE WS-HOLD-LINE TO HLINE8O
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PF5 - PLACE A HOLD                                             *
      *----------------------------------------------------------------*
       3000-PLACE-HOLD.
           PERFORM 3100-EDIT-NEW-HOLD
           IF WS-ERROR
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(CA-ACCT) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT NOT FOUND OR LOCKED' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           PERFORM 3200-EDIT-AGAINST-ACCOUNT
           IF WS-ERROR
               EXEC CICS UNLOCK FILE('ACCTMST') END-EXEC
               EXIT PARAGRAPH
           END-IF
           PERFORM 3300-FIND-LAST-SEQ
           INITIALIZE HOLD-RECORD
           MOVE CA-ACCT           TO HOLD-ACCT-NUMBER
           COMPUTE HOLD-SEQ = WS-LAST-SEQ + 1
           MOVE NTYPEI            TO HOLD-TYPE
           MOVE WS-AMOUNT         TO HOLD-AMOUNT
           MOVE CTL-BUSINESS-DATE TO HOLD-PLACED-DATE
           MOVE WS-REL-DATE       TO HOLD-RELEASE-DATE
           SET HOLD-ACTIVE TO TRUE
           MOVE NRSNI             TO HOLD-REASON
           MOVE NREFI             TO HOLD-TRAN-REF
           MOVE WS-USERID         TO HOLD-PLACED-BY
           MOVE HOLD-KEY          TO WS-HOLD-KEY
           EXEC CICS WRITE FILE('HOLDMST') FROM(HOLD-RECORD)
                RIDFLD(WS-HOLD-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE 'HOLD FILE WRITE FAILED - NOT PLACED' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           ADD WS-AMOUNT      TO ACCT-HOLD-AMT
           SUBTRACT WS-AMOUNT FROM ACCT-AVAIL-BAL
           MOVE CTL-BUSINESS-DATE TO ACCT-LAST-MAINT-DATE
           MOVE WS-USERID         TO ACCT-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE SPACES TO NTYPEO NAMTO NRELDTO NRSNO NREFO
           MOVE SPACES TO WS-MESSAGE
           STRING 'HOLD ' HOLD-SEQ ' PLACED - TYPE ' HOLD-TYPE
                  DELIMITED BY SIZE INTO WS-MESSAGE.

      *    SCREEN EDITS AND ROLE AUTHORITY FOR THE NEW HOLD
       3100-EDIT-NEW-HOLD.
           EVALUATE TRUE
               WHEN NTYPEI = 'CK' OR NTYPEI = 'CX'
                   MOVE 'CK/CX HOLDS ARE PLACED BY POSTING ONLY'
                     TO WS-MESSAGE
                   MOVE -1 TO NTYPEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN NTYPEI NOT = 'AD' AND NOT = 'LG' AND NOT = 'LN'
                                     AND NOT = 'FR'
                   MOVE 'HOLD TYPE MUST BE AD, LG, LN OR FR'
                     TO WS-MESSAGE
                   MOVE -1 TO NTYPEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN FUNCTION TEST-NUMVAL(NAMTI) NOT = 0
                   MOVE 'HOLD AMOUNT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO NAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN NRSNL = 0
                   MOVE 'HOLD REASON IS REQUIRED' TO WS-MESSAGE
                   MOVE -1 TO NRSNL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN NTYPEI = 'LG' AND NREFL = 0
                   MOVE 'LEGAL HOLD NEEDS COURT ORDER / LEVY REFERENCE'
                     TO WS-MESSAGE
                   MOVE -1 TO NREFL
                   MOVE 'Y' TO WS-ERROR-FLAG
           END-EVALUATE
           IF WS-ERROR
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(NAMTI)
           IF WS-AMOUNT NOT > 0
               MOVE 'HOLD AMOUNT MUST BE GREATER THAN ZERO'
                 TO WS-MESSAGE
               MOVE -1 TO NAMTL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF

      *    WHO MAY PLACE WHICH TYPE
           MOVE 'N' TO WS-AUTH-OK
           EVALUATE TRUE
               WHEN NTYPEI = 'AD'
                   MOVE 'Y' TO WS-AUTH-OK
               WHEN NTYPEI = 'FR'
                   IF TLR-ROLE-SENIOR-TELLER OR TLR-ROLE-SUPERVISOR
                      OR TLR-ROLE-MANAGER OR TLR-ROLE-OPERATIONS
                       MOVE 'Y' TO WS-AUTH-OK
                   END-IF
               WHEN OTHER
                   IF TLR-ROLE-MANAGER OR TLR-ROLE-OPERATIONS
                       MOVE 'Y' TO WS-AUTH-OK
                   END-IF
           END-EVALUATE
           IF WS-AUTH-OK = 'N'
               STRING 'ROLE ' TLR-ROLE ' MAY NOT PLACE A '
                      NTYPEI ' HOLD' DELIMITED BY SIZE
                 INTO WS-MESSAGE
               MOVE -1 TO NTYPEL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF

      *    RELEASE DATE
           IF NRELDTL = 0 OR NRELDTI = SPACES
               IF NTYPEI = 'AD'
                   MOVE 'ADMIN HOLD NEEDS A RELEASE DATE' TO WS-MESSAGE
                   MOVE -1 TO NRELDTL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               MOVE WS-INDEFINITE TO WS-REL-DATE
           ELSE
               IF NRELDTI NOT NUMERIC
                   MOVE 'RELEASE DATE MUST BE CCYYMMDD' TO WS-MESSAGE
                   MOVE -1 TO NRELDTL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               MOVE NRELDTI TO WS-REL-DATE
               INITIALIZE DATE-PARMS
               SET DTU-VALIDATE TO TRUE
               MOVE WS-REL-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DTU-RESULT-YES
                  OR WS-REL-DATE NOT > CTL-BUSINESS-DATE
                   MOVE 'RELEASE DATE INVALID OR NOT IN THE FUTURE'
                     TO WS-MESSAGE
                   MOVE -1 TO NRELDTL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *    ADMIN HOLD - NO MORE THAN 10 BUSINESS DAYS (AUD-13-022)
           IF NTYPEI = 'AD'
               INITIALIZE DATE-PARMS
               SET DTU-ADD-BUS-DAYS TO TRUE
               MOVE CTL-BUSINESS-DATE     TO DTU-DATE-1
               MOVE WS-ADMIN-MAX-BUS-DAYS TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-MAX-ADMIN-DATE
               IF WS-REL-DATE > WS-MAX-ADMIN-DATE
                   MOVE WS-MAX-ADMIN-DATE TO WS-DATE-ED
                   STRING 'ADMIN HOLD MAX 10 BUSINESS DAYS - LATEST '
                          WS-DATE-ED DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   MOVE -1 TO NRELDTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
           END-IF.

      *    EDITS THAT NEED THE ACCOUNT RECORD
       3200-EDIT-AGAINST-ACCOUNT.
           EVALUATE TRUE
               WHEN ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
                   MOVE 'NO NEW HOLDS - ACCOUNT CLOSED/ESCHEATED/C-O'
                     TO WS-MESSAGE
                   MOVE -1 TO ACCTNOL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN NTYPEI = 'AD' AND WS-AMOUNT > ACCT-AVAIL-BAL
                   MOVE 'ADMIN HOLD MAY NOT EXCEED AVAILABLE BALANCE'
                     TO WS-MESSAGE
                   MOVE -1 TO NAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-AMOUNT > ACCT-LEDGER-BAL
                   MOVE 'HOLD MAY NOT EXCEED THE LEDGER BALANCE'
                     TO WS-MESSAGE
                   MOVE -1 TO NAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
           END-EVALUATE.

       3300-FIND-LAST-SEQ.
           MOVE ZERO    TO WS-LAST-SEQ
           MOVE CA-ACCT TO WS-HK-ACCT
           MOVE ZERO    TO WS-HK-SEQ
           MOVE 'N' TO WS-BROWSE-DONE
           EXEC CICS STARTBR FILE('HOLDMST') RIDFLD(WS-HOLD-KEY)
                GTEQ RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Y' TO WS-BROWSE-DONE
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE = 'Y'
               EXEC CICS READNEXT FILE('HOLDMST') INTO(HOLD-RECORD)
                    RIDFLD(WS-HOLD-KEY) RESP(WS-RESP) END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR HOLD-ACCT-NUMBER NOT = CA-ACCT
                   MOVE 'Y' TO WS-BROWSE-DONE
               ELSE
                   MOVE HOLD-SEQ TO WS-LAST-SEQ
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('HOLDMST') RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * PF6 - RELEASE A HOLD                                           *
      *----------------------------------------------------------------*
       4000-RELEASE-HOLD.
           IF RELSEQL = 0 OR RELSEQI NOT NUMERIC
               MOVE 'ENTER THE 4-DIGIT SEQ OF THE HOLD TO RELEASE'
                 TO WS-MESSAGE
               MOVE -1 TO RELSEQL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE RELSEQI TO WS-REL-SEQ
           MOVE CA-ACCT    TO WS-HK-ACCT
           MOVE WS-REL-SEQ TO WS-HK-SEQ
           EXEC CICS READ FILE('HOLDMST') INTO(HOLD-RECORD)
                RIDFLD(WS-HOLD-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'HOLD NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO RELSEQL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF NOT HOLD-ACTIVE
               EXEC CICS UNLOCK FILE('HOLDMST') END-EXEC
               MOVE 'HOLD IS NOT ACTIVE' TO WS-MESSAGE
               MOVE -1 TO RELSEQL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF

      *    WHO MAY RELEASE WHICH TYPE
           MOVE 'N' TO WS-AUTH-OK
           EVALUATE TRUE
               WHEN HOLD-LEGAL OR HOLD-FRAUD OR HOLD-LIEN
                   IF TLR-ROLE-MANAGER OR TLR-ROLE-OPERATIONS
                       MOVE 'Y' TO WS-AUTH-OK
                   END-IF
               WHEN HOLD-CHECK-DEPOSIT OR HOLD-CHECK-EXCEPTION
                   IF TLR-ROLE-SUPERVISOR OR TLR-ROLE-MANAGER
                      OR TLR-ROLE-OPERATIONS
                       MOVE 'Y' TO WS-AUTH-OK
                   END-IF
               WHEN OTHER
                   MOVE 'Y' TO WS-AUTH-OK
           END-EVALUATE
           IF WS-AUTH-OK = 'N'
               EXEC CICS UNLOCK FILE('HOLDMST') END-EXEC
               STRING 'ROLE ' TLR-ROLE ' MAY NOT RELEASE A '
                      HOLD-TYPE ' HOLD - REFER TO OPS / MANAGER'
                      DELIMITED BY SIZE INTO WS-MESSAGE
               MOVE -1 TO RELSEQL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF

           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(CA-ACCT) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS UNLOCK FILE('HOLDMST') END-EXEC
               MOVE 'ACCOUNT LOCKED - TRY AGAIN' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           ADD HOLD-AMOUNT      TO ACCT-AVAIL-BAL
           SUBTRACT HOLD-AMOUNT FROM ACCT-HOLD-AMT
           IF ACCT-HOLD-AMT < 0
               MOVE ZERO TO ACCT-HOLD-AMT
           END-IF
           MOVE CTL-BUSINESS-DATE TO ACCT-LAST-MAINT-DATE
           MOVE WS-USERID         TO ACCT-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                RESP(WS-RESP) END-EXEC
           SET HOLD-RELEASED TO TRUE
           MOVE CTL-BUSINESS-DATE TO HOLD-RELEASE-DATE
           EXEC CICS REWRITE FILE('HOLDMST') FROM(HOLD-RECORD)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE SPACES TO RELSEQO
           MOVE HOLD-AMOUNT TO WS-HAMT-ED
           STRING 'HOLD ' HOLD-SEQ ' RELEASED - ' WS-HAMT-ED
                  ' NOW AVAILABLE' DELIMITED BY SIZE INTO WS-MESSAGE.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           EXEC CICS SEND MAP('HOLDMNM') MAPSET('HOLDMNS')
                FROM(HOLDMNMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-READ-CONTROL.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) RESP(WS-RESP) END-EXEC.

       8300-READ-OPERATOR.
           EXEC CICS READ FILE('TELLMST') INTO(TELLER-RECORD)
                RIDFLD(WS-USERID) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR NOT TLR-ACTIVE
               MOVE 'OPERATOR NOT AUTHORISED FOR CB12' TO WS-MESSAGE
               EXEC CICS SEND TEXT FROM(WS-MESSAGE) ERASE FREEKB
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.
