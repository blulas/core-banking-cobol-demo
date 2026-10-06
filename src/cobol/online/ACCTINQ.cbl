      *================================================================*
      * PROGRAM  : ACCTINQ                                             *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL / DB2           *
      * TRANS    : CB04                                                *
      * MAPSET   : ACINQS  MAP ACINQM                                  *
      * PURPOSE  : ACCOUNT INQUIRY. SHOWS BALANCES, HOLDS, INTEREST,   *
      *            OVERDRAFT AND REG D COUNTERS FROM ACCTMST AND THE   *
      *            LAST TRANSACTIONS FROM DB2 CBS.TRAN_HISTORY, 10 PER *
      *            PAGE (PF7 / PF8). THE CURRENT DAY'S ACTIVITY IS NOT *
      *            IN DB2 UNTIL EODCTL END - MEMO TOTALS ARE SHOWN.    *
      *            - AVAILABLE = LEDGER - HOLDS. THE OD LIMIT IS SHOWN *
      *              ONLY WHEN THE CUSTOMER HAS OPTED IN (REG E)       *
      *            - EFFECTIVE RATE IS THE PROMO RATE UNTIL THE PROMO  *
      *              EXPIRY DATE, THEN THE CONTRACT RATE               *
      *            - REG D: 6 CONVENIENT WITHDRAWALS PER CYCLE ON      *
      *              SAVINGS / MMDA (BANK POLICY, SEE REGDCHK)         *
      *            - MAINTENANCE FEE WAIVER PROJECTION VIA FEEWAIVE    *
      * FILES    : ACCTMST, CUSTMST, PRODMST, HOLDMST, BANKCTL         *
      * DB2      : CBS.TRAN_HISTORY (DCLTRNH) - CURSOR TRNHCSR         *
      * CALLS    : FEEWAIVE                                            *
      * PF KEYS  : ENTER INQUIRE, PF7 NEWER, PF8 OLDER, PF3 MENU,      *
      *            PF12 CANCEL                                         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 RJT  ORIGINAL - VSAM HISTORY FILE                   *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-06-09 BHS  HISTORY MOVED TO DB2 CBS.TRAN_HISTORY (CHG0077)*
      * 2009-11-17 DPS  REG E OD OPT-IN DISPLAY                        *
      * 2011-07-21 DPS  ACTIVE HOLD LINE (REG CC)                      *
      * 2017-02-08 AKW  PROMO RATE DISPLAY                             *
      * 2020-05-01 AKW  REG D COUNTER RETAINED AFTER FED CHANGE        *
      * 2021-03-02 AKW  FEE WAIVER PROJECTION (FEEWAIVE) - CHG1904     *
      * 2024-08-12 TKO  NSF 12-MONTH COUNT DISPLAY (INC19920)          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB04'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-MESSAGE                      PIC X(79).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-DATE-OUT                     PIC X(10).

       01  WS-PAGE-SIZE                    PIC 9(02) VALUE 10.
       01  WS-REGD-LIMIT                   PIC 9(03) VALUE 006.
       01  WS-SKIP                         PIC 9(05).
       01  WS-FETCH-CNT                    PIC 9(05).
       01  WS-LINE-CNT                     PIC 9(02).
       01  WS-HOLD-CNT                     PIC 9(03).
       01  WS-FLAGS.
           05  WS-CURSOR-FLAG              PIC X(01).
               88  WS-CURSOR-DONE                    VALUE 'Y'.
           05  WS-HOLD-FLAG                PIC X(01).
               88  WS-HOLD-DONE                      VALUE 'Y'.
           05  WS-MORE-FLAG                PIC X(01).
               88  WS-MORE-ROWS                      VALUE 'Y'.

       01  WS-SQL-ACCT                     PIC X(12).
       01  WS-SQLCODE-EDIT                 PIC -9(04).

       01  WS-WORK.
           05  WS-EFF-RATE                 PIC 9(03)V9(06).
           05  WS-ACCR-CENTS               PIC S9(13)V99 COMP-3.
           05  WS-AVG-BAL                  PIC S9(13)V99 COMP-3.
           05  WS-HOLD-KEY.
               10  WS-HK-ACCT              PIC X(12).
               10  WS-HK-SEQ               PIC 9(04).

       01  WS-EDIT-BAL                     PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-EDIT-RATE                    PIC ZZ9.999999.
       01  WS-EDIT-RATE4                   PIC Z9.9999.
       01  WS-EDIT-ODL                     PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-EDIT-CNT                     PIC ZZ9.
       01  WS-EDIT-DATE.
           05  WS-ED-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-ED-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-ED-DD                    PIC 9(02).
       01  WS-DATE-IN                      PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY                  PIC 9(04).
           05  WS-DI-MM                    PIC 9(02).
           05  WS-DI-DD                    PIC 9(02).

       01  WS-TRAN-LINE.
           05  WS-TL-DATE                  PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-TL-CODE                  PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-TL-DRCR                  PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-TL-AMOUNT                PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-TL-CHANNEL               PIC X(03).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-TL-DESC                  PIC X(36).

       01  WS-HOLD-LINE.
           05  WS-HL-TYPE                  PIC X(02).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-HL-AMOUNT                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(05) VALUE ' REL '.
           05  WS-HL-RELEASE               PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-HL-REASON                PIC X(24).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-HL-MORE                  PIC X(06).

       01  WS-COMMAREA.
           05  CA-ACCT-NUMBER              PIC X(12).
           05  CA-PAGE                     PIC 9(03).
           05  CA-MORE-FLAG                PIC X(01).

       COPY ACCTREC.
       COPY CUSTREC.
       COPY PRODREC.
       COPY HOLDREC.
       COPY BANKCTL.
       COPY FWVPARM.
       COPY ACINQS.
       COPY DFHAID.
       COPY DFHBMSCA.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTRNH END-EXEC.

      *    MOST RECENT FIRST. TRAN_ID BREAKS TIES WITHIN A POST DATE.
           EXEC SQL DECLARE TRNHCSR CURSOR FOR
               SELECT CHAR(POST_DATE, ISO), TRAN_CODE, DR_CR, AMOUNT,
                      CHANNEL, DESCRIPTION, TRAN_ID
                 FROM CBS.TRAN_HISTORY
                WHERE ACCT_NUMBER = :WS-SQL-ACCT
                  AND STATUS = 'S'
                ORDER BY POST_DATE DESC, TRAN_ID DESC
                FOR FETCH ONLY
                WITH UR
           END-EXEC.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(16).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO ACINQMO
               MOVE 'ENTER ACCOUNT NUMBER' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       INITIALIZE WS-COMMAREA
                       MOVE LOW-VALUES TO ACINQMO
                       MOVE 'ENTER ACCOUNT NUMBER' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
                   WHEN DFHENTER
                       PERFORM 1000-RECEIVE-INPUT
                       MOVE 1 TO CA-PAGE
                       PERFORM 2000-INQUIRE
                   WHEN DFHPF8
                       IF CA-MORE-FLAG = 'Y'
                           ADD 1 TO CA-PAGE
                       END-IF
                       PERFORM 2000-INQUIRE
                   WHEN DFHPF7
                       IF CA-PAGE > 1
                           SUBTRACT 1 FROM CA-PAGE
                       END-IF
                       PERFORM 2000-INQUIRE
                   WHEN OTHER
                       MOVE LOW-VALUES TO ACINQMO
                       MOVE 'INVALID KEY' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       1000-RECEIVE-INPUT.
           EXEC CICS RECEIVE MAP('ACINQM') MAPSET('ACINQS')
                     INTO(ACINQMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL) AND ACCTNOL > 0
               MOVE ACCTNOI TO CA-ACCT-NUMBER
           END-IF.

      *----------------------------------------------------------------*
       2000-INQUIRE.
           MOVE LOW-VALUES TO ACINQMO
           IF CA-ACCT-NUMBER = SPACES OR LOW-VALUES
               MOVE 'ACCOUNT NUMBER IS REQUIRED' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           MOVE CA-ACCT-NUMBER TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(ACCT-NUMBER) RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE 'ACCTMST READ ERROR - CONTACT HELP DESK'
                     TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP
                   EXIT PARAGRAPH
           END-EVALUATE
      *    LOAN ACCOUNTS ARE INQUIRED ON CB40 (LNINQ)
           IF ACCT-CATEGORY = 'LN' OR ACCT-CATEGORY = 'LC'
               MOVE 'LOAN ACCOUNT - USE CB40 LOAN INQUIRY' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-MESSAGE
           PERFORM 2100-FORMAT-HEADER
           PERFORM 2200-FORMAT-BALANCES
           PERFORM 2300-FORMAT-INTEREST
           PERFORM 2400-ACTIVE-HOLDS
           PERFORM 3000-TRANSACTION-HISTORY
           IF WS-MESSAGE = SPACES
               PERFORM 2500-FEE-WAIVER-PROJECTION
           END-IF
           PERFORM 8000-SEND-MAP.

      *----------------------------------------------------------------*
       2100-FORMAT-HEADER.
           MOVE ACCT-NUMBER TO ACCTNOO
           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           EXEC CICS READ FILE('PRODMST') INTO(PRODUCT-RECORD)
                     RIDFLD(PROD-CODE) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE PROD-NAME TO PRODNMO
           ELSE
               INITIALIZE PRODUCT-RECORD
               MOVE ACCT-PRODUCT-CODE TO PRODNMO
           END-IF
           EVALUATE TRUE
               WHEN ACCT-ACTIVE        MOVE 'ACTIVE'      TO ASTATO
               WHEN ACCT-DORMANT       MOVE 'DORMANT'     TO ASTATO
               WHEN ACCT-FROZEN        MOVE 'FROZEN'      TO ASTATO
               WHEN ACCT-CLOSED        MOVE 'CLOSED'      TO ASTATO
               WHEN ACCT-PENDING-OPEN  MOVE 'PENDING'     TO ASTATO
               WHEN ACCT-ESCHEATED     MOVE 'ESCHEATED'   TO ASTATO
               WHEN ACCT-CHARGED-OFF   MOVE 'CHARGED OFF' TO ASTATO
               WHEN ACCT-RESTRICTED
                   STRING 'RESTRICT ' ACCT-RESTRICT-REASON
                          DELIMITED BY SIZE INTO ASTATO
           END-EVALUATE
           IF NOT ACCT-ACTIVE
               MOVE DFHRED TO ASTATC
           END-IF
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           PERFORM 2110-CUSTOMER-NAME
           PERFORM 2120-NAME-TO-LINE
           MOVE WS-MESSAGE(1:40) TO OWNERO
           MOVE SPACES TO WS-MESSAGE
           IF ACCT-JOINT-CUST-ID NOT = SPACES
               MOVE ACCT-JOINT-CUST-ID TO CUST-ID
               PERFORM 2110-CUSTOMER-NAME
               PERFORM 2120-NAME-TO-LINE
               MOVE WS-MESSAGE(1:40) TO JOINTO
               MOVE SPACES TO WS-MESSAGE
           ELSE
               EVALUATE TRUE
                   WHEN ACCT-OWN-TRUST
                       MOVE '(TRUST ACCOUNT)' TO JOINTO
                   WHEN ACCT-OWN-CORPORATE
                       MOVE '(CORPORATE ACCOUNT)' TO JOINTO
                   WHEN OTHER
                       MOVE '(NONE)' TO JOINTO
               END-EVALUATE
           END-IF
           MOVE ACCT-OPEN-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO OPENDTO
           MOVE ACCT-LAST-ACTIVITY-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO LASTACTO.

       2110-CUSTOMER-NAME.
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               INITIALIZE CUSTOMER-RECORD
               MOVE '** CUSTOMER NOT ON FILE **' TO CUST-LAST-NAME
           END-IF.

      *    BUILD 'CUSTID  FIRST LAST' INTO WS-MESSAGE (SCRATCH)
       2120-NAME-TO-LINE.
           MOVE SPACES TO WS-MESSAGE
           IF CUST-FIRST-NAME = SPACES
               STRING CUST-ID ' ' FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO WS-MESSAGE
           ELSE
               STRING CUST-ID ' ' FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      * BALANCES. AVAILABLE ON FILE ALREADY EXCLUDES HOLDS. THE OD     *
      * LIMIT IS ONLY USABLE WHEN THE CUSTOMER HAS OPTED IN (REG E).   *
      *----------------------------------------------------------------*
       2200-FORMAT-BALANCES.
           MOVE ACCT-LEDGER-BAL TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL     TO LEDGERO
           IF ACCT-LEDGER-BAL < ZERO
               MOVE DFHRED TO LEDGERC
           END-IF
           MOVE ACCT-AVAIL-BAL  TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL     TO AVAILO
           MOVE ACCT-HOLD-AMT   TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL     TO HOLDAMTO
           MOVE ACCT-OD-OPT-IN  TO ODOPTO
           IF ACCT-OD-OPTED-IN
               MOVE ACCT-OD-LIMIT TO WS-EDIT-ODL
           ELSE
               MOVE ZERO TO WS-EDIT-ODL
           END-IF
           MOVE WS-EDIT-ODL TO ODLIMO
           MOVE ACCT-NSF-COUNT-12M TO WS-EDIT-CNT
           MOVE WS-EDIT-CNT TO NSFCNTO
           IF ACCT-NSF-COUNT-12M >= 3
               MOVE DFHRED TO NSFCNTC
           END-IF
           MOVE ACCT-REGD-WD-COUNT TO WS-EDIT-CNT
           MOVE WS-EDIT-CNT TO REGDWDO
           IF (ACCT-CATEGORY = 'SV' OR ACCT-CATEGORY = 'MM')
              AND ACCT-REGD-WD-COUNT >= WS-REGD-LIMIT
               MOVE DFHRED TO REGDWDC
               MOVE 'REG D LIMIT REACHED - NEXT WITHDRAWAL INCURS FEE'
                 TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      * EFFECTIVE RATE: PROMO RATE WHILE PROMO EXPIRY >= BUSINESS DATE *
      *----------------------------------------------------------------*
       2300-FORMAT-INTEREST.
           COMPUTE WS-ACCR-CENTS ROUNDED = ACCT-ACCRUED-INT
           MOVE WS-ACCR-CENTS TO WS-EDIT-BAL
           MOVE WS-EDIT-BAL TO ACCRINTO
           IF ACCT-PROMO-RATE > ZERO
              AND ACCT-PROMO-EXP-DATE >= CTL-BUSINESS-DATE
               MOVE ACCT-PROMO-RATE TO WS-EFF-RATE
               MOVE ACCT-PROMO-RATE TO WS-EDIT-RATE4
               MOVE ACCT-PROMO-EXP-DATE TO WS-DATE-IN
               PERFORM 8100-EDIT-DATE
               STRING WS-EDIT-RATE4 '% TO ' WS-EDIT-DATE
                      DELIMITED BY SIZE INTO PROMOO
           ELSE
               MOVE ACCT-INT-RATE TO WS-EFF-RATE
               MOVE 'NONE' TO PROMOO
           END-IF
           MOVE WS-EFF-RATE TO WS-EDIT-RATE
           MOVE WS-EDIT-RATE TO RATEO
           IF ACCT-MATURITY-DATE > ZERO
               MOVE ACCT-MATURITY-DATE TO WS-DATE-IN
               PERFORM 8100-EDIT-DATE
               MOVE WS-EDIT-DATE TO MATDTO
               IF ACCT-MATURITY-DATE <= CTL-NEXT-BUS-DATE
                   MOVE DFHYELLO TO MATDTC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * FIRST ACTIVE HOLD ON THE LINE, '+N MORE' IF SEVERAL            *
      *----------------------------------------------------------------*
       2400-ACTIVE-HOLDS.
           MOVE ZERO TO WS-HOLD-CNT
           MOVE ACCT-NUMBER TO WS-HK-ACCT
           MOVE ZERO        TO WS-HK-SEQ
           MOVE 'N' TO WS-HOLD-FLAG
           MOVE SPACES TO WS-HOLD-LINE
           EXEC CICS STARTBR FILE('HOLDMST') RIDFLD(WS-HOLD-KEY)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NONE' TO HOLDLNO
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-HOLD-DONE
               EXEC CICS READNEXT FILE('HOLDMST') INTO(HOLD-RECORD)
                         RIDFLD(WS-HOLD-KEY) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR HOLD-ACCT-NUMBER NOT = ACCT-NUMBER
                   SET WS-HOLD-DONE TO TRUE
               ELSE
                   IF HOLD-ACTIVE
                       ADD 1 TO WS-HOLD-CNT
                       IF WS-HOLD-CNT = 1
                           PERFORM 2410-FORMAT-HOLD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('HOLDMST') END-EXEC
           EVALUATE TRUE
               WHEN WS-HOLD-CNT = 0
                   MOVE 'NONE' TO HOLDLNO
               WHEN WS-HOLD-CNT = 1
                   MOVE WS-HOLD-LINE TO HOLDLNO
               WHEN OTHER
                   SUBTRACT 1 FROM WS-HOLD-CNT
                   MOVE WS-HOLD-CNT TO WS-EDIT-CNT
                   STRING '+' WS-EDIT-CNT DELIMITED BY SIZE
                          INTO WS-HL-MORE
                   MOVE WS-HOLD-LINE TO HOLDLNO
           END-EVALUATE.

       2410-FORMAT-HOLD.
           MOVE HOLD-TYPE   TO WS-HL-TYPE
           MOVE HOLD-AMOUNT TO WS-HL-AMOUNT
           IF HOLD-RELEASE-DATE = 99999999
               MOVE 'INDEFINITE' TO WS-HL-RELEASE
           ELSE
               MOVE HOLD-RELEASE-DATE TO WS-DATE-IN
               PERFORM 8100-EDIT-DATE
               MOVE WS-EDIT-DATE TO WS-HL-RELEASE
           END-IF
           MOVE HOLD-REASON TO WS-HL-REASON
           IF HOLD-LEGAL OR HOLD-FRAUD
               MOVE DFHRED TO HOLDLNC
           END-IF.

      *----------------------------------------------------------------*
      * MAINTENANCE FEE PROJECTION FOR THE CURRENT CYCLE. THE INQUIRY  *
      * DOES NOT HAVE DIRECT DEPOSIT TOTALS OR LINKED-CHECKING DATA -  *
      * THOSE WAIVERS ARE ONLY DETERMINED BY FEEASSES AT MONTH-END.    *
      *----------------------------------------------------------------*
       2500-FEE-WAIVER-PROJECTION.
           IF PROD-MAINT-FEE NOT > ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           PERFORM 2110-CUSTOMER-NAME
           INITIALIZE FEEWAIVE-PARMS
           IF ACCT-DAYS-IN-CYCLE > ZERO
               COMPUTE WS-AVG-BAL ROUNDED =
                       ACCT-BAL-ACCUM-CYC / ACCT-DAYS-IN-CYCLE
           ELSE
               MOVE ACCT-LEDGER-BAL TO WS-AVG-BAL
           END-IF
           MOVE ACCT-PRODUCT-CODE      TO FWV-PRODUCT-CODE
           MOVE CTL-BUSINESS-DATE      TO FWV-BUSINESS-DATE
           MOVE ACCT-OPEN-DATE         TO FWV-OPEN-DATE
           MOVE ACCT-LAST-STMT-DATE    TO FWV-CYCLE-START-DATE
           MOVE ACCT-MIN-BAL-CYC       TO FWV-MIN-BAL-CYC
           MOVE WS-AVG-BAL             TO FWV-AVG-BAL-CYC
           MOVE ACCT-LEDGER-BAL        TO FWV-CURRENT-BAL
           MOVE ZERO                   TO FWV-DIRECT-DEP-AMT
           MOVE CUST-TOTAL-REL-BALANCE TO FWV-REL-BALANCE
           MOVE CUST-SEGMENT           TO FWV-CUST-SEGMENT
           MOVE CUST-BIRTH-DATE        TO FWV-CUST-BIRTH-DATE
           MOVE CUST-EMPLOYEE-FLAG     TO FWV-EMPLOYEE-FLAG
           MOVE 'N'                    TO FWV-HAS-DDAP-FLAG
           CALL 'FEEWAIVE' USING FEEWAIVE-PARMS
           IF FWV-RETURN-CODE = 00
               IF FWV-FEE-WAIVED
                   STRING 'MAINT FEE PROJECTED WAIVED - CODE '
                          FWV-WAIVER-CODE
                          DELIMITED BY SIZE INTO WS-MESSAGE
               ELSE
                   MOVE PROD-MAINT-FEE TO WS-EDIT-ODL
                   STRING 'MAINT FEE OF ' WS-EDIT-ODL
                          ' PROJECTED AT CYCLE END'
                          DELIMITED BY SIZE INTO WS-MESSAGE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * HISTORY PAGE N: SKIP (N-1)*10 ROWS, SHOW 10, PEEK ONE MORE TO  *
      * KNOW WHETHER PF8 HAS ANYTHING TO SHOW                          *
      *----------------------------------------------------------------*
       3000-TRANSACTION-HISTORY.
           MOVE ACCT-NUMBER TO WS-SQL-ACCT
           COMPUTE WS-SKIP = (CA-PAGE - 1) * WS-PAGE-SIZE
           MOVE ZERO TO WS-FETCH-CNT WS-LINE-CNT
           MOVE 'N' TO WS-CURSOR-FLAG WS-MORE-FLAG
           EXEC SQL OPEN TRNHCSR END-EXEC
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-SQLCODE-EDIT
               STRING 'TRANSACTION HISTORY UNAVAILABLE - SQLCODE '
                      WS-SQLCODE-EDIT
                      DELIMITED BY SIZE INTO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-CURSOR-DONE
               EXEC SQL FETCH TRNHCSR
                   INTO :TH-POST-DATE, :TH-TRAN-CODE, :TH-DR-CR,
                        :TH-AMOUNT, :TH-CHANNEL,
                        :TH-DESCRIPTION :TH-DESCRIPTION-IND,
                        :TH-TRAN-ID
               END-EXEC
               EVALUATE SQLCODE
                   WHEN 0
                       ADD 1 TO WS-FETCH-CNT
                       EVALUATE TRUE
                           WHEN WS-FETCH-CNT <= WS-SKIP
                               CONTINUE
                           WHEN WS-LINE-CNT < WS-PAGE-SIZE
                               ADD 1 TO WS-LINE-CNT
                               PERFORM 3100-FORMAT-TRAN-LINE
                           WHEN OTHER
                               SET WS-MORE-ROWS TO TRUE
                               SET WS-CURSOR-DONE TO TRUE
                       END-EVALUATE
                   WHEN 100
                       SET WS-CURSOR-DONE TO TRUE
                   WHEN OTHER
                       MOVE SQLCODE TO WS-SQLCODE-EDIT
                       STRING 'HISTORY FETCH ERROR - SQLCODE '
                              WS-SQLCODE-EDIT
                              DELIMITED BY SIZE INTO WS-MESSAGE
                       SET WS-CURSOR-DONE TO TRUE
               END-EVALUATE
           END-PERFORM
           EXEC SQL CLOSE TRNHCSR END-EXEC
           MOVE WS-MORE-FLAG TO CA-MORE-FLAG
           MOVE CA-PAGE TO PAGENOO
           EVALUATE TRUE
               WHEN WS-LINE-CNT = 0 AND CA-PAGE = 1
                   MOVE 'NO POSTED HISTORY (TODAY POSTS AT EOD)'
                     TO TLN01O
               WHEN WS-LINE-CNT = 0
                   MOVE 'NO OLDER TRANSACTIONS' TO WS-MESSAGE
                   SUBTRACT 1 FROM CA-PAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       3100-FORMAT-TRAN-LINE.
           MOVE TH-POST-DATE  TO WS-TL-DATE
           MOVE TH-TRAN-CODE  TO WS-TL-CODE
           MOVE TH-DR-CR      TO WS-TL-DRCR
           MOVE TH-AMOUNT     TO WS-TL-AMOUNT
           MOVE TH-CHANNEL    TO WS-TL-CHANNEL
           IF TH-DESCRIPTION-IND < 0
               MOVE SPACES TO WS-TL-DESC
           ELSE
               MOVE TH-DESCRIPTION-TEXT(1:TH-DESCRIPTION-LEN)
                 TO WS-TL-DESC
           END-IF
           EVALUATE WS-LINE-CNT
               WHEN 01 MOVE WS-TRAN-LINE TO TLN01O
               WHEN 02 MOVE WS-TRAN-LINE TO TLN02O
               WHEN 03 MOVE WS-TRAN-LINE TO TLN03O
               WHEN 04 MOVE WS-TRAN-LINE TO TLN04O
               WHEN 05 MOVE WS-TRAN-LINE TO TLN05O
               WHEN 06 MOVE WS-TRAN-LINE TO TLN06O
               WHEN 07 MOVE WS-TRAN-LINE TO TLN07O
               WHEN 08 MOVE WS-TRAN-LINE TO TLN08O
               WHEN 09 MOVE WS-TRAN-LINE TO TLN09O
               WHEN 10 MOVE WS-TRAN-LINE TO TLN10O
           END-EVALUATE.

      *----------------------------------------------------------------*
       8000-SEND-MAP.
           MOVE WS-MESSAGE TO MSGO
           MOVE 'ENTER=INQUIRE PF7=NEWER PF8=OLDER PF3=MENU PF12=CANCEL'
             TO PFKEYSO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT) DATESEP('/')
           END-EXEC
           MOVE WS-DATE-OUT TO CURDATEO
           MOVE -1 TO ACCTNOL
           EXEC CICS SEND MAP('ACINQM') MAPSET('ACINQS')
                     FROM(ACINQMO) ERASE CURSOR
           END-EXEC.

       8100-EDIT-DATE.
           IF WS-DATE-IN = ZERO
               MOVE SPACES TO WS-EDIT-DATE
           ELSE
               MOVE WS-DI-CCYY TO WS-ED-CCYY
               MOVE WS-DI-MM   TO WS-ED-MM
               MOVE WS-DI-DD   TO WS-ED-DD
           END-IF.
