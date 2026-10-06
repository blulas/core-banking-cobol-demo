      *================================================================*
      * PROGRAM  : FEEASSES                                            *
      * TYPE     : BATCH - EOD STEP 16 (MONTH-END ONLY)                *
      * PURPOSE  : MONTHLY MAINTENANCE FEE ASSESSMENT FOR RETAIL       *
      *            DEPOSIT ACCOUNTS. FEE AMOUNT FROM FEECALC, WAIVER   *
      *            ELIGIBILITY FROM FEEWAIVE USING THE CYCLE           *
      *            STATISTICS MAINTAINED BY INTACCR AND THE DIRECT     *
      *            DEPOSITS (DACH) RECEIVED SINCE THE LAST STATEMENT.  *
      *            - FROZEN AND OFAC-RESTRICTED ACCOUNTS ARE NOT       *
      *              CHARGED                                           *
      *            - DORMANT ACCOUNTS ARE CHARGED ONLY WHERE THE STATE *
      *              OF THE ACCOUNT PERMITS DORMANCY FEES (TABLE)      *
      *            - A DDAP FEE MAY NOT TAKE THE AVAILABLE BALANCE     *
      *              BELOW ZERO: THE FEE IS REDUCED TO THE AVAILABLE   *
      *              BALANCE (PARTIAL FEE), AND NOT ASSESSED AT ALL    *
      *              WHEN NOTHING IS AVAILABLE. OTHER PRODUCTS ARE     *
      *              CHARGED IN FULL.                                  *
      *            - PAPER STATEMENT FEE FOR NON E-STATEMENT ACCOUNTS  *
      *            - ACCT-FEES-MTD IS RESET FOR EVERY DEPOSIT ACCOUNT  *
      *            CORPORATE ANALYSIS FEES ARE ANLYSFEE'S JOB.         *
      * PARM     : FORCE = RUN ON A NON MONTH-END DATE                 *
      * INPUT    : BANKCTL (GETCTL), PRODMST, CUSTMST, TRANHIST,       *
      *            TRANPOST (DACH SCAN)                                *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : TRANPOST (EXTEND) WFMN / WFOT, FEERPT REPORT        *
      * CALLS    : GETCTL, FEECALC, FEEWAIVE, ABENDRTN                 *
      * TRAN IDS : 50000001 - 54999999 (ANLYSFEE USES 55000000+)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1994-09-01 RJT  WAIVER LOGIC MOVED TO FEEWAIVE                 *
      * 2006-02-13 BHS  DIRECT DEPOSIT TOTAL FROM TRANHIST - CHG0298   *
      * 2009-11-17 DPS  REG E - FEE MAY NOT OVERDRAW DDAP              *
      * 2011-08-08 DPS  DORMANT FEE STATE TABLE (UNCLAIMED PROPERTY)   *
      * 2013-06-24 DPS  NO FEES ON OFAC BLOCKED ACCOUNTS               *
      * 2016-04-11 DPS  PERFORMANCE - SKIP FEEWAIVE CALL FOR DDAP      *
      *                 ACCOUNTS THAT CLEARLY QUALIFY ON MIN BALANCE   *
      *                 (PRB0912)                                      *
      * 2018-01-15 AKW  PAPER STATEMENT FEE - CHG1502                  *
      * 2020-11-16 AKW  INC20417 - INLINE RELATIONSHIP BALANCE         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEEASSES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT OPTIONAL TRANHIST-FILE ASSIGN TO "TRANHIST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT REPORT-FILE ASSIGN TO "FEERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  TRANHIST-FILE.
       COPY TRANREC REPLACING ==TRANSACTION-RECORD== BY ==HIST-RECORD==
                     LEADING ==TRAN-== BY ==HST-==.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-PARM                         PIC X(10).
           88  WS-PARM-FORCE                         VALUE 'FORCE'.
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.

      *--- DORMANT ACCOUNT FEE PERMITTED BY STATE (Y/N) ---------------*
      *    STATES NOT LISTED DEFAULT TO PERMITTED                      *
       01  WS-DORMANT-STATE-VALUES.
           05  FILLER PIC X(03) VALUE 'NYN'.
           05  FILLER PIC X(03) VALUE 'CAN'.
           05  FILLER PIC X(03) VALUE 'NJN'.
           05  FILLER PIC X(03) VALUE 'MAN'.
           05  FILLER PIC X(03) VALUE 'CTN'.
           05  FILLER PIC X(03) VALUE 'ILN'.
           05  FILLER PIC X(03) VALUE 'WAN'.
           05  FILLER PIC X(03) VALUE 'OHY'.
           05  FILLER PIC X(03) VALUE 'TXY'.
           05  FILLER PIC X(03) VALUE 'FLY'.
           05  FILLER PIC X(03) VALUE 'DEY'.
           05  FILLER PIC X(03) VALUE 'NCY'.
           05  FILLER PIC X(03) VALUE 'COY'.
       01  WS-DORMANT-STATE-TABLE REDEFINES WS-DORMANT-STATE-VALUES.
           05  WS-DS-ENTRY OCCURS 13 TIMES INDEXED BY WS-DS-IDX.
               10  WS-DS-STATE             PIC X(02).
               10  WS-DS-FEE-ALLOWED       PIC X(01).
       01  WS-DORMANT-FEE-OK               PIC X(01).

      *    PRB0912 - QUICK MIN-BALANCE QUALIFIER FOR DDAP
       01  WS-DDAP-QUICK-MIN-BAL           PIC 9(07)V99 VALUE 1000.00.

      *--- DIRECT DEPOSIT (DACH) TABLE FROM TRANHIST + TRANPOST -------*
       01  WS-DACH-TABLE.
           05  WS-DACH-COUNT               PIC 9(05) COMP VALUE 0.
           05  WS-DACH-ENTRY OCCURS 5000 TIMES INDEXED BY WS-DA-IDX.
               10  WS-DACH-ACCT            PIC X(12).
               10  WS-DACH-DATE            PIC 9(08).
               10  WS-DACH-AMT             PIC S9(13)V99 COMP-3.

      *--- RELATIONSHIP / LINKED-CHECKING TABLE (INC20417) ------------*
       01  WS-REL-TABLE.
           05  WS-REL-COUNT                PIC 9(04) COMP VALUE 0.
           05  WS-REL-ENTRY OCCURS 2000 TIMES
                                           INDEXED BY WS-RL-IDX
                                                      WS-RL-IDX2.
               10  WS-REL-CUST-ID          PIC X(10).
               10  WS-REL-ID               PIC X(10).
               10  WS-REL-BAL              PIC S9(13)V99 COMP-3.
               10  WS-REL-HAS-DDAP         PIC X(01).
       01  WS-REL-WORK-ID                  PIC X(10).

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-FEE-COUNT                PIC 9(07) VALUE 0.
           05  WS-PARTIAL-COUNT            PIC 9(07) VALUE 0.
           05  WS-WAIVED-COUNT             PIC 9(07) VALUE 0.
           05  WS-NOTASSESSED-COUNT        PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
           05  WS-TRAN-SEQ                 PIC 9(08) VALUE 50000000.

       01  WS-WORK.
           05  WS-FEE-AMT                  PIC S9(07)V99 COMP-3.
           05  WS-FULL-FEE                 PIC S9(07)V99 COMP-3.
           05  WS-DD-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-AVG-BAL                  PIC S9(13)V99 COMP-3.
           05  WS-REL-BALANCE              PIC S9(13)V99 COMP-3.
           05  WS-HAS-DDAP                 PIC X(01).
           05  WS-TOT-FEES                 PIC S9(11)V99 COMP-3
                                           VALUE 0.
           05  WS-POST-AMT                 PIC S9(13)V99 COMP-3.
           05  WS-POST-CODE                PIC X(04).
           05  WS-POST-DESC                PIC X(40).
           05  WS-ACTION                   PIC X(40).

       01  WS-HEADING-1.
           05  FILLER PIC X(09) VALUE 'FEEASSES '.
           05  FILLER PIC X(49)
               VALUE 'MONTHLY MAINTENANCE FEE ASSESSMENT'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  H1-BUS-DATE                 PIC 9(08).
           05  FILLER PIC X(51) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER PIC X(40) VALUE
               'ACCOUNT      PROD ST  MIN BAL CYCLE    A'.
           05  FILLER PIC X(40) VALUE
               'VG BAL CYCLE    DIRECT DEP      FEE  WVR'.
           05  FILLER PIC X(52) VALUE '  ACTION'.
       01  WS-DETAIL-LINE.
           05  DL-ACCT                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-PROD                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-MIN-BAL                  PIC -,---,---,--9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-AVG-BAL                  PIC -,---,---,--9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-DD-AMT                   PIC ---,--9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-FEE                      PIC ZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-WAIVER                   PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-ACTION                   PIC X(40).
           05  FILLER                      PIC X(10) VALUE SPACE.
       01  WS-TOTAL-LINE.
           05  TL-LABEL                    PIC X(30).
           05  TL-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC ---,---,--9.99.
           05  FILLER                      PIC X(76) VALUE SPACES.

       COPY BANKCTL.
       COPY FEEPARM.
       COPY FWVPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND NOT WS-PARM-FORCE
               DISPLAY 'FEEASSES NOT MONTH-END (' CTL-BUSINESS-DATE
                       ') - NO FEES ASSESSED TODAY'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALIZE
           PERFORM 1200-LOAD-DIRECT-DEPOSITS
           PERFORM 1100-BUILD-REL-TABLE
           PERFORM 2000-PROCESS-ACCOUNTS
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT PRODUCT-FILE CUSTOMER-FILE
           IF WS-PROD-STATUS NOT = '00' OR WS-CUST-STATUS NOT = '00'
               MOVE 'PRODMST' TO ABND-FILE-NAME
               MOVE WS-PROD-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO H1-BUS-DATE
           WRITE REPORT-LINE FROM WS-HEADING-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-HEADING-2.

      *----------------------------------------------------------------*
      * INC20417 - RELATIONSHIP BALANCE BY RELATIONSHIP ID, AND WHICH  *
      * CUSTOMERS OWN AN OPEN PERSONAL CHECKING (SVST LINKED WAIVER).  *
      *----------------------------------------------------------------*
       1100-BUILD-REL-TABLE.
           MOVE 'N' TO WS-EOF-FLAG
           MOVE LOW-VALUES TO CUST-ID
           START CUSTOMER-FILE KEY >= CUST-ID
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF WS-REL-COUNT < 2000
                           ADD 1 TO WS-REL-COUNT
                           SET WS-RL-IDX TO WS-REL-COUNT
                           MOVE CUST-ID TO WS-REL-CUST-ID(WS-RL-IDX)
                           MOVE CUST-RELATIONSHIP-ID
                             TO WS-REL-ID(WS-RL-IDX)
                           IF WS-REL-ID(WS-RL-IDX) = SPACES
                               MOVE CUST-ID TO WS-REL-ID(WS-RL-IDX)
                           END-IF
                           MOVE ZERO TO WS-REL-BAL(WS-RL-IDX)
                           MOVE 'N' TO WS-REL-HAS-DDAP(WS-RL-IDX)
                       END-IF
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-EOF-FLAG
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1110-ADD-TO-RELATIONSHIP
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-EOF-FLAG.

       1110-ADD-TO-RELATIONSHIP.
           IF ACCT-CATEGORY NOT = 'SV' AND 'DD' AND 'TD' AND 'MM'
              OR ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
               EXIT PARAGRAPH
           END-IF
           SET WS-RL-IDX TO 1
           SEARCH WS-REL-ENTRY
               AT END CONTINUE
               WHEN WS-RL-IDX > WS-REL-COUNT
                   CONTINUE
               WHEN WS-REL-CUST-ID(WS-RL-IDX) = ACCT-PRIMARY-CUST-ID
                   IF ACCT-PRODUCT-CODE = 'DDAP'
                       MOVE 'Y' TO WS-REL-HAS-DDAP(WS-RL-IDX)
                   END-IF
                   IF ACCT-LEDGER-BAL > ZERO
                       MOVE WS-REL-ID(WS-RL-IDX) TO WS-REL-WORK-ID
                       PERFORM VARYING WS-RL-IDX2 FROM 1 BY 1
                               UNTIL WS-RL-IDX2 > WS-REL-COUNT
                           IF WS-REL-ID(WS-RL-IDX2) = WS-REL-WORK-ID
                               ADD ACCT-LEDGER-BAL
                                 TO WS-REL-BAL(WS-RL-IDX2)
                           END-IF
                       END-PERFORM
                   END-IF
           END-SEARCH.

      *----------------------------------------------------------------*
      * DACH CREDITS FROM HISTORY AND FROM TODAY'S POSTINGS            *
      *----------------------------------------------------------------*
       1200-LOAD-DIRECT-DEPOSITS.
           OPEN INPUT TRANHIST-FILE
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANHIST-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF HST-CODE = 'DACH' AND HST-POSTED
                          AND WS-DACH-COUNT < 5000
                           ADD 1 TO WS-DACH-COUNT
                           MOVE HST-ACCT-NUMBER
                             TO WS-DACH-ACCT(WS-DACH-COUNT)
                           MOVE HST-POST-DATE
                             TO WS-DACH-DATE(WS-DACH-COUNT)
                           MOVE HST-AMOUNT
                             TO WS-DACH-AMT(WS-DACH-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRANHIST-FILE

           OPEN INPUT TRANPOST-FILE
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF TRAN-CODE = 'DACH' AND TRAN-POSTED
                          AND WS-DACH-COUNT < 5000
                           ADD 1 TO WS-DACH-COUNT
                           MOVE TRAN-ACCT-NUMBER
                             TO WS-DACH-ACCT(WS-DACH-COUNT)
                           MOVE TRAN-POST-DATE
                             TO WS-DACH-DATE(WS-DACH-COUNT)
                           MOVE TRAN-AMOUNT
                             TO WS-DACH-AMT(WS-DACH-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRANPOST-FILE
           MOVE 'N' TO WS-EOF-FLAG

           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF.

      *----------------------------------------------------------------*
       2000-PROCESS-ACCOUNTS.
           MOVE 'N' TO WS-EOF-FLAG
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-ASSESS-ACCOUNT
               END-READ
           END-PERFORM.

       2100-ASSESS-ACCOUNT.
           ADD 1 TO WS-READ-COUNT
           IF ACCT-CATEGORY NOT = 'SV' AND 'DD' AND 'MM'
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-ACTION DL-WAIVER
           MOVE ZERO TO WS-FEE-AMT WS-DD-AMT WS-AVG-BAL

      *    FEE AMOUNT FIRST - PRODUCTS WITHOUT A FEE ARE NOT REPORTED
           INITIALIZE FEECALC-PARMS
           SET FEE-MAINTENANCE TO TRUE
           MOVE ACCT-PRODUCT-CODE TO FEE-PRODUCT-CODE
           CALL 'FEECALC' USING FEECALC-PARMS
           MOVE FEE-AMOUNT TO WS-FULL-FEE
           IF FEE-RETURN-CODE = 00
               PERFORM 2200-MAINTENANCE-FEE
               PERFORM 2800-WRITE-DETAIL
           END-IF
           IF ACCT-ACTIVE OR ACCT-DORMANT
               PERFORM 2600-PAPER-STATEMENT-FEE
           END-IF

           MOVE ZERO TO ACCT-FEES-MTD
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'FEEASSES' TO ABND-PROGRAM
               MOVE '2100-ASSESS-ACCOUNT' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF.

      *----------------------------------------------------------------*
       2200-MAINTENANCE-FEE.
           IF ACCT-DAYS-IN-CYCLE > ZERO
               COMPUTE WS-AVG-BAL ROUNDED =
                       ACCT-BAL-ACCUM-CYC / ACCT-DAYS-IN-CYCLE
           ELSE
               MOVE ACCT-LEDGER-BAL TO WS-AVG-BAL
           END-IF
           PERFORM 2300-SUM-DIRECT-DEPOSITS

           EVALUATE TRUE
               WHEN ACCT-FROZEN
                   MOVE 'NOT ASSESSED - FROZEN' TO WS-ACTION
                   ADD 1 TO WS-NOTASSESSED-COUNT
                   EXIT PARAGRAPH
               WHEN ACCT-RESTRICTED
                   MOVE 'NOT ASSESSED - RESTRICTED / BLOCKED'
                     TO WS-ACTION
                   ADD 1 TO WS-NOTASSESSED-COUNT
                   EXIT PARAGRAPH
               WHEN ACCT-DORMANT
                   PERFORM 2250-DORMANT-STATE-CHECK
                   IF WS-DORMANT-FEE-OK = 'N'
                       MOVE 'NOT ASSESSED - STATE BARS DORMANT FEE'
                         TO WS-ACTION
                       ADD 1 TO WS-NOTASSESSED-COUNT
                       EXIT PARAGRAPH
                   END-IF
               WHEN ACCT-ACTIVE
                   CONTINUE
               WHEN OTHER
                   MOVE 'NOT ASSESSED - ACCOUNT STATUS' TO WS-ACTION
                   ADD 1 TO WS-NOTASSESSED-COUNT
                   EXIT PARAGRAPH
           END-EVALUATE

      *    PRB0912 - CLEARLY QUALIFIES ON MINIMUM BALANCE, SKIP CALL
           IF ACCT-PRODUCT-CODE = 'DDAP'
              AND ACCT-MIN-BAL-CYC >= WS-DDAP-QUICK-MIN-BAL
               MOVE 'MB' TO ACCT-FEE-WAIVER-CODE DL-WAIVER
               MOVE 'WAIVED' TO WS-ACTION
               ADD 1 TO WS-WAIVED-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM 2400-CALL-FEEWAIVE
           IF FWV-FEE-WAIVED
               MOVE FWV-WAIVER-CODE TO ACCT-FEE-WAIVER-CODE DL-WAIVER
               MOVE 'WAIVED' TO WS-ACTION
               ADD 1 TO WS-WAIVED-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO ACCT-FEE-WAIVER-CODE
           PERFORM 2500-CHARGE-FEE.

      *----------------------------------------------------------------*
       2250-DORMANT-STATE-CHECK.
           MOVE 'Y' TO WS-DORMANT-FEE-OK
           SET WS-DS-IDX TO 1
           SEARCH WS-DS-ENTRY
               AT END CONTINUE
               WHEN WS-DS-STATE(WS-DS-IDX) = ACCT-ESCHEAT-STATE
                   MOVE WS-DS-FEE-ALLOWED(WS-DS-IDX)
                     TO WS-DORMANT-FEE-OK
           END-SEARCH.

      *----------------------------------------------------------------*
      * DIRECT DEPOSITS POSTED AFTER THE LAST STATEMENT DATE           *
      *----------------------------------------------------------------*
       2300-SUM-DIRECT-DEPOSITS.
           PERFORM VARYING WS-DA-IDX FROM 1 BY 1
                   UNTIL WS-DA-IDX > WS-DACH-COUNT
               IF WS-DACH-ACCT(WS-DA-IDX) = ACCT-NUMBER
                  AND WS-DACH-DATE(WS-DA-IDX) > ACCT-LAST-STMT-DATE
                  AND WS-DACH-DATE(WS-DA-IDX) <= CTL-BUSINESS-DATE
                   ADD WS-DACH-AMT(WS-DA-IDX) TO WS-DD-AMT
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2400-CALL-FEEWAIVE.
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY INITIALIZE CUSTOMER-RECORD
           END-READ
           MOVE CUST-TOTAL-REL-BALANCE TO WS-REL-BALANCE
           MOVE 'N' TO WS-HAS-DDAP
           SET WS-RL-IDX TO 1
           SEARCH WS-REL-ENTRY
               AT END CONTINUE
               WHEN WS-RL-IDX > WS-REL-COUNT
                   CONTINUE
               WHEN WS-REL-CUST-ID(WS-RL-IDX) = ACCT-PRIMARY-CUST-ID
                   MOVE WS-REL-HAS-DDAP(WS-RL-IDX) TO WS-HAS-DDAP
                   IF WS-REL-BALANCE = ZERO
                       MOVE WS-REL-BAL(WS-RL-IDX) TO WS-REL-BALANCE
                   END-IF
           END-SEARCH

           INITIALIZE FEEWAIVE-PARMS
           MOVE ACCT-PRODUCT-CODE   TO FWV-PRODUCT-CODE
           MOVE CTL-BUSINESS-DATE   TO FWV-BUSINESS-DATE
           MOVE ACCT-OPEN-DATE      TO FWV-OPEN-DATE
           MOVE ACCT-LAST-STMT-DATE TO FWV-CYCLE-START-DATE
           MOVE ACCT-MIN-BAL-CYC    TO FWV-MIN-BAL-CYC
           MOVE WS-AVG-BAL          TO FWV-AVG-BAL-CYC
           MOVE ACCT-LEDGER-BAL     TO FWV-CURRENT-BAL
           MOVE WS-DD-AMT           TO FWV-DIRECT-DEP-AMT
           MOVE WS-REL-BALANCE      TO FWV-REL-BALANCE
           MOVE CUST-SEGMENT        TO FWV-CUST-SEGMENT
           MOVE CUST-BIRTH-DATE     TO FWV-CUST-BIRTH-DATE
           IF NOT CUST-INDIVIDUAL
               MOVE ZERO TO FWV-CUST-BIRTH-DATE
           END-IF
           MOVE CUST-EMPLOYEE-FLAG  TO FWV-EMPLOYEE-FLAG
           MOVE WS-HAS-DDAP         TO FWV-HAS-DDAP-FLAG
           CALL 'FEEWAIVE' USING FEEWAIVE-PARMS
           IF FWV-RETURN-CODE = 12
               ADD 1 TO WS-ERROR-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REG E 2009 - A DDAP FEE MAY NOT OVERDRAW THE ACCOUNT           *
      *----------------------------------------------------------------*
       2500-CHARGE-FEE.
           MOVE WS-FULL-FEE TO WS-FEE-AMT
           IF ACCT-PRODUCT-CODE = 'DDAP'
               IF ACCT-AVAIL-BAL NOT > ZERO
                   MOVE ZERO TO WS-FEE-AMT
                   MOVE 'NOT ASSESSED - NO AVAILABLE BALANCE'
                     TO WS-ACTION
                   ADD 1 TO WS-NOTASSESSED-COUNT
                   EXIT PARAGRAPH
               END-IF
               IF ACCT-AVAIL-BAL < WS-FEE-AMT
                   MOVE ACCT-AVAIL-BAL TO WS-FEE-AMT
                   MOVE 'PARTIAL FEE - LIMITED TO AVAILABLE'
                     TO WS-ACTION
                   ADD 1 TO WS-PARTIAL-COUNT
               END-IF
           END-IF
           MOVE WS-FEE-AMT TO WS-POST-AMT
           MOVE FEE-TRAN-CODE TO WS-POST-CODE
           MOVE FEE-DESCRIPTION TO WS-POST-DESC
           PERFORM 8000-WRITE-TRANPOST
           SUBTRACT WS-FEE-AMT FROM ACCT-LEDGER-BAL ACCT-AVAIL-BAL
           ADD WS-FEE-AMT TO ACCT-FEES-YTD WS-TOT-FEES
           ADD 1 TO ACCT-DR-COUNT-CYC
           ADD 1 TO WS-FEE-COUNT
           IF WS-ACTION = SPACES
               MOVE 'FEE CHARGED (WFMN)' TO WS-ACTION
           END-IF.

      *----------------------------------------------------------------*
       2600-PAPER-STATEMENT-FEE.
           IF ACCT-ESTMT-FLAG = 'Y'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE FEECALC-PARMS
           SET FEE-PAPER-STMT TO TRUE
           MOVE ACCT-PRODUCT-CODE TO FEE-PRODUCT-CODE
           MOVE ACCT-ESTMT-FLAG   TO FEE-ESTMT-FLAG
           CALL 'FEECALC' USING FEECALC-PARMS
           IF FEE-RETURN-CODE = 00 AND ACCT-AVAIL-BAL >= FEE-AMOUNT
               MOVE FEE-AMOUNT      TO WS-POST-AMT
               MOVE FEE-TRAN-CODE   TO WS-POST-CODE
               MOVE FEE-DESCRIPTION TO WS-POST-DESC
               PERFORM 8000-WRITE-TRANPOST
               SUBTRACT FEE-AMOUNT FROM ACCT-LEDGER-BAL ACCT-AVAIL-BAL
               ADD FEE-AMOUNT TO ACCT-FEES-YTD WS-TOT-FEES
               MOVE ACCT-NUMBER       TO DL-ACCT
               MOVE 'PAPER STATEMENT FEE (WFOT)' TO WS-ACTION
               MOVE FEE-AMOUNT TO WS-FEE-AMT
               PERFORM 2800-WRITE-DETAIL
           END-IF.

      *----------------------------------------------------------------*
       2800-WRITE-DETAIL.
           MOVE ACCT-NUMBER       TO DL-ACCT
           MOVE ACCT-PRODUCT-CODE TO DL-PROD
           MOVE ACCT-STATUS       TO DL-STATUS
           MOVE ACCT-MIN-BAL-CYC  TO DL-MIN-BAL
           MOVE WS-AVG-BAL        TO DL-AVG-BAL
           MOVE WS-DD-AMT         TO DL-DD-AMT
           MOVE WS-FEE-AMT        TO DL-FEE
           MOVE WS-ACTION         TO DL-ACTION
           WRITE REPORT-LINE FROM WS-DETAIL-LINE.

      *----------------------------------------------------------------*
       8000-WRITE-TRANPOST.
           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-TRAN-SEQ
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE
                                     TRAN-EFFECTIVE-DATE
                                     TRAN-POST-DATE
           MOVE WS-TRAN-SEQ       TO TRAN-ID-SEQ
           MOVE ACCT-NUMBER       TO TRAN-ACCT-NUMBER
           MOVE 231000            TO TRAN-TIME
           MOVE WS-POST-CODE      TO TRAN-CODE
           MOVE 'D'               TO TRAN-DR-CR
           MOVE WS-POST-AMT       TO TRAN-AMOUNT
           MOVE WS-POST-AMT       TO TRAN-FEE-AMOUNT
           MOVE 'BAT'             TO TRAN-CHANNEL
           MOVE WS-POST-DESC      TO TRAN-DESCRIPTION
           SET TRAN-POSTED        TO TRUE
           MOVE 'FEEASSES'        TO TRAN-USER-ID
                                     TRAN-SOURCE-PGM
           MOVE ACCT-COST-CENTER  TO TRAN-COST-CENTER
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'FEEASSES' TO ABND-PROGRAM
               MOVE '8000-WRITE-TRANPOST' TO ABND-PARAGRAPH
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'FEES CHARGED' TO TL-LABEL
           MOVE WS-FEE-COUNT TO TL-COUNT
           MOVE WS-TOT-FEES TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE ZERO TO TL-AMOUNT
           MOVE '  OF WHICH PARTIAL' TO TL-LABEL
           MOVE WS-PARTIAL-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'FEES WAIVED' TO TL-LABEL
           MOVE WS-WAIVED-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'NOT ASSESSED' TO TL-LABEL
           MOVE WS-NOTASSESSED-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           CLOSE ACCOUNT-FILE PRODUCT-FILE CUSTOMER-FILE
                 TRANPOST-FILE REPORT-FILE
           DISPLAY 'FEEASSES READ=' WS-READ-COUNT
                   ' CHARGED=' WS-FEE-COUNT
                   ' WAIVED=' WS-WAIVED-COUNT
                   ' NOTASSESSED=' WS-NOTASSESSED-COUNT
                   ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND-OPEN.
           MOVE 'FEEASSES' TO ABND-PROGRAM
           MOVE '1000-INITIALIZE' TO ABND-PARAGRAPH
           MOVE 'FILE OPEN FAILED' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
