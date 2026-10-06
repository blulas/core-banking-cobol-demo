      *================================================================*
      * PROGRAM  : ANLYSFEE                                            *
      * TYPE     : BATCH - MONTH END (EOD STEP 16)                     *
      * PURPOSE  : COMMERCIAL ACCOUNT ANALYSIS. ACCOUNTS WITH          *
      *            ACCT-ANALYSIS-FLAG = Y ARE GROUPED BY CUSTOMER      *
      *            RELATIONSHIP (CUST-RELATIONSHIP-ID). FOR EACH       *
      *            RELATIONSHIP:                                       *
      *              - AVERAGE COLLECTED BALANCE = SUM OF CYCLE        *
      *                AVERAGE LEDGER (ACCUM / DAYS) LESS HOLDS        *
      *              - SERVICE VOLUMES FROM TRANHIST + TRANPOST FOR    *
      *                THE BUSINESS MONTH, BY TRAN CODE                *
      *                  ACH ITEMS  DACH WACH   WIRES OUT  WWIR        *
      *                  WIRES IN   DWIR        CHECKS PD  WCHK        *
      *                  DEPOSITS   DCSH DCHK                          *
      *              - ACCOUNT COUNTS: MAINTENANCE (NON-VIRTUAL),      *
      *                VIRTUAL, ZBA (CZBA), SWEEP (SWEEP FLAG, NOT ZBA)*
      *              - EARNINGS CREDIT FROM ECRCALC                    *
      *              - CHARGE / OFFSET / CARRY-FORWARD FROM ANLYSCAL   *
      *              - NET CHARGE DEBITED (WFAN) TO THE SETTLEMENT     *
      *                ACCOUNT = LOWEST-NUMBERED COPR ACCOUNT OF THE   *
      *                TOP-LEVEL (NO PARENT) CUSTOMER                  *
      *            ANALYSIS STATEMENT PRINTED PER RELATIONSHIP.        *
      *            PUBLIC FUNDS CARRY-FORWARD: READ FROM ANLYCFI       *
      *            (PRIOR GENERATION), WRITTEN TO ANLYCFO (NEW).       *
      *            MONTH END ONLY UNLESS PARM 'FORCE'.                 *
      *            TRAN-ID-SEQ 55000001 - 59999999 (FEEASSES USES      *
      *            50000001 - 54999999).                               *
      * INPUT    : BANKCTL, ACCTMST, CUSTMST, TRANHIST, TRANPOST,      *
      *            ANLYCFI                                             *
      * OUTPUT   : ACCTMST, TRANPOST (EXTEND), ANLYCFO, ANLYSRPT       *
      * RETURN   : 0 OK, 4 WARNINGS, 16 ABEND                          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-06-01 RJT  ORIGINAL - COMMERCIAL ACCOUNT ANALYSIS         *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED                           *
      * 2001-10-01 BHS  ZBA / SWEEP SERVICE COUNTS                     *
      * 2006-05-22 BHS  CHG0731 PUBLIC FUNDS CARRY-FORWARD FILE        *
      * 2011-04-12 DPS  CHG1544 RELATIONSHIP-LEVEL ANALYSIS            *
      * 2015-10-02 DPS  TRANPOST OUTPUT; VOLUMES FROM TRANHIST FILE    *
      *                 INSTEAD OF DB2 (CBS.TRAN_HISTORY)              *
      * 2019-03-25 AKW  VIRTUAL ACCOUNT COUNT                          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANLYSFEE.

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
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT OPTIONAL HIST-FILE ASSIGN TO "TRANHIST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.
           SELECT OPTIONAL POSTIN-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PIN-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL CFIN-FILE ASSIGN TO "ANLYCFI"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CFI-STATUS.
           SELECT CFOUT-FILE ASSIGN TO "ANLYCFO"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CFO-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ANLYSRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  HIST-FILE.
       COPY TRANREC REPLACING ==TRANSACTION-RECORD== BY ==HIST-RECORD==
                    LEADING ==TRAN-== BY ==HST-==.
       FD  POSTIN-FILE.
       COPY TRANREC REPLACING ==TRANSACTION-RECORD== BY ==PIN-RECORD==
                    LEADING ==TRAN-== BY ==PIN-==.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  CFIN-FILE.
       01  CFIN-LINE                       PIC X(100).
       FD  CFOUT-FILE.
       01  CFOUT-LINE                      PIC X(100).
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-PIN-STATUS               PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-CFI-STATUS               PIC X(02).
           05  WS-CFO-STATUS               PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-PARM                         PIC X(10).
           88  WS-FORCE                              VALUE 'FORCE'.
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.
       01  WS-RUN-RC                       PIC 9(02) VALUE 0.
       01  WS-TRAN-SEQ                     PIC 9(08) VALUE 55000000.
       01  WS-BUS-YYYYMM                   PIC 9(06).
       01  WS-DAYS-IN-MONTH                PIC 9(03).
       01  WS-IX                           PIC 9(03).
       01  WS-RX                           PIC 9(03).
       01  WS-SX                           PIC 9(02).
       01  WS-WORK-YYYYMM                  PIC 9(06).
       01  WS-WORK-ACCT                    PIC X(12).
       01  WS-WORK-CODE                    PIC X(04).
       01  WS-AVG-LEDGER                   PIC S9(13)V99 COMP-3.
       01  WS-REL-KEY                      PIC X(10).
       01  WS-COUNTERS.
           05  WS-ACCTS-ANALYSED           PIC 9(05) VALUE 0.
           05  WS-ITEMS-COUNTED            PIC 9(07) VALUE 0.
           05  WS-FEES-POSTED              PIC 9(05) VALUE 0.
           05  WS-TOTAL-FEES               PIC S9(13)V99 COMP-3
                                           VALUE 0.

      *--- RELATIONSHIPS ---------------------------------------------*
       01  WS-REL-TABLE.
           05  WS-REL-COUNT                PIC 9(03) VALUE 0.
           05  WS-REL OCCURS 50 TIMES.
               10  WS-REL-ID               PIC X(10).
               10  WS-REL-NAME             PIC X(30).
               10  WS-REL-CUST-ID          PIC X(10).
               10  WS-REL-SEGMENT          PIC X(02).
               10  WS-REL-MSB              PIC X(01).
               10  WS-REL-SETTLE-ACCT      PIC X(12).
               10  WS-REL-FIRST-ACCT       PIC X(12).
               10  WS-REL-ACCT-COUNT       PIC 9(03).
               10  WS-REL-AVG-LEDGER       PIC S9(13)V99 COMP-3.
               10  WS-REL-HOLDS            PIC S9(13)V99 COMP-3.
               10  WS-REL-AVG-COLLECTED    PIC S9(13)V99 COMP-3.
               10  WS-REL-VOLUME           PIC 9(07) OCCURS 9 TIMES.
               10  WS-REL-CF-IN            PIC S9(11)V99 COMP-3
                                           OCCURS 3 TIMES.
       01  WS-ACCT-INDEX.
           05  WS-AIX-COUNT                PIC 9(03) VALUE 0.
           05  WS-AIX OCCURS 300 TIMES.
               10  WS-AIX-ACCT             PIC X(12).
               10  WS-AIX-REL              PIC 9(03).

      *--- CARRY-FORWARD FILE ----------------------------------------*
       01  WS-CF-FIELDS.
           05  WS-CF-REL                   PIC X(10).
           05  WS-CF-F                     PIC X(20) OCCURS 3.
       01  WS-CF-OUT-REC.
           05  WS-CO-REL                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE '|'.
           05  WS-CO-CF1                   PIC -(11)9.99.
           05  FILLER                      PIC X(01) VALUE '|'.
           05  WS-CO-CF2                   PIC -(11)9.99.
           05  FILLER                      PIC X(01) VALUE '|'.
           05  WS-CO-CF3                   PIC -(11)9.99.

      *--- STATEMENT -------------------------------------------------*
       01  WS-ST-LINE.
           05  WS-ST-TEXT                  PIC X(132).
       01  WS-ST-HDR.
           05  FILLER PIC X(48) VALUE
               'CBS  ANLYSFEE  - ACCOUNT ANALYSIS STATEMENT     '.
           05  FILLER PIC X(14) VALUE 'PERIOD ENDING '.
           05  WS-SH-DATE                  PIC 9(08).
           05  FILLER PIC X(62) VALUE SPACES.
       01  WS-ST-REL.
           05  FILLER PIC X(14) VALUE 'RELATIONSHIP  '.
           05  WS-SR-ID                    PIC X(10).
           05  FILLER PIC X(02) VALUE SPACES.
           05  WS-SR-NAME                  PIC X(30).
           05  FILLER PIC X(10) VALUE ' SEGMENT  '.
           05  WS-SR-SEG                   PIC X(02).
           05  FILLER PIC X(14) VALUE '  SETTLEMENT  '.
           05  WS-SR-SETTLE                PIC X(12).
           05  FILLER PIC X(10) VALUE '  ACCTS   '.
           05  WS-SR-COUNT                 PIC ZZ9.
       01  WS-ST-AMT.
           05  FILLER PIC X(04) VALUE SPACES.
           05  WS-SA-LABEL                 PIC X(40).
           05  WS-SA-AMOUNT                PIC -(13)9.99.
           05  FILLER PIC X(04) VALUE SPACES.
           05  WS-SA-NOTE                  PIC X(60).
       01  WS-ST-SVC.
           05  FILLER PIC X(04) VALUE SPACES.
           05  WS-SS-CODE                  PIC X(04).
           05  FILLER PIC X(02) VALUE SPACES.
           05  WS-SS-VOLUME                PIC Z(6)9.
           05  FILLER PIC X(04) VALUE ' @  '.
           05  WS-SS-PRICE                 PIC Z(4)9.9999.
           05  FILLER PIC X(04) VALUE ' =  '.
           05  WS-SS-CHARGE                PIC -(9)9.99.
       01  WS-RATE-ED                      PIC ZZ9.999999.

       COPY ECRPARM.
       COPY ANLYPARM.
       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND NOT WS-FORCE
               DISPLAY 'ANLYSFEE NOT MONTH END - NO ACTION'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALISE
           PERFORM 2000-COLLECT-ACCOUNTS
           PERFORM 3000-COUNT-HISTORY
           PERFORM 3500-COUNT-TODAY
           PERFORM 4000-READ-CARRY-FORWARD
           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND WS-POST-STATUS NOT = '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           PERFORM 5000-ANALYSE-RELATIONSHIP
               VARYING WS-RX FROM 1 BY 1 UNTIL WS-RX > WS-REL-COUNT
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           MOVE CTL-BUSINESS-DATE(1:6) TO WS-BUS-YYYYMM
           INITIALIZE DATE-PARMS
           SET DTU-END-OF-MONTH TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-DAYS-IN-MONTH =
                   FUNCTION MOD(DTU-RESULT-DATE, 100)
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE CFOUT-FILE
           MOVE '# RELATIONSHIP|CF1 (LAST MONTH)|CF2|CF3 (OLDEST)'
             TO CFOUT-LINE
           WRITE CFOUT-LINE.

      *----------------------------------------------------------------*
      * PASS 1 - ANALYSED ACCOUNTS -> RELATIONSHIPS                    *
      *----------------------------------------------------------------*
       2000-COLLECT-ACCOUNTS.
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY IS NOT LESS THAN ACCT-NUMBER
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF ACCT-ON-ANALYSIS AND NOT ACCT-CLOSED
                          AND NOT ACCT-ESCHEATED
                           PERFORM 2100-ADD-ACCOUNT
                       END-IF
               END-READ
           END-PERFORM
      *    NO TOP-LEVEL OPERATING ACCOUNT - SETTLE TO FIRST ACCOUNT
           PERFORM VARYING WS-RX FROM 1 BY 1 UNTIL WS-RX > WS-REL-COUNT
               IF WS-REL-SETTLE-ACCT(WS-RX) = SPACES
                   MOVE WS-REL-FIRST-ACCT(WS-RX)
                     TO WS-REL-SETTLE-ACCT(WS-RX)
               END-IF
           END-PERFORM.

       2100-ADD-ACCOUNT.
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'ANLYSFEE CUSTOMER NOT FOUND ' CUST-ID
                           ' ACCT ' ACCT-NUMBER
                   MOVE 4 TO WS-RUN-RC
                   EXIT PARAGRAPH
           END-READ
           IF CUST-RELATIONSHIP-ID NOT = SPACES
               MOVE CUST-RELATIONSHIP-ID TO WS-REL-KEY
           ELSE
               MOVE CUST-ID TO WS-REL-KEY
           END-IF
           PERFORM VARYING WS-RX FROM 1 BY 1
                   UNTIL WS-RX > WS-REL-COUNT
                      OR WS-REL-ID(WS-RX) = WS-REL-KEY
               CONTINUE
           END-PERFORM
           IF WS-RX > WS-REL-COUNT
               IF WS-REL-COUNT >= 50
                   DISPLAY 'ANLYSFEE RELATIONSHIP TABLE FULL'
                   MOVE 4 TO WS-RUN-RC
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-REL-COUNT
               MOVE WS-REL-COUNT TO WS-RX
               INITIALIZE WS-REL(WS-RX)
               MOVE WS-REL-KEY    TO WS-REL-ID(WS-RX)
               MOVE CUST-LAST-NAME TO WS-REL-NAME(WS-RX)
               MOVE CUST-ID       TO WS-REL-CUST-ID(WS-RX)
               MOVE CUST-SEGMENT  TO WS-REL-SEGMENT(WS-RX)
               MOVE CUST-MSB-FLAG TO WS-REL-MSB(WS-RX)
               MOVE ACCT-NUMBER   TO WS-REL-FIRST-ACCT(WS-RX)
           END-IF
      *    TOP-LEVEL CUSTOMER'S FIRST OPERATING ACCOUNT SETTLES FEES
      *    AND GIVES THE STATEMENT ITS NAME AND PRICING SEGMENT
           IF CUST-PARENT-CUST-ID = SPACES
              AND ACCT-PRODUCT-CODE = 'COPR'
              AND WS-REL-SETTLE-ACCT(WS-RX) = SPACES
               MOVE ACCT-NUMBER    TO WS-REL-SETTLE-ACCT(WS-RX)
               MOVE CUST-LAST-NAME TO WS-REL-NAME(WS-RX)
               MOVE CUST-ID        TO WS-REL-CUST-ID(WS-RX)
               MOVE CUST-SEGMENT   TO WS-REL-SEGMENT(WS-RX)
               MOVE CUST-MSB-FLAG  TO WS-REL-MSB(WS-RX)
           END-IF

           ADD 1 TO WS-REL-ACCT-COUNT(WS-RX) WS-ACCTS-ANALYSED
           IF ACCT-DAYS-IN-CYCLE > 0
               COMPUTE WS-AVG-LEDGER ROUNDED =
                       ACCT-BAL-ACCUM-CYC / ACCT-DAYS-IN-CYCLE
           ELSE
               MOVE ACCT-LEDGER-BAL TO WS-AVG-LEDGER
           END-IF
           ADD WS-AVG-LEDGER TO WS-REL-AVG-LEDGER(WS-RX)
           ADD ACCT-HOLD-AMT TO WS-REL-HOLDS(WS-RX)
           COMPUTE WS-REL-AVG-COLLECTED(WS-RX) =
                   WS-REL-AVG-LEDGER(WS-RX) - WS-REL-HOLDS(WS-RX)

      *    ACCOUNT-BASED SERVICES
           IF ACCT-IS-VIRTUAL
               ADD 1 TO WS-REL-VOLUME(WS-RX, 7)
           ELSE
               ADD 1 TO WS-REL-VOLUME(WS-RX, 6)
           END-IF
           IF ACCT-PRODUCT-CODE = 'CZBA'
               ADD 1 TO WS-REL-VOLUME(WS-RX, 8)
           ELSE
               IF ACCT-HAS-SWEEP
                   ADD 1 TO WS-REL-VOLUME(WS-RX, 9)
               END-IF
           END-IF

           IF WS-AIX-COUNT < 300
               ADD 1 TO WS-AIX-COUNT
               MOVE ACCT-NUMBER TO WS-AIX-ACCT(WS-AIX-COUNT)
               MOVE WS-RX       TO WS-AIX-REL(WS-AIX-COUNT)
           END-IF.

      *----------------------------------------------------------------*
      * PASS 2 - ITEM VOLUMES FOR THE MONTH                            *
      *----------------------------------------------------------------*
       3000-COUNT-HISTORY.
           OPEN INPUT HIST-FILE
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-HIST-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ HIST-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF HST-POSTED
                           MOVE HST-POST-DATE(1:6) TO WS-WORK-YYYYMM
                           MOVE HST-ACCT-NUMBER    TO WS-WORK-ACCT
                           MOVE HST-CODE           TO WS-WORK-CODE
                           PERFORM 3900-COUNT-ITEM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE HIST-FILE.

       3500-COUNT-TODAY.
           OPEN INPUT POSTIN-FILE
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-PIN-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ POSTIN-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF PIN-POSTED
                           MOVE PIN-POST-DATE(1:6) TO WS-WORK-YYYYMM
                           MOVE PIN-ACCT-NUMBER    TO WS-WORK-ACCT
                           MOVE PIN-CODE           TO WS-WORK-CODE
                           PERFORM 3900-COUNT-ITEM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE POSTIN-FILE.

       3900-COUNT-ITEM.
           IF WS-WORK-YYYYMM NOT = WS-BUS-YYYYMM
               EXIT PARAGRAPH
           END-IF
           EVALUATE WS-WORK-CODE
               WHEN 'DACH'
               WHEN 'WACH'
                   MOVE 1 TO WS-SX
               WHEN 'WWIR'
                   MOVE 2 TO WS-SX
               WHEN 'DWIR'
                   MOVE 3 TO WS-SX
               WHEN 'WCHK'
                   MOVE 4 TO WS-SX
               WHEN 'DCSH'
               WHEN 'DCHK'
                   MOVE 5 TO WS-SX
               WHEN OTHER
                   EXIT PARAGRAPH
           END-EVALUATE
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-AIX-COUNT
               IF WS-AIX-ACCT(WS-IX) = WS-WORK-ACCT
                   ADD 1 TO WS-REL-VOLUME(WS-AIX-REL(WS-IX), WS-SX)
                   ADD 1 TO WS-ITEMS-COUNTED
                   MOVE WS-AIX-COUNT TO WS-IX
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * PRIOR GENERATION OF PUBLIC FUNDS CARRY-FORWARD                 *
      *----------------------------------------------------------------*
       4000-READ-CARRY-FORWARD.
           OPEN INPUT CFIN-FILE
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-CFI-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ CFIN-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF CFIN-LINE(1:1) NOT = '#'
                          AND CFIN-LINE NOT = SPACES
                           PERFORM 4100-APPLY-CF-LINE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CFIN-FILE.

       4100-APPLY-CF-LINE.
           MOVE SPACES TO WS-CF-FIELDS
           UNSTRING CFIN-LINE DELIMITED BY '|'
               INTO WS-CF-REL WS-CF-F(1) WS-CF-F(2) WS-CF-F(3)
           END-UNSTRING
           PERFORM VARYING WS-RX FROM 1 BY 1
                   UNTIL WS-RX > WS-REL-COUNT
               IF WS-REL-ID(WS-RX) = WS-CF-REL
                   COMPUTE WS-REL-CF-IN(WS-RX, 1) =
                           FUNCTION NUMVAL(WS-CF-F(1))
                   COMPUTE WS-REL-CF-IN(WS-RX, 2) =
                           FUNCTION NUMVAL(WS-CF-F(2))
                   COMPUTE WS-REL-CF-IN(WS-RX, 3) =
                           FUNCTION NUMVAL(WS-CF-F(3))
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * ANALYSIS PER RELATIONSHIP                                      *
      *----------------------------------------------------------------*
       5000-ANALYSE-RELATIONSHIP.
           INITIALIZE ECR-CALC-PARMS
           MOVE WS-REL-AVG-COLLECTED(WS-RX) TO ECR-AVG-COLLECTED-BAL
           MOVE CTL-ECR-BASE-RATE           TO ECR-BASE-RATE
           MOVE CTL-FED-FUNDS-RATE          TO ECR-FED-FUNDS-RATE
           MOVE WS-DAYS-IN-MONTH            TO ECR-DAYS-IN-PERIOD
           MOVE WS-REL-MSB(WS-RX)           TO ECR-MSB-FLAG
           CALL 'ECRCALC' USING ECR-CALC-PARMS
           IF ECR-RETURN-CODE > 4
               DISPLAY 'ANLYSFEE ECRCALC RC ' ECR-RETURN-CODE
                       ' REL ' WS-REL-ID(WS-RX)
               MOVE 4 TO WS-RUN-RC
               EXIT PARAGRAPH
           END-IF

           INITIALIZE ANALYSIS-CALC-PARMS
           MOVE WS-REL-SEGMENT(WS-RX)  TO ANL-SEGMENT
           MOVE WS-REL-MSB(WS-RX)      TO ANL-MSB-FLAG
           MOVE ECR-EARNINGS-CREDIT    TO ANL-EARNINGS-CREDIT
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 9
               MOVE WS-REL-VOLUME(WS-RX, WS-SX)
                 TO ANL-SVC-VOLUME(WS-SX)
           END-PERFORM
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 3
               MOVE WS-REL-CF-IN(WS-RX, WS-SX) TO ANL-CF-IN(WS-SX)
           END-PERFORM
           CALL 'ANLYSCAL' USING ANALYSIS-CALC-PARMS

           IF ANL-NET-CHARGE > 0
               PERFORM 6000-POST-FEE
           END-IF
           IF ANL-SEGMENT = 'IN'
               MOVE WS-REL-ID(WS-RX) TO WS-CO-REL
               MOVE ANL-CF-OUT(1)    TO WS-CO-CF1
               MOVE ANL-CF-OUT(2)    TO WS-CO-CF2
               MOVE ANL-CF-OUT(3)    TO WS-CO-CF3
               MOVE WS-CF-OUT-REC    TO CFOUT-LINE
               WRITE CFOUT-LINE
           END-IF
           PERFORM 7000-PRINT-STATEMENT.

       6000-POST-FEE.
           MOVE WS-REL-SETTLE-ACCT(WS-RX) TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY 'ANLYSFEE SETTLEMENT ACCT NOT FOUND '
                           ACCT-NUMBER
                   MOVE 4 TO WS-RUN-RC
                   EXIT PARAGRAPH
           END-READ
           SUBTRACT ANL-NET-CHARGE FROM ACCT-LEDGER-BAL ACCT-AVAIL-BAL
           ADD ANL-NET-CHARGE TO ACCT-FEES-MTD ACCT-FEES-YTD
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE '6000-POST-FEE' TO ABND-PARAGRAPH
               MOVE 'ACCOUNT REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           ADD 1 TO WS-TRAN-SEQ
           INITIALIZE TRANSACTION-RECORD
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE TRAN-EFFECTIVE-DATE
                                     TRAN-POST-DATE
           MOVE WS-TRAN-SEQ       TO TRAN-ID-SEQ
           MOVE ACCT-NUMBER       TO TRAN-ACCT-NUMBER
           MOVE 224500            TO TRAN-TIME
           MOVE 'WFAN'            TO TRAN-CODE
           MOVE 'D'               TO TRAN-DR-CR
           MOVE ANL-NET-CHARGE    TO TRAN-AMOUNT
           MOVE 'BAT'             TO TRAN-CHANNEL
           STRING 'ACCOUNT ANALYSIS FEE ' WS-BUS-YYYYMM
                  DELIMITED BY SIZE INTO TRAN-DESCRIPTION
           MOVE WS-REL-ID(WS-RX)  TO TRAN-REFERENCE
           MOVE 'S'               TO TRAN-STATUS
           MOVE 'ANLYSFEE'        TO TRAN-USER-ID TRAN-SOURCE-PGM
           MOVE ACCT-COST-CENTER  TO TRAN-COST-CENTER
           MOVE ANL-NET-CHARGE    TO TRAN-FEE-AMOUNT
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE '6000-POST-FEE' TO ABND-PARAGRAPH
               MOVE 'TRANPOST WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           ADD 1 TO WS-FEES-POSTED
           ADD ANL-NET-CHARGE TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ANALYSIS STATEMENT (PRINT IMAGE)                               *
      *----------------------------------------------------------------*
       7000-PRINT-STATEMENT.
           MOVE CTL-BUSINESS-DATE TO WS-SH-DATE
           WRITE REPORT-LINE FROM WS-ST-HDR
           MOVE WS-REL-ID(WS-RX)          TO WS-SR-ID
           MOVE WS-REL-NAME(WS-RX)        TO WS-SR-NAME
           MOVE WS-REL-SEGMENT(WS-RX)     TO WS-SR-SEG
           MOVE WS-REL-SETTLE-ACCT(WS-RX) TO WS-SR-SETTLE
           MOVE WS-REL-ACCT-COUNT(WS-RX)  TO WS-SR-COUNT
           WRITE REPORT-LINE FROM WS-ST-REL
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE

           MOVE SPACES TO WS-SA-NOTE
           MOVE 'AVERAGE LEDGER BALANCE' TO WS-SA-LABEL
           MOVE WS-REL-AVG-LEDGER(WS-RX) TO WS-SA-AMOUNT
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE 'LESS UNCOLLECTED FUNDS (HOLDS)' TO WS-SA-LABEL
           MOVE WS-REL-HOLDS(WS-RX) TO WS-SA-AMOUNT
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE 'AVERAGE COLLECTED BALANCE' TO WS-SA-LABEL
           MOVE WS-REL-AVG-COLLECTED(WS-RX) TO WS-SA-AMOUNT
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE 'INVESTABLE BALANCE (LESS RESERVE)' TO WS-SA-LABEL
           MOVE ECR-INVESTABLE-BAL TO WS-SA-AMOUNT
           MOVE ECR-RATE TO WS-RATE-ED
           STRING 'ECR ' WS-RATE-ED '% TIER ' ECR-TIER-CODE
                  ' ' ECR-REASON-CODE
                  DELIMITED BY SIZE INTO WS-SA-NOTE
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE SPACES TO WS-SA-NOTE
           MOVE 'EARNINGS CREDIT ALLOWANCE' TO WS-SA-LABEL
           MOVE ECR-EARNINGS-CREDIT TO WS-SA-AMOUNT
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE

           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 9
               IF ANL-SVC-VOLUME(WS-SX) > 0
                   MOVE ANL-SVC-CODE(WS-SX)       TO WS-SS-CODE
                   MOVE ANL-SVC-VOLUME(WS-SX)     TO WS-SS-VOLUME
                   MOVE ANL-SVC-UNIT-PRICE(WS-SX) TO WS-SS-PRICE
                   MOVE ANL-SVC-CHARGE(WS-SX)     TO WS-SS-CHARGE
                   WRITE REPORT-LINE FROM WS-ST-SVC
               END-IF
           END-PERFORM
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE

           MOVE 'TOTAL SERVICE CHARGES' TO WS-SA-LABEL
           MOVE ANL-TOTAL-SVC-CHARGE TO WS-SA-AMOUNT
           WRITE REPORT-LINE FROM WS-ST-AMT
           IF ANL-SEGMENT = 'IN'
               MOVE 'LESS PRIOR CREDIT CARRIED FORWARD' TO WS-SA-LABEL
               MOVE ANL-CF-APPLIED TO WS-SA-AMOUNT
               WRITE REPORT-LINE FROM WS-ST-AMT
           END-IF
           MOVE 'LESS EARNINGS CREDIT APPLIED' TO WS-SA-LABEL
           MOVE ANL-CREDIT-APPLIED TO WS-SA-AMOUNT
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE 'NET ANALYSIS CHARGE (WFAN)' TO WS-SA-LABEL
           MOVE ANL-NET-CHARGE TO WS-SA-AMOUNT
           IF ANL-CHARGE-WAIVED
               MOVE 'UNDER MINIMUM - WAIVED' TO WS-SA-NOTE
           END-IF
           WRITE REPORT-LINE FROM WS-ST-AMT
           MOVE SPACES TO WS-SA-NOTE
           IF ANL-SEGMENT = 'IN'
               MOVE 'EXCESS CREDIT CARRIED FORWARD' TO WS-SA-LABEL
               COMPUTE WS-SA-AMOUNT = ANL-CF-OUT(1) + ANL-CF-OUT(2)
                                    + ANL-CF-OUT(3)
               MOVE 'PUBLIC FUNDS - 3 MONTH CARRY-FORWARD (CHG0731)'
                 TO WS-SA-NOTE
               WRITE REPORT-LINE FROM WS-ST-AMT
               MOVE SPACES TO WS-SA-NOTE
               MOVE 'CARRY-FORWARD EXPIRED' TO WS-SA-LABEL
               MOVE ANL-CF-EXPIRED TO WS-SA-AMOUNT
               WRITE REPORT-LINE FROM WS-ST-AMT
           ELSE
               MOVE 'EXCESS CREDIT FORFEITED' TO WS-SA-LABEL
               MOVE ANL-CREDIT-FORFEITED TO WS-SA-AMOUNT
               MOVE 'EXCESS EARNINGS CREDIT DOES NOT CARRY FORWARD'
                 TO WS-SA-NOTE
               WRITE REPORT-LINE FROM WS-ST-AMT
               MOVE SPACES TO WS-SA-NOTE
           END-IF
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE.

       9000-TERMINATE.
           CLOSE ACCOUNT-FILE CUSTOMER-FILE TRANPOST-FILE CFOUT-FILE
                 REPORT-FILE
           DISPLAY 'ANLYSFEE ACCOUNTS=' WS-ACCTS-ANALYSED
                   ' RELATIONSHIPS=' WS-REL-COUNT
                   ' ITEMS=' WS-ITEMS-COUNTED
                   ' FEES POSTED=' WS-FEES-POSTED
           MOVE WS-RUN-RC TO RETURN-CODE.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'ANLYSFEE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
