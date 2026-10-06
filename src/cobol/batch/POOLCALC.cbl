      *================================================================*
      * PROGRAM  : POOLCALC                                            *
      * TYPE     : BATCH - MONTH END (EOD STEP 16)                     *
      * PURPOSE  : NOTIONAL POOL INTEREST. FOR EACH ACTIVE POOL THE    *
      *            CYCLE AVERAGE BALANCE OF THE HEADER AND EVERY       *
      *            MEMBER (ACCT-BAL-ACCUM-CYC / ACCT-DAYS-IN-CYCLE) IS *
      *            PASSED TO POOLALOC, WHICH RETURNS EACH MEMBER'S     *
      *            SHARE OF CREDIT OR DEBIT INTEREST.                  *
      *              CREDIT INTEREST -> DINT CREDIT TO THE MEMBER      *
      *              DEBIT INTEREST  -> ADJD DEBIT TO THE MEMBER WITH  *
      *                                 TRAN-GL-ACCOUNT 4020 (POOL     *
      *                                 DEBIT INTEREST INCOME) BECAUSE *
      *                                 TRANCODE HAS NO DEBIT-INTEREST *
      *                                 CODE                           *
      *            POOL LIMIT BREACH -> LIMT ALERT.                    *
      *            RUNS ONLY ON MONTH END (CTL-MONTH-END-FLAG) UNLESS  *
      *            PARM 'FORCE'. A POOL ALREADY ALLOCATED FOR THE      *
      *            BUSINESS DATE IS NOT ALLOCATED AGAIN.               *
      *            TRAN-ID-SEQ 35000001 - 39999999.                    *
      * INPUT    : BANKCTL, POOLMST, ACCTMST                           *
      * OUTPUT   : ACCTMST, POOLMST, TRANPOST, ALERTS, POOLRPT         *
      * RETURN   : 0 OK, 4 LIMIT BREACH OR MEMBER ERRORS, 16 ABEND     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-03-15 BHS  ORIGINAL - NOTIONAL POOLING                    *
      * 2009-01-20 DPS  RATES FROM BANKCTL FED FUNDS / PRIME           *
      * 2013-06-11 DPS  POOL LIMIT ALERT; ADJD WITH GL OVERRIDE 4020   *
      * 2015-10-02 DPS  TRANPOST OUTPUT FOR GL EXTRACT                 *
      * 2017-11-30 AKW  INC27719 RERUN PROTECTION (LAST ALLOC DATE)    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POOLCALC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POOL-FILE ASSIGN TO "POOLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS POOL-ID
               FILE STATUS IS WS-POOL-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "POOLRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  POOL-FILE.
       COPY POOLREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-POOL-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-PARM                         PIC X(10).
           88  WS-FORCE                              VALUE 'FORCE'.
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.
       01  WS-TRAN-SEQ                     PIC 9(08) VALUE 35000000.
       01  WS-RUN-RC                       PIC 9(02) VALUE 0.
       01  WS-DAYS-IN-MONTH                PIC 9(03).
       01  WS-IX                           PIC 9(02).
       01  WS-MBR-ERRORS                   PIC 9(03).
       01  WS-COUNTERS.
           05  WS-POOLS-READ               PIC 9(05) VALUE 0.
           05  WS-POOLS-ALLOCATED          PIC 9(05) VALUE 0.
           05  WS-POSTINGS                 PIC 9(05) VALUE 0.
           05  WS-TOTAL-CREDIT             PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-DEBIT              PIC S9(13)V99 COMP-3
                                           VALUE 0.
       01  WS-POST-WORK.
           05  WS-POST-ACCT                PIC X(12).
           05  WS-POST-CODE                PIC X(04).
           05  WS-POST-DRCR                PIC X(01).
           05  WS-POST-AMOUNT              PIC S9(13)V99 COMP-3.
           05  WS-POST-GL                  PIC X(10).
           05  WS-POST-DESC                PIC X(40).

      *--- REPORT ----------------------------------------------------*
       01  WS-RPT-HEAD1.
           05  FILLER PIC X(48) VALUE
               'CBS  POOLCALC  - NOTIONAL POOL INTEREST REPORT  '.
           05  FILLER PIC X(16) VALUE 'BUSINESS DATE  '.
           05  WS-RH-DATE                  PIC 9(08).
           05  FILLER PIC X(10) VALUE '   DAYS  '.
           05  WS-RH-DAYS                  PIC ZZ9.
       01  WS-RPT-POOL.
           05  FILLER PIC X(06) VALUE 'POOL  '.
           05  WS-RP-ID                    PIC X(08).
           05  FILLER PIC X(02) VALUE SPACES.
           05  WS-RP-NAME                  PIC X(30).
           05  FILLER PIC X(09) VALUE ' METHOD '.
           05  WS-RP-METHOD                PIC X(01).
           05  FILLER PIC X(10) VALUE '  CR RATE '.
           05  WS-RP-CR-RATE               PIC ZZ9.999999.
           05  FILLER PIC X(10) VALUE '  DR RATE '.
           05  WS-RP-DR-RATE               PIC ZZ9.999999.
       01  WS-RPT-POS.
           05  FILLER PIC X(16) VALUE '  GROSS CREDIT  '.
           05  WS-RS-CREDIT                PIC -(13)9.99.
           05  FILLER PIC X(16) VALUE '  GROSS DEBIT  '.
           05  WS-RS-DEBIT                 PIC -(13)9.99.
           05  FILLER PIC X(08) VALUE '  NET  '.
           05  WS-RS-NET                   PIC -(13)9.99.
           05  FILLER PIC X(08) VALUE '  LIMIT '.
           05  WS-RS-LIMIT                 PIC -(13)9.99.
       01  WS-RPT-MBR.
           05  FILLER PIC X(04) VALUE SPACES.
           05  WS-RM-ACCT                  PIC X(12).
           05  FILLER PIC X(01) VALUE SPACE.
           05  WS-RM-HDR                   PIC X(06).
           05  FILLER PIC X(12) VALUE ' AVG BAL   '.
           05  WS-RM-AVG                   PIC -(13)9.99.
           05  FILLER PIC X(12) VALUE '  CR INT   '.
           05  WS-RM-CR                    PIC -(9)9.99.
           05  FILLER PIC X(12) VALUE '  DR INT   '.
           05  WS-RM-DR                    PIC -(9)9.99.
       01  WS-RPT-MSG.
           05  FILLER PIC X(04) VALUE SPACES.
           05  WS-RG-TEXT                  PIC X(128).

       COPY POOLPARM.
       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND NOT WS-FORCE
               DISPLAY 'POOLCALC NOT MONTH END - NO ACTION'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALISE
           MOVE LOW-VALUES TO POOL-ID
           START POOL-FILE KEY IS NOT LESS THAN POOL-ID
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-POOL-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ POOL-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-POOLS-READ
                       PERFORM 2000-PROCESS-POOL
               END-READ
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
      *    INTEREST DAYS = CALENDAR DAYS IN THE BUSINESS MONTH
           INITIALIZE DATE-PARMS
           SET DTU-END-OF-MONTH TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-DAYS-IN-MONTH =
                   FUNCTION MOD(DTU-RESULT-DATE, 100)

           OPEN I-O POOL-FILE
           IF WS-POOL-STATUS NOT = '00'
               MOVE 'POOLMST' TO ABND-FILE-NAME
               MOVE WS-POOL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND WS-POST-STATUS NOT = '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND ALERT-FILE
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RH-DATE
           MOVE WS-DAYS-IN-MONTH  TO WS-RH-DAYS
           WRITE REPORT-LINE FROM WS-RPT-HEAD1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
       2000-PROCESS-POOL.
           IF NOT POOL-ACTIVE
               EXIT PARAGRAPH
           END-IF
           IF POOL-LAST-ALLOC-DATE >= CTL-BUSINESS-DATE
               DISPLAY 'POOLCALC ' POOL-ID ' ALREADY ALLOCATED '
                       POOL-LAST-ALLOC-DATE
               EXIT PARAGRAPH
           END-IF

           INITIALIZE POOL-ALLOC-PARMS
           MOVE POOL-ALLOC-METHOD   TO PLA-ALLOC-METHOD
           MOVE CTL-FED-FUNDS-RATE  TO PLA-REF-CREDIT-RATE
           MOVE CTL-PRIME-RATE      TO PLA-REF-DEBIT-RATE
           MOVE POOL-CREDIT-SPREAD  TO PLA-CREDIT-SPREAD
           MOVE POOL-DEBIT-SPREAD   TO PLA-DEBIT-SPREAD
           MOVE POOL-LIMIT          TO PLA-POOL-LIMIT
           MOVE WS-DAYS-IN-MONTH    TO PLA-DAYS
           MOVE ZERO TO WS-MBR-ERRORS

      *    HEADER IS MEMBER 1
           MOVE 1 TO PLA-MEMBER-COUNT
           MOVE POOL-HEADER-ACCT TO PLA-MBR-ACCT(1)
           MOVE 'Y'              TO PLA-MBR-HEADER-FLAG(1)
           PERFORM 2100-AVERAGE-BALANCE
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > POOL-MEMBER-COUNT OR WS-IX > 20
               ADD 1 TO PLA-MEMBER-COUNT
               MOVE POOL-MEMBER-ACCT(WS-IX)
                 TO PLA-MBR-ACCT(PLA-MEMBER-COUNT)
               MOVE 'N' TO PLA-MBR-HEADER-FLAG(PLA-MEMBER-COUNT)
               PERFORM 2100-AVERAGE-BALANCE
           END-PERFORM

           CALL 'POOLALOC' USING POOL-ALLOC-PARMS
           IF PLA-RETURN-CODE > 4
               DISPLAY 'POOLCALC ' POOL-ID ' POOLALOC RC '
                       PLA-RETURN-CODE ' ' PLA-REASON-CODE
               MOVE 4 TO WS-RUN-RC
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-POOLS-ALLOCATED
           PERFORM 3000-REPORT-POOL
           PERFORM 4000-POST-MEMBERS
               VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > PLA-MEMBER-COUNT
           IF PLA-LIMIT-BREACHED
               PERFORM 8500-LIMIT-ALERT
           END-IF
           PERFORM 5000-UPDATE-POOL.

      *    CYCLE AVERAGE = ACCUMULATED DAILY BALANCES / DAYS IN CYCLE
       2100-AVERAGE-BALANCE.
           MOVE PLA-MBR-ACCT(PLA-MEMBER-COUNT) TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-MBR-ERRORS
                   MOVE 4 TO WS-RUN-RC
                   MOVE ZERO TO PLA-MBR-AVG-BAL(PLA-MEMBER-COUNT)
                   DISPLAY 'POOLCALC MEMBER NOT FOUND '
                           PLA-MBR-ACCT(PLA-MEMBER-COUNT)
                   EXIT PARAGRAPH
           END-READ
           IF ACCT-DAYS-IN-CYCLE > 0
               COMPUTE PLA-MBR-AVG-BAL(PLA-MEMBER-COUNT) ROUNDED =
                       ACCT-BAL-ACCUM-CYC / ACCT-DAYS-IN-CYCLE
           ELSE
               MOVE ACCT-LEDGER-BAL
                 TO PLA-MBR-AVG-BAL(PLA-MEMBER-COUNT)
           END-IF.

       3000-REPORT-POOL.
           MOVE POOL-ID           TO WS-RP-ID
           MOVE POOL-NAME         TO WS-RP-NAME
           MOVE POOL-ALLOC-METHOD TO WS-RP-METHOD
           MOVE PLA-CREDIT-RATE   TO WS-RP-CR-RATE
           MOVE PLA-DEBIT-RATE    TO WS-RP-DR-RATE
           WRITE REPORT-LINE FROM WS-RPT-POOL
           MOVE PLA-GROSS-CREDIT-POS TO WS-RS-CREDIT
           MOVE PLA-GROSS-DEBIT-POS  TO WS-RS-DEBIT
           MOVE PLA-NET-POSITION     TO WS-RS-NET
           MOVE POOL-LIMIT           TO WS-RS-LIMIT
           WRITE REPORT-LINE FROM WS-RPT-POS
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PLA-MEMBER-COUNT
               MOVE PLA-MBR-ACCT(WS-IX)       TO WS-RM-ACCT
               IF PLA-MBR-HEADER-FLAG(WS-IX) = 'Y'
                   MOVE 'HEADER' TO WS-RM-HDR
               ELSE
                   MOVE 'MEMBER' TO WS-RM-HDR
               END-IF
               MOVE PLA-MBR-AVG-BAL(WS-IX)    TO WS-RM-AVG
               MOVE PLA-MBR-CREDIT-INT(WS-IX) TO WS-RM-CR
               MOVE PLA-MBR-DEBIT-INT(WS-IX)  TO WS-RM-DR
               WRITE REPORT-LINE FROM WS-RPT-MBR
           END-PERFORM
           IF PLA-LIMIT-BREACHED
               MOVE '*** POOL LIMIT BREACHED - GROSS DEBIT POSITION '
                 & 'EXCEEDS APPROVED LIMIT - LIMT ALERT RAISED'
                 TO WS-RG-TEXT
               WRITE REPORT-LINE FROM WS-RPT-MSG
           END-IF
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

       4000-POST-MEMBERS.
           IF PLA-MBR-CREDIT-INT(WS-IX) > 0
               MOVE PLA-MBR-ACCT(WS-IX)       TO WS-POST-ACCT
               MOVE 'DINT'                    TO WS-POST-CODE
               MOVE 'C'                       TO WS-POST-DRCR
               MOVE PLA-MBR-CREDIT-INT(WS-IX) TO WS-POST-AMOUNT
               MOVE SPACES                    TO WS-POST-GL
               MOVE 'NOTIONAL POOL CREDIT INTEREST'
                 TO WS-POST-DESC
               PERFORM 8000-POST
               ADD WS-POST-AMOUNT TO WS-TOTAL-CREDIT
           END-IF
           IF PLA-MBR-DEBIT-INT(WS-IX) > 0
               MOVE PLA-MBR-ACCT(WS-IX)       TO WS-POST-ACCT
               MOVE 'ADJD'                    TO WS-POST-CODE
               MOVE 'D'                       TO WS-POST-DRCR
               MOVE PLA-MBR-DEBIT-INT(WS-IX)  TO WS-POST-AMOUNT
               MOVE '4020'                    TO WS-POST-GL
               MOVE 'NOTIONAL POOL DEBIT INTEREST'
                 TO WS-POST-DESC
               PERFORM 8000-POST
               ADD WS-POST-AMOUNT TO WS-TOTAL-DEBIT
           END-IF.

       5000-UPDATE-POOL.
           MOVE CTL-BUSINESS-DATE    TO POOL-LAST-ALLOC-DATE
           MOVE PLA-TOTAL-CREDIT-INT TO POOL-LAST-CREDIT-INT
           MOVE PLA-TOTAL-DEBIT-INT  TO POOL-LAST-DEBIT-INT
           MOVE PLA-NET-POSITION     TO POOL-LAST-NET-POSITION
           REWRITE POOL-RECORD
           IF WS-POOL-STATUS NOT = '00'
               MOVE 'POOLMST' TO ABND-FILE-NAME
               MOVE WS-POOL-STATUS TO ABND-FILE-STATUS
               MOVE '5000-UPDATE-POOL' TO ABND-PARAGRAPH
               MOVE 'POOL REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

      *----------------------------------------------------------------*
       8000-POST.
           MOVE WS-POST-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   MOVE 4 TO WS-RUN-RC
                   EXIT PARAGRAPH
           END-READ
           IF WS-POST-DRCR = 'D'
               SUBTRACT WS-POST-AMOUNT FROM ACCT-LEDGER-BAL
                                            ACCT-AVAIL-BAL
           ELSE
               ADD WS-POST-AMOUNT TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
               ADD WS-POST-AMOUNT TO ACCT-INT-PAID-YTD
           END-IF
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE '8000-POST' TO ABND-PARAGRAPH
               MOVE 'ACCOUNT REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           ADD 1 TO WS-TRAN-SEQ
           INITIALIZE TRANSACTION-RECORD
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE TRAN-EFFECTIVE-DATE
                                     TRAN-POST-DATE
           MOVE WS-TRAN-SEQ       TO TRAN-ID-SEQ
           MOVE WS-POST-ACCT      TO TRAN-ACCT-NUMBER
           MOVE 223000            TO TRAN-TIME
           MOVE WS-POST-CODE      TO TRAN-CODE
           MOVE WS-POST-DRCR      TO TRAN-DR-CR
           MOVE WS-POST-AMOUNT    TO TRAN-AMOUNT
           MOVE 'BAT'             TO TRAN-CHANNEL
           MOVE WS-POST-DESC      TO TRAN-DESCRIPTION
           MOVE POOL-ID           TO TRAN-REFERENCE
           MOVE 'S'               TO TRAN-STATUS
           MOVE 'POOLCALC'        TO TRAN-USER-ID TRAN-SOURCE-PGM
           MOVE ACCT-COST-CENTER  TO TRAN-COST-CENTER
           MOVE WS-POST-GL        TO TRAN-GL-ACCOUNT
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               MOVE '8000-POST' TO ABND-PARAGRAPH
               MOVE 'TRANPOST WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           ADD 1 TO WS-POSTINGS.

       8500-LIMIT-ALERT.
           MOVE 4 TO WS-RUN-RC
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE  TO ALRT-DATE
           MOVE 223000             TO ALRT-TIME
           MOVE 'LIMT'             TO ALRT-TYPE
           MOVE 'H'                TO ALRT-SEVERITY
           MOVE POOL-CUST-ID       TO ALRT-CUST-ID
           MOVE POOL-HEADER-ACCT   TO ALRT-ACCT-NUMBER
           MOVE PLA-GROSS-DEBIT-POS TO ALRT-AMOUNT
           MOVE 'POOL-LIM'         TO ALRT-RULE-ID
           STRING POOL-ID ' NOTIONAL POOL AGGREGATE DEBIT EXCEEDS '
                  'APPROVED POOL LIMIT - CREDIT REVIEW'
                  DELIMITED BY SIZE INTO ALRT-DESCRIPTION
           MOVE 'O'                TO ALRT-STATUS
           MOVE 'POOLCALC'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD.

       9000-TERMINATE.
           CLOSE POOL-FILE ACCOUNT-FILE TRANPOST-FILE ALERT-FILE
                 REPORT-FILE
           DISPLAY 'POOLCALC POOLS READ=' WS-POOLS-READ
                   ' ALLOCATED=' WS-POOLS-ALLOCATED
                   ' POSTINGS=' WS-POSTINGS
           MOVE WS-RUN-RC TO RETURN-CODE.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'POOLCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
