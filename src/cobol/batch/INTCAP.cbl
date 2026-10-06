      *================================================================*
      * PROGRAM  : INTCAP                                              *
      * TYPE     : BATCH - EOD STEP 16 (MONTH-END ONLY)                *
      * PURPOSE  : INTEREST CAPITALISATION. CREDITS UNPAID ACCRUED     *
      *            INTEREST TO THE ACCOUNT (DINT) FOR PRODUCTS THAT    *
      *            CAPITALISE MONTHLY, AND AT QUARTER-END ALSO FOR     *
      *            QUARTERLY PRODUCTS (SVST). CDS CAPITALISE AT        *
      *            MATURITY (CDMATUR), NOT HERE.                       *
      *            - THE ACCRUAL (6 DECIMALS) IS ROUNDED TO CENTS; THE *
      *              ROUNDING REMAINDER STAYS IN ACCT-ACCRUED-INT AND  *
      *              IS CARRIED INTO NEXT MONTH                        *
      *            - IRS BACKUP WITHHOLDING (26 USC 3406) OF 24% OF    *
      *              THE INTEREST PAID IS DEBITED (WBWH) WHEN THE      *
      *              ACCOUNT CARRIES A B-NOTICE (ACCT-BACKUP-WH-FLAG)  *
      *            - UPDATES INT-PAID-YTD, WITHHELD-YTD, LAST-CAP-DATE *
      * PARM     : FORCE = RUN ON A NON MONTH-END DATE (QUARTERLY      *
      *            PRODUCTS ARE THEN ALSO CAPITALISED)                 *
      * INPUT    : BANKCTL (GETCTL), PRODMST                           *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : TRANPOST (EXTEND) DINT / WBWH, INTCAPR REPORT       *
      * TRAN IDS : 40000001 - 44999999                                 *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1993-03-15 RJT  QUARTERLY CAPITALISATION FOR PASSBOOK SAVINGS  *
      * 1999-12-06 MKL  ROUNDING REMAINDER CARRIED FORWARD - INC01722  *
      * 2002-01-02 BHS  BACKUP WITHHOLDING RATE 30% -> 30.5% (EGTRRA)  *
      * 2003-01-02 BHS  BACKUP WITHHOLDING 30% -> 28%                  *
      * 2018-01-02 AKW  BACKUP WITHHOLDING 28% -> 24% (TCJA) CHG1499   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTCAP.
       AUTHOR. R J TRAVERS.

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
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT REPORT-FILE ASSIGN TO "INTCAPR"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-PARM                         PIC X(10).
           88  WS-PARM-FORCE                         VALUE 'FORCE'.
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-CAP-FLAG                     PIC X(01).
           88  WS-CAPITALISE                         VALUE 'Y'.

      *    26 USC 3406 BACKUP WITHHOLDING RATE
       01  WS-BACKUP-WH-RATE               PIC V9(04) VALUE .2400.
      *    2003-01 BHS  WAS .2800
      *01  WS-BACKUP-WH-RATE               PIC V9(04) VALUE .2800.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-CAP-COUNT                PIC 9(07) VALUE 0.
           05  WS-WH-COUNT                 PIC 9(07) VALUE 0.
           05  WS-NOTDUE-COUNT             PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
           05  WS-TRAN-SEQ                 PIC 9(08) VALUE 40000000.

       01  WS-AMOUNTS.
           05  WS-PAY-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-WH-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-REMAINDER                PIC S9(09)V9(06) COMP-3.
           05  WS-TOT-PAID                 PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-WH                   PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-POST-AMT                 PIC S9(13)V99 COMP-3.
           05  WS-POST-CODE                PIC X(04).
           05  WS-POST-DRCR                PIC X(01).
           05  WS-POST-DESC                PIC X(40).

       01  WS-HEADING-1.
           05  FILLER PIC X(08) VALUE 'INTCAP  '.
           05  FILLER PIC X(50)
               VALUE 'INTEREST CAPITALISATION REPORT'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  H1-BUS-DATE                 PIC 9(08).
           05  FILLER PIC X(51) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER PIC X(30) VALUE
               'ACCOUNT      PROD CAP    ACCRU'.
           05  FILLER PIC X(30) VALUE
               'ED (6DP)     INTEREST PAID  BA'.
           05  FILLER PIC X(30) VALUE
               'CKUP WITHHELD   CARRIED REMAIN'.
           05  FILLER PIC X(42) VALUE
               'DER    NEW LEDGER BALANCE'.
       01  WS-DETAIL-LINE.
           05  DL-ACCT                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-PROD                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-CAP                      PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-ACCRUED                  PIC ---,---,--9.999999.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-PAID                     PIC ----,---,--9.99.
           05  FILLER                      PIC X(03) VALUE SPACE.
           05  DL-WH                       PIC ----,---,--9.99.
           05  FILLER                      PIC X(03) VALUE SPACE.
           05  DL-REMAINDER                PIC ----9.999999.
           05  FILLER                      PIC X(05) VALUE SPACE.
           05  DL-NEW-BAL                  PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(20) VALUE SPACE.
       01  WS-TOTAL-LINE.
           05  TL-LABEL                    PIC X(30).
           05  TL-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC ---,---,---,--9.99.
           05  FILLER                      PIC X(72) VALUE SPACES.

       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF NOT CTL-IS-MONTH-END AND NOT WS-PARM-FORCE
               DISPLAY 'INTCAP  NOT MONTH-END (' CTL-BUSINESS-DATE
                       ') - NO CAPITALISATION TODAY'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 1000-INITIALIZE
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
           OPEN INPUT PRODUCT-FILE
           IF WS-PROD-STATUS NOT = '00'
               MOVE 'PRODMST' TO ABND-FILE-NAME
               MOVE WS-PROD-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'INTCAPR' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           MOVE CTL-BUSINESS-DATE TO H1-BUS-DATE
           WRITE REPORT-LINE FROM WS-HEADING-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-HEADING-2.

      *----------------------------------------------------------------*
       2000-PROCESS-ACCOUNTS.
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-CAPITALISE-ACCOUNT
               END-READ
           END-PERFORM.

       2100-CAPITALISE-ACCOUNT.
           ADD 1 TO WS-READ-COUNT
           IF ACCT-CATEGORY NOT = 'SV' AND 'DD' AND 'MM'
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-ACTIVE AND NOT ACCT-DORMANT
              AND NOT ACCT-FROZEN AND NOT ACCT-RESTRICTED
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'INTCAP  PRODUCT NOT FOUND ' PROD-CODE
                   EXIT PARAGRAPH
           END-READ

           MOVE 'N' TO WS-CAP-FLAG
           EVALUATE TRUE
               WHEN PROD-CAP-MONTHLY
                   SET WS-CAPITALISE TO TRUE
               WHEN PROD-CAP-QUARTERLY
                   IF CTL-IS-QUARTER-END OR WS-PARM-FORCE
                       SET WS-CAPITALISE TO TRUE
                   ELSE
                       ADD 1 TO WS-NOTDUE-COUNT
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT WS-CAPITALISE
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-PAY-AMT ROUNDED = ACCT-ACCRUED-INT
           IF WS-PAY-AMT NOT > ZERO
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-REMAINDER = ACCT-ACCRUED-INT - WS-PAY-AMT
           MOVE ZERO TO WS-WH-AMT

           MOVE WS-PAY-AMT TO WS-POST-AMT
           MOVE 'DINT' TO WS-POST-CODE
           MOVE 'C'    TO WS-POST-DRCR
           MOVE 'INTEREST PAID' TO WS-POST-DESC
           PERFORM 8000-WRITE-TRANPOST
           ADD WS-PAY-AMT TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
                             ACCT-INT-PAID-YTD WS-TOT-PAID
           ADD 1 TO ACCT-CR-COUNT-CYC
           MOVE WS-REMAINDER TO ACCT-ACCRUED-INT
           MOVE CTL-BUSINESS-DATE TO ACCT-LAST-CAP-DATE
           ADD 1 TO WS-CAP-COUNT

           IF ACCT-BACKUP-WITHHOLDING
               COMPUTE WS-WH-AMT ROUNDED =
                       WS-PAY-AMT * WS-BACKUP-WH-RATE
               IF WS-WH-AMT > ZERO
                   MOVE WS-WH-AMT TO WS-POST-AMT
                   MOVE 'WBWH' TO WS-POST-CODE
                   MOVE 'D'    TO WS-POST-DRCR
                   MOVE 'IRS BACKUP WITHHOLDING 24%' TO WS-POST-DESC
                   PERFORM 8000-WRITE-TRANPOST
                   SUBTRACT WS-WH-AMT FROM ACCT-LEDGER-BAL
                                           ACCT-AVAIL-BAL
                   ADD WS-WH-AMT TO ACCT-WITHHELD-YTD WS-TOT-WH
                   ADD 1 TO ACCT-DR-COUNT-CYC
                   ADD 1 TO WS-WH-COUNT
               END-IF
           END-IF

           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'INTCAP' TO ABND-PROGRAM
               MOVE '2100-CAPITALISE-ACCOUNT' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF

           MOVE ACCT-NUMBER       TO DL-ACCT
           MOVE ACCT-PRODUCT-CODE TO DL-PROD
           MOVE PROD-CAPITALIZE-FREQ TO DL-CAP
           COMPUTE DL-ACCRUED = WS-PAY-AMT + WS-REMAINDER
           MOVE WS-PAY-AMT        TO DL-PAID
           MOVE WS-WH-AMT         TO DL-WH
           MOVE WS-REMAINDER      TO DL-REMAINDER
           MOVE ACCT-LEDGER-BAL   TO DL-NEW-BAL
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
           MOVE 230000            TO TRAN-TIME
           MOVE WS-POST-CODE      TO TRAN-CODE
           MOVE WS-POST-DRCR      TO TRAN-DR-CR
           MOVE WS-POST-AMT       TO TRAN-AMOUNT
           MOVE 'BAT'             TO TRAN-CHANNEL
           MOVE WS-POST-DESC      TO TRAN-DESCRIPTION
           SET TRAN-POSTED        TO TRUE
           MOVE 'INTCAP'          TO TRAN-USER-ID
                                     TRAN-SOURCE-PGM
           MOVE ACCT-COST-CENTER  TO TRAN-COST-CENTER
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           WRITE TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'INTCAP' TO ABND-PROGRAM
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
           MOVE 'ACCOUNTS CAPITALISED' TO TL-LABEL
           MOVE WS-CAP-COUNT TO TL-COUNT
           MOVE WS-TOT-PAID TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'BACKUP WITHHOLDING' TO TL-LABEL
           MOVE WS-WH-COUNT TO TL-COUNT
           MOVE WS-TOT-WH TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'QUARTERLY - NOT DUE' TO TL-LABEL
           MOVE WS-NOTDUE-COUNT TO TL-COUNT
           MOVE ZERO TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           CLOSE ACCOUNT-FILE PRODUCT-FILE TRANPOST-FILE REPORT-FILE
           DISPLAY 'INTCAP  READ=' WS-READ-COUNT
                   ' CAPITALISED=' WS-CAP-COUNT
                   ' WITHHELD=' WS-WH-COUNT
                   ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       9900-ABEND-OPEN.
           MOVE 'INTCAP' TO ABND-PROGRAM
           MOVE '1000-INITIALIZE' TO ABND-PARAGRAPH
           MOVE 'FILE OPEN FAILED' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
