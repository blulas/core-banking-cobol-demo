      *================================================================*
      * PROGRAM  : INT1099                                             *
      * TYPE     : BATCH - EOD STEP 17 (YEAR-END ONLY)                 *
      * PURPOSE  : FORM 1099-INT INFORMATION RETURN EXTRACT.           *
      *            INTEREST PAID AND FEDERAL TAX WITHHELD ARE SUMMED   *
      *            OVER ALL DEPOSIT ACCOUNTS (OPEN OR CLOSED) BY       *
      *            PRIMARY OWNER - THE PRIMARY OWNER'S TIN IS THE ONLY *
      *            TIN REPORTED FOR A JOINT ACCOUNT.                   *
      *            - REPORTABLE WHEN BOX 1 INTEREST >= $10.00, OR ANY  *
      *              BACKUP WITHHOLDING WAS TAKEN (BOX 4)              *
      *            - CORPORATIONS ARE EXEMPT RECIPIENTS - NOT REPORTED *
      *            - FOREIGN ADDRESS (COUNTRY NOT US) - REPORTED ON    *
      *              FORM 1042-S INSTEAD; LISTED AS AN EXCEPTION       *
      *            - B-NOTICE INDICATOR SET WHEN ANY ACCOUNT OF THE    *
      *              PAYEE IS UNDER BACKUP WITHHOLDING                 *
      *            OUTPUT RECORDS (SIMPLIFIED IRS PUB 1220 LAYOUT,     *
      *            250 BYTES): T TRANSMITTER, A PAYER, B PAYEE,        *
      *            C END OF PAYER, F END OF TRANSMISSION.              *
      *            AFTER THE EXTRACT THE YTD FIELDS ROLL TO PRIOR YEAR *
      *            (INT-PAID-PRIOR-YR = INT-PAID-YTD, YTD FIELDS ZERO).*
      * PARM     : FORCE   = RUN ON ANY DATE (EXTRACT AND RESET)       *
      *            EXTRACT = RUN ON ANY DATE, EXTRACT ONLY - NO RESET  *
      * INPUT    : BANKCTL (GETCTL), CUSTMST                           *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : TAX1099 (IRS EXTRACT), TAXRPT (CONTROL REPORT)      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-01-03 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - 4 DIGIT TAX YEAR                         *
      * 2001-12-17 BHS  BACKUP WITHHOLDING BOX 4 ALWAYS REPORTABLE     *
      * 2008-12-01 DPS  FOREIGN PAYEES TO 1042-S EXCEPTION LIST        *
      * 2014-12-08 DPS  PARM EXTRACT FOR TAX DEPT TEST RUNS - CHG1205  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INT1099.

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
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT TAX-FILE ASSIGN TO "TAX1099"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TAX-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TAXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  TAX-FILE.
       01  TAX-RECORD                      PIC X(250).
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-TAX-STATUS               PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-PARM                         PIC X(10).
           88  WS-PARM-FORCE                         VALUE 'FORCE'.
           88  WS-PARM-EXTRACT                       VALUE 'EXTRACT'.
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-RESET-FLAG                   PIC X(01) VALUE 'Y'.
           88  WS-DO-RESET                           VALUE 'Y'.

       01  WS-REPORTING-THRESHOLD          PIC 9(05)V99 VALUE 10.00.
       01  WS-TAX-YEAR                     PIC 9(04).
       01  WS-PAYER-TIN                    PIC X(09) VALUE '980000001'.
       01  WS-PAYER-NAME                   PIC X(40)
                                           VALUE 'CBS BANK, N.A.'.

       01  WS-PAYEE-TABLE.
           05  WS-PAYEE-COUNT              PIC 9(04) COMP VALUE 0.
           05  WS-PAYEE OCCURS 2000 TIMES INDEXED BY WS-PY-IDX.
               10  WS-PY-CUST-ID           PIC X(10).
               10  WS-PY-INTEREST          PIC S9(13)V99 COMP-3.
               10  WS-PY-WITHHELD          PIC S9(13)V99 COMP-3.
               10  WS-PY-BNOTICE           PIC X(01).
               10  WS-PY-ACCTS             PIC 9(03).

       01  WS-COUNTERS.
           05  WS-B-COUNT                  PIC 9(07) VALUE 0.
           05  WS-BELOW-COUNT              PIC 9(07) VALUE 0.
           05  WS-EXEMPT-COUNT             PIC 9(07) VALUE 0.
           05  WS-FOREIGN-COUNT            PIC 9(07) VALUE 0.
           05  WS-RESET-COUNT              PIC 9(07) VALUE 0.
           05  WS-TOT-BOX1                 PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-BOX4                 PIC S9(13)V99 COMP-3
                                           VALUE 0.

       01  WS-T-RECORD.
           05  FILLER                      PIC X(01) VALUE 'T'.
           05  T-TAX-YEAR                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  T-TIN                       PIC X(09).
           05  FILLER                      PIC X(05) VALUE 'CBS01'.
           05  T-NAME                      PIC X(40).
           05  T-TEST-IND                  PIC X(01).
           05  FILLER                      PIC X(189) VALUE SPACES.
       01  WS-A-RECORD.
           05  FILLER                      PIC X(01) VALUE 'A'.
           05  A-TAX-YEAR                  PIC 9(04).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  A-TIN                       PIC X(09).
           05  FILLER                      PIC X(04) VALUE 'CBSB'.
           05  FILLER                      PIC X(01) VALUE SPACE.
      *        TYPE OF RETURN 6 = 1099-INT, AMOUNT CODES 1 AND 4
           05  FILLER                      PIC X(01) VALUE '6'.
           05  FILLER                      PIC X(16) VALUE '14'.
           05  A-NAME                      PIC X(40).
           05  FILLER                      PIC X(168) VALUE SPACES.
       01  WS-B-RECORD.
           05  FILLER                      PIC X(01) VALUE 'B'.
           05  B-TAX-YEAR                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
      *        TIN TYPE 1 = EIN, 2 = SSN / ITIN
           05  B-TIN-TYPE                  PIC X(01).
           05  B-TIN                       PIC X(09).
           05  B-PAYER-ACCT                PIC X(20).
           05  B-BOX1-INTEREST             PIC 9(12).
           05  B-BOX4-WITHHELD             PIC 9(12).
           05  B-NAME                      PIC X(40).
           05  B-ADDRESS                   PIC X(40).
           05  B-CITY                      PIC X(40).
           05  B-STATE                     PIC X(02).
           05  B-ZIP                       PIC X(09).
      *        2 = B-NOTICE / BACKUP WITHHOLDING IN FORCE
           05  B-NOTICE-IND                PIC X(01).
           05  FILLER                      PIC X(58) VALUE SPACES.
       01  WS-C-RECORD.
           05  FILLER                      PIC X(01) VALUE 'C'.
           05  C-PAYEE-COUNT               PIC 9(08).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  C-TOT-BOX1                  PIC 9(15)V99.
           05  C-TOT-BOX4                  PIC 9(15)V99.
           05  FILLER                      PIC X(201) VALUE SPACES.
       01  WS-F-RECORD.
           05  FILLER                      PIC X(01) VALUE 'F'.
           05  F-A-COUNT                   PIC 9(08) VALUE 1.
           05  F-B-COUNT                   PIC 9(08).
           05  FILLER                      PIC X(233) VALUE SPACES.

       01  WS-RPT-LINE.
           05  RL-CUST                     PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RL-NAME                     PIC X(35).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RL-TIN-TYPE                 PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RL-INTEREST                 PIC ---,---,--9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RL-WITHHELD                 PIC ---,---,--9.99.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  RL-BNOTICE                  PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  RL-ACTION                   PIC X(40).
           05  FILLER                      PIC X(09) VALUE SPACE.
       01  WS-AMT-CENTS                    PIC 9(12).
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
           IF NOT CTL-IS-YEAR-END AND NOT WS-PARM-FORCE
              AND NOT WS-PARM-EXTRACT
               DISPLAY 'INT1099 NOT YEAR-END (' CTL-BUSINESS-DATE
                       ') - NO 1099-INT EXTRACT TODAY'
               MOVE 0 TO RETURN-CODE
               STOP RUN
           END-IF
           IF WS-PARM-EXTRACT
               MOVE 'N' TO WS-RESET-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE(1:4) TO WS-TAX-YEAR
           PERFORM 1000-INITIALIZE
           PERFORM 2000-ACCUMULATE
           PERFORM 3000-WRITE-EXTRACT
           IF WS-DO-RESET
               PERFORM 4000-RESET-YTD
           END-IF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN I-O ACCOUNT-FILE
           OPEN INPUT CUSTOMER-FILE
           OPEN OUTPUT TAX-FILE REPORT-FILE
           IF WS-ACCT-STATUS NOT = '00' OR WS-CUST-STATUS NOT = '00'
              OR WS-TAX-STATUS NOT = '00' OR WS-RPT-STATUS NOT = '00'
               MOVE 'INT1099' TO ABND-PROGRAM
               MOVE '1000-INITIALIZE' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'FILE OPEN FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           MOVE SPACES TO REPORT-LINE
           STRING 'INT1099  FORM 1099-INT EXTRACT  TAX YEAR '
                  WS-TAX-YEAR '   BUSINESS DATE ' CTL-BUSINESS-DATE
                  '   PARM ' WS-PARM
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING 'CUSTOMER   NAME                                T   '
                  'BOX 1 INTEREST   BOX 4 WITHHELD  B  ACTION'
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
       2000-ACCUMULATE.
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-ADD-ACCOUNT
               END-READ
           END-PERFORM.

       2100-ADD-ACCOUNT.
           IF ACCT-CATEGORY NOT = 'SV' AND 'DD' AND 'TD' AND 'MM'
               EXIT PARAGRAPH
           END-IF
           IF ACCT-INT-PAID-YTD = ZERO AND ACCT-WITHHELD-YTD = ZERO
               EXIT PARAGRAPH
           END-IF
           SET WS-PY-IDX TO 1
           SEARCH WS-PAYEE
               AT END
                   IF WS-PAYEE-COUNT < 2000
                       ADD 1 TO WS-PAYEE-COUNT
                       SET WS-PY-IDX TO WS-PAYEE-COUNT
                       MOVE ACCT-PRIMARY-CUST-ID
                         TO WS-PY-CUST-ID(WS-PY-IDX)
                       MOVE ZERO TO WS-PY-INTEREST(WS-PY-IDX)
                                    WS-PY-WITHHELD(WS-PY-IDX)
                                    WS-PY-ACCTS(WS-PY-IDX)
                       MOVE 'N' TO WS-PY-BNOTICE(WS-PY-IDX)
                       PERFORM 2110-ADD-AMOUNTS
                   END-IF
               WHEN WS-PY-IDX > WS-PAYEE-COUNT
                   ADD 1 TO WS-PAYEE-COUNT
                   SET WS-PY-IDX TO WS-PAYEE-COUNT
                   MOVE ACCT-PRIMARY-CUST-ID
                     TO WS-PY-CUST-ID(WS-PY-IDX)
                   MOVE ZERO TO WS-PY-INTEREST(WS-PY-IDX)
                                WS-PY-WITHHELD(WS-PY-IDX)
                                WS-PY-ACCTS(WS-PY-IDX)
                   MOVE 'N' TO WS-PY-BNOTICE(WS-PY-IDX)
                   PERFORM 2110-ADD-AMOUNTS
               WHEN WS-PY-CUST-ID(WS-PY-IDX) = ACCT-PRIMARY-CUST-ID
                   PERFORM 2110-ADD-AMOUNTS
           END-SEARCH.

       2110-ADD-AMOUNTS.
           ADD ACCT-INT-PAID-YTD TO WS-PY-INTEREST(WS-PY-IDX)
           ADD ACCT-WITHHELD-YTD TO WS-PY-WITHHELD(WS-PY-IDX)
           ADD 1 TO WS-PY-ACCTS(WS-PY-IDX)
           IF ACCT-BACKUP-WITHHOLDING
               MOVE 'Y' TO WS-PY-BNOTICE(WS-PY-IDX)
           END-IF.

      *----------------------------------------------------------------*
       3000-WRITE-EXTRACT.
           MOVE WS-TAX-YEAR TO T-TAX-YEAR A-TAX-YEAR
           MOVE WS-PAYER-TIN TO T-TIN A-TIN
           MOVE WS-PAYER-NAME TO T-NAME A-NAME
           IF CTL-TEST OR NOT WS-DO-RESET
               MOVE 'T' TO T-TEST-IND
           ELSE
               MOVE SPACE TO T-TEST-IND
           END-IF
           WRITE TAX-RECORD FROM WS-T-RECORD
           WRITE TAX-RECORD FROM WS-A-RECORD
           PERFORM 3100-ONE-PAYEE
               VARYING WS-PY-IDX FROM 1 BY 1
               UNTIL WS-PY-IDX > WS-PAYEE-COUNT
           MOVE WS-B-COUNT TO C-PAYEE-COUNT F-B-COUNT
           MOVE WS-TOT-BOX1 TO C-TOT-BOX1
           MOVE WS-TOT-BOX4 TO C-TOT-BOX4
           WRITE TAX-RECORD FROM WS-C-RECORD
           WRITE TAX-RECORD FROM WS-F-RECORD.

       3100-ONE-PAYEE.
           MOVE WS-PY-CUST-ID(WS-PY-IDX) TO CUST-ID RL-CUST
           READ CUSTOMER-FILE
               INVALID KEY
                   INITIALIZE CUSTOMER-RECORD
                   MOVE '** CUSTOMER NOT ON FILE **' TO CUST-LAST-NAME
           END-READ
           MOVE SPACES TO RL-NAME
           IF CUST-INDIVIDUAL
               STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO RL-NAME
           ELSE
               MOVE CUST-LAST-NAME TO RL-NAME
           END-IF
           MOVE CUST-TAX-ID-TYPE TO RL-TIN-TYPE
           MOVE WS-PY-INTEREST(WS-PY-IDX) TO RL-INTEREST
           MOVE WS-PY-WITHHELD(WS-PY-IDX) TO RL-WITHHELD
           MOVE WS-PY-BNOTICE(WS-PY-IDX)  TO RL-BNOTICE

           EVALUATE TRUE
               WHEN CUST-CORPORATE
                   MOVE 'EXEMPT RECIPIENT (CORPORATION)' TO RL-ACTION
                   ADD 1 TO WS-EXEMPT-COUNT
               WHEN CUST-COUNTRY NOT = 'US'
                AND CUST-COUNTRY NOT = SPACES
                   MOVE 'FOREIGN PAYEE - REPORT ON 1042-S' TO RL-ACTION
                   ADD 1 TO WS-FOREIGN-COUNT
               WHEN WS-PY-INTEREST(WS-PY-IDX) >= WS-REPORTING-THRESHOLD
               WHEN WS-PY-WITHHELD(WS-PY-IDX) > ZERO
                   PERFORM 3200-WRITE-B-RECORD
                   MOVE '1099-INT B RECORD WRITTEN' TO RL-ACTION
               WHEN OTHER
                   MOVE 'BELOW $10 THRESHOLD - NOT REPORTED'
                     TO RL-ACTION
                   ADD 1 TO WS-BELOW-COUNT
           END-EVALUATE
           WRITE REPORT-LINE FROM WS-RPT-LINE.

       3200-WRITE-B-RECORD.
           MOVE WS-TAX-YEAR TO B-TAX-YEAR
           IF CUST-TAXID-EIN
               MOVE '1' TO B-TIN-TYPE
           ELSE
               MOVE '2' TO B-TIN-TYPE
           END-IF
           MOVE CUST-TAX-ID TO B-TIN
           MOVE CUST-ID TO B-PAYER-ACCT
           COMPUTE WS-AMT-CENTS = WS-PY-INTEREST(WS-PY-IDX) * 100
           MOVE WS-AMT-CENTS TO B-BOX1-INTEREST
           COMPUTE WS-AMT-CENTS = WS-PY-WITHHELD(WS-PY-IDX) * 100
           MOVE WS-AMT-CENTS TO B-BOX4-WITHHELD
           MOVE RL-NAME TO B-NAME
           MOVE CUST-ADDR-LINE1 TO B-ADDRESS
           MOVE CUST-CITY TO B-CITY
           MOVE CUST-STATE TO B-STATE
           MOVE CUST-ZIP TO B-ZIP
           IF WS-PY-BNOTICE(WS-PY-IDX) = 'Y'
               MOVE '2' TO B-NOTICE-IND
           ELSE
               MOVE SPACE TO B-NOTICE-IND
           END-IF
           WRITE TAX-RECORD FROM WS-B-RECORD
           ADD 1 TO WS-B-COUNT
           ADD WS-PY-INTEREST(WS-PY-IDX) TO WS-TOT-BOX1
           ADD WS-PY-WITHHELD(WS-PY-IDX) TO WS-TOT-BOX4.

      *----------------------------------------------------------------*
       4000-RESET-YTD.
           MOVE 'N' TO WS-EOF-FLAG
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF ACCT-CATEGORY = 'SV' OR 'DD' OR 'TD' OR 'MM'
                           MOVE ACCT-INT-PAID-YTD
                             TO ACCT-INT-PAID-PRIOR-YR
                           MOVE ZERO TO ACCT-INT-PAID-YTD
                                        ACCT-WITHHELD-YTD
                           REWRITE ACCOUNT-RECORD
                           IF WS-ACCT-STATUS NOT = '00'
                               MOVE 'INT1099' TO ABND-PROGRAM
                               MOVE '4000-RESET-YTD' TO ABND-PARAGRAPH
                               MOVE 'ACCTMST' TO ABND-FILE-NAME
                               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
                               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
                               CALL 'ABENDRTN' USING ABEND-PARMS
                           END-IF
                           ADD 1 TO WS-RESET-COUNT
                       END-IF
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE '1099-INT PAYEE (B) RECORDS' TO TL-LABEL
           MOVE WS-B-COUNT TO TL-COUNT
           MOVE WS-TOT-BOX1 TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'BOX 4 FEDERAL TAX WITHHELD' TO TL-LABEL
           MOVE ZERO TO TL-COUNT
           MOVE WS-TOT-BOX4 TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE ZERO TO TL-AMOUNT
           MOVE 'BELOW THRESHOLD' TO TL-LABEL
           MOVE WS-BELOW-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'EXEMPT RECIPIENTS' TO TL-LABEL
           MOVE WS-EXEMPT-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'FOREIGN (1042-S)' TO TL-LABEL
           MOVE WS-FOREIGN-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'ACCOUNTS ROLLED TO PRIOR YEAR' TO TL-LABEL
           MOVE WS-RESET-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           CLOSE ACCOUNT-FILE CUSTOMER-FILE TAX-FILE REPORT-FILE
           DISPLAY 'INT1099 PAYEES=' WS-PAYEE-COUNT
                   ' B-RECORDS=' WS-B-COUNT
                   ' BELOW=' WS-BELOW-COUNT
                   ' EXEMPT=' WS-EXEMPT-COUNT
                   ' RESET=' WS-RESET-COUNT
           MOVE 0 TO RETURN-CODE.
