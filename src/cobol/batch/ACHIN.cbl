      *================================================================*
      * PROGRAM  : ACHIN                                               *
      * TYPE     : BATCH - EOD STEP 02                                 *
      * PURPOSE  : INBOUND ACH (RDFI) PROCESSING. READS THE NACHA FILE *
      *            RECEIVED FROM THE FED ACH OPERATOR, RECONCILES      *
      *            BATCH AND FILE CONTROL TOTALS (ENTRY COUNT, ENTRY   *
      *            HASH, DEBIT AND CREDIT TOTALS), VALIDATES EACH      *
      *            ENTRY (ACHVALID) AND:                               *
      *              - MAPS CREDITS 22/32 TO DACH, DEBITS 27/37 TO     *
      *                WACH ON DD ACHTRAN (TRANEDIT INPUT),            *
      *                CHANNEL 'SDA' FOR SAME-DAY BATCHES ('SD' IN     *
      *                COMPANY DESCRIPTIVE DATE), ELSE 'ACH'           *
      *              - ACCOUNT NOT ON FILE -> R03, CLOSED/ESCHEATED    *
      *                -> R02, ACHVALID REJECT -> ITS R CODE; RETURN   *
      *                ENTRIES (6 + 99 ADDENDA) STAGED TO DD ACHRTN    *
      *              - PRENOTES (23/28/33/38) ARE VALIDATED, NOT POSTED*
      *            FROZEN / RESTRICTED / NSF ARE LEFT TO POSTING; THEY *
      *            COME BACK THROUGH TRANREJ TO ACHRET.                *
      * INPUT    : ACHINBND (NACHA 94), ACCTMST, BANKCTL               *
      * OUTPUT   : ACHTRAN (TRANREC), ACHRTN (NACHA 94, STAGING),      *
      *            ACHINRPT (REPORT), ALERTS (RECN ON CONTROL BREAK)   *
      * TRAN IDS : BUSINESS DATE + SEQ 10000001 UPWARDS                *
      * RC       : 00 OK, 04 CONTROL BREAK OR REJECTED RECORDS         *
      *----------------------------------------------------------------*
      * NOTE     : TRAN-DESCRIPTION LAYOUT FOR ACH ITEMS (USED BY      *
      *            ACHRET): 1-16 COMPANY NAME, 18-27 ENTRY DESC,       *
      *            29-31 SEC CODE, 33-34 ORIGINAL NACHA TRAN CODE.     *
      *            TRAN-REFERENCE = TRACE NUMBER, TRAN-COUNTER-ACCT =  *
      *            COMPANY ID, TRAN-COUNTERPARTY-ROUTING = ODFI ABA.   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-12 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - EFFECTIVE DATE WINDOWING (20YY)          *
      * 2001-04-30 BHS  RETURN ENTRIES STAGED FOR ACHRET               *
      * 2008-03-03 BHS  ENTRY HASH RECONCILIATION - INC07714           *
      * 2013-09-16 DPS  ACHVALID FOR SEC / ACCOUNT TYPE                *
      * 2016-09-23 DPS  SAME-DAY ACH CHANNEL 'SDA'                     *
      * 2021-11-08 AKW  RECN ALERT ON CONTROL BREAK                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACHIN.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACH-IN-FILE ASSIGN TO "ACHINBND"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT ACH-TRAN-FILE ASSIGN TO "ACHTRAN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           SELECT ACH-RTN-FILE ASSIGN TO "ACHRTN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RTN-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ACHINRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACH-IN-FILE.
       01  ACH-IN-REC                      PIC X(94).
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  ACH-TRAN-FILE.
       COPY TRANREC.
       FD  ACH-RTN-FILE.
       01  ACH-RTN-REC                     PIC X(94).
       FD  REPORT-FILE.
       01  REPORT-REC                      PIC X(132).
       FD  ALERT-FILE.
       COPY ALERTREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-IN-STATUS                PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-RTN-STATUS               PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.

       01  WS-CONSTANTS.
           05  WS-OUR-ABA                  PIC X(09) VALUE '026000013'.
           05  WS-OUR-DFI                  PIC X(08) VALUE '02600001'.
           05  WS-SEQ-BASE                 PIC 9(08) VALUE 10000000.
           05  WS-HASH-MODULUS             PIC 9(11) VALUE 10000000000.

       COPY NACHAREC.

      *--- CURRENT BATCH HEADER ---------------------------------------*
       01  WS-BATCH-HDR.
           05  WS-BH-SERVICE-CLASS         PIC 9(03).
           05  WS-BH-COMPANY-NAME          PIC X(16).
           05  WS-BH-COMPANY-ID            PIC X(10).
           05  WS-BH-SEC-CODE              PIC X(03).
           05  WS-BH-ENTRY-DESC            PIC X(10).
           05  WS-BH-DESC-DATE             PIC X(06).
           05  WS-BH-EFF-DATE              PIC X(06).
           05  WS-BH-ODFI                  PIC X(08).
           05  WS-BH-BATCH-NUMBER          PIC 9(07).
           05  WS-BH-EFF-CCYYMMDD          PIC 9(08).
           05  WS-BH-CHANNEL               PIC X(03).
           05  WS-BH-ODFI-ABA              PIC X(09).
           05  WS-BH-OPEN                  PIC X(01) VALUE 'N'.
               88  WS-BATCH-OPEN                     VALUE 'Y'.
       01  WS-FILE-HDR.
           05  WS-FH-CREATE-TIME           PIC 9(04) VALUE 0.
           05  WS-FH-IMMED-DEST            PIC X(10).

      *--- CONTROL ACCUMULATORS ---------------------------------------*
       01  WS-BATCH-TOTALS.
           05  WS-BT-ENTRY-ADDENDA         PIC 9(06).
           05  WS-BT-HASH                  PIC 9(15).
           05  WS-BT-DEBIT                 PIC 9(10)V99.
           05  WS-BT-CREDIT                PIC 9(10)V99.
       01  WS-FILE-TOTALS.
           05  WS-FT-BATCH-COUNT           PIC 9(06) VALUE 0.
           05  WS-FT-ENTRY-ADDENDA         PIC 9(08) VALUE 0.
           05  WS-FT-HASH                  PIC 9(15) VALUE 0.
           05  WS-FT-DEBIT                 PIC 9(10)V99 VALUE 0.
           05  WS-FT-CREDIT                PIC 9(10)V99 VALUE 0.
           05  WS-FT-RECORD-COUNT          PIC 9(08) VALUE 0.
       01  WS-COUNTERS.
           05  WS-ENTRY-COUNT              PIC 9(07) VALUE 0.
           05  WS-CREDIT-COUNT             PIC 9(07) VALUE 0.
           05  WS-DEBIT-COUNT              PIC 9(07) VALUE 0.
           05  WS-PRENOTE-COUNT            PIC 9(07) VALUE 0.
           05  WS-RETURN-COUNT             PIC 9(07) VALUE 0.
           05  WS-MISROUTE-COUNT           PIC 9(07) VALUE 0.
           05  WS-BREAK-COUNT              PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
           05  WS-TRAN-SEQ                 PIC 9(08) VALUE 0.
           05  WS-RETURN-TRACE-SEQ         PIC 9(07) VALUE 0.
           05  WS-CREDIT-AMT               PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-DEBIT-AMT                PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-RETURN-AMT               PIC S9(13)V99 COMP-3
                                           VALUE 0.

      *--- CURRENT ENTRY ----------------------------------------------*
       01  WS-ENTRY.
           05  WS-EN-TRAN-CODE             PIC X(02).
           05  WS-EN-ROUTING               PIC X(09).
           05  WS-EN-ACCOUNT               PIC X(17).
           05  WS-EN-AMOUNT                PIC 9(08)V99.
           05  WS-EN-INDIV-ID              PIC X(15).
           05  WS-EN-INDIV-NAME            PIC X(22).
           05  WS-EN-TRACE                 PIC X(15).
           05  WS-EN-RETURN-CODE           PIC X(03).
           05  WS-EN-DISPOSITION           PIC X(20).
           05  WS-EN-ACCT-FOUND            PIC X(01).
       01  WS-RDFI-NUM                     PIC 9(08).

      *--- CHECK DIGIT OF ODFI ----------------------------------------*
       01  WS-CHECK-DIGIT-WORK.
           05  WS-CD-ROUTING               PIC 9(08).
           05  WS-CD-ROUTING-R REDEFINES WS-CD-ROUTING.
               10  WS-CD-DIGIT             PIC 9(01) OCCURS 8.
           05  WS-CD-SUM                   PIC 9(04).
           05  WS-CD-RESULT                PIC 9(01).
           05  WS-CD-I                     PIC 9(02).
       01  WS-CD-WEIGHTS-V                 PIC X(08) VALUE '37137137'.
       01  WS-CD-WEIGHTS REDEFINES WS-CD-WEIGHTS-V.
           05  WS-CD-WEIGHT                PIC 9(01) OCCURS 8.

      *--- REPORT -----------------------------------------------------*
       01  WS-RPT-LINE                     PIC X(132).
       01  WS-ED-AMT                       PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ED-AMT2                      PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-ED-CNT                       PIC ZZZ,ZZ9.
       01  WS-ED-HASH                      PIC 9(10).
       01  WS-ED-HASH2                     PIC 9(10).
       01  WS-RECON-FLAG                   PIC X(04).
       01  WS-HASH-10                      PIC 9(10).

       COPY TRANCODE.
       COPY AVLPARM.
       COPY DATEPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM UNTIL WS-EOF
               READ ACH-IN-FILE INTO NACHA-RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2000-PROCESS-RECORD
               END-READ
           END-PERFORM
           IF WS-BATCH-OPEN
               MOVE 'BATCH CONTROL RECORD MISSING AT END OF FILE'
                 TO WS-RPT-LINE
               PERFORM 8900-WRITE-REPORT
               ADD 1 TO WS-BREAK-COUNT
           END-IF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT  ACH-IN-FILE ACCOUNT-FILE
           OPEN OUTPUT ACH-TRAN-FILE ACH-RTN-FILE REPORT-FILE
           OPEN EXTEND ALERT-FILE
           IF WS-IN-STATUS NOT = '00'
               MOVE 'ACHINBND' TO ABND-FILE-NAME
               MOVE WS-IN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-TRAN-STATUS NOT = '00' OR WS-RTN-STATUS NOT = '00'
              OR WS-RPT-STATUS NOT = '00'
               MOVE 'ACHTRAN' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           MOVE WS-SEQ-BASE TO WS-TRAN-SEQ
           MOVE SPACES TO WS-RPT-LINE
           STRING 'ACHIN    INBOUND ACH PROCESSING - RDFI '
                  WS-OUR-ABA '   BUSINESS DATE ' CTL-BUSINESS-DATE
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE ALL '-' TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

      *----------------------------------------------------------------*
       2000-PROCESS-RECORD.
           IF NACHA-REC-DATA(1:10) = '9999999999'
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-FT-RECORD-COUNT
           EVALUATE TRUE
               WHEN NACHA-FILE-HEADER
                   PERFORM 2100-FILE-HEADER
               WHEN NACHA-BATCH-HEADER
                   PERFORM 2200-BATCH-HEADER
               WHEN NACHA-ENTRY-DETAIL
                   PERFORM 2300-ENTRY-DETAIL
               WHEN NACHA-ADDENDA
                   ADD 1 TO WS-BT-ENTRY-ADDENDA
               WHEN NACHA-BATCH-CONTROL
                   PERFORM 2400-BATCH-CONTROL
               WHEN NACHA-FILE-CONTROL
                   PERFORM 2500-FILE-CONTROL
               WHEN OTHER
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE SPACES TO WS-RPT-LINE
                   STRING '*** UNKNOWN RECORD TYPE: '
                          NACHA-REC-DATA(1:40)
                          DELIMITED BY SIZE INTO WS-RPT-LINE
                   PERFORM 8900-WRITE-REPORT
           END-EVALUATE.

       2100-FILE-HEADER.
           MOVE NFH-CREATION-TIME TO WS-FH-CREATE-TIME
           MOVE NFH-IMMED-DEST    TO WS-FH-IMMED-DEST
           MOVE SPACES TO WS-RPT-LINE
           STRING 'FILE HEADER  DEST=' NFH-IMMED-DEST
                  ' ORIGIN=' NFH-IMMED-ORIGIN
                  ' CREATED=' NFH-CREATION-DATE ' ' NFH-CREATION-TIME
                  ' ID=' NFH-FILE-ID-MODIFIER
                  ' ' NFH-IMMED-ORIGIN-NAME
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           IF NFH-IMMED-DEST(2:9) NOT = WS-OUR-ABA
               ADD 1 TO WS-ERROR-COUNT
               MOVE '*** WARNING - IMMEDIATE DESTINATION IS NOT OUR ABA'
                 TO WS-RPT-LINE
               PERFORM 8900-WRITE-REPORT
           END-IF.

      *----------------------------------------------------------------*
       2200-BATCH-HEADER.
           IF WS-BATCH-OPEN
               ADD 1 TO WS-BREAK-COUNT
               MOVE '*** BATCH HEADER WITHOUT PRIOR BATCH CONTROL'
                 TO WS-RPT-LINE
               PERFORM 8900-WRITE-REPORT
           END-IF
           MOVE 'Y' TO WS-BH-OPEN
           ADD 1 TO WS-FT-BATCH-COUNT
           INITIALIZE WS-BATCH-TOTALS
           MOVE NBH-SERVICE-CLASS     TO WS-BH-SERVICE-CLASS
           MOVE NBH-COMPANY-NAME      TO WS-BH-COMPANY-NAME
           MOVE NBH-COMPANY-ID        TO WS-BH-COMPANY-ID
           MOVE NBH-SEC-CODE          TO WS-BH-SEC-CODE
           MOVE NBH-ENTRY-DESC        TO WS-BH-ENTRY-DESC
           MOVE NBH-DESCRIPTIVE-DATE  TO WS-BH-DESC-DATE
           MOVE NBH-EFFECTIVE-DATE    TO WS-BH-EFF-DATE
           MOVE NBH-ODFI-ID           TO WS-BH-ODFI
           MOVE NBH-BATCH-NUMBER      TO WS-BH-BATCH-NUMBER

      *    1998-11 MKL Y2K WINDOW - ALL ACH DATES ARE 20YY
           MOVE ZERO TO WS-BH-EFF-CCYYMMDD
           IF WS-BH-EFF-DATE IS NUMERIC
               COMPUTE WS-BH-EFF-CCYYMMDD =
                   20000000 + FUNCTION NUMVAL(WS-BH-EFF-DATE)
               INITIALIZE DATE-PARMS
               SET DTU-VALIDATE TO TRUE
               MOVE WS-BH-EFF-CCYYMMDD TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DTU-RESULT-YES
                   MOVE ZERO TO WS-BH-EFF-CCYYMMDD
               END-IF
           END-IF
      *    STALE OR MISSING EFFECTIVE DATE - SETTLE ON BUSINESS DATE
           IF WS-BH-EFF-CCYYMMDD < CTL-BUSINESS-DATE
               MOVE CTL-BUSINESS-DATE TO WS-BH-EFF-CCYYMMDD
           END-IF

      *    2016-09 SAME-DAY ENTRIES CARRY 'SD' + HHMM IN THE COMPANY
      *    DESCRIPTIVE DATE
           IF WS-BH-DESC-DATE(1:2) = 'SD'
               MOVE 'SDA' TO WS-BH-CHANNEL
           ELSE
               MOVE 'ACH' TO WS-BH-CHANNEL
           END-IF

           MOVE WS-BH-ODFI TO WS-CD-ROUTING
           PERFORM 8300-CHECK-DIGIT
           MOVE SPACES TO WS-BH-ODFI-ABA
           STRING WS-BH-ODFI WS-CD-RESULT DELIMITED BY SIZE
             INTO WS-BH-ODFI-ABA

           MOVE SPACES TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           STRING 'BATCH ' WS-BH-BATCH-NUMBER
                  '  ' WS-BH-COMPANY-NAME
                  '  ID ' WS-BH-COMPANY-ID
                  '  SEC ' WS-BH-SEC-CODE
                  '  ' WS-BH-ENTRY-DESC
                  '  SVC ' WS-BH-SERVICE-CLASS
                  '  EFF ' WS-BH-EFF-CCYYMMDD
                  '  CH ' WS-BH-CHANNEL
                  '  ODFI ' WS-BH-ODFI-ABA
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

      *----------------------------------------------------------------*
      * ENTRY DETAIL                                                   *
      *----------------------------------------------------------------*
       2300-ENTRY-DETAIL.
           ADD 1 TO WS-ENTRY-COUNT WS-BT-ENTRY-ADDENDA
           MOVE NED-TRAN-CODE       TO WS-EN-TRAN-CODE
           MOVE SPACES              TO WS-EN-ROUTING
           STRING NED-RDFI-ID NED-CHECK-DIGIT DELIMITED BY SIZE
             INTO WS-EN-ROUTING
           MOVE NED-DFI-ACCOUNT     TO WS-EN-ACCOUNT
           IF NED-AMOUNT IS NUMERIC
               MOVE NED-AMOUNT      TO WS-EN-AMOUNT
           ELSE
               MOVE ZERO            TO WS-EN-AMOUNT
           END-IF
           MOVE NED-INDIVIDUAL-ID   TO WS-EN-INDIV-ID
           MOVE NED-INDIVIDUAL-NAME TO WS-EN-INDIV-NAME
           MOVE NED-TRACE-NUMBER    TO WS-EN-TRACE
           MOVE SPACES              TO WS-EN-RETURN-CODE
                                       WS-EN-DISPOSITION

      *    CONTROL TOTALS ARE ACCUMULATED FOR EVERY ENTRY RECEIVED
           IF NED-RDFI-ID IS NUMERIC
               MOVE NED-RDFI-ID TO WS-RDFI-NUM
               ADD WS-RDFI-NUM TO WS-BT-HASH
           END-IF
           EVALUATE WS-EN-TRAN-CODE(2:1)
               WHEN '6' WHEN '7' WHEN '8' WHEN '9'
                   ADD WS-EN-AMOUNT TO WS-BT-DEBIT
               WHEN OTHER
                   ADD WS-EN-AMOUNT TO WS-BT-CREDIT
           END-EVALUATE

           IF NED-RDFI-ID NOT = WS-OUR-DFI
               ADD 1 TO WS-MISROUTE-COUNT
               MOVE 'MISROUTED - TO OPS' TO WS-EN-DISPOSITION
               PERFORM 8800-REPORT-ENTRY
               EXIT PARAGRAPH
           END-IF

           PERFORM 2310-LOOKUP-ACCOUNT
           IF WS-EN-RETURN-CODE = SPACES
               PERFORM 2320-VALIDATE-ENTRY
           END-IF

           EVALUATE TRUE
               WHEN WS-EN-RETURN-CODE NOT = SPACES
                   PERFORM 8000-WRITE-RETURN
               WHEN AVL-IS-PRENOTE
                   ADD 1 TO WS-PRENOTE-COUNT
                   MOVE 'PRENOTE ACCEPTED' TO WS-EN-DISPOSITION
               WHEN OTHER
                   PERFORM 2330-WRITE-TRANSACTION
           END-EVALUATE
           PERFORM 8800-REPORT-ENTRY.

       2310-LOOKUP-ACCOUNT.
           MOVE 'N' TO WS-EN-ACCT-FOUND
           IF WS-EN-ACCOUNT(13:5) NOT = SPACES
               MOVE 'R03' TO WS-EN-RETURN-CODE
               MOVE 'RETURN R03 NO ACCT' TO WS-EN-DISPOSITION
               EXIT PARAGRAPH
           END-IF
           MOVE WS-EN-ACCOUNT(1:12) TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   MOVE 'R03' TO WS-EN-RETURN-CODE
                   MOVE 'RETURN R03 NO ACCT' TO WS-EN-DISPOSITION
               NOT INVALID KEY
                   MOVE 'Y' TO WS-EN-ACCT-FOUND
           END-READ
           IF WS-EN-ACCT-FOUND = 'Y'
               EVALUATE TRUE
                   WHEN ACCT-CLOSED
                   WHEN ACCT-ESCHEATED
                       MOVE 'R02' TO WS-EN-RETURN-CODE
                       MOVE 'RETURN R02 CLOSED' TO WS-EN-DISPOSITION
               END-EVALUATE
           END-IF.

       2320-VALIDATE-ENTRY.
           INITIALIZE ACH-VALID-PARMS
           MOVE WS-EN-ROUTING   TO AVL-ROUTING
           MOVE WS-EN-TRAN-CODE TO AVL-TRAN-CODE
           MOVE WS-EN-AMOUNT    TO AVL-AMOUNT
           MOVE WS-BH-SEC-CODE  TO AVL-SEC-CODE
           IF ACCT-OWN-CORPORATE
               MOVE 'C' TO AVL-ACCT-TYPE
           ELSE
               MOVE 'I' TO AVL-ACCT-TYPE
           END-IF
      *    RDFI SIDE - ODFI WARRANTS WEB AUTHORIZATION (NACHA 2.5.17)
           MOVE 'Y' TO AVL-AUTH-FLAG
           CALL 'ACHVALID' USING ACH-VALID-PARMS
           IF AVL-REJECT
               MOVE AVL-RETURN-REASON TO WS-EN-RETURN-CODE
               MOVE SPACES TO WS-EN-DISPOSITION
               STRING 'RETURN ' AVL-RETURN-REASON ' ' AVL-REASON-CODE
                      DELIMITED BY SIZE INTO WS-EN-DISPOSITION
           END-IF.

       2330-WRITE-TRANSACTION.
           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-TRAN-SEQ
           MOVE CTL-BUSINESS-DATE   TO TRAN-ID-DATE
           MOVE WS-TRAN-SEQ         TO TRAN-ID-SEQ
           MOVE ACCT-NUMBER         TO TRAN-ACCT-NUMBER
           MOVE WS-BH-EFF-CCYYMMDD  TO TRAN-EFFECTIVE-DATE
           MOVE ZERO                TO TRAN-POST-DATE
           COMPUTE TRAN-TIME = WS-FH-CREATE-TIME * 100
           IF AVL-DR-CR = 'C'
               MOVE 'DACH' TO TRAN-CODE
               ADD 1 TO WS-CREDIT-COUNT
               ADD WS-EN-AMOUNT TO WS-CREDIT-AMT
           ELSE
               MOVE 'WACH' TO TRAN-CODE
               ADD 1 TO WS-DEBIT-COUNT
               ADD WS-EN-AMOUNT TO WS-DEBIT-AMT
           END-IF
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END
                   MOVE AVL-DR-CR TO TRAN-DR-CR
               WHEN TC-CODE(TC-IDX) = TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)     TO TRAN-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX) TO TRAN-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX) TO TRAN-REGD-FLAG
           END-SEARCH
           MOVE WS-EN-AMOUNT        TO TRAN-AMOUNT
           MOVE WS-BH-CHANNEL       TO TRAN-CHANNEL
           MOVE SPACES              TO TRAN-DESCRIPTION
           MOVE WS-BH-COMPANY-NAME  TO TRAN-DESCRIPTION(1:16)
           MOVE WS-BH-ENTRY-DESC    TO TRAN-DESCRIPTION(18:10)
           MOVE WS-BH-SEC-CODE      TO TRAN-DESCRIPTION(29:3)
           MOVE WS-EN-TRAN-CODE     TO TRAN-DESCRIPTION(33:2)
           MOVE WS-EN-TRACE         TO TRAN-REFERENCE
           MOVE WS-BH-COMPANY-ID    TO TRAN-COUNTER-ACCT
           MOVE WS-BH-COMPANY-NAME  TO TRAN-COUNTERPARTY-NAME
           MOVE WS-BH-ODFI-ABA      TO TRAN-COUNTERPARTY-ROUTING
           MOVE 'US'                TO TRAN-COUNTERPARTY-COUNTRY
           SET TRAN-PENDING         TO TRUE
           MOVE 'ACHIN'             TO TRAN-USER-ID
           MOVE ACCT-COST-CENTER    TO TRAN-COST-CENTER
           MOVE 'ACHIN'             TO TRAN-SOURCE-PGM
           WRITE TRANSACTION-RECORD
           IF WS-TRAN-STATUS NOT = '00'
               MOVE 'ACHTRAN' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               MOVE '2330-WRITE-TRANSACTION' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON ACHTRAN' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           MOVE SPACES TO WS-EN-DISPOSITION
           STRING TRAN-CODE ' ' TRAN-CHANNEL ' ' TRAN-ID-SEQ
                  DELIMITED BY SIZE INTO WS-EN-DISPOSITION.

      *----------------------------------------------------------------*
      * BATCH CONTROL - RECONCILE COUNT, HASH, DEBITS, CREDITS         *
      *----------------------------------------------------------------*
       2400-BATCH-CONTROL.
           MOVE 'N' TO WS-BH-OPEN
           COMPUTE WS-HASH-10 =
               FUNCTION MOD(WS-BT-HASH, WS-HASH-MODULUS)
           MOVE 'OK  ' TO WS-RECON-FLAG
           IF NBC-ENTRY-ADDENDA-CNT NOT = WS-BT-ENTRY-ADDENDA
              OR NBC-ENTRY-HASH     NOT = WS-HASH-10
              OR NBC-TOTAL-DEBIT    NOT = WS-BT-DEBIT
              OR NBC-TOTAL-CREDIT   NOT = WS-BT-CREDIT
              OR NBC-BATCH-NUMBER   NOT = WS-BH-BATCH-NUMBER
               MOVE 'BRK ' TO WS-RECON-FLAG
               ADD 1 TO WS-BREAK-COUNT
               PERFORM 8500-WRITE-RECN-ALERT
           END-IF
           ADD WS-BT-ENTRY-ADDENDA TO WS-FT-ENTRY-ADDENDA
           ADD WS-BT-HASH          TO WS-FT-HASH
           ADD WS-BT-DEBIT         TO WS-FT-DEBIT
           ADD WS-BT-CREDIT        TO WS-FT-CREDIT

           MOVE SPACES TO WS-RPT-LINE
           MOVE WS-BT-ENTRY-ADDENDA TO WS-ED-CNT
           MOVE WS-BT-DEBIT  TO WS-ED-AMT
           MOVE WS-BT-CREDIT TO WS-ED-AMT2
           STRING '  BATCH CONTROL ' WS-RECON-FLAG
                  ' COUNT ' WS-ED-CNT
                  ' HASH ' WS-HASH-10 '/' NBC-ENTRY-HASH
                  ' DR ' WS-ED-AMT ' CR ' WS-ED-AMT2
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

       2500-FILE-CONTROL.
           COMPUTE WS-HASH-10 =
               FUNCTION MOD(WS-FT-HASH, WS-HASH-MODULUS)
           MOVE 'OK  ' TO WS-RECON-FLAG
           IF NFC-BATCH-COUNT        NOT = WS-FT-BATCH-COUNT
              OR NFC-ENTRY-ADDENDA-CNT NOT = WS-FT-ENTRY-ADDENDA
              OR NFC-ENTRY-HASH        NOT = WS-HASH-10
              OR NFC-TOTAL-DEBIT       NOT = WS-FT-DEBIT
              OR NFC-TOTAL-CREDIT      NOT = WS-FT-CREDIT
               MOVE 'BRK ' TO WS-RECON-FLAG
               ADD 1 TO WS-BREAK-COUNT
               MOVE ZERO TO WS-BH-BATCH-NUMBER
               MOVE 'FILE' TO WS-BH-COMPANY-ID
               MOVE WS-FT-CREDIT TO WS-BT-CREDIT
               PERFORM 8500-WRITE-RECN-ALERT
           END-IF
           MOVE SPACES TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-FT-DEBIT  TO WS-ED-AMT
           MOVE WS-FT-CREDIT TO WS-ED-AMT2
           MOVE WS-FT-ENTRY-ADDENDA TO WS-ED-CNT
           STRING 'FILE CONTROL ' WS-RECON-FLAG
                  ' BATCHES ' NFC-BATCH-COUNT '/' WS-FT-BATCH-COUNT
                  ' COUNT ' WS-ED-CNT
                  ' HASH ' WS-HASH-10 '/' NFC-ENTRY-HASH
                  ' DR ' WS-ED-AMT ' CR ' WS-ED-AMT2
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

      *----------------------------------------------------------------*
      * RETURN ENTRY (6) + RETURN ADDENDA (7, TYPE 99) TO ACHRTN       *
      *----------------------------------------------------------------*
       8000-WRITE-RETURN.
           ADD 1 TO WS-RETURN-COUNT WS-RETURN-TRACE-SEQ
           ADD WS-EN-AMOUNT TO WS-RETURN-AMT
           MOVE SPACES TO NACHA-REC-DATA
           MOVE '6' TO NED-REC-TYPE
      *    RETURN TRAN CODE = ORIGINAL - 1 (22->21, 27->26, 32->31 ...)
           EVALUATE WS-EN-TRAN-CODE
               WHEN '22' MOVE '21' TO NED-TRAN-CODE
               WHEN '23' MOVE '21' TO NED-TRAN-CODE
               WHEN '27' MOVE '26' TO NED-TRAN-CODE
               WHEN '28' MOVE '26' TO NED-TRAN-CODE
               WHEN '32' MOVE '31' TO NED-TRAN-CODE
               WHEN '33' MOVE '31' TO NED-TRAN-CODE
               WHEN '37' MOVE '36' TO NED-TRAN-CODE
               WHEN '38' MOVE '36' TO NED-TRAN-CODE
               WHEN OTHER MOVE WS-EN-TRAN-CODE TO NED-TRAN-CODE
           END-EVALUATE
           MOVE WS-BH-ODFI-ABA(1:8)  TO NED-RDFI-ID
           MOVE WS-BH-ODFI-ABA(9:1)  TO NED-CHECK-DIGIT
           MOVE WS-EN-ACCOUNT        TO NED-DFI-ACCOUNT
           MOVE WS-EN-AMOUNT         TO NED-AMOUNT
           MOVE WS-EN-INDIV-ID       TO NED-INDIVIDUAL-ID
           MOVE WS-EN-INDIV-NAME     TO NED-INDIVIDUAL-NAME
           MOVE '1'                  TO NED-ADDENDA-IND
           MOVE WS-OUR-DFI           TO NED-TRACE-ODFI
           MOVE WS-RETURN-TRACE-SEQ  TO NED-TRACE-SEQ
           WRITE ACH-RTN-REC FROM NACHA-REC-DATA
           PERFORM 8010-CHECK-RTN-WRITE

           MOVE SPACES TO NACHA-REC-DATA
           MOVE '7'                  TO NAR-REC-TYPE
           MOVE '99'                 TO NAR-ADDENDA-TYPE
           MOVE WS-EN-RETURN-CODE    TO NAR-RETURN-REASON
           MOVE WS-EN-TRACE          TO NAR-ORIG-TRACE
           MOVE WS-OUR-DFI           TO NAR-ORIG-RDFI
           MOVE WS-BH-SEC-CODE       TO NAR-INFO-SEC
           MOVE WS-BH-COMPANY-ID     TO NAR-INFO-CO-ID
           MOVE WS-BH-COMPANY-NAME   TO NAR-INFO-CO-NAME
           MOVE WS-BH-EFF-DATE       TO NAR-INFO-EFF-DATE
           MOVE SPACES               TO NAR-TRACE-NUMBER
           STRING WS-OUR-DFI WS-RETURN-TRACE-SEQ DELIMITED BY SIZE
             INTO NAR-TRACE-NUMBER
           WRITE ACH-RTN-REC FROM NACHA-REC-DATA
           PERFORM 8010-CHECK-RTN-WRITE.

       8010-CHECK-RTN-WRITE.
           IF WS-RTN-STATUS NOT = '00'
               MOVE 'ACHRTN' TO ABND-FILE-NAME
               MOVE WS-RTN-STATUS TO ABND-FILE-STATUS
               MOVE '8000-WRITE-RETURN' TO ABND-PARAGRAPH
               MOVE 'WRITE FAILED ON ACHRTN' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

      *    ABA CHECK DIGIT OF WS-CD-ROUTING (3-7-1 WEIGHTS)
       8300-CHECK-DIGIT.
           MOVE ZERO TO WS-CD-SUM
           PERFORM VARYING WS-CD-I FROM 1 BY 1 UNTIL WS-CD-I > 8
               COMPUTE WS-CD-SUM = WS-CD-SUM
                   + WS-CD-DIGIT(WS-CD-I) * WS-CD-WEIGHT(WS-CD-I)
           END-PERFORM
           COMPUTE WS-CD-RESULT =
               FUNCTION MOD(10 - FUNCTION MOD(WS-CD-SUM, 10), 10).

       8500-WRITE-RECN-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           ACCEPT ALRT-TIME FROM TIME
           MOVE 'RECN'            TO ALRT-TYPE
           SET ALRT-HIGH          TO TRUE
           MOVE WS-BT-CREDIT      TO ALRT-AMOUNT
           MOVE 'ACHIN001'        TO ALRT-RULE-ID
           STRING 'ACH INBOUND CONTROL BREAK BATCH '
                  WS-BH-BATCH-NUMBER ' CO ' WS-BH-COMPANY-ID
                  DELIMITED BY SIZE INTO ALRT-DESCRIPTION
           SET ALRT-OPEN          TO TRUE
           MOVE 'ACHIN'           TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD.

       8800-REPORT-ENTRY.
           MOVE SPACES TO WS-RPT-LINE
           MOVE WS-EN-AMOUNT TO WS-ED-AMT
           STRING '    ' WS-EN-TRAN-CODE ' ' WS-EN-ACCOUNT
                  ' ' WS-ED-AMT ' ' WS-EN-INDIV-NAME
                  ' ' WS-EN-TRACE ' ' WS-EN-DISPOSITION
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT.

       8900-WRITE-REPORT.
           WRITE REPORT-REC FROM WS-RPT-LINE
           MOVE SPACES TO WS-RPT-LINE.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE ALL '-' TO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-ENTRY-COUNT TO WS-ED-CNT
           STRING 'ENTRIES RECEIVED ' WS-ED-CNT DELIMITED BY SIZE
             INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-CREDIT-COUNT TO WS-ED-CNT
           MOVE WS-CREDIT-AMT   TO WS-ED-AMT
           STRING 'CREDITS TO ACHTRAN (DACH) ' WS-ED-CNT '  ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-DEBIT-COUNT TO WS-ED-CNT
           MOVE WS-DEBIT-AMT   TO WS-ED-AMT
           STRING 'DEBITS  TO ACHTRAN (WACH) ' WS-ED-CNT '  ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-RETURN-COUNT TO WS-ED-CNT
           MOVE WS-RETURN-AMT   TO WS-ED-AMT
           STRING 'RETURNED (STAGED ACHRTN)  ' WS-ED-CNT '  ' WS-ED-AMT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-PRENOTE-COUNT TO WS-ED-CNT
           STRING 'PRENOTES ACCEPTED         ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-MISROUTE-COUNT TO WS-ED-CNT
           STRING 'MISROUTED                 ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT
           MOVE WS-BREAK-COUNT TO WS-ED-CNT
           STRING 'CONTROL BREAKS            ' WS-ED-CNT
                  DELIMITED BY SIZE INTO WS-RPT-LINE
           PERFORM 8900-WRITE-REPORT

           CLOSE ACH-IN-FILE ACCOUNT-FILE ACH-TRAN-FILE ACH-RTN-FILE
                 REPORT-FILE ALERT-FILE
           DISPLAY 'ACHIN   ENTRIES=' WS-ENTRY-COUNT
                   ' CREDITS=' WS-CREDIT-COUNT
                   ' DEBITS=' WS-DEBIT-COUNT
                   ' RETURNS=' WS-RETURN-COUNT
                   ' PRENOTES=' WS-PRENOTE-COUNT
                   ' BREAKS=' WS-BREAK-COUNT
           IF WS-BREAK-COUNT > 0 OR WS-ERROR-COUNT > 0
              OR WS-MISROUTE-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'ACHIN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
