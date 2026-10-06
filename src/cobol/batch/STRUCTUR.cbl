      *================================================================*
      * PROGRAM  : STRUCTUR                                            *
      * TYPE     : BATCH - EOD STEP 08 (COMPLIANCE)                    *
      * PURPOSE  : STRUCTURING DETECTION. COLLECTS CASH ITEMS FOR THE  *
      *            LAST 10 BUSINESS DAYS (TRANHIST) PLUS TODAY         *
      *            (TRANPOST), GROUPS THEM BY PRIMARY ACCOUNT OWNER,   *
      *            CALLS STRCRULE PER CUSTOMER AND RAISES A STRC ALERT *
      *            FOR EVERY CUSTOMER WITH ONE OR MORE PATTERNS.       *
      * INPUT    : TRANHIST  POSTED HISTORY (LOOKBACK)                 *
      *            TRANPOST  POSTED TODAY                              *
      *            ACCTMST   ACCOUNT MASTER (RANDOM)                   *
      *            CUSTMST   CUSTOMER MASTER (RANDOM)                  *
      * OUTPUT   : ALERTS    STRC ALERTS (EXTEND)                      *
      *            STRCRPT   STRUCTURING REPORT                        *
      * RETURN   : 00 OK, 04 ITEMS SKIPPED, 16 FILE ERROR              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-02-17 BHS  ORIGINAL                                       *
      * 2006-11-09 BHS  CHG0233 LOOKBACK 5 TO 10 BUSINESS DAYS         *
      * 2009-06-08 DPS  BRANCH (COST CENTRE) PASSED FOR S3             *
      * 2016-02-08 DPS  INC27765 SKIP HISTORY DATED TODAY (RERUN AFTER *
      *                 EODCTL END DOUBLE-COUNTED TODAY'S CASH)        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRUCTUR.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANHIST-FILE ASSIGN TO "TRANHIST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HIST-STATUS.
           SELECT TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "STRCRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANHIST-FILE.
       01  TRANHIST-REC                    PIC X(281).
       FD  TRANPOST-FILE.
       01  TRANPOST-REC                    PIC X(281).
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'STRUCTUR'.
       01  WS-FILE-STATUSES.
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.

      *    LOOKBACK: TODAY PLUS THE 9 PRIOR BUSINESS DAYS
       01  WS-LOOKBACK-BUS-DAYS            PIC S9(05) VALUE 10.
       01  WS-LOOKBACK-START               PIC 9(08).

       01  WS-COUNTERS.
           05  WS-HIST-READ                PIC 9(07) VALUE 0.
           05  WS-POST-READ                PIC 9(07) VALUE 0.
           05  WS-CASH-COUNT               PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-CUST-EVAL-COUNT          PIC 9(05) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(05) VALUE 0.
           05  WS-RETURN-CODE              PIC 9(02) VALUE 0.

       01  WS-MAX-CUST                     PIC 9(03) VALUE 300.
       01  WS-MAX-ITEMS                    PIC 9(03) VALUE 200.
       01  WS-CUST-TABLE.
           05  WS-CUST-COUNT               PIC 9(03) COMP VALUE 0.
           05  WS-CUST-ENTRY OCCURS 300 TIMES.
               10  WS-CT-CUST-ID           PIC X(10).
               10  WS-CT-ITEM-COUNT        PIC 9(03) COMP.
               10  WS-CT-ITEM OCCURS 200 TIMES.
                   15  WS-CT-DATE          PIC 9(08).
                   15  WS-CT-DR-CR         PIC X(01).
                   15  WS-CT-AMOUNT        PIC S9(13)V99 COMP-3.
                   15  WS-CT-BRANCH        PIC X(05).
                   15  WS-CT-ACCT          PIC X(12).

       01  WS-WORK-FIELDS.
           05  WS-CX                       PIC 9(03) COMP.
           05  WS-IX                       PIC 9(03) COMP.
           05  WS-FOUND                    PIC X(01).
           05  WS-OWNER-ID                 PIC X(10).
           05  WS-ITEM-DATE                PIC 9(08).
           05  WS-TIME                     PIC 9(08).
           05  WS-CUST-NAME                PIC X(40).

       01  WS-RPT-HEADER-1.
           05  FILLER PIC X(09) VALUE 'STRUCTUR '.
           05  FILLER PIC X(49) VALUE
               'STRUCTURING DETECTION - CASH ACTIVITY REVIEW'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  WS-H1-DATE                  PIC 9(08).
           05  FILLER PIC X(17) VALUE '  LOOKBACK FROM '.
           05  WS-H1-START                 PIC 9(08).
       01  WS-RPT-HEADER-2.
           05  FILLER PIC X(40) VALUE 'CUST-ID    NAME'.
           05  FILLER PIC X(40) VALUE
               'ITEMS PATTERNS  SCORE SEV BAND-DAYS ROLL'.
           05  FILLER PIC X(52) VALUE
               'ING-5D-IN   SUSPECT-AMT'.
       01  WS-RPT-DETAIL.
           05  WS-D-CUST-ID                PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NAME                   PIC X(28).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-ITEMS                  PIC ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-PATTERNS               PIC X(08).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-SCORE                  PIC ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-SEV                    PIC X(01).
           05  FILLER                      PIC X(06) VALUE SPACES.
           05  WS-D-BAND-DAYS              PIC Z9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-ROLLING                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-SUSPECT                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(31) VALUE SPACES.
       01  WS-RPT-COUNT.
           05  WS-C-LABEL                  PIC X(40).
           05  WS-C-VALUE                  PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.

       COPY TRANREC.
       COPY BANKCTL.
       COPY STRPARM.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-READ-HISTORY
           PERFORM 2500-READ-TODAY
           PERFORM 3000-EVALUATE-CUSTOMERS
           PERFORM 9000-TERMINATE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           COMPUTE DTU-NUMBER = 1 - WS-LOOKBACK-BUS-DAYS
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-LOOKBACK-START

           OPEN INPUT TRANHIST-FILE
           IF WS-HIST-STATUS NOT = '00'
               MOVE 'TRANHIST' TO ABND-FILE-NAME
               MOVE WS-HIST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT TRANPOST-FILE
           IF WS-TRAN-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'STRCRPT' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           MOVE CTL-BUSINESS-DATE TO WS-H1-DATE
           MOVE WS-LOOKBACK-START TO WS-H1-START
           WRITE REPORT-LINE FROM WS-RPT-HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-RPT-HEADER-2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
      * HISTORY: POSTED CASH ITEMS IN [LOOKBACK START, TODAY)          *
      *----------------------------------------------------------------*
       2000-READ-HISTORY.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANHIST-FILE INTO TRANSACTION-RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-HIST-READ
                       IF TRAN-IS-CASH AND TRAN-POSTED
                          AND TRAN-POST-DATE >= WS-LOOKBACK-START
                          AND TRAN-POST-DATE <  CTL-BUSINESS-DATE
                           MOVE TRAN-POST-DATE TO WS-ITEM-DATE
                           PERFORM 2100-ADD-CASH-ITEM
                       END-IF
               END-READ
               IF WS-HIST-STATUS NOT = '00' AND NOT = '10'
                   MOVE 'TRANHIST' TO ABND-FILE-NAME
                   MOVE WS-HIST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
           END-PERFORM.

       2500-READ-TODAY.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE INTO TRANSACTION-RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-POST-READ
                       IF TRAN-IS-CASH AND TRAN-POSTED
                          AND TRAN-POST-DATE = CTL-BUSINESS-DATE
                           MOVE CTL-BUSINESS-DATE TO WS-ITEM-DATE
                           PERFORM 2100-ADD-CASH-ITEM
                       END-IF
               END-READ
               IF WS-TRAN-STATUS NOT = '00' AND NOT = '10'
                   MOVE 'TRANPOST' TO ABND-FILE-NAME
                   MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
           END-PERFORM.

       2100-ADD-CASH-ITEM.
           ADD 1 TO WS-CASH-COUNT
           MOVE TRAN-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY 'STRUCTUR ACCT NOT FOUND ' TRAN-ACCT-NUMBER
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ
           MOVE ACCT-PRIMARY-CUST-ID TO WS-OWNER-ID
           PERFORM 2200-FIND-CUSTOMER
           IF WS-FOUND = 'N'
               EXIT PARAGRAPH
           END-IF
           IF WS-CT-ITEM-COUNT(WS-CX) >= WS-MAX-ITEMS
               ADD 1 TO WS-SKIP-COUNT
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-CT-ITEM-COUNT(WS-CX)
           MOVE WS-CT-ITEM-COUNT(WS-CX) TO WS-IX
           MOVE WS-ITEM-DATE     TO WS-CT-DATE(WS-CX, WS-IX)
           MOVE TRAN-DR-CR       TO WS-CT-DR-CR(WS-CX, WS-IX)
           MOVE TRAN-AMOUNT      TO WS-CT-AMOUNT(WS-CX, WS-IX)
           MOVE TRAN-ACCT-NUMBER TO WS-CT-ACCT(WS-CX, WS-IX)
           IF TRAN-COST-CENTER = SPACES
               MOVE TRAN-CHANNEL TO WS-CT-BRANCH(WS-CX, WS-IX)
           ELSE
               MOVE TRAN-COST-CENTER TO WS-CT-BRANCH(WS-CX, WS-IX)
           END-IF.

       2200-FIND-CUSTOMER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CUST-COUNT OR WS-FOUND = 'Y'
               IF WS-CT-CUST-ID(WS-CX) = WS-OWNER-ID
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'Y'
               SUBTRACT 1 FROM WS-CX
           ELSE
               IF WS-CUST-COUNT >= WS-MAX-CUST
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
               ELSE
                   ADD 1 TO WS-CUST-COUNT
                   MOVE WS-CUST-COUNT TO WS-CX
                   MOVE WS-OWNER-ID TO WS-CT-CUST-ID(WS-CX)
                   MOVE 0 TO WS-CT-ITEM-COUNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-EVALUATE-CUSTOMERS.
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CUST-COUNT
               PERFORM 3100-EVALUATE-ONE
           END-PERFORM.

       3100-EVALUATE-ONE.
           INITIALIZE STRC-RULE-PARMS
           MOVE CTL-BUSINESS-DATE    TO STP-BUSINESS-DATE
           MOVE WS-CT-CUST-ID(WS-CX) TO STP-CUST-ID
           MOVE WS-CT-ITEM-COUNT(WS-CX) TO STP-ITEM-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CT-ITEM-COUNT(WS-CX)
               MOVE WS-CT-DATE(WS-CX, WS-IX)   TO STP-ITEM-DATE(WS-IX)
               MOVE WS-CT-DR-CR(WS-CX, WS-IX)  TO STP-ITEM-DR-CR(WS-IX)
               MOVE WS-CT-AMOUNT(WS-CX, WS-IX) TO STP-ITEM-AMOUNT(WS-IX)
               MOVE WS-CT-BRANCH(WS-CX, WS-IX) TO STP-ITEM-BRANCH(WS-IX)
               MOVE WS-CT-ACCT(WS-CX, WS-IX)   TO STP-ITEM-ACCT(WS-IX)
           END-PERFORM
           CALL 'STRCRULE' USING STRC-RULE-PARMS
           ADD 1 TO WS-CUST-EVAL-COUNT
           EVALUATE STP-RETURN-CODE
               WHEN 00
                   CONTINUE
               WHEN 04
                   PERFORM 3200-REPORT-PATTERN
               WHEN OTHER
                   DISPLAY 'STRUCTUR STRCRULE RC ' STP-RETURN-CODE
                           ' CUST ' STP-CUST-ID ' ' STP-DESCRIPTION
                   MOVE 04 TO WS-RETURN-CODE
           END-EVALUATE.

       3200-REPORT-PATTERN.
           MOVE STP-CUST-ID TO CUST-ID
           MOVE SPACES TO WS-CUST-NAME
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '** NOT ON CUSTMST **' TO WS-CUST-NAME
               NOT INVALID KEY
                   IF CUST-INDIVIDUAL
                       STRING CUST-FIRST-NAME DELIMITED BY '  '
                              ' ' DELIMITED BY SIZE
                              CUST-LAST-NAME DELIMITED BY '  '
                         INTO WS-CUST-NAME
                       END-STRING
                   ELSE
                       MOVE CUST-LAST-NAME TO WS-CUST-NAME
                   END-IF
           END-READ
      *    ALERT
           INITIALIZE ALERT-RECORD
           ACCEPT WS-TIME FROM TIME
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE WS-TIME(1:6)      TO ALRT-TIME
           MOVE 'STRC'            TO ALRT-TYPE
           MOVE STP-SEVERITY      TO ALRT-SEVERITY
           MOVE STP-CUST-ID       TO ALRT-CUST-ID
           MOVE WS-CT-ACCT(WS-CX, WS-CT-ITEM-COUNT(WS-CX))
                                  TO ALRT-ACCT-NUMBER
           MOVE STP-SUSPECT-AMOUNT TO ALRT-AMOUNT
           STRING 'STRC' STP-PATTERN-CODES DELIMITED BY SPACE
             INTO ALRT-RULE-ID
           END-STRING
           MOVE STP-DESCRIPTION   TO ALRT-DESCRIPTION
           SET ALRT-OPEN TO TRUE
           MOVE WS-PROGRAM-NAME   TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ALERT-COUNT
      *    REPORT
           MOVE STP-CUST-ID          TO WS-D-CUST-ID
           MOVE WS-CUST-NAME         TO WS-D-NAME
           MOVE STP-ITEM-COUNT       TO WS-D-ITEMS
           MOVE STP-PATTERN-CODES    TO WS-D-PATTERNS
           MOVE STP-SCORE            TO WS-D-SCORE
           MOVE STP-SEVERITY         TO WS-D-SEV
           MOVE STP-BAND-DAYS        TO WS-D-BAND-DAYS
           MOVE STP-ROLLING-CASH-IN  TO WS-D-ROLLING
           MOVE STP-SUSPECT-AMOUNT   TO WS-D-SUSPECT
           WRITE REPORT-LINE FROM WS-RPT-DETAIL.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TRANHIST RECORDS READ' TO WS-C-LABEL
           MOVE WS-HIST-READ TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'TRANPOST RECORDS READ' TO WS-C-LABEL
           MOVE WS-POST-READ TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CASH ITEMS IN WINDOW' TO WS-C-LABEL
           MOVE WS-CASH-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CUSTOMERS EVALUATED' TO WS-C-LABEL
           MOVE WS-CUST-EVAL-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'STRC ALERTS RAISED' TO WS-C-LABEL
           MOVE WS-ALERT-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'ITEMS SKIPPED' TO WS-C-LABEL
           MOVE WS-SKIP-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           CLOSE TRANHIST-FILE TRANPOST-FILE ACCOUNT-FILE
                 CUSTOMER-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'STRUCTUR HIST=' WS-HIST-READ
                   ' POST=' WS-POST-READ
                   ' CASH=' WS-CASH-COUNT
                   ' CUSTOMERS=' WS-CUST-EVAL-COUNT
                   ' ALERTS=' WS-ALERT-COUNT
                   ' RC=' WS-RETURN-CODE.

       9900-ABEND.
           MOVE WS-PROGRAM-NAME TO ABND-PROGRAM
           MOVE 'FILE I/O' TO ABND-PARAGRAPH
           MOVE 'UNEXPECTED FILE STATUS' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
