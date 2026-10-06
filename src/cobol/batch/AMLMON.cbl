      *================================================================*
      * PROGRAM  : AMLMON                                              *
      * TYPE     : BATCH - EOD STEP 08 (COMPLIANCE)                    *
      * PURPOSE  : AML TRANSACTION MONITORING. BUILDS A 30-DAY ACTIVITY*
      *            PROFILE PER CUSTOMER FROM TRANHIST PLUS TODAY'S     *
      *            TRANPOST, ADDS THE CUSTOMER'S CDD PROFILE FROM      *
      *            CUSTMST AND ACCOUNT STATUS FROM ACCTMST, CALLS THE  *
      *            AMLRULES SCENARIO ENGINE AND WRITES ONE AMLn ALERT  *
      *            PER SCENARIO HIT. ALSO RUNS THE AML8 CASH-BELOW-    *
      *            THRESHOLD PRE-SCREEN FOR THE SAR TRIAGE QUEUE.      *
      *            ACTIVITY ON A JOINT ACCOUNT IS PROFILED UNDER BOTH  *
      *            THE PRIMARY AND THE JOINT OWNER.                    *
      * INPUT    : TRANHIST  POSTED HISTORY (30 CALENDAR DAYS)         *
      *            TRANPOST  POSTED TODAY                              *
      *            ACCTMST   ACCOUNT MASTER (RANDOM)                   *
      *            CUSTMST   CUSTOMER MASTER (RANDOM)                  *
      *            ALERTS    DORM0002 REACTIVATION ALERTS (READ FIRST) *
      * OUTPUT   : ALERTS    AML1-AML8 ALERTS (EXTEND)                 *
      *            AMLRPT    MONITORING REPORT                         *
      * RETURN   : 00 OK, 04 ITEMS SKIPPED / TABLE FULL, 16 FILE ERROR *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2004-08-02 BHS  ORIGINAL                                       *
      * 2005-03-14 BHS  AML8 CASH NEAR THRESHOLD PRE-SCREEN FOR SAR    *
      *                 TRIAGE (BSA OFFICER REQUEST)                   *
      * 2010-05-03 DPS  CHG0655 JOINT OWNER PROFILED AS WELL AS        *
      *                 PRIMARY (FFIEC MANUAL - CUSTOMER LEVEL VIEW)   *
      * 2011-01-18 DPS  ACCOUNT TABLE FOR AML5 DORMANT REACTIVATION    *
      * 2012-07-09 DPS  INC21180 POSTTRAN CLEARS THE DORMANT DATE WHEN *
      *                 IT REACTIVATES AN ACCOUNT - AML5 NOW TAKES THE *
      *                 REACTIVATION FROM THE DORM0002 ALERTS          *
      * 2016-02-08 DPS  INC27765 SKIP HISTORY DATED TODAY              *
      * 2018-05-11 AKW  CDD RULE - INCOME / SEGMENT PASSED TO ENGINE   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMLMON.
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
           SELECT OPTIONAL ALERT-IN-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRI-STATUS.
           SELECT REPORT-FILE ASSIGN TO "AMLRPT"
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
       FD  ALERT-IN-FILE.
       01  ALERT-IN-REC                    PIC X(166).
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'AMLMON'.
       01  WS-FILE-STATUSES.
           05  WS-HIST-STATUS              PIC X(02).
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-ALRI-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.

       01  WS-LOOKBACK-DAYS                PIC S9(05) VALUE 30.
       01  WS-LOOKBACK-START               PIC 9(08).

      *----------------------------------------------------------------*
      * AML8 - CASH JUST BELOW THE CTR THRESHOLD (SAR TRIAGE)          *
      *----------------------------------------------------------------*
       01  WS-AML8-PARAMETERS.
           05  WS-AML8-BAND-LOW            PIC S9(13)V99 COMP-3
                                           VALUE 9000.00.
           05  WS-AML8-BAND-HIGH           PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
           05  WS-AML8-WINDOW-DAYS         PIC S9(05) VALUE 10.
           05  WS-AML8-MIN-DAYS            PIC 9(02)  VALUE 2.
       01  WS-AML8-WORK.
           05  WS-AML8-START               PIC 9(08).
           05  WS-AML8-DAYS                PIC 9(03) COMP.
           05  WS-AML8-LAST-DATE           PIC 9(08).
           05  WS-AML8-TODAY               PIC X(01).
           05  WS-AML8-TOTAL               PIC S9(13)V99 COMP-3.
           05  WS-AML8-ACCT                PIC X(12).

      *----------------------------------------------------------------*
      * ACCOUNTS REACTIVATED FROM DORMANCY IN THE LOOKBACK WINDOW      *
      * (DORM0002 ALERTS WRITTEN BY POSTTRAN) - INC21180               *
      *----------------------------------------------------------------*
       01  WS-REACT-TABLE.
           05  WS-REACT-COUNT              PIC 9(03) COMP VALUE 0.
           05  WS-REACT-ENTRY OCCURS 100 TIMES.
               10  WS-RT-ACCT              PIC X(12).
               10  WS-RT-DATE              PIC 9(08).
       01  WS-RX                           PIC 9(03) COMP.

       01  WS-COUNTERS.
           05  WS-HIST-READ                PIC 9(07) VALUE 0.
           05  WS-POST-READ                PIC 9(07) VALUE 0.
           05  WS-ITEM-COUNT               PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-CUST-EVAL-COUNT          PIC 9(05) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(05) VALUE 0.
           05  WS-SCEN-ALERTS OCCURS 8 TIMES PIC 9(05).
           05  WS-RETURN-CODE              PIC 9(02) VALUE 0.

       01  WS-MAX-CUST                     PIC 9(03) VALUE 300.
       01  WS-MAX-ITEMS                    PIC 9(03) VALUE 250.
       01  WS-MAX-ACCTS                    PIC 9(02) VALUE 20.
       01  WS-CUST-TABLE.
           05  WS-CUST-COUNT               PIC 9(03) COMP VALUE 0.
           05  WS-CUST-ENTRY OCCURS 300 TIMES.
               10  WS-CT-CUST-ID           PIC X(10).
               10  WS-CT-ACCT-COUNT        PIC 9(02) COMP.
               10  WS-CT-ACCT OCCURS 20 TIMES.
                   15  WS-CA-NUMBER        PIC X(12).
                   15  WS-CA-STATUS        PIC X(01).
                   15  WS-CA-OPEN-DATE     PIC 9(08).
                   15  WS-CA-DORMANT-DATE  PIC 9(08).
                   15  WS-CA-LAST-ACT      PIC 9(08).
               10  WS-CT-ITEM-COUNT        PIC 9(03) COMP.
               10  WS-CT-ITEM OCCURS 250 TIMES.
                   15  WS-CI-DATE          PIC 9(08).
                   15  WS-CI-ACCT          PIC X(12).
                   15  WS-CI-CODE          PIC X(04).
                   15  WS-CI-DR-CR         PIC X(01).
                   15  WS-CI-AMOUNT        PIC S9(13)V99 COMP-3.
                   15  WS-CI-CHANNEL       PIC X(03).
                   15  WS-CI-CASH-FLAG     PIC X(01).
                   15  WS-CI-CP-ACCT       PIC X(17).
                   15  WS-CI-CP-COUNTRY    PIC X(02).

       01  WS-WORK-FIELDS.
           05  WS-CX                       PIC 9(03) COMP.
           05  WS-IX                       PIC 9(03) COMP.
           05  WS-AX                       PIC 9(02) COMP.
           05  WS-HX                       PIC 9(02) COMP.
           05  WS-SX                       PIC 9(02) COMP.
           05  WS-FOUND                    PIC X(01).
           05  WS-OWNER-ID                 PIC X(10).
           05  WS-ITEM-DATE                PIC 9(08).
           05  WS-TIME                     PIC 9(08).
           05  WS-CUST-NAME                PIC X(40).
           05  WS-AMT-ED                   PIC ZZ,ZZZ,ZZ9.99.
           05  WS-DAYS-ED                  PIC 9(02).
           05  WS-SCEN-DIGIT               PIC 9(01).

       01  WS-SAVE-ACCOUNT.
           05  WS-SV-NUMBER                PIC X(12).
           05  WS-SV-STATUS                PIC X(01).
           05  WS-SV-OPEN-DATE             PIC 9(08).
           05  WS-SV-DORMANT-DATE          PIC 9(08).
           05  WS-SV-LAST-ACT              PIC 9(08).
           05  WS-SV-JOINT-ID              PIC X(10).

       01  WS-RPT-HEADER-1.
           05  FILLER PIC X(09) VALUE 'AMLMON   '.
           05  FILLER PIC X(49) VALUE
               'AML TRANSACTION MONITORING - SCENARIO HITS'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  WS-H1-DATE                  PIC 9(08).
           05  FILLER PIC X(17) VALUE '  LOOKBACK FROM '.
           05  WS-H1-START                 PIC 9(08).
       01  WS-RPT-HEADER-2.
           05  FILLER PIC X(40) VALUE 'CUST-ID    NAME'.
           05  FILLER PIC X(40) VALUE
               'SCEN SEV ACCOUNT             AMOUNT  DES'.
           05  FILLER PIC X(52) VALUE 'CRIPTION'.
       01  WS-RPT-DETAIL.
           05  WS-D-CUST-ID                PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NAME                   PIC X(28).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-SCEN                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-SEV                    PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-ACCT                   PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-AMOUNT                 PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-DESC                   PIC X(56).
       01  WS-RPT-COUNT.
           05  WS-C-LABEL                  PIC X(40).
           05  WS-C-VALUE                  PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.

       COPY TRANREC.
       COPY BANKCTL.
       COPY AMLPARM.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 1500-LOAD-REACTIVATIONS
           PERFORM 1600-OPEN-OUTPUTS
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
           SET DTU-ADD-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           COMPUTE DTU-NUMBER = 0 - WS-LOOKBACK-DAYS
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-LOOKBACK-START
           INITIALIZE DATE-PARMS
           SET DTU-ADD-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           COMPUTE DTU-NUMBER = 1 - WS-AML8-WINDOW-DAYS
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-AML8-START
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 8
               MOVE 0 TO WS-SCEN-ALERTS(WS-SX)
           END-PERFORM

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
           END-IF.

      *----------------------------------------------------------------*
      * DORM0002 = DORMANT ACCOUNT REACTIVATED (POSTTRAN 3500). THE    *
      * MASTER NO LONGER CARRIES THE DORMANT DATE AFTER REACTIVATION,  *
      * SO THE ACCOUNT IS TREATED AS DORMANT UNTIL THE DAY BEFORE.     *
      *----------------------------------------------------------------*
       1500-LOAD-REACTIVATIONS.
           OPEN INPUT ALERT-IN-FILE
           IF WS-ALRI-STATUS NOT = '00' AND WS-ALRI-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRI-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ALERT-IN-FILE INTO ALERT-RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF ALRT-TYPE = 'DORM'
                          AND ALRT-RULE-ID = 'DORM0002'
                          AND ALRT-DATE >= WS-LOOKBACK-START
                          AND ALRT-DATE <= CTL-BUSINESS-DATE
                          AND WS-REACT-COUNT < 100
                           ADD 1 TO WS-REACT-COUNT
                           MOVE ALRT-ACCT-NUMBER
                             TO WS-RT-ACCT(WS-REACT-COUNT)
                           MOVE ALRT-DATE
                             TO WS-RT-DATE(WS-REACT-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ALERT-IN-FILE.

       1600-OPEN-OUTPUTS.
           OPEN EXTEND ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'AMLRPT' TO ABND-FILE-NAME
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
       2000-READ-HISTORY.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANHIST-FILE INTO TRANSACTION-RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-HIST-READ
                       IF TRAN-POSTED
                          AND TRAN-POST-DATE >= WS-LOOKBACK-START
                          AND TRAN-POST-DATE <  CTL-BUSINESS-DATE
                           MOVE TRAN-POST-DATE TO WS-ITEM-DATE
                           PERFORM 2100-ADD-ITEM
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
                       IF TRAN-POSTED
                          AND TRAN-POST-DATE = CTL-BUSINESS-DATE
                           MOVE CTL-BUSINESS-DATE TO WS-ITEM-DATE
                           PERFORM 2100-ADD-ITEM
                       END-IF
               END-READ
               IF WS-TRAN-STATUS NOT = '00' AND NOT = '10'
                   MOVE 'TRANPOST' TO ABND-FILE-NAME
                   MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * ATTRIBUTE THE ITEM TO THE PRIMARY OWNER AND, FOR A JOINT       *
      * ACCOUNT, ALSO TO THE JOINT OWNER (CHG0655).                    *
      *----------------------------------------------------------------*
       2100-ADD-ITEM.
           ADD 1 TO WS-ITEM-COUNT
           MOVE TRAN-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY 'AMLMON   ACCT NOT FOUND ' TRAN-ACCT-NUMBER
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ
           MOVE ACCT-NUMBER             TO WS-SV-NUMBER
           MOVE ACCT-STATUS             TO WS-SV-STATUS
           MOVE ACCT-OPEN-DATE          TO WS-SV-OPEN-DATE
           MOVE ACCT-DORMANT-DATE       TO WS-SV-DORMANT-DATE
           IF WS-SV-DORMANT-DATE = ZERO
               PERFORM 2150-CHECK-REACTIVATED
           END-IF
           MOVE ACCT-LAST-ACTIVITY-DATE TO WS-SV-LAST-ACT
           MOVE ACCT-JOINT-CUST-ID      TO WS-SV-JOINT-ID
           MOVE ACCT-PRIMARY-CUST-ID    TO WS-OWNER-ID
           PERFORM 2200-ADD-TO-OWNER
           IF WS-SV-JOINT-ID NOT = SPACES
              AND WS-SV-JOINT-ID NOT = WS-OWNER-ID
               MOVE WS-SV-JOINT-ID TO WS-OWNER-ID
               PERFORM 2200-ADD-TO-OWNER
           END-IF.

       2150-CHECK-REACTIVATED.
           PERFORM VARYING WS-RX FROM 1 BY 1
                   UNTIL WS-RX > WS-REACT-COUNT
               IF WS-RT-ACCT(WS-RX) = ACCT-NUMBER
                   COMPUTE WS-SV-DORMANT-DATE = FUNCTION DATE-OF-INTEGER
                         (FUNCTION INTEGER-OF-DATE(WS-RT-DATE(WS-RX))
                          - 1)
               END-IF
           END-PERFORM.

       2200-ADD-TO-OWNER.
           PERFORM 2300-FIND-CUSTOMER
           IF WS-FOUND = 'N'
               EXIT PARAGRAPH
           END-IF
      *    ACCOUNT TABLE
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-AX FROM 1 BY 1
                   UNTIL WS-AX > WS-CT-ACCT-COUNT(WS-CX)
               IF WS-CA-NUMBER(WS-CX, WS-AX) = WS-SV-NUMBER
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N'
               IF WS-CT-ACCT-COUNT(WS-CX) < WS-MAX-ACCTS
                   ADD 1 TO WS-CT-ACCT-COUNT(WS-CX)
                   MOVE WS-CT-ACCT-COUNT(WS-CX) TO WS-AX
                   MOVE WS-SV-NUMBER  TO WS-CA-NUMBER(WS-CX, WS-AX)
                   MOVE WS-SV-STATUS  TO WS-CA-STATUS(WS-CX, WS-AX)
                   MOVE WS-SV-OPEN-DATE
                     TO WS-CA-OPEN-DATE(WS-CX, WS-AX)
                   MOVE WS-SV-DORMANT-DATE
                     TO WS-CA-DORMANT-DATE(WS-CX, WS-AX)
                   MOVE WS-SV-LAST-ACT
                     TO WS-CA-LAST-ACT(WS-CX, WS-AX)
               END-IF
           END-IF
      *    ITEM TABLE
           IF WS-CT-ITEM-COUNT(WS-CX) >= WS-MAX-ITEMS
               ADD 1 TO WS-SKIP-COUNT
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-CT-ITEM-COUNT(WS-CX)
           MOVE WS-CT-ITEM-COUNT(WS-CX) TO WS-IX
           MOVE WS-ITEM-DATE        TO WS-CI-DATE(WS-CX, WS-IX)
           MOVE TRAN-ACCT-NUMBER    TO WS-CI-ACCT(WS-CX, WS-IX)
           MOVE TRAN-CODE           TO WS-CI-CODE(WS-CX, WS-IX)
           MOVE TRAN-DR-CR          TO WS-CI-DR-CR(WS-CX, WS-IX)
           MOVE TRAN-AMOUNT         TO WS-CI-AMOUNT(WS-CX, WS-IX)
           MOVE TRAN-CHANNEL        TO WS-CI-CHANNEL(WS-CX, WS-IX)
           MOVE TRAN-CASH-FLAG      TO WS-CI-CASH-FLAG(WS-CX, WS-IX)
           MOVE TRAN-COUNTER-ACCT   TO WS-CI-CP-ACCT(WS-CX, WS-IX)
           MOVE TRAN-COUNTERPARTY-COUNTRY
                                    TO WS-CI-CP-COUNTRY(WS-CX, WS-IX).

       2300-FIND-CUSTOMER.
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
                             WS-CT-ACCT-COUNT(WS-CX)
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
           MOVE WS-CT-CUST-ID(WS-CX) TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'AMLMON   CUSTOMER NOT FOUND ' CUST-ID
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ
           ADD 1 TO WS-CUST-EVAL-COUNT
           MOVE SPACES TO WS-CUST-NAME
           IF CUST-INDIVIDUAL
               STRING CUST-FIRST-NAME DELIMITED BY '  '
                      ' ' DELIMITED BY SIZE
                      CUST-LAST-NAME DELIMITED BY '  '
                 INTO WS-CUST-NAME
               END-STRING
           ELSE
               MOVE CUST-LAST-NAME TO WS-CUST-NAME
           END-IF

           INITIALIZE AML-RULE-PARMS
           MOVE CTL-BUSINESS-DATE   TO AMP-BUSINESS-DATE
           MOVE CUST-ID             TO AMP-CUST-ID
           MOVE CUST-TYPE           TO AMP-CUST-TYPE
           MOVE CUST-SEGMENT        TO AMP-SEGMENT
           MOVE CUST-PEP-FLAG       TO AMP-PEP-FLAG
           MOVE CUST-MSB-FLAG       TO AMP-MSB-FLAG
           MOVE CUST-RISK-RATING    TO AMP-RISK-RATING
           MOVE CUST-ANNUAL-INCOME  TO AMP-ANNUAL-INCOME
           MOVE CUST-SINCE-DATE     TO AMP-CUST-SINCE-DATE
           MOVE WS-CT-ACCT-COUNT(WS-CX) TO AMP-ACCT-COUNT
           PERFORM VARYING WS-AX FROM 1 BY 1
                   UNTIL WS-AX > WS-CT-ACCT-COUNT(WS-CX)
               MOVE WS-CA-NUMBER(WS-CX, WS-AX)
                 TO AMP-ACCT-NUMBER(WS-AX)
               MOVE WS-CA-STATUS(WS-CX, WS-AX)
                 TO AMP-ACCT-STATUS(WS-AX)
               MOVE WS-CA-OPEN-DATE(WS-CX, WS-AX)
                 TO AMP-ACCT-OPEN-DATE(WS-AX)
               MOVE WS-CA-DORMANT-DATE(WS-CX, WS-AX)
                 TO AMP-ACCT-DORMANT-DATE(WS-AX)
               MOVE WS-CA-LAST-ACT(WS-CX, WS-AX)
                 TO AMP-ACCT-LAST-ACT-DATE(WS-AX)
           END-PERFORM
           MOVE WS-CT-ITEM-COUNT(WS-CX) TO AMP-ITEM-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CT-ITEM-COUNT(WS-CX)
               MOVE WS-CI-DATE(WS-CX, WS-IX)   TO AMP-ITEM-DATE(WS-IX)
               MOVE WS-CI-ACCT(WS-CX, WS-IX)   TO AMP-ITEM-ACCT(WS-IX)
               MOVE WS-CI-CODE(WS-CX, WS-IX)   TO AMP-ITEM-CODE(WS-IX)
               MOVE WS-CI-DR-CR(WS-CX, WS-IX)  TO AMP-ITEM-DR-CR(WS-IX)
               MOVE WS-CI-AMOUNT(WS-CX, WS-IX) TO AMP-ITEM-AMOUNT(WS-IX)
               MOVE WS-CI-CHANNEL(WS-CX, WS-IX)
                 TO AMP-ITEM-CHANNEL(WS-IX)
               MOVE WS-CI-CASH-FLAG(WS-CX, WS-IX)
                 TO AMP-ITEM-CASH-FLAG(WS-IX)
               MOVE WS-CI-CP-ACCT(WS-CX, WS-IX)
                 TO AMP-ITEM-CP-ACCT(WS-IX)
               MOVE WS-CI-CP-COUNTRY(WS-CX, WS-IX)
                 TO AMP-ITEM-CP-COUNTRY(WS-IX)
           END-PERFORM

           CALL 'AMLRULES' USING AML-RULE-PARMS
           EVALUATE AMP-RETURN-CODE
               WHEN 00
                   CONTINUE
               WHEN 04
                   PERFORM VARYING WS-HX FROM 1 BY 1
                           UNTIL WS-HX > AMP-HIT-COUNT
                       PERFORM 3200-WRITE-HIT
                   END-PERFORM
               WHEN OTHER
                   DISPLAY 'AMLMON   AMLRULES RC ' AMP-RETURN-CODE
                           ' CUST ' CUST-ID
                   MOVE 04 TO WS-RETURN-CODE
           END-EVALUATE
           PERFORM 4000-AML8-CASH-NEAR-THRESHOLD.

       3200-WRITE-HIT.
           INITIALIZE ALERT-RECORD
           ACCEPT WS-TIME FROM TIME
           MOVE CTL-BUSINESS-DATE          TO ALRT-DATE
           MOVE WS-TIME(1:6)               TO ALRT-TIME
           MOVE AMP-HIT-SCENARIO(WS-HX)    TO ALRT-TYPE
           MOVE AMP-HIT-SEVERITY(WS-HX)    TO ALRT-SEVERITY
           MOVE CUST-ID                    TO ALRT-CUST-ID
           MOVE AMP-HIT-ACCT(WS-HX)        TO ALRT-ACCT-NUMBER
           MOVE AMP-HIT-AMOUNT(WS-HX)      TO ALRT-AMOUNT
           MOVE AMP-HIT-RULE-ID(WS-HX)     TO ALRT-RULE-ID
           MOVE AMP-HIT-DESCRIPTION(WS-HX) TO ALRT-DESCRIPTION
           PERFORM 8000-WRITE-ALERT.

      *----------------------------------------------------------------*
      * AML8 - SAR TRIAGE PRE-SCREEN. CASH DEPOSITS JUST BELOW THE CTR *
      * THRESHOLD ON 2 OR MORE DAYS IN THE LAST 10 CALENDAR DAYS, WITH *
      * CASH ACTIVITY TODAY.                                           *
      *----------------------------------------------------------------*
       4000-AML8-CASH-NEAR-THRESHOLD.
           MOVE 0     TO WS-AML8-DAYS
           MOVE 0     TO WS-AML8-LAST-DATE
           MOVE ZERO  TO WS-AML8-TOTAL
           MOVE 'N'   TO WS-AML8-TODAY
           MOVE SPACES TO WS-AML8-ACCT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CT-ITEM-COUNT(WS-CX)
               IF WS-CI-CASH-FLAG(WS-CX, WS-IX) = 'Y'
                   IF WS-CI-DATE(WS-CX, WS-IX) = CTL-BUSINESS-DATE
                       MOVE 'Y' TO WS-AML8-TODAY
                   END-IF
                   IF WS-CI-DR-CR(WS-CX, WS-IX) = 'C'
                      AND WS-CI-DATE(WS-CX, WS-IX) >= WS-AML8-START
                      AND WS-CI-AMOUNT(WS-CX, WS-IX)
                          >= WS-AML8-BAND-LOW
                      AND WS-CI-AMOUNT(WS-CX, WS-IX)
                          <= WS-AML8-BAND-HIGH
                       ADD WS-CI-AMOUNT(WS-CX, WS-IX) TO WS-AML8-TOTAL
                       MOVE WS-CI-ACCT(WS-CX, WS-IX) TO WS-AML8-ACCT
      *                ITEMS ARE IN DATE ORDER - COUNT DATE CHANGES
                       IF WS-CI-DATE(WS-CX, WS-IX)
                          NOT = WS-AML8-LAST-DATE
                           ADD 1 TO WS-AML8-DAYS
                           MOVE WS-CI-DATE(WS-CX, WS-IX)
                             TO WS-AML8-LAST-DATE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-AML8-DAYS >= WS-AML8-MIN-DAYS AND WS-AML8-TODAY = 'Y'
               INITIALIZE ALERT-RECORD
               ACCEPT WS-TIME FROM TIME
               MOVE CTL-BUSINESS-DATE TO ALRT-DATE
               MOVE WS-TIME(1:6)      TO ALRT-TIME
               MOVE 'AML8'            TO ALRT-TYPE
               SET ALRT-MEDIUM        TO TRUE
               MOVE CUST-ID           TO ALRT-CUST-ID
               MOVE WS-AML8-ACCT      TO ALRT-ACCT-NUMBER
               MOVE WS-AML8-TOTAL     TO ALRT-AMOUNT
               MOVE 'AMLCASH9'        TO ALRT-RULE-ID
               MOVE WS-AML8-TOTAL     TO WS-AMT-ED
               MOVE WS-AML8-DAYS      TO WS-DAYS-ED
               STRING 'CASH 9,000-10,000 ON ' DELIMITED BY SIZE
                      WS-DAYS-ED DELIMITED BY SIZE
                      ' DAYS IN 10 - TOTAL ' DELIMITED BY SIZE
                      WS-AMT-ED DELIMITED BY SIZE
                 INTO ALRT-DESCRIPTION
               END-STRING
               PERFORM 8000-WRITE-ALERT
           END-IF.

      *----------------------------------------------------------------*
       8000-WRITE-ALERT.
           SET ALRT-OPEN TO TRUE
           MOVE WS-PROGRAM-NAME TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ALERT-COUNT
           IF ALRT-TYPE(4:1) >= '1' AND ALRT-TYPE(4:1) <= '8'
               MOVE ALRT-TYPE(4:1) TO WS-SX
               ADD 1 TO WS-SCEN-ALERTS(WS-SX)
           END-IF
           MOVE CUST-ID          TO WS-D-CUST-ID
           MOVE WS-CUST-NAME     TO WS-D-NAME
           MOVE ALRT-TYPE        TO WS-D-SCEN
           MOVE ALRT-SEVERITY    TO WS-D-SEV
           MOVE ALRT-ACCT-NUMBER TO WS-D-ACCT
           MOVE ALRT-AMOUNT      TO WS-D-AMOUNT
           MOVE ALRT-DESCRIPTION TO WS-D-DESC
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
           MOVE 'ITEMS PROFILED' TO WS-C-LABEL
           MOVE WS-ITEM-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CUSTOMERS EVALUATED' TO WS-C-LABEL
           MOVE WS-CUST-EVAL-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 8
               MOVE SPACES TO WS-C-LABEL
               MOVE WS-SX TO WS-SCEN-DIGIT
               STRING 'ALERTS AML' WS-SCEN-DIGIT DELIMITED BY SIZE
                 INTO WS-C-LABEL
               END-STRING
               MOVE WS-SCEN-ALERTS(WS-SX) TO WS-C-VALUE
               WRITE REPORT-LINE FROM WS-RPT-COUNT
           END-PERFORM
           MOVE 'TOTAL ALERTS RAISED' TO WS-C-LABEL
           MOVE WS-ALERT-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'ITEMS SKIPPED' TO WS-C-LABEL
           MOVE WS-SKIP-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           CLOSE TRANHIST-FILE TRANPOST-FILE ACCOUNT-FILE
                 CUSTOMER-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'AMLMON   HIST=' WS-HIST-READ
                   ' POST=' WS-POST-READ
                   ' ITEMS=' WS-ITEM-COUNT
                   ' CUSTOMERS=' WS-CUST-EVAL-COUNT
                   ' ALERTS=' WS-ALERT-COUNT
                   ' RC=' WS-RETURN-CODE.

       9900-ABEND.
           MOVE WS-PROGRAM-NAME TO ABND-PROGRAM
           MOVE 'FILE I/O' TO ABND-PARAGRAPH
           MOVE 'UNEXPECTED FILE STATUS' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
