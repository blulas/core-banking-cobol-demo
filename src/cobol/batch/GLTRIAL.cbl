      *================================================================*
      * PROGRAM  : GLTRIAL                                             *
      * TYPE     : BATCH - END OF DAY (EOD STEP 19)                    *
      * PURPOSE  : POSTS THE DAY'S GL JOURNAL (GLJRNL) TO THE GL       *
      *            BALANCE FILE (GLBAL) AND PRODUCES THE TRIAL         *
      *            BALANCE.                                            *
      *              - NEW MONTH: OPENING = PRIOR CLOSING, PERIOD      *
      *                DEBITS/CREDITS RESET                            *
      *              - TODAY BUCKETS RESET FOR ACCOUNTS NOT POSTED     *
      *                TODAY                                           *
      *              - GL/COST CENTER NOT ON GLBAL IS CREATED FROM     *
      *                THE CHART OF ACCOUNTS (UNKNOWN GL = LIABILITY,  *
      *                CREDIT NORMAL, FLAGGED ON THE REPORT)           *
      *            CONTROLS (BOTH MUST HOLD, ELSE RC 8 + RECN ALERT):  *
      *              1. JOURNAL DEBITS = JOURNAL CREDITS               *
      *              2. SUM OF DEBIT-NORMAL CLOSING BALANCES = SUM OF  *
      *                 CREDIT-NORMAL CLOSING BALANCES                 *
      *            NOTE - NOT RESTARTABLE. RERUNNING AFTER A           *
      *            SUCCESSFUL RUN POSTS THE JOURNAL TWICE. RESTORE     *
      *            GLBAL FROM THE PRE-STEP BACKUP (CBSGL STEP005).     *
      * INPUT    : BANKCTL, GLJRNL                                     *
      * OUTPUT   : GLBAL (UPDATED), GLTBRPT, ALERTS                    *
      * RETURN   : 0 BALANCED, 4 UNKNOWN GL CREATED, 8 OUT OF BALANCE  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED                           *
      * 2006-02-14 BHS  COST CENTER LEVEL BALANCES                     *
      * 2009-10-05 DPS  INC12655 BALANCE-LEVEL CONTROL ADDED           *
      * 2015-10-02 DPS  TODAY BUCKETS, RECN ALERT ON OUT OF BALANCE    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLTRIAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL JOURNAL-FILE ASSIGN TO "GLJRNL"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-JRNL-STATUS.
           SELECT GLBAL-FILE ASSIGN TO "GLBAL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLB-KEY
               FILE STATUS IS WS-GLB-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "GLTBRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JOURNAL-FILE.
       COPY GLREC.
       FD  GLBAL-FILE.
       COPY GLBALREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-JRNL-STATUS              PIC X(02).
           05  WS-GLB-STATUS               PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.
       01  WS-RUN-RC                       PIC 9(02) VALUE 0.
       01  WS-PERIOD-START                 PIC 9(08).
       01  WS-COUNTERS.
           05  WS-JRNL-READ                PIC 9(07) VALUE 0.
           05  WS-GL-CREATED               PIC 9(05) VALUE 0.
           05  WS-GL-UNKNOWN               PIC 9(05) VALUE 0.
           05  WS-GL-ROLLED                PIC 9(05) VALUE 0.
       01  WS-TOTALS.
           05  WS-JRNL-DEBITS              PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-JRNL-CREDITS             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-DR-NORMAL-TOTAL          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-CR-NORMAL-TOTAL          PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TYPE-TOTAL               PIC S9(15)V99 COMP-3
                                           OCCURS 5 TIMES.
           05  WS-DIFFERENCE               PIC S9(15)V99 COMP-3.
       01  WS-TX                           PIC 9(01).
       01  WS-TYPE-CODES                   PIC X(05) VALUE 'ALQIE'.
       01  WS-TYPE-NAMES.
           05  FILLER PIC X(12) VALUE 'ASSETS      '.
           05  FILLER PIC X(12) VALUE 'LIABILITIES '.
           05  FILLER PIC X(12) VALUE 'EQUITY      '.
           05  FILLER PIC X(12) VALUE 'INCOME      '.
           05  FILLER PIC X(12) VALUE 'EXPENSE     '.
       01  WS-TYPE-NAME-TBL REDEFINES WS-TYPE-NAMES.
           05  WS-TYPE-NAME                PIC X(12) OCCURS 5 TIMES.
      *--- REPORT ----------------------------------------------------*
       01  WS-RPT-HEAD1.
           05  FILLER PIC X(46) VALUE
               'CBS  GLTRIAL   - GENERAL LEDGER TRIAL BALANCE'.
           05  FILLER PIC X(16) VALUE ' BUSINESS DATE  '.
           05  WS-RH-DATE                  PIC 9(08).
       01  WS-RPT-HEAD2.
           05  FILLER PIC X(48) VALUE
               'GL ACCT    CC    DESCRIPTION                    '.
           05  FILLER PIC X(38) VALUE
               'TN         OPENING      TODAY DEBITS  '.
           05  FILLER PIC X(46) VALUE
               '   TODAY CREDITS           CLOSING'.
       01  WS-RPT-DETAIL.
           05  WS-RD-GL                    PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-CC                    PIC X(05).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-DESC                  PIC X(30).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-TYPE                  PIC X(01).
           05  WS-RD-NORMAL                PIC X(01).
           05  WS-RD-OPEN                  PIC -(14)9.99.
           05  WS-RD-DR                    PIC -(14)9.99.
           05  WS-RD-CR                    PIC -(14)9.99.
           05  WS-RD-CLOSE                 PIC -(14)9.99.
       01  WS-RPT-TOT.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  WS-RT-LABEL                 PIC X(44).
           05  WS-RT-AMOUNT                PIC -(15)9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-RT-NOTE                  PIC X(40).

       COPY GLCHART.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-ROLL-BALANCES
           PERFORM 3000-POST-JOURNAL
           PERFORM 4000-TRIAL-BALANCE
           PERFORM 5000-CONTROLS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           COMPUTE WS-PERIOD-START =
                   FUNCTION INTEGER-PART(CTL-BUSINESS-DATE / 100)
                   * 100 + 1
           OPEN I-O GLBAL-FILE
           IF WS-GLB-STATUS NOT = '00'
               MOVE 'GLBAL' TO ABND-FILE-NAME
               MOVE WS-GLB-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT JOURNAL-FILE
           IF WS-JRNL-STATUS NOT = '00' AND WS-JRNL-STATUS NOT = '05'
               MOVE 'GLJRNL' TO ABND-FILE-NAME
               MOVE WS-JRNL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND ALERT-FILE
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RH-DATE
           WRITE REPORT-LINE FROM WS-RPT-HEAD1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
      * START OF DAY / START OF PERIOD HOUSEKEEPING                    *
      *----------------------------------------------------------------*
       2000-ROLL-BALANCES.
           MOVE LOW-VALUES TO GLB-KEY
           START GLBAL-FILE KEY IS NOT LESS THAN GLB-KEY
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-GLB-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ GLBAL-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-ROLL-ONE
               END-READ
           END-PERFORM.

       2100-ROLL-ONE.
           IF GLB-LAST-POST-DATE = CTL-BUSINESS-DATE
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO GLB-TODAY-DEBITS GLB-TODAY-CREDITS
           IF GLB-PERIOD-START < WS-PERIOD-START
               MOVE GLB-CLOSING-BAL  TO GLB-OPENING-BAL
               MOVE ZERO             TO GLB-PERIOD-DEBITS
                                        GLB-PERIOD-CREDITS
               MOVE WS-PERIOD-START  TO GLB-PERIOD-START
               ADD 1 TO WS-GL-ROLLED
           END-IF
           REWRITE GL-BALANCE-RECORD
           IF WS-GLB-STATUS NOT = '00'
               MOVE 'GLBAL' TO ABND-FILE-NAME
               MOVE WS-GLB-STATUS TO ABND-FILE-STATUS
               MOVE '2100-ROLL-ONE' TO ABND-PARAGRAPH
               MOVE 'GLBAL REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

      *----------------------------------------------------------------*
      * POST JOURNAL                                                   *
      *----------------------------------------------------------------*
       3000-POST-JOURNAL.
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-JRNL-STATUS = '05'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ JOURNAL-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-JRNL-READ
                       PERFORM 3100-POST-ENTRY
               END-READ
           END-PERFORM.

       3100-POST-ENTRY.
           IF GLJ-DEBIT
               ADD GLJ-AMOUNT TO WS-JRNL-DEBITS
           ELSE
               ADD GLJ-AMOUNT TO WS-JRNL-CREDITS
           END-IF
           MOVE GLJ-GL-ACCOUNT  TO GLB-GL-ACCOUNT
           MOVE GLJ-COST-CENTER TO GLB-COST-CENTER
           READ GLBAL-FILE
               INVALID KEY
                   PERFORM 3200-CREATE-GL
           END-READ
           IF GLJ-DEBIT
               ADD GLJ-AMOUNT TO GLB-TODAY-DEBITS GLB-PERIOD-DEBITS
           ELSE
               ADD GLJ-AMOUNT TO GLB-TODAY-CREDITS GLB-PERIOD-CREDITS
           END-IF
           IF GLB-DEBIT-NORMAL
               COMPUTE GLB-CLOSING-BAL = GLB-OPENING-BAL
                       + GLB-PERIOD-DEBITS - GLB-PERIOD-CREDITS
           ELSE
               COMPUTE GLB-CLOSING-BAL = GLB-OPENING-BAL
                       + GLB-PERIOD-CREDITS - GLB-PERIOD-DEBITS
           END-IF
           MOVE CTL-BUSINESS-DATE TO GLB-LAST-POST-DATE
           REWRITE GL-BALANCE-RECORD
           IF WS-GLB-STATUS NOT = '00'
               MOVE 'GLBAL' TO ABND-FILE-NAME
               MOVE WS-GLB-STATUS TO ABND-FILE-STATUS
               MOVE '3100-POST-ENTRY' TO ABND-PARAGRAPH
               MOVE 'GLBAL REWRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF.

       3200-CREATE-GL.
           INITIALIZE GL-BALANCE-RECORD
           MOVE GLJ-GL-ACCOUNT  TO GLB-GL-ACCOUNT
           MOVE GLJ-COST-CENTER TO GLB-COST-CENTER
           MOVE WS-PERIOD-START TO GLB-PERIOD-START
           SET GLC-IDX TO 1
           SEARCH GLC-ENTRY
               AT END
                   MOVE 'NOT ON CHART OF ACCOUNTS' TO GLB-DESCRIPTION
                   MOVE 'L' TO GLB-ACCT-TYPE
                   MOVE 'C' TO GLB-NORMAL-BAL
                   ADD 1 TO WS-GL-UNKNOWN
                   MOVE 4 TO WS-RUN-RC
               WHEN GLC-GL-ACCOUNT(GLC-IDX) = GLJ-GL-ACCOUNT
                   MOVE GLC-DESCRIPTION(GLC-IDX) TO GLB-DESCRIPTION
                   MOVE GLC-ACCT-TYPE(GLC-IDX)   TO GLB-ACCT-TYPE
                   MOVE GLC-NORMAL-BAL(GLC-IDX)  TO GLB-NORMAL-BAL
           END-SEARCH
           WRITE GL-BALANCE-RECORD
           IF WS-GLB-STATUS NOT = '00'
               MOVE 'GLBAL' TO ABND-FILE-NAME
               MOVE WS-GLB-STATUS TO ABND-FILE-STATUS
               MOVE '3200-CREATE-GL' TO ABND-PARAGRAPH
               MOVE 'GLBAL WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           ADD 1 TO WS-GL-CREATED.

      *----------------------------------------------------------------*
      * TRIAL BALANCE                                                  *
      *----------------------------------------------------------------*
       4000-TRIAL-BALANCE.
           WRITE REPORT-LINE FROM WS-RPT-HEAD2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               MOVE ZERO TO WS-TYPE-TOTAL(WS-TX)
           END-PERFORM
           MOVE LOW-VALUES TO GLB-KEY
           START GLBAL-FILE KEY IS NOT LESS THAN GLB-KEY
           MOVE 'N' TO WS-EOF-FLAG
           IF WS-GLB-STATUS NOT = '00'
               SET WS-EOF TO TRUE
           END-IF
           PERFORM UNTIL WS-EOF
               READ GLBAL-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 4100-TB-LINE
               END-READ
           END-PERFORM
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               MOVE SPACES TO WS-RT-NOTE
               STRING 'TOTAL ' WS-TYPE-NAME(WS-TX) DELIMITED BY SIZE
                 INTO WS-RT-LABEL
               MOVE WS-TYPE-TOTAL(WS-TX) TO WS-RT-AMOUNT
               WRITE REPORT-LINE FROM WS-RPT-TOT
           END-PERFORM.

       4100-TB-LINE.
           MOVE GLB-GL-ACCOUNT    TO WS-RD-GL
           MOVE GLB-COST-CENTER   TO WS-RD-CC
           MOVE GLB-DESCRIPTION   TO WS-RD-DESC
           MOVE GLB-ACCT-TYPE     TO WS-RD-TYPE
           MOVE GLB-NORMAL-BAL    TO WS-RD-NORMAL
           MOVE GLB-OPENING-BAL   TO WS-RD-OPEN
           MOVE GLB-TODAY-DEBITS  TO WS-RD-DR
           MOVE GLB-TODAY-CREDITS TO WS-RD-CR
           MOVE GLB-CLOSING-BAL   TO WS-RD-CLOSE
           WRITE REPORT-LINE FROM WS-RPT-DETAIL
           IF GLB-DEBIT-NORMAL
               ADD GLB-CLOSING-BAL TO WS-DR-NORMAL-TOTAL
           ELSE
               ADD GLB-CLOSING-BAL TO WS-CR-NORMAL-TOTAL
           END-IF
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF GLB-ACCT-TYPE = WS-TYPE-CODES(WS-TX:1)
                   ADD GLB-CLOSING-BAL TO WS-TYPE-TOTAL(WS-TX)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * CONTROLS                                                       *
      *----------------------------------------------------------------*
       5000-CONTROLS.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO WS-RT-NOTE
           MOVE 'JOURNAL DEBITS TODAY' TO WS-RT-LABEL
           MOVE WS-JRNL-DEBITS TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'JOURNAL CREDITS TODAY' TO WS-RT-LABEL
           MOVE WS-JRNL-CREDITS TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'DEBIT-NORMAL CLOSING BALANCES' TO WS-RT-LABEL
           MOVE WS-DR-NORMAL-TOTAL TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'CREDIT-NORMAL CLOSING BALANCES' TO WS-RT-LABEL
           MOVE WS-CR-NORMAL-TOTAL TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT

           IF WS-JRNL-DEBITS NOT = WS-JRNL-CREDITS
               COMPUTE WS-DIFFERENCE = WS-JRNL-DEBITS - WS-JRNL-CREDITS
               MOVE 'JOURNAL OUT OF BALANCE' TO WS-RT-LABEL
               MOVE WS-DIFFERENCE TO WS-RT-AMOUNT
               MOVE '*** TRIAL BALANCE FAILED ***' TO WS-RT-NOTE
               WRITE REPORT-LINE FROM WS-RPT-TOT
               PERFORM 8500-RECN-ALERT
           END-IF
           IF WS-DR-NORMAL-TOTAL NOT = WS-CR-NORMAL-TOTAL
               COMPUTE WS-DIFFERENCE =
                       WS-DR-NORMAL-TOTAL - WS-CR-NORMAL-TOTAL
               MOVE 'LEDGER OUT OF BALANCE' TO WS-RT-LABEL
               MOVE WS-DIFFERENCE TO WS-RT-AMOUNT
               MOVE '*** TRIAL BALANCE FAILED ***' TO WS-RT-NOTE
               WRITE REPORT-LINE FROM WS-RPT-TOT
               PERFORM 8500-RECN-ALERT
           END-IF
           IF WS-RUN-RC < 8
               MOVE 'TRIAL BALANCE' TO WS-RT-LABEL
               MOVE ZERO TO WS-RT-AMOUNT
               MOVE 'IN BALANCE' TO WS-RT-NOTE
               WRITE REPORT-LINE FROM WS-RPT-TOT
           END-IF.

       8500-RECN-ALERT.
           MOVE 8 TO WS-RUN-RC
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE 233000            TO ALRT-TIME
           MOVE 'RECN'            TO ALRT-TYPE
           MOVE 'C'               TO ALRT-SEVERITY
           MOVE WS-DIFFERENCE     TO ALRT-AMOUNT
           MOVE 'GL-TRIAL'        TO ALRT-RULE-ID
           MOVE WS-RT-LABEL       TO ALRT-DESCRIPTION
           MOVE 'O'               TO ALRT-STATUS
           MOVE 'GLTRIAL'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD.

       9000-TERMINATE.
           CLOSE JOURNAL-FILE GLBAL-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'GLTRIAL JOURNAL ENTRIES=' WS-JRNL-READ
                   ' GL CREATED=' WS-GL-CREATED
                   ' UNKNOWN GL=' WS-GL-UNKNOWN
                   ' PERIOD ROLLED=' WS-GL-ROLLED
           IF WS-RUN-RC >= 8
               DISPLAY 'GLTRIAL *** TRIAL BALANCE OUT OF BALANCE ***'
           END-IF
           MOVE WS-RUN-RC TO RETURN-CODE.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'GLTRIAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
