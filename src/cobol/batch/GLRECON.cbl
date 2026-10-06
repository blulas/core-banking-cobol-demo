      *================================================================*
      * PROGRAM  : GLRECON                                             *
      * TYPE     : BATCH - END OF DAY (EOD STEP 19)                    *
      * PURPOSE  : DEPOSIT SUB-LEDGER TO GENERAL LEDGER RECONCILIATION.*
      *            1. ACCTMST LEDGER BALANCES ARE SUMMED BY DEPOSIT    *
      *               LIABILITY GL (GLDERIVE CATEGORY MAP) AND ACCOUNT *
      *               COST CENTER.                                     *
      *            2. EACH GLBAL CLOSING BALANCE FOR THE DEPOSIT       *
      *               LIABILITY ACCOUNTS 2010 2020 2110 2120 2130 IS   *
      *               COMPARED WITH THE SUB-LEDGER TOTAL FOR THE SAME  *
      *               GL + COST CENTER. ANY DIFFERENCE (TOLERANCE      *
      *               $0.00) IS A BREAK -> RECN ALERT (HIGH).          *
      *            3. SUB-LEDGER TOTALS WITH NO GL RECORD ARE BREAKS.  *
      *            4. SUSPENSE (2999) AND INTERNAL TRANSFER CLEARING   *
      *               (1190) MUST BE ZERO AT END OF DAY -> RECN ALERT  *
      *               (MEDIUM) IF NOT.                                 *
      *            RUN AFTER GLTRIAL, BEFORE EODCTL END.               *
      * INPUT    : BANKCTL, ACCTMST, GLBAL                             *
      * OUTPUT   : GLRCRPT, ALERTS                                     *
      * RETURN   : 0 RECONCILED, 4 BREAKS / OPEN CLEARING, 16 ABEND    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-13 RJT  ORIGINAL - DDA PROOF                           *
      * 2006-02-14 BHS  BY COST CENTER FOR ORACLE GL                   *
      * 2011-04-12 DPS  CHG1544 CORPORATE DDA 2020 RECONCILED          *
      * 2013-03-04 DPS  RECN ALERTS                                    *
      * 2015-10-02 DPS  SUSPENSE / CLEARING ZERO-BALANCE CHECK         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLRECON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT GLBAL-FILE ASSIGN TO "GLBAL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GLB-KEY
               FILE STATUS IS WS-GLB-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "GLRCRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  GLBAL-FILE.
       COPY GLBALREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-GLB-STATUS               PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.
       01  WS-RUN-RC                       PIC 9(02) VALUE 0.
      *    RECONCILIATION TOLERANCE - EXACT TO THE CENT
       01  WS-TOLERANCE                    PIC S9(05)V99 COMP-3
                                           VALUE 0.00.
       01  WS-RECON-GL-LIST.
           05  FILLER PIC X(04) VALUE '2010'.
           05  FILLER PIC X(04) VALUE '2020'.
           05  FILLER PIC X(04) VALUE '2110'.
           05  FILLER PIC X(04) VALUE '2120'.
           05  FILLER PIC X(04) VALUE '2130'.
       01  WS-RECON-GL-TBL REDEFINES WS-RECON-GL-LIST.
           05  WS-RECON-GL                 PIC X(04) OCCURS 5 TIMES.
       01  WS-GX                           PIC 9(01).
       01  WS-IS-RECON-GL                  PIC X(01).
       01  WS-COUNTERS.
           05  WS-ACCTS-READ               PIC 9(07) VALUE 0.
           05  WS-GL-COMPARED              PIC 9(05) VALUE 0.
           05  WS-BREAKS                   PIC 9(05) VALUE 0.
           05  WS-CLEARING-OPEN            PIC 9(05) VALUE 0.
       01  WS-TOTALS.
           05  WS-TOT-GL                   PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-SUB                  PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-BREAK                PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *--- SUB-LEDGER TOTALS BY GL + COST CENTER -------------------*
       01  WS-SUB-TABLE.
           05  WS-SUB-COUNT                PIC 9(03) VALUE 0.
           05  WS-SUB OCCURS 200 TIMES.
               10  WS-SUB-GL               PIC X(10).
               10  WS-SUB-CC               PIC X(05).
               10  WS-SUB-BAL              PIC S9(15)V99 COMP-3.
               10  WS-SUB-ACCTS            PIC 9(05).
               10  WS-SUB-MATCHED          PIC X(01).
       01  WS-SX                           PIC 9(03).
       01  WS-DIFF                         PIC S9(15)V99 COMP-3.
       01  WS-ABS-DIFF                     PIC S9(15)V99 COMP-3.
       01  WS-SUB-AMOUNT                   PIC S9(15)V99 COMP-3.
       01  WS-SUB-COUNT-ACCTS              PIC 9(05).
      *--- REPORT ----------------------------------------------------*
       01  WS-RPT-HEAD1.
           05  FILLER PIC X(50) VALUE
               'CBS  GLRECON   - SUB-LEDGER TO GL RECONCILIATION'.
           05  FILLER PIC X(16) VALUE '  BUSINESS DATE '.
           05  WS-RH-DATE                  PIC 9(08).
       01  WS-RPT-HEAD2.
           05  FILLER PIC X(40) VALUE
               'GL ACCT    CC    ACCTS      GL CLOSING  '.
           05  FILLER PIC X(44) VALUE
               '    SUB-LEDGER TOTAL        DIFFERENCE  STAT'.
       01  WS-RPT-DETAIL.
           05  WS-RD-GL                    PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-CC                    PIC X(05).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RD-ACCTS                 PIC Z(4)9.
           05  WS-RD-GLBAL                 PIC -(14)9.99.
           05  WS-RD-SUB                   PIC -(14)9.99.
           05  WS-RD-DIFF                  PIC -(14)9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-RD-STATUS                PIC X(30).
       01  WS-RPT-TOT.
           05  WS-RT-LABEL                 PIC X(40).
           05  WS-RT-AMOUNT                PIC -(15)9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-RT-COUNT                 PIC Z(4)9.

       COPY GLDPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-SUM-SUB-LEDGER
           PERFORM 3000-COMPARE-GL
           PERFORM 4000-UNMATCHED-SUB-LEDGER
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT GLBAL-FILE
           IF WS-GLB-STATUS NOT = '00'
               MOVE 'GLBAL' TO ABND-FILE-NAME
               MOVE WS-GLB-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN EXTEND ALERT-FILE
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RH-DATE
           WRITE REPORT-LINE FROM WS-RPT-HEAD1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-RPT-HEAD2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
      * 1. SUB-LEDGER                                                  *
      *----------------------------------------------------------------*
       2000-SUM-SUB-LEDGER.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-ACCTS-READ
                       PERFORM 2100-ADD-ACCOUNT
               END-READ
           END-PERFORM.

       2100-ADD-ACCOUNT.
           INITIALIZE GL-DERIVE-PARMS
           MOVE 'C'                 TO GLD-FUNCTION
           MOVE ACCT-CATEGORY       TO GLD-ACCT-CATEGORY
           MOVE ACCT-OWNERSHIP-TYPE TO GLD-OWNERSHIP-TYPE
           MOVE ACCT-PRODUCT-CODE   TO GLD-PRODUCT-CODE
           MOVE ACCT-COST-CENTER    TO GLD-COST-CENTER
           CALL 'GLDERIVE' USING GL-DERIVE-PARMS
           PERFORM VARYING WS-SX FROM 1 BY 1
                   UNTIL WS-SX > WS-SUB-COUNT
                      OR (WS-SUB-GL(WS-SX) = GLD-CUST-GL
                          AND WS-SUB-CC(WS-SX) = ACCT-COST-CENTER)
               CONTINUE
           END-PERFORM
           IF WS-SX > WS-SUB-COUNT
               IF WS-SUB-COUNT >= 200
                   DISPLAY 'GLRECON SUB-LEDGER TABLE FULL'
                   MOVE 4 TO WS-RUN-RC
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-SUB-COUNT
               MOVE GLD-CUST-GL      TO WS-SUB-GL(WS-SX)
               MOVE ACCT-COST-CENTER TO WS-SUB-CC(WS-SX)
               MOVE ZERO             TO WS-SUB-BAL(WS-SX)
                                        WS-SUB-ACCTS(WS-SX)
               MOVE 'N'              TO WS-SUB-MATCHED(WS-SX)
           END-IF
           ADD ACCT-LEDGER-BAL TO WS-SUB-BAL(WS-SX)
           ADD 1 TO WS-SUB-ACCTS(WS-SX).

      *----------------------------------------------------------------*
      * 2. GL SIDE                                                     *
      *----------------------------------------------------------------*
       3000-COMPARE-GL.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ GLBAL-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 3100-CHECK-GL
               END-READ
           END-PERFORM.

       3100-CHECK-GL.
           MOVE 'N' TO WS-IS-RECON-GL
           PERFORM VARYING WS-GX FROM 1 BY 1 UNTIL WS-GX > 5
               IF GLB-GL-ACCOUNT = WS-RECON-GL(WS-GX)
                   MOVE 'Y' TO WS-IS-RECON-GL
               END-IF
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-IS-RECON-GL = 'Y'
                   PERFORM 3200-RECONCILE-ONE
               WHEN GLB-GL-ACCOUNT = '2999'
               WHEN GLB-GL-ACCOUNT = '1190'
                   PERFORM 3300-CHECK-CLEARING
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       3200-RECONCILE-ONE.
           ADD 1 TO WS-GL-COMPARED
           MOVE ZERO TO WS-SUB-AMOUNT WS-SUB-COUNT-ACCTS
           PERFORM VARYING WS-SX FROM 1 BY 1
                   UNTIL WS-SX > WS-SUB-COUNT
               IF WS-SUB-GL(WS-SX) = GLB-GL-ACCOUNT
                  AND WS-SUB-CC(WS-SX) = GLB-COST-CENTER
                   MOVE WS-SUB-BAL(WS-SX)   TO WS-SUB-AMOUNT
                   MOVE WS-SUB-ACCTS(WS-SX) TO WS-SUB-COUNT-ACCTS
                   MOVE 'Y' TO WS-SUB-MATCHED(WS-SX)
               END-IF
           END-PERFORM
           COMPUTE WS-DIFF = GLB-CLOSING-BAL - WS-SUB-AMOUNT
           ADD GLB-CLOSING-BAL TO WS-TOT-GL
           ADD WS-SUB-AMOUNT   TO WS-TOT-SUB
           MOVE GLB-GL-ACCOUNT     TO WS-RD-GL
           MOVE GLB-COST-CENTER    TO WS-RD-CC
           MOVE WS-SUB-COUNT-ACCTS TO WS-RD-ACCTS
           MOVE GLB-CLOSING-BAL    TO WS-RD-GLBAL
           MOVE WS-SUB-AMOUNT      TO WS-RD-SUB
           MOVE WS-DIFF            TO WS-RD-DIFF
           IF WS-DIFF < 0
               COMPUTE WS-ABS-DIFF = 0 - WS-DIFF
           ELSE
               MOVE WS-DIFF TO WS-ABS-DIFF
           END-IF
           IF WS-ABS-DIFF > WS-TOLERANCE
               MOVE '*** BREAK ***' TO WS-RD-STATUS
               ADD 1 TO WS-BREAKS
               ADD WS-ABS-DIFF TO WS-TOT-BREAK
               MOVE 4 TO WS-RUN-RC
               PERFORM 8500-BREAK-ALERT
           ELSE
               MOVE 'OK' TO WS-RD-STATUS
           END-IF
           WRITE REPORT-LINE FROM WS-RPT-DETAIL.

       3300-CHECK-CLEARING.
           IF GLB-CLOSING-BAL = 0
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-CLEARING-OPEN
           MOVE 4 TO WS-RUN-RC
           MOVE GLB-GL-ACCOUNT  TO WS-RD-GL
           MOVE GLB-COST-CENTER TO WS-RD-CC
           MOVE ZERO            TO WS-RD-ACCTS WS-RD-SUB
           MOVE GLB-CLOSING-BAL TO WS-RD-GLBAL WS-RD-DIFF
           MOVE '*** CLEARING NOT ZERO ***' TO WS-RD-STATUS
           WRITE REPORT-LINE FROM WS-RPT-DETAIL
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE 234000            TO ALRT-TIME
           MOVE 'RECN'            TO ALRT-TYPE
           MOVE 'M'               TO ALRT-SEVERITY
           MOVE GLB-CLOSING-BAL   TO ALRT-AMOUNT
           MOVE 'GL-CLEAR'        TO ALRT-RULE-ID
           STRING 'GL ' GLB-GL-ACCOUNT(1:4) ' CC ' GLB-COST-CENTER
                  ' ' GLB-DESCRIPTION ' NOT ZERO AT EOD'
                  DELIMITED BY SIZE INTO ALRT-DESCRIPTION
           MOVE 'O'               TO ALRT-STATUS
           MOVE 'GLRECON'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD.

      *----------------------------------------------------------------*
      * 3. SUB-LEDGER WITHOUT A GL RECORD                              *
      *----------------------------------------------------------------*
       4000-UNMATCHED-SUB-LEDGER.
           PERFORM VARYING WS-SX FROM 1 BY 1
                   UNTIL WS-SX > WS-SUB-COUNT
               IF WS-SUB-MATCHED(WS-SX) = 'N'
                  AND WS-SUB-BAL(WS-SX) NOT = 0
                   MOVE WS-SUB-GL(WS-SX)    TO WS-RD-GL GLB-GL-ACCOUNT
                   MOVE WS-SUB-CC(WS-SX)    TO WS-RD-CC GLB-COST-CENTER
                   MOVE WS-SUB-ACCTS(WS-SX) TO WS-RD-ACCTS
                   MOVE ZERO                TO WS-RD-GLBAL
                   MOVE WS-SUB-BAL(WS-SX)   TO WS-RD-SUB
                   COMPUTE WS-DIFF = 0 - WS-SUB-BAL(WS-SX)
                   MOVE WS-DIFF             TO WS-RD-DIFF
                   MOVE '*** NO GL RECORD ***' TO WS-RD-STATUS
                   WRITE REPORT-LINE FROM WS-RPT-DETAIL
                   ADD 1 TO WS-BREAKS
                   ADD WS-SUB-BAL(WS-SX) TO WS-TOT-SUB
                   MOVE 4 TO WS-RUN-RC
                   PERFORM 8500-BREAK-ALERT
               END-IF
           END-PERFORM.

       8500-BREAK-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE 234000            TO ALRT-TIME
           MOVE 'RECN'            TO ALRT-TYPE
           MOVE 'H'               TO ALRT-SEVERITY
           MOVE WS-DIFF           TO ALRT-AMOUNT
           MOVE 'GL-SUBL'         TO ALRT-RULE-ID
           STRING 'DEPOSIT SUB-LEDGER VS GL BREAK - GL '
                  GLB-GL-ACCOUNT(1:4) ' CC ' GLB-COST-CENTER
                  DELIMITED BY SIZE INTO ALRT-DESCRIPTION
           MOVE 'O'               TO ALRT-STATUS
           MOVE 'GLRECON'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD.

       9000-TERMINATE.
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TOTAL GL DEPOSIT LIABILITIES' TO WS-RT-LABEL
           MOVE WS-TOT-GL TO WS-RT-AMOUNT
           MOVE WS-GL-COMPARED TO WS-RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'TOTAL DEPOSIT SUB-LEDGER' TO WS-RT-LABEL
           MOVE WS-TOT-SUB TO WS-RT-AMOUNT
           MOVE WS-SUB-COUNT TO WS-RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'BREAKS (ABSOLUTE)' TO WS-RT-LABEL
           MOVE WS-TOT-BREAK TO WS-RT-AMOUNT
           MOVE WS-BREAKS TO WS-RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'CLEARING / SUSPENSE NOT ZERO' TO WS-RT-LABEL
           MOVE ZERO TO WS-RT-AMOUNT
           MOVE WS-CLEARING-OPEN TO WS-RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           CLOSE ACCOUNT-FILE GLBAL-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'GLRECON ACCOUNTS=' WS-ACCTS-READ
                   ' GL COMPARED=' WS-GL-COMPARED
                   ' BREAKS=' WS-BREAKS
                   ' CLEARING OPEN=' WS-CLEARING-OPEN
           MOVE WS-RUN-RC TO RETURN-CODE.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'GLRECON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
