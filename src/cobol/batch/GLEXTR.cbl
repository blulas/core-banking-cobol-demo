      *================================================================*
      * PROGRAM  : GLEXTR                                              *
      * TYPE     : BATCH - END OF DAY (EOD STEP 19)                    *
      * PURPOSE  : GENERAL LEDGER EXTRACT. EVERY POSTED ITEM OF THE    *
      *            DAY ON TRANPOST (ALL DOMAINS) IS TURNED INTO A      *
      *            BALANCED PAIR OF JOURNAL ENTRIES (ONE DEBIT, ONE    *
      *            CREDIT, SAME AMOUNT) USING GLDERIVE.                *
      *              - ACCOUNT CATEGORY / OWNERSHIP / COST CENTER FROM *
      *                ACCTMST; ITEMS FOR ACCOUNTS NOT ON ACCTMST      *
      *                (E.G. LOAN NUMBERS) FALL TO SUSPENSE 2999       *
      *              - TRAN-GL-ACCOUNT, WHEN SET BY THE POSTING        *
      *                PROGRAM, OVERRIDES THE OFFSET GL                *
      *              - ONLY STATUS 'S' (POSTED) ITEMS ARE EXTRACTED    *
      *            DEPOSIT INTEREST EXPENSE IS RECOGNISED WHEN PAID    *
      *            (DINT); THE DAILY ACCRUAL (INTACCR) IS NOT JOURNAL- *
      *            ISED - SEE CHG2201.                                 *
      * INPUT    : BANKCTL, TRANPOST, ACCTMST                          *
      * OUTPUT   : GLJRNL (NEW EACH DAY), GLEXRPT                      *
      * RETURN   : 0 OK, 4 ITEMS SENT TO SUSPENSE, 16 ABEND            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - GL INTERFACE                        *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED                           *
      * 2006-02-14 BHS  ORACLE GL - GL ACCOUNT X(10), COST CENTER      *
      * 2012-08-20 DPS  CHG2201 INTEREST ACCRUAL JOURNAL REMOVED -     *
      *                 FINANCE BOOKS ACCRUAL BY MANUAL ENTRY          *
      * 2013-06-11 DPS  TRAN-GL-ACCOUNT OVERRIDE                       *
      * 2015-10-02 DPS  READ TRANPOST INSTEAD OF POSTING EXTRACT       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLEXTR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT JOURNAL-FILE ASSIGN TO "GLJRNL"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-JRNL-STATUS.
           SELECT REPORT-FILE ASSIGN TO "GLEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  JOURNAL-FILE.
       COPY GLREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-POST-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-JRNL-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-RUN-RC                       PIC 9(02) VALUE 0.
       01  WS-JOURNAL-SEQ                  PIC 9(08) VALUE 0.
       01  WS-COUNTERS.
           05  WS-TRAN-READ                PIC 9(07) VALUE 0.
           05  WS-TRAN-SKIPPED             PIC 9(07) VALUE 0.
           05  WS-TRAN-EXTRACTED           PIC 9(07) VALUE 0.
           05  WS-TRAN-SUSPENSE            PIC 9(07) VALUE 0.
           05  WS-TRAN-NO-ACCT             PIC 9(07) VALUE 0.
           05  WS-TOTAL-DEBITS             PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CREDITS            PIC S9(15)V99 COMP-3
                                           VALUE 0.
      *--- SUMMARY BY GL / SIDE --------------------------------------*
       01  WS-SUMMARY-TABLE.
           05  WS-SUM-COUNT                PIC 9(03) VALUE 0.
           05  WS-SUM OCCURS 120 TIMES.
               10  WS-SUM-GL               PIC X(10).
               10  WS-SUM-DR               PIC S9(15)V99 COMP-3.
               10  WS-SUM-CR               PIC S9(15)V99 COMP-3.
               10  WS-SUM-ITEMS            PIC 9(07).
       01  WS-SX                           PIC 9(03).
       01  WS-LOOKUP-GL                    PIC X(10).
       01  WS-LOOKUP-AMT                   PIC S9(13)V99 COMP-3.
       01  WS-LOOKUP-SIDE                  PIC X(01).
      *--- REPORT LINES ----------------------------------------------*
       01  WS-RPT-HEAD1.
           05  FILLER PIC X(44) VALUE
               'CBS  GLEXTR    - GENERAL LEDGER EXTRACT     '.
           05  FILLER PIC X(16) VALUE 'BUSINESS DATE  '.
           05  WS-RH-DATE                  PIC 9(08).
       01  WS-RPT-HEAD2.
           05  FILLER PIC X(40) VALUE
               'GL ACCOUNT   ITEMS              DEBITS'.
           05  FILLER PIC X(20) VALUE '             CREDITS'.
       01  WS-RPT-SUM.
           05  WS-RS-GL                    PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RS-ITEMS                 PIC Z(6)9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RS-DR                    PIC -(14)9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RS-CR                    PIC -(14)9.99.
       01  WS-RPT-SUSP.
           05  FILLER PIC X(12) VALUE 'SUSPENSE -  '.
           05  WS-RU-TRAN-ID               PIC X(16).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RU-ACCT                  PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RU-CODE                  PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RU-AMOUNT                PIC -(13)9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RU-REASON                PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-RU-PGM                   PIC X(08).
       01  WS-RPT-TOT.
           05  WS-RT-LABEL                 PIC X(30).
           05  WS-RT-AMOUNT                PIC -(15)9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-RT-COUNT                 PIC Z(6)9.

       COPY GLDPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-TRAN-READ
                       PERFORM 2000-EXTRACT-ITEM
               END-READ
           END-PERFORM
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT TRANPOST-FILE
           IF WS-POST-STATUS NOT = '00' AND WS-POST-STATUS NOT = '05'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           IF WS-POST-STATUS = '05'
               DISPLAY 'GLEXTR  TRANPOST NOT PRESENT - EMPTY JOURNAL'
               SET WS-EOF TO TRUE
           END-IF
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT JOURNAL-FILE
           IF WS-JRNL-STATUS NOT = '00'
               MOVE 'GLJRNL' TO ABND-FILE-NAME
               MOVE WS-JRNL-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RH-DATE
           WRITE REPORT-LINE FROM WS-RPT-HEAD1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
       2000-EXTRACT-ITEM.
           IF NOT TRAN-POSTED OR TRAN-AMOUNT = 0
               ADD 1 TO WS-TRAN-SKIPPED
               EXIT PARAGRAPH
           END-IF

           INITIALIZE GL-DERIVE-PARMS
           MOVE 'T'              TO GLD-FUNCTION
           MOVE TRAN-CODE        TO GLD-TRAN-CODE
           MOVE TRAN-DR-CR       TO GLD-DR-CR
           MOVE TRAN-GL-ACCOUNT  TO GLD-OVERRIDE-GL
           MOVE TRAN-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-TRAN-NO-ACCT
                   MOVE 'XX'             TO GLD-ACCT-CATEGORY
                   MOVE SPACE            TO GLD-OWNERSHIP-TYPE
                   MOVE SPACES           TO GLD-PRODUCT-CODE
                   MOVE TRAN-COST-CENTER TO GLD-COST-CENTER
               NOT INVALID KEY
                   MOVE ACCT-CATEGORY       TO GLD-ACCT-CATEGORY
                   MOVE ACCT-OWNERSHIP-TYPE TO GLD-OWNERSHIP-TYPE
                   MOVE ACCT-PRODUCT-CODE   TO GLD-PRODUCT-CODE
                   MOVE ACCT-COST-CENTER    TO GLD-COST-CENTER
           END-READ
           IF GLD-COST-CENTER = SPACES
               MOVE '90000' TO GLD-COST-CENTER
           END-IF
           CALL 'GLDERIVE' USING GL-DERIVE-PARMS
           IF GLD-RETURN-CODE > 4
               DISPLAY 'GLEXTR  GLDERIVE RC ' GLD-RETURN-CODE
                       ' TRAN ' TRAN-ID ' ' GLD-REASON-CODE
               MOVE '2999' TO GLD-DEBIT-GL GLD-CREDIT-GL
               MOVE '90000' TO GLD-DEBIT-CC GLD-CREDIT-CC
               MOVE 'Y' TO GLD-SUSPENSE-FLAG
           END-IF
           IF GLD-TO-SUSPENSE
               ADD 1 TO WS-TRAN-SUSPENSE
               MOVE 4 TO WS-RUN-RC
               MOVE TRAN-ID          TO WS-RU-TRAN-ID
               MOVE TRAN-ACCT-NUMBER TO WS-RU-ACCT
               MOVE TRAN-CODE        TO WS-RU-CODE
               MOVE TRAN-AMOUNT      TO WS-RU-AMOUNT
               MOVE GLD-REASON-CODE  TO WS-RU-REASON
               MOVE TRAN-SOURCE-PGM  TO WS-RU-PGM
               WRITE REPORT-LINE FROM WS-RPT-SUSP
           END-IF

      *    DEBIT LEG
           INITIALIZE GL-JOURNAL-RECORD
           MOVE GLD-DEBIT-GL TO GLJ-GL-ACCOUNT
           MOVE GLD-DEBIT-CC TO GLJ-COST-CENTER
           MOVE 'D'          TO GLJ-DR-CR
           PERFORM 2100-WRITE-ENTRY
           ADD TRAN-AMOUNT TO WS-TOTAL-DEBITS
      *    CREDIT LEG
           INITIALIZE GL-JOURNAL-RECORD
           MOVE GLD-CREDIT-GL TO GLJ-GL-ACCOUNT
           MOVE GLD-CREDIT-CC TO GLJ-COST-CENTER
           MOVE 'C'           TO GLJ-DR-CR
           PERFORM 2100-WRITE-ENTRY
           ADD TRAN-AMOUNT TO WS-TOTAL-CREDITS
           ADD 1 TO WS-TRAN-EXTRACTED.

       2100-WRITE-ENTRY.
           ADD 1 TO WS-JOURNAL-SEQ
           MOVE CTL-BUSINESS-DATE TO GLJ-DATE
           MOVE WS-JOURNAL-SEQ    TO GLJ-JOURNAL-SEQ
           MOVE TRAN-AMOUNT       TO GLJ-AMOUNT
           MOVE TRAN-SOURCE-PGM   TO GLJ-SOURCE-PGM
           MOVE TRAN-ID           TO GLJ-SOURCE-TRAN-ID
           MOVE TRAN-ACCT-NUMBER  TO GLJ-ACCT-NUMBER
           MOVE TRAN-CODE         TO GLJ-TRAN-CODE
           MOVE TRAN-DESCRIPTION  TO GLJ-DESCRIPTION
           MOVE GLD-SUSPENSE-FLAG TO GLJ-SUSPENSE-FLAG
           WRITE GL-JOURNAL-RECORD
           IF WS-JRNL-STATUS NOT = '00'
               MOVE 'GLJRNL' TO ABND-FILE-NAME
               MOVE WS-JRNL-STATUS TO ABND-FILE-STATUS
               MOVE '2100-WRITE-ENTRY' TO ABND-PARAGRAPH
               MOVE 'JOURNAL WRITE FAILED' TO ABND-MESSAGE
               PERFORM 9910-ABEND
           END-IF
           MOVE GLJ-GL-ACCOUNT TO WS-LOOKUP-GL
           MOVE GLJ-AMOUNT     TO WS-LOOKUP-AMT
           MOVE GLJ-DR-CR      TO WS-LOOKUP-SIDE
           PERFORM 8000-ACCUMULATE.

       8000-ACCUMULATE.
           PERFORM VARYING WS-SX FROM 1 BY 1
                   UNTIL WS-SX > WS-SUM-COUNT
                      OR WS-SUM-GL(WS-SX) = WS-LOOKUP-GL
               CONTINUE
           END-PERFORM
           IF WS-SX > WS-SUM-COUNT
               IF WS-SUM-COUNT >= 120
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-SUM-COUNT
               MOVE WS-LOOKUP-GL TO WS-SUM-GL(WS-SX)
               MOVE ZERO TO WS-SUM-DR(WS-SX) WS-SUM-CR(WS-SX)
                            WS-SUM-ITEMS(WS-SX)
           END-IF
           ADD 1 TO WS-SUM-ITEMS(WS-SX)
           IF WS-LOOKUP-SIDE = 'D'
               ADD WS-LOOKUP-AMT TO WS-SUM-DR(WS-SX)
           ELSE
               ADD WS-LOOKUP-AMT TO WS-SUM-CR(WS-SX)
           END-IF.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-RPT-HEAD2
           PERFORM VARYING WS-SX FROM 1 BY 1
                   UNTIL WS-SX > WS-SUM-COUNT
               MOVE WS-SUM-GL(WS-SX)    TO WS-RS-GL
               MOVE WS-SUM-ITEMS(WS-SX) TO WS-RS-ITEMS
               MOVE WS-SUM-DR(WS-SX)    TO WS-RS-DR
               MOVE WS-SUM-CR(WS-SX)    TO WS-RS-CR
               WRITE REPORT-LINE FROM WS-RPT-SUM
           END-PERFORM
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TOTAL DEBITS'  TO WS-RT-LABEL
           MOVE WS-TOTAL-DEBITS TO WS-RT-AMOUNT
           MOVE WS-TRAN-EXTRACTED TO WS-RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'TOTAL CREDITS' TO WS-RT-LABEL
           MOVE WS-TOTAL-CREDITS TO WS-RT-AMOUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           MOVE 'ITEMS TO SUSPENSE' TO WS-RT-LABEL
           MOVE ZERO TO WS-RT-AMOUNT
           MOVE WS-TRAN-SUSPENSE TO WS-RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOT
           CLOSE TRANPOST-FILE ACCOUNT-FILE JOURNAL-FILE REPORT-FILE
           DISPLAY 'GLEXTR  TRANPOST READ=' WS-TRAN-READ
                   ' EXTRACTED=' WS-TRAN-EXTRACTED
                   ' SKIPPED=' WS-TRAN-SKIPPED
                   ' SUSPENSE=' WS-TRAN-SUSPENSE
                   ' NO-ACCT=' WS-TRAN-NO-ACCT
           DISPLAY 'GLEXTR  JOURNAL ENTRIES=' WS-JOURNAL-SEQ
           MOVE WS-RUN-RC TO RETURN-CODE.

       9900-ABEND-OPEN.
           MOVE '1000-INITIALISE' TO ABND-PARAGRAPH
           MOVE 'OPEN FAILED' TO ABND-MESSAGE
           PERFORM 9910-ABEND.

       9910-ABEND.
           MOVE 'GLEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
