      *================================================================*
      * PROGRAM  : TRANEDIT                                            *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EOD STEP 04 - EDITS THE DAY'S MONETARY TRANSACTIONS *
      *            FROM ALL CHANNELS BEFORE POSTING.                   *
      *            - READS CHANNEL INPUT (TRANIN), INBOUND ACH         *
      *              (ACHTRAN) AND PAYMENT INSTRUCTIONS (PAYTRAN). ANY *
      *              OF THE THREE MAY BE EMPTY OR NOT CATALOGUED.      *
      *            - LOOKS UP ACCOUNT AND PRODUCT, CALLS TRANVAL.      *
      *            - ACCEPTED ITEMS ARE SORTED BY ACCOUNT, CREDITS     *
      *              BEFORE DEBITS, POSTING PRIORITY, ARRIVAL ORDER,   *
      *              AND WRITTEN TO TRANEDT FOR POSTTRAN.              *
      *            - REJECTED ITEMS GO TO TRANREJ WITH THE REASON.     *
      *            - CONTROL REPORT: COUNTS, AMOUNTS AND ACCOUNT HASH  *
      *              TOTALS PER SOURCE, REJECTS BY REASON.             *
      * INPUT    : TRANIN, ACHTRAN, PAYTRAN (TRANREC, OPTIONAL)        *
      *            ACCTMST, PRODMST (KSDS), BANKCTL VIA GETCTL         *
      * OUTPUT   : TRANEDT, TRANREJ (TRANREC), EDITRPT (PRINT)         *
      * CALLS    : GETCTL, TRANVAL, ABENDRTN                           *
      * RETURN   : 0 OK, 4 ONE OR MORE ITEMS REJECTED                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - TRANIN ONLY                         *
      * 1994-03-22 RJT  ACH INPUT FILE ADDED                           *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-06-17 BHS  INTERNAL SORT REPLACES DFSORT STEP (CHG0233)   *
      * 2007-05-14 BHS  DAILY ATM LIMIT ACCUMULATED ACROSS THE RUN     *
      * 2012-10-01 DPS  PAYTRAN (PAYMENT HUB) INPUT ADDED              *
      * 2015-10-02 DPS  CORE CONVERSION - HASH TOTALS PER SOURCE       *
      * 2016-09-23 DPS  SDA CHANNEL                                    *
      * 2019-08-12 AKW  INPUT FILES OPTIONAL - INC24410 ABEND WHEN     *
      *                 PAYMENT HUB SENT NO FILE                       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANEDIT.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL TRANIN-FILE ASSIGN TO "TRANIN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OPTIONAL ACHTRAN-FILE ASSIGN TO "ACHTRAN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OPTIONAL PAYTRAN-FILE ASSIGN TO "PAYTRAN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT TRANEDT-FILE ASSIGN TO "TRANEDT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EDT-STATUS.
           SELECT TRANREJ-FILE ASSIGN TO "TRANREJ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REJ-STATUS.
           SELECT REPORT-FILE ASSIGN TO "EDITRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.
           SELECT SORT-WORK ASSIGN TO "SORTWK1".

       DATA DIVISION.
       FILE SECTION.
       FD  TRANIN-FILE.
       01  TRANIN-REC                      PIC X(281).
       FD  ACHTRAN-FILE.
       01  ACHTRAN-REC                     PIC X(281).
       FD  PAYTRAN-FILE.
       01  PAYTRAN-REC                     PIC X(281).
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  TRANEDT-FILE.
       01  TRANEDT-REC                     PIC X(281).
       FD  TRANREJ-FILE.
       01  TRANREJ-REC                     PIC X(281).
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).
       SD  SORT-WORK.
       01  SORT-REC.
           05  SR-ACCT-NUMBER              PIC X(12).
      *        1 = CREDIT, 2 = DEBIT
           05  SR-DRCR-CLASS               PIC 9(01).
           05  SR-POST-PRIORITY            PIC 9(01).
           05  SR-SOURCE                   PIC 9(01).
           05  SR-ARRIVAL-SEQ              PIC 9(08).
           05  SR-TRAN-DATA                PIC X(281).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-IN-STATUS                PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-EDT-STATUS               PIC X(02).
           05  WS-REJ-STATUS               PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01).
               88  WS-EOF                            VALUE 'Y'.
           05  WS-SORT-EOF-FLAG            PIC X(01) VALUE 'N'.
               88  WS-SORT-EOF                       VALUE 'Y'.

       01  WS-SOURCE-SUB                   PIC 9(01).
       01  WS-ARRIVAL-SEQ                  PIC 9(08) VALUE 0.
       01  WS-WRITTEN-COUNT                PIC 9(07) VALUE 0.
       01  WS-TOTAL-REJECTS                PIC 9(07) VALUE 0.
       01  WS-ACCT-NUM-N                   PIC 9(12).

      *----------------------------------------------------------------*
      * CONTROL TOTALS PER SOURCE (1 TRANIN, 2 ACHTRAN, 3 PAYTRAN)     *
      *----------------------------------------------------------------*
       01  WS-SOURCE-TOTALS.
           05  WS-SRC OCCURS 3 TIMES.
               10  WS-SRC-NAME             PIC X(08).
               10  WS-SRC-PRESENT          PIC X(01).
               10  WS-SRC-READ             PIC 9(07).
               10  WS-SRC-ACCEPTED         PIC 9(07).
               10  WS-SRC-REJECTED         PIC 9(07).
               10  WS-SRC-DR-AMT           PIC S9(15)V99 COMP-3.
               10  WS-SRC-CR-AMT           PIC S9(15)V99 COMP-3.
               10  WS-SRC-REJ-AMT          PIC S9(15)V99 COMP-3.
               10  WS-SRC-ACCT-HASH        PIC 9(15) COMP-3.

      *----------------------------------------------------------------*
      * REJECTS BY REASON CODE                                         *
      *----------------------------------------------------------------*
       01  WS-REASON-TABLE.
           05  WS-REASON-COUNT-USED        PIC 9(02) VALUE 0.
           05  WS-REASON OCCURS 30 TIMES INDEXED BY RSN-IDX.
               10  WS-RSN-CODE             PIC X(04).
               10  WS-RSN-TEXT             PIC X(40).
               10  WS-RSN-COUNT            PIC 9(07).

      *----------------------------------------------------------------*
      * ATM / CASH WITHDRAWALS ACCEPTED IN THIS RUN, BY ACCOUNT, SO    *
      * THE DAILY LIMIT IS ENFORCED ACROSS ITEMS IN THE SAME FILE      *
      *----------------------------------------------------------------*
       01  WS-WD-TABLE.
           05  WS-WD-USED-COUNT            PIC 9(04) VALUE 0.
           05  WS-WD-ENTRY OCCURS 500 TIMES INDEXED BY WD-IDX.
               10  WS-WD-ACCT              PIC X(12).
               10  WS-WD-AMT               PIC S9(09)V99 COMP-3.
       01  WS-WD-RUN-AMT                   PIC S9(09)V99 COMP-3.
       01  WS-WD-FOUND                     PIC X(01).

       01  WS-RPT-DETAIL.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-NAME                     PIC X(08).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-PRESENT                  PIC X(07).
           05  RD-READ                     PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-ACCEPTED                 PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-REJECTED                 PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-CR-AMT                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-DR-AMT                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RD-HASH                     PIC Z(14)9.
       01  WS-RPT-REASON.
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  RR-CODE                     PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RR-TEXT                     PIC X(40).
           05  RR-COUNT                    PIC ZZZ,ZZ9.
       01  WS-RPT-DATE-ED                  PIC 9999/99/99.

       COPY TRANREC.
       COPY TRVPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           SORT SORT-WORK
               ON ASCENDING KEY SR-ACCT-NUMBER
                                SR-DRCR-CLASS
                                SR-POST-PRIORITY
                                SR-SOURCE
                                SR-ARRIVAL-SEQ
               INPUT  PROCEDURE IS 2000-EDIT-ALL-SOURCES
               OUTPUT PROCEDURE IS 3000-WRITE-EDITED
           PERFORM 8000-WRITE-REPORT
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           INITIALIZE WS-SOURCE-TOTALS
           MOVE 'TRANIN'  TO WS-SRC-NAME(1)
           MOVE 'ACHTRAN' TO WS-SRC-NAME(2)
           MOVE 'PAYTRAN' TO WS-SRC-NAME(3)
           OPEN INPUT ACCOUNT-FILE PRODUCT-FILE
           IF WS-ACCT-STATUS NOT = '00' OR WS-PROD-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - ACCTMST / PRODMST' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT TRANREJ-FILE
           IF WS-REJ-STATUS NOT = '00'
               MOVE 'TRANREJ' TO ABND-FILE-NAME
               MOVE WS-REJ-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - TRANREJ' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
      * SORT INPUT PROCEDURE - READ, EDIT AND RELEASE EACH SOURCE      *
      *----------------------------------------------------------------*
       2000-EDIT-ALL-SOURCES.
           MOVE 1 TO WS-SOURCE-SUB
           OPEN INPUT TRANIN-FILE
           PERFORM 2010-CHECK-OPEN
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANIN-FILE INTO TRANSACTION-RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-EDIT-ONE
               END-READ
           END-PERFORM
           CLOSE TRANIN-FILE

           MOVE 2 TO WS-SOURCE-SUB
           OPEN INPUT ACHTRAN-FILE
           PERFORM 2010-CHECK-OPEN
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ACHTRAN-FILE INTO TRANSACTION-RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-EDIT-ONE
               END-READ
           END-PERFORM
           CLOSE ACHTRAN-FILE

           MOVE 3 TO WS-SOURCE-SUB
           OPEN INPUT PAYTRAN-FILE
           PERFORM 2010-CHECK-OPEN
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ PAYTRAN-FILE INTO TRANSACTION-RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-EDIT-ONE
               END-READ
           END-PERFORM
           CLOSE PAYTRAN-FILE.

      *    STATUS 05 = OPTIONAL FILE NOT PRESENT - TREATED AS EMPTY
       2010-CHECK-OPEN.
           EVALUATE WS-IN-STATUS
               WHEN '00'
                   MOVE 'Y' TO WS-SRC-PRESENT(WS-SOURCE-SUB)
               WHEN '05'
                   MOVE 'N' TO WS-SRC-PRESENT(WS-SOURCE-SUB)
               WHEN OTHER
                   MOVE WS-SRC-NAME(WS-SOURCE-SUB) TO ABND-FILE-NAME
                   MOVE WS-IN-STATUS TO ABND-FILE-STATUS
                   MOVE 'OPEN FAILED - TRANSACTION INPUT'
                     TO ABND-MESSAGE
                   PERFORM 9900-ABEND
           END-EVALUATE.

       2100-EDIT-ONE.
           ADD 1 TO WS-SRC-READ(WS-SOURCE-SUB)
           ADD 1 TO WS-ARRIVAL-SEQ
           PERFORM 2200-BUILD-TRANVAL-PARMS
           CALL 'TRANVAL' USING TRANVAL-PARMS
      *    TRAN CODE TABLE IS AUTHORITATIVE FOR DIRECTION AND FLAGS
           IF TRV-DR-CR NOT = SPACE
               MOVE TRV-DR-CR     TO TRAN-DR-CR
               MOVE TRV-CASH-FLAG TO TRAN-CASH-FLAG
               MOVE TRV-REGD-FLAG TO TRAN-REGD-FLAG
           END-IF
           IF TRV-ACCEPTED
               PERFORM 2300-ACCEPT
           ELSE
               PERFORM 2400-REJECT
           END-IF.

       2200-BUILD-TRANVAL-PARMS.
           INITIALIZE TRANVAL-PARMS
           MOVE TRAN-CODE           TO TRV-TRAN-CODE
           MOVE TRAN-AMOUNT         TO TRV-AMOUNT
           MOVE TRAN-CHANNEL        TO TRV-CHANNEL
           MOVE TRAN-EFFECTIVE-DATE TO TRV-EFFECTIVE-DATE
           MOVE CTL-BUSINESS-DATE   TO TRV-BUSINESS-DATE
           MOVE 'N'                 TO TRV-ACCT-FOUND-FLAG
                                       TRV-PROD-FOUND-FLAG
           MOVE TRAN-ACCT-NUMBER    TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   MOVE 'Y'                 TO TRV-ACCT-FOUND-FLAG
                   MOVE ACCT-STATUS         TO TRV-ACCT-STATUS
                   MOVE ACCT-CATEGORY       TO TRV-ACCT-CATEGORY
                   MOVE ACCT-MATURITY-DATE  TO TRV-MATURITY-DATE
                   MOVE ACCT-GRACE-END-DATE TO TRV-GRACE-END-DATE
                   MOVE ACCT-DAILY-WD-LIMIT TO TRV-DAILY-WD-LIMIT
                   PERFORM 2210-RUN-WD-USED
                   COMPUTE TRV-DAILY-WD-USED = ACCT-DAILY-WD-USED
                                             + WS-WD-RUN-AMT
                   MOVE ACCT-PRODUCT-CODE   TO PROD-CODE
                   READ PRODUCT-FILE
                       INVALID KEY
                           CONTINUE
                       NOT INVALID KEY
                           MOVE 'Y' TO TRV-PROD-FOUND-FLAG
                           MOVE PROD-CHECK-ALLOWED
                             TO TRV-PROD-CHECK-ALLOWED
                   END-READ
           END-READ
      *    ITEMS KEYED BY A TELLER AT THE BRANCH ARE TAKEN AS ID
      *    VERIFIED - TELLER PROCEDURE 7.3 REQUIRES PHOTO ID FOR ANY
      *    DORMANT ACCOUNT TRANSACTION (CHG0761)
           IF TRAN-CHANNEL = 'BRN' AND TRAN-USER-ID NOT = SPACES
               MOVE 'Y' TO TRV-ID-VERIFIED-FLAG
           ELSE
               MOVE 'N' TO TRV-ID-VERIFIED-FLAG
           END-IF.

       2210-RUN-WD-USED.
           MOVE ZERO TO WS-WD-RUN-AMT
           MOVE 'N'  TO WS-WD-FOUND
           PERFORM VARYING WD-IDX FROM 1 BY 1
                   UNTIL WD-IDX > WS-WD-USED-COUNT
                      OR WS-WD-FOUND = 'Y'
               IF WS-WD-ACCT(WD-IDX) = TRAN-ACCT-NUMBER
                   MOVE WS-WD-AMT(WD-IDX) TO WS-WD-RUN-AMT
                   MOVE 'Y' TO WS-WD-FOUND
               END-IF
           END-PERFORM.

       2300-ACCEPT.
           IF TRV-WD-LIMIT-FLAG = 'Y'
               PERFORM 2310-ADD-RUN-WD
           END-IF
           ADD 1 TO WS-SRC-ACCEPTED(WS-SOURCE-SUB)
           PERFORM 2500-ADD-CONTROL-TOTALS
           SET TRAN-PENDING TO TRUE
           MOVE SPACES TO TRAN-REJECT-CODE
           MOVE TRAN-ACCT-NUMBER TO SR-ACCT-NUMBER
           IF TRAN-IS-CREDIT
               MOVE 1 TO SR-DRCR-CLASS
           ELSE
               MOVE 2 TO SR-DRCR-CLASS
           END-IF
           MOVE TRV-POST-PRIORITY   TO SR-POST-PRIORITY
           MOVE WS-SOURCE-SUB       TO SR-SOURCE
           MOVE WS-ARRIVAL-SEQ      TO SR-ARRIVAL-SEQ
           MOVE TRANSACTION-RECORD  TO SR-TRAN-DATA
           RELEASE SORT-REC.

       2310-ADD-RUN-WD.
           MOVE 'N' TO WS-WD-FOUND
           PERFORM VARYING WD-IDX FROM 1 BY 1
                   UNTIL WD-IDX > WS-WD-USED-COUNT
                      OR WS-WD-FOUND = 'Y'
               IF WS-WD-ACCT(WD-IDX) = TRAN-ACCT-NUMBER
                   ADD TRAN-AMOUNT TO WS-WD-AMT(WD-IDX)
                   MOVE 'Y' TO WS-WD-FOUND
               END-IF
           END-PERFORM
           IF WS-WD-FOUND = 'N' AND WS-WD-USED-COUNT < 500
               ADD 1 TO WS-WD-USED-COUNT
               MOVE TRAN-ACCT-NUMBER TO WS-WD-ACCT(WS-WD-USED-COUNT)
               MOVE TRAN-AMOUNT      TO WS-WD-AMT(WS-WD-USED-COUNT)
           END-IF.

       2400-REJECT.
           ADD 1 TO WS-SRC-REJECTED(WS-SOURCE-SUB)
           ADD 1 TO WS-TOTAL-REJECTS
           ADD TRAN-AMOUNT TO WS-SRC-REJ-AMT(WS-SOURCE-SUB)
           SET TRAN-REJECTED TO TRUE
           MOVE TRV-REJECT-CODE   TO TRAN-REJECT-CODE
           MOVE CTL-BUSINESS-DATE TO TRAN-POST-DATE
           MOVE 'TRANEDIT'        TO TRAN-SOURCE-PGM
           WRITE TRANREJ-REC FROM TRANSACTION-RECORD
           IF WS-REJ-STATUS NOT = '00'
               MOVE 'TRANREJ' TO ABND-FILE-NAME
               MOVE WS-REJ-STATUS TO ABND-FILE-STATUS
               MOVE 'WRITE FAILED - TRANREJ' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           DISPLAY 'TRANEDIT REJECT ' TRAN-ACCT-NUMBER ' '
                   TRAN-CODE ' ' TRV-REJECT-CODE ' '
                   TRV-REJECT-TEXT
           PERFORM 2410-COUNT-REASON.

       2410-COUNT-REASON.
           SET RSN-IDX TO 1
           SEARCH WS-REASON
               AT END
                   IF WS-REASON-COUNT-USED < 30
                       ADD 1 TO WS-REASON-COUNT-USED
                       SET RSN-IDX TO WS-REASON-COUNT-USED
                       MOVE TRV-REJECT-CODE TO WS-RSN-CODE(RSN-IDX)
                       MOVE TRV-REJECT-TEXT TO WS-RSN-TEXT(RSN-IDX)
                       MOVE 1               TO WS-RSN-COUNT(RSN-IDX)
                   END-IF
               WHEN WS-RSN-CODE(RSN-IDX) = TRV-REJECT-CODE
                   ADD 1 TO WS-RSN-COUNT(RSN-IDX)
           END-SEARCH.

      *    HASH = SUM OF ACCOUNT NUMBERS MODULO 10**15
       2500-ADD-CONTROL-TOTALS.
           IF TRAN-IS-CREDIT
               ADD TRAN-AMOUNT TO WS-SRC-CR-AMT(WS-SOURCE-SUB)
           ELSE
               ADD TRAN-AMOUNT TO WS-SRC-DR-AMT(WS-SOURCE-SUB)
           END-IF
           IF TRAN-ACCT-NUMBER IS NUMERIC
               MOVE TRAN-ACCT-NUMBER TO WS-ACCT-NUM-N
               COMPUTE WS-SRC-ACCT-HASH(WS-SOURCE-SUB) =
                   FUNCTION MOD(WS-SRC-ACCT-HASH(WS-SOURCE-SUB)
                                + WS-ACCT-NUM-N, 1000000000000000)
           END-IF.

      *----------------------------------------------------------------*
      * SORT OUTPUT PROCEDURE - WRITE THE EDITED FILE IN POSTING ORDER *
      *----------------------------------------------------------------*
       3000-WRITE-EDITED.
           OPEN OUTPUT TRANEDT-FILE
           IF WS-EDT-STATUS NOT = '00'
               MOVE 'TRANEDT' TO ABND-FILE-NAME
               MOVE WS-EDT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - TRANEDT' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           PERFORM UNTIL WS-SORT-EOF
               RETURN SORT-WORK
                   AT END SET WS-SORT-EOF TO TRUE
                   NOT AT END
                       WRITE TRANEDT-REC FROM SR-TRAN-DATA
                       IF WS-EDT-STATUS NOT = '00'
                           MOVE 'TRANEDT' TO ABND-FILE-NAME
                           MOVE WS-EDT-STATUS TO ABND-FILE-STATUS
                           MOVE 'WRITE FAILED - TRANEDT'
                             TO ABND-MESSAGE
                           PERFORM 9900-ABEND
                       END-IF
                       ADD 1 TO WS-WRITTEN-COUNT
               END-RETURN
           END-PERFORM
           CLOSE TRANEDT-FILE.

      *----------------------------------------------------------------*
      * CONTROL REPORT                                                 *
      *----------------------------------------------------------------*
       8000-WRITE-REPORT.
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RPT-DATE-ED
           MOVE SPACES TO REPORT-LINE
           STRING 'CBS  TRANEDIT  TRANSACTION EDIT CONTROL REPORT'
                  '      BUSINESS DATE ' WS-RPT-DATE-ED
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES             TO REPORT-LINE
           MOVE 'SOURCE'           TO REPORT-LINE(3:6)
           MOVE 'FILE'             TO REPORT-LINE(13:4)
           MOVE 'READ'             TO REPORT-LINE(23:4)
           MOVE 'ACCEPTED'         TO REPORT-LINE(28:8)
           MOVE 'REJECTED'         TO REPORT-LINE(37:8)
           MOVE 'CREDITS ACCEPTED' TO REPORT-LINE(48:16)
           MOVE 'DEBITS ACCEPTED'  TO REPORT-LINE(69:15)
           MOVE 'ACCT HASH'        TO REPORT-LINE(92:9)
           WRITE REPORT-LINE
           PERFORM VARYING WS-SOURCE-SUB FROM 1 BY 1
                   UNTIL WS-SOURCE-SUB > 3
               MOVE WS-SRC-NAME(WS-SOURCE-SUB) TO RD-NAME
               IF WS-SRC-PRESENT(WS-SOURCE-SUB) = 'Y'
                   MOVE 'PRESENT' TO RD-PRESENT
               ELSE
                   MOVE 'ABSENT ' TO RD-PRESENT
               END-IF
               MOVE WS-SRC-READ(WS-SOURCE-SUB)      TO RD-READ
               MOVE WS-SRC-ACCEPTED(WS-SOURCE-SUB)  TO RD-ACCEPTED
               MOVE WS-SRC-REJECTED(WS-SOURCE-SUB)  TO RD-REJECTED
               MOVE WS-SRC-CR-AMT(WS-SOURCE-SUB)    TO RD-CR-AMT
               MOVE WS-SRC-DR-AMT(WS-SOURCE-SUB)    TO RD-DR-AMT
               MOVE WS-SRC-ACCT-HASH(WS-SOURCE-SUB) TO RD-HASH
               WRITE REPORT-LINE FROM WS-RPT-DETAIL
               DISPLAY 'TRANEDIT ' WS-SRC-NAME(WS-SOURCE-SUB)
                       ' READ=' WS-SRC-READ(WS-SOURCE-SUB)
                       ' ACCEPTED=' WS-SRC-ACCEPTED(WS-SOURCE-SUB)
                       ' REJECTED=' WS-SRC-REJECTED(WS-SOURCE-SUB)
           END-PERFORM
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE '  REJECTS BY REASON' TO REPORT-LINE
           WRITE REPORT-LINE
           PERFORM VARYING RSN-IDX FROM 1 BY 1
                   UNTIL RSN-IDX > WS-REASON-COUNT-USED
               MOVE WS-RSN-CODE(RSN-IDX)  TO RR-CODE
               MOVE WS-RSN-TEXT(RSN-IDX)  TO RR-TEXT
               MOVE WS-RSN-COUNT(RSN-IDX) TO RR-COUNT
               WRITE REPORT-LINE FROM WS-RPT-REASON
           END-PERFORM
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING '  RECORDS WRITTEN TO TRANEDT: ' WS-WRITTEN-COUNT
                  '   TO TRANREJ: ' WS-TOTAL-REJECTS
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           CLOSE REPORT-FILE.

       9000-TERMINATE.
           CLOSE ACCOUNT-FILE PRODUCT-FILE TRANREJ-FILE
           DISPLAY 'TRANEDIT EDITED=' WS-WRITTEN-COUNT
                   ' REJECTED=' WS-TOTAL-REJECTS
           IF WS-TOTAL-REJECTS > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRANEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
