      *================================================================*
      * PROGRAM  : POSTTRAN                                            *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EOD STEP 05 - MONETARY POSTING ENGINE.              *
      *            READS THE EDITED TRANSACTIONS (TRANEDT, IN ACCOUNT  *
      *            ORDER) AND FOR EACH ACCOUNT:                        *
      *            - ORDERS THE ITEMS: CREDITS FIRST, THEN DEBITS BY   *
      *              TC-POST-PRIORITY, THEN AMOUNT LOW-TO-HIGH         *
      *            - POSTS CREDITS: LEDGER, AVAILABLE, CYCLE COUNTS,   *
      *              CASH-IN, LAST ACTIVITY, DORMANT REACTIVATION AND  *
      *              REG CC HOLDS ON CHECK DEPOSITS (REGCCHLD)         *
      *            - POSTS DEBITS: PAY / RETURN DECISION (ODDECIDE),   *
      *              OD PROTECTION TRANSFER (WXFR / DXFR), OD AND NSF  *
      *              FEES (WFOD / WFNS), TRANSFER FEE (WFOT), REG D    *
      *              CONVENIENT WITHDRAWAL COUNT AND EXCESS FEE        *
      *              (REGDCHK / WFRD)                                  *
      *            - RETURNED ITEMS GO TO TRANREJ WITH CODE 'NSF ' SO  *
      *              ACHRET CAN RETURN ACH DEBITS TO THE ODFI          *
      *            - EVERY POSTED ITEM, AND EVERY GENERATED FEE AND    *
      *              TRANSFER (TRAN-ID-SEQ 20000001+), GOES TO TRANPOST*
      *            DAYS-OVERDRAWN IS MAINTAINED BY ODAGING, NOT HERE.  *
      * INPUT    : TRANEDT (TRANREC), CUSTMST, PRODMST, BANKCTL        *
      * I-O      : ACCTMST, HOLDMST                                    *
      * OUTPUT   : TRANPOST, TRANREJ, ALERTS (EXTEND), POSTRPT         *
      * CALLS    : GETCTL, ODDECIDE, REGCCHLD, REGDCHK, ABENDRTN       *
      * RETURN   : 0 OK, 4 ITEMS FOR ACCOUNTS NOT ON FILE              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1991-03-11 RJT  NSF RETURN PROCESSING                          *
      * 1993-04-19 RJT  AVAILABLE BALANCE / CHECK HOLDS                *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  CASH IN / OUT TODAY FOR CTR AGGREGATION        *
      * 2003-08-04 BHS  OD PROTECTION TRANSFER                         *
      * 2006-03-20 BHS  DEBITS POSTED HIGH-TO-LOW WITHIN PRIORITY      *
      * 2010-07-01 DPS  REG-2010-E / OCC GUIDANCE - DEBITS NOW POSTED  *
      *                 LOW-TO-HIGH WITHIN PRIORITY (CHG0811)          *
      * 2010-07-01 DPS  SEPARATE WFOD / WFNS FEE TRANSACTIONS          *
      * 2011-07-21 DPS  REG CC HOLDS WRITTEN TO HOLDMST VIA REGCCHLD   *
      * 2012-05-07 DPS  REG D EXCESS FEE VIA REGDCHK                   *
      * 2014-11-03 DPS  MEMO-POSTED ONLINE ITEMS RELIEVED AT POSTING   *
      * 2015-10-02 DPS  CORE CONVERSION - GENERATED SEQ 20000000+      *
      * 2018-09-17 AKW  RETURNED ITEMS TO TRANREJ 'NSF ' FOR ACHRET    *
      * 2020-05-01 AKW  CHG3301 REG D POLICY - CONVERT ALERT           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTTRAN.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANEDT-FILE ASSIGN TO "TRANEDT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EDT-STATUS.
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
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT HOLD-FILE ASSIGN TO "HOLDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HOLD-KEY
               FILE STATUS IS WS-HOLD-STATUS.
           SELECT OPTIONAL TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-POST-STATUS.
           SELECT OPTIONAL TRANREJ-FILE ASSIGN TO "TRANREJ"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REJ-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "POSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANEDT-FILE.
       01  TRANEDT-REC                     PIC X(281).
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  HOLD-FILE.
       COPY HOLDREC.
       FD  TRANPOST-FILE.
       01  TRANPOST-REC                    PIC X(281).
       FD  TRANREJ-FILE.
       01  TRANREJ-REC                     PIC X(281).
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-EDT-STATUS               PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-HOLD-STATUS              PIC X(02).
           05  WS-POST-STATUS              PIC X(02).
           05  WS-REJ-STATUS               PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-ACCT-FOUND-FLAG          PIC X(01).
               88  WS-ACCT-FOUND                     VALUE 'Y'.
           05  WS-HOLD-SCAN-DONE           PIC X(01).

      *----------------------------------------------------------------*
      * CURRENT ACCOUNT (WORKING COPY) AND TRANSACTION BUFFER          *
      *----------------------------------------------------------------*
       COPY ACCTREC REPLACING ==ACCOUNT-RECORD== BY ==CA-ACCOUNT-REC==
                              LEADING ==ACCT-== BY ==CA-==.
       01  WS-SAVE-ACCT                    PIC X(464).
       01  WS-CUR-ACCT-NUMBER              PIC X(12).
       01  WS-NEXT-TRAN                    PIC X(281).

       01  WS-CUR-CUST.
           05  WS-CUR-SEGMENT              PIC X(02).
           05  WS-CUR-EMPLOYEE             PIC X(01).
       01  WS-CUR-PROD.
           05  WS-CUR-PROD-OD-ALLOWED      PIC X(01).
           05  WS-CUR-PROD-REGD            PIC X(01).
       01  WS-RGD-PROD-REGD                PIC X(01).

       01  WS-BUFFER.
           05  WS-BUF-COUNT                PIC 9(04) VALUE 0.
           05  WS-BUF-ENTRY OCCURS 500 TIMES.
               10  BUF-TRAN                PIC X(281).
               10  BUF-CLASS               PIC 9(01).
               10  BUF-PRIO                PIC 9(01).
               10  BUF-AMT                 PIC S9(13)V99 COMP-3.
       01  WS-ORDER-TABLE.
           05  WS-ORDER                    PIC 9(04) OCCURS 500 TIMES.
       01  WS-SORT-WORK.
           05  WS-I                        PIC 9(04).
           05  WS-J                        PIC 9(04).
           05  WS-K                        PIC 9(04).
           05  WS-A                        PIC 9(04).
           05  WS-B                        PIC 9(04).
           05  WS-A-AFTER-B                PIC X(01).

      *----------------------------------------------------------------*
      * POSTING WORK FIELDS                                            *
      *----------------------------------------------------------------*
       01  WS-POSTING.
           05  WS-TC-CUST-INIT             PIC X(01).
           05  WS-TC-PRIORITY              PIC 9(01).
           05  WS-MEMO-PORTION             PIC S9(13)V99 COMP-3.
           05  WS-AVAIL-EFFECT             PIC S9(13)V99 COMP-3.
           05  WS-CHK-DEPOSITS-TODAY       PIC S9(13)V99 COMP-3.
           05  WS-GEN-SEQ                  PIC 9(08) VALUE 20000000.
           05  WS-GEN-CODE                 PIC X(04).
           05  WS-GEN-DRCR                 PIC X(01).
           05  WS-GEN-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-GEN-DESC                 PIC X(40).
           05  WS-GEN-COUNTER              PIC X(17).
           05  WS-GEN-ORIG-ID              PIC X(16).
           05  WS-FEE-CODE                 PIC X(04).
           05  WS-FEE-AMT                  PIC S9(07)V99 COMP-3.
           05  WS-FEE-DESC                 PIC X(40).
           05  WS-PROTECT-OK               PIC X(01).
           05  WS-PROTECT-AVAIL            PIC S9(13)V99 COMP-3.
           05  WS-XFER-AMT                 PIC S9(13)V99 COMP-3.
           05  WS-LAST-HOLD-SEQ            PIC 9(04).
           05  WS-TIER                     PIC 9(01).
           05  WS-RETURN-CODE              PIC 9(02) VALUE 0.
           05  WS-ALRT-TYPE                PIC X(04).
           05  WS-ALRT-SEV                 PIC X(01).
           05  WS-ALRT-RULE                PIC X(08).
           05  WS-ALRT-DESC                PIC X(80).
           05  WS-ALRT-AMT                 PIC S9(13)V99 COMP-3.

       COPY TRANREC.
       COPY TRANREC REPLACING ==TRANSACTION-RECORD== BY
                              ==GEN-TRAN-RECORD==
                              LEADING ==TRAN-== BY ==GEN-==.
       COPY TRANCODE.
       COPY ODDPARM.
       COPY RCCPARM.
       COPY RGDPARM.
       COPY BANKCTL.
       COPY ABNDPARM.

      *----------------------------------------------------------------*
      * DAILY TOTALS                                                   *
      *----------------------------------------------------------------*
       01  WS-TOTALS.
           05  WS-T-READ                   PIC 9(07) VALUE 0.
           05  WS-T-ACCOUNTS               PIC 9(07) VALUE 0.
           05  WS-T-CR-COUNT               PIC 9(07) VALUE 0.
           05  WS-T-CR-AMT                 PIC S9(15)V99 COMP-3
                                                     VALUE 0.
           05  WS-T-DR-COUNT               PIC 9(07) VALUE 0.
           05  WS-T-DR-AMT                 PIC S9(15)V99 COMP-3
                                                     VALUE 0.
           05  WS-T-OD-COUNT               PIC 9(07) VALUE 0.
           05  WS-T-OD-AMT                 PIC S9(15)V99 COMP-3
                                                     VALUE 0.
           05  WS-T-XFER-COUNT             PIC 9(07) VALUE 0.
           05  WS-T-XFER-AMT               PIC S9(15)V99 COMP-3
                                                     VALUE 0.
           05  WS-T-RET-COUNT              PIC 9(07) VALUE 0.
           05  WS-T-RET-AMT                PIC S9(15)V99 COMP-3
                                                     VALUE 0.
           05  WS-T-HOLD-COUNT             PIC 9(07) VALUE 0.
           05  WS-T-HOLD-AMT               PIC S9(15)V99 COMP-3
                                                     VALUE 0.
           05  WS-T-REACTIVATED            PIC 9(07) VALUE 0.
           05  WS-T-NOTFOUND               PIC 9(07) VALUE 0.
           05  WS-T-GENERATED              PIC 9(07) VALUE 0.
           05  WS-T-WAIVED                 PIC 9(07) VALUE 0.
       01  WS-FEE-TOTALS.
           05  WS-FT OCCURS 4 TIMES.
               10  WS-FT-CODE              PIC X(04).
               10  WS-FT-COUNT             PIC 9(07).
               10  WS-FT-AMT               PIC S9(13)V99 COMP-3.
       01  WS-FT-SUB                       PIC 9(01).

       01  WS-RPT-EVENT.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RE-ACCT                     PIC X(12).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RE-CODE                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RE-AMT                      PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RE-EVENT                    PIC X(16).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  RE-REASON                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RE-FEE-CODE                 PIC X(04).
           05  RE-FEE                      PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RE-WAIVE                    PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RE-AVAIL                    PIC -,---,---,--9.99.
       01  WS-RPT-TOTAL.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  RT-LABEL                    PIC X(34).
           05  RT-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  RT-AMT                      PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-RPT-DATE-ED                  PIC 9999/99/99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 1100-READ-TRANEDT
           PERFORM UNTIL WS-EOF
               MOVE TRAN-ACCT-NUMBER TO WS-CUR-ACCT-NUMBER
               MOVE 0 TO WS-BUF-COUNT
               PERFORM UNTIL WS-EOF
                          OR TRAN-ACCT-NUMBER NOT = WS-CUR-ACCT-NUMBER
                   PERFORM 1200-BUFFER-ITEM
                   PERFORM 1100-READ-TRANEDT
               END-PERFORM
               MOVE TRANSACTION-RECORD TO WS-NEXT-TRAN
               PERFORM 2000-PROCESS-ACCOUNT
               MOVE WS-NEXT-TRAN TO TRANSACTION-RECORD
           END-PERFORM
           PERFORM 8000-WRITE-TOTALS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE 'WFOD' TO WS-FT-CODE(1)
           MOVE 'WFNS' TO WS-FT-CODE(2)
           MOVE 'WFOT' TO WS-FT-CODE(3)
           MOVE 'WFRD' TO WS-FT-CODE(4)
           PERFORM VARYING WS-FT-SUB FROM 1 BY 1 UNTIL WS-FT-SUB > 4
               MOVE 0 TO WS-FT-COUNT(WS-FT-SUB) WS-FT-AMT(WS-FT-SUB)
           END-PERFORM
           OPEN INPUT TRANEDT-FILE CUSTOMER-FILE PRODUCT-FILE
           IF WS-EDT-STATUS NOT = '00'
               MOVE 'TRANEDT' TO ABND-FILE-NAME
               MOVE WS-EDT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           IF WS-CUST-STATUS NOT = '00' OR WS-PROD-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN I-O HOLD-FILE
           IF WS-HOLD-STATUS NOT = '00'
               MOVE 'HOLDMST' TO ABND-FILE-NAME
               MOVE WS-HOLD-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND TRANPOST-FILE TRANREJ-FILE ALERT-FILE
           IF (WS-POST-STATUS NOT = '00' AND NOT = '05')
              OR (WS-REJ-STATUS NOT = '00' AND NOT = '05')
              OR (WS-ALRT-STATUS NOT = '00' AND NOT = '05')
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-RPT-DATE-ED
           MOVE SPACES TO REPORT-LINE
           STRING 'CBS  POSTTRAN  DAILY POSTING REPORT'
                  '                BUSINESS DATE ' WS-RPT-DATE-ED
                  DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE '  OVERDRAFT / RETURN / PROTECTION / HOLD EVENTS'
             TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           MOVE 'ACCOUNT'  TO REPORT-LINE(3:7)
           MOVE 'CODE'     TO REPORT-LINE(17:4)
           MOVE 'AMOUNT'   TO REPORT-LINE(30:6)
           MOVE 'EVENT'    TO REPORT-LINE(38:5)
           MOVE 'RSN'      TO REPORT-LINE(55:3)
           MOVE 'FEE'      TO REPORT-LINE(61:3)
           MOVE 'WAIV'     TO REPORT-LINE(81:4)
           MOVE 'AVAIL AFTER' TO REPORT-LINE(91:11)
           WRITE REPORT-LINE.

       1100-READ-TRANEDT.
           READ TRANEDT-FILE INTO TRANSACTION-RECORD
               AT END SET WS-EOF TO TRUE
               NOT AT END ADD 1 TO WS-T-READ
           END-READ.

       1200-BUFFER-ITEM.
           IF WS-BUF-COUNT NOT < 500
               MOVE 'TRANEDT' TO ABND-FILE-NAME
               MOVE 'MORE THAN 500 ITEMS FOR ONE ACCOUNT'
                 TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-BUF-COUNT
           MOVE TRANSACTION-RECORD TO BUF-TRAN(WS-BUF-COUNT)
           IF TRAN-IS-CREDIT
               MOVE 1 TO BUF-CLASS(WS-BUF-COUNT)
           ELSE
               MOVE 2 TO BUF-CLASS(WS-BUF-COUNT)
           END-IF
           PERFORM 8800-LOOKUP-TRAN-CODE
           MOVE WS-TC-PRIORITY TO BUF-PRIO(WS-BUF-COUNT)
           MOVE TRAN-AMOUNT    TO BUF-AMT(WS-BUF-COUNT).

      *----------------------------------------------------------------*
      * ONE ACCOUNT: LOAD MASTER, ORDER ITEMS, POST, REWRITE           *
      *----------------------------------------------------------------*
       2000-PROCESS-ACCOUNT.
           ADD 1 TO WS-T-ACCOUNTS
           PERFORM 2100-ORDER-ITEMS
           MOVE WS-CUR-ACCT-NUMBER TO ACCT-NUMBER
           MOVE 'Y' TO WS-ACCT-FOUND-FLAG
           READ ACCOUNT-FILE
               INVALID KEY MOVE 'N' TO WS-ACCT-FOUND-FLAG
           END-READ
           IF NOT WS-ACCT-FOUND
               PERFORM VARYING WS-K FROM 1 BY 1
                       UNTIL WS-K > WS-BUF-COUNT
                   MOVE BUF-TRAN(WS-ORDER(WS-K)) TO TRANSACTION-RECORD
                   MOVE 'ACNF' TO TRAN-REJECT-CODE
                   MOVE ZERO   TO TRAN-FEE-AMOUNT
                   PERFORM 7500-WRITE-REJECT
                   ADD 1 TO WS-T-NOTFOUND
               END-PERFORM
               MOVE 4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE ACCOUNT-RECORD TO CA-ACCOUNT-REC
           PERFORM 2200-LOAD-CUSTOMER-PRODUCT
           MOVE ZERO TO WS-CHK-DEPOSITS-TODAY

           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-BUF-COUNT
               MOVE BUF-TRAN(WS-ORDER(WS-K)) TO TRANSACTION-RECORD
               PERFORM 8800-LOOKUP-TRAN-CODE
               IF TRAN-IS-CREDIT
                   PERFORM 3000-POST-CREDIT
               ELSE
                   PERFORM 4000-POST-DEBIT
               END-IF
           END-PERFORM

           MOVE CA-ACCOUNT-REC TO ACCOUNT-RECORD
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED - CURRENT ACCOUNT' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
      * POSTING ORDER (INSERTION SORT ON AN INDEX TABLE, STABLE):      *
      *   1. CREDITS BEFORE DEBITS                                     *
      *   2. TC-POST-PRIORITY ASCENDING                                *
      *   3. AMOUNT ASCENDING ("LOW-TO-HIGH")                          *
      *   4. ORDER RECEIVED                                            *
      *----------------------------------------------------------------*
       2100-ORDER-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-BUF-COUNT
               MOVE WS-I TO WS-ORDER(WS-I)
           END-PERFORM
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > WS-BUF-COUNT
               MOVE WS-ORDER(WS-I) TO WS-B
               MOVE WS-I TO WS-J
               MOVE 'Y' TO WS-A-AFTER-B
               PERFORM UNTIL WS-J < 2 OR WS-A-AFTER-B = 'N'
                   MOVE WS-ORDER(WS-J - 1) TO WS-A
                   PERFORM 2150-COMPARE
                   IF WS-A-AFTER-B = 'Y'
                       MOVE WS-A TO WS-ORDER(WS-J)
                       SUBTRACT 1 FROM WS-J
                   END-IF
               END-PERFORM
               MOVE WS-B TO WS-ORDER(WS-J)
           END-PERFORM.

      *    SETS WS-A-AFTER-B = 'Y' WHEN ITEM A MUST POST AFTER ITEM B
       2150-COMPARE.
           MOVE 'N' TO WS-A-AFTER-B
           EVALUATE TRUE
               WHEN BUF-CLASS(WS-A) > BUF-CLASS(WS-B)
                   MOVE 'Y' TO WS-A-AFTER-B
               WHEN BUF-CLASS(WS-A) < BUF-CLASS(WS-B)
                   CONTINUE
               WHEN BUF-PRIO(WS-A) > BUF-PRIO(WS-B)
                   MOVE 'Y' TO WS-A-AFTER-B
               WHEN BUF-PRIO(WS-A) < BUF-PRIO(WS-B)
                   CONTINUE
      *        2010-07-01 DPS CHG0811 - LOW-TO-HIGH. OLD HIGH-TO-LOW:
      *        WHEN BUF-AMT(WS-A) < BUF-AMT(WS-B)
      *            MOVE 'Y' TO WS-A-AFTER-B
               WHEN BUF-AMT(WS-A) > BUF-AMT(WS-B)
                   MOVE 'Y' TO WS-A-AFTER-B
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       2200-LOAD-CUSTOMER-PRODUCT.
           MOVE 'RT' TO WS-CUR-SEGMENT
           MOVE 'N'  TO WS-CUR-EMPLOYEE
           MOVE CA-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'POSTTRAN CUSTOMER NOT FOUND ' CUST-ID
                           ' ACCT=' CA-NUMBER
               NOT INVALID KEY
                   MOVE CUST-SEGMENT       TO WS-CUR-SEGMENT
                   MOVE CUST-EMPLOYEE-FLAG TO WS-CUR-EMPLOYEE
           END-READ
           MOVE 'N' TO WS-CUR-PROD-OD-ALLOWED WS-CUR-PROD-REGD
           MOVE CA-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               INVALID KEY
                   DISPLAY 'POSTTRAN PRODUCT NOT FOUND ' PROD-CODE
               NOT INVALID KEY
                   MOVE PROD-OD-ALLOWED TO WS-CUR-PROD-OD-ALLOWED
                   MOVE PROD-REG-D-FLAG TO WS-CUR-PROD-REGD
           END-READ.

      *----------------------------------------------------------------*
      * CREDITS                                                        *
      *----------------------------------------------------------------*
       3000-POST-CREDIT.
           PERFORM 8300-MEMO-PORTION
           COMPUTE WS-AVAIL-EFFECT = TRAN-AMOUNT - WS-MEMO-PORTION
           SUBTRACT WS-MEMO-PORTION FROM CA-MEMO-CR-TODAY
           ADD TRAN-AMOUNT     TO CA-LEDGER-BAL
           ADD WS-AVAIL-EFFECT TO CA-AVAIL-BAL
           ADD 1 TO CA-CR-COUNT-CYC
      *    MEMO-POSTED TELLER CASH WAS ALREADY COUNTED BY TELLTRN
           IF TRAN-IS-CASH
               ADD WS-AVAIL-EFFECT TO CA-CASH-IN-TODAY
           END-IF
           PERFORM 3500-CUSTOMER-ACTIVITY
           MOVE ZERO TO TRAN-HOLD-DAYS
           MOVE 'N'  TO TRAN-OD-FLAG
           IF TRAN-CODE = 'DCHK'
               PERFORM 5000-REG-CC-HOLDS
           END-IF
           ADD 1 TO WS-T-CR-COUNT
           ADD TRAN-AMOUNT TO WS-T-CR-AMT
           PERFORM 7400-WRITE-POSTED.

      *    LAST ACTIVITY ONLY MOVES FOR CUSTOMER-INITIATED ITEMS.
      *    A DORMANT ACCOUNT IS REACTIVATED BY SUCH AN ITEM (TRANEDIT
      *    HAS ALREADY REJECTED DORMANT DEBITS WITHOUT ID VERIFICATION)
       3500-CUSTOMER-ACTIVITY.
           IF WS-TC-CUST-INIT = 'Y'
               MOVE CTL-BUSINESS-DATE TO CA-LAST-ACTIVITY-DATE
               IF CA-DORMANT
                   SET CA-ACTIVE TO TRUE
                   MOVE ZERO TO CA-DORMANT-DATE
                   ADD 1 TO WS-T-REACTIVATED
                   MOVE 'DORM' TO WS-ALRT-TYPE
                   MOVE 'L'    TO WS-ALRT-SEV
                   MOVE 'DORM0002' TO WS-ALRT-RULE
                   MOVE 'DORMANT ACCOUNT REACTIVATED BY CUSTOMER ITEM'
                     TO WS-ALRT-DESC
                   MOVE TRAN-AMOUNT TO WS-ALRT-AMT
                   PERFORM 7700-WRITE-ALERT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * DEBITS                                                         *
      *----------------------------------------------------------------*
       4000-POST-DEBIT.
           PERFORM 8300-MEMO-PORTION
           PERFORM 4050-BUILD-ODD-PARMS
           CALL 'ODDECIDE' USING ODDECIDE-PARMS
           MOVE ODD-FEES-TODAY TO CA-OD-FEES-TODAY

           IF ODD-RETURN
               PERFORM 4500-RETURN-ITEM
               EXIT PARAGRAPH
           END-IF

           IF ODD-PAY-WITH-XFER AND ODD-XFER-AMOUNT > 0
               PERFORM 4100-PROTECTION-TRANSFER
           END-IF

      *    APPLY THE DEBIT
           COMPUTE WS-AVAIL-EFFECT = TRAN-AMOUNT - WS-MEMO-PORTION
           SUBTRACT WS-MEMO-PORTION FROM CA-MEMO-DR-TODAY
           SUBTRACT TRAN-AMOUNT     FROM CA-LEDGER-BAL
           SUBTRACT WS-AVAIL-EFFECT FROM CA-AVAIL-BAL
           ADD 1 TO CA-DR-COUNT-CYC
           IF TRAN-IS-CASH
               ADD WS-AVAIL-EFFECT TO CA-CASH-OUT-TODAY
           END-IF
           IF TRAN-CODE = 'WATM'
              OR (TRAN-CODE = 'WCSH' AND TRAN-CH-ATM)
               ADD TRAN-AMOUNT TO CA-DAILY-WD-USED
           END-IF
           PERFORM 3500-CUSTOMER-ACTIVITY
           MOVE 'N' TO TRAN-OD-FLAG
           MOVE ZERO TO TRAN-FEE-AMOUNT
           IF ODD-INTO-OVERDRAFT
               MOVE 'Y' TO TRAN-OD-FLAG
               IF ODD-REASON-CODE = 'ODPD'
                   IF CA-OD-COUNT-YTD < 999
                       ADD 1 TO CA-OD-COUNT-YTD
                   END-IF
                   ADD 1 TO WS-T-OD-COUNT
                   ADD TRAN-AMOUNT TO WS-T-OD-AMT
               END-IF
               MOVE ODD-FEE-AMOUNT TO TRAN-FEE-AMOUNT
           END-IF

      *    REG D CONVENIENT WITHDRAWAL COUNT (SV / MM ONLY)
           MOVE WS-CUR-PROD-REGD TO WS-RGD-PROD-REGD
           PERFORM 6000-REG-D-CHECK
           ADD 1 TO WS-T-DR-COUNT
           ADD TRAN-AMOUNT TO WS-T-DR-AMT
           PERFORM 7400-WRITE-POSTED

           IF ODD-INTO-OVERDRAFT AND ODD-REASON-CODE = 'ODPD'
               PERFORM 8600-REPORT-EVENT
           END-IF
           IF ODD-FEE-AMOUNT > 0
               MOVE ODD-FEE-CODE TO WS-FEE-CODE
               MOVE ODD-FEE-AMOUNT TO WS-FEE-AMT
               MOVE 'OVERDRAFT ITEM FEE' TO WS-FEE-DESC
               PERFORM 7000-POST-FEE
           ELSE
               IF ODD-FEE-WAIVE-CODE NOT = SPACES
                   ADD 1 TO WS-T-WAIVED
               END-IF
           END-IF
           IF RGD-FEE-AMOUNT > 0
               MOVE RGD-FEE-CODE   TO WS-FEE-CODE
               MOVE RGD-FEE-AMOUNT TO WS-FEE-AMT
               MOVE 'EXCESS SAVINGS WITHDRAWAL FEE' TO WS-FEE-DESC
               PERFORM 7000-POST-FEE
           END-IF.

       4050-BUILD-ODD-PARMS.
           INITIALIZE ODDECIDE-PARMS
           MOVE TRAN-CODE              TO ODD-TRAN-CODE
           MOVE TRAN-CHANNEL           TO ODD-CHANNEL
           MOVE TRAN-AMOUNT            TO ODD-AMOUNT
      *    A MEMO-POSTED ITEM HAS ALREADY REDUCED AVAILABLE ONLINE
           COMPUTE ODD-AVAIL-BAL = CA-AVAIL-BAL + WS-MEMO-PORTION
           MOVE CA-OD-LIMIT            TO ODD-OD-LIMIT
           MOVE CA-OD-OPT-IN           TO ODD-OD-OPT-IN
           MOVE WS-CUR-PROD-OD-ALLOWED TO ODD-PROD-OD-ALLOWED
           EVALUATE TRUE
               WHEN CA-POOL-ID NOT = SPACES
                   MOVE 'Y' TO ODD-POOL-FLAG
               WHEN CA-PARENT-ACCT NOT = SPACES AND CA-HAS-SWEEP
                   MOVE 'Z' TO ODD-POOL-FLAG
               WHEN OTHER
                   MOVE 'N' TO ODD-POOL-FLAG
           END-EVALUATE
           MOVE CA-NSF-COUNT-12M       TO ODD-NSF-COUNT-12M
           MOVE CA-DAYS-OVERDRAWN      TO ODD-DAYS-OVERDRAWN
           MOVE CA-OPEN-DATE           TO ODD-ACCT-OPEN-DATE
           MOVE CTL-BUSINESS-DATE      TO ODD-BUSINESS-DATE
           MOVE WS-CUR-SEGMENT         TO ODD-CUST-SEGMENT
           MOVE WS-CUR-EMPLOYEE        TO ODD-EMPLOYEE-FLAG
           MOVE CA-OD-FEES-TODAY       TO ODD-FEES-TODAY
           MOVE 'N'                    TO ODD-PROTECT-FLAG
           MOVE ZERO                   TO ODD-PROTECT-AVAIL-BAL
           IF CA-OD-PROTECT-ACCT NOT = SPACES
               MOVE CA-OD-PROTECT-ACCT TO ACCT-NUMBER
               READ ACCOUNT-FILE
                   INVALID KEY
                       DISPLAY 'POSTTRAN OD PROTECT ACCT NOT FOUND '
                               CA-OD-PROTECT-ACCT
                   NOT INVALID KEY
                       IF ACCT-ACTIVE AND ACCT-AVAIL-BAL > 0
                           MOVE 'Y' TO ODD-PROTECT-FLAG
                           MOVE ACCT-AVAIL-BAL
                             TO ODD-PROTECT-AVAIL-BAL
                       END-IF
               END-READ
           END-IF.

      *----------------------------------------------------------------*
      * OD PROTECTION TRANSFER: WXFR ON THE PROTECTING ACCOUNT (WHICH  *
      * COUNTS AS A REG D CONVENIENT WITHDRAWAL), DXFR ON THIS ACCOUNT,*
      * TRANSFER FEE WFOT ON THIS ACCOUNT.                             *
      *----------------------------------------------------------------*
       4100-PROTECTION-TRANSFER.
           MOVE ODD-XFER-AMOUNT TO WS-XFER-AMT
           MOVE CA-ACCOUNT-REC TO WS-SAVE-ACCT
           MOVE CA-OD-PROTECT-ACCT TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   MOVE 'ACCTMST' TO ABND-FILE-NAME
                   MOVE 'OD PROTECT ACCOUNT VANISHED' TO ABND-MESSAGE
                   PERFORM 9900-ABEND
           END-READ
           MOVE ACCOUNT-RECORD TO CA-ACCOUNT-REC
           MOVE 'N' TO WS-RGD-PROD-REGD
           MOVE CA-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               NOT INVALID KEY
                   MOVE PROD-REG-D-FLAG TO WS-RGD-PROD-REGD
           END-READ

           SUBTRACT WS-XFER-AMT FROM CA-LEDGER-BAL CA-AVAIL-BAL
           ADD 1 TO CA-DR-COUNT-CYC
           MOVE 'WXFR'         TO WS-GEN-CODE
           MOVE 'D'            TO WS-GEN-DRCR
           MOVE WS-XFER-AMT    TO WS-GEN-AMT
           MOVE 'OD PROTECTION TRANSFER OUT' TO WS-GEN-DESC
           MOVE WS-CUR-ACCT-NUMBER TO WS-GEN-COUNTER
           PERFORM 7300-WRITE-GENERATED
      *    REG D ON THE PROTECTING ACCOUNT - GENERATED WXFR IS 'BAT'
           INITIALIZE REGDCHK-PARMS
           MOVE CA-CATEGORY        TO RGD-ACCT-CATEGORY
           MOVE WS-RGD-PROD-REGD   TO RGD-PROD-REGD-FLAG
           MOVE 'WXFR'             TO RGD-TRAN-CODE
           MOVE 'Y'                TO RGD-TRAN-REGD-FLAG
           MOVE 'BAT'              TO RGD-CHANNEL
           MOVE CA-REGD-WD-COUNT   TO RGD-WD-COUNT
           MOVE CA-REGD-VIOLATIONS-12M TO RGD-VIOLATIONS-12M
           CALL 'REGDCHK' USING REGDCHK-PARMS
           MOVE RGD-WD-COUNT       TO CA-REGD-WD-COUNT
           MOVE RGD-VIOLATIONS-12M TO CA-REGD-VIOLATIONS-12M
           IF RGD-FEE-AMOUNT > 0
               MOVE RGD-FEE-CODE   TO WS-FEE-CODE
               MOVE RGD-FEE-AMOUNT TO WS-FEE-AMT
               MOVE 'EXCESS SAVINGS WITHDRAWAL FEE' TO WS-FEE-DESC
               PERFORM 7000-POST-FEE
           END-IF
           IF RGD-RECOMMEND-CONVERT
               PERFORM 6100-REG-D-ALERT
           END-IF
           MOVE CA-ACCOUNT-REC TO ACCOUNT-RECORD
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED - OD PROTECT ACCOUNT'
                 TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE WS-SAVE-ACCT TO CA-ACCOUNT-REC

      *    CREDIT SIDE ON THE OVERDRAWING ACCOUNT
           ADD WS-XFER-AMT TO CA-LEDGER-BAL CA-AVAIL-BAL
           ADD 1 TO CA-CR-COUNT-CYC
           MOVE 'DXFR'         TO WS-GEN-CODE
           MOVE 'C'            TO WS-GEN-DRCR
           MOVE WS-XFER-AMT    TO WS-GEN-AMT
           MOVE 'OD PROTECTION TRANSFER IN' TO WS-GEN-DESC
           MOVE CA-OD-PROTECT-ACCT TO WS-GEN-COUNTER
           PERFORM 7300-WRITE-GENERATED
           ADD 1 TO WS-T-XFER-COUNT
           ADD WS-XFER-AMT TO WS-T-XFER-AMT
           IF ODD-XFER-FEE > 0
               MOVE 'WFOT'       TO WS-FEE-CODE
               MOVE ODD-XFER-FEE TO WS-FEE-AMT
               MOVE 'OD PROTECTION TRANSFER FEE' TO WS-FEE-DESC
               PERFORM 7000-POST-FEE
           END-IF
           PERFORM 8600-REPORT-EVENT.

      *----------------------------------------------------------------*
      * RETURNED ITEM: TO TRANREJ AS 'NSF ', NSF FEE IF ANY            *
      *----------------------------------------------------------------*
       4500-RETURN-ITEM.
           MOVE 'NSF ' TO TRAN-REJECT-CODE
           MOVE ODD-FEE-AMOUNT TO TRAN-FEE-AMOUNT
           PERFORM 7500-WRITE-REJECT
           ADD 1 TO WS-T-RET-COUNT
           ADD TRAN-AMOUNT TO WS-T-RET-AMT
      *    A REG E DECLINE IS NOT AN NSF EVENT
           IF ODD-REASON-CODE NOT = 'REGE'
               IF CA-NSF-COUNT-12M < 999
                   ADD 1 TO CA-NSF-COUNT-12M
               END-IF
           END-IF
           PERFORM 8600-REPORT-EVENT
           IF ODD-FEE-AMOUNT > 0
               MOVE ODD-FEE-CODE   TO WS-FEE-CODE
               MOVE ODD-FEE-AMOUNT TO WS-FEE-AMT
               MOVE 'RETURNED ITEM NSF FEE' TO WS-FEE-DESC
               PERFORM 7000-POST-FEE
           ELSE
               IF ODD-FEE-WAIVE-CODE NOT = SPACES
                   ADD 1 TO WS-T-WAIVED
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * REG CC HOLDS ON CHECK DEPOSITS - ONE HOLDMST RECORD PER TIER   *
      *----------------------------------------------------------------*
       5000-REG-CC-HOLDS.
           INITIALIZE REGCCHLD-PARMS
           MOVE TRAN-CODE             TO RCC-TRAN-CODE
           MOVE TRAN-CHANNEL          TO RCC-CHANNEL
           MOVE TRAN-AMOUNT           TO RCC-AMOUNT
           MOVE WS-CHK-DEPOSITS-TODAY TO RCC-DEPOSITS-TODAY
           MOVE CTL-BUSINESS-DATE     TO RCC-BUSINESS-DATE
           MOVE CA-OPEN-DATE          TO RCC-ACCT-OPEN-DATE
           MOVE CA-NSF-COUNT-12M      TO RCC-NSF-COUNT-12M
           MOVE WS-CUR-SEGMENT        TO RCC-CUST-SEGMENT
           CALL 'REGCCHLD' USING REGCCHLD-PARMS
           ADD TRAN-AMOUNT TO WS-CHK-DEPOSITS-TODAY
           IF RCC-HOLD-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           PERFORM 5100-FIND-LAST-HOLD-SEQ
           PERFORM VARYING WS-TIER FROM 1 BY 1
                   UNTIL WS-TIER > RCC-HOLD-COUNT
               INITIALIZE HOLD-RECORD
               ADD 1 TO WS-LAST-HOLD-SEQ
               MOVE CA-NUMBER                 TO HOLD-ACCT-NUMBER
               MOVE WS-LAST-HOLD-SEQ          TO HOLD-SEQ
               MOVE RCC-HOLD-TYPE(WS-TIER)    TO HOLD-TYPE
               MOVE RCC-HOLD-AMT(WS-TIER)     TO HOLD-AMOUNT
               MOVE CTL-BUSINESS-DATE         TO HOLD-PLACED-DATE
               MOVE RCC-RELEASE-DATE(WS-TIER) TO HOLD-RELEASE-DATE
               SET HOLD-ACTIVE TO TRUE
               MOVE RCC-HOLD-REASON(WS-TIER)  TO HOLD-REASON
               MOVE TRAN-REFERENCE            TO HOLD-TRAN-REF
               MOVE 'POSTTRAN'                TO HOLD-PLACED-BY
               WRITE HOLD-RECORD
               IF WS-HOLD-STATUS NOT = '00'
                   MOVE 'HOLDMST' TO ABND-FILE-NAME
                   MOVE WS-HOLD-STATUS TO ABND-FILE-STATUS
                   MOVE 'WRITE FAILED - HOLDMST' TO ABND-MESSAGE
                   PERFORM 9900-ABEND
               END-IF
               ADD 1 TO WS-T-HOLD-COUNT
               ADD RCC-HOLD-AMT(WS-TIER) TO WS-T-HOLD-AMT
           END-PERFORM
           ADD RCC-TOTAL-HOLD-AMT      TO CA-HOLD-AMT
           SUBTRACT RCC-TOTAL-HOLD-AMT FROM CA-AVAIL-BAL
           MOVE RCC-MAX-HOLD-DAYS      TO TRAN-HOLD-DAYS
           MOVE SPACES TO WS-RPT-EVENT
           MOVE CA-NUMBER    TO RE-ACCT
           MOVE TRAN-CODE    TO RE-CODE
           MOVE TRAN-AMOUNT  TO RE-AMT
           MOVE 'REG CC HOLD'  TO RE-EVENT
           MOVE RCC-EXCEPTION-CODE TO RE-REASON
           MOVE RCC-TOTAL-HOLD-AMT TO RE-FEE
           MOVE 'HOLD' TO RE-FEE-CODE
           COMPUTE RE-AVAIL = CA-AVAIL-BAL
           WRITE REPORT-LINE FROM WS-RPT-EVENT.

       5100-FIND-LAST-HOLD-SEQ.
           MOVE ZERO TO WS-LAST-HOLD-SEQ
           MOVE CA-NUMBER TO HOLD-ACCT-NUMBER
           MOVE ZERO      TO HOLD-SEQ
           MOVE 'N' TO WS-HOLD-SCAN-DONE
           START HOLD-FILE KEY IS NOT LESS THAN HOLD-KEY
               INVALID KEY MOVE 'Y' TO WS-HOLD-SCAN-DONE
           END-START
           PERFORM UNTIL WS-HOLD-SCAN-DONE = 'Y'
               READ HOLD-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-HOLD-SCAN-DONE
                   NOT AT END
                       IF HOLD-ACCT-NUMBER = CA-NUMBER
                           MOVE HOLD-SEQ TO WS-LAST-HOLD-SEQ
                       ELSE
                           MOVE 'Y' TO WS-HOLD-SCAN-DONE
                       END-IF
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
      * REG D - COUNT THE WITHDRAWAL ON THE CURRENT ACCOUNT            *
      *----------------------------------------------------------------*
       6000-REG-D-CHECK.
           INITIALIZE REGDCHK-PARMS
           MOVE CA-CATEGORY        TO RGD-ACCT-CATEGORY
           MOVE WS-RGD-PROD-REGD   TO RGD-PROD-REGD-FLAG
           MOVE TRAN-CODE          TO RGD-TRAN-CODE
           MOVE TRAN-REGD-FLAG     TO RGD-TRAN-REGD-FLAG
           MOVE TRAN-CHANNEL       TO RGD-CHANNEL
           MOVE CA-REGD-WD-COUNT   TO RGD-WD-COUNT
           MOVE CA-REGD-VIOLATIONS-12M TO RGD-VIOLATIONS-12M
           CALL 'REGDCHK' USING REGDCHK-PARMS
           MOVE RGD-WD-COUNT       TO CA-REGD-WD-COUNT
           MOVE RGD-VIOLATIONS-12M TO CA-REGD-VIOLATIONS-12M
           IF RGD-COUNTED-FLAG = 'Y'
               MOVE 'Y' TO TRAN-REGD-FLAG
           ELSE
               MOVE 'N' TO TRAN-REGD-FLAG
           END-IF
           IF RGD-RECOMMEND-CONVERT
               PERFORM 6100-REG-D-ALERT
           END-IF.

       6100-REG-D-ALERT.
           MOVE 'LIMT' TO WS-ALRT-TYPE
           MOVE 'M'    TO WS-ALRT-SEV
           MOVE 'REGD0003' TO WS-ALRT-RULE
           MOVE SPACES TO WS-ALRT-DESC
           STRING 'REG D: ' CA-REGD-VIOLATIONS-12M
                  ' EXCESS-WITHDRAWAL CYCLES IN 12M - RECOMMEND '
                  'CONVERT TO CHECKING' DELIMITED BY SIZE
             INTO WS-ALRT-DESC
           MOVE TRAN-AMOUNT TO WS-ALRT-AMT
           PERFORM 7700-WRITE-ALERT.

      *----------------------------------------------------------------*
      * FEE POSTING - FORCE POSTED ON THE ACCOUNT IN CA-ACCOUNT-REC    *
      *----------------------------------------------------------------*
       7000-POST-FEE.
           SUBTRACT WS-FEE-AMT FROM CA-LEDGER-BAL CA-AVAIL-BAL
           ADD WS-FEE-AMT TO CA-FEES-MTD CA-FEES-YTD
           MOVE WS-FEE-CODE TO WS-GEN-CODE
           MOVE 'D'         TO WS-GEN-DRCR
           MOVE WS-FEE-AMT  TO WS-GEN-AMT
           MOVE WS-FEE-DESC TO WS-GEN-DESC
           MOVE SPACES      TO WS-GEN-COUNTER
           PERFORM 7300-WRITE-GENERATED
           PERFORM VARYING WS-FT-SUB FROM 1 BY 1 UNTIL WS-FT-SUB > 4
               IF WS-FT-CODE(WS-FT-SUB) = WS-FEE-CODE
                   ADD 1 TO WS-FT-COUNT(WS-FT-SUB)
                   ADD WS-FEE-AMT TO WS-FT-AMT(WS-FT-SUB)
               END-IF
           END-PERFORM.

      *    GENERATED ITEM FOR CA-ACCOUNT-REC, TRIGGERED BY CURRENT ITEM
       7300-WRITE-GENERATED.
           INITIALIZE GEN-TRAN-RECORD
           ADD 1 TO WS-GEN-SEQ
           MOVE CTL-BUSINESS-DATE   TO GEN-ID-DATE
           MOVE WS-GEN-SEQ          TO GEN-ID-SEQ
           MOVE CA-NUMBER           TO GEN-ACCT-NUMBER
           MOVE CTL-BUSINESS-DATE   TO GEN-EFFECTIVE-DATE
                                       GEN-POST-DATE
           MOVE TRAN-TIME           TO GEN-TIME
           MOVE WS-GEN-CODE         TO GEN-CODE
           MOVE WS-GEN-DRCR         TO GEN-DR-CR
           MOVE WS-GEN-AMT          TO GEN-AMOUNT
           MOVE 'BAT'               TO GEN-CHANNEL
           MOVE WS-GEN-DESC         TO GEN-DESCRIPTION
           MOVE TRAN-REFERENCE      TO GEN-REFERENCE
           MOVE WS-GEN-COUNTER      TO GEN-COUNTER-ACCT
           SET GEN-POSTED TO TRUE
           MOVE TRAN-ID             TO GEN-ORIG-TRAN-ID
           MOVE 'POSTTRAN'          TO GEN-USER-ID
           MOVE CA-COST-CENTER      TO GEN-COST-CENTER
           MOVE 'N'                 TO GEN-CASH-FLAG GEN-OD-FLAG
                                       GEN-REGD-FLAG
           IF WS-GEN-CODE = 'WXFR'
               MOVE 'Y' TO GEN-REGD-FLAG
           END-IF
           IF WS-GEN-DRCR = 'D' AND CA-LEDGER-BAL < 0
               MOVE 'Y' TO GEN-OD-FLAG
           END-IF
           MOVE 'POSTTRAN'          TO GEN-SOURCE-PGM
           WRITE TRANPOST-REC FROM GEN-TRAN-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-T-GENERATED.

       7400-WRITE-POSTED.
           SET TRAN-POSTED TO TRUE
           MOVE CTL-BUSINESS-DATE TO TRAN-POST-DATE
           MOVE 'POSTTRAN'        TO TRAN-SOURCE-PGM
           WRITE TRANPOST-REC FROM TRANSACTION-RECORD
           IF WS-POST-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-POST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

       7500-WRITE-REJECT.
           SET TRAN-REJECTED TO TRUE
           MOVE CTL-BUSINESS-DATE TO TRAN-POST-DATE
           MOVE 'POSTTRAN'        TO TRAN-SOURCE-PGM
           WRITE TRANREJ-REC FROM TRANSACTION-RECORD
           IF WS-REJ-STATUS NOT = '00'
               MOVE 'TRANREJ' TO ABND-FILE-NAME
               MOVE WS-REJ-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

       7700-WRITE-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CTL-BUSINESS-DATE  TO ALRT-DATE
           MOVE TRAN-TIME          TO ALRT-TIME
           MOVE WS-ALRT-TYPE       TO ALRT-TYPE
           MOVE WS-ALRT-SEV        TO ALRT-SEVERITY
           MOVE CA-PRIMARY-CUST-ID TO ALRT-CUST-ID
           MOVE CA-NUMBER          TO ALRT-ACCT-NUMBER
           MOVE WS-ALRT-AMT        TO ALRT-AMOUNT
           MOVE WS-ALRT-RULE       TO ALRT-RULE-ID
           MOVE WS-ALRT-DESC       TO ALRT-DESCRIPTION
           SET ALRT-OPEN TO TRUE
           MOVE 'POSTTRAN'         TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
      * PORTION OF THIS ITEM ALREADY MEMO-POSTED BY AN ONLINE          *
      * TRANSACTION (XFERFUND / TELLTRN) - ONLY FOR ONLINE CHANNELS    *
      *----------------------------------------------------------------*
       8300-MEMO-PORTION.
           MOVE ZERO TO WS-MEMO-PORTION
           IF TRAN-CH-ONLINE OR TRAN-CH-MOBILE OR TRAN-CH-BRANCH
              OR TRAN-CH-API
               IF TRAN-IS-CREDIT
                   IF CA-MEMO-CR-TODAY > 0
                       IF CA-MEMO-CR-TODAY < TRAN-AMOUNT
                           MOVE CA-MEMO-CR-TODAY TO WS-MEMO-PORTION
                       ELSE
                           MOVE TRAN-AMOUNT TO WS-MEMO-PORTION
                       END-IF
                   END-IF
               ELSE
                   IF CA-MEMO-DR-TODAY > 0
                       IF CA-MEMO-DR-TODAY < TRAN-AMOUNT
                           MOVE CA-MEMO-DR-TODAY TO WS-MEMO-PORTION
                       ELSE
                           MOVE TRAN-AMOUNT TO WS-MEMO-PORTION
                       END-IF
                   END-IF
               END-IF
           END-IF.

       8600-REPORT-EVENT.
           MOVE SPACES TO WS-RPT-EVENT
           MOVE CA-NUMBER       TO RE-ACCT
           MOVE TRAN-CODE       TO RE-CODE
           MOVE TRAN-AMOUNT     TO RE-AMT
           EVALUATE TRUE
               WHEN ODD-RETURN
                   MOVE 'RETURNED NSF'     TO RE-EVENT
               WHEN ODD-PAY-WITH-XFER
                   MOVE 'PAID - OD XFER'   TO RE-EVENT
               WHEN OTHER
                   MOVE 'PAID INTO OD'     TO RE-EVENT
           END-EVALUATE
           MOVE ODD-REASON-CODE TO RE-REASON
           IF ODD-PAY-WITH-XFER AND ODD-FEE-AMOUNT = 0
               MOVE 'WFOT'       TO RE-FEE-CODE
               MOVE ODD-XFER-FEE TO RE-FEE
           ELSE
               MOVE ODD-FEE-CODE    TO RE-FEE-CODE
               MOVE ODD-FEE-AMOUNT  TO RE-FEE
           END-IF
           MOVE ODD-FEE-WAIVE-CODE TO RE-WAIVE
           MOVE ODD-RESULT-AVAIL   TO RE-AVAIL
           WRITE REPORT-LINE FROM WS-RPT-EVENT.

       8800-LOOKUP-TRAN-CODE.
           MOVE 'N' TO WS-TC-CUST-INIT
           MOVE 9   TO WS-TC-PRIORITY
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END CONTINUE
               WHEN TC-CODE(TC-IDX) = TRAN-CODE
                   MOVE TC-CUST-INIT-FLAG(TC-IDX) TO WS-TC-CUST-INIT
                   MOVE TC-POST-PRIORITY(TC-IDX)  TO WS-TC-PRIORITY
           END-SEARCH.

      *----------------------------------------------------------------*
      * DAILY TOTALS                                                   *
      *----------------------------------------------------------------*
       8000-WRITE-TOTALS.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE '  DAILY POSTING TOTALS                    COUNT'
             TO REPORT-LINE
           MOVE 'AMOUNT' TO REPORT-LINE(63:6)
           WRITE REPORT-LINE
           MOVE 'ITEMS READ FROM TRANEDT' TO RT-LABEL
           MOVE WS-T-READ TO RT-COUNT
           MOVE ZERO TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'ACCOUNTS PROCESSED' TO RT-LABEL
           MOVE WS-T-ACCOUNTS TO RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'CREDITS POSTED' TO RT-LABEL
           MOVE WS-T-CR-COUNT TO RT-COUNT
           MOVE WS-T-CR-AMT TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'DEBITS POSTED' TO RT-LABEL
           MOVE WS-T-DR-COUNT TO RT-COUNT
           MOVE WS-T-DR-AMT TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE '  OF WHICH PAID INTO OVERDRAFT' TO RT-LABEL
           MOVE WS-T-OD-COUNT TO RT-COUNT
           MOVE WS-T-OD-AMT TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'OD PROTECTION TRANSFERS' TO RT-LABEL
           MOVE WS-T-XFER-COUNT TO RT-COUNT
           MOVE WS-T-XFER-AMT TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'ITEMS RETURNED (NSF)' TO RT-LABEL
           MOVE WS-T-RET-COUNT TO RT-COUNT
           MOVE WS-T-RET-AMT TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'REG CC HOLDS PLACED' TO RT-LABEL
           MOVE WS-T-HOLD-COUNT TO RT-COUNT
           MOVE WS-T-HOLD-AMT TO RT-AMT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           PERFORM VARYING WS-FT-SUB FROM 1 BY 1 UNTIL WS-FT-SUB > 4
               MOVE SPACES TO RT-LABEL
               STRING 'FEES ASSESSED ' WS-FT-CODE(WS-FT-SUB)
                      DELIMITED BY SIZE INTO RT-LABEL
               MOVE WS-FT-COUNT(WS-FT-SUB) TO RT-COUNT
               MOVE WS-FT-AMT(WS-FT-SUB) TO RT-AMT
               WRITE REPORT-LINE FROM WS-RPT-TOTAL
           END-PERFORM
           MOVE ZERO TO RT-AMT
           MOVE 'OD/NSF FEES WAIVED' TO RT-LABEL
           MOVE WS-T-WAIVED TO RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'DORMANT ACCOUNTS REACTIVATED' TO RT-LABEL
           MOVE WS-T-REACTIVATED TO RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'GENERATED ITEMS (20000001+)' TO RT-LABEL
           MOVE WS-T-GENERATED TO RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'ITEMS FOR ACCOUNTS NOT ON FILE' TO RT-LABEL
           MOVE WS-T-NOTFOUND TO RT-COUNT
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           DISPLAY 'POSTTRAN READ=' WS-T-READ
                   ' ACCOUNTS=' WS-T-ACCOUNTS
                   ' CR=' WS-T-CR-COUNT ' DR=' WS-T-DR-COUNT
                   ' OD=' WS-T-OD-COUNT ' XFER=' WS-T-XFER-COUNT
                   ' RETURNED=' WS-T-RET-COUNT
                   ' HOLDS=' WS-T-HOLD-COUNT
                   ' GENERATED=' WS-T-GENERATED.

       9000-TERMINATE.
           CLOSE TRANEDT-FILE ACCOUNT-FILE CUSTOMER-FILE PRODUCT-FILE
                 HOLD-FILE TRANPOST-FILE TRANREJ-FILE ALERT-FILE
                 REPORT-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE.

       9900-ABEND.
           MOVE 'POSTTRAN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           IF ABND-MESSAGE = SPACES
               MOVE 'FILE ERROR' TO ABND-MESSAGE
           END-IF
           CALL 'ABENDRTN' USING ABEND-PARMS.
