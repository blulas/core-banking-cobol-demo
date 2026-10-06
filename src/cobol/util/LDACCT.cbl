      *================================================================*
      * PROGRAM  : LDACCT                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE ACCOUNT MASTER (ACCTMST) FROM A PIPE-     *
      *            DELIMITED EXTRACT (ACCTSEED). CATEGORY IS TAKEN     *
      *            FROM THE PRODUCT MASTER. FIELDS NOT PRESENT IN THE  *
      *            LEGACY EXTRACT ARE DEFAULTED PER CONVERSION SPEC    *
      *            CNV-0117.                                           *
      * INPUT    : ACCTSEED  ACCT|PROD|CUST|JOINT|OWN|STATUS|RESTRICT| *
      *              OPEN|LASTACT|DORMANT|ESCHEATST|LEDGER|ODLIMIT|    *
      *              ODOPTIN|ODPROTECT|ODCOUNTYTD|NSF12M|RATE|PROMORATE*
      *              |PROMOEXP|TERM|MATURITY|ORIGDEP|RENEW|PAYOUT|     *
      *              STMTDAY|PARENT|VIRTUAL|SWEEPFLAG|SWEEPID|POOLID|  *
      *              ANALYSIS|REGDWD|BACKUPWH|COSTCENTER|DAYSOD|       *
      *              INTPAIDYTD                                        *
      *            PRODMST   (KSDS)                                    *
      * OUTPUT   : ACCTMST   (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      * 2015-11-19 DPS  CNV-0117 DEFAULT DAILY LIMITS                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDACCT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "ACCTSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(600).
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-PROD-STATUS                  PIC X(02).
       01  WS-ACCT-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(40) OCCURS 37.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT SEED-FILE PRODUCT-FILE
           OPEN OUTPUT ACCOUNT-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-PROD-STATUS NOT = '00'
              OR WS-ACCT-STATUS NOT = '00'
               MOVE 'LDACCT' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - ACCTSEED/PRODMST/ACCTMST'
                 TO ABND-MESSAGE
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE PRODUCT-FILE ACCOUNT-FILE
           DISPLAY 'LDACCT  READ=' WS-READ-COUNT
                   ' LOADED=' WS-LOAD-COUNT ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           END-IF
           STOP RUN.

       1000-PROCESS-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-READ-COUNT
           MOVE SPACES TO WS-FIELDS
           UNSTRING SEED-LINE DELIMITED BY '|'
               INTO WS-F(1)  WS-F(2)  WS-F(3)  WS-F(4)  WS-F(5)
                    WS-F(6)  WS-F(7)  WS-F(8)  WS-F(9)  WS-F(10)
                    WS-F(11) WS-F(12) WS-F(13) WS-F(14) WS-F(15)
                    WS-F(16) WS-F(17) WS-F(18) WS-F(19) WS-F(20)
                    WS-F(21) WS-F(22) WS-F(23) WS-F(24) WS-F(25)
                    WS-F(26) WS-F(27) WS-F(28) WS-F(29) WS-F(30)
                    WS-F(31) WS-F(32) WS-F(33) WS-F(34) WS-F(35)
                    WS-F(36) WS-F(37)
           END-UNSTRING
           INITIALIZE ACCOUNT-RECORD
           MOVE WS-F(1)  TO ACCT-NUMBER
           MOVE WS-F(2)  TO ACCT-PRODUCT-CODE
           MOVE WS-F(3)  TO ACCT-PRIMARY-CUST-ID
           MOVE WS-F(4)  TO ACCT-JOINT-CUST-ID
           MOVE WS-F(5)  TO ACCT-OWNERSHIP-TYPE
           MOVE WS-F(6)  TO ACCT-STATUS
           MOVE WS-F(7)  TO ACCT-RESTRICT-REASON
           COMPUTE ACCT-OPEN-DATE          = FUNCTION NUMVAL(WS-F(8))
           COMPUTE ACCT-LAST-ACTIVITY-DATE = FUNCTION NUMVAL(WS-F(9))
           COMPUTE ACCT-DORMANT-DATE       = FUNCTION NUMVAL(WS-F(10))
           MOVE WS-F(11) TO ACCT-ESCHEAT-STATE
           COMPUTE ACCT-LEDGER-BAL         = FUNCTION NUMVAL(WS-F(12))
           COMPUTE ACCT-OD-LIMIT           = FUNCTION NUMVAL(WS-F(13))
           MOVE WS-F(14) TO ACCT-OD-OPT-IN
           MOVE WS-F(15) TO ACCT-OD-PROTECT-ACCT
           COMPUTE ACCT-OD-COUNT-YTD       = FUNCTION NUMVAL(WS-F(16))
           COMPUTE ACCT-NSF-COUNT-12M      = FUNCTION NUMVAL(WS-F(17))
           COMPUTE ACCT-INT-RATE           = FUNCTION NUMVAL(WS-F(18))
           COMPUTE ACCT-PROMO-RATE         = FUNCTION NUMVAL(WS-F(19))
           COMPUTE ACCT-PROMO-EXP-DATE     = FUNCTION NUMVAL(WS-F(20))
           COMPUTE ACCT-TERM-MONTHS        = FUNCTION NUMVAL(WS-F(21))
           COMPUTE ACCT-MATURITY-DATE      = FUNCTION NUMVAL(WS-F(22))
           COMPUTE ACCT-ORIG-DEPOSIT-AMT   = FUNCTION NUMVAL(WS-F(23))
           MOVE WS-F(24) TO ACCT-RENEWAL-OPTION
           MOVE WS-F(25) TO ACCT-PAYOUT-ACCT
           COMPUTE ACCT-STMT-CYCLE-DAY     = FUNCTION NUMVAL(WS-F(26))
           MOVE WS-F(27) TO ACCT-PARENT-ACCT
           MOVE WS-F(28) TO ACCT-VIRTUAL-FLAG
           MOVE WS-F(29) TO ACCT-SWEEP-FLAG
           MOVE WS-F(30) TO ACCT-SWEEP-ID
           MOVE WS-F(31) TO ACCT-POOL-ID
           MOVE WS-F(32) TO ACCT-ANALYSIS-FLAG
           COMPUTE ACCT-REGD-WD-COUNT      = FUNCTION NUMVAL(WS-F(33))
           MOVE WS-F(34) TO ACCT-BACKUP-WH-FLAG
           MOVE WS-F(35) TO ACCT-COST-CENTER
           COMPUTE ACCT-DAYS-OVERDRAWN     = FUNCTION NUMVAL(WS-F(36))
           COMPUTE ACCT-INT-PAID-YTD       = FUNCTION NUMVAL(WS-F(37))
           PERFORM 2000-APPLY-DEFAULTS
           WRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDACCT  WRITE ERROR ' WS-ACCT-STATUS
                       ' KEY=' ACCT-NUMBER
           END-IF.

      *----------------------------------------------------------------*
      * CNV-0117 CONVERSION DEFAULTS                                   *
      *----------------------------------------------------------------*
       2000-APPLY-DEFAULTS.
           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               INVALID KEY
                   DISPLAY 'LDACCT  UNKNOWN PRODUCT ' PROD-CODE
                           ' ACCT=' ACCT-NUMBER
                   MOVE 'XX' TO ACCT-CATEGORY
               NOT INVALID KEY
                   MOVE PROD-CATEGORY TO ACCT-CATEGORY
           END-READ
           MOVE 'USD'              TO ACCT-CURRENCY
           MOVE ACCT-LEDGER-BAL    TO ACCT-AVAIL-BAL
                                      ACCT-PREV-DAY-LEDGER-BAL
                                      ACCT-MIN-BAL-CYC
           MOVE ZERO               TO ACCT-HOLD-AMT
                                      ACCT-MEMO-DR-TODAY
                                      ACCT-MEMO-CR-TODAY
                                      ACCT-ACCRUED-INT
           MOVE ACCT-LEDGER-BAL    TO ACCT-BAL-ACCUM-CYC
           MOVE 1                  TO ACCT-DAYS-IN-CYCLE
           MOVE CTL-PREV-BUS-DATE  TO ACCT-LAST-ACCRUAL-DATE
           IF ACCT-STMT-CYCLE-DAY = 0
               MOVE 31 TO ACCT-STMT-CYCLE-DAY
           END-IF
           MOVE 'M'                TO ACCT-STMT-FREQ
           MOVE 'Y'                TO ACCT-ESTMT-FLAG
           IF ACCT-OWN-CORPORATE
               MOVE 0              TO ACCT-DAILY-WD-LIMIT
               MOVE 50000000.00    TO ACCT-DAILY-ACH-LIMIT
               MOVE 250000000.00   TO ACCT-DAILY-WIRE-LIMIT
           ELSE
               MOVE 1000.00        TO ACCT-DAILY-WD-LIMIT
               MOVE 25000.00       TO ACCT-DAILY-ACH-LIMIT
               MOVE 100000.00      TO ACCT-DAILY-WIRE-LIMIT
           END-IF
           MOVE ACCT-OPEN-DATE     TO ACCT-LAST-MAINT-DATE
           MOVE 'CONVERT'          TO ACCT-LAST-MAINT-USER.
