      *================================================================*
      * PROGRAM  : INTACCR                                             *
      * TYPE     : BATCH - EOD STEP 11                                 *
      * PURPOSE  : DAILY DEPOSIT INTEREST ACCRUAL AND CYCLE STATISTICS.*
      *            FOR EVERY OPEN DEPOSIT ACCOUNT:                     *
      *            1. RE-PRICE THE ACCOUNT VIA RATETIER (NOT CDS - A   *
      *               CD KEEPS ITS CONTRACT RATE UNTIL RENEWAL) AND    *
      *               LOG EVERY RATE CHANGE / PROMO EXPIRY             *
      *            2. ACCRUE ONE DAY OF INTEREST VIA INTCALC FOR EACH  *
      *               CALENDAR DAY SINCE ACCT-LAST-ACCRUAL-DATE - SO   *
      *               WEEKENDS AND HOLIDAYS ARE ACCRUED BY THE NEXT    *
      *               BUSINESS DAY RUN. ON MONTH-END THE RUN ACCRUES   *
      *               THROUGH THE LAST CALENDAR DAY OF THE MONTH SO    *
      *               INTCAP PAYS A FULL MONTH.                        *
      *            3. CDS ACCRUE UP TO THE DAY BEFORE MATURITY (THE    *
      *               MATURITY DAY BELONGS TO THE RENEWED TERM)        *
      *            4. ROLL THE CYCLE ACCUMULATORS USED BY FEEASSES AND *
      *               STATEMENTS (BALANCE ACCUMULATOR, DAYS IN CYCLE,  *
      *               CYCLE MINIMUM BALANCE) FOR ALL DEPOSIT ACCOUNTS, *
      *               INTEREST BEARING OR NOT.                         *
      *            INTEREST CONTINUES TO ACCRUE ON DORMANT, FROZEN     *
      *            (ESTATE) AND OFAC-RESTRICTED (BLOCKED PROPERTY -    *
      *            31 CFR 501.603) ACCOUNTS.                           *
      * INPUT    : BANKCTL (VIA GETCTL), PRODMST, CUSTMST              *
      * I-O      : ACCTMST                                             *
      * OUTPUT   : RATECHG  RATE CHANGE / ACCRUAL REPORT (132)         *
      * CALLS    : GETCTL, RATETIER, INTCALC, DATEUTIL, ABENDRTN       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1993-01-11 RJT  ACCRUE SKIPPED WEEKEND / HOLIDAY DAYS          *
      * 1998-11-02 MKL  Y2K                                            *
      * 2004-07-30 BHS  MONTH-END ACCRUES THROUGH CALENDAR MONTH END   *
      * 2011-01-10 DPS  CYCLE MINIMUM BALANCE FOR FEE WAIVERS          *
      * 2013-06-24 DPS  OFAC BLOCKED ACCOUNTS CONTINUE TO ACCRUE       *
      *                 PER LEGAL OPINION LGL-13-022                   *
      * 2017-02-08 AKW  PROMO RATE EXPIRY HANDLING                     *
      * 2019-08-19 AKW  RELATIONSHIP PRICING - PASS REL BALANCE        *
      * 2020-11-16 AKW  INC20417 - CUST-TOTAL-REL-BALANCE IS ZERO      *
      *                 UNTIL RELBAL HAS RUN FOR A NEW CIF RECORD.     *
      *                 COMPUTE RELATIONSHIP BALANCE INLINE WHEN ZERO  *
      * 2024-03-04 JQP  CDS NO LONGER ACCRUE ON MATURITY DAY - INC22301*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTACCR.
       AUTHOR. R J TRAVERS.

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
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT REPORT-FILE ASSIGN TO "RATECHG"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  PRODUCT-FILE.
       COPY PRODREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-PROD-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC X(01) VALUE 'N'.
               88  WS-EOF                            VALUE 'Y'.
           05  WS-INT-BEARING-FLAG         PIC X(01).
               88  WS-INT-BEARING                    VALUE 'Y'.
           05  WS-CUST-FOUND-FLAG          PIC X(01).
               88  WS-CUST-FOUND                     VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-ACCRUED-COUNT            PIC 9(07) VALUE 0.
           05  WS-NONINT-COUNT             PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-RATECHG-COUNT            PIC 9(07) VALUE 0.
           05  WS-PROMOEXP-COUNT           PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
           05  WS-UPDATE-COUNT             PIC 9(07) VALUE 0.

       01  WS-DATES.
           05  WS-THROUGH-DATE             PIC 9(08).
           05  WS-ACCT-THROUGH             PIC 9(08).
           05  WS-FROM-DATE                PIC 9(08).
           05  WS-ACCRUAL-DATE             PIC 9(08).
           05  WS-DAYS                     PIC S9(05) COMP.
           05  WS-DAY-IX                   PIC S9(05) COMP.
           05  WS-MAX-CATCHUP-DAYS         PIC S9(05) COMP VALUE 400.

       01  WS-AMOUNTS.
           05  WS-DAY-ACCRUAL-TOT          PIC S9(09)V9(06) COMP-3.
           05  WS-TOTAL-ACCRUAL            PIC S9(11)V9(06) COMP-3
                                           VALUE 0.
           05  WS-OLD-RATE                 PIC 9(03)V9(06).
           05  WS-REL-BALANCE              PIC S9(13)V99 COMP-3.

      *--- RELATIONSHIP BALANCE FALLBACK TABLE (INC20417) -------------*
       01  WS-REL-TABLE.
           05  WS-REL-COUNT                PIC 9(04) COMP VALUE 0.
           05  WS-REL-ENTRY OCCURS 2000 TIMES
                                           INDEXED BY WS-RL-IDX
                                                      WS-RL-IDX2.
               10  WS-REL-CUST-ID          PIC X(10).
               10  WS-REL-ID               PIC X(10).
               10  WS-REL-BAL              PIC S9(13)V99 COMP-3.
       01  WS-REL-WORK-ID                  PIC X(10).

       01  WS-HEADING-1.
           05  FILLER PIC X(08) VALUE 'INTACCR '.
           05  FILLER PIC X(50)
               VALUE 'DAILY INTEREST ACCRUAL / RATE CHANGE REPORT'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  H1-BUS-DATE                 PIC 9(08).
           05  FILLER PIC X(18) VALUE '  ACCRUED THROUGH '.
           05  H1-THRU-DATE                PIC 9(08).
           05  FILLER PIC X(25) VALUE SPACES.
       01  WS-HEADING-2.
           05  FILLER PIC X(30) VALUE
               'ACCOUNT      PROD ST     LEDGE'.
           05  FILLER PIC X(30) VALUE
               'R BALANCE   RATE %    TIER DAY'.
           05  FILLER PIC X(30) VALUE
               'S    ACCRUAL THIS RUN    ACCRU'.
           05  FILLER PIC X(42) VALUE
               'ED TO DATE  EVENT'.
       01  WS-DETAIL-LINE.
           05  DL-ACCT                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-PROD                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-STATUS                   PIC X(01).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-BALANCE                  PIC -,---,---,--9.99.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-RATE                     PIC ZZ9.999999.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-TIER                     PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  DL-DAYS                     PIC ZZZ9.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-ACCRUAL                  PIC --,---,--9.999999.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-ACCRUED                  PIC ---,---,--9.999999.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-EVENT                    PIC X(40).
       01  WS-EVENT-LINE.
           05  FILLER                      PIC X(20) VALUE SPACES.
           05  EL-TEXT                     PIC X(30).
           05  FILLER                      PIC X(06) VALUE ' OLD '.
           05  EL-OLD-RATE                 PIC ZZ9.999999.
           05  FILLER                      PIC X(06) VALUE ' NEW '.
           05  EL-NEW-RATE                 PIC ZZ9.999999.
           05  FILLER                      PIC X(06) VALUE ' APY '.
           05  EL-APY                      PIC ZZ9.999999.
           05  FILLER                      PIC X(07) VALUE ' TIER '.
           05  EL-TIER                     PIC X(04).
           05  FILLER                      PIC X(23) VALUE SPACES.
       01  WS-EVENT-BUFFER.
           05  WS-EVENT-COUNT              PIC 9(01) VALUE 0.
           05  WS-EVENT-SAVED              PIC X(132) OCCURS 2.
       01  WS-TOTAL-LINE.
           05  TL-LABEL                    PIC X(30).
           05  TL-COUNT                    PIC ZZZ,ZZ9.
           05  FILLER                      PIC X(05) VALUE SPACES.
           05  TL-AMOUNT                   PIC ---,---,---,--9.999999.
           05  FILLER                      PIC X(70) VALUE SPACES.

       COPY BANKCTL.
       COPY RTTPARM.
       COPY INTPARM.
       COPY DATEPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 1100-BUILD-REL-TABLE
           PERFORM 2000-PROCESS-ACCOUNTS
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE CTL-BUSINESS-DATE TO WS-THROUGH-DATE
           IF CTL-IS-MONTH-END
               INITIALIZE DATE-PARMS
               SET DTU-END-OF-MONTH TO TRUE
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-THROUGH-DATE
           END-IF

           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT PRODUCT-FILE
           IF WS-PROD-STATUS NOT = '00'
               MOVE 'PRODMST' TO ABND-FILE-NAME
               MOVE WS-PROD-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'RATECHG' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND-OPEN
           END-IF

           MOVE CTL-BUSINESS-DATE TO H1-BUS-DATE
           MOVE WS-THROUGH-DATE   TO H1-THRU-DATE
           WRITE REPORT-LINE FROM WS-HEADING-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-HEADING-2
           DISPLAY 'INTACCR BUSINESS DATE ' CTL-BUSINESS-DATE
                   ' ACCRUING THROUGH ' WS-THROUGH-DATE.

      *----------------------------------------------------------------*
      * INC20417 - SUM POSITIVE DEPOSIT BALANCES BY RELATIONSHIP ID.   *
      * USED ONLY WHEN CUST-TOTAL-REL-BALANCE IS STILL ZERO.           *
      *----------------------------------------------------------------*
       1100-BUILD-REL-TABLE.
           MOVE LOW-VALUES TO CUST-ID
           START CUSTOMER-FILE KEY >= CUST-ID
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF WS-REL-COUNT < 2000
                           ADD 1 TO WS-REL-COUNT
                           SET WS-RL-IDX TO WS-REL-COUNT
                           MOVE CUST-ID TO WS-REL-CUST-ID(WS-RL-IDX)
                           MOVE CUST-RELATIONSHIP-ID
                             TO WS-REL-ID(WS-RL-IDX)
                           IF WS-REL-ID(WS-RL-IDX) = SPACES
                               MOVE CUST-ID TO WS-REL-ID(WS-RL-IDX)
                           END-IF
                           MOVE ZERO TO WS-REL-BAL(WS-RL-IDX)
                       END-IF
               END-READ
           END-PERFORM

           MOVE 'N' TO WS-EOF-FLAG
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1110-ADD-TO-RELATIONSHIP
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-EOF-FLAG.

       1110-ADD-TO-RELATIONSHIP.
           IF (ACCT-CATEGORY = 'SV' OR 'DD' OR 'TD' OR 'MM')
              AND ACCT-LEDGER-BAL > ZERO
              AND NOT ACCT-CLOSED AND NOT ACCT-ESCHEATED
              AND NOT ACCT-CHARGED-OFF
               SET WS-RL-IDX TO 1
               SEARCH WS-REL-ENTRY
                   AT END CONTINUE
                   WHEN WS-RL-IDX > WS-REL-COUNT
                       CONTINUE
                   WHEN WS-REL-CUST-ID(WS-RL-IDX)
                        = ACCT-PRIMARY-CUST-ID
                       MOVE WS-REL-ID(WS-RL-IDX) TO WS-REL-WORK-ID
                       PERFORM VARYING WS-RL-IDX2 FROM 1 BY 1
                               UNTIL WS-RL-IDX2 > WS-REL-COUNT
                           IF WS-REL-ID(WS-RL-IDX2) = WS-REL-WORK-ID
                               ADD ACCT-LEDGER-BAL
                                 TO WS-REL-BAL(WS-RL-IDX2)
                           END-IF
                       END-PERFORM
               END-SEARCH
           END-IF.

      *----------------------------------------------------------------*
       2000-PROCESS-ACCOUNTS.
           MOVE LOW-VALUES TO ACCT-NUMBER
           START ACCOUNT-FILE KEY >= ACCT-NUMBER
               INVALID KEY SET WS-EOF TO TRUE
           END-START
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-PROCESS-ONE-ACCOUNT
               END-READ
           END-PERFORM.

       2100-PROCESS-ONE-ACCOUNT.
           ADD 1 TO WS-READ-COUNT
           IF ACCT-CATEGORY NOT = 'SV' AND 'DD' AND 'TD' AND 'MM'
               ADD 1 TO WS-SKIP-COUNT
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-ACTIVE AND NOT ACCT-DORMANT
              AND NOT ACCT-FROZEN AND NOT ACCT-RESTRICTED
               ADD 1 TO WS-SKIP-COUNT
               EXIT PARAGRAPH
           END-IF

           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           READ PRODUCT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'INTACCR PRODUCT NOT FOUND ' PROD-CODE
                           ' ACCT ' ACCT-NUMBER
                   EXIT PARAGRAPH
           END-READ
           IF PROD-RATE-TABLE-ID = 'NONE' OR SPACES
               MOVE 'N' TO WS-INT-BEARING-FLAG
               ADD 1 TO WS-NONINT-COUNT
           ELSE
               SET WS-INT-BEARING TO TRUE
           END-IF

           PERFORM 2150-COMPUTE-DAYS
           IF WS-INT-BEARING
               PERFORM 2200-GET-CUSTOMER
               PERFORM 2300-REPRICE
               PERFORM 2400-ACCRUE
           END-IF
           PERFORM 2500-CYCLE-STATISTICS
           IF WS-DAYS > 0
               MOVE WS-THROUGH-DATE TO ACCT-LAST-ACCRUAL-DATE
           END-IF

           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'INTACCR' TO ABND-PROGRAM
               MOVE '2100-PROCESS-ONE-ACCOUNT' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'REWRITE FAILED' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           ADD 1 TO WS-UPDATE-COUNT.

      *----------------------------------------------------------------*
      * NUMBER OF CALENDAR DAYS TO PROCESS = THROUGH-DATE MINUS THE    *
      * LAST DATE ALREADY PROCESSED. A NEW ACCOUNT STARTS ON ITS OPEN  *
      * DATE (THE OPENING DAY EARNS INTEREST).                         *
      *----------------------------------------------------------------*
       2150-COMPUTE-DAYS.
           MOVE ZERO TO WS-DAYS
           IF ACCT-LAST-ACCRUAL-DATE = ZERO
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE ACCT-OPEN-DATE TO DTU-DATE-1
               MOVE -1 TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO ACCT-LAST-ACCRUAL-DATE
           END-IF
           IF ACCT-LAST-ACCRUAL-DATE >= WS-THROUGH-DATE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE ACCT-LAST-ACCRUAL-DATE TO DTU-DATE-1
           MOVE WS-THROUGH-DATE        TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DTU-OK
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'INTACCR BAD LAST ACCRUAL DATE ' ACCT-NUMBER
                       ' ' ACCT-LAST-ACCRUAL-DATE
           END-IF
           IF WS-DAYS > WS-MAX-CATCHUP-DAYS
               DISPLAY 'INTACCR CATCH-UP LIMITED TO 400 DAYS '
                       ACCT-NUMBER
               MOVE WS-MAX-CATCHUP-DAYS TO WS-DAYS
           END-IF.

      *----------------------------------------------------------------*
       2200-GET-CUSTOMER.
           MOVE 'N' TO WS-CUST-FOUND-FLAG
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   INITIALIZE CUSTOMER-RECORD
               NOT INVALID KEY
                   SET WS-CUST-FOUND TO TRUE
           END-READ
           MOVE CUST-TOTAL-REL-BALANCE TO WS-REL-BALANCE
           IF WS-REL-BALANCE = ZERO AND WS-CUST-FOUND
               SET WS-RL-IDX TO 1
               SEARCH WS-REL-ENTRY
                   AT END CONTINUE
                   WHEN WS-RL-IDX > WS-REL-COUNT
                       CONTINUE
                   WHEN WS-REL-CUST-ID(WS-RL-IDX) = CUST-ID
                       MOVE WS-REL-BAL(WS-RL-IDX) TO WS-REL-BALANCE
               END-SEARCH
           END-IF.

      *----------------------------------------------------------------*
      * RE-PRICE. CDS KEEP THE CONTRACT RATE (SET AT OPEN / RENEWAL).  *
      *----------------------------------------------------------------*
       2300-REPRICE.
           MOVE SPACES TO DL-EVENT
           INITIALIZE RATETIER-PARMS
           MOVE ACCT-PRODUCT-CODE   TO RTT-PRODUCT-CODE
           MOVE ACCT-LEDGER-BAL     TO RTT-BALANCE
           MOVE WS-REL-BALANCE      TO RTT-REL-BALANCE
           MOVE CUST-SEGMENT        TO RTT-CUST-SEGMENT
           MOVE ACCT-TERM-MONTHS    TO RTT-TERM-MONTHS
           IF RTT-TERM-MONTHS = ZERO
               MOVE PROD-TERM-MONTHS TO RTT-TERM-MONTHS
           END-IF
           MOVE CTL-BUSINESS-DATE   TO RTT-AS-OF-DATE
           MOVE ACCT-PROMO-RATE     TO RTT-PROMO-RATE
           MOVE ACCT-PROMO-EXP-DATE TO RTT-PROMO-EXP-DATE
           MOVE CUST-EMPLOYEE-FLAG  TO RTT-EMPLOYEE-FLAG
           CALL 'RATETIER' USING RATETIER-PARMS
           MOVE RTT-TIER-ID TO DL-TIER

           IF RTT-RETURN-CODE > 04
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'RATETIER ERROR - RATE NOT CHANGED' TO DL-EVENT
               MOVE SPACES TO DL-TIER
               EXIT PARAGRAPH
           END-IF

           IF ACCT-PROMO-RATE > ZERO
              AND CTL-BUSINESS-DATE > ACCT-PROMO-EXP-DATE
               MOVE 'PROMO RATE EXPIRED' TO EL-TEXT
               MOVE ACCT-PROMO-RATE TO EL-OLD-RATE
               MOVE RTT-RESULT-RATE TO EL-NEW-RATE
               MOVE RTT-RESULT-APY  TO EL-APY
               MOVE RTT-TIER-ID     TO EL-TIER
               PERFORM 8100-WRITE-EVENT
               MOVE ZERO TO ACCT-PROMO-RATE ACCT-PROMO-EXP-DATE
               ADD 1 TO WS-PROMOEXP-COUNT
           END-IF

           IF ACCT-CATEGORY = 'TD'
               MOVE 'CD' TO DL-TIER
               EXIT PARAGRAPH
           END-IF
           IF RTT-RESULT-RATE NOT = ACCT-INT-RATE
               MOVE ACCT-INT-RATE   TO WS-OLD-RATE
               MOVE 'RATE CHANGE' TO EL-TEXT
               MOVE WS-OLD-RATE     TO EL-OLD-RATE
               MOVE RTT-RESULT-RATE TO EL-NEW-RATE
               MOVE RTT-RESULT-APY  TO EL-APY
               MOVE RTT-TIER-ID     TO EL-TIER
               PERFORM 8100-WRITE-EVENT
               MOVE RTT-RESULT-RATE TO ACCT-INT-RATE
               MOVE 'RATE CHANGED' TO DL-EVENT
               ADD 1 TO WS-RATECHG-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ONE INTCALC CALL PER CALENDAR DAY. A CD STOPS THE DAY BEFORE   *
      * ITS MATURITY DATE.                                             *
      *----------------------------------------------------------------*
       2400-ACCRUE.
           MOVE ZERO TO WS-DAY-ACCRUAL-TOT
           MOVE WS-THROUGH-DATE TO WS-ACCT-THROUGH
           IF ACCT-CATEGORY = 'TD' AND ACCT-MATURITY-DATE > ZERO
              AND ACCT-MATURITY-DATE <= WS-THROUGH-DATE
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE ACCT-MATURITY-DATE TO DTU-DATE-1
               MOVE -1 TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-ACCT-THROUGH
           END-IF

           MOVE ACCT-LAST-ACCRUAL-DATE TO WS-ACCRUAL-DATE
           PERFORM VARYING WS-DAY-IX FROM 1 BY 1
                   UNTIL WS-DAY-IX > WS-DAYS
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE WS-ACCRUAL-DATE TO DTU-DATE-1
               MOVE 1 TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-ACCRUAL-DATE
               IF WS-ACCRUAL-DATE <= WS-ACCT-THROUGH
                   INITIALIZE INTCALC-PARMS
                   MOVE ACCT-PRODUCT-CODE   TO INT-PRODUCT-CODE
                   MOVE PROD-ACCRUAL-BASIS  TO INT-ACCRUAL-BASIS
                   MOVE PROD-COMPOUND-FREQ  TO INT-COMPOUND-FREQ
                   MOVE WS-ACCRUAL-DATE     TO INT-ACCRUAL-DATE
                   MOVE ACCT-LEDGER-BAL     TO INT-BALANCE
                   MOVE ACCT-ACCRUED-INT    TO INT-ACCRUED-TO-DATE
                   MOVE ACCT-INT-RATE       TO INT-ANNUAL-RATE
                   CALL 'INTCALC' USING INTCALC-PARMS
                   IF INT-RETURN-CODE > 04
                       ADD 1 TO WS-ERROR-COUNT
                       MOVE 'INTCALC ERROR' TO DL-EVENT
                   ELSE
                       ADD INT-DAILY-ACCRUAL TO ACCT-ACCRUED-INT
                       ADD INT-DAILY-ACCRUAL TO WS-DAY-ACCRUAL-TOT
                   END-IF
                   IF INT-RSN-REDUCED-RATE AND DL-EVENT = SPACES
                       MOVE 'BELOW MINIMUM - REDUCED RATE' TO DL-EVENT
                   END-IF
                   IF INT-RSN-BELOW-MIN AND DL-EVENT = SPACES
                       MOVE 'BELOW MINIMUM - NO INTEREST' TO DL-EVENT
                   END-IF
               END-IF
           END-PERFORM
           ADD WS-DAY-ACCRUAL-TOT TO WS-TOTAL-ACCRUAL
           ADD 1 TO WS-ACCRUED-COUNT

           MOVE ACCT-NUMBER       TO DL-ACCT
           MOVE ACCT-PRODUCT-CODE TO DL-PROD
           MOVE ACCT-STATUS       TO DL-STATUS
           MOVE ACCT-LEDGER-BAL   TO DL-BALANCE
           MOVE ACCT-INT-RATE     TO DL-RATE
           MOVE WS-DAYS           TO DL-DAYS
           MOVE WS-DAY-ACCRUAL-TOT TO DL-ACCRUAL
           MOVE ACCT-ACCRUED-INT  TO DL-ACCRUED
           WRITE REPORT-LINE FROM WS-DETAIL-LINE
           PERFORM 8200-FLUSH-EVENTS.

      *----------------------------------------------------------------*
      * CYCLE STATISTICS - ONE END-OF-DAY BALANCE PER CALENDAR DAY.    *
      * A NEW CYCLE (DAYS-IN-CYCLE ZERO) RESTARTS THE MINIMUM.         *
      *----------------------------------------------------------------*
       2500-CYCLE-STATISTICS.
           IF WS-DAYS = 0
               EXIT PARAGRAPH
           END-IF
           IF ACCT-DAYS-IN-CYCLE = ZERO
              OR ACCT-LEDGER-BAL < ACCT-MIN-BAL-CYC
               MOVE ACCT-LEDGER-BAL TO ACCT-MIN-BAL-CYC
           END-IF
           COMPUTE ACCT-BAL-ACCUM-CYC = ACCT-BAL-ACCUM-CYC
                                      + (ACCT-LEDGER-BAL * WS-DAYS)
           ADD WS-DAYS TO ACCT-DAYS-IN-CYCLE.

      *----------------------------------------------------------------*
       8100-WRITE-EVENT.
           IF WS-EVENT-COUNT < 2
               ADD 1 TO WS-EVENT-COUNT
               MOVE WS-EVENT-LINE TO WS-EVENT-SAVED(WS-EVENT-COUNT)
           END-IF.

       8200-FLUSH-EVENTS.
           PERFORM VARYING WS-DAY-IX FROM 1 BY 1
                   UNTIL WS-DAY-IX > WS-EVENT-COUNT
               WRITE REPORT-LINE FROM WS-EVENT-SAVED(WS-DAY-IX)
           END-PERFORM
           MOVE 0 TO WS-EVENT-COUNT.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'ACCOUNTS READ' TO TL-LABEL
           MOVE WS-READ-COUNT TO TL-COUNT
           MOVE ZERO TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'INTEREST BEARING ACCRUED' TO TL-LABEL
           MOVE WS-ACCRUED-COUNT TO TL-COUNT
           MOVE WS-TOTAL-ACCRUAL TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'NON-INTEREST (STATS ONLY)' TO TL-LABEL
           MOVE WS-NONINT-COUNT TO TL-COUNT
           MOVE ZERO TO TL-AMOUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'RATE CHANGES' TO TL-LABEL
           MOVE WS-RATECHG-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'PROMOTIONS EXPIRED' TO TL-LABEL
           MOVE WS-PROMOEXP-COUNT TO TL-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE
           MOVE 'SKIPPED / ERRORS' TO TL-LABEL
           COMPUTE TL-COUNT = WS-SKIP-COUNT + WS-ERROR-COUNT
           WRITE REPORT-LINE FROM WS-TOTAL-LINE

           CLOSE ACCOUNT-FILE PRODUCT-FILE CUSTOMER-FILE REPORT-FILE
           DISPLAY 'INTACCR READ=' WS-READ-COUNT
                   ' ACCRUED=' WS-ACCRUED-COUNT
                   ' RATECHG=' WS-RATECHG-COUNT
                   ' PROMOEXP=' WS-PROMOEXP-COUNT
                   ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

      *----------------------------------------------------------------*
       9900-ABEND-OPEN.
           MOVE 'INTACCR' TO ABND-PROGRAM
           MOVE '1000-INITIALIZE' TO ABND-PARAGRAPH
           MOVE 'FILE OPEN FAILED' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
