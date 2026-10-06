      *================================================================*
      * PROGRAM  : REGCCHLD                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : REGULATION CC FUNDS AVAILABILITY FOR CHECK DEPOSITS.*
      *            STANDARD SCHEDULE (12 CFR 229.10 / 229.12):         *
      *              FIRST $275 OF THE DAY'S CHECKS - NEXT BUS DAY     *
      *              REMAINDER                      - 2ND BUS DAY      *
      *            EXCEPTION HOLDS (12 CFR 229.13) - REMAINDER AFTER   *
      *            THE FIRST $275 IS HELD TO THE 7TH BUSINESS DAY:     *
      *              NEW ACCOUNT (OPEN < 30 CALENDAR DAYS)             *
      *              REPEATED OVERDRAFTS (NSF ITEMS IN 12M >= 6)       *
      *            LARGE DEPOSIT - PORTION OF THE DAY'S CHECKS OVER    *
      *              $6,725 HELD 5 BUSINESS DAYS BEYOND STANDARD.      *
      *            NO HOLD: CASH, WIRES, ACH, INTERNAL TRANSFERS AND   *
      *            ALL OTHER NON-CHECK CREDITS. PRIVATE WEALTH - NO    *
      *            HOLDS (BANK POLICY PB-0031).                        *
      * LINKAGE  : RCCPARM                                             *
      * CALLS    : DATEUTIL (ADDB)                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-19 RJT  ORIGINAL - EFAA SCHEDULE $100 NEXT DAY         *
      * 2004-10-28 BHS  CHECK 21 - LOCAL/NONLOCAL DISTINCTION REMOVED  *
      * 2010-07-26 DPS  $200 NEXT DAY, $5,000 LARGE DEPOSIT            *
      * 2011-07-21 DPS  EXCEPTION HOLD TIERS (CX)                      *
      * 2015-10-02 DPS  CORE CONVERSION - PW NO-HOLD POLICY PB-0031    *
      * 2020-07-01 AKW  REG-2020-CC $225 NEXT DAY, $5,525 LARGE DEP    *
      * 2025-07-01 AKW  REG-2025-CC $275 NEXT DAY, $6,725 LARGE DEP    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGCCHLD.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DATEPARM.

      *----------------------------------------------------------------*
      * REG CC AVAILABILITY PARAMETERS                                 *
      *----------------------------------------------------------------*
       01  WS-REGCC-POLICY.
      *    2025-07 AKW REG-2025-CC - WAS:
      *    05  WS-NEXT-DAY-AMT             PIC S9(07)V99 VALUE 225.00.
      *    05  WS-LARGE-DEPOSIT-AMT        PIC S9(07)V99 VALUE 5525.00.
           05  WS-NEXT-DAY-AMT             PIC S9(07)V99 VALUE 275.00.
           05  WS-LARGE-DEPOSIT-AMT        PIC S9(07)V99 VALUE 6725.00.
           05  WS-NEXT-DAY-BUS-DAYS        PIC 9(02)     VALUE 01.
           05  WS-STANDARD-BUS-DAYS        PIC 9(02)     VALUE 02.
           05  WS-EXCEPTION-EXTRA-DAYS     PIC 9(02)     VALUE 05.
           05  WS-NEW-ACCT-BUS-DAYS        PIC 9(02)     VALUE 07.
           05  WS-NEW-ACCT-CAL-DAYS        PIC 9(03)     VALUE 030.
           05  WS-REPEAT-OD-NSF-COUNT      PIC 9(03)     VALUE 006.

       01  WS-WORK.
           05  WS-REMAINING                PIC S9(13)V99 COMP-3.
           05  WS-NEXT-DAY-LEFT            PIC S9(13)V99 COMP-3.
           05  WS-STD-ROOM                 PIC S9(13)V99 COMP-3.
           05  WS-TIER-AMT                 PIC S9(13)V99 COMP-3.
           05  WS-ACCT-AGE-DAYS            PIC S9(07).
           05  WS-TIER-DAYS                PIC 9(02).
           05  WS-TIER-TYPE                PIC X(02).
           05  WS-TIER-REASON              PIC X(30).
           05  WS-SUB                      PIC 9(01).

       LINKAGE SECTION.
       COPY RCCPARM.

       PROCEDURE DIVISION USING REGCCHLD-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           EVALUATE TRUE
               WHEN RCC-TRAN-CODE NOT = 'DCHK'
                   GOBACK
               WHEN RCC-CUST-SEGMENT = 'PW'
                   GOBACK
               WHEN RCC-AMOUNT NOT > 0
                   GOBACK
           END-EVALUATE

           PERFORM 1100-ACCOUNT-AGE
           MOVE RCC-AMOUNT TO WS-REMAINING

      *    TIER 1 - NEXT-DAY AMOUNT, SHARED ACROSS THE DAY'S DEPOSITS
           COMPUTE WS-NEXT-DAY-LEFT = WS-NEXT-DAY-AMT
                                    - RCC-DEPOSITS-TODAY
           IF WS-NEXT-DAY-LEFT > 0
               IF WS-NEXT-DAY-LEFT > WS-REMAINING
                   MOVE WS-REMAINING TO WS-TIER-AMT
               ELSE
                   MOVE WS-NEXT-DAY-LEFT TO WS-TIER-AMT
               END-IF
               MOVE 'CK' TO WS-TIER-TYPE
               MOVE WS-NEXT-DAY-BUS-DAYS TO WS-TIER-DAYS
               MOVE 'REG CC NEXT BUSINESS DAY' TO WS-TIER-REASON
               PERFORM 8000-ADD-TIER
           END-IF
           IF WS-REMAINING = 0
               GOBACK
           END-IF

           EVALUATE TRUE
               WHEN WS-ACCT-AGE-DAYS < WS-NEW-ACCT-CAL-DAYS
                   MOVE 'NEWA' TO RCC-EXCEPTION-CODE
                   MOVE WS-REMAINING TO WS-TIER-AMT
                   MOVE 'CX' TO WS-TIER-TYPE
                   MOVE WS-NEW-ACCT-BUS-DAYS TO WS-TIER-DAYS
                   MOVE 'REG CC EXCEPTION - NEW ACCOUNT'
                     TO WS-TIER-REASON
                   PERFORM 8000-ADD-TIER
               WHEN RCC-NSF-COUNT-12M NOT < WS-REPEAT-OD-NSF-COUNT
                   MOVE 'RPOD' TO RCC-EXCEPTION-CODE
                   MOVE WS-REMAINING TO WS-TIER-AMT
                   MOVE 'CX' TO WS-TIER-TYPE
                   COMPUTE WS-TIER-DAYS = WS-STANDARD-BUS-DAYS
                                        + WS-EXCEPTION-EXTRA-DAYS
                   MOVE 'REG CC EXCEPTION - REPEATED OD'
                     TO WS-TIER-REASON
                   PERFORM 8000-ADD-TIER
               WHEN OTHER
                   PERFORM 2000-STANDARD-AND-LARGE
           END-EVALUATE.
           GOBACK.

       1000-INITIALISE.
           MOVE 0      TO RCC-HOLD-COUNT RCC-MAX-HOLD-DAYS
           MOVE ZERO   TO RCC-TOTAL-HOLD-AMT
           MOVE SPACES TO RCC-EXCEPTION-CODE
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 3
               MOVE SPACES TO RCC-HOLD-TYPE(WS-SUB)
                              RCC-HOLD-REASON(WS-SUB)
               MOVE ZERO   TO RCC-HOLD-AMT(WS-SUB)
                              RCC-HOLD-BUS-DAYS(WS-SUB)
                              RCC-RELEASE-DATE(WS-SUB)
           END-PERFORM
           MOVE 00 TO RCC-RETURN-CODE.

       1100-ACCOUNT-AGE.
           MOVE 99999 TO WS-ACCT-AGE-DAYS
           IF RCC-ACCT-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE RCC-ACCT-OPEN-DATE TO DTU-DATE-1
               MOVE RCC-BUSINESS-DATE  TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-ACCT-AGE-DAYS
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * STANDARD 2ND-DAY TIER UP TO THE LARGE DEPOSIT THRESHOLD        *
      * (AGGREGATED WITH CHECKS ALREADY DEPOSITED TODAY), EXCESS HELD  *
      * TO 2 + 5 = 7TH BUSINESS DAY.                                   *
      *----------------------------------------------------------------*
       2000-STANDARD-AND-LARGE.
           COMPUTE WS-STD-ROOM = WS-LARGE-DEPOSIT-AMT
                               - RCC-DEPOSITS-TODAY
                               - (RCC-AMOUNT - WS-REMAINING)
           IF WS-STD-ROOM < 0
               MOVE ZERO TO WS-STD-ROOM
           END-IF
           IF WS-STD-ROOM > 0
               IF WS-STD-ROOM > WS-REMAINING
                   MOVE WS-REMAINING TO WS-TIER-AMT
               ELSE
                   MOVE WS-STD-ROOM TO WS-TIER-AMT
               END-IF
               MOVE 'CK' TO WS-TIER-TYPE
               MOVE WS-STANDARD-BUS-DAYS TO WS-TIER-DAYS
               MOVE 'REG CC SECOND BUSINESS DAY' TO WS-TIER-REASON
               PERFORM 8000-ADD-TIER
           END-IF
           IF WS-REMAINING > 0
               MOVE 'LRGD' TO RCC-EXCEPTION-CODE
               MOVE WS-REMAINING TO WS-TIER-AMT
               MOVE 'CX' TO WS-TIER-TYPE
               COMPUTE WS-TIER-DAYS = WS-STANDARD-BUS-DAYS
                                    + WS-EXCEPTION-EXTRA-DAYS
               MOVE 'REG CC EXCEPTION - LARGE DEPOSIT'
                 TO WS-TIER-REASON
               PERFORM 8000-ADD-TIER
           END-IF.

      *----------------------------------------------------------------*
      * ADD ONE HOLD TIER AND COMPUTE ITS RELEASE DATE                 *
      *----------------------------------------------------------------*
       8000-ADD-TIER.
           IF WS-TIER-AMT NOT > 0 OR RCC-HOLD-COUNT NOT < 3
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO RCC-HOLD-COUNT
           MOVE RCC-HOLD-COUNT TO WS-SUB
           MOVE WS-TIER-TYPE   TO RCC-HOLD-TYPE(WS-SUB)
           MOVE WS-TIER-AMT    TO RCC-HOLD-AMT(WS-SUB)
           MOVE WS-TIER-DAYS   TO RCC-HOLD-BUS-DAYS(WS-SUB)
           MOVE WS-TIER-REASON TO RCC-HOLD-REASON(WS-SUB)
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE RCC-BUSINESS-DATE TO DTU-DATE-1
           MOVE WS-TIER-DAYS      TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-OK
               MOVE 12 TO RCC-RETURN-CODE
           END-IF
           MOVE DTU-RESULT-DATE TO RCC-RELEASE-DATE(WS-SUB)
           ADD WS-TIER-AMT TO RCC-TOTAL-HOLD-AMT
           SUBTRACT WS-TIER-AMT FROM WS-REMAINING
           IF WS-TIER-DAYS > RCC-MAX-HOLD-DAYS
               MOVE WS-TIER-DAYS TO RCC-MAX-HOLD-DAYS
           END-IF
           IF RCC-RETURN-CODE = 00
               MOVE 04 TO RCC-RETURN-CODE
           END-IF.
