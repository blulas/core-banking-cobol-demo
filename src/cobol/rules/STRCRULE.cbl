      *================================================================*
      * PROGRAM  : STRCRULE                                            *
      * TYPE     : CALLED BUSINESS RULE MODULE                         *
      * PURPOSE  : STRUCTURING DETECTION (31 USC 5324). EVALUATES THE  *
      *            CASH ACTIVITY OF ONE CUSTOMER OVER THE LOOKBACK     *
      *            WINDOW FOR PATTERNS THAT SUGGEST CASH IS BEING      *
      *            BROKEN UP TO EVADE THE $10,000 CTR THRESHOLD.       *
      *            S1 - CASH DEPOSITS OF $8,000 TO $10,000 ON 2 OR     *
      *                 MORE BUSINESS DAYS WITHIN 10 CALENDAR DAYS     *
      *            S2 - CASH OVER A ROLLING 5 BUSINESS DAYS > $10,000  *
      *                 WITH NO SINGLE DAY > $10,000 (NO CTR FILED)    *
      *            S3 - 2+ SAME-DAY CASH ITEMS, EACH $5,000-$10,000,   *
      *                 AT 2 OR MORE DIFFERENT BRANCHES                *
      *            S4 - 3+ ROUND-HUNDRED CASH DEPOSITS OF $3,000 OR    *
      *                 MORE WITHIN 7 CALENDAR DAYS                    *
      *            THE CUSTOMER MUST HAVE CASH ACTIVITY TODAY FOR ANY  *
      *            PATTERN TO BE RAISED (AVOIDS DAILY RE-ALERTING).    *
      * LINKAGE  : STRPARM                                             *
      * CALLED BY: STRUCTUR (BATCH)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-02-17 BHS  ORIGINAL (S1, S2)                              *
      * 2006-11-09 BHS  CHG0233 S1 LOWER BAND 9,000 TO 8,000 PER       *
      *                 BSA OFFICER (EXAM FINDING 2006-14)             *
      * 2009-06-08 DPS  S3 MULTI-BRANCH, S4 ROUND DOLLAR (OCC MRA)     *
      * 2014-03-21 DPS  CHG1187 SCORE / SEVERITY RETURNED TO CALLER    *
      * 2021-02-02 AKW  INC40877 S2 WINDOW USES BUSINESS DAYS          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRCRULE.
       AUTHOR. B H SANDERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * STRUCTURING PARAMETERS                                         *
      *----------------------------------------------------------------*
       01  WS-STRC-PARAMETERS.
           05  WS-CTR-THRESHOLD            PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
      *    S1 - BAND JUST BELOW THE CTR THRESHOLD
      *    2006-11 BHS CHG0233 WAS 9000.00
           05  WS-S1-BAND-LOW              PIC S9(13)V99 COMP-3
                                           VALUE 8000.00.
           05  WS-S1-BAND-HIGH             PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
           05  WS-S1-WINDOW-DAYS           PIC S9(05) VALUE 10.
           05  WS-S1-MIN-DAYS              PIC 9(02)  VALUE 2.
      *    S2 - ROLLING BUSINESS-DAY WINDOW
           05  WS-S2-WINDOW-BDAYS          PIC S9(05) VALUE 5.
      *    S3 - SAME DAY, MULTIPLE BRANCHES
           05  WS-S3-ITEM-LOW              PIC S9(13)V99 COMP-3
                                           VALUE 5000.00.
           05  WS-S3-MIN-ITEMS             PIC 9(02)  VALUE 2.
           05  WS-S3-MIN-BRANCHES          PIC 9(02)  VALUE 2.
      *    S4 - ROUND DOLLAR DEPOSITS
           05  WS-S4-ITEM-LOW              PIC S9(13)V99 COMP-3
                                           VALUE 3000.00.
           05  WS-S4-WINDOW-DAYS           PIC S9(05) VALUE 7.
           05  WS-S4-MIN-ITEMS             PIC 9(02)  VALUE 3.
           05  WS-S4-ROUND-CENTS           PIC 9(05)  VALUE 10000.
      *    SCORE WEIGHTS
           05  WS-S1-WEIGHT                PIC 9(03)  VALUE 040.
           05  WS-S2-WEIGHT                PIC 9(03)  VALUE 030.
           05  WS-S3-WEIGHT                PIC 9(03)  VALUE 025.
           05  WS-S4-WEIGHT                PIC 9(03)  VALUE 015.
           05  WS-HIGH-SCORE               PIC 9(03)  VALUE 060.
           05  WS-MEDIUM-SCORE             PIC 9(03)  VALUE 030.

       01  WS-WINDOW-DATES.
           05  WS-S1-START-DATE            PIC 9(08).
           05  WS-S2-START-DATE            PIC 9(08).
           05  WS-S4-START-DATE            PIC 9(08).

       01  WS-DAY-TABLE.
           05  WS-DAY-COUNT                PIC 9(03) COMP.
           05  WS-DAY OCCURS 200 TIMES.
               10  WS-DAY-DATE             PIC 9(08).
               10  WS-DAY-CASH-IN          PIC S9(13)V99 COMP-3.
               10  WS-DAY-CASH-OUT         PIC S9(13)V99 COMP-3.
               10  WS-DAY-BAND-FLAG        PIC X(01).

       01  WS-WORK-FIELDS.
           05  WS-IX                       PIC 9(03) COMP.
           05  WS-JX                       PIC 9(03) COMP.
           05  WS-FOUND                    PIC X(01).
           05  WS-TODAY-ACTIVITY           PIC X(01).
           05  WS-TODAY-QUAL-ITEMS         PIC 9(03) COMP.
           05  WS-S2-MAX-DAY-IN            PIC S9(13)V99 COMP-3.
           05  WS-S2-MAX-DAY-OUT           PIC S9(13)V99 COMP-3.
           05  WS-CENTS                    PIC 9(15).
           05  WS-SCORE-WORK               PIC 9(03).
           05  WS-BRANCH-COUNT             PIC 9(03) COMP.
           05  WS-BRANCH-TBL OCCURS 50 TIMES PIC X(05).
           05  WS-AMT-ED                   PIC Z,ZZZ,ZZ9.99.

       COPY DATEPARM.

       LINKAGE SECTION.
       COPY STRPARM.

       PROCEDURE DIVISION USING STRC-RULE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF STP-RETURN-CODE = 00
               PERFORM 1100-BUILD-DAY-TABLE
           END-IF
           IF STP-RETURN-CODE = 00 AND WS-TODAY-ACTIVITY = 'Y'
               PERFORM 2000-CHECK-S1-BAND
               PERFORM 3000-CHECK-S2-ROLLING
               PERFORM 4000-CHECK-S3-BRANCHES
               PERFORM 5000-CHECK-S4-ROUND
               PERFORM 6000-SCORE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE 00     TO STP-RETURN-CODE
           MOVE 0      TO STP-PATTERN-COUNT STP-SCORE STP-BAND-DAYS
                          STP-TODAY-BRANCHES STP-ROUND-COUNT
           MOVE ZERO   TO STP-ROLLING-CASH-IN STP-ROLLING-CASH-OUT
                          STP-SUSPECT-AMOUNT
           MOVE SPACES TO STP-PATTERN-CODES STP-DESCRIPTION
           MOVE 'L'    TO STP-SEVERITY
           MOVE 'N'    TO WS-TODAY-ACTIVITY
           MOVE 0      TO WS-DAY-COUNT

           IF STP-ITEM-COUNT > 200
               MOVE 12 TO STP-RETURN-CODE
               MOVE 'ITEM TABLE OVERFLOW' TO STP-DESCRIPTION
           ELSE
               INITIALIZE DATE-PARMS
               SET DTU-VALIDATE TO TRUE
               MOVE STP-BUSINESS-DATE TO DTU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DTU-RESULT-YES
                   MOVE 12 TO STP-RETURN-CODE
                   MOVE 'INVALID BUSINESS DATE' TO STP-DESCRIPTION
               END-IF
           END-IF
           IF STP-RETURN-CODE = 00
      *        S1 WINDOW - TODAY AND THE 9 PRIOR CALENDAR DAYS
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE STP-BUSINESS-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = 1 - WS-S1-WINDOW-DAYS
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-S1-START-DATE
      *        S2 WINDOW - TODAY AND THE 4 PRIOR BUSINESS DAYS
               INITIALIZE DATE-PARMS
               SET DTU-ADD-BUS-DAYS TO TRUE
               MOVE STP-BUSINESS-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = 1 - WS-S2-WINDOW-BDAYS
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-S2-START-DATE
      *        S4 WINDOW - TODAY AND THE 6 PRIOR CALENDAR DAYS
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE STP-BUSINESS-DATE TO DTU-DATE-1
               COMPUTE DTU-NUMBER = 1 - WS-S4-WINDOW-DAYS
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO WS-S4-START-DATE
           END-IF.

      *----------------------------------------------------------------*
      * BUILD ONE ENTRY PER CALENDAR DATE WITH CASH IN / OUT TOTALS.   *
      *----------------------------------------------------------------*
       1100-BUILD-DAY-TABLE.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > STP-ITEM-COUNT
               IF STP-ITEM-DATE(WS-IX) = STP-BUSINESS-DATE
                   MOVE 'Y' TO WS-TODAY-ACTIVITY
               END-IF
               MOVE 'N' TO WS-FOUND
               PERFORM VARYING WS-JX FROM 1 BY 1
                       UNTIL WS-JX > WS-DAY-COUNT OR WS-FOUND = 'Y'
                   IF WS-DAY-DATE(WS-JX) = STP-ITEM-DATE(WS-IX)
                       MOVE 'Y' TO WS-FOUND
                   END-IF
               END-PERFORM
               IF WS-FOUND = 'Y'
                   SUBTRACT 1 FROM WS-JX
               ELSE
                   ADD 1 TO WS-DAY-COUNT
                   MOVE WS-DAY-COUNT TO WS-JX
                   MOVE STP-ITEM-DATE(WS-IX) TO WS-DAY-DATE(WS-JX)
                   MOVE ZERO TO WS-DAY-CASH-IN(WS-JX)
                                WS-DAY-CASH-OUT(WS-JX)
                   MOVE 'N'  TO WS-DAY-BAND-FLAG(WS-JX)
               END-IF
               IF STP-ITEM-DR-CR(WS-IX) = 'C'
                   ADD STP-ITEM-AMOUNT(WS-IX) TO WS-DAY-CASH-IN(WS-JX)
               ELSE
                   ADD STP-ITEM-AMOUNT(WS-IX) TO WS-DAY-CASH-OUT(WS-JX)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * S1 - SINGLE DEPOSITS IN THE $8,000-$10,000 BAND ON 2 OR MORE   *
      * DIFFERENT BUSINESS DAYS WITHIN 10 CALENDAR DAYS.               *
      *----------------------------------------------------------------*
       2000-CHECK-S1-BAND.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > STP-ITEM-COUNT
               IF STP-ITEM-DR-CR(WS-IX) = 'C'
                  AND STP-ITEM-DATE(WS-IX) >= WS-S1-START-DATE
                  AND STP-ITEM-DATE(WS-IX) <= STP-BUSINESS-DATE
                  AND STP-ITEM-AMOUNT(WS-IX) >= WS-S1-BAND-LOW
                  AND STP-ITEM-AMOUNT(WS-IX) <= WS-S1-BAND-HIGH
                   ADD STP-ITEM-AMOUNT(WS-IX) TO STP-SUSPECT-AMOUNT
                   PERFORM VARYING WS-JX FROM 1 BY 1
                           UNTIL WS-JX > WS-DAY-COUNT
                       IF WS-DAY-DATE(WS-JX) = STP-ITEM-DATE(WS-IX)
                          AND WS-DAY-BAND-FLAG(WS-JX) = 'N'
                           MOVE 'Y' TO WS-DAY-BAND-FLAG(WS-JX)
                           ADD 1 TO STP-BAND-DAYS
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF STP-BAND-DAYS >= WS-S1-MIN-DAYS
               ADD 1 TO STP-PATTERN-COUNT
               MOVE 'S1' TO STP-PATTERN-CODE(STP-PATTERN-COUNT)
           END-IF.

      *----------------------------------------------------------------*
      * S2 - ROLLING 5 BUSINESS DAYS. AGGREGATE CASH IN (OR OUT) IS    *
      * OVER $10,000 BUT NO SINGLE DAY WAS OVER $10,000, I.E. NO CTR   *
      * WAS TRIGGERED FOR ANY OF THE DAYS.                             *
      *----------------------------------------------------------------*
       3000-CHECK-S2-ROLLING.
           MOVE ZERO TO WS-S2-MAX-DAY-IN WS-S2-MAX-DAY-OUT
           PERFORM VARYING WS-JX FROM 1 BY 1
                   UNTIL WS-JX > WS-DAY-COUNT
               IF WS-DAY-DATE(WS-JX) >= WS-S2-START-DATE
                  AND WS-DAY-DATE(WS-JX) <= STP-BUSINESS-DATE
                   ADD WS-DAY-CASH-IN(WS-JX)  TO STP-ROLLING-CASH-IN
                   ADD WS-DAY-CASH-OUT(WS-JX) TO STP-ROLLING-CASH-OUT
                   IF WS-DAY-CASH-IN(WS-JX) > WS-S2-MAX-DAY-IN
                       MOVE WS-DAY-CASH-IN(WS-JX) TO WS-S2-MAX-DAY-IN
                   END-IF
                   IF WS-DAY-CASH-OUT(WS-JX) > WS-S2-MAX-DAY-OUT
                       MOVE WS-DAY-CASH-OUT(WS-JX) TO WS-S2-MAX-DAY-OUT
                   END-IF
               END-IF
           END-PERFORM
           EVALUATE TRUE
               WHEN STP-ROLLING-CASH-IN > WS-CTR-THRESHOLD
                AND WS-S2-MAX-DAY-IN NOT > WS-CTR-THRESHOLD
                   ADD 1 TO STP-PATTERN-COUNT
                   MOVE 'S2' TO STP-PATTERN-CODE(STP-PATTERN-COUNT)
               WHEN STP-ROLLING-CASH-OUT > WS-CTR-THRESHOLD
                AND WS-S2-MAX-DAY-OUT NOT > WS-CTR-THRESHOLD
                   ADD 1 TO STP-PATTERN-COUNT
                   MOVE 'S2' TO STP-PATTERN-CODE(STP-PATTERN-COUNT)
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * S3 - TODAY: 2+ CASH ITEMS EACH FROM $5,000 UP TO $10,000 AT 2+ *
      * DIFFERENT BRANCHES (COST CENTRES).                             *
      *----------------------------------------------------------------*
       4000-CHECK-S3-BRANCHES.
           MOVE 0 TO WS-TODAY-QUAL-ITEMS WS-BRANCH-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > STP-ITEM-COUNT
               IF STP-ITEM-DATE(WS-IX) = STP-BUSINESS-DATE
                  AND STP-ITEM-AMOUNT(WS-IX) >= WS-S3-ITEM-LOW
                  AND STP-ITEM-AMOUNT(WS-IX) <= WS-CTR-THRESHOLD
                   ADD 1 TO WS-TODAY-QUAL-ITEMS
                   MOVE 'N' TO WS-FOUND
                   PERFORM VARYING WS-JX FROM 1 BY 1
                           UNTIL WS-JX > WS-BRANCH-COUNT
                       IF WS-BRANCH-TBL(WS-JX) = STP-ITEM-BRANCH(WS-IX)
                           MOVE 'Y' TO WS-FOUND
                       END-IF
                   END-PERFORM
                   IF WS-FOUND = 'N' AND WS-BRANCH-COUNT < 50
                       ADD 1 TO WS-BRANCH-COUNT
                       MOVE STP-ITEM-BRANCH(WS-IX)
                         TO WS-BRANCH-TBL(WS-BRANCH-COUNT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE WS-BRANCH-COUNT TO STP-TODAY-BRANCHES
           IF WS-TODAY-QUAL-ITEMS >= WS-S3-MIN-ITEMS
              AND WS-BRANCH-COUNT >= WS-S3-MIN-BRANCHES
               ADD 1 TO STP-PATTERN-COUNT
               MOVE 'S3' TO STP-PATTERN-CODE(STP-PATTERN-COUNT)
           END-IF.

      *----------------------------------------------------------------*
      * S4 - 3+ ROUND-HUNDRED CASH DEPOSITS OF $3,000 OR MORE WITHIN   *
      * 7 CALENDAR DAYS. ROUND = WHOLE HUNDREDS, NO CENTS.             *
      *----------------------------------------------------------------*
       5000-CHECK-S4-ROUND.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > STP-ITEM-COUNT
               IF STP-ITEM-DR-CR(WS-IX) = 'C'
                  AND STP-ITEM-DATE(WS-IX) >= WS-S4-START-DATE
                  AND STP-ITEM-DATE(WS-IX) <= STP-BUSINESS-DATE
                  AND STP-ITEM-AMOUNT(WS-IX) >= WS-S4-ITEM-LOW
                   COMPUTE WS-CENTS = STP-ITEM-AMOUNT(WS-IX) * 100
                   IF FUNCTION MOD(WS-CENTS, WS-S4-ROUND-CENTS) = 0
                       ADD 1 TO STP-ROUND-COUNT
                   END-IF
               END-IF
           END-PERFORM
           IF STP-ROUND-COUNT >= WS-S4-MIN-ITEMS
               ADD 1 TO STP-PATTERN-COUNT
               MOVE 'S4' TO STP-PATTERN-CODE(STP-PATTERN-COUNT)
           END-IF.

      *----------------------------------------------------------------*
       6000-SCORE.
           MOVE 0 TO WS-SCORE-WORK
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > STP-PATTERN-COUNT
               EVALUATE STP-PATTERN-CODE(WS-IX)
                   WHEN 'S1' ADD WS-S1-WEIGHT TO WS-SCORE-WORK
                   WHEN 'S2' ADD WS-S2-WEIGHT TO WS-SCORE-WORK
                   WHEN 'S3' ADD WS-S3-WEIGHT TO WS-SCORE-WORK
                   WHEN 'S4' ADD WS-S4-WEIGHT TO WS-SCORE-WORK
               END-EVALUATE
           END-PERFORM
           IF WS-SCORE-WORK > 100
               MOVE 100 TO WS-SCORE-WORK
           END-IF
           MOVE WS-SCORE-WORK TO STP-SCORE
           EVALUATE TRUE
               WHEN STP-SCORE >= WS-HIGH-SCORE
                   MOVE 'H' TO STP-SEVERITY
               WHEN STP-SCORE >= WS-MEDIUM-SCORE
                   MOVE 'M' TO STP-SEVERITY
               WHEN OTHER
                   MOVE 'L' TO STP-SEVERITY
           END-EVALUATE
           IF STP-PATTERN-COUNT > 0
               MOVE 04 TO STP-RETURN-CODE
               IF STP-SUSPECT-AMOUNT = ZERO
                   MOVE STP-ROLLING-CASH-IN TO STP-SUSPECT-AMOUNT
               END-IF
               MOVE STP-SUSPECT-AMOUNT TO WS-AMT-ED
               STRING 'POSSIBLE STRUCTURING PATTERNS '
                      STP-PATTERN-CODES DELIMITED BY SIZE
                      ' SCORE ' STP-SCORE
                      ' AMT ' WS-AMT-ED DELIMITED BY SIZE
                 INTO STP-DESCRIPTION
               END-STRING
           END-IF.
