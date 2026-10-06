      *================================================================*
      * PROGRAM  : POOLALOC                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : NOTIONAL POOL INTEREST CALCULATION AND ALLOCATION.  *
      *            1. GROSS CREDIT = SUM OF POSITIVE MEMBER AVERAGES   *
      *               GROSS DEBIT  = SUM OF NEGATIVE MEMBER AVERAGES   *
      *               NET POSITION = GROSS CREDIT - GROSS DEBIT        *
      *            2. CREDIT RATE = FED FUNDS - CREDIT SPREAD (MIN 0)  *
      *               DEBIT RATE  = PRIME + DEBIT SPREAD               *
      *            3. NET >= 0 : DEBIT POSITIONS ARE FULLY OFFSET AND  *
      *               PAY NO INTEREST. CREDIT INTEREST ON THE NET      *
      *               POSITION = NET x CREDIT RATE x DAYS / 360,       *
      *               ALLOCATED                                        *
      *                 P - TO POSITIVE MEMBERS PRO RATA TO THEIR      *
      *                     POSITIVE BALANCE                           *
      *                 H - ENTIRELY TO THE HEADER ACCOUNT             *
      *            4. NET < 0 : NO CREDIT INTEREST. DEBIT INTEREST ON  *
      *               THE NET SHORTFALL = |NET| x DEBIT RATE x DAYS /  *
      *               360, CHARGED                                     *
      *                 P - TO NEGATIVE MEMBERS PRO RATA TO THEIR      *
      *                     NEGATIVE BALANCE                           *
      *                 H - ENTIRELY TO THE HEADER ACCOUNT             *
      *            5. ROUNDING DIFFERENCE GOES TO THE LARGEST SHARE.   *
      *            6. GROSS DEBIT > POOL LIMIT = LIMIT BREACH (RC 04). *
      * CALLED BY: POOLCALC (BATCH), CMPOSINQ (CICS)                   *
      * LINKAGE  : POOLPARM                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-03-15 BHS  ORIGINAL - NOTIONAL POOLING                    *
      * 2009-01-20 DPS  RATES AS SPREADS TO FED FUNDS / PRIME          *
      * 2009-02-02 DPS  INC11733 CREDIT RATE FLOORED AT ZERO           *
      * 2013-06-11 DPS  POOL LIMIT CHECK ON GROSS DEBIT POSITION       *
      * 2019-03-25 AKW  MEMBER TABLE 10 -> 20                          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POOLALOC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  WS-DAY-BASIS                PIC 9(03) VALUE 360.
           05  WS-MAX-MEMBERS              PIC 9(02) VALUE 21.
       01  WS-WORK.
           05  WS-IX                       PIC 9(02).
           05  WS-HEADER-IX                PIC 9(02).
           05  WS-LARGEST-IX               PIC 9(02).
           05  WS-LARGEST-BAL              PIC S9(13)V99 COMP-3.
           05  WS-ABS-NET                  PIC S9(13)V99 COMP-3.
           05  WS-ABS-BAL                  PIC S9(13)V99 COMP-3.
           05  WS-POOL-INTEREST            PIC S9(11)V99 COMP-3.
           05  WS-ALLOCATED                PIC S9(11)V99 COMP-3.
           05  WS-SHARE                    PIC S9(11)V99 COMP-3.
           05  WS-DIFFERENCE               PIC S9(11)V99 COMP-3.
           05  WS-INTEREST-WORK            PIC S9(13)V9(06) COMP-3.
           05  WS-SIDE                     PIC X(01).
               88  WS-CREDIT-SIDE                    VALUE 'C'.
               88  WS-DEBIT-SIDE                     VALUE 'D'.

       LINKAGE SECTION.
       COPY POOLPARM.

       PROCEDURE DIVISION USING POOL-ALLOC-PARMS.
       0000-MAIN.
           PERFORM 1000-VALIDATE
           IF PLA-RETURN-CODE = 0
               PERFORM 2000-POOL-POSITION
               PERFORM 3000-RATES
               PERFORM 4000-POOL-INTEREST
               PERFORM 5000-ALLOCATE
               PERFORM 6000-LIMIT-CHECK
           END-IF
           GOBACK.

       1000-VALIDATE.
           MOVE ZERO   TO PLA-RETURN-CODE PLA-GROSS-CREDIT-POS
                          PLA-GROSS-DEBIT-POS PLA-NET-POSITION
                          PLA-TOTAL-CREDIT-INT PLA-TOTAL-DEBIT-INT
                          PLA-CREDIT-RATE PLA-DEBIT-RATE WS-HEADER-IX
           MOVE 'N'    TO PLA-LIMIT-BREACH-FLAG
           MOVE SPACES TO PLA-REASON-CODE
           IF PLA-MEMBER-COUNT = 0 OR PLA-MEMBER-COUNT > WS-MAX-MEMBERS
               MOVE 12     TO PLA-RETURN-CODE
               MOVE 'EMBR' TO PLA-REASON-CODE
               EXIT PARAGRAPH
           END-IF
           IF NOT PLA-PROPORTIONAL AND NOT PLA-TO-HEADER
               MOVE 12     TO PLA-RETURN-CODE
               MOVE 'EMTH' TO PLA-REASON-CODE
               EXIT PARAGRAPH
           END-IF
           IF PLA-DAYS = 0
               MOVE 12     TO PLA-RETURN-CODE
               MOVE 'EDAY' TO PLA-REASON-CODE
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PLA-MEMBER-COUNT
               MOVE ZERO TO PLA-MBR-CREDIT-INT(WS-IX)
                            PLA-MBR-DEBIT-INT(WS-IX)
               IF PLA-MBR-HEADER-FLAG(WS-IX) = 'Y'
                   MOVE WS-IX TO WS-HEADER-IX
               END-IF
           END-PERFORM
           IF PLA-TO-HEADER AND WS-HEADER-IX = 0
               MOVE 12     TO PLA-RETURN-CODE
               MOVE 'EHDR' TO PLA-REASON-CODE
           END-IF.

       2000-POOL-POSITION.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PLA-MEMBER-COUNT
               IF PLA-MBR-AVG-BAL(WS-IX) > 0
                   ADD PLA-MBR-AVG-BAL(WS-IX) TO PLA-GROSS-CREDIT-POS
               ELSE
                   SUBTRACT PLA-MBR-AVG-BAL(WS-IX)
                       FROM PLA-GROSS-DEBIT-POS
               END-IF
           END-PERFORM
           COMPUTE PLA-NET-POSITION =
                   PLA-GROSS-CREDIT-POS - PLA-GROSS-DEBIT-POS.

       3000-RATES.
           IF PLA-REF-CREDIT-RATE > PLA-CREDIT-SPREAD
               COMPUTE PLA-CREDIT-RATE =
                       PLA-REF-CREDIT-RATE - PLA-CREDIT-SPREAD
           ELSE
               MOVE ZERO TO PLA-CREDIT-RATE
           END-IF
           COMPUTE PLA-DEBIT-RATE =
                   PLA-REF-DEBIT-RATE + PLA-DEBIT-SPREAD.

       4000-POOL-INTEREST.
           IF PLA-NET-POSITION >= 0
               SET WS-CREDIT-SIDE TO TRUE
               COMPUTE WS-INTEREST-WORK =
                       PLA-NET-POSITION * PLA-CREDIT-RATE / 100
                     * PLA-DAYS / WS-DAY-BASIS
               COMPUTE PLA-TOTAL-CREDIT-INT ROUNDED = WS-INTEREST-WORK
               MOVE PLA-TOTAL-CREDIT-INT TO WS-POOL-INTEREST
               MOVE 'NETC' TO PLA-REASON-CODE
           ELSE
               SET WS-DEBIT-SIDE TO TRUE
               COMPUTE WS-ABS-NET = 0 - PLA-NET-POSITION
               COMPUTE WS-INTEREST-WORK =
                       WS-ABS-NET * PLA-DEBIT-RATE / 100
                     * PLA-DAYS / WS-DAY-BASIS
               COMPUTE PLA-TOTAL-DEBIT-INT ROUNDED = WS-INTEREST-WORK
               MOVE PLA-TOTAL-DEBIT-INT TO WS-POOL-INTEREST
               MOVE 'NETD' TO PLA-REASON-CODE
           END-IF.

      *----------------------------------------------------------------*
      * ALLOCATION                                                     *
      *----------------------------------------------------------------*
       5000-ALLOCATE.
           IF WS-POOL-INTEREST = 0
               EXIT PARAGRAPH
           END-IF
           IF PLA-TO-HEADER
               IF WS-CREDIT-SIDE
                   MOVE WS-POOL-INTEREST
                     TO PLA-MBR-CREDIT-INT(WS-HEADER-IX)
               ELSE
                   MOVE WS-POOL-INTEREST
                     TO PLA-MBR-DEBIT-INT(WS-HEADER-IX)
               END-IF
               EXIT PARAGRAPH
           END-IF

           MOVE ZERO TO WS-ALLOCATED WS-LARGEST-IX WS-LARGEST-BAL
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PLA-MEMBER-COUNT
               IF WS-CREDIT-SIDE AND PLA-MBR-AVG-BAL(WS-IX) > 0
                   COMPUTE PLA-MBR-CREDIT-INT(WS-IX) ROUNDED =
                           WS-POOL-INTEREST * PLA-MBR-AVG-BAL(WS-IX)
                         / PLA-GROSS-CREDIT-POS
                   ADD PLA-MBR-CREDIT-INT(WS-IX) TO WS-ALLOCATED
                   MOVE PLA-MBR-AVG-BAL(WS-IX) TO WS-ABS-BAL
                   PERFORM 5100-TRACK-LARGEST
               END-IF
               IF WS-DEBIT-SIDE AND PLA-MBR-AVG-BAL(WS-IX) < 0
                   COMPUTE WS-ABS-BAL = 0 - PLA-MBR-AVG-BAL(WS-IX)
                   COMPUTE PLA-MBR-DEBIT-INT(WS-IX) ROUNDED =
                           WS-POOL-INTEREST * WS-ABS-BAL
                         / PLA-GROSS-DEBIT-POS
                   ADD PLA-MBR-DEBIT-INT(WS-IX) TO WS-ALLOCATED
                   PERFORM 5100-TRACK-LARGEST
               END-IF
           END-PERFORM

      *    PENNY DIFFERENCE FROM ROUNDING TO THE LARGEST SHARE
           COMPUTE WS-DIFFERENCE = WS-POOL-INTEREST - WS-ALLOCATED
           IF WS-DIFFERENCE NOT = 0 AND WS-LARGEST-IX > 0
               IF WS-CREDIT-SIDE
                   ADD WS-DIFFERENCE
                       TO PLA-MBR-CREDIT-INT(WS-LARGEST-IX)
               ELSE
                   ADD WS-DIFFERENCE
                       TO PLA-MBR-DEBIT-INT(WS-LARGEST-IX)
               END-IF
           END-IF.

       5100-TRACK-LARGEST.
           IF WS-ABS-BAL > WS-LARGEST-BAL
               MOVE WS-ABS-BAL TO WS-LARGEST-BAL
               MOVE WS-IX      TO WS-LARGEST-IX
           END-IF.

      *    2013-06 CREDIT RISK - AGGREGATE OVERDRAWN POSITION OF THE
      *    MEMBERS MAY NOT EXCEED THE APPROVED POOL LIMIT
       6000-LIMIT-CHECK.
           IF PLA-GROSS-DEBIT-POS > PLA-POOL-LIMIT
               MOVE 'Y'    TO PLA-LIMIT-BREACH-FLAG
               MOVE 04     TO PLA-RETURN-CODE
           END-IF.
