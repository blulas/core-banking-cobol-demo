      *================================================================*
      * PROGRAM  : SWEEPRUL                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COMPUTES THE SWEEP AMOUNT AND DIRECTION FOR ONE     *
      *            SWEEP DEFINITION GIVEN THE CURRENT BALANCES OF THE  *
      *            SOURCE AND TARGET ACCOUNTS.                         *
      *              Z  ZERO BALANCE - SOURCE SWEPT TO EXACTLY ZERO    *
      *              T  TARGET BALANCE - EXCESS INVESTED, DEFICIT      *
      *                 FUNDED BACK FROM THE INVESTMENT ACCOUNT        *
      *              R  RANGE - ONLY WHEN OUTSIDE MIN/MAX, BACK TO     *
      *                 THE MIDPOINT OF THE BAND                       *
      *            SWEEPS ALWAYS WORK FROM THE AVAILABLE (COLLECTED)   *
      *            BALANCE. UNCOLLECTED FUNDS UNDER HOLD ARE NEVER     *
      *            SWEPT OUT.                                          *
      * CALLED BY: SWEEPRUN (BATCH), CMPOSINQ (CICS - PROJECTION)      *
      * LINKAGE  : SWRPARM                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-10-01 BHS  ORIGINAL - TXB CASH CONCENTRATION              *
      * 2002-02-11 BHS  INC0388 SWEEP FROM AVAILABLE NOT LEDGER        *
      * 2004-05-17 BHS  RANGE (BAND) SWEEPS                            *
      * 2008-09-29 DPS  INC11402 ZBA PARTIAL FUNDING WHEN PARENT SHORT *
      * 2011-04-12 DPS  CHG1544 INCREMENT AND MINIMUM TRANSFER         *
      * 2012-01-09 DPS  CHG1690 CMMI MINIMUM PURCHASE $10,000          *
      * 2016-03-14 AKW  CHG2741 DEFICIT FUNDING LIMITED TO INVESTMENT  *
      *                 ACCOUNT AVAILABLE BALANCE                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWEEPRUL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *--- RULE CONSTANTS ---------------------------------------------*
       01  WS-RULE-CONSTANTS.
           05  WS-DEFAULT-INCREMENT        PIC S9(09)V99 COMP-3
                                           VALUE 1000.00.
           05  WS-DEFAULT-MIN-TRANSFER     PIC S9(09)V99 COMP-3
                                           VALUE 1000.00.
           05  WS-CMMI-MIN-PURCHASE        PIC S9(09)V99 COMP-3
                                           VALUE 10000.00.
       01  WS-WORK.
           05  WS-EXCESS                   PIC S9(13)V99 COMP-3.
           05  WS-DEFICIT                  PIC S9(13)V99 COMP-3.
           05  WS-MIDPOINT                 PIC S9(13)V99 COMP-3.
           05  WS-UNITS                    PIC S9(13)    COMP-3.
           05  WS-ROUNDED                  PIC S9(13)V99 COMP-3.
           05  WS-AMOUNT-IN                PIC S9(13)V99 COMP-3.
           05  WS-FUNDABLE                 PIC S9(13)V99 COMP-3.

       LINKAGE SECTION.
       COPY SWRPARM.

       PROCEDURE DIVISION USING SWEEP-RULE-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF SWR-RETURN-CODE = 0
               EVALUATE TRUE
                   WHEN SWR-TYPE-ZBA
                       PERFORM 2000-ZBA-SWEEP
                   WHEN SWR-TYPE-TARGET
                       PERFORM 3000-TARGET-SWEEP
                   WHEN SWR-TYPE-RANGE
                       PERFORM 4000-RANGE-SWEEP
               END-EVALUATE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
      * EFFECTIVE INCREMENT AND MINIMUM TRANSFER                       *
      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE ZERO   TO SWR-SWEEP-AMOUNT SWR-REQUIRED-AMT
                          SWR-RETURN-CODE
           SET SWR-DIR-NONE TO TRUE
           MOVE 'N'    TO SWR-PARTIAL-FLAG SWR-ALERT-FLAG
           MOVE SPACES TO SWR-REASON-CODE SWR-REASON-TEXT

           IF NOT SWR-TYPE-ZBA AND NOT SWR-TYPE-TARGET
              AND NOT SWR-TYPE-RANGE
               MOVE 12     TO SWR-RETURN-CODE
               MOVE 'ETYP' TO SWR-REASON-CODE
               MOVE 'INVALID SWEEP TYPE' TO SWR-REASON-TEXT
               EXIT PARAGRAPH
           END-IF
           IF SWR-TYPE-RANGE AND SWR-MIN-BAL > SWR-MAX-BAL
               MOVE 12     TO SWR-RETURN-CODE
               MOVE 'EBND' TO SWR-REASON-CODE
               MOVE 'RANGE MINIMUM EXCEEDS MAXIMUM' TO SWR-REASON-TEXT
               EXIT PARAGRAPH
           END-IF
           IF SWR-TYPE-TARGET AND SWR-TARGET-BAL < 0
               MOVE 12     TO SWR-RETURN-CODE
               MOVE 'ETGT' TO SWR-REASON-CODE
               MOVE 'NEGATIVE TARGET BALANCE' TO SWR-REASON-TEXT
               EXIT PARAGRAPH
           END-IF

           IF SWR-INCREMENT > 0
               MOVE SWR-INCREMENT        TO SWR-EFF-INCREMENT
           ELSE
               MOVE WS-DEFAULT-INCREMENT TO SWR-EFF-INCREMENT
           END-IF

           IF SWR-MIN-TRANSFER > 0
               MOVE SWR-MIN-TRANSFER        TO SWR-EFF-MIN-TRANSFER
           ELSE
               MOVE WS-DEFAULT-MIN-TRANSFER TO SWR-EFF-MIN-TRANSFER
           END-IF
      *    CHG1690 - INSTITUTIONAL MONEY MARKET FUND MINIMUM PURCHASE /
      *    REDEMPTION IS $10,000 REGARDLESS OF THE SWEEP DEFINITION
           IF SWR-TGT-PRODUCT-CODE = 'CMMI'
              AND NOT SWR-TYPE-ZBA
              AND SWR-EFF-MIN-TRANSFER < WS-CMMI-MIN-PURCHASE
               MOVE WS-CMMI-MIN-PURCHASE TO SWR-EFF-MIN-TRANSFER
           END-IF.

      *----------------------------------------------------------------*
      * ZBA - SUB-ACCOUNT TO EXACTLY ZERO. NO INCREMENT, NO MINIMUM.   *
      * POSITIVE AVAILABLE -> CONCENTRATE TO PARENT.                   *
      * NEGATIVE AVAILABLE -> FUND FROM PARENT. IF THE PARENT CANNOT   *
      * COVER, FUND WHAT IS AVAILABLE AND RAISE AN ALERT (INC11402).   *
      *----------------------------------------------------------------*
       2000-ZBA-SWEEP.
           EVALUATE TRUE
               WHEN SWR-SRC-AVAIL-BAL > 0
                   MOVE SWR-SRC-AVAIL-BAL TO SWR-REQUIRED-AMT
                                             SWR-SWEEP-AMOUNT
                   SET SWR-DIR-OUT TO TRUE
                   IF SWR-SRC-HOLD-AMT > 0
                       MOVE 'ZHLD' TO SWR-REASON-CODE
                       MOVE 'ZBA SWEEP - UNCOLLECTED FUNDS RETAINED'
                         TO SWR-REASON-TEXT
                   ELSE
                       MOVE 'ZOUT' TO SWR-REASON-CODE
                       MOVE 'ZBA CONCENTRATION TO PARENT'
                         TO SWR-REASON-TEXT
                   END-IF
               WHEN SWR-SRC-AVAIL-BAL < 0
                   COMPUTE SWR-REQUIRED-AMT = 0 - SWR-SRC-AVAIL-BAL
                   EVALUATE TRUE
                       WHEN SWR-TGT-AVAIL-BAL >= SWR-REQUIRED-AMT
                           MOVE SWR-REQUIRED-AMT TO SWR-SWEEP-AMOUNT
                           SET SWR-DIR-IN TO TRUE
                           MOVE 'ZFND' TO SWR-REASON-CODE
                           MOVE 'ZBA FUNDED FROM PARENT'
                             TO SWR-REASON-TEXT
                       WHEN SWR-TGT-AVAIL-BAL > 0
                           MOVE SWR-TGT-AVAIL-BAL TO SWR-SWEEP-AMOUNT
                           SET SWR-DIR-IN TO TRUE
                           MOVE 'Y'    TO SWR-PARTIAL-FLAG
                                          SWR-ALERT-FLAG
                           MOVE 04     TO SWR-RETURN-CODE
                           MOVE 'ZPRT' TO SWR-REASON-CODE
                           MOVE 'ZBA PARTIAL FUNDING - PARENT SHORT'
                             TO SWR-REASON-TEXT
                       WHEN OTHER
                           MOVE 'Y'    TO SWR-ALERT-FLAG
                           MOVE 04     TO SWR-RETURN-CODE
                           MOVE 'ZNOF' TO SWR-REASON-CODE
                           MOVE 'ZBA NOT FUNDED - PARENT HAS NO FUNDS'
                             TO SWR-REASON-TEXT
                   END-EVALUATE
               WHEN OTHER
                   MOVE 'ZERO' TO SWR-REASON-CODE
                   MOVE 'ZBA ALREADY AT ZERO' TO SWR-REASON-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TARGET BALANCE                                                 *
      *  EXCESS  = AVAILABLE - TARGET, ROUNDED DOWN TO INCREMENT       *
      *  DEFICIT = TARGET - AVAILABLE, ROUNDED UP TO INCREMENT, BUT    *
      *            NEVER MORE THAN THE INVESTMENT ACCOUNT HOLDS        *
      *            (ROUNDED DOWN TO INCREMENT) - CHG2741               *
      *  AMOUNTS BELOW THE MINIMUM TRANSFER ARE NOT SWEPT              *
      *----------------------------------------------------------------*
       3000-TARGET-SWEEP.
           COMPUTE WS-EXCESS = SWR-SRC-AVAIL-BAL - SWR-TARGET-BAL
           EVALUATE TRUE
               WHEN WS-EXCESS > 0
                   MOVE WS-EXCESS TO SWR-REQUIRED-AMT
                   PERFORM 8000-ROUND-DOWN
                   MOVE WS-ROUNDED TO SWR-SWEEP-AMOUNT
                   SET SWR-DIR-OUT TO TRUE
                   MOVE 'TEXC' TO SWR-REASON-CODE
                   MOVE 'EXCESS OVER TARGET INVESTED'
                     TO SWR-REASON-TEXT
                   PERFORM 8200-APPLY-MINIMUM
               WHEN WS-EXCESS < 0
                   COMPUTE WS-DEFICIT = 0 - WS-EXCESS
                   MOVE WS-DEFICIT TO SWR-REQUIRED-AMT
                   PERFORM 8300-FUND-DEFICIT
               WHEN OTHER
                   MOVE 'TATT' TO SWR-REASON-CODE
                   MOVE 'ACCOUNT AT TARGET BALANCE' TO SWR-REASON-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RANGE - INSIDE THE BAND (INCLUSIVE) NOTHING MOVES. OUTSIDE IT  *
      * THE ACCOUNT IS BROUGHT BACK TO THE MIDPOINT OF THE BAND.       *
      *----------------------------------------------------------------*
       4000-RANGE-SWEEP.
           COMPUTE WS-MIDPOINT ROUNDED =
                   (SWR-MIN-BAL + SWR-MAX-BAL) / 2
           EVALUATE TRUE
               WHEN SWR-SRC-AVAIL-BAL > SWR-MAX-BAL
                   COMPUTE WS-EXCESS = SWR-SRC-AVAIL-BAL - WS-MIDPOINT
                   MOVE WS-EXCESS TO SWR-REQUIRED-AMT
                   PERFORM 8000-ROUND-DOWN
                   MOVE WS-ROUNDED TO SWR-SWEEP-AMOUNT
                   SET SWR-DIR-OUT TO TRUE
                   MOVE 'RHI ' TO SWR-REASON-CODE
                   MOVE 'ABOVE BAND - SWEPT TO MIDPOINT'
                     TO SWR-REASON-TEXT
                   PERFORM 8200-APPLY-MINIMUM
               WHEN SWR-SRC-AVAIL-BAL < SWR-MIN-BAL
                   COMPUTE WS-DEFICIT = WS-MIDPOINT - SWR-SRC-AVAIL-BAL
                   MOVE WS-DEFICIT TO SWR-REQUIRED-AMT
                   PERFORM 8300-FUND-DEFICIT
                   IF SWR-REASON-CODE = 'TDEF'
                       MOVE 'RLO ' TO SWR-REASON-CODE
                       MOVE 'BELOW BAND - FUNDED TO MIDPOINT'
                         TO SWR-REASON-TEXT
                   END-IF
               WHEN OTHER
                   MOVE 'RINB' TO SWR-REASON-CODE
                   MOVE 'WITHIN BAND - NO SWEEP' TO SWR-REASON-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8000-ROUND-DOWN.
           COMPUTE WS-UNITS = WS-EXCESS / SWR-EFF-INCREMENT
           COMPUTE WS-ROUNDED = WS-UNITS * SWR-EFF-INCREMENT.

       8100-ROUND-UP.
           COMPUTE WS-UNITS = WS-DEFICIT / SWR-EFF-INCREMENT
           IF WS-UNITS * SWR-EFF-INCREMENT < WS-DEFICIT
               ADD 1 TO WS-UNITS
           END-IF
           COMPUTE WS-ROUNDED = WS-UNITS * SWR-EFF-INCREMENT.

       8200-APPLY-MINIMUM.
           IF SWR-SWEEP-AMOUNT < SWR-EFF-MIN-TRANSFER
               MOVE ZERO   TO SWR-SWEEP-AMOUNT
               SET SWR-DIR-NONE TO TRUE
               MOVE 'BMIN' TO SWR-REASON-CODE
               MOVE 'BELOW MINIMUM TRANSFER - NOT SWEPT'
                 TO SWR-REASON-TEXT
           END-IF.

      *    DEFICIT FUNDING FROM THE INVESTMENT ACCOUNT (T AND R)
       8300-FUND-DEFICIT.
           PERFORM 8100-ROUND-UP
           MOVE WS-ROUNDED TO WS-AMOUNT-IN
           IF SWR-TGT-AVAIL-BAL <= 0
               MOVE 04     TO SWR-RETURN-CODE
               MOVE 'TNOF' TO SWR-REASON-CODE
               MOVE 'DEFICIT NOT FUNDED - INVESTMENT ACCT EMPTY'
                 TO SWR-REASON-TEXT
               EXIT PARAGRAPH
           END-IF
           IF SWR-TGT-AVAIL-BAL < WS-AMOUNT-IN
               MOVE SWR-TGT-AVAIL-BAL TO WS-EXCESS
               PERFORM 8000-ROUND-DOWN
               MOVE WS-ROUNDED TO WS-AMOUNT-IN
               MOVE 'Y'    TO SWR-PARTIAL-FLAG
               MOVE 04     TO SWR-RETURN-CODE
               MOVE 'TPRT' TO SWR-REASON-CODE
               MOVE 'DEFICIT PARTIALLY FUNDED' TO SWR-REASON-TEXT
           ELSE
               MOVE 'TDEF' TO SWR-REASON-CODE
               MOVE 'DEFICIT FUNDED FROM INVESTMENT ACCOUNT'
                 TO SWR-REASON-TEXT
           END-IF
           MOVE WS-AMOUNT-IN TO SWR-SWEEP-AMOUNT
           SET SWR-DIR-IN TO TRUE
           PERFORM 8200-APPLY-MINIMUM.
