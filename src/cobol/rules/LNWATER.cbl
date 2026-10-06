      *================================================================*
      * PROGRAM  : LNWATER                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOAN PAYMENT APPLICATION WATERFALL.                 *
      *            PAYOFF      - PAYMENT COVERS PRINCIPAL + ACCRUED    *
      *                          INTEREST + LATE FEES: LOAN PAID IN    *
      *                          FULL, EXCESS TO SUSPENSE FOR REFUND.  *
      *            CURRENT     - FEES, THEN BILLED INTEREST, THEN      *
      *                          BILLED PRINCIPAL.                     *
      *            DELINQUENT  - OLDEST INSTALLMENT FIRST (INTEREST    *
      *                          THEN PRINCIPAL OF EACH), LATE FEES    *
      *                          LAST.                                 *
      *            NON-ACCRUAL - COST RECOVERY: ALL TO PRINCIPAL.      *
      *            CHARGED-OFF - ALL TO RECOVERY.                      *
      *            OVERPAYMENT - UNDER $1.00 TO SUSPENSE; LPER / LLOC  *
      *                          REDUCE PRINCIPAL; LNTM / LCRE HELD IN *
      *                          SUSPENSE (PREPAYMENT TERMS APPLY).    *
      * LINKAGE  : LNWPARM                                             *
      * CALLED BY: LNPAYAP                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 1996-09-23 RJT  NON-ACCRUAL COST RECOVERY PER OCC EXAM         *
      * 2006-02-13 BHS  DUE TABLE - OLDEST INSTALLMENT FIRST - CHG0412 *
      * 2011-01-18 DPS  OVERPAYMENT UNDER $1 TO SUSPENSE - INC14410    *
      * 2015-07-01 DPS  LATE FEES APPLIED AFTER INSTALLMENTS ON        *
      *                 DELINQUENT LOANS (CFPB EXAM FINDING) - CHG1655 *
      * 2016-04-25 AKW  LPER EXTRA PAYMENTS REDUCE PRINCIPAL           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNWATER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  WS-REMAIN                   PIC S9(13)V99 COMP-3.
           05  WS-OWED                     PIC S9(13)V99 COMP-3.
           05  WS-APPLY                    PIC S9(13)V99 COMP-3.
           05  WS-ACCRUED-CENTS            PIC S9(11)V99 COMP-3.
           05  WS-I                        PIC 9(02).
           05  WS-J                        PIC 9(02).
           05  WS-NEW-COUNT                PIC 9(02).
       01  WS-SUSPENSE-THRESHOLD           PIC 9(03)V99 VALUE 1.00.

       LINKAGE SECTION.
       COPY LNWPARM.

       PROCEDURE DIVISION USING WATERFALL-PARMS.
       0000-MAIN.
           MOVE 00 TO LNW-RETURN-CODE
           MOVE ZERO TO LNW-APPLIED-FEES LNW-APPLIED-INT
                        LNW-APPLIED-PRIN LNW-APPLIED-EXTRA-PRIN
                        LNW-APPLIED-SUSPENSE LNW-APPLIED-RECOVERY
                        LNW-INSTALLMENTS-PAID LNW-PAYOFF-AMT
           MOVE 'N' TO LNW-PAYOFF-FLAG
           MOVE SPACE TO LNW-WATERFALL-USED
           IF LNW-PAYMENT-AMT NOT > ZERO OR LNW-DUE-COUNT > 8
               MOVE 12 TO LNW-RETURN-CODE
               GOBACK
           END-IF
           MOVE LNW-PAYMENT-AMT TO WS-REMAIN

      *    PAYOFF AMOUNT - PRINCIPAL + ACCRUED INTEREST + LATE FEES
           COMPUTE WS-ACCRUED-CENTS ROUNDED = LNW-ACCRUED-INT
           COMPUTE LNW-PAYOFF-AMT =
               LNW-PRINCIPAL-BAL + WS-ACCRUED-CENTS + LNW-LATE-FEES

           EVALUATE TRUE
               WHEN LNW-LOAN-STATUS = 'C'
                   SET LNW-WF-RECOVERY TO TRUE
                   PERFORM 4000-RECOVERY
               WHEN LNW-LOAN-STATUS = 'P'
                   SET LNW-WF-CURRENT TO TRUE
                   PERFORM 6100-TO-SUSPENSE
               WHEN WS-REMAIN NOT < LNW-PAYOFF-AMT
                   SET LNW-WF-PAYOFF TO TRUE
                   PERFORM 5000-PAYOFF
               WHEN LNW-LOAN-STATUS = 'N'
                   SET LNW-WF-NON-ACCRUAL TO TRUE
                   PERFORM 3000-NON-ACCRUAL
               WHEN LNW-LOAN-STATUS = 'D'
               WHEN LNW-DAYS-PAST-DUE > ZERO
                   SET LNW-WF-DELINQUENT TO TRUE
                   PERFORM 2000-DELINQUENT
                   PERFORM 6000-OVERPAYMENT
               WHEN OTHER
                   SET LNW-WF-CURRENT TO TRUE
                   PERFORM 1000-CURRENT
                   PERFORM 6000-OVERPAYMENT
           END-EVALUATE

           PERFORM 7000-COMPRESS-DUE-TABLE
           GOBACK.

      *================================================================*
      * CURRENT LOAN - FEES, INTEREST, PRINCIPAL                       *
      *================================================================*
       1000-CURRENT.
           PERFORM 2100-APPLY-FEES
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LNW-DUE-COUNT OR WS-REMAIN = ZERO
               PERFORM 2200-APPLY-INSTALLMENT-INT
           END-PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LNW-DUE-COUNT OR WS-REMAIN = ZERO
               PERFORM 2300-APPLY-INSTALLMENT-PRIN
           END-PERFORM.

      *================================================================*
      * DELINQUENT LOAN - OLDEST INSTALLMENT FIRST, FEES LAST          *
      *================================================================*
       2000-DELINQUENT.
      *    2015-07 DPS CHG1655 - FEES NO LONGER TAKEN FIRST
      *    PERFORM 2100-APPLY-FEES
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LNW-DUE-COUNT OR WS-REMAIN = ZERO
               PERFORM 2200-APPLY-INSTALLMENT-INT
               PERFORM 2300-APPLY-INSTALLMENT-PRIN
           END-PERFORM
           PERFORM 2100-APPLY-FEES.

       2100-APPLY-FEES.
           MOVE LNW-LATE-FEES TO WS-OWED
           PERFORM 8000-APPLY
           SUBTRACT WS-APPLY FROM LNW-LATE-FEES
           ADD WS-APPLY TO LNW-APPLIED-FEES.

       2200-APPLY-INSTALLMENT-INT.
           MOVE LNW-DUE-INT(WS-I) TO WS-OWED
           PERFORM 8000-APPLY
           SUBTRACT WS-APPLY FROM LNW-DUE-INT(WS-I)
           SUBTRACT WS-APPLY FROM LNW-ACCRUED-INT
           ADD WS-APPLY TO LNW-APPLIED-INT.

       2300-APPLY-INSTALLMENT-PRIN.
           MOVE LNW-DUE-PRIN(WS-I) TO WS-OWED
           PERFORM 8000-APPLY
           SUBTRACT WS-APPLY FROM LNW-DUE-PRIN(WS-I)
           SUBTRACT WS-APPLY FROM LNW-PRINCIPAL-BAL
           ADD WS-APPLY TO LNW-APPLIED-PRIN.

      *================================================================*
      * NON-ACCRUAL - COST RECOVERY. THE CASH REDUCES BOOK PRINCIPAL;  *
      * THE SAME AMOUNT SATISFIES THE CONTRACTUAL INSTALLMENTS OLDEST  *
      * FIRST SO THE DELINQUENCY AGING STILL MOVES.                    *
      *================================================================*
       3000-NON-ACCRUAL.
           MOVE LNW-PRINCIPAL-BAL TO WS-OWED
           PERFORM 8000-APPLY
           SUBTRACT WS-APPLY FROM LNW-PRINCIPAL-BAL
           ADD WS-APPLY TO LNW-APPLIED-PRIN
           MOVE WS-APPLY TO WS-OWED
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LNW-DUE-COUNT OR WS-OWED = ZERO
               IF LNW-DUE-INT(WS-I) NOT > WS-OWED
                   SUBTRACT LNW-DUE-INT(WS-I) FROM WS-OWED
                   MOVE ZERO TO LNW-DUE-INT(WS-I)
               ELSE
                   SUBTRACT WS-OWED FROM LNW-DUE-INT(WS-I)
                   MOVE ZERO TO WS-OWED
               END-IF
               IF LNW-DUE-PRIN(WS-I) NOT > WS-OWED
                   SUBTRACT LNW-DUE-PRIN(WS-I) FROM WS-OWED
                   MOVE ZERO TO LNW-DUE-PRIN(WS-I)
               ELSE
                   SUBTRACT WS-OWED FROM LNW-DUE-PRIN(WS-I)
                   MOVE ZERO TO WS-OWED
               END-IF
           END-PERFORM
           IF WS-REMAIN > ZERO
               PERFORM 6100-TO-SUSPENSE
           END-IF.

      *================================================================*
      * CHARGED-OFF - RECOVERY                                         *
      *================================================================*
       4000-RECOVERY.
           MOVE WS-REMAIN TO LNW-APPLIED-RECOVERY
           MOVE ZERO TO WS-REMAIN.

      *================================================================*
      * PAYOFF - SETTLE EVERYTHING, EXCESS TO SUSPENSE FOR REFUND      *
      *================================================================*
       5000-PAYOFF.
           MOVE LNW-LATE-FEES     TO LNW-APPLIED-FEES
           MOVE WS-ACCRUED-CENTS  TO LNW-APPLIED-INT
           MOVE LNW-PRINCIPAL-BAL TO LNW-APPLIED-PRIN
           SUBTRACT LNW-PAYOFF-AMT FROM WS-REMAIN
           MOVE ZERO TO LNW-LATE-FEES LNW-ACCRUED-INT
                        LNW-PRINCIPAL-BAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LNW-DUE-COUNT
               MOVE ZERO TO LNW-DUE-INT(WS-I) LNW-DUE-PRIN(WS-I)
           END-PERFORM
           SET LNW-PAID-IN-FULL TO TRUE
           IF WS-REMAIN > ZERO
               PERFORM 6100-TO-SUSPENSE
           END-IF.

      *================================================================*
      * OVERPAYMENT                                                    *
      *================================================================*
       6000-OVERPAYMENT.
           IF WS-REMAIN = ZERO
               EXIT PARAGRAPH
           END-IF
           IF WS-REMAIN < WS-SUSPENSE-THRESHOLD
               PERFORM 6100-TO-SUSPENSE
               EXIT PARAGRAPH
           END-IF
           EVALUATE LNW-PRODUCT-CODE
               WHEN 'LPER'
               WHEN 'LLOC'
                   MOVE LNW-PRINCIPAL-BAL TO WS-OWED
                   PERFORM 8000-APPLY
                   SUBTRACT WS-APPLY FROM LNW-PRINCIPAL-BAL
                   ADD WS-APPLY TO LNW-APPLIED-EXTRA-PRIN
                   IF WS-REMAIN > ZERO
                       PERFORM 6100-TO-SUSPENSE
                   END-IF
               WHEN OTHER
      *            COMMERCIAL TERM / CRE - PREPAYMENT TERMS IN NOTE
                   PERFORM 6100-TO-SUSPENSE
           END-EVALUATE.

       6100-TO-SUSPENSE.
           ADD WS-REMAIN TO LNW-SUSPENSE-BAL
           ADD WS-REMAIN TO LNW-APPLIED-SUSPENSE
           MOVE ZERO TO WS-REMAIN.

      *================================================================*
      * DROP FULLY PAID INSTALLMENTS, KEEP OLDEST-FIRST ORDER          *
      *================================================================*
       7000-COMPRESS-DUE-TABLE.
           MOVE ZERO TO WS-NEW-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > LNW-DUE-COUNT
               IF LNW-DUE-INT(WS-I) = ZERO
                  AND LNW-DUE-PRIN(WS-I) = ZERO
                   ADD 1 TO LNW-INSTALLMENTS-PAID
               ELSE
                   ADD 1 TO WS-NEW-COUNT
                   IF WS-NEW-COUNT NOT = WS-I
                       MOVE LNW-DUE-ENTRY(WS-I)
                         TO LNW-DUE-ENTRY(WS-NEW-COUNT)
                   END-IF
               END-IF
           END-PERFORM
           PERFORM VARYING WS-J FROM WS-NEW-COUNT BY 1
                   UNTIL WS-J NOT < LNW-DUE-COUNT
               COMPUTE WS-I = WS-J + 1
               MOVE ZERO   TO LNW-DUE-DATE(WS-I) LNW-DUE-INT(WS-I)
                              LNW-DUE-PRIN(WS-I)
               MOVE SPACE  TO LNW-DUE-FEE-FLAG(WS-I)
           END-PERFORM
           MOVE WS-NEW-COUNT TO LNW-DUE-COUNT.

      *----------------------------------------------------------------*
      * APPLY THE LESSER OF WHAT REMAINS AND WHAT IS OWED              *
      *----------------------------------------------------------------*
       8000-APPLY.
           IF WS-OWED NOT > ZERO
               MOVE ZERO TO WS-APPLY
           ELSE
               IF WS-REMAIN < WS-OWED
                   MOVE WS-REMAIN TO WS-APPLY
               ELSE
                   MOVE WS-OWED TO WS-APPLY
               END-IF
           END-IF
           SUBTRACT WS-APPLY FROM WS-REMAIN.
