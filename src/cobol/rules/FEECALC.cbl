      *================================================================*
      * PROGRAM  : FEECALC                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : RETURNS THE AMOUNT AND POSTING CODE OF A DEPOSIT    *
      *            SERVICE FEE. ELIGIBILITY FOR A MAINTENANCE FEE      *
      *            WAIVER IS DECIDED SEPARATELY BY FEEWAIVE.           *
      *              MONTHLY MAINTENANCE  DDAP 12  DDPR 25  MMKT 15    *
      *                                   SVST 5   OTHERS NONE         *
      *              WIRE  OUTGOING DOMESTIC 25  INTERNATIONAL 45      *
      *                    INCOMING 15                                 *
      *              PAPER STATEMENT 2 (NOT ENROLLED IN E-STATEMENTS)  *
      *              STOP PAYMENT 30 PER ITEM                          *
      *            PRIVATE WEALTH CLIENTS PAY NO WIRE OR STOP PAYMENT  *
      *            FEES. CORPORATE / INSTITUTIONAL WIRE FEES ARE       *
      *            BILLED THROUGH ACCOUNT ANALYSIS (ANLYSFEE), NOT     *
      *            HERE.                                               *
      * LINKAGE  : FEEPARM                                             *
      * CALLED BY: FEEASSES, POSTTRAN, WIREOUT, STOPPAY (CB14)         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - MAINTENANCE FEE ONLY                *
      * 2004-03-22 BHS  WIRE AND STOP PAYMENT FEES                     *
      * 2010-07-01 DPS  REG E - ONE TRAN CODE PER FEE TYPE             *
      * 2014-09-02 DPS  DDAP FEE 10.00 -> 12.00 - CHG1140              *
      * 2018-01-15 AKW  PAPER STATEMENT FEE - CHG1502                  *
      * 2019-08-19 AKW  PRIVATE WEALTH FEE EXEMPTIONS                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEECALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    MONTHLY MAINTENANCE FEE BY PRODUCT - PROD(4) FEE 9(3)V99
       01  WS-MAINT-FEE-VALUES.
           05  FILLER  PIC X(04)    VALUE 'DDAP'.
           05  FILLER  PIC 9(03)V99 VALUE 12.00.
           05  FILLER  PIC X(04)    VALUE 'DDPR'.
           05  FILLER  PIC 9(03)V99 VALUE 25.00.
           05  FILLER  PIC X(04)    VALUE 'MMKT'.
           05  FILLER  PIC 9(03)V99 VALUE 15.00.
           05  FILLER  PIC X(04)    VALUE 'SVST'.
           05  FILLER  PIC 9(03)V99 VALUE 5.00.
       01  WS-MAINT-FEE-TABLE REDEFINES WS-MAINT-FEE-VALUES.
           05  WS-MF-ENTRY OCCURS 4 TIMES INDEXED BY WS-MF-IDX.
               10  WS-MF-PRODUCT           PIC X(04).
               10  WS-MF-AMOUNT            PIC 9(03)V99.
      *    2014-09 DPS DDAP WAS 10.00 - CHG1140
      *    05  FILLER  PIC 9(03)V99 VALUE 10.00.

       01  WS-SERVICE-FEES.
           05  WS-WIRE-DOM-OUT-FEE         PIC 9(03)V99 VALUE 25.00.
           05  WS-WIRE-INTL-OUT-FEE        PIC 9(03)V99 VALUE 45.00.
           05  WS-WIRE-INCOMING-FEE        PIC 9(03)V99 VALUE 15.00.
           05  WS-PAPER-STMT-FEE           PIC 9(03)V99 VALUE 2.00.
           05  WS-STOP-PAYMENT-FEE         PIC 9(03)V99 VALUE 30.00.

       01  WS-UNIT-FEE                     PIC 9(05)V99.
       01  WS-COUNT                        PIC 9(03).

       LINKAGE SECTION.
       COPY FEEPARM.

       PROCEDURE DIVISION USING FEECALC-PARMS.
       0000-MAIN.
           MOVE ZERO   TO FEE-AMOUNT FEE-RETURN-CODE WS-UNIT-FEE
           MOVE SPACES TO FEE-TRAN-CODE FEE-DESCRIPTION
           MOVE FEE-ITEM-COUNT TO WS-COUNT
           IF WS-COUNT = ZERO
               MOVE 1 TO WS-COUNT
           END-IF

           EVALUATE TRUE
               WHEN FEE-MAINTENANCE
                   PERFORM 1000-MAINTENANCE-FEE
               WHEN FEE-WIRE-DOMESTIC
               WHEN FEE-WIRE-INTL
               WHEN FEE-WIRE-INCOMING
                   PERFORM 2000-WIRE-FEE
               WHEN FEE-PAPER-STMT
                   PERFORM 3000-PAPER-STATEMENT-FEE
               WHEN FEE-STOP-PAYMENT
                   PERFORM 4000-STOP-PAYMENT-FEE
               WHEN OTHER
                   MOVE 12 TO FEE-RETURN-CODE
                   GOBACK
           END-EVALUATE

           COMPUTE FEE-AMOUNT = WS-UNIT-FEE * WS-COUNT
           IF FEE-AMOUNT = ZERO
               MOVE 04 TO FEE-RETURN-CODE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
       1000-MAINTENANCE-FEE.
           MOVE 'WFMN' TO FEE-TRAN-CODE
           MOVE 'MONTHLY MAINTENANCE FEE' TO FEE-DESCRIPTION
           MOVE 1 TO WS-COUNT
           SET WS-MF-IDX TO 1
           SEARCH WS-MF-ENTRY
               AT END
                   MOVE ZERO TO WS-UNIT-FEE
               WHEN WS-MF-PRODUCT(WS-MF-IDX) = FEE-PRODUCT-CODE
                   MOVE WS-MF-AMOUNT(WS-MF-IDX) TO WS-UNIT-FEE
           END-SEARCH.

      *----------------------------------------------------------------*
       2000-WIRE-FEE.
           MOVE 'WFWR' TO FEE-TRAN-CODE
           EVALUATE TRUE
               WHEN FEE-WIRE-DOMESTIC
                   MOVE WS-WIRE-DOM-OUT-FEE  TO WS-UNIT-FEE
                   MOVE 'OUTGOING DOMESTIC WIRE FEE' TO FEE-DESCRIPTION
               WHEN FEE-WIRE-INTL
                   MOVE WS-WIRE-INTL-OUT-FEE TO WS-UNIT-FEE
                   MOVE 'OUTGOING INTL WIRE FEE' TO FEE-DESCRIPTION
               WHEN FEE-WIRE-INCOMING
                   MOVE WS-WIRE-INCOMING-FEE TO WS-UNIT-FEE
                   MOVE 'INCOMING WIRE FEE' TO FEE-DESCRIPTION
           END-EVALUATE
           EVALUATE FEE-CUST-SEGMENT
               WHEN 'PW'
                   MOVE ZERO TO WS-UNIT-FEE
               WHEN 'CB'
               WHEN 'IN'
                   MOVE ZERO TO WS-UNIT-FEE
                   MOVE 'WIRE FEE BILLED VIA ANALYSIS'
                     TO FEE-DESCRIPTION
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-PAPER-STATEMENT-FEE.
           MOVE 'WFOT' TO FEE-TRAN-CODE
           MOVE 'PAPER STATEMENT FEE' TO FEE-DESCRIPTION
           MOVE 1 TO WS-COUNT
           IF FEE-ESTMT-FLAG = 'Y' OR FEE-CUST-SEGMENT = 'PW'
               MOVE ZERO TO WS-UNIT-FEE
           ELSE
               MOVE WS-PAPER-STMT-FEE TO WS-UNIT-FEE
           END-IF.

      *----------------------------------------------------------------*
       4000-STOP-PAYMENT-FEE.
           MOVE 'WFOT' TO FEE-TRAN-CODE
           MOVE 'STOP PAYMENT FEE' TO FEE-DESCRIPTION
           IF FEE-CUST-SEGMENT = 'PW'
               MOVE ZERO TO WS-UNIT-FEE
           ELSE
               MOVE WS-STOP-PAYMENT-FEE TO WS-UNIT-FEE
           END-IF.
