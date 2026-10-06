      *================================================================*
      * PROGRAM  : ANLYSCAL                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : COMMERCIAL ACCOUNT ANALYSIS SERVICE CHARGE.         *
      *            SERVICE CHARGE = SUM(VOLUME x UNIT PRICE)           *
      *            NET CHARGE     = SERVICE CHARGE - EARNINGS CREDIT   *
      *                             (NEVER BELOW ZERO)                 *
      *            EXCESS EARNINGS CREDIT IS FORFEITED (BANK POLICY)   *
      *            EXCEPT FOR GOVERNMENT / PUBLIC ENTITIES (SEGMENT    *
      *            'IN') WHERE STATE PUBLIC-DEPOSIT RULES REQUIRE THE  *
      *            UNUSED CREDIT TO CARRY FORWARD FOR 3 MONTHS:        *
      *              - OLDEST CARRY-FORWARD IS USED FIRST, THEN THE    *
      *                CURRENT MONTH'S EARNINGS CREDIT                 *
      *              - UNUSED CURRENT CREDIT BECOMES BUCKET 1, UNUSED  *
      *                BUCKET 1 AGES TO 2, BUCKET 2 AGES TO 3, UNUSED  *
      *                BUCKET 3 EXPIRES                                *
      *            NET CHARGES UNDER $1.00 ARE WAIVED.                 *
      *            MSB CUSTOMERS PAY 150% OF THE ACH ITEM PRICE.       *
      * CALLED BY: ANLYSFEE                                            *
      * LINKAGE  : ANLYPARM                                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-06-01 RJT  ORIGINAL - COMMERCIAL ACCOUNT ANALYSIS         *
      * 2001-10-01 BHS  ZBA AND SWEEP MAINTENANCE PRICING              *
      * 2006-05-22 BHS  CHG0731 PUBLIC FUNDS CREDIT CARRY-FORWARD      *
      *                 (3 MONTHS) FOR SEGMENT IN                      *
      * 2012-02-01 DPS  CHG1702 PRICE INCREASE WIRES OUT 12.00->15.00  *
      * 2019-03-25 AKW  VIRTUAL ACCOUNT PRICING $1.00 / ACCT / MONTH   *
      * 2019-10-07 AKW  CHG3310 MSB ACH SURCHARGE 150%                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANLYSCAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *--- STANDARD PRICE LIST (TXB PRICING SCHEDULE 2019-A) ---------*
      *    CODE(4) DESCRIPTION(24) UNIT PRICE 9(05)V9(04)
       01  WS-PRICE-TABLE-VALUES.
           05  FILLER PIC X(37) VALUE
               'ACHIACH ITEMS ORIG/RECEIVED 000001000'.
           05  FILLER PIC X(37) VALUE
               'WIROWIRE TRANSFER - OUTGOING000150000'.
           05  FILLER PIC X(37) VALUE
               'WIRIWIRE TRANSFER - INCOMING000080000'.
           05  FILLER PIC X(37) VALUE
               'CHKPCHECKS PAID             000002000'.
           05  FILLER PIC X(37) VALUE
               'DEPODEPOSITS CREDITED       000005000'.
           05  FILLER PIC X(37) VALUE
               'MANTACCOUNT MAINTENANCE     000250000'.
           05  FILLER PIC X(37) VALUE
               'VIRTVIRTUAL ACCOUNT MAINT   000010000'.
           05  FILLER PIC X(37) VALUE
               'ZBAMZBA ACCOUNT MAINTENANCE 000350000'.
           05  FILLER PIC X(37) VALUE
               'SWPMSWEEP SERVICE           001000000'.
       01  WS-PRICE-TABLE REDEFINES WS-PRICE-TABLE-VALUES.
           05  WS-PRICE-ENTRY OCCURS 9 TIMES.
               10  WS-PRC-CODE             PIC X(04).
               10  WS-PRC-DESC             PIC X(24).
               10  WS-PRC-UNIT-PRICE       PIC 9(05)V9(04).
       01  WS-CONSTANTS.
           05  WS-SVC-ACH                  PIC 9(02) VALUE 1.
           05  WS-MSB-ACH-FACTOR           PIC 9(01)V99 VALUE 1.50.
           05  WS-MIN-CHARGE               PIC S9(05)V99 COMP-3
                                           VALUE 1.00.
           05  WS-CF-SEGMENT               PIC X(02) VALUE 'IN'.
       01  WS-WORK.
           05  WS-IX                       PIC 9(02).
           05  WS-REMAINING                PIC S9(11)V99 COMP-3.
           05  WS-CF-LEFT                  PIC S9(11)V99 COMP-3
                                           OCCURS 3 TIMES.
           05  WS-ECR-LEFT                 PIC S9(11)V99 COMP-3.
           05  WS-USE                      PIC S9(11)V99 COMP-3.

       LINKAGE SECTION.
       COPY ANLYPARM.

       PROCEDURE DIVISION USING ANALYSIS-CALC-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-PRICE-SERVICES
           IF ANL-SEGMENT = WS-CF-SEGMENT
               PERFORM 3000-APPLY-WITH-CARRY-FORWARD
           ELSE
               PERFORM 4000-APPLY-NO-CARRY-FORWARD
           END-IF
           PERFORM 5000-MINIMUM-CHARGE
           GOBACK.

       1000-INITIALISE.
           MOVE ZERO TO ANL-TOTAL-SVC-CHARGE ANL-CF-APPLIED
                        ANL-CREDIT-APPLIED ANL-NET-CHARGE
                        ANL-EXCESS-CREDIT ANL-CREDIT-FORFEITED
                        ANL-CF-EXPIRED ANL-RETURN-CODE
           MOVE 'N'  TO ANL-WAIVED-FLAG
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
               MOVE ZERO TO ANL-CF-OUT(WS-IX)
               IF ANL-CF-IN(WS-IX) < 0
                   MOVE ZERO TO ANL-CF-IN(WS-IX)
               END-IF
               MOVE ANL-CF-IN(WS-IX) TO WS-CF-LEFT(WS-IX)
           END-PERFORM
           IF ANL-EARNINGS-CREDIT < 0
               MOVE ZERO TO ANL-EARNINGS-CREDIT
           END-IF.

       2000-PRICE-SERVICES.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 9
               MOVE WS-PRC-CODE(WS-IX)       TO ANL-SVC-CODE(WS-IX)
               MOVE WS-PRC-UNIT-PRICE(WS-IX)
                 TO ANL-SVC-UNIT-PRICE(WS-IX)
      *        CHG3310 - MONEY SERVICES BUSINESS ACH SURCHARGE
               IF WS-IX = WS-SVC-ACH AND ANL-MSB-FLAG = 'Y'
                   COMPUTE ANL-SVC-UNIT-PRICE(WS-IX) ROUNDED =
                           WS-PRC-UNIT-PRICE(WS-IX) * WS-MSB-ACH-FACTOR
               END-IF
               COMPUTE ANL-SVC-CHARGE(WS-IX) ROUNDED =
                       ANL-SVC-VOLUME(WS-IX) * ANL-SVC-UNIT-PRICE(WS-IX)
               ADD ANL-SVC-CHARGE(WS-IX) TO ANL-TOTAL-SVC-CHARGE
           END-PERFORM.

      *----------------------------------------------------------------*
      * CHG0731 PUBLIC FUNDS - OLDEST CARRY-FORWARD FIRST (3,2,1),     *
      * THEN THIS MONTH'S CREDIT. UNUSED AMOUNTS AGE ONE BUCKET.       *
      *----------------------------------------------------------------*
       3000-APPLY-WITH-CARRY-FORWARD.
           MOVE ANL-TOTAL-SVC-CHARGE TO WS-REMAINING
           PERFORM VARYING WS-IX FROM 3 BY -1 UNTIL WS-IX < 1
               IF WS-REMAINING > 0 AND WS-CF-LEFT(WS-IX) > 0
                   IF WS-CF-LEFT(WS-IX) > WS-REMAINING
                       MOVE WS-REMAINING TO WS-USE
                   ELSE
                       MOVE WS-CF-LEFT(WS-IX) TO WS-USE
                   END-IF
                   SUBTRACT WS-USE FROM WS-CF-LEFT(WS-IX)
                   SUBTRACT WS-USE FROM WS-REMAINING
                   ADD WS-USE TO ANL-CF-APPLIED
               END-IF
           END-PERFORM
           PERFORM 3500-APPLY-CURRENT-CREDIT
           MOVE ANL-EXCESS-CREDIT TO ANL-CF-OUT(1)
           MOVE WS-CF-LEFT(1)     TO ANL-CF-OUT(2)
           MOVE WS-CF-LEFT(2)     TO ANL-CF-OUT(3)
           MOVE WS-CF-LEFT(3)     TO ANL-CF-EXPIRED.

       3500-APPLY-CURRENT-CREDIT.
           MOVE ANL-EARNINGS-CREDIT TO WS-ECR-LEFT
           IF WS-ECR-LEFT > WS-REMAINING
               MOVE WS-REMAINING TO ANL-CREDIT-APPLIED
           ELSE
               MOVE WS-ECR-LEFT  TO ANL-CREDIT-APPLIED
           END-IF
           SUBTRACT ANL-CREDIT-APPLIED FROM WS-REMAINING
           SUBTRACT ANL-CREDIT-APPLIED FROM WS-ECR-LEFT
           MOVE WS-REMAINING TO ANL-NET-CHARGE
           MOVE WS-ECR-LEFT  TO ANL-EXCESS-CREDIT.

      *----------------------------------------------------------------*
      * ALL OTHER SEGMENTS - EXCESS CREDIT DOES NOT CARRY FORWARD      *
      *----------------------------------------------------------------*
       4000-APPLY-NO-CARRY-FORWARD.
           MOVE ANL-TOTAL-SVC-CHARGE TO WS-REMAINING
           PERFORM 3500-APPLY-CURRENT-CREDIT
           MOVE ANL-EXCESS-CREDIT TO ANL-CREDIT-FORFEITED
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
               ADD WS-CF-LEFT(WS-IX) TO ANL-CREDIT-FORFEITED
           END-PERFORM.

       5000-MINIMUM-CHARGE.
           IF ANL-NET-CHARGE > 0 AND ANL-NET-CHARGE < WS-MIN-CHARGE
               MOVE ZERO TO ANL-NET-CHARGE
               MOVE 'Y'  TO ANL-WAIVED-FLAG
           END-IF.
