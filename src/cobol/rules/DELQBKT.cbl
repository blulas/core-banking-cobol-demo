      *================================================================*
      * PROGRAM  : DELQBKT                                             *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : LOAN DELINQUENCY RULES.                             *
      *            AGE  - BUCKET FROM DAYS PAST DUE, LOAN STATUS,      *
      *                   CREDIT BUREAU DELINQUENCY REPORTING (30+),   *
      *                   COMMERCIAL NON-ACCRUAL (90+), CHARGE-OFF     *
      *                   (FFIEC: CLOSED-END 120, OPEN-END 180),       *
      *                   COLLECTIONS ALERT (60+), 30/60/90 CROSSINGS. *
      *            LFEE - LATE FEE FOR AN INSTALLMENT MORE THAN 15     *
      *                   DAYS LATE. CONSUMER: GREATER OF $15 OR 5%    *
      *                   OF THE INSTALLMENT, MAX $40, THEN STATE CAP. *
      *                   COMMERCIAL: 5% OF THE INSTALLMENT, MIN $100. *
      * LINKAGE  : DLQPARM                                             *
      * CALLED BY: LNDELQ, LNINQ (CB40)                                *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - COMMERCIAL NON-ACCRUAL AT 90 DAYS   *
      * 1999-06-30 MKL  FFIEC UNIFORM RETAIL CREDIT CLASSIFICATION -   *
      *                 CHARGE-OFF CLOSED-END 120 / OPEN-END 180       *
      * 2009-08-03 DPS  30/60/90 CROSSING FLAGS FOR COUNTERS           *
      * 2010-02-22 DPS  GRACE PERIOD 10 -> 15 DAYS - CHG0977           *
      * 2016-04-25 AKW  LPER LATE FEE $15 / 5% / $40 MAX, STATE CAPS   *
      * 2020-04-06 AKW  CARES ACT - FORBEARANCE FREEZES DELINQUENCY    *
      * 2021-03-15 JPL  NC LATE CHARGE 4% LIMIT ADDED - LEGAL-2021-031 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELQBKT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * BUCKET TABLE - LOW DPD, HIGH DPD, CODE, DESCRIPTION            *
      *----------------------------------------------------------------*
       01  WS-BUCKET-VALUES.
           05  FILLER PIC X(19) VALUE '000000000CURRENT   '.
           05  FILLER PIC X(19) VALUE '000100291DPD 1-29  '.
           05  FILLER PIC X(19) VALUE '003000592DPD 30-59 '.
           05  FILLER PIC X(19) VALUE '006000893DPD 60-89 '.
           05  FILLER PIC X(19) VALUE '009001194DPD 90-119'.
           05  FILLER PIC X(19) VALUE '012099995DPD 120+  '.
       01  WS-BUCKET-TABLE REDEFINES WS-BUCKET-VALUES.
           05  WS-BKT-ENTRY OCCURS 6 TIMES INDEXED BY BKT-IDX.
               10  WS-BKT-LOW              PIC 9(04).
               10  WS-BKT-HIGH             PIC 9(04).
               10  WS-BKT-CODE             PIC X(01).
               10  WS-BKT-DESC             PIC X(10).
      *----------------------------------------------------------------*
      * DELINQUENCY THRESHOLDS (DAYS PAST DUE)                         *
      *----------------------------------------------------------------*
       01  WS-THRESHOLDS.
           05  WS-DELINQUENT-DPD           PIC 9(04) VALUE 30.
           05  WS-BUREAU-DPD               PIC 9(04) VALUE 30.
           05  WS-ALERT-DPD                PIC 9(04) VALUE 60.
           05  WS-NONACCRUAL-DPD           PIC 9(04) VALUE 90.
           05  WS-CHGOFF-CLOSED-END-DPD    PIC 9(04) VALUE 120.
           05  WS-CHGOFF-OPEN-END-DPD      PIC 9(04) VALUE 180.
      *----------------------------------------------------------------*
      * LATE FEE POLICY                                                *
      *----------------------------------------------------------------*
       01  WS-LATE-FEE-POLICY.
           05  WS-GRACE-DAYS               PIC 9(02)    VALUE 15.
           05  WS-CONS-MIN-FEE             PIC 9(03)V99 VALUE 15.00.
           05  WS-CONS-FEE-PCT             PIC 9(02)V99 VALUE 5.00.
           05  WS-CONS-MAX-FEE             PIC 9(03)V99 VALUE 40.00.
           05  WS-COMM-FEE-PCT             PIC 9(02)V99 VALUE 5.00.
           05  WS-COMM-MIN-FEE             PIC 9(05)V99 VALUE 100.00.
      *----------------------------------------------------------------*
      * STATE LATE CHARGE LIMITS (CONSUMER) - STATE, MAX $, MAX PCT    *
      * PCT 00.00 = NO PERCENTAGE LIMIT                                *
      *----------------------------------------------------------------*
       01  WS-STATE-CAP-VALUES.
           05  FILLER PIC X(11) VALUE 'CA010000000'.
           05  FILLER PIC X(11) VALUE 'MA010000000'.
           05  FILLER PIC X(11) VALUE 'NC015000400'.
           05  FILLER PIC X(11) VALUE 'NY025000000'.
           05  FILLER PIC X(11) VALUE 'CO025000000'.
       01  WS-STATE-CAP-TABLE REDEFINES WS-STATE-CAP-VALUES.
           05  WS-SCAP-ENTRY OCCURS 5 TIMES INDEXED BY SCAP-IDX.
               10  WS-SCAP-STATE           PIC X(02).
               10  WS-SCAP-MAX-AMT         PIC 9(03)V99.
               10  WS-SCAP-MAX-PCT         PIC 9(02)V99.
       01  WS-FEE-WORK.
           05  WS-FEE                      PIC S9(07)V99 COMP-3.
           05  WS-PCT-FEE                  PIC S9(07)V99 COMP-3.
           05  WS-STATE-PCT-FEE            PIC S9(07)V99 COMP-3.
       01  WS-OPEN-END-FLAG                PIC X(01).
           88  WS-OPEN-END                           VALUE 'Y'.
       01  WS-COMMERCIAL-FLAG              PIC X(01).
           88  WS-COMMERCIAL                         VALUE 'Y'.

       LINKAGE SECTION.
       COPY DLQPARM.

       PROCEDURE DIVISION USING DELQ-PARMS.
       0000-MAIN.
           MOVE 00 TO DLQ-RETURN-CODE
           MOVE SPACES TO DLQ-BUCKET DLQ-BUCKET-DESC
           MOVE 'N' TO DLQ-BUREAU-FLAG DLQ-NONACCRUAL-FLAG
                       DLQ-CHARGEOFF-FLAG DLQ-ALERT-FLAG
                       DLQ-CROSSED-30 DLQ-CROSSED-60 DLQ-CROSSED-90
                       DLQ-ASSESS-FEE-FLAG
           MOVE SPACE TO DLQ-ALERT-SEVERITY
           MOVE ZERO TO DLQ-LATE-FEE-AMT
           MOVE DLQ-CURRENT-STATUS TO DLQ-NEW-STATUS

           IF DLQ-CATEGORY = 'LC'
               SET WS-OPEN-END TO TRUE
           ELSE
               MOVE 'N' TO WS-OPEN-END-FLAG
           END-IF
           EVALUATE DLQ-PRODUCT-CODE
               WHEN 'LPER'
                   MOVE 'N' TO WS-COMMERCIAL-FLAG
               WHEN 'LNTM'
               WHEN 'LLOC'
               WHEN 'LCRE'
                   SET WS-COMMERCIAL TO TRUE
               WHEN OTHER
                   MOVE 12 TO DLQ-RETURN-CODE
                   GOBACK
           END-EVALUATE

           EVALUATE TRUE
               WHEN DLQ-AGE
                   PERFORM 1000-AGE-LOAN
               WHEN DLQ-LATE-FEE
                   PERFORM 2000-LATE-FEE
               WHEN OTHER
                   MOVE 12 TO DLQ-RETURN-CODE
           END-EVALUATE
           GOBACK.

      *================================================================*
      * AGE LOAN                                                       *
      *================================================================*
       1000-AGE-LOAN.
           PERFORM 1100-SET-BUCKET
      *    PAID OFF AND CHARGED OFF LOANS DO NOT CHANGE STATUS
           IF DLQ-CURRENT-STATUS = 'P' OR DLQ-CURRENT-STATUS = 'C'
               EXIT PARAGRAPH
           END-IF
      *    CARES ACT FORBEARANCE - REPORT CURRENT, NO ESCALATION
           IF DLQ-CURRENT-STATUS = 'F'
               EXIT PARAGRAPH
           END-IF
           PERFORM 1200-CROSSINGS
           PERFORM 1300-STATUS-RULES.

       1100-SET-BUCKET.
           SET BKT-IDX TO 1
           SEARCH WS-BKT-ENTRY
               AT END
                   MOVE '5' TO DLQ-BUCKET
                   MOVE 'DPD 120+' TO DLQ-BUCKET-DESC
               WHEN DLQ-DAYS-PAST-DUE NOT < WS-BKT-LOW(BKT-IDX)
                AND DLQ-DAYS-PAST-DUE NOT > WS-BKT-HIGH(BKT-IDX)
                   MOVE WS-BKT-CODE(BKT-IDX) TO DLQ-BUCKET
                   MOVE WS-BKT-DESC(BKT-IDX) TO DLQ-BUCKET-DESC
           END-SEARCH.

       1200-CROSSINGS.
           IF DLQ-PRIOR-DPD < 30 AND DLQ-DAYS-PAST-DUE NOT < 30
               MOVE 'Y' TO DLQ-CROSSED-30
           END-IF
           IF DLQ-PRIOR-DPD < 60 AND DLQ-DAYS-PAST-DUE NOT < 60
               MOVE 'Y' TO DLQ-CROSSED-60
           END-IF
           IF DLQ-PRIOR-DPD < 90 AND DLQ-DAYS-PAST-DUE NOT < 90
               MOVE 'Y' TO DLQ-CROSSED-90
           END-IF.

      *----------------------------------------------------------------*
      * STATUS DECISION TABLE                                          *
      *----------------------------------------------------------------*
       1300-STATUS-RULES.
           IF DLQ-DAYS-PAST-DUE NOT < WS-BUREAU-DPD
               MOVE 'Y' TO DLQ-BUREAU-FLAG
           END-IF

           EVALUATE TRUE
      *        CONSUMER CLOSED-END - CHARGE OFF AT 120 DPD
               WHEN NOT WS-COMMERCIAL AND NOT WS-OPEN-END
                AND DLQ-DAYS-PAST-DUE NOT < WS-CHGOFF-CLOSED-END-DPD
                   MOVE 'Y' TO DLQ-CHARGEOFF-FLAG
                   MOVE 'C' TO DLQ-NEW-STATUS
      *        CONSUMER OPEN-END - CHARGE OFF AT 180 DPD
               WHEN NOT WS-COMMERCIAL AND WS-OPEN-END
                AND DLQ-DAYS-PAST-DUE NOT < WS-CHGOFF-OPEN-END-DPD
                   MOVE 'Y' TO DLQ-CHARGEOFF-FLAG
                   MOVE 'C' TO DLQ-NEW-STATUS
      *        COMMERCIAL - NON-ACCRUAL AT 90 DPD (NO AUTO CHARGE-OFF)
               WHEN WS-COMMERCIAL
                AND DLQ-DAYS-PAST-DUE NOT < WS-NONACCRUAL-DPD
                   MOVE 'N' TO DLQ-NEW-STATUS
                   IF DLQ-CURRENT-STATUS NOT = 'N'
                       MOVE 'Y' TO DLQ-NONACCRUAL-FLAG
                   END-IF
      *        NON-ACCRUAL STAYS NON-ACCRUAL UNTIL CREDIT RESTORES IT
               WHEN DLQ-CURRENT-STATUS = 'N'
                   MOVE 'N' TO DLQ-NEW-STATUS
               WHEN DLQ-DAYS-PAST-DUE NOT < WS-DELINQUENT-DPD
                   MOVE 'D' TO DLQ-NEW-STATUS
               WHEN OTHER
                   MOVE 'A' TO DLQ-NEW-STATUS
           END-EVALUATE

      *    COLLECTIONS ALERT
           EVALUATE TRUE
               WHEN DLQ-CHARGEOFF-FLAG = 'Y'
                   MOVE 'Y' TO DLQ-ALERT-FLAG
                   MOVE 'C' TO DLQ-ALERT-SEVERITY
               WHEN DLQ-NONACCRUAL-FLAG = 'Y'
                   MOVE 'Y' TO DLQ-ALERT-FLAG
                   MOVE 'H' TO DLQ-ALERT-SEVERITY
               WHEN DLQ-CROSSED-90 = 'Y'
                   MOVE 'Y' TO DLQ-ALERT-FLAG
                   MOVE 'H' TO DLQ-ALERT-SEVERITY
               WHEN DLQ-CROSSED-60 = 'Y'
                   MOVE 'Y' TO DLQ-ALERT-FLAG
                   MOVE 'M' TO DLQ-ALERT-SEVERITY
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *================================================================*
      * LATE FEE FOR ONE INSTALLMENT                                   *
      *================================================================*
       2000-LATE-FEE.
           IF DLQ-FEE-ASSESSED-FLAG = 'Y'
               EXIT PARAGRAPH
           END-IF
      *    NO LATE FEES IN FORBEARANCE, NON-ACCRUAL OR AFTER CHARGE-OFF
           IF DLQ-CURRENT-STATUS = 'F' OR DLQ-CURRENT-STATUS = 'N'
              OR DLQ-CURRENT-STATUS = 'C' OR DLQ-CURRENT-STATUS = 'P'
               EXIT PARAGRAPH
           END-IF
           IF DLQ-INSTALL-DAYS-LATE NOT > WS-GRACE-DAYS
               EXIT PARAGRAPH
           END-IF
           IF DLQ-INSTALLMENT-AMT NOT > ZERO
               EXIT PARAGRAPH
           END-IF

           IF WS-COMMERCIAL
               COMPUTE WS-FEE ROUNDED =
                   DLQ-INSTALLMENT-AMT * WS-COMM-FEE-PCT / 100
               IF WS-FEE < WS-COMM-MIN-FEE
                   MOVE WS-COMM-MIN-FEE TO WS-FEE
               END-IF
           ELSE
               COMPUTE WS-PCT-FEE ROUNDED =
                   DLQ-INSTALLMENT-AMT * WS-CONS-FEE-PCT / 100
               IF WS-PCT-FEE > WS-CONS-MIN-FEE
                   MOVE WS-PCT-FEE TO WS-FEE
               ELSE
                   MOVE WS-CONS-MIN-FEE TO WS-FEE
               END-IF
               IF WS-FEE > WS-CONS-MAX-FEE
                   MOVE WS-CONS-MAX-FEE TO WS-FEE
               END-IF
               PERFORM 2100-STATE-CAP
           END-IF
           MOVE WS-FEE TO DLQ-LATE-FEE-AMT
           MOVE 'Y' TO DLQ-ASSESS-FEE-FLAG.

       2100-STATE-CAP.
           SET SCAP-IDX TO 1
           SEARCH WS-SCAP-ENTRY
               AT END
                   CONTINUE
               WHEN WS-SCAP-STATE(SCAP-IDX) = DLQ-STATE
                   IF WS-FEE > WS-SCAP-MAX-AMT(SCAP-IDX)
                       MOVE WS-SCAP-MAX-AMT(SCAP-IDX) TO WS-FEE
                   END-IF
                   IF WS-SCAP-MAX-PCT(SCAP-IDX) > ZERO
                       COMPUTE WS-STATE-PCT-FEE ROUNDED =
                           DLQ-INSTALLMENT-AMT
                           * WS-SCAP-MAX-PCT(SCAP-IDX) / 100
                       IF WS-FEE > WS-STATE-PCT-FEE
                           MOVE WS-STATE-PCT-FEE TO WS-FEE
                       END-IF
                   END-IF
           END-SEARCH.
