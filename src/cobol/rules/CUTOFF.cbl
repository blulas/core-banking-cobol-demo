      *================================================================*
      * PROGRAM  : CUTOFF                                              *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : DETERMINES THE PROCESS (DEBIT/RELEASE) DATE AND THE *
      *            SETTLEMENT DATE OF AN OUTBOUND PAYMENT FROM THE     *
      *            PAYMENT RAIL, THE TIME OF REQUEST AND THE RAIL      *
      *            CUTOFF TIMES.                                       *
      *              BOOK     24X7, SAME DAY, NO CUTOFF                *
      *              ACH      CUTOFF CTL-ACH-CUTOFF-TIME (16:00),      *
      *                       SETTLES NEXT BUSINESS DAY                *
      *              SDACH    CUTOFF CTL-SDACH-CUTOFF-TIME (14:30) AND *
      *                       <= $1,000,000 PER ENTRY, SETTLES SAME    *
      *                       DAY - OTHERWISE ROLLS TO STANDARD ACH    *
      *              WIRE     CUTOFF CTL-WIRE-CUTOFF-TIME (17:45),     *
      *                       SETTLES SAME DAY                         *
      *              INTL     CUTOFF 15:00, SETTLES +2 BUSINESS DAYS   *
      *            NON-BUSINESS DAY REQUESTS ROLL FORWARD (ADDB 0).    *
      * LINKAGE  : CUTPARM                                             *
      * CALLS    : DATEUTIL                                            *
      * CALLED BY: PAYINIT, WIREAPPR (ONLINE), PAYPROC (BATCH)         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - WIRE CUTOFF 17:00                   *
      * 1996-02-12 RJT  ACH ORIGINATION - CREDITS 2 DAY, DEBITS 1 DAY  *
      * 2003-06-30 BHS  WIRE CUTOFF MOVED TO 17:45 (FED EXTENDED HRS)  *
      * 2009-04-20 DPS  INTERNATIONAL WIRES VIA CORRESPONDENT          *
      * 2010-09-17 DPS  ACH CREDITS NOW 1 DAY (NACHA RULE CHANGE)      *
      * 2014-11-03 DPS  CHG0998 INTL SPOT SETTLEMENT T+2               *
      * 2016-09-23 DPS  SAME-DAY ACH - $25,000 ENTRY LIMIT             *
      * 2020-03-20 AKW  SDACH ENTRY LIMIT RAISED TO $100,000           *
      * 2022-03-18 AKW  CHG2207 SDACH ENTRY LIMIT RAISED TO $1,000,000 *
      * 2023-01-09 AKW  RAIL TABLE - SETTLE DAYS PER RAIL              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUTOFF.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * RAIL TABLE: TYPE(1) CUTOFF-SOURCE(1) DEFAULT-CUTOFF(4)         *
      *             SETTLE-DAYS(1)                                     *
      *   CUTOFF SOURCE  C = BANKCTL (CALLER), N = NONE                *
      *----------------------------------------------------------------*
       01  WS-RAIL-TABLE-VALUES.
           05  FILLER  PIC X(07) VALUE 'BN23590'.
           05  FILLER  PIC X(07) VALUE 'AC16001'.
           05  FILLER  PIC X(07) VALUE 'SC14300'.
           05  FILLER  PIC X(07) VALUE 'WC17450'.
           05  FILLER  PIC X(07) VALUE 'IN15001'.
       01  WS-RAIL-TABLE REDEFINES WS-RAIL-TABLE-VALUES.
           05  WS-RAIL-ENTRY OCCURS 5 TIMES INDEXED BY RAIL-IDX.
               10  WS-RAIL-TYPE            PIC X(01).
               10  WS-RAIL-CUTOFF-SRC      PIC X(01).
               10  WS-RAIL-DFLT-CUTOFF     PIC 9(04).
               10  WS-RAIL-SETTLE-DAYS     PIC 9(01).

       01  WS-CONSTANTS.
      *    NACHA SAME-DAY ENTRY LIMIT - CHG2207 (WAS 100,000.00)
           05  WS-SDACH-ENTRY-LIMIT        PIC S9(13)V99 COMP-3
                                           VALUE 1000000.00.
      *    CORRESPONDENT CUTOFF FOR USD INTERNATIONAL - NOT ON BANKCTL
           05  WS-INTL-CUTOFF              PIC 9(04) VALUE 1500.

       01  WS-WORK.
           05  WS-START-DATE               PIC 9(08).
           05  WS-CUTOFF-TIME              PIC 9(04).
           05  WS-SETTLE-DAYS              PIC 9(01).
           05  WS-TODAY-FLAG               PIC X(01).
               88  WS-REQUEST-IS-TODAY               VALUE 'Y'.

       COPY DATEPARM.

       LINKAGE SECTION.
       COPY CUTPARM.

       PROCEDURE DIVISION USING CUTOFF-PARMS.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           IF CUT-RETURN-CODE = 12
               GOBACK
           END-IF
           PERFORM 1100-ROLL-START-DATE
           IF CUT-RETURN-CODE = 12
               GOBACK
           END-IF

           EVALUATE TRUE
               WHEN CUT-BOOK
                   PERFORM 2100-BOOK-TRANSFER
               WHEN CUT-SDACH
                   PERFORM 2200-SAME-DAY-ACH
               WHEN CUT-ACH
                   PERFORM 2300-STANDARD-ACH
               WHEN CUT-WIRE
                   PERFORM 2400-DOMESTIC-WIRE
               WHEN CUT-INTL-WIRE
                   PERFORM 2500-INTERNATIONAL-WIRE
           END-EVALUATE

           IF CUT-PROCESS-DATE NOT = CUT-REQUEST-DATE
              OR CUT-EFFECTIVE-TYPE NOT = CUT-PAY-TYPE
               SET CUT-WAS-ROLLED TO TRUE
               MOVE 04 TO CUT-RETURN-CODE
           END-IF
           IF CUT-REASON-CODE = SPACES
               MOVE 'SAME' TO CUT-REASON-CODE
               MOVE 'PROCESSED ON REQUESTED DATE' TO CUT-REASON-TEXT
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           MOVE ZERO   TO CUT-RETURN-CODE
                          CUT-PROCESS-DATE
                          CUT-SETTLE-DATE
                          CUT-CUTOFF-APPLIED
           MOVE 'N'    TO CUT-ROLLED-FLAG
           MOVE SPACES TO CUT-REASON-CODE CUT-REASON-TEXT
           MOVE CUT-PAY-TYPE TO CUT-EFFECTIVE-TYPE
           IF NOT (CUT-BOOK OR CUT-ACH OR CUT-SDACH OR CUT-WIRE
                   OR CUT-INTL-WIRE)
               MOVE 12 TO CUT-RETURN-CODE
               MOVE 'TYPE' TO CUT-REASON-CODE
               MOVE 'INVALID PAYMENT TYPE' TO CUT-REASON-TEXT
           END-IF
           IF CUT-BUSINESS-DATE = ZERO
               MOVE 12 TO CUT-RETURN-CODE
               MOVE 'BDAT' TO CUT-REASON-CODE
               MOVE 'BUSINESS DATE NOT SUPPLIED' TO CUT-REASON-TEXT
           END-IF
      *    CALLER MAY PASS ZERO CUTOFFS - USE TABLE DEFAULTS
           IF CUT-ACH-CUTOFF = ZERO
               MOVE WS-RAIL-DFLT-CUTOFF(2) TO CUT-ACH-CUTOFF
           END-IF
           IF CUT-SDACH-CUTOFF = ZERO
               MOVE WS-RAIL-DFLT-CUTOFF(3) TO CUT-SDACH-CUTOFF
           END-IF
           IF CUT-WIRE-CUTOFF = ZERO
               MOVE WS-RAIL-DFLT-CUTOFF(4) TO CUT-WIRE-CUTOFF
           END-IF.

      *----------------------------------------------------------------*
      * REQUESTED DATE: ZERO = TODAY; BACK-DATED = TODAY; THEN ROLL TO *
      * THE NEXT BUSINESS DAY IF IT FALLS ON A WEEKEND OR FED HOLIDAY. *
      *----------------------------------------------------------------*
       1100-ROLL-START-DATE.
           IF CUT-REQUEST-DATE = ZERO
               MOVE CUT-BUSINESS-DATE TO CUT-REQUEST-DATE
           END-IF
           MOVE CUT-REQUEST-DATE TO WS-START-DATE
           IF CUT-REQUEST-DATE < CUT-BUSINESS-DATE
               MOVE CUT-BUSINESS-DATE TO WS-START-DATE
               MOVE 'BKDT' TO CUT-REASON-CODE
               MOVE 'BACK VALUE NOT PERMITTED - BUSINESS DATE'
                 TO CUT-REASON-TEXT
           END-IF

           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE WS-START-DATE TO DTU-DATE-1
           MOVE 0             TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-OK
               MOVE 12 TO CUT-RETURN-CODE
               MOVE 'DATE' TO CUT-REASON-CODE
               MOVE 'INVALID REQUESTED DATE' TO CUT-REASON-TEXT
               EXIT PARAGRAPH
           END-IF
           IF DTU-RESULT-DATE NOT = WS-START-DATE
               MOVE DTU-RESULT-DATE TO WS-START-DATE
               MOVE 'NBDR' TO CUT-REASON-CODE
               MOVE 'NON-BUSINESS DAY - ROLLED FORWARD'
                 TO CUT-REASON-TEXT
           END-IF
      *    THE TIME OF DAY ONLY MATTERS FOR TODAY'S REQUESTS
           IF WS-START-DATE = CUT-BUSINESS-DATE
               MOVE 'Y' TO WS-TODAY-FLAG
           ELSE
               MOVE 'N' TO WS-TODAY-FLAG
           END-IF.

      *----------------------------------------------------------------*
       2100-BOOK-TRANSFER.
           MOVE WS-START-DATE TO CUT-PROCESS-DATE
           SET RAIL-IDX TO 1
           MOVE WS-RAIL-SETTLE-DAYS(RAIL-IDX) TO WS-SETTLE-DAYS
           MOVE CUT-PROCESS-DATE TO CUT-SETTLE-DATE.

      *----------------------------------------------------------------*
      * SAME-DAY ACH: ENTRY LIMIT FIRST, THEN THE SDACH WINDOW. EITHER *
      * FAILURE DOWNGRADES THE ENTRY TO A STANDARD ACH ENTRY.          *
      *----------------------------------------------------------------*
       2200-SAME-DAY-ACH.
           EVALUATE TRUE
               WHEN CUT-AMOUNT > WS-SDACH-ENTRY-LIMIT
                   MOVE 'A' TO CUT-EFFECTIVE-TYPE
                   PERFORM 2300-STANDARD-ACH
                   MOVE 'SDLM' TO CUT-REASON-CODE
                   MOVE 'OVER SDACH ENTRY LIMIT - STANDARD ACH'
                     TO CUT-REASON-TEXT
               WHEN WS-REQUEST-IS-TODAY
                AND CUT-REQUEST-TIME > CUT-SDACH-CUTOFF
                   MOVE 'A' TO CUT-EFFECTIVE-TYPE
                   PERFORM 2300-STANDARD-ACH
                   IF CUT-REASON-CODE NOT = 'PCUT'
                       MOVE 'SDCT' TO CUT-REASON-CODE
                       MOVE 'AFTER SDACH CUTOFF - STANDARD ACH'
                         TO CUT-REASON-TEXT
                   END-IF
               WHEN OTHER
                   MOVE CUT-SDACH-CUTOFF TO CUT-CUTOFF-APPLIED
                   MOVE WS-START-DATE TO CUT-PROCESS-DATE
                   SET RAIL-IDX TO 3
                   MOVE WS-RAIL-SETTLE-DAYS(RAIL-IDX) TO WS-SETTLE-DAYS
                   PERFORM 8100-SETTLE-DATE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STANDARD ACH - CREDITS AND DEBITS BOTH SETTLE NEXT BUSINESS    *
      * DAY SINCE 2010-09 (SEE CHANGE LOG).                            *
      *----------------------------------------------------------------*
       2300-STANDARD-ACH.
           MOVE CUT-ACH-CUTOFF TO CUT-CUTOFF-APPLIED
           MOVE WS-START-DATE  TO CUT-PROCESS-DATE
           IF WS-REQUEST-IS-TODAY
              AND CUT-REQUEST-TIME > CUT-ACH-CUTOFF
               PERFORM 8000-NEXT-BUSINESS-DAY
               MOVE 'PCUT' TO CUT-REASON-CODE
               MOVE 'AFTER ACH CUTOFF - NEXT BUSINESS DAY'
                 TO CUT-REASON-TEXT
           END-IF
      *    1996-02 RJT CREDITS SETTLED 2 DAYS, DEBITS 1 DAY
      *    IF CUT-DR-CR = 'C'
      *        MOVE 2 TO WS-SETTLE-DAYS
      *    ELSE
      *        MOVE 1 TO WS-SETTLE-DAYS
      *    END-IF
      *    2010-09 DPS REMOVED - BOTH 1 DAY - SEE CHG0611
           SET RAIL-IDX TO 2
           MOVE WS-RAIL-SETTLE-DAYS(RAIL-IDX) TO WS-SETTLE-DAYS
           PERFORM 8100-SETTLE-DATE.

      *----------------------------------------------------------------*
       2400-DOMESTIC-WIRE.
           MOVE CUT-WIRE-CUTOFF TO CUT-CUTOFF-APPLIED
           MOVE WS-START-DATE   TO CUT-PROCESS-DATE
           IF WS-REQUEST-IS-TODAY
              AND CUT-REQUEST-TIME > CUT-WIRE-CUTOFF
               PERFORM 8000-NEXT-BUSINESS-DAY
               MOVE 'PCUT' TO CUT-REASON-CODE
               MOVE 'AFTER FEDWIRE CUTOFF - NEXT BUSINESS DAY'
                 TO CUT-REASON-TEXT
           END-IF
           SET RAIL-IDX TO 4
           MOVE WS-RAIL-SETTLE-DAYS(RAIL-IDX) TO WS-SETTLE-DAYS
           PERFORM 8100-SETTLE-DATE.

      *----------------------------------------------------------------*
      * INTERNATIONAL (USD VIA CORRESPONDENT). CHG0998 - CORRESPONDENT *
      * SETTLES SPOT, T+2.                                             *
      *----------------------------------------------------------------*
       2500-INTERNATIONAL-WIRE.
           MOVE WS-INTL-CUTOFF TO CUT-CUTOFF-APPLIED
           MOVE WS-START-DATE  TO CUT-PROCESS-DATE
           IF WS-REQUEST-IS-TODAY
              AND CUT-REQUEST-TIME > WS-INTL-CUTOFF
               PERFORM 8000-NEXT-BUSINESS-DAY
               MOVE 'PCUT' TO CUT-REASON-CODE
               MOVE 'AFTER INTL WIRE CUTOFF - NEXT BUSINESS DAY'
                 TO CUT-REASON-TEXT
           END-IF
      *    SET RAIL-IDX TO 5
      *    MOVE WS-RAIL-SETTLE-DAYS(RAIL-IDX) TO WS-SETTLE-DAYS
           MOVE 2 TO WS-SETTLE-DAYS
           PERFORM 8100-SETTLE-DATE.

      *----------------------------------------------------------------*
       8000-NEXT-BUSINESS-DAY.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CUT-PROCESS-DATE TO DTU-DATE-1
           MOVE 1                TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO CUT-PROCESS-DATE.

       8100-SETTLE-DATE.
           IF WS-SETTLE-DAYS = 0
               MOVE CUT-PROCESS-DATE TO CUT-SETTLE-DATE
           ELSE
               INITIALIZE DATE-PARMS
               SET DTU-ADD-BUS-DAYS TO TRUE
               MOVE CUT-PROCESS-DATE TO DTU-DATE-1
               MOVE WS-SETTLE-DAYS   TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO CUT-SETTLE-DATE
           END-IF.
