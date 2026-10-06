      *================================================================*
      * PROGRAM  : CDPENLTY                                            *
      * TYPE     : CALLED RULE MODULE                                  *
      * PURPOSE  : CERTIFICATE OF DEPOSIT EARLY WITHDRAWAL PENALTY.    *
      *            PENALTY = WITHDRAWAL * RATE * DAYS / 365 WHERE DAYS *
      *            IS THE PRODUCT PENALTY (CD06 90, CD12 180, CD36     *
      *            365, CDNP 0). NO PENALTY AT/AFTER MATURITY, WITHIN  *
      *            THE POST-RENEWAL GRACE PERIOD OR ON DEATH OF THE    *
      *            OWNER. A WITHDRAWAL WITHIN 6 DAYS OF FUNDING ALWAYS *
      *            COSTS AT LEAST 7 DAYS INTEREST (REG D 204.2(C)) -   *
      *            INCLUDING THE NO-PENALTY CD.                        *
      *            THE PENALTY COMES FIRST FROM EARNED INTEREST. ONLY  *
      *            CD36 AND THE REG D 7-DAY PENALTY MAY INVADE         *
      *            PRINCIPAL; FOR OTHER CDS THE PENALTY IS LIMITED TO  *
      *            THE INTEREST EARNED (DEPOSIT AGREEMENT SEC 7.3).    *
      * LINKAGE  : CDPPARM                                             *
      * CALLS    : DATEUTIL                                            *
      * CALLED BY: CDQUOTE (CB21), CDCLOSE                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1997-06-02 RJT  DEATH OF OWNER WAIVER                          *
      * 2004-11-15 BHS  GRACE PERIOD 10 CALENDAR DAYS AFTER RENEWAL    *
      * 2009-04-20 DPS  IRA WAIVERS REMOVED - CHG0987                  *
      * 2013-08-05 DPS  CD36 PENALTY MAY INVADE PRINCIPAL - CHG0981    *
      * 2017-02-08 AKW  NO-PENALTY CD - REG D 7 DAY MINIMUM            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDPENLTY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DATEPARM.

       01  WS-CONSTANTS.
           05  WS-REGD-MIN-DAYS            PIC 9(03)  VALUE 7.
           05  WS-REGD-WINDOW-DAYS         PIC 9(03)  VALUE 6.

       01  WS-WORK-FIELDS.
           05  WS-GRACE-END                PIC 9(08).
           05  WS-DAYS-HELD                PIC S9(07).
           05  WS-WD-AMT                   PIC S9(13)V99 COMP-3.
           05  WS-INT-AVAIL                PIC S9(13)V99 COMP-3.
           05  WS-PENALTY                  PIC S9(13)V99 COMP-3.
           05  WS-FULL-WD-FLAG             PIC X(01).
               88  WS-FULL-WITHDRAWAL                VALUE 'Y'.
           05  WS-INVADE-FLAG              PIC X(01).
               88  WS-MAY-INVADE-PRINCIPAL           VALUE 'Y'.

       LINKAGE SECTION.
       COPY CDPPARM.

       PROCEDURE DIVISION USING CDPENLTY-PARMS.
       0000-MAIN.
           MOVE ZERO   TO CDP-DAYS-APPLIED CDP-PENALTY-AMT
                          CDP-PENALTY-FROM-INT CDP-PENALTY-FROM-PRIN
                          CDP-NET-PROCEEDS CDP-RETURN-CODE
           MOVE SPACES TO CDP-WAIVER-CODE
           MOVE 'N'    TO WS-FULL-WD-FLAG WS-INVADE-FLAG

           PERFORM 1000-VALIDATE
           IF CDP-RETURN-CODE NOT = 00
               GOBACK
           END-IF

           PERFORM 2000-CHECK-WAIVERS
           IF CDP-WAIVER-CODE = SPACES
               PERFORM 3000-PENALTY-DAYS
           END-IF
           IF CDP-WAIVER-CODE = SPACES
               PERFORM 4000-COMPUTE-PENALTY
           END-IF
           PERFORM 5000-NET-PROCEEDS
           IF CDP-PENALTY-AMT = ZERO
               MOVE 04 TO CDP-RETURN-CODE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
       1000-VALIDATE.
           IF CDP-PRINCIPAL NOT > ZERO
              OR CDP-WITHDRAWAL-AMT < ZERO
              OR CDP-FUNDING-DATE = ZERO
              OR CDP-WITHDRAWAL-DATE < CDP-FUNDING-DATE
               MOVE 12 TO CDP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF CDP-WITHDRAWAL-AMT > CDP-PRINCIPAL
               MOVE 08 TO CDP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF CDP-WITHDRAWAL-AMT = ZERO
              OR CDP-WITHDRAWAL-AMT = CDP-PRINCIPAL
               MOVE CDP-PRINCIPAL TO WS-WD-AMT
               SET WS-FULL-WITHDRAWAL TO TRUE
           ELSE
               MOVE CDP-WITHDRAWAL-AMT TO WS-WD-AMT
           END-IF
      *    EARNED INTEREST AVAILABLE TO ABSORB THE PENALTY, TO CENTS
           COMPUTE WS-INT-AVAIL ROUNDED = CDP-ACCRUED-INT
           IF WS-INT-AVAIL < ZERO
               MOVE ZERO TO WS-INT-AVAIL
           END-IF.

      *----------------------------------------------------------------*
      * NO PENALTY:                                                    *
      *   MT - ON OR AFTER THE MATURITY DATE                           *
      *   GP - ON OR BEFORE THE GRACE END DATE (10 CALENDAR DAYS AFTER *
      *        THE LAST RENEWAL, SET BY CDMATUR)                       *
      *   DC - OWNER DECEASED                                          *
      *----------------------------------------------------------------*
       2000-CHECK-WAIVERS.
           MOVE CDP-GRACE-END-DATE TO WS-GRACE-END
           EVALUATE TRUE
               WHEN CDP-MATURITY-DATE > ZERO
                AND CDP-WITHDRAWAL-DATE >= CDP-MATURITY-DATE
                   MOVE 'MT' TO CDP-WAIVER-CODE
               WHEN WS-GRACE-END > ZERO
                AND CDP-WITHDRAWAL-DATE <= WS-GRACE-END
                   MOVE 'GP' TO CDP-WAIVER-CODE
               WHEN CDP-OWNER-DECEASED = 'Y'
                   MOVE 'DC' TO CDP-WAIVER-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      *    2009-04 DPS IRA CDS MOVED TO TRUST PLATFORM - CHG0987
      *    IRA DISTRIBUTION AT AGE 59 1/2 OR REQUIRED MINIMUM
      *    DISTRIBUTION AT AGE 70 1/2 WAS PENALTY FREE
      *        WHEN CDP-IRA-FLAG = 'Y' AND CDP-OWNER-AGE-MOS >= 846
      *            MOVE 'IR' TO CDP-WAIVER-CODE

      *----------------------------------------------------------------*
      * PENALTY DAYS. WITHIN 6 DAYS OF FUNDING THE REG D MINIMUM OF 7  *
      * DAYS SIMPLE INTEREST APPLIES TO EVERY CD, AND MAY BE TAKEN     *
      * FROM PRINCIPAL. AFTER THAT THE PRODUCT PENALTY DAYS APPLY;     *
      * ZERO DAYS (CDNP) MEANS NO PENALTY.                             *
      *----------------------------------------------------------------*
       3000-PENALTY-DAYS.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE CDP-FUNDING-DATE    TO DTU-DATE-1
           MOVE CDP-WITHDRAWAL-DATE TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DTU-OK
               MOVE 12 TO CDP-RETURN-CODE
               MOVE 'XX' TO CDP-WAIVER-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE DTU-RESULT-NUMBER TO WS-DAYS-HELD

           IF WS-DAYS-HELD <= WS-REGD-WINDOW-DAYS
               IF CDP-PENALTY-DAYS > WS-REGD-MIN-DAYS
                   MOVE CDP-PENALTY-DAYS TO CDP-DAYS-APPLIED
               ELSE
                   MOVE WS-REGD-MIN-DAYS TO CDP-DAYS-APPLIED
               END-IF
               SET WS-MAY-INVADE-PRINCIPAL TO TRUE
           ELSE
               IF CDP-PENALTY-DAYS = ZERO
                   MOVE 'NP' TO CDP-WAIVER-CODE
               ELSE
                   MOVE CDP-PENALTY-DAYS TO CDP-DAYS-APPLIED
               END-IF
           END-IF
           IF CDP-PRODUCT-CODE = 'CD36'
               SET WS-MAY-INVADE-PRINCIPAL TO TRUE
           END-IF.

      *----------------------------------------------------------------*
       4000-COMPUTE-PENALTY.
           COMPUTE WS-PENALTY ROUNDED =
                   (WS-WD-AMT * CDP-INT-RATE * CDP-DAYS-APPLIED)
                 / (100 * 365)
           IF WS-PENALTY <= WS-INT-AVAIL
               MOVE WS-PENALTY TO CDP-PENALTY-FROM-INT
           ELSE
               MOVE WS-INT-AVAIL TO CDP-PENALTY-FROM-INT
               IF WS-MAY-INVADE-PRINCIPAL
                   COMPUTE CDP-PENALTY-FROM-PRIN =
                           WS-PENALTY - WS-INT-AVAIL
               END-IF
           END-IF
           COMPUTE CDP-PENALTY-AMT = CDP-PENALTY-FROM-INT
                                   + CDP-PENALTY-FROM-PRIN.

      *----------------------------------------------------------------*
      * FULL WITHDRAWAL PAYS PRINCIPAL + EARNED INTEREST - PENALTY.    *
      * PARTIAL: THE INTEREST PART OF THE PENALTY IS CHARGED AGAINST   *
      * THE ACCRUAL, SO PROCEEDS ARE REDUCED ONLY BY THE PRINCIPAL     *
      * PART.                                                          *
      *----------------------------------------------------------------*
       5000-NET-PROCEEDS.
           IF WS-FULL-WITHDRAWAL
               COMPUTE CDP-NET-PROCEEDS = WS-WD-AMT + WS-INT-AVAIL
                                        - CDP-PENALTY-AMT
           ELSE
               COMPUTE CDP-NET-PROCEEDS = WS-WD-AMT
                                        - CDP-PENALTY-FROM-PRIN
           END-IF.
