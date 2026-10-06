      *================================================================*
      * PROGRAM  : DATEUTIL                                            *
      * TYPE     : CALLED SUBROUTINE                                   *
      * PURPOSE  : COMMON DATE ARITHMETIC AND BUSINESS-DAY CALENDAR.   *
      *            SEE COPYBOOK DATEPARM FOR THE FUNCTION CODES.       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-14 RJT  ORIGINAL (ADDD, DIFF, DOWK)                    *
      * 1994-12-01 RJT  BUSINESS DAY FUNCTIONS USING CALTBL            *
      * 1998-11-02 MKL  Y2K - REWRITTEN USING INTEGER-OF-DATE          *
      * 2003-05-09 BHS  30/360 DAY COUNT FOR LOAN ACCRUAL              *
      * 2012-08-14 DPS  ADDM / LBDM FOR STATEMENT AND CD MATURITY      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATEUTIL.
       AUTHOR. R J TRAVERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CALTBL.

       01  WS-WORK-FIELDS.
           05  WS-INT-1                    PIC S9(09) COMP.
           05  WS-INT-2                    PIC S9(09) COMP.
           05  WS-DAYS-TO-GO               PIC S9(05) COMP.
           05  WS-DOW                      PIC 9(01).
           05  WS-IS-HOLIDAY               PIC X(01).
           05  WS-TEST-DATE                PIC 9(08).

       01  WS-DATE-PARTS.
           05  WS-DATE-X                   PIC 9(08).
           05  WS-DATE-R REDEFINES WS-DATE-X.
               10  WS-CCYY                 PIC 9(04).
               10  WS-MM                   PIC 9(02).
               10  WS-DD                   PIC 9(02).

       01  WS-DATE-PARTS-2.
           05  WS-DATE2-X                  PIC 9(08).
           05  WS-DATE2-R REDEFINES WS-DATE2-X.
               10  WS-CCYY2                PIC 9(04).
               10  WS-MM2                  PIC 9(02).
               10  WS-DD2                  PIC 9(02).

       01  WS-MONTH-ARITH.
           05  WS-TOTAL-MONTHS             PIC S9(07) COMP.
           05  WS-NEW-CCYY                 PIC 9(04).
           05  WS-NEW-MM                   PIC 9(02).
           05  WS-NEW-DD                   PIC 9(02).
           05  WS-MAX-DD                   PIC 9(02).

       01  WS-DAYS-IN-MONTH-TABLE.
           05  FILLER PIC X(24) VALUE '312831303130313130313031'.
       01  WS-DIM-R REDEFINES WS-DAYS-IN-MONTH-TABLE.
           05  WS-DIM                      PIC 9(02) OCCURS 12.

       01  WS-LEAP-FLAG                    PIC X(01).
           88  WS-LEAP-YEAR                          VALUE 'Y'.

       LINKAGE SECTION.
       COPY DATEPARM.

       PROCEDURE DIVISION USING DATE-PARMS.
       0000-MAIN.
           MOVE 00     TO DTU-RETURN-CODE
           MOVE ZERO   TO DTU-RESULT-DATE
                          DTU-RESULT-NUMBER
           MOVE SPACE  TO DTU-RESULT-FLAG

           IF NOT DTU-VALIDATE
               MOVE DTU-DATE-1 TO WS-TEST-DATE
               PERFORM 9000-CHECK-DATE
               IF DTU-INVALID-DATE
                   GOBACK
               END-IF
           END-IF

           EVALUATE TRUE
               WHEN DTU-ADD-DAYS
                   PERFORM 1000-ADD-DAYS
               WHEN DTU-ADD-BUS-DAYS
                   PERFORM 1100-ADD-BUSINESS-DAYS
               WHEN DTU-DAYS-BETWEEN
                   PERFORM 1200-DAYS-BETWEEN
               WHEN DTU-DAYS-30-360
                   PERFORM 1300-DAYS-30-360
               WHEN DTU-DAY-OF-WEEK
                   MOVE DTU-DATE-1 TO WS-TEST-DATE
                   PERFORM 8000-GET-DOW
                   MOVE WS-DOW TO DTU-RESULT-NUMBER
               WHEN DTU-IS-BUS-DAY
                   MOVE DTU-DATE-1 TO WS-TEST-DATE
                   PERFORM 8100-TEST-BUSINESS-DAY
                   IF WS-IS-HOLIDAY = 'Y'
                       MOVE 'N' TO DTU-RESULT-FLAG
                   ELSE
                       MOVE 'Y' TO DTU-RESULT-FLAG
                   END-IF
               WHEN DTU-END-OF-MONTH
                   PERFORM 1400-END-OF-MONTH
               WHEN DTU-LAST-BUS-DAY-MONTH
                   PERFORM 1500-LAST-BUS-DAY-OF-MONTH
               WHEN DTU-ADD-MONTHS
                   PERFORM 1600-ADD-MONTHS
               WHEN DTU-DAYS-IN-YEAR
                   MOVE DTU-DATE-1 TO WS-DATE-X
                   PERFORM 8200-SET-LEAP-FLAG
                   IF WS-LEAP-YEAR
                       MOVE 366 TO DTU-RESULT-NUMBER
                   ELSE
                       MOVE 365 TO DTU-RESULT-NUMBER
                   END-IF
               WHEN DTU-VALIDATE
                   MOVE DTU-DATE-1 TO WS-TEST-DATE
                   PERFORM 9000-CHECK-DATE
                   IF DTU-OK
                       MOVE 'Y' TO DTU-RESULT-FLAG
                   ELSE
                       MOVE 'N' TO DTU-RESULT-FLAG
                       MOVE 00  TO DTU-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 12 TO DTU-RETURN-CODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
       1000-ADD-DAYS.
           COMPUTE WS-INT-1 = FUNCTION INTEGER-OF-DATE(DTU-DATE-1)
                            + DTU-NUMBER
           COMPUTE DTU-RESULT-DATE = FUNCTION DATE-OF-INTEGER(WS-INT-1).

      *----------------------------------------------------------------*
      * ADD N BUSINESS DAYS. N = 0 MEANS "THIS DAY IF BUSINESS DAY,    *
      * OTHERWISE THE NEXT BUSINESS DAY". NEGATIVE N STEPS BACKWARD.   *
      *----------------------------------------------------------------*
       1100-ADD-BUSINESS-DAYS.
           COMPUTE WS-INT-1 = FUNCTION INTEGER-OF-DATE(DTU-DATE-1)
           IF DTU-NUMBER = 0
               MOVE DTU-DATE-1 TO WS-TEST-DATE
               PERFORM 8100-TEST-BUSINESS-DAY
               PERFORM UNTIL WS-IS-HOLIDAY = 'N'
                   ADD 1 TO WS-INT-1
                   COMPUTE WS-TEST-DATE =
                       FUNCTION DATE-OF-INTEGER(WS-INT-1)
                   PERFORM 8100-TEST-BUSINESS-DAY
               END-PERFORM
           ELSE
               MOVE DTU-NUMBER TO WS-DAYS-TO-GO
               PERFORM UNTIL WS-DAYS-TO-GO = 0
                   IF WS-DAYS-TO-GO > 0
                       ADD 1 TO WS-INT-1
                   ELSE
                       SUBTRACT 1 FROM WS-INT-1
                   END-IF
                   COMPUTE WS-TEST-DATE =
                       FUNCTION DATE-OF-INTEGER(WS-INT-1)
                   PERFORM 8100-TEST-BUSINESS-DAY
                   IF WS-IS-HOLIDAY = 'N'
                       IF WS-DAYS-TO-GO > 0
                           SUBTRACT 1 FROM WS-DAYS-TO-GO
                       ELSE
                           ADD 1 TO WS-DAYS-TO-GO
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           COMPUTE DTU-RESULT-DATE = FUNCTION DATE-OF-INTEGER(WS-INT-1).

      *----------------------------------------------------------------*
       1200-DAYS-BETWEEN.
           MOVE DTU-DATE-2 TO WS-TEST-DATE
           PERFORM 9000-CHECK-DATE
           IF DTU-OK
               COMPUTE DTU-RESULT-NUMBER =
                     FUNCTION INTEGER-OF-DATE(DTU-DATE-2)
                   - FUNCTION INTEGER-OF-DATE(DTU-DATE-1)
           END-IF.

      *----------------------------------------------------------------*
      * 30/360 US (NASD) DAY COUNT                                     *
      *----------------------------------------------------------------*
       1300-DAYS-30-360.
           MOVE DTU-DATE-2 TO WS-TEST-DATE
           PERFORM 9000-CHECK-DATE
           IF DTU-OK
               MOVE DTU-DATE-1 TO WS-DATE-X
               MOVE DTU-DATE-2 TO WS-DATE2-X
               IF WS-DD = 31
                   MOVE 30 TO WS-DD
               END-IF
               IF WS-DD2 = 31 AND WS-DD = 30
                   MOVE 30 TO WS-DD2
               END-IF
               COMPUTE DTU-RESULT-NUMBER =
                     (WS-CCYY2 - WS-CCYY) * 360
                   + (WS-MM2   - WS-MM)   * 30
                   + (WS-DD2   - WS-DD)
           END-IF.

      *----------------------------------------------------------------*
       1400-END-OF-MONTH.
           MOVE DTU-DATE-1 TO WS-DATE-X
           PERFORM 8200-SET-LEAP-FLAG
           MOVE WS-DIM(WS-MM) TO WS-DD
           IF WS-MM = 2 AND WS-LEAP-YEAR
               MOVE 29 TO WS-DD
           END-IF
           MOVE WS-DATE-X TO DTU-RESULT-DATE.

      *----------------------------------------------------------------*
       1500-LAST-BUS-DAY-OF-MONTH.
           PERFORM 1400-END-OF-MONTH
           MOVE DTU-RESULT-DATE TO WS-TEST-DATE
           COMPUTE WS-INT-1 = FUNCTION INTEGER-OF-DATE(WS-TEST-DATE)
           PERFORM 8100-TEST-BUSINESS-DAY
           PERFORM UNTIL WS-IS-HOLIDAY = 'N'
               SUBTRACT 1 FROM WS-INT-1
               COMPUTE WS-TEST-DATE =
                   FUNCTION DATE-OF-INTEGER(WS-INT-1)
               PERFORM 8100-TEST-BUSINESS-DAY
           END-PERFORM
           MOVE WS-TEST-DATE TO DTU-RESULT-DATE.

      *----------------------------------------------------------------*
      * ADD MONTHS - IF THE TARGET MONTH IS SHORTER, CLAMP TO ITS      *
      * LAST DAY (JAN 31 + 1 MONTH = FEB 28/29)                        *
      *----------------------------------------------------------------*
       1600-ADD-MONTHS.
           MOVE DTU-DATE-1 TO WS-DATE-X
           COMPUTE WS-TOTAL-MONTHS = (WS-CCYY * 12) + (WS-MM - 1)
                                   + DTU-NUMBER
           DIVIDE WS-TOTAL-MONTHS BY 12 GIVING WS-NEW-CCYY
                  REMAINDER WS-NEW-MM
           ADD 1 TO WS-NEW-MM
           MOVE WS-NEW-CCYY TO WS-CCYY
           MOVE WS-NEW-MM   TO WS-MM
           MOVE WS-DD       TO WS-NEW-DD
           PERFORM 8200-SET-LEAP-FLAG
           MOVE WS-DIM(WS-MM) TO WS-MAX-DD
           IF WS-MM = 2 AND WS-LEAP-YEAR
               MOVE 29 TO WS-MAX-DD
           END-IF
           IF WS-NEW-DD > WS-MAX-DD
               MOVE WS-MAX-DD TO WS-DD
           END-IF
           MOVE WS-DATE-X TO DTU-RESULT-DATE.

      *----------------------------------------------------------------*
      * DAY OF WEEK: INTEGER-OF-DATE 1 = MONDAY 1601-01-01             *
      *----------------------------------------------------------------*
       8000-GET-DOW.
           COMPUTE WS-INT-2 = FUNCTION INTEGER-OF-DATE(WS-TEST-DATE)
           COMPUTE WS-DOW = FUNCTION MOD(WS-INT-2 - 1, 7) + 1.

      *----------------------------------------------------------------*
      * SETS WS-IS-HOLIDAY = 'Y' IF WS-TEST-DATE IS NOT A BUSINESS DAY *
      *----------------------------------------------------------------*
       8100-TEST-BUSINESS-DAY.
           MOVE 'N' TO WS-IS-HOLIDAY
           PERFORM 8000-GET-DOW
           IF WS-DOW > 5
               MOVE 'Y' TO WS-IS-HOLIDAY
           ELSE
               SET HOL-IDX TO 1
               SEARCH HOLIDAY-ENTRY
                   AT END
                       CONTINUE
                   WHEN HOLIDAY-ENTRY(HOL-IDX) = WS-TEST-DATE
                       MOVE 'Y' TO WS-IS-HOLIDAY
               END-SEARCH
           END-IF.

      *----------------------------------------------------------------*
       8200-SET-LEAP-FLAG.
           MOVE 'N' TO WS-LEAP-FLAG
           IF FUNCTION MOD(WS-CCYY, 4) = 0
               MOVE 'Y' TO WS-LEAP-FLAG
               IF FUNCTION MOD(WS-CCYY, 100) = 0
                  AND FUNCTION MOD(WS-CCYY, 400) NOT = 0
                   MOVE 'N' TO WS-LEAP-FLAG
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       9000-CHECK-DATE.
           MOVE 00 TO DTU-RETURN-CODE
           IF WS-TEST-DATE NOT NUMERIC
               MOVE 08 TO DTU-RETURN-CODE
           ELSE
               IF FUNCTION TEST-DATE-YYYYMMDD(WS-TEST-DATE) NOT = 0
                   MOVE 08 TO DTU-RETURN-CODE
               END-IF
           END-IF.
