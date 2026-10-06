      *================================================================*
      * COPYBOOK : DATEPARM                                            *
      * PURPOSE  : LINKAGE FOR DATEUTIL - COMMON DATE ROUTINES         *
      *   ADDD  DATE-1 + NUMBER CALENDAR DAYS          -> RESULT-DATE  *
      *   ADDB  DATE-1 + NUMBER BUSINESS DAYS          -> RESULT-DATE  *
      *         (NUMBER = 0 ROLLS FORWARD TO NEXT BUSINESS DAY)        *
      *   DIFF  DATE-2 - DATE-1 IN CALENDAR DAYS       -> RESULT-NUMBER*
      *   D360  DATE-2 - DATE-1 ON 30/360 BASIS        -> RESULT-NUMBER*
      *   DOWK  DAY OF WEEK OF DATE-1 (1=MON..7=SUN)   -> RESULT-NUMBER*
      *   ISBD  IS DATE-1 A BUSINESS DAY               -> RESULT-FLAG  *
      *   EOMO  LAST CALENDAR DAY OF DATE-1 MONTH      -> RESULT-DATE  *
      *   LBDM  LAST BUSINESS DAY OF DATE-1 MONTH      -> RESULT-DATE  *
      *   ADDM  DATE-1 + NUMBER MONTHS (EOM CLAMPED)   -> RESULT-DATE  *
      *   DIYR  DAYS IN YEAR OF DATE-1 (365/366)       -> RESULT-NUMBER*
      *   VALD  IS DATE-1 A VALID CCYYMMDD DATE        -> RESULT-FLAG  *
      *================================================================*
       01  DATE-PARMS.
           05  DTU-FUNCTION                PIC X(04).
               88  DTU-ADD-DAYS                      VALUE 'ADDD'.
               88  DTU-ADD-BUS-DAYS                  VALUE 'ADDB'.
               88  DTU-DAYS-BETWEEN                  VALUE 'DIFF'.
               88  DTU-DAYS-30-360                   VALUE 'D360'.
               88  DTU-DAY-OF-WEEK                   VALUE 'DOWK'.
               88  DTU-IS-BUS-DAY                    VALUE 'ISBD'.
               88  DTU-END-OF-MONTH                  VALUE 'EOMO'.
               88  DTU-LAST-BUS-DAY-MONTH            VALUE 'LBDM'.
               88  DTU-ADD-MONTHS                    VALUE 'ADDM'.
               88  DTU-DAYS-IN-YEAR                  VALUE 'DIYR'.
               88  DTU-VALIDATE                      VALUE 'VALD'.
           05  DTU-DATE-1                  PIC 9(08).
           05  DTU-DATE-2                  PIC 9(08).
           05  DTU-NUMBER                  PIC S9(05).
           05  DTU-RESULT-DATE             PIC 9(08).
           05  DTU-RESULT-NUMBER           PIC S9(07).
           05  DTU-RESULT-FLAG             PIC X(01).
               88  DTU-RESULT-YES                    VALUE 'Y'.
               88  DTU-RESULT-NO                     VALUE 'N'.
           05  DTU-RETURN-CODE             PIC 9(02).
               88  DTU-OK                            VALUE 00.
               88  DTU-INVALID-DATE                  VALUE 08.
               88  DTU-INVALID-FUNCTION              VALUE 12.
