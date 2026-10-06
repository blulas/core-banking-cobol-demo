      *================================================================*
      * COPYBOOK : TESTPARM                                            *
      * PURPOSE  : LINKAGE FOR TESTLIB - UNIT TEST ASSERTION LIBRARY   *
      *   INIT  START A SUITE (TST-SUITE-NAME)                         *
      *   AEQ   ASSERT TST-EXPECTED = TST-ACTUAL (ALPHANUMERIC)        *
      *   NEQ   ASSERT TST-EXP-NUM  = TST-ACT-NUM (NUMERIC, 6 DEC)     *
      *   TRUE  ASSERT TST-CONDITION = 'Y'                             *
      *   SUMM  PRINT SUMMARY; TST-FAILED-COUNT > 0 SETS RC 8          *
      *================================================================*
       01  TEST-PARMS.
           05  TST-FUNCTION                PIC X(04).
           05  TST-SUITE-NAME              PIC X(30).
           05  TST-CASE-NAME               PIC X(60).
           05  TST-EXPECTED                PIC X(60).
           05  TST-ACTUAL                  PIC X(60).
           05  TST-EXP-NUM                 PIC S9(13)V9(06).
           05  TST-ACT-NUM                 PIC S9(13)V9(06).
           05  TST-CONDITION               PIC X(01).
           05  TST-PASSED-COUNT            PIC 9(05).
           05  TST-FAILED-COUNT            PIC 9(05).
