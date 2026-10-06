      *================================================================*
      * PROGRAM  : TESTLIB                                             *
      * TYPE     : CALLED SUBROUTINE (UNIT TEST SUPPORT)               *
      * PURPOSE  : MINIMAL ASSERTION LIBRARY FOR COBOL UNIT TESTS.     *
      *            SEE COPYBOOK TESTPARM FOR THE FUNCTION CODES.       *
      *            OUTPUT FORMAT:                                      *
      *              PASS  <SUITE> :: <CASE>                           *
      *              FAIL  <SUITE> :: <CASE>                           *
      *                    EXPECTED [...] ACTUAL [...]                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTLIB.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SUITE                        PIC X(30) VALUE SPACES.
       01  WS-PASSED                       PIC 9(05) VALUE 0.
       01  WS-FAILED                       PIC 9(05) VALUE 0.
       01  WS-EXP-ED                       PIC -(13)9.9(06).
       01  WS-ACT-ED                       PIC -(13)9.9(06).
       01  WS-RESULT                       PIC X(04).

       LINKAGE SECTION.
       COPY TESTPARM.

       PROCEDURE DIVISION USING TEST-PARMS.
       0000-MAIN.
           EVALUATE TST-FUNCTION
               WHEN 'INIT'
                   MOVE TST-SUITE-NAME TO WS-SUITE
                   MOVE 0 TO WS-PASSED WS-FAILED
                   DISPLAY '=== SUITE ' WS-SUITE
               WHEN 'AEQ '
               WHEN 'AEQ'
                   IF TST-EXPECTED = TST-ACTUAL
                       PERFORM 8000-PASS
                   ELSE
                       PERFORM 8100-FAIL
                       DISPLAY '      EXPECTED ['
                               FUNCTION TRIM(TST-EXPECTED TRAILING)
                               '] ACTUAL ['
                               FUNCTION TRIM(TST-ACTUAL TRAILING) ']'
                   END-IF
               WHEN 'NEQ '
               WHEN 'NEQ'
                   IF TST-EXP-NUM = TST-ACT-NUM
                       PERFORM 8000-PASS
                   ELSE
                       PERFORM 8100-FAIL
                       MOVE TST-EXP-NUM TO WS-EXP-ED
                       MOVE TST-ACT-NUM TO WS-ACT-ED
                       DISPLAY '      EXPECTED ['
                               FUNCTION TRIM(WS-EXP-ED)
                               '] ACTUAL ['
                               FUNCTION TRIM(WS-ACT-ED) ']'
                   END-IF
               WHEN 'TRUE'
                   IF TST-CONDITION = 'Y'
                       PERFORM 8000-PASS
                   ELSE
                       PERFORM 8100-FAIL
                   END-IF
               WHEN 'SUMM'
                   DISPLAY '--- ' FUNCTION TRIM(WS-SUITE)
                           ' PASSED=' WS-PASSED
                           ' FAILED=' WS-FAILED
                   IF WS-FAILED > 0
                       MOVE 8 TO RETURN-CODE
                   ELSE
                       MOVE 0 TO RETURN-CODE
                   END-IF
               WHEN OTHER
                   DISPLAY 'TESTLIB: UNKNOWN FUNCTION ' TST-FUNCTION
           END-EVALUATE
           MOVE WS-PASSED TO TST-PASSED-COUNT
           MOVE WS-FAILED TO TST-FAILED-COUNT
           GOBACK.

       8000-PASS.
           ADD 1 TO WS-PASSED
           DISPLAY 'PASS  ' FUNCTION TRIM(WS-SUITE) ' :: '
                   FUNCTION TRIM(TST-CASE-NAME TRAILING).

       8100-FAIL.
           ADD 1 TO WS-FAILED
           DISPLAY 'FAIL  ' FUNCTION TRIM(WS-SUITE) ' :: '
                   FUNCTION TRIM(TST-CASE-NAME TRAILING).
