      *================================================================*
      * PROGRAM  : ABENDRTN                                            *
      * TYPE     : CALLED SUBROUTINE                                   *
      * PURPOSE  : STANDARD ABNORMAL TERMINATION. WRITES A DIAGNOSTIC  *
      *            BLOCK TO SYSOUT AND ENDS THE RUN UNIT WITH THE      *
      *            REQUESTED CONDITION CODE (DEFAULT 16) SO THAT       *
      *            DOWNSTREAM JCL STEPS ARE BYPASSED.                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 2004-01-12 BHS  REPLACED CALL TO ILBOABN0 WITH STOP RUN        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ABENDRTN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TIMESTAMP                    PIC X(21).

       LINKAGE SECTION.
       COPY ABNDPARM.

       PROCEDURE DIVISION USING ABEND-PARMS.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           IF ABND-CODE = ZERO
               MOVE 16 TO ABND-CODE
           END-IF
           DISPLAY '************************************************'
           DISPLAY '* ABENDRTN - ABNORMAL TERMINATION              *'
           DISPLAY '************************************************'
           DISPLAY '* TIME      : ' WS-TIMESTAMP(1:14)
           DISPLAY '* PROGRAM   : ' ABND-PROGRAM
           DISPLAY '* PARAGRAPH : ' ABND-PARAGRAPH
           IF ABND-FILE-NAME NOT = SPACES
               DISPLAY '* FILE      : ' ABND-FILE-NAME
                       '  STATUS: ' ABND-FILE-STATUS
           END-IF
           DISPLAY '* MESSAGE   : ' ABND-MESSAGE
           DISPLAY '* COND CODE : ' ABND-CODE
           DISPLAY '************************************************'
           MOVE ABND-CODE TO RETURN-CODE
           STOP RUN.
