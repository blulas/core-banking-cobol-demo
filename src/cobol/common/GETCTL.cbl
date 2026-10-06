      *================================================================*
      * PROGRAM  : GETCTL                                              *
      * TYPE     : CALLED SUBROUTINE                                   *
      * PURPOSE  : RETURNS THE BANK CONTROL RECORD (BUSINESS DATE,     *
      *            CUTOFFS, REFERENCE RATES). THE FILE IS READ ON THE  *
      *            FIRST CALL ONLY AND CACHED FOR THE RUN UNIT.        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-14 RJT  ORIGINAL                                       *
      * 2007-03-30 BHS  CACHE RECORD - AVOID RE-OPEN PER CALL          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETCTL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANKCTL-FILE ASSIGN TO "BANKCTL"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CTL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BANKCTL-FILE.
       01  BANKCTL-FILE-REC                PIC X(121).

       WORKING-STORAGE SECTION.
       01  WS-CTL-STATUS                   PIC X(02).
       01  WS-LOADED-FLAG                  PIC X(01) VALUE 'N'.
           88  WS-CTL-LOADED                         VALUE 'Y'.
       01  WS-CACHED-CTL                   PIC X(121).
       COPY ABNDPARM.

       LINKAGE SECTION.
       COPY BANKCTL.

       PROCEDURE DIVISION USING BANK-CONTROL-RECORD.
       0000-MAIN.
           IF NOT WS-CTL-LOADED
               PERFORM 1000-LOAD-CONTROL
           END-IF
           MOVE WS-CACHED-CTL TO BANK-CONTROL-RECORD
           GOBACK.

       1000-LOAD-CONTROL.
           OPEN INPUT BANKCTL-FILE
           IF WS-CTL-STATUS NOT = '00'
               MOVE 'GETCTL'   TO ABND-PROGRAM
               MOVE '1000-LOAD-CONTROL' TO ABND-PARAGRAPH
               MOVE 'BANKCTL'  TO ABND-FILE-NAME
               MOVE WS-CTL-STATUS TO ABND-FILE-STATUS
               MOVE 'UNABLE TO OPEN BANK CONTROL FILE' TO ABND-MESSAGE
               MOVE 16 TO ABND-CODE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           READ BANKCTL-FILE INTO WS-CACHED-CTL
           IF WS-CTL-STATUS NOT = '00'
               MOVE 'GETCTL'   TO ABND-PROGRAM
               MOVE '1000-LOAD-CONTROL' TO ABND-PARAGRAPH
               MOVE 'BANKCTL'  TO ABND-FILE-NAME
               MOVE WS-CTL-STATUS TO ABND-FILE-STATUS
               MOVE 'BANK CONTROL RECORD MISSING' TO ABND-MESSAGE
               MOVE 16 TO ABND-CODE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           CLOSE BANKCTL-FILE
           SET WS-CTL-LOADED TO TRUE.
