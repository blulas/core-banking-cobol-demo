      *================================================================*
      * PROGRAM  : LDSWEEP                                             *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE SWEEP DEFINITION MASTER (SWEEPMST) FROM   *
      *            A PIPE-DELIMITED EXTRACT (SWPSEED).                 *
      * INPUT    : SWPSEED   ID|TYPE|CUST|SOURCE|TARGET|TARGETBAL|     *
      *              MINBAL|MAXBAL|INCREMENT|MINXFER|FREQ|STATUS|EFF|  *
      *              EXP|PRIORITY|LASTDATE|LASTAMT|LASTDIR             *
      * OUTPUT   : SWEEPMST  (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDSWEEP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "SWPSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT SWEEP-FILE ASSIGN TO "SWEEPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SWP-ID
               FILE STATUS IS WS-SWP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(400).
       FD  SWEEP-FILE.
       COPY SWEEPREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-SWP-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(30) OCCURS 18.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT SWEEP-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-SWP-STATUS NOT = '00'
               MOVE 'LDSWEEP' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'SWEEPMST' TO ABND-FILE-NAME
               MOVE WS-SWP-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - SWPSEED/SWEEPMST' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE SWEEP-FILE
           DISPLAY 'LDSWEEP READ=' WS-READ-COUNT
                   ' LOADED=' WS-LOAD-COUNT ' ERRORS=' WS-ERROR-COUNT
           IF WS-ERROR-COUNT > 0
               MOVE 4 TO RETURN-CODE
           END-IF
           STOP RUN.

       1000-PROCESS-LINE.
           IF SEED-LINE(1:1) = '#' OR SEED-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-READ-COUNT
           MOVE SPACES TO WS-FIELDS
           UNSTRING SEED-LINE DELIMITED BY '|'
               INTO WS-F(1)  WS-F(2)  WS-F(3)  WS-F(4)  WS-F(5)
                    WS-F(6)  WS-F(7)  WS-F(8)  WS-F(9)  WS-F(10)
                    WS-F(11) WS-F(12) WS-F(13) WS-F(14) WS-F(15)
                    WS-F(16) WS-F(17) WS-F(18)
           END-UNSTRING
           INITIALIZE SWEEP-RECORD
           MOVE WS-F(1)  TO SWP-ID
           MOVE WS-F(2)  TO SWP-TYPE
           MOVE WS-F(3)  TO SWP-CUST-ID
           MOVE WS-F(4)  TO SWP-SOURCE-ACCT
           MOVE WS-F(5)  TO SWP-TARGET-ACCT
           COMPUTE SWP-TARGET-BAL     = FUNCTION NUMVAL(WS-F(6))
           COMPUTE SWP-MIN-BAL        = FUNCTION NUMVAL(WS-F(7))
           COMPUTE SWP-MAX-BAL        = FUNCTION NUMVAL(WS-F(8))
           COMPUTE SWP-INCREMENT      = FUNCTION NUMVAL(WS-F(9))
           COMPUTE SWP-MIN-TRANSFER   = FUNCTION NUMVAL(WS-F(10))
           MOVE WS-F(11) TO SWP-FREQUENCY
           MOVE WS-F(12) TO SWP-STATUS
           COMPUTE SWP-EFF-DATE       = FUNCTION NUMVAL(WS-F(13))
           COMPUTE SWP-EXP-DATE       = FUNCTION NUMVAL(WS-F(14))
           COMPUTE SWP-PRIORITY       = FUNCTION NUMVAL(WS-F(15))
           COMPUTE SWP-LAST-SWEEP-DATE = FUNCTION NUMVAL(WS-F(16))
           COMPUTE SWP-LAST-SWEEP-AMT = FUNCTION NUMVAL(WS-F(17))
           MOVE WS-F(18) TO SWP-LAST-SWEEP-DIR
           MOVE SWP-EFF-DATE TO SWP-LAST-MAINT-DATE
           MOVE 'CONVERT'    TO SWP-LAST-MAINT-USER
           WRITE SWEEP-RECORD
           IF WS-SWP-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDSWEEP WRITE ERROR ' WS-SWP-STATUS
                       ' KEY=' SWP-ID
           END-IF.
