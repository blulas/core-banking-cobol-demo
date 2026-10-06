      *================================================================*
      * PROGRAM  : LDPOOL                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE NOTIONAL POOL MASTER (POOLMST) FROM A     *
      *            PIPE-DELIMITED EXTRACT (POOLSEED). MEMBER ACCOUNTS  *
      *            ARE A COMMA-SEPARATED LIST IN THE LAST FIELD.       *
      * INPUT    : POOLSEED  ID|NAME|CUST|HEADER|METHOD|CREDITSPREAD|  *
      *              DEBITSPREAD|LIMIT|STATUS|EFF|LASTALLOC|MEMBERS    *
      * OUTPUT   : POOLMST   (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      * 2019-03-25 AKW  MEMBER LIST 10 -> 20                           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDPOOL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "POOLSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT POOL-FILE ASSIGN TO "POOLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS POOL-ID
               FILE STATUS IS WS-POOL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(600).
       FD  POOL-FILE.
       COPY POOLREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-POOL-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(40) OCCURS 11.
           05  WS-MEMBER-LIST              PIC X(300).
       01  WS-MEMBER-WORK.
           05  WS-PTR                      PIC 9(03).
           05  WS-MBR                      PIC X(12).
           05  WS-IX                       PIC 9(02).
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT POOL-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-POOL-STATUS NOT = '00'
               MOVE 'LDPOOL'  TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'POOLMST' TO ABND-FILE-NAME
               MOVE WS-POOL-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - POOLSEED/POOLMST' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE POOL-FILE
           DISPLAY 'LDPOOL  READ=' WS-READ-COUNT
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
                    WS-F(11) WS-MEMBER-LIST
           END-UNSTRING
           INITIALIZE POOL-RECORD
           MOVE WS-F(1)  TO POOL-ID
           MOVE WS-F(2)  TO POOL-NAME
           MOVE WS-F(3)  TO POOL-CUST-ID
           MOVE WS-F(4)  TO POOL-HEADER-ACCT
           MOVE WS-F(5)  TO POOL-ALLOC-METHOD
           COMPUTE POOL-CREDIT-SPREAD   = FUNCTION NUMVAL(WS-F(6))
           COMPUTE POOL-DEBIT-SPREAD    = FUNCTION NUMVAL(WS-F(7))
           COMPUTE POOL-LIMIT           = FUNCTION NUMVAL(WS-F(8))
           MOVE WS-F(9)  TO POOL-STATUS
           COMPUTE POOL-EFF-DATE        = FUNCTION NUMVAL(WS-F(10))
           COMPUTE POOL-LAST-ALLOC-DATE = FUNCTION NUMVAL(WS-F(11))
           MOVE 1 TO WS-PTR
           MOVE 0 TO WS-IX
           PERFORM UNTIL WS-PTR > 300 OR WS-IX >= 20
               MOVE SPACES TO WS-MBR
               UNSTRING WS-MEMBER-LIST DELIMITED BY ',' OR SPACE
                   INTO WS-MBR WITH POINTER WS-PTR
               END-UNSTRING
               IF WS-MBR = SPACES
                   MOVE 301 TO WS-PTR
               ELSE
                   ADD 1 TO WS-IX
                   MOVE WS-MBR TO POOL-MEMBER-ACCT(WS-IX)
               END-IF
           END-PERFORM
           MOVE WS-IX TO POOL-MEMBER-COUNT
           WRITE POOL-RECORD
           IF WS-POOL-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDPOOL  WRITE ERROR ' WS-POOL-STATUS
                       ' KEY=' POOL-ID
           END-IF.
