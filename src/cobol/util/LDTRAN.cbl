      *================================================================*
      * PROGRAM  : LDTRAN                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : CONVERTS PIPE-DELIMITED TRANSACTIONS (TRANSEED)     *
      *            INTO FIXED TRANREC FORMAT (TRANOUT).                *
      *            PARM 'DAY'  - TODAY'S CHANNEL INPUT, STATUS PENDING *
      *            PARM 'HIST' - PRIOR HISTORY, STATUS POSTED,         *
      *                          POST DATE = EFFECTIVE DATE            *
      *            DR/CR, CASH AND REG D FLAGS COME FROM TRANCODE.     *
      * INPUT    : TRANSEED  ACCT|CODE|AMOUNT|CHANNEL|EFFDATE|TIME|    *
      *              DESC|REFERENCE|COUNTERACCT|CPNAME|CPROUTING|      *
      *              CPCOUNTRY|USER|COSTCENTER                         *
      * OUTPUT   : TRANOUT   (SEQUENTIAL TRANREC)                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDTRAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "TRANSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT TRAN-OUT-FILE ASSIGN TO "TRANOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(400).
       FD  TRAN-OUT-FILE.
       COPY TRANREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-OUT-STATUS                   PIC X(02).
       01  WS-PARM                         PIC X(10).
           88  WS-MODE-DAY                           VALUE 'DAY'.
           88  WS-MODE-HIST                          VALUE 'HIST'.
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
           05  WS-SEQ                      PIC 9(08) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(40) OCCURS 14.
       COPY TRANCODE.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-PARM FROM COMMAND-LINE
           IF NOT WS-MODE-DAY AND NOT WS-MODE-HIST
               MOVE 'DAY' TO WS-PARM
           END-IF
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           IF WS-MODE-HIST
               MOVE 90000000 TO WS-SEQ
           END-IF
           OPEN INPUT SEED-FILE
           OPEN OUTPUT TRAN-OUT-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-OUT-STATUS NOT = '00'
               MOVE 'LDTRAN' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - TRANSEED/TRANOUT' TO ABND-MESSAGE
               MOVE WS-OUT-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE TRAN-OUT-FILE
           DISPLAY 'LDTRAN  MODE=' WS-PARM(1:4) ' READ=' WS-READ-COUNT
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
                    WS-F(11) WS-F(12) WS-F(13) WS-F(14)
           END-UNSTRING
           INITIALIZE TRANSACTION-RECORD
           ADD 1 TO WS-SEQ
           MOVE WS-F(1)  TO TRAN-ACCT-NUMBER
           MOVE WS-F(2)  TO TRAN-CODE
           COMPUTE TRAN-AMOUNT = FUNCTION NUMVAL(WS-F(3))
           MOVE WS-F(4)  TO TRAN-CHANNEL
           COMPUTE TRAN-EFFECTIVE-DATE = FUNCTION NUMVAL(WS-F(5))
           IF TRAN-EFFECTIVE-DATE = 0
               MOVE CTL-BUSINESS-DATE TO TRAN-EFFECTIVE-DATE
           END-IF
           COMPUTE TRAN-TIME = FUNCTION NUMVAL(WS-F(6))
           MOVE WS-F(7)  TO TRAN-DESCRIPTION
           MOVE WS-F(8)  TO TRAN-REFERENCE
           MOVE WS-F(9)  TO TRAN-COUNTER-ACCT
           MOVE WS-F(10) TO TRAN-COUNTERPARTY-NAME
           MOVE WS-F(11) TO TRAN-COUNTERPARTY-ROUTING
           MOVE WS-F(12) TO TRAN-COUNTERPARTY-COUNTRY
           MOVE WS-F(13) TO TRAN-USER-ID
           MOVE WS-F(14) TO TRAN-COST-CENTER
           MOVE 'LDTRAN' TO TRAN-SOURCE-PGM

           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'LDTRAN  UNKNOWN TRAN CODE ' TRAN-CODE
                           ' ACCT=' TRAN-ACCT-NUMBER
                   MOVE 'D' TO TRAN-DR-CR
               WHEN TC-CODE(TC-IDX) = TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)     TO TRAN-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX) TO TRAN-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX) TO TRAN-REGD-FLAG
           END-SEARCH

           IF WS-MODE-HIST
               MOVE TRAN-EFFECTIVE-DATE TO TRAN-ID-DATE
                                           TRAN-POST-DATE
               SET TRAN-POSTED TO TRUE
           ELSE
               MOVE CTL-BUSINESS-DATE   TO TRAN-ID-DATE
               MOVE ZERO                TO TRAN-POST-DATE
               SET TRAN-PENDING TO TRUE
           END-IF
           MOVE WS-SEQ TO TRAN-ID-SEQ
           WRITE TRANSACTION-RECORD
           IF WS-OUT-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
