      *================================================================*
      * PROGRAM  : LDHOLD                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : BUILDS THE HOLD MASTER (HOLDMST) FROM A PIPE-       *
      *            DELIMITED EXTRACT (HOLDSEED) AND APPLIES EACH       *
      *            ACTIVE HOLD TO THE ACCOUNT MASTER (ACCT-HOLD-AMT    *
      *            UP, ACCT-AVAIL-BAL DOWN). HOLDMST IS ALWAYS CREATED,*
      *            EVEN WHEN THE EXTRACT IS EMPTY OR MISSING.          *
      * INPUT    : HOLDSEED  ACCT|SEQ|TYPE|AMOUNT|PLACED|RELEASE|      *
      *              STATUS|REASON|TRANREF|PLACEDBY                    *
      * I-O      : ACCTMST   (KSDS)                                    *
      * OUTPUT   : HOLDMST   (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      * 2015-11-19 DPS  CNV-0121 APPLY ACTIVE HOLDS TO AVAILABLE BAL   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDHOLD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SEED-FILE ASSIGN TO "HOLDSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT HOLD-FILE ASSIGN TO "HOLDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HOLD-KEY
               FILE STATUS IS WS-HOLD-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(300).
       FD  HOLD-FILE.
       COPY HOLDREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-HOLD-STATUS                  PIC X(02).
       01  WS-ACCT-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-APPLIED-COUNT            PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(40) OCCURS 10.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT HOLD-FILE
           OPEN I-O ACCOUNT-FILE
           IF (WS-SEED-STATUS NOT = '00' AND WS-SEED-STATUS NOT = '05')
              OR WS-HOLD-STATUS NOT = '00'
              OR WS-ACCT-STATUS NOT = '00'
               MOVE 'LDHOLD' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - HOLDSEED/HOLDMST/ACCTMST'
                 TO ABND-MESSAGE
               MOVE WS-HOLD-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE HOLD-FILE ACCOUNT-FILE
           DISPLAY 'LDHOLD  READ=' WS-READ-COUNT
                   ' LOADED=' WS-LOAD-COUNT
                   ' APPLIED=' WS-APPLIED-COUNT
                   ' ERRORS=' WS-ERROR-COUNT
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
               INTO WS-F(1) WS-F(2) WS-F(3) WS-F(4)  WS-F(5)
                    WS-F(6) WS-F(7) WS-F(8) WS-F(9)  WS-F(10)
           END-UNSTRING
           INITIALIZE HOLD-RECORD
           MOVE WS-F(1)  TO HOLD-ACCT-NUMBER
           COMPUTE HOLD-SEQ          = FUNCTION NUMVAL(WS-F(2))
           MOVE WS-F(3)  TO HOLD-TYPE
           COMPUTE HOLD-AMOUNT       = FUNCTION NUMVAL(WS-F(4))
           COMPUTE HOLD-PLACED-DATE  = FUNCTION NUMVAL(WS-F(5))
           COMPUTE HOLD-RELEASE-DATE = FUNCTION NUMVAL(WS-F(6))
           IF HOLD-RELEASE-DATE = 0
               MOVE 99999999 TO HOLD-RELEASE-DATE
           END-IF
           MOVE WS-F(7)  TO HOLD-STATUS
           IF HOLD-STATUS = SPACE
               SET HOLD-ACTIVE TO TRUE
           END-IF
           MOVE WS-F(8)  TO HOLD-REASON
           MOVE WS-F(9)  TO HOLD-TRAN-REF
           MOVE WS-F(10) TO HOLD-PLACED-BY

           MOVE HOLD-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'LDHOLD  ACCOUNT NOT FOUND ' HOLD-ACCT-NUMBER
                   EXIT PARAGRAPH
           END-READ

           WRITE HOLD-RECORD
           IF WS-HOLD-STATUS NOT = '00'
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDHOLD  WRITE ERROR ' WS-HOLD-STATUS
                       ' KEY=' HOLD-KEY
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-LOAD-COUNT

           IF HOLD-ACTIVE
               ADD HOLD-AMOUNT      TO ACCT-HOLD-AMT
               SUBTRACT HOLD-AMOUNT FROM ACCT-AVAIL-BAL
               REWRITE ACCOUNT-RECORD
               IF WS-ACCT-STATUS = '00'
                   ADD 1 TO WS-APPLIED-COUNT
               ELSE
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'LDHOLD  ACCTMST REWRITE ' WS-ACCT-STATUS
                           ' KEY=' ACCT-NUMBER
               END-IF
           END-IF.
