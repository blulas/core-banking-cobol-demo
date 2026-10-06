      *================================================================*
      * PROGRAM  : LDINTST                                             *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : APPLIES THE INTEREST / STATEMENT-CYCLE STATE        *
      *            SNAPSHOT (INTSEED) TO ACCOUNTS ALREADY LOADED BY    *
      *            LDACCT: UNPAID ACCRUED INTEREST, LAST ACCRUAL /     *
      *            CAPITALISATION / STATEMENT DATES, CYCLE BALANCE     *
      *            ACCUMULATOR, DAYS IN CYCLE, CYCLE MINIMUM BALANCE   *
      *            AND IRS BACKUP WITHHOLDING (B-NOTICE) STATUS.       *
      *            A BLANK FIELD LEAVES THE LDACCT VALUE IN PLACE.     *
      * INPUT    : INTSEED   ACCT|ACCRUED|LASTACCR|LASTCAP|LASTSTMT|   *
      *                      DAYSCYC|BALACCUM|MINBAL|BACKUPWH|WHYTD    *
      * I-O      : ACCTMST   (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT (CNV-0117)  *
      * 2015-11-30 DPS  B-NOTICE FLAG FROM TAX REPORTING EXTRACT       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDINTST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "INTSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(300).
       FD  ACCOUNT-FILE.
       COPY ACCTREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-ACCT-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-UPDATE-COUNT             PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(30) OCCURS 10.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN I-O ACCOUNT-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-ACCT-STATUS NOT = '00'
               MOVE 'LDINTST' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - INTSEED/ACCTMST' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE ACCOUNT-FILE
           DISPLAY 'LDINTST READ=' WS-READ-COUNT
                   ' UPDATED=' WS-UPDATE-COUNT ' ERRORS=' WS-ERROR-COUNT
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
               INTO WS-F(1) WS-F(2) WS-F(3) WS-F(4) WS-F(5)
                    WS-F(6) WS-F(7) WS-F(8) WS-F(9) WS-F(10)
           END-UNSTRING
           MOVE WS-F(1) TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'LDINTST ACCOUNT NOT ON FILE ' WS-F(1)
                   EXIT PARAGRAPH
           END-READ
           IF WS-F(2) NOT = SPACES
               COMPUTE ACCT-ACCRUED-INT = FUNCTION NUMVAL(WS-F(2))
           END-IF
           IF WS-F(3) NOT = SPACES
               COMPUTE ACCT-LAST-ACCRUAL-DATE = FUNCTION NUMVAL(WS-F(3))
           END-IF
           IF WS-F(4) NOT = SPACES
               COMPUTE ACCT-LAST-CAP-DATE = FUNCTION NUMVAL(WS-F(4))
           END-IF
           IF WS-F(5) NOT = SPACES
               COMPUTE ACCT-LAST-STMT-DATE = FUNCTION NUMVAL(WS-F(5))
           END-IF
           IF WS-F(6) NOT = SPACES
               COMPUTE ACCT-DAYS-IN-CYCLE = FUNCTION NUMVAL(WS-F(6))
           END-IF
           IF WS-F(7) NOT = SPACES
               COMPUTE ACCT-BAL-ACCUM-CYC = FUNCTION NUMVAL(WS-F(7))
           END-IF
           IF WS-F(8) NOT = SPACES
               COMPUTE ACCT-MIN-BAL-CYC = FUNCTION NUMVAL(WS-F(8))
           END-IF
           IF WS-F(9) NOT = SPACES
               MOVE WS-F(9) TO ACCT-BACKUP-WH-FLAG
           END-IF
           IF WS-F(10) NOT = SPACES
               COMPUTE ACCT-WITHHELD-YTD = FUNCTION NUMVAL(WS-F(10))
           END-IF
           REWRITE ACCOUNT-RECORD
           IF WS-ACCT-STATUS = '00'
               ADD 1 TO WS-UPDATE-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDINTST REWRITE ERROR ' WS-ACCT-STATUS
                       ' KEY=' ACCT-NUMBER
           END-IF.
