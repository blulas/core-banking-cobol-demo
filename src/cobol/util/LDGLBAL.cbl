      *================================================================*
      * PROGRAM  : LDGLBAL                                             *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS GL OPENING BALANCES (GLBAL) FROM A PIPE-      *
      *            DELIMITED EXTRACT (GLBSEED). DESCRIPTION, ACCOUNT   *
      *            TYPE AND NORMAL BALANCE COME FROM THE CHART OF      *
      *            ACCOUNTS (GLCHART). UNKNOWN GL ACCOUNTS ARE         *
      *            REJECTED.                                           *
      * INPUT    : GLBSEED   GL|COSTCENTER|OPENING|PERIODSTART         *
      * OUTPUT   : GLBAL     (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDGLBAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "GLBSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT GLBAL-FILE ASSIGN TO "GLBAL"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GLB-KEY
               FILE STATUS IS WS-GLB-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(200).
       FD  GLBAL-FILE.
       COPY GLBALREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-GLB-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-TOTALS.
           05  WS-TOTAL-DEBIT-NORMAL       PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-CREDIT-NORMAL      PIC S9(15)V99 COMP-3
                                           VALUE 0.
           05  WS-TOTAL-ED                 PIC -(15)9.99.
       01  WS-FIELDS.
           05  WS-F                        PIC X(30) OCCURS 4.
       COPY GLCHART.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT GLBAL-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-GLB-STATUS NOT = '00'
               MOVE 'LDGLBAL' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'GLBAL'   TO ABND-FILE-NAME
               MOVE WS-GLB-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - GLBSEED/GLBAL' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE GLBAL-FILE
           DISPLAY 'LDGLBAL READ=' WS-READ-COUNT
                   ' LOADED=' WS-LOAD-COUNT ' ERRORS=' WS-ERROR-COUNT
           MOVE WS-TOTAL-DEBIT-NORMAL TO WS-TOTAL-ED
           DISPLAY 'LDGLBAL DEBIT-NORMAL TOTAL  ' WS-TOTAL-ED
           MOVE WS-TOTAL-CREDIT-NORMAL TO WS-TOTAL-ED
           DISPLAY 'LDGLBAL CREDIT-NORMAL TOTAL ' WS-TOTAL-ED
           IF WS-TOTAL-DEBIT-NORMAL NOT = WS-TOTAL-CREDIT-NORMAL
               DISPLAY 'LDGLBAL *** OPENING BALANCES DO NOT BALANCE'
               MOVE 4 TO RETURN-CODE
           END-IF
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
               INTO WS-F(1) WS-F(2) WS-F(3) WS-F(4)
           END-UNSTRING
           INITIALIZE GL-BALANCE-RECORD
           MOVE WS-F(1) TO GLB-GL-ACCOUNT
           MOVE WS-F(2) TO GLB-COST-CENTER
           SET GLC-IDX TO 1
           SEARCH GLC-ENTRY
               AT END
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'LDGLBAL GL NOT ON CHART ' GLB-GL-ACCOUNT
                   EXIT PARAGRAPH
               WHEN GLC-GL-ACCOUNT(GLC-IDX) = GLB-GL-ACCOUNT
                   MOVE GLC-DESCRIPTION(GLC-IDX) TO GLB-DESCRIPTION
                   MOVE GLC-ACCT-TYPE(GLC-IDX)   TO GLB-ACCT-TYPE
                   MOVE GLC-NORMAL-BAL(GLC-IDX)  TO GLB-NORMAL-BAL
           END-SEARCH
           COMPUTE GLB-OPENING-BAL   = FUNCTION NUMVAL(WS-F(3))
           COMPUTE GLB-PERIOD-START  = FUNCTION NUMVAL(WS-F(4))
           MOVE GLB-OPENING-BAL TO GLB-CLOSING-BAL
           MOVE ZERO TO GLB-PERIOD-DEBITS GLB-PERIOD-CREDITS
                        GLB-TODAY-DEBITS GLB-TODAY-CREDITS
                        GLB-LAST-POST-DATE
           IF GLB-DEBIT-NORMAL
               ADD GLB-OPENING-BAL TO WS-TOTAL-DEBIT-NORMAL
           ELSE
               ADD GLB-OPENING-BAL TO WS-TOTAL-CREDIT-NORMAL
           END-IF
           WRITE GL-BALANCE-RECORD
           IF WS-GLB-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDGLBAL WRITE ERROR ' WS-GLB-STATUS
                       ' KEY=' GLB-KEY
           END-IF.
