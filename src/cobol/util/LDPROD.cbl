      *================================================================*
      * PROGRAM  : LDPROD                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE PRODUCT MASTER (PRODMST) FROM A PIPE-     *
      *            DELIMITED EXTRACT (PRODSEED). LINES BEGINNING WITH  *
      *            '#' ARE COMMENTS.                                   *
      * INPUT    : PRODSEED  CODE|NAME|CAT|CUSTTYPE|MINOPEN|MINBAL|FEE|*
      *                      RATETBL|TERM|BASIS|COMPOUND|CAP|REGD|OD|  *
      *                      CHECK|ANALYSIS|PENALTYDAYS|STATUS|EFFDATE *
      * OUTPUT   : PRODMST   (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDPROD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "PRODSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT PRODUCT-FILE ASSIGN TO "PRODMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-CODE
               FILE STATUS IS WS-PROD-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(400).
       FD  PRODUCT-FILE.
       COPY PRODREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-PROD-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(40) OCCURS 19.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT PRODUCT-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-PROD-STATUS NOT = '00'
               MOVE 'LDPROD' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - PRODSEED/PRODMST' TO ABND-MESSAGE
               MOVE WS-PROD-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE PRODUCT-FILE
           DISPLAY 'LDPROD  READ=' WS-READ-COUNT
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
                    WS-F(16) WS-F(17) WS-F(18) WS-F(19)
           END-UNSTRING
           INITIALIZE PRODUCT-RECORD
           MOVE WS-F(1)  TO PROD-CODE
           MOVE WS-F(2)  TO PROD-NAME
           MOVE WS-F(3)  TO PROD-CATEGORY
           MOVE WS-F(4)  TO PROD-CUST-TYPE-ALLOWED
           COMPUTE PROD-MIN-OPEN-AMT = FUNCTION NUMVAL(WS-F(5))
           COMPUTE PROD-MIN-BALANCE  = FUNCTION NUMVAL(WS-F(6))
           COMPUTE PROD-MAINT-FEE    = FUNCTION NUMVAL(WS-F(7))
           MOVE WS-F(8)  TO PROD-RATE-TABLE-ID
           COMPUTE PROD-TERM-MONTHS  = FUNCTION NUMVAL(WS-F(9))
           MOVE WS-F(10) TO PROD-ACCRUAL-BASIS
           MOVE WS-F(11) TO PROD-COMPOUND-FREQ
           MOVE WS-F(12) TO PROD-CAPITALIZE-FREQ
           MOVE WS-F(13) TO PROD-REG-D-FLAG
           MOVE WS-F(14) TO PROD-OD-ALLOWED
           MOVE WS-F(15) TO PROD-CHECK-ALLOWED
           MOVE WS-F(16) TO PROD-ANALYSIS-FLAG
           COMPUTE PROD-EARLY-WD-PENALTY-DAYS =
                   FUNCTION NUMVAL(WS-F(17))
           MOVE WS-F(18) TO PROD-STATUS
           COMPUTE PROD-EFF-DATE     = FUNCTION NUMVAL(WS-F(19))
           WRITE PRODUCT-RECORD
           IF WS-PROD-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDPROD  WRITE ERROR ' WS-PROD-STATUS
                       ' KEY=' PROD-CODE
           END-IF.
