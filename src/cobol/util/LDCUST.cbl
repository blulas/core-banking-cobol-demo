      *================================================================*
      * PROGRAM  : LDCUST                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE CUSTOMER MASTER (CUSTMST) FROM A PIPE-    *
      *            DELIMITED EXTRACT (CUSTSEED).                       *
      * INPUT    : CUSTSEED  ID|TYPE|LAST|FIRST|MI|TAXID|TAXIDTYPE|    *
      *              BIRTH|ADDR1|CITY|STATE|ZIP|COUNTRY|PHONE|EMAIL|   *
      *              SINCE|STATUS|KYC|KYCTIER|KYCREVIEW|RISK|RISKSCORE|*
      *              PEP|INDUSTRY|INCOME|SEGMENT|RELID|PARENT|EMPLOYEE|*
      *              OFAC|OFACDATE|CTREXEMPT|MSB                       *
      * OUTPUT   : CUSTMST   (KSDS)                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDCUST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "CUSTSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(600).
       FD  CUSTOMER-FILE.
       COPY CUSTREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-CUST-STATUS                  PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(50) OCCURS 33.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT SEED-FILE
           OPEN OUTPUT CUSTOMER-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-CUST-STATUS NOT = '00'
               MOVE 'LDCUST' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'OPEN FAILED - CUSTSEED/CUSTMST' TO ABND-MESSAGE
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE CUSTOMER-FILE
           DISPLAY 'LDCUST  READ=' WS-READ-COUNT
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
                    WS-F(16) WS-F(17) WS-F(18) WS-F(19) WS-F(20)
                    WS-F(21) WS-F(22) WS-F(23) WS-F(24) WS-F(25)
                    WS-F(26) WS-F(27) WS-F(28) WS-F(29) WS-F(30)
                    WS-F(31) WS-F(32) WS-F(33)
           END-UNSTRING
           INITIALIZE CUSTOMER-RECORD
           MOVE WS-F(1)  TO CUST-ID
           MOVE WS-F(2)  TO CUST-TYPE
           MOVE WS-F(3)  TO CUST-LAST-NAME
           MOVE WS-F(4)  TO CUST-FIRST-NAME
           MOVE WS-F(5)  TO CUST-MIDDLE-INIT
           MOVE WS-F(6)  TO CUST-TAX-ID
           MOVE WS-F(7)  TO CUST-TAX-ID-TYPE
           COMPUTE CUST-BIRTH-DATE = FUNCTION NUMVAL(WS-F(8))
           MOVE WS-F(9)  TO CUST-ADDR-LINE1
           MOVE WS-F(10) TO CUST-CITY
           MOVE WS-F(11) TO CUST-STATE
           MOVE WS-F(12) TO CUST-ZIP
           MOVE WS-F(13) TO CUST-COUNTRY
           MOVE WS-F(14) TO CUST-PHONE
           MOVE WS-F(15) TO CUST-EMAIL
           COMPUTE CUST-SINCE-DATE = FUNCTION NUMVAL(WS-F(16))
           MOVE WS-F(17) TO CUST-STATUS
           MOVE WS-F(18) TO CUST-KYC-STATUS
           MOVE WS-F(19) TO CUST-KYC-TIER
           COMPUTE CUST-KYC-REVIEW-DATE = FUNCTION NUMVAL(WS-F(20))
           MOVE WS-F(21) TO CUST-RISK-RATING
           COMPUTE CUST-RISK-SCORE = FUNCTION NUMVAL(WS-F(22))
           MOVE WS-F(23) TO CUST-PEP-FLAG
           MOVE WS-F(24) TO CUST-INDUSTRY-CODE
           COMPUTE CUST-ANNUAL-INCOME = FUNCTION NUMVAL(WS-F(25))
           MOVE WS-F(26) TO CUST-SEGMENT
           MOVE WS-F(27) TO CUST-RELATIONSHIP-ID
           MOVE WS-F(28) TO CUST-PARENT-CUST-ID
           MOVE WS-F(29) TO CUST-EMPLOYEE-FLAG
           MOVE WS-F(30) TO CUST-OFAC-STATUS
           COMPUTE CUST-OFAC-LAST-SCREEN = FUNCTION NUMVAL(WS-F(31))
           MOVE WS-F(32) TO CUST-CTR-EXEMPT-FLAG
           MOVE WS-F(33) TO CUST-MSB-FLAG
           MOVE ZERO     TO CUST-TOTAL-REL-BALANCE
           MOVE CUST-SINCE-DATE TO CUST-LAST-MAINT-DATE
           MOVE 'CONVERT'       TO CUST-LAST-MAINT-USER
           WRITE CUSTOMER-RECORD
           IF WS-CUST-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDCUST  WRITE ERROR ' WS-CUST-STATUS
                       ' KEY=' CUST-ID
           END-IF.
