      *================================================================*
      * PROGRAM  : LDPAYM                                              *
      * TYPE     : BATCH UTILITY - DATA CONVERSION                     *
      * PURPOSE  : LOADS THE PAYMENT INSTRUCTION MASTER (PAYMST) FROM  *
      *            A PIPE-DELIMITED EXTRACT (PAYSEED).                 *
      * INPUT    : PAYSEED  ID|TYPE|DEBITACCT|CUST|BENENAME|BENEACCT|  *
      *              BENEACCTTYPE|BENEROUTING|BENEBIC|BENEBANK|        *
      *              BENECTRY|BENEADDR1|BENEADDR2|AMOUNT|CCY|REQDATE|  *
      *              VALUEDATE|URGENT|FREQ|NEXTDATE|ENDDATE|STATUS|    *
      *              REJREASON|MAKER|CHECKER|APPRDATE|APPRTIME|SEC|    *
      *              BATCHREF|OFAC|OFACSCORE|COMPL|RECONF|PURPOSE|     *
      *              REMIT|CHANNEL|CREATEDDATE|CREATEDTIME             *
      * OUTPUT   : PAYMST   (KSDS, KEY PAY-ID, AIX PAY-DEBIT-ACCT)     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2015-10-02 DPS  ORIGINAL - CORE CONVERSION PROJECT             *
      * 2019-06-03 AKW  BATCH REFERENCE                                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LDPAYM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEED-FILE ASSIGN TO "PAYSEED"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SEED-STATUS.
           SELECT PAYMENT-FILE ASSIGN TO "PAYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PAY-ID
               ALTERNATE RECORD KEY IS PAY-DEBIT-ACCT
                   WITH DUPLICATES
               FILE STATUS IS WS-PAY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEED-FILE.
       01  SEED-LINE                       PIC X(800).
       FD  PAYMENT-FILE.
       COPY PAYREC.

       WORKING-STORAGE SECTION.
       01  WS-SEED-STATUS                  PIC X(02).
       01  WS-PAY-STATUS                   PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-LOAD-COUNT               PIC 9(07) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE 0.
       01  WS-FIELDS.
           05  WS-F                        PIC X(80) OCCURS 38.
       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT SEED-FILE
           OPEN OUTPUT PAYMENT-FILE
           IF WS-SEED-STATUS NOT = '00' OR WS-PAY-STATUS NOT = '00'
               MOVE 'LDPAYM' TO ABND-PROGRAM
               MOVE '0000-MAIN' TO ABND-PARAGRAPH
               MOVE 'PAYMST' TO ABND-FILE-NAME
               MOVE WS-PAY-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN FAILED - PAYSEED/PAYMST' TO ABND-MESSAGE
               CALL 'ABENDRTN' USING ABEND-PARMS
           END-IF
           PERFORM UNTIL WS-EOF
               READ SEED-FILE
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 1000-PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE SEED-FILE PAYMENT-FILE
           DISPLAY 'LDPAYM  READ=' WS-READ-COUNT
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
                    WS-F(31) WS-F(32) WS-F(33) WS-F(34) WS-F(35)
                    WS-F(36) WS-F(37) WS-F(38)
           END-UNSTRING
           INITIALIZE PAYMENT-RECORD
           MOVE WS-F(1)  TO PAY-ID
           MOVE WS-F(2)  TO PAY-TYPE
           MOVE WS-F(3)  TO PAY-DEBIT-ACCT
           MOVE WS-F(4)  TO PAY-CUST-ID
           MOVE WS-F(5)  TO PAY-BENE-NAME
           MOVE WS-F(6)  TO PAY-BENE-ACCT
           MOVE WS-F(7)  TO PAY-BENE-ACCT-TYPE
           MOVE WS-F(8)  TO PAY-BENE-ROUTING
           MOVE WS-F(9)  TO PAY-BENE-BIC
           MOVE WS-F(10) TO PAY-BENE-BANK-NAME
           MOVE WS-F(11) TO PAY-BENE-COUNTRY
           MOVE WS-F(12) TO PAY-BENE-ADDR1
           MOVE WS-F(13) TO PAY-BENE-ADDR2
           COMPUTE PAY-AMOUNT          = FUNCTION NUMVAL(WS-F(14))
           MOVE WS-F(15) TO PAY-CURRENCY
           COMPUTE PAY-REQ-VALUE-DATE  = FUNCTION NUMVAL(WS-F(16))
           COMPUTE PAY-VALUE-DATE      = FUNCTION NUMVAL(WS-F(17))
           MOVE WS-F(18) TO PAY-URGENT-FLAG
           MOVE WS-F(19) TO PAY-FREQUENCY
           COMPUTE PAY-NEXT-DATE       = FUNCTION NUMVAL(WS-F(20))
           COMPUTE PAY-END-DATE        = FUNCTION NUMVAL(WS-F(21))
           MOVE WS-F(22) TO PAY-STATUS
           MOVE WS-F(23) TO PAY-REJECT-REASON
           MOVE WS-F(24) TO PAY-MAKER-USER
           MOVE WS-F(25) TO PAY-CHECKER-USER
           COMPUTE PAY-APPROVAL-DATE   = FUNCTION NUMVAL(WS-F(26))
           COMPUTE PAY-APPROVAL-TIME   = FUNCTION NUMVAL(WS-F(27))
           MOVE WS-F(28) TO PAY-SEC-CODE
           MOVE WS-F(29) TO PAY-BATCH-REF
           MOVE WS-F(30) TO PAY-OFAC-RESULT
           COMPUTE PAY-OFAC-SCORE      = FUNCTION NUMVAL(WS-F(31))
           MOVE WS-F(32) TO PAY-COMPLIANCE-FLAG
           MOVE WS-F(33) TO PAY-RECONFIRM-FLAG
           MOVE WS-F(34) TO PAY-PURPOSE-CODE
           MOVE WS-F(35) TO PAY-REMIT-INFO
           MOVE WS-F(36) TO PAY-CHANNEL
           COMPUTE PAY-CREATED-DATE    = FUNCTION NUMVAL(WS-F(37))
           COMPUTE PAY-CREATED-TIME    = FUNCTION NUMVAL(WS-F(38))
           IF PAY-CURRENCY = SPACES
               MOVE 'USD' TO PAY-CURRENCY
           END-IF
           MOVE PAY-CREATED-DATE TO PAY-OFAC-DATE
           MOVE PAY-CREATED-DATE TO PAY-LAST-MAINT-DATE
           MOVE 'CONVERT'        TO PAY-LAST-MAINT-USER
           WRITE PAYMENT-RECORD
           IF WS-PAY-STATUS = '00'
               ADD 1 TO WS-LOAD-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY 'LDPAYM  WRITE ERROR ' WS-PAY-STATUS
                       ' KEY=' PAY-ID
           END-IF.
