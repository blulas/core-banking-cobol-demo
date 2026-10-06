      *================================================================*
      * PROGRAM  : CTRSCAN                                             *
      * TYPE     : BATCH - EOD STEP 08 (COMPLIANCE)                    *
      * PURPOSE  : CURRENCY TRANSACTION REPORT SCAN. COLLECTS EVERY    *
      *            POSTED CASH ITEM FOR THE BUSINESS DAY FROM TRANPOST,*
      *            GROUPS THEM BY THE PERSON CONDUCTING (THE PRIMARY   *
      *            OWNER OF THE ACCOUNT), CALLS CTRRULE FOR EACH       *
      *            PERSON AND WRITES A CTR RECORD, A CTR1 ALERT AND A  *
      *            REPORT LINE FOR EVERY REPORTABLE PERSON.            *
      * INPUT    : TRANPOST  POSTED TRANSACTIONS (TODAY)               *
      *            ACCTMST   ACCOUNT MASTER (RANDOM)                   *
      *            CUSTMST   CUSTOMER MASTER (RANDOM)                  *
      *            BANKCTL   VIA GETCTL                                *
      * OUTPUT   : CTRFILE   CTR RECORDS FOR BSA E-FILING (CTRREC)     *
      *            ALERTS    CTR1 ALERTS (EXTEND)                      *
      *            CTRRPT    CTR DETERMINATION REPORT                  *
      * RETURN   : 00 OK, 04 ITEMS SKIPPED (ACCOUNT/CUSTOMER NOT FOUND *
      *            OR TABLE FULL), 16 FILE ERROR (ABENDRTN)            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-01 RJT  ORIGINAL - FORM 4789 PRINT                     *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  READ TRANPOST CASH FLAG INSTEAD OF TRAN CODES  *
      * 2002-10-21 BHS  AGGREGATE BY CUSTOMER ACROSS ACCOUNTS          *
      * 2002-11-04 BHS  JOINT ACCOUNTS - CASH ATTRIBUTED TO PRIMARY    *
      *                 OWNER AS PERSON CONDUCTING (BSA OFFICER MEMO   *
      *                 2002-07)                                       *
      * 2012-03-30 DPS  REG-2012-F FINCEN 112 LAYOUT, CTRFILE ADDED    *
      * 2016-02-08 DPS  INC27765 ITEMS NOT POSTED TODAY WERE COUNTED   *
      * 2019-10-07 AKW  MSB FLAG PASSED TO CTRRULE                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTRSCAN.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANPOST-FILE ASSIGN TO "TRANPOST"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT CTR-FILE ASSIGN TO "CTRFILE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CTR-STATUS.
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CTRRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  CTR-FILE.
       COPY CTRREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'CTRSCAN'.
       01  WS-FILE-STATUSES.
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-CTR-STATUS               PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01) VALUE 'N'.
           88  WS-EOF                                VALUE 'Y'.

      *----------------------------------------------------------------*
      * FILING INSTITUTION (FINCEN 112 PART IV)                        *
      *----------------------------------------------------------------*
       01  WS-FILING-INSTITUTION.
           05  WS-FI-NAME                  PIC X(35)
                                           VALUE 'CBS BANK USA NA'.
           05  WS-FI-EIN                   PIC X(09) VALUE '980000417'.
           05  WS-FI-RSSD                  PIC X(10) VALUE '0002182786'.
           05  WS-FI-REGULATOR             PIC X(01) VALUE '3'.

       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(07) VALUE 0.
           05  WS-CASH-COUNT               PIC 9(07) VALUE 0.
           05  WS-SKIP-COUNT               PIC 9(07) VALUE 0.
           05  WS-CUST-EVAL-COUNT          PIC 9(05) VALUE 0.
           05  WS-CTR-COUNT                PIC 9(05) VALUE 0.
           05  WS-EXEMPT-COUNT             PIC 9(05) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(05) VALUE 0.
           05  WS-CTR-SEQ                  PIC 9(06) VALUE 0.
           05  WS-RETURN-CODE              PIC 9(02) VALUE 0.
       01  WS-TOTALS.
           05  WS-TOT-CASH-IN              PIC S9(13)V99 COMP-3
                                           VALUE 0.
           05  WS-TOT-CASH-OUT             PIC S9(13)V99 COMP-3
                                           VALUE 0.

      *----------------------------------------------------------------*
      * CUSTOMER AGGREGATION TABLE                                     *
      *----------------------------------------------------------------*
       01  WS-MAX-CUST                     PIC 9(03) VALUE 300.
       01  WS-MAX-ITEMS                    PIC 9(03) VALUE 100.
       01  WS-CUST-TABLE.
           05  WS-CUST-COUNT               PIC 9(03) COMP VALUE 0.
           05  WS-CUST-ENTRY OCCURS 300 TIMES.
               10  WS-CT-CUST-ID           PIC X(10).
               10  WS-CT-ITEM-COUNT        PIC 9(03) COMP.
               10  WS-CT-ITEM OCCURS 100 TIMES.
                   15  WS-CT-ACCT          PIC X(12).
                   15  WS-CT-DR-CR         PIC X(01).
                   15  WS-CT-AMOUNT        PIC S9(13)V99 COMP-3.
                   15  WS-CT-BRANCH        PIC X(05).

       01  WS-WORK-FIELDS.
           05  WS-CX                       PIC 9(03) COMP.
           05  WS-IX                       PIC 9(03) COMP.
           05  WS-AX                       PIC 9(02) COMP.
           05  WS-FOUND                    PIC X(01).
           05  WS-OWNER-ID                 PIC X(10).
           05  WS-TIME                     PIC 9(08).
           05  WS-CUST-NAME                PIC X(40).

       01  WS-RPT-HEADER-1.
           05  FILLER PIC X(08) VALUE 'CTRSCAN '.
           05  FILLER PIC X(50) VALUE
               'CURRENCY TRANSACTION REPORT DETERMINATION'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  WS-H1-DATE                  PIC 9(08).
           05  FILLER PIC X(51) VALUE SPACES.
       01  WS-RPT-HEADER-2.
           05  FILLER PIC X(40) VALUE
               'CUST-ID    NAME'.
           05  FILLER PIC X(28) VALUE
               '      CASH-IN      CASH-OUT'.
           05  FILLER PIC X(18) VALUE
               '  ITEMS ACCTS EXM '.
           05  FILLER PIC X(46) VALUE
               'DECISION                                 DUE'.
       01  WS-RPT-DETAIL.
           05  WS-D-CUST-ID                PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NAME                   PIC X(28).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-CASH-IN                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-CASH-OUT               PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-ITEMS                  PIC ZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-ACCTS                  PIC Z9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-EXEMPT                 PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-DECISION               PIC X(40).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-DUE                    PIC X(08).
       01  WS-RPT-NOTE.
           05  FILLER                      PIC X(13) VALUE
               '           **'.
           05  WS-N-TEXT                   PIC X(119).
       01  WS-RPT-TOTAL.
           05  WS-T-LABEL                  PIC X(40).
           05  WS-T-VALUE                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                      PIC X(74) VALUE SPACES.
       01  WS-RPT-COUNT.
           05  WS-C-LABEL                  PIC X(40).
           05  WS-C-VALUE                  PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(77) VALUE SPACES.
       01  WS-COUNT-ED                     PIC ZZZ,ZZ9.

       COPY BANKCTL.
       COPY CTRPARM.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 2000-COLLECT-CASH-ITEMS
           PERFORM 3000-EVALUATE-CUSTOMERS
           PERFORM 9000-TERMINATE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT TRANPOST-FILE
           IF WS-TRAN-STATUS NOT = '00'
               MOVE 'TRANPOST' TO ABND-FILE-NAME
               MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT CTR-FILE
           IF WS-CTR-STATUS NOT = '00'
               MOVE 'CTRFILE' TO ABND-FILE-NAME
               MOVE WS-CTR-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN EXTEND ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'CTRRPT' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           MOVE CTL-BUSINESS-DATE TO WS-H1-DATE
           WRITE REPORT-LINE FROM WS-RPT-HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-RPT-HEADER-2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE.

      *----------------------------------------------------------------*
      * READ TRANPOST - KEEP POSTED CASH ITEMS FOR TODAY ONLY.         *
      *----------------------------------------------------------------*
       2000-COLLECT-CASH-ITEMS.
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ-COUNT
                       IF TRAN-IS-CASH
                          AND TRAN-POSTED
                          AND TRAN-POST-DATE = CTL-BUSINESS-DATE
                           PERFORM 2100-ADD-CASH-ITEM
                       END-IF
               END-READ
               IF WS-TRAN-STATUS NOT = '00' AND NOT = '10'
                   MOVE 'TRANPOST' TO ABND-FILE-NAME
                   MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
           END-PERFORM.

       2100-ADD-CASH-ITEM.
           ADD 1 TO WS-CASH-COUNT
           MOVE TRAN-ACCT-NUMBER TO ACCT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY 'CTRSCAN  ACCT NOT FOUND ' TRAN-ACCT-NUMBER
                           ' TRAN ' TRAN-ID
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ
      *    PERSON CONDUCTING = PRIMARY OWNER. JOINT OWNER IS NOT
      *    AGGREGATED (BSA OFFICER MEMO 2002-07).
           MOVE ACCT-PRIMARY-CUST-ID TO WS-OWNER-ID
           PERFORM 2200-FIND-CUSTOMER
           IF WS-FOUND = 'N'
               EXIT PARAGRAPH
           END-IF
           IF WS-CT-ITEM-COUNT(WS-CX) >= WS-MAX-ITEMS
               DISPLAY 'CTRSCAN  ITEM TABLE FULL FOR ' WS-OWNER-ID
               ADD 1 TO WS-SKIP-COUNT
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-CT-ITEM-COUNT(WS-CX)
           MOVE WS-CT-ITEM-COUNT(WS-CX) TO WS-IX
           MOVE TRAN-ACCT-NUMBER TO WS-CT-ACCT(WS-CX, WS-IX)
           MOVE TRAN-DR-CR       TO WS-CT-DR-CR(WS-CX, WS-IX)
           MOVE TRAN-AMOUNT      TO WS-CT-AMOUNT(WS-CX, WS-IX)
           IF TRAN-COST-CENTER = SPACES
               MOVE TRAN-CHANNEL TO WS-CT-BRANCH(WS-CX, WS-IX)
           ELSE
               MOVE TRAN-COST-CENTER TO WS-CT-BRANCH(WS-CX, WS-IX)
           END-IF.

       2200-FIND-CUSTOMER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CUST-COUNT OR WS-FOUND = 'Y'
               IF WS-CT-CUST-ID(WS-CX) = WS-OWNER-ID
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'Y'
               SUBTRACT 1 FROM WS-CX
           ELSE
               IF WS-CUST-COUNT >= WS-MAX-CUST
                   DISPLAY 'CTRSCAN  CUSTOMER TABLE FULL - SKIPPED '
                           WS-OWNER-ID
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
               ELSE
                   ADD 1 TO WS-CUST-COUNT
                   MOVE WS-CUST-COUNT TO WS-CX
                   MOVE WS-OWNER-ID TO WS-CT-CUST-ID(WS-CX)
                   MOVE 0 TO WS-CT-ITEM-COUNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-EVALUATE-CUSTOMERS.
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CUST-COUNT
               PERFORM 3100-EVALUATE-ONE
           END-PERFORM.

       3100-EVALUATE-ONE.
           MOVE WS-CT-CUST-ID(WS-CX) TO CUST-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY 'CTRSCAN  CUSTOMER NOT FOUND ' CUST-ID
                   ADD 1 TO WS-SKIP-COUNT
                   MOVE 04 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ
           ADD 1 TO WS-CUST-EVAL-COUNT
           INITIALIZE CTR-RULE-PARMS
           MOVE CTL-BUSINESS-DATE    TO CTP-BUSINESS-DATE
           MOVE CUST-ID              TO CTP-CUST-ID
           MOVE CUST-TYPE            TO CTP-CUST-TYPE
           MOVE CUST-CTR-EXEMPT-FLAG TO CTP-CTR-EXEMPT-FLAG
           MOVE CUST-MSB-FLAG        TO CTP-MSB-FLAG
           MOVE WS-CT-ITEM-COUNT(WS-CX) TO CTP-ITEM-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CT-ITEM-COUNT(WS-CX)
               MOVE WS-CT-ACCT(WS-CX, WS-IX)   TO CTP-ITEM-ACCT(WS-IX)
               MOVE WS-CT-DR-CR(WS-CX, WS-IX)  TO CTP-ITEM-DR-CR(WS-IX)
               MOVE WS-CT-AMOUNT(WS-CX, WS-IX) TO CTP-ITEM-AMOUNT(WS-IX)
               MOVE WS-CT-BRANCH(WS-CX, WS-IX) TO CTP-ITEM-BRANCH(WS-IX)
           END-PERFORM
           CALL 'CTRRULE' USING CTR-RULE-PARMS
           IF CTP-RETURN-CODE > 04
               DISPLAY 'CTRSCAN  CTRRULE RC ' CTP-RETURN-CODE
                       ' CUST ' CUST-ID ' ' CTP-DECISION-TEXT
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           ADD CTP-CASH-IN-TOTAL  TO WS-TOT-CASH-IN
           ADD CTP-CASH-OUT-TOTAL TO WS-TOT-CASH-OUT
           IF CTP-CTR-IS-REQUIRED
               PERFORM 3200-WRITE-CTR
               PERFORM 3300-WRITE-ALERT
           END-IF
           IF CTP-EXEMPT AND CTP-OVER-THRESHOLD = 'Y'
               ADD 1 TO WS-EXEMPT-COUNT
           END-IF
           PERFORM 3400-WRITE-REPORT-LINE.

      *----------------------------------------------------------------*
       3200-WRITE-CTR.
           INITIALIZE CTR-RECORD
           ADD 1 TO WS-CTR-SEQ WS-CTR-COUNT
           MOVE CTL-BUSINESS-DATE   TO CTR-REPORT-DATE CTR-TRAN-DATE
           MOVE WS-CTR-SEQ          TO CTR-REPORT-SEQ
           SET CTR-INITIAL-REPORT   TO TRUE
           MOVE CTL-BANK-ID         TO CTR-FI-BANK-ID
           MOVE WS-FI-NAME          TO CTR-FI-NAME
           MOVE WS-FI-EIN           TO CTR-FI-EIN
           MOVE WS-FI-RSSD          TO CTR-FI-RSSD
           MOVE WS-FI-REGULATOR     TO CTR-FI-REGULATOR
           MOVE CTP-FILING-DUE-DATE TO CTR-FILING-DUE-DATE
           MOVE CUST-ID             TO CTR-CUST-ID
           IF CUST-INDIVIDUAL
               SET CTR-PERSON-INDIVIDUAL TO TRUE
           ELSE
               SET CTR-PERSON-ENTITY TO TRUE
           END-IF
           MOVE CUST-LAST-NAME      TO CTR-LAST-NAME
           MOVE CUST-FIRST-NAME     TO CTR-FIRST-NAME
           MOVE CUST-MIDDLE-INIT    TO CTR-MIDDLE-INIT
           MOVE CUST-TAX-ID         TO CTR-TAX-ID
           MOVE CUST-TAX-ID-TYPE    TO CTR-TAX-ID-TYPE
           MOVE CUST-BIRTH-DATE     TO CTR-BIRTH-DATE
           MOVE CUST-ADDR-LINE1     TO CTR-ADDR-LINE1
           MOVE CUST-CITY           TO CTR-CITY
           MOVE CUST-STATE          TO CTR-STATE
           MOVE CUST-ZIP            TO CTR-ZIP
           MOVE CUST-COUNTRY        TO CTR-COUNTRY
           MOVE CUST-INDUSTRY-CODE  TO CTR-OCCUPATION
           MOVE CTP-CASH-IN-TOTAL   TO CTR-CASH-IN-TOTAL
           MOVE CTP-CASH-OUT-TOTAL  TO CTR-CASH-OUT-TOTAL
           MOVE CTP-CASH-IN-REPORTABLE  TO CTR-CASH-IN-REPORTABLE
           MOVE CTP-CASH-OUT-REPORTABLE TO CTR-CASH-OUT-REPORTABLE
           MOVE CTP-ITEM-COUNT      TO CTR-TRAN-COUNT
           MOVE CTP-AGGREGATED-FLAG TO CTR-AGGREGATED-FLAG
           MOVE CTP-MULTI-BRANCH-FLAG TO CTR-MULTI-BRANCH-FLAG
           MOVE 'N'                 TO CTR-ARMORED-CAR-FLAG
           MOVE CTP-EXEMPT-STATUS   TO CTR-EXEMPT-STATUS
           MOVE CUST-MSB-FLAG       TO CTR-MSB-FLAG
           PERFORM 3210-BUILD-ACCOUNT-LIST
           SET CTR-PENDING-FILING   TO TRUE
           MOVE WS-PROGRAM-NAME     TO CTR-SOURCE-PGM
           WRITE CTR-RECORD
           IF WS-CTR-STATUS NOT = '00'
               MOVE 'CTRFILE' TO ABND-FILE-NAME
               MOVE WS-CTR-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

      *    FINCEN 112 ALLOWS SEVERAL ACCOUNTS PER PERSON - KEEP FIRST 6
       3210-BUILD-ACCOUNT-LIST.
           MOVE 0 TO CTR-ACCOUNT-COUNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-CT-ITEM-COUNT(WS-CX)
               MOVE 'N' TO WS-FOUND
               PERFORM VARYING WS-AX FROM 1 BY 1
                       UNTIL WS-AX > CTR-ACCOUNT-COUNT
                   IF CTR-ACCT-NUMBER(WS-AX) = WS-CT-ACCT(WS-CX, WS-IX)
                       MOVE 'Y' TO WS-FOUND
                       PERFORM 3220-ADD-TO-ACCOUNT
                   END-IF
               END-PERFORM
               IF WS-FOUND = 'N' AND CTR-ACCOUNT-COUNT < 6
                   ADD 1 TO CTR-ACCOUNT-COUNT
                   MOVE CTR-ACCOUNT-COUNT TO WS-AX
                   MOVE WS-CT-ACCT(WS-CX, WS-IX)
                     TO CTR-ACCT-NUMBER(WS-AX)
                   MOVE ZERO TO CTR-ACCT-CASH-IN(WS-AX)
                                CTR-ACCT-CASH-OUT(WS-AX)
                   PERFORM 3220-ADD-TO-ACCOUNT
               END-IF
           END-PERFORM.

       3220-ADD-TO-ACCOUNT.
           IF WS-CT-DR-CR(WS-CX, WS-IX) = 'C'
               ADD WS-CT-AMOUNT(WS-CX, WS-IX)
                 TO CTR-ACCT-CASH-IN(WS-AX)
           ELSE
               ADD WS-CT-AMOUNT(WS-CX, WS-IX)
                 TO CTR-ACCT-CASH-OUT(WS-AX)
           END-IF.

      *----------------------------------------------------------------*
       3300-WRITE-ALERT.
           INITIALIZE ALERT-RECORD
           ACCEPT WS-TIME FROM TIME
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE WS-TIME(1:6)      TO ALRT-TIME
           MOVE 'CTR1'            TO ALRT-TYPE
           IF CTP-MULTI-BRANCH-FLAG = 'Y' OR CUST-IS-MSB
               SET ALRT-HIGH TO TRUE
           ELSE
               SET ALRT-MEDIUM TO TRUE
           END-IF
           MOVE CUST-ID           TO ALRT-CUST-ID
           MOVE CTR-ACCT-NUMBER(1) TO ALRT-ACCT-NUMBER
           IF CTP-CASH-IN-TOTAL > CTP-CASH-OUT-TOTAL
               MOVE CTP-CASH-IN-TOTAL  TO ALRT-AMOUNT
           ELSE
               MOVE CTP-CASH-OUT-TOTAL TO ALRT-AMOUNT
           END-IF
           MOVE 'CTRDAILY'        TO ALRT-RULE-ID
           STRING 'CTR REQUIRED - ' DELIMITED BY SIZE
                  CTP-DECISION-TEXT DELIMITED BY '  '
                  ' - FILE BY ' DELIMITED BY SIZE
                  CTP-FILING-DUE-DATE DELIMITED BY SIZE
             INTO ALRT-DESCRIPTION
           END-STRING
           SET ALRT-OPEN TO TRUE
           MOVE WS-PROGRAM-NAME   TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ALERT-COUNT.

      *----------------------------------------------------------------*
       3400-WRITE-REPORT-LINE.
           MOVE SPACES TO WS-CUST-NAME
           IF CUST-INDIVIDUAL
               STRING CUST-FIRST-NAME DELIMITED BY '  '
                      ' ' DELIMITED BY SIZE
                      CUST-LAST-NAME DELIMITED BY '  '
                 INTO WS-CUST-NAME
               END-STRING
           ELSE
               MOVE CUST-LAST-NAME TO WS-CUST-NAME
           END-IF
           MOVE CUST-ID            TO WS-D-CUST-ID
           MOVE WS-CUST-NAME       TO WS-D-NAME
           MOVE CTP-CASH-IN-TOTAL  TO WS-D-CASH-IN
           MOVE CTP-CASH-OUT-TOTAL TO WS-D-CASH-OUT
           MOVE CTP-ITEM-COUNT     TO WS-D-ITEMS
           MOVE CTP-ACCOUNT-COUNT  TO WS-D-ACCTS
           MOVE CTP-EXEMPT-STATUS  TO WS-D-EXEMPT
           MOVE CTP-DECISION-TEXT  TO WS-D-DECISION
           IF CTP-CTR-IS-REQUIRED
               MOVE CTP-FILING-DUE-DATE TO WS-D-DUE
           ELSE
               MOVE SPACES TO WS-D-DUE
           END-IF
           WRITE REPORT-LINE FROM WS-RPT-DETAIL
           IF CTP-EXEMPT-REVIEW-FLAG = 'Y'
               MOVE CTP-EXEMPT-REVIEW-NOTE TO WS-N-TEXT
               WRITE REPORT-LINE FROM WS-RPT-NOTE
           END-IF
           IF CTP-AGGREGATED-FLAG = 'Y'
               MOVE 'MULTIPLE TRANSACTIONS AGGREGATED (31 CFR 1010.313)'
                 TO WS-N-TEXT
               WRITE REPORT-LINE FROM WS-RPT-NOTE
           END-IF.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'TRANPOST RECORDS READ' TO WS-C-LABEL
           MOVE WS-READ-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CASH ITEMS POSTED TODAY' TO WS-C-LABEL
           MOVE WS-CASH-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CUSTOMERS EVALUATED' TO WS-C-LABEL
           MOVE WS-CUST-EVAL-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CTRS GENERATED' TO WS-C-LABEL
           MOVE WS-CTR-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'EXEMPT PERSONS OVER THRESHOLD' TO WS-C-LABEL
           MOVE WS-EXEMPT-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'ITEMS SKIPPED' TO WS-C-LABEL
           MOVE WS-SKIP-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'TOTAL CASH IN' TO WS-T-LABEL
           MOVE WS-TOT-CASH-IN TO WS-T-VALUE
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           MOVE 'TOTAL CASH OUT' TO WS-T-LABEL
           MOVE WS-TOT-CASH-OUT TO WS-T-VALUE
           WRITE REPORT-LINE FROM WS-RPT-TOTAL
           CLOSE TRANPOST-FILE ACCOUNT-FILE CUSTOMER-FILE
                 CTR-FILE ALERT-FILE REPORT-FILE
           DISPLAY 'CTRSCAN  READ=' WS-READ-COUNT
                   ' CASH=' WS-CASH-COUNT
                   ' CUSTOMERS=' WS-CUST-EVAL-COUNT
                   ' CTRS=' WS-CTR-COUNT
                   ' EXEMPT=' WS-EXEMPT-COUNT
                   ' SKIPPED=' WS-SKIP-COUNT
                   ' RC=' WS-RETURN-CODE.

       9900-ABEND.
           MOVE WS-PROGRAM-NAME TO ABND-PROGRAM
           MOVE 'FILE I/O' TO ABND-PARAGRAPH
           MOVE 'UNEXPECTED FILE STATUS' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
