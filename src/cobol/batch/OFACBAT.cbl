      *================================================================*
      * PROGRAM  : OFACBAT                                             *
      * TYPE     : BATCH - EOD STEP 20 (COMPLIANCE)                    *
      * PURPOSE  : OFAC BATCH RE-SCREENING. AFTER EVERY SDN LIST       *
      *            UPDATE OFAC EXPECTS THE WHOLE CUSTOMER BASE TO BE   *
      *            RE-SCREENED. THIS STEP SCREENS EVERY CUSTMST NAME   *
      *            AND THE COUNTERPARTY OF EVERY WIRE POSTED TODAY     *
      *            AGAINST THE SDN LIST BY CALLING OFACSCRN, UPDATES   *
      *            CUST-OFAC-STATUS / CUST-OFAC-LAST-SCREEN AND RAISES *
      *            AN OFAC ALERT FOR EVERY POTENTIAL OR CONFIRMED HIT. *
      *              RESULT H (>= 90) OR P (75-89) -> STATUS H, ALERT  *
      *              STATUS R (CONFIRMED BY OFACREV) IS NEVER CHANGED  *
      *              STATUS H STAYS H UNTIL DISPOSITIONED ONLINE       *
      * INPUT    : SDNLIST   SANCTIONS LIST (PIPE DELIMITED, SDNREC)   *
      *            CUSTMST   CUSTOMER MASTER (SEQUENTIAL I-O)          *
      *            TRANPOST  POSTED TODAY (WIRES ONLY)                 *
      *            ACCTMST   ACCOUNT MASTER (RANDOM)                   *
      * OUTPUT   : CUSTMST   OFAC STATUS / LAST SCREEN DATE            *
      *            ALERTS    OFAC ALERTS (EXTEND)                      *
      *            OFACRPT   RE-SCREENING REPORT                       *
      * RETURN   : 00 OK, 04 SDN LIST EMPTY OR SCREENING ERRORS,       *
      *            16 FILE ERROR                                       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2002-03-15 BHS  ORIGINAL - MONTHLY CUSTOMER RE-SCREEN          *
      * 2005-02-28 BHS  WIRE COUNTERPARTY RE-SCREEN ADDED              *
      * 2011-01-24 DPS  DAILY RUN, IN-MEMORY SDN TABLE (SDNTBL)        *
      * 2017-06-12 AKW  INC33410 DO NOT OVERWRITE CONFIRMED (R) STATUS *
      * 2019-11-04 AKW  MV-0193 POTENTIAL MATCHES ALERTED AT MEDIUM    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OFACBAT.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL SDN-FILE ASSIGN TO "SDNLIST"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SDN-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
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
           SELECT OPTIONAL ALERT-FILE ASSIGN TO "ALERTS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ALRT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "OFACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SDN-FILE.
       01  SDN-LINE                        PIC X(200).
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  TRANPOST-FILE.
       COPY TRANREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  ALERT-FILE.
       COPY ALERTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME                 PIC X(08) VALUE 'OFACBAT'.
       01  WS-FILE-STATUSES.
           05  WS-SDN-STATUS               PIC X(02).
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-TRAN-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-ALRT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).
       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-SDN-LOADED               PIC 9(05) VALUE 0.
           05  WS-CUST-SCREENED            PIC 9(07) VALUE 0.
           05  WS-CUST-HITS                PIC 9(05) VALUE 0.
           05  WS-CUST-POTENTIAL           PIC 9(05) VALUE 0.
           05  WS-CUST-CONFIRMED-SKIP      PIC 9(05) VALUE 0.
           05  WS-WIRES-SCREENED           PIC 9(07) VALUE 0.
           05  WS-WIRE-HITS                PIC 9(05) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(05) VALUE 0.
           05  WS-ALERT-COUNT              PIC 9(05) VALUE 0.
           05  WS-RETURN-CODE              PIC 9(02) VALUE 0.

       01  WS-SDN-FIELDS.
           05  WS-SF-ENT-NUM               PIC X(10).
           05  WS-SF-TYPE                  PIC X(01).
           05  WS-SF-NAME                  PIC X(60).
           05  WS-SF-COUNTRY               PIC X(02).
           05  WS-SF-PROGRAM               PIC X(10).
           05  WS-SF-LIST-DATE             PIC X(08).
           05  WS-SF-REMARKS               PIC X(40).

       01  WS-WORK-FIELDS.
           05  WS-TIME                     PIC 9(08).
           05  WS-SCREEN-NAME              PIC X(60).
           05  WS-OLD-OFAC-STATUS          PIC X(01).
           05  WS-SUBJECT                  PIC X(04).
           05  WS-SCORE-ED                 PIC ZZ9.
           05  WS-ENT-ED                   PIC 9(06).

       01  WS-RPT-HEADER-1.
           05  FILLER PIC X(09) VALUE 'OFACBAT  '.
           05  FILLER PIC X(49) VALUE
               'OFAC SDN RE-SCREENING - CUSTOMERS AND WIRES'.
           05  FILLER PIC X(15) VALUE 'BUSINESS DATE '.
           05  WS-H1-DATE                  PIC 9(08).
           05  FILLER PIC X(18) VALUE '  SDN ENTRIES '.
           05  WS-H1-SDN                   PIC ZZ,ZZ9.
       01  WS-RPT-HEADER-2.
           05  FILLER PIC X(48) VALUE
               'TYPE ID / ACCOUNT  SCREENED NAME'.
           05  FILLER PIC X(44) VALUE
               'RES SCR RULE OLD NEW SDN-ENT MATCHED NAME'.
           05  FILLER PIC X(40) VALUE SPACES.
       01  WS-RPT-DETAIL.
           05  WS-D-SUBJECT                PIC X(04).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-ID                     PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-NAME                   PIC X(30).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-RESULT                 PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-SCORE                  PIC ZZ9.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-RULE                   PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-OLD                    PIC X(01).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-NEW                    PIC X(01).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-ENT                    PIC 9(06).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-MATCH                  PIC X(40).
           05  FILLER                      PIC X(15) VALUE SPACES.
       01  WS-RPT-COUNT.
           05  WS-C-LABEL                  PIC X(40).
           05  WS-C-VALUE                  PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                      PIC X(81) VALUE SPACES.

       COPY BANKCTL.
       COPY OFSPARM.
       COPY SDNTBL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISE
           PERFORM 1500-LOAD-SDN-TABLE
           IF WS-SDN-LOADED = 0
               DISPLAY 'OFACBAT  SDN LIST EMPTY OR MISSING - '
                       'RE-SCREENING NOT PERFORMED'
               MOVE 04 TO WS-RETURN-CODE
           ELSE
               PERFORM 2000-SCREEN-CUSTOMERS
               PERFORM 3000-SCREEN-WIRES
           END-IF
           PERFORM 9000-TERMINATE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALISE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           OPEN INPUT SDN-FILE
           IF WS-SDN-STATUS NOT = '00' AND WS-SDN-STATUS NOT = '05'
               MOVE 'SDNLIST' TO ABND-FILE-NAME
               MOVE WS-SDN-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN I-O CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
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
           OPEN EXTEND ALERT-FILE
           IF WS-ALRT-STATUS NOT = '00' AND WS-ALRT-STATUS NOT = '05'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-RPT-STATUS NOT = '00'
               MOVE 'OFACRPT' TO ABND-FILE-NAME
               MOVE WS-RPT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

      *----------------------------------------------------------------*
      * LOAD THE SDN TABLE ONCE (SEE SDNTBL). OFACSCRN NORMALISES IT   *
      * ON ITS FIRST CALL.                                             *
      *----------------------------------------------------------------*
       1500-LOAD-SDN-TABLE.
           INITIALIZE SDN-TABLE
           MOVE 500 TO SDNT-MAX-ENTRIES
           MOVE 'N' TO SDNT-PREPARED-FLAG
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ SDN-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 1510-LOAD-ENTRY
               END-READ
           END-PERFORM
           MOVE WS-SDN-LOADED TO WS-H1-SDN
           MOVE CTL-BUSINESS-DATE TO WS-H1-DATE
           WRITE REPORT-LINE FROM WS-RPT-HEADER-1
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM WS-RPT-HEADER-2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE.

       1510-LOAD-ENTRY.
           IF SDN-LINE(1:1) = '#' OR SDN-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           IF SDNT-COUNT >= SDNT-MAX-ENTRIES
               DISPLAY 'OFACBAT  SDN TABLE FULL - ENTRY IGNORED'
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-SDN-FIELDS
           UNSTRING SDN-LINE DELIMITED BY '|'
               INTO WS-SF-ENT-NUM WS-SF-TYPE WS-SF-NAME WS-SF-COUNTRY
                    WS-SF-PROGRAM WS-SF-LIST-DATE WS-SF-REMARKS
           END-UNSTRING
           ADD 1 TO SDNT-COUNT WS-SDN-LOADED
           COMPUTE SDNT-ENT-NUM(SDNT-COUNT) =
                   FUNCTION NUMVAL(WS-SF-ENT-NUM)
           MOVE WS-SF-TYPE    TO SDNT-TYPE(SDNT-COUNT)
           MOVE WS-SF-NAME    TO SDNT-NAME(SDNT-COUNT)
           MOVE WS-SF-COUNTRY TO SDNT-COUNTRY(SDNT-COUNT)
           MOVE WS-SF-PROGRAM TO SDNT-PROGRAM(SDNT-COUNT).

      *----------------------------------------------------------------*
      * CUSTOMER RE-SCREEN                                             *
      *----------------------------------------------------------------*
       2000-SCREEN-CUSTOMERS.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE NEXT RECORD
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM 2100-SCREEN-ONE-CUSTOMER
               END-READ
           END-PERFORM.

       2100-SCREEN-ONE-CUSTOMER.
           ADD 1 TO WS-CUST-SCREENED
           MOVE CUST-OFAC-STATUS TO WS-OLD-OFAC-STATUS
           INITIALIZE OFS-INPUT
           MOVE SPACES TO WS-SCREEN-NAME
           IF CUST-INDIVIDUAL
               STRING CUST-LAST-NAME DELIMITED BY '  '
                      ', ' DELIMITED BY SIZE
                      CUST-FIRST-NAME DELIMITED BY '  '
                 INTO WS-SCREEN-NAME
               END-STRING
               MOVE CUST-LAST-NAME  TO OFS-LAST-NAME
               MOVE CUST-FIRST-NAME TO OFS-FIRST-NAME
               SET OFS-PARTY-INDIVIDUAL TO TRUE
           ELSE
               MOVE CUST-LAST-NAME TO WS-SCREEN-NAME
               SET OFS-PARTY-ENTITY TO TRUE
           END-IF
           MOVE WS-SCREEN-NAME TO OFS-FULL-NAME
           MOVE CUST-COUNTRY   TO OFS-COUNTRY
           CALL 'OFACSCRN' USING OFAC-SCREEN-PARMS SDN-TABLE
           IF OFS-RC-ERROR
               ADD 1 TO WS-ERROR-COUNT
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           EVALUATE TRUE
      *        INC33410 - CONFIRMED MATCHES ARE OWNED BY OFACREV
               WHEN CUST-OFAC-CONFIRMED-HIT
                   IF NOT OFS-CLEAR
                       ADD 1 TO WS-CUST-CONFIRMED-SKIP
                   END-IF
               WHEN OFS-HIT
                   ADD 1 TO WS-CUST-HITS
                   SET CUST-OFAC-POTENTIAL-HIT TO TRUE
                   MOVE 'CUST' TO WS-SUBJECT
                   PERFORM 2200-CUSTOMER-ALERT
               WHEN OFS-POTENTIAL-HIT
                   ADD 1 TO WS-CUST-POTENTIAL
                   SET CUST-OFAC-POTENTIAL-HIT TO TRUE
                   MOVE 'CUST' TO WS-SUBJECT
                   PERFORM 2200-CUSTOMER-ALERT
               WHEN CUST-OFAC-POTENTIAL-HIT
      *            PENDING REVIEW - LEFT FOR THE ANALYST TO DISPOSITION
                   CONTINUE
               WHEN OTHER
                   SET CUST-OFAC-CLEAR TO TRUE
           END-EVALUATE
           MOVE CTL-BUSINESS-DATE TO CUST-OFAC-LAST-SCREEN
           REWRITE CUSTOMER-RECORD
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF.

       2200-CUSTOMER-ALERT.
           INITIALIZE ALERT-RECORD
           MOVE CUST-ID TO ALRT-CUST-ID
           MOVE SPACES  TO ALRT-ACCT-NUMBER
           MOVE ZERO    TO ALRT-AMOUNT
           PERFORM 8000-WRITE-OFAC-ALERT
           MOVE CUST-ID TO WS-D-ID
           PERFORM 8100-REPORT-LINE.

      *----------------------------------------------------------------*
      * WIRE COUNTERPARTY RE-SCREEN - TODAY'S POSTED WIRES             *
      *----------------------------------------------------------------*
       3000-SCREEN-WIRES.
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ TRANPOST-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF TRAN-POSTED
                          AND (TRAN-CODE = 'DWIR' OR TRAN-CODE = 'WWIR')
                          AND TRAN-COUNTERPARTY-NAME NOT = SPACES
                           PERFORM 3100-SCREEN-ONE-WIRE
                       END-IF
               END-READ
               IF WS-TRAN-STATUS NOT = '00' AND NOT = '10'
                   MOVE 'TRANPOST' TO ABND-FILE-NAME
                   MOVE WS-TRAN-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
           END-PERFORM.

       3100-SCREEN-ONE-WIRE.
           ADD 1 TO WS-WIRES-SCREENED
           INITIALIZE OFS-INPUT
           MOVE TRAN-COUNTERPARTY-NAME    TO OFS-FULL-NAME
                                             WS-SCREEN-NAME
           MOVE TRAN-COUNTERPARTY-COUNTRY TO OFS-COUNTRY
           MOVE SPACE                     TO OFS-PARTY-TYPE
           CALL 'OFACSCRN' USING OFAC-SCREEN-PARMS SDN-TABLE
           IF OFS-RC-ERROR
               ADD 1 TO WS-ERROR-COUNT
               MOVE 04 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF OFS-HIT OR OFS-POTENTIAL-HIT
               ADD 1 TO WS-WIRE-HITS
               INITIALIZE ALERT-RECORD
               MOVE TRAN-ACCT-NUMBER TO ACCT-NUMBER ALRT-ACCT-NUMBER
               READ ACCOUNT-FILE
                   INVALID KEY
                       MOVE SPACES TO ALRT-CUST-ID
                   NOT INVALID KEY
                       MOVE ACCT-PRIMARY-CUST-ID TO ALRT-CUST-ID
               END-READ
               MOVE TRAN-AMOUNT TO ALRT-AMOUNT
               MOVE 'WIRE' TO WS-SUBJECT
               MOVE SPACE  TO WS-OLD-OFAC-STATUS
               PERFORM 8000-WRITE-OFAC-ALERT
               MOVE TRAN-ACCT-NUMBER TO WS-D-ID
               PERFORM 8100-REPORT-LINE
           END-IF.

      *----------------------------------------------------------------*
       8000-WRITE-OFAC-ALERT.
           ACCEPT WS-TIME FROM TIME
           MOVE CTL-BUSINESS-DATE TO ALRT-DATE
           MOVE WS-TIME(1:6)      TO ALRT-TIME
           MOVE 'OFAC'            TO ALRT-TYPE
           IF OFS-HIT
               SET ALRT-CRITICAL TO TRUE
               MOVE 'OFACHIT'  TO ALRT-RULE-ID
           ELSE
               SET ALRT-HIGH TO TRUE
               MOVE 'OFACPOT'  TO ALRT-RULE-ID
           END-IF
           MOVE OFS-SCORE       TO WS-SCORE-ED
           MOVE OFS-MATCH-ENT-NUM TO WS-ENT-ED
           STRING WS-SUBJECT ' ' DELIMITED BY SIZE
                  'SDN ' WS-ENT-ED ' SCORE ' WS-SCORE-ED
                  ' ' OFS-MATCH-RULE ' ' DELIMITED BY SIZE
                  OFS-MATCH-NAME DELIMITED BY '  '
             INTO ALRT-DESCRIPTION
           END-STRING
           SET ALRT-OPEN TO TRUE
           MOVE WS-PROGRAM-NAME TO ALRT-SOURCE-PGM
           WRITE ALERT-RECORD
           IF WS-ALRT-STATUS NOT = '00'
               MOVE 'ALERTS' TO ABND-FILE-NAME
               MOVE WS-ALRT-STATUS TO ABND-FILE-STATUS
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-ALERT-COUNT.

       8100-REPORT-LINE.
           MOVE WS-SUBJECT         TO WS-D-SUBJECT
           MOVE WS-SCREEN-NAME     TO WS-D-NAME
           MOVE OFS-RESULT         TO WS-D-RESULT
           MOVE OFS-SCORE          TO WS-D-SCORE
           MOVE OFS-MATCH-RULE     TO WS-D-RULE
           MOVE WS-OLD-OFAC-STATUS TO WS-D-OLD
           IF WS-SUBJECT = 'CUST'
               MOVE CUST-OFAC-STATUS TO WS-D-NEW
           ELSE
               MOVE SPACE TO WS-D-NEW
           END-IF
           MOVE OFS-MATCH-ENT-NUM  TO WS-D-ENT
           MOVE OFS-MATCH-NAME     TO WS-D-MATCH
           WRITE REPORT-LINE FROM WS-RPT-DETAIL.

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'SDN ENTRIES LOADED' TO WS-C-LABEL
           MOVE WS-SDN-LOADED TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CUSTOMERS SCREENED' TO WS-C-LABEL
           MOVE WS-CUST-SCREENED TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CUSTOMER HITS (>= 90)' TO WS-C-LABEL
           MOVE WS-CUST-HITS TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CUSTOMER POTENTIAL MATCHES (75-89)' TO WS-C-LABEL
           MOVE WS-CUST-POTENTIAL TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'CONFIRMED (R) - NOT RE-ALERTED' TO WS-C-LABEL
           MOVE WS-CUST-CONFIRMED-SKIP TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'WIRES SCREENED' TO WS-C-LABEL
           MOVE WS-WIRES-SCREENED TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'WIRE COUNTERPARTY MATCHES' TO WS-C-LABEL
           MOVE WS-WIRE-HITS TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           MOVE 'SCREENING ERRORS' TO WS-C-LABEL
           MOVE WS-ERROR-COUNT TO WS-C-VALUE
           WRITE REPORT-LINE FROM WS-RPT-COUNT
           CLOSE SDN-FILE CUSTOMER-FILE TRANPOST-FILE ACCOUNT-FILE
                 ALERT-FILE REPORT-FILE
           DISPLAY 'OFACBAT  SDN=' WS-SDN-LOADED
                   ' CUSTOMERS=' WS-CUST-SCREENED
                   ' HITS=' WS-CUST-HITS
                   ' POTENTIAL=' WS-CUST-POTENTIAL
                   ' WIRES=' WS-WIRES-SCREENED
                   ' WIRE-HITS=' WS-WIRE-HITS
                   ' ALERTS=' WS-ALERT-COUNT
                   ' RC=' WS-RETURN-CODE.

       9900-ABEND.
           MOVE WS-PROGRAM-NAME TO ABND-PROGRAM
           MOVE 'FILE I/O' TO ABND-PARAGRAPH
           MOVE 'UNEXPECTED FILE STATUS' TO ABND-MESSAGE
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
