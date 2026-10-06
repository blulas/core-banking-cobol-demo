      *================================================================*
      * PROGRAM  : RELBAL                                              *
      * TYPE     : BATCH - DAILY (EOD STEP 18)                         *
      * PURPOSE  : AGGREGATES DEPOSIT BALANCES BY RELATIONSHIP ID AND  *
      *            STORES THE TOTAL IN CUST-TOTAL-REL-BALANCE ON EVERY *
      *            CUSTOMER IN THE RELATIONSHIP. USED BY FEE WAIVERS   *
      *            (FEEWAIVE) AND RELATIONSHIP RATE BONUSES (RATETIER).*
      *            RULES:                                              *
      *            - DEPOSIT CATEGORIES SV DD TD MM ONLY (NO LOANS)    *
      *            - CLOSED, ESCHEATED, CHARGED-OFF AND PENDING-OPEN   *
      *              ACCOUNTS EXCLUDED; DORMANT / FROZEN / RESTRICTED  *
      *              BALANCES STILL COUNT                              *
      *            - VIRTUAL ACCOUNTS EXCLUDED (HEADER HOLDS BALANCE)  *
      *            - OVERDRAWN (NEGATIVE) BALANCES COUNT AS ZERO       *
      *            - JOINT ACCOUNTS COUNT FOR BOTH OWNERS'             *
      *              RELATIONSHIPS (ONCE PER RELATIONSHIP)             *
      *            - TRUST ACCOUNTS ALSO COUNT TOWARD THE GRANTOR'S    *
      *              (CUST-PARENT-CUST-ID) RELATIONSHIP                *
      * INPUT    : ACCTMST, BANKCTL (VIA GETCTL)                       *
      * I-O      : CUSTMST                                             *
      * OUTPUT   : RELBRPT                                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2005-09-12 BHS  ORIGINAL - PREMIER CHECKING WAIVER SUPPORT     *
      * 2008-02-04 BHS  JOINT OWNER RELATIONSHIP CREDIT (CHG0151)      *
      * 2012-06-18 DPS  TRUST BALANCES TO GRANTOR RELATIONSHIP         *
      * 2014-03-12 DPS  OVERDRAWN BALANCES FLOORED AT ZERO (CHG1187)   *
      * 2019-03-25 AKW  EXCLUDE VIRTUAL ACCOUNTS                       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELBAL.
       AUTHOR. B H SANDOVAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCTMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NUMBER
               ALTERNATE RECORD KEY IS ACCT-PRIMARY-CUST-ID
                   WITH DUPLICATES
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "RELBRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTREC.
       FD  ACCOUNT-FILE.
       COPY ACCTREC.
       FD  REPORT-FILE.
       01  REPORT-LINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-CUST-STATUS              PIC X(02).
           05  WS-ACCT-STATUS              PIC X(02).
           05  WS-RPT-STATUS               PIC X(02).

       01  WS-EOF-FLAG                     PIC X(01).
           88  WS-EOF                                VALUE 'Y'.

      *--- CUSTOMER -> RELATIONSHIP LOOKUP ----------------------------*
       01  WS-CUST-TABLE.
           05  WS-CUST-COUNT               PIC 9(05) VALUE 0.
           05  WS-CUST-ENTRY OCCURS 1 TO 9999 TIMES
                             DEPENDING ON WS-CUST-COUNT
                             ASCENDING KEY IS WS-CT-CUST-ID
                             INDEXED BY WS-CT-IDX.
               10  WS-CT-CUST-ID           PIC X(10).
               10  WS-CT-REL-ID            PIC X(10).
               10  WS-CT-TYPE              PIC X(01).
               10  WS-CT-PARENT-ID         PIC X(10).

      *--- RELATIONSHIP TOTALS ----------------------------------------*
       01  WS-REL-TABLE.
           05  WS-REL-COUNT                PIC 9(05) VALUE 0.
           05  WS-REL-ENTRY OCCURS 1 TO 9999 TIMES
                            DEPENDING ON WS-REL-COUNT
                            INDEXED BY WS-RL-IDX.
               10  WS-RL-REL-ID            PIC X(10).
               10  WS-RL-TOTAL             PIC S9(13)V99 COMP-3.
               10  WS-RL-ACCT-COUNT        PIC 9(05).
       01  WS-TABLE-MAX                    PIC 9(05) VALUE 9999.

      *--- RELATIONSHIPS CREDITED BY THE CURRENT ACCOUNT --------------*
       01  WS-ACCT-RELS.
           05  WS-AR-COUNT                 PIC 9(01).
           05  WS-AR-REL-ID                PIC X(10) OCCURS 3 TIMES.
       01  WS-AR-SUB                       PIC 9(01).
       01  WS-AR-DUP                       PIC X(01).

       01  WS-WORK.
           05  WS-LOOKUP-CUST              PIC X(10).
           05  WS-LOOKUP-REL               PIC X(10).
           05  WS-LOOKUP-TYPE              PIC X(01).
           05  WS-LOOKUP-PARENT            PIC X(10).
           05  WS-COUNT-BAL                PIC S9(13)V99 COMP-3.
           05  WS-NEW-TOTAL                PIC S9(13)V99 COMP-3.
           05  WS-NEW-ACCTS                PIC 9(05).

       01  WS-COUNTERS.
           05  WS-ACCT-READ                PIC 9(07) VALUE 0.
           05  WS-ACCT-COUNTED             PIC 9(07) VALUE 0.
           05  WS-ACCT-FLOORED             PIC 9(07) VALUE 0.
           05  WS-CUST-UPDATED             PIC 9(07) VALUE 0.
           05  WS-GRAND-TOTAL              PIC S9(15)V99 COMP-3
                                           VALUE 0.

       01  WS-HEADING-1.
           05  FILLER PIC X(40)
               VALUE 'CBS RELBAL   RELATIONSHIP BALANCE REPORT'.
           05  FILLER PIC X(16) VALUE ' BUSINESS DATE: '.
           05  WS-H1-DATE                  PIC 9(08).
       01  WS-HEADING-2.
           05  FILLER PIC X(40) VALUE
               'CUSTOMER   RELATIONSHIP  ACCTS     RELAT'.
           05  FILLER PIC X(40) VALUE
               'IONSHIP BALANCE        PRIOR BALANCE'.
       01  WS-DETAIL-LINE.
           05  WS-D-CUST                   PIC X(10).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-D-REL                    PIC X(10).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-ACCTS                  PIC ZZZZ9.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-TOTAL                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-D-PRIOR                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-D-NOTE                   PIC X(20).
       01  WS-EDIT-CNT                     PIC Z,ZZZ,ZZ9.
       01  WS-EDIT-AMT                     PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.

       COPY BANKCTL.
       COPY ABNDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           PERFORM 1000-LOAD-CUSTOMERS
           PERFORM 2000-ACCUMULATE-ACCOUNTS
           PERFORM 3000-UPDATE-CUSTOMERS
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * CUSTMST IS READ IN KEY SEQUENCE SO THE TABLE IS IN ASCENDING   *
      * CUST-ID ORDER FOR SEARCH ALL.                                  *
      *----------------------------------------------------------------*
       1000-LOAD-CUSTOMERS.
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN INPUT FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END
                       IF WS-CUST-COUNT >= WS-TABLE-MAX
                           MOVE 'CUSTOMER TABLE FULL' TO ABND-MESSAGE
                           PERFORM 9900-ABEND
                       END-IF
                       ADD 1 TO WS-CUST-COUNT
                       MOVE CUST-ID   TO WS-CT-CUST-ID(WS-CUST-COUNT)
                       MOVE CUST-RELATIONSHIP-ID
                                      TO WS-CT-REL-ID(WS-CUST-COUNT)
      *                CUSTOMER WITH NO RELATIONSHIP ID IS ITS OWN
                       IF CUST-RELATIONSHIP-ID = SPACES
                           MOVE CUST-ID TO WS-CT-REL-ID(WS-CUST-COUNT)
                       END-IF
                       MOVE CUST-TYPE TO WS-CT-TYPE(WS-CUST-COUNT)
                       MOVE CUST-PARENT-CUST-ID
                                      TO WS-CT-PARENT-ID(WS-CUST-COUNT)
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE.

      *----------------------------------------------------------------*
       2000-ACCUMULATE-ACCOUNTS.
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
               MOVE 'ACCTMST' TO ABND-FILE-NAME
               MOVE WS-ACCT-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN INPUT FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ ACCOUNT-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 2100-PROCESS-ACCOUNT
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       2100-PROCESS-ACCOUNT.
           ADD 1 TO WS-ACCT-READ
           IF (ACCT-CATEGORY NOT = 'SV' AND NOT = 'DD'
               AND NOT = 'TD' AND NOT = 'MM')
              OR ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
              OR ACCT-PENDING-OPEN OR ACCT-IS-VIRTUAL
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-ACCT-COUNTED
           IF ACCT-LEDGER-BAL < ZERO
               MOVE ZERO TO WS-COUNT-BAL
               ADD 1 TO WS-ACCT-FLOORED
           ELSE
               MOVE ACCT-LEDGER-BAL TO WS-COUNT-BAL
           END-IF
           MOVE ZERO TO WS-AR-COUNT
      *    PRIMARY OWNER (AND GRANTOR IF THE OWNER IS A TRUST)
           MOVE ACCT-PRIMARY-CUST-ID TO WS-LOOKUP-CUST
           PERFORM 8000-LOOKUP-CUSTOMER
           IF WS-LOOKUP-REL NOT = SPACES
               PERFORM 8100-ADD-ACCT-REL
               IF WS-LOOKUP-TYPE = 'T' AND WS-LOOKUP-PARENT NOT = SPACES
                   MOVE WS-LOOKUP-PARENT TO WS-LOOKUP-CUST
                   PERFORM 8000-LOOKUP-CUSTOMER
                   IF WS-LOOKUP-REL NOT = SPACES
                       PERFORM 8100-ADD-ACCT-REL
                   END-IF
               END-IF
           END-IF
      *    JOINT OWNER
           IF ACCT-JOINT-CUST-ID NOT = SPACES
               MOVE ACCT-JOINT-CUST-ID TO WS-LOOKUP-CUST
               PERFORM 8000-LOOKUP-CUSTOMER
               IF WS-LOOKUP-REL NOT = SPACES
                   PERFORM 8100-ADD-ACCT-REL
               END-IF
           END-IF
           PERFORM VARYING WS-AR-SUB FROM 1 BY 1
                   UNTIL WS-AR-SUB > WS-AR-COUNT
               PERFORM 8200-CREDIT-RELATIONSHIP
           END-PERFORM.

      *----------------------------------------------------------------*
       3000-UPDATE-CUSTOMERS.
           OPEN I-O CUSTOMER-FILE
           IF WS-CUST-STATUS NOT = '00'
               MOVE 'CUSTMST' TO ABND-FILE-NAME
               MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
               MOVE 'OPEN I-O FAILED' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           MOVE CTL-BUSINESS-DATE TO WS-H1-DATE
           WRITE REPORT-LINE FROM WS-HEADING-1
           WRITE REPORT-LINE FROM WS-HEADING-2
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'N' TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF
               READ CUSTOMER-FILE NEXT RECORD
                   AT END SET WS-EOF TO TRUE
                   NOT AT END PERFORM 3100-UPDATE-ONE
               END-READ
           END-PERFORM.

       3100-UPDATE-ONE.
           MOVE CUST-RELATIONSHIP-ID TO WS-LOOKUP-REL
           IF WS-LOOKUP-REL = SPACES
               MOVE CUST-ID TO WS-LOOKUP-REL
           END-IF
           MOVE ZERO TO WS-NEW-TOTAL WS-NEW-ACCTS
           SET WS-RL-IDX TO 1
           IF WS-REL-COUNT > 0
               SEARCH WS-REL-ENTRY
                   AT END CONTINUE
                   WHEN WS-RL-REL-ID(WS-RL-IDX) = WS-LOOKUP-REL
                       MOVE WS-RL-TOTAL(WS-RL-IDX)      TO WS-NEW-TOTAL
                       MOVE WS-RL-ACCT-COUNT(WS-RL-IDX) TO WS-NEW-ACCTS
               END-SEARCH
           END-IF
           MOVE CUST-ID                TO WS-D-CUST
           MOVE WS-LOOKUP-REL          TO WS-D-REL
           MOVE WS-NEW-ACCTS           TO WS-D-ACCTS
           MOVE WS-NEW-TOTAL           TO WS-D-TOTAL
           MOVE CUST-TOTAL-REL-BALANCE TO WS-D-PRIOR
           MOVE SPACES                 TO WS-D-NOTE
           IF WS-NEW-TOTAL NOT = CUST-TOTAL-REL-BALANCE
               MOVE WS-NEW-TOTAL TO CUST-TOTAL-REL-BALANCE
               REWRITE CUSTOMER-RECORD
               IF WS-CUST-STATUS NOT = '00'
                   MOVE 'CUSTMST' TO ABND-FILE-NAME
                   MOVE WS-CUST-STATUS TO ABND-FILE-STATUS
                   MOVE 'REWRITE FAILED' TO ABND-MESSAGE
                   PERFORM 9900-ABEND
               END-IF
               ADD 1 TO WS-CUST-UPDATED
               MOVE 'UPDATED' TO WS-D-NOTE
           END-IF
           WRITE REPORT-LINE FROM WS-DETAIL-LINE.

      *----------------------------------------------------------------*
       8000-LOOKUP-CUSTOMER.
           MOVE SPACES TO WS-LOOKUP-REL WS-LOOKUP-TYPE WS-LOOKUP-PARENT
           IF WS-CUST-COUNT > 0
               SEARCH ALL WS-CUST-ENTRY
                   AT END
                       DISPLAY 'RELBAL   ACCOUNT ' ACCT-NUMBER
                               ' OWNER NOT ON CUSTMST ' WS-LOOKUP-CUST
                   WHEN WS-CT-CUST-ID(WS-CT-IDX) = WS-LOOKUP-CUST
                       MOVE WS-CT-REL-ID(WS-CT-IDX)   TO WS-LOOKUP-REL
                       MOVE WS-CT-TYPE(WS-CT-IDX)     TO WS-LOOKUP-TYPE
                       MOVE WS-CT-PARENT-ID(WS-CT-IDX)
                         TO WS-LOOKUP-PARENT
               END-SEARCH
           END-IF.

      *    ADD WS-LOOKUP-REL TO THE ACCOUNT'S LIST UNLESS ALREADY THERE
       8100-ADD-ACCT-REL.
           MOVE 'N' TO WS-AR-DUP
           PERFORM VARYING WS-AR-SUB FROM 1 BY 1
                   UNTIL WS-AR-SUB > WS-AR-COUNT
               IF WS-AR-REL-ID(WS-AR-SUB) = WS-LOOKUP-REL
                   MOVE 'Y' TO WS-AR-DUP
               END-IF
           END-PERFORM
           IF WS-AR-DUP = 'N' AND WS-AR-COUNT < 3
               ADD 1 TO WS-AR-COUNT
               MOVE WS-LOOKUP-REL TO WS-AR-REL-ID(WS-AR-COUNT)
           END-IF.

       8200-CREDIT-RELATIONSHIP.
           SET WS-RL-IDX TO 1
           IF WS-REL-COUNT > 0
               SEARCH WS-REL-ENTRY
                   AT END PERFORM 8210-NEW-RELATIONSHIP
                   WHEN WS-RL-REL-ID(WS-RL-IDX) =
                        WS-AR-REL-ID(WS-AR-SUB)
                       ADD WS-COUNT-BAL TO WS-RL-TOTAL(WS-RL-IDX)
                       ADD 1 TO WS-RL-ACCT-COUNT(WS-RL-IDX)
               END-SEARCH
           ELSE
               PERFORM 8210-NEW-RELATIONSHIP
           END-IF
           ADD WS-COUNT-BAL TO WS-GRAND-TOTAL.

       8210-NEW-RELATIONSHIP.
           IF WS-REL-COUNT >= WS-TABLE-MAX
               MOVE 'RELATIONSHIP TABLE FULL' TO ABND-MESSAGE
               PERFORM 9900-ABEND
           END-IF
           ADD 1 TO WS-REL-COUNT
           MOVE WS-AR-REL-ID(WS-AR-SUB) TO WS-RL-REL-ID(WS-REL-COUNT)
           MOVE WS-COUNT-BAL            TO WS-RL-TOTAL(WS-REL-COUNT)
           MOVE 1 TO WS-RL-ACCT-COUNT(WS-REL-COUNT).

      *----------------------------------------------------------------*
       9000-TERMINATE.
           MOVE ALL '-' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-ACCT-READ TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'ACCOUNTS READ ............ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-ACCT-COUNTED TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'DEPOSIT ACCOUNTS COUNTED . ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-ACCT-FLOORED TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'OVERDRAWN FLOORED AT ZERO  ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-REL-COUNT TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'RELATIONSHIPS ............ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           MOVE WS-CUST-UPDATED TO WS-EDIT-CNT
           MOVE SPACES TO REPORT-LINE
           STRING 'CUSTOMERS UPDATED ........ ' WS-EDIT-CNT
               DELIMITED BY SIZE INTO REPORT-LINE
           WRITE REPORT-LINE
           CLOSE CUSTOMER-FILE REPORT-FILE
           DISPLAY 'RELBAL   ACCOUNTS=' WS-ACCT-READ
                   ' COUNTED=' WS-ACCT-COUNTED
                   ' RELATIONSHIPS=' WS-REL-COUNT
                   ' CUSTOMERS-UPDATED=' WS-CUST-UPDATED
           MOVE 0 TO RETURN-CODE.

      *----------------------------------------------------------------*
       9900-ABEND.
           MOVE 'RELBAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
