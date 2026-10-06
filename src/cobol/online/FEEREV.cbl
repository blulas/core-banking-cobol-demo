      *================================================================*
      * PROGRAM  : FEEREV                                              *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CB22       MAPSET FEERVS   MAP FEERVM               *
      * PURPOSE  : SERVICE FEE REVERSAL (REFUND). THE OPERATOR KEYS    *
      *            THE ACCOUNT AND THE TRAN ID OF THE POSTED FEE; THE  *
      *            FEE IS READ FROM TRANSACTION HISTORY (DB2) AND THE  *
      *            REFUND IS CHECKED AGAINST THE OPERATOR AUTHORITY:   *
      *              TELLER / SENIOR TELLER  $35.00 PER FEE, AND NO    *
      *                MORE THAN 2 CUSTOMER-SERVICE REFUNDS PER        *
      *                ACCOUNT IN A ROLLING 12 MONTHS                  *
      *              SUPERVISOR              $100.00 PER FEE, SAME     *
      *                FREQUENCY LIMIT                                 *
      *              BRANCH MANAGER          $500.00 PER FEE, NO       *
      *                FREQUENCY LIMIT                                 *
      *              OPERATIONS              NOT AUTHORISED            *
      *            BANK ERROR (BE) AND DUPLICATE FEE (DP) REFUNDS DO   *
      *            NOT COUNT TOWARD THE FREQUENCY LIMIT.               *
      *            A FEE CAN BE REFUNDED ONCE, UP TO ITS FULL AMOUNT,  *
      *            AND ONLY WITHIN 12 MONTHS OF POSTING.               *
      *            PF5 QUEUES A DRFD CREDIT ON TRANQUE; IT IS EDITED   *
      *            AND POSTED IN THE NIGHTLY CYCLE (TRANEDIT/POSTTRAN).*
      *            ENTER - VALIDATE   PF5 - CONFIRM   PF3 - MENU       *
      *            PF12  - CANCEL                                      *
      * FILES    : ACCTMST, CUSTMST, TELLMST, BANKCTL (READ),          *
      *            BANKCTL (UPDATE - TRAN SEQ), TRANQUE (WRITE)        *
      * DB2      : CBS.TRAN_HISTORY (SELECT)                           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-09-15 BHS  ORIGINAL - TELLER $25 / MANAGER $500           *
      * 2006-05-22 BHS  SUPERVISOR TIER $100                           *
      * 2010-07-01 DPS  REG E - FEE CODES WFOD/WFNS/WFMN/WFWR/WFRD/WFOT*
      *                 TELLER LIMIT RAISED TO $35 (OD FEE AMOUNT)     *
      * 2014-01-13 DPS  FEE HISTORY FROM DB2 TRAN_HISTORY (WAS VSAM    *
      *                 TRANHIST BROWSE) - CHG1102                     *
      * 2016-05-23 DPS  REFUND FREQUENCY LIMIT 2 PER 12 MONTHS FOR     *
      *                 TELLERS - CHG1311                              *
      * 2018-09-04 AKW  BANK ERROR / DUPLICATE EXCLUDED FROM LIMIT     *
      *                 (INC33190)                                     *
      * 2021-11-08 JQP  ANALYSIS FEES (WFAN) EXCLUDED - REFUND VIA     *
      *                 TREASURY SERVICES ONLY                         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEEREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                      PIC X(08) VALUE 'FEEREV'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB22'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-TIME-HHMMSS                  PIC 9(06).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-RBA                          PIC S9(08) COMP.

      *----------------------------------------------------------------*
      * REFUND AUTHORITY BY ROLE                                       *
      *   ROLE / MAX PER FEE / FREQUENCY LIMIT APPLIES (Y/N)           *
      *----------------------------------------------------------------*
       01  WS-AUTH-TABLE-VALUES.
           05  FILLER PIC X(13) VALUE 'TLR00003500Y '.
           05  FILLER PIC X(13) VALUE 'SRT00003500Y '.
           05  FILLER PIC X(13) VALUE 'SUP00010000Y '.
           05  FILLER PIC X(13) VALUE 'MGR00050000N '.
       01  WS-AUTH-TABLE REDEFINES WS-AUTH-TABLE-VALUES.
           05  WS-AUTH-ENTRY OCCURS 4 TIMES INDEXED BY AU-IDX.
               10  WS-AU-ROLE              PIC X(03).
               10  WS-AU-MAX-AMT           PIC 9(06)V99.
               10  WS-AU-FREQ-FLAG         PIC X(01).
               10  FILLER                  PIC X(01).

       01  WS-CONSTANTS.
           05  WS-MAX-REFUNDS-12M          PIC 9(02) VALUE 2.
           05  WS-LOOKBACK-MONTHS          PIC S9(05) VALUE -12.

       01  WS-WORK.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-MESSAGE                  PIC X(79).
           05  WS-AUTH-FOUND               PIC X(01).
           05  WS-MAX-AMT                  PIC S9(07)V99 COMP-3.
           05  WS-FREQ-APPLIES             PIC X(01).
           05  WS-ROLE-TEXT                PIC X(10).
           05  WS-REFUND-AMT               PIC S9(13)V99 COMP-3.
           05  WS-FEE-AMT                  PIC S9(13)V99 COMP-3.
           05  WS-FEE-DATE                 PIC 9(08).
           05  WS-FEE-DATE-X REDEFINES WS-FEE-DATE.
               10  WS-FD-CCYY              PIC X(04).
               10  WS-FD-MM                PIC X(02).
               10  WS-FD-DD                PIC X(02).
           05  WS-LOOKBACK-DATE            PIC 9(08).
           05  WS-LOOKBACK-X REDEFINES WS-LOOKBACK-DATE.
               10  WS-LB-CCYY              PIC X(04).
               10  WS-LB-MM                PIC X(02).
               10  WS-LB-DD                PIC X(02).
           05  WS-SEQ                      PIC 9(08).
           05  WS-AMT-ED                   PIC ZZZ,ZZ9.99.
           05  WS-AMT-ED2                  PIC Z,ZZZ,ZZ9.99.
           05  WS-CNT-ED                   PIC ZZ9.
           05  WS-DATE-ED                  PIC 9999/99/99.
           05  WS-SQLCODE-EDIT             PIC -(9)9.

      *    DB2 HOST VARIABLES
       01  WS-SQL-ACCT                     PIC X(12).
       01  WS-SQL-TRAN-ID                  PIC X(16).
       01  WS-SQL-REF                      PIC X(20).
       01  WS-SQL-FROM-DATE                PIC X(10).
       01  WS-SQL-COUNT                    PIC S9(09) COMP.
       01  WS-SQL-EFF-DATE                 PIC X(10).

       COPY ACCTREC.
       COPY CUSTREC.
       COPY TLRREC.
       COPY BANKCTL.
       COPY TRANREC.
       COPY TRANCODE.
       COPY DATEPARM.
       COPY FEERVS.
       COPY DFHAID.
       COPY DFHBMSCA.

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLTRNH END-EXEC.

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-FIRST-TIME                     VALUE SPACE.
               88  CA-VALIDATED                      VALUE 'V'.
           05  CA-ACCT                     PIC X(12).
           05  CA-FEE-TRAN-ID              PIC X(16).
           05  CA-FEE-CODE                 PIC X(04).
           05  CA-REASON                   PIC X(02).
           05  CA-REFUND-AMT               PIC S9(13)V99 COMP-3.
           05  CA-COST-CENTER              PIC X(05).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(48).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               PERFORM 1000-SEND-INITIAL
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                       INITIALIZE WS-COMMAREA
                       PERFORM 1000-SEND-INITIAL
                   WHEN DFHENTER
                       PERFORM 2000-VALIDATE
                   WHEN DFHPF5
                       PERFORM 3000-CONFIRM
                   WHEN OTHER
                       MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                       PERFORM 8000-SEND-DATAONLY
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANID)
                COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-SEND-INITIAL.
           MOVE LOW-VALUES TO FEERVMO
           PERFORM 8100-READ-CONTROL
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           MOVE 'ENTER ACCOUNT, FEE TRAN ID AND REASON' TO MSGO
           EXEC CICS SEND MAP('FEERVM') MAPSET('FEERVS')
                FROM(FEERVMO) ERASE RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * ENTER - VALIDATE THE REFUND REQUEST                            *
      *----------------------------------------------------------------*
       2000-VALIDATE.
           EXEC CICS RECEIVE MAP('FEERVM') MAPSET('FEERVS')
                INTO(FEERVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8100-READ-CONTROL
           IF NOT WS-ERROR
               PERFORM 2100-EDIT-INPUT
           END-IF
           IF NOT WS-ERROR
               PERFORM 2200-OPERATOR-AUTHORITY
           END-IF
           IF NOT WS-ERROR
               PERFORM 2300-READ-ACCOUNT
           END-IF
           IF NOT WS-ERROR
               PERFORM 2400-READ-FEE
           END-IF
           IF NOT WS-ERROR
               PERFORM 2500-CHECK-PRIOR-REFUND
           END-IF
           IF NOT WS-ERROR
               PERFORM 2600-CHECK-LIMITS
           END-IF
           IF NOT WS-ERROR
               SET CA-VALIDATED TO TRUE
               MOVE WS-REFUND-AMT TO CA-REFUND-AMT WS-AMT-ED2
               STRING 'REFUND ' FUNCTION TRIM(WS-AMT-ED2)
                      ' OF ' CA-FEE-CODE ' FEE ' CA-FEE-TRAN-ID
                      ' - PRESS PF5 TO CONFIRM'
                      DELIMITED BY SIZE INTO CONFIRMO
               MOVE 'REVIEW AND PRESS PF5 TO QUEUE THE REFUND'
                 TO WS-MESSAGE
           END-IF
           PERFORM 8000-SEND-DATAONLY.

       2100-EDIT-INPUT.
           IF ACCTNOL = 0 OR ACCTNOI = SPACES OR LOW-VALUES
               MOVE 'ACCOUNT NUMBER IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE ACCTNOI TO CA-ACCT
           IF FEETIDL = 0 OR FEETIDI = SPACES OR LOW-VALUES
              OR FEETIDI IS NOT NUMERIC
               MOVE 'FEE TRAN ID (16 DIGITS) IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO FEETIDL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE FEETIDI TO CA-FEE-TRAN-ID
           IF REASONI NOT = 'CS' AND NOT = 'BE' AND NOT = 'DP'
               MOVE 'REASON MUST BE CS, BE OR DP' TO WS-MESSAGE
               MOVE -1 TO REASONL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE REASONI TO CA-REASON
      *    BLANK AMOUNT = FULL FEE (SET IN 2400)
           MOVE ZERO TO WS-REFUND-AMT
           IF RFAMTL > 0 AND RFAMTI NOT = SPACES
               IF FUNCTION TEST-NUMVAL(RFAMTI) NOT = 0
                   MOVE 'REFUND AMOUNT MUST BE NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO RFAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               END-IF
               COMPUTE WS-REFUND-AMT = FUNCTION NUMVAL(RFAMTI)
               IF WS-REFUND-AMT NOT > ZERO
                   MOVE 'REFUND AMOUNT MUST BE GREATER THAN ZERO'
                     TO WS-MESSAGE
                   MOVE -1 TO RFAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               END-IF
           END-IF.

       2200-OPERATOR-AUTHORITY.
           MOVE WS-USERID TO TLR-USER-ID
           EXEC CICS READ FILE('TELLMST') INTO(TELLER-RECORD)
                RIDFLD(TLR-USER-ID) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR NOT TLR-ACTIVE
               MOVE 'OPERATOR NOT AUTHORISED FOR FEE REFUNDS'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-AUTH-FOUND
           SET AU-IDX TO 1
           SEARCH WS-AUTH-ENTRY
               AT END
                   CONTINUE
               WHEN WS-AU-ROLE(AU-IDX) = TLR-ROLE
                   MOVE 'Y' TO WS-AUTH-FOUND
                   MOVE WS-AU-MAX-AMT(AU-IDX)   TO WS-MAX-AMT
                   MOVE WS-AU-FREQ-FLAG(AU-IDX) TO WS-FREQ-APPLIES
           END-SEARCH
           IF WS-AUTH-FOUND = 'N'
      *        OPS (BACK OFFICE) HAS NO REFUND AUTHORITY - CHG0612
               MOVE 'ROLE HAS NO FEE REFUND AUTHORITY - SEE MANAGER'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN TLR-ROLE-TELLER        MOVE 'TELLER'     TO ROLEO
               WHEN TLR-ROLE-SENIOR-TELLER MOVE 'SR TELLER'  TO ROLEO
               WHEN TLR-ROLE-SUPERVISOR    MOVE 'SUPERVISOR' TO ROLEO
               WHEN TLR-ROLE-MANAGER       MOVE 'MANAGER'    TO ROLEO
           END-EVALUATE
           MOVE WS-MAX-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO LIMITO
           MOVE TLR-BRANCH TO CA-COST-CENTER.

       2300-READ-ACCOUNT.
           MOVE CA-ACCT TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(ACCT-NUMBER) RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT FILE UNAVAILABLE - TRY AGAIN'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
      *    CLOSED / ESCHEATED / CHARGED-OFF ACCOUNTS ARE REFUNDED BY
      *    DEPOSIT OPERATIONS THROUGH THE GL SUSPENSE PROCESS
           IF ACCT-CLOSED OR ACCT-ESCHEATED OR ACCT-CHARGED-OFF
               MOVE 'ACCOUNT NOT OPEN - REFER TO DEPOSIT OPERATIONS'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO OWNERO
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                RIDFLD(CUST-ID) RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO OWNERO
           END-IF.

      *----------------------------------------------------------------*
      * THE FEE MUST BE A POSTED FEE ON THIS ACCOUNT, POSTED WITHIN    *
      * THE LAST 12 MONTHS                                             *
      *----------------------------------------------------------------*
       2400-READ-FEE.
           INITIALIZE DATE-PARMS
           SET DTU-ADD-MONTHS TO TRUE
           MOVE CTL-BUSINESS-DATE  TO DTU-DATE-1
           MOVE WS-LOOKBACK-MONTHS TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-LOOKBACK-DATE
           STRING WS-LB-CCYY '-' WS-LB-MM '-' WS-LB-DD
                  DELIMITED BY SIZE INTO WS-SQL-FROM-DATE

           MOVE CA-ACCT        TO WS-SQL-ACCT
           MOVE CA-FEE-TRAN-ID TO WS-SQL-TRAN-ID
           EXEC SQL
               SELECT TRAN_CODE, AMOUNT, CHAR(EFFECTIVE_DATE, ISO),
                      STATUS
                 INTO :TH-TRAN-CODE, :TH-AMOUNT, :WS-SQL-EFF-DATE,
                      :TH-STATUS
                 FROM CBS.TRAN_HISTORY
                WHERE TRAN_ID     = :WS-SQL-TRAN-ID
                  AND ACCT_NUMBER = :WS-SQL-ACCT
                WITH UR
           END-EXEC
           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE 'FEE NOT FOUND ON THIS ACCOUNT (POSTED HISTORY)'
                     TO WS-MESSAGE
                   MOVE -1 TO FEETIDL
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE SQLCODE TO WS-SQLCODE-EDIT
                   STRING 'TRANSACTION HISTORY UNAVAILABLE - SQLCODE '
                          WS-SQLCODE-EDIT
                          DELIMITED BY SIZE INTO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
                   EXIT PARAGRAPH
           END-EVALUATE

           MOVE TH-TRAN-CODE TO CA-FEE-CODE FEECDO
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               AT END
                   MOVE SPACES TO FEEDESCO
               WHEN TC-CODE(TC-IDX) = TH-TRAN-CODE
                   MOVE TC-DESC(TC-IDX) TO FEEDESCO
           END-SEARCH
           MOVE TH-AMOUNT TO WS-FEE-AMT WS-AMT-ED2
           MOVE WS-AMT-ED2 TO FEEAMTO
           MOVE WS-SQL-EFF-DATE(1:4) TO WS-FD-CCYY
           MOVE WS-SQL-EFF-DATE(6:2) TO WS-FD-MM
           MOVE WS-SQL-EFF-DATE(9:2) TO WS-FD-DD
           MOVE WS-FEE-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO FEEDTO

           EVALUATE TRUE
               WHEN TH-TRAN-CODE(1:2) NOT = 'WF'
                   MOVE 'TRANSACTION IS NOT A SERVICE FEE' TO WS-MESSAGE
                   MOVE -1 TO FEETIDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN TH-TRAN-CODE = 'WFAN'
                   MOVE 'ANALYSIS FEES - REFUND VIA TREASURY SERVICES'
                     TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN TH-STATUS NOT = 'S'
                   MOVE 'FEE IS NOT IN POSTED STATUS' TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-FEE-DATE < WS-LOOKBACK-DATE
                   MOVE 'FEE IS OLDER THAN 12 MONTHS - NOT REFUNDABLE'
                     TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-REFUND-AMT > WS-FEE-AMT
                   MOVE 'REFUND CANNOT EXCEED THE FEE AMOUNT'
                     TO WS-MESSAGE
                   MOVE -1 TO RFAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-REFUND-AMT = ZERO
                   MOVE WS-FEE-AMT TO WS-REFUND-AMT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *    ONE REFUND PER FEE - THE REFUND CARRIES THE FEE TRAN ID IN
      *    ITS REFERENCE FIELD
       2500-CHECK-PRIOR-REFUND.
           MOVE CA-FEE-TRAN-ID TO WS-SQL-REF
           EXEC SQL
               SELECT COUNT(*)
                 INTO :WS-SQL-COUNT
                 FROM CBS.TRAN_HISTORY
                WHERE ACCT_NUMBER = :WS-SQL-ACCT
                  AND TRAN_CODE   = 'DRFD'
                  AND REFERENCE   = :WS-SQL-REF
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-SQLCODE-EDIT
               STRING 'TRANSACTION HISTORY UNAVAILABLE - SQLCODE '
                      WS-SQLCODE-EDIT
                      DELIMITED BY SIZE INTO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF WS-SQL-COUNT > 0
               MOVE 'THIS FEE HAS ALREADY BEEN REFUNDED' TO WS-MESSAGE
               MOVE -1 TO FEETIDL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT AND FREQUENCY LIMITS. CUSTOMER-SERVICE REFUNDS IN THE   *
      * LAST 12 MONTHS ARE IDENTIFIED BY THE DRFD DESCRIPTION PREFIX   *
      * WRITTEN IN 3100.                                               *
      *----------------------------------------------------------------*
       2600-CHECK-LIMITS.
           EXEC SQL
               SELECT COUNT(*)
                 INTO :WS-SQL-COUNT
                 FROM CBS.TRAN_HISTORY
                WHERE ACCT_NUMBER    = :WS-SQL-ACCT
                  AND TRAN_CODE      = 'DRFD'
                  AND EFFECTIVE_DATE > DATE(:WS-SQL-FROM-DATE)
                  AND DESCRIPTION LIKE 'FEE REFUND CS%'
                WITH UR
           END-EXEC
           IF SQLCODE NOT = 0
               MOVE ZERO TO WS-SQL-COUNT
           END-IF
           MOVE WS-SQL-COUNT TO WS-CNT-ED
           MOVE WS-CNT-ED TO RFCNTO

           IF WS-REFUND-AMT > WS-MAX-AMT
               MOVE WS-MAX-AMT TO WS-AMT-ED
               STRING 'REFUND EXCEEDS YOUR LIMIT OF '
                      FUNCTION TRIM(WS-AMT-ED)
                      ' - MANAGER APPROVAL REQUIRED'
                      DELIMITED BY SIZE INTO WS-MESSAGE
               MOVE -1 TO RFAMTL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           IF WS-FREQ-APPLIES = 'Y'
              AND CA-REASON = 'CS'
              AND WS-SQL-COUNT >= WS-MAX-REFUNDS-12M
               MOVE 'ACCOUNT HAS 2 REFUNDS IN 12 MONTHS - MANAGER ONLY'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PF5 - CONFIRM. RE-CHECK AUTHORITY (USER MAY HAVE CHANGED) AND  *
      * QUEUE THE DRFD CREDIT                                          *
      *----------------------------------------------------------------*
       3000-CONFIRM.
           MOVE LOW-VALUES TO FEERVMO
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF NOT CA-VALIDATED
               MOVE 'PRESS ENTER TO VALIDATE BEFORE CONFIRMING'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-READ-CONTROL
           PERFORM 2200-OPERATOR-AUTHORITY
           IF NOT WS-ERROR AND CA-REFUND-AMT > WS-MAX-AMT
               MOVE 'REFUND EXCEEDS YOUR LIMIT - MANAGER APPROVAL REQD'
                 TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF WS-ERROR
               INITIALIZE WS-COMMAREA
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF

           PERFORM 8200-NEXT-TRAN-SEQ
           PERFORM 3100-WRITE-TRANQUE
           EXEC CICS SYNCPOINT END-EXEC
           MOVE SPACES TO WS-MESSAGE
           STRING 'REFUND QUEUED - REF ' TRAN-ID
                  ' - POSTS IN TONIGHT''S CYCLE'
                  DELIMITED BY SIZE INTO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       3100-WRITE-TRANQUE.
           INITIALIZE TRANSACTION-RECORD
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                TIME(WS-TIME-HHMMSS) END-EXEC
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE
                                     TRAN-EFFECTIVE-DATE
           MOVE WS-SEQ            TO TRAN-ID-SEQ
           MOVE CA-ACCT           TO TRAN-ACCT-NUMBER
           MOVE ZERO              TO TRAN-POST-DATE TRAN-HOLD-DAYS
                                     TRAN-FEE-AMOUNT
           MOVE WS-TIME-HHMMSS    TO TRAN-TIME
           MOVE 'DRFD'            TO TRAN-CODE
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               WHEN TC-CODE(TC-IDX) = TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)     TO TRAN-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX) TO TRAN-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX) TO TRAN-REGD-FLAG
           END-SEARCH
           MOVE CA-REFUND-AMT     TO TRAN-AMOUNT
           MOVE 'BRN'             TO TRAN-CHANNEL
           STRING 'FEE REFUND ' CA-REASON ' ' CA-FEE-CODE
                  DELIMITED BY SIZE INTO TRAN-DESCRIPTION
           MOVE CA-FEE-TRAN-ID    TO TRAN-REFERENCE
                                     TRAN-ORIG-TRAN-ID
           MOVE SPACES            TO TRAN-COUNTER-ACCT
                                     TRAN-COUNTERPARTY-NAME
                                     TRAN-COUNTERPARTY-ROUTING
                                     TRAN-COUNTERPARTY-COUNTRY
                                     TRAN-REJECT-CODE TRAN-GL-ACCOUNT
           MOVE CA-COST-CENTER    TO TRAN-COST-CENTER
           SET TRAN-PENDING TO TRUE
           MOVE 'N'               TO TRAN-OD-FLAG
           MOVE WS-USERID         TO TRAN-USER-ID
           MOVE WS-PROGRAM        TO TRAN-SOURCE-PGM
           EXEC CICS WRITE FILE('TRANQUE') FROM(TRANSACTION-RECORD)
                RIDFLD(WS-RBA) RBA RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE 'TRANSACTION QUEUE UNAVAILABLE - BACKED OUT'
                 TO WS-MESSAGE
               INITIALIZE WS-COMMAREA
               PERFORM 8000-SEND-DATAONLY
               EXEC CICS RETURN TRANSID(WS-TRANID)
                    COMMAREA(WS-COMMAREA)
                    LENGTH(LENGTH OF WS-COMMAREA)
               END-EXEC
           END-IF.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           EXEC CICS SEND MAP('FEERVM') MAPSET('FEERVS')
                FROM(FEERVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-READ-CONTROL.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'BANK CONTROL RECORD UNAVAILABLE' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

      *    ONLINE ITEMS USE THE CHANNEL RANGE 05000000-09999999
       8200-NEXT-TRAN-SEQ.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) UPDATE RESP(WS-RESP) END-EXEC
           ADD 1 TO CTL-LAST-TRAN-SEQ
           COMPUTE WS-SEQ = 5000000
                          + FUNCTION MOD(CTL-LAST-TRAN-SEQ, 4999998)
           EXEC CICS REWRITE FILE('BANKCTL')
                FROM(BANK-CONTROL-RECORD) RESP(WS-RESP) END-EXEC.
