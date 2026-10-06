      *================================================================*
      * PROGRAM  : XFERFUND                                            *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CB10       MAPSET XFERFNS  MAP XFERFNM              *
      * PURPOSE  : INTERNAL FUNDS TRANSFER BETWEEN TWO DEPOSIT         *
      *            ACCOUNTS OWNED BY THE SAME CUSTOMER (PRIMARY OR     *
      *            JOINT OWNER ON BOTH).                               *
      *            ENTER - VALIDATE AND SHOW PROJECTED BALANCES        *
      *            PF5   - CONFIRM: MEMO-POST BOTH ACCOUNTS AND QUEUE  *
      *                    THE WXFR / DXFR PAIR TO TRANQUE FOR THE     *
      *                    NIGHTLY POSTING RUN (TRANEDIT / POSTTRAN)   *
      *            EDITS   - TRANVAL (SAME MODULE AS BATCH EDIT)       *
      *            REG D   - REGDCHK (COUNT / EXCESS FEE DISPLAY ONLY; *
      *                      THE FEE IS ASSESSED BY POSTTRAN)          *
      *            OD      - INLINE AVAILABLE-BALANCE CHECK (SEE       *
      *                      2400-CHECK-FUNDS)                         *
      * FILES    : ACCTMST (READ/UPDATE), PRODMST, BANKCTL (UPDATE -   *
      *            TRAN SEQUENCE), TRANQUE (ESDS, WRITE)               *
      * LINKS    : TRANVAL, REGDCHK                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1997-05-12 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-03-10 BHS  JOINT OWNER MAY TRANSFER (CHG0298)             *
      * 2009-11-17 DPS  INLINE OD CHECK - ODDECIDE LINK TOO SLOW AT    *
      *                 PEAK (INC09981). TEMPORARY - SEE CHG0842       *
      * 2009-11-17 DPS  REG D COUNT VIA REGDCHK                        *
      * 2012-04-30 DPS  TRANVAL EDITS ON BOTH LEGS                     *
      * 2014-11-03 DPS  CONFIRMATION STEP PF5, MEMO POSTING            *
      * 2016-02-08 DPS  RESP CHECKING - REMOVED HANDLE CONDITION       *
      * 2020-05-01 AKW  CHG3301 REG D MESSAGE TEXT                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XFERFUND.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                      PIC X(08) VALUE 'XFERFUND'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB10'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-TIME-HHMMSS                  PIC 9(06).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-RBA                          PIC S9(08) COMP.

       01  WS-WORK.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-PROJ-AVAIL               PIC S9(13)V99 COMP-3.
           05  WS-OD-USED                  PIC S9(13)V99 COMP-3.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-MESSAGE                  PIC X(79).
           05  WS-OWNER-OK                 PIC X(01).
           05  WS-AMT-ED                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AMT-ED2                  PIC ZZZ,ZZZ,ZZ9.99-.
           05  WS-CNT-ED                   PIC ZZ9.
           05  WS-DATE-ED                  PIC 9999/99/99.
           05  WS-SEQ                      PIC 9(08).

      *    ACCOUNT IMAGES (FROM / TO)
       COPY ACCTREC.
       COPY ACCTREC REPLACING ==ACCOUNT-RECORD== BY ==WS-FROM-ACCT==
                              LEADING ==ACCT-== BY ==FR-==.
       COPY ACCTREC REPLACING ==ACCOUNT-RECORD== BY ==WS-TO-ACCT==
                              LEADING ==ACCT-== BY ==TO-==.
       COPY PRODREC.
       COPY BANKCTL.
       COPY TRANREC.
       COPY TRANCODE.
       COPY TRVPARM.
       COPY RGDPARM.
       COPY XFERFNS.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-FIRST-TIME                     VALUE SPACE.
               88  CA-VALIDATED                      VALUE 'V'.
           05  CA-FROM-ACCT                PIC X(12).
           05  CA-TO-ACCT                  PIC X(12).
           05  CA-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  CA-CUST-ID                  PIC X(10).
           05  CA-MEMO                     PIC X(30).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(70).

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
           MOVE LOW-VALUES TO XFERFNMO
           PERFORM 8100-READ-CONTROL
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           MOVE 'ENTER FROM / TO ACCOUNTS AND AMOUNT' TO MSGO
           EXEC CICS SEND MAP('XFERFNM') MAPSET('XFERFNS')
                FROM(XFERFNMO) ERASE RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * ENTER - VALIDATE                                               *
      *----------------------------------------------------------------*
       2000-VALIDATE.
           EXEC CICS RECEIVE MAP('XFERFNM') MAPSET('XFERFNS')
                INTO(XFERFNMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           PERFORM 8100-READ-CONTROL
           PERFORM 2100-EDIT-SCREEN
           IF NOT WS-ERROR
               PERFORM 2200-READ-ACCOUNTS
           END-IF
           IF NOT WS-ERROR
               PERFORM 2300-CHECK-OWNERSHIP
           END-IF
           IF NOT WS-ERROR
               PERFORM 2350-TRANVAL-BOTH-LEGS
           END-IF
           IF NOT WS-ERROR
               PERFORM 2400-CHECK-FUNDS
           END-IF
           IF NOT WS-ERROR
               PERFORM 2500-REG-D-PREVIEW
           END-IF
           IF NOT WS-ERROR
               SET CA-VALIDATED TO TRUE
               MOVE FROMACI TO CA-FROM-ACCT
               MOVE TOACI   TO CA-TO-ACCT
               MOVE WS-AMOUNT TO CA-AMOUNT
               MOVE CUSTIDI TO CA-CUST-ID
               MOVE MEMOI   TO CA-MEMO
               IF WS-MESSAGE = SPACES
                   MOVE 'VALIDATED - ENTER Y IN CONFIRM AND PRESS PF5'
                     TO WS-MESSAGE
               END-IF
           ELSE
               MOVE SPACE TO CA-STATE
           END-IF
           PERFORM 8000-SEND-DATAONLY.

       2100-EDIT-SCREEN.
           EVALUATE TRUE
               WHEN FROMACL = 0 OR FROMACI NOT NUMERIC
                   MOVE 'FROM ACCOUNT MUST BE 12 DIGITS' TO WS-MESSAGE
                   MOVE -1 TO FROMACL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN TOACL = 0 OR TOACI NOT NUMERIC
                   MOVE 'TO ACCOUNT MUST BE 12 DIGITS' TO WS-MESSAGE
                   MOVE -1 TO TOACL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN FROMACI = TOACI
                   MOVE 'FROM AND TO ACCOUNTS MUST DIFFER'
                     TO WS-MESSAGE
                   MOVE -1 TO TOACL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN CUSTIDL = 0
                   MOVE 'REQUESTING CUSTOMER ID REQUIRED'
                     TO WS-MESSAGE
                   MOVE -1 TO CUSTIDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN FUNCTION TEST-NUMVAL(AMOUNTI) NOT = 0
                   MOVE 'AMOUNT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO AMOUNTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN OTHER
                   COMPUTE WS-AMOUNT = FUNCTION NUMVAL(AMOUNTI)
                   IF WS-AMOUNT NOT > 0
                       MOVE 'AMOUNT MUST BE GREATER THAN ZERO'
                         TO WS-MESSAGE
                       MOVE -1 TO AMOUNTL
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
           END-EVALUATE.

       2200-READ-ACCOUNTS.
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(FROMACI) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'FROM ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO FROMACL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE ACCOUNT-RECORD TO WS-FROM-ACCT
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(TOACI) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'TO ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO TOACL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE ACCOUNT-RECORD TO WS-TO-ACCT
           MOVE FR-PRODUCT-CODE TO FRPRDO
           MOVE TO-PRODUCT-CODE TO TOPRDO
           MOVE FR-AVAIL-BAL TO WS-AMT-ED
           MOVE WS-AMT-ED    TO FRAVLO
           MOVE FR-OD-LIMIT  TO WS-AMT-ED2
           MOVE WS-AMT-ED2   TO FRODLO
           MOVE TO-AVAIL-BAL TO WS-AMT-ED
           MOVE WS-AMT-ED    TO TOAVLO.

      *----------------------------------------------------------------*
      * REQUESTER MUST BE PRIMARY OR JOINT OWNER OF BOTH ACCOUNTS      *
      *----------------------------------------------------------------*
       2300-CHECK-OWNERSHIP.
           MOVE 'N' TO WS-OWNER-OK
           IF (FR-PRIMARY-CUST-ID = CUSTIDI
               OR FR-JOINT-CUST-ID = CUSTIDI)
              AND (TO-PRIMARY-CUST-ID = CUSTIDI
               OR TO-JOINT-CUST-ID = CUSTIDI)
               MOVE 'Y' TO WS-OWNER-OK
           END-IF
           IF WS-OWNER-OK = 'N'
               MOVE 'CUSTOMER IS NOT AN OWNER OF BOTH ACCOUNTS'
                 TO WS-MESSAGE
               MOVE -1 TO CUSTIDL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2350-TRANVAL-BOTH-LEGS.
      *    DEBIT LEG
           INITIALIZE TRANVAL-PARMS
           MOVE 'WXFR'              TO TRV-TRAN-CODE
           MOVE WS-AMOUNT           TO TRV-AMOUNT
           MOVE 'ONL'               TO TRV-CHANNEL
           MOVE CTL-BUSINESS-DATE   TO TRV-EFFECTIVE-DATE
                                       TRV-BUSINESS-DATE
           MOVE 'Y'                 TO TRV-ACCT-FOUND-FLAG
           MOVE FR-STATUS           TO TRV-ACCT-STATUS
           MOVE FR-CATEGORY         TO TRV-ACCT-CATEGORY
           MOVE FR-MATURITY-DATE    TO TRV-MATURITY-DATE
           MOVE FR-GRACE-END-DATE   TO TRV-GRACE-END-DATE
           MOVE 'Y'                 TO TRV-PROD-FOUND-FLAG
           MOVE 'N'                 TO TRV-ID-VERIFIED-FLAG
           MOVE FR-DAILY-WD-LIMIT   TO TRV-DAILY-WD-LIMIT
           MOVE FR-DAILY-WD-USED    TO TRV-DAILY-WD-USED
           EXEC CICS LINK PROGRAM('TRANVAL') COMMAREA(TRANVAL-PARMS)
                LENGTH(LENGTH OF TRANVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF NOT TRV-ACCEPTED
               STRING 'FROM: ' TRV-REJECT-CODE ' ' TRV-REJECT-TEXT
                      DELIMITED BY SIZE INTO WS-MESSAGE
               MOVE -1 TO FROMACL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
      *    CREDIT LEG
           MOVE 'DXFR'              TO TRV-TRAN-CODE
           MOVE TO-STATUS           TO TRV-ACCT-STATUS
           MOVE TO-CATEGORY         TO TRV-ACCT-CATEGORY
           MOVE TO-MATURITY-DATE    TO TRV-MATURITY-DATE
           MOVE TO-GRACE-END-DATE   TO TRV-GRACE-END-DATE
           EXEC CICS LINK PROGRAM('TRANVAL') COMMAREA(TRANVAL-PARMS)
                LENGTH(LENGTH OF TRANVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF NOT TRV-ACCEPTED
               STRING 'TO: ' TRV-REJECT-CODE ' ' TRV-REJECT-TEXT
                      DELIMITED BY SIZE INTO WS-MESSAGE
               MOVE -1 TO TOACL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AVAILABLE FUNDS / OVERDRAFT - ONLINE TRANSFERS                 *
      *   AVAILABLE AFTER TRANSFER >= 0           ALLOWED              *
      *   INTO OVERDRAFT: CUSTOMER MUST BE OD OPTED-IN AND THE         *
      *   OVERDRAFT MUST STAY UNDER THE OD LIMIT. NO OD FEE ONLINE.    *
      *   (INC09981 - ODDECIDE NOT LINKED FROM CB10)                   *
      *----------------------------------------------------------------*
       2400-CHECK-FUNDS.
           COMPUTE WS-PROJ-AVAIL = FR-AVAIL-BAL - WS-AMOUNT
           MOVE ZERO TO WS-OD-USED
           IF WS-PROJ-AVAIL < 0
               COMPUTE WS-OD-USED = 0 - WS-PROJ-AVAIL
               EVALUATE TRUE
                   WHEN FR-OD-OPT-IN NOT = 'Y'
                       MOVE 'INSUFFICIENT FUNDS - NOT ENROLLED IN OD'
                         TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   WHEN WS-OD-USED NOT < FR-OD-LIMIT
                       MOVE 'INSUFFICIENT FUNDS - EXCEEDS OD LIMIT'
                         TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   WHEN OTHER
                       MOVE 'TRANSFER WILL OVERDRAW ACCOUNT - CONFIRM'
                         TO WS-MESSAGE
               END-EVALUATE
               IF WS-ERROR
                   MOVE -1 TO AMOUNTL
               END-IF
           END-IF
           MOVE WS-PROJ-AVAIL TO WS-AMT-ED
           MOVE WS-AMT-ED     TO PROJBLO
           MOVE WS-OD-USED    TO WS-AMT-ED2
           MOVE WS-AMT-ED2    TO ODUSEDO.

      *----------------------------------------------------------------*
      * REG D PREVIEW - COUNT IS NOT UPDATED ONLINE (POSTTRAN DOES IT) *
      *----------------------------------------------------------------*
       2500-REG-D-PREVIEW.
           MOVE FR-PRODUCT-CODE TO PROD-CODE
           EXEC CICS READ FILE('PRODMST') INTO(PRODUCT-RECORD)
                RIDFLD(PROD-CODE) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'N' TO PROD-REG-D-FLAG
           END-IF
           INITIALIZE REGDCHK-PARMS
           MOVE FR-CATEGORY            TO RGD-ACCT-CATEGORY
           MOVE PROD-REG-D-FLAG        TO RGD-PROD-REGD-FLAG
           MOVE 'WXFR'                 TO RGD-TRAN-CODE
           MOVE 'Y'                    TO RGD-TRAN-REGD-FLAG
           MOVE 'ONL'                  TO RGD-CHANNEL
           MOVE FR-REGD-WD-COUNT       TO RGD-WD-COUNT
           MOVE FR-REGD-VIOLATIONS-12M TO RGD-VIOLATIONS-12M
           EXEC CICS LINK PROGRAM('REGDCHK') COMMAREA(REGDCHK-PARMS)
                LENGTH(LENGTH OF REGDCHK-PARMS) RESP(WS-RESP)
           END-EXEC
           MOVE RGD-WD-COUNT TO WS-CNT-ED
           MOVE WS-CNT-ED    TO REGDCTO
           MOVE RGD-FEE-AMOUNT TO WS-AMT-ED2
           MOVE WS-AMT-ED2(6:10) TO REGDFEO
           IF RGD-IS-EXCESS AND WS-MESSAGE = SPACES
               MOVE 'EXCEEDS 6 CONVENIENT WITHDRAWALS - $10.00 FEE'
                 TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
      * PF5 - CONFIRM: MEMO POST BOTH ACCOUNTS, QUEUE FOR BATCH        *
      *----------------------------------------------------------------*
       3000-CONFIRM.
           EXEC CICS RECEIVE MAP('XFERFNM') MAPSET('XFERFNS')
                INTO(XFERFNMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF NOT CA-VALIDATED
               MOVE 'PRESS ENTER TO VALIDATE FIRST' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CONFRMI NOT = 'Y'
               MOVE 'ENTER Y IN CONFIRM TO PROCEED' TO WS-MESSAGE
               MOVE -1 TO CONFRML
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE CA-AMOUNT TO WS-AMOUNT
           PERFORM 8200-NEXT-TRAN-SEQ

      *    DEBIT LEG - RE-READ FOR UPDATE AND RE-CHECK FUNDS
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(CA-FROM-ACCT) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'FROM ACCOUNT UNAVAILABLE - TRY AGAIN'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE ACCOUNT-RECORD TO WS-FROM-ACCT
           PERFORM 2400-CHECK-FUNDS
           IF WS-ERROR
               EXEC CICS UNLOCK FILE('ACCTMST') END-EXEC
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           SUBTRACT WS-AMOUNT FROM ACCT-AVAIL-BAL
           ADD WS-AMOUNT      TO ACCT-MEMO-DR-TODAY
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                RESP(WS-RESP) END-EXEC

      *    CREDIT LEG
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(CA-TO-ACCT) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE 'TO ACCOUNT UNAVAILABLE - TRANSFER BACKED OUT'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           ADD WS-AMOUNT TO ACCT-AVAIL-BAL
           ADD WS-AMOUNT TO ACCT-MEMO-CR-TODAY
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                RESP(WS-RESP) END-EXEC

      *    QUEUE BOTH LEGS FOR THE NIGHTLY POSTING RUN
           MOVE 'WXFR' TO TRAN-CODE
           MOVE CA-FROM-ACCT TO TRAN-ACCT-NUMBER
           MOVE CA-TO-ACCT   TO TRAN-COUNTER-ACCT
           PERFORM 3100-WRITE-TRANQUE
           MOVE 'DXFR' TO TRAN-CODE
           MOVE CA-TO-ACCT   TO TRAN-ACCT-NUMBER
           MOVE CA-FROM-ACCT TO TRAN-COUNTER-ACCT
           PERFORM 3100-WRITE-TRANQUE

           EXEC CICS SYNCPOINT END-EXEC
           MOVE TRAN-ID TO XFRREFO
           MOVE 'TRANSFER COMPLETE - FUNDS AVAILABLE NOW' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       3100-WRITE-TRANQUE.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                TIME(WS-TIME-HHMMSS) END-EXEC
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE
                                     TRAN-EFFECTIVE-DATE
           MOVE WS-SEQ            TO TRAN-ID-SEQ
           ADD 1 TO WS-SEQ
           MOVE ZERO              TO TRAN-POST-DATE TRAN-HOLD-DAYS
                                     TRAN-FEE-AMOUNT
           MOVE WS-TIME-HHMMSS    TO TRAN-TIME
           SET TC-IDX TO 1
           SEARCH TC-ENTRY
               WHEN TC-CODE(TC-IDX) = TRAN-CODE
                   MOVE TC-DR-CR(TC-IDX)     TO TRAN-DR-CR
                   MOVE TC-CASH-FLAG(TC-IDX) TO TRAN-CASH-FLAG
                   MOVE TC-REGD-FLAG(TC-IDX) TO TRAN-REGD-FLAG
           END-SEARCH
           MOVE WS-AMOUNT         TO TRAN-AMOUNT
           MOVE 'ONL'             TO TRAN-CHANNEL
           IF CA-MEMO = SPACES
               MOVE 'ONLINE TRANSFER' TO TRAN-DESCRIPTION
           ELSE
               MOVE CA-MEMO TO TRAN-DESCRIPTION
           END-IF
           MOVE 'CB10 XFERFUND'   TO TRAN-REFERENCE
           MOVE SPACES            TO TRAN-COUNTERPARTY-NAME
                                     TRAN-COUNTERPARTY-ROUTING
                                     TRAN-COUNTERPARTY-COUNTRY
                                     TRAN-REJECT-CODE TRAN-ORIG-TRAN-ID
                                     TRAN-GL-ACCOUNT TRAN-COST-CENTER
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
           EXEC CICS SEND MAP('XFERFNM') MAPSET('XFERFNS')
                FROM(XFERFNMO) DATAONLY CURSOR RESP(WS-RESP)
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
           ADD 2 TO CTL-LAST-TRAN-SEQ
           COMPUTE WS-SEQ = 5000000
                          + FUNCTION MOD(CTL-LAST-TRAN-SEQ, 4999998)
           EXEC CICS REWRITE FILE('BANKCTL')
                FROM(BANK-CONTROL-RECORD) RESP(WS-RESP) END-EXEC.
