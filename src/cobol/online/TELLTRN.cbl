      *================================================================*
      * PROGRAM  : TELLTRN                                             *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CB11       MAPSET TELLTRS  MAP TELLTRM              *
      * PURPOSE  : TELLER WINDOW TRANSACTIONS                          *
      *              D  CASH DEPOSIT        (DCSH)                     *
      *              W  CASH WITHDRAWAL     (WCSH)                     *
      *              C  CHECK DEPOSIT       (DCHK) - HOLD DISCLOSURE   *
      *            RULES:                                              *
      *            - EDITS VIA TRANVAL (CHANNEL BRN, ID VERIFIED FLAG  *
      *              FROM THE SCREEN - REQUIRED FOR DORMANT ACCOUNTS)  *
      *            - CTR: WHEN CASH IN (OR OUT) TODAY PLUS THIS ITEM   *
      *              REACHES $10,000 THE CONDUCTOR'S NAME, DOB,        *
      *              ADDRESS, ID AND OCCUPATION MUST BE CAPTURED AND   *
      *              ARE QUEUED TO TD QUEUE CTRQ FOR CTRSCAN / FINCEN  *
      *            - WITHDRAWAL > $5,000 NEEDS SUPERVISOR OVERRIDE     *
      *              (SUP OR MGR, WITHIN THEIR OVERRIDE LIMIT)         *
      *            - WITHDRAWAL > AVAILABLE BALANCE NEEDS A MANAGER    *
      *              OVERRIDE AND MAY NOT EXCEED THE OD LIMIT          *
      *            - TELLER DRAWER CASH-OUT LIMIT BY ROLE              *
      *            CASH IS MEMO-POSTED (AVAILABLE, MEMO, CASH TODAY)   *
      *            AND QUEUED TO TRANQUE FOR THE NIGHTLY POSTING RUN.  *
      * FILES    : ACCTMST (UPDATE), TELLMST (UPDATE), BANKCTL,        *
      *            TRANQUE (ESDS), TD QUEUE CTRQ                       *
      * LINKS    : TRANVAL; CALLS DATEUTIL                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-10-02 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-09-10 BHS  CTR CONDUCTOR CAPTURE - BSA / 31 CFR 103.22    *
      * 2006-05-22 BHS  SUPERVISOR OVERRIDE OVER $5,000 (AUD-06-114)   *
      * 2008-02-11 BHS  DRAWER LIMITS BY ROLE                          *
      * 2011-07-21 DPS  CHECK DEPOSIT WITH REG CC HOLD DISCLOSURE      *
      * 2012-04-30 DPS  TRANVAL EDITS                                  *
      * 2016-02-08 DPS  RESP CHECKING - REMOVED HANDLE CONDITION       *
      * 2020-07-01 AKW  REG-2020-CC DISCLOSURE AMOUNTS                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TELLTRN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM                      PIC X(08) VALUE 'TELLTRN'.
       01  WS-TRANID                       PIC X(04) VALUE 'CB11'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-USERID                       PIC X(08).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-RBA                          PIC S9(08) COMP.
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-TIME-HHMMSS                  PIC 9(06).

      *----------------------------------------------------------------*
      * TELLER POLICY                                                  *
      *----------------------------------------------------------------*
       01  WS-TELLER-POLICY.
           05  WS-CTR-THRESHOLD            PIC S9(07)V99 VALUE 10000.00.
           05  WS-OVERRIDE-THRESHOLD       PIC S9(07)V99 VALUE 5000.00.
      *    REG CC DISCLOSURE - SEE REGCCHLD FOR THE HOLD ITSELF
           05  WS-DISC-NEXT-DAY-AMT        PIC S9(07)V99 VALUE 225.00.
           05  WS-DISC-LARGE-DEP-AMT       PIC S9(07)V99 VALUE 5525.00.
       01  WS-DRAWER-LIMIT-VALUES.
           05  FILLER PIC X(14) VALUE 'TLR00001500000'.
           05  FILLER PIC X(14) VALUE 'SRT00002500000'.
           05  FILLER PIC X(14) VALUE 'SUP00005000000'.
           05  FILLER PIC X(14) VALUE 'MGR00010000000'.
           05  FILLER PIC X(14) VALUE 'OPS00000000000'.
       01  WS-DRAWER-LIMITS REDEFINES WS-DRAWER-LIMIT-VALUES.
           05  WS-DL-ENTRY OCCURS 5 TIMES INDEXED BY DL-IDX.
               10  WS-DL-ROLE              PIC X(03).
               10  WS-DL-LIMIT             PIC 9(09)V99.

       01  WS-WORK.
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-CASH-TODAY               PIC S9(13)V99 COMP-3.
           05  WS-DRAWER-LIMIT             PIC S9(11)V99 COMP-3.
           05  WS-PROJ-AVAIL               PIC S9(13)V99 COMP-3.
           05  WS-TIER-1                   PIC S9(13)V99 COMP-3.
           05  WS-TIER-2                   PIC S9(13)V99 COMP-3.
           05  WS-TIER-3                   PIC S9(13)V99 COMP-3.
           05  WS-TRAN-CODE                PIC X(04).
           05  WS-CTR-REQUIRED             PIC X(01).
               88  WS-CTR-NEEDED                     VALUE 'Y'.
           05  WS-OVERRIDE-NEEDED          PIC X(01).
               88  WS-SUP-OVERRIDE                   VALUE 'S'.
               88  WS-MGR-OVERRIDE                   VALUE 'M'.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-ERROR                          VALUE 'Y'.
           05  WS-MESSAGE                  PIC X(79).
           05  WS-AMT-ED                   PIC ZZ,ZZZ,ZZ9.99-.
           05  WS-AMT-ED2                  PIC ZZZ,ZZ9.99.
           05  WS-AMT-ED3                  PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-DATE-ED                  PIC 9999/99/99.
           05  WS-DATE-ED2                 PIC 9999/99/99.
           05  WS-DATE-ED3                 PIC 9999/99/99.
           05  WS-SEQ                      PIC 9(08).

      *    CTR CONDUCTOR CAPTURE - WRITTEN TO TD QUEUE CTRQ
       01  WS-CTR-CAPTURE.
           05  CTRC-DATE                   PIC 9(08).
           05  CTRC-TIME                   PIC 9(06).
           05  CTRC-ACCT-NUMBER            PIC X(12).
           05  CTRC-CUST-ID                PIC X(10).
           05  CTRC-TRAN-CODE              PIC X(04).
           05  CTRC-AMOUNT                 PIC S9(13)V99 COMP-3.
           05  CTRC-CASH-TODAY             PIC S9(13)V99 COMP-3.
           05  CTRC-COND-NAME              PIC X(30).
           05  CTRC-COND-DOB               PIC X(08).
           05  CTRC-COND-ADDRESS           PIC X(40).
           05  CTRC-COND-ID-TYPE           PIC X(02).
           05  CTRC-COND-ID-NUMBER         PIC X(20).
           05  CTRC-COND-ID-ISSUER         PIC X(02).
           05  CTRC-COND-OCCUPATION        PIC X(20).
           05  CTRC-ON-BEHALF-FLAG         PIC X(01).
           05  CTRC-TELLER-ID              PIC X(08).
           05  CTRC-BRANCH                 PIC X(05).

       COPY ACCTREC.
       COPY TLRREC.
       COPY TLRREC REPLACING ==TELLER-RECORD== BY ==WS-SUPERVISOR==
                             LEADING ==TLR-== BY ==SUP-==.
       COPY BANKCTL.
       COPY TRANREC.
       COPY TRANCODE.
       COPY TRVPARM.
       COPY DATEPARM.
       COPY TELLTRS.
       COPY DFHAID.
       COPY DFHBMSCA.

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-VALIDATED                      VALUE 'V'.
           05  CA-FUNCTION                 PIC X(01).
           05  CA-ACCT                     PIC X(12).
           05  CA-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  CA-CTR-REQUIRED             PIC X(01).

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(23).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           PERFORM 8100-READ-CONTROL
           PERFORM 8300-READ-TELLER
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
                       PERFORM 2000-VALIDATE
                       IF NOT WS-ERROR
                           PERFORM 3000-POST
                       END-IF
                   WHEN OTHER
                       MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                       PERFORM 8000-SEND-DATAONLY
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANID)
                COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-SEND-INITIAL.
           MOVE LOW-VALUES TO TELLTRMO
           PERFORM 8400-SHOW-TELLER
           MOVE 'SELECT FUNCTION, ENTER ACCOUNT AND AMOUNT' TO MSGO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           EXEC CICS SEND MAP('TELLTRM') MAPSET('TELLTRS')
                FROM(TELLTRMO) ERASE RESP(WS-RESP) END-EXEC.

      *----------------------------------------------------------------*
      * VALIDATION (ENTER AND PF5)                                     *
      *----------------------------------------------------------------*
       2000-VALIDATE.
           EXEC CICS RECEIVE MAP('TELLTRM') MAPSET('TELLTRS')
                INTO(TELLTRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG WS-CTR-REQUIRED
           MOVE SPACE TO WS-OVERRIDE-NEEDED
           MOVE SPACES TO WS-MESSAGE HLDTX1O HLDTX2O
           PERFORM 8400-SHOW-TELLER
           EVALUATE FUNCI
               WHEN 'D'  MOVE 'DCSH' TO WS-TRAN-CODE
               WHEN 'W'  MOVE 'WCSH' TO WS-TRAN-CODE
               WHEN 'C'  MOVE 'DCHK' TO WS-TRAN-CODE
               WHEN OTHER
                   MOVE 'FUNCTION MUST BE D, W OR C' TO WS-MESSAGE
                   MOVE -1 TO FUNCL
                   MOVE 'Y' TO WS-ERROR-FLAG
           END-EVALUATE
           IF NOT WS-ERROR
               PERFORM 2100-EDIT-AMOUNT-ACCOUNT
           END-IF
           IF NOT WS-ERROR
               PERFORM 2200-TRANVAL
           END-IF
           IF NOT WS-ERROR AND WS-TRAN-CODE NOT = 'DCHK'
               PERFORM 2300-CTR-CHECK
           END-IF
           IF NOT WS-ERROR AND WS-TRAN-CODE = 'WCSH'
               PERFORM 2400-WITHDRAWAL-CONTROLS
           END-IF
           IF NOT WS-ERROR AND WS-TRAN-CODE = 'DCHK'
               PERFORM 2500-HOLD-DISCLOSURE
           END-IF
           IF NOT WS-ERROR
               SET CA-VALIDATED TO TRUE
               MOVE FUNCI   TO CA-FUNCTION
               MOVE ACCTNOI TO CA-ACCT
               MOVE WS-AMOUNT TO CA-AMOUNT
               MOVE WS-CTR-REQUIRED TO CA-CTR-REQUIRED
               IF WS-MESSAGE = SPACES
                   MOVE 'VALIDATED - PRESS PF5 TO POST' TO WS-MESSAGE
               END-IF
           ELSE
               MOVE SPACE TO CA-STATE
           END-IF
           IF EIBAID NOT = DFHPF5 OR WS-ERROR
               PERFORM 8000-SEND-DATAONLY
           END-IF.

       2100-EDIT-AMOUNT-ACCOUNT.
           IF FUNCTION TEST-NUMVAL(AMOUNTI) NOT = 0
               MOVE 'AMOUNT IS NOT NUMERIC' TO WS-MESSAGE
               MOVE -1 TO AMOUNTL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(AMOUNTI)
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(ACCTNOI) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-AVAIL-BAL     TO WS-AMT-ED
           MOVE WS-AMT-ED          TO AVAILO
           MOVE ACCT-CASH-IN-TODAY TO WS-AMT-ED
           MOVE WS-AMT-ED          TO CSHINO
           MOVE ACCT-CASH-OUT-TODAY TO WS-AMT-ED
           MOVE WS-AMT-ED          TO CSHOUTO
           EVALUATE TRUE
               WHEN ACCT-ACTIVE      MOVE 'ACTIVE'     TO ASTATO
               WHEN ACCT-DORMANT     MOVE 'DORMANT'    TO ASTATO
               WHEN ACCT-FROZEN      MOVE 'FROZEN'     TO ASTATO
               WHEN ACCT-RESTRICTED  MOVE 'RESTRICTED' TO ASTATO
               WHEN OTHER            MOVE ACCT-STATUS  TO ASTATO
           END-EVALUATE.

       2200-TRANVAL.
           INITIALIZE TRANVAL-PARMS
           MOVE WS-TRAN-CODE        TO TRV-TRAN-CODE
           MOVE WS-AMOUNT           TO TRV-AMOUNT
           MOVE 'BRN'               TO TRV-CHANNEL
           MOVE CTL-BUSINESS-DATE   TO TRV-EFFECTIVE-DATE
                                       TRV-BUSINESS-DATE
           MOVE 'Y'                 TO TRV-ACCT-FOUND-FLAG
                                       TRV-PROD-FOUND-FLAG
           MOVE ACCT-STATUS         TO TRV-ACCT-STATUS
           MOVE ACCT-CATEGORY       TO TRV-ACCT-CATEGORY
           MOVE ACCT-MATURITY-DATE  TO TRV-MATURITY-DATE
           MOVE ACCT-GRACE-END-DATE TO TRV-GRACE-END-DATE
           MOVE ACCT-DAILY-WD-LIMIT TO TRV-DAILY-WD-LIMIT
           MOVE ACCT-DAILY-WD-USED  TO TRV-DAILY-WD-USED
           IF IDVERI = 'Y'
               MOVE 'Y' TO TRV-ID-VERIFIED-FLAG
           ELSE
               MOVE 'N' TO TRV-ID-VERIFIED-FLAG
           END-IF
           EXEC CICS LINK PROGRAM('TRANVAL') COMMAREA(TRANVAL-PARMS)
                LENGTH(LENGTH OF TRANVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF NOT TRV-ACCEPTED
               STRING TRV-REJECT-CODE ' ' TRV-REJECT-TEXT
                      DELIMITED BY SIZE INTO WS-MESSAGE
               MOVE -1 TO ACCTNOL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CTR - CASH TODAY INCLUDING THIS ITEM AT OR OVER $10,000        *
      *----------------------------------------------------------------*
       2300-CTR-CHECK.
           IF WS-TRAN-CODE = 'DCSH'
               COMPUTE WS-CASH-TODAY = ACCT-CASH-IN-TODAY + WS-AMOUNT
           ELSE
               COMPUTE WS-CASH-TODAY = ACCT-CASH-OUT-TODAY + WS-AMOUNT
           END-IF
           IF WS-CASH-TODAY >= WS-CTR-THRESHOLD
               MOVE 'Y' TO WS-CTR-REQUIRED
               EVALUATE TRUE
                   WHEN CNDNML = 0 OR CNDADRL = 0 OR CNDOCCL = 0
                       MOVE 'CTR REQUIRED - CONDUCTOR NAME/ADDR/OCC'
                         TO WS-MESSAGE
                       MOVE -1 TO CNDNML
                       MOVE 'Y' TO WS-ERROR-FLAG
                   WHEN CNDDOBI NOT NUMERIC
                       MOVE 'CTR REQUIRED - CONDUCTOR DOB CCYYMMDD'
                         TO WS-MESSAGE
                       MOVE -1 TO CNDDOBL
                       MOVE 'Y' TO WS-ERROR-FLAG
                   WHEN CNDIDTI NOT = 'DL' AND NOT = 'PP'
                                    AND NOT = 'SC' AND NOT = 'MI'
                       MOVE 'CTR - ID TYPE DL, PP, SC (STATE ID) OR MI'
                         TO WS-MESSAGE
                       MOVE -1 TO CNDIDTL
                       MOVE 'Y' TO WS-ERROR-FLAG
                   WHEN CNDIDNL = 0
                       MOVE 'CTR REQUIRED - CONDUCTOR ID NUMBER'
                         TO WS-MESSAGE
                       MOVE -1 TO CNDIDNL
                       MOVE 'Y' TO WS-ERROR-FLAG
                   WHEN OTHER
                       MOVE 'CTR WILL BE FILED - CONDUCTOR CAPTURED'
                         TO WS-MESSAGE
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
      * CASH WITHDRAWAL CONTROLS                                       *
      *   > $5,000              SUPERVISOR (SUP/MGR) OVERRIDE          *
      *   > AVAILABLE BALANCE   MANAGER OVERRIDE, WITHIN OD LIMIT      *
      *   DRAWER CASH-OUT       TLR 15,000 SRT 25,000 SUP 50,000       *
      *                         MGR 100,000 OPS NONE                   *
      *----------------------------------------------------------------*
       2400-WITHDRAWAL-CONTROLS.
           COMPUTE WS-PROJ-AVAIL = ACCT-AVAIL-BAL - WS-AMOUNT
           EVALUATE TRUE
               WHEN WS-PROJ-AVAIL < 0
                   SET WS-MGR-OVERRIDE TO TRUE
                   IF (0 - WS-PROJ-AVAIL) > ACCT-OD-LIMIT
                       MOVE 'EXCEEDS AVAILABLE PLUS OD LIMIT - DECLINE'
                         TO WS-MESSAGE
                       MOVE -1 TO AMOUNTL
                       MOVE 'Y' TO WS-ERROR-FLAG
                       EXIT PARAGRAPH
                   END-IF
               WHEN WS-AMOUNT > WS-OVERRIDE-THRESHOLD
                   SET WS-SUP-OVERRIDE TO TRUE
           END-EVALUATE
           IF WS-SUP-OVERRIDE OR WS-MGR-OVERRIDE
               PERFORM 2410-VERIFY-OVERRIDE
               IF WS-ERROR
                   EXIT PARAGRAPH
               END-IF
           END-IF
           SET DL-IDX TO 1
           MOVE ZERO TO WS-DRAWER-LIMIT
           SEARCH WS-DL-ENTRY
               WHEN WS-DL-ROLE(DL-IDX) = TLR-ROLE
                   MOVE WS-DL-LIMIT(DL-IDX) TO WS-DRAWER-LIMIT
           END-SEARCH
           IF TLR-CASH-OUT-TODAY + WS-AMOUNT > WS-DRAWER-LIMIT
               MOVE 'DRAWER CASH-OUT LIMIT - BUY CASH FROM VAULT'
                 TO WS-MESSAGE
               MOVE -1 TO AMOUNTL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2410-VERIFY-OVERRIDE.
           IF SUPIDL = 0 OR SUPIDI = WS-USERID
               MOVE 'SUPERVISOR OVERRIDE REQUIRED (NOT SELF)'
                 TO WS-MESSAGE
               MOVE -1 TO SUPIDL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('TELLMST') INTO(WS-SUPERVISOR)
                RIDFLD(SUPIDI) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR NOT SUP-ACTIVE
               MOVE 'OVERRIDE ID NOT FOUND OR INACTIVE' TO WS-MESSAGE
               MOVE -1 TO SUPIDL
               MOVE 'Y' TO WS-ERROR-FLAG
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN WS-MGR-OVERRIDE AND NOT SUP-ROLE-MANAGER
                   MOVE 'WITHDRAWAL OVER AVAILABLE NEEDS MANAGER'
                     TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-SUP-OVERRIDE AND NOT SUP-ROLE-SUPERVISOR
                                    AND NOT SUP-ROLE-MANAGER
                   MOVE 'OVERRIDE NEEDS SUPERVISOR OR MANAGER'
                     TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
               WHEN WS-AMOUNT > SUP-OVERRIDE-LIMIT
                   MOVE 'AMOUNT EXCEEDS APPROVER OVERRIDE LIMIT'
                     TO WS-MESSAGE
                   MOVE 'Y' TO WS-ERROR-FLAG
           END-EVALUATE
           IF WS-ERROR
               MOVE -1 TO SUPIDL
               EXIT PARAGRAPH
           END-IF
           EXEC CICS VERIFY PASSWORD(SUPPWDI) USERID(SUPIDI)
                RESP(WS-RESP) RESP2(WS-RESP2) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'OVERRIDE PASSWORD NOT VERIFIED' TO WS-MESSAGE
               MOVE -1 TO SUPPWDL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHECK DEPOSIT - CUSTOMER HOLD DISCLOSURE (RECEIPT TEXT)        *
      *   FIRST $225 NEXT BUSINESS DAY, NEXT PORTION UP TO $5,525 ON   *
      *   THE 2ND BUSINESS DAY, REMAINDER ON THE 7TH BUSINESS DAY.     *
      *   THE ACTUAL HOLD IS PLACED BY POSTTRAN (REGCCHLD).            *
      *----------------------------------------------------------------*
       2500-HOLD-DISCLOSURE.
           MOVE ZERO TO WS-TIER-1 WS-TIER-2 WS-TIER-3
           IF WS-AMOUNT > WS-DISC-NEXT-DAY-AMT
               MOVE WS-DISC-NEXT-DAY-AMT TO WS-TIER-1
           ELSE
               MOVE WS-AMOUNT TO WS-TIER-1
           END-IF
           IF WS-AMOUNT > WS-DISC-LARGE-DEP-AMT
               COMPUTE WS-TIER-2 = WS-DISC-LARGE-DEP-AMT - WS-TIER-1
               COMPUTE WS-TIER-3 = WS-AMOUNT - WS-DISC-LARGE-DEP-AMT
           ELSE
               COMPUTE WS-TIER-2 = WS-AMOUNT - WS-TIER-1
           END-IF
           INITIALIZE DATE-PARMS
           SET DTU-ADD-BUS-DAYS TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           MOVE 1 TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-DATE-ED
           MOVE 2 TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-DATE-ED2
           MOVE 7 TO DTU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-DATE TO WS-DATE-ED3
           MOVE WS-TIER-1 TO WS-AMT-ED2
           MOVE WS-TIER-2 TO WS-AMT-ED3
           STRING 'FUNDS: ' WS-AMT-ED2 ' ON ' WS-DATE-ED
                  '  ' WS-AMT-ED3 ' ON ' WS-DATE-ED2
                  DELIMITED BY SIZE INTO HLDTX1O
           IF WS-TIER-3 > 0
               MOVE WS-TIER-3 TO WS-AMT-ED3
               STRING '       ' WS-AMT-ED3 ' ON ' WS-DATE-ED3
                      '  (LARGE DEPOSIT - EXCEPTIONS MAY APPLY)'
                      DELIMITED BY SIZE INTO HLDTX2O
           ELSE
               MOVE 'ADDITIONAL HOLDS MAY APPLY - SEE FUNDS NOTICE'
                 TO HLDTX2O
           END-IF.

      *----------------------------------------------------------------*
      * PF5 - MEMO POST, UPDATE DRAWER, QUEUE TO BATCH, CTR CAPTURE    *
      *----------------------------------------------------------------*
       3000-POST.
           PERFORM 8200-NEXT-TRAN-SEQ
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                RIDFLD(CA-ACCT) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           EVALUATE WS-TRAN-CODE
               WHEN 'DCSH'
                   ADD WS-AMOUNT TO ACCT-AVAIL-BAL
                                    ACCT-MEMO-CR-TODAY
                                    ACCT-CASH-IN-TODAY
               WHEN 'WCSH'
                   SUBTRACT WS-AMOUNT FROM ACCT-AVAIL-BAL
                   ADD WS-AMOUNT TO ACCT-MEMO-DR-TODAY
                                    ACCT-CASH-OUT-TODAY
      *        CHECK DEPOSITS ARE NOT MEMO POSTED - POSTTRAN PLACES
      *        THE REG CC HOLD AND MAKES THE FUNDS AVAILABLE
               WHEN 'DCHK'
                   CONTINUE
           END-EVALUATE
           IF TRV-REACTIVATE-FLAG = 'Y'
               MOVE CTL-BUSINESS-DATE TO ACCT-LAST-ACTIVITY-DATE
           END-IF
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                RESP(WS-RESP) END-EXEC
           EXEC CICS READ FILE('TELLMST') INTO(TELLER-RECORD)
                RIDFLD(WS-USERID) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               EVALUATE WS-TRAN-CODE
                   WHEN 'DCSH' ADD WS-AMOUNT TO TLR-CASH-IN-TODAY
                   WHEN 'WCSH' ADD WS-AMOUNT TO TLR-CASH-OUT-TODAY
               END-EVALUATE
               EXEC CICS REWRITE FILE('TELLMST') FROM(TELLER-RECORD)
                    RESP(WS-RESP) END-EXEC
           END-IF
           PERFORM 3100-WRITE-TRANQUE
           IF CA-CTR-REQUIRED = 'Y'
               PERFORM 3200-WRITE-CTR-CAPTURE
           END-IF
           EXEC CICS SYNCPOINT END-EXEC
           MOVE TRAN-ID TO TRNREFO
           MOVE ACCT-AVAIL-BAL TO WS-AMT-ED
           MOVE WS-AMT-ED TO NEWAVLO
           MOVE 'POSTED - GIVE RECEIPT TO CUSTOMER' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       3100-WRITE-TRANQUE.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                TIME(WS-TIME-HHMMSS) END-EXEC
           INITIALIZE TRANSACTION-RECORD
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE TRAN-EFFECTIVE-DATE
           MOVE WS-SEQ            TO TRAN-ID-SEQ
           MOVE CA-ACCT           TO TRAN-ACCT-NUMBER
           MOVE WS-TIME-HHMMSS    TO TRAN-TIME
           MOVE WS-TRAN-CODE      TO TRAN-CODE
           MOVE TRV-DR-CR         TO TRAN-DR-CR
           MOVE TRV-CASH-FLAG     TO TRAN-CASH-FLAG
           MOVE TRV-REGD-FLAG     TO TRAN-REGD-FLAG
           MOVE WS-AMOUNT         TO TRAN-AMOUNT
           MOVE 'BRN'             TO TRAN-CHANNEL
           EVALUATE WS-TRAN-CODE
               WHEN 'DCSH' MOVE 'TELLER CASH DEPOSIT'
                             TO TRAN-DESCRIPTION
               WHEN 'WCSH' MOVE 'TELLER CASH WITHDRAWAL'
                             TO TRAN-DESCRIPTION
               WHEN 'DCHK' MOVE 'TELLER CHECK DEPOSIT'
                             TO TRAN-DESCRIPTION
           END-EVALUATE
           STRING 'CB11 ' TLR-BRANCH ' ' TLR-DRAWER-ID
                  DELIMITED BY SIZE INTO TRAN-REFERENCE
           SET TRAN-PENDING TO TRUE
           MOVE WS-USERID         TO TRAN-USER-ID
           MOVE TLR-BRANCH        TO TRAN-COST-CENTER
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

       3200-WRITE-CTR-CAPTURE.
           MOVE CTL-BUSINESS-DATE   TO CTRC-DATE
           MOVE WS-TIME-HHMMSS      TO CTRC-TIME
           MOVE CA-ACCT             TO CTRC-ACCT-NUMBER
           MOVE ACCT-PRIMARY-CUST-ID TO CTRC-CUST-ID
           MOVE WS-TRAN-CODE        TO CTRC-TRAN-CODE
           MOVE WS-AMOUNT           TO CTRC-AMOUNT
           MOVE WS-CASH-TODAY       TO CTRC-CASH-TODAY
           MOVE CNDNMI              TO CTRC-COND-NAME
           MOVE CNDDOBI             TO CTRC-COND-DOB
           MOVE CNDADRI             TO CTRC-COND-ADDRESS
           MOVE CNDIDTI             TO CTRC-COND-ID-TYPE
           MOVE CNDIDNI             TO CTRC-COND-ID-NUMBER
           MOVE CNDIDSI             TO CTRC-COND-ID-ISSUER
           MOVE CNDOCCI             TO CTRC-COND-OCCUPATION
           MOVE CNDOBOI             TO CTRC-ON-BEHALF-FLAG
           MOVE WS-USERID           TO CTRC-TELLER-ID
           MOVE TLR-BRANCH          TO CTRC-BRANCH
           EXEC CICS WRITEQ TD QUEUE('CTRQ') FROM(WS-CTR-CAPTURE)
                LENGTH(LENGTH OF WS-CTR-CAPTURE) RESP(WS-RESP)
           END-EXEC.

      *----------------------------------------------------------------*
      * COMMON ROUTINES                                                *
      *----------------------------------------------------------------*
       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           MOVE CTL-BUSINESS-DATE TO WS-DATE-ED
           MOVE WS-DATE-ED TO CURDATEO
           EXEC CICS SEND MAP('TELLTRM') MAPSET('TELLTRS')
                FROM(TELLTRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-READ-CONTROL.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) RESP(WS-RESP) END-EXEC.

       8200-NEXT-TRAN-SEQ.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) UPDATE RESP(WS-RESP) END-EXEC
           ADD 1 TO CTL-LAST-TRAN-SEQ
           COMPUTE WS-SEQ = 5000000
                          + FUNCTION MOD(CTL-LAST-TRAN-SEQ, 4999998)
           EXEC CICS REWRITE FILE('BANKCTL')
                FROM(BANK-CONTROL-RECORD) RESP(WS-RESP) END-EXEC.

       8300-READ-TELLER.
           EXEC CICS READ FILE('TELLMST') INTO(TELLER-RECORD)
                RIDFLD(WS-USERID) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR NOT TLR-ACTIVE
               MOVE 'TELLER NOT AUTHORISED FOR CB11' TO WS-MESSAGE
               EXEC CICS SEND TEXT FROM(WS-MESSAGE) ERASE FREEKB
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.

       8400-SHOW-TELLER.
           MOVE WS-USERID TO TLRIDO
           MOVE TLR-CASH-OUT-TODAY TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRWOUTO
           SET DL-IDX TO 1
           SEARCH WS-DL-ENTRY
               WHEN WS-DL-ROLE(DL-IDX) = TLR-ROLE
                   MOVE WS-DL-LIMIT(DL-IDX) TO WS-AMT-ED
                   MOVE WS-AMT-ED TO DRWLIMO
           END-SEARCH.
