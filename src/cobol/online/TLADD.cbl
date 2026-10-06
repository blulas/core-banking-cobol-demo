      *================================================================*
      * PROGRAM  : TLADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TL23                                                *
      * PURPOSE  : TELLER BALANCING HISTORY. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TLDTE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-01-21 MKL  ORIGINAL                                       *
      * 1995-08-06 RJT  CASH-OUT CALCULATION CORRECTED PRB1411         *
      * 1999-05-10 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-07-06 TWH  NEW VAULT-SELL REPORT COLUMNS CHG3564          *
      * 2004-08-02 BHS  VAULT-SELL NOTICE TEXT PER LEGAL CHG7926       *
      * 2006-05-08 SKN  OVERSHORT STATUS CODES ADDED PRB7303           *
      * 2010-09-20 DPS  CASH-OUT CALCULATION CORRECTED REQ5706         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TL23'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-MESSAGE                      PIC X(79) VALUE SPACES.
       01  WS-ERROR-FLAG                   PIC X(01) VALUE 'N'.
           88  WS-ERROR                           VALUE 'Y'.
       01  WS-AMT-ED                       PIC -(12)9.99.
       01  WS-RATE-ED                      PIC ZZ9.999999.
       01  WS-NUM-WORK                     PIC S9(13)V99 COMP-3.
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
           05  CA-KEY                      PIC X(12).
       COPY TBLMST.
       COPY TLADDS.
       COPY TLDTEP.
       COPY DCLTBL.
       COPY BANKCTL.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(13).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                RIDFLD(WS-CTL-KEY) RESP(WS-RESP) END-EXEC
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               PERFORM 1000-SEND-INITIAL
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHENTER
                       PERFORM 2000-RECEIVE-VALIDATE
                   WHEN DFHPF5
                       PERFORM 2000-RECEIVE-VALIDATE
                       IF NOT WS-ERROR
                           PERFORM 3000-APPLY
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
           MOVE LOW-VALUES TO TLADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TLADDM') MAPSET('TLADDS')
                FROM(TLADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TLADDM') MAPSET('TLADDS')
                INTO(TLADDMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO TBLM-KEY
           EXEC CICS READ FILE('TBLMST') INTO(TBL-MASTER-REC)
                RIDFLD(TBLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF SOURCEL > 0
              AND SOURCEI NOT = 'AF'
              AND SOURCEI NOT = 'PT'
              AND SOURCEI NOT = 'SZ'
               MOVE 'SOURCE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO SOURCEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF HOLDFLL > 0 AND HOLDFLI NOT = 'Y' AND HOLDFLI NOT = 'N'
               MOVE 'HOLD FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF VAULTS1L > 0
               IF FUNCTION TEST-NUMVAL(VAULTS1I) NOT = 0
                   MOVE 'VAULT SELL AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO VAULTS1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(VAULTS1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'VAULT SELL AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CHANNEL > 0
              AND CHANNEI NOT = 'D'
              AND CHANNEI NOT = 'F'
              AND CHANNEI NOT = 'M'
               MOVE 'CHANNEL CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CHANNEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE TLDTE-PARMS
           MOVE TBLM-CASH-IN-YTD
             TO TLDTE-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLDTE-CASH-OUT-YTD
           MOVE TBLM-CASH-OUT-AMT
             TO TLDTE-CASH-OUT-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLDTE-VAULT-BUY-YTD
           MOVE TBLM-BATCH-AMT
             TO TLDTE-BATCH-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLDTE-CASH-IN-AMT
           MOVE TBLM-LAST-STMT-DATE
             TO TLDTE-LAST-STMT-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLDTE-LAST-ACT-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLDTE-NOTICE-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLDTE-EFFECTIVE-DATE
           MOVE TBLM-CYCLE-CD
             TO TLDTE-CYCLE-CD
           MOVE TBLM-CHANNEL-CD
             TO TLDTE-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLDTE-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLDTE-DRAWER-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLDTE-SUSPEND-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLDTE-HOLD-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLDTE-STOP-FLAG
           MOVE TBLM-NOTICE-CNT
             TO TLDTE-NOTICE-CNT
           MOVE TBLM-RETRY-CNT
             TO TLDTE-RETRY-CNT
           MOVE TBLM-DISCOUNT
             TO TLDTE-DISCOUNT
           MOVE TBLM-FEE-PCT
             TO TLDTE-FEE-PCT
           MOVE TBLM-DESC
             TO TLDTE-DESC
           MOVE TBLM-CONTACT
             TO TLDTE-CONTACT
           EXEC CICS LINK PROGRAM('TLDTE') COMMAREA(TLDTE-PARMS)
                LENGTH(LENGTH OF TLDTE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TLDTE-RETURN-CODE = 08
               MOVE 'RULE TLDTE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TBLM-KEY TO KEYO
           MOVE TBLM-NOTICE-DATE TO NOTICEO
           MOVE TBLM-VAULT-SELL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO VAULTSO
           MOVE TBLM-SOURCE-CD TO SOURCEO
           MOVE TBLM-RETRY-CNT TO RETRYCO
           MOVE TBLM-REVIEW-FLAG TO REVIEWO
           MOVE TBLM-CYCLE-CD TO CYCLECO
           MOVE TBLM-EXPIRY-DATE TO EXPIRYO
           MOVE TBLM-CASH-IN-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASHINO
           MOVE TBLM-LAST-STMT-DATE TO LASTSTO
           MOVE TBLM-HOLD-FLAG TO HOLDFLO
           MOVE TBLM-VAULT-SELL-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO VAULTS1O
           MOVE TBLM-VAULT-BUY-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO VAULTBO
           MOVE TBLM-CHANNEL-CD TO CHANNEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TBLMST') INTO(TBL-MASTER-REC)
                RIDFLD(TBLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF SOURCEL > 0
               MOVE SOURCEI TO TBLM-SOURCE-CD
           END-IF
           IF HOLDFLL > 0
               MOVE HOLDFLI TO TBLM-HOLD-FLAG
           END-IF
           IF VAULTS1L > 0
               COMPUTE TBLM-VAULT-SELL-AMT = FUNCTION NUMVAL(VAULTS1I)
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO TBLM-CHANNEL-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO TBLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('TBLMST') FROM(TBL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TLADDM') MAPSET('TLADDS')
                FROM(TLADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
