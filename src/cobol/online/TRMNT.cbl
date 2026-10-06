      *================================================================*
      * PROGRAM  : TRMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TR61                                                *
      * PURPOSE  : PERSONAL TRUST OVERRIDE. ENTER VALIDATES AND        *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TRPEN, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-02-18 RJT  ORIGINAL                                       *
      * 1992-07-16 LMB  PERFORMANCE - DISTRIB READ AHEAD REQ4719       *
      * 1993-04-13 DPS  DISTRIB TABLE EXPANDED REQ4956                 *
      * 1993-03-21 MKL  TRUSTEE-FEE NOTICE TEXT PER LEGAL REQ9816      *
      * 1997-02-23 TWH  PRINCIPAL RERUN PROTECTION PRB8369             *
      * 1997-06-22 EWS  TAX CUTOFF MOVED CHG4839                       *
      * 1998-10-07 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-03 SKN  AUDIT FINDING - PRINCIPAL CONTROL TOTALS CHG491*
      * 1999-05-07 TWH  PRINCIPAL NOTICE TEXT PER LEGAL INC3695        *
      * 2002-01-04 TWH  PRINCIPAL TABLE EXPANDED INC7015               *
      * 2003-07-16 EWS  MARKET RERUN PROTECTION PRB1428                *
      * 2005-08-15 DPS  INCOME RERUN PROTECTION PRB8131                *
      * 2006-12-06 GAF  PERFORMANCE - DISTRIB READ AHEAD PRB8784       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TR61'.
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
       COPY TRSMST.
       COPY TRMNTS.
       COPY TRPENP.
       COPY DCLTRS.
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
           MOVE LOW-VALUES TO TRMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TRMNTM') MAPSET('TRMNTS')
                FROM(TRMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TRMNTM') MAPSET('TRMNTS')
                INTO(TRMNTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO TRSM-KEY
           EXEC CICS READ FILE('TRSMST') INTO(TRS-MASTER-REC)
                RIDFLD(TRSM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DISTRIL > 0
               IF FUNCTION TEST-NUMVAL(DISTRII) NOT = 0
                   MOVE 'DISTRIB AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DISTRIL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DISTRII)
                   IF WS-NUM-WORK < 0
                       MOVE 'DISTRIB AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF MARKETL > 0
               IF FUNCTION TEST-NUMVAL(MARKETI) NOT = 0
                   MOVE 'MARKET AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO MARKETL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(MARKETI)
                   IF WS-NUM-WORK < 0
                       MOVE 'MARKET AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CYCLECL > 0
              AND CYCLECI NOT = 'EN'
              AND CYCLECI NOT = 'FY'
              AND CYCLECI NOT = 'HP'
               MOVE 'CYCLE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CYCLECL
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
           INITIALIZE TRPEN-PARMS
           MOVE TRSM-DISTRIB-YTD
             TO TRPEN-DISTRIB-YTD
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRPEN-TRUSTEE-FEE-AMT
           MOVE TRSM-INCOME-AMT
             TO TRPEN-INCOME-AMT
           MOVE TRSM-INCOME-YTD
             TO TRPEN-INCOME-YTD
           MOVE TRSM-TAX-AMT
             TO TRPEN-TAX-AMT
           MOVE TRSM-MARKET-YTD
             TO TRPEN-MARKET-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRPEN-CLOSE-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRPEN-NOTICE-DATE
           MOVE TRSM-LAST-STMT-DATE
             TO TRPEN-LAST-STMT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRPEN-OPEN-DATE
           MOVE TRSM-REGION-CD
             TO TRPEN-REGION-CD
           MOVE TRSM-CYCLE-CD
             TO TRPEN-CYCLE-CD
           MOVE TRSM-STATUS-CD
             TO TRPEN-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRPEN-TRUST-TYPE-CD
           MOVE TRSM-SUSPEND-FLAG
             TO TRPEN-SUSPEND-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRPEN-EXEMPT-FLAG
           MOVE TRSM-PREFERRED-FLAG
             TO TRPEN-PREFERRED-FLAG
           MOVE TRSM-RETRY-CNT
             TO TRPEN-RETRY-CNT
           MOVE TRSM-PAYMENT-CNT
             TO TRPEN-PAYMENT-CNT
           MOVE TRSM-DISCOUNT
             TO TRPEN-DISCOUNT
           MOVE TRSM-RATE
             TO TRPEN-RATE
           MOVE TRSM-ADDR
             TO TRPEN-ADDR
           MOVE TRSM-REF-NO
             TO TRPEN-REF-NO
           EXEC CICS LINK PROGRAM('TRPEN') COMMAREA(TRPEN-PARMS)
                LENGTH(LENGTH OF TRPEN-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TRPEN-RETURN-CODE = 08
               MOVE 'RULE TRPEN DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TRSM-KEY TO KEYO
           MOVE TRSM-DISTRIB-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISTRIO
           MOVE TRSM-PAYMENT-CNT TO PAYMENO
           MOVE TRSM-NOTICE-DATE TO NOTICEO
           MOVE TRSM-MARKET-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO MARKETO
           MOVE TRSM-TRUST-TYPE-CD TO TRUSTTO
           MOVE TRSM-CYCLE-CD TO CYCLECO
           MOVE TRSM-DISTRIB-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISTRI1O
           MOVE TRSM-SUSPEND-FLAG TO SUSPENO
           MOVE TRSM-RETRY-CNT TO RETRYCO
           MOVE TRSM-STATUS-CD TO STATUSO
           MOVE TRSM-LAST-STMT-DATE TO LASTSTO
           MOVE TRSM-STOP-FLAG TO STOPFLO
           MOVE TRSM-NOTICE-CNT TO NOTICE1O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TRSMST') INTO(TRS-MASTER-REC)
                RIDFLD(TRSM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DISTRIL > 0
               COMPUTE TRSM-DISTRIB-AMT = FUNCTION NUMVAL(DISTRII)
           END-IF
           IF MARKETL > 0
               COMPUTE TRSM-MARKET-AMT = FUNCTION NUMVAL(MARKETI)
           END-IF
           IF CYCLECL > 0
               MOVE CYCLECI TO TRSM-CYCLE-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO TRSM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('TRSMST') FROM(TRS-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TRMNTM') MAPSET('TRMNTS')
                FROM(TRMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
