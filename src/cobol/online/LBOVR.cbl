      *================================================================*
      * PROGRAM  : LBOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : LB72                                                *
      * PURPOSE  : LOCKBOX REMITTANCE MAINTENANCE. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : LBWAIV, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-26 PQR  ORIGINAL                                       *
      * 2004-11-21 LMB  AUDIT FINDING - FEE CONTROL TOTALS REQ4218     *
      * 2005-05-15 PQR  EXCEPT STATUS CODES ADDED PRB8162              *
      * 2011-02-01 JLM  CHECK NOTICE TEXT PER LEGAL CHG0861            *
      * 2011-03-27 CRP  EXCEPT STATUS CODES ADDED REQ8038              *
      * 2012-12-26 SKN  EXCEPT STATUS CODES ADDED REQ5164              *
      * 2013-04-18 PQR  CHECK NOTICE TEXT PER LEGAL PRB5088            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'LB72'.
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
       COPY LBXMST.
       COPY LBOVRS.
       COPY LBWAIVP.
       COPY DCLLBX.
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
           MOVE LOW-VALUES TO LBOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('LBOVRM') MAPSET('LBOVRS')
                FROM(LBOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('LBOVRM') MAPSET('LBOVRS')
                INTO(LBOVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO LBXM-KEY
           EXEC CICS READ FILE('LBXMST') INTO(LBX-MASTER-REC)
                RIDFLD(LBXM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REMITYL > 0
               IF FUNCTION TEST-NUMVAL(REMITYI) NOT = 0
                   MOVE 'REMIT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO REMITYL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(REMITYI)
                   IF WS-NUM-WORK < 0
                       MOVE 'REMIT YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CHECKAL > 0
               IF FUNCTION TEST-NUMVAL(CHECKAI) NOT = 0
                   MOVE 'CHECK AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CHECKAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CHECKAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CHECK AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE LBWAIV-PARMS
           MOVE LBXM-EXCEPT-AMT
             TO LBWAIV-EXCEPT-AMT
           MOVE LBXM-FLOAT-AMT
             TO LBWAIV-FLOAT-AMT
           MOVE LBXM-CHECK-YTD
             TO LBWAIV-CHECK-YTD
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBWAIV-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBWAIV-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBWAIV-REMIT-YTD
           MOVE LBXM-MATURITY-DATE
             TO LBWAIV-MATURITY-DATE
           MOVE LBXM-EXPIRY-DATE
             TO LBWAIV-EXPIRY-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBWAIV-LAST-PAY-DATE
           MOVE LBXM-LAST-STMT-DATE
             TO LBWAIV-LAST-STMT-DATE
           MOVE LBXM-REASON-CD
             TO LBWAIV-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBWAIV-LOCKBOX-CD
           MOVE LBXM-REGION-CD
             TO LBWAIV-REGION-CD
           MOVE LBXM-CHANNEL-CD
             TO LBWAIV-CHANNEL-CD
           MOVE LBXM-PREFERRED-FLAG
             TO LBWAIV-PREFERRED-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBWAIV-EXEMPT-FLAG
           MOVE LBXM-STOP-FLAG
             TO LBWAIV-STOP-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBWAIV-PAYMENT-CNT
           MOVE LBXM-EXCEPT-CNT
             TO LBWAIV-EXCEPT-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBWAIV-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBWAIV-DISCOUNT
           MOVE LBXM-CONTACT
             TO LBWAIV-CONTACT
           MOVE LBXM-CUST-ID
             TO LBWAIV-CUST-ID
           EXEC CICS LINK PROGRAM('LBWAIV') COMMAREA(LBWAIV-PARMS)
                LENGTH(LENGTH OF LBWAIV-PARMS) RESP(WS-RESP)
           END-EXEC
           IF LBWAIV-RETURN-CODE = 08
               MOVE 'RULE LBWAIV DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE LBXM-KEY TO KEYO
           MOVE LBXM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE LBXM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE LBXM-REMIT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REMITYO
           MOVE LBXM-NAME TO NAMEO
           MOVE LBXM-FLOAT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FLOATAO
           MOVE LBXM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE LBXM-EXCEPT-CNT TO EXCEPTO
           MOVE LBXM-REMIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO REMITAO
           MOVE LBXM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE LBXM-CHECK-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CHECKAO
           MOVE LBXM-CYCLE-CD TO CYCLECO
           MOVE LBXM-LAST-PAY-DATE TO LASTPAO
           MOVE LBXM-EXEMPT-FLAG TO EXEMPTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('LBXMST') INTO(LBX-MASTER-REC)
                RIDFLD(LBXM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REMITYL > 0
               COMPUTE LBXM-REMIT-YTD = FUNCTION NUMVAL(REMITYI)
           END-IF
           IF CHECKAL > 0
               COMPUTE LBXM-CHECK-AMT = FUNCTION NUMVAL(CHECKAI)
           END-IF
           EXEC CICS REWRITE FILE('LBXMST') FROM(LBX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('LBOVRM') MAPSET('LBOVRS')
                FROM(LBOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
