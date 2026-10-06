      *================================================================*
      * PROGRAM  : LBLST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : LB45                                                *
      * PURPOSE  : LOCKBOX REMITTANCE OVERRIDE. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : LBWAIV, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-20 SKN  ORIGINAL                                       *
      * 1996-06-27 EWS  EXCEPT NOTICE TEXT PER LEGAL CHG1647           *
      * 1998-04-19 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-15 PQR  CASH-ON-HAND EXCEPTIONS TO OPS QUEUE PRB8696   *
      * 1999-05-08 SKN  PERFORMANCE - CASH-ON-HAND READ AHEAD REQ1311  *
      * 2002-08-07 GAF  AUDIT FINDING - CASH-ON-HAND CONTROL TOTALS REQ*
      * 2008-09-23 PQR  CASH-ON-HAND FEE SCHEDULE UPDATE INC9464       *
      * 2011-10-14 JLM  AUDIT FINDING - CHECK CONTROL TOTALS CHG3495   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBLST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'LB45'.
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
       COPY LBLSTS.
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
           MOVE LOW-VALUES TO LBLSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('LBLSTM') MAPSET('LBLSTS')
                FROM(LBLSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('LBLSTM') MAPSET('LBLSTS')
                INTO(LBLSTMI) RESP(WS-RESP) END-EXEC
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
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           MOVE LBXM-REMIT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REMITYO
           MOVE LBXM-EXEMPT-FLAG TO EXEMPTO
           MOVE LBXM-STOP-FLAG TO STOPFLO
           MOVE LBXM-DEPOSIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEPOSIO
           MOVE LBXM-PAYMENT-CNT TO PAYMENO
           MOVE LBXM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE LBXM-LOCKBOX-CD TO LOCKBOO
           MOVE LBXM-LAST-STMT-DATE TO LASTSTO
           MOVE LBXM-DESC TO DESCO
           MOVE LBXM-REMIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO REMITAO
           MOVE LBXM-EXCEPT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPTO
           MOVE LBXM-EXCEPT-CNT TO EXCEPT1O
           MOVE LBXM-REF-NO TO REFNOO
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
           IF STOPFLL > 0
               MOVE STOPFLI TO LBXM-STOP-FLAG
           END-IF
           EXEC CICS REWRITE FILE('LBXMST') FROM(LBX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('LBLSTM') MAPSET('LBLSTS')
                FROM(LBLSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
