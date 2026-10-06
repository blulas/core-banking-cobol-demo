      *================================================================*
      * PROGRAM  : BPHST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BP52                                                *
      * PURPOSE  : BILL PAYMENT SERVICE OVERRIDE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BPSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-14 JLM  ORIGINAL                                       *
      * 1997-03-19 RJT  NEW RETURNED REPORT COLUMNS INC7209            *
      * 1998-05-25 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-05 EWS  PAYMENT RERUN PROTECTION CHG3841               *
      * 2004-04-03 JLM  AUDIT FINDING - RETURNED CONTROL TOTALS INC2833*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPHST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BP52'.
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
       COPY BPYMST.
       COPY BPHSTS.
       COPY BPSTATP.
       COPY DCLBPY.
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
           MOVE LOW-VALUES TO BPHSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BPHSTM') MAPSET('BPHSTS')
                FROM(BPHSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BPHSTM') MAPSET('BPHSTS')
                INTO(BPHSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO BPYM-KEY
           EXEC CICS READ FILE('BPYMST') INTO(BPY-MASTER-REC)
                RIDFLD(BPYM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FEEYTDL > 0
               IF FUNCTION TEST-NUMVAL(FEEYTDI) NOT = 0
                   MOVE 'FEE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FEEYTDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FEEYTDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FEE YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF REMITML > 0
              AND REMITMI NOT = 'CGV'
              AND REMITMI NOT = 'FNS'
              AND REMITMI NOT = 'NMS'
               MOVE 'REMIT METHOD CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REMITML
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF REFUNDL > 0
               IF FUNCTION TEST-NUMVAL(REFUNDI) NOT = 0
                   MOVE 'REFUND AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO REFUNDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(REFUNDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'REFUND AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF PAYMENL > 0
               IF FUNCTION TEST-NUMVAL(PAYMENI) NOT = 0
                   MOVE 'PAYMENT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO PAYMENL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(PAYMENI)
                   IF WS-NUM-WORK < 0
                       MOVE 'PAYMENT AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE BPSTAT-PARMS
           MOVE BPYM-PAYMENT-AMT
             TO BPSTAT-PAYMENT-AMT
           MOVE BPYM-RETURNED-AMT
             TO BPSTAT-RETURNED-AMT
           MOVE BPYM-REFUND-AMT
             TO BPSTAT-REFUND-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPSTAT-PAYMENT-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPSTAT-RETURNED-YTD
           MOVE BPYM-FEE-AMT
             TO BPSTAT-FEE-AMT
           MOVE BPYM-OPEN-DATE
             TO BPSTAT-OPEN-DATE
           MOVE BPYM-MATURITY-DATE
             TO BPSTAT-MATURITY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPSTAT-LAST-STMT-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPSTAT-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPSTAT-CYCLE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPSTAT-REMIT-METHOD-CD
           MOVE BPYM-STATUS-CD
             TO BPSTAT-STATUS-CD
           MOVE BPYM-REASON-CD
             TO BPSTAT-REASON-CD
           MOVE BPYM-AUTO-RENEW-FLAG
             TO BPSTAT-AUTO-RENEW-FLAG
           MOVE BPYM-EXEMPT-FLAG
             TO BPSTAT-EXEMPT-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPSTAT-SUSPEND-FLAG
           MOVE BPYM-PAYMENT-CNT
             TO BPSTAT-PAYMENT-CNT
           MOVE BPYM-EXCEPT-CNT
             TO BPSTAT-EXCEPT-CNT
           MOVE BPYM-PENALTY-PCT
             TO BPSTAT-PENALTY-PCT
           MOVE BPYM-RATE
             TO BPSTAT-RATE
           MOVE BPYM-REF-NO
             TO BPSTAT-REF-NO
           MOVE BPYM-CUST-ID
             TO BPSTAT-CUST-ID
           EXEC CICS LINK PROGRAM('BPSTAT') COMMAREA(BPSTAT-PARMS)
                LENGTH(LENGTH OF BPSTAT-PARMS) RESP(WS-RESP)
           END-EXEC
           IF BPSTAT-RETURN-CODE = 08
               MOVE 'RULE BPSTAT DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE BPYM-KEY TO KEYO
           MOVE BPYM-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEYTDO
           MOVE BPYM-CONTACT TO CONTACO
           MOVE BPYM-HOLD-FLAG TO HOLDFLO
           MOVE BPYM-REMIT-METHOD-CD TO REMITMO
           MOVE BPYM-EFFECTIVE-DATE TO EFFECTO
           MOVE BPYM-ITEM-CNT TO ITEMCNO
           MOVE BPYM-REFUND-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO REFUNDO
           MOVE BPYM-DESC TO DESCO
           MOVE BPYM-NOTICE-DATE TO NOTICEO
           MOVE BPYM-CLOSE-DATE TO CLOSEDO
           MOVE BPYM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE BPYM-PAYMENT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMENO
           MOVE BPYM-CYCLE-CNT TO CYCLECO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('BPYMST') INTO(BPY-MASTER-REC)
                RIDFLD(BPYM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FEEYTDL > 0
               COMPUTE BPYM-FEE-YTD = FUNCTION NUMVAL(FEEYTDI)
           END-IF
           IF REMITML > 0
               MOVE REMITMI TO BPYM-REMIT-METHOD-CD
           END-IF
           IF REFUNDL > 0
               COMPUTE BPYM-REFUND-AMT = FUNCTION NUMVAL(REFUNDI)
           END-IF
           IF PAYMENL > 0
               COMPUTE BPYM-PAYMENT-AMT = FUNCTION NUMVAL(PAYMENI)
           END-IF
           EXEC CICS REWRITE FILE('BPYMST') FROM(BPY-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('BPHSTM') MAPSET('BPHSTS')
                FROM(BPHSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
