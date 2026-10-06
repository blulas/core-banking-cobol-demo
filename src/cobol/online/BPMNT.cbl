      *================================================================*
      * PROGRAM  : BPMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BP15                                                *
      * PURPOSE  : BILL PAYMENT SERVICE OVERRIDE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BPSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-26 PQR  ORIGINAL                                       *
      * 1997-06-12 MKL  RETURNED RERUN PROTECTION PRB0545              *
      * 1997-02-13 DPS  REFUND EXCEPTIONS TO OPS QUEUE PRB9922         *
      * 1998-07-20 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-10-25 LMB  REFUND STATUS CODES ADDED INC0337              *
      * 2002-12-24 GAF  RETURNED STATUS CODES ADDED INC1323            *
      * 2003-10-10 RJT  AUDIT FINDING - PAYMENT CONTROL TOTALS PRB3155 *
      * 2004-04-16 LMB  FEE NOTICE TEXT PER LEGAL INC1541              *
      * 2005-11-19 MKL  PERFORMANCE - FEE READ AHEAD REQ8844           *
      * 2007-12-06 MKL  FEE FEE SCHEDULE UPDATE PRB5680                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BP15'.
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
       COPY BPMNTS.
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
           MOVE LOW-VALUES TO BPMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BPMNTM') MAPSET('BPMNTS')
                FROM(BPMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BPMNTM') MAPSET('BPMNTS')
                INTO(BPMNTMI) RESP(WS-RESP) END-EXEC
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
           IF PREFERL > 0 AND PREFERI NOT = 'Y' AND PREFERI NOT = 'N'
               MOVE 'PREFERRED FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF REASONL > 0
              AND REASONI NOT = 'AX'
              AND REASONI NOT = 'BX'
              AND REASONI NOT = 'GM'
               MOVE 'REASON CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REASONL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SOURCEL > 0
              AND SOURCEI NOT = 'CT'
              AND SOURCEI NOT = 'FV'
              AND SOURCEI NOT = 'HD'
               MOVE 'SOURCE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO SOURCEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF FEEAMTL > 0
               IF FUNCTION TEST-NUMVAL(FEEAMTI) NOT = 0
                   MOVE 'FEE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FEEAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FEEAMTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FEE AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
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
           MOVE BPYM-PAYMENT-CNT TO PAYMENO
           MOVE BPYM-SUSPEND-FLAG TO SUSPENO
           MOVE BPYM-NOTICE-DATE TO NOTICEO
           MOVE BPYM-PAYMENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMEN1O
           MOVE BPYM-PREFERRED-FLAG TO PREFERO
           MOVE BPYM-EXCEPT-CNT TO EXCEPTO
           MOVE BPYM-PAYEE-TYPE-CD TO PAYEETO
           MOVE BPYM-REASON-CD TO REASONO
           MOVE BPYM-SOURCE-CD TO SOURCEO
           MOVE BPYM-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEAMTO
           MOVE BPYM-NAME TO NAMEO
           MOVE BPYM-DESC TO DESCO
           MOVE BPYM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('BPYMST') INTO(BPY-MASTER-REC)
                RIDFLD(BPYM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF PREFERL > 0
               MOVE PREFERI TO BPYM-PREFERRED-FLAG
           END-IF
           IF REASONL > 0
               MOVE REASONI TO BPYM-REASON-CD
           END-IF
           IF SOURCEL > 0
               MOVE SOURCEI TO BPYM-SOURCE-CD
           END-IF
           IF FEEAMTL > 0
               COMPUTE BPYM-FEE-AMT = FUNCTION NUMVAL(FEEAMTI)
           END-IF
           EXEC CICS REWRITE FILE('BPYMST') FROM(BPY-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('BPMNTM') MAPSET('BPMNTS')
                FROM(BPMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
