      *================================================================*
      * PROGRAM  : PYINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PY68                                                *
      * PURPOSE  : PAYROLL SERVICES REVIEW. ENTER VALIDATES AND        *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PYPRC, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-07-20 TWH  ORIGINAL                                       *
      * 1993-11-03 RJT  GROSS FEE SCHEDULE UPDATE REQ3363              *
      * 1994-03-14 RJT  FICA LIMIT REVISED REQ1106                     *
      * 1995-01-22 JLM  NEW NET REPORT COLUMNS INC9756                 *
      * 1997-06-19 LMB  NEW FED-TAX REPORT COLUMNS REQ4653             *
      * 2002-03-06 MKL  STATE-TAX NOTICE TEXT PER LEGAL REQ8288        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PY68'.
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
       COPY PRLMST.
       COPY PYINQS.
       COPY PYPRCP.
       COPY DCLPRL.
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
           MOVE LOW-VALUES TO PYINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PYINQM') MAPSET('PYINQS')
                FROM(PYINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PYINQM') MAPSET('PYINQS')
                INTO(PYINQMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO PRLM-KEY
           EXEC CICS READ FILE('PRLMST') INTO(PRL-MASTER-REC)
                RIDFLD(PRLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'MV'
              AND STATUSI NOT = 'RD'
              AND STATUSI NOT = 'ST'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF GROSSYL > 0
               IF FUNCTION TEST-NUMVAL(GROSSYI) NOT = 0
                   MOVE 'GROSS YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO GROSSYL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(GROSSYI)
                   IF WS-NUM-WORK < 0
                       MOVE 'GROSS YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF SUSPENL > 0 AND SUSPENI NOT = 'Y' AND SUSPENI NOT = 'N'
               MOVE 'SUSPEND FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE PYPRC-PARMS
           MOVE PRLM-STATE-TAX-YTD
             TO PYPRC-STATE-TAX-YTD
           MOVE PRLM-FED-TAX-AMT
             TO PYPRC-FED-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYPRC-NET-AMT
           MOVE PRLM-STATE-TAX-AMT
             TO PYPRC-STATE-TAX-AMT
           MOVE PRLM-FICA-YTD
             TO PYPRC-FICA-YTD
           MOVE PRLM-FED-TAX-YTD
             TO PYPRC-FED-TAX-YTD
           MOVE PRLM-NEXT-DUE-DATE
             TO PYPRC-NEXT-DUE-DATE
           MOVE PRLM-LAST-PAY-DATE
             TO PYPRC-LAST-PAY-DATE
           MOVE PRLM-OPEN-DATE
             TO PYPRC-OPEN-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYPRC-EXPIRY-DATE
           MOVE PRLM-TAX-STATE-CD
             TO PYPRC-TAX-STATE-CD
           MOVE PRLM-STATUS-CD
             TO PYPRC-STATUS-CD
           MOVE PRLM-SOURCE-CD
             TO PYPRC-SOURCE-CD
           MOVE PRLM-REGION-CD
             TO PYPRC-REGION-CD
           MOVE PRLM-STOP-FLAG
             TO PYPRC-STOP-FLAG
           MOVE PRLM-AUTO-RENEW-FLAG
             TO PYPRC-AUTO-RENEW-FLAG
           MOVE PRLM-PREFERRED-FLAG
             TO PYPRC-PREFERRED-FLAG
           MOVE PRLM-NOTICE-CNT
             TO PYPRC-NOTICE-CNT
           MOVE PRLM-ITEM-CNT
             TO PYPRC-ITEM-CNT
           MOVE PRLM-FEE-PCT
             TO PYPRC-FEE-PCT
           MOVE PRLM-MARGIN
             TO PYPRC-MARGIN
           MOVE PRLM-CUST-ID
             TO PYPRC-CUST-ID
           MOVE PRLM-CONTACT
             TO PYPRC-CONTACT
           EXEC CICS LINK PROGRAM('PYPRC') COMMAREA(PYPRC-PARMS)
                LENGTH(LENGTH OF PYPRC-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PYPRC-RETURN-CODE = 08
               MOVE 'RULE PYPRC DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PRLM-KEY TO KEYO
           MOVE PRLM-LAST-ACT-DATE TO LASTACO
           MOVE PRLM-STATUS-CD TO STATUSO
           MOVE PRLM-DEDUCT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEDUCTO
           MOVE PRLM-DEDUCT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEDUCT1O
           MOVE PRLM-STATE-TAX-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO STATETO
           MOVE PRLM-GROSS-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO GROSSYO
           MOVE PRLM-SUSPEND-FLAG TO SUSPENO
           MOVE PRLM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE PRLM-CLOSE-DATE TO CLOSEDO
           MOVE PRLM-LAST-PAY-DATE TO LASTPAO
           MOVE PRLM-CUST-ID TO CUSTIDO
           MOVE PRLM-PAYMENT-CNT TO PAYMENO
           MOVE PRLM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PRLMST') INTO(PRL-MASTER-REC)
                RIDFLD(PRLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO PRLM-STATUS-CD
           END-IF
           IF GROSSYL > 0
               COMPUTE PRLM-GROSS-YTD = FUNCTION NUMVAL(GROSSYI)
           END-IF
           IF SUSPENL > 0
               MOVE SUSPENI TO PRLM-SUSPEND-FLAG
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO PRLM-NOTICE-SENT-FLAG
           END-IF
           EXEC CICS REWRITE FILE('PRLMST') FROM(PRL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('PYINQM') MAPSET('PYINQS')
                FROM(PYINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
