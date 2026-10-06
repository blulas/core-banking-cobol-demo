      *================================================================*
      * PROGRAM  : BPINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BP50                                                *
      * PURPOSE  : BILL PAYMENT SERVICE HISTORY. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BPSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-04 RJT  ORIGINAL                                       *
      * 1995-06-27 TWH  NEW REFUND REPORT COLUMNS PRB3257              *
      * 1996-01-07 SKN  FEE NOTICE TEXT PER LEGAL CHG8604              *
      * 1996-04-07 DPS  RETURNED EXCEPTIONS TO OPS QUEUE PRB0634       *
      * 2003-04-13 LMB  RETURNED FEE SCHEDULE UPDATE CHG2562           *
      * 2005-01-28 RJT  RETURNED FEE SCHEDULE UPDATE CHG0687           *
      * 2005-06-09 MKL  PAYMENT STATUS CODES ADDED PRB8994             *
      * 2006-06-22 MKL  PAYMENT NOTICE TEXT PER LEGAL PRB4588          *
      * 2007-03-05 TWH  FEE TABLE EXPANDED PRB3175                     *
      * 2007-06-19 LMB  PAYMENT STATUS CODES ADDED INC7929             *
      * 2008-05-24 PQR  REFUND ROUNDING - HALF UP INC7615              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BP50'.
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
       COPY BPINQS.
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
           MOVE LOW-VALUES TO BPINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BPINQM') MAPSET('BPINQS')
                FROM(BPINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BPINQM') MAPSET('BPINQS')
                INTO(BPINQMI) RESP(WS-RESP) END-EXEC
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
           IF SOURCEL > 0
              AND SOURCEI NOT = 'CT'
              AND SOURCEI NOT = 'FV'
              AND SOURCEI NOT = 'HD'
               MOVE 'SOURCE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO SOURCEL
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
           MOVE BPYM-SOURCE-CD TO SOURCEO
           MOVE BPYM-OVERRIDE-FLAG TO OVERRIO
           MOVE BPYM-REMIT-METHOD-CD TO REMITMO
           MOVE BPYM-STATUS-CD TO STATUSO
           MOVE BPYM-REF-NO TO REFNOO
           MOVE BPYM-REFUND-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO REFUNDO
           MOVE BPYM-EXPIRY-DATE TO EXPIRYO
           MOVE BPYM-REFUND-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REFUND1O
           MOVE BPYM-PAYMENT-CNT TO PAYMENO
           MOVE BPYM-CLOSE-DATE TO CLOSEDO
           MOVE BPYM-SUSPEND-FLAG TO SUSPENO
           MOVE BPYM-OPEN-DATE TO OPENDAO
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
           IF SOURCEL > 0
               MOVE SOURCEI TO BPYM-SOURCE-CD
           END-IF
           EXEC CICS REWRITE FILE('BPYMST') FROM(BPY-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('BPINQM') MAPSET('BPINQS')
                FROM(BPINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
