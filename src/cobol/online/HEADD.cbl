      *================================================================*
      * PROGRAM  : HEADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : HE61                                                *
      * PURPOSE  : HOME EQUITY LINES REVIEW. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : HESTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-23 BHS  ORIGINAL                                       *
      * 1995-11-13 MKL  DRAW CUTOFF MOVED PRB8560                      *
      * 1996-07-17 BHS  BALANCE CUTOFF MOVED REQ4595                   *
      * 1996-10-10 BHS  AUDIT FINDING - APPRAISE CONTROL TOTALS CHG7269*
      * 1997-12-04 BHS  MIN-PAY STATUS CODES ADDED REQ4448             *
      * 2007-03-15 DPS  INTEREST TABLE EXPANDED CHG0982                *
      * 2007-05-01 JLM  INTEREST NOTICE TEXT PER LEGAL PRB9910         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'HE61'.
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
       COPY HELMST.
       COPY HEADDS.
       COPY HESTATP.
       COPY DCLHEL.
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
           MOVE LOW-VALUES TO HEADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('HEADDM') MAPSET('HEADDS')
                FROM(HEADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('HEADDM') MAPSET('HEADDS')
                INTO(HEADDMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO HELM-KEY
           EXEC CICS READ FILE('HELMST') INTO(HEL-MASTER-REC)
                RIDFLD(HELM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DRAWAML > 0
               IF FUNCTION TEST-NUMVAL(DRAWAMI) NOT = 0
                   MOVE 'DRAW AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DRAWAML
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DRAWAMI)
                   IF WS-NUM-WORK < 0
                       MOVE 'DRAW AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF BALANCL > 0
               IF FUNCTION TEST-NUMVAL(BALANCI) NOT = 0
                   MOVE 'BALANCE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO BALANCL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(BALANCI)
                   IF WS-NUM-WORK < 0
                       MOVE 'BALANCE AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF MINPAYL > 0
               IF FUNCTION TEST-NUMVAL(MINPAYI) NOT = 0
                   MOVE 'MIN PAY YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO MINPAYL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(MINPAYI)
                   IF WS-NUM-WORK < 0
                       MOVE 'MIN PAY YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
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
           INITIALIZE HESTAT-PARMS
           MOVE HELM-DRAW-AMT
             TO HESTAT-DRAW-AMT
           MOVE HELM-ANNUAL-FEE-AMT
             TO HESTAT-ANNUAL-FEE-AMT
           MOVE HELM-APPRAISE-YTD
             TO HESTAT-APPRAISE-YTD
           MOVE HELM-BALANCE-AMT
             TO HESTAT-BALANCE-AMT
           MOVE HELM-ANNUAL-FEE-YTD
             TO HESTAT-ANNUAL-FEE-YTD
           MOVE HELM-BALANCE-YTD
             TO HESTAT-BALANCE-YTD
           MOVE HELM-EFFECTIVE-DATE
             TO HESTAT-EFFECTIVE-DATE
           MOVE HELM-OPEN-DATE
             TO HESTAT-OPEN-DATE
           MOVE HELM-LAST-MAINT-DATE
             TO HESTAT-LAST-MAINT-DATE
           MOVE HELM-CLOSE-DATE
             TO HESTAT-CLOSE-DATE
           MOVE HELM-REASON-CD
             TO HESTAT-REASON-CD
           MOVE HELM-TYPE-CD
             TO HESTAT-TYPE-CD
           MOVE HELM-REGION-CD
             TO HESTAT-REGION-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HESTAT-DRAW-PERIOD-CD
           MOVE HELM-PREFERRED-FLAG
             TO HESTAT-PREFERRED-FLAG
           MOVE HELM-AUTO-RENEW-FLAG
             TO HESTAT-AUTO-RENEW-FLAG
           MOVE HELM-OVERRIDE-FLAG
             TO HESTAT-OVERRIDE-FLAG
           MOVE HELM-RETRY-CNT
             TO HESTAT-RETRY-CNT
           MOVE HELM-PAYMENT-CNT
             TO HESTAT-PAYMENT-CNT
           MOVE HELM-DISCOUNT
             TO HESTAT-DISCOUNT
           MOVE HELM-MARGIN
             TO HESTAT-MARGIN
           MOVE HELM-CONTACT
             TO HESTAT-CONTACT
           MOVE HELM-CUST-ID
             TO HESTAT-CUST-ID
           EXEC CICS LINK PROGRAM('HESTAT') COMMAREA(HESTAT-PARMS)
                LENGTH(LENGTH OF HESTAT-PARMS) RESP(WS-RESP)
           END-EXEC
           IF HESTAT-RETURN-CODE = 08
               MOVE 'RULE HESTAT DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE HELM-KEY TO KEYO
           MOVE HELM-DRAW-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRAWAMO
           MOVE HELM-BALANCE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO BALANCO
           MOVE HELM-OPEN-DATE TO OPENDAO
           MOVE HELM-CLOSE-DATE TO CLOSEDO
           MOVE HELM-BALANCE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO BALANC1O
           MOVE HELM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE HELM-APPRAISE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO APPRAIO
           MOVE HELM-RETRY-CNT TO RETRYCO
           MOVE HELM-LIMIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO LIMITAO
           MOVE HELM-OVERRIDE-FLAG TO OVERRIO
           MOVE HELM-PAYMENT-CNT TO PAYMENO
           MOVE HELM-MIN-PAY-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO MINPAYO
           MOVE HELM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('HELMST') INTO(HEL-MASTER-REC)
                RIDFLD(HELM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DRAWAML > 0
               COMPUTE HELM-DRAW-AMT = FUNCTION NUMVAL(DRAWAMI)
           END-IF
           IF BALANCL > 0
               COMPUTE HELM-BALANCE-AMT = FUNCTION NUMVAL(BALANCI)
           END-IF
           IF MINPAYL > 0
               COMPUTE HELM-MIN-PAY-YTD = FUNCTION NUMVAL(MINPAYI)
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO HELM-NOTICE-SENT-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO HELM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('HELMST') FROM(HEL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('HEADDM') MAPSET('HEADDS')
                FROM(HEADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
