      *================================================================*
      * PROGRAM  : HELST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : HE50                                                *
      * PURPOSE  : HOME EQUITY LINES REVIEW. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : HESTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-15 JLM  ORIGINAL                                       *
      * 1991-01-09 SKN  INTEREST TABLE EXPANDED INC4061                *
      * 1998-08-09 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-20 LMB  MIN-PAY CUTOFF MOVED REQ7724                   *
      * 1999-05-04 LMB  BALANCE TABLE EXPANDED PRB9612                 *
      * 2000-10-20 BHS  INTEREST ROUNDING - HALF UP REQ1108            *
      * 2002-10-08 LMB  APPRAISE LIMIT REVISED REQ1017                 *
      * 2004-01-20 SKN  APPRAISE TABLE EXPANDED PRB6987                *
      * 2007-02-19 PQR  PERFORMANCE - APPRAISE READ AHEAD CHG3449      *
      * 2008-12-01 SKN  DRAW ROUNDING - HALF UP CHG5296                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'HE50'.
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
       COPY HELSTS.
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
           MOVE LOW-VALUES TO HELSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('HELSTM') MAPSET('HELSTS')
                FROM(HELSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('HELSTM') MAPSET('HELSTS')
                INTO(HELSTMI) RESP(WS-RESP) END-EXEC
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
           IF LIMITAL > 0
               IF FUNCTION TEST-NUMVAL(LIMITAI) NOT = 0
                   MOVE 'LIMIT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO LIMITAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(LIMITAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'LIMIT AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF AUTOREL > 0 AND AUTOREI NOT = 'Y' AND AUTOREI NOT = 'N'
               MOVE 'AUTO RENEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
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
           MOVE HELM-ADDR TO ADDRO
           MOVE HELM-LIMIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO LIMITAO
           MOVE HELM-CUST-ID TO CUSTIDO
           MOVE HELM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE HELM-APPRAISE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO APPRAIO
           MOVE HELM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE HELM-HOLD-FLAG TO HOLDFLO
           MOVE HELM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE HELM-ANNUAL-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ANNUALO
           MOVE HELM-EFFECTIVE-DATE TO EFFECTO
           MOVE HELM-DRAW-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRAWAMO
           MOVE HELM-DESC TO DESCO
           MOVE HELM-REGION-CD TO REGIONO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('HELMST') INTO(HEL-MASTER-REC)
                RIDFLD(HELM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF LIMITAL > 0
               COMPUTE HELM-LIMIT-AMT = FUNCTION NUMVAL(LIMITAI)
           END-IF
           IF AUTOREL > 0
               MOVE AUTOREI TO HELM-AUTO-RENEW-FLAG
           END-IF
           IF DRAWAML > 0
               COMPUTE HELM-DRAW-AMT = FUNCTION NUMVAL(DRAWAMI)
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
           EXEC CICS SEND MAP('HELSTM') MAPSET('HELSTS')
                FROM(HELSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
