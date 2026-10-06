      *================================================================*
      * PROGRAM  : HEREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : HE88                                                *
      * PURPOSE  : HOME EQUITY LINES HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : HELIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-19 TWH  ORIGINAL                                       *
      * 1992-06-22 CRP  LIMIT NOTICE TEXT PER LEGAL PRB6376            *
      * 1993-10-13 DPS  APPRAISE EXCEPTIONS TO OPS QUEUE REQ7872       *
      * 1994-01-20 TWH  APPRAISE STATUS CODES ADDED CHG4374            *
      * 1998-01-01 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-28 JLM  ANNUAL-FEE CALCULATION CORRECTED INC7877       *
      * 2000-11-10 GAF  ANNUAL-FEE STATUS CODES ADDED CHG9980          *
      * 2001-12-01 EWS  APPRAISE CUTOFF MOVED PRB5193                  *
      * 2002-11-01 DPS  ANNUAL-FEE RERUN PROTECTION REQ8909            *
      * 2004-04-27 TWH  MIN-PAY ROUNDING - HALF UP PRB5072             *
      * 2008-09-09 RJT  LIMIT ROUNDING - HALF UP INC2936               *
      * 2008-11-11 LMB  MIN-PAY NOTICE TEXT PER LEGAL REQ3022          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'HE88'.
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
       COPY HEREVS.
       COPY HELIMP.
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
           MOVE LOW-VALUES TO HEREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('HEREVM') MAPSET('HEREVS')
                FROM(HEREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('HEREVM') MAPSET('HEREVS')
                INTO(HEREVMI) RESP(WS-RESP) END-EXEC
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
           IF REASONL > 0
              AND REASONI NOT = 'KST'
              AND REASONI NOT = 'NWS'
              AND REASONI NOT = 'ZJM'
               MOVE 'REASON CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REASONL
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
           INITIALIZE HELIM-PARMS
           MOVE HELM-BALANCE-YTD
             TO HELIM-BALANCE-YTD
           MOVE HELM-APPRAISE-YTD
             TO HELIM-APPRAISE-YTD
           MOVE HELM-ANNUAL-FEE-AMT
             TO HELIM-ANNUAL-FEE-AMT
           MOVE HELM-APPRAISE-AMT
             TO HELIM-APPRAISE-AMT
           MOVE HELM-INTEREST-YTD
             TO HELIM-INTEREST-YTD
           MOVE HELM-MIN-PAY-AMT
             TO HELIM-MIN-PAY-AMT
           MOVE HELM-EFFECTIVE-DATE
             TO HELIM-EFFECTIVE-DATE
           MOVE HELM-LAST-PAY-DATE
             TO HELIM-LAST-PAY-DATE
           MOVE HELM-NEXT-DUE-DATE
             TO HELIM-NEXT-DUE-DATE
           MOVE HELM-MATURITY-DATE
             TO HELIM-MATURITY-DATE
           MOVE HELM-DRAW-PERIOD-CD
             TO HELIM-DRAW-PERIOD-CD
           MOVE HELM-STATUS-CD
             TO HELIM-STATUS-CD
           MOVE HELM-TYPE-CD
             TO HELIM-TYPE-CD
           MOVE HELM-LIEN-POS-CD
             TO HELIM-LIEN-POS-CD
           MOVE HELM-AUTO-RENEW-FLAG
             TO HELIM-AUTO-RENEW-FLAG
           MOVE HELM-OVERRIDE-FLAG
             TO HELIM-OVERRIDE-FLAG
           MOVE HELM-HOLD-FLAG
             TO HELIM-HOLD-FLAG
           MOVE HELM-PAYMENT-CNT
             TO HELIM-PAYMENT-CNT
           MOVE HELM-EXCEPT-CNT
             TO HELIM-EXCEPT-CNT
           MOVE HELM-PENALTY-PCT
             TO HELIM-PENALTY-PCT
           MOVE HELM-DISCOUNT
             TO HELIM-DISCOUNT
           MOVE HELM-REF-NO
             TO HELIM-REF-NO
           MOVE HELM-DESC
             TO HELIM-DESC
           EXEC CICS LINK PROGRAM('HELIM') COMMAREA(HELIM-PARMS)
                LENGTH(LENGTH OF HELIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF HELIM-RETURN-CODE = 08
               MOVE 'RULE HELIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE HELM-KEY TO KEYO
           MOVE HELM-EFFECTIVE-DATE TO EFFECTO
           MOVE HELM-LAST-MAINT-DATE TO LASTMAO
           MOVE HELM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE HELM-BALANCE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO BALANCO
           MOVE HELM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE HELM-NEXT-DUE-DATE TO NEXTDUO
           MOVE HELM-CLOSE-DATE TO CLOSEDO
           MOVE HELM-DRAW-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRAWAMO
           MOVE HELM-HOLD-FLAG TO HOLDFLO
           MOVE HELM-LAST-PAY-DATE TO LASTPAO
           MOVE HELM-LIEN-POS-CD TO LIENPOO
           MOVE HELM-DRAW-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRAWYTO
           MOVE HELM-REASON-CD TO REASONO
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
           IF REASONL > 0
               MOVE REASONI TO HELM-REASON-CD
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
           EXEC CICS SEND MAP('HEREVM') MAPSET('HEREVS')
                FROM(HEREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
