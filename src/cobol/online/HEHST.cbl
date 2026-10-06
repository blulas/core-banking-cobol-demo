      *================================================================*
      * PROGRAM  : HEHST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : HE86                                                *
      * PURPOSE  : HOME EQUITY LINES OVERRIDE. ENTER VALIDATES AND     *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : HELIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-18 DPS  ORIGINAL                                       *
      * 1995-08-22 JLM  INTEREST FEE SCHEDULE UPDATE PRB3102           *
      * 1996-09-22 RJT  DRAW RERUN PROTECTION CHG0511                  *
      * 1996-11-05 GAF  LIMIT FEE SCHEDULE UPDATE INC9883              *
      * 1996-10-17 RJT  APPRAISE RERUN PROTECTION REQ0401              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEHST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'HE86'.
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
       COPY HEHSTS.
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
           MOVE LOW-VALUES TO HEHSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('HEHSTM') MAPSET('HEHSTS')
                FROM(HEHSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('HEHSTM') MAPSET('HEHSTS')
                INTO(HEHSTMI) RESP(WS-RESP) END-EXEC
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
           IF ANNUALL > 0
               IF FUNCTION TEST-NUMVAL(ANNUALI) NOT = 0
                   MOVE 'ANNUAL FEE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ANNUALL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ANNUALI)
                   IF WS-NUM-WORK < 0
                       MOVE 'ANNUAL FEE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF BALANCL > 0
               IF FUNCTION TEST-NUMVAL(BALANCI) NOT = 0
                   MOVE 'BALANCE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO BALANCL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(BALANCI)
                   IF WS-NUM-WORK < 0
                       MOVE 'BALANCE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF INTEREL > 0
               IF FUNCTION TEST-NUMVAL(INTEREI) NOT = 0
                   MOVE 'INTEREST YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO INTEREL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(INTEREI)
                   IF WS-NUM-WORK < 0
                       MOVE 'INTEREST YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF HOLDFLL > 0 AND HOLDFLI NOT = 'Y' AND HOLDFLI NOT = 'N'
               MOVE 'HOLD FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF AUTOREL > 0 AND AUTOREI NOT = 'Y' AND AUTOREI NOT = 'N'
               MOVE 'AUTO RENEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF APPRAIL > 0
               IF FUNCTION TEST-NUMVAL(APPRAII) NOT = 0
                   MOVE 'APPRAISE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO APPRAIL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(APPRAII)
                   IF WS-NUM-WORK < 0
                       MOVE 'APPRAISE AMT MAY NOT BE NEGATIVE' TO
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
           MOVE HELM-ANNUAL-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ANNUALO
           MOVE HELM-BALANCE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO BALANCO
           MOVE HELM-INTEREST-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTEREO
           MOVE HELM-REGION-CD TO REGIONO
           MOVE HELM-DESC TO DESCO
           MOVE HELM-HOLD-FLAG TO HOLDFLO
           MOVE HELM-MATURITY-DATE TO MATURIO
           MOVE HELM-NAME TO NAMEO
           MOVE HELM-MIN-PAY-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO MINPAYO
           MOVE HELM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE HELM-DRAW-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRAWYTO
           MOVE HELM-LIEN-POS-CD TO LIENPOO
           MOVE HELM-APPRAISE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO APPRAIO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('HELMST') INTO(HEL-MASTER-REC)
                RIDFLD(HELM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF ANNUALL > 0
               COMPUTE HELM-ANNUAL-FEE-YTD = FUNCTION NUMVAL(ANNUALI)
           END-IF
           IF BALANCL > 0
               COMPUTE HELM-BALANCE-YTD = FUNCTION NUMVAL(BALANCI)
           END-IF
           IF INTEREL > 0
               COMPUTE HELM-INTEREST-YTD = FUNCTION NUMVAL(INTEREI)
           END-IF
           IF HOLDFLL > 0
               MOVE HOLDFLI TO HELM-HOLD-FLAG
           END-IF
           IF AUTOREL > 0
               MOVE AUTOREI TO HELM-AUTO-RENEW-FLAG
           END-IF
           IF APPRAIL > 0
               COMPUTE HELM-APPRAISE-AMT = FUNCTION NUMVAL(APPRAII)
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
           EXEC CICS SEND MAP('HEHSTM') MAPSET('HEHSTS')
                FROM(HEHSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
