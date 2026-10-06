      *================================================================*
      * PROGRAM  : BUADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BU28                                                *
      * PURPOSE  : BUMP-UP CERTIFICATES OVERRIDE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BUSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-06 DPS  ORIGINAL                                       *
      * 1993-04-25 GAF  PENALTY EXCEPTIONS TO OPS QUEUE INC9436        *
      * 1994-05-28 JLM  NEW PENALTY REPORT COLUMNS PRB8937             *
      * 1997-08-12 MKL  AUDIT FINDING - BUMP CONTROL TOTALS PRB5206    *
      * 1998-04-26 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-11-01 JLM  ADDON NOTICE TEXT PER LEGAL CHG8167            *
      * 2000-06-25 PQR  BUMP CALCULATION CORRECTED INC3827             *
      * 2002-06-14 EWS  PRINCIPAL STATUS CODES ADDED REQ3809           *
      * 2003-11-15 CRP  PENALTY NOTICE TEXT PER LEGAL INC7822          *
      * 2004-09-18 JLM  PENALTY CUTOFF MOVED CHG0275                   *
      * 2006-05-26 SKN  AUDIT FINDING - ADDON CONTROL TOTALS CHG6058   *
      * 2007-09-03 BHS  AUDIT FINDING - INTEREST CONTROL TOTALS INC4097*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BU28'.
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
       COPY BCDMST.
       COPY BUADDS.
       COPY BUSTATP.
       COPY DCLBCD.
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
           MOVE LOW-VALUES TO BUADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BUADDM') MAPSET('BUADDS')
                FROM(BUADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BUADDM') MAPSET('BUADDS')
                INTO(BUADDMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO BCDM-KEY
           EXEC CICS READ FILE('BCDMST') INTO(BCD-MASTER-REC)
                RIDFLD(BCDM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STEPPLL > 0
              AND STEPPLI NOT = 'AYX'
              AND STEPPLI NOT = 'DGU'
              AND STEPPLI NOT = 'GWJ'
               MOVE 'STEP PLAN CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STEPPLL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SUSPENL > 0 AND SUSPENI NOT = 'Y' AND SUSPENI NOT = 'N'
               MOVE 'SUSPEND FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE BUSTAT-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUSTAT-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUSTAT-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUSTAT-BUMP-AMT
           MOVE BCDM-PENALTY-AMT
             TO BUSTAT-PENALTY-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUSTAT-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-YTD
             TO BUSTAT-INTEREST-YTD
           MOVE BCDM-LAST-PAY-DATE
             TO BUSTAT-LAST-PAY-DATE
           MOVE BCDM-EXPIRY-DATE
             TO BUSTAT-EXPIRY-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUSTAT-MATURITY-DATE
           MOVE BCDM-LAST-STMT-DATE
             TO BUSTAT-LAST-STMT-DATE
           MOVE BCDM-ELECTION-CD
             TO BUSTAT-ELECTION-CD
           MOVE BCDM-CHANNEL-CD
             TO BUSTAT-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUSTAT-SOURCE-CD
           MOVE BCDM-STEP-PLAN-CD
             TO BUSTAT-STEP-PLAN-CD
           MOVE BCDM-OVERRIDE-FLAG
             TO BUSTAT-OVERRIDE-FLAG
           MOVE BCDM-REVIEW-FLAG
             TO BUSTAT-REVIEW-FLAG
           MOVE BCDM-STOP-FLAG
             TO BUSTAT-STOP-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUSTAT-CYCLE-CNT
           MOVE BCDM-ITEM-CNT
             TO BUSTAT-ITEM-CNT
           MOVE BCDM-PENALTY-PCT
             TO BUSTAT-PENALTY-PCT
           MOVE BCDM-RATE
             TO BUSTAT-RATE
           MOVE BCDM-ADDR
             TO BUSTAT-ADDR
           MOVE BCDM-NAME
             TO BUSTAT-NAME
           EXEC CICS LINK PROGRAM('BUSTAT') COMMAREA(BUSTAT-PARMS)
                LENGTH(LENGTH OF BUSTAT-PARMS) RESP(WS-RESP)
           END-EXEC
           IF BUSTAT-RETURN-CODE = 08
               MOVE 'RULE BUSTAT DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE BCDM-KEY TO KEYO
           MOVE BCDM-LAST-STMT-DATE TO LASTSTO
           MOVE BCDM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE BCDM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE BCDM-CYCLE-CD TO CYCLECO
           MOVE BCDM-STEP-PLAN-CD TO STEPPLO
           MOVE BCDM-PRINCIPAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINCIO
           MOVE BCDM-INTEREST-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTEREO
           MOVE BCDM-CYCLE-CNT TO CYCLEC1O
           MOVE BCDM-CLOSE-DATE TO CLOSEDO
           MOVE BCDM-SUSPEND-FLAG TO SUSPENO
           MOVE BCDM-HOLD-FLAG TO HOLDFLO
           MOVE BCDM-MATURITY-DATE TO MATURIO
           MOVE BCDM-LAST-PAY-DATE TO LASTPAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('BCDMST') INTO(BCD-MASTER-REC)
                RIDFLD(BCDM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STEPPLL > 0
               MOVE STEPPLI TO BCDM-STEP-PLAN-CD
           END-IF
           IF SUSPENL > 0
               MOVE SUSPENI TO BCDM-SUSPEND-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO BCDM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('BCDMST') FROM(BCD-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('BUADDM') MAPSET('BUADDS')
                FROM(BUADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
