      *================================================================*
      * PROGRAM  : SLLST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SL76                                                *
      * PURPOSE  : STUDENT LOANS MAINTENANCE. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SLCAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-01-10 MKL  ORIGINAL                                       *
      * 1995-01-23 TWH  DISBURSE RERUN PROTECTION PRB9077              *
      * 1995-06-28 EWS  CAPINT LIMIT REVISED REQ1573                   *
      * 1995-02-18 JLM  CAPINT CALCULATION CORRECTED INC4514           *
      * 2001-12-21 GAF  PAYMENT ROUNDING - HALF UP PRB2591             *
      * 2002-05-17 SKN  CAPINT NOTICE TEXT PER LEGAL REQ0959           *
      * 2004-05-22 TWH  CAPINT ROUNDING - HALF UP REQ2249              *
      * 2005-03-24 SKN  PAYMENT CUTOFF MOVED PRB7105                   *
      * 2006-08-13 TWH  PAYMENT STATUS CODES ADDED INC9910             *
      * 2006-05-24 DPS  NEW PAYMENT REPORT COLUMNS REQ6952             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SL76'.
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
       COPY STLMST.
       COPY SLLSTS.
       COPY SLCALP.
       COPY DCLSTL.
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
           MOVE LOW-VALUES TO SLLSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SLLSTM') MAPSET('SLLSTS')
                FROM(SLLSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SLLSTM') MAPSET('SLLSTS')
                INTO(SLLSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO STLM-KEY
           EXEC CICS READ FILE('STLMST') INTO(STL-MASTER-REC)
                RIDFLD(STLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'HY'
              AND STATUSI NOT = 'JZ'
              AND STATUSI NOT = 'RK'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF REVIEWL > 0 AND REVIEWI NOT = 'Y' AND REVIEWI NOT = 'N'
               MOVE 'REVIEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF DISBURL > 0
               IF FUNCTION TEST-NUMVAL(DISBURI) NOT = 0
                   MOVE 'DISBURSE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DISBURL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DISBURI)
                   IF WS-NUM-WORK < 0
                       MOVE 'DISBURSE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CAPINTL > 0
               IF FUNCTION TEST-NUMVAL(CAPINTI) NOT = 0
                   MOVE 'CAPINT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CAPINTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CAPINTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CAPINT YTD MAY NOT BE NEGATIVE' TO
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
           INITIALIZE SLCAL-PARMS
           MOVE STLM-CAPINT-AMT
             TO SLCAL-CAPINT-AMT
           MOVE STLM-SUBSIDY-YTD
             TO SLCAL-SUBSIDY-YTD
           MOVE STLM-PAYMENT-AMT
             TO SLCAL-PAYMENT-AMT
           MOVE STLM-PRINCIPAL-AMT
             TO SLCAL-PRINCIPAL-AMT
           MOVE STLM-DEFERINT-YTD
             TO SLCAL-DEFERINT-YTD
           MOVE STLM-DISBURSE-YTD
             TO SLCAL-DISBURSE-YTD
           MOVE STLM-OPEN-DATE
             TO SLCAL-OPEN-DATE
           MOVE STLM-EXPIRY-DATE
             TO SLCAL-EXPIRY-DATE
           MOVE STLM-LAST-ACT-DATE
             TO SLCAL-LAST-ACT-DATE
           MOVE STLM-LAST-PAY-DATE
             TO SLCAL-LAST-PAY-DATE
           MOVE STLM-CYCLE-CD
             TO SLCAL-CYCLE-CD
           MOVE STLM-DEFER-CODE-CD
             TO SLCAL-DEFER-CODE-CD
           MOVE STLM-SOURCE-CD
             TO SLCAL-SOURCE-CD
           MOVE STLM-REGION-CD
             TO SLCAL-REGION-CD
           MOVE STLM-REVIEW-FLAG
             TO SLCAL-REVIEW-FLAG
           MOVE STLM-NOTICE-SENT-FLAG
             TO SLCAL-NOTICE-SENT-FLAG
           MOVE STLM-OVERRIDE-FLAG
             TO SLCAL-OVERRIDE-FLAG
           MOVE STLM-EXCEPT-CNT
             TO SLCAL-EXCEPT-CNT
           MOVE STLM-PAYMENT-CNT
             TO SLCAL-PAYMENT-CNT
           MOVE STLM-PENALTY-PCT
             TO SLCAL-PENALTY-PCT
           MOVE STLM-FEE-PCT
             TO SLCAL-FEE-PCT
           MOVE STLM-NAME
             TO SLCAL-NAME
           MOVE STLM-ADDR
             TO SLCAL-ADDR
           EXEC CICS LINK PROGRAM('SLCAL') COMMAREA(SLCAL-PARMS)
                LENGTH(LENGTH OF SLCAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SLCAL-RETURN-CODE = 08
               MOVE 'RULE SLCAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE STLM-KEY TO KEYO
           MOVE STLM-STATUS-CD TO STATUSO
           MOVE STLM-REVIEW-FLAG TO REVIEWO
           MOVE STLM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE STLM-SUSPEND-FLAG TO SUSPENO
           MOVE STLM-DISBURSE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISBURO
           MOVE STLM-LAST-ACT-DATE TO LASTACO
           MOVE STLM-EXPIRY-DATE TO EXPIRYO
           MOVE STLM-ITEM-CNT TO ITEMCNO
           MOVE STLM-LAST-MAINT-DATE TO LASTMAO
           MOVE STLM-MATURITY-DATE TO MATURIO
           MOVE STLM-CAPINT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CAPINTO
           MOVE STLM-OPEN-DATE TO OPENDAO
           MOVE STLM-PAYMENT-CNT TO PAYMENO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('STLMST') INTO(STL-MASTER-REC)
                RIDFLD(STLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO STLM-STATUS-CD
           END-IF
           IF REVIEWL > 0
               MOVE REVIEWI TO STLM-REVIEW-FLAG
           END-IF
           IF DISBURL > 0
               COMPUTE STLM-DISBURSE-YTD = FUNCTION NUMVAL(DISBURI)
           END-IF
           IF CAPINTL > 0
               COMPUTE STLM-CAPINT-YTD = FUNCTION NUMVAL(CAPINTI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO STLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('STLMST') FROM(STL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('SLLSTM') MAPSET('SLLSTS')
                FROM(SLLSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
