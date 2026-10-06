      *================================================================*
      * PROGRAM  : TCOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TC66                                                *
      * PURPOSE  : TRAVELERS CHEQUES MAINTENANCE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TCDTE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-02-19 PQR  ORIGINAL                                       *
      * 1987-10-06 GAF  NEW SHORTAGE REPORT COLUMNS REQ3647            *
      * 1987-01-22 CRP  AUDIT FINDING - REMIT CONTROL TOTALS CHG0760   *
      * 1993-10-25 SKN  SHORTAGE CUTOFF MOVED REQ5061                  *
      * 1997-01-04 CRP  REMIT TABLE EXPANDED INC1394                   *
      * 1999-02-05 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-03 MKL  COMMISSION CUTOFF MOVED INC3535                *
      * 2002-11-15 CRP  NEW SHORTAGE REPORT COLUMNS CHG2722            *
      * 2006-08-05 CRP  REFUND ROUNDING - HALF UP INC1378              *
      * 2013-03-25 MKL  FACE RERUN PROTECTION PRB5554                  *
      * 2014-04-12 MKL  NEW CONSIGN REPORT COLUMNS PRB3518             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TC66'.
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
       COPY TRCMST.
       COPY TCOVRS.
       COPY TCDTEP.
       COPY DCLTRC.
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
           MOVE LOW-VALUES TO TCOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TCOVRM') MAPSET('TCOVRS')
                FROM(TCOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TCOVRM') MAPSET('TCOVRS')
                INTO(TCOVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO TRCM-KEY
           EXEC CICS READ FILE('TRCMST') INTO(TRC-MASTER-REC)
                RIDFLD(TRCM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DENOMCL > 0
              AND DENOMCI NOT = 'G'
              AND DENOMCI NOT = 'M'
              AND DENOMCI NOT = 'N'
               MOVE 'DENOM CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO DENOMCL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'HK'
              AND STATUSI NOT = 'RU'
              AND STATUSI NOT = 'WA'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SHORTAL > 0
               IF FUNCTION TEST-NUMVAL(SHORTAI) NOT = 0
                   MOVE 'SHORTAGE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO SHORTAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(SHORTAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'SHORTAGE AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF COMMISL > 0
               IF FUNCTION TEST-NUMVAL(COMMISI) NOT = 0
                   MOVE 'COMMISSION AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO COMMISL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(COMMISI)
                   IF WS-NUM-WORK < 0
                       MOVE 'COMMISSION AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE TCDTE-PARMS
           MOVE TRCM-SHORTAGE-AMT
             TO TCDTE-SHORTAGE-AMT
           MOVE TRCM-COMMISSION-YTD
             TO TCDTE-COMMISSION-YTD
           MOVE TRCM-REFUND-YTD
             TO TCDTE-REFUND-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCDTE-SHORTAGE-YTD
           MOVE TRCM-CONSIGN-AMT
             TO TCDTE-CONSIGN-AMT
           MOVE TRCM-REFUND-AMT
             TO TCDTE-REFUND-AMT
           MOVE TRCM-MATURITY-DATE
             TO TCDTE-MATURITY-DATE
           MOVE TRCM-CLOSE-DATE
             TO TCDTE-CLOSE-DATE
           MOVE TRCM-NEXT-DUE-DATE
             TO TCDTE-NEXT-DUE-DATE
           MOVE TRCM-LAST-ACT-DATE
             TO TCDTE-LAST-ACT-DATE
           MOVE TRCM-BRANCH-CD
             TO TCDTE-BRANCH-CD
           MOVE TRCM-ISSUER-CD
             TO TCDTE-ISSUER-CD
           MOVE TRCM-CYCLE-CD
             TO TCDTE-CYCLE-CD
           MOVE TRCM-STATUS-CD
             TO TCDTE-STATUS-CD
           MOVE TRCM-STOP-FLAG
             TO TCDTE-STOP-FLAG
           MOVE TRCM-OVERRIDE-FLAG
             TO TCDTE-OVERRIDE-FLAG
           MOVE TRCM-REVIEW-FLAG
             TO TCDTE-REVIEW-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCDTE-EXCEPT-CNT
           MOVE TRCM-ITEM-CNT
             TO TCDTE-ITEM-CNT
           MOVE TRCM-RATE
             TO TCDTE-RATE
           MOVE TRCM-MARGIN
             TO TCDTE-MARGIN
           MOVE TRCM-REF-NO
             TO TCDTE-REF-NO
           MOVE TRCM-CUST-ID
             TO TCDTE-CUST-ID
           EXEC CICS LINK PROGRAM('TCDTE') COMMAREA(TCDTE-PARMS)
                LENGTH(LENGTH OF TCDTE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TCDTE-RETURN-CODE = 08
               MOVE 'RULE TCDTE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TRCM-KEY TO KEYO
           MOVE TRCM-LAST-STMT-DATE TO LASTSTO
           MOVE TRCM-NAME TO NAMEO
           MOVE TRCM-DENOM-CD TO DENOMCO
           MOVE TRCM-REFUND-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REFUNDO
           MOVE TRCM-STATUS-CD TO STATUSO
           MOVE TRCM-SUSPEND-FLAG TO SUSPENO
           MOVE TRCM-SHORTAGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO SHORTAO
           MOVE TRCM-LAST-PAY-DATE TO LASTPAO
           MOVE TRCM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE TRCM-COMMISSION-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COMMISO
           MOVE TRCM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE TRCM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE TRCM-RETRY-CNT TO RETRYCO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TRCMST') INTO(TRC-MASTER-REC)
                RIDFLD(TRCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DENOMCL > 0
               MOVE DENOMCI TO TRCM-DENOM-CD
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO TRCM-STATUS-CD
           END-IF
           IF SHORTAL > 0
               COMPUTE TRCM-SHORTAGE-AMT = FUNCTION NUMVAL(SHORTAI)
           END-IF
           IF COMMISL > 0
               COMPUTE TRCM-COMMISSION-AMT = FUNCTION NUMVAL(COMMISI)
           END-IF
           EXEC CICS REWRITE FILE('TRCMST') FROM(TRC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TCOVRM') MAPSET('TCOVRS')
                FROM(TCOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
