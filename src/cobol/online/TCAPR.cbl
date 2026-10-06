      *================================================================*
      * PROGRAM  : TCAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TC65                                                *
      * PURPOSE  : TRAVELERS CHEQUES REVIEW. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TCCAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-02-21 LMB  ORIGINAL                                       *
      * 1991-06-24 GAF  AUDIT FINDING - CONSIGN CONTROL TOTALS INC2944 *
      * 1994-05-23 SKN  FACE CALCULATION CORRECTED INC5659             *
      * 2007-11-18 SKN  FACE TABLE EXPANDED REQ9523                    *
      * 2014-03-27 CRP  SHORTAGE RERUN PROTECTION REQ6419              *
      * 2014-05-15 PQR  REMIT FEE SCHEDULE UPDATE PRB5400              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TC65'.
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
       COPY TCAPRS.
       COPY TCCALP.
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
           MOVE LOW-VALUES TO TCAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TCAPRM') MAPSET('TCAPRS')
                FROM(TCAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TCAPRM') MAPSET('TCAPRS')
                INTO(TCAPRMI) RESP(WS-RESP) END-EXEC
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
           IF ISSUERL > 0
              AND ISSUERI NOT = 'AF'
              AND ISSUERI NOT = 'CJ'
              AND ISSUERI NOT = 'DU'
               MOVE 'ISSUER CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO ISSUERL
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
           INITIALIZE TCCAL-PARMS
           MOVE TRCM-COMMISSION-YTD
             TO TCCAL-COMMISSION-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCCAL-SHORTAGE-YTD
           MOVE TRCM-COMMISSION-AMT
             TO TCCAL-COMMISSION-AMT
           MOVE TRCM-SHORTAGE-AMT
             TO TCCAL-SHORTAGE-AMT
           MOVE TRCM-CONSIGN-AMT
             TO TCCAL-CONSIGN-AMT
           MOVE TRCM-CONSIGN-YTD
             TO TCCAL-CONSIGN-YTD
           MOVE TRCM-MATURITY-DATE
             TO TCCAL-MATURITY-DATE
           MOVE TRCM-LAST-ACT-DATE
             TO TCCAL-LAST-ACT-DATE
           MOVE TRCM-EFFECTIVE-DATE
             TO TCCAL-EFFECTIVE-DATE
           MOVE TRCM-NEXT-DUE-DATE
             TO TCCAL-NEXT-DUE-DATE
           MOVE TRCM-BRANCH-CD
             TO TCCAL-BRANCH-CD
           MOVE TRCM-DENOM-CD
             TO TCCAL-DENOM-CD
           MOVE TRCM-ISSUER-CD
             TO TCCAL-ISSUER-CD
           MOVE TRCM-SOURCE-CD
             TO TCCAL-SOURCE-CD
           MOVE TRCM-EXEMPT-FLAG
             TO TCCAL-EXEMPT-FLAG
           MOVE TRCM-REVIEW-FLAG
             TO TCCAL-REVIEW-FLAG
           MOVE TRCM-STOP-FLAG
             TO TCCAL-STOP-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCCAL-EXCEPT-CNT
           MOVE TRCM-ITEM-CNT
             TO TCCAL-ITEM-CNT
           MOVE TRCM-RATE
             TO TCCAL-RATE
           MOVE TRCM-FEE-PCT
             TO TCCAL-FEE-PCT
           MOVE TRCM-NAME
             TO TCCAL-NAME
           MOVE TRCM-ADDR
             TO TCCAL-ADDR
           EXEC CICS LINK PROGRAM('TCCAL') COMMAREA(TCCAL-PARMS)
                LENGTH(LENGTH OF TCCAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TCCAL-RETURN-CODE = 08
               MOVE 'RULE TCCAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TRCM-KEY TO KEYO
           MOVE TRCM-COMMISSION-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO COMMISO
           MOVE TRCM-CONTACT TO CONTACO
           MOVE TRCM-CYCLE-CNT TO CYCLECO
           MOVE TRCM-FACE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FACEYTO
           MOVE TRCM-EXCEPT-CNT TO EXCEPTO
           MOVE TRCM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE TRCM-LAST-PAY-DATE TO LASTPAO
           MOVE TRCM-SUSPEND-FLAG TO SUSPENO
           MOVE TRCM-REMIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO REMITAO
           MOVE TRCM-ISSUER-CD TO ISSUERO
           MOVE TRCM-STOP-FLAG TO STOPFLO
           MOVE TRCM-CONSIGN-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CONSIGO
           MOVE TRCM-REMIT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REMITYO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TRCMST') INTO(TRC-MASTER-REC)
                RIDFLD(TRCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF ISSUERL > 0
               MOVE ISSUERI TO TRCM-ISSUER-CD
           END-IF
           EXEC CICS REWRITE FILE('TRCMST') FROM(TRC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TCAPRM') MAPSET('TCAPRS')
                FROM(TCAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
