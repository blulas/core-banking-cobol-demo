      *================================================================*
      * PROGRAM  : IRREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : IR26                                                *
      * PURPOSE  : IRA CUSTODY INQUIRY. ENTER VALIDATES AND DISPLAYS,  *
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : IRVAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-08 EWS  ORIGINAL                                       *
      * 1997-08-23 DPS  WITHHOLD CALCULATION CORRECTED REQ5358         *
      * 1998-01-15 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-20 LMB  AUDIT FINDING - ROLLOVER CONTROL TOTALS CHG3233*
      * 2011-07-07 EWS  WITHHOLD TABLE EXPANDED REQ9476                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'IR26'.
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
       COPY IRAMST.
       COPY IRREVS.
       COPY IRVALP.
       COPY DCLIRA.
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
           MOVE LOW-VALUES TO IRREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('IRREVM') MAPSET('IRREVS')
                FROM(IRREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('IRREVM') MAPSET('IRREVS')
                INTO(IRREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO IRAM-KEY
           EXEC CICS READ FILE('IRAMST') INTO(IRA-MASTER-REC)
                RIDFLD(IRAM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CONTRIL > 0
               IF FUNCTION TEST-NUMVAL(CONTRII) NOT = 0
                   MOVE 'CONTRIB YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CONTRIL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CONTRII)
                   IF WS-NUM-WORK < 0
                       MOVE 'CONTRIB YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CHANNEL > 0
              AND CHANNEI NOT = 'AS'
              AND CHANNEI NOT = 'AY'
              AND CHANNEI NOT = 'JR'
               MOVE 'CHANNEL CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CHANNEL
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
           INITIALIZE IRVAL-PARMS
           MOVE IRAM-WITHHOLD-YTD
             TO IRVAL-WITHHOLD-YTD
           MOVE IRAM-EXCESS-AMT
             TO IRVAL-EXCESS-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRVAL-ROLLOVER-AMT
           MOVE IRAM-RMD-YTD
             TO IRVAL-RMD-YTD
           MOVE IRAM-FMV-YTD
             TO IRVAL-FMV-YTD
           MOVE IRAM-FMV-AMT
             TO IRVAL-FMV-AMT
           MOVE IRAM-NEXT-DUE-DATE
             TO IRVAL-NEXT-DUE-DATE
           MOVE IRAM-EFFECTIVE-DATE
             TO IRVAL-EFFECTIVE-DATE
           MOVE IRAM-MATURITY-DATE
             TO IRVAL-MATURITY-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRVAL-LAST-STMT-DATE
           MOVE IRAM-TYPE-CD
             TO IRVAL-TYPE-CD
           MOVE IRAM-DIST-CODE-CD
             TO IRVAL-DIST-CODE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRVAL-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRVAL-REGION-CD
           MOVE IRAM-HOLD-FLAG
             TO IRVAL-HOLD-FLAG
           MOVE IRAM-OVERRIDE-FLAG
             TO IRVAL-OVERRIDE-FLAG
           MOVE IRAM-STOP-FLAG
             TO IRVAL-STOP-FLAG
           MOVE IRAM-ITEM-CNT
             TO IRVAL-ITEM-CNT
           MOVE IRAM-CYCLE-CNT
             TO IRVAL-CYCLE-CNT
           MOVE IRAM-RATE
             TO IRVAL-RATE
           MOVE IRAM-MARGIN
             TO IRVAL-MARGIN
           MOVE IRAM-CONTACT
             TO IRVAL-CONTACT
           MOVE IRAM-ADDR
             TO IRVAL-ADDR
           EXEC CICS LINK PROGRAM('IRVAL') COMMAREA(IRVAL-PARMS)
                LENGTH(LENGTH OF IRVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF IRVAL-RETURN-CODE = 08
               MOVE 'RULE IRVAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE IRAM-KEY TO KEYO
           MOVE IRAM-CONTRIB-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CONTRIO
           MOVE IRAM-PAYMENT-CNT TO PAYMENO
           MOVE IRAM-STOP-FLAG TO STOPFLO
           MOVE IRAM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE IRAM-DISTRIB-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISTRIO
           MOVE IRAM-DISTRIB-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISTRI1O
           MOVE IRAM-REASON-CD TO REASONO
           MOVE IRAM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE IRAM-REVIEW-FLAG TO REVIEWO
           MOVE IRAM-RMD-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RMDYTDO
           MOVE IRAM-CHANNEL-CD TO CHANNEO
           MOVE IRAM-EXCESS-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCESSO
           MOVE IRAM-MATURITY-DATE TO MATURIO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('IRAMST') INTO(IRA-MASTER-REC)
                RIDFLD(IRAM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CONTRIL > 0
               COMPUTE IRAM-CONTRIB-YTD = FUNCTION NUMVAL(CONTRII)
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO IRAM-CHANNEL-CD
           END-IF
           EXEC CICS REWRITE FILE('IRAMST') FROM(IRA-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('IRREVM') MAPSET('IRREVS')
                FROM(IRREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
