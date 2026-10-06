      *================================================================*
      * PROGRAM  : AROVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : AR85                                                *
      * PURPOSE  : ACCOUNT RECONCILEMENT MAINTENANCE. ENTER VALIDATES  *
      *            AND DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO*
      *            THE MENU.                                           *
      * CALLS    : ARSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-28 GAF  ORIGINAL                                       *
      * 2006-01-05 MKL  FEE TABLE EXPANDED CHG9519                     *
      * 2006-07-09 DPS  PAID LIMIT REVISED PRB2308                     *
      * 2006-01-11 CRP  STOP TABLE EXPANDED INC6208                    *
      * 2013-06-19 DPS  PAID NOTICE TEXT PER LEGAL CHG2724             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AROVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'AR85'.
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
       COPY ARPMST.
       COPY AROVRS.
       COPY ARSTATP.
       COPY DCLARP.
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
           MOVE LOW-VALUES TO AROVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('AROVRM') MAPSET('AROVRS')
                FROM(AROVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('AROVRM') MAPSET('AROVRS')
                INTO(AROVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO ARPM-KEY
           EXEC CICS READ FILE('ARPMST') INTO(ARP-MASTER-REC)
                RIDFLD(ARPM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REGIONL > 0
              AND REGIONI NOT = 'AD'
              AND REGIONI NOT = 'BU'
              AND REGIONI NOT = 'GM'
               MOVE 'REGION CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REGIONL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SOURCEL > 0
              AND SOURCEI NOT = 'GW'
              AND SOURCEI NOT = 'MB'
              AND SOURCEI NOT = 'MH'
               MOVE 'SOURCE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO SOURCEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF CYCLEC1L > 0
              AND CYCLEC1I NOT = 'CP'
              AND CYCLEC1I NOT = 'MR'
              AND CYCLEC1I NOT = 'PY'
               MOVE 'CYCLE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CYCLEC1L
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE ARSTAT-PARMS
           MOVE ARPM-ISSUED-YTD
             TO ARSTAT-ISSUED-YTD
           MOVE ARPM-FEE-AMT
             TO ARSTAT-FEE-AMT
           MOVE ARPM-OUTSTAND-AMT
             TO ARSTAT-OUTSTAND-AMT
           MOVE ARPM-PAID-AMT
             TO ARSTAT-PAID-AMT
           MOVE ARPM-ISSUED-AMT
             TO ARSTAT-ISSUED-AMT
           MOVE ARPM-OUTSTAND-YTD
             TO ARSTAT-OUTSTAND-YTD
           MOVE ARPM-CLOSE-DATE
             TO ARSTAT-CLOSE-DATE
           MOVE ARPM-LAST-STMT-DATE
             TO ARSTAT-LAST-STMT-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARSTAT-NEXT-REVIEW-DATE
           MOVE ARPM-EFFECTIVE-DATE
             TO ARSTAT-EFFECTIVE-DATE
           MOVE ARPM-BRANCH-CD
             TO ARSTAT-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARSTAT-CHANNEL-CD
           MOVE ARPM-CYCLE-CD
             TO ARSTAT-CYCLE-CD
           MOVE ARPM-SOURCE-CD
             TO ARSTAT-SOURCE-CD
           MOVE ARPM-NOTICE-SENT-FLAG
             TO ARSTAT-NOTICE-SENT-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARSTAT-PREFERRED-FLAG
           MOVE ARPM-OVERRIDE-FLAG
             TO ARSTAT-OVERRIDE-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARSTAT-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARSTAT-CYCLE-CNT
           MOVE ARPM-PENALTY-PCT
             TO ARSTAT-PENALTY-PCT
           MOVE ARPM-DISCOUNT
             TO ARSTAT-DISCOUNT
           MOVE ARPM-CUST-ID
             TO ARSTAT-CUST-ID
           MOVE ARPM-REF-NO
             TO ARSTAT-REF-NO
           EXEC CICS LINK PROGRAM('ARSTAT') COMMAREA(ARSTAT-PARMS)
                LENGTH(LENGTH OF ARSTAT-PARMS) RESP(WS-RESP)
           END-EXEC
           IF ARSTAT-RETURN-CODE = 08
               MOVE 'RULE ARSTAT DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ARPM-KEY TO KEYO
           MOVE ARPM-CYCLE-CNT TO CYCLECO
           MOVE ARPM-LAST-ACT-DATE TO LASTACO
           MOVE ARPM-REGION-CD TO REGIONO
           MOVE ARPM-PAID-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAIDAMO
           MOVE ARPM-SOURCE-CD TO SOURCEO
           MOVE ARPM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE ARPM-LAST-MAINT-DATE TO LASTMAO
           MOVE ARPM-ISSUED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEDO
           MOVE ARPM-NEXT-DUE-DATE TO NEXTDUO
           MOVE ARPM-CYCLE-CD TO CYCLEC1O
           MOVE ARPM-EFFECTIVE-DATE TO EFFECTO
           MOVE ARPM-VOID-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO VOIDYTO
           MOVE ARPM-STOP-FLAG TO STOPFLO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ARPMST') INTO(ARP-MASTER-REC)
                RIDFLD(ARPM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REGIONL > 0
               MOVE REGIONI TO ARPM-REGION-CD
           END-IF
           IF SOURCEL > 0
               MOVE SOURCEI TO ARPM-SOURCE-CD
           END-IF
           IF CYCLEC1L > 0
               MOVE CYCLEC1I TO ARPM-CYCLE-CD
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO ARPM-STOP-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO ARPM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('ARPMST') FROM(ARP-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('AROVRM') MAPSET('AROVRS')
                FROM(AROVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
