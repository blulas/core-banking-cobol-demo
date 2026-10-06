      *================================================================*
      * PROGRAM  : YKHST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : YK28                                                *
      * PURPOSE  : DATE WINDOW BRIDGE MAINTENANCE. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : YKTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-04 JLM  ORIGINAL                                       *
      * 2003-03-15 MKL  AMOUNT-1 ROUNDING - HALF UP INC7746            *
      * 2005-03-07 PQR  AMOUNT-1 CUTOFF MOVED INC6491                  *
      * 2006-09-09 RJT  CONTROL CUTOFF MOVED PRB5952                   *
      * 2008-06-27 BHS  AMOUNT-1 FEE SCHEDULE UPDATE REQ6433           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKHST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'YK28'.
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
       COPY YDBMST.
       COPY YKHSTS.
       COPY YKTIERP.
       COPY DCLYDB.
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
           MOVE LOW-VALUES TO YKHSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('YKHSTM') MAPSET('YKHSTS')
                FROM(YKHSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('YKHSTM') MAPSET('YKHSTS')
                INTO(YKHSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO YDBM-KEY
           EXEC CICS READ FILE('YDBMST') INTO(YDB-MASTER-REC)
                RIDFLD(YDBM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXEMPTL > 0 AND EXEMPTI NOT = 'Y' AND EXEMPTI NOT = 'N'
               MOVE 'EXEMPT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF AMOUNTL > 0
               IF FUNCTION TEST-NUMVAL(AMOUNTI) NOT = 0
                   MOVE 'AMOUNT 1 YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO AMOUNTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(AMOUNTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'AMOUNT 1 YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF AMOUNT1L > 0
               IF FUNCTION TEST-NUMVAL(AMOUNT1I) NOT = 0
                   MOVE 'AMOUNT 1 AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO AMOUNT1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(AMOUNT1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'AMOUNT 1 AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE YKTIER-PARMS
           MOVE YDBM-AMOUNT-2-YTD
             TO YKTIER-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKTIER-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKTIER-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKTIER-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKTIER-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKTIER-CONTROL-AMT
           MOVE YDBM-LAST-ACT-DATE
             TO YKTIER-LAST-ACT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKTIER-MATURITY-DATE
           MOVE YDBM-LAST-MAINT-DATE
             TO YKTIER-LAST-MAINT-DATE
           MOVE YDBM-EXPIRY-DATE
             TO YKTIER-EXPIRY-DATE
           MOVE YDBM-CYCLE-CD
             TO YKTIER-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKTIER-SOURCE-CD
           MOVE YDBM-REGION-CD
             TO YKTIER-REGION-CD
           MOVE YDBM-PIVOT-CD
             TO YKTIER-PIVOT-CD
           MOVE YDBM-PREFERRED-FLAG
             TO YKTIER-PREFERRED-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKTIER-AUTO-RENEW-FLAG
           MOVE YDBM-REVIEW-FLAG
             TO YKTIER-REVIEW-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKTIER-ITEM-CNT
           MOVE YDBM-RETRY-CNT
             TO YKTIER-RETRY-CNT
           MOVE YDBM-DISCOUNT
             TO YKTIER-DISCOUNT
           MOVE YDBM-PENALTY-PCT
             TO YKTIER-PENALTY-PCT
           MOVE YDBM-CUST-ID
             TO YKTIER-CUST-ID
           MOVE YDBM-CONTACT
             TO YKTIER-CONTACT
           EXEC CICS LINK PROGRAM('YKTIER') COMMAREA(YKTIER-PARMS)
                LENGTH(LENGTH OF YKTIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF YKTIER-RETURN-CODE = 08
               MOVE 'RULE YKTIER DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE YDBM-KEY TO KEYO
           MOVE YDBM-NEXT-DUE-DATE TO NEXTDUO
           MOVE YDBM-CONTROL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CONTROO
           MOVE YDBM-EXEMPT-FLAG TO EXEMPTO
           MOVE YDBM-CYCLE-CNT TO CYCLECO
           MOVE YDBM-AMOUNT-1-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO AMOUNTO
           MOVE YDBM-CUST-ID TO CUSTIDO
           MOVE YDBM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE YDBM-OVERRIDE-FLAG TO OVERRIO
           MOVE YDBM-NAME TO NAMEO
           MOVE YDBM-REASON-CD TO REASONO
           MOVE YDBM-AMOUNT-1-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO AMOUNT1O
           MOVE YDBM-DESC TO DESCO
           MOVE YDBM-RETRY-CNT TO RETRYCO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('YDBMST') INTO(YDB-MASTER-REC)
                RIDFLD(YDBM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO YDBM-EXEMPT-FLAG
           END-IF
           IF AMOUNTL > 0
               COMPUTE YDBM-AMOUNT-1-YTD = FUNCTION NUMVAL(AMOUNTI)
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO YDBM-OVERRIDE-FLAG
           END-IF
           IF AMOUNT1L > 0
               COMPUTE YDBM-AMOUNT-1-AMT = FUNCTION NUMVAL(AMOUNT1I)
           END-IF
           MOVE CTL-BUSINESS-DATE TO YDBM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('YDBMST') FROM(YDB-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('YKHSTM') MAPSET('YKHSTS')
                FROM(YKHSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
