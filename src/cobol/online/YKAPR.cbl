      *================================================================*
      * PROGRAM  : YKAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : YK94                                                *
      * PURPOSE  : DATE WINDOW BRIDGE HISTORY. ENTER VALIDATES AND     *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : YKPRC, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-16 DPS  ORIGINAL                                       *
      * 1999-11-08 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-06-18 PQR  CONTROL STATUS CODES ADDED CHG0233             *
      * 2005-03-17 MKL  AMOUNT-1 ROUNDING - HALF UP REQ9494            *
      * 2006-12-22 DPS  AMOUNT-1 RERUN PROTECTION REQ3854              *
      * 2006-12-15 DPS  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS REQ1419*
      * 2007-01-15 LMB  CONTROL LIMIT REVISED CHG2629                  *
      * 2007-05-03 EWS  CONTROL CUTOFF MOVED REQ8239                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'YK94'.
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
       COPY YKAPRS.
       COPY YKPRCP.
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
           MOVE LOW-VALUES TO YKAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('YKAPRM') MAPSET('YKAPRS')
                FROM(YKAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('YKAPRM') MAPSET('YKAPRS')
                INTO(YKAPRMI) RESP(WS-RESP) END-EXEC
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
           IF AMOUNTL > 0
               IF FUNCTION TEST-NUMVAL(AMOUNTI) NOT = 0
                   MOVE 'AMOUNT 2 YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO AMOUNTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(AMOUNTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'AMOUNT 2 YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF PREFERL > 0 AND PREFERI NOT = 'Y' AND PREFERI NOT = 'N'
               MOVE 'PREFERRED FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE YKPRC-PARMS
           MOVE YDBM-AMOUNT-1-AMT
             TO YKPRC-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-YTD
             TO YKPRC-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKPRC-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKPRC-AMOUNT-2-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKPRC-CONTROL-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKPRC-AMOUNT-2-YTD
           MOVE YDBM-EXPIRY-DATE
             TO YKPRC-EXPIRY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKPRC-LAST-STMT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKPRC-MATURITY-DATE
           MOVE YDBM-LAST-ACT-DATE
             TO YKPRC-LAST-ACT-DATE
           MOVE YDBM-CYCLE-CD
             TO YKPRC-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKPRC-SOURCE-CD
           MOVE YDBM-PIVOT-CD
             TO YKPRC-PIVOT-CD
           MOVE YDBM-REGION-CD
             TO YKPRC-REGION-CD
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKPRC-NOTICE-SENT-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKPRC-AUTO-RENEW-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKPRC-PREFERRED-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKPRC-ITEM-CNT
           MOVE YDBM-PAYMENT-CNT
             TO YKPRC-PAYMENT-CNT
           MOVE YDBM-MARGIN
             TO YKPRC-MARGIN
           MOVE YDBM-DISCOUNT
             TO YKPRC-DISCOUNT
           MOVE YDBM-NAME
             TO YKPRC-NAME
           MOVE YDBM-CONTACT
             TO YKPRC-CONTACT
           EXEC CICS LINK PROGRAM('YKPRC') COMMAREA(YKPRC-PARMS)
                LENGTH(LENGTH OF YKPRC-PARMS) RESP(WS-RESP)
           END-EXEC
           IF YKPRC-RETURN-CODE = 08
               MOVE 'RULE YKPRC DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE YDBM-KEY TO KEYO
           MOVE YDBM-LAST-ACT-DATE TO LASTACO
           MOVE YDBM-CUST-ID TO CUSTIDO
           MOVE YDBM-CYCLE-CNT TO CYCLECO
           MOVE YDBM-AMOUNT-2-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO AMOUNTO
           MOVE YDBM-OVERRIDE-FLAG TO OVERRIO
           MOVE YDBM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE YDBM-REF-NO TO REFNOO
           MOVE YDBM-DESC TO DESCO
           MOVE YDBM-PREFERRED-FLAG TO PREFERO
           MOVE YDBM-ITEM-CNT TO ITEMCNO
           MOVE YDBM-AMOUNT-2-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO AMOUNT1O
           MOVE YDBM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE YDBM-AMOUNT-1-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO AMOUNT2O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('YDBMST') INTO(YDB-MASTER-REC)
                RIDFLD(YDBM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF AMOUNTL > 0
               COMPUTE YDBM-AMOUNT-2-YTD = FUNCTION NUMVAL(AMOUNTI)
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO YDBM-OVERRIDE-FLAG
           END-IF
           IF PREFERL > 0
               MOVE PREFERI TO YDBM-PREFERRED-FLAG
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
           EXEC CICS SEND MAP('YKAPRM') MAPSET('YKAPRS')
                FROM(YKAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
