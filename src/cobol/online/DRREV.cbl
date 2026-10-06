      *================================================================*
      * PROGRAM  : DRREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : DR90                                                *
      * PURPOSE  : DIVIDEND REINVESTMENT INQUIRY. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : DRLIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-10-15 JLM  ORIGINAL                                       *
      * 1989-03-15 PQR  AUDIT FINDING - REINVEST CONTROL TOTALS INC7682*
      * 1995-07-28 JLM  DIVIDEND EXCEPTIONS TO OPS QUEUE PRB3136       *
      * 1997-01-18 CRP  SHARES-AMT LIMIT REVISED INC0941               *
      * 1999-02-05 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-07-24 PQR  PERFORMANCE - WITHHOLD READ AHEAD INC2184      *
      * 2002-06-16 RJT  WITHHOLD FEE SCHEDULE UPDATE CHG8121           *
      * 2003-04-26 BHS  CASH-PAY FEE SCHEDULE UPDATE INC2805           *
      * 2003-03-28 PQR  PERFORMANCE - DIVIDEND READ AHEAD REQ3030      *
      * 2005-09-02 GAF  REINVEST FEE SCHEDULE UPDATE INC5060           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'DR90'.
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
       COPY DRPMST.
       COPY DRREVS.
       COPY DRLIMP.
       COPY DCLDRP.
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
           MOVE LOW-VALUES TO DRREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('DRREVM') MAPSET('DRREVS')
                FROM(DRREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('DRREVM') MAPSET('DRREVS')
                INTO(DRREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO DRPM-KEY
           EXEC CICS READ FILE('DRPMST') INTO(DRP-MASTER-REC)
                RIDFLD(DRPM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DIVIDE1L > 0
               IF FUNCTION TEST-NUMVAL(DIVIDE1I) NOT = 0
                   MOVE 'DIVIDEND AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DIVIDE1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DIVIDE1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'DIVIDEND AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CYCLECL > 0
              AND CYCLECI NOT = 'DE'
              AND CYCLECI NOT = 'FB'
              AND CYCLECI NOT = 'PC'
               MOVE 'CYCLE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CYCLECL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF CHANNEL > 0
              AND CHANNEI NOT = 'RM'
              AND CHANNEI NOT = 'VP'
              AND CHANNEI NOT = 'YS'
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
           INITIALIZE DRLIM-PARMS
           MOVE DRPM-CASH-PAY-AMT
             TO DRLIM-CASH-PAY-AMT
           MOVE DRPM-DIVIDEND-AMT
             TO DRLIM-DIVIDEND-AMT
           MOVE DRPM-CASH-PAY-YTD
             TO DRLIM-CASH-PAY-YTD
           MOVE DRPM-WITHHOLD-YTD
             TO DRLIM-WITHHOLD-YTD
           MOVE DRPM-WITHHOLD-AMT
             TO DRLIM-WITHHOLD-AMT
           MOVE DRPM-REINVEST-AMT
             TO DRLIM-REINVEST-AMT
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRLIM-NEXT-REVIEW-DATE
           MOVE DRPM-LAST-PAY-DATE
             TO DRLIM-LAST-PAY-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRLIM-MATURITY-DATE
           MOVE DRPM-LAST-MAINT-DATE
             TO DRLIM-LAST-MAINT-DATE
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRLIM-HOLDER-TYPE-CD
           MOVE DRPM-CHANNEL-CD
             TO DRLIM-CHANNEL-CD
           MOVE DRPM-SOURCE-CD
             TO DRLIM-SOURCE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRLIM-PLAN-OPTION-CD
           MOVE DRPM-EXEMPT-FLAG
             TO DRLIM-EXEMPT-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRLIM-PREFERRED-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRLIM-SUSPEND-FLAG
           MOVE DRPM-PAYMENT-CNT
             TO DRLIM-PAYMENT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRLIM-NOTICE-CNT
           MOVE DRPM-RATE
             TO DRLIM-RATE
           MOVE DRPM-DISCOUNT
             TO DRLIM-DISCOUNT
           MOVE DRPM-CUST-ID
             TO DRLIM-CUST-ID
           MOVE DRPM-DESC
             TO DRLIM-DESC
           EXEC CICS LINK PROGRAM('DRLIM') COMMAREA(DRLIM-PARMS)
                LENGTH(LENGTH OF DRLIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF DRLIM-RETURN-CODE = 08
               MOVE 'RULE DRLIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE DRPM-KEY TO KEYO
           MOVE DRPM-DIVIDEND-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DIVIDEO
           MOVE DRPM-NAME TO NAMEO
           MOVE DRPM-DIVIDEND-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DIVIDE1O
           MOVE DRPM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE DRPM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE DRPM-CYCLE-CD TO CYCLECO
           MOVE DRPM-LAST-PAY-DATE TO LASTPAO
           MOVE DRPM-OVERRIDE-FLAG TO OVERRIO
           MOVE DRPM-CONTACT TO CONTACO
           MOVE DRPM-REINVEST-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REINVEO
           MOVE DRPM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE DRPM-ITEM-CNT TO ITEMCNO
           MOVE DRPM-CHANNEL-CD TO CHANNEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('DRPMST') INTO(DRP-MASTER-REC)
                RIDFLD(DRPM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DIVIDE1L > 0
               COMPUTE DRPM-DIVIDEND-AMT = FUNCTION NUMVAL(DIVIDE1I)
           END-IF
           IF CYCLECL > 0
               MOVE CYCLECI TO DRPM-CYCLE-CD
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO DRPM-OVERRIDE-FLAG
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO DRPM-CHANNEL-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO DRPM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('DRPMST') FROM(DRP-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('DRREVM') MAPSET('DRREVS')
                FROM(DRREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
