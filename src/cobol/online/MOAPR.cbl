      *================================================================*
      * PROGRAM  : MOAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : MO86                                                *
      * PURPOSE  : MONEY ORDERS INQUIRY. ENTER VALIDATES AND DISPLAYS, *
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : MORISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-28 GAF  ORIGINAL                                       *
      * 1996-03-02 PQR  VOIDED TABLE EXPANDED CHG0360                  *
      * 1997-01-01 GAF  PERFORMANCE - PAID READ AHEAD PRB9588          *
      * 1998-04-07 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-15 JLM  PERFORMANCE - OUTSTAND READ AHEAD INC6161      *
      * 1999-12-03 SKN  OUTSTAND FEE SCHEDULE UPDATE REQ6141           *
      * 2002-07-12 SKN  VOIDED LIMIT REVISED PRB2319                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'MO86'.
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
       COPY MORMST.
       COPY MOAPRS.
       COPY MORISKP.
       COPY DCLMOR.
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
           MOVE LOW-VALUES TO MOAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('MOAPRM') MAPSET('MOAPRS')
                FROM(MOAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('MOAPRM') MAPSET('MOAPRS')
                INTO(MOAPRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO MORM-KEY
           EXEC CICS READ FILE('MORMST') INTO(MOR-MASTER-REC)
                RIDFLD(MORM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHANNEL > 0
              AND CHANNEI NOT = 'DR'
              AND CHANNEI NOT = 'DX'
              AND CHANNEI NOT = 'PT'
               MOVE 'CHANNEL CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CHANNEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF FEEAMTL > 0
               IF FUNCTION TEST-NUMVAL(FEEAMTI) NOT = 0
                   MOVE 'FEE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FEEAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FEEAMTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FEE AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
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
           INITIALIZE MORISK-PARMS
           MOVE MORM-ESCHEAT-YTD
             TO MORISK-ESCHEAT-YTD
           MOVE MORM-OUTSTAND-YTD
             TO MORISK-OUTSTAND-YTD
           MOVE MORM-PAID-AMT
             TO MORISK-PAID-AMT
           MOVE MORM-FEE-AMT
             TO MORISK-FEE-AMT
           MOVE MORM-FEE-YTD
             TO MORISK-FEE-YTD
           MOVE MORM-OUTSTAND-AMT
             TO MORISK-OUTSTAND-AMT
           MOVE MORM-NOTICE-DATE
             TO MORISK-NOTICE-DATE
           MOVE MORM-OPEN-DATE
             TO MORISK-OPEN-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MORISK-LAST-ACT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MORISK-EXPIRY-DATE
           MOVE MORM-REASON-CD
             TO MORISK-REASON-CD
           MOVE MORM-CYCLE-CD
             TO MORISK-CYCLE-CD
           MOVE MORM-PAYEE-TYPE-CD
             TO MORISK-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MORISK-CHANNEL-CD
           MOVE MORM-PREFERRED-FLAG
             TO MORISK-PREFERRED-FLAG
           MOVE MORM-STOP-FLAG
             TO MORISK-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MORISK-AUTO-RENEW-FLAG
           MOVE MORM-ITEM-CNT
             TO MORISK-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MORISK-RETRY-CNT
           MOVE MORM-PENALTY-PCT
             TO MORISK-PENALTY-PCT
           MOVE MORM-FEE-PCT
             TO MORISK-FEE-PCT
           MOVE MORM-ADDR
             TO MORISK-ADDR
           MOVE MORM-DESC
             TO MORISK-DESC
           EXEC CICS LINK PROGRAM('MORISK') COMMAREA(MORISK-PARMS)
                LENGTH(LENGTH OF MORISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF MORISK-RETURN-CODE = 08
               MOVE 'RULE MORISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE MORM-KEY TO KEYO
           MOVE MORM-NOTICE-DATE TO NOTICEO
           MOVE MORM-CHANNEL-CD TO CHANNEO
           MOVE MORM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE MORM-RETRY-CNT TO RETRYCO
           MOVE MORM-STOP-FLAG TO STOPFLO
           MOVE MORM-LAST-STMT-DATE TO LASTSTO
           MOVE MORM-LAST-ACT-DATE TO LASTACO
           MOVE MORM-CLOSE-DATE TO CLOSEDO
           MOVE MORM-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEAMTO
           MOVE MORM-TYPE-CD TO TYPECDO
           MOVE MORM-LAST-MAINT-DATE TO LASTMAO
           MOVE MORM-PAID-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAIDYTO
           MOVE MORM-LAST-PAY-DATE TO LASTPAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('MORMST') INTO(MOR-MASTER-REC)
                RIDFLD(MORM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO MORM-CHANNEL-CD
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO MORM-STOP-FLAG
           END-IF
           IF FEEAMTL > 0
               COMPUTE MORM-FEE-AMT = FUNCTION NUMVAL(FEEAMTI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO MORM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('MORMST') FROM(MOR-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('MOAPRM') MAPSET('MOAPRS')
                FROM(MOAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
