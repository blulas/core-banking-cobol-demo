      *================================================================*
      * PROGRAM  : IRMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : IR52                                                *
      * PURPOSE  : IRA CUSTODY HISTORY. ENTER VALIDATES AND DISPLAYS,  *
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : IRPRC, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-12 DPS  ORIGINAL                                       *
      * 2002-03-07 SKN  DISTRIB TABLE EXPANDED PRB2483                 *
      * 2002-10-07 DPS  CONTRIB TABLE EXPANDED REQ1926                 *
      * 2007-03-14 EWS  PERFORMANCE - CONTRIB READ AHEAD REQ2559       *
      * 2008-04-07 BHS  NEW RMD REPORT COLUMNS CHG4366                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'IR52'.
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
       COPY IRMNTS.
       COPY IRPRCP.
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
           MOVE LOW-VALUES TO IRMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('IRMNTM') MAPSET('IRMNTS')
                FROM(IRMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('IRMNTM') MAPSET('IRMNTS')
                INTO(IRMNTMI) RESP(WS-RESP) END-EXEC
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
           IF FMVAMTL > 0
               IF FUNCTION TEST-NUMVAL(FMVAMTI) NOT = 0
                   MOVE 'FMV AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FMVAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FMVAMTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FMV AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
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
           INITIALIZE IRPRC-PARMS
           MOVE IRAM-RMD-YTD
             TO IRPRC-RMD-YTD
           MOVE IRAM-CONTRIB-YTD
             TO IRPRC-CONTRIB-YTD
           MOVE IRAM-EXCESS-YTD
             TO IRPRC-EXCESS-YTD
           MOVE IRAM-DISTRIB-YTD
             TO IRPRC-DISTRIB-YTD
           MOVE IRAM-FMV-AMT
             TO IRPRC-FMV-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRPRC-ROLLOVER-AMT
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRPRC-NEXT-REVIEW-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRPRC-LAST-STMT-DATE
           MOVE IRAM-LAST-ACT-DATE
             TO IRPRC-LAST-ACT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRPRC-EXPIRY-DATE
           MOVE IRAM-IRA-TYPE-CD
             TO IRPRC-IRA-TYPE-CD
           MOVE IRAM-TYPE-CD
             TO IRPRC-TYPE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRPRC-CHANNEL-CD
           MOVE IRAM-REASON-CD
             TO IRPRC-REASON-CD
           MOVE IRAM-OVERRIDE-FLAG
             TO IRPRC-OVERRIDE-FLAG
           MOVE IRAM-SUSPEND-FLAG
             TO IRPRC-SUSPEND-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRPRC-AUTO-RENEW-FLAG
           MOVE IRAM-PAYMENT-CNT
             TO IRPRC-PAYMENT-CNT
           MOVE IRAM-RETRY-CNT
             TO IRPRC-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRPRC-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRPRC-FEE-PCT
           MOVE IRAM-NAME
             TO IRPRC-NAME
           MOVE IRAM-ADDR
             TO IRPRC-ADDR
           EXEC CICS LINK PROGRAM('IRPRC') COMMAREA(IRPRC-PARMS)
                LENGTH(LENGTH OF IRPRC-PARMS) RESP(WS-RESP)
           END-EXEC
           IF IRPRC-RETURN-CODE = 08
               MOVE 'RULE IRPRC DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE IRAM-KEY TO KEYO
           MOVE IRAM-FMV-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FMVYTDO
           MOVE IRAM-NOTICE-DATE TO NOTICEO
           MOVE IRAM-CHANNEL-CD TO CHANNEO
           MOVE IRAM-ITEM-CNT TO ITEMCNO
           MOVE IRAM-WITHHOLD-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO WITHHOO
           MOVE IRAM-OPEN-DATE TO OPENDAO
           MOVE IRAM-DISTRIB-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISTRIO
           MOVE IRAM-PAYMENT-CNT TO PAYMENO
           MOVE IRAM-DISTRIB-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISTRI1O
           MOVE IRAM-EFFECTIVE-DATE TO EFFECTO
           MOVE IRAM-FMV-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FMVAMTO
           MOVE IRAM-EXCESS-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCESSO
           MOVE IRAM-REF-NO TO REFNOO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('IRAMST') INTO(IRA-MASTER-REC)
                RIDFLD(IRAM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FMVAMTL > 0
               COMPUTE IRAM-FMV-AMT = FUNCTION NUMVAL(FMVAMTI)
           END-IF
           EXEC CICS REWRITE FILE('IRAMST') FROM(IRA-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('IRMNTM') MAPSET('IRMNTS')
                FROM(IRMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
