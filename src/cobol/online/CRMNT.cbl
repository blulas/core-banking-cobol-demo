      *================================================================*
      * PROGRAM  : CRMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CR33                                                *
      * PURPOSE  : PROPRIETARY CREDIT CARD MAINTENANCE. ENTER VALIDATES*
      *            AND DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO*
      *            THE MENU.                                           *
      * CALLS    : CRDTE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-22 RJT  ORIGINAL                                       *
      * 2002-04-25 BHS  AUDIT FINDING - PURCHASE CONTROL TOTALS INC2643*
      * 2006-10-10 SKN  CASH-ADV FEE SCHEDULE UPDATE INC4189           *
      * 2006-03-02 CRP  FINANCE RERUN PROTECTION PRB4401               *
      * 2007-12-11 CRP  LATE-FEE TABLE EXPANDED INC8095                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CR33'.
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
       COPY CRCMST.
       COPY CRMNTS.
       COPY CRDTEP.
       COPY DCLCRC.
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
           MOVE LOW-VALUES TO CRMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CRMNTM') MAPSET('CRMNTS')
                FROM(CRMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CRMNTM') MAPSET('CRMNTS')
                INTO(CRMNTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO CRCM-KEY
           EXEC CICS READ FILE('CRCMST') INTO(CRC-MASTER-REC)
                RIDFLD(CRCM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF PAYMENL > 0
               IF FUNCTION TEST-NUMVAL(PAYMENI) NOT = 0
                   MOVE 'PAYMENT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO PAYMENL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(PAYMENI)
                   IF WS-NUM-WORK < 0
                       MOVE 'PAYMENT AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF LIMITAL > 0
               IF FUNCTION TEST-NUMVAL(LIMITAI) NOT = 0
                   MOVE 'LIMIT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO LIMITAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(LIMITAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'LIMIT AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF MINDUEL > 0
               IF FUNCTION TEST-NUMVAL(MINDUEI) NOT = 0
                   MOVE 'MIN DUE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO MINDUEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(MINDUEI)
                   IF WS-NUM-WORK < 0
                       MOVE 'MIN DUE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF EXEMPTL > 0 AND EXEMPTI NOT = 'Y' AND EXEMPTI NOT = 'N'
               MOVE 'EXEMPT FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE CRDTE-PARMS
           MOVE CRCM-PURCHASE-YTD
             TO CRDTE-PURCHASE-YTD
           MOVE CRCM-LIMIT-AMT
             TO CRDTE-LIMIT-AMT
           MOVE CRCM-FINANCE-AMT
             TO CRDTE-FINANCE-AMT
           MOVE CRCM-LIMIT-YTD
             TO CRDTE-LIMIT-YTD
           MOVE CRCM-PURCHASE-AMT
             TO CRDTE-PURCHASE-AMT
           MOVE CRCM-LATE-FEE-YTD
             TO CRDTE-LATE-FEE-YTD
           MOVE CRCM-EXPIRY-DATE
             TO CRDTE-EXPIRY-DATE
           MOVE CRCM-OPEN-DATE
             TO CRDTE-OPEN-DATE
           MOVE CRCM-LAST-PAY-DATE
             TO CRDTE-LAST-PAY-DATE
           MOVE CRCM-CLOSE-DATE
             TO CRDTE-CLOSE-DATE
           MOVE CRCM-CARD-TYPE-CD
             TO CRDTE-CARD-TYPE-CD
           MOVE CRCM-SOURCE-CD
             TO CRDTE-SOURCE-CD
           MOVE CRCM-TYPE-CD
             TO CRDTE-TYPE-CD
           MOVE CRCM-BRANCH-CD
             TO CRDTE-BRANCH-CD
           MOVE CRCM-PREFERRED-FLAG
             TO CRDTE-PREFERRED-FLAG
           MOVE CRCM-STOP-FLAG
             TO CRDTE-STOP-FLAG
           MOVE CRCM-EXEMPT-FLAG
             TO CRDTE-EXEMPT-FLAG
           MOVE CRCM-NOTICE-CNT
             TO CRDTE-NOTICE-CNT
           MOVE CRCM-CYCLE-CNT
             TO CRDTE-CYCLE-CNT
           MOVE CRCM-DISCOUNT
             TO CRDTE-DISCOUNT
           MOVE CRCM-PENALTY-PCT
             TO CRDTE-PENALTY-PCT
           MOVE CRCM-NAME
             TO CRDTE-NAME
           MOVE CRCM-CUST-ID
             TO CRDTE-CUST-ID
           EXEC CICS LINK PROGRAM('CRDTE') COMMAREA(CRDTE-PARMS)
                LENGTH(LENGTH OF CRDTE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CRDTE-RETURN-CODE = 08
               MOVE 'RULE CRDTE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE CRCM-KEY TO KEYO
           MOVE CRCM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE CRCM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE CRCM-LIMIT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO LIMITYO
           MOVE CRCM-ADDR TO ADDRO
           MOVE CRCM-LAST-ACT-DATE TO LASTACO
           MOVE CRCM-PAYMENT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMENO
           MOVE CRCM-LIMIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO LIMITAO
           MOVE CRCM-LAST-PAY-DATE TO LASTPAO
           MOVE CRCM-CYCLE-CNT TO CYCLECO
           MOVE CRCM-MIN-DUE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO MINDUEO
           MOVE CRCM-EXEMPT-FLAG TO EXEMPTO
           MOVE CRCM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE CRCM-LATE-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO LATEFEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('CRCMST') INTO(CRC-MASTER-REC)
                RIDFLD(CRCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO CRCM-NOTICE-SENT-FLAG
           END-IF
           IF PAYMENL > 0
               COMPUTE CRCM-PAYMENT-AMT = FUNCTION NUMVAL(PAYMENI)
           END-IF
           IF LIMITAL > 0
               COMPUTE CRCM-LIMIT-AMT = FUNCTION NUMVAL(LIMITAI)
           END-IF
           IF MINDUEL > 0
               COMPUTE CRCM-MIN-DUE-YTD = FUNCTION NUMVAL(MINDUEI)
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO CRCM-EXEMPT-FLAG
           END-IF
           EXEC CICS REWRITE FILE('CRCMST') FROM(CRC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('CRMNTM') MAPSET('CRMNTS')
                FROM(CRMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
