      *================================================================*
      * PROGRAM  : CRINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CR68                                                *
      * PURPOSE  : PROPRIETARY CREDIT CARD REVIEW. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CRVAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-09 EWS  ORIGINAL                                       *
      * 1997-04-22 RJT  FINANCE TABLE EXPANDED PRB7714                 *
      * 2002-10-04 SKN  NEW LATE-FEE REPORT COLUMNS INC9018            *
      * 2004-11-21 BHS  PERFORMANCE - LATE-FEE READ AHEAD PRB9573      *
      * 2006-07-02 SKN  AUDIT FINDING - LIMIT CONTROL TOTALS INC2280   *
      * 2006-12-25 RJT  PAYMENT EXCEPTIONS TO OPS QUEUE REQ7837        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CR68'.
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
       COPY CRINQS.
       COPY CRVALP.
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
           MOVE LOW-VALUES TO CRINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CRINQM') MAPSET('CRINQS')
                FROM(CRINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CRINQM') MAPSET('CRINQS')
                INTO(CRINQMI) RESP(WS-RESP) END-EXEC
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
           IF PREFERL > 0 AND PREFERI NOT = 'Y' AND PREFERI NOT = 'N'
               MOVE 'PREFERRED FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF FINANC1L > 0
               IF FUNCTION TEST-NUMVAL(FINANC1I) NOT = 0
                   MOVE 'FINANCE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FINANC1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FINANC1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'FINANCE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF BILLCYL > 0
              AND BILLCYI NOT = 'B'
              AND BILLCYI NOT = 'D'
              AND BILLCYI NOT = 'E'
               MOVE 'BILL CYCLE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO BILLCYL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF AUTOREL > 0 AND AUTOREI NOT = 'Y' AND AUTOREI NOT = 'N'
               MOVE 'AUTO RENEW FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE CRVAL-PARMS
           MOVE CRCM-FINANCE-YTD
             TO CRVAL-FINANCE-YTD
           MOVE CRCM-LIMIT-AMT
             TO CRVAL-LIMIT-AMT
           MOVE CRCM-LATE-FEE-YTD
             TO CRVAL-LATE-FEE-YTD
           MOVE CRCM-MIN-DUE-AMT
             TO CRVAL-MIN-DUE-AMT
           MOVE CRCM-MIN-DUE-YTD
             TO CRVAL-MIN-DUE-YTD
           MOVE CRCM-PAYMENT-AMT
             TO CRVAL-PAYMENT-AMT
           MOVE CRCM-OPEN-DATE
             TO CRVAL-OPEN-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRVAL-MATURITY-DATE
           MOVE CRCM-EXPIRY-DATE
             TO CRVAL-EXPIRY-DATE
           MOVE CRCM-EFFECTIVE-DATE
             TO CRVAL-EFFECTIVE-DATE
           MOVE CRCM-TYPE-CD
             TO CRVAL-TYPE-CD
           MOVE CRCM-STATUS-CD
             TO CRVAL-STATUS-CD
           MOVE CRCM-BRANCH-CD
             TO CRVAL-BRANCH-CD
           MOVE CRCM-BILL-CYCLE-CD
             TO CRVAL-BILL-CYCLE-CD
           MOVE CRCM-STOP-FLAG
             TO CRVAL-STOP-FLAG
           MOVE CRCM-PREFERRED-FLAG
             TO CRVAL-PREFERRED-FLAG
           MOVE CRCM-OVERRIDE-FLAG
             TO CRVAL-OVERRIDE-FLAG
           MOVE CRCM-CYCLE-CNT
             TO CRVAL-CYCLE-CNT
           MOVE CRCM-NOTICE-CNT
             TO CRVAL-NOTICE-CNT
           MOVE CRCM-DISCOUNT
             TO CRVAL-DISCOUNT
           MOVE CRCM-PENALTY-PCT
             TO CRVAL-PENALTY-PCT
           MOVE CRCM-CONTACT
             TO CRVAL-CONTACT
           MOVE CRCM-DESC
             TO CRVAL-DESC
           EXEC CICS LINK PROGRAM('CRVAL') COMMAREA(CRVAL-PARMS)
                LENGTH(LENGTH OF CRVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CRVAL-RETURN-CODE = 08
               MOVE 'RULE CRVAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE CRCM-KEY TO KEYO
           MOVE CRCM-LAST-ACT-DATE TO LASTACO
           MOVE CRCM-EFFECTIVE-DATE TO EFFECTO
           MOVE CRCM-FINANCE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FINANCO
           MOVE CRCM-PREFERRED-FLAG TO PREFERO
           MOVE CRCM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE CRCM-CYCLE-CNT TO CYCLECO
           MOVE CRCM-FINANCE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FINANC1O
           MOVE CRCM-PURCHASE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PURCHAO
           MOVE CRCM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE CRCM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE CRCM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE CRCM-BILL-CYCLE-CD TO BILLCYO
           MOVE CRCM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('CRCMST') INTO(CRC-MASTER-REC)
                RIDFLD(CRCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF PREFERL > 0
               MOVE PREFERI TO CRCM-PREFERRED-FLAG
           END-IF
           IF FINANC1L > 0
               COMPUTE CRCM-FINANCE-YTD = FUNCTION NUMVAL(FINANC1I)
           END-IF
           IF BILLCYL > 0
               MOVE BILLCYI TO CRCM-BILL-CYCLE-CD
           END-IF
           IF AUTOREL > 0
               MOVE AUTOREI TO CRCM-AUTO-RENEW-FLAG
           END-IF
           EXEC CICS REWRITE FILE('CRCMST') FROM(CRC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('CRINQM') MAPSET('CRINQS')
                FROM(CRINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
