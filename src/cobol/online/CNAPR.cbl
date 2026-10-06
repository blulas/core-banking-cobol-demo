      *================================================================*
      * PROGRAM  : CNAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CN25                                                *
      * PURPOSE  : COIN AND CURRENCY OVERRIDE. ENTER VALIDATES AND     *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CNDTE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-16 DPS  ORIGINAL                                       *
      * 1986-03-07 EWS  PERFORMANCE - CURRENCY READ AHEAD CHG2130      *
      * 1987-08-15 DPS  VARIANCE LIMIT REVISED REQ3451                 *
      * 1988-06-20 DPS  NEW FEE REPORT COLUMNS INC6730                 *
      * 1988-08-20 TWH  COIN ROUNDING - HALF UP REQ2684                *
      * 1992-12-18 SKN  COIN LIMIT REVISED PRB9089                     *
      * 1996-01-23 PQR  AUDIT FINDING - SHIPMENT CONTROL TOTALS CHG9677*
      * 1999-12-01 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-13 SKN  COIN ROUNDING - HALF UP CHG8882                *
      * 2002-05-03 MKL  CURRENCY CUTOFF MOVED REQ0259                  *
      * 2003-05-20 LMB  FEE TABLE EXPANDED INC9915                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CN25'.
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
       COPY CCOMST.
       COPY CNAPRS.
       COPY CNDTEP.
       COPY DCLCCO.
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
           MOVE LOW-VALUES TO CNAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CNAPRM') MAPSET('CNAPRS')
                FROM(CNAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CNAPRM') MAPSET('CNAPRS')
                INTO(CNAPRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO CCOM-KEY
           EXEC CICS READ FILE('CCOMST') INTO(CCO-MASTER-REC)
                RIDFLD(CCOM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE CNDTE-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNDTE-SHIPMENT-AMT
           MOVE CCOM-FEE-AMT
             TO CNDTE-FEE-AMT
           MOVE CCOM-COIN-YTD
             TO CNDTE-COIN-YTD
           MOVE CCOM-FEE-YTD
             TO CNDTE-FEE-YTD
           MOVE CCOM-COIN-AMT
             TO CNDTE-COIN-AMT
           MOVE CCOM-CURRENCY-YTD
             TO CNDTE-CURRENCY-YTD
           MOVE CCOM-EFFECTIVE-DATE
             TO CNDTE-EFFECTIVE-DATE
           MOVE CCOM-LAST-ACT-DATE
             TO CNDTE-LAST-ACT-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNDTE-NEXT-REVIEW-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNDTE-MATURITY-DATE
           MOVE CCOM-CHANNEL-CD
             TO CNDTE-CHANNEL-CD
           MOVE CCOM-DENOM-CD
             TO CNDTE-DENOM-CD
           MOVE CCOM-STATUS-CD
             TO CNDTE-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNDTE-CARRIER-CD
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNDTE-NOTICE-SENT-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNDTE-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNDTE-AUTO-RENEW-FLAG
           MOVE CCOM-ITEM-CNT
             TO CNDTE-ITEM-CNT
           MOVE CCOM-NOTICE-CNT
             TO CNDTE-NOTICE-CNT
           MOVE CCOM-PENALTY-PCT
             TO CNDTE-PENALTY-PCT
           MOVE CCOM-FEE-PCT
             TO CNDTE-FEE-PCT
           MOVE CCOM-CUST-ID
             TO CNDTE-CUST-ID
           MOVE CCOM-DESC
             TO CNDTE-DESC
           EXEC CICS LINK PROGRAM('CNDTE') COMMAREA(CNDTE-PARMS)
                LENGTH(LENGTH OF CNDTE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CNDTE-RETURN-CODE = 08
               MOVE 'RULE CNDTE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE CCOM-KEY TO KEYO
           MOVE CCOM-NAME TO NAMEO
           MOVE CCOM-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEAMTO
           MOVE CCOM-STRAP-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO STRAPAO
           MOVE CCOM-CHANNEL-CD TO CHANNEO
           MOVE CCOM-ITEM-CNT TO ITEMCNO
           MOVE CCOM-RETRY-CNT TO RETRYCO
           MOVE CCOM-NOTICE-DATE TO NOTICEO
           MOVE CCOM-VARIANCE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO VARIANO
           MOVE CCOM-COIN-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COINAMO
           MOVE CCOM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE CCOM-CUST-ID TO CUSTIDO
           MOVE CCOM-CURRENCY-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CURRENO
           MOVE CCOM-HOLD-FLAG TO HOLDFLO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('CCOMST') INTO(CCO-MASTER-REC)
                RIDFLD(CCOM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           EXEC CICS REWRITE FILE('CCOMST') FROM(CCO-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('CNAPRM') MAPSET('CNAPRS')
                FROM(CNAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
