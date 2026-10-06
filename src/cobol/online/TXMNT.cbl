      *================================================================*
      * PROGRAM  : TXMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TX53                                                *
      * PURPOSE  : TELEX WIRE MESSAGING REVIEW. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TXELG, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-20 BHS  ORIGINAL                                       *
      * 1993-07-24 RJT  CORRESP TABLE EXPANDED REQ9501                 *
      * 1994-06-04 GAF  CHARGE FEE SCHEDULE UPDATE REQ7062             *
      * 1994-05-24 DPS  SETTLE ROUNDING - HALF UP INC1498              *
      * 1998-04-04 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-16 CRP  AUDIT FINDING - CHARGE CONTROL TOTALS CHG0318  *
      * 2003-02-25 EWS  CORRESP FEE SCHEDULE UPDATE CHG6173            *
      * 2003-09-13 TWH  SETTLE STATUS CODES ADDED CHG8731              *
      * 2005-05-04 MKL  SETTLE CUTOFF MOVED CHG9842                    *
      * 2007-02-07 GAF  QUEUED CALCULATION CORRECTED PRB4251           *
      * 2008-11-08 GAF  COVER FEE SCHEDULE UPDATE INC5528              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TX53'.
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
       COPY TLXMST.
       COPY TXMNTS.
       COPY TXELGP.
       COPY DCLTLX.
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
           MOVE LOW-VALUES TO TXMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TXMNTM') MAPSET('TXMNTS')
                FROM(TXMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TXMNTM') MAPSET('TXMNTS')
                INTO(TXMNTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO TLXM-KEY
           EXEC CICS READ FILE('TLXMST') INTO(TLX-MASTER-REC)
                RIDFLD(TLXM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHARGEL > 0
               IF FUNCTION TEST-NUMVAL(CHARGEI) NOT = 0
                   MOVE 'CHARGE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CHARGEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CHARGEI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CHARGE AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF NOTICE1L > 0 AND NOTICE1I NOT = 'Y' AND NOTICE1I NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE TXELG-PARMS
           MOVE TLXM-WIRE-YTD
             TO TXELG-WIRE-YTD
           MOVE TLXM-CHARGE-AMT
             TO TXELG-CHARGE-AMT
           MOVE TLXM-QUEUED-AMT
             TO TXELG-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXELG-CORRESP-YTD
           MOVE TLXM-CORRESP-AMT
             TO TXELG-CORRESP-AMT
           MOVE TLXM-WIRE-AMT
             TO TXELG-WIRE-AMT
           MOVE TLXM-LAST-PAY-DATE
             TO TXELG-LAST-PAY-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXELG-NEXT-DUE-DATE
           MOVE TLXM-EFFECTIVE-DATE
             TO TXELG-EFFECTIVE-DATE
           MOVE TLXM-CLOSE-DATE
             TO TXELG-CLOSE-DATE
           MOVE TLXM-MSG-TYPE-CD
             TO TXELG-MSG-TYPE-CD
           MOVE TLXM-REGION-CD
             TO TXELG-REGION-CD
           MOVE TLXM-TYPE-CD
             TO TXELG-TYPE-CD
           MOVE TLXM-REASON-CD
             TO TXELG-REASON-CD
           MOVE TLXM-SUSPEND-FLAG
             TO TXELG-SUSPEND-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXELG-STOP-FLAG
           MOVE TLXM-OVERRIDE-FLAG
             TO TXELG-OVERRIDE-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXELG-PAYMENT-CNT
           MOVE TLXM-NOTICE-CNT
             TO TXELG-NOTICE-CNT
           MOVE TLXM-FEE-PCT
             TO TXELG-FEE-PCT
           MOVE TLXM-RATE
             TO TXELG-RATE
           MOVE TLXM-ADDR
             TO TXELG-ADDR
           MOVE TLXM-DESC
             TO TXELG-DESC
           EXEC CICS LINK PROGRAM('TXELG') COMMAREA(TXELG-PARMS)
                LENGTH(LENGTH OF TXELG-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TXELG-RETURN-CODE = 08
               MOVE 'RULE TXELG DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TLXM-KEY TO KEYO
           MOVE TLXM-LAST-PAY-DATE TO LASTPAO
           MOVE TLXM-QUEUED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO QUEUEDO
           MOVE TLXM-CUST-ID TO CUSTIDO
           MOVE TLXM-NOTICE-CNT TO NOTICEO
           MOVE TLXM-SUSPEND-FLAG TO SUSPENO
           MOVE TLXM-EXCEPT-CNT TO EXCEPTO
           MOVE TLXM-WIRE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO WIREYTO
           MOVE TLXM-CHARGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CHARGEO
           MOVE TLXM-ADDR TO ADDRO
           MOVE TLXM-NOTICE-SENT-FLAG TO NOTICE1O
           MOVE TLXM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE TLXM-PAYMENT-CNT TO PAYMENO
           MOVE TLXM-NEXT-DUE-DATE TO NEXTDUO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TLXMST') INTO(TLX-MASTER-REC)
                RIDFLD(TLXM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHARGEL > 0
               COMPUTE TLXM-CHARGE-AMT = FUNCTION NUMVAL(CHARGEI)
           END-IF
           IF NOTICE1L > 0
               MOVE NOTICE1I TO TLXM-NOTICE-SENT-FLAG
           END-IF
           EXEC CICS REWRITE FILE('TLXMST') FROM(TLX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TXMNTM') MAPSET('TXMNTS')
                FROM(TXMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
