      *================================================================*
      * PROGRAM  : ATINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : AT93                                                *
      * PURPOSE  : ATM NETWORK SETTLEMENT INQUIRY. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : ATWAIV, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-01 RJT  ORIGINAL                                       *
      * 1997-05-06 PQR  SETTLE NOTICE TEXT PER LEGAL INC1229           *
      * 2003-03-12 CRP  ADJUST TABLE EXPANDED PRB2729                  *
      * 2005-06-06 MKL  PERFORMANCE - INTERCHG READ AHEAD CHG1329      *
      * 2007-11-20 BHS  SETTLE NOTICE TEXT PER LEGAL INC2923           *
      * 2007-02-08 LMB  ADJUST CALCULATION CORRECTED PRB6652           *
      * 2010-12-20 BHS  SETTLE ROUNDING - HALF UP PRB3782              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'AT93'.
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
       COPY ATNMST.
       COPY ATINQS.
       COPY ATWAIVP.
       COPY DCLATN.
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
           MOVE LOW-VALUES TO ATINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('ATINQM') MAPSET('ATINQS')
                FROM(ATINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('ATINQM') MAPSET('ATINQS')
                INTO(ATINQMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO ATNM-KEY
           EXEC CICS READ FILE('ATNMST') INTO(ATN-MASTER-REC)
                RIDFLD(ATNM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REASONL > 0
              AND REASONI NOT = 'AK'
              AND REASONI NOT = 'HX'
              AND REASONI NOT = 'PR'
               MOVE 'REASON CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REASONL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SETTLEL > 0
               IF FUNCTION TEST-NUMVAL(SETTLEI) NOT = 0
                   MOVE 'SETTLE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO SETTLEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(SETTLEI)
                   IF WS-NUM-WORK < 0
                       MOVE 'SETTLE AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE ATWAIV-PARMS
           MOVE ATNM-INTERCHG-YTD
             TO ATWAIV-INTERCHG-YTD
           MOVE ATNM-SETTLE-AMT
             TO ATWAIV-SETTLE-AMT
           MOVE ATNM-ADJUST-YTD
             TO ATWAIV-ADJUST-YTD
           MOVE ATNM-DISPENSED-YTD
             TO ATWAIV-DISPENSED-YTD
           MOVE ATNM-ADJUST-AMT
             TO ATWAIV-ADJUST-AMT
           MOVE ATNM-SETTLE-YTD
             TO ATWAIV-SETTLE-YTD
           MOVE ATNM-NEXT-REVIEW-DATE
             TO ATWAIV-NEXT-REVIEW-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATWAIV-MATURITY-DATE
           MOVE ATNM-LAST-PAY-DATE
             TO ATWAIV-LAST-PAY-DATE
           MOVE ATNM-EFFECTIVE-DATE
             TO ATWAIV-EFFECTIVE-DATE
           MOVE ATNM-BRANCH-CD
             TO ATWAIV-BRANCH-CD
           MOVE ATNM-TERMINAL-CD
             TO ATWAIV-TERMINAL-CD
           MOVE ATNM-NETWORK-CD
             TO ATWAIV-NETWORK-CD
           MOVE ATNM-STATUS-CD
             TO ATWAIV-STATUS-CD
           MOVE ATNM-SUSPEND-FLAG
             TO ATWAIV-SUSPEND-FLAG
           MOVE ATNM-NOTICE-SENT-FLAG
             TO ATWAIV-NOTICE-SENT-FLAG
           MOVE ATNM-REVIEW-FLAG
             TO ATWAIV-REVIEW-FLAG
           MOVE ATNM-PAYMENT-CNT
             TO ATWAIV-PAYMENT-CNT
           MOVE ATNM-ITEM-CNT
             TO ATWAIV-ITEM-CNT
           MOVE ATNM-FEE-PCT
             TO ATWAIV-FEE-PCT
           MOVE ATNM-RATE
             TO ATWAIV-RATE
           MOVE ATNM-DESC
             TO ATWAIV-DESC
           MOVE ATNM-NAME
             TO ATWAIV-NAME
           EXEC CICS LINK PROGRAM('ATWAIV') COMMAREA(ATWAIV-PARMS)
                LENGTH(LENGTH OF ATWAIV-PARMS) RESP(WS-RESP)
           END-EXEC
           IF ATWAIV-RETURN-CODE = 08
               MOVE 'RULE ATWAIV DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ATNM-KEY TO KEYO
           MOVE ATNM-CUST-ID TO CUSTIDO
           MOVE ATNM-MATURITY-DATE TO MATURIO
           MOVE ATNM-REASON-CD TO REASONO
           MOVE ATNM-PAYMENT-CNT TO PAYMENO
           MOVE ATNM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE ATNM-DISPENSED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISPENO
           MOVE ATNM-CASSETTE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASSETO
           MOVE ATNM-SETTLE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO SETTLEO
           MOVE ATNM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE ATNM-CLOSE-DATE TO CLOSEDO
           MOVE ATNM-INTERCHG-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTERCO
           MOVE ATNM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE ATNM-DISPENSED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DISPEN1O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ATNMST') INTO(ATN-MASTER-REC)
                RIDFLD(ATNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REASONL > 0
               MOVE REASONI TO ATNM-REASON-CD
           END-IF
           IF SETTLEL > 0
               COMPUTE ATNM-SETTLE-AMT = FUNCTION NUMVAL(SETTLEI)
           END-IF
           EXEC CICS REWRITE FILE('ATNMST') FROM(ATN-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('ATINQM') MAPSET('ATINQS')
                FROM(ATINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
