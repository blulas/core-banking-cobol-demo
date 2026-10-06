      *================================================================*
      * PROGRAM  : TXAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TX12                                                *
      * PURPOSE  : TELEX WIRE MESSAGING INQUIRY. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TXCAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-09 PQR  ORIGINAL                                       *
      * 1997-11-09 LMB  SETTLE TABLE EXPANDED INC0261                  *
      * 1998-03-13 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-10-06 GAF  PERFORMANCE - COVER READ AHEAD CHG3807         *
      * 2000-03-05 CRP  WIRE RERUN PROTECTION CHG1713                  *
      * 2004-12-13 GAF  QUEUED STATUS CODES ADDED REQ6111              *
      * 2004-11-16 MKL  CORRESP CUTOFF MOVED INC6158                   *
      * 2004-03-13 MKL  SETTLE ROUNDING - HALF UP REQ9948              *
      * 2006-08-02 CRP  NEW COVER REPORT COLUMNS CHG3573               *
      * 2007-06-11 DPS  SETTLE FEE SCHEDULE UPDATE REQ2026             *
      * 2007-08-03 BHS  CHARGE CUTOFF MOVED CHG9697                    *
      * 2007-06-25 JLM  CORRESP CUTOFF MOVED CHG5133                   *
      * 2010-12-06 RJT  SETTLE STATUS CODES ADDED PRB5392              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TX12'.
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
       COPY TXAPRS.
       COPY TXCALP.
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
           MOVE LOW-VALUES TO TXAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TXAPRM') MAPSET('TXAPRS')
                FROM(TXAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TXAPRM') MAPSET('TXAPRS')
                INTO(TXAPRMI) RESP(WS-RESP) END-EXEC
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
           IF COVERAL > 0
               IF FUNCTION TEST-NUMVAL(COVERAI) NOT = 0
                   MOVE 'COVER AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO COVERAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(COVERAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'COVER AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF TESTKEL > 0
              AND TESTKEI NOT = 'EA'
              AND TESTKEI NOT = 'GA'
              AND TESTKEI NOT = 'HL'
               MOVE 'TEST KEY CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO TESTKEL
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
           INITIALIZE TXCAL-PARMS
           MOVE TLXM-CHARGE-AMT
             TO TXCAL-CHARGE-AMT
           MOVE TLXM-SETTLE-AMT
             TO TXCAL-SETTLE-AMT
           MOVE TLXM-QUEUED-YTD
             TO TXCAL-QUEUED-YTD
           MOVE TLXM-CHARGE-YTD
             TO TXCAL-CHARGE-YTD
           MOVE TLXM-COVER-YTD
             TO TXCAL-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXCAL-QUEUED-AMT
           MOVE TLXM-LAST-ACT-DATE
             TO TXCAL-LAST-ACT-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXCAL-NEXT-DUE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXCAL-EXPIRY-DATE
           MOVE TLXM-NEXT-REVIEW-DATE
             TO TXCAL-NEXT-REVIEW-DATE
           MOVE TLXM-REASON-CD
             TO TXCAL-REASON-CD
           MOVE TLXM-BRANCH-CD
             TO TXCAL-BRANCH-CD
           MOVE TLXM-TYPE-CD
             TO TXCAL-TYPE-CD
           MOVE TLXM-MSG-TYPE-CD
             TO TXCAL-MSG-TYPE-CD
           MOVE TLXM-PREFERRED-FLAG
             TO TXCAL-PREFERRED-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXCAL-REVIEW-FLAG
           MOVE TLXM-SUSPEND-FLAG
             TO TXCAL-SUSPEND-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXCAL-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXCAL-EXCEPT-CNT
           MOVE TLXM-FEE-PCT
             TO TXCAL-FEE-PCT
           MOVE TLXM-RATE
             TO TXCAL-RATE
           MOVE TLXM-DESC
             TO TXCAL-DESC
           MOVE TLXM-NAME
             TO TXCAL-NAME
           EXEC CICS LINK PROGRAM('TXCAL') COMMAREA(TXCAL-PARMS)
                LENGTH(LENGTH OF TXCAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TXCAL-RETURN-CODE = 08
               MOVE 'RULE TXCAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TLXM-KEY TO KEYO
           MOVE TLXM-WIRE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO WIREAMO
           MOVE TLXM-BRANCH-CD TO BRANCHO
           MOVE TLXM-SETTLE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO SETTLEO
           MOVE TLXM-COVER-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COVERAO
           MOVE TLXM-CONTACT TO CONTACO
           MOVE TLXM-WIRE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO WIREYTO
           MOVE TLXM-CHARGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CHARGEO
           MOVE TLXM-TYPE-CD TO TYPECDO
           MOVE TLXM-EXCEPT-CNT TO EXCEPTO
           MOVE TLXM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE TLXM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE TLXM-TEST-KEY-CD TO TESTKEO
           MOVE TLXM-PREFERRED-FLAG TO PREFERO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TLXMST') INTO(TLX-MASTER-REC)
                RIDFLD(TLXM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF SETTLEL > 0
               COMPUTE TLXM-SETTLE-AMT = FUNCTION NUMVAL(SETTLEI)
           END-IF
           IF COVERAL > 0
               COMPUTE TLXM-COVER-AMT = FUNCTION NUMVAL(COVERAI)
           END-IF
           IF TESTKEL > 0
               MOVE TESTKEI TO TLXM-TEST-KEY-CD
           END-IF
           EXEC CICS REWRITE FILE('TLXMST') FROM(TLX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TXAPRM') MAPSET('TXAPRS')
                FROM(TXAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
