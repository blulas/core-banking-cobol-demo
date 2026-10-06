      *================================================================*
      * PROGRAM  : TXADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TX20                                                *
      * PURPOSE  : TELEX WIRE MESSAGING OVERRIDE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TXCAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-11-09 DPS  ORIGINAL                                       *
      * 1997-04-15 MKL  WIRE FEE SCHEDULE UPDATE CHG2883               *
      * 1997-11-11 DPS  COVER LIMIT REVISED INC4820                    *
      * 1999-09-22 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-04 MKL  COVER CALCULATION CORRECTED CHG2165            *
      * 2002-09-22 PQR  SETTLE CALCULATION CORRECTED INC2375           *
      * 2008-06-10 PQR  SETTLE STATUS CODES ADDED INC4903              *
      * 2008-01-25 MKL  QUEUED NOTICE TEXT PER LEGAL INC7915           *
      * 2009-01-13 EWS  CORRESP FEE SCHEDULE UPDATE CHG8434            *
      * 2009-11-23 DPS  AUDIT FINDING - CORRESP CONTROL TOTALS PRB0645 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TX20'.
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
       COPY TXADDS.
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
           MOVE LOW-VALUES TO TXADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TXADDM') MAPSET('TXADDS')
                FROM(TXADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TXADDM') MAPSET('TXADDS')
                INTO(TXADDMI) RESP(WS-RESP) END-EXEC
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
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF WIREAML > 0
               IF FUNCTION TEST-NUMVAL(WIREAMI) NOT = 0
                   MOVE 'WIRE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO WIREAML
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(WIREAMI)
                   IF WS-NUM-WORK < 0
                       MOVE 'WIRE AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CORRESL > 0
               IF FUNCTION TEST-NUMVAL(CORRESI) NOT = 0
                   MOVE 'CORRESP YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CORRESL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CORRESI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CORRESP YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF TYPECDL > 0
              AND TYPECDI NOT = 'K'
              AND TYPECDI NOT = 'L'
              AND TYPECDI NOT = 'M'
               MOVE 'TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO TYPECDL
               MOVE 'Y' TO WS-ERROR-FLAG
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
           MOVE TLXM-REF-NO TO REFNOO
           MOVE TLXM-COVER-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO COVERYO
           MOVE TLXM-OVERRIDE-FLAG TO OVERRIO
           MOVE TLXM-LAST-ACT-DATE TO LASTACO
           MOVE TLXM-WIRE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO WIREAMO
           MOVE TLXM-CORRESP-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CORRESO
           MOVE TLXM-TYPE-CD TO TYPECDO
           MOVE TLXM-COVER-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COVERAO
           MOVE TLXM-CLOSE-DATE TO CLOSEDO
           MOVE TLXM-BRANCH-CD TO BRANCHO
           MOVE TLXM-STOP-FLAG TO STOPFLO
           MOVE TLXM-NEXT-DUE-DATE TO NEXTDUO
           MOVE TLXM-OPEN-DATE TO OPENDAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TLXMST') INTO(TLX-MASTER-REC)
                RIDFLD(TLXM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO TLXM-OVERRIDE-FLAG
           END-IF
           IF WIREAML > 0
               COMPUTE TLXM-WIRE-AMT = FUNCTION NUMVAL(WIREAMI)
           END-IF
           IF CORRESL > 0
               COMPUTE TLXM-CORRESP-YTD = FUNCTION NUMVAL(CORRESI)
           END-IF
           IF TYPECDL > 0
               MOVE TYPECDI TO TLXM-TYPE-CD
           END-IF
           IF COVERAL > 0
               COMPUTE TLXM-COVER-AMT = FUNCTION NUMVAL(COVERAI)
           END-IF
           EXEC CICS REWRITE FILE('TLXMST') FROM(TLX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TXADDM') MAPSET('TXADDS')
                FROM(TXADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
