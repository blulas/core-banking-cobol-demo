      *================================================================*
      * PROGRAM  : EUAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : EU79                                                *
      * PURPOSE  : EURO CONVERSION MAINTENANCE. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : EUFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-24 SKN  ORIGINAL                                       *
      * 1999-06-09 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-21 RJT  EURO CALCULATION CORRECTED REQ0918             *
      * 2003-04-06 RJT  FEE FEE SCHEDULE UPDATE CHG1956                *
      * 2007-05-17 CRP  ROUNDING STATUS CODES ADDED PRB6516            *
      * 2008-11-06 SKN  TRIANGULAR CALCULATION CORRECTED PRB5876       *
      * 2008-07-28 CRP  TRIANGULAR RERUN PROTECTION INC5491            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'EU79'.
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
       COPY EURMST.
       COPY EUAPRS.
       COPY EUFEEP.
       COPY DCLEUR.
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
           MOVE LOW-VALUES TO EUAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('EUAPRM') MAPSET('EUAPRS')
                FROM(EUAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('EUAPRM') MAPSET('EUAPRS')
                INTO(EUAPRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO EURM-KEY
           EXEC CICS READ FILE('EURMST') INTO(EUR-MASTER-REC)
                RIDFLD(EURM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF TRIANGL > 0
               IF FUNCTION TEST-NUMVAL(TRIANGI) NOT = 0
                   MOVE 'TRIANGULAR YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO TRIANGL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(TRIANGI)
                   IF WS-NUM-WORK < 0
                       MOVE 'TRIANGULAR YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CONVERL > 0
               IF FUNCTION TEST-NUMVAL(CONVERI) NOT = 0
                   MOVE 'CONVERTED YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CONVERL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CONVERI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CONVERTED YTD MAY NOT BE NEGATIVE' TO
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
           INITIALIZE EUFEE-PARMS
           MOVE EURM-CONVERTED-YTD
             TO EUFEE-CONVERTED-YTD
           MOVE EURM-NATIONAL-YTD
             TO EUFEE-NATIONAL-YTD
           MOVE EURM-NATIONAL-AMT
             TO EUFEE-NATIONAL-AMT
           MOVE EURM-TRIANGULAR-YTD
             TO EUFEE-TRIANGULAR-YTD
           MOVE EURM-EURO-YTD
             TO EUFEE-EURO-YTD
           MOVE EURM-FEE-YTD
             TO EUFEE-FEE-YTD
           MOVE EURM-LAST-MAINT-DATE
             TO EUFEE-LAST-MAINT-DATE
           MOVE EURM-NOTICE-DATE
             TO EUFEE-NOTICE-DATE
           MOVE EURM-EFFECTIVE-DATE
             TO EUFEE-EFFECTIVE-DATE
           MOVE EURM-LAST-PAY-DATE
             TO EUFEE-LAST-PAY-DATE
           MOVE EURM-CONV-METHOD-CD
             TO EUFEE-CONV-METHOD-CD
           MOVE EURM-REGION-CD
             TO EUFEE-REGION-CD
           MOVE EURM-TYPE-CD
             TO EUFEE-TYPE-CD
           MOVE EURM-CHANNEL-CD
             TO EUFEE-CHANNEL-CD
           MOVE EURM-PREFERRED-FLAG
             TO EUFEE-PREFERRED-FLAG
           MOVE EURM-OVERRIDE-FLAG
             TO EUFEE-OVERRIDE-FLAG
           MOVE EURM-REVIEW-FLAG
             TO EUFEE-REVIEW-FLAG
           MOVE EURM-CYCLE-CNT
             TO EUFEE-CYCLE-CNT
           MOVE EURM-PAYMENT-CNT
             TO EUFEE-PAYMENT-CNT
           MOVE EURM-FEE-PCT
             TO EUFEE-FEE-PCT
           MOVE EURM-RATE
             TO EUFEE-RATE
           MOVE EURM-REF-NO
             TO EUFEE-REF-NO
           MOVE EURM-CUST-ID
             TO EUFEE-CUST-ID
           EXEC CICS LINK PROGRAM('EUFEE') COMMAREA(EUFEE-PARMS)
                LENGTH(LENGTH OF EUFEE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF EUFEE-RETURN-CODE = 08
               MOVE 'RULE EUFEE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE EURM-KEY TO KEYO
           MOVE EURM-STATUS-CD TO STATUSO
           MOVE EURM-OVERRIDE-FLAG TO OVERRIO
           MOVE EURM-NOTICE-DATE TO NOTICEO
           MOVE EURM-TRIANGULAR-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO TRIANGO
           MOVE EURM-CONVERTED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CONVERO
           MOVE EURM-REF-NO TO REFNOO
           MOVE EURM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE EURM-ROUNDING-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ROUNDIO
           MOVE EURM-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEYTDO
           MOVE EURM-REVIEW-FLAG TO REVIEWO
           MOVE EURM-ROUNDING-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ROUNDI1O
           MOVE EURM-EURO-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO EUROYTO
           MOVE EURM-RESIDUAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RESIDUO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('EURMST') INTO(EUR-MASTER-REC)
                RIDFLD(EURM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF TRIANGL > 0
               COMPUTE EURM-TRIANGULAR-YTD = FUNCTION NUMVAL(TRIANGI)
           END-IF
           IF CONVERL > 0
               COMPUTE EURM-CONVERTED-YTD = FUNCTION NUMVAL(CONVERI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO EURM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('EURMST') FROM(EUR-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('EUAPRM') MAPSET('EUAPRS')
                FROM(EUAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
