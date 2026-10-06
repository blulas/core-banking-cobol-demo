      *================================================================*
      * PROGRAM  : EUINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : EU73                                                *
      * PURPOSE  : EURO CONVERSION REVIEW. ENTER VALIDATES AND         *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : EUFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-19 TWH  ORIGINAL                                       *
      * 1998-06-02 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-04-27 EWS  PERFORMANCE - NATIONAL READ AHEAD INC4981      *
      * 2004-09-21 GAF  EURO FEE SCHEDULE UPDATE REQ1709               *
      * 2006-08-23 DPS  NATIONAL ROUNDING - HALF UP INC4509            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'EU73'.
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
       COPY EUINQS.
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
           MOVE LOW-VALUES TO EUINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('EUINQM') MAPSET('EUINQS')
                FROM(EUINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('EUINQM') MAPSET('EUINQS')
                INTO(EUINQMI) RESP(WS-RESP) END-EXEC
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
           IF CHANNEL > 0
              AND CHANNEI NOT = 'AEA'
              AND CHANNEI NOT = 'EBM'
              AND CHANNEI NOT = 'GAU'
               MOVE 'CHANNEL CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CHANNEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'B'
              AND STATUSI NOT = 'F'
              AND STATUSI NOT = 'K'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF REVIEWL > 0 AND REVIEWI NOT = 'Y' AND REVIEWI NOT = 'N'
               MOVE 'REVIEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF RESIDUL > 0
               IF FUNCTION TEST-NUMVAL(RESIDUI) NOT = 0
                   MOVE 'RESIDUAL YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO RESIDUL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(RESIDUI)
                   IF WS-NUM-WORK < 0
                       MOVE 'RESIDUAL YTD MAY NOT BE NEGATIVE' TO
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
           MOVE EURM-CHANNEL-CD TO CHANNEO
           MOVE EURM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE EURM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE EURM-CYCLE-CNT TO CYCLECO
           MOVE EURM-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEYTDO
           MOVE EURM-STATUS-CD TO STATUSO
           MOVE EURM-REGION-CD TO REGIONO
           MOVE EURM-OPEN-DATE TO OPENDAO
           MOVE EURM-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEAMTO
           MOVE EURM-TYPE-CD TO TYPECDO
           MOVE EURM-REVIEW-FLAG TO REVIEWO
           MOVE EURM-RESIDUAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RESIDUO
           MOVE EURM-CURRENCY-CD TO CURRENO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('EURMST') INTO(EUR-MASTER-REC)
                RIDFLD(EURM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO EURM-CHANNEL-CD
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO EURM-STATUS-CD
           END-IF
           IF REVIEWL > 0
               MOVE REVIEWI TO EURM-REVIEW-FLAG
           END-IF
           IF RESIDUL > 0
               COMPUTE EURM-RESIDUAL-YTD = FUNCTION NUMVAL(RESIDUI)
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
           EXEC CICS SEND MAP('EUINQM') MAPSET('EUINQS')
                FROM(EUINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
