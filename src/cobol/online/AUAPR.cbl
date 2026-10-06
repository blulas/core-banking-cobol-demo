      *================================================================*
      * PROGRAM  : AUAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : AU53                                                *
      * PURPOSE  : DEALER AUTO FINANCE MAINTENANCE. ENTER VALIDATES AND*
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : AUWAIV, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-02-08 PQR  ORIGINAL                                       *
      * 1997-07-25 DPS  RESERVE LIMIT REVISED INC4147                  *
      * 2004-02-06 BHS  AMOUNT-FIN FEE SCHEDULE UPDATE PRB9077         *
      * 2004-06-28 PQR  AMOUNT-FIN FEE SCHEDULE UPDATE INC6316         *
      * 2006-04-21 EWS  AUDIT FINDING - AMOUNT-FIN CONTROL TOTALS PRB61*
      * 2006-02-02 RJT  CHARGEBK TABLE EXPANDED CHG4230                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'AU53'.
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
       COPY ADLMST.
       COPY AUAPRS.
       COPY AUWAIVP.
       COPY DCLADL.
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
           MOVE LOW-VALUES TO AUAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('AUAPRM') MAPSET('AUAPRS')
                FROM(AUAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('AUAPRM') MAPSET('AUAPRS')
                INTO(AUAPRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO ADLM-KEY
           EXEC CICS READ FILE('ADLMST') INTO(ADL-MASTER-REC)
                RIDFLD(ADLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHARGEL > 0
               IF FUNCTION TEST-NUMVAL(CHARGEI) NOT = 0
                   MOVE 'CHARGEBK AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CHARGEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CHARGEI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CHARGEBK AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE AUWAIV-PARMS
           MOVE ADLM-PAYMENT-AMT
             TO AUWAIV-PAYMENT-AMT
           MOVE ADLM-GAP-AMT
             TO AUWAIV-GAP-AMT
           MOVE ADLM-AMOUNT-FIN-YTD
             TO AUWAIV-AMOUNT-FIN-YTD
           MOVE ADLM-PAYMENT-YTD
             TO AUWAIV-PAYMENT-YTD
           MOVE ADLM-CHARGEBK-AMT
             TO AUWAIV-CHARGEBK-AMT
           MOVE ADLM-AMOUNT-FIN-AMT
             TO AUWAIV-AMOUNT-FIN-AMT
           MOVE ADLM-NEXT-REVIEW-DATE
             TO AUWAIV-NEXT-REVIEW-DATE
           MOVE ADLM-LAST-PAY-DATE
             TO AUWAIV-LAST-PAY-DATE
           MOVE ADLM-LAST-MAINT-DATE
             TO AUWAIV-LAST-MAINT-DATE
           MOVE ADLM-NEXT-DUE-DATE
             TO AUWAIV-NEXT-DUE-DATE
           MOVE ADLM-TIER-CD
             TO AUWAIV-TIER-CD
           MOVE ADLM-CYCLE-CD
             TO AUWAIV-CYCLE-CD
           MOVE ADLM-STATUS-CD
             TO AUWAIV-STATUS-CD
           MOVE ADLM-TYPE-CD
             TO AUWAIV-TYPE-CD
           MOVE ADLM-PREFERRED-FLAG
             TO AUWAIV-PREFERRED-FLAG
           MOVE ADLM-NOTICE-SENT-FLAG
             TO AUWAIV-NOTICE-SENT-FLAG
           MOVE ADLM-EXEMPT-FLAG
             TO AUWAIV-EXEMPT-FLAG
           MOVE ADLM-PAYMENT-CNT
             TO AUWAIV-PAYMENT-CNT
           MOVE ADLM-RETRY-CNT
             TO AUWAIV-RETRY-CNT
           MOVE ADLM-FEE-PCT
             TO AUWAIV-FEE-PCT
           MOVE ADLM-RATE
             TO AUWAIV-RATE
           MOVE ADLM-CONTACT
             TO AUWAIV-CONTACT
           MOVE ADLM-NAME
             TO AUWAIV-NAME
           EXEC CICS LINK PROGRAM('AUWAIV') COMMAREA(AUWAIV-PARMS)
                LENGTH(LENGTH OF AUWAIV-PARMS) RESP(WS-RESP)
           END-EXEC
           IF AUWAIV-RETURN-CODE = 08
               MOVE 'RULE AUWAIV DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ADLM-KEY TO KEYO
           MOVE ADLM-NAME TO NAMEO
           MOVE ADLM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE ADLM-PAYMENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMENO
           MOVE ADLM-CONTACT TO CONTACO
           MOVE ADLM-OVERRIDE-FLAG TO OVERRIO
           MOVE ADLM-GAP-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO GAPAMTO
           MOVE ADLM-CHARGEBK-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CHARGEO
           MOVE ADLM-LAST-PAY-DATE TO LASTPAO
           MOVE ADLM-DESC TO DESCO
           MOVE ADLM-TYPE-CD TO TYPECDO
           MOVE ADLM-ADDR TO ADDRO
           MOVE ADLM-TIER-CD TO TIERCDO
           MOVE ADLM-LAST-MAINT-DATE TO LASTMAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ADLMST') INTO(ADL-MASTER-REC)
                RIDFLD(ADLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CHARGEL > 0
               COMPUTE ADLM-CHARGEBK-AMT = FUNCTION NUMVAL(CHARGEI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO ADLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('ADLMST') FROM(ADL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('AUAPRM') MAPSET('AUAPRS')
                FROM(AUAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
