      *================================================================*
      * PROGRAM  : AUMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : AU88                                                *
      * PURPOSE  : DEALER AUTO FINANCE MAINTENANCE. ENTER VALIDATES AND*
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : AURISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-11-27 PQR  ORIGINAL                                       *
      * 1996-05-22 GAF  RESERVE TABLE EXPANDED INC4507                 *
      * 1998-04-22 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-10-08 MKL  REBATE RERUN PROTECTION CHG6966                *
      * 2006-06-28 JLM  REBATE STATUS CODES ADDED REQ8382              *
      * 2008-08-07 GAF  PAYMENT STATUS CODES ADDED PRB7405             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'AU88'.
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
       COPY AUMNTS.
       COPY AURISKP.
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
           MOVE LOW-VALUES TO AUMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('AUMNTM') MAPSET('AUMNTS')
                FROM(AUMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('AUMNTM') MAPSET('AUMNTS')
                INTO(AUMNTMI) RESP(WS-RESP) END-EXEC
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
           IF CYCLECL > 0
              AND CYCLECI NOT = 'A'
              AND CYCLECI NOT = 'H'
              AND CYCLECI NOT = 'J'
               MOVE 'CYCLE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CYCLECL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF EXEMPTL > 0 AND EXEMPTI NOT = 'Y' AND EXEMPTI NOT = 'N'
               MOVE 'EXEMPT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF GAPAMTL > 0
               IF FUNCTION TEST-NUMVAL(GAPAMTI) NOT = 0
                   MOVE 'GAP AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO GAPAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(GAPAMTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'GAP AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'EUA'
              AND STATUSI NOT = 'HMW'
              AND STATUSI NOT = 'RCB'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
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
           INITIALIZE AURISK-PARMS
           MOVE ADLM-GAP-AMT
             TO AURISK-GAP-AMT
           MOVE ADLM-AMOUNT-FIN-AMT
             TO AURISK-AMOUNT-FIN-AMT
           MOVE ADLM-CHARGEBK-AMT
             TO AURISK-CHARGEBK-AMT
           MOVE ADLM-REBATE-AMT
             TO AURISK-REBATE-AMT
           MOVE ADLM-RESERVE-AMT
             TO AURISK-RESERVE-AMT
           MOVE ADLM-PAYMENT-AMT
             TO AURISK-PAYMENT-AMT
           MOVE ADLM-NEXT-REVIEW-DATE
             TO AURISK-NEXT-REVIEW-DATE
           MOVE ADLM-EFFECTIVE-DATE
             TO AURISK-EFFECTIVE-DATE
           MOVE ADLM-LAST-STMT-DATE
             TO AURISK-LAST-STMT-DATE
           MOVE ADLM-LAST-ACT-DATE
             TO AURISK-LAST-ACT-DATE
           MOVE ADLM-STATUS-CD
             TO AURISK-STATUS-CD
           MOVE ADLM-TIER-CD
             TO AURISK-TIER-CD
           MOVE ADLM-CYCLE-CD
             TO AURISK-CYCLE-CD
           MOVE ADLM-DEALER-CD
             TO AURISK-DEALER-CD
           MOVE ADLM-REVIEW-FLAG
             TO AURISK-REVIEW-FLAG
           MOVE ADLM-EXEMPT-FLAG
             TO AURISK-EXEMPT-FLAG
           MOVE ADLM-PREFERRED-FLAG
             TO AURISK-PREFERRED-FLAG
           MOVE ADLM-PAYMENT-CNT
             TO AURISK-PAYMENT-CNT
           MOVE ADLM-RETRY-CNT
             TO AURISK-RETRY-CNT
           MOVE ADLM-RATE
             TO AURISK-RATE
           MOVE ADLM-MARGIN
             TO AURISK-MARGIN
           MOVE ADLM-REF-NO
             TO AURISK-REF-NO
           MOVE ADLM-CONTACT
             TO AURISK-CONTACT
           EXEC CICS LINK PROGRAM('AURISK') COMMAREA(AURISK-PARMS)
                LENGTH(LENGTH OF AURISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF AURISK-RETURN-CODE = 08
               MOVE 'RULE AURISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ADLM-KEY TO KEYO
           MOVE ADLM-EXCEPT-CNT TO EXCEPTO
           MOVE ADLM-PAYMENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMENO
           MOVE ADLM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE ADLM-CYCLE-CD TO CYCLECO
           MOVE ADLM-ADDR TO ADDRO
           MOVE ADLM-EXEMPT-FLAG TO EXEMPTO
           MOVE ADLM-STOP-FLAG TO STOPFLO
           MOVE ADLM-LAST-PAY-DATE TO LASTPAO
           MOVE ADLM-CHARGEBK-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CHARGEO
           MOVE ADLM-GAP-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO GAPAMTO
           MOVE ADLM-PAYMENT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMEN1O
           MOVE ADLM-OVERRIDE-FLAG TO OVERRIO
           MOVE ADLM-STATUS-CD TO STATUSO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ADLMST') INTO(ADL-MASTER-REC)
                RIDFLD(ADLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CYCLECL > 0
               MOVE CYCLECI TO ADLM-CYCLE-CD
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO ADLM-EXEMPT-FLAG
           END-IF
           IF GAPAMTL > 0
               COMPUTE ADLM-GAP-AMT = FUNCTION NUMVAL(GAPAMTI)
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO ADLM-STATUS-CD
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
           EXEC CICS SEND MAP('AUMNTM') MAPSET('AUMNTS')
                FROM(AUMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
