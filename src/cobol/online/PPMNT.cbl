      *================================================================*
      * PROGRAM  : PPMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PP90                                                *
      * PURPOSE  : POSITIVE PAY INQUIRY. ENTER VALIDATES AND DISPLAYS, *
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : PPELG, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-13 PQR  ORIGINAL                                       *
      * 1993-06-05 JLM  PAID FEE SCHEDULE UPDATE PRB5872               *
      * 1996-07-02 BHS  AUDIT FINDING - STALE CONTROL TOTALS INC9416   *
      * 2000-02-05 MKL  VOID CUTOFF MOVED REQ6855                      *
      * 2000-05-08 RJT  AUDIT FINDING - VOID CONTROL TOTALS PRB8887    *
      * 2001-08-21 TWH  EXCEPT RERUN PROTECTION INC6666                *
      * 2002-02-23 PQR  STALE RERUN PROTECTION INC5659                 *
      * 2002-02-16 LMB  VOID FEE SCHEDULE UPDATE REQ6733               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PP90'.
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
       COPY PPYMST.
       COPY PPMNTS.
       COPY PPELGP.
       COPY DCLPPY.
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
           MOVE LOW-VALUES TO PPMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PPMNTM') MAPSET('PPMNTS')
                FROM(PPMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PPMNTM') MAPSET('PPMNTS')
                INTO(PPMNTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO PPYM-KEY
           EXEC CICS READ FILE('PPYMST') INTO(PPY-MASTER-REC)
                RIDFLD(PPYM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXCEPTL > 0
               IF FUNCTION TEST-NUMVAL(EXCEPTI) NOT = 0
                   MOVE 'EXCEPT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO EXCEPTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(EXCEPTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'EXCEPT AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF ISSUEDL > 0
               IF FUNCTION TEST-NUMVAL(ISSUEDI) NOT = 0
                   MOVE 'ISSUED AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ISSUEDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ISSUEDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'ISSUED AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CYCLECL > 0
              AND CYCLECI NOT = 'A'
              AND CYCLECI NOT = 'B'
              AND CYCLECI NOT = 'C'
               MOVE 'CYCLE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CYCLECL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF ISSUED1L > 0
               IF FUNCTION TEST-NUMVAL(ISSUED1I) NOT = 0
                   MOVE 'ISSUED YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ISSUED1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ISSUED1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'ISSUED YTD MAY NOT BE NEGATIVE' TO
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
           INITIALIZE PPELG-PARMS
           MOVE PPYM-PAID-AMT
             TO PPELG-PAID-AMT
           MOVE PPYM-PAID-YTD
             TO PPELG-PAID-YTD
           MOVE PPYM-VOID-AMT
             TO PPELG-VOID-AMT
           MOVE PPYM-ISSUED-YTD
             TO PPELG-ISSUED-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPELG-EXCEPT-AMT
           MOVE PPYM-STALE-AMT
             TO PPELG-STALE-AMT
           MOVE PPYM-NEXT-DUE-DATE
             TO PPELG-NEXT-DUE-DATE
           MOVE PPYM-EFFECTIVE-DATE
             TO PPELG-EFFECTIVE-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPELG-EXPIRY-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPELG-CLOSE-DATE
           MOVE PPYM-STATUS-CD
             TO PPELG-STATUS-CD
           MOVE PPYM-REASON-CD
             TO PPELG-REASON-CD
           MOVE PPYM-CYCLE-CD
             TO PPELG-CYCLE-CD
           MOVE PPYM-REGION-CD
             TO PPELG-REGION-CD
           MOVE PPYM-HOLD-FLAG
             TO PPELG-HOLD-FLAG
           MOVE PPYM-STOP-FLAG
             TO PPELG-STOP-FLAG
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPELG-NOTICE-SENT-FLAG
           MOVE PPYM-EXCEPT-CNT
             TO PPELG-EXCEPT-CNT
           MOVE PPYM-CYCLE-CNT
             TO PPELG-CYCLE-CNT
           MOVE PPYM-DISCOUNT
             TO PPELG-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPELG-MARGIN
           MOVE PPYM-REF-NO
             TO PPELG-REF-NO
           MOVE PPYM-CONTACT
             TO PPELG-CONTACT
           EXEC CICS LINK PROGRAM('PPELG') COMMAREA(PPELG-PARMS)
                LENGTH(LENGTH OF PPELG-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PPELG-RETURN-CODE = 08
               MOVE 'RULE PPELG DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PPYM-KEY TO KEYO
           MOVE PPYM-PAYMENT-CNT TO PAYMENO
           MOVE PPYM-STALE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STALEYO
           MOVE PPYM-EXCEPT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPTO
           MOVE PPYM-STALE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO STALEAO
           MOVE PPYM-EXCEPT-CNT TO EXCEPT1O
           MOVE PPYM-PAID-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAIDYTO
           MOVE PPYM-STATUS-CD TO STATUSO
           MOVE PPYM-MATCH-RULE-CD TO MATCHRO
           MOVE PPYM-LAST-MAINT-DATE TO LASTMAO
           MOVE PPYM-ISSUED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEDO
           MOVE PPYM-CYCLE-CD TO CYCLECO
           MOVE PPYM-REF-NO TO REFNOO
           MOVE PPYM-ISSUED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUED1O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PPYMST') INTO(PPY-MASTER-REC)
                RIDFLD(PPYM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXCEPTL > 0
               COMPUTE PPYM-EXCEPT-AMT = FUNCTION NUMVAL(EXCEPTI)
           END-IF
           IF ISSUEDL > 0
               COMPUTE PPYM-ISSUED-AMT = FUNCTION NUMVAL(ISSUEDI)
           END-IF
           IF CYCLECL > 0
               MOVE CYCLECI TO PPYM-CYCLE-CD
           END-IF
           IF ISSUED1L > 0
               COMPUTE PPYM-ISSUED-YTD = FUNCTION NUMVAL(ISSUED1I)
           END-IF
           MOVE CTL-BUSINESS-DATE TO PPYM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('PPYMST') FROM(PPY-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('PPMNTM') MAPSET('PPMNTS')
                FROM(PPMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
