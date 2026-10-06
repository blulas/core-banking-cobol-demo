      *================================================================*
      * PROGRAM  : XCOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : XC59                                                *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS HISTORY. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : XCTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-15 PQR  ORIGINAL                                       *
      * 1996-10-03 JLM  CONTRIB RERUN PROTECTION REQ1459               *
      * 1996-03-15 LMB  PERFORMANCE - BONUS READ AHEAD REQ1511         *
      * 2000-09-12 PQR  WEEKLY LIMIT REVISED INC0747                   *
      * 2001-05-26 PQR  MISSED CALCULATION CORRECTED PRB0721           *
      * 2001-11-27 PQR  ACCRUED CALCULATION CORRECTED CHG0874          *
      * 2002-03-03 SKN  PENALTY TABLE EXPANDED PRB5053                 *
      * 2003-02-09 RJT  GOAL LIMIT REVISED REQ2539                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'XC59'.
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
       COPY XCLMST.
       COPY XCOVRS.
       COPY XCTIERP.
       COPY DCLXCL.
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
           MOVE LOW-VALUES TO XCOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('XCOVRM') MAPSET('XCOVRS')
                FROM(XCOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('XCOVRM') MAPSET('XCOVRS')
                INTO(XCOVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO XCLM-KEY
           EXEC CICS READ FILE('XCLMST') INTO(XCL-MASTER-REC)
                RIDFLD(XCLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF PAYOUTL > 0
               IF FUNCTION TEST-NUMVAL(PAYOUTI) NOT = 0
                   MOVE 'PAYOUT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO PAYOUTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(PAYOUTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'PAYOUT YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF BONUSAL > 0
               IF FUNCTION TEST-NUMVAL(BONUSAI) NOT = 0
                   MOVE 'BONUS AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO BONUSAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(BONUSAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'BONUS AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF MISSEDL > 0
               IF FUNCTION TEST-NUMVAL(MISSEDI) NOT = 0
                   MOVE 'MISSED AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO MISSEDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(MISSEDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'MISSED AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE XCTIER-PARMS
           MOVE XCLM-BONUS-YTD
             TO XCTIER-BONUS-YTD
           MOVE XCLM-ACCRUED-AMT
             TO XCTIER-ACCRUED-AMT
           MOVE XCLM-PAYOUT-AMT
             TO XCTIER-PAYOUT-AMT
           MOVE XCLM-ACCRUED-YTD
             TO XCTIER-ACCRUED-YTD
           MOVE XCLM-MISSED-AMT
             TO XCTIER-MISSED-AMT
           MOVE XCLM-GOAL-AMT
             TO XCTIER-GOAL-AMT
           MOVE XCLM-NOTICE-DATE
             TO XCTIER-NOTICE-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCTIER-LAST-PAY-DATE
           MOVE XCLM-LAST-STMT-DATE
             TO XCTIER-LAST-STMT-DATE
           MOVE XCLM-EXPIRY-DATE
             TO XCTIER-EXPIRY-DATE
           MOVE XCLM-PAYOUT-METHOD-CD
             TO XCTIER-PAYOUT-METHOD-CD
           MOVE XCLM-REGION-CD
             TO XCTIER-REGION-CD
           MOVE XCLM-PLAN-CD
             TO XCTIER-PLAN-CD
           MOVE XCLM-REASON-CD
             TO XCTIER-REASON-CD
           MOVE XCLM-OVERRIDE-FLAG
             TO XCTIER-OVERRIDE-FLAG
           MOVE XCLM-SUSPEND-FLAG
             TO XCTIER-SUSPEND-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCTIER-PREFERRED-FLAG
           MOVE XCLM-RETRY-CNT
             TO XCTIER-RETRY-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCTIER-CYCLE-CNT
           MOVE XCLM-RATE
             TO XCTIER-RATE
           MOVE XCLM-DISCOUNT
             TO XCTIER-DISCOUNT
           MOVE XCLM-NAME
             TO XCTIER-NAME
           MOVE XCLM-CONTACT
             TO XCTIER-CONTACT
           EXEC CICS LINK PROGRAM('XCTIER') COMMAREA(XCTIER-PARMS)
                LENGTH(LENGTH OF XCTIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF XCTIER-RETURN-CODE = 08
               MOVE 'RULE XCTIER DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE XCLM-KEY TO KEYO
           MOVE XCLM-DESC TO DESCO
           MOVE XCLM-STOP-FLAG TO STOPFLO
           MOVE XCLM-CONTACT TO CONTACO
           MOVE XCLM-RETRY-CNT TO RETRYCO
           MOVE XCLM-GOAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO GOALYTO
           MOVE XCLM-ACCRUED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ACCRUEO
           MOVE XCLM-PAYOUT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYOUTO
           MOVE XCLM-BONUS-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO BONUSAO
           MOVE XCLM-MISSED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO MISSEDO
           MOVE XCLM-NOTICE-DATE TO NOTICEO
           MOVE XCLM-NEXT-DUE-DATE TO NEXTDUO
           MOVE XCLM-PLAN-CD TO PLANCDO
           MOVE XCLM-REASON-CD TO REASONO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('XCLMST') INTO(XCL-MASTER-REC)
                RIDFLD(XCLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO XCLM-STOP-FLAG
           END-IF
           IF PAYOUTL > 0
               COMPUTE XCLM-PAYOUT-YTD = FUNCTION NUMVAL(PAYOUTI)
           END-IF
           IF BONUSAL > 0
               COMPUTE XCLM-BONUS-AMT = FUNCTION NUMVAL(BONUSAI)
           END-IF
           IF MISSEDL > 0
               COMPUTE XCLM-MISSED-AMT = FUNCTION NUMVAL(MISSEDI)
           END-IF
           EXEC CICS REWRITE FILE('XCLMST') FROM(XCL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('XCOVRM') MAPSET('XCOVRS')
                FROM(XCOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
