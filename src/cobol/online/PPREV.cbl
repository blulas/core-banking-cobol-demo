      *================================================================*
      * PROGRAM  : PPREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PP57                                                *
      * PURPOSE  : POSITIVE PAY MAINTENANCE. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PPLIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-18 GAF  ORIGINAL                                       *
      * 1996-10-26 JLM  EXCEPT NOTICE TEXT PER LEGAL INC9575           *
      * 1997-09-20 PQR  PAID CUTOFF MOVED REQ7787                      *
      * 1998-06-03 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-23 LMB  VOID RERUN PROTECTION INC4275                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PP57'.
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
       COPY PPREVS.
       COPY PPLIMP.
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
           MOVE LOW-VALUES TO PPREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PPREVM') MAPSET('PPREVS')
                FROM(PPREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PPREVM') MAPSET('PPREVS')
                INTO(PPREVMI) RESP(WS-RESP) END-EXEC
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
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE PPLIM-PARMS
           MOVE PPYM-STALE-YTD
             TO PPLIM-STALE-YTD
           MOVE PPYM-EXCEPT-YTD
             TO PPLIM-EXCEPT-YTD
           MOVE PPYM-VOID-YTD
             TO PPLIM-VOID-YTD
           MOVE PPYM-PAID-YTD
             TO PPLIM-PAID-YTD
           MOVE PPYM-ISSUED-YTD
             TO PPLIM-ISSUED-YTD
           MOVE PPYM-PAID-AMT
             TO PPLIM-PAID-AMT
           MOVE PPYM-NEXT-REVIEW-DATE
             TO PPLIM-NEXT-REVIEW-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPLIM-CLOSE-DATE
           MOVE PPYM-OPEN-DATE
             TO PPLIM-OPEN-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPLIM-EXPIRY-DATE
           MOVE PPYM-MATCH-RULE-CD
             TO PPLIM-MATCH-RULE-CD
           MOVE PPYM-DECISION-CD
             TO PPLIM-DECISION-CD
           MOVE PPYM-REASON-CD
             TO PPLIM-REASON-CD
           MOVE PPYM-REGION-CD
             TO PPLIM-REGION-CD
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPLIM-NOTICE-SENT-FLAG
           MOVE PPYM-AUTO-RENEW-FLAG
             TO PPLIM-AUTO-RENEW-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPLIM-EXEMPT-FLAG
           MOVE PPYM-CYCLE-CNT
             TO PPLIM-CYCLE-CNT
           MOVE PPYM-NOTICE-CNT
             TO PPLIM-NOTICE-CNT
           MOVE PPYM-RATE
             TO PPLIM-RATE
           MOVE PPYM-DISCOUNT
             TO PPLIM-DISCOUNT
           MOVE PPYM-NAME
             TO PPLIM-NAME
           MOVE PPYM-REF-NO
             TO PPLIM-REF-NO
           EXEC CICS LINK PROGRAM('PPLIM') COMMAREA(PPLIM-PARMS)
                LENGTH(LENGTH OF PPLIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PPLIM-RETURN-CODE = 08
               MOVE 'RULE PPLIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PPYM-KEY TO KEYO
           MOVE PPYM-LAST-MAINT-DATE TO LASTMAO
           MOVE PPYM-NAME TO NAMEO
           MOVE PPYM-REGION-CD TO REGIONO
           MOVE PPYM-EFFECTIVE-DATE TO EFFECTO
           MOVE PPYM-CUST-ID TO CUSTIDO
           MOVE PPYM-OVERRIDE-FLAG TO OVERRIO
           MOVE PPYM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE PPYM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE PPYM-EXCEPT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPTO
           MOVE PPYM-REASON-CD TO REASONO
           MOVE PPYM-STOP-FLAG TO STOPFLO
           MOVE PPYM-ISSUED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEDO
           MOVE PPYM-DESC TO DESCO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PPYMST') INTO(PPY-MASTER-REC)
                RIDFLD(PPYM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO PPYM-OVERRIDE-FLAG
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO PPYM-STOP-FLAG
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
           EXEC CICS SEND MAP('PPREVM') MAPSET('PPREVS')
                FROM(PPREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
