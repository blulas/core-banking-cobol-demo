      *================================================================*
      * PROGRAM  : PPINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PP69                                                *
      * PURPOSE  : POSITIVE PAY MAINTENANCE. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PPELG, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-07 PQR  ORIGINAL                                       *
      * 1992-06-03 TWH  PERFORMANCE - ISSUED READ AHEAD CHG0672        *
      * 1996-11-02 BHS  PAID ROUNDING - HALF UP INC4526                *
      * 1996-03-17 TWH  VOID STATUS CODES ADDED PRB4679                *
      * 1997-08-08 SKN  ISSUED CUTOFF MOVED REQ1003                    *
      * 1999-11-12 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-10-23 LMB  AUDIT FINDING - ISSUED CONTROL TOTALS INC7380  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PP69'.
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
       COPY PPINQS.
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
           MOVE LOW-VALUES TO PPINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PPINQM') MAPSET('PPINQS')
                FROM(PPINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PPINQM') MAPSET('PPINQS')
                INTO(PPINQMI) RESP(WS-RESP) END-EXEC
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
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF EXCEPT1L > 0
               IF FUNCTION TEST-NUMVAL(EXCEPT1I) NOT = 0
                   MOVE 'EXCEPT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO EXCEPT1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(EXCEPT1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'EXCEPT YTD MAY NOT BE NEGATIVE' TO
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
           MOVE PPYM-CUST-ID TO CUSTIDO
           MOVE PPYM-LAST-PAY-DATE TO LASTPAO
           MOVE PPYM-EXCEPT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPTO
           MOVE PPYM-STATUS-CD TO STATUSO
           MOVE PPYM-ISSUED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEDO
           MOVE PPYM-LAST-ACT-DATE TO LASTACO
           MOVE PPYM-HOLD-FLAG TO HOLDFLO
           MOVE PPYM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE PPYM-EFFECTIVE-DATE TO EFFECTO
           MOVE PPYM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE PPYM-VOID-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO VOIDAMO
           MOVE PPYM-CYCLE-CNT TO CYCLECO
           MOVE PPYM-EXCEPT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPT1O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PPYMST') INTO(PPY-MASTER-REC)
                RIDFLD(PPYM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO PPYM-NOTICE-SENT-FLAG
           END-IF
           IF EXCEPT1L > 0
               COMPUTE PPYM-EXCEPT-YTD = FUNCTION NUMVAL(EXCEPT1I)
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
           EXEC CICS SEND MAP('PPINQM') MAPSET('PPINQS')
                FROM(PPINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
