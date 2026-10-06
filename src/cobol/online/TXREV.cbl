      *================================================================*
      * PROGRAM  : TXREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TX70                                                *
      * PURPOSE  : TELEX WIRE MESSAGING REVIEW. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TXPRC, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-08-02 PQR  ORIGINAL                                       *
      * 1998-10-27 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-10-12 JLM  CORRESP EXCEPTIONS TO OPS QUEUE CHG4213        *
      * 2004-05-16 CRP  CORRESP CALCULATION CORRECTED INC1108          *
      * 2005-05-09 PQR  NEW QUEUED REPORT COLUMNS INC5620              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TX70'.
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
       COPY TXREVS.
       COPY TXPRCP.
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
           MOVE LOW-VALUES TO TXREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TXREVM') MAPSET('TXREVS')
                FROM(TXREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TXREVM') MAPSET('TXREVS')
                INTO(TXREVMI) RESP(WS-RESP) END-EXEC
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
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF BRANCHL > 0
              AND BRANCHI NOT = 'CC'
              AND BRANCHI NOT = 'KR'
              AND BRANCHI NOT = 'LD'
               MOVE 'BRANCH CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO BRANCHL
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
           INITIALIZE TXPRC-PARMS
           MOVE TLXM-COVER-YTD
             TO TXPRC-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXPRC-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXPRC-CORRESP-YTD
           MOVE TLXM-SETTLE-YTD
             TO TXPRC-SETTLE-YTD
           MOVE TLXM-QUEUED-YTD
             TO TXPRC-QUEUED-YTD
           MOVE TLXM-SETTLE-AMT
             TO TXPRC-SETTLE-AMT
           MOVE TLXM-OPEN-DATE
             TO TXPRC-OPEN-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXPRC-NEXT-DUE-DATE
           MOVE TLXM-NOTICE-DATE
             TO TXPRC-NOTICE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXPRC-EXPIRY-DATE
           MOVE TLXM-TEST-KEY-CD
             TO TXPRC-TEST-KEY-CD
           MOVE TLXM-REGION-CD
             TO TXPRC-REGION-CD
           MOVE TLXM-BRANCH-CD
             TO TXPRC-BRANCH-CD
           MOVE TLXM-REASON-CD
             TO TXPRC-REASON-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXPRC-NOTICE-SENT-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXPRC-STOP-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXPRC-REVIEW-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXPRC-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXPRC-EXCEPT-CNT
           MOVE TLXM-RATE
             TO TXPRC-RATE
           MOVE TLXM-DISCOUNT
             TO TXPRC-DISCOUNT
           MOVE TLXM-ADDR
             TO TXPRC-ADDR
           MOVE TLXM-NAME
             TO TXPRC-NAME
           EXEC CICS LINK PROGRAM('TXPRC') COMMAREA(TXPRC-PARMS)
                LENGTH(LENGTH OF TXPRC-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TXPRC-RETURN-CODE = 08
               MOVE 'RULE TXPRC DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TLXM-KEY TO KEYO
           MOVE TLXM-COVER-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COVERAO
           MOVE TLXM-STOP-FLAG TO STOPFLO
           MOVE TLXM-BRANCH-CD TO BRANCHO
           MOVE TLXM-REGION-CD TO REGIONO
           MOVE TLXM-EXCEPT-CNT TO EXCEPTO
           MOVE TLXM-OPEN-DATE TO OPENDAO
           MOVE TLXM-LAST-ACT-DATE TO LASTACO
           MOVE TLXM-NOTICE-CNT TO NOTICEO
           MOVE TLXM-NEXT-DUE-DATE TO NEXTDUO
           MOVE TLXM-TYPE-CD TO TYPECDO
           MOVE TLXM-CONTACT TO CONTACO
           MOVE TLXM-EFFECTIVE-DATE TO EFFECTO
           MOVE TLXM-CLOSE-DATE TO CLOSEDO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TLXMST') INTO(TLX-MASTER-REC)
                RIDFLD(TLXM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF COVERAL > 0
               COMPUTE TLXM-COVER-AMT = FUNCTION NUMVAL(COVERAI)
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO TLXM-STOP-FLAG
           END-IF
           IF BRANCHL > 0
               MOVE BRANCHI TO TLXM-BRANCH-CD
           END-IF
           EXEC CICS REWRITE FILE('TLXMST') FROM(TLX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TXREVM') MAPSET('TXREVS')
                FROM(TXREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
