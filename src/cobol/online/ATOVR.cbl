      *================================================================*
      * PROGRAM  : ATOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : AT91                                                *
      * PURPOSE  : ATM NETWORK SETTLEMENT HISTORY. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : ATTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-07 DPS  ORIGINAL                                       *
      * 1995-05-04 PQR  AUDIT FINDING - SETTLE CONTROL TOTALS REQ0425  *
      * 1998-03-26 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-08-13 MKL  SURCHG LIMIT REVISED PRB8562                   *
      * 2008-12-28 BHS  CASSETTE EXCEPTIONS TO OPS QUEUE REQ8975       *
      * 2009-06-10 RJT  DISPENSED FEE SCHEDULE UPDATE INC4350          *
      * 2009-09-13 CRP  DISPENSED ROUNDING - HALF UP CHG2858           *
      * 2009-08-27 CRP  SURCHG NOTICE TEXT PER LEGAL CHG1047           *
      * 2011-11-12 LMB  SETTLE ROUNDING - HALF UP INC3430              *
      * 2012-06-05 PQR  DISPENSED CALCULATION CORRECTED PRB2541        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'AT91'.
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
       COPY ATNMST.
       COPY ATOVRS.
       COPY ATTIERP.
       COPY DCLATN.
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
           MOVE LOW-VALUES TO ATOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('ATOVRM') MAPSET('ATOVRS')
                FROM(ATOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('ATOVRM') MAPSET('ATOVRS')
                INTO(ATOVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO ATNM-KEY
           EXEC CICS READ FILE('ATNMST') INTO(ATN-MASTER-REC)
                RIDFLD(ATNM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CASSETL > 0
               IF FUNCTION TEST-NUMVAL(CASSETI) NOT = 0
                   MOVE 'CASSETTE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CASSETL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CASSETI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CASSETTE AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
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
           INITIALIZE ATTIER-PARMS
           MOVE ATNM-SETTLE-AMT
             TO ATTIER-SETTLE-AMT
           MOVE ATNM-INTERCHG-YTD
             TO ATTIER-INTERCHG-YTD
           MOVE ATNM-ADJUST-YTD
             TO ATTIER-ADJUST-YTD
           MOVE ATNM-ADJUST-AMT
             TO ATTIER-ADJUST-AMT
           MOVE ATNM-CASSETTE-YTD
             TO ATTIER-CASSETTE-YTD
           MOVE ATNM-SURCHG-AMT
             TO ATTIER-SURCHG-AMT
           MOVE ATNM-NOTICE-DATE
             TO ATTIER-NOTICE-DATE
           MOVE ATNM-EXPIRY-DATE
             TO ATTIER-EXPIRY-DATE
           MOVE ATNM-NEXT-REVIEW-DATE
             TO ATTIER-NEXT-REVIEW-DATE
           MOVE ATNM-NEXT-DUE-DATE
             TO ATTIER-NEXT-DUE-DATE
           MOVE ATNM-BRANCH-CD
             TO ATTIER-BRANCH-CD
           MOVE ATNM-STATUS-CD
             TO ATTIER-STATUS-CD
           MOVE ATNM-REGION-CD
             TO ATTIER-REGION-CD
           MOVE ATNM-REASON-CD
             TO ATTIER-REASON-CD
           MOVE ATNM-STOP-FLAG
             TO ATTIER-STOP-FLAG
           MOVE ATNM-EXEMPT-FLAG
             TO ATTIER-EXEMPT-FLAG
           MOVE ATNM-NOTICE-SENT-FLAG
             TO ATTIER-NOTICE-SENT-FLAG
           MOVE ATNM-ITEM-CNT
             TO ATTIER-ITEM-CNT
           MOVE ATNM-RETRY-CNT
             TO ATTIER-RETRY-CNT
           MOVE ATNM-FEE-PCT
             TO ATTIER-FEE-PCT
           MOVE ATNM-RATE
             TO ATTIER-RATE
           MOVE ATNM-DESC
             TO ATTIER-DESC
           MOVE ATNM-CUST-ID
             TO ATTIER-CUST-ID
           EXEC CICS LINK PROGRAM('ATTIER') COMMAREA(ATTIER-PARMS)
                LENGTH(LENGTH OF ATTIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF ATTIER-RETURN-CODE = 08
               MOVE 'RULE ATTIER DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ATNM-KEY TO KEYO
           MOVE ATNM-CUST-ID TO CUSTIDO
           MOVE ATNM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE ATNM-RETRY-CNT TO RETRYCO
           MOVE ATNM-DESC TO DESCO
           MOVE ATNM-REGION-CD TO REGIONO
           MOVE ATNM-STATUS-CD TO STATUSO
           MOVE ATNM-NAME TO NAMEO
           MOVE ATNM-CASSETTE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASSETO
           MOVE ATNM-MATURITY-DATE TO MATURIO
           MOVE ATNM-STOP-FLAG TO STOPFLO
           MOVE ATNM-REF-NO TO REFNOO
           MOVE ATNM-INTERCHG-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTERCO
           MOVE ATNM-ADDR TO ADDRO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ATNMST') INTO(ATN-MASTER-REC)
                RIDFLD(ATNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CASSETL > 0
               COMPUTE ATNM-CASSETTE-AMT = FUNCTION NUMVAL(CASSETI)
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO ATNM-STOP-FLAG
           END-IF
           EXEC CICS REWRITE FILE('ATNMST') FROM(ATN-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('ATOVRM') MAPSET('ATOVRS')
                FROM(ATOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
