      *================================================================*
      * PROGRAM  : ATAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : AT35                                                *
      * PURPOSE  : ATM NETWORK SETTLEMENT INQUIRY. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : ATTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-03 MKL  ORIGINAL                                       *
      * 1993-05-03 EWS  INTERCHG CALCULATION CORRECTED PRB9610         *
      * 1993-07-01 LMB  PERFORMANCE - CASSETTE READ AHEAD REQ5149      *
      * 1995-10-06 EWS  SETTLE FEE SCHEDULE UPDATE PRB3432             *
      * 2002-05-03 DPS  ADJUST TABLE EXPANDED CHG2257                  *
      * 2003-09-15 RJT  PERFORMANCE - DISPENSED READ AHEAD CHG8976     *
      * 2005-07-28 GAF  DISPENSED LIMIT REVISED REQ8876                *
      * 2007-07-23 DPS  SETTLE TABLE EXPANDED REQ4585                  *
      * 2007-03-16 EWS  DISPENSED EXCEPTIONS TO OPS QUEUE PRB8199      *
      * 2008-05-27 RJT  AUDIT FINDING - DISPENSED CONTROL TOTALS INC703*
      * 2009-12-05 SKN  INTERCHG ROUNDING - HALF UP CHG2243            *
      * 2014-02-24 PQR  SURCHG TABLE EXPANDED CHG6892                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'AT35'.
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
       COPY ATAPRS.
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
           MOVE LOW-VALUES TO ATAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('ATAPRM') MAPSET('ATAPRS')
                FROM(ATAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('ATAPRM') MAPSET('ATAPRS')
                INTO(ATAPRMI) RESP(WS-RESP) END-EXEC
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
           IF BRANCHL > 0
              AND BRANCHI NOT = 'DU'
              AND BRANCHI NOT = 'HX'
              AND BRANCHI NOT = 'YC'
               MOVE 'BRANCH CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO BRANCHL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF INTERCL > 0
               IF FUNCTION TEST-NUMVAL(INTERCI) NOT = 0
                   MOVE 'INTERCHG AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO INTERCL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(INTERCI)
                   IF WS-NUM-WORK < 0
                       MOVE 'INTERCHG AMT MAY NOT BE NEGATIVE' TO
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
           MOVE ATNM-CLOSE-DATE TO CLOSEDO
           MOVE ATNM-MATURITY-DATE TO MATURIO
           MOVE ATNM-BRANCH-CD TO BRANCHO
           MOVE ATNM-NETWORK-CD TO NETWORO
           MOVE ATNM-NAME TO NAMEO
           MOVE ATNM-RETRY-CNT TO RETRYCO
           MOVE ATNM-NOTICE-DATE TO NOTICEO
           MOVE ATNM-INTERCHG-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTERCO
           MOVE ATNM-ADDR TO ADDRO
           MOVE ATNM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE ATNM-CASSETTE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASSETO
           MOVE ATNM-NOTICE-CNT TO NOTICE1O
           MOVE ATNM-DESC TO DESCO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ATNMST') INTO(ATN-MASTER-REC)
                RIDFLD(ATNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF BRANCHL > 0
               MOVE BRANCHI TO ATNM-BRANCH-CD
           END-IF
           IF INTERCL > 0
               COMPUTE ATNM-INTERCHG-AMT = FUNCTION NUMVAL(INTERCI)
           END-IF
           EXEC CICS REWRITE FILE('ATNMST') FROM(ATN-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('ATAPRM') MAPSET('ATAPRS')
                FROM(ATAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
