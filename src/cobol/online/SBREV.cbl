      *================================================================*
      * PROGRAM  : SBREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SB70                                                *
      * PURPOSE  : SAVINGS BOND SERVICES INQUIRY. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SBLIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-10-24 SKN  ORIGINAL                                       *
      * 1996-03-06 EWS  ISSUE-PRICE RERUN PROTECTION CHG3891           *
      * 1999-04-14 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-01-17 RJT  INTEREST FEE SCHEDULE UPDATE REQ7791           *
      * 2005-10-10 DPS  FACE LIMIT REVISED REQ3770                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SB70'.
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
       COPY SBNMST.
       COPY SBREVS.
       COPY SBLIMP.
       COPY DCLSBN.
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
           MOVE LOW-VALUES TO SBREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SBREVM') MAPSET('SBREVS')
                FROM(SBREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SBREVM') MAPSET('SBREVS')
                INTO(SBREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO SBNM-KEY
           EXEC CICS READ FILE('SBNMST') INTO(SBN-MASTER-REC)
                RIDFLD(SBNM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REDEEML > 0
               IF FUNCTION TEST-NUMVAL(REDEEMI) NOT = 0
                   MOVE 'REDEEM YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO REDEEML
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(REDEEMI)
                   IF WS-NUM-WORK < 0
                       MOVE 'REDEEM YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF ACCRUAL > 0
               IF FUNCTION TEST-NUMVAL(ACCRUAI) NOT = 0
                   MOVE 'ACCRUAL YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ACCRUAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ACCRUAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'ACCRUAL YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'CS'
              AND STATUSI NOT = 'HF'
              AND STATUSI NOT = 'JZ'
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
           INITIALIZE SBLIM-PARMS
           MOVE SBNM-ISSUE-PRICE-YTD
             TO SBLIM-ISSUE-PRICE-YTD
           MOVE SBNM-FACE-YTD
             TO SBLIM-FACE-YTD
           MOVE SBNM-PURCHASE-YTD
             TO SBLIM-PURCHASE-YTD
           MOVE SBNM-TAXABLE-AMT
             TO SBLIM-TAXABLE-AMT
           MOVE SBNM-PURCHASE-AMT
             TO SBLIM-PURCHASE-AMT
           MOVE SBNM-TAXABLE-YTD
             TO SBLIM-TAXABLE-YTD
           MOVE SBNM-LAST-MAINT-DATE
             TO SBLIM-LAST-MAINT-DATE
           MOVE SBNM-NOTICE-DATE
             TO SBLIM-NOTICE-DATE
           MOVE SBNM-MATURITY-DATE
             TO SBLIM-MATURITY-DATE
           MOVE SBNM-NEXT-REVIEW-DATE
             TO SBLIM-NEXT-REVIEW-DATE
           MOVE SBNM-SOURCE-CD
             TO SBLIM-SOURCE-CD
           MOVE SBNM-REASON-CD
             TO SBLIM-REASON-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBLIM-ISSUE-TYPE-CD
           MOVE SBNM-SERIES-CD
             TO SBLIM-SERIES-CD
           MOVE SBNM-OVERRIDE-FLAG
             TO SBLIM-OVERRIDE-FLAG
           MOVE SBNM-PREFERRED-FLAG
             TO SBLIM-PREFERRED-FLAG
           MOVE SBNM-HOLD-FLAG
             TO SBLIM-HOLD-FLAG
           MOVE SBNM-PAYMENT-CNT
             TO SBLIM-PAYMENT-CNT
           MOVE SBNM-EXCEPT-CNT
             TO SBLIM-EXCEPT-CNT
           MOVE SBNM-PENALTY-PCT
             TO SBLIM-PENALTY-PCT
           MOVE SBNM-FEE-PCT
             TO SBLIM-FEE-PCT
           MOVE SBNM-REF-NO
             TO SBLIM-REF-NO
           MOVE SBNM-ADDR
             TO SBLIM-ADDR
           EXEC CICS LINK PROGRAM('SBLIM') COMMAREA(SBLIM-PARMS)
                LENGTH(LENGTH OF SBLIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SBLIM-RETURN-CODE = 08
               MOVE 'RULE SBLIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE SBNM-KEY TO KEYO
           MOVE SBNM-REDEEM-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO REDEEMO
           MOVE SBNM-REDEEM-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO REDEEM1O
           MOVE SBNM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE SBNM-HOLD-FLAG TO HOLDFLO
           MOVE SBNM-FACE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FACEYTO
           MOVE SBNM-REVIEW-FLAG TO REVIEWO
           MOVE SBNM-OPEN-DATE TO OPENDAO
           MOVE SBNM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE SBNM-ISSUE-TYPE-CD TO ISSUETO
           MOVE SBNM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE SBNM-PAYMENT-CNT TO PAYMENO
           MOVE SBNM-ACCRUAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ACCRUAO
           MOVE SBNM-STATUS-CD TO STATUSO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('SBNMST') INTO(SBN-MASTER-REC)
                RIDFLD(SBNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REDEEML > 0
               COMPUTE SBNM-REDEEM-YTD = FUNCTION NUMVAL(REDEEMI)
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO SBNM-NOTICE-SENT-FLAG
           END-IF
           IF ACCRUAL > 0
               COMPUTE SBNM-ACCRUAL-YTD = FUNCTION NUMVAL(ACCRUAI)
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO SBNM-STATUS-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO SBNM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('SBNMST') FROM(SBN-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('SBREVM') MAPSET('SBREVS')
                FROM(SBREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
