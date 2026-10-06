      *================================================================*
      * PROGRAM  : SBINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SB53                                                *
      * PURPOSE  : SAVINGS BOND SERVICES HISTORY. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SBSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-09 BHS  ORIGINAL                                       *
      * 1996-11-09 JLM  TAXABLE CUTOFF MOVED PRB8228                   *
      * 1998-03-10 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-20 SKN  ACCRUAL NOTICE TEXT PER LEGAL CHG3255          *
      * 2000-08-07 TWH  INTEREST TABLE EXPANDED REQ8362                *
      * 2001-07-16 PQR  TAXABLE TABLE EXPANDED CHG0492                 *
      * 2001-07-28 RJT  FACE FEE SCHEDULE UPDATE REQ6581               *
      * 2004-10-03 JLM  ISSUE-PRICE LIMIT REVISED CHG3675              *
      * 2004-01-06 JLM  REDEEM CALCULATION CORRECTED REQ1859           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SB53'.
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
       COPY SBINQS.
       COPY SBSTATP.
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
           MOVE LOW-VALUES TO SBINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SBINQM') MAPSET('SBINQS')
                FROM(SBINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SBINQM') MAPSET('SBINQS')
                INTO(SBINQMI) RESP(WS-RESP) END-EXEC
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
           IF TAXABLL > 0
               IF FUNCTION TEST-NUMVAL(TAXABLI) NOT = 0
                   MOVE 'TAXABLE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO TAXABLL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(TAXABLI)
                   IF WS-NUM-WORK < 0
                       MOVE 'TAXABLE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF ISSUEP1L > 0
               IF FUNCTION TEST-NUMVAL(ISSUEP1I) NOT = 0
                   MOVE 'ISSUE PRICE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ISSUEP1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ISSUEP1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'ISSUE PRICE AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF TAXABL1L > 0
               IF FUNCTION TEST-NUMVAL(TAXABL1I) NOT = 0
                   MOVE 'TAXABLE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO TAXABL1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(TAXABL1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'TAXABLE AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE SBSTAT-PARMS
           MOVE SBNM-ACCRUAL-AMT
             TO SBSTAT-ACCRUAL-AMT
           MOVE SBNM-TAXABLE-YTD
             TO SBSTAT-TAXABLE-YTD
           MOVE SBNM-INTEREST-YTD
             TO SBSTAT-INTEREST-YTD
           MOVE SBNM-FACE-AMT
             TO SBSTAT-FACE-AMT
           MOVE SBNM-REDEEM-AMT
             TO SBSTAT-REDEEM-AMT
           MOVE SBNM-FACE-YTD
             TO SBSTAT-FACE-YTD
           MOVE SBNM-MATURITY-DATE
             TO SBSTAT-MATURITY-DATE
           MOVE SBNM-EFFECTIVE-DATE
             TO SBSTAT-EFFECTIVE-DATE
           MOVE SBNM-NOTICE-DATE
             TO SBSTAT-NOTICE-DATE
           MOVE SBNM-NEXT-DUE-DATE
             TO SBSTAT-NEXT-DUE-DATE
           MOVE SBNM-REASON-CD
             TO SBSTAT-REASON-CD
           MOVE SBNM-SOURCE-CD
             TO SBSTAT-SOURCE-CD
           MOVE SBNM-STATUS-CD
             TO SBSTAT-STATUS-CD
           MOVE SBNM-ISSUE-TYPE-CD
             TO SBSTAT-ISSUE-TYPE-CD
           MOVE SBNM-REVIEW-FLAG
             TO SBSTAT-REVIEW-FLAG
           MOVE SBNM-PREFERRED-FLAG
             TO SBSTAT-PREFERRED-FLAG
           MOVE SBNM-AUTO-RENEW-FLAG
             TO SBSTAT-AUTO-RENEW-FLAG
           MOVE SBNM-EXCEPT-CNT
             TO SBSTAT-EXCEPT-CNT
           MOVE SBNM-RETRY-CNT
             TO SBSTAT-RETRY-CNT
           MOVE SBNM-MARGIN
             TO SBSTAT-MARGIN
           MOVE SBNM-PENALTY-PCT
             TO SBSTAT-PENALTY-PCT
           MOVE SBNM-DESC
             TO SBSTAT-DESC
           MOVE SBNM-NAME
             TO SBSTAT-NAME
           EXEC CICS LINK PROGRAM('SBSTAT') COMMAREA(SBSTAT-PARMS)
                LENGTH(LENGTH OF SBSTAT-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SBSTAT-RETURN-CODE = 08
               MOVE 'RULE SBSTAT DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE SBNM-KEY TO KEYO
           MOVE SBNM-OPEN-DATE TO OPENDAO
           MOVE SBNM-ISSUE-PRICE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEPO
           MOVE SBNM-ADDR TO ADDRO
           MOVE SBNM-MATURITY-DATE TO MATURIO
           MOVE SBNM-TAXABLE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO TAXABLO
           MOVE SBNM-CYCLE-CNT TO CYCLECO
           MOVE SBNM-CUST-ID TO CUSTIDO
           MOVE SBNM-ISSUE-PRICE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEP1O
           MOVE SBNM-TAXABLE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO TAXABL1O
           MOVE SBNM-NAME TO NAMEO
           MOVE SBNM-REF-NO TO REFNOO
           MOVE SBNM-EFFECTIVE-DATE TO EFFECTO
           MOVE SBNM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('SBNMST') INTO(SBN-MASTER-REC)
                RIDFLD(SBNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF TAXABLL > 0
               COMPUTE SBNM-TAXABLE-YTD = FUNCTION NUMVAL(TAXABLI)
           END-IF
           IF ISSUEP1L > 0
               COMPUTE SBNM-ISSUE-PRICE-AMT = FUNCTION NUMVAL(ISSUEP1I)
           END-IF
           IF TAXABL1L > 0
               COMPUTE SBNM-TAXABLE-AMT = FUNCTION NUMVAL(TAXABL1I)
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
           EXEC CICS SEND MAP('SBINQM') MAPSET('SBINQS')
                FROM(SBINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
