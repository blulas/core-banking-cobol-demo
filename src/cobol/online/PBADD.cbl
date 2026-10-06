      *================================================================*
      * PROGRAM  : PBADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PB20                                                *
      * PURPOSE  : PASSBOOK PRINTING HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PBSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-06-21 BHS  ORIGINAL                                       *
      * 1999-03-07 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-27 GAF  INTEREST ROUNDING - HALF UP INC8164            *
      * 2000-07-26 CRP  NEW INTEREST REPORT COLUMNS CHG3473            *
      * 2002-06-22 EWS  LINE RERUN PROTECTION CHG8301                  *
      * 2003-02-21 EWS  NEW BALANCE REPORT COLUMNS CHG7193             *
      * 2004-06-05 SKN  UNPOSTED STATUS CODES ADDED CHG8558            *
      * 2005-12-09 DPS  PRINTED TABLE EXPANDED INC8176                 *
      * 2006-02-11 JLM  NEW LINE REPORT COLUMNS PRB7857                *
      * 2011-07-24 RJT  UNPOSTED CALCULATION CORRECTED REQ2121         *
      * 2012-03-08 JLM  AUDIT FINDING - UNPOSTED CONTROL TOTALS PRB1045*
      * 2012-02-15 LMB  UNPOSTED LIMIT REVISED REQ1247                 *
      * 2013-11-10 JLM  PRINTED CUTOFF MOVED INC5238                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PB20'.
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
       COPY PBKMST.
       COPY PBADDS.
       COPY PBSTATP.
       COPY DCLPBK.
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
           MOVE LOW-VALUES TO PBADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PBADDM') MAPSET('PBADDS')
                FROM(PBADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PBADDM') MAPSET('PBADDS')
                INTO(PBADDMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO PBKM-KEY
           EXEC CICS READ FILE('PBKMST') INTO(PBK-MASTER-REC)
                RIDFLD(PBKM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF UNPOSTL > 0
               IF FUNCTION TEST-NUMVAL(UNPOSTI) NOT = 0
                   MOVE 'UNPOSTED AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO UNPOSTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(UNPOSTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'UNPOSTED AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CHANNEL > 0
              AND CHANNEI NOT = 'B'
              AND CHANNEI NOT = 'D'
              AND CHANNEI NOT = 'H'
               MOVE 'CHANNEL CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CHANNEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF BRANCHL > 0
              AND BRANCHI NOT = 'DF'
              AND BRANCHI NOT = 'NV'
              AND BRANCHI NOT = 'PP'
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
           INITIALIZE PBSTAT-PARMS
           MOVE PBKM-UNPOSTED-YTD
             TO PBSTAT-UNPOSTED-YTD
           MOVE PBKM-BALANCE-AMT
             TO PBSTAT-BALANCE-AMT
           MOVE PBKM-LINE-YTD
             TO PBSTAT-LINE-YTD
           MOVE PBKM-CARRYFWD-YTD
             TO PBSTAT-CARRYFWD-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBSTAT-UNPOSTED-AMT
           MOVE PBKM-LINE-AMT
             TO PBSTAT-LINE-AMT
           MOVE PBKM-CLOSE-DATE
             TO PBSTAT-CLOSE-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBSTAT-EFFECTIVE-DATE
           MOVE PBKM-NEXT-DUE-DATE
             TO PBSTAT-NEXT-DUE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBSTAT-NEXT-REVIEW-DATE
           MOVE PBKM-BOOK-TYPE-CD
             TO PBSTAT-BOOK-TYPE-CD
           MOVE PBKM-PRINTER-CD
             TO PBSTAT-PRINTER-CD
           MOVE PBKM-REGION-CD
             TO PBSTAT-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBSTAT-SOURCE-CD
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBSTAT-NOTICE-SENT-FLAG
           MOVE PBKM-REVIEW-FLAG
             TO PBSTAT-REVIEW-FLAG
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBSTAT-AUTO-RENEW-FLAG
           MOVE PBKM-RETRY-CNT
             TO PBSTAT-RETRY-CNT
           MOVE PBKM-CYCLE-CNT
             TO PBSTAT-CYCLE-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBSTAT-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBSTAT-MARGIN
           MOVE PBKM-CONTACT
             TO PBSTAT-CONTACT
           MOVE PBKM-CUST-ID
             TO PBSTAT-CUST-ID
           EXEC CICS LINK PROGRAM('PBSTAT') COMMAREA(PBSTAT-PARMS)
                LENGTH(LENGTH OF PBSTAT-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PBSTAT-RETURN-CODE = 08
               MOVE 'RULE PBSTAT DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PBKM-KEY TO KEYO
           MOVE PBKM-CUST-ID TO CUSTIDO
           MOVE PBKM-MATURITY-DATE TO MATURIO
           MOVE PBKM-CYCLE-CNT TO CYCLECO
           MOVE PBKM-UNPOSTED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO UNPOSTO
           MOVE PBKM-INTEREST-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTEREO
           MOVE PBKM-CLOSE-DATE TO CLOSEDO
           MOVE PBKM-CHANNEL-CD TO CHANNEO
           MOVE PBKM-REGION-CD TO REGIONO
           MOVE PBKM-BRANCH-CD TO BRANCHO
           MOVE PBKM-UNPOSTED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO UNPOST1O
           MOVE PBKM-CARRYFWD-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CARRYFO
           MOVE PBKM-LAST-ACT-DATE TO LASTACO
           MOVE PBKM-SOURCE-CD TO SOURCEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PBKMST') INTO(PBK-MASTER-REC)
                RIDFLD(PBKM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF UNPOSTL > 0
               COMPUTE PBKM-UNPOSTED-AMT = FUNCTION NUMVAL(UNPOSTI)
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO PBKM-CHANNEL-CD
           END-IF
           IF BRANCHL > 0
               MOVE BRANCHI TO PBKM-BRANCH-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO PBKM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('PBKMST') FROM(PBK-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('PBADDM') MAPSET('PBADDS')
                FROM(PBADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
