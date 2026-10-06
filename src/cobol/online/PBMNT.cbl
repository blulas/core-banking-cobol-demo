      *================================================================*
      * PROGRAM  : PBMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PB66                                                *
      * PURPOSE  : PASSBOOK PRINTING MAINTENANCE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PBSTAT, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-09-19 RJT  ORIGINAL                                       *
      * 1999-11-24 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-12 LMB  UNPOSTED NOTICE TEXT PER LEGAL CHG5021         *
      * 2000-05-26 JLM  PRINTED CUTOFF MOVED PRB0717                   *
      * 2000-09-27 JLM  INTEREST NOTICE TEXT PER LEGAL REQ6956         *
      * 2001-09-08 BHS  PERFORMANCE - PRINTED READ AHEAD INC0113       *
      * 2002-06-20 EWS  UNPOSTED ROUNDING - HALF UP CHG5552            *
      * 2002-03-13 EWS  CARRYFWD CALCULATION CORRECTED REQ1101         *
      * 2002-06-03 DPS  BALANCE CUTOFF MOVED PRB4029                   *
      * 2004-09-08 BHS  PRINTED RERUN PROTECTION INC6646               *
      * 2004-06-06 CRP  NEW UNPOSTED REPORT COLUMNS INC2579            *
      * 2010-12-21 DPS  PRINTED CALCULATION CORRECTED CHG2338          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PB66'.
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
       COPY PBMNTS.
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
           MOVE LOW-VALUES TO PBMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PBMNTM') MAPSET('PBMNTS')
                FROM(PBMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PBMNTM') MAPSET('PBMNTS')
                INTO(PBMNTMI) RESP(WS-RESP) END-EXEC
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
                   MOVE 'UNPOSTED YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO UNPOSTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(UNPOSTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'UNPOSTED YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF INTEREL > 0
               IF FUNCTION TEST-NUMVAL(INTEREI) NOT = 0
                   MOVE 'INTEREST AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO INTEREL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(INTEREI)
                   IF WS-NUM-WORK < 0
                       MOVE 'INTEREST AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF PRINTE2L > 0
               IF FUNCTION TEST-NUMVAL(PRINTE2I) NOT = 0
                   MOVE 'PRINTED YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO PRINTE2L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(PRINTE2I)
                   IF WS-NUM-WORK < 0
                       MOVE 'PRINTED YTD MAY NOT BE NEGATIVE' TO
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
           MOVE PBKM-UNPOSTED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO UNPOSTO
           MOVE PBKM-CYCLE-CNT TO CYCLECO
           MOVE PBKM-INTEREST-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTEREO
           MOVE PBKM-LINE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO LINEYTO
           MOVE PBKM-PRINTED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINTEO
           MOVE PBKM-PAYMENT-CNT TO PAYMENO
           MOVE PBKM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE PBKM-PRINTER-CD TO PRINTE1O
           MOVE PBKM-RETRY-CNT TO RETRYCO
           MOVE PBKM-EXEMPT-FLAG TO EXEMPTO
           MOVE PBKM-PRINTED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINTE2O
           MOVE PBKM-CHANNEL-CD TO CHANNEO
           MOVE PBKM-LAST-MAINT-DATE TO LASTMAO
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
               COMPUTE PBKM-UNPOSTED-YTD = FUNCTION NUMVAL(UNPOSTI)
           END-IF
           IF INTEREL > 0
               COMPUTE PBKM-INTEREST-AMT = FUNCTION NUMVAL(INTEREI)
           END-IF
           IF PRINTE2L > 0
               COMPUTE PBKM-PRINTED-YTD = FUNCTION NUMVAL(PRINTE2I)
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO PBKM-CHANNEL-CD
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
           EXEC CICS SEND MAP('PBMNTM') MAPSET('PBMNTS')
                FROM(PBMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
