      *================================================================*
      * PROGRAM  : PBOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PB99                                                *
      * PURPOSE  : PASSBOOK PRINTING HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PBPRC, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-03 MKL  ORIGINAL                                       *
      * 2000-10-23 EWS  PRINTED LIMIT REVISED REQ1863                  *
      * 2000-04-10 EWS  CARRYFWD LIMIT REVISED CHG7662                 *
      * 2001-05-08 PQR  UNPOSTED RERUN PROTECTION INC8832              *
      * 2009-10-14 DPS  UNPOSTED FEE SCHEDULE UPDATE CHG3297           *
      * 2010-02-04 PQR  PRINTED FEE SCHEDULE UPDATE CHG1732            *
      * 2010-12-17 MKL  INTEREST EXCEPTIONS TO OPS QUEUE PRB9018       *
      * 2012-03-27 SKN  BALANCE FEE SCHEDULE UPDATE REQ8403            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PB99'.
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
       COPY PBOVRS.
       COPY PBPRCP.
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
           MOVE LOW-VALUES TO PBOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PBOVRM') MAPSET('PBOVRS')
                FROM(PBOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PBOVRM') MAPSET('PBOVRS')
                INTO(PBOVRMI) RESP(WS-RESP) END-EXEC
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
           IF BOOKTYL > 0
              AND BOOKTYI NOT = 'CH'
              AND BOOKTYI NOT = 'HG'
              AND BOOKTYI NOT = 'KB'
               MOVE 'BOOK TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO BOOKTYL
               MOVE 'Y' TO WS-ERROR-FLAG
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
           INITIALIZE PBPRC-PARMS
           MOVE PBKM-PRINTED-AMT
             TO PBPRC-PRINTED-AMT
           MOVE PBKM-LINE-YTD
             TO PBPRC-LINE-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBPRC-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBPRC-CARRYFWD-YTD
           MOVE PBKM-INTEREST-AMT
             TO PBPRC-INTEREST-AMT
           MOVE PBKM-INTEREST-YTD
             TO PBPRC-INTEREST-YTD
           MOVE PBKM-MATURITY-DATE
             TO PBPRC-MATURITY-DATE
           MOVE PBKM-EXPIRY-DATE
             TO PBPRC-EXPIRY-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBPRC-EFFECTIVE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBPRC-NEXT-REVIEW-DATE
           MOVE PBKM-CHANNEL-CD
             TO PBPRC-CHANNEL-CD
           MOVE PBKM-REGION-CD
             TO PBPRC-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBPRC-SOURCE-CD
           MOVE PBKM-PRINTER-CD
             TO PBPRC-PRINTER-CD
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBPRC-AUTO-RENEW-FLAG
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBPRC-NOTICE-SENT-FLAG
           MOVE PBKM-HOLD-FLAG
             TO PBPRC-HOLD-FLAG
           MOVE PBKM-PAYMENT-CNT
             TO PBPRC-PAYMENT-CNT
           MOVE PBKM-ITEM-CNT
             TO PBPRC-ITEM-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBPRC-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBPRC-MARGIN
           MOVE PBKM-CUST-ID
             TO PBPRC-CUST-ID
           MOVE PBKM-ADDR
             TO PBPRC-ADDR
           EXEC CICS LINK PROGRAM('PBPRC') COMMAREA(PBPRC-PARMS)
                LENGTH(LENGTH OF PBPRC-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PBPRC-RETURN-CODE = 08
               MOVE 'RULE PBPRC DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PBKM-KEY TO KEYO
           MOVE PBKM-NEXT-DUE-DATE TO NEXTDUO
           MOVE PBKM-BOOK-TYPE-CD TO BOOKTYO
           MOVE PBKM-CARRYFWD-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CARRYFO
           MOVE PBKM-EXPIRY-DATE TO EXPIRYO
           MOVE PBKM-RETRY-CNT TO RETRYCO
           MOVE PBKM-MATURITY-DATE TO MATURIO
           MOVE PBKM-CYCLE-CNT TO CYCLECO
           MOVE PBKM-OPEN-DATE TO OPENDAO
           MOVE PBKM-REGION-CD TO REGIONO
           MOVE PBKM-LAST-ACT-DATE TO LASTACO
           MOVE PBKM-CHANNEL-CD TO CHANNEO
           MOVE PBKM-HOLD-FLAG TO HOLDFLO
           MOVE PBKM-CUST-ID TO CUSTIDO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PBKMST') INTO(PBK-MASTER-REC)
                RIDFLD(PBKM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF BOOKTYL > 0
               MOVE BOOKTYI TO PBKM-BOOK-TYPE-CD
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
           EXEC CICS SEND MAP('PBOVRM') MAPSET('PBOVRS')
                FROM(PBOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
