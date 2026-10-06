      *================================================================*
      * PROGRAM  : MFLST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : MF39                                                *
      * PURPOSE  : MICROFICHE STATEMENTS REVIEW. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : MFRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-03-28 LMB  ORIGINAL                                       *
      * 1998-09-25 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-10 GAF  PERFORMANCE - RESEARCH READ AHEAD PRB8111      *
      * 1998-10-22 BHS  COPY-FEE FEE SCHEDULE UPDATE PRB8814           *
      * 1998-12-25 CRP  STORAGE ROUNDING - HALF UP PRB2860             *
      * 1998-12-04 TWH  STORAGE ROUNDING - HALF UP INC8550             *
      * 2001-02-14 CRP  POSTAGE NOTICE TEXT PER LEGAL PRB4843          *
      * 2001-05-09 RJT  STORAGE NOTICE TEXT PER LEGAL PRB2053          *
      * 2002-03-03 RJT  COPY-FEE TABLE EXPANDED CHG6485                *
      * 2003-01-14 CRP  STORAGE NOTICE TEXT PER LEGAL CHG4428          *
      * 2003-07-20 TWH  RESEARCH CUTOFF MOVED INC1280                  *
      * 2004-09-10 BHS  RESEARCH FEE SCHEDULE UPDATE REQ3616           *
      * 2004-01-03 PQR  POSTAGE RERUN PROTECTION INC0641               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFLST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'MF39'.
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
       COPY MFCMST.
       COPY MFLSTS.
       COPY MFRISKP.
       COPY DCLMFC.
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
           MOVE LOW-VALUES TO MFLSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('MFLSTM') MAPSET('MFLSTS')
                FROM(MFLSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('MFLSTM') MAPSET('MFLSTS')
                INTO(MFLSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO MFCM-KEY
           EXEC CICS READ FILE('MFCMST') INTO(MFC-MASTER-REC)
                RIDFLD(MFCM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STORAGL > 0
               IF FUNCTION TEST-NUMVAL(STORAGI) NOT = 0
                   MOVE 'STORAGE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO STORAGL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(STORAGI)
                   IF WS-NUM-WORK < 0
                       MOVE 'STORAGE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF AUTOREL > 0 AND AUTOREI NOT = 'Y' AND AUTOREI NOT = 'N'
               MOVE 'AUTO RENEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF EXEMPTL > 0 AND EXEMPTI NOT = 'Y' AND EXEMPTI NOT = 'N'
               MOVE 'EXEMPT FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE MFRISK-PARMS
           MOVE MFCM-COPY-FEE-YTD
             TO MFRISK-COPY-FEE-YTD
           MOVE MFCM-COPY-FEE-AMT
             TO MFRISK-COPY-FEE-AMT
           MOVE MFCM-RESEARCH-YTD
             TO MFRISK-RESEARCH-YTD
           MOVE MFCM-RESEARCH-AMT
             TO MFRISK-RESEARCH-AMT
           MOVE MFCM-STORAGE-AMT
             TO MFRISK-STORAGE-AMT
           MOVE MFCM-STORAGE-YTD
             TO MFRISK-STORAGE-YTD
           MOVE MFCM-NOTICE-DATE
             TO MFRISK-NOTICE-DATE
           MOVE MFCM-LAST-PAY-DATE
             TO MFRISK-LAST-PAY-DATE
           MOVE MFCM-OPEN-DATE
             TO MFRISK-OPEN-DATE
           MOVE MFCM-LAST-MAINT-DATE
             TO MFRISK-LAST-MAINT-DATE
           MOVE MFCM-CHANNEL-CD
             TO MFRISK-CHANNEL-CD
           MOVE MFCM-BRANCH-CD
             TO MFRISK-BRANCH-CD
           MOVE MFCM-RETENTION-CD
             TO MFRISK-RETENTION-CD
           MOVE MFCM-STATUS-CD
             TO MFRISK-STATUS-CD
           MOVE MFCM-REVIEW-FLAG
             TO MFRISK-REVIEW-FLAG
           MOVE MFCM-AUTO-RENEW-FLAG
             TO MFRISK-AUTO-RENEW-FLAG
           MOVE MFCM-STOP-FLAG
             TO MFRISK-STOP-FLAG
           MOVE MFCM-CYCLE-CNT
             TO MFRISK-CYCLE-CNT
           MOVE MFCM-NOTICE-CNT
             TO MFRISK-NOTICE-CNT
           MOVE MFCM-MARGIN
             TO MFRISK-MARGIN
           MOVE MFCM-FEE-PCT
             TO MFRISK-FEE-PCT
           MOVE MFCM-NAME
             TO MFRISK-NAME
           MOVE MFCM-ADDR
             TO MFRISK-ADDR
           EXEC CICS LINK PROGRAM('MFRISK') COMMAREA(MFRISK-PARMS)
                LENGTH(LENGTH OF MFRISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF MFRISK-RETURN-CODE = 08
               MOVE 'RULE MFRISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE MFCM-KEY TO KEYO
           MOVE MFCM-LAST-MAINT-DATE TO LASTMAO
           MOVE MFCM-CYCLE-CNT TO CYCLECO
           MOVE MFCM-EXPIRY-DATE TO EXPIRYO
           MOVE MFCM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE MFCM-MATURITY-DATE TO MATURIO
           MOVE MFCM-STORAGE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAGO
           MOVE MFCM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE MFCM-DESC TO DESCO
           MOVE MFCM-LAST-PAY-DATE TO LASTPAO
           MOVE MFCM-STOP-FLAG TO STOPFLO
           MOVE MFCM-RESEARCH-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO RESEARO
           MOVE MFCM-STORAGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAG1O
           MOVE MFCM-EXEMPT-FLAG TO EXEMPTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('MFCMST') INTO(MFC-MASTER-REC)
                RIDFLD(MFCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STORAGL > 0
               COMPUTE MFCM-STORAGE-YTD = FUNCTION NUMVAL(STORAGI)
           END-IF
           IF AUTOREL > 0
               MOVE AUTOREI TO MFCM-AUTO-RENEW-FLAG
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO MFCM-EXEMPT-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO MFCM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('MFCMST') FROM(MFC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('MFLSTM') MAPSET('MFLSTS')
                FROM(MFLSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
