      *================================================================*
      * PROGRAM  : MFOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : MF18                                                *
      * PURPOSE  : MICROFICHE STATEMENTS REVIEW. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : MFLIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-11-12 EWS  ORIGINAL                                       *
      * 1999-03-20 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-01 RJT  AUDIT FINDING - POSTAGE CONTROL TOTALS INC5319 *
      * 2005-08-20 PQR  PERFORMANCE - POSTAGE READ AHEAD PRB4360       *
      * 2007-09-12 JLM  RESEARCH LIMIT REVISED CHG6660                 *
      * 2008-05-28 BHS  RESEARCH LIMIT REVISED REQ0703                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'MF18'.
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
       COPY MFOVRS.
       COPY MFLIMP.
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
           MOVE LOW-VALUES TO MFOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('MFOVRM') MAPSET('MFOVRS')
                FROM(MFOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('MFOVRM') MAPSET('MFOVRS')
                INTO(MFOVRMI) RESP(WS-RESP) END-EXEC
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
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF FICHETL > 0
              AND FICHETI NOT = 'AW'
              AND FICHETI NOT = 'JM'
              AND FICHETI NOT = 'KV'
               MOVE 'FICHE TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO FICHETL
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
           INITIALIZE MFLIM-PARMS
           MOVE MFCM-COPY-FEE-YTD
             TO MFLIM-COPY-FEE-YTD
           MOVE MFCM-STORAGE-AMT
             TO MFLIM-STORAGE-AMT
           MOVE MFCM-STORAGE-YTD
             TO MFLIM-STORAGE-YTD
           MOVE MFCM-RESEARCH-YTD
             TO MFLIM-RESEARCH-YTD
           MOVE MFCM-POSTAGE-AMT
             TO MFLIM-POSTAGE-AMT
           MOVE MFCM-POSTAGE-YTD
             TO MFLIM-POSTAGE-YTD
           MOVE MFCM-CLOSE-DATE
             TO MFLIM-CLOSE-DATE
           MOVE MFCM-EXPIRY-DATE
             TO MFLIM-EXPIRY-DATE
           MOVE MFCM-LAST-PAY-DATE
             TO MFLIM-LAST-PAY-DATE
           MOVE MFCM-OPEN-DATE
             TO MFLIM-OPEN-DATE
           MOVE MFCM-TYPE-CD
             TO MFLIM-TYPE-CD
           MOVE MFCM-RETENTION-CD
             TO MFLIM-RETENTION-CD
           MOVE MFCM-CHANNEL-CD
             TO MFLIM-CHANNEL-CD
           MOVE MFCM-BRANCH-CD
             TO MFLIM-BRANCH-CD
           MOVE MFCM-REVIEW-FLAG
             TO MFLIM-REVIEW-FLAG
           MOVE MFCM-STOP-FLAG
             TO MFLIM-STOP-FLAG
           MOVE MFCM-NOTICE-SENT-FLAG
             TO MFLIM-NOTICE-SENT-FLAG
           MOVE MFCM-CYCLE-CNT
             TO MFLIM-CYCLE-CNT
           MOVE MFCM-PAYMENT-CNT
             TO MFLIM-PAYMENT-CNT
           MOVE MFCM-DISCOUNT
             TO MFLIM-DISCOUNT
           MOVE MFCM-MARGIN
             TO MFLIM-MARGIN
           MOVE MFCM-ADDR
             TO MFLIM-ADDR
           MOVE MFCM-CONTACT
             TO MFLIM-CONTACT
           EXEC CICS LINK PROGRAM('MFLIM') COMMAREA(MFLIM-PARMS)
                LENGTH(LENGTH OF MFLIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF MFLIM-RETURN-CODE = 08
               MOVE 'RULE MFLIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE MFCM-KEY TO KEYO
           MOVE MFCM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE MFCM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE MFCM-POSTAGE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO POSTAGO
           MOVE MFCM-STORAGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAGO
           MOVE MFCM-OPEN-DATE TO OPENDAO
           MOVE MFCM-FICHE-TYPE-CD TO FICHETO
           MOVE MFCM-ADDR TO ADDRO
           MOVE MFCM-CUST-ID TO CUSTIDO
           MOVE MFCM-TYPE-CD TO TYPECDO
           MOVE MFCM-EXPIRY-DATE TO EXPIRYO
           MOVE MFCM-LAST-PAY-DATE TO LASTPAO
           MOVE MFCM-COPY-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COPYFEO
           MOVE MFCM-CONTACT TO CONTACO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('MFCMST') INTO(MFC-MASTER-REC)
                RIDFLD(MFCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO MFCM-NOTICE-SENT-FLAG
           END-IF
           IF FICHETL > 0
               MOVE FICHETI TO MFCM-FICHE-TYPE-CD
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
           EXEC CICS SEND MAP('MFOVRM') MAPSET('MFOVRS')
                FROM(MFOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
