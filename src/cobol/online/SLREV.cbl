      *================================================================*
      * PROGRAM  : SLREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SL74                                                *
      * PURPOSE  : STUDENT LOANS HISTORY. ENTER VALIDATES AND DISPLAYS,*
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : SLCAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-10-11 LMB  ORIGINAL                                       *
      * 1988-01-24 BHS  DEFERINT NOTICE TEXT PER LEGAL INC3195         *
      * 1989-11-28 BHS  AUDIT FINDING - DEFERINT CONTROL TOTALS REQ7827*
      * 1992-04-22 LMB  NEW SUBSIDY REPORT COLUMNS REQ6667             *
      * 1992-03-01 PQR  CAPINT CUTOFF MOVED PRB7645                    *
      * 1993-03-16 BHS  DEFERINT STATUS CODES ADDED CHG4434            *
      * 1996-10-24 PQR  DEFERINT CUTOFF MOVED CHG6226                  *
      * 1997-04-21 LMB  NEW PAYMENT REPORT COLUMNS REQ0179             *
      * 1998-07-15 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-08-20 RJT  DISBURSE STATUS CODES ADDED PRB3161            *
      * 2006-09-22 DPS  DISBURSE STATUS CODES ADDED PRB3396            *
      * 2006-10-05 MKL  CAPINT CALCULATION CORRECTED REQ5290           *
      * 2006-09-19 DPS  PAYMENT STATUS CODES ADDED REQ8570             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SL74'.
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
       COPY STLMST.
       COPY SLREVS.
       COPY SLCALP.
       COPY DCLSTL.
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
           MOVE LOW-VALUES TO SLREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SLREVM') MAPSET('SLREVS')
                FROM(SLREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SLREVM') MAPSET('SLREVS')
                INTO(SLREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO STLM-KEY
           EXEC CICS READ FILE('STLMST') INTO(STL-MASTER-REC)
                RIDFLD(STLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REGIONL > 0
              AND REGIONI NOT = 'C'
              AND REGIONI NOT = 'D'
              AND REGIONI NOT = 'T'
               MOVE 'REGION CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REGIONL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'HY'
              AND STATUSI NOT = 'JZ'
              AND STATUSI NOT = 'RK'
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
           INITIALIZE SLCAL-PARMS
           MOVE STLM-CAPINT-AMT
             TO SLCAL-CAPINT-AMT
           MOVE STLM-SUBSIDY-YTD
             TO SLCAL-SUBSIDY-YTD
           MOVE STLM-PAYMENT-AMT
             TO SLCAL-PAYMENT-AMT
           MOVE STLM-PRINCIPAL-AMT
             TO SLCAL-PRINCIPAL-AMT
           MOVE STLM-DEFERINT-YTD
             TO SLCAL-DEFERINT-YTD
           MOVE STLM-DISBURSE-YTD
             TO SLCAL-DISBURSE-YTD
           MOVE STLM-OPEN-DATE
             TO SLCAL-OPEN-DATE
           MOVE STLM-EXPIRY-DATE
             TO SLCAL-EXPIRY-DATE
           MOVE STLM-LAST-ACT-DATE
             TO SLCAL-LAST-ACT-DATE
           MOVE STLM-LAST-PAY-DATE
             TO SLCAL-LAST-PAY-DATE
           MOVE STLM-CYCLE-CD
             TO SLCAL-CYCLE-CD
           MOVE STLM-DEFER-CODE-CD
             TO SLCAL-DEFER-CODE-CD
           MOVE STLM-SOURCE-CD
             TO SLCAL-SOURCE-CD
           MOVE STLM-REGION-CD
             TO SLCAL-REGION-CD
           MOVE STLM-REVIEW-FLAG
             TO SLCAL-REVIEW-FLAG
           MOVE STLM-NOTICE-SENT-FLAG
             TO SLCAL-NOTICE-SENT-FLAG
           MOVE STLM-OVERRIDE-FLAG
             TO SLCAL-OVERRIDE-FLAG
           MOVE STLM-EXCEPT-CNT
             TO SLCAL-EXCEPT-CNT
           MOVE STLM-PAYMENT-CNT
             TO SLCAL-PAYMENT-CNT
           MOVE STLM-PENALTY-PCT
             TO SLCAL-PENALTY-PCT
           MOVE STLM-FEE-PCT
             TO SLCAL-FEE-PCT
           MOVE STLM-NAME
             TO SLCAL-NAME
           MOVE STLM-ADDR
             TO SLCAL-ADDR
           EXEC CICS LINK PROGRAM('SLCAL') COMMAREA(SLCAL-PARMS)
                LENGTH(LENGTH OF SLCAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SLCAL-RETURN-CODE = 08
               MOVE 'RULE SLCAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE STLM-KEY TO KEYO
           MOVE STLM-OPEN-DATE TO OPENDAO
           MOVE STLM-CONTACT TO CONTACO
           MOVE STLM-LAST-MAINT-DATE TO LASTMAO
           MOVE STLM-ITEM-CNT TO ITEMCNO
           MOVE STLM-PREFERRED-FLAG TO PREFERO
           MOVE STLM-EXCEPT-CNT TO EXCEPTO
           MOVE STLM-REGION-CD TO REGIONO
           MOVE STLM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE STLM-LAST-ACT-DATE TO LASTACO
           MOVE STLM-PRINCIPAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINCIO
           MOVE STLM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE STLM-STATUS-CD TO STATUSO
           MOVE STLM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('STLMST') INTO(STL-MASTER-REC)
                RIDFLD(STLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REGIONL > 0
               MOVE REGIONI TO STLM-REGION-CD
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO STLM-NOTICE-SENT-FLAG
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO STLM-STATUS-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO STLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('STLMST') FROM(STL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('SLREVM') MAPSET('SLREVS')
                FROM(SLREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
