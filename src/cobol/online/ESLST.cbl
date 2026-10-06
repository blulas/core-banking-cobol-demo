      *================================================================*
      * PROGRAM  : ESLST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : ES84                                                *
      * PURPOSE  : MORTGAGE ESCROW REVIEW. ENTER VALIDATES AND         *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : ESRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-07-04 BHS  ORIGINAL                                       *
      * 1992-08-09 CRP  TAX CUTOFF MOVED INC7383                       *
      * 1993-12-28 CRP  CUSHION LIMIT REVISED PRB8649                  *
      * 1995-07-11 GAF  SHORTAGE EXCEPTIONS TO OPS QUEUE REQ9572       *
      * 1996-05-25 GAF  CUSHION NOTICE TEXT PER LEGAL CHG8867          *
      * 2000-12-10 RJT  CUSHION LIMIT REVISED INC7663                  *
      * 2003-03-02 MKL  DISBURSE ROUNDING - HALF UP PRB8345            *
      * 2003-05-03 GAF  TAX FEE SCHEDULE UPDATE PRB0954                *
      * 2006-03-18 GAF  AUDIT FINDING - SHORTAGE CONTROL TOTALS CHG8130*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESLST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'ES84'.
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
       COPY ESCMST.
       COPY ESLSTS.
       COPY ESRISKP.
       COPY DCLESC.
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
           MOVE LOW-VALUES TO ESLSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('ESLSTM') MAPSET('ESLSTS')
                FROM(ESLSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('ESLSTM') MAPSET('ESLSTS')
                INTO(ESLSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO ESCM-KEY
           EXEC CICS READ FILE('ESCMST') INTO(ESC-MASTER-REC)
                RIDFLD(ESCM-KEY) RESP(WS-RESP) END-EXEC
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
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF TAXYTDL > 0
               IF FUNCTION TEST-NUMVAL(TAXYTDI) NOT = 0
                   MOVE 'TAX YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO TAXYTDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(TAXYTDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'TAX YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
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
           INITIALIZE ESRISK-PARMS
           MOVE ESCM-DISBURSE-YTD
             TO ESRISK-DISBURSE-YTD
           MOVE ESCM-TAX-YTD
             TO ESRISK-TAX-YTD
           MOVE ESCM-SHORTAGE-AMT
             TO ESRISK-SHORTAGE-AMT
           MOVE ESCM-SHORTAGE-YTD
             TO ESRISK-SHORTAGE-YTD
           MOVE ESCM-INSURANCE-AMT
             TO ESRISK-INSURANCE-AMT
           MOVE ESCM-INSURANCE-YTD
             TO ESRISK-INSURANCE-YTD
           MOVE ESCM-LAST-PAY-DATE
             TO ESRISK-LAST-PAY-DATE
           MOVE ESCM-NEXT-REVIEW-DATE
             TO ESRISK-NEXT-REVIEW-DATE
           MOVE ESCM-NOTICE-DATE
             TO ESRISK-NOTICE-DATE
           MOVE ESCM-LAST-ACT-DATE
             TO ESRISK-LAST-ACT-DATE
           MOVE ESCM-REASON-CD
             TO ESRISK-REASON-CD
           MOVE ESCM-REGION-CD
             TO ESRISK-REGION-CD
           MOVE ESCM-TYPE-CD
             TO ESRISK-TYPE-CD
           MOVE ESCM-STATUS-CD
             TO ESRISK-STATUS-CD
           MOVE ESCM-HOLD-FLAG
             TO ESRISK-HOLD-FLAG
           MOVE ESCM-STOP-FLAG
             TO ESRISK-STOP-FLAG
           MOVE ESCM-PREFERRED-FLAG
             TO ESRISK-PREFERRED-FLAG
           MOVE ESCM-CYCLE-CNT
             TO ESRISK-CYCLE-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESRISK-EXCEPT-CNT
           MOVE ESCM-RATE
             TO ESRISK-RATE
           MOVE ESCM-FEE-PCT
             TO ESRISK-FEE-PCT
           MOVE ESCM-NAME
             TO ESRISK-NAME
           MOVE ESCM-DESC
             TO ESRISK-DESC
           EXEC CICS LINK PROGRAM('ESRISK') COMMAREA(ESRISK-PARMS)
                LENGTH(LENGTH OF ESRISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF ESRISK-RETURN-CODE = 08
               MOVE 'RULE ESRISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ESCM-KEY TO KEYO
           MOVE ESCM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE ESCM-OVERRIDE-FLAG TO OVERRIO
           MOVE ESCM-INSURANCE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INSURAO
           MOVE ESCM-REF-NO TO REFNOO
           MOVE ESCM-RETRY-CNT TO RETRYCO
           MOVE ESCM-NOTICE-DATE TO NOTICE1O
           MOVE ESCM-REGION-CD TO REGIONO
           MOVE ESCM-NEXT-DUE-DATE TO NEXTDUO
           MOVE ESCM-PREFERRED-FLAG TO PREFERO
           MOVE ESCM-TAX-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO TAXYTDO
           MOVE ESCM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE ESCM-PAYMENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMENO
           MOVE ESCM-SHORTAGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO SHORTAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ESCMST') INTO(ESC-MASTER-REC)
                RIDFLD(ESCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO ESCM-NOTICE-SENT-FLAG
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO ESCM-OVERRIDE-FLAG
           END-IF
           IF TAXYTDL > 0
               COMPUTE ESCM-TAX-YTD = FUNCTION NUMVAL(TAXYTDI)
           END-IF
           EXEC CICS REWRITE FILE('ESCMST') FROM(ESC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('ESLSTM') MAPSET('ESLSTS')
                FROM(ESLSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
