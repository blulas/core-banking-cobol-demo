      *================================================================*
      * PROGRAM  : CLHST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CL65                                                *
      * PURPOSE  : COLLECTIONS QUEUE MAINTENANCE. ENTER VALIDATES AND  *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CLRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-26 PQR  ORIGINAL                                       *
      * 1997-08-12 JLM  WRITEOFF CUTOFF MOVED CHG4235                  *
      * 1997-08-17 EWS  FEE CALCULATION CORRECTED CHG9576              *
      * 1997-12-23 JLM  PROMISED EXCEPTIONS TO OPS QUEUE INC2265       *
      * 1999-08-25 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-02-27 CRP  FEE EXCEPTIONS TO OPS QUEUE INC1532            *
      * 2001-04-12 TWH  COLLECTED STATUS CODES ADDED INC9254           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLHST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CL65'.
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
       COPY COLMST.
       COPY CLHSTS.
       COPY CLRISKP.
       COPY DCLCOL.
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
           MOVE LOW-VALUES TO CLHSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CLHSTM') MAPSET('CLHSTS')
                FROM(CLHSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CLHSTM') MAPSET('CLHSTS')
                INTO(CLHSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO COLM-KEY
           EXEC CICS READ FILE('COLMST') INTO(COL-MASTER-REC)
                RIDFLD(COLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF QUEUECL > 0
              AND QUEUECI NOT = 'DL'
              AND QUEUECI NOT = 'ER'
              AND QUEUECI NOT = 'HA'
               MOVE 'QUEUE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO QUEUECL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF NOTICEL > 0 AND NOTICEI NOT = 'Y' AND NOTICEI NOT = 'N'
               MOVE 'NOTICE SENT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF FEEYTDL > 0
               IF FUNCTION TEST-NUMVAL(FEEYTDI) NOT = 0
                   MOVE 'FEE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FEEYTDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FEEYTDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FEE YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF PROMISL > 0
               IF FUNCTION TEST-NUMVAL(PROMISI) NOT = 0
                   MOVE 'PROMISED AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO PROMISL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(PROMISI)
                   IF WS-NUM-WORK < 0
                       MOVE 'PROMISED AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE CLRISK-PARMS
           MOVE COLM-PAST-DUE-AMT
             TO CLRISK-PAST-DUE-AMT
           MOVE COLM-PAST-DUE-YTD
             TO CLRISK-PAST-DUE-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLRISK-COLLECTED-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLRISK-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLRISK-FEE-YTD
           MOVE COLM-PROMISED-YTD
             TO CLRISK-PROMISED-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLRISK-EFFECTIVE-DATE
           MOVE COLM-OPEN-DATE
             TO CLRISK-OPEN-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLRISK-LAST-MAINT-DATE
           MOVE COLM-NOTICE-DATE
             TO CLRISK-NOTICE-DATE
           MOVE COLM-CYCLE-CD
             TO CLRISK-CYCLE-CD
           MOVE COLM-REGION-CD
             TO CLRISK-REGION-CD
           MOVE COLM-ACTION-CD
             TO CLRISK-ACTION-CD
           MOVE COLM-STATUS-CD
             TO CLRISK-STATUS-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLRISK-NOTICE-SENT-FLAG
           MOVE COLM-REVIEW-FLAG
             TO CLRISK-REVIEW-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLRISK-EXEMPT-FLAG
           MOVE COLM-ITEM-CNT
             TO CLRISK-ITEM-CNT
           MOVE COLM-RETRY-CNT
             TO CLRISK-RETRY-CNT
           MOVE COLM-FEE-PCT
             TO CLRISK-FEE-PCT
           MOVE COLM-MARGIN
             TO CLRISK-MARGIN
           MOVE COLM-DESC
             TO CLRISK-DESC
           MOVE COLM-CONTACT
             TO CLRISK-CONTACT
           EXEC CICS LINK PROGRAM('CLRISK') COMMAREA(CLRISK-PARMS)
                LENGTH(LENGTH OF CLRISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CLRISK-RETURN-CODE = 08
               MOVE 'RULE CLRISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE COLM-KEY TO KEYO
           MOVE COLM-CYCLE-CNT TO CYCLECO
           MOVE COLM-QUEUE-CD TO QUEUECO
           MOVE COLM-CONTACT TO CONTACO
           MOVE COLM-PAST-DUE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PASTDUO
           MOVE COLM-RETRY-CNT TO RETRYCO
           MOVE COLM-DESC TO DESCO
           MOVE COLM-PREFERRED-FLAG TO PREFERO
           MOVE COLM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE COLM-NAME TO NAMEO
           MOVE COLM-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEYTDO
           MOVE COLM-NOTICE-CNT TO NOTICE1O
           MOVE COLM-REASON-CD TO REASONO
           MOVE COLM-PROMISED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PROMISO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('COLMST') INTO(COL-MASTER-REC)
                RIDFLD(COLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF QUEUECL > 0
               MOVE QUEUECI TO COLM-QUEUE-CD
           END-IF
           IF NOTICEL > 0
               MOVE NOTICEI TO COLM-NOTICE-SENT-FLAG
           END-IF
           IF FEEYTDL > 0
               COMPUTE COLM-FEE-YTD = FUNCTION NUMVAL(FEEYTDI)
           END-IF
           IF PROMISL > 0
               COMPUTE COLM-PROMISED-AMT = FUNCTION NUMVAL(PROMISI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO COLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('COLMST') FROM(COL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('CLHSTM') MAPSET('CLHSTS')
                FROM(CLHSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
