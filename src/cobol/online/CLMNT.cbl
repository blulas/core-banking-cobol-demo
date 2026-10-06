      *================================================================*
      * PROGRAM  : CLMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CL16                                                *
      * PURPOSE  : COLLECTIONS QUEUE HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CLELG, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-13 EWS  ORIGINAL                                       *
      * 1997-08-13 TWH  PROMISED STATUS CODES ADDED CHG5975            *
      * 1997-06-11 TWH  FEE ROUNDING - HALF UP PRB4565                 *
      * 1999-06-15 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-10 SKN  FEE RERUN PROTECTION CHG2216                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CL16'.
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
       COPY CLMNTS.
       COPY CLELGP.
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
           MOVE LOW-VALUES TO CLMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CLMNTM') MAPSET('CLMNTS')
                FROM(CLMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CLMNTM') MAPSET('CLMNTS')
                INTO(CLMNTMI) RESP(WS-RESP) END-EXEC
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
           IF REVIEWL > 0 AND REVIEWI NOT = 'Y' AND REVIEWI NOT = 'N'
               MOVE 'REVIEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'PS'
              AND STATUSI NOT = 'UR'
              AND STATUSI NOT = 'XM'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE CLELG-PARMS
           MOVE COLM-WRITEOFF-AMT
             TO CLELG-WRITEOFF-AMT
           MOVE COLM-COLLECTED-AMT
             TO CLELG-COLLECTED-AMT
           MOVE COLM-FEE-AMT
             TO CLELG-FEE-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLELG-WRITEOFF-YTD
           MOVE COLM-PAST-DUE-AMT
             TO CLELG-PAST-DUE-AMT
           MOVE COLM-FEE-YTD
             TO CLELG-FEE-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLELG-EFFECTIVE-DATE
           MOVE COLM-EXPIRY-DATE
             TO CLELG-EXPIRY-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLELG-LAST-MAINT-DATE
           MOVE COLM-NEXT-DUE-DATE
             TO CLELG-NEXT-DUE-DATE
           MOVE COLM-CYCLE-CD
             TO CLELG-CYCLE-CD
           MOVE COLM-QUEUE-CD
             TO CLELG-QUEUE-CD
           MOVE COLM-STATUS-CD
             TO CLELG-STATUS-CD
           MOVE COLM-REASON-CD
             TO CLELG-REASON-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLELG-NOTICE-SENT-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLELG-EXEMPT-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLELG-OVERRIDE-FLAG
           MOVE COLM-NOTICE-CNT
             TO CLELG-NOTICE-CNT
           MOVE COLM-ITEM-CNT
             TO CLELG-ITEM-CNT
           MOVE COLM-MARGIN
             TO CLELG-MARGIN
           MOVE COLM-FEE-PCT
             TO CLELG-FEE-PCT
           MOVE COLM-DESC
             TO CLELG-DESC
           MOVE COLM-ADDR
             TO CLELG-ADDR
           EXEC CICS LINK PROGRAM('CLELG') COMMAREA(CLELG-PARMS)
                LENGTH(LENGTH OF CLELG-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CLELG-RETURN-CODE = 08
               MOVE 'RULE CLELG DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE COLM-KEY TO KEYO
           MOVE COLM-LAST-PAY-DATE TO LASTPAO
           MOVE COLM-REVIEW-FLAG TO REVIEWO
           MOVE COLM-PREFERRED-FLAG TO PREFERO
           MOVE COLM-STATUS-CD TO STATUSO
           MOVE COLM-COLLECTED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO COLLECO
           MOVE COLM-NAME TO NAMEO
           MOVE COLM-OVERRIDE-FLAG TO OVERRIO
           MOVE COLM-WRITEOFF-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO WRITEOO
           MOVE COLM-CUST-ID TO CUSTIDO
           MOVE COLM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE COLM-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEYTDO
           MOVE COLM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE COLM-REF-NO TO REFNOO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('COLMST') INTO(COL-MASTER-REC)
                RIDFLD(COLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REVIEWL > 0
               MOVE REVIEWI TO COLM-REVIEW-FLAG
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO COLM-STATUS-CD
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO COLM-OVERRIDE-FLAG
           END-IF
           IF FEEYTDL > 0
               COMPUTE COLM-FEE-YTD = FUNCTION NUMVAL(FEEYTDI)
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
           EXEC CICS SEND MAP('CLMNTM') MAPSET('CLMNTS')
                FROM(CLMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
