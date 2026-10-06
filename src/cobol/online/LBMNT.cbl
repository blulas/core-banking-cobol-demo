      *================================================================*
      * PROGRAM  : LBMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : LB75                                                *
      * PURPOSE  : LOCKBOX REMITTANCE OVERRIDE. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : LBFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-17 BHS  ORIGINAL                                       *
      * 1997-04-08 BHS  PERFORMANCE - DEPOSIT READ AHEAD PRB9920       *
      * 1997-09-08 SKN  DEPOSIT STATUS CODES ADDED REQ4535             *
      * 1997-04-19 EWS  FEE TABLE EXPANDED INC4277                     *
      * 2001-02-14 CRP  EXCEPT NOTICE TEXT PER LEGAL REQ4709           *
      * 2005-09-18 MKL  FEE RERUN PROTECTION INC6780                   *
      * 2006-09-28 LMB  PERFORMANCE - DEPOSIT READ AHEAD REQ6481       *
      * 2009-11-27 EWS  REMIT FEE SCHEDULE UPDATE INC0153              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'LB75'.
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
       COPY LBXMST.
       COPY LBMNTS.
       COPY LBFEEP.
       COPY DCLLBX.
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
           MOVE LOW-VALUES TO LBMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('LBMNTM') MAPSET('LBMNTS')
                FROM(LBMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('LBMNTM') MAPSET('LBMNTS')
                INTO(LBMNTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO LBXM-KEY
           EXEC CICS READ FILE('LBXMST') INTO(LBX-MASTER-REC)
                RIDFLD(LBXM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF PREFERL > 0 AND PREFERI NOT = 'Y' AND PREFERI NOT = 'N'
               MOVE 'PREFERRED FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF HOLDFLL > 0 AND HOLDFLI NOT = 'Y' AND HOLDFLI NOT = 'N'
               MOVE 'HOLD FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF CASHONL > 0
               IF FUNCTION TEST-NUMVAL(CASHONI) NOT = 0
                   MOVE 'CASH ON HAND AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CASHONL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CASHONI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CASH ON HAND AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE LBFEE-PARMS
           MOVE LBXM-EXCEPT-YTD
             TO LBFEE-EXCEPT-YTD
           MOVE LBXM-DEPOSIT-YTD
             TO LBFEE-DEPOSIT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBFEE-REMIT-YTD
           MOVE LBXM-EXCEPT-AMT
             TO LBFEE-EXCEPT-AMT
           MOVE LBXM-FLOAT-YTD
             TO LBFEE-FLOAT-YTD
           MOVE LBXM-FLOAT-AMT
             TO LBFEE-FLOAT-AMT
           MOVE LBXM-CLOSE-DATE
             TO LBFEE-CLOSE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBFEE-NEXT-DUE-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBFEE-LAST-PAY-DATE
           MOVE LBXM-OPEN-DATE
             TO LBFEE-OPEN-DATE
           MOVE LBXM-LOCKBOX-CD
             TO LBFEE-LOCKBOX-CD
           MOVE LBXM-REASON-CD
             TO LBFEE-REASON-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBFEE-BATCH-TYPE-CD
           MOVE LBXM-CHANNEL-CD
             TO LBFEE-CHANNEL-CD
           MOVE LBXM-STOP-FLAG
             TO LBFEE-STOP-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBFEE-EXEMPT-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBFEE-NOTICE-SENT-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBFEE-PAYMENT-CNT
           MOVE LBXM-ITEM-CNT
             TO LBFEE-ITEM-CNT
           MOVE LBXM-DISCOUNT
             TO LBFEE-DISCOUNT
           MOVE LBXM-PENALTY-PCT
             TO LBFEE-PENALTY-PCT
           MOVE LBXM-NAME
             TO LBFEE-NAME
           MOVE LBXM-ADDR
             TO LBFEE-ADDR
           EXEC CICS LINK PROGRAM('LBFEE') COMMAREA(LBFEE-PARMS)
                LENGTH(LENGTH OF LBFEE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF LBFEE-RETURN-CODE = 08
               MOVE 'RULE LBFEE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE LBXM-KEY TO KEYO
           MOVE LBXM-ADDR TO ADDRO
           MOVE LBXM-DEPOSIT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEPOSIO
           MOVE LBXM-NOTICE-SENT-FLAG TO NOTICEO
           MOVE LBXM-BATCH-TYPE-CD TO BATCHTO
           MOVE LBXM-PREFERRED-FLAG TO PREFERO
           MOVE LBXM-CUST-ID TO CUSTIDO
           MOVE LBXM-MATURITY-DATE TO MATURIO
           MOVE LBXM-HOLD-FLAG TO HOLDFLO
           MOVE LBXM-NEXT-DUE-DATE TO NEXTDUO
           MOVE LBXM-EXCEPT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPTO
           MOVE LBXM-LAST-PAY-DATE TO LASTPAO
           MOVE LBXM-CONTACT TO CONTACO
           MOVE LBXM-CASH-ON-HAND-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASHONO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('LBXMST') INTO(LBX-MASTER-REC)
                RIDFLD(LBXM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF PREFERL > 0
               MOVE PREFERI TO LBXM-PREFERRED-FLAG
           END-IF
           IF HOLDFLL > 0
               MOVE HOLDFLI TO LBXM-HOLD-FLAG
           END-IF
           IF CASHONL > 0
               COMPUTE LBXM-CASH-ON-HAND-AMT = FUNCTION NUMVAL(CASHONI)
           END-IF
           EXEC CICS REWRITE FILE('LBXMST') FROM(LBX-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('LBMNTM') MAPSET('LBMNTS')
                FROM(LBMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
