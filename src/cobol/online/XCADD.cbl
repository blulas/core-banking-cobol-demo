      *================================================================*
      * PROGRAM  : XCADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : XC40                                                *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS INQUIRY. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : XCLIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-22 DPS  ORIGINAL                                       *
      * 1992-10-15 DPS  GOAL NOTICE TEXT PER LEGAL CHG8053             *
      * 1993-01-10 PQR  AUDIT FINDING - GOAL CONTROL TOTALS REQ7379    *
      * 1995-08-26 MKL  AUDIT FINDING - WEEKLY CONTROL TOTALS PRB5159  *
      * 1995-12-07 EWS  ACCRUED CUTOFF MOVED REQ5290                   *
      * 1999-04-04 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-10-15 EWS  GOAL TABLE EXPANDED CHG2471                    *
      * 2001-09-03 TWH  PERFORMANCE - CONTRIB READ AHEAD INC1367       *
      * 2001-08-04 JLM  PENALTY ROUNDING - HALF UP PRB3638             *
      * 2002-01-09 SKN  PAYOUT LIMIT REVISED REQ8509                   *
      * 2003-04-09 CRP  WEEKLY CALCULATION CORRECTED INC3624           *
      * 2004-06-19 DPS  NEW WEEKLY REPORT COLUMNS REQ4691              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'XC40'.
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
       COPY XCLMST.
       COPY XCADDS.
       COPY XCLIMP.
       COPY DCLXCL.
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
           MOVE LOW-VALUES TO XCADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('XCADDM') MAPSET('XCADDS')
                FROM(XCADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('XCADDM') MAPSET('XCADDS')
                INTO(XCADDMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO XCLM-KEY
           EXEC CICS READ FILE('XCLMST') INTO(XCL-MASTER-REC)
                RIDFLD(XCLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE XCLIM-PARMS
           MOVE XCLM-PAYOUT-YTD
             TO XCLIM-PAYOUT-YTD
           MOVE XCLM-ACCRUED-AMT
             TO XCLIM-ACCRUED-AMT
           MOVE XCLM-GOAL-AMT
             TO XCLIM-GOAL-AMT
           MOVE XCLM-PAYOUT-AMT
             TO XCLIM-PAYOUT-AMT
           MOVE XCLM-WEEKLY-AMT
             TO XCLIM-WEEKLY-AMT
           MOVE XCLM-PENALTY-YTD
             TO XCLIM-PENALTY-YTD
           MOVE XCLM-NOTICE-DATE
             TO XCLIM-NOTICE-DATE
           MOVE XCLM-OPEN-DATE
             TO XCLIM-OPEN-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCLIM-LAST-PAY-DATE
           MOVE XCLM-LAST-STMT-DATE
             TO XCLIM-LAST-STMT-DATE
           MOVE XCLM-REGION-CD
             TO XCLIM-REGION-CD
           MOVE XCLM-PLAN-CD
             TO XCLIM-PLAN-CD
           MOVE XCLM-CYCLE-CD
             TO XCLIM-CYCLE-CD
           MOVE XCLM-REASON-CD
             TO XCLIM-REASON-CD
           MOVE XCLM-SUSPEND-FLAG
             TO XCLIM-SUSPEND-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCLIM-STOP-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCLIM-PREFERRED-FLAG
           MOVE XCLM-PAYMENT-CNT
             TO XCLIM-PAYMENT-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCLIM-CYCLE-CNT
           MOVE XCLM-DISCOUNT
             TO XCLIM-DISCOUNT
           MOVE XCLM-FEE-PCT
             TO XCLIM-FEE-PCT
           MOVE XCLM-REF-NO
             TO XCLIM-REF-NO
           MOVE XCLM-ADDR
             TO XCLIM-ADDR
           EXEC CICS LINK PROGRAM('XCLIM') COMMAREA(XCLIM-PARMS)
                LENGTH(LENGTH OF XCLIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF XCLIM-RETURN-CODE = 08
               MOVE 'RULE XCLIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE XCLM-KEY TO KEYO
           MOVE XCLM-NAME TO NAMEO
           MOVE XCLM-ADDR TO ADDRO
           MOVE XCLM-EXPIRY-DATE TO EXPIRYO
           MOVE XCLM-MISSED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO MISSEDO
           MOVE XCLM-SUSPEND-FLAG TO SUSPENO
           MOVE XCLM-LAST-ACT-DATE TO LASTACO
           MOVE XCLM-CUST-ID TO CUSTIDO
           MOVE XCLM-GOAL-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO GOALAMO
           MOVE XCLM-REASON-CD TO REASONO
           MOVE XCLM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE XCLM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE XCLM-TYPE-CD TO TYPECDO
           MOVE XCLM-ACCRUED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ACCRUEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('XCLMST') INTO(XCL-MASTER-REC)
                RIDFLD(XCLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           EXEC CICS REWRITE FILE('XCLMST') FROM(XCL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('XCADDM') MAPSET('XCADDS')
                FROM(XCADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
