      *================================================================*
      * PROGRAM  : SLAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SL26                                                *
      * PURPOSE  : STUDENT LOANS INQUIRY. ENTER VALIDATES AND DISPLAYS,*
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : SLLIM, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-08-02 RJT  ORIGINAL                                       *
      * 1988-04-27 BHS  PERFORMANCE - PAYMENT READ AHEAD REQ0629       *
      * 1990-06-11 DPS  DISBURSE EXCEPTIONS TO OPS QUEUE CHG5325       *
      * 1992-07-28 SKN  DEFERINT NOTICE TEXT PER LEGAL PRB3572         *
      * 1992-12-02 MKL  SUBSIDY RERUN PROTECTION INC5173               *
      * 1995-01-06 SKN  NEW CAPINT REPORT COLUMNS PRB8572              *
      * 2002-09-20 RJT  SUBSIDY STATUS CODES ADDED CHG5304             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SL26'.
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
       COPY SLAPRS.
       COPY SLLIMP.
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
           MOVE LOW-VALUES TO SLAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SLAPRM') MAPSET('SLAPRS')
                FROM(SLAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SLAPRM') MAPSET('SLAPRS')
                INTO(SLAPRMI) RESP(WS-RESP) END-EXEC
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
           IF CAPINTL > 0
               IF FUNCTION TEST-NUMVAL(CAPINTI) NOT = 0
                   MOVE 'CAPINT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CAPINTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CAPINTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CAPINT AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE SLLIM-PARMS
           MOVE STLM-CAPINT-AMT
             TO SLLIM-CAPINT-AMT
           MOVE STLM-DISBURSE-YTD
             TO SLLIM-DISBURSE-YTD
           MOVE STLM-SUBSIDY-YTD
             TO SLLIM-SUBSIDY-YTD
           MOVE STLM-PAYMENT-YTD
             TO SLLIM-PAYMENT-YTD
           MOVE STLM-PRINCIPAL-YTD
             TO SLLIM-PRINCIPAL-YTD
           MOVE STLM-PRINCIPAL-AMT
             TO SLLIM-PRINCIPAL-AMT
           MOVE STLM-MATURITY-DATE
             TO SLLIM-MATURITY-DATE
           MOVE STLM-CLOSE-DATE
             TO SLLIM-CLOSE-DATE
           MOVE STLM-LAST-STMT-DATE
             TO SLLIM-LAST-STMT-DATE
           MOVE STLM-EXPIRY-DATE
             TO SLLIM-EXPIRY-DATE
           MOVE STLM-LOAN-TYPE-CD
             TO SLLIM-LOAN-TYPE-CD
           MOVE STLM-SOURCE-CD
             TO SLLIM-SOURCE-CD
           MOVE STLM-REGION-CD
             TO SLLIM-REGION-CD
           MOVE STLM-CYCLE-CD
             TO SLLIM-CYCLE-CD
           MOVE STLM-NOTICE-SENT-FLAG
             TO SLLIM-NOTICE-SENT-FLAG
           MOVE STLM-HOLD-FLAG
             TO SLLIM-HOLD-FLAG
           MOVE STLM-REVIEW-FLAG
             TO SLLIM-REVIEW-FLAG
           MOVE STLM-PAYMENT-CNT
             TO SLLIM-PAYMENT-CNT
           MOVE STLM-ITEM-CNT
             TO SLLIM-ITEM-CNT
           MOVE STLM-DISCOUNT
             TO SLLIM-DISCOUNT
           MOVE STLM-FEE-PCT
             TO SLLIM-FEE-PCT
           MOVE STLM-CONTACT
             TO SLLIM-CONTACT
           MOVE STLM-NAME
             TO SLLIM-NAME
           EXEC CICS LINK PROGRAM('SLLIM') COMMAREA(SLLIM-PARMS)
                LENGTH(LENGTH OF SLLIM-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SLLIM-RETURN-CODE = 08
               MOVE 'RULE SLLIM DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE STLM-KEY TO KEYO
           MOVE STLM-SUSPEND-FLAG TO SUSPENO
           MOVE STLM-SUBSIDY-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO SUBSIDO
           MOVE STLM-PRINCIPAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINCIO
           MOVE STLM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE STLM-REF-NO TO REFNOO
           MOVE STLM-MATURITY-DATE TO MATURIO
           MOVE STLM-LOAN-TYPE-CD TO LOANTYO
           MOVE STLM-CUST-ID TO CUSTIDO
           MOVE STLM-PREFERRED-FLAG TO PREFERO
           MOVE STLM-OVERRIDE-FLAG TO OVERRIO
           MOVE STLM-PAYMENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAYMENO
           MOVE STLM-CAPINT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CAPINTO
           MOVE STLM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('STLMST') INTO(STL-MASTER-REC)
                RIDFLD(STLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CAPINTL > 0
               COMPUTE STLM-CAPINT-AMT = FUNCTION NUMVAL(CAPINTI)
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
           EXEC CICS SEND MAP('SLAPRM') MAPSET('SLAPRS')
                FROM(SLAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
