      *================================================================*
      * PROGRAM  : PYAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PY13                                                *
      * PURPOSE  : PAYROLL SERVICES OVERRIDE. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PYPEN, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-03-14 TWH  ORIGINAL                                       *
      * 1991-01-08 PQR  AUDIT FINDING - STATE-TAX CONTROL TOTALS REQ585*
      * 1992-11-26 CRP  STATE-TAX STATUS CODES ADDED PRB6336           *
      * 1997-03-08 MKL  NEW DEDUCT REPORT COLUMNS CHG6539              *
      * 1998-06-17 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-04 EWS  STATE-TAX FEE SCHEDULE UPDATE CHG1472          *
      * 2002-07-05 PQR  NEW DEDUCT REPORT COLUMNS CHG4035              *
      * 2005-06-18 EWS  FICA CALCULATION CORRECTED REQ5790             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PY13'.
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
       COPY PRLMST.
       COPY PYAPRS.
       COPY PYPENP.
       COPY DCLPRL.
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
           MOVE LOW-VALUES TO PYAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PYAPRM') MAPSET('PYAPRS')
                FROM(PYAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PYAPRM') MAPSET('PYAPRS')
                INTO(PYAPRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO PRLM-KEY
           EXEC CICS READ FILE('PRLMST') INTO(PRL-MASTER-REC)
                RIDFLD(PRLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FEDTAXL > 0
               IF FUNCTION TEST-NUMVAL(FEDTAXI) NOT = 0
                   MOVE 'FED TAX YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FEDTAXL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FEDTAXI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FED TAX YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF STATETL > 0
               IF FUNCTION TEST-NUMVAL(STATETI) NOT = 0
                   MOVE 'STATE TAX YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO STATETL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(STATETI)
                   IF WS-NUM-WORK < 0
                       MOVE 'STATE TAX YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF DEDUCTL > 0
               IF FUNCTION TEST-NUMVAL(DEDUCTI) NOT = 0
                   MOVE 'DEDUCT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DEDUCTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DEDUCTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'DEDUCT AMT MAY NOT BE NEGATIVE' TO
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
           INITIALIZE PYPEN-PARMS
           MOVE PRLM-NET-AMT
             TO PYPEN-NET-AMT
           MOVE PRLM-FED-TAX-YTD
             TO PYPEN-FED-TAX-YTD
           MOVE PRLM-GROSS-AMT
             TO PYPEN-GROSS-AMT
           MOVE PRLM-DEDUCT-YTD
             TO PYPEN-DEDUCT-YTD
           MOVE PRLM-FICA-YTD
             TO PYPEN-FICA-YTD
           MOVE PRLM-STATE-TAX-YTD
             TO PYPEN-STATE-TAX-YTD
           MOVE PRLM-EFFECTIVE-DATE
             TO PYPEN-EFFECTIVE-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYPEN-CLOSE-DATE
           MOVE PRLM-NEXT-DUE-DATE
             TO PYPEN-NEXT-DUE-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYPEN-EXPIRY-DATE
           MOVE PRLM-SOURCE-CD
             TO PYPEN-SOURCE-CD
           MOVE PRLM-REASON-CD
             TO PYPEN-REASON-CD
           MOVE PRLM-PAY-FREQ-CD
             TO PYPEN-PAY-FREQ-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYPEN-TAX-STATE-CD
           MOVE PRLM-PREFERRED-FLAG
             TO PYPEN-PREFERRED-FLAG
           MOVE PRLM-SUSPEND-FLAG
             TO PYPEN-SUSPEND-FLAG
           MOVE PRLM-STOP-FLAG
             TO PYPEN-STOP-FLAG
           MOVE PRLM-PAYMENT-CNT
             TO PYPEN-PAYMENT-CNT
           MOVE PRLM-CYCLE-CNT
             TO PYPEN-CYCLE-CNT
           MOVE PRLM-MARGIN
             TO PYPEN-MARGIN
           MOVE PRLM-RATE
             TO PYPEN-RATE
           MOVE PRLM-CUST-ID
             TO PYPEN-CUST-ID
           MOVE PRLM-REF-NO
             TO PYPEN-REF-NO
           EXEC CICS LINK PROGRAM('PYPEN') COMMAREA(PYPEN-PARMS)
                LENGTH(LENGTH OF PYPEN-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PYPEN-RETURN-CODE = 08
               MOVE 'RULE PYPEN DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PRLM-KEY TO KEYO
           MOVE PRLM-NEXT-DUE-DATE TO NEXTDUO
           MOVE PRLM-CLOSE-DATE TO CLOSEDO
           MOVE PRLM-CONTACT TO CONTACO
           MOVE PRLM-FICA-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FICAYTO
           MOVE PRLM-FED-TAX-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEDTAXO
           MOVE PRLM-CUST-ID TO CUSTIDO
           MOVE PRLM-STATE-TAX-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STATETO
           MOVE PRLM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE PRLM-EXPIRY-DATE TO EXPIRYO
           MOVE PRLM-DEDUCT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEDUCTO
           MOVE PRLM-SOURCE-CD TO SOURCEO
           MOVE PRLM-DESC TO DESCO
           MOVE PRLM-GROSS-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO GROSSAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PRLMST') INTO(PRL-MASTER-REC)
                RIDFLD(PRLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FEDTAXL > 0
               COMPUTE PRLM-FED-TAX-YTD = FUNCTION NUMVAL(FEDTAXI)
           END-IF
           IF STATETL > 0
               COMPUTE PRLM-STATE-TAX-YTD = FUNCTION NUMVAL(STATETI)
           END-IF
           IF DEDUCTL > 0
               COMPUTE PRLM-DEDUCT-AMT = FUNCTION NUMVAL(DEDUCTI)
           END-IF
           EXEC CICS REWRITE FILE('PRLMST') FROM(PRL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('PYAPRM') MAPSET('PYAPRS')
                FROM(PYAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
