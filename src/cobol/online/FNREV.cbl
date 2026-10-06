      *================================================================*
      * PROGRAM  : FNREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : FN28                                                *
      * PURPOSE  : FOREIGN BANKNOTES HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : FNVAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-05 MKL  ORIGINAL                                       *
      * 1998-11-14 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-21 GAF  NEW SHIPPING REPORT COLUMNS INC1738            *
      * 2004-12-05 TWH  NEW FOREIGN REPORT COLUMNS PRB1698             *
      * 2005-02-01 PQR  FOREIGN RERUN PROTECTION REQ4423               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'FN28'.
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
       COPY FXNMST.
       COPY FNREVS.
       COPY FNVALP.
       COPY DCLFXN.
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
           MOVE LOW-VALUES TO FNREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('FNREVM') MAPSET('FNREVS')
                FROM(FNREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('FNREVM') MAPSET('FNREVS')
                INTO(FNREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO FXNM-KEY
           EXEC CICS READ FILE('FXNMST') INTO(FXN-MASTER-REC)
                RIDFLD(FXNM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FOREIGL > 0
               IF FUNCTION TEST-NUMVAL(FOREIGI) NOT = 0
                   MOVE 'FOREIGN AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FOREIGL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FOREIGI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FOREIGN AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF USDAMTL > 0
               IF FUNCTION TEST-NUMVAL(USDAMTI) NOT = 0
                   MOVE 'USD AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO USDAMTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(USDAMTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'USD AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
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
           INITIALIZE FNVAL-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNVAL-FOREIGN-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNVAL-SHIPPING-AMT
           MOVE FXNM-FOREIGN-YTD
             TO FNVAL-FOREIGN-YTD
           MOVE FXNM-SHIPPING-YTD
             TO FNVAL-SHIPPING-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNVAL-MARGIN-AMT
           MOVE FXNM-USD-AMT
             TO FNVAL-USD-AMT
           MOVE FXNM-OPEN-DATE
             TO FNVAL-OPEN-DATE
           MOVE FXNM-EXPIRY-DATE
             TO FNVAL-EXPIRY-DATE
           MOVE FXNM-NOTICE-DATE
             TO FNVAL-NOTICE-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNVAL-LAST-ACT-DATE
           MOVE FXNM-TYPE-CD
             TO FNVAL-TYPE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNVAL-ORDER-TYPE-CD
           MOVE FXNM-SOURCE-CD
             TO FNVAL-SOURCE-CD
           MOVE FXNM-BRANCH-CD
             TO FNVAL-BRANCH-CD
           MOVE FXNM-EXEMPT-FLAG
             TO FNVAL-EXEMPT-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNVAL-NOTICE-SENT-FLAG
           MOVE FXNM-REVIEW-FLAG
             TO FNVAL-REVIEW-FLAG
           MOVE FXNM-EXCEPT-CNT
             TO FNVAL-EXCEPT-CNT
           MOVE FXNM-PAYMENT-CNT
             TO FNVAL-PAYMENT-CNT
           MOVE FXNM-PENALTY-PCT
             TO FNVAL-PENALTY-PCT
           MOVE FXNM-RATE
             TO FNVAL-RATE
           MOVE FXNM-CONTACT
             TO FNVAL-CONTACT
           MOVE FXNM-REF-NO
             TO FNVAL-REF-NO
           EXEC CICS LINK PROGRAM('FNVAL') COMMAREA(FNVAL-PARMS)
                LENGTH(LENGTH OF FNVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF FNVAL-RETURN-CODE = 08
               MOVE 'RULE FNVAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE FXNM-KEY TO KEYO
           MOVE FXNM-OPEN-DATE TO OPENDAO
           MOVE FXNM-CONTACT TO CONTACO
           MOVE FXNM-FOREIGN-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FOREIGO
           MOVE FXNM-LAST-PAY-DATE TO LASTPAO
           MOVE FXNM-USD-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO USDAMTO
           MOVE FXNM-EXCEPT-CNT TO EXCEPTO
           MOVE FXNM-NOTICE-CNT TO NOTICEO
           MOVE FXNM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE FXNM-REF-NO TO REFNOO
           MOVE FXNM-TYPE-CD TO TYPECDO
           MOVE FXNM-CUST-ID TO CUSTIDO
           MOVE FXNM-LAST-ACT-DATE TO LASTACO
           MOVE FXNM-NOTICE-DATE TO NOTICE1O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('FXNMST') INTO(FXN-MASTER-REC)
                RIDFLD(FXNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF FOREIGL > 0
               COMPUTE FXNM-FOREIGN-AMT = FUNCTION NUMVAL(FOREIGI)
           END-IF
           IF USDAMTL > 0
               COMPUTE FXNM-USD-AMT = FUNCTION NUMVAL(USDAMTI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO FXNM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('FXNMST') FROM(FXN-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('FNREVM') MAPSET('FNREVS')
                FROM(FNREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
