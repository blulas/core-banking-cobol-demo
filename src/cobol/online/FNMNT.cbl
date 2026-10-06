      *================================================================*
      * PROGRAM  : FNMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : FN58                                                *
      * PURPOSE  : FOREIGN BANKNOTES HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : FNTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-14 SKN  ORIGINAL                                       *
      * 1996-03-18 LMB  BUYBACK TABLE EXPANDED PRB0335                 *
      * 1998-07-28 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-11-05 SKN  MARGIN CUTOFF MOVED CHG2773                    *
      * 1999-10-01 DPS  SHIPPING TABLE EXPANDED REQ7273                *
      * 2000-09-10 MKL  FOREIGN STATUS CODES ADDED INC4499             *
      * 2001-09-13 SKN  USD NOTICE TEXT PER LEGAL PRB0558              *
      * 2001-04-02 MKL  FOREIGN EXCEPTIONS TO OPS QUEUE REQ3603        *
      * 2003-07-18 JLM  PERFORMANCE - SHIPPING READ AHEAD REQ2184      *
      * 2005-04-06 RJT  PERFORMANCE - USD READ AHEAD PRB8001           *
      * 2005-09-02 GAF  SHIPPING EXCEPTIONS TO OPS QUEUE PRB1242       *
      * 2005-11-15 RJT  MARGIN LIMIT REVISED CHG3972                   *
      * 2005-12-05 SKN  AUDIT FINDING - SHIPPING CONTROL TOTALS CHG8809*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'FN58'.
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
       COPY FNMNTS.
       COPY FNTIERP.
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
           MOVE LOW-VALUES TO FNMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('FNMNTM') MAPSET('FNMNTS')
                FROM(FNMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('FNMNTM') MAPSET('FNMNTS')
                INTO(FNMNTMI) RESP(WS-RESP) END-EXEC
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
           IF ORDERTL > 0
              AND ORDERTI NOT = 'DTU'
              AND ORDERTI NOT = 'KZV'
              AND ORDERTI NOT = 'PHV'
               MOVE 'ORDER TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO ORDERTL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF BUYBACL > 0
               IF FUNCTION TEST-NUMVAL(BUYBACI) NOT = 0
                   MOVE 'BUYBACK YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO BUYBACL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(BUYBACI)
                   IF WS-NUM-WORK < 0
                       MOVE 'BUYBACK YTD MAY NOT BE NEGATIVE' TO
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
           INITIALIZE FNTIER-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNTIER-FOREIGN-AMT
           MOVE FXNM-USD-AMT
             TO FNTIER-USD-AMT
           MOVE FXNM-SHIPPING-AMT
             TO FNTIER-SHIPPING-AMT
           MOVE FXNM-BUYBACK-AMT
             TO FNTIER-BUYBACK-AMT
           MOVE FXNM-BUYBACK-YTD
             TO FNTIER-BUYBACK-YTD
           MOVE FXNM-FOREIGN-YTD
             TO FNTIER-FOREIGN-YTD
           MOVE FXNM-CLOSE-DATE
             TO FNTIER-CLOSE-DATE
           MOVE FXNM-LAST-MAINT-DATE
             TO FNTIER-LAST-MAINT-DATE
           MOVE FXNM-OPEN-DATE
             TO FNTIER-OPEN-DATE
           MOVE FXNM-EFFECTIVE-DATE
             TO FNTIER-EFFECTIVE-DATE
           MOVE FXNM-STATUS-CD
             TO FNTIER-STATUS-CD
           MOVE FXNM-SOURCE-CD
             TO FNTIER-SOURCE-CD
           MOVE FXNM-TYPE-CD
             TO FNTIER-TYPE-CD
           MOVE FXNM-BRANCH-CD
             TO FNTIER-BRANCH-CD
           MOVE FXNM-AUTO-RENEW-FLAG
             TO FNTIER-AUTO-RENEW-FLAG
           MOVE FXNM-NOTICE-SENT-FLAG
             TO FNTIER-NOTICE-SENT-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNTIER-PREFERRED-FLAG
           MOVE FXNM-PAYMENT-CNT
             TO FNTIER-PAYMENT-CNT
           MOVE FXNM-EXCEPT-CNT
             TO FNTIER-EXCEPT-CNT
           MOVE FXNM-MARGIN
             TO FNTIER-MARGIN
           MOVE FXNM-RATE
             TO FNTIER-RATE
           MOVE FXNM-DESC
             TO FNTIER-DESC
           MOVE FXNM-REF-NO
             TO FNTIER-REF-NO
           EXEC CICS LINK PROGRAM('FNTIER') COMMAREA(FNTIER-PARMS)
                LENGTH(LENGTH OF FNTIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF FNTIER-RETURN-CODE = 08
               MOVE 'RULE FNTIER DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE FXNM-KEY TO KEYO
           MOVE FXNM-ORDER-TYPE-CD TO ORDERTO
           MOVE FXNM-NOTICE-DATE TO NOTICEO
           MOVE FXNM-CURRENCY-CD TO CURRENO
           MOVE FXNM-MARGIN-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO MARGINO
           MOVE FXNM-EXEMPT-FLAG TO EXEMPTO
           MOVE FXNM-CYCLE-CNT TO CYCLECO
           MOVE FXNM-REVIEW-FLAG TO REVIEWO
           MOVE FXNM-SHIPPING-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO SHIPPIO
           MOVE FXNM-NOTICE-CNT TO NOTICE1O
           MOVE FXNM-BRANCH-CD TO BRANCHO
           MOVE FXNM-BUYBACK-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO BUYBACO
           MOVE FXNM-OVERRIDE-FLAG TO OVERRIO
           MOVE FXNM-OPEN-DATE TO OPENDAO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('FXNMST') INTO(FXN-MASTER-REC)
                RIDFLD(FXNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF ORDERTL > 0
               MOVE ORDERTI TO FXNM-ORDER-TYPE-CD
           END-IF
           IF BUYBACL > 0
               COMPUTE FXNM-BUYBACK-YTD = FUNCTION NUMVAL(BUYBACI)
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
           EXEC CICS SEND MAP('FNMNTM') MAPSET('FNMNTS')
                FROM(FNMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
