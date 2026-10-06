      *================================================================*
      * PROGRAM  : BULST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BU85                                                *
      * PURPOSE  : BUMP-UP CERTIFICATES HISTORY. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BUTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-06-12 SKN  ORIGINAL                                       *
      * 1995-11-02 JLM  AUDIT FINDING - PRINCIPAL CONTROL TOTALS INC351*
      * 1998-06-14 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-16 RJT  PENALTY TABLE EXPANDED CHG9722                 *
      * 2005-01-11 PQR  PRINCIPAL STATUS CODES ADDED REQ6761           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BULST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BU85'.
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
       COPY BCDMST.
       COPY BULSTS.
       COPY BUTIERP.
       COPY DCLBCD.
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
           MOVE LOW-VALUES TO BULSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BULSTM') MAPSET('BULSTS')
                FROM(BULSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BULSTM') MAPSET('BULSTS')
                INTO(BULSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO BCDM-KEY
           EXEC CICS READ FILE('BCDMST') INTO(BCD-MASTER-REC)
                RIDFLD(BCDM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXEMPTL > 0 AND EXEMPTI NOT = 'Y' AND EXEMPTI NOT = 'N'
               MOVE 'EXEMPT FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF PRINCIL > 0
               IF FUNCTION TEST-NUMVAL(PRINCII) NOT = 0
                   MOVE 'PRINCIPAL YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO PRINCIL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(PRINCII)
                   IF WS-NUM-WORK < 0
                       MOVE 'PRINCIPAL YTD MAY NOT BE NEGATIVE' TO
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
           INITIALIZE BUTIER-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUTIER-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUTIER-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUTIER-BUMP-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUTIER-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-AMT
             TO BUTIER-INTEREST-AMT
           MOVE BCDM-PRINCIPAL-YTD
             TO BUTIER-PRINCIPAL-YTD
           MOVE BCDM-NEXT-DUE-DATE
             TO BUTIER-NEXT-DUE-DATE
           MOVE BCDM-CLOSE-DATE
             TO BUTIER-CLOSE-DATE
           MOVE BCDM-EFFECTIVE-DATE
             TO BUTIER-EFFECTIVE-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUTIER-MATURITY-DATE
           MOVE BCDM-REGION-CD
             TO BUTIER-REGION-CD
           MOVE BCDM-CYCLE-CD
             TO BUTIER-CYCLE-CD
           MOVE BCDM-CHANNEL-CD
             TO BUTIER-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUTIER-SOURCE-CD
           MOVE BCDM-REVIEW-FLAG
             TO BUTIER-REVIEW-FLAG
           MOVE BCDM-EXEMPT-FLAG
             TO BUTIER-EXEMPT-FLAG
           MOVE BCDM-OVERRIDE-FLAG
             TO BUTIER-OVERRIDE-FLAG
           MOVE BCDM-EXCEPT-CNT
             TO BUTIER-EXCEPT-CNT
           MOVE BCDM-RETRY-CNT
             TO BUTIER-RETRY-CNT
           MOVE BCDM-MARGIN
             TO BUTIER-MARGIN
           MOVE BCDM-RATE
             TO BUTIER-RATE
           MOVE BCDM-CUST-ID
             TO BUTIER-CUST-ID
           MOVE BCDM-REF-NO
             TO BUTIER-REF-NO
           EXEC CICS LINK PROGRAM('BUTIER') COMMAREA(BUTIER-PARMS)
                LENGTH(LENGTH OF BUTIER-PARMS) RESP(WS-RESP)
           END-EXEC
           IF BUTIER-RETURN-CODE = 08
               MOVE 'RULE BUTIER DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE BCDM-KEY TO KEYO
           MOVE BCDM-EXEMPT-FLAG TO EXEMPTO
           MOVE BCDM-LAST-MAINT-DATE TO LASTMAO
           MOVE BCDM-REF-NO TO REFNOO
           MOVE BCDM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE BCDM-INTEREST-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTEREO
           MOVE BCDM-CYCLE-CD TO CYCLECO
           MOVE BCDM-EXCEPT-CNT TO EXCEPTO
           MOVE BCDM-CUST-ID TO CUSTIDO
           MOVE BCDM-MATURITY-DATE TO MATURIO
           MOVE BCDM-PRINCIPAL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINCIO
           MOVE BCDM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE BCDM-SOURCE-CD TO SOURCEO
           MOVE BCDM-CHANNEL-CD TO CHANNEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('BCDMST') INTO(BCD-MASTER-REC)
                RIDFLD(BCDM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO BCDM-EXEMPT-FLAG
           END-IF
           IF PRINCIL > 0
               COMPUTE BCDM-PRINCIPAL-YTD = FUNCTION NUMVAL(PRINCII)
           END-IF
           MOVE CTL-BUSINESS-DATE TO BCDM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('BCDMST') FROM(BCD-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('BULSTM') MAPSET('BULSTS')
                FROM(BULSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
