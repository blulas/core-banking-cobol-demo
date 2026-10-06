      *================================================================*
      * PROGRAM  : SDADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SD89                                                *
      * PURPOSE  : SAFE DEPOSIT BOXES REVIEW. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SDFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-10-25 GAF  ORIGINAL                                       *
      * 1996-01-14 DPS  PREPAID CUTOFF MOVED REQ1234                   *
      * 1998-11-26 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-14 DPS  DRILL ROUNDING - HALF UP CHG6526               *
      * 2006-06-25 LMB  RENT STATUS CODES ADDED INC0498                *
      * 2008-05-14 EWS  DEPOSIT LIMIT REVISED REQ6397                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SD89'.
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
       COPY SDBMST.
       COPY SDADDS.
       COPY SDFEEP.
       COPY DCLSDB.
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
           MOVE LOW-VALUES TO SDADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SDADDM') MAPSET('SDADDS')
                FROM(SDADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SDADDM') MAPSET('SDADDS')
                INTO(SDADDMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO SDBM-KEY
           EXEC CICS READ FILE('SDBMST') INTO(SDB-MASTER-REC)
                RIDFLD(SDBM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DRILLYL > 0
               IF FUNCTION TEST-NUMVAL(DRILLYI) NOT = 0
                   MOVE 'DRILL YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DRILLYL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DRILLYI)
                   IF WS-NUM-WORK < 0
                       MOVE 'DRILL YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF KEYFEEL > 0
               IF FUNCTION TEST-NUMVAL(KEYFEEI) NOT = 0
                   MOVE 'KEY FEE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO KEYFEEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(KEYFEEI)
                   IF WS-NUM-WORK < 0
                       MOVE 'KEY FEE AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE SDFEE-PARMS
           MOVE SDBM-PREPAID-AMT
             TO SDFEE-PREPAID-AMT
           MOVE SDBM-PREPAID-YTD
             TO SDFEE-PREPAID-YTD
           MOVE SDBM-KEY-FEE-AMT
             TO SDFEE-KEY-FEE-AMT
           MOVE SDBM-DEPOSIT-YTD
             TO SDFEE-DEPOSIT-YTD
           MOVE SDBM-RENT-YTD
             TO SDFEE-RENT-YTD
           MOVE SDBM-DRILL-YTD
             TO SDFEE-DRILL-YTD
           MOVE SDBM-LAST-MAINT-DATE
             TO SDFEE-LAST-MAINT-DATE
           MOVE SDBM-LAST-ACT-DATE
             TO SDFEE-LAST-ACT-DATE
           MOVE SDBM-MATURITY-DATE
             TO SDFEE-MATURITY-DATE
           MOVE SDBM-OPEN-DATE
             TO SDFEE-OPEN-DATE
           MOVE SDBM-SOURCE-CD
             TO SDFEE-SOURCE-CD
           MOVE SDBM-BRANCH-CD
             TO SDFEE-BRANCH-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDFEE-BOX-SIZE-CD
           MOVE SDBM-VAULT-CD
             TO SDFEE-VAULT-CD
           MOVE SDBM-SUSPEND-FLAG
             TO SDFEE-SUSPEND-FLAG
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDFEE-AUTO-RENEW-FLAG
           MOVE SDBM-EXEMPT-FLAG
             TO SDFEE-EXEMPT-FLAG
           MOVE SDBM-RETRY-CNT
             TO SDFEE-RETRY-CNT
           MOVE SDBM-ITEM-CNT
             TO SDFEE-ITEM-CNT
           MOVE SDBM-DISCOUNT
             TO SDFEE-DISCOUNT
           MOVE SDBM-PENALTY-PCT
             TO SDFEE-PENALTY-PCT
           MOVE SDBM-CONTACT
             TO SDFEE-CONTACT
           MOVE SDBM-CUST-ID
             TO SDFEE-CUST-ID
           EXEC CICS LINK PROGRAM('SDFEE') COMMAREA(SDFEE-PARMS)
                LENGTH(LENGTH OF SDFEE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SDFEE-RETURN-CODE = 08
               MOVE 'RULE SDFEE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE SDBM-KEY TO KEYO
           MOVE SDBM-CUST-ID TO CUSTIDO
           MOVE SDBM-REASON-CD TO REASONO
           MOVE SDBM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE SDBM-DEPOSIT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEPOSIO
           MOVE SDBM-DESC TO DESCO
           MOVE SDBM-LATE-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO LATEFEO
           MOVE SDBM-DRILL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRILLYO
           MOVE SDBM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE SDBM-KEY-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO KEYFEEO
           MOVE SDBM-KEY-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO KEYFEE1O
           MOVE SDBM-REF-NO TO REFNOO
           MOVE SDBM-RENT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO RENTAMO
           MOVE SDBM-OVERRIDE-FLAG TO OVERRIO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('SDBMST') INTO(SDB-MASTER-REC)
                RIDFLD(SDBM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DRILLYL > 0
               COMPUTE SDBM-DRILL-YTD = FUNCTION NUMVAL(DRILLYI)
           END-IF
           IF KEYFEEL > 0
               COMPUTE SDBM-KEY-FEE-AMT = FUNCTION NUMVAL(KEYFEEI)
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO SDBM-OVERRIDE-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO SDBM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('SDBMST') FROM(SDB-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('SDADDM') MAPSET('SDADDS')
                FROM(SDADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
