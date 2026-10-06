      *================================================================*
      * PROGRAM  : SDAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SD22                                                *
      * PURPOSE  : SAFE DEPOSIT BOXES REVIEW. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SDVAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-10 CRP  ORIGINAL                                       *
      * 1996-06-06 GAF  LATE-FEE STATUS CODES ADDED REQ0802            *
      * 1996-02-20 CRP  LATE-FEE CUTOFF MOVED INC5013                  *
      * 1998-02-08 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-09 PQR  LATE-FEE ROUNDING - HALF UP CHG8486            *
      * 2000-09-14 DPS  PREPAID CALCULATION CORRECTED CHG0954          *
      * 2001-04-10 BHS  RENT LIMIT REVISED INC0497                     *
      * 2004-09-14 JLM  NEW KEY-FEE REPORT COLUMNS PRB5441             *
      * 2005-05-20 TWH  KEY-FEE TABLE EXPANDED PRB9076                 *
      * 2007-10-14 JLM  NEW KEY-FEE REPORT COLUMNS CHG8748             *
      * 2007-03-14 BHS  DRILL RERUN PROTECTION CHG6329                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SD22'.
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
       COPY SDAPRS.
       COPY SDVALP.
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
           MOVE LOW-VALUES TO SDAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SDAPRM') MAPSET('SDAPRS')
                FROM(SDAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SDAPRM') MAPSET('SDAPRS')
                INTO(SDAPRMI) RESP(WS-RESP) END-EXEC
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
           IF DRILLAL > 0
               IF FUNCTION TEST-NUMVAL(DRILLAI) NOT = 0
                   MOVE 'DRILL AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DRILLAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DRILLAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'DRILL AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF KEYFEE1L > 0
               IF FUNCTION TEST-NUMVAL(KEYFEE1I) NOT = 0
                   MOVE 'KEY FEE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO KEYFEE1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(KEYFEE1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'KEY FEE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF RENTYTL > 0
               IF FUNCTION TEST-NUMVAL(RENTYTI) NOT = 0
                   MOVE 'RENT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO RENTYTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(RENTYTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'RENT YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF VAULTCL > 0
              AND VAULTCI NOT = 'BU'
              AND VAULTCI NOT = 'ET'
              AND VAULTCI NOT = 'FF'
               MOVE 'VAULT CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO VAULTCL
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
           INITIALIZE SDVAL-PARMS
           MOVE SDBM-DRILL-YTD
             TO SDVAL-DRILL-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDVAL-KEY-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDVAL-DEPOSIT-AMT
           MOVE SDBM-RENT-AMT
             TO SDVAL-RENT-AMT
           MOVE SDBM-RENT-YTD
             TO SDVAL-RENT-YTD
           MOVE SDBM-DRILL-AMT
             TO SDVAL-DRILL-AMT
           MOVE SDBM-NEXT-DUE-DATE
             TO SDVAL-NEXT-DUE-DATE
           MOVE SDBM-LAST-MAINT-DATE
             TO SDVAL-LAST-MAINT-DATE
           MOVE SDBM-OPEN-DATE
             TO SDVAL-OPEN-DATE
           MOVE SDBM-LAST-STMT-DATE
             TO SDVAL-LAST-STMT-DATE
           MOVE SDBM-CYCLE-CD
             TO SDVAL-CYCLE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDVAL-BOX-SIZE-CD
           MOVE SDBM-REASON-CD
             TO SDVAL-REASON-CD
           MOVE SDBM-BRANCH-CD
             TO SDVAL-BRANCH-CD
           MOVE SDBM-OVERRIDE-FLAG
             TO SDVAL-OVERRIDE-FLAG
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDVAL-AUTO-RENEW-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDVAL-REVIEW-FLAG
           MOVE SDBM-NOTICE-CNT
             TO SDVAL-NOTICE-CNT
           MOVE SDBM-RETRY-CNT
             TO SDVAL-RETRY-CNT
           MOVE SDBM-PENALTY-PCT
             TO SDVAL-PENALTY-PCT
           MOVE SDBM-DISCOUNT
             TO SDVAL-DISCOUNT
           MOVE SDBM-NAME
             TO SDVAL-NAME
           MOVE SDBM-CONTACT
             TO SDVAL-CONTACT
           EXEC CICS LINK PROGRAM('SDVAL') COMMAREA(SDVAL-PARMS)
                LENGTH(LENGTH OF SDVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SDVAL-RETURN-CODE = 08
               MOVE 'RULE SDVAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE SDBM-KEY TO KEYO
           MOVE SDBM-DRILL-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRILLAO
           MOVE SDBM-KEY-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO KEYFEEO
           MOVE SDBM-NEXT-DUE-DATE TO NEXTDUO
           MOVE SDBM-NOTICE-DATE TO NOTICEO
           MOVE SDBM-NAME TO NAMEO
           MOVE SDBM-KEY-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO KEYFEE1O
           MOVE SDBM-OPEN-DATE TO OPENDAO
           MOVE SDBM-RENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RENTYTO
           MOVE SDBM-CUST-ID TO CUSTIDO
           MOVE SDBM-CLOSE-DATE TO CLOSEDO
           MOVE SDBM-VAULT-CD TO VAULTCO
           MOVE SDBM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE SDBM-ITEM-CNT TO ITEMCNO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('SDBMST') INTO(SDB-MASTER-REC)
                RIDFLD(SDBM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF DRILLAL > 0
               COMPUTE SDBM-DRILL-AMT = FUNCTION NUMVAL(DRILLAI)
           END-IF
           IF KEYFEE1L > 0
               COMPUTE SDBM-KEY-FEE-YTD = FUNCTION NUMVAL(KEYFEE1I)
           END-IF
           IF RENTYTL > 0
               COMPUTE SDBM-RENT-YTD = FUNCTION NUMVAL(RENTYTI)
           END-IF
           IF VAULTCL > 0
               MOVE VAULTCI TO SDBM-VAULT-CD
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
           EXEC CICS SEND MAP('SDAPRM') MAPSET('SDAPRS')
                FROM(SDAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
