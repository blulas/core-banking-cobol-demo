      *================================================================*
      * PROGRAM  : SDMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SD85                                                *
      * PURPOSE  : SAFE DEPOSIT BOXES OVERRIDE. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SDRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-07-01 BHS  ORIGINAL                                       *
      * 1993-09-07 CRP  PREPAID ROUNDING - HALF UP INC7000             *
      * 1993-04-02 SKN  DRILL FEE SCHEDULE UPDATE INC7336              *
      * 1994-10-11 EWS  KEY-FEE EXCEPTIONS TO OPS QUEUE CHG4014        *
      * 1995-10-20 EWS  DEPOSIT LIMIT REVISED REQ2292                  *
      * 1995-07-08 DPS  RENT TABLE EXPANDED CHG4625                    *
      * 2002-10-11 LMB  PREPAID LIMIT REVISED INC4089                  *
      * 2003-09-28 PQR  PREPAID NOTICE TEXT PER LEGAL INC6791          *
      * 2004-07-25 TWH  KEY-FEE CALCULATION CORRECTED INC3763          *
      * 2005-08-19 JLM  NEW DEPOSIT REPORT COLUMNS INC9590             *
      * 2006-11-22 CRP  KEY-FEE CALCULATION CORRECTED CHG1305          *
      * 2007-02-28 CRP  NEW KEY-FEE REPORT COLUMNS PRB8433             *
      * 2008-04-07 DPS  AUDIT FINDING - DEPOSIT CONTROL TOTALS CHG6967 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SD85'.
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
       COPY SDMNTS.
       COPY SDRISKP.
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
           MOVE LOW-VALUES TO SDMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SDMNTM') MAPSET('SDMNTS')
                FROM(SDMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SDMNTM') MAPSET('SDMNTS')
                INTO(SDMNTMI) RESP(WS-RESP) END-EXEC
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
           IF RENTAML > 0
               IF FUNCTION TEST-NUMVAL(RENTAMI) NOT = 0
                   MOVE 'RENT AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO RENTAML
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(RENTAMI)
                   IF WS-NUM-WORK < 0
                       MOVE 'RENT AMT MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF DEPOSIL > 0
               IF FUNCTION TEST-NUMVAL(DEPOSII) NOT = 0
                   MOVE 'DEPOSIT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO DEPOSIL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(DEPOSII)
                   IF WS-NUM-WORK < 0
                       MOVE 'DEPOSIT YTD MAY NOT BE NEGATIVE' TO
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
           INITIALIZE SDRISK-PARMS
           MOVE SDBM-LATE-FEE-YTD
             TO SDRISK-LATE-FEE-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDRISK-KEY-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDRISK-DEPOSIT-AMT
           MOVE SDBM-DEPOSIT-YTD
             TO SDRISK-DEPOSIT-YTD
           MOVE SDBM-LATE-FEE-AMT
             TO SDRISK-LATE-FEE-AMT
           MOVE SDBM-KEY-FEE-AMT
             TO SDRISK-KEY-FEE-AMT
           MOVE SDBM-NEXT-DUE-DATE
             TO SDRISK-NEXT-DUE-DATE
           MOVE SDBM-OPEN-DATE
             TO SDRISK-OPEN-DATE
           MOVE SDBM-NEXT-REVIEW-DATE
             TO SDRISK-NEXT-REVIEW-DATE
           MOVE SDBM-LAST-MAINT-DATE
             TO SDRISK-LAST-MAINT-DATE
           MOVE SDBM-REASON-CD
             TO SDRISK-REASON-CD
           MOVE SDBM-CYCLE-CD
             TO SDRISK-CYCLE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDRISK-BOX-SIZE-CD
           MOVE SDBM-VAULT-CD
             TO SDRISK-VAULT-CD
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDRISK-AUTO-RENEW-FLAG
           MOVE SDBM-OVERRIDE-FLAG
             TO SDRISK-OVERRIDE-FLAG
           MOVE SDBM-PREFERRED-FLAG
             TO SDRISK-PREFERRED-FLAG
           MOVE SDBM-ITEM-CNT
             TO SDRISK-ITEM-CNT
           MOVE SDBM-PAYMENT-CNT
             TO SDRISK-PAYMENT-CNT
           MOVE SDBM-FEE-PCT
             TO SDRISK-FEE-PCT
           MOVE SDBM-PENALTY-PCT
             TO SDRISK-PENALTY-PCT
           MOVE SDBM-DESC
             TO SDRISK-DESC
           MOVE SDBM-CUST-ID
             TO SDRISK-CUST-ID
           EXEC CICS LINK PROGRAM('SDRISK') COMMAREA(SDRISK-PARMS)
                LENGTH(LENGTH OF SDRISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF SDRISK-RETURN-CODE = 08
               MOVE 'RULE SDRISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE SDBM-KEY TO KEYO
           MOVE SDBM-PAYMENT-CNT TO PAYMENO
           MOVE SDBM-CLOSE-DATE TO CLOSEDO
           MOVE SDBM-CONTACT TO CONTACO
           MOVE SDBM-DRILL-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRILLAO
           MOVE SDBM-SOURCE-CD TO SOURCEO
           MOVE SDBM-RENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RENTYTO
           MOVE SDBM-OPEN-DATE TO OPENDAO
           MOVE SDBM-NOTICE-CNT TO NOTICEO
           MOVE SDBM-ADDR TO ADDRO
           MOVE SDBM-RENT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO RENTAMO
           MOVE SDBM-DEPOSIT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DEPOSIO
           MOVE SDBM-CUST-ID TO CUSTIDO
           MOVE SDBM-NAME TO NAMEO
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
           IF RENTYTL > 0
               COMPUTE SDBM-RENT-YTD = FUNCTION NUMVAL(RENTYTI)
           END-IF
           IF RENTAML > 0
               COMPUTE SDBM-RENT-AMT = FUNCTION NUMVAL(RENTAMI)
           END-IF
           IF DEPOSIL > 0
               COMPUTE SDBM-DEPOSIT-YTD = FUNCTION NUMVAL(DEPOSII)
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
           EXEC CICS SEND MAP('SDMNTM') MAPSET('SDMNTS')
                FROM(SDMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
