      *================================================================*
      * PROGRAM  : SDLST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : SD88                                                *
      * PURPOSE  : SAFE DEPOSIT BOXES MAINTENANCE. ENTER VALIDATES AND *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : SDRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-01-25 BHS  ORIGINAL                                       *
      * 1995-06-05 CRP  RENT CUTOFF MOVED PRB6423                      *
      * 1995-10-24 DPS  KEY-FEE TABLE EXPANDED PRB8989                 *
      * 1998-03-24 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-03 JLM  PERFORMANCE - KEY-FEE READ AHEAD INC2547       *
      * 2005-03-11 LMB  PREPAID EXCEPTIONS TO OPS QUEUE INC1146        *
      * 2008-01-22 LMB  DRILL EXCEPTIONS TO OPS QUEUE INC8489          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDLST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'SD88'.
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
       COPY SDLSTS.
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
           MOVE LOW-VALUES TO SDLSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('SDLSTM') MAPSET('SDLSTS')
                FROM(SDLSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('SDLSTM') MAPSET('SDLSTS')
                INTO(SDLSTMI) RESP(WS-RESP) END-EXEC
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
           IF LATEFE1L > 0
               IF FUNCTION TEST-NUMVAL(LATEFE1I) NOT = 0
                   MOVE 'LATE FEE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO LATEFE1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(LATEFE1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'LATE FEE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF SOURCEL > 0
              AND SOURCEI NOT = 'DD'
              AND SOURCEI NOT = 'JZ'
              AND SOURCEI NOT = 'PS'
               MOVE 'SOURCE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO SOURCEL
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
           MOVE SDBM-ITEM-CNT TO ITEMCNO
           MOVE SDBM-DRILL-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO DRILLYO
           MOVE SDBM-RENT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RENTYTO
           MOVE SDBM-KEY-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO KEYFEEO
           MOVE SDBM-LATE-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO LATEFEO
           MOVE SDBM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE SDBM-CLOSE-DATE TO CLOSEDO
           MOVE SDBM-VAULT-CD TO VAULTCO
           MOVE SDBM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE SDBM-EXEMPT-FLAG TO EXEMPTO
           MOVE SDBM-CONTACT TO CONTACO
           MOVE SDBM-LATE-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO LATEFE1O
           MOVE SDBM-SOURCE-CD TO SOURCEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('SDBMST') INTO(SDB-MASTER-REC)
                RIDFLD(SDBM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF LATEFE1L > 0
               COMPUTE SDBM-LATE-FEE-YTD = FUNCTION NUMVAL(LATEFE1I)
           END-IF
           IF SOURCEL > 0
               MOVE SOURCEI TO SDBM-SOURCE-CD
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
           EXEC CICS SEND MAP('SDLSTM') MAPSET('SDLSTS')
                FROM(SDLSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
