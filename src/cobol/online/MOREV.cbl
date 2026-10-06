      *================================================================*
      * PROGRAM  : MOREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : MO28                                                *
      * PURPOSE  : MONEY ORDERS OVERRIDE. ENTER VALIDATES AND DISPLAYS,*
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : MOFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-11 EWS  ORIGINAL                                       *
      * 2002-07-25 BHS  PAID ROUNDING - HALF UP PRB5979                *
      * 2002-09-07 LMB  PAID NOTICE TEXT PER LEGAL INC6837             *
      * 2003-02-06 EWS  VOIDED FEE SCHEDULE UPDATE PRB5900             *
      * 2004-07-12 SKN  AUDIT FINDING - VOIDED CONTROL TOTALS REQ2698  *
      * 2004-03-17 RJT  ESCHEAT CALCULATION CORRECTED PRB5603          *
      * 2005-11-17 BHS  AUDIT FINDING - VOIDED CONTROL TOTALS CHG6332  *
      * 2006-12-26 GAF  AUDIT FINDING - ESCHEAT CONTROL TOTALS INC8663 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'MO28'.
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
       COPY MORMST.
       COPY MOREVS.
       COPY MOFEEP.
       COPY DCLMOR.
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
           MOVE LOW-VALUES TO MOREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('MOREVM') MAPSET('MOREVS')
                FROM(MOREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('MOREVM') MAPSET('MOREVS')
                INTO(MOREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO MORM-KEY
           EXEC CICS READ FILE('MORMST') INTO(MOR-MASTER-REC)
                RIDFLD(MORM-KEY) RESP(WS-RESP) END-EXEC
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
           PERFORM 2500-CALL-RULE
           PERFORM 2600-FORMAT-SCREEN
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS PF5 TO APPLY' TO WS-MESSAGE
           END-IF
           MOVE 'V' TO CA-STATE
           PERFORM 8000-SEND-DATAONLY.

       2500-CALL-RULE.
           INITIALIZE MOFEE-PARMS
           MOVE MORM-OUTSTAND-AMT
             TO MOFEE-OUTSTAND-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MOFEE-ESCHEAT-AMT
           MOVE MORM-PAID-YTD
             TO MOFEE-PAID-YTD
           MOVE MORM-PAID-AMT
             TO MOFEE-PAID-AMT
           MOVE MORM-FACE-AMT
             TO MOFEE-FACE-AMT
           MOVE MORM-FEE-AMT
             TO MOFEE-FEE-AMT
           MOVE MORM-LAST-STMT-DATE
             TO MOFEE-LAST-STMT-DATE
           MOVE MORM-LAST-PAY-DATE
             TO MOFEE-LAST-PAY-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MOFEE-LAST-ACT-DATE
           MOVE MORM-EFFECTIVE-DATE
             TO MOFEE-EFFECTIVE-DATE
           MOVE MORM-PAYEE-TYPE-CD
             TO MOFEE-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MOFEE-CHANNEL-CD
           MOVE MORM-REASON-CD
             TO MOFEE-REASON-CD
           MOVE MORM-ITEM-TYPE-CD
             TO MOFEE-ITEM-TYPE-CD
           MOVE MORM-OVERRIDE-FLAG
             TO MOFEE-OVERRIDE-FLAG
           MOVE MORM-STOP-FLAG
             TO MOFEE-STOP-FLAG
           MOVE MORM-PREFERRED-FLAG
             TO MOFEE-PREFERRED-FLAG
           MOVE MORM-ITEM-CNT
             TO MOFEE-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MOFEE-RETRY-CNT
           MOVE MORM-DISCOUNT
             TO MOFEE-DISCOUNT
           MOVE MORM-FEE-PCT
             TO MOFEE-FEE-PCT
           MOVE MORM-REF-NO
             TO MOFEE-REF-NO
           MOVE MORM-NAME
             TO MOFEE-NAME
           EXEC CICS LINK PROGRAM('MOFEE') COMMAREA(MOFEE-PARMS)
                LENGTH(LENGTH OF MOFEE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF MOFEE-RETURN-CODE = 08
               MOVE 'RULE MOFEE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE MORM-KEY TO KEYO
           MOVE MORM-OUTSTAND-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO OUTSTAO
           MOVE MORM-EXEMPT-FLAG TO EXEMPTO
           MOVE MORM-NAME TO NAMEO
           MOVE MORM-OUTSTAND-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO OUTSTA1O
           MOVE MORM-OPEN-DATE TO OPENDAO
           MOVE MORM-TYPE-CD TO TYPECDO
           MOVE MORM-REASON-CD TO REASONO
           MOVE MORM-ESCHEAT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ESCHEAO
           MOVE MORM-CLOSE-DATE TO CLOSEDO
           MOVE MORM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE MORM-NOTICE-CNT TO NOTICEO
           MOVE MORM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE MORM-VOIDED-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO VOIDEDO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('MORMST') INTO(MOR-MASTER-REC)
                RIDFLD(MORM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO MORM-EXEMPT-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO MORM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('MORMST') FROM(MOR-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('MOREVM') MAPSET('MOREVS')
                FROM(MOREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
