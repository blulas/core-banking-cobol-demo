      *================================================================*
      * PROGRAM  : CLOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CL83                                                *
      * PURPOSE  : COLLECTIONS QUEUE INQUIRY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CLRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-01 SKN  ORIGINAL                                       *
      * 1996-08-13 PQR  PAST-DUE TABLE EXPANDED REQ3291                *
      * 1997-07-25 SKN  PAST-DUE CALCULATION CORRECTED CHG4587         *
      * 2000-03-25 SKN  COLLECTED RERUN PROTECTION INC3191             *
      * 2000-08-05 MKL  WRITEOFF FEE SCHEDULE UPDATE CHG1107           *
      * 2001-08-27 RJT  NEW WRITEOFF REPORT COLUMNS PRB7349            *
      * 2001-08-19 MKL  PERFORMANCE - PROMISED READ AHEAD CHG8833      *
      * 2001-08-20 RJT  FEE NOTICE TEXT PER LEGAL INC1869              *
      * 2001-08-03 SKN  COLLECTED TABLE EXPANDED REQ8547               *
      * 2002-02-16 SKN  NEW WRITEOFF REPORT COLUMNS PRB9424            *
      * 2003-10-22 MKL  WRITEOFF FEE SCHEDULE UPDATE PRB6686           *
      * 2003-12-22 TWH  WRITEOFF RERUN PROTECTION REQ4719              *
      * 2003-09-22 RJT  PROMISED FEE SCHEDULE UPDATE CHG7318           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CL83'.
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
       COPY COLMST.
       COPY CLOVRS.
       COPY CLRISKP.
       COPY DCLCOL.
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
           MOVE LOW-VALUES TO CLOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CLOVRM') MAPSET('CLOVRS')
                FROM(CLOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CLOVRM') MAPSET('CLOVRS')
                INTO(CLOVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO COLM-KEY
           EXEC CICS READ FILE('COLMST') INTO(COL-MASTER-REC)
                RIDFLD(COLM-KEY) RESP(WS-RESP) END-EXEC
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
           INITIALIZE CLRISK-PARMS
           MOVE COLM-PAST-DUE-AMT
             TO CLRISK-PAST-DUE-AMT
           MOVE COLM-PAST-DUE-YTD
             TO CLRISK-PAST-DUE-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLRISK-COLLECTED-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLRISK-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLRISK-FEE-YTD
           MOVE COLM-PROMISED-YTD
             TO CLRISK-PROMISED-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLRISK-EFFECTIVE-DATE
           MOVE COLM-OPEN-DATE
             TO CLRISK-OPEN-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLRISK-LAST-MAINT-DATE
           MOVE COLM-NOTICE-DATE
             TO CLRISK-NOTICE-DATE
           MOVE COLM-CYCLE-CD
             TO CLRISK-CYCLE-CD
           MOVE COLM-REGION-CD
             TO CLRISK-REGION-CD
           MOVE COLM-ACTION-CD
             TO CLRISK-ACTION-CD
           MOVE COLM-STATUS-CD
             TO CLRISK-STATUS-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLRISK-NOTICE-SENT-FLAG
           MOVE COLM-REVIEW-FLAG
             TO CLRISK-REVIEW-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLRISK-EXEMPT-FLAG
           MOVE COLM-ITEM-CNT
             TO CLRISK-ITEM-CNT
           MOVE COLM-RETRY-CNT
             TO CLRISK-RETRY-CNT
           MOVE COLM-FEE-PCT
             TO CLRISK-FEE-PCT
           MOVE COLM-MARGIN
             TO CLRISK-MARGIN
           MOVE COLM-DESC
             TO CLRISK-DESC
           MOVE COLM-CONTACT
             TO CLRISK-CONTACT
           EXEC CICS LINK PROGRAM('CLRISK') COMMAREA(CLRISK-PARMS)
                LENGTH(LENGTH OF CLRISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CLRISK-RETURN-CODE = 08
               MOVE 'RULE CLRISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE COLM-KEY TO KEYO
           MOVE COLM-REASON-CD TO REASONO
           MOVE COLM-WRITEOFF-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO WRITEOO
           MOVE COLM-PAST-DUE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PASTDUO
           MOVE COLM-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FEEYTDO
           MOVE COLM-CONTACT TO CONTACO
           MOVE COLM-OPEN-DATE TO OPENDAO
           MOVE COLM-EXEMPT-FLAG TO EXEMPTO
           MOVE COLM-EXPIRY-DATE TO EXPIRYO
           MOVE COLM-PREFERRED-FLAG TO PREFERO
           MOVE COLM-ITEM-CNT TO ITEMCNO
           MOVE COLM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE COLM-LAST-MAINT-DATE TO LASTMAO
           MOVE COLM-CYCLE-CNT TO CYCLECO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('COLMST') INTO(COL-MASTER-REC)
                RIDFLD(COLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO COLM-EXEMPT-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO COLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('COLMST') FROM(COL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('CLOVRM') MAPSET('CLOVRS')
                FROM(CLOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
