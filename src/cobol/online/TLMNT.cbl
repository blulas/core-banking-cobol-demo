      *================================================================*
      * PROGRAM  : TLMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : TL31                                                *
      * PURPOSE  : TELLER BALANCING REVIEW. ENTER VALIDATES AND        *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : TLPEN, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-10-21 EWS  ORIGINAL                                       *
      * 1994-07-26 RJT  NEW CASH-OUT REPORT COLUMNS CHG8202            *
      * 2003-10-05 BHS  VAULT-SELL LIMIT REVISED PRB1458               *
      * 2004-01-04 GAF  PERFORMANCE - BATCH READ AHEAD REQ0448         *
      * 2004-03-10 SKN  OVERSHORT TABLE EXPANDED REQ0198               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'TL31'.
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
       COPY TBLMST.
       COPY TLMNTS.
       COPY TLPENP.
       COPY DCLTBL.
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
           MOVE LOW-VALUES TO TLMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('TLMNTM') MAPSET('TLMNTS')
                FROM(TLMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('TLMNTM') MAPSET('TLMNTS')
                INTO(TLMNTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO TBLM-KEY
           EXEC CICS READ FILE('TBLMST') INTO(TBL-MASTER-REC)
                RIDFLD(TBLM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REASONL > 0
              AND REASONI NOT = 'GS'
              AND REASONI NOT = 'GV'
              AND REASONI NOT = 'PG'
               MOVE 'REASON CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REASONL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SUSPENL > 0 AND SUSPENI NOT = 'Y' AND SUSPENI NOT = 'N'
               MOVE 'SUSPEND FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE TLPEN-PARMS
           MOVE TBLM-OVERSHORT-AMT
             TO TLPEN-OVERSHORT-AMT
           MOVE TBLM-VAULT-BUY-AMT
             TO TLPEN-VAULT-BUY-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLPEN-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLPEN-CASH-OUT-YTD
           MOVE TBLM-BATCH-AMT
             TO TLPEN-BATCH-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLPEN-CASH-OUT-AMT
           MOVE TBLM-OPEN-DATE
             TO TLPEN-OPEN-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLPEN-EFFECTIVE-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLPEN-NOTICE-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLPEN-NEXT-REVIEW-DATE
           MOVE TBLM-DRAWER-CD
             TO TLPEN-DRAWER-CD
           MOVE TBLM-REASON-CD
             TO TLPEN-REASON-CD
           MOVE TBLM-CHANNEL-CD
             TO TLPEN-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLPEN-SOURCE-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLPEN-SUSPEND-FLAG
           MOVE TBLM-REVIEW-FLAG
             TO TLPEN-REVIEW-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLPEN-HOLD-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLPEN-ITEM-CNT
           MOVE TBLM-RETRY-CNT
             TO TLPEN-RETRY-CNT
           MOVE TBLM-RATE
             TO TLPEN-RATE
           MOVE TBLM-FEE-PCT
             TO TLPEN-FEE-PCT
           MOVE TBLM-REF-NO
             TO TLPEN-REF-NO
           MOVE TBLM-DESC
             TO TLPEN-DESC
           EXEC CICS LINK PROGRAM('TLPEN') COMMAREA(TLPEN-PARMS)
                LENGTH(LENGTH OF TLPEN-PARMS) RESP(WS-RESP)
           END-EXEC
           IF TLPEN-RETURN-CODE = 08
               MOVE 'RULE TLPEN DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE TBLM-KEY TO KEYO
           MOVE TBLM-STOP-FLAG TO STOPFLO
           MOVE TBLM-CASH-OUT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASHOUO
           MOVE TBLM-EXCEPT-CNT TO EXCEPTO
           MOVE TBLM-CASH-IN-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CASHINO
           MOVE TBLM-DESC TO DESCO
           MOVE TBLM-NOTICE-DATE TO NOTICEO
           MOVE TBLM-REASON-CD TO REASONO
           MOVE TBLM-SUSPEND-FLAG TO SUSPENO
           MOVE TBLM-RETRY-CNT TO RETRYCO
           MOVE TBLM-SOURCE-CD TO SOURCEO
           MOVE TBLM-PROOF-TYPE-CD TO PROOFTO
           MOVE TBLM-BATCH-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO BATCHAO
           MOVE TBLM-EXEMPT-FLAG TO EXEMPTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('TBLMST') INTO(TBL-MASTER-REC)
                RIDFLD(TBLM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF REASONL > 0
               MOVE REASONI TO TBLM-REASON-CD
           END-IF
           IF SUSPENL > 0
               MOVE SUSPENI TO TBLM-SUSPEND-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO TBLM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('TBLMST') FROM(TBL-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('TLMNTM') MAPSET('TLMNTS')
                FROM(TLMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
