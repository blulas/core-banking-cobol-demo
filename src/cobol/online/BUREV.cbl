      *================================================================*
      * PROGRAM  : BUREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BU52                                                *
      * PURPOSE  : BUMP-UP CERTIFICATES INQUIRY. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BUTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-17 GAF  ORIGINAL                                       *
      * 1993-01-15 RJT  BUMP TABLE EXPANDED CHG9527                    *
      * 1993-05-25 RJT  PENALTY CUTOFF MOVED INC4038                   *
      * 1994-02-25 JLM  ADDON FEE SCHEDULE UPDATE CHG9145              *
      * 1995-12-26 CRP  ADDON RERUN PROTECTION INC0295                 *
      * 1998-12-23 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-12-09 JLM  AUDIT FINDING - BUMP CONTROL TOTALS PRB5968    *
      * 2002-05-08 SKN  BUMP RERUN PROTECTION REQ0124                  *
      * 2003-11-18 JLM  BUMP FEE SCHEDULE UPDATE INC4025               *
      * 2005-08-19 DPS  PRINCIPAL RERUN PROTECTION REQ7040             *
      * 2006-12-28 DPS  PENALTY TABLE EXPANDED PRB7129                 *
      * 2007-05-21 JLM  BUMP CUTOFF MOVED INC9252                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BU52'.
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
       COPY BUREVS.
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
           MOVE LOW-VALUES TO BUREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BUREVM') MAPSET('BUREVS')
                FROM(BUREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BUREVM') MAPSET('BUREVS')
                INTO(BUREVMI) RESP(WS-RESP) END-EXEC
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
           IF ADDONAL > 0
               IF FUNCTION TEST-NUMVAL(ADDONAI) NOT = 0
                   MOVE 'ADDON AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ADDONAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ADDONAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'ADDON AMT MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF STEPPLL > 0
              AND STEPPLI NOT = 'AYX'
              AND STEPPLI NOT = 'DGU'
              AND STEPPLI NOT = 'GWJ'
               MOVE 'STEP PLAN CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STEPPLL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF SUSPENL > 0 AND SUSPENI NOT = 'Y' AND SUSPENI NOT = 'N'
               MOVE 'SUSPEND FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STOPFLL > 0 AND STOPFLI NOT = 'Y' AND STOPFLI NOT = 'N'
               MOVE 'STOP FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           MOVE BCDM-SOURCE-CD TO SOURCEO
           MOVE BCDM-ADDON-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO ADDONAO
           MOVE BCDM-STEP-PLAN-CD TO STEPPLO
           MOVE BCDM-CLOSE-DATE TO CLOSEDO
           MOVE BCDM-SUSPEND-FLAG TO SUSPENO
           MOVE BCDM-PRINCIPAL-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PRINCIO
           MOVE BCDM-ADDR TO ADDRO
           MOVE BCDM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE BCDM-LAST-PAY-DATE TO LASTPAO
           MOVE BCDM-LAST-STMT-DATE TO LASTSTO
           MOVE BCDM-CUST-ID TO CUSTIDO
           MOVE BCDM-NEXT-DUE-DATE TO NEXTDUO
           MOVE BCDM-STOP-FLAG TO STOPFLO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('BCDMST') INTO(BCD-MASTER-REC)
                RIDFLD(BCDM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF ADDONAL > 0
               COMPUTE BCDM-ADDON-AMT = FUNCTION NUMVAL(ADDONAI)
           END-IF
           IF STEPPLL > 0
               MOVE STEPPLI TO BCDM-STEP-PLAN-CD
           END-IF
           IF SUSPENL > 0
               MOVE SUSPENI TO BCDM-SUSPEND-FLAG
           END-IF
           IF STOPFLL > 0
               MOVE STOPFLI TO BCDM-STOP-FLAG
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
           EXEC CICS SEND MAP('BUREVM') MAPSET('BUREVS')
                FROM(BUREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
