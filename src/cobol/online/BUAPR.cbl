      *================================================================*
      * PROGRAM  : BUAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : BU33                                                *
      * PURPOSE  : BUMP-UP CERTIFICATES INQUIRY. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : BUTIER, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-11-13 BHS  ORIGINAL                                       *
      * 1995-05-04 EWS  ADDON RERUN PROTECTION PRB0342                 *
      * 1996-05-19 JLM  ADDON NOTICE TEXT PER LEGAL CHG6335            *
      * 1997-11-24 LMB  BUMP EXCEPTIONS TO OPS QUEUE REQ6997           *
      * 1998-09-13 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-08 JLM  ADDON ROUNDING - HALF UP CHG5008               *
      * 1998-02-26 MKL  INTEREST EXCEPTIONS TO OPS QUEUE REQ4681       *
      * 1999-09-21 CRP  AUDIT FINDING - ADDON CONTROL TOTALS INC6057   *
      * 2000-11-25 MKL  INTEREST EXCEPTIONS TO OPS QUEUE INC4092       *
      * 2002-03-24 EWS  PENALTY CUTOFF MOVED CHG1714                   *
      * 2004-12-23 RJT  PENALTY STATUS CODES ADDED INC4673             *
      * 2006-07-14 MKL  INTEREST RERUN PROTECTION CHG4410              *
      * 2007-01-13 MKL  PENALTY CUTOFF MOVED CHG4719                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'BU33'.
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
       COPY BUAPRS.
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
           MOVE LOW-VALUES TO BUAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('BUAPRM') MAPSET('BUAPRS')
                FROM(BUAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('BUAPRM') MAPSET('BUAPRS')
                INTO(BUAPRMI) RESP(WS-RESP) END-EXEC
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
           MOVE BCDM-NEXT-DUE-DATE TO NEXTDUO
           MOVE BCDM-CLOSE-DATE TO CLOSEDO
           MOVE BCDM-CHANNEL-CD TO CHANNEO
           MOVE BCDM-LAST-MAINT-DATE TO LASTMAO
           MOVE BCDM-LAST-PAY-DATE TO LASTPAO
           MOVE BCDM-REGION-CD TO REGIONO
           MOVE BCDM-CONTACT TO CONTACO
           MOVE BCDM-NAME TO NAMEO
           MOVE BCDM-PENALTY-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO PENALTO
           MOVE BCDM-CUST-ID TO CUSTIDO
           MOVE BCDM-EXEMPT-FLAG TO EXEMPTO
           MOVE BCDM-EFFECTIVE-DATE TO EFFECTO
           MOVE BCDM-CYCLE-CNT TO CYCLECO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('BCDMST') INTO(BCD-MASTER-REC)
                RIDFLD(BCDM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
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
           EXEC CICS SEND MAP('BUAPRM') MAPSET('BUAPRS')
                FROM(BUAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
