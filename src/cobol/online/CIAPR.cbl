      *================================================================*
      * PROGRAM  : CIAPR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CI90                                                *
      * PURPOSE  : CHECK IMAGE ARCHIVE OVERRIDE. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CIRISK, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-06-09 JLM  ORIGINAL                                       *
      * 1989-08-24 RJT  RETRIEVE-FEE FEE SCHEDULE UPDATE PRB1933       *
      * 1991-04-22 BHS  STORAGE CALCULATION CORRECTED PRB5846          *
      * 1993-01-06 CRP  NEW STORAGE REPORT COLUMNS REQ6849             *
      * 1994-05-16 DPS  NEW IMAGE-FEE REPORT COLUMNS REQ6083           *
      * 1995-09-14 DPS  RETRIEVE-FEE EXCEPTIONS TO OPS QUEUE REQ3916   *
      * 1997-07-16 CRP  ITEM STATUS CODES ADDED PRB3173                *
      * 1997-08-22 GAF  STORAGE TABLE EXPANDED CHG3895                 *
      * 1998-03-10 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-20 MKL  IMAGE-FEE ROUNDING - HALF UP INC8639           *
      * 2002-07-28 CRP  ITEM CUTOFF MOVED INC3945                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIAPR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CI90'.
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
       COPY CIMMST.
       COPY CIAPRS.
       COPY CIRISKP.
       COPY DCLCIM.
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
           MOVE LOW-VALUES TO CIAPRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CIAPRM') MAPSET('CIAPRS')
                FROM(CIAPRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CIAPRM') MAPSET('CIAPRS')
                INTO(CIAPRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO CIMM-KEY
           EXEC CICS READ FILE('CIMMST') INTO(CIM-MASTER-REC)
                RIDFLD(CIMM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF HOLDFLL > 0 AND HOLDFLI NOT = 'Y' AND HOLDFLI NOT = 'N'
               MOVE 'HOLD FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF STATUSL > 0
              AND STATUSI NOT = 'AJ'
              AND STATUSI NOT = 'JY'
              AND STATUSI NOT = 'RU'
               MOVE 'STATUS CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO STATUSL
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
           INITIALIZE CIRISK-PARMS
           MOVE CIMM-RETRIEVE-FEE-AMT
             TO CIRISK-RETRIEVE-FEE-AMT
           MOVE CIMM-IMAGE-FEE-AMT
             TO CIRISK-IMAGE-FEE-AMT
           MOVE CIMM-STORAGE-YTD
             TO CIRISK-STORAGE-YTD
           MOVE CIMM-RETRIEVE-FEE-YTD
             TO CIRISK-RETRIEVE-FEE-YTD
           MOVE CIMM-STORAGE-AMT
             TO CIRISK-STORAGE-AMT
           MOVE CIMM-ITEM-YTD
             TO CIRISK-ITEM-YTD
           MOVE CIMM-LAST-MAINT-DATE
             TO CIRISK-LAST-MAINT-DATE
           MOVE CIMM-CLOSE-DATE
             TO CIRISK-CLOSE-DATE
           MOVE CIMM-NEXT-DUE-DATE
             TO CIRISK-NEXT-DUE-DATE
           MOVE CIMM-OPEN-DATE
             TO CIRISK-OPEN-DATE
           MOVE CIMM-TYPE-CD
             TO CIRISK-TYPE-CD
           MOVE CIMM-ITEM-TYPE-CD
             TO CIRISK-ITEM-TYPE-CD
           MOVE CIMM-STATUS-CD
             TO CIRISK-STATUS-CD
           MOVE CIMM-CAPTURE-SITE-CD
             TO CIRISK-CAPTURE-SITE-CD
           MOVE CIMM-REVIEW-FLAG
             TO CIRISK-REVIEW-FLAG
           MOVE CIMM-HOLD-FLAG
             TO CIRISK-HOLD-FLAG
           MOVE CIMM-OVERRIDE-FLAG
             TO CIRISK-OVERRIDE-FLAG
           MOVE CIMM-PAYMENT-CNT
             TO CIRISK-PAYMENT-CNT
           MOVE CIMM-NOTICE-CNT
             TO CIRISK-NOTICE-CNT
           MOVE CIMM-MARGIN
             TO CIRISK-MARGIN
           MOVE CIMM-FEE-PCT
             TO CIRISK-FEE-PCT
           MOVE CIMM-NAME
             TO CIRISK-NAME
           MOVE CIMM-REF-NO
             TO CIRISK-REF-NO
           EXEC CICS LINK PROGRAM('CIRISK') COMMAREA(CIRISK-PARMS)
                LENGTH(LENGTH OF CIRISK-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CIRISK-RETURN-CODE = 08
               MOVE 'RULE CIRISK DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE CIMM-KEY TO KEYO
           MOVE CIMM-RETRY-CNT TO RETRYCO
           MOVE CIMM-IMAGE-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO IMAGEFO
           MOVE CIMM-PAYMENT-CNT TO PAYMENO
           MOVE CIMM-EXCEPT-CNT TO EXCEPTO
           MOVE CIMM-EXEMPT-FLAG TO EXEMPTO
           MOVE CIMM-LAST-MAINT-DATE TO LASTMAO
           MOVE CIMM-NOTICE-DATE TO NOTICEO
           MOVE CIMM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE CIMM-BRANCH-CD TO BRANCHO
           MOVE CIMM-NOTICE-CNT TO NOTICE1O
           MOVE CIMM-HOLD-FLAG TO HOLDFLO
           MOVE CIMM-STATUS-CD TO STATUSO
           MOVE CIMM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('CIMMST') INTO(CIM-MASTER-REC)
                RIDFLD(CIMM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF HOLDFLL > 0
               MOVE HOLDFLI TO CIMM-HOLD-FLAG
           END-IF
           IF STATUSL > 0
               MOVE STATUSI TO CIMM-STATUS-CD
           END-IF
           MOVE CTL-BUSINESS-DATE TO CIMM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('CIMMST') FROM(CIM-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('CIAPRM') MAPSET('CIAPRS')
                FROM(CIAPRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
