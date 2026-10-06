      *================================================================*
      * PROGRAM  : CIREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CI69                                                *
      * PURPOSE  : CHECK IMAGE ARCHIVE HISTORY. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CIVAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-03-27 SKN  ORIGINAL                                       *
      * 1993-03-01 JLM  STORAGE CALCULATION CORRECTED CHG5684          *
      * 1997-01-13 RJT  IMAGE-FEE ROUNDING - HALF UP INC7718           *
      * 1997-11-16 SKN  RETRIEVE-FEE NOTICE TEXT PER LEGAL REQ6250     *
      * 2001-04-22 BHS  STORAGE STATUS CODES ADDED CHG4783             *
      * 2001-02-09 LMB  ITEM TABLE EXPANDED INC6545                    *
      * 2004-10-18 TWH  IMAGE-FEE LIMIT REVISED CHG7415                *
      * 2007-03-27 TWH  IMAGE-FEE LIMIT REVISED CHG6365                *
      * 2008-08-12 EWS  ITEM ROUNDING - HALF UP INC3884                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CI69'.
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
       COPY CIREVS.
       COPY CIVALP.
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
           MOVE LOW-VALUES TO CIREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CIREVM') MAPSET('CIREVS')
                FROM(CIREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CIREVM') MAPSET('CIREVS')
                INTO(CIREVMI) RESP(WS-RESP) END-EXEC
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
           IF STORAGL > 0
               IF FUNCTION TEST-NUMVAL(STORAGI) NOT = 0
                   MOVE 'STORAGE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO STORAGL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(STORAGI)
                   IF WS-NUM-WORK < 0
                       MOVE 'STORAGE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF PREFERL > 0 AND PREFERI NOT = 'Y' AND PREFERI NOT = 'N'
               MOVE 'PREFERRED FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF TYPECDL > 0
              AND TYPECDI NOT = 'CT'
              AND TYPECDI NOT = 'NS'
              AND TYPECDI NOT = 'TR'
               MOVE 'TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO TYPECDL
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
           INITIALIZE CIVAL-PARMS
           MOVE CIMM-IMAGE-FEE-AMT
             TO CIVAL-IMAGE-FEE-AMT
           MOVE CIMM-RETRIEVE-FEE-AMT
             TO CIVAL-RETRIEVE-FEE-AMT
           MOVE CIMM-STORAGE-AMT
             TO CIVAL-STORAGE-AMT
           MOVE CIMM-STORAGE-YTD
             TO CIVAL-STORAGE-YTD
           MOVE CIMM-ITEM-AMT
             TO CIVAL-ITEM-AMT
           MOVE CIMM-ITEM-YTD
             TO CIVAL-ITEM-YTD
           MOVE CIMM-OPEN-DATE
             TO CIVAL-OPEN-DATE
           MOVE CIMM-EFFECTIVE-DATE
             TO CIVAL-EFFECTIVE-DATE
           MOVE CIMM-LAST-STMT-DATE
             TO CIVAL-LAST-STMT-DATE
           MOVE CIMM-LAST-MAINT-DATE
             TO CIVAL-LAST-MAINT-DATE
           MOVE CIMM-BRANCH-CD
             TO CIVAL-BRANCH-CD
           MOVE CIMM-CHANNEL-CD
             TO CIVAL-CHANNEL-CD
           MOVE CIMM-ITEM-TYPE-CD
             TO CIVAL-ITEM-TYPE-CD
           MOVE CIMM-CAPTURE-SITE-CD
             TO CIVAL-CAPTURE-SITE-CD
           MOVE CIMM-EXEMPT-FLAG
             TO CIVAL-EXEMPT-FLAG
           MOVE CIMM-HOLD-FLAG
             TO CIVAL-HOLD-FLAG
           MOVE CIMM-OVERRIDE-FLAG
             TO CIVAL-OVERRIDE-FLAG
           MOVE CIMM-PAYMENT-CNT
             TO CIVAL-PAYMENT-CNT
           MOVE CIMM-NOTICE-CNT
             TO CIVAL-NOTICE-CNT
           MOVE CIMM-MARGIN
             TO CIVAL-MARGIN
           MOVE CIMM-PENALTY-PCT
             TO CIVAL-PENALTY-PCT
           MOVE CIMM-NAME
             TO CIVAL-NAME
           MOVE CIMM-ADDR
             TO CIVAL-ADDR
           EXEC CICS LINK PROGRAM('CIVAL') COMMAREA(CIVAL-PARMS)
                LENGTH(LENGTH OF CIVAL-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CIVAL-RETURN-CODE = 08
               MOVE 'RULE CIVAL DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE CIMM-KEY TO KEYO
           MOVE CIMM-RETRIEVE-FEE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO RETRIEO
           MOVE CIMM-NOTICE-CNT TO NOTICEO
           MOVE CIMM-STORAGE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAGO
           MOVE CIMM-CUST-ID TO CUSTIDO
           MOVE CIMM-PREFERRED-FLAG TO PREFERO
           MOVE CIMM-MARGIN TO WS-RATE-ED
           MOVE WS-RATE-ED TO MARGINO
           MOVE CIMM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE CIMM-TYPE-CD TO TYPECDO
           MOVE CIMM-CONTACT TO CONTACO
           MOVE CIMM-CLOSE-DATE TO CLOSEDO
           MOVE CIMM-CHANNEL-CD TO CHANNEO
           MOVE CIMM-ADDR TO ADDRO
           MOVE CIMM-EXCEPT-CNT TO EXCEPTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('CIMMST') INTO(CIM-MASTER-REC)
                RIDFLD(CIMM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF STORAGL > 0
               COMPUTE CIMM-STORAGE-YTD = FUNCTION NUMVAL(STORAGI)
           END-IF
           IF PREFERL > 0
               MOVE PREFERI TO CIMM-PREFERRED-FLAG
           END-IF
           IF TYPECDL > 0
               MOVE TYPECDI TO CIMM-TYPE-CD
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
           EXEC CICS SEND MAP('CIREVM') MAPSET('CIREVS')
                FROM(CIREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
