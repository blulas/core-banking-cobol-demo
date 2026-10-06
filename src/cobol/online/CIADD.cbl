      *================================================================*
      * PROGRAM  : CIADD                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CI78                                                *
      * PURPOSE  : CHECK IMAGE ARCHIVE OVERRIDE. ENTER VALIDATES AND   *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CIVAL, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-10-13 GAF  ORIGINAL                                       *
      * 1996-02-14 EWS  IMAGE-FEE LIMIT REVISED REQ9463                *
      * 2005-12-27 CRP  STORAGE FEE SCHEDULE UPDATE REQ9527            *
      * 2007-01-09 BHS  IMAGE-FEE FEE SCHEDULE UPDATE INC5450          *
      * 2008-07-15 RJT  ITEM EXCEPTIONS TO OPS QUEUE CHG2312           *
      * 2008-10-19 PQR  ITEM FEE SCHEDULE UPDATE INC6749               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIADD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CI78'.
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
       COPY CIADDS.
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
           MOVE LOW-VALUES TO CIADDMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CIADDM') MAPSET('CIADDS')
                FROM(CIADDMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CIADDM') MAPSET('CIADDS')
                INTO(CIADDMI) RESP(WS-RESP) END-EXEC
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
           IF RETRIEL > 0
               IF FUNCTION TEST-NUMVAL(RETRIEI) NOT = 0
                   MOVE 'RETRIEVE FEE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO RETRIEL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(RETRIEI)
                   IF WS-NUM-WORK < 0
                       MOVE 'RETRIEVE FEE YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF CHANNEL > 0
              AND CHANNEI NOT = 'FW'
              AND CHANNEI NOT = 'KL'
              AND CHANNEI NOT = 'NB'
               MOVE 'CHANNEL CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO CHANNEL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF ITEMTYL > 0
              AND ITEMTYI NOT = 'CW'
              AND ITEMTYI NOT = 'FB'
              AND ITEMTYI NOT = 'HN'
               MOVE 'ITEM TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO ITEMTYL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF RETRIE1L > 0
               IF FUNCTION TEST-NUMVAL(RETRIE1I) NOT = 0
                   MOVE 'RETRIEVE FEE AMT IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO RETRIE1L
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(RETRIE1I)
                   IF WS-NUM-WORK < 0
                       MOVE 'RETRIEVE FEE AMT MAY NOT BE NEGATIVE' TO
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
           MOVE CIMM-NOTICE-DATE TO NOTICEO
           MOVE CIMM-TYPE-CD TO TYPECDO
           MOVE CIMM-CHANNEL-CD TO CHANNEO
           MOVE CIMM-ITEM-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ITEMYTO
           MOVE CIMM-REF-NO TO REFNOO
           MOVE CIMM-STORAGE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAGO
           MOVE CIMM-OVERRIDE-FLAG TO OVERRIO
           MOVE CIMM-CONTACT TO CONTACO
           MOVE CIMM-MATURITY-DATE TO MATURIO
           MOVE CIMM-RETRY-CNT TO RETRYCO
           MOVE CIMM-ITEM-TYPE-CD TO ITEMTYO
           MOVE CIMM-RETRIEVE-FEE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO RETRIE1O
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('CIMMST') INTO(CIM-MASTER-REC)
                RIDFLD(CIMM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF RETRIEL > 0
               COMPUTE CIMM-RETRIEVE-FEE-YTD = FUNCTION NUMVAL(RETRIEI)
           END-IF
           IF CHANNEL > 0
               MOVE CHANNEI TO CIMM-CHANNEL-CD
           END-IF
           IF ITEMTYL > 0
               MOVE ITEMTYI TO CIMM-ITEM-TYPE-CD
           END-IF
           IF RETRIE1L > 0
               COMPUTE CIMM-RETRIEVE-FEE-AMT = FUNCTION NUMVAL(RETRIE1I)
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
           EXEC CICS SEND MAP('CIADDM') MAPSET('CIADDS')
                FROM(CIADDMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
