      *================================================================*
      * PROGRAM  : CIMNT                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : CI38                                                *
      * PURPOSE  : CHECK IMAGE ARCHIVE REVIEW. ENTER VALIDATES AND     *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : CIPRC, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-09-20 MKL  ORIGINAL                                       *
      * 1988-12-17 MKL  ITEM RERUN PROTECTION REQ8968                  *
      * 1996-02-21 LMB  RETRIEVE-FEE TABLE EXPANDED REQ2871            *
      * 1998-04-04 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-10-11 TWH  STORAGE TABLE EXPANDED CHG3802                 *
      * 2000-12-03 GAF  IMAGE-FEE CALCULATION CORRECTED PRB2167        *
      * 2005-06-16 DPS  PERFORMANCE - ITEM READ AHEAD INC2440          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIMNT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'CI38'.
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
       COPY CIMNTS.
       COPY CIPRCP.
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
           MOVE LOW-VALUES TO CIMNTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('CIMNTM') MAPSET('CIMNTS')
                FROM(CIMNTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('CIMNTM') MAPSET('CIMNTS')
                INTO(CIMNTMI) RESP(WS-RESP) END-EXEC
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
           INITIALIZE CIPRC-PARMS
           MOVE CIMM-IMAGE-FEE-AMT
             TO CIPRC-IMAGE-FEE-AMT
           MOVE CIMM-ITEM-AMT
             TO CIPRC-ITEM-AMT
           MOVE CIMM-RETRIEVE-FEE-YTD
             TO CIPRC-RETRIEVE-FEE-YTD
           MOVE CIMM-RETRIEVE-FEE-AMT
             TO CIPRC-RETRIEVE-FEE-AMT
           MOVE CIMM-ITEM-YTD
             TO CIPRC-ITEM-YTD
           MOVE CIMM-STORAGE-AMT
             TO CIPRC-STORAGE-AMT
           MOVE CIMM-NEXT-DUE-DATE
             TO CIPRC-NEXT-DUE-DATE
           MOVE CIMM-NOTICE-DATE
             TO CIPRC-NOTICE-DATE
           MOVE CIMM-EFFECTIVE-DATE
             TO CIPRC-EFFECTIVE-DATE
           MOVE CIMM-MATURITY-DATE
             TO CIPRC-MATURITY-DATE
           MOVE CIMM-STATUS-CD
             TO CIPRC-STATUS-CD
           MOVE CIMM-CAPTURE-SITE-CD
             TO CIPRC-CAPTURE-SITE-CD
           MOVE CIMM-CHANNEL-CD
             TO CIPRC-CHANNEL-CD
           MOVE CIMM-ITEM-TYPE-CD
             TO CIPRC-ITEM-TYPE-CD
           MOVE CIMM-EXEMPT-FLAG
             TO CIPRC-EXEMPT-FLAG
           MOVE CIMM-PREFERRED-FLAG
             TO CIPRC-PREFERRED-FLAG
           MOVE CIMM-SUSPEND-FLAG
             TO CIPRC-SUSPEND-FLAG
           MOVE CIMM-NOTICE-CNT
             TO CIPRC-NOTICE-CNT
           MOVE CIMM-RETRY-CNT
             TO CIPRC-RETRY-CNT
           MOVE CIMM-PENALTY-PCT
             TO CIPRC-PENALTY-PCT
           MOVE CIMM-MARGIN
             TO CIPRC-MARGIN
           MOVE CIMM-REF-NO
             TO CIPRC-REF-NO
           MOVE CIMM-ADDR
             TO CIPRC-ADDR
           EXEC CICS LINK PROGRAM('CIPRC') COMMAREA(CIPRC-PARMS)
                LENGTH(LENGTH OF CIPRC-PARMS) RESP(WS-RESP)
           END-EXEC
           IF CIPRC-RETURN-CODE = 08
               MOVE 'RULE CIPRC DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE CIMM-KEY TO KEYO
           MOVE CIMM-EXCEPT-CNT TO EXCEPTO
           MOVE CIMM-PREFERRED-FLAG TO PREFERO
           MOVE CIMM-CHANNEL-CD TO CHANNEO
           MOVE CIMM-RETRY-CNT TO RETRYCO
           MOVE CIMM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE CIMM-LAST-STMT-DATE TO LASTSTO
           MOVE CIMM-STORAGE-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAGO
           MOVE CIMM-STORAGE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STORAG1O
           MOVE CIMM-NEXT-DUE-DATE TO NEXTDUO
           MOVE CIMM-HOLD-FLAG TO HOLDFLO
           MOVE CIMM-REF-NO TO REFNOO
           MOVE CIMM-OPEN-DATE TO OPENDAO
           MOVE CIMM-SUSPEND-FLAG TO SUSPENO
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
           IF SUSPENL > 0
               MOVE SUSPENI TO CIMM-SUSPEND-FLAG
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
           EXEC CICS SEND MAP('CIMNTM') MAPSET('CIMNTS')
                FROM(CIMNTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
