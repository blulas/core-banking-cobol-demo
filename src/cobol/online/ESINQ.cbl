      *================================================================*
      * PROGRAM  : ESINQ                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : ES16                                                *
      * PURPOSE  : MORTGAGE ESCROW MAINTENANCE. ENTER VALIDATES AND    *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : ESFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-08-17 SKN  ORIGINAL                                       *
      * 1994-11-02 PQR  NEW TAX REPORT COLUMNS INC6532                 *
      * 1996-10-10 MKL  DISBURSE TABLE EXPANDED REQ0311                *
      * 1997-09-25 GAF  DISBURSE CALCULATION CORRECTED PRB7923         *
      * 2008-09-12 EWS  PAYMENT TABLE EXPANDED INC9812                 *
      * 2009-02-22 LMB  SURPLUS NOTICE TEXT PER LEGAL INC7066          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'ES16'.
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
       COPY ESCMST.
       COPY ESINQS.
       COPY ESFEEP.
       COPY DCLESC.
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
           MOVE LOW-VALUES TO ESINQMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('ESINQM') MAPSET('ESINQS')
                FROM(ESINQMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('ESINQM') MAPSET('ESINQS')
                INTO(ESINQMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO ESCM-KEY
           EXEC CICS READ FILE('ESCMST') INTO(ESC-MASTER-REC)
                RIDFLD(ESCM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CUSHIOL > 0
               IF FUNCTION TEST-NUMVAL(CUSHIOI) NOT = 0
                   MOVE 'CUSHION YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO CUSHIOL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(CUSHIOI)
                   IF WS-NUM-WORK < 0
                       MOVE 'CUSHION YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF SUSPENL > 0 AND SUSPENI NOT = 'Y' AND SUSPENI NOT = 'N'
               MOVE 'SUSPEND FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF HOLDFLL > 0 AND HOLDFLI NOT = 'Y' AND HOLDFLI NOT = 'N'
               MOVE 'HOLD FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE ESFEE-PARMS
           MOVE ESCM-INSURANCE-AMT
             TO ESFEE-INSURANCE-AMT
           MOVE ESCM-TAX-AMT
             TO ESFEE-TAX-AMT
           MOVE ESCM-DISBURSE-AMT
             TO ESFEE-DISBURSE-AMT
           MOVE ESCM-SURPLUS-YTD
             TO ESFEE-SURPLUS-YTD
           MOVE ESCM-CUSHION-YTD
             TO ESFEE-CUSHION-YTD
           MOVE ESCM-TAX-YTD
             TO ESFEE-TAX-YTD
           MOVE ESCM-LAST-STMT-DATE
             TO ESFEE-LAST-STMT-DATE
           MOVE ESCM-CLOSE-DATE
             TO ESFEE-CLOSE-DATE
           MOVE ESCM-EFFECTIVE-DATE
             TO ESFEE-EFFECTIVE-DATE
           MOVE ESCM-NEXT-REVIEW-DATE
             TO ESFEE-NEXT-REVIEW-DATE
           MOVE ESCM-REGION-CD
             TO ESFEE-REGION-CD
           MOVE ESCM-ANALYSIS-CD
             TO ESFEE-ANALYSIS-CD
           MOVE ESCM-REASON-CD
             TO ESFEE-REASON-CD
           MOVE ESCM-TYPE-CD
             TO ESFEE-TYPE-CD
           MOVE ESCM-PREFERRED-FLAG
             TO ESFEE-PREFERRED-FLAG
           MOVE ESCM-OVERRIDE-FLAG
             TO ESFEE-OVERRIDE-FLAG
           MOVE ESCM-SUSPEND-FLAG
             TO ESFEE-SUSPEND-FLAG
           MOVE ESCM-NOTICE-CNT
             TO ESFEE-NOTICE-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESFEE-EXCEPT-CNT
           MOVE ESCM-FEE-PCT
             TO ESFEE-FEE-PCT
           MOVE ESCM-DISCOUNT
             TO ESFEE-DISCOUNT
           MOVE ESCM-ADDR
             TO ESFEE-ADDR
           MOVE ESCM-CONTACT
             TO ESFEE-CONTACT
           EXEC CICS LINK PROGRAM('ESFEE') COMMAREA(ESFEE-PARMS)
                LENGTH(LENGTH OF ESFEE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF ESFEE-RETURN-CODE = 08
               MOVE 'RULE ESFEE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE ESCM-KEY TO KEYO
           MOVE ESCM-CUSHION-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CUSHIOO
           MOVE ESCM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE ESCM-EXCEPT-CNT TO EXCEPTO
           MOVE ESCM-CONTACT TO CONTACO
           MOVE ESCM-SUSPEND-FLAG TO SUSPENO
           MOVE ESCM-NEXT-DUE-DATE TO NEXTDUO
           MOVE ESCM-HOLD-FLAG TO HOLDFLO
           MOVE ESCM-RETRY-CNT TO RETRYCO
           MOVE ESCM-ADDR TO ADDRO
           MOVE ESCM-CUST-ID TO CUSTIDO
           MOVE ESCM-INSURANCE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO INSURAO
           MOVE ESCM-CLOSE-DATE TO CLOSEDO
           MOVE ESCM-NOTICE-CNT TO NOTICEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ESCMST') INTO(ESC-MASTER-REC)
                RIDFLD(ESCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF CUSHIOL > 0
               COMPUTE ESCM-CUSHION-YTD = FUNCTION NUMVAL(CUSHIOI)
           END-IF
           IF SUSPENL > 0
               MOVE SUSPENI TO ESCM-SUSPEND-FLAG
           END-IF
           IF HOLDFLL > 0
               MOVE HOLDFLI TO ESCM-HOLD-FLAG
           END-IF
           EXEC CICS REWRITE FILE('ESCMST') FROM(ESC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('ESINQM') MAPSET('ESINQS')
                FROM(ESINQMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
