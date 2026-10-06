      *================================================================*
      * PROGRAM  : ESREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : ES89                                                *
      * PURPOSE  : MORTGAGE ESCROW OVERRIDE. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : ESFEE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-01-12 MKL  ORIGINAL                                       *
      * 1995-06-14 TWH  DISBURSE ROUNDING - HALF UP PRB5818            *
      * 1996-12-06 TWH  SURPLUS RERUN PROTECTION PRB3744               *
      * 1997-05-25 CRP  AUDIT FINDING - PAYMENT CONTROL TOTALS PRB8655 *
      * 1998-11-09 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-02 EWS  SURPLUS STATUS CODES ADDED PRB3098             *
      * 2000-08-15 CRP  CUSHION ROUNDING - HALF UP PRB3460             *
      * 2008-04-07 MKL  TAX TABLE EXPANDED REQ3356                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'ES89'.
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
       COPY ESREVS.
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
           MOVE LOW-VALUES TO ESREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('ESREVM') MAPSET('ESREVS')
                FROM(ESREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('ESREVM') MAPSET('ESREVS')
                INTO(ESREVMI) RESP(WS-RESP) END-EXEC
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
           MOVE ESCM-NEXT-DUE-DATE TO NEXTDUO
           MOVE ESCM-NOTICE-CNT TO NOTICEO
           MOVE ESCM-EXCEPT-CNT TO EXCEPTO
           MOVE ESCM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE ESCM-CYCLE-CNT TO CYCLECO
           MOVE ESCM-SURPLUS-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO SURPLUO
           MOVE ESCM-SUSPEND-FLAG TO SUSPENO
           MOVE ESCM-LAST-STMT-DATE TO LASTSTO
           MOVE ESCM-CUSHION-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CUSHIOO
           MOVE ESCM-ANALYSIS-CD TO ANALYSO
           MOVE ESCM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE ESCM-HOLD-FLAG TO HOLDFLO
           MOVE ESCM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('ESCMST') INTO(ESC-MASTER-REC)
                RIDFLD(ESCM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           EXEC CICS REWRITE FILE('ESCMST') FROM(ESC-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('ESREVM') MAPSET('ESREVS')
                FROM(ESREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
