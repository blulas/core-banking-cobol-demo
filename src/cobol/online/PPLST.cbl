      *================================================================*
      * PROGRAM  : PPLST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PP41                                                *
      * PURPOSE  : POSITIVE PAY INQUIRY. ENTER VALIDATES AND DISPLAYS, *
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : PPELG, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-04 LMB  ORIGINAL                                       *
      * 1993-12-12 GAF  PERFORMANCE - PAID READ AHEAD PRB3372          *
      * 1993-11-02 MKL  ISSUED STATUS CODES ADDED REQ5120              *
      * 1995-12-02 DPS  STALE RERUN PROTECTION CHG4962                 *
      * 1995-11-05 CRP  AUDIT FINDING - STALE CONTROL TOTALS CHG2947   *
      * 1996-10-01 GAF  VOID STATUS CODES ADDED REQ7831                *
      * 1997-03-20 JLM  PAID FEE SCHEDULE UPDATE INC3953               *
      * 1998-06-12 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-06 GAF  VOID TABLE EXPANDED INC5479                    *
      * 1999-06-07 EWS  EXCEPT CUTOFF MOVED CHG0445                    *
      * 2000-10-24 EWS  VOID STATUS CODES ADDED CHG1945                *
      * 2002-03-24 BHS  EXCEPT CALCULATION CORRECTED PRB2334           *
      * 2002-09-28 GAF  PERFORMANCE - PAID READ AHEAD CHG3990          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPLST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PP41'.
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
       COPY PPYMST.
       COPY PPLSTS.
       COPY PPELGP.
       COPY DCLPPY.
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
           MOVE LOW-VALUES TO PPLSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PPLSTM') MAPSET('PPLSTS')
                FROM(PPLSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PPLSTM') MAPSET('PPLSTS')
                INTO(PPLSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO PPYM-KEY
           EXEC CICS READ FILE('PPYMST') INTO(PPY-MASTER-REC)
                RIDFLD(PPYM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF VOIDYTL > 0
               IF FUNCTION TEST-NUMVAL(VOIDYTI) NOT = 0
                   MOVE 'VOID YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO VOIDYTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(VOIDYTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'VOID YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
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
           INITIALIZE PPELG-PARMS
           MOVE PPYM-PAID-AMT
             TO PPELG-PAID-AMT
           MOVE PPYM-PAID-YTD
             TO PPELG-PAID-YTD
           MOVE PPYM-VOID-AMT
             TO PPELG-VOID-AMT
           MOVE PPYM-ISSUED-YTD
             TO PPELG-ISSUED-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPELG-EXCEPT-AMT
           MOVE PPYM-STALE-AMT
             TO PPELG-STALE-AMT
           MOVE PPYM-NEXT-DUE-DATE
             TO PPELG-NEXT-DUE-DATE
           MOVE PPYM-EFFECTIVE-DATE
             TO PPELG-EFFECTIVE-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPELG-EXPIRY-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPELG-CLOSE-DATE
           MOVE PPYM-STATUS-CD
             TO PPELG-STATUS-CD
           MOVE PPYM-REASON-CD
             TO PPELG-REASON-CD
           MOVE PPYM-CYCLE-CD
             TO PPELG-CYCLE-CD
           MOVE PPYM-REGION-CD
             TO PPELG-REGION-CD
           MOVE PPYM-HOLD-FLAG
             TO PPELG-HOLD-FLAG
           MOVE PPYM-STOP-FLAG
             TO PPELG-STOP-FLAG
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPELG-NOTICE-SENT-FLAG
           MOVE PPYM-EXCEPT-CNT
             TO PPELG-EXCEPT-CNT
           MOVE PPYM-CYCLE-CNT
             TO PPELG-CYCLE-CNT
           MOVE PPYM-DISCOUNT
             TO PPELG-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPELG-MARGIN
           MOVE PPYM-REF-NO
             TO PPELG-REF-NO
           MOVE PPYM-CONTACT
             TO PPELG-CONTACT
           EXEC CICS LINK PROGRAM('PPELG') COMMAREA(PPELG-PARMS)
                LENGTH(LENGTH OF PPELG-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PPELG-RETURN-CODE = 08
               MOVE 'RULE PPELG DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PPYM-KEY TO KEYO
           MOVE PPYM-NEXT-REVIEW-DATE TO NEXTREO
           MOVE PPYM-EXPIRY-DATE TO EXPIRYO
           MOVE PPYM-EXCEPT-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO EXCEPTO
           MOVE PPYM-REF-NO TO REFNOO
           MOVE PPYM-ISSUED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ISSUEDO
           MOVE PPYM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE PPYM-NOTICE-CNT TO NOTICEO
           MOVE PPYM-NEXT-DUE-DATE TO NEXTDUO
           MOVE PPYM-STALE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO STALEYO
           MOVE PPYM-VOID-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO VOIDYTO
           MOVE PPYM-CLOSE-DATE TO CLOSEDO
           MOVE PPYM-CUST-ID TO CUSTIDO
           MOVE PPYM-PAID-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO PAIDYTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PPYMST') INTO(PPY-MASTER-REC)
                RIDFLD(PPYM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF VOIDYTL > 0
               COMPUTE PPYM-VOID-YTD = FUNCTION NUMVAL(VOIDYTI)
           END-IF
           MOVE CTL-BUSINESS-DATE TO PPYM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('PPYMST') FROM(PPY-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('PPLSTM') MAPSET('PPLSTS')
                FROM(PPLSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
