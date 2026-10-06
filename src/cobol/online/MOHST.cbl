      *================================================================*
      * PROGRAM  : MOHST                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : MO43                                                *
      * PURPOSE  : MONEY ORDERS OVERRIDE. ENTER VALIDATES AND DISPLAYS,*
      *            PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE MENU.    *
      * CALLS    : MODTE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-22 MKL  ORIGINAL                                       *
      * 1995-08-26 SKN  VOIDED RERUN PROTECTION INC3024                *
      * 1995-07-25 MKL  FEE NOTICE TEXT PER LEGAL PRB6251              *
      * 1996-06-04 EWS  OUTSTAND CUTOFF MOVED CHG6986                  *
      * 1997-06-15 CRP  OUTSTAND ROUNDING - HALF UP PRB2569            *
      * 2000-01-16 EWS  OUTSTAND LIMIT REVISED PRB2701                 *
      * 2000-05-25 PQR  VOIDED NOTICE TEXT PER LEGAL INC3820           *
      * 2005-07-03 LMB  OUTSTAND CALCULATION CORRECTED CHG3159         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOHST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'MO43'.
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
       COPY MORMST.
       COPY MOHSTS.
       COPY MODTEP.
       COPY DCLMOR.
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
           MOVE LOW-VALUES TO MOHSTMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('MOHSTM') MAPSET('MOHSTS')
                FROM(MOHSTMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('MOHSTM') MAPSET('MOHSTS')
                INTO(MOHSTMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO MORM-KEY
           EXEC CICS READ FILE('MORMST') INTO(MOR-MASTER-REC)
                RIDFLD(MORM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF AUTOREL > 0 AND AUTOREI NOT = 'Y' AND AUTOREI NOT = 'N'
               MOVE 'AUTO RENEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF FACEYTL > 0
               IF FUNCTION TEST-NUMVAL(FACEYTI) NOT = 0
                   MOVE 'FACE YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO FACEYTL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(FACEYTI)
                   IF WS-NUM-WORK < 0
                       MOVE 'FACE YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF ESCHEAL > 0
               IF FUNCTION TEST-NUMVAL(ESCHEAI) NOT = 0
                   MOVE 'ESCHEAT YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO ESCHEAL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(ESCHEAI)
                   IF WS-NUM-WORK < 0
                       MOVE 'ESCHEAT YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF REASONL > 0
              AND REASONI NOT = 'B'
              AND REASONI NOT = 'L'
              AND REASONI NOT = 'P'
               MOVE 'REASON CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REASONL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF OVERRIL > 0 AND OVERRII NOT = 'Y' AND OVERRII NOT = 'N'
               MOVE 'OVERRIDE FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE MODTE-PARMS
           MOVE MORM-FEE-YTD
             TO MODTE-FEE-YTD
           MOVE MORM-FEE-AMT
             TO MODTE-FEE-AMT
           MOVE MORM-PAID-AMT
             TO MODTE-PAID-AMT
           MOVE MORM-VOIDED-AMT
             TO MODTE-VOIDED-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MODTE-ESCHEAT-AMT
           MOVE MORM-FACE-AMT
             TO MODTE-FACE-AMT
           MOVE MORM-LAST-MAINT-DATE
             TO MODTE-LAST-MAINT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MODTE-EXPIRY-DATE
           MOVE MORM-CLOSE-DATE
             TO MODTE-CLOSE-DATE
           MOVE MORM-OPEN-DATE
             TO MODTE-OPEN-DATE
           MOVE MORM-ITEM-TYPE-CD
             TO MODTE-ITEM-TYPE-CD
           MOVE MORM-REASON-CD
             TO MODTE-REASON-CD
           MOVE MORM-TYPE-CD
             TO MODTE-TYPE-CD
           MOVE MORM-CYCLE-CD
             TO MODTE-CYCLE-CD
           MOVE MORM-STOP-FLAG
             TO MODTE-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MODTE-AUTO-RENEW-FLAG
           MOVE MORM-EXEMPT-FLAG
             TO MODTE-EXEMPT-FLAG
           MOVE MORM-PAYMENT-CNT
             TO MODTE-PAYMENT-CNT
           MOVE MORM-ITEM-CNT
             TO MODTE-ITEM-CNT
           MOVE MORM-FEE-PCT
             TO MODTE-FEE-PCT
           MOVE MORM-DISCOUNT
             TO MODTE-DISCOUNT
           MOVE MORM-NAME
             TO MODTE-NAME
           MOVE MORM-REF-NO
             TO MODTE-REF-NO
           EXEC CICS LINK PROGRAM('MODTE') COMMAREA(MODTE-PARMS)
                LENGTH(LENGTH OF MODTE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF MODTE-RETURN-CODE = 08
               MOVE 'RULE MODTE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE MORM-KEY TO KEYO
           MOVE MORM-VOIDED-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO VOIDEDO
           MOVE MORM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE MORM-FACE-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO FACEYTO
           MOVE MORM-FEE-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO FEEPCTO
           MOVE MORM-ESCHEAT-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO ESCHEAO
           MOVE MORM-OPEN-DATE TO OPENDAO
           MOVE MORM-LAST-MAINT-DATE TO LASTMAO
           MOVE MORM-DESC TO DESCO
           MOVE MORM-LAST-ACT-DATE TO LASTACO
           MOVE MORM-REASON-CD TO REASONO
           MOVE MORM-OUTSTAND-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO OUTSTAO
           MOVE MORM-DISCOUNT TO WS-RATE-ED
           MOVE WS-RATE-ED TO DISCOUO
           MOVE MORM-OVERRIDE-FLAG TO OVERRIO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('MORMST') INTO(MOR-MASTER-REC)
                RIDFLD(MORM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF AUTOREL > 0
               MOVE AUTOREI TO MORM-AUTO-RENEW-FLAG
           END-IF
           IF FACEYTL > 0
               COMPUTE MORM-FACE-YTD = FUNCTION NUMVAL(FACEYTI)
           END-IF
           IF ESCHEAL > 0
               COMPUTE MORM-ESCHEAT-YTD = FUNCTION NUMVAL(ESCHEAI)
           END-IF
           IF REASONL > 0
               MOVE REASONI TO MORM-REASON-CD
           END-IF
           IF OVERRIL > 0
               MOVE OVERRII TO MORM-OVERRIDE-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO MORM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('MORMST') FROM(MOR-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('MOHSTM') MAPSET('MOHSTS')
                FROM(MOHSTMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
