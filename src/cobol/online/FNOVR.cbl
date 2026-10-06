      *================================================================*
      * PROGRAM  : FNOVR                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : FN23                                                *
      * PURPOSE  : FOREIGN BANKNOTES REVIEW. ENTER VALIDATES AND       *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : FNWAIV, DATEUTIL                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-14 EWS  ORIGINAL                                       *
      * 1998-05-12 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-04-25 JLM  SHIPPING CUTOFF MOVED CHG6297                  *
      * 2000-08-18 TWH  USD ROUNDING - HALF UP CHG3727                 *
      * 2001-04-14 DPS  FOREIGN STATUS CODES ADDED PRB0433             *
      * 2003-09-11 MKL  PERFORMANCE - MARGIN READ AHEAD REQ0891        *
      * 2003-07-13 GAF  MARGIN CALCULATION CORRECTED CHG6621           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FNOVR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'FN23'.
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
       COPY FXNMST.
       COPY FNOVRS.
       COPY FNWAIVP.
       COPY DCLFXN.
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
           MOVE LOW-VALUES TO FNOVRMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('FNOVRM') MAPSET('FNOVRS')
                FROM(FNOVRMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('FNOVRM') MAPSET('FNOVRS')
                INTO(FNOVRMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO FXNM-KEY
           EXEC CICS READ FILE('FXNMST') INTO(FXN-MASTER-REC)
                RIDFLD(FXNM-KEY) RESP(WS-RESP) END-EXEC
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
           IF REVIEWL > 0 AND REVIEWI NOT = 'Y' AND REVIEWI NOT = 'N'
               MOVE 'REVIEW FLAG MUST BE Y OR N' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF USDYTDL > 0
               IF FUNCTION TEST-NUMVAL(USDYTDI) NOT = 0
                   MOVE 'USD YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO USDYTDL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(USDYTDI)
                   IF WS-NUM-WORK < 0
                       MOVE 'USD YTD MAY NOT BE NEGATIVE' TO WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF TYPECDL > 0
              AND TYPECDI NOT = 'CFP'
              AND TYPECDI NOT = 'FFU'
              AND TYPECDI NOT = 'JNR'
               MOVE 'TYPE CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO TYPECDL
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF
           IF EXEMPTL > 0 AND EXEMPTI NOT = 'Y' AND EXEMPTI NOT = 'N'
               MOVE 'EXEMPT FLAG MUST BE Y OR N' TO WS-MESSAGE
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
           INITIALIZE FNWAIV-PARMS
           MOVE FXNM-FOREIGN-AMT
             TO FNWAIV-FOREIGN-AMT
           MOVE FXNM-BUYBACK-YTD
             TO FNWAIV-BUYBACK-YTD
           MOVE FXNM-MARGIN-YTD
             TO FNWAIV-MARGIN-YTD
           MOVE FXNM-USD-YTD
             TO FNWAIV-USD-YTD
           MOVE FXNM-MARGIN-AMT
             TO FNWAIV-MARGIN-AMT
           MOVE FXNM-USD-AMT
             TO FNWAIV-USD-AMT
           MOVE FXNM-NOTICE-DATE
             TO FNWAIV-NOTICE-DATE
           MOVE FXNM-LAST-MAINT-DATE
             TO FNWAIV-LAST-MAINT-DATE
           MOVE FXNM-EFFECTIVE-DATE
             TO FNWAIV-EFFECTIVE-DATE
           MOVE FXNM-LAST-ACT-DATE
             TO FNWAIV-LAST-ACT-DATE
           MOVE FXNM-CURRENCY-CD
             TO FNWAIV-CURRENCY-CD
           MOVE FXNM-SOURCE-CD
             TO FNWAIV-SOURCE-CD
           MOVE FXNM-ORDER-TYPE-CD
             TO FNWAIV-ORDER-TYPE-CD
           MOVE FXNM-TYPE-CD
             TO FNWAIV-TYPE-CD
           MOVE FXNM-REVIEW-FLAG
             TO FNWAIV-REVIEW-FLAG
           MOVE FXNM-EXEMPT-FLAG
             TO FNWAIV-EXEMPT-FLAG
           MOVE FXNM-PREFERRED-FLAG
             TO FNWAIV-PREFERRED-FLAG
           MOVE FXNM-EXCEPT-CNT
             TO FNWAIV-EXCEPT-CNT
           MOVE FXNM-NOTICE-CNT
             TO FNWAIV-NOTICE-CNT
           MOVE FXNM-PENALTY-PCT
             TO FNWAIV-PENALTY-PCT
           MOVE FXNM-RATE
             TO FNWAIV-RATE
           MOVE FXNM-NAME
             TO FNWAIV-NAME
           MOVE FXNM-CUST-ID
             TO FNWAIV-CUST-ID
           EXEC CICS LINK PROGRAM('FNWAIV') COMMAREA(FNWAIV-PARMS)
                LENGTH(LENGTH OF FNWAIV-PARMS) RESP(WS-RESP)
           END-EXEC
           IF FNWAIV-RETURN-CODE = 08
               MOVE 'RULE FNWAIV DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE FXNM-KEY TO KEYO
           MOVE FXNM-AUTO-RENEW-FLAG TO AUTOREO
           MOVE FXNM-REVIEW-FLAG TO REVIEWO
           MOVE FXNM-ORDER-TYPE-CD TO ORDERTO
           MOVE FXNM-CUST-ID TO CUSTIDO
           MOVE FXNM-BUYBACK-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO BUYBACO
           MOVE FXNM-NAME TO NAMEO
           MOVE FXNM-USD-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO USDYTDO
           MOVE FXNM-TYPE-CD TO TYPECDO
           MOVE FXNM-RATE TO WS-RATE-ED
           MOVE WS-RATE-ED TO RATEO
           MOVE FXNM-DESC TO DESCO
           MOVE FXNM-FOREIGN-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO FOREIGO
           MOVE FXNM-EXEMPT-FLAG TO EXEMPTO
           MOVE FXNM-PENALTY-PCT TO WS-RATE-ED
           MOVE WS-RATE-ED TO PENALTO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('FXNMST') INTO(FXN-MASTER-REC)
                RIDFLD(FXNM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF AUTOREL > 0
               MOVE AUTOREI TO FXNM-AUTO-RENEW-FLAG
           END-IF
           IF REVIEWL > 0
               MOVE REVIEWI TO FXNM-REVIEW-FLAG
           END-IF
           IF USDYTDL > 0
               COMPUTE FXNM-USD-YTD = FUNCTION NUMVAL(USDYTDI)
           END-IF
           IF TYPECDL > 0
               MOVE TYPECDI TO FXNM-TYPE-CD
           END-IF
           IF EXEMPTL > 0
               MOVE EXEMPTI TO FXNM-EXEMPT-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO FXNM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('FXNMST') FROM(FXN-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('FNOVRM') MAPSET('FNOVRS')
                FROM(FNOVRMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
