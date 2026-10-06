      *================================================================*
      * PROGRAM  : PBREV                                               *
      * TYPE     : ONLINE - CICS PSEUDO-CONVERSATIONAL                 *
      * TRANS ID : PB92                                                *
      * PURPOSE  : PASSBOOK PRINTING HISTORY. ENTER VALIDATES AND      *
      *            DISPLAYS, PF5 APPLIES THE CHANGE, PF3 RETURNS TO THE*
      *            MENU.                                               *
      * CALLS    : PBDTE, DATEUTIL                                     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-04 LMB  ORIGINAL                                       *
      * 1997-08-09 JLM  PRINTED TABLE EXPANDED REQ0815                 *
      * 2001-01-07 DPS  UNPOSTED EXCEPTIONS TO OPS QUEUE CHG4194       *
      * 2003-01-12 SKN  BALANCE EXCEPTIONS TO OPS QUEUE REQ9991        *
      * 2007-08-22 BHS  PERFORMANCE - LINE READ AHEAD CHG1803          *
      * 2008-06-03 DPS  INTEREST LIMIT REVISED REQ2826                 *
      * 2009-03-07 BHS  INTEREST EXCEPTIONS TO OPS QUEUE PRB5169       *
      * 2012-04-08 MKL  PERFORMANCE - LINE READ AHEAD PRB9053          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBREV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANID                       PIC X(04) VALUE 'PB92'.
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
       COPY PBKMST.
       COPY PBREVS.
       COPY PBDTEP.
       COPY DCLPBK.
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
           MOVE LOW-VALUES TO PBREVMO
           MOVE 'ENTER THE KEY AND PRESS ENTER' TO MSGO
           EXEC CICS SEND MAP('PBREVM') MAPSET('PBREVS')
                FROM(PBREVMO) ERASE RESP(WS-RESP) END-EXEC.

       2000-RECEIVE-VALIDATE.
           EXEC CICS RECEIVE MAP('PBREVM') MAPSET('PBREVS')
                INTO(PBREVMI) RESP(WS-RESP) END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           MOVE SPACES TO WS-MESSAGE
           IF KEYL = 0 OR KEYI = SPACES
               MOVE 'KEY IS REQUIRED' TO WS-MESSAGE
               MOVE -1 TO KEYL
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           MOVE KEYI TO PBKM-KEY
           EXEC CICS READ FILE('PBKMST') INTO(PBK-MASTER-REC)
                RIDFLD(PBKM-KEY) RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD NOT FOUND' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF INTEREL > 0
               IF FUNCTION TEST-NUMVAL(INTEREI) NOT = 0
                   MOVE 'INTEREST YTD IS NOT NUMERIC' TO WS-MESSAGE
                   MOVE -1 TO INTEREL
                   MOVE 'Y' TO WS-ERROR-FLAG
               ELSE
                   COMPUTE WS-NUM-WORK = FUNCTION NUMVAL(INTEREI)
                   IF WS-NUM-WORK < 0
                       MOVE 'INTEREST YTD MAY NOT BE NEGATIVE' TO
                           WS-MESSAGE
                       MOVE 'Y' TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF
           IF REGIONL > 0
              AND REGIONI NOT = 'ER'
              AND REGIONI NOT = 'GL'
              AND REGIONI NOT = 'PE'
               MOVE 'REGION CD CODE INVALID' TO WS-MESSAGE
               MOVE -1 TO REGIONL
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
           INITIALIZE PBDTE-PARMS
           MOVE PBKM-PRINTED-YTD
             TO PBDTE-PRINTED-YTD
           MOVE PBKM-INTEREST-YTD
             TO PBDTE-INTEREST-YTD
           MOVE PBKM-UNPOSTED-YTD
             TO PBDTE-UNPOSTED-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBDTE-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-AMT
             TO PBDTE-CARRYFWD-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBDTE-CARRYFWD-YTD
           MOVE PBKM-NEXT-DUE-DATE
             TO PBDTE-NEXT-DUE-DATE
           MOVE PBKM-MATURITY-DATE
             TO PBDTE-MATURITY-DATE
           MOVE PBKM-OPEN-DATE
             TO PBDTE-OPEN-DATE
           MOVE PBKM-CLOSE-DATE
             TO PBDTE-CLOSE-DATE
           MOVE PBKM-REGION-CD
             TO PBDTE-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBDTE-SOURCE-CD
           MOVE PBKM-BRANCH-CD
             TO PBDTE-BRANCH-CD
           MOVE PBKM-PRINTER-CD
             TO PBDTE-PRINTER-CD
           MOVE PBKM-REVIEW-FLAG
             TO PBDTE-REVIEW-FLAG
           MOVE PBKM-EXEMPT-FLAG
             TO PBDTE-EXEMPT-FLAG
           MOVE PBKM-SUSPEND-FLAG
             TO PBDTE-SUSPEND-FLAG
           MOVE PBKM-CYCLE-CNT
             TO PBDTE-CYCLE-CNT
           MOVE PBKM-RETRY-CNT
             TO PBDTE-RETRY-CNT
           MOVE PBKM-MARGIN
             TO PBDTE-MARGIN
           MOVE PBKM-FEE-PCT
             TO PBDTE-FEE-PCT
           MOVE PBKM-CUST-ID
             TO PBDTE-CUST-ID
           MOVE PBKM-NAME
             TO PBDTE-NAME
           EXEC CICS LINK PROGRAM('PBDTE') COMMAREA(PBDTE-PARMS)
                LENGTH(LENGTH OF PBDTE-PARMS) RESP(WS-RESP)
           END-EXEC
           IF PBDTE-RETURN-CODE = 08
               MOVE 'RULE PBDTE DECLINED THE REQUEST' TO WS-MESSAGE
               MOVE 'Y' TO WS-ERROR-FLAG
           END-IF.

       2600-FORMAT-SCREEN.
           MOVE PBKM-KEY TO KEYO
           MOVE PBKM-EXPIRY-DATE TO EXPIRYO
           MOVE PBKM-INTEREST-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO INTEREO
           MOVE PBKM-REF-NO TO REFNOO
           MOVE PBKM-REGION-CD TO REGIONO
           MOVE PBKM-ITEM-CNT TO ITEMCNO
           MOVE PBKM-SUSPEND-FLAG TO SUSPENO
           MOVE PBKM-NAME TO NAMEO
           MOVE PBKM-DESC TO DESCO
           MOVE PBKM-REVIEW-FLAG TO REVIEWO
           MOVE PBKM-PAYMENT-CNT TO PAYMENO
           MOVE PBKM-CARRYFWD-AMT TO WS-AMT-ED
           MOVE WS-AMT-ED TO CARRYFO
           MOVE PBKM-CARRYFWD-YTD TO WS-AMT-ED
           MOVE WS-AMT-ED TO CARRYF1O
           MOVE PBKM-LAST-ACT-DATE TO LASTACO
           MOVE CTL-BUSINESS-DATE TO CURDATEO.

       3000-APPLY.
           EXEC CICS READ FILE('PBKMST') INTO(PBK-MASTER-REC)
                RIDFLD(PBKM-KEY) UPDATE RESP(WS-RESP) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'RECORD LOCKED - TRY AGAIN' TO WS-MESSAGE
               PERFORM 8000-SEND-DATAONLY
               EXIT PARAGRAPH
           END-IF
           IF INTEREL > 0
               COMPUTE PBKM-INTEREST-YTD = FUNCTION NUMVAL(INTEREI)
           END-IF
           IF REGIONL > 0
               MOVE REGIONI TO PBKM-REGION-CD
           END-IF
           IF SUSPENL > 0
               MOVE SUSPENI TO PBKM-SUSPEND-FLAG
           END-IF
           MOVE CTL-BUSINESS-DATE TO PBKM-LAST-MAINT-DATE
           EXEC CICS REWRITE FILE('PBKMST') FROM(PBK-MASTER-REC)
                RESP(WS-RESP) END-EXEC
           EXEC CICS SYNCPOINT END-EXEC
           MOVE 'CHANGE APPLIED' TO WS-MESSAGE
           INITIALIZE WS-COMMAREA
           PERFORM 8000-SEND-DATAONLY.

       8000-SEND-DATAONLY.
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('PBREVM') MAPSET('PBREVS')
                FROM(PBREVMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.
