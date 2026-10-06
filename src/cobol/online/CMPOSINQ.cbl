      *================================================================*
      * PROGRAM  : CMPOSINQ                                            *
      * TYPE     : CICS ONLINE - TRANSACTION CB50                      *
      * PURPOSE  : CORPORATE CASH POSITION INQUIRY. FOR ONE CUSTOMER   *
      *            LISTS EVERY ACCOUNT (ACCTMST AIX BROWSE) WITH       *
      *            LEDGER / AVAILABLE / COLLECTED BALANCE AND THE      *
      *            SWEEP THAT TONIGHT'S SWEEPRUN WOULD MAKE ON CURRENT *
      *            BALANCES (SWEEPRUL - SAME RULE MODULE AS BATCH).    *
      *            VIRTUAL ACCOUNTS PROJECT A FULL CONCENTRATION TO    *
      *            THE HEADER. IF THE CUSTOMER OWNS A NOTIONAL POOL    *
      *            HEADER, THE NET POOL POSITION AND THE PROJECTED     *
      *            MONTH INTEREST (POOLALOC) ARE SHOWN.                *
      * MAPSET   : CMPOSIS / CMPOSIM                                   *
      * FILES    : BANKCTL, CUSTMST, ACCTMST, ACCTCUST (AIX PATH),     *
      *            SWEEPMST, POOLMST                                   *
      * COMMAREA : CUSTOMER ID, PAGE NUMBER                            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-10-01 BHS  ORIGINAL                                       *
      * 2004-05-17 BHS  RANGE SWEEP PROJECTION                         *
      * 2011-04-12 DPS  CHG1544 PROJECTION VIA SWEEPRUL                *
      * 2013-06-11 DPS  NOTIONAL POOL POSITION                         *
      * 2019-03-25 AKW  VIRTUAL ACCOUNTS (V MARKER, CONCENTRATION)     *
      * 2020-09-14 AKW  INC29910 PAGING PF7/PF8                        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMPOSINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB50'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP2                        PIC S9(08) COMP.
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-LINES-PER-PAGE               PIC 9(02) VALUE 10.
       01  WS-WORK.
           05  WS-LINE-IX                  PIC 9(02).
           05  WS-SKIP                     PIC 9(04).
           05  WS-SEEN                     PIC 9(04).
           05  WS-BROWSE-KEY               PIC X(10).
           05  WS-END-BROWSE               PIC X(01).
               88  WS-BROWSE-DONE                    VALUE 'Y'.
           05  WS-MORE-FLAG                PIC X(01).
           05  WS-COLLECTED                PIC S9(13)V99 COMP-3.
           05  WS-PROJECTED                PIC S9(13)V99 COMP-3.
           05  WS-TOT-LEDGER               PIC S9(15)V99 COMP-3.
           05  WS-TOT-AVAIL                PIC S9(15)V99 COMP-3.
           05  WS-TOT-SWEEP                PIC S9(15)V99 COMP-3.
           05  WS-POOL-HEADER-ID           PIC X(08).
           05  WS-IX                       PIC 9(02).
           05  WS-DAYS-IN-MONTH            PIC 9(03).
           05  WS-SAVE-ACCT                PIC X(12).
       01  WS-ACCT-SNAP.
           05  WS-SNAP-LEDGER              PIC S9(13)V99 COMP-3.
           05  WS-SNAP-AVAIL               PIC S9(13)V99 COMP-3.
           05  WS-SNAP-HOLD                PIC S9(13)V99 COMP-3.
      *--- DISPLAY LINE ----------------------------------------------*
       01  WS-ACCT-LINE.
           05  WS-AL-ACCT                  PIC X(12).
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-AL-PROD                  PIC X(04).
           05  WS-AL-VIRT                  PIC X(01).
           05  WS-AL-LEDGER                PIC -(12)9.99.
           05  WS-AL-AVAIL                 PIC -(12)9.99.
           05  WS-AL-COLL                  PIC -(12)9.99.
           05  WS-AL-SWEEP                 PIC -(9)9.
       01  WS-AMT-ED                       PIC -(13)9.99.
       01  WS-RATE-ED                      PIC Z9.99.
       01  WS-RATE-PAIR.
           05  WS-RP-CR                    PIC Z9.99.
           05  FILLER                      PIC X(01) VALUE '/'.
           05  WS-RP-DR                    PIC Z9.99.
       01  WS-DATE-DISP.
           05  WS-DD-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DD-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-DD-DD                    PIC 9(02).
       01  WS-DATE-9                       PIC 9(08).
       01  WS-DATE-X REDEFINES WS-DATE-9.
           05  WS-DX-CCYY                  PIC 9(04).
           05  WS-DX-MM                    PIC 9(02).
           05  WS-DX-DD                    PIC 9(02).
       01  WS-MESSAGE                      PIC X(79).

       01  WS-COMMAREA.
           05  CA-CUST-ID                  PIC X(10).
           05  CA-PAGE                     PIC 9(03).

       COPY CMPOSIS.
       COPY BANKCTL.
       COPY CUSTREC.
       COPY ACCTREC.
       COPY SWEEPREC.
       COPY POOLREC.
       COPY SWRPARM.
       COPY POOLPARM.
       COPY DATEPARM.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(13).

       PROCEDURE DIVISION.
       0000-MAIN.
           IF EIBCALEN = 0
               MOVE SPACES TO WS-COMMAREA
               MOVE 1      TO CA-PAGE
               MOVE LOW-VALUES TO CMPOSIMO
               MOVE 'ENTER A CORPORATE CUSTOMER ID' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           MOVE DFHCOMMAREA TO WS-COMMAREA

           EVALUATE EIBAID
               WHEN DFHPF3
                   EXEC CICS XCTL PROGRAM('CBMENU')
                             RESP(WS-RESP)
                   END-EXEC
               WHEN DFHPF12
                   MOVE SPACES TO WS-COMMAREA
                   MOVE 1 TO CA-PAGE
                   MOVE LOW-VALUES TO CMPOSIMO
                   MOVE 'CANCELLED' TO WS-MESSAGE
                   PERFORM 8000-SEND-MAP-ERASE
               WHEN DFHENTER
                   PERFORM 1000-RECEIVE-MAP
                   MOVE 1 TO CA-PAGE
                   PERFORM 2000-BUILD-POSITION
               WHEN DFHPF8
                   ADD 1 TO CA-PAGE
                   PERFORM 2000-BUILD-POSITION
               WHEN DFHPF7
                   IF CA-PAGE > 1
                       SUBTRACT 1 FROM CA-PAGE
                   END-IF
                   PERFORM 2000-BUILD-POSITION
               WHEN OTHER
                   MOVE 'INVALID KEY PRESSED' TO WS-MESSAGE
                   PERFORM 8100-SEND-DATAONLY
           END-EVALUATE
           PERFORM 9000-RETURN.

       1000-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('CMPOSIM') MAPSET('CMPOSIS')
                     INTO(CMPOSIMI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL) AND CUSTIDL > 0
               MOVE CUSTIDI TO CA-CUST-ID
           END-IF.

      *----------------------------------------------------------------*
      * CASH POSITION                                                  *
      *----------------------------------------------------------------*
       2000-BUILD-POSITION.
           MOVE LOW-VALUES TO CMPOSIMO
           MOVE ZERO TO WS-TOT-LEDGER WS-TOT-AVAIL WS-TOT-SWEEP
           MOVE SPACES TO WS-POOL-HEADER-ID WS-MESSAGE
           IF CA-CUST-ID = SPACES
               MOVE 'CUSTOMER ID IS REQUIRED' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF

           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CA-CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           IF NOT CUST-CORPORATE
               MOVE 'CASH POSITION IS FOR CORPORATE CUSTOMERS ONLY'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           MOVE CA-CUST-ID           TO CUSTIDO
           MOVE CUST-LAST-NAME       TO CUSTNMO
           MOVE CUST-SEGMENT         TO SEGMTO
           MOVE CUST-RELATIONSHIP-ID TO RELIDO
           MOVE CTL-BUSINESS-DATE    TO WS-DATE-9
           PERFORM 8300-FORMAT-DATE
           MOVE WS-DATE-DISP         TO BUSDTO

           INITIALIZE DATE-PARMS
           SET DTU-END-OF-MONTH TO TRUE
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-DAYS-IN-MONTH =
                   FUNCTION MOD(DTU-RESULT-DATE, 100)

           COMPUTE WS-SKIP = (CA-PAGE - 1) * WS-LINES-PER-PAGE
           MOVE ZERO TO WS-SEEN WS-LINE-IX
           MOVE 'N'  TO WS-END-BROWSE WS-MORE-FLAG
           MOVE CA-CUST-ID TO WS-BROWSE-KEY
           EXEC CICS STARTBR FILE('ACCTCUST') RIDFLD(WS-BROWSE-KEY)
                     KEYLENGTH(10) GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO ACCOUNTS FOR CUSTOMER' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP-ERASE
               PERFORM 9000-RETURN
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READNEXT FILE('ACCTCUST')
                         INTO(ACCOUNT-RECORD)
                         RIDFLD(WS-BROWSE-KEY) KEYLENGTH(10)
                         RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  AND WS-RESP NOT = DFHRESP(DUPKEY)
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   IF ACCT-PRIMARY-CUST-ID NOT = CA-CUST-ID
                       SET WS-BROWSE-DONE TO TRUE
                   ELSE
                       PERFORM 2100-ACCOUNT-LINE
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCTCUST') RESP(WS-RESP) END-EXEC

           IF WS-SEEN <= WS-SKIP AND CA-PAGE > 1
               SUBTRACT 1 FROM CA-PAGE
               MOVE 'NO MORE ACCOUNTS' TO WS-MESSAGE
           END-IF
           MOVE WS-TOT-LEDGER TO WS-AMT-ED
           MOVE WS-AMT-ED     TO TOTLEDO
           MOVE WS-TOT-AVAIL  TO WS-AMT-ED
           MOVE WS-AMT-ED     TO TOTAVLO
           MOVE WS-TOT-SWEEP  TO WS-AMT-ED
           MOVE WS-AMT-ED     TO TOTSWPO
           MOVE CA-PAGE       TO PAGENOO
           IF WS-POOL-HEADER-ID NOT = SPACES
               PERFORM 3000-POOL-POSITION
           END-IF
           IF WS-MESSAGE = SPACES
               IF WS-MORE-FLAG = 'Y'
                   MOVE 'PF8 FOR MORE ACCOUNTS' TO WS-MESSAGE
               ELSE
                   MOVE 'PROJECTED SWEEPS USE CURRENT AVAILABLE BALANCES'
                     TO WS-MESSAGE
               END-IF
           END-IF
           PERFORM 8000-SEND-MAP-ERASE.

      *    TOTALS COVER EVERY ACCOUNT; ONLY THE CURRENT PAGE IS LISTED
       2100-ACCOUNT-LINE.
           ADD 1 TO WS-SEEN
           MOVE ACCT-LEDGER-BAL TO WS-SNAP-LEDGER
           MOVE ACCT-AVAIL-BAL  TO WS-SNAP-AVAIL
           MOVE ACCT-HOLD-AMT   TO WS-SNAP-HOLD
      *    COLLECTED = LEDGER LESS UNCOLLECTED (HOLDS)
           COMPUTE WS-COLLECTED = ACCT-LEDGER-BAL - ACCT-HOLD-AMT
           PERFORM 2200-PROJECT-SWEEP
           ADD ACCT-LEDGER-BAL TO WS-TOT-LEDGER
           ADD ACCT-AVAIL-BAL  TO WS-TOT-AVAIL
           ADD WS-PROJECTED    TO WS-TOT-SWEEP
           IF ACCT-POOL-ID NOT = SPACES
               MOVE ACCT-POOL-ID TO WS-POOL-HEADER-ID
           END-IF
           IF WS-SEEN <= WS-SKIP
               EXIT PARAGRAPH
           END-IF
           IF WS-LINE-IX >= WS-LINES-PER-PAGE
               MOVE 'Y' TO WS-MORE-FLAG
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-LINE-IX
           MOVE ACCT-NUMBER       TO WS-AL-ACCT
           MOVE ACCT-PRODUCT-CODE TO WS-AL-PROD
           IF ACCT-IS-VIRTUAL
               MOVE 'V' TO WS-AL-VIRT
           ELSE
               MOVE SPACE TO WS-AL-VIRT
           END-IF
           MOVE WS-SNAP-LEDGER    TO WS-AL-LEDGER
           MOVE WS-SNAP-AVAIL     TO WS-AL-AVAIL
           MOVE WS-COLLECTED      TO WS-AL-COLL
           MOVE WS-PROJECTED      TO WS-AL-SWEEP
           EVALUATE WS-LINE-IX
               WHEN 1  MOVE WS-ACCT-LINE TO ACLN01O
               WHEN 2  MOVE WS-ACCT-LINE TO ACLN02O
               WHEN 3  MOVE WS-ACCT-LINE TO ACLN03O
               WHEN 4  MOVE WS-ACCT-LINE TO ACLN04O
               WHEN 5  MOVE WS-ACCT-LINE TO ACLN05O
               WHEN 6  MOVE WS-ACCT-LINE TO ACLN06O
               WHEN 7  MOVE WS-ACCT-LINE TO ACLN07O
               WHEN 8  MOVE WS-ACCT-LINE TO ACLN08O
               WHEN 9  MOVE WS-ACCT-LINE TO ACLN09O
               WHEN 10 MOVE WS-ACCT-LINE TO ACLN10O
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROJECTED SWEEP (SIGNED: - LEAVES THE ACCOUNT, + ARRIVES)      *
      *----------------------------------------------------------------*
       2200-PROJECT-SWEEP.
           MOVE ZERO TO WS-PROJECTED
           MOVE ACCT-NUMBER TO WS-SAVE-ACCT
           IF ACCT-IS-VIRTUAL
               COMPUTE WS-PROJECTED = 0 - ACCT-AVAIL-BAL
               EXIT PARAGRAPH
           END-IF
           IF ACCT-SWEEP-ID = SPACES
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('SWEEPMST') INTO(SWEEP-RECORD)
                     RIDFLD(ACCT-SWEEP-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR NOT SWP-ACTIVE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE SWEEP-RULE-PARMS
           MOVE SWP-TYPE         TO SWR-SWEEP-TYPE
           MOVE WS-SNAP-LEDGER   TO SWR-SRC-LEDGER-BAL
           MOVE WS-SNAP-AVAIL    TO SWR-SRC-AVAIL-BAL
           MOVE WS-SNAP-HOLD     TO SWR-SRC-HOLD-AMT
           MOVE SWP-TARGET-BAL   TO SWR-TARGET-BAL
           MOVE SWP-MIN-BAL      TO SWR-MIN-BAL
           MOVE SWP-MAX-BAL      TO SWR-MAX-BAL
           MOVE SWP-INCREMENT    TO SWR-INCREMENT
           MOVE SWP-MIN-TRANSFER TO SWR-MIN-TRANSFER
      *    TARGET ACCOUNT VIA THE BASE CLUSTER (BROWSE IS ON THE PATH)
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(SWP-TARGET-ACCT) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE ACCT-PRODUCT-CODE TO SWR-TGT-PRODUCT-CODE
               MOVE ACCT-AVAIL-BAL    TO SWR-TGT-AVAIL-BAL
           END-IF
           CALL 'SWEEPRUL' USING SWEEP-RULE-PARMS
           EVALUATE TRUE
               WHEN SWR-DIR-OUT
                   COMPUTE WS-PROJECTED = 0 - SWR-SWEEP-AMOUNT
               WHEN SWR-DIR-IN
                   MOVE SWR-SWEEP-AMOUNT TO WS-PROJECTED
           END-EVALUATE
           IF SWR-RAISE-ALERT
               MOVE 'WARNING - ZBA FUNDING SHORT AT PARENT ACCOUNT'
                 TO WS-MESSAGE
           END-IF
      *    RESTORE THE BROWSED ACCOUNT
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(WS-SAVE-ACCT) RESP(WS-RESP)
           END-EXEC.

      *----------------------------------------------------------------*
      * NOTIONAL POOL - NET POSITION AND PROJECTED MONTH INTEREST      *
      *----------------------------------------------------------------*
       3000-POOL-POSITION.
           EXEC CICS READ FILE('POOLMST') INTO(POOL-RECORD)
                     RIDFLD(WS-POOL-HEADER-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POOL-ALLOC-PARMS
           MOVE POOL-ALLOC-METHOD  TO PLA-ALLOC-METHOD
           MOVE CTL-FED-FUNDS-RATE TO PLA-REF-CREDIT-RATE
           MOVE CTL-PRIME-RATE     TO PLA-REF-DEBIT-RATE
           MOVE POOL-CREDIT-SPREAD TO PLA-CREDIT-SPREAD
           MOVE POOL-DEBIT-SPREAD  TO PLA-DEBIT-SPREAD
           MOVE POOL-LIMIT         TO PLA-POOL-LIMIT
           MOVE WS-DAYS-IN-MONTH   TO PLA-DAYS
           MOVE 1                  TO PLA-MEMBER-COUNT
           MOVE POOL-HEADER-ACCT   TO PLA-MBR-ACCT(1)
           MOVE 'Y'                TO PLA-MBR-HEADER-FLAG(1)
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > POOL-MEMBER-COUNT
               ADD 1 TO PLA-MEMBER-COUNT
               MOVE POOL-MEMBER-ACCT(WS-IX)
                 TO PLA-MBR-ACCT(PLA-MEMBER-COUNT)
               MOVE 'N' TO PLA-MBR-HEADER-FLAG(PLA-MEMBER-COUNT)
           END-PERFORM
      *    ONLINE USES CURRENT LEDGER, NOT THE CYCLE AVERAGE
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PLA-MEMBER-COUNT
               EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                         RIDFLD(PLA-MBR-ACCT(WS-IX)) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   MOVE ACCT-LEDGER-BAL TO PLA-MBR-AVG-BAL(WS-IX)
               END-IF
           END-PERFORM
           CALL 'POOLALOC' USING POOL-ALLOC-PARMS
           MOVE POOL-ID           TO POOLIDO
           MOVE PLA-NET-POSITION  TO WS-AMT-ED
           MOVE WS-AMT-ED         TO POOLNETO
           MOVE PLA-CREDIT-RATE   TO WS-RP-CR
           MOVE PLA-DEBIT-RATE    TO WS-RP-DR
           MOVE WS-RATE-PAIR      TO PLRATEO
           COMPUTE WS-AMT-ED = PLA-TOTAL-CREDIT-INT
                             - PLA-TOTAL-DEBIT-INT
           MOVE WS-AMT-ED         TO PLINTO
           IF PLA-LIMIT-BREACHED
               MOVE 'POOL LIMIT EXCEEDED - AGGREGATE DEBIT OVER LIMIT'
                 TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
       8000-SEND-MAP-ERASE.
           PERFORM 8200-COMMON-FIELDS
           EXEC CICS SEND MAP('CMPOSIM') MAPSET('CMPOSIS')
                     FROM(CMPOSIMO) ERASE CURSOR RESP(WS-RESP)
           END-EXEC.

       8100-SEND-DATAONLY.
           PERFORM 8200-COMMON-FIELDS
           EXEC CICS SEND MAP('CMPOSIM') MAPSET('CMPOSIS')
                     FROM(CMPOSIMO) DATAONLY CURSOR RESP(WS-RESP)
           END-EXEC.

       8200-COMMON-FIELDS.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-DATE-9) RESP(WS-RESP)
           END-EXEC
           PERFORM 8300-FORMAT-DATE
           MOVE WS-DATE-DISP TO CURDATEO
           MOVE WS-MESSAGE   TO MSGO
           MOVE -1           TO CUSTIDL.

       8300-FORMAT-DATE.
           MOVE WS-DX-CCYY TO WS-DD-CCYY
           MOVE WS-DX-MM   TO WS-DD-MM
           MOVE WS-DX-DD   TO WS-DD-DD.

       9000-RETURN.
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
