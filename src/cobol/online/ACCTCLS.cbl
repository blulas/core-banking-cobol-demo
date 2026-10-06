      *================================================================*
      * PROGRAM  : ACCTCLS                                             *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB05                                                *
      * MAPSET   : ACCLSS  MAP ACCLSM                                  *
      * PURPOSE  : DEPOSIT ACCOUNT CLOSURE.                            *
      *            ENTER = DISPLAY ACCOUNT                             *
      *            PF5   = RUN CLOSURE CHECKS AND QUOTE THE PAYOUT     *
      *            PF10  = CLOSE (ONLY AFTER A CLEAN QUOTE FOR THE     *
      *                    SAME ACCOUNT, PAYOUT AND NET AMOUNT)        *
      *            CLOSURE RULES:                                      *
      *            - ACCOUNT MUST BE ACTIVE OR DORMANT. FROZEN ONLY    *
      *              FOR ESTATE SETTLEMENT (REASON DC, MANAGER).       *
      *              RESTRICTED ACCOUNTS ARE REFERRED TO COMPLIANCE    *
      *            - NO ACTIVE HOLDS, NOT OVERDRAWN, NO MEMO ACTIVITY  *
      *              TODAY (UNCOLLECTED FUNDS)                         *
      *            - NOT THE OD-PROTECTION SOURCE, SWEEP / ZBA PARENT  *
      *              OR POOL MEMBER FOR ANOTHER ACCOUNT                *
      *            - ACCRUED INTEREST IS PAID FIRST (BACKUP WITHHOLDING*
      *              24% WHERE FLAGGED)                                *
      *            - CD BEFORE MATURITY: EARLY WITHDRAWAL PENALTY FROM *
      *              CDPENLTY (SAME MODULE AS CB21 CDQUOTE)            *
      *            - NON-CD OPENED FEWER THAN 90 DAYS: EARLY CLOSURE   *
      *              FEE 25.00 (MANAGER WAIVER, NOT FOR DC / BK)       *
      *            - NET PROCEEDS TRANSFERRED TO A PAYOUT ACCOUNT OWNED*
      *              BY THE SAME CUSTOMER                              *
      * FILES    : ACCTMST (UPDATE), ACCTCUS (AIX), CUSTMST, PRODMST,  *
      *            HOLDMST, BANKCTL                                    *
      * QUEUES   : TD TRNP - POSTED JOURNAL (EXTRA-PARTITION ->        *
      *            TRANPOST FOR THE EOD GL AND COMPLIANCE STEPS)       *
      * CALLS    : CDPENLTY, DATEUTIL                                  *
      * PF KEYS  : ENTER DISPLAY, PF5 QUOTE, PF10 CLOSE, PF3 MENU,     *
      *            PF12 CANCEL                                         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-01-20 BHS  OD PROTECTION SOURCE CHECK (INC10442)          *
      * 2009-04-20 DPS  CD PENALTY VIA CDPENLTY (CHG0987)              *
      * 2011-04-12 DPS  SWEEP / ZBA PARENT CHECK                       *
      * 2012-09-04 DPS  EARLY CLOSURE FEE 25.00 < 90 DAYS (CHG0812)    *
      * 2014-11-03 DPS  BACKUP WITHHOLDING ON CLOSING INTEREST         *
      * 2016-03-30 DPS  PF5 QUOTE MANDATORY BEFORE PF10 (CHG1290)      *
      * 2019-03-25 AKW  POOL MEMBERSHIP CHECK                          *
      * 2022-01-10 JLM  ESTATE CLOSURE OF FROZEN DECEASED ACCOUNTS     *
      *                 (INC17501)                                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTCLS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB05'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-MESSAGE                      PIC X(79).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-USERID                       PIC X(08).
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-DATE-OUT                     PIC X(10).
       01  WS-TIME-OUT                     PIC 9(06).

      *--- CLOSURE RULE VALUES ----------------------------------------*
       01  WS-CLOSE-RULES.
           05  WS-EARLY-CLOSE-DAYS         PIC S9(05) VALUE +90.
           05  WS-EARLY-CLOSE-FEE          PIC S9(13)V99 COMP-3
                                           VALUE 25.00.
      *    2012-09 DPS WAS 15.00 FOR SAVINGS - CHG0812 UNIFIED AT 25
      *    05  WS-EARLY-CLOSE-FEE-SV       PIC S9(13)V99 COMP-3
      *                                    VALUE 15.00.
           05  WS-BACKUP-WH-RATE           PIC V9(04) VALUE .2400.

      *--- CLOSE REASON CODES AND WHO MAY USE THEM --------------------*
      *    CODE DESCRIPTION (20)       ROLE  FEE-WAIVED
       01  WS-REASON-VALUES.
           05  FILLER PIC X(25) VALUE 'CRCUSTOMER REQUEST   ANYN'.
           05  FILLER PIC X(25) VALUE 'MVCUSTOMER RELOCATED ANYN'.
           05  FILLER PIC X(25) VALUE 'RTRATE / COMPETITOR  ANYN'.
           05  FILLER PIC X(25) VALUE 'DCDECEASED - ESTATE  MGRY'.
           05  FILLER PIC X(25) VALUE 'BKBANK INITIATED     MGRY'.
           05  FILLER PIC X(25) VALUE 'FRFRAUD / ACCT TAKEOVCMPY'.
       01  WS-REASON-TABLE REDEFINES WS-REASON-VALUES.
           05  WS-RSN-ENTRY OCCURS 6 TIMES INDEXED BY WS-RSN-IDX.
               10  WS-RSN-CODE             PIC X(02).
               10  WS-RSN-DESC             PIC X(19).
               10  WS-RSN-ROLE             PIC X(03).
               10  WS-RSN-FEE-WAIVED       PIC X(01).

       01  WS-FLAGS.
           05  WS-BLOCK-FLAG               PIC X(01).
               88  WS-CLOSE-BLOCKED                  VALUE 'Y'.
           05  WS-BROWSE-FLAG              PIC X(01).
               88  WS-BROWSE-DONE                    VALUE 'Y'.
           05  WS-RSN-FOUND-FLAG           PIC X(01).
               88  WS-RSN-FOUND                      VALUE 'Y'.
           05  WS-IS-MANAGER-FLAG          PIC X(01).
               88  WS-IS-MANAGER                     VALUE 'Y'.

       01  WS-WORK.
           05  WS-CHK-CNT                  PIC 9(02).
           05  WS-CHK-TEXT                 PIC X(75) VALUE SPACES.
           05  WS-AIX-KEY                  PIC X(10).
           05  WS-HOLD-KEY.
               10  WS-HK-ACCT              PIC X(12).
               10  WS-HK-SEQ               PIC 9(04).
           05  WS-DAYS-OPEN                PIC S9(07).
           05  WS-INT-CENTS                PIC S9(13)V99 COMP-3.
           05  WS-WITHHOLD                 PIC S9(13)V99 COMP-3.
           05  WS-PENALTY                  PIC S9(13)V99 COMP-3.
           05  WS-FEE                      PIC S9(13)V99 COMP-3.
           05  WS-NET                      PIC S9(13)V99 COMP-3.
           05  WS-QUOTED-NET               PIC S9(13)V99 COMP-3.
           05  WS-AFTER-INT                PIC S9(13)V99 COMP-3.
           05  WS-TRAN-SEQ                 PIC 9(08).
           05  WS-SEQ-SUFFIX               PIC 9(01).
           05  WS-CLOSE-OWNER-1            PIC X(10).
           05  WS-CLOSE-OWNER-2            PIC X(10).
           05  WS-CUST-DECEASED            PIC X(01).

      *    SAVED COPY OF THE ACCOUNT BEING CLOSED (ACCOUNT-RECORD IS
      *    REUSED FOR THE AIX BROWSE AND THE PAYOUT ACCOUNT)
       01  WS-CLOSE-ACCOUNT                PIC X(464).

       01  WS-EDIT-AMT                     PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-EDIT-AMT17                   PIC ZZZ,ZZZ,ZZ9.999999.
       01  WS-EDIT-DATE.
           05  WS-ED-CCYY                  PIC 9(04).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-ED-MM                    PIC 9(02).
           05  FILLER                      PIC X(01) VALUE '-'.
           05  WS-ED-DD                    PIC 9(02).
       01  WS-DATE-IN                      PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY                  PIC 9(04).
           05  WS-DI-MM                    PIC 9(02).
           05  WS-DI-DD                    PIC 9(02).

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-NO-QUOTE                       VALUE ' '.
               88  CA-QUOTE-OK                       VALUE 'Q'.
           05  CA-ACCT-NUMBER              PIC X(12).
           05  CA-PAYOUT-ACCT              PIC X(12).
           05  CA-REASON                   PIC X(02).
           05  CA-WAIVE                    PIC X(01).
           05  CA-NET                      PIC S9(13)V99 COMP-3.

       COPY ACCTREC.
       COPY CUSTREC.
       COPY PRODREC.
       COPY HOLDREC.
       COPY TRANREC.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY CDPPARM.
       COPY ACCLSS.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(36).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           MOVE 'N' TO WS-IS-MANAGER-FLAG
           IF WS-USERID(1:3) = 'MGR'
               SET WS-IS-MANAGER TO TRUE
           END-IF
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO ACCLSMO
               MOVE 'ENTER ACCOUNT NUMBER' TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       INITIALIZE WS-COMMAREA
                       MOVE LOW-VALUES TO ACCLSMO
                       MOVE 'CANCELLED - NOTHING CLOSED' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
                   WHEN DFHENTER
                       PERFORM 1000-RECEIVE-INPUT
                       SET CA-NO-QUOTE TO TRUE
                       PERFORM 2000-DISPLAY-ACCOUNT
                       PERFORM 8000-SEND-MAP
                   WHEN DFHPF5
                       PERFORM 1000-RECEIVE-INPUT
                       PERFORM 3000-QUOTE
                       PERFORM 8000-SEND-MAP
                   WHEN DFHPF10
                       PERFORM 6000-CONFIRM-CLOSE
                       PERFORM 8000-SEND-MAP
                   WHEN OTHER
                       MOVE LOW-VALUES TO ACCLSMO
                       MOVE 'INVALID KEY' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      * A CHANGE TO ANY KEY FIELD INVALIDATES AN EARLIER QUOTE         *
      *----------------------------------------------------------------*
       1000-RECEIVE-INPUT.
           EXEC CICS RECEIVE MAP('ACCLSM') MAPSET('ACCLSS')
                     INTO(ACCLSMI) RESP(WS-RESP)
           END-EXEC
           IF ACCTNOL > 0 AND ACCTNOI NOT = CA-ACCT-NUMBER
               MOVE ACCTNOI TO CA-ACCT-NUMBER
               SET CA-NO-QUOTE TO TRUE
           END-IF
           IF PAYACTL > 0 AND PAYACTI NOT = CA-PAYOUT-ACCT
               MOVE PAYACTI TO CA-PAYOUT-ACCT
               SET CA-NO-QUOTE TO TRUE
           END-IF
           IF RSNCDL > 0 AND RSNCDI NOT = CA-REASON
               MOVE RSNCDI TO CA-REASON
               SET CA-NO-QUOTE TO TRUE
           END-IF
           IF WAIVEL > 0 AND WAIVEI NOT = CA-WAIVE
               MOVE WAIVEI TO CA-WAIVE
               SET CA-NO-QUOTE TO TRUE
           END-IF
           IF CA-PAYOUT-ACCT = LOW-VALUES
               MOVE SPACES TO CA-PAYOUT-ACCT
           END-IF
           IF CA-REASON = SPACES OR LOW-VALUES
               MOVE 'CR' TO CA-REASON
           END-IF.

      *----------------------------------------------------------------*
       2000-DISPLAY-ACCOUNT.
           MOVE LOW-VALUES TO ACCLSMO
           PERFORM 2100-READ-ACCOUNT
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'PF5 TO RUN CLOSURE CHECKS AND QUOTE' TO WS-MESSAGE
           END-IF.

       2100-READ-ACCOUNT.
           IF CA-ACCT-NUMBER = SPACES OR LOW-VALUES
               MOVE 'ACCOUNT NUMBER IS REQUIRED' TO WS-MESSAGE
               MOVE 99 TO WS-RESP
               EXIT PARAGRAPH
           END-IF
           MOVE CA-ACCT-NUMBER TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(ACCT-NUMBER) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-NUMBER TO ACCTNOO
           MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           EXEC CICS READ FILE('PRODMST') INTO(PRODUCT-RECORD)
                     RIDFLD(PROD-CODE) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               INITIALIZE PRODUCT-RECORD
               MOVE ACCT-PRODUCT-CODE TO PROD-CODE
           END-IF
           MOVE PROD-NAME TO PRODNMO
           EVALUATE TRUE
               WHEN ACCT-ACTIVE        MOVE 'ACTIVE'      TO ASTATO
               WHEN ACCT-DORMANT       MOVE 'DORMANT'     TO ASTATO
               WHEN ACCT-FROZEN        MOVE 'FROZEN'      TO ASTATO
               WHEN ACCT-CLOSED        MOVE 'CLOSED'      TO ASTATO
               WHEN ACCT-PENDING-OPEN  MOVE 'PENDING'     TO ASTATO
               WHEN ACCT-ESCHEATED     MOVE 'ESCHEATED'   TO ASTATO
               WHEN ACCT-CHARGED-OFF   MOVE 'CHARGED OFF' TO ASTATO
               WHEN ACCT-RESTRICTED
                   STRING 'RESTRICT ' ACCT-RESTRICT-REASON
                          DELIMITED BY SIZE INTO ASTATO
           END-EVALUATE
           MOVE ACCT-OPEN-DATE TO WS-DATE-IN
           PERFORM 8100-EDIT-DATE
           MOVE WS-EDIT-DATE TO OPENDTO
           MOVE ACCT-LEDGER-BAL TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT TO LEDGERO
           MOVE ACCT-ACCRUED-INT TO WS-EDIT-AMT17
           MOVE WS-EDIT-AMT17 TO ACCRINTO
           MOVE CA-PAYOUT-ACCT TO PAYACTO
           MOVE CA-REASON      TO RSNCDO
           MOVE CA-WAIVE       TO WAIVEO
           MOVE ACCT-PRIMARY-CUST-ID TO WS-CLOSE-OWNER-1
           MOVE ACCT-JOINT-CUST-ID   TO WS-CLOSE-OWNER-2
           MOVE ACCT-PRIMARY-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               STRING CUST-ID ' ' FUNCTION TRIM(CUST-FIRST-NAME) ' '
                      FUNCTION TRIM(CUST-LAST-NAME)
                      DELIMITED BY SIZE INTO OWNERO
               MOVE 'N' TO WS-CUST-DECEASED
               IF CUST-DECEASED
                   MOVE 'Y' TO WS-CUST-DECEASED
               END-IF
           ELSE
               MOVE 'N' TO WS-CUST-DECEASED
           END-IF
           MOVE ACCOUNT-RECORD TO WS-CLOSE-ACCOUNT
           MOVE DFHRESP(NORMAL) TO WS-RESP.

      *----------------------------------------------------------------*
      * PF5 - ALL CHECKS ARE RUN AND LISTED (NOT FIRST-FAIL). THE      *
      * QUOTE IS ONLY 'OK' WHEN NOTHING BLOCKS THE CLOSURE.            *
      *----------------------------------------------------------------*
       3000-QUOTE.
           MOVE LOW-VALUES TO ACCLSMO
           SET CA-NO-QUOTE TO TRUE
           PERFORM 2100-READ-ACCOUNT
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-BLOCK-FLAG
           MOVE ZERO TO WS-CHK-CNT
           PERFORM 3100-REASON-RULES
           PERFORM 3200-STATUS-RULES
           PERFORM 3300-BALANCE-RULES
           PERFORM 3400-HOLD-RULES
           PERFORM 3500-LINKED-ACCOUNT-RULES
           MOVE WS-CLOSE-ACCOUNT TO ACCOUNT-RECORD
           PERFORM 4000-COMPUTE-PROCEEDS
           PERFORM 3600-PAYOUT-RULES
           MOVE WS-CLOSE-ACCOUNT TO ACCOUNT-RECORD
           IF WS-CHK-CNT = 0
               MOVE 'ALL CLOSURE CHECKS PASSED' TO CHK01O
           END-IF
           IF WS-CLOSE-BLOCKED
               MOVE 'NOT ELIGIBLE TO CLOSE' TO CLSSTATO
               MOVE DFHRED TO CLSSTATC
               MOVE 'ACCOUNT CANNOT BE CLOSED - SEE CLOSURE CHECKS'
                 TO WS-MESSAGE
           ELSE
               SET CA-QUOTE-OK TO TRUE
               MOVE WS-NET TO CA-NET
               MOVE 'QUOTED - READY TO CLOSE' TO CLSSTATO
               MOVE 'REVIEW PAYOUT - PRESS PF10 TO CLOSE THE ACCOUNT'
                 TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
       3100-REASON-RULES.
           MOVE 'N' TO WS-RSN-FOUND-FLAG
           SET WS-RSN-IDX TO 1
           SEARCH WS-RSN-ENTRY
               AT END CONTINUE
               WHEN WS-RSN-CODE(WS-RSN-IDX) = CA-REASON
                   SET WS-RSN-FOUND TO TRUE
           END-SEARCH
           IF NOT WS-RSN-FOUND
               MOVE 'CLOSE REASON MUST BE CR MV RT DC BK OR FR'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
               SET WS-RSN-IDX TO 1
               EXIT PARAGRAPH
           END-IF
           EVALUATE WS-RSN-ROLE(WS-RSN-IDX)
               WHEN 'ANY'
                   CONTINUE
               WHEN 'MGR'
                   IF NOT WS-IS-MANAGER
                      AND WS-USERID(1:3) NOT = 'CMP'
                       STRING 'REASON ' CA-REASON
                              ' REQUIRES A MANAGER OR COMPLIANCE USER'
                              DELIMITED BY SIZE INTO WS-CHK-TEXT
                       PERFORM 8200-ADD-BLOCK
                   END-IF
               WHEN 'CMP'
                   IF WS-USERID(1:3) NOT = 'CMP'
                       STRING 'REASON ' CA-REASON
                              ' REQUIRES A COMPLIANCE USER'
                              DELIMITED BY SIZE INTO WS-CHK-TEXT
                       PERFORM 8200-ADD-BLOCK
                   END-IF
           END-EVALUATE
           IF CA-WAIVE = 'Y' AND NOT WS-IS-MANAGER
               MOVE 'FEE WAIVER (Y) MAY ONLY BE ENTERED BY A MANAGER'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF.

      *----------------------------------------------------------------*
      * STATUS. FROZEN ACCOUNTS ARE CLOSED ONLY TO SETTLE AN ESTATE.   *
      *----------------------------------------------------------------*
       3200-STATUS-RULES.
           EVALUATE TRUE
               WHEN ACCT-ACTIVE OR ACCT-DORMANT
                   CONTINUE
               WHEN ACCT-FROZEN
                   IF CA-REASON NOT = 'DC' OR WS-CUST-DECEASED NOT = 'Y'
                       MOVE 'ACCOUNT FROZEN - ONLY ESTATE CLOSURE (DC)'
                         TO WS-CHK-TEXT
                       PERFORM 8200-ADD-BLOCK
                   END-IF
               WHEN ACCT-RESTRICTED
                   STRING 'ACCOUNT RESTRICTED (' ACCT-RESTRICT-REASON
                          ') - REFER TO COMPLIANCE BEFORE CLOSING'
                          DELIMITED BY SIZE INTO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
               WHEN OTHER
                   MOVE 'ACCOUNT IS NOT OPEN - NOTHING TO CLOSE'
                     TO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
           END-EVALUATE
           IF ACCT-CATEGORY = 'LN' OR ACCT-CATEGORY = 'LC'
               MOVE 'LOAN ACCOUNT - PAY OFF ON CB41, NOT CLOSED HERE'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF.

      *----------------------------------------------------------------*
       3300-BALANCE-RULES.
           IF ACCT-LEDGER-BAL < ZERO
               MOVE 'ACCOUNT OVERDRAWN - CURE THE OVERDRAFT FIRST'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF
      *    MEMO POSTS TODAY ARE NOT YET COLLECTED / FINAL
           IF ACCT-MEMO-DR-TODAY NOT = ZERO
              OR ACCT-MEMO-CR-TODAY NOT = ZERO
               MOVE 'ACTIVITY TODAY - CLOSE ON THE NEXT BUSINESS DAY'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF.

      *----------------------------------------------------------------*
      * ANY ACTIVE HOLD (CHECK, LEGAL, LIEN, FRAUD ...) BLOCKS CLOSURE *
      *----------------------------------------------------------------*
       3400-HOLD-RULES.
           IF ACCT-HOLD-AMT > ZERO
               MOVE 'HOLDS OUTSTANDING ON ACCOUNT - RELEASE ON CB12'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-NUMBER TO WS-HK-ACCT
           MOVE ZERO        TO WS-HK-SEQ
           MOVE 'N' TO WS-BROWSE-FLAG
           EXEC CICS STARTBR FILE('HOLDMST') RIDFLD(WS-HOLD-KEY)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READNEXT FILE('HOLDMST') INTO(HOLD-RECORD)
                         RIDFLD(WS-HOLD-KEY) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  OR HOLD-ACCT-NUMBER NOT = ACCT-NUMBER
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   IF HOLD-ACTIVE
                       STRING 'ACTIVE ' HOLD-TYPE ' HOLD - '
                              HOLD-REASON
                              DELIMITED BY SIZE INTO WS-CHK-TEXT
                       PERFORM 8200-ADD-BLOCK
                       SET WS-BROWSE-DONE TO TRUE
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('HOLDMST') END-EXEC.

      *----------------------------------------------------------------*
      * LINKED ACCOUNTS. THE CUSTOMER'S OTHER ACCOUNTS (AIX ON THE     *
      * PRIMARY OWNER) MUST NOT DEPEND ON THIS ONE.                    *
      *----------------------------------------------------------------*
       3500-LINKED-ACCOUNT-RULES.
           IF ACCT-HAS-SWEEP OR ACCT-POOL-ID NOT = SPACES
               MOVE 'ACCOUNT IN A SWEEP / POOL - REMOVE ON CB51 FIRST'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF
           MOVE WS-CLOSE-OWNER-1 TO WS-AIX-KEY
           MOVE 'N' TO WS-BROWSE-FLAG
           EXEC CICS STARTBR FILE('ACCTCUS') RIDFLD(WS-AIX-KEY)
                     KEYLENGTH(10) EQUAL RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-BROWSE-DONE
               EXEC CICS READNEXT FILE('ACCTCUS')
                         INTO(ACCOUNT-RECORD) RIDFLD(WS-AIX-KEY)
                         KEYLENGTH(10) RESP(WS-RESP)
               END-EXEC
               IF (WS-RESP NOT = DFHRESP(NORMAL)
                   AND WS-RESP NOT = DFHRESP(DUPKEY))
                  OR ACCT-PRIMARY-CUST-ID NOT = WS-CLOSE-OWNER-1
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   IF ACCT-NUMBER NOT = CA-ACCT-NUMBER
                      AND NOT ACCT-CLOSED AND NOT ACCT-ESCHEATED
                       PERFORM 3510-CHECK-DEPENDENT
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCTCUS') END-EXEC.

       3510-CHECK-DEPENDENT.
           IF ACCT-OD-PROTECT-ACCT = CA-ACCT-NUMBER
               STRING 'OD PROTECTION SOURCE FOR ' ACCT-NUMBER
                      ' - UNLINK FIRST'
                      DELIMITED BY SIZE INTO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF
           IF ACCT-PARENT-ACCT = CA-ACCT-NUMBER
               STRING 'PARENT (ZBA / VIRTUAL) OF ' ACCT-NUMBER
                      ' - CLOSE OR RE-PARENT CHILD FIRST'
                      DELIMITED BY SIZE INTO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF
           IF ACCT-PAYOUT-ACCT = CA-ACCT-NUMBER
              AND ACCT-CATEGORY = 'TD'
               STRING 'CD ' ACCT-NUMBER ' PAYS OUT TO THIS ACCOUNT'
                      ' - CHANGE CD PAYOUT FIRST'
                      DELIMITED BY SIZE INTO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
           END-IF.

      *----------------------------------------------------------------*
      * PROCEEDS = LEDGER + INTEREST - WITHHOLDING - PENALTY - FEE     *
      *----------------------------------------------------------------*
       4000-COMPUTE-PROCEEDS.
           MOVE ZERO TO WS-INT-CENTS WS-WITHHOLD WS-PENALTY WS-FEE
      *--- 1. ACCRUED INTEREST PAID FIRST, TO THE CENT -----------------
           COMPUTE WS-INT-CENTS ROUNDED = ACCT-ACCRUED-INT
           IF WS-INT-CENTS < ZERO
               MOVE ZERO TO WS-INT-CENTS
           END-IF
           IF ACCT-BACKUP-WITHHOLDING AND WS-INT-CENTS > ZERO
               COMPUTE WS-WITHHOLD ROUNDED =
                       WS-INT-CENTS * WS-BACKUP-WH-RATE
           END-IF
      *--- 2. CD EARLY WITHDRAWAL PENALTY -----------------------------
           IF ACCT-CATEGORY = 'TD'
               PERFORM 4100-CD-PENALTY
           END-IF
      *--- 3. EARLY CLOSURE FEE (NON-CD) ------------------------------
           IF ACCT-CATEGORY NOT = 'TD'
               PERFORM 4200-EARLY-CLOSURE-FEE
           END-IF
           COMPUTE WS-AFTER-INT = ACCT-LEDGER-BAL + WS-INT-CENTS
                                - WS-WITHHOLD - WS-PENALTY
      *    THE FEE NEVER TAKES THE ACCOUNT NEGATIVE
           IF WS-FEE > WS-AFTER-INT
               IF WS-AFTER-INT > ZERO
                   MOVE WS-AFTER-INT TO WS-FEE
               ELSE
                   MOVE ZERO TO WS-FEE
               END-IF
           END-IF
           COMPUTE WS-NET = WS-AFTER-INT - WS-FEE
           MOVE WS-PENALTY TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT TO PENALTYO
           MOVE WS-FEE     TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT TO ECFEEO
           MOVE WS-NET     TO WS-EDIT-AMT
           MOVE WS-EDIT-AMT TO NETAMTO.

       4100-CD-PENALTY.
           INITIALIZE CDPENLTY-PARMS
           MOVE ACCT-PRODUCT-CODE          TO CDP-PRODUCT-CODE
           MOVE PROD-EARLY-WD-PENALTY-DAYS TO CDP-PENALTY-DAYS
           MOVE ACCT-LEDGER-BAL            TO CDP-PRINCIPAL
           MOVE ZERO                       TO CDP-WITHDRAWAL-AMT
           MOVE ACCT-INT-RATE              TO CDP-INT-RATE
           MOVE ACCT-OPEN-DATE             TO CDP-FUNDING-DATE
           MOVE ACCT-MATURITY-DATE         TO CDP-MATURITY-DATE
           MOVE ACCT-GRACE-END-DATE        TO CDP-GRACE-END-DATE
           MOVE CTL-BUSINESS-DATE          TO CDP-WITHDRAWAL-DATE
           MOVE ACCT-ACCRUED-INT           TO CDP-ACCRUED-INT
           MOVE WS-CUST-DECEASED           TO CDP-OWNER-DECEASED
           EXEC CICS LINK PROGRAM('CDPENLTY')
                     COMMAREA(CDPENLTY-PARMS)
                     LENGTH(LENGTH OF CDPENLTY-PARMS)
           END-EXEC
           EVALUATE CDP-RETURN-CODE
               WHEN 00
                   MOVE CDP-PENALTY-AMT TO WS-PENALTY
                   STRING 'CD BEFORE MATURITY - PENALTY '
                          CDP-DAYS-APPLIED ' DAYS INTEREST'
                          DELIMITED BY SIZE INTO WS-CHK-TEXT
                   PERFORM 8300-ADD-INFO
               WHEN 04
                   STRING 'CD PENALTY WAIVED - CODE ' CDP-WAIVER-CODE
                          DELIMITED BY SIZE INTO WS-CHK-TEXT
                   PERFORM 8300-ADD-INFO
               WHEN OTHER
                   MOVE 'CD PENALTY COULD NOT BE COMPUTED - SEE CB21'
                     TO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OPENED FEWER THAN 90 DAYS AGO -> 25.00. NOT CHARGED FOR REASON *
      * CODES FLAGGED FEE-WAIVED (DC BK FR) OR WITH A MANAGER WAIVER.  *
      *----------------------------------------------------------------*
       4200-EARLY-CLOSURE-FEE.
           INITIALIZE DATE-PARMS
           SET DTU-DAYS-BETWEEN TO TRUE
           MOVE ACCT-OPEN-DATE    TO DTU-DATE-1
           MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DTU-RESULT-NUMBER TO WS-DAYS-OPEN
           IF WS-DAYS-OPEN >= WS-EARLY-CLOSE-DAYS
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN WS-RSN-FOUND
                AND WS-RSN-FEE-WAIVED(WS-RSN-IDX) = 'Y'
                   STRING 'EARLY CLOSURE FEE NOT CHARGED - REASON '
                          CA-REASON
                          DELIMITED BY SIZE INTO WS-CHK-TEXT
                   PERFORM 8300-ADD-INFO
               WHEN CA-WAIVE = 'Y' AND WS-IS-MANAGER
                   MOVE 'EARLY CLOSURE FEE WAIVED BY MANAGER'
                     TO WS-CHK-TEXT
                   PERFORM 8300-ADD-INFO
               WHEN OTHER
                   MOVE WS-EARLY-CLOSE-FEE TO WS-FEE
                   MOVE 'OPENED < 90 DAYS - EARLY CLOSURE FEE APPLIES'
                     TO WS-CHK-TEXT
                   PERFORM 8300-ADD-INFO
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYOUT ACCOUNT - REQUIRED WHEN THERE ARE PROCEEDS. MUST BE AN  *
      * OPEN DEPOSIT ACCOUNT OF THE PRIMARY OR JOINT OWNER (ESTATE     *
      * CLOSURES MAY PAY ANY OPEN ACCOUNT - THE ESTATE ACCOUNT).       *
      *----------------------------------------------------------------*
       3600-PAYOUT-RULES.
           IF WS-NET NOT > ZERO
               EXIT PARAGRAPH
           END-IF
           IF CA-PAYOUT-ACCT = SPACES
               MOVE 'PAYOUT ACCOUNT REQUIRED FOR THE NET PROCEEDS'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
               EXIT PARAGRAPH
           END-IF
           IF CA-PAYOUT-ACCT = CA-ACCT-NUMBER
               MOVE 'PAYOUT ACCOUNT CANNOT BE THE ACCOUNT BEING CLOSED'
                 TO WS-CHK-TEXT
               PERFORM 8200-ADD-BLOCK
               EXIT PARAGRAPH
           END-IF
           MOVE CA-PAYOUT-ACCT TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(ACCT-NUMBER) RESP(WS-RESP)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'PAYOUT ACCOUNT NOT FOUND' TO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
               WHEN NOT ACCT-ACTIVE
                   MOVE 'PAYOUT ACCOUNT IS NOT ACTIVE' TO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
               WHEN ACCT-CATEGORY NOT = 'DD' AND NOT = 'SV'
                                       AND NOT = 'MM'
                   MOVE 'PAYOUT MUST BE CHECKING, SAVINGS OR MMDA'
                     TO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
               WHEN CA-REASON = 'DC'
                   CONTINUE
               WHEN ACCT-PRIMARY-CUST-ID NOT = WS-CLOSE-OWNER-1
                AND ACCT-PRIMARY-CUST-ID NOT = WS-CLOSE-OWNER-2
                AND ACCT-JOINT-CUST-ID   NOT = WS-CLOSE-OWNER-1
                   MOVE 'PAYOUT ACCOUNT NOT OWNED BY THE SAME CUSTOMER'
                     TO WS-CHK-TEXT
                   PERFORM 8200-ADD-BLOCK
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PF10 - RE-RUN THE QUOTE. CLOSE ONLY IF IT IS STILL CLEAN AND   *
      * THE NET AMOUNT IS UNCHANGED SINCE THE CUSTOMER WAS QUOTED.     *
      *----------------------------------------------------------------*
       6000-CONFIRM-CLOSE.
           IF NOT CA-QUOTE-OK
               MOVE LOW-VALUES TO ACCLSMO
               MOVE 'PRESS PF5 FOR A CLOSURE QUOTE BEFORE PF10'
                 TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           MOVE CA-NET TO WS-QUOTED-NET
           PERFORM 3000-QUOTE
           IF NOT CA-QUOTE-OK
               EXIT PARAGRAPH
           END-IF
           IF WS-NET NOT = WS-QUOTED-NET
               SET CA-NO-QUOTE TO TRUE
               MOVE 'PAYOUT CHANGED SINCE QUOTE - PRESS PF5 TO REQUOTE'
                 TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           PERFORM 6100-POST-CLOSING-ENTRIES
           PERFORM 6200-UPDATE-CLOSED-ACCOUNT
           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET CA-NO-QUOTE TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF WS-NET > ZERO
               PERFORM 6300-CREDIT-PAYOUT
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   SET CA-NO-QUOTE TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-IF
           EXEC CICS SYNCPOINT END-EXEC
           SET CA-NO-QUOTE TO TRUE
           MOVE 'ACCOUNT CLOSED' TO CLSSTATO
           STRING 'ACCOUNT ' CA-ACCT-NUMBER ' CLOSED - REASON '
                  CA-REASON
                  DELIMITED BY SIZE INTO WS-MESSAGE.

      *----------------------------------------------------------------*
      * JOURNAL: DINT, WBWH, WPEN, WFOT, WXFR ON THE CLOSING ACCOUNT,  *
      * DXFR ON THE PAYOUT ACCOUNT. ALREADY APPLIED (STATUS S).        *
      *----------------------------------------------------------------*
       6100-POST-CLOSING-ENTRIES.
           MOVE WS-CLOSE-ACCOUNT TO ACCOUNT-RECORD
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     TIME(WS-TIME-OUT)
           END-EXEC
           COMPUTE WS-TRAN-SEQ = EIBTASKN * 10
           MOVE ZERO TO WS-SEQ-SUFFIX
           IF WS-INT-CENTS > ZERO
               INITIALIZE TRANSACTION-RECORD
               MOVE 'DINT' TO TRAN-CODE
               MOVE 'C'    TO TRAN-DR-CR
               MOVE WS-INT-CENTS TO TRAN-AMOUNT
               MOVE 'INTEREST PAID AT CLOSING' TO TRAN-DESCRIPTION
               PERFORM 6110-WRITE-ENTRY
           END-IF
           IF WS-WITHHOLD > ZERO
               INITIALIZE TRANSACTION-RECORD
               MOVE 'WBWH' TO TRAN-CODE
               MOVE 'D'    TO TRAN-DR-CR
               MOVE WS-WITHHOLD TO TRAN-AMOUNT
               MOVE 'BACKUP WITHHOLDING 24%' TO TRAN-DESCRIPTION
               PERFORM 6110-WRITE-ENTRY
           END-IF
           IF WS-PENALTY > ZERO
               INITIALIZE TRANSACTION-RECORD
               MOVE 'WPEN' TO TRAN-CODE
               MOVE 'D'    TO TRAN-DR-CR
               MOVE WS-PENALTY TO TRAN-AMOUNT
               MOVE 'CD EARLY WITHDRAWAL PENALTY' TO TRAN-DESCRIPTION
               PERFORM 6110-WRITE-ENTRY
           END-IF
           IF WS-FEE > ZERO
               INITIALIZE TRANSACTION-RECORD
               MOVE 'WFOT' TO TRAN-CODE
               MOVE 'D'    TO TRAN-DR-CR
               MOVE WS-FEE TO TRAN-AMOUNT
               MOVE WS-FEE TO TRAN-FEE-AMOUNT
               MOVE 'EARLY ACCOUNT CLOSURE FEE' TO TRAN-DESCRIPTION
               PERFORM 6110-WRITE-ENTRY
           END-IF
           IF WS-NET > ZERO
               INITIALIZE TRANSACTION-RECORD
               MOVE 'WXFR' TO TRAN-CODE
               MOVE 'D'    TO TRAN-DR-CR
               MOVE WS-NET TO TRAN-AMOUNT
               MOVE CA-PAYOUT-ACCT TO TRAN-COUNTER-ACCT
               MOVE 'CLOSING BALANCE TRANSFER' TO TRAN-DESCRIPTION
               PERFORM 6110-WRITE-ENTRY
               INITIALIZE TRANSACTION-RECORD
               MOVE 'DXFR' TO TRAN-CODE
               MOVE 'C'    TO TRAN-DR-CR
               MOVE WS-NET TO TRAN-AMOUNT
               MOVE CA-ACCT-NUMBER TO TRAN-COUNTER-ACCT
               MOVE 'PROCEEDS OF CLOSED ACCOUNT' TO TRAN-DESCRIPTION
               MOVE CA-PAYOUT-ACCT TO TRAN-ACCT-NUMBER
               PERFORM 6110-WRITE-ENTRY
           END-IF.

       6110-WRITE-ENTRY.
           ADD 1 TO WS-SEQ-SUFFIX
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE TRAN-EFFECTIVE-DATE
                                     TRAN-POST-DATE
           COMPUTE TRAN-ID-SEQ = WS-TRAN-SEQ + WS-SEQ-SUFFIX
           IF TRAN-ACCT-NUMBER = SPACES OR LOW-VALUES
               MOVE CA-ACCT-NUMBER TO TRAN-ACCT-NUMBER
           END-IF
           MOVE WS-TIME-OUT       TO TRAN-TIME
           MOVE 'BRN'             TO TRAN-CHANNEL
           MOVE CA-REASON         TO TRAN-REFERENCE
           SET TRAN-POSTED        TO TRUE
           MOVE WS-USERID         TO TRAN-USER-ID
           MOVE ACCT-COST-CENTER  TO TRAN-COST-CENTER
           MOVE 'N'               TO TRAN-CASH-FLAG TRAN-REGD-FLAG
                                     TRAN-OD-FLAG
           MOVE 'ACCTCLS'         TO TRAN-SOURCE-PGM
           EXEC CICS WRITEQ TD QUEUE('TRNP')
                     FROM(TRANSACTION-RECORD)
                     LENGTH(LENGTH OF TRANSACTION-RECORD)
           END-EXEC.

       6200-UPDATE-CLOSED-ACCOUNT.
           MOVE CA-ACCT-NUMBER TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(ACCT-NUMBER) UPDATE RESP(WS-RESP)
           END-EXEC
           ADD WS-INT-CENTS  TO ACCT-INT-PAID-YTD
           ADD WS-WITHHOLD   TO ACCT-WITHHELD-YTD
           ADD WS-FEE        TO ACCT-FEES-MTD ACCT-FEES-YTD
           MOVE ZERO TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
                        ACCT-ACCRUED-INT ACCT-OD-LIMIT
           MOVE 'N'               TO ACCT-OD-OPT-IN
           SET ACCT-CLOSED        TO TRUE
           MOVE SPACES            TO ACCT-RESTRICT-REASON
           MOVE CTL-BUSINESS-DATE TO ACCT-CLOSE-DATE
                                     ACCT-LAST-MAINT-DATE
           MOVE WS-USERID         TO ACCT-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE 'CLOSE FAILED - ACCTMST REWRITE ERROR, BACKED OUT'
                 TO WS-MESSAGE
           END-IF.

       6300-CREDIT-PAYOUT.
           MOVE CA-PAYOUT-ACCT TO ACCT-NUMBER
           EXEC CICS READ FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(ACCT-NUMBER) UPDATE RESP(WS-RESP)
           END-EXEC
           ADD WS-NET TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
           MOVE CTL-BUSINESS-DATE TO ACCT-LAST-MAINT-DATE
           MOVE WS-USERID         TO ACCT-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
               MOVE 'CLOSE FAILED - PAYOUT CREDIT ERROR, BACKED OUT'
                 TO WS-MESSAGE
           END-IF.

      *----------------------------------------------------------------*
       8000-SEND-MAP.
           MOVE WS-MESSAGE TO MSGO
           MOVE 'ENTER=DISPLAY PF5=QUOTE PF10=CLOSE PF3=MENU PF12=CNCL'
             TO PFKEYSO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT) DATESEP('/')
           END-EXEC
           MOVE WS-DATE-OUT TO CURDATEO
           MOVE -1 TO ACCTNOL
           EXEC CICS SEND MAP('ACCLSM') MAPSET('ACCLSS')
                     FROM(ACCLSMO) ERASE CURSOR
           END-EXEC.

       8100-EDIT-DATE.
           IF WS-DATE-IN = ZERO
               MOVE SPACES TO WS-EDIT-DATE
           ELSE
               MOVE WS-DI-CCYY TO WS-ED-CCYY
               MOVE WS-DI-MM   TO WS-ED-MM
               MOVE WS-DI-DD   TO WS-ED-DD
           END-IF.

      *    BLOCKING CHECK - LISTED AND PREVENTS THE CLOSE
       8200-ADD-BLOCK.
           SET WS-CLOSE-BLOCKED TO TRUE
           PERFORM 8300-ADD-INFO.

      *    INFORMATIONAL OR BLOCKING LINE - FIRST 5 ARE SHOWN
       8300-ADD-INFO.
           ADD 1 TO WS-CHK-CNT
           EVALUATE WS-CHK-CNT
               WHEN 1 MOVE WS-CHK-TEXT TO CHK01O
               WHEN 2 MOVE WS-CHK-TEXT TO CHK02O
               WHEN 3 MOVE WS-CHK-TEXT TO CHK03O
               WHEN 4 MOVE WS-CHK-TEXT TO CHK04O
               WHEN 5 MOVE WS-CHK-TEXT TO CHK05O
               WHEN OTHER CONTINUE
           END-EVALUATE
           MOVE SPACES TO WS-CHK-TEXT.
