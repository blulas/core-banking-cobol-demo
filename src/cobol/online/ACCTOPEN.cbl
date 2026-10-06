      *================================================================*
      * PROGRAM  : ACCTOPEN                                            *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANS    : CB03                                                *
      * MAPSET   : ACOPNS  MAP ACOPNM                                  *
      * PURPOSE  : DEPOSIT ACCOUNT OPENING.                            *
      *            ENTER = VALIDATE AND QUOTE (ELIGCHK + RATETIER)     *
      *            PF5   = CONFIRM AND OPEN (AFTER A CLEAN QUOTE)      *
      *            - ELIGIBILITY FROM ELIGCHK (SAME MODULE AS THE API) *
      *            - JOINT OWNER SCREENED INLINE (AGE, KYC, OFAC)      *
      *            - NEXT ACCOUNT NUMBER FROM ACCTMST HIGH KEY UNDER   *
      *              ENQ (RETAIL 1XXXXXXXXXXX, CORPORATE 2XXXXXXXXXXX) *
      *            - REG E: OVERDRAFT OPT-IN DEFAULTS TO N FOR DDAP;   *
      *              ONLY AN EXPLICIT Y OPTS THE CUSTOMER IN           *
      *            - RATE / APY FROM RATETIER (INTEREST DOMAIN)        *
      *            - NEW-TO-BANK HYSV: 5.00% PROMO FOR 3 MONTHS        *
      *            - KYC-PENDING HYSV OPENS RESTRICTED (REASON KP)     *
      *            - INITIAL DEPOSIT BY TRANSFER IS QUEUED TO TD QUEUE *
      *              TRNI FOR THE EOD TRANSACTION EDIT; CASH DEPOSITS  *
      *              ARE TAKEN AT THE TELLER (CB11)                    *
      * FILES    : CUSTMST, PRODMST, ACCTMST, ACCTCUS (AIX), BANKCTL   *
      * CALLS    : ELIGCHK, RATETIER, KYCTIER, DATEUTIL                *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-20 RJT  ORIGINAL                                       *
      * 2003-01-20 BHS  ELIGIBILITY VIA ELIGCHK                        *
      * 2005-02-28 BHS  JOINT OWNER OFAC CHECK                         *
      * 2009-11-17 DPS  REG E - OD OPT-IN DEFAULT N (REG-2009-E)       *
      * 2012-02-27 DPS  JOINT OWNER MUST BE AN ADULT (CHG0601)         *
      * 2017-02-08 AKW  HYSV PROMO RATE, RATE FROM RATETIER            *
      * 2017-03-14 AKW  KYC-PENDING HYSV CAP PRE-EDIT (DIGITAL)        *
      * 2019-08-19 AKW  RELATIONSHIP BALANCE PASSED TO RATETIER        *
      * 2023-04-17 TKO  CORPORATE ACCOUNT NUMBER RANGE 2XXXXXXXXXXX    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTOPEN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB03'.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-MESSAGE                      PIC X(79).
       01  WS-CTL-KEY                      PIC X(08) VALUE 'BANKCTL1'.
       01  WS-USERID                       PIC X(08).
       01  WS-ABSTIME                      PIC S9(15) COMP-3.
       01  WS-DATE-OUT                     PIC X(10).
       01  WS-TIME-OUT                     PIC 9(06).

       01  WS-FLAGS.
           05  WS-ERROR-FLAG               PIC X(01).
               88  WS-INPUT-ERROR                    VALUE 'Y'.
           05  WS-BROWSE-FLAG              PIC X(01).
               88  WS-BROWSE-DONE                    VALUE 'Y'.

      *--- OPENING DEFAULTS -------------------------------------------*
       01  WS-OPEN-RULES.
           05  WS-KYC-PENDING-HYSV-MAX     PIC S9(13)V99 COMP-3
                                           VALUE 10000.00.
           05  WS-JOINT-MIN-AGE            PIC 9(03) VALUE 018.
           05  WS-DDAP-OD-LIMIT            PIC S9(09)V99 COMP-3
                                           VALUE 300.00.
           05  WS-DDPR-OD-LIMIT            PIC S9(09)V99 COMP-3
                                           VALUE 1000.00.
           05  WS-NEW-TO-BANK-DAYS         PIC S9(05) VALUE +30.
           05  WS-HYSV-PROMO-RATE          PIC 9(03)V9(06)
                                           VALUE 5.000000.
           05  WS-HYSV-PROMO-MONTHS        PIC S9(05) VALUE +3.
           05  WS-CD-GRACE-DAYS            PIC S9(05) VALUE +10.

       01  WS-WORK.
           05  WS-DEPOSIT-AMT              PIC S9(13)V99 COMP-3.
           05  WS-HYSV-COUNT               PIC 9(03).
           05  WS-SAME-PROD-COUNT          PIC 9(03).
           05  WS-AIX-KEY                  PIC X(10).
           05  WS-JOINT-AGE                PIC S9(04).
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-R REDEFINES WS-TODAY.
               10  WS-TODAY-CCYY           PIC 9(04).
               10  WS-TODAY-MMDD           PIC 9(04).
           05  WS-JBIRTH                   PIC 9(08).
           05  WS-JBIRTH-R REDEFINES WS-JBIRTH.
               10  WS-JBIRTH-CCYY          PIC 9(04).
               10  WS-JBIRTH-MMDD          PIC 9(04).
           05  WS-NEW-ACCT-NUM             PIC 9(12).
           05  WS-HIGH-KEY                 PIC X(12).
           05  WS-SUB                      PIC 9(02).
           05  WS-DECLINE-TEXT             PIC X(60).
           05  WS-DECL-PTR                 PIC 9(03).
           05  WS-PROMO-RATE               PIC 9(03)V9(06).
           05  WS-PROMO-EXP                PIC 9(08).
           05  WS-EDIT-RATE                PIC ZZ9.999999.
           05  WS-EDIT-AMT                 PIC ZZZ,ZZZ,ZZ9.99.
           05  WS-OD-OPT-IN                PIC X(01).
           05  WS-OD-LIMIT                 PIC S9(09)V99 COMP-3.
           05  WS-TRAN-SEQ                 PIC 9(08).

      *--- DECLINE CODE DESCRIPTIONS (SEE ELIGPARM) -------------------*
       01  WS-DECLINE-DESC-VALUES.
           05  FILLER PIC X(23) VALUE 'E01NOT A DEPOSIT PROD  '.
           05  FILLER PIC X(23) VALUE 'E02PRODUCT RETIRED     '.
           05  FILLER PIC X(23) VALUE 'E03WRONG CUSTOMER TYPE '.
           05  FILLER PIC X(23) VALUE 'E04CUSTOMER STATUS     '.
           05  FILLER PIC X(23) VALUE 'E05KYC NOT VERIFIED    '.
           05  FILLER PIC X(23) VALUE 'E06KYC PENDING LIMIT   '.
           05  FILLER PIC X(23) VALUE 'E07BELOW MIN DEPOSIT   '.
           05  FILLER PIC X(23) VALUE 'E08UNDER AGE 18        '.
           05  FILLER PIC X(23) VALUE 'E09MAX ACCOUNTS HELD   '.
           05  FILLER PIC X(23) VALUE 'E10SEGMENT NOT CB/IN   '.
           05  FILLER PIC X(23) VALUE 'E11OFAC NOT CLEAR      '.
           05  FILLER PIC X(23) VALUE 'E12RISK PROHIBITED     '.
           05  FILLER PIC X(23) VALUE 'E13INVALID BIRTH DATE  '.
           05  FILLER PIC X(23) VALUE 'E14KYC REVIEW OVERDUE  '.
       01  WS-DECLINE-DESC-TABLE REDEFINES WS-DECLINE-DESC-VALUES.
           05  WS-DD-ENTRY OCCURS 14 TIMES INDEXED BY WS-DD-IDX.
               10  WS-DD-CODE              PIC X(03).
               10  WS-DD-TEXT              PIC X(20).

       01  WS-COMMAREA.
           05  CA-STATE                    PIC X(01).
               88  CA-NO-QUOTE                       VALUE ' '.
               88  CA-QUOTE-OK                       VALUE 'Q'.
           05  CA-CUST-ID                  PIC X(10).
           05  CA-PRODUCT                  PIC X(04).
           05  CA-JOINT-ID                 PIC X(10).
           05  CA-DEPOSIT                  PIC S9(13)V99 COMP-3.
           05  CA-RATE                     PIC 9(03)V9(06).
           05  CA-APY                      PIC 9(03)V9(06).
           05  CA-PROMO-RATE               PIC 9(03)V9(06).
           05  CA-PROMO-EXP                PIC 9(08).
           05  CA-OD-OPT-IN                PIC X(01).
           05  CA-OD-LIMIT                 PIC S9(09)V99 COMP-3.
           05  CA-OWNERSHIP                PIC X(01).
           05  CA-FUND-ACCT                PIC X(12).
           05  CA-RENEW                    PIC X(01).
           05  CA-PAYOUT                   PIC X(12).

       COPY CUSTREC.
       01  WS-JOINT-CUSTOMER.
           05  WS-JC-DATA                  PIC X(400).
       COPY PRODREC.
       COPY ACCTREC.
       COPY TRANREC.
       COPY BANKCTL.
       COPY DATEPARM.
       COPY ELIGPARM.
       COPY RTTPARM.
       COPY KYCTPARM.
       COPY ACOPNS.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(95).

       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
           EXEC CICS READ FILE('BANKCTL') INTO(BANK-CONTROL-RECORD)
                     RIDFLD(WS-CTL-KEY) RESP(WS-RESP)
           END-EXEC
           MOVE CTL-BUSINESS-DATE TO WS-TODAY
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               MOVE LOW-VALUES TO ACOPNMO
               MOVE 'ENTER CUSTOMER, PRODUCT AND DEPOSIT - PRESS ENTER'
                 TO WS-MESSAGE
               PERFORM 8000-SEND-MAP
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       EXEC CICS XCTL PROGRAM('CBMENU') END-EXEC
                   WHEN DFHPF12
                   WHEN DFHCLEAR
                       INITIALIZE WS-COMMAREA
                       MOVE LOW-VALUES TO ACOPNMO
                       MOVE 'CANCELLED' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
                   WHEN DFHENTER
                       PERFORM 1000-RECEIVE-AND-QUOTE
                   WHEN DFHPF5
                       IF CA-QUOTE-OK
                           PERFORM 1000-RECEIVE-AND-QUOTE
                           IF CA-QUOTE-OK
                               PERFORM 5000-OPEN-ACCOUNT
                           END-IF
                       ELSE
                           MOVE 'PRESS ENTER TO VALIDATE BEFORE PF5'
                             TO WS-MESSAGE
                           PERFORM 8000-SEND-MAP
                       END-IF
                   WHEN OTHER
                       MOVE 'INVALID KEY' TO WS-MESSAGE
                       PERFORM 8000-SEND-MAP
               END-EVALUATE
           END-IF
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
       1000-RECEIVE-AND-QUOTE.
           EXEC CICS RECEIVE MAP('ACOPNM') MAPSET('ACOPNS')
                     INTO(ACOPNMI) RESP(WS-RESP)
           END-EXEC
           MOVE 'N' TO WS-ERROR-FLAG
           SET CA-NO-QUOTE TO TRUE
           MOVE CUSTIDI  TO CA-CUST-ID
           MOVE PRODCDI  TO CA-PRODUCT
           MOVE JOINTIDI TO CA-JOINT-ID
           IF JOINTIDL = 0 OR JOINTIDI = LOW-VALUES
               MOVE SPACES TO CA-JOINT-ID
           END-IF
           MOVE FUNDACTI TO CA-FUND-ACCT
           MOVE RENEWI   TO CA-RENEW
           MOVE PAYACTI  TO CA-PAYOUT
           MOVE OWNTYPI  TO CA-OWNERSHIP
           IF DEPAMTL > 0
               COMPUTE WS-DEPOSIT-AMT = FUNCTION NUMVAL(DEPAMTI)
           ELSE
               MOVE ZERO TO WS-DEPOSIT-AMT
           END-IF
           MOVE WS-DEPOSIT-AMT TO CA-DEPOSIT
           MOVE LOW-VALUES TO ACOPNMO
           PERFORM 2000-READ-CUSTOMER-PRODUCT
           IF NOT WS-INPUT-ERROR
               PERFORM 2500-PRE-EDITS
           END-IF
           IF NOT WS-INPUT-ERROR AND CA-JOINT-ID NOT = SPACES
               PERFORM 2600-JOINT-OWNER-CHECKS
           END-IF
           IF NOT WS-INPUT-ERROR
               PERFORM 3000-ELIGIBILITY
           END-IF
           IF NOT WS-INPUT-ERROR
               PERFORM 4000-PRICE-AND-DEFAULTS
               SET CA-QUOTE-OK TO TRUE
               MOVE 'ELIGIBLE - REVIEW TERMS, PRESS PF5 TO OPEN'
                 TO WS-MESSAGE
           END-IF
           PERFORM 8000-SEND-MAP.

      *----------------------------------------------------------------*
       2000-READ-CUSTOMER-PRODUCT.
           MOVE CA-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'CUSTOMER NOT FOUND' TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               MOVE -1 TO CUSTIDL
               EXIT PARAGRAPH
           END-IF
           STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                  FUNCTION TRIM(CUST-LAST-NAME)
                  DELIMITED BY SIZE INTO CNAMEO
           MOVE CA-PRODUCT TO PROD-CODE
           EXEC CICS READ FILE('PRODMST') INTO(PRODUCT-RECORD)
                     RIDFLD(PROD-CODE) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'PRODUCT CODE NOT FOUND' TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               MOVE -1 TO PRODCDL
               EXIT PARAGRAPH
           END-IF
           MOVE PROD-NAME TO PRODNMO.

      *----------------------------------------------------------------*
      * SCREEN PRE-EDITS (BEFORE ELIGCHK)                              *
      *----------------------------------------------------------------*
       2500-PRE-EDITS.
      *    DIGITAL ONBOARDING CAP FOR CUSTOMERS STILL IN CIP
           IF CUST-KYC-PENDING AND CA-PRODUCT = 'HYSV'
              AND WS-DEPOSIT-AMT >= WS-KYC-PENDING-HYSV-MAX
               MOVE 'KYC PENDING - HYSV OPENING DEPOSIT MUST BE UNDER 1
      -             '0,000.00' TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               MOVE -1 TO DEPAMTL
           END-IF
           IF CA-OWNERSHIP = 'J' AND CA-JOINT-ID = SPACES
               MOVE 'JOINT OWNERSHIP REQUIRES A JOINT OWNER ID'
                 TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               MOVE -1 TO JOINTIDL
           END-IF
           IF PROD-TIME-DEPOSIT
               IF CA-RENEW NOT = 'A' AND NOT = 'M' AND NOT = 'C'
                   MOVE 'CD RENEWAL OPTION MUST BE A, M OR C'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
                   MOVE -1 TO RENEWL
               END-IF
               IF (CA-RENEW = 'M' OR CA-RENEW = 'C')
                  AND (CA-PAYOUT = SPACES OR LOW-VALUES)
                   MOVE 'PAYOUT ACCOUNT REQUIRED FOR RENEWAL M OR C'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
                   MOVE -1 TO PAYACTL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * JOINT OWNER - MUST EXIST, BE AN INDIVIDUAL ADULT, KYC VERIFIED *
      * AND OFAC CLEAR (ELIGCHK ONLY SCREENS THE PRIMARY OWNER)        *
      *----------------------------------------------------------------*
       2600-JOINT-OWNER-CHECKS.
           EXEC CICS READ FILE('CUSTMST') INTO(WS-JOINT-CUSTOMER)
                     RIDFLD(CA-JOINT-ID) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'JOINT OWNER NOT FOUND' TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
               MOVE -1 TO JOINTIDL
               EXIT PARAGRAPH
           END-IF
      *    SWAP IN THE JOINT OWNER, CHECK, THEN RESTORE THE PRIMARY
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CA-JOINT-ID) RESP(WS-RESP)
           END-EXEC
           STRING FUNCTION TRIM(CUST-FIRST-NAME) ' '
                  FUNCTION TRIM(CUST-LAST-NAME)
                  DELIMITED BY SIZE INTO JNAMEO
           MOVE CUST-BIRTH-DATE TO WS-JBIRTH
           COMPUTE WS-JOINT-AGE = WS-TODAY-CCYY - WS-JBIRTH-CCYY
           EVALUATE TRUE
               WHEN NOT CUST-INDIVIDUAL
                   MOVE 'JOINT OWNER MUST BE AN INDIVIDUAL'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
               WHEN WS-JOINT-AGE < WS-JOINT-MIN-AGE
                   MOVE 'JOINT OWNER MUST BE 18 OR OLDER'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
               WHEN NOT CUST-KYC-VERIFIED
                   MOVE 'JOINT OWNER KYC NOT VERIFIED' TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
               WHEN NOT CUST-OFAC-CLEAR
                   MOVE 'JOINT OWNER OFAC NOT CLEAR - REFER TO CMP'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
               WHEN CUST-DECEASED OR CUST-BLOCKED
                   MOVE 'JOINT OWNER STATUS DOES NOT PERMIT OPENING'
                     TO WS-MESSAGE
                   SET WS-INPUT-ERROR TO TRUE
           END-EVALUATE
           MOVE CA-CUST-ID TO CUST-ID
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) RESP(WS-RESP)
           END-EXEC.

      *----------------------------------------------------------------*
      * ELIGCHK - COUNT EXISTING OPEN ACCOUNTS OF THIS PRODUCT FIRST   *
      *----------------------------------------------------------------*
       3000-ELIGIBILITY.
           PERFORM 3100-COUNT-SAME-PRODUCT
           INITIALIZE ELIGCHK-PARMS
           MOVE PROD-CODE              TO ELG-PRODUCT-CODE
           MOVE PROD-CATEGORY          TO ELG-PROD-CATEGORY
           MOVE PROD-CUST-TYPE-ALLOWED TO ELG-PROD-CUST-TYPE
           MOVE PROD-STATUS            TO ELG-PROD-STATUS
           MOVE PROD-EFF-DATE          TO ELG-PROD-EFF-DATE
           MOVE PROD-MIN-OPEN-AMT      TO ELG-PROD-MIN-OPEN-AMT
           MOVE CUST-TYPE              TO ELG-CUST-TYPE
           MOVE CUST-STATUS            TO ELG-CUST-STATUS
           MOVE CUST-KYC-STATUS        TO ELG-KYC-STATUS
           MOVE CUST-KYC-REVIEW-DATE   TO ELG-KYC-REVIEW-DATE
           MOVE CUST-BIRTH-DATE        TO ELG-BIRTH-DATE
           MOVE CUST-SEGMENT           TO ELG-SEGMENT
           MOVE CUST-OFAC-STATUS       TO ELG-OFAC-STATUS
           MOVE CUST-RISK-RATING       TO ELG-RISK-RATING
           MOVE WS-DEPOSIT-AMT         TO ELG-OPEN-AMOUNT
           MOVE WS-SAME-PROD-COUNT     TO ELG-EXISTING-SAME-PROD
           MOVE CTL-BUSINESS-DATE      TO ELG-AS-OF-DATE
           EXEC CICS LINK PROGRAM('ELIGCHK')
                     COMMAREA(ELIGCHK-PARMS)
                     LENGTH(LENGTH OF ELIGCHK-PARMS)
           END-EXEC
           IF ELG-IS-ELIGIBLE
               MOVE 'ELIGIBLE' TO ELIGDSCO
           ELSE
               SET WS-INPUT-ERROR TO TRUE
               MOVE 'NOT ELIGIBLE' TO ELIGDSCO
               MOVE SPACES TO WS-DECLINE-TEXT
               MOVE 1 TO WS-DECL-PTR
               PERFORM VARYING WS-SUB FROM 1 BY 1
                       UNTIL WS-SUB > ELG-DECLINE-COUNT
                          OR WS-SUB > 6
                   SET WS-DD-IDX TO 1
                   SEARCH WS-DD-ENTRY
                       AT END CONTINUE
                       WHEN WS-DD-CODE(WS-DD-IDX) =
                            ELG-DECLINE-CODE(WS-SUB)
                           STRING ELG-DECLINE-CODE(WS-SUB) ' '
                                  WS-DD-TEXT(WS-DD-IDX) ' '
                                  DELIMITED BY SIZE
                                  INTO WS-DECLINE-TEXT
                                  WITH POINTER WS-DECL-PTR
                   END-SEARCH
                   IF WS-SUB = 3
                       MOVE WS-DECLINE-TEXT TO DECLN1O
                       MOVE SPACES TO WS-DECLINE-TEXT
                       MOVE 1 TO WS-DECL-PTR
                   END-IF
               END-PERFORM
               IF ELG-DECLINE-COUNT > 3
                   MOVE WS-DECLINE-TEXT TO DECLN2O
               ELSE
                   IF ELG-DECLINE-COUNT < 3
                       MOVE WS-DECLINE-TEXT TO DECLN1O
                   END-IF
               END-IF
               MOVE 'ACCOUNT CANNOT BE OPENED - SEE DECLINE REASONS'
                 TO WS-MESSAGE
           END-IF.

       3100-COUNT-SAME-PRODUCT.
           MOVE ZERO TO WS-SAME-PROD-COUNT
           MOVE CUST-ID TO WS-AIX-KEY
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
                  OR ACCT-PRIMARY-CUST-ID NOT = CUST-ID
                   SET WS-BROWSE-DONE TO TRUE
               ELSE
                   IF ACCT-PRODUCT-CODE = PROD-CODE
                      AND NOT ACCT-CLOSED AND NOT ACCT-ESCHEATED
                       ADD 1 TO WS-SAME-PROD-COUNT
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('ACCTCUS') END-EXEC.

      *----------------------------------------------------------------*
      * RATE / APY (RATETIER), PROMO, OVERDRAFT DEFAULTS, CD MATURITY  *
      *----------------------------------------------------------------*
       4000-PRICE-AND-DEFAULTS.
           MOVE ZERO TO WS-PROMO-RATE WS-PROMO-EXP
      *    NEW-TO-BANK HYSV PROMOTION - CUSTOMER SINCE <= 30 DAYS
           IF PROD-CODE = 'HYSV'
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CUST-SINCE-DATE   TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-RESULT-NUMBER <= WS-NEW-TO-BANK-DAYS
                   MOVE WS-HYSV-PROMO-RATE TO WS-PROMO-RATE
                   INITIALIZE DATE-PARMS
                   SET DTU-ADD-MONTHS TO TRUE
                   MOVE CTL-BUSINESS-DATE    TO DTU-DATE-1
                   MOVE WS-HYSV-PROMO-MONTHS TO DTU-NUMBER
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DTU-RESULT-DATE TO WS-PROMO-EXP
               END-IF
           END-IF
           INITIALIZE RATETIER-PARMS
           MOVE PROD-CODE              TO RTT-PRODUCT-CODE
           MOVE WS-DEPOSIT-AMT         TO RTT-BALANCE
           MOVE CUST-TOTAL-REL-BALANCE TO RTT-REL-BALANCE
           MOVE CUST-SEGMENT           TO RTT-CUST-SEGMENT
           MOVE PROD-TERM-MONTHS       TO RTT-TERM-MONTHS
           MOVE CTL-BUSINESS-DATE      TO RTT-AS-OF-DATE
           MOVE WS-PROMO-RATE          TO RTT-PROMO-RATE
           MOVE WS-PROMO-EXP           TO RTT-PROMO-EXP-DATE
           MOVE CUST-EMPLOYEE-FLAG     TO RTT-EMPLOYEE-FLAG
           CALL 'RATETIER' USING RATETIER-PARMS
           IF RTT-RETURN-CODE > 08
               MOVE 'RATE TABLE NOT FOUND - CONTACT DEPOSIT OPS'
                 TO WS-MESSAGE
               SET WS-INPUT-ERROR TO TRUE
           END-IF
           MOVE RTT-RESULT-RATE TO CA-RATE WS-EDIT-RATE
           MOVE WS-EDIT-RATE    TO RATEO
           MOVE RTT-RESULT-APY  TO CA-APY WS-EDIT-RATE
           MOVE WS-EDIT-RATE    TO APYO
           MOVE RTT-TIER-ID     TO RTTIERO
           MOVE WS-PROMO-RATE   TO CA-PROMO-RATE
           MOVE WS-PROMO-EXP    TO CA-PROMO-EXP
      *    REG E (12 CFR 1005.17) - OPT-IN MUST BE AFFIRMATIVE. ANY
      *    VALUE OTHER THAN 'Y' (INCLUDING BLANK) IS TREATED AS 'N'.
           MOVE 'N'  TO WS-OD-OPT-IN
           MOVE ZERO TO WS-OD-LIMIT
           IF PROD-ALLOWS-OD
               IF ODOPTINI = 'Y'
                   MOVE 'Y' TO WS-OD-OPT-IN
               END-IF
               EVALUATE PROD-CODE
                   WHEN 'DDAP'
                       IF WS-OD-OPT-IN = 'Y'
                           MOVE WS-DDAP-OD-LIMIT TO WS-OD-LIMIT
                       END-IF
                   WHEN 'DDPR'
                       MOVE WS-DDPR-OD-LIMIT TO WS-OD-LIMIT
               END-EVALUATE
           END-IF
           MOVE WS-OD-OPT-IN TO CA-OD-OPT-IN ODOPTINO
           MOVE WS-OD-LIMIT  TO CA-OD-LIMIT WS-EDIT-AMT
           MOVE WS-EDIT-AMT  TO ODLIMO
           IF PROD-TIME-DEPOSIT
               INITIALIZE DATE-PARMS
               SET DTU-ADD-MONTHS TO TRUE
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
               MOVE PROD-TERM-MONTHS  TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO MATDTO
           END-IF.

      *----------------------------------------------------------------*
      * PF5 - ASSIGN NUMBER, WRITE ACCTMST, QUEUE FUNDING TRANSFER     *
      *----------------------------------------------------------------*
       5000-OPEN-ACCOUNT.
           EXEC CICS ENQ RESOURCE('CBSACCTNUM') LENGTH(10) END-EXEC
           PERFORM 5100-NEXT-ACCOUNT-NUMBER
           PERFORM 5200-BUILD-ACCOUNT
           EXEC CICS WRITE FILE('ACCTMST') FROM(ACCOUNT-RECORD)
                     RIDFLD(ACCT-NUMBER) RESP(WS-RESP)
           END-EXEC
           EXEC CICS DEQ RESOURCE('CBSACCTNUM') LENGTH(10) END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ACCOUNT WRITE FAILED - NOT OPENED' TO WS-MESSAGE
               SET CA-NO-QUOTE TO TRUE
               PERFORM 8000-SEND-MAP
               EXIT PARAGRAPH
           END-IF
           PERFORM 5300-ACTIVATE-PROSPECT
           IF CA-FUND-ACCT NOT = SPACES AND NOT = LOW-VALUES
              AND CA-DEPOSIT > ZERO
               PERFORM 5400-QUEUE-FUNDING
           END-IF
           MOVE ACCT-NUMBER TO NEWACCTO
           MOVE CUST-KYC-TIER TO KTIERO
           MOVE CUST-KYC-REVIEW-DATE TO KREVDTO
           IF ACCT-RESTRICTED
               MOVE 'OPENED RESTRICTED (KYC PENDING) - NO DEBITS'
                 TO WS-MESSAGE
           ELSE
               STRING 'ACCOUNT ' ACCT-NUMBER ' OPENED'
                      DELIMITED BY SIZE INTO WS-MESSAGE
           END-IF
           SET CA-NO-QUOTE TO TRUE
           PERFORM 8000-SEND-MAP.

       5100-NEXT-ACCOUNT-NUMBER.
           IF CUST-CORPORATE
               MOVE '299999999999' TO WS-HIGH-KEY
           ELSE
               MOVE '199999999999' TO WS-HIGH-KEY
           END-IF
           EXEC CICS STARTBR FILE('ACCTMST') RIDFLD(WS-HIGH-KEY)
                     GTEQ RESP(WS-RESP)
           END-EXEC
           EXEC CICS READPREV FILE('ACCTMST') INTO(ACCOUNT-RECORD)
                     RIDFLD(WS-HIGH-KEY) RESP(WS-RESP)
           END-EXEC
      *    READPREV AFTER STARTBR GTEQ RETURNS THE START RECORD FIRST
           IF WS-HIGH-KEY(1:1) = '2' OR WS-HIGH-KEY > '199999999999'
               EXEC CICS READPREV FILE('ACCTMST')
                         INTO(ACCOUNT-RECORD)
                         RIDFLD(WS-HIGH-KEY) RESP(WS-RESP)
               END-EXEC
           END-IF
           EXEC CICS ENDBR FILE('ACCTMST') END-EXEC
           MOVE WS-HIGH-KEY TO WS-NEW-ACCT-NUM
           ADD 1 TO WS-NEW-ACCT-NUM.

       5200-BUILD-ACCOUNT.
           INITIALIZE ACCOUNT-RECORD
           MOVE WS-NEW-ACCT-NUM    TO ACCT-NUMBER
           MOVE PROD-CODE          TO ACCT-PRODUCT-CODE
           MOVE PROD-CATEGORY      TO ACCT-CATEGORY
           MOVE CUST-ID            TO ACCT-PRIMARY-CUST-ID
           MOVE CA-JOINT-ID        TO ACCT-JOINT-CUST-ID
           EVALUATE TRUE
               WHEN CA-JOINT-ID NOT = SPACES
                   SET ACCT-OWN-JOINT TO TRUE
               WHEN CUST-TRUST
                   SET ACCT-OWN-TRUST TO TRUE
               WHEN CUST-CORPORATE
                   SET ACCT-OWN-CORPORATE TO TRUE
               WHEN OTHER
                   SET ACCT-OWN-SINGLE TO TRUE
           END-EVALUATE
           MOVE 'USD'              TO ACCT-CURRENCY
           SET ACCT-ACTIVE TO TRUE
           IF CUST-KYC-PENDING
               SET ACCT-RESTRICTED TO TRUE
               MOVE 'KP' TO ACCT-RESTRICT-REASON
           END-IF
           MOVE CTL-BUSINESS-DATE  TO ACCT-OPEN-DATE
                                      ACCT-LAST-ACTIVITY-DATE
                                      ACCT-LAST-MAINT-DATE
                                      ACCT-LAST-ACCRUAL-DATE
           MOVE CUST-STATE         TO ACCT-ESCHEAT-STATE
           MOVE ZERO               TO ACCT-LEDGER-BAL ACCT-AVAIL-BAL
           MOVE CA-OD-LIMIT        TO ACCT-OD-LIMIT
           MOVE CA-OD-OPT-IN       TO ACCT-OD-OPT-IN
           MOVE CA-RATE            TO ACCT-INT-RATE
           MOVE CA-PROMO-RATE      TO ACCT-PROMO-RATE
           MOVE CA-PROMO-EXP       TO ACCT-PROMO-EXP-DATE
           MOVE 'N'                TO ACCT-BACKUP-WH-FLAG
           IF CUST-TAX-ID = SPACES OR CUST-TAXID-NONE
      *        NO TIN CERTIFIED - IRS BACKUP WITHHOLDING APPLIES
               MOVE 'Y' TO ACCT-BACKUP-WH-FLAG
           END-IF
           IF PROD-TIME-DEPOSIT
               MOVE PROD-TERM-MONTHS TO ACCT-TERM-MONTHS
               MOVE CA-DEPOSIT       TO ACCT-ORIG-DEPOSIT-AMT
               MOVE CA-RENEW         TO ACCT-RENEWAL-OPTION
               MOVE CA-PAYOUT        TO ACCT-PAYOUT-ACCT
               INITIALIZE DATE-PARMS
               SET DTU-ADD-MONTHS TO TRUE
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-1
               MOVE PROD-TERM-MONTHS  TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO ACCT-MATURITY-DATE
               INITIALIZE DATE-PARMS
               SET DTU-ADD-DAYS TO TRUE
               MOVE ACCT-MATURITY-DATE TO DTU-DATE-1
               MOVE WS-CD-GRACE-DAYS   TO DTU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-DATE TO ACCT-GRACE-END-DATE
           END-IF
           MOVE 31  TO ACCT-STMT-CYCLE-DAY
           MOVE 'M' TO ACCT-STMT-FREQ
           MOVE 'Y' TO ACCT-ESTMT-FLAG
           IF ACCT-OWN-CORPORATE
               MOVE 0              TO ACCT-DAILY-WD-LIMIT
               MOVE 50000000.00    TO ACCT-DAILY-ACH-LIMIT
               MOVE 250000000.00   TO ACCT-DAILY-WIRE-LIMIT
               MOVE PROD-ANALYSIS-FLAG TO ACCT-ANALYSIS-FLAG
           ELSE
               MOVE 1000.00        TO ACCT-DAILY-WD-LIMIT
               MOVE 25000.00       TO ACCT-DAILY-ACH-LIMIT
               MOVE 100000.00      TO ACCT-DAILY-WIRE-LIMIT
           END-IF
           MOVE WS-USERID TO ACCT-LAST-MAINT-USER.

      *----------------------------------------------------------------*
      * PROSPECT BECOMES ACTIVE CUSTOMER ON FIRST ACCOUNT. THE KYC     *
      * REVIEW DATE IS SET FROM KYCTIER IF NONE IS ON FILE.            *
      *----------------------------------------------------------------*
       5300-ACTIVATE-PROSPECT.
           IF NOT CUST-PROSPECT AND CUST-KYC-REVIEW-DATE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           EXEC CICS READ FILE('CUSTMST') INTO(CUSTOMER-RECORD)
                     RIDFLD(CUST-ID) UPDATE RESP(WS-RESP)
           END-EXEC
           IF CUST-PROSPECT
               SET CUST-ACTIVE TO TRUE
               MOVE CTL-BUSINESS-DATE TO CUST-SINCE-DATE
           END-IF
           INITIALIZE KYCTIER-PARMS
           MOVE CUST-TYPE          TO KYT-CUST-TYPE
           MOVE CUST-PEP-FLAG      TO KYT-PEP-FLAG
           MOVE CUST-MSB-FLAG      TO KYT-MSB-FLAG
           MOVE CUST-RISK-RATING   TO KYT-RISK-RATING
           MOVE CUST-COUNTRY       TO KYT-COUNTRY
           MOVE CUST-SEGMENT       TO KYT-SEGMENT
           MOVE CUST-ANNUAL-INCOME TO KYT-ANNUAL-INCOME
           MOVE CUST-INDUSTRY-CODE TO KYT-INDUSTRY-CODE
           MOVE CUST-OFAC-STATUS   TO KYT-OFAC-STATUS
           MOVE CTL-BUSINESS-DATE  TO KYT-AS-OF-DATE
           CALL 'KYCTIER' USING KYCTIER-PARMS
           IF KYT-RETURN-CODE = 00
               MOVE KYT-RESULT-TIER      TO CUST-KYC-TIER
               MOVE KYT-NEXT-REVIEW-DATE TO CUST-KYC-REVIEW-DATE
           END-IF
           MOVE CTL-BUSINESS-DATE TO CUST-LAST-MAINT-DATE
           MOVE WS-USERID         TO CUST-LAST-MAINT-USER
           EXEC CICS REWRITE FILE('CUSTMST') FROM(CUSTOMER-RECORD)
                     RESP(WS-RESP)
           END-EXEC.

      *----------------------------------------------------------------*
      * FUNDING TRANSFER - WXFR FROM FUNDING ACCOUNT, DXFR TO NEW      *
      * ACCOUNT, QUEUED TO TD QUEUE TRNI (EXTRA-PARTITION -> TRANIN)   *
      *----------------------------------------------------------------*
       5400-QUEUE-FUNDING.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     TIME(WS-TIME-OUT)
           END-EXEC
           INITIALIZE TRANSACTION-RECORD
           MOVE CTL-BUSINESS-DATE TO TRAN-ID-DATE TRAN-EFFECTIVE-DATE
           MOVE EIBTASKN          TO WS-TRAN-SEQ
           MOVE WS-TRAN-SEQ       TO TRAN-ID-SEQ
           MOVE CA-FUND-ACCT      TO TRAN-ACCT-NUMBER
           MOVE WS-TIME-OUT       TO TRAN-TIME
           MOVE 'WXFR'            TO TRAN-CODE
           MOVE 'D'               TO TRAN-DR-CR
           MOVE CA-DEPOSIT        TO TRAN-AMOUNT
           MOVE 'BRN'             TO TRAN-CHANNEL
           MOVE 'TRANSFER TO NEW ACCOUNT' TO TRAN-DESCRIPTION
           MOVE ACCT-NUMBER       TO TRAN-COUNTER-ACCT
           SET TRAN-PENDING       TO TRUE
           MOVE WS-USERID         TO TRAN-USER-ID
           MOVE 'Y'               TO TRAN-REGD-FLAG
           MOVE 'ACCTOPEN'        TO TRAN-SOURCE-PGM
           EXEC CICS WRITEQ TD QUEUE('TRNI')
                     FROM(TRANSACTION-RECORD)
                     LENGTH(LENGTH OF TRANSACTION-RECORD)
           END-EXEC
           MOVE ACCT-NUMBER       TO TRAN-ACCT-NUMBER
           MOVE CA-FUND-ACCT      TO TRAN-COUNTER-ACCT
           MOVE 'DXFR'            TO TRAN-CODE
           MOVE 'C'               TO TRAN-DR-CR
           MOVE 'N'               TO TRAN-REGD-FLAG
           MOVE 'OPENING DEPOSIT - TRANSFER IN' TO TRAN-DESCRIPTION
           EXEC CICS WRITEQ TD QUEUE('TRNI')
                     FROM(TRANSACTION-RECORD)
                     LENGTH(LENGTH OF TRANSACTION-RECORD)
           END-EXEC.

      *----------------------------------------------------------------*
       8000-SEND-MAP.
           MOVE WS-MESSAGE TO MSGO
           MOVE 'ENTER=VALIDATE/QUOTE  PF5=OPEN  PF3=MENU  PF12=CANCEL'
             TO PFKEYSO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE-OUT) DATESEP('/')
           END-EXEC
           MOVE WS-DATE-OUT TO CURDATEO
           EXEC CICS SEND MAP('ACOPNM') MAPSET('ACOPNS')
                     FROM(ACOPNMO) ERASE CURSOR
           END-EXEC.
