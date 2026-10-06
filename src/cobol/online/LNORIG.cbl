      *================================================================*
      * PROGRAM  : LNORIG                                              *
      * TYPE     : CICS ONLINE - PSEUDO-CONVERSATIONAL                 *
      * TRANSACTION : CB42                                             *
      * MAPSET   : LNORIGS   MAP LNORIGM                               *
      * PURPOSE  : CONSUMER PERSONAL INSTALLMENT LOAN (LPER)           *
      *            APPLICATION AND CREDIT DECISION.                    *
      *            ENTER  - VALIDATE THE APPLICATION, SHOW CIF DATA    *
      *            PF5    - VALIDATE, PRICE (LNPRICE), DECIDE          *
      *                     (CRDECIDE), PAYMENT (AMORTCLC). APPROVED   *
      *                     APPLICATIONS ARE WRITTEN TO LNAPPL FOR     *
      *                     BOOKING BY LOAN OPERATIONS.                *
      *            PF12   - CLEAR / NEW APPLICATION                    *
      *            PROSPECTS NOT ON THE CIF ARE ALLOWED (KYC PENDING). *
      * FILES    : CUSTMST  (KSDS)  READ                               *
      *            ACCTMST  (KSDS)  READ  - AUTOPAY DEPOSIT ACCOUNT    *
      *            LNAPPL   (KSDS)  WRITE - APPROVED APPLICATIONS      *
      * CALLS    : GETCTL   (BANKCTL)  - BUSINESS DATE                 *
      *            LNPRICE  (LNPRPARM) - LPER RATE                     *
      *            CRDECIDE (CRDPARM)  - CREDIT DECISION / REASONS     *
      *            AMORTCLC (AMRTPARM) - MONTHLY PAYMENT               *
      * XCTL     : CBMENU   (PF3)                                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2016-04-25 AKW  ORIGINAL - LPER LAUNCH                         *
      * 2016-07-18 AKW  PROSPECTS ALLOWED WITH KYC PENDING - INC16211  *
      * 2017-09-11 AKW  COUNTEROFFER DISPLAY - CHG2231                 *
      * 2018-03-05 AKW  AUTOPAY ACCOUNT MUST BELONG TO APPLICANT -     *
      *                 INC17840                                       *
      * 2018-11-05 AKW  RESP/RESP2 ON ALL CICS COMMANDS - AUDIT A18-07 *
      * 2019-02-04 JPL  CRDECIDE MAX AMOUNT 40,000 - CHG2689           *
      * 2019-02-04 JPL  REQUESTED AMOUNT FIELD WIDENED TO 6 DIGITS     *
      * 2021-08-23 JPL  DUPLICATE SUBMIT GUARD - INC21107              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNORIG.
       AUTHOR. A K WOJCIK.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'LNORIG'.
       01  WS-TRANSID                      PIC X(04) VALUE 'CB42'.

       01  WS-CICS-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP2                    PIC S9(08) COMP.
           05  WS-RESP-D                   PIC 9(04).
           05  WS-RESP2-D                  PIC 9(04).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-DISP-DATE                PIC X(10).
           05  WS-TASK-NUM                 PIC 9(07).
           05  WS-USERID                   PIC X(08).

       01  WS-FLAGS.
           05  WS-ERROR-FLAG               PIC X(01) VALUE 'N'.
               88  WS-ERROR                          VALUE 'Y'.
               88  WS-NO-ERROR                       VALUE 'N'.
           05  WS-CUST-FOUND-FLAG          PIC X(01) VALUE 'N'.
               88  WS-CUST-FOUND                     VALUE 'Y'.
               88  WS-CUST-NOT-FOUND                 VALUE 'N'.
           05  WS-PRICE-DECLINE-FLAG       PIC X(01) VALUE 'N'.
               88  WS-PRICE-DECLINED                 VALUE 'Y'.

       01  WS-MESSAGE                      PIC X(79) VALUE SPACES.
       01  WS-BUS-DATE                     PIC 9(08).
       01  WS-CUST-KEY                     PIC X(10).
       01  WS-ACCT-KEY                     PIC X(12).
       01  WS-IX                           PIC 9(01).
       01  WS-LEAD                         PIC 9(02) COMP.

      *    COMMAREA - SAVED BETWEEN PSEUDO-CONVERSATIONAL TASKS
       01  WS-COMMAREA.
           05  CA-PROGRAM                  PIC X(08).
           05  CA-CUST-ID                  PIC X(10).
      *        SET WHEN AN APPROVED APPLICATION HAS BEEN WRITTEN.
      *        PF5 IS REFUSED UNTIL PF12 STARTS A NEW APPLICATION.
           05  CA-APPL-NUMBER              PIC X(12).
           05  FILLER                      PIC X(10).

      *    SCREEN INPUT WORK (NUMERIC FIELDS ARE JUSTIFY=(RIGHT,ZERO))
       01  WS-INPUT-WORK.
           05  WS-SCORE-X                  PIC X(03).
           05  WS-SCORE REDEFINES WS-SCORE-X
                                           PIC 9(03).
           05  WS-INCOME-X                 PIC X(09).
           05  WS-INCOME REDEFINES WS-INCOME-X
                                           PIC 9(09).
           05  WS-MDEBT-X                  PIC X(07).
           05  WS-MDEBT REDEFINES WS-MDEBT-X
                                           PIC 9(07).
           05  WS-EMPMOS-X                 PIC X(03).
           05  WS-EMPMOS REDEFINES WS-EMPMOS-X
                                           PIC 9(03).
           05  WS-REQAMT-X                 PIC X(06).
           05  WS-REQ-AMT REDEFINES WS-REQAMT-X
                                           PIC 9(06).
           05  WS-TERM-X                   PIC X(02).
           05  WS-TERM REDEFINES WS-TERM-X
                                           PIC 9(02).

      *    APPLICANT DATA FROM THE CIF (OR PROSPECT DEFAULTS)
       01  WS-APPLICANT.
           05  WS-APP-NAME                 PIC X(30).
           05  WS-APP-STATE                PIC X(02).
           05  WS-APP-KYC                  PIC X(01).
           05  WS-APP-EMPLOYEE             PIC X(01).
           05  WS-APP-EXISTING             PIC X(01).
           05  WS-APP-REL-BAL              PIC S9(13)V99 COMP-3.
           05  WS-APP-CIF-INCOME           PIC S9(13)V99 COMP-3.

      *    EDITED DISPLAY FIELDS
       01  WS-EDIT-FIELDS.
           05  WS-AMT18-ED                 PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-AMT14-ED                 PIC ZZ,ZZZ,ZZ9.99-.
           05  WS-CTR-AMT-ED               PIC ZZZ,ZZ9.99.
           05  WS-CTR-RATE-ED              PIC Z9.99.
           05  WS-RATE-DISP.
               10  WS-RATE-ED              PIC Z9.999.
               10  FILLER                  PIC X(01) VALUE '%'.
           05  WS-DTI-DISP.
               10  WS-DTI-ED               PIC ZZ9.99.
               10  FILLER                  PIC X(01) VALUE '%'.
           05  WS-RC-ED                    PIC 99.
           05  WS-COUNTER-LINE             PIC X(60).
           05  WS-REASON-LINE.
               10  WS-RSN-CODE             PIC X(03).
               10  FILLER                  PIC X(03) VALUE ' - '.
               10  WS-RSN-TEXT             PIC X(40).

      *----------------------------------------------------------------*
      * LNAPPL - APPROVED APPLICATION PENDING BOOKING                  *
      *   KEY APL-APPL-NUMBER = 'LA' + YYMMDD + 4 DIGITS OF TASK NO.   *
      *   LOAN OPERATIONS BOOKS FROM THIS FILE (LNBOOK, CB43)          *
      *----------------------------------------------------------------*
       01  WS-LNAPPL-RECORD.
           05  APL-APPL-NUMBER.
               10  APL-APPL-PREFIX         PIC X(02) VALUE 'LA'.
               10  APL-APPL-YYMMDD         PIC 9(06).
               10  APL-APPL-SEQ            PIC 9(04).
           05  APL-STATUS                  PIC X(01).
               88  APL-PENDING-BOOKING               VALUE 'P'.
               88  APL-BOOKED                        VALUE 'B'.
               88  APL-WITHDRAWN                     VALUE 'W'.
           05  APL-APPL-DATE               PIC 9(08).
           05  APL-CUST-ID                 PIC X(10).
           05  APL-EXISTING-CUST           PIC X(01).
           05  APL-KYC-STATUS              PIC X(01).
           05  APL-STATE                   PIC X(02).
           05  APL-PRODUCT-CODE            PIC X(04).
           05  APL-REQUESTED-AMT           PIC S9(13)V99 COMP-3.
           05  APL-TERM-MONTHS             PIC 9(03).
           05  APL-RATE                    PIC 9(03)V9(06).
           05  APL-PAYMENT-AMT             PIC S9(11)V99 COMP-3.
           05  APL-TOTAL-INTEREST          PIC S9(13)V99 COMP-3.
           05  APL-CREDIT-SCORE            PIC 9(03).
           05  APL-ANNUAL-INCOME           PIC S9(11)V99 COMP-3.
           05  APL-MONTHLY-DEBT            PIC S9(09)V99 COMP-3.
           05  APL-DTI-PCT                 PIC 9(03)V99.
           05  APL-EMPLOYMENT-MONTHS       PIC 9(03).
           05  APL-BANKRUPTCY-7YR          PIC X(01).
           05  APL-AUTOPAY-FLAG            PIC X(01).
           05  APL-AUTOPAY-ACCT            PIC X(12).
           05  APL-EMPLOYEE-FLAG           PIC X(01).
           05  APL-DECISION                PIC X(07).
           05  APL-USER-ID                 PIC X(08).
           05  APL-TERM-ID                 PIC X(04).
           05  FILLER                      PIC X(30).

      *    COPYBOOKS
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY LNORIGS.
       COPY BANKCTL.
       COPY CUSTREC.
       COPY ACCTREC.
       COPY LNPRPARM.
       COPY CRDPARM.
       COPY AMRTPARM.

       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(40).

       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           PERFORM 1100-GET-BUSINESS-DATE

           IF EIBCALEN = 0
               PERFORM 1000-FIRST-TIME
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               EVALUATE EIBAID
                   WHEN DFHPF3
                       PERFORM 9100-RETURN-TO-MENU
                   WHEN DFHCLEAR
                   WHEN DFHPF12
                       PERFORM 1000-FIRST-TIME
                   WHEN DFHENTER
                       PERFORM 2000-VALIDATE-APPLICATION
                   WHEN DFHPF5
                       PERFORM 3000-DECIDE-APPLICATION
                   WHEN OTHER
                       PERFORM 2050-RECEIVE-MAP
                       MOVE 'INVALID KEY - ENTER, PF5, PF3 OR PF12'
                           TO WS-MESSAGE
                       PERFORM 8900-SEND-MAP
               END-EVALUATE
           END-IF

           PERFORM 9000-RETURN-TRANSID
           GOBACK.

      *================================================================*
      * 1000 - FIRST ENTRY / PF12 - NEW APPLICATION                    *
      *================================================================*
       1000-FIRST-TIME.
           MOVE LOW-VALUES TO LNORIGMO
           INITIALIZE WS-COMMAREA
           MOVE 'ENTER APPLICATION - ENTER TO VALIDATE, PF5 TO DECIDE'
               TO WS-MESSAGE
           MOVE -1 TO CUSTIDL
           PERFORM 8900-SEND-MAP.

      *    BUSINESS DATE FROM THE BANK CONTROL RECORD, NOT THE CLOCK
       1100-GET-BUSINESS-DATE.
           CALL 'GETCTL' USING BANK-CONTROL-RECORD
           MOVE CTL-BUSINESS-DATE TO WS-BUS-DATE.

      *================================================================*
      * 2000 - ENTER - VALIDATE ONLY                                   *
      *================================================================*
       2000-VALIDATE-APPLICATION.
           SET WS-NO-ERROR TO TRUE
           PERFORM 2050-RECEIVE-MAP
           IF WS-NO-ERROR
               PERFORM 2100-LOOKUP-CUSTOMER
           END-IF
           IF WS-NO-ERROR
               PERFORM 2200-EDIT-APPLICATION
           END-IF
           IF WS-NO-ERROR
               MOVE 'APPLICATION IS COMPLETE - PRESS PF5 TO DECIDE'
                   TO WS-MESSAGE
           END-IF
           PERFORM 8900-SEND-MAP.

       2050-RECEIVE-MAP.
           EXEC CICS RECEIVE MAP('LNORIGM')
                     MAPSET('LNORIGS')
                     INTO(LNORIGMI)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE LOW-VALUES TO LNORIGMI
                   SET WS-ERROR TO TRUE
                   MOVE 'ENTER THE APPLICATION DETAILS' TO WS-MESSAGE
                   MOVE -1 TO CUSTIDL
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE

           INSPECT CUSTIDI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT STATEI  REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SCOREI  REPLACING ALL LOW-VALUE BY SPACE
           INSPECT INCOMEI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT MDEBTI  REPLACING ALL LOW-VALUE BY SPACE
           INSPECT EMPMOSI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT REQAMTI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT TERMI   REPLACING ALL LOW-VALUE BY SPACE
           INSPECT BKRPTI  REPLACING ALL LOW-VALUE BY SPACE
           INSPECT AUTOPYI REPLACING ALL LOW-VALUE BY SPACE
           INSPECT APACCTI REPLACING ALL LOW-VALUE BY SPACE.

      *----------------------------------------------------------------*
      * 2100 - APPLICANT FROM THE CIF                                  *
      *   FOUND     : STATE, KYC, EMPLOYEE FLAG, RELATIONSHIP DEPOSITS *
      *               FROM CUSTMST. EXISTING CUSTOMER = 'Y' WHEN THE   *
      *               CUSTOMER-SINCE DATE IS BEFORE THE BUSINESS DATE. *
      *   NOT FOUND : PROSPECT - EXISTING 'N', KYC 'P' (PENDING),      *
      *               STATE MUST BE KEYED.                             *
      *   DECEASED / BLOCKED CUSTOMERS CANNOT APPLY.                   *
      *----------------------------------------------------------------*
       2100-LOOKUP-CUSTOMER.
           INITIALIZE WS-APPLICANT
           SET WS-CUST-NOT-FOUND TO TRUE

           IF CUSTIDI = SPACES OR CUSTIDI(1:1) NOT = 'C'
               SET WS-ERROR TO TRUE
               MOVE 'CUSTOMER ID IS REQUIRED (CNNNNNNNNN)'
                   TO WS-MESSAGE
               MOVE DFHUNIMD TO CUSTIDA
               MOVE -1 TO CUSTIDL
           ELSE
               MOVE CUSTIDI TO WS-CUST-KEY CA-CUST-ID
               EXEC CICS READ FILE('CUSTMST')
                         INTO(CUSTOMER-RECORD)
                         RIDFLD(WS-CUST-KEY)
                         RESP(WS-RESP)
                         RESP2(WS-RESP2)
               END-EXEC
               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       SET WS-CUST-FOUND TO TRUE
                       PERFORM 2110-LOAD-CIF-APPLICANT
                   WHEN DFHRESP(NOTFND)
                       PERFORM 2120-LOAD-PROSPECT
                   WHEN OTHER
                       PERFORM 9900-CICS-ERROR
               END-EVALUATE
           END-IF

           IF WS-NO-ERROR
               MOVE WS-APP-NAME     TO CUSTNMO
               MOVE WS-APP-EXISTING TO EXISTO
               MOVE WS-APP-KYC      TO KYCSTO
               MOVE WS-APP-EMPLOYEE TO EMPFLGO
               MOVE WS-APP-REL-BAL  TO WS-AMT18-ED
               MOVE WS-AMT18-ED     TO RELBALO
           END-IF.

       2110-LOAD-CIF-APPLICANT.
           IF CUST-INDIVIDUAL
               STRING CUST-FIRST-NAME DELIMITED BY '  '
                      ' '             DELIMITED BY SIZE
                      CUST-LAST-NAME  DELIMITED BY '  '
                 INTO WS-APP-NAME
               END-STRING
           ELSE
               MOVE CUST-LAST-NAME TO WS-APP-NAME
           END-IF
           MOVE CUST-STATE             TO WS-APP-STATE STATEO
           MOVE CUST-KYC-STATUS        TO WS-APP-KYC
           MOVE CUST-TOTAL-REL-BALANCE TO WS-APP-REL-BAL
           MOVE CUST-ANNUAL-INCOME     TO WS-APP-CIF-INCOME
           IF CUST-IS-EMPLOYEE
               MOVE 'Y' TO WS-APP-EMPLOYEE
           ELSE
               MOVE 'N' TO WS-APP-EMPLOYEE
           END-IF
           IF CUST-SINCE-DATE < WS-BUS-DATE
               MOVE 'Y' TO WS-APP-EXISTING
           ELSE
               MOVE 'N' TO WS-APP-EXISTING
           END-IF

           EVALUATE TRUE
               WHEN CUST-DECEASED
                   SET WS-ERROR TO TRUE
                   MOVE 'CUSTOMER IS DECEASED - APPLICATION NOT ALLOWED'
                       TO WS-MESSAGE
               WHEN CUST-BLOCKED
                   SET WS-ERROR TO TRUE
                   MOVE 'CUSTOMER IS BLOCKED - REFER TO BSA/AML'
                       TO WS-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ERROR
               MOVE -1 TO CUSTIDL
           END-IF.

       2120-LOAD-PROSPECT.
           MOVE '*** PROSPECT - NOT ON CIF ***' TO WS-APP-NAME
           MOVE STATEI TO WS-APP-STATE
           MOVE 'P'    TO WS-APP-KYC
           MOVE 'N'    TO WS-APP-EMPLOYEE WS-APP-EXISTING
           MOVE ZERO   TO WS-APP-REL-BAL WS-APP-CIF-INCOME.

      *----------------------------------------------------------------*
      * 2200 - SCREEN FIELD EDITS (FIRST ERROR ONLY IS REPORTED)       *
      *----------------------------------------------------------------*
       2200-EDIT-APPLICATION.
           PERFORM 2210-EDIT-STATE
           IF WS-NO-ERROR
               PERFORM 2220-EDIT-CREDIT-SCORE
           END-IF
           IF WS-NO-ERROR
               PERFORM 2230-EDIT-INCOME
           END-IF
           IF WS-NO-ERROR
               PERFORM 2240-EDIT-MONTHLY-DEBT
           END-IF
           IF WS-NO-ERROR
               PERFORM 2250-EDIT-AMOUNT
           END-IF
           IF WS-NO-ERROR
               PERFORM 2260-EDIT-TERM
           END-IF
           IF WS-NO-ERROR
               PERFORM 2270-EDIT-EMPLOYMENT
           END-IF
           IF WS-NO-ERROR
               PERFORM 2280-EDIT-BANKRUPTCY
           END-IF
           IF WS-NO-ERROR
               PERFORM 2290-EDIT-AUTOPAY
           END-IF.

       2210-EDIT-STATE.
           IF WS-APP-STATE = SPACES
               SET WS-ERROR TO TRUE
               MOVE 'STATE IS REQUIRED FOR A PROSPECT' TO WS-MESSAGE
               MOVE DFHUNIMD TO STATEA
               MOVE -1 TO STATEL
           END-IF.

      *    FICO RANGE 300 - 850
       2220-EDIT-CREDIT-SCORE.
           MOVE SCOREI TO WS-SCORE-X
           EVALUATE TRUE
               WHEN WS-SCORE-X = SPACES
                   SET WS-ERROR TO TRUE
                   MOVE 'CREDIT SCORE IS REQUIRED' TO WS-MESSAGE
               WHEN WS-SCORE-X IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'CREDIT SCORE MUST BE NUMERIC' TO WS-MESSAGE
               WHEN WS-SCORE < 300 OR WS-SCORE > 850
                   SET WS-ERROR TO TRUE
                   MOVE 'CREDIT SCORE MUST BE 300 - 850' TO WS-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ERROR
               MOVE DFHUNIMD TO SCOREA
               MOVE -1 TO SCOREL
           END-IF.

      *    BLANK INCOME DEFAULTS FROM THE CIF (EXISTING CUSTOMERS)
       2230-EDIT-INCOME.
           MOVE INCOMEI TO WS-INCOME-X
           EVALUATE TRUE
               WHEN (WS-INCOME-X = SPACES OR WS-INCOME-X = ZEROS)
                AND WS-CUST-FOUND
                AND WS-APP-CIF-INCOME > 0
                   MOVE WS-APP-CIF-INCOME TO WS-INCOME
                   MOVE WS-INCOME-X       TO INCOMEO
               WHEN WS-INCOME-X = SPACES OR WS-INCOME-X = ZEROS
                   SET WS-ERROR TO TRUE
                   MOVE 'ANNUAL INCOME IS REQUIRED' TO WS-MESSAGE
               WHEN WS-INCOME-X IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'ANNUAL INCOME MUST BE WHOLE DOLLARS'
                       TO WS-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ERROR
               MOVE DFHUNIMD TO INCOMEA
               MOVE -1 TO INCOMEL
           END-IF.

       2240-EDIT-MONTHLY-DEBT.
           MOVE MDEBTI TO WS-MDEBT-X
           IF WS-MDEBT-X = SPACES
               MOVE ZEROS TO WS-MDEBT-X
           END-IF
           IF WS-MDEBT-X IS NOT NUMERIC
               SET WS-ERROR TO TRUE
               MOVE 'MONTHLY DEBT MUST BE WHOLE DOLLARS (0 IF NONE)'
                   TO WS-MESSAGE
               MOVE DFHUNIMD TO MDEBTA
               MOVE -1 TO MDEBTL
           END-IF.

      *----------------------------------------------------------------*
      * 2250 - REQUESTED AMOUNT                                        *
      *   LPER LOAN AMOUNT LIMITS - PRODUCT GUIDE PG-LPER-01           *
      *----------------------------------------------------------------*
       2250-EDIT-AMOUNT.
           MOVE REQAMTI TO WS-REQAMT-X
           EVALUATE TRUE
               WHEN WS-REQAMT-X = SPACES OR WS-REQAMT-X = ZEROS
                   SET WS-ERROR TO TRUE
                   MOVE 'REQUESTED AMOUNT IS REQUIRED' TO WS-MESSAGE
               WHEN WS-REQAMT-X IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'REQUESTED AMOUNT MUST BE WHOLE DOLLARS'
                       TO WS-MESSAGE
               WHEN WS-REQ-AMT < 3500 OR WS-REQ-AMT > 35000
                   SET WS-ERROR TO TRUE
                   MOVE 'AMOUNT MUST BE 3,500 - 35,000' TO WS-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ERROR
               MOVE DFHUNIMD TO REQAMTA
               MOVE -1 TO REQAMTL
           END-IF.

      *    LPER TERMS - 12 / 24 / 36 / 48 / 60 MONTHS ONLY
       2260-EDIT-TERM.
           MOVE TERMI TO WS-TERM-X
           IF WS-TERM-X IS NOT NUMERIC
               MOVE ZEROS TO WS-TERM-X
           END-IF
           EVALUATE WS-TERM
               WHEN 12
               WHEN 24
               WHEN 36
               WHEN 48
               WHEN 60
                   CONTINUE
               WHEN OTHER
                   SET WS-ERROR TO TRUE
                   MOVE 'TERM MUST BE 12, 24, 36, 48 OR 60 MONTHS'
                       TO WS-MESSAGE
                   MOVE DFHUNIMD TO TERMA
                   MOVE -1 TO TERML
           END-EVALUATE.

       2270-EDIT-EMPLOYMENT.
           MOVE EMPMOSI TO WS-EMPMOS-X
           IF WS-EMPMOS-X = SPACES OR WS-EMPMOS-X IS NOT NUMERIC
               SET WS-ERROR TO TRUE
               MOVE 'EMPLOYMENT MONTHS IS REQUIRED (0 IF NONE)'
                   TO WS-MESSAGE
               MOVE DFHUNIMD TO EMPMOSA
               MOVE -1 TO EMPMOSL
           END-IF.

       2280-EDIT-BANKRUPTCY.
           IF BKRPTI NOT = 'Y' AND BKRPTI NOT = 'N'
               SET WS-ERROR TO TRUE
               MOVE 'BANKRUPTCY IN LAST 7 YEARS MUST BE Y OR N'
                   TO WS-MESSAGE
               MOVE DFHUNIMD TO BKRPTA
               MOVE -1 TO BKRPTL
           END-IF.

      *----------------------------------------------------------------*
      * 2290 - AUTOPAY. WHEN Y THE DEPOSIT ACCOUNT MUST BE AN ACTIVE   *
      *        ACCOUNT WHOSE PRIMARY OWNER IS THE APPLICANT.           *
      *----------------------------------------------------------------*
       2290-EDIT-AUTOPAY.
           EVALUATE TRUE
               WHEN AUTOPYI = 'N'
                   MOVE SPACES TO APACCTO
               WHEN AUTOPYI NOT = 'Y'
                   SET WS-ERROR TO TRUE
                   MOVE 'AUTOPAY MUST BE Y OR N' TO WS-MESSAGE
                   MOVE DFHUNIMD TO AUTOPYA
                   MOVE -1 TO AUTOPYL
               WHEN APACCTI = SPACES OR APACCTI IS NOT NUMERIC
                   SET WS-ERROR TO TRUE
                   MOVE 'AUTOPAY DEPOSIT ACCOUNT IS REQUIRED'
                       TO WS-MESSAGE
                   MOVE DFHUNIMD TO APACCTA
                   MOVE -1 TO APACCTL
               WHEN OTHER
                   PERFORM 2295-VERIFY-AUTOPAY-ACCT
           END-EVALUATE.

       2295-VERIFY-AUTOPAY-ACCT.
           MOVE APACCTI TO WS-ACCT-KEY
           EXEC CICS READ FILE('ACCTMST')
                     INTO(ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           EVALUATE TRUE
               WHEN WS-RESP = DFHRESP(NOTFND)
                   SET WS-ERROR TO TRUE
                   MOVE 'AUTOPAY ACCOUNT NOT FOUND' TO WS-MESSAGE
               WHEN WS-RESP NOT = DFHRESP(NORMAL)
                   PERFORM 9900-CICS-ERROR
               WHEN NOT ACCT-ACTIVE
                   SET WS-ERROR TO TRUE
                   MOVE 'AUTOPAY ACCOUNT IS NOT ACTIVE' TO WS-MESSAGE
               WHEN ACCT-PRIMARY-CUST-ID NOT = WS-CUST-KEY
                   SET WS-ERROR TO TRUE
                   MOVE 'AUTOPAY ACCOUNT NOT OWNED BY APPLICANT'
                       TO WS-MESSAGE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-ERROR
               MOVE DFHUNIMD TO APACCTA
               MOVE -1 TO APACCTL
           END-IF.

      *================================================================*
      * 3000 - PF5 - VALIDATE, PRICE, DECIDE, PAYMENT, SAVE            *
      *================================================================*
       3000-DECIDE-APPLICATION.
           SET WS-NO-ERROR TO TRUE
           PERFORM 2050-RECEIVE-MAP

           IF WS-NO-ERROR AND CA-APPL-NUMBER NOT = SPACES
               SET WS-ERROR TO TRUE
               MOVE CA-APPL-NUMBER TO APPLNOO
               MOVE 'APPLICATION ALREADY SUBMITTED - PF12 FOR A NEW ONE'
                   TO WS-MESSAGE
           END-IF
           IF WS-NO-ERROR
               PERFORM 2100-LOOKUP-CUSTOMER
           END-IF
           IF WS-NO-ERROR
               PERFORM 2200-EDIT-APPLICATION
           END-IF
           IF WS-NO-ERROR
               PERFORM 3100-PRICE-LOAN
           END-IF
           IF WS-NO-ERROR AND NOT WS-PRICE-DECLINED
               PERFORM 3200-CREDIT-DECISION
           END-IF
           IF WS-NO-ERROR AND NOT WS-PRICE-DECLINED
               AND NOT CRD-DECLINED
               PERFORM 3300-CALC-PAYMENT
           END-IF
           IF WS-NO-ERROR AND NOT WS-PRICE-DECLINED
               PERFORM 3400-FORMAT-DECISION
               IF CRD-APPROVED
                   PERFORM 3500-WRITE-APPLICATION
               END-IF
           END-IF

           PERFORM 8900-SEND-MAP.

      *----------------------------------------------------------------*
      * 3100 - RATE FROM LNPRICE (LPER GRID). RC 08 = PRICING DECLINE  *
      *----------------------------------------------------------------*
       3100-PRICE-LOAN.
           INITIALIZE LNPRICE-PARMS
           MOVE 'LPER'          TO LNP-PRODUCT-CODE
           MOVE WS-SCORE        TO LNP-CREDIT-SCORE
           MOVE WS-TERM         TO LNP-TERM-MONTHS
           MOVE AUTOPYI         TO LNP-AUTOPAY-FLAG
           MOVE WS-APP-EMPLOYEE TO LNP-EMPLOYEE-FLAG
           MOVE WS-APP-REL-BAL  TO LNP-REL-DEPOSIT-BAL

           CALL 'LNPRICE' USING LNPRICE-PARMS

           EVALUATE TRUE
               WHEN LNP-OK
                   CONTINUE
               WHEN LNP-DECLINED
                   SET WS-PRICE-DECLINED TO TRUE
                   MOVE 'DECLINE' TO DECISNO
                   MOVE DFHBMASB  TO DECISNA
                   MOVE SPACES    TO RATEO PMTO TOTINTO DTIO COUNTERO
                   MOVE SPACES    TO WS-MESSAGE
                   STRING 'DECLINED BY PRICING - REASON '
                                             DELIMITED BY SIZE
                          LNP-DECLINE-REASON DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
                   MOVE LNP-DECLINE-REASON TO WS-RSN-CODE
                   MOVE 'NOT ELIGIBLE FOR LPER PRICING GRID'
                       TO WS-RSN-TEXT
                   MOVE WS-REASON-LINE TO RSN1O
               WHEN OTHER
                   SET WS-ERROR TO TRUE
                   MOVE LNP-RETURN-CODE TO WS-RC-ED
                   MOVE SPACES TO WS-MESSAGE
                   STRING 'LNPRICE ERROR RC=' DELIMITED BY SIZE
                          WS-RC-ED            DELIMITED BY SIZE
                          ' - CONTACT HELP DESK' DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 3200 - CREDIT DECISION (SAME RULES MODULE AS BATCH)            *
      *----------------------------------------------------------------*
       3200-CREDIT-DECISION.
           INITIALIZE CREDIT-DECISION-PARMS
           MOVE 'C'               TO CRD-APPL-TYPE
           MOVE WS-SCORE          TO CRD-CREDIT-SCORE
           MOVE WS-INCOME         TO CRD-ANNUAL-INCOME
           MOVE WS-MDEBT          TO CRD-MONTHLY-DEBT
           MOVE WS-REQ-AMT        TO CRD-REQUESTED-AMT
           MOVE WS-TERM           TO CRD-TERM-MONTHS
           MOVE LNP-FINAL-RATE    TO CRD-PROPOSED-RATE
           MOVE WS-EMPMOS         TO CRD-EMPLOYMENT-MONTHS
           MOVE BKRPTI            TO CRD-BANKRUPTCY-7YR
           MOVE WS-APP-EXISTING   TO CRD-EXISTING-CUST
           MOVE WS-APP-KYC        TO CRD-KYC-STATUS
           MOVE WS-APP-STATE      TO CRD-STATE
           MOVE 'LPER'            TO CRD-PRODUCT-CODE

           CALL 'CRDECIDE' USING CREDIT-DECISION-PARMS

           IF CRD-RC-INVALID
               SET WS-ERROR TO TRUE
               MOVE CRD-RETURN-CODE TO WS-RC-ED
               MOVE SPACES TO WS-MESSAGE
               STRING 'CRDECIDE ERROR RC=' DELIMITED BY SIZE
                      WS-RC-ED             DELIMITED BY SIZE
                      ' - CONTACT HELP DESK' DELIMITED BY SIZE
                 INTO WS-MESSAGE
               END-STRING
           END-IF.

      *----------------------------------------------------------------*
      * 3300 - LEVEL MONTHLY PAYMENT, ACTUAL/365, NO IO PERIOD         *
      *----------------------------------------------------------------*
       3300-CALC-PAYMENT.
           INITIALIZE AMORT-PARMS
           MOVE 'PMT '         TO AMT-FUNCTION
           MOVE WS-REQ-AMT     TO AMT-PRINCIPAL
           MOVE LNP-FINAL-RATE TO AMT-ANNUAL-RATE
           MOVE '3'            TO AMT-ACCRUAL-BASIS
           MOVE 'M'            TO AMT-PAYMENT-FREQ
           MOVE WS-TERM        TO AMT-TERM-MONTHS
           MOVE ZERO           TO AMT-AMORT-MONTHS AMT-IO-MONTHS

           CALL 'AMORTCLC' USING AMORT-PARMS

           IF NOT AMT-OK
               SET WS-ERROR TO TRUE
               MOVE AMT-RETURN-CODE TO WS-RC-ED
               MOVE SPACES TO WS-MESSAGE
               STRING 'AMORTCLC ERROR RC=' DELIMITED BY SIZE
                      WS-RC-ED             DELIMITED BY SIZE
                      ' - CONTACT HELP DESK' DELIMITED BY SIZE
                 INTO WS-MESSAGE
               END-STRING
           END-IF.

      *----------------------------------------------------------------*
      * 3400 - DISPLAY DECISION, TERMS, COUNTEROFFER, REASONS          *
      *----------------------------------------------------------------*
       3400-FORMAT-DECISION.
           MOVE CRD-DECISION   TO DECISNO
           MOVE DFHBMASB       TO DECISNA
           MOVE LNP-FINAL-RATE TO WS-RATE-ED
           MOVE WS-RATE-DISP   TO RATEO
           MOVE CRD-DTI-PCT    TO WS-DTI-ED
           MOVE WS-DTI-DISP    TO DTIO

           IF CRD-DECLINED
               MOVE SPACES TO PMTO TOTINTO
           ELSE
               MOVE AMT-PAYMENT-AMT    TO WS-AMT14-ED
               MOVE WS-AMT14-ED        TO PMTO
               MOVE AMT-TOTAL-INTEREST TO WS-AMT14-ED
               MOVE WS-AMT14-ED        TO TOTINTO
           END-IF

      *    2017-09 AKW COUNTEROFFER - CHG2231
           MOVE SPACES TO COUNTERO
           IF CRD-COUNTER-AMT > 0
               PERFORM 3410-FORMAT-COUNTEROFFER
           END-IF

      *    REG B ADVERSE ACTION - UP TO 4 PRINCIPAL REASONS
           MOVE SPACES TO RSN1O RSN2O RSN3O RSN4O
           PERFORM VARYING WS-IX FROM 1 BY 1
                     UNTIL WS-IX > 4 OR WS-IX > CRD-REASON-COUNT
               MOVE CRD-REASON-CODE(WS-IX) TO WS-RSN-CODE
               MOVE CRD-REASON-TEXT(WS-IX) TO WS-RSN-TEXT
               EVALUATE WS-IX
                   WHEN 1 MOVE WS-REASON-LINE TO RSN1O
                   WHEN 2 MOVE WS-REASON-LINE TO RSN2O
                   WHEN 3 MOVE WS-REASON-LINE TO RSN3O
                   WHEN 4 MOVE WS-REASON-LINE TO RSN4O
               END-EVALUATE
           END-PERFORM

           EVALUATE TRUE
               WHEN CRD-APPROVED
                   MOVE 'APPROVED' TO WS-MESSAGE
               WHEN CRD-REFERRED
                   MOVE 'REFERRED TO CONSUMER UNDERWRITING (QUEUE CUW1)'
                       TO WS-MESSAGE
               WHEN CRD-DECLINED AND CRD-COUNTER-AMT > 0
                   MOVE 'DECLINED AS REQUESTED - COUNTEROFFER AVAILABLE'
                       TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'DECLINED - ADVERSE ACTION NOTICE TO BE MAILED'
                       TO WS-MESSAGE
           END-EVALUATE.

      *    'COUNTEROFFER: $NN,NNN.00 AT X.XX%'
       3410-FORMAT-COUNTEROFFER.
           MOVE CRD-COUNTER-AMT TO WS-CTR-AMT-ED
           MOVE LNP-FINAL-RATE  TO WS-CTR-RATE-ED
           MOVE SPACES TO WS-COUNTER-LINE
           MOVE ZERO TO WS-LEAD
           INSPECT WS-CTR-AMT-ED TALLYING WS-LEAD FOR LEADING SPACE
           STRING 'COUNTEROFFER: $'            DELIMITED BY SIZE
                  WS-CTR-AMT-ED(WS-LEAD + 1:)  DELIMITED BY SIZE
                  ' AT '                       DELIMITED BY SIZE
             INTO WS-COUNTER-LINE
           END-STRING
           MOVE ZERO TO WS-LEAD
           INSPECT WS-CTR-RATE-ED TALLYING WS-LEAD FOR LEADING SPACE
           STRING WS-COUNTER-LINE DELIMITED BY '  '
                  ' '                          DELIMITED BY SIZE
                  WS-CTR-RATE-ED(WS-LEAD + 1:) DELIMITED BY SIZE
                  '%'                          DELIMITED BY SIZE
             INTO COUNTERO
           END-STRING.

      *----------------------------------------------------------------*
      * 3500 - APPROVED - WRITE LNAPPL (PENDING BOOKING)               *
      *----------------------------------------------------------------*
       3500-WRITE-APPLICATION.
           EXEC CICS ASSIGN USERID(WS-USERID)
                     RESP(WS-RESP)
           END-EXEC
           MOVE EIBTASKN           TO WS-TASK-NUM
           MOVE WS-BUS-DATE(3:6)   TO APL-APPL-YYMMDD
           MOVE WS-TASK-NUM(4:4)   TO APL-APPL-SEQ
           SET APL-PENDING-BOOKING TO TRUE
           MOVE WS-BUS-DATE        TO APL-APPL-DATE
           MOVE WS-CUST-KEY        TO APL-CUST-ID
           MOVE WS-APP-EXISTING    TO APL-EXISTING-CUST
           MOVE WS-APP-KYC         TO APL-KYC-STATUS
           MOVE WS-APP-STATE       TO APL-STATE
           MOVE 'LPER'             TO APL-PRODUCT-CODE
           MOVE WS-REQ-AMT         TO APL-REQUESTED-AMT
           MOVE WS-TERM            TO APL-TERM-MONTHS
           MOVE LNP-FINAL-RATE     TO APL-RATE
           MOVE AMT-PAYMENT-AMT    TO APL-PAYMENT-AMT
           MOVE AMT-TOTAL-INTEREST TO APL-TOTAL-INTEREST
           MOVE WS-SCORE           TO APL-CREDIT-SCORE
           MOVE WS-INCOME          TO APL-ANNUAL-INCOME
           MOVE WS-MDEBT           TO APL-MONTHLY-DEBT
           MOVE CRD-DTI-PCT        TO APL-DTI-PCT
           MOVE WS-EMPMOS          TO APL-EMPLOYMENT-MONTHS
           MOVE BKRPTI             TO APL-BANKRUPTCY-7YR
           MOVE AUTOPYI            TO APL-AUTOPAY-FLAG
           MOVE APACCTI            TO APL-AUTOPAY-ACCT
           IF AUTOPYI NOT = 'Y'
               MOVE SPACES         TO APL-AUTOPAY-ACCT
           END-IF
           MOVE WS-APP-EMPLOYEE    TO APL-EMPLOYEE-FLAG
           MOVE CRD-DECISION       TO APL-DECISION
           MOVE WS-USERID          TO APL-USER-ID
           MOVE EIBTRMID           TO APL-TERM-ID

           EXEC CICS WRITE FILE('LNAPPL')
                     FROM(WS-LNAPPL-RECORD)
                     RIDFLD(APL-APPL-NUMBER)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE APL-APPL-NUMBER TO APPLNOO CA-APPL-NUMBER
                   MOVE SPACES TO WS-MESSAGE
                   STRING 'APPROVED - APPLICATION '
                                           DELIMITED BY SIZE
                          APL-APPL-NUMBER  DELIMITED BY SIZE
                          ' SENT FOR BOOKING'
                                           DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   END-STRING
               WHEN DFHRESP(DUPREC)
                   MOVE 'APPROVED - DUPLICATE APPL NO, PRESS PF5 AGAIN'
                       TO WS-MESSAGE
               WHEN OTHER
                   PERFORM 9900-CICS-ERROR
           END-EVALUATE.

      *================================================================*
      * 8000 - COMMON ROUTINES                                         *
      *================================================================*
       8800-SET-HEADER.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)
                     RESP(WS-RESP)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DISP-DATE)
                     DATESEP('/')
                     RESP(WS-RESP)
           END-EXEC
           MOVE WS-DISP-DATE TO CURDATEO.

       8900-SEND-MAP.
           PERFORM 8800-SET-HEADER
           MOVE WS-MESSAGE TO MSGO
           EXEC CICS SEND MAP('LNORIGM')
                     MAPSET('LNORIGS')
                     FROM(LNORIGMO)
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND ABCODE('LN42') END-EXEC
           END-IF.

      *================================================================*
      * 9000 - TERMINATION AND ERROR HANDLING                          *
      *================================================================*
       9000-RETURN-TRANSID.
           MOVE WS-PROGRAM-ID TO CA-PROGRAM
           EXEC CICS RETURN TRANSID(WS-TRANSID)
                     COMMAREA(WS-COMMAREA)
                     LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       9100-RETURN-TO-MENU.
           EXEC CICS XCTL PROGRAM('CBMENU')
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC
           MOVE LOW-VALUES TO LNORIGMO
           MOVE 'MAIN MENU (CBMENU) NOT AVAILABLE - CONTACT HELP DESK'
               TO WS-MESSAGE
           PERFORM 8900-SEND-MAP.

       9900-CICS-ERROR.
           SET WS-ERROR TO TRUE
           MOVE WS-RESP  TO WS-RESP-D
           MOVE WS-RESP2 TO WS-RESP2-D
           MOVE SPACES TO WS-MESSAGE
           STRING 'CICS ERROR RESP=' DELIMITED BY SIZE
                  WS-RESP-D          DELIMITED BY SIZE
                  ' RESP2='          DELIMITED BY SIZE
                  WS-RESP2-D         DELIMITED BY SIZE
                  ' - CONTACT HELP DESK' DELIMITED BY SIZE
             INTO WS-MESSAGE
           END-STRING.
