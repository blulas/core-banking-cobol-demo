      *================================================================*
      * COPYBOOK : LOANREC                                             *
      * PURPOSE  : LOAN MASTER RECORD - CONSUMER INSTALLMENT AND       *
      *            COMMERCIAL TERM / REVOLVING / CRE LOANS             *
      * FILE     : LOANMST  (KSDS, KEY = LOAN-NUMBER '3000000000NN',   *
      *                      AIX  = LOAN-BORROWER-CUST-ID WITH DUPS)   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - COMMERCIAL LOAN SYSTEM (CLS)        *
      * 1995-05-15 RJT  REVOLVING LINES - COMMITMENT AMOUNT            *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-01-20 BHS  CRE - INTEREST-ONLY MONTHS, AMORT MONTHS,      *
      *                 COLLATERAL / LTV / DSCR                        *
      * 2006-02-13 BHS  DUE TABLE (UNPAID INSTALLMENTS) REPLACES       *
      *                 SINGLE PAST-DUE AMOUNT - CHG0412               *
      * 2009-08-03 DPS  FFIEC DELINQUENCY COUNTERS 30/60/90            *
      * 2012-04-30 DPS  NON-ACCRUAL INTEREST MEMO - CHG1187            *
      * 2016-04-25 AKW  PERSONAL INSTALLMENT LOAN (LPER) - CREDIT      *
      *                 SCORE, AUTOPAY, BORROWER STATE                 *
      * 2020-04-06 AKW  CARES ACT FORBEARANCE STATUS 'F'               *
      * 2022-01-10 AKW  SOFR INDEX (LIBOR CESSATION) - REG-2022-L01    *
      *================================================================*
       01  LOAN-RECORD.
           05  LOAN-NUMBER                 PIC X(12).
           05  LOAN-PRODUCT-CODE           PIC X(04).
               88  LOAN-IS-PERSONAL                  VALUE 'LPER'.
               88  LOAN-IS-TERM                      VALUE 'LNTM'.
               88  LOAN-IS-REVOLVER                  VALUE 'LLOC'.
               88  LOAN-IS-CRE                       VALUE 'LCRE'.
               88  LOAN-IS-COMMERCIAL    VALUE 'LNTM' 'LLOC' 'LCRE'.
           05  LOAN-CATEGORY               PIC X(02).
               88  LOAN-CLOSED-END                   VALUE 'LN'.
               88  LOAN-OPEN-END                     VALUE 'LC'.
           05  LOAN-BORROWER-CUST-ID       PIC X(10).
           05  LOAN-CO-BORROWER-CUST-ID    PIC X(10).
           05  LOAN-STATUS                 PIC X(01).
               88  LOAN-ACTIVE                       VALUE 'A'.
               88  LOAN-PAID-OFF                     VALUE 'P'.
               88  LOAN-DELINQUENT                   VALUE 'D'.
               88  LOAN-CHARGED-OFF                  VALUE 'C'.
               88  LOAN-NON-ACCRUAL                  VALUE 'N'.
               88  LOAN-FORBEARANCE                  VALUE 'F'.
               88  LOAN-IS-OPEN           VALUE 'A' 'D' 'N' 'F'.
               88  LOAN-ACCRUING          VALUE 'A' 'D' 'F'.
           05  LOAN-STATUS-DATE            PIC 9(08).
           05  LOAN-COST-CENTER            PIC X(05).
           05  LOAN-OFFICER-ID             PIC X(08).
           05  LOAN-BORROWER-STATE         PIC X(02).
      *--- AMOUNTS ---------------------------------------------------*
           05  LOAN-ORIG-AMOUNT            PIC S9(13)V99 COMP-3.
      *    COMMITMENT APPLIES TO REVOLVING LINES (LLOC) ONLY
           05  LOAN-COMMITMENT-AMT         PIC S9(13)V99 COMP-3.
           05  LOAN-PRINCIPAL-BAL          PIC S9(13)V99 COMP-3.
      *    ACCRUED AND UNPAID INTEREST. MAY BE NEGATIVE WHEN BILLED
      *    INTEREST IS PAID BEFORE IT HAS ACCRUED (PREPAID INTEREST)
           05  LOAN-ACCRUED-INT            PIC S9(09)V9(06) COMP-3.
           05  LOAN-SUSPENSE-BAL           PIC S9(11)V99 COMP-3.
      *--- RATE ------------------------------------------------------*
           05  LOAN-INT-RATE               PIC 9(03)V9(06).
           05  LOAN-RATE-TYPE              PIC X(01).
               88  LOAN-FIXED-RATE                   VALUE 'F'.
               88  LOAN-VARIABLE-RATE                VALUE 'V'.
           05  LOAN-RATE-INDEX             PIC X(05).
               88  LOAN-INDEX-PRIME                  VALUE 'PRIME'.
               88  LOAN-INDEX-SOFR                   VALUE 'SOFR '.
           05  LOAN-MARGIN-RATE            PIC 9(03)V9(06).
           05  LOAN-FLOOR-RATE             PIC 9(03)V9(06).
           05  LOAN-CAP-RATE               PIC 9(03)V9(06).
      *    COPIED FROM PROD-ACCRUAL-BASIS AT BOOKING
           05  LOAN-ACCRUAL-BASIS          PIC X(01).
               88  LOAN-ACTUAL-365                   VALUE '3'.
               88  LOAN-ACTUAL-360                   VALUE 'A'.
               88  LOAN-30-360                       VALUE '0'.
           05  LOAN-LAST-RATE-CHG-DATE     PIC 9(08).
      *--- TERMS -----------------------------------------------------*
           05  LOAN-ORIG-DATE              PIC 9(08).
           05  LOAN-MATURITY-DATE          PIC 9(08).
           05  LOAN-TERM-MONTHS            PIC 9(03).
      *    AMORTIZATION MONTHS (0 = SAME AS TERM). CRE 300 / 120 TERM
           05  LOAN-AMORT-MONTHS           PIC 9(03).
           05  LOAN-IO-MONTHS              PIC 9(03).
           05  LOAN-PAYMENT-AMT            PIC S9(11)V99 COMP-3.
           05  LOAN-PAYMENT-FREQ           PIC X(01).
               88  LOAN-PAY-MONTHLY                  VALUE 'M'.
               88  LOAN-PAY-QUARTERLY                VALUE 'Q'.
           05  LOAN-NEXT-DUE-DATE          PIC 9(08).
           05  LOAN-INSTALLMENTS-BILLED    PIC 9(03).
           05  LOAN-LAST-BILL-DATE         PIC 9(08).
           05  LOAN-LAST-BILLED-DUE-DATE   PIC 9(08).
           05  LOAN-LAST-PMT-DATE          PIC 9(08).
           05  LOAN-LAST-PMT-AMT           PIC S9(11)V99 COMP-3.
      *--- DUE TABLE - BILLED AND UNPAID INSTALLMENTS, OLDEST FIRST --*
           05  LOAN-DUE-COUNT              PIC 9(02).
           05  LOAN-DUE-ENTRY              OCCURS 8 TIMES.
               10  LOAN-DUE-DATE           PIC 9(08).
               10  LOAN-DUE-INT            PIC S9(09)V99 COMP-3.
               10  LOAN-DUE-PRIN           PIC S9(11)V99 COMP-3.
               10  LOAN-DUE-FEE-FLAG       PIC X(01).
                   88  LOAN-DUE-FEE-ASSESSED         VALUE 'Y'.
      *--- DELINQUENCY -----------------------------------------------*
           05  LOAN-PAST-DUE-PRIN          PIC S9(11)V99 COMP-3.
           05  LOAN-PAST-DUE-INT           PIC S9(09)V99 COMP-3.
           05  LOAN-PAST-DUE-FEES          PIC S9(07)V99 COMP-3.
           05  LOAN-OLDEST-DUE-DATE        PIC 9(08).
           05  LOAN-DAYS-PAST-DUE          PIC 9(04).
           05  LOAN-DELQ-BUCKET            PIC X(01).
               88  LOAN-BKT-CURRENT                  VALUE '0'.
               88  LOAN-BKT-1-29                     VALUE '1'.
               88  LOAN-BKT-30-59                    VALUE '2'.
               88  LOAN-BKT-60-89                    VALUE '3'.
               88  LOAN-BKT-90-119                   VALUE '4'.
               88  LOAN-BKT-120-PLUS                 VALUE '5'.
           05  LOAN-LATE-FEES-OUTST        PIC S9(07)V99 COMP-3.
           05  LOAN-LATE-FEE-COUNT         PIC 9(03).
           05  LOAN-TIMES-30-DPD           PIC 9(02).
           05  LOAN-TIMES-60-DPD           PIC 9(02).
           05  LOAN-TIMES-90-DPD           PIC 9(02).
           05  LOAN-DLQ-CTR-START-DATE     PIC 9(08).
           05  LOAN-BUREAU-RPT-FLAG        PIC X(01).
               88  LOAN-REPORT-DELINQUENT            VALUE 'Y'.
      *--- AUTOPAY ---------------------------------------------------*
           05  LOAN-AUTOPAY-FLAG           PIC X(01).
               88  LOAN-ON-AUTOPAY                   VALUE 'Y'.
           05  LOAN-AUTOPAY-ACCT           PIC X(12).
           05  LOAN-AUTOPAY-GEN-DUE-DATE   PIC 9(08).
      *--- CREDIT / COLLATERAL ---------------------------------------*
           05  LOAN-CREDIT-SCORE           PIC 9(03).
           05  LOAN-RISK-GRADE             PIC 9(02).
           05  LOAN-COLLATERAL-TYPE        PIC X(02).
               88  LOAN-COLL-NONE                    VALUE 'NO'.
               88  LOAN-COLL-REAL-ESTATE             VALUE 'RE'.
               88  LOAN-COLL-RECEIVABLES             VALUE 'AR'.
               88  LOAN-COLL-EQUIPMENT               VALUE 'EQ'.
               88  LOAN-COLL-BLANKET                 VALUE 'BL'.
           05  LOAN-COLLATERAL-VALUE       PIC S9(13)V99 COMP-3.
           05  LOAN-LTV-PCT                PIC 9(03)V99.
           05  LOAN-DSCR                   PIC 9(02)V99.
           05  LOAN-COVENANT-FLAGS.
               10  LOAN-COV-DSCR-BREACH    PIC X(01).
               10  LOAN-COV-LTV-BREACH     PIC X(01).
               10  LOAN-COV-FIN-STMT-LATE  PIC X(01).
               10  LOAN-COV-LIQUIDITY      PIC X(01).
               10  LOAN-COV-WAIVED         PIC X(01).
      *--- NON-ACCRUAL / CHARGE-OFF / PAYOFF -------------------------*
           05  LOAN-NONACCRUAL-DATE        PIC 9(08).
      *    INTEREST REVERSED AT NON-ACCRUAL / CHARGE-OFF (MEMO ONLY)
           05  LOAN-NONACCR-INT-MEMO       PIC S9(09)V99 COMP-3.
           05  LOAN-CHARGEOFF-DATE         PIC 9(08).
           05  LOAN-CHARGEOFF-AMT          PIC S9(13)V99 COMP-3.
           05  LOAN-RECOVERY-AMT           PIC S9(13)V99 COMP-3.
           05  LOAN-PAYOFF-DATE            PIC 9(08).
           05  LOAN-PAYOFF-AMT             PIC S9(13)V99 COMP-3.
      *--- ACCRUAL / YTD ---------------------------------------------*
           05  LOAN-LAST-ACCRUAL-DATE      PIC 9(08).
           05  LOAN-INT-PAID-YTD           PIC S9(11)V99 COMP-3.
           05  LOAN-INT-ACCRUED-YTD        PIC S9(11)V99 COMP-3.
      *--- MAINTENANCE -----------------------------------------------*
           05  LOAN-LAST-MAINT-DATE        PIC 9(08).
           05  LOAN-LAST-MAINT-USER        PIC X(08).
           05  FILLER                      PIC X(40).
