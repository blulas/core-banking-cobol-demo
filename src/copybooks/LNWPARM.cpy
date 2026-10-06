      *================================================================*
      * COPYBOOK : LNWPARM                                             *
      * PURPOSE  : LINKAGE FOR LNWATER - PAYMENT APPLICATION WATERFALL *
      *   THE CALLER PASSES THE LOAN BALANCES AND DUE TABLE; LNWATER   *
      *   APPLIES THE PAYMENT AND RETURNS THE UPDATED VALUES PLUS THE  *
      *   SPLIT OF THE PAYMENT.                                        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 2006-02-13 BHS  DUE TABLE - CHG0412                            *
      * 2015-07-01 DPS  LATE FEES APPLIED LAST - CHG1655               *
      *================================================================*
       01  WATERFALL-PARMS.
      *--- INPUT -----------------------------------------------------*
           05  LNW-PRODUCT-CODE            PIC X(04).
           05  LNW-LOAN-STATUS             PIC X(01).
           05  LNW-DAYS-PAST-DUE           PIC 9(04).
           05  LNW-PAYMENT-AMT             PIC S9(13)V99 COMP-3.
      *--- INPUT / OUTPUT (UPDATED IN PLACE) -------------------------*
           05  LNW-PRINCIPAL-BAL           PIC S9(13)V99 COMP-3.
           05  LNW-ACCRUED-INT             PIC S9(09)V9(06) COMP-3.
           05  LNW-LATE-FEES               PIC S9(07)V99 COMP-3.
           05  LNW-SUSPENSE-BAL            PIC S9(11)V99 COMP-3.
           05  LNW-DUE-COUNT               PIC 9(02).
           05  LNW-DUE-ENTRY               OCCURS 8 TIMES.
               10  LNW-DUE-DATE            PIC 9(08).
               10  LNW-DUE-INT             PIC S9(09)V99 COMP-3.
               10  LNW-DUE-PRIN            PIC S9(11)V99 COMP-3.
               10  LNW-DUE-FEE-FLAG        PIC X(01).
      *--- OUTPUT ----------------------------------------------------*
           05  LNW-WATERFALL-USED          PIC X(01).
               88  LNW-WF-CURRENT                    VALUE 'C'.
               88  LNW-WF-DELINQUENT                 VALUE 'D'.
               88  LNW-WF-NON-ACCRUAL                VALUE 'N'.
               88  LNW-WF-RECOVERY                   VALUE 'R'.
               88  LNW-WF-PAYOFF                     VALUE 'P'.
           05  LNW-APPLIED-FEES            PIC S9(07)V99 COMP-3.
           05  LNW-APPLIED-INT             PIC S9(11)V99 COMP-3.
           05  LNW-APPLIED-PRIN            PIC S9(13)V99 COMP-3.
           05  LNW-APPLIED-EXTRA-PRIN      PIC S9(13)V99 COMP-3.
           05  LNW-APPLIED-SUSPENSE        PIC S9(11)V99 COMP-3.
           05  LNW-APPLIED-RECOVERY        PIC S9(13)V99 COMP-3.
           05  LNW-INSTALLMENTS-PAID       PIC 9(02).
           05  LNW-PAYOFF-AMT              PIC S9(13)V99 COMP-3.
           05  LNW-PAYOFF-FLAG             PIC X(01).
               88  LNW-PAID-IN-FULL                  VALUE 'Y'.
           05  LNW-RETURN-CODE             PIC 9(02).
               88  LNW-OK                            VALUE 00.
               88  LNW-INVALID                       VALUE 12.
