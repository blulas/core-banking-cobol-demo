      *================================================================*
      * COPYBOOK : AMRTPARM                                            *
      * PURPOSE  : LINKAGE FOR AMORTCLC - LEVEL PAYMENT / SCHEDULE     *
      *   PMT   LEVEL PAYMENT FOR PRINCIPAL / RATE / TERM              *
      *   LINE  SCHEDULE LINE FOR PERIOD AMT-PERIOD-NUMBER             *
      *   MAXP  MAXIMUM PRINCIPAL SUPPORTED BY AMT-PAYMENT-AMT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 2003-01-20 BHS  IO MONTHS / AMORT MONTHS FOR CRE BALLOON       *
      * 2016-04-25 AKW  MAXP FOR LPER COUNTEROFFER                     *
      *================================================================*
       01  AMORT-PARMS.
           05  AMT-FUNCTION                PIC X(04).
               88  AMT-CALC-PAYMENT                  VALUE 'PMT '.
               88  AMT-CALC-LINE                     VALUE 'LINE'.
               88  AMT-CALC-MAX-PRINCIPAL            VALUE 'MAXP'.
      *--- INPUT -----------------------------------------------------*
           05  AMT-PRINCIPAL               PIC S9(13)V99 COMP-3.
           05  AMT-ANNUAL-RATE             PIC 9(03)V9(06).
           05  AMT-ACCRUAL-BASIS           PIC X(01).
               88  AMT-ACTUAL-365                    VALUE '3'.
               88  AMT-ACTUAL-360                    VALUE 'A'.
               88  AMT-30-360                        VALUE '0'.
           05  AMT-PAYMENT-FREQ            PIC X(01).
               88  AMT-MONTHLY                       VALUE 'M'.
               88  AMT-QUARTERLY                     VALUE 'Q'.
      *    TERM = MONTHS TO MATURITY. AMORT = AMORTIZATION MONTHS
      *    (ZERO = SAME AS TERM). IO = LEADING INTEREST-ONLY MONTHS
           05  AMT-TERM-MONTHS             PIC 9(03).
           05  AMT-AMORT-MONTHS            PIC 9(03).
           05  AMT-IO-MONTHS               PIC 9(03).
           05  AMT-PERIOD-NUMBER           PIC 9(03).
      *    INPUT FOR MAXP, OUTPUT FOR PMT AND LINE
           05  AMT-PAYMENT-AMT             PIC S9(13)V99 COMP-3.
      *--- OUTPUT ----------------------------------------------------*
           05  AMT-PERIODIC-RATE           PIC 9V9(15)    COMP-3.
           05  AMT-NUM-PAYMENTS            PIC 9(03).
           05  AMT-NUM-AMORT-PAYMENTS      PIC 9(03).
           05  AMT-IO-PAYMENT-AMT          PIC S9(13)V99 COMP-3.
           05  AMT-FINAL-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
           05  AMT-BALLOON-AMT             PIC S9(13)V99 COMP-3.
           05  AMT-TOTAL-INTEREST          PIC S9(13)V99 COMP-3.
           05  AMT-MAX-PRINCIPAL           PIC S9(13)V99 COMP-3.
           05  AMT-LINE-TYPE               PIC X(01).
               88  AMT-LINE-IO                       VALUE 'I'.
               88  AMT-LINE-AMORTIZING               VALUE 'A'.
               88  AMT-LINE-FINAL                    VALUE 'F'.
               88  AMT-LINE-BALLOON                  VALUE 'B'.
           05  AMT-LINE-BEGIN-BAL          PIC S9(13)V99 COMP-3.
           05  AMT-LINE-PAYMENT            PIC S9(13)V99 COMP-3.
           05  AMT-LINE-INTEREST           PIC S9(13)V99 COMP-3.
           05  AMT-LINE-PRINCIPAL          PIC S9(13)V99 COMP-3.
           05  AMT-LINE-END-BAL            PIC S9(13)V99 COMP-3.
           05  AMT-RETURN-CODE             PIC 9(02).
               88  AMT-OK                            VALUE 00.
               88  AMT-INVALID-INPUT                 VALUE 12.
