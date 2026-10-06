      *================================================================*
      * COPYBOOK : INTPARM                                             *
      * PURPOSE  : LINKAGE FOR INTCALC - ONE DAY OF INTEREST ACCRUAL   *
      *   INPUT : PRODUCT, ACCRUAL BASIS (PROD-ACCRUAL-BASIS),         *
      *           COMPOUNDING (PROD-COMPOUND-FREQ), THE CALENDAR DAY   *
      *           BEING ACCRUED, END-OF-DAY LEDGER BALANCE, ACCRUED    *
      *           (UNPAID) INTEREST TO DATE AND THE ANNUAL RATE.       *
      *   OUTPUT: THE DAY'S ACCRUAL (6 DECIMALS, TRUNCATED), THE RATE  *
      *           ACTUALLY APPLIED, THE DAY-COUNT DIVISOR AND THE      *
      *           PRINCIPAL THE ACCRUAL WAS COMPUTED ON.               *
      *   RETURN CODES: 00 ACCRUED                                     *
      *                 04 NO ACCRUAL (NEGATIVE / BELOW MINIMUM /      *
      *                    30-360 DAY 31)                              *
      *                 12 INVALID BASIS OR DATE                       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 2003-05-09 BHS  30/360 AND ACTUAL/360 BASIS                    *
      * 2017-02-08 AKW  DAILY COMPOUNDING FOR ONLINE SAVINGS           *
      *================================================================*
       01  INTCALC-PARMS.
           05  INT-PRODUCT-CODE            PIC X(04).
           05  INT-ACCRUAL-BASIS           PIC X(01).
               88  INT-BASIS-ACT-365                 VALUE '3'.
               88  INT-BASIS-ACT-360                 VALUE 'A'.
               88  INT-BASIS-30-360                  VALUE '0'.
           05  INT-COMPOUND-FREQ           PIC X(01).
               88  INT-COMPOUND-DAILY                VALUE 'D'.
           05  INT-ACCRUAL-DATE            PIC 9(08).
           05  INT-BALANCE                 PIC S9(13)V99 COMP-3.
           05  INT-ACCRUED-TO-DATE         PIC S9(09)V9(06) COMP-3.
           05  INT-ANNUAL-RATE             PIC 9(03)V9(06).
      *--- OUTPUT ----------------------------------------------------*
           05  INT-DAILY-ACCRUAL           PIC S9(09)V9(06) COMP-3.
           05  INT-APPLIED-RATE            PIC 9(03)V9(06).
           05  INT-DAY-BASIS               PIC 9(03).
           05  INT-PRINCIPAL-USED          PIC S9(13)V9(06) COMP-3.
           05  INT-REASON-CODE             PIC X(02).
               88  INT-RSN-ACCRUED                   VALUE 'AC'.
               88  INT-RSN-NEGATIVE                  VALUE 'NG'.
               88  INT-RSN-BELOW-MIN                 VALUE 'BM'.
               88  INT-RSN-REDUCED-RATE              VALUE 'RR'.
               88  INT-RSN-DAY-31                    VALUE '31'.
           05  INT-RETURN-CODE             PIC 9(02).
