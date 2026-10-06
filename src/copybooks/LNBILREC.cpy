      *================================================================*
      * COPYBOOK : LNBILREC                                            *
      * PURPOSE  : LOAN BILLING RECORD - ONE PER BILL. INPUT TO THE    *
      *            STATEMENT / E-BILL VENDOR EXTRACT.                  *
      * FILE     : LNBILLS  (SEQUENTIAL, FIXED, DISPLAY)               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 2010-02-22 DPS  CARD ACT STYLE LATE FEE WARNING AMOUNT         *
      *================================================================*
       01  LOAN-BILL-RECORD.
           05  LBL-LOAN-NUMBER             PIC X(12).
           05  LBL-PRODUCT-CODE            PIC X(04).
           05  LBL-BORROWER-CUST-ID        PIC X(10).
           05  LBL-BILL-DATE               PIC 9(08).
           05  LBL-DUE-DATE                PIC 9(08).
           05  LBL-INSTALLMENT-NO          PIC 9(03).
           05  LBL-CURR-INTEREST           PIC 9(09)V99.
           05  LBL-CURR-PRINCIPAL          PIC 9(11)V99.
           05  LBL-PAST-DUE-AMT            PIC 9(11)V99.
           05  LBL-LATE-FEES-DUE           PIC 9(07)V99.
           05  LBL-TOTAL-DUE               PIC 9(11)V99.
           05  LBL-PRINCIPAL-BAL           PIC 9(13)V99.
           05  LBL-INT-RATE                PIC 9(03)V9(06).
           05  LBL-DAYS-PAST-DUE           PIC 9(04).
      *    FEE THAT WILL BE CHARGED IF THE PAYMENT IS LATE
           05  LBL-LATE-FEE-WARNING        PIC 9(05)V99.
           05  LBL-AUTOPAY-FLAG            PIC X(01).
           05  LBL-AUTOPAY-ACCT            PIC X(12).
           05  LBL-MESSAGE-CODE            PIC X(04).
           05  FILLER                      PIC X(20).
