      *================================================================*
      * COPYBOOK : AMORTREC                                            *
      * PURPOSE  : AMORTIZATION SCHEDULE LINE - ONE PERIOD OF A LOAN   *
      *            REPAYMENT SCHEDULE. RETURNED BY AMORTCLC (FUNCTION  *
      *            'LINE') AND USED BY SCHEDULE DISPLAYS / PRINTS.     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 2003-01-20 BHS  LINE TYPE - INTEREST ONLY / BALLOON            *
      *================================================================*
       01  AMORT-SCHEDULE-LINE.
           05  ASL-LOAN-NUMBER             PIC X(12).
           05  ASL-PERIOD-NUMBER           PIC 9(03).
           05  ASL-DUE-DATE                PIC 9(08).
           05  ASL-LINE-TYPE               PIC X(01).
               88  ASL-INTEREST-ONLY                 VALUE 'I'.
               88  ASL-AMORTIZING                    VALUE 'A'.
               88  ASL-FINAL-PAYMENT                 VALUE 'F'.
               88  ASL-BALLOON                       VALUE 'B'.
           05  ASL-BEGIN-BALANCE           PIC S9(13)V99 COMP-3.
           05  ASL-PAYMENT-AMT             PIC S9(13)V99 COMP-3.
           05  ASL-INTEREST-AMT            PIC S9(13)V99 COMP-3.
           05  ASL-PRINCIPAL-AMT           PIC S9(13)V99 COMP-3.
           05  ASL-END-BALANCE             PIC S9(13)V99 COMP-3.
           05  ASL-CUM-INTEREST            PIC S9(13)V99 COMP-3.
