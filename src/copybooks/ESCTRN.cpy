      *================================================================*
      * COPYBOOK : ESCTRN                                              *
      * PURPOSE  : MORTGAGE ESCROW DAILY TRANSACTION RECORD            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-12-14 DPS  ORIGINAL                                       *
      * 1992-08-15 CRP  SHORTAGE TABLE EXPANDED INC6957                *
      * 1993-06-06 SKN  CUSHION EXCEPTIONS TO OPS QUEUE CHG5878        *
      * 1998-01-17 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-16 PQR  TAX ROUNDING - HALF UP CHG7006                 *
      * 1999-11-04 SKN  SURPLUS STATUS CODES ADDED REQ6643             *
      * 2003-05-08 GAF  CUSHION CUTOFF MOVED REQ5471                   *
      * 2004-08-03 RJT  PERFORMANCE - SURPLUS READ AHEAD REQ1979       *
      * 2008-11-20 BHS  NEW SHORTAGE REPORT COLUMNS PRB7162            *
      * 2013-03-12 BHS  DISBURSE FEE SCHEDULE UPDATE PRB0977           *
      * 2013-04-18 MKL  NEW TAX REPORT COLUMNS REQ8767                 *
      *================================================================*
       01  ESC-TRAN-REC.
           05  ESCT-KEY                    PIC X(12).
           05  ESCT-OPERATOR               PIC X(08).
           05  ESCT-REJECT-CD              PIC X(04).
           05  ESCT-MEMO                   PIC X(30).
           05  ESC-TRAN-AMOUNTS.
               10  ESCT-CUSHION-AMT        PIC S9(13)V99 COMP-3.
               10  ESCT-SHORTAGE-AMT       PIC S9(13)V99 COMP-3.
               10  ESCT-INSURANCE-AMT      PIC S9(13)V99 COMP-3.
               10  ESCT-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
           05  ESC-TRAN-DATES.
               10  ESCT-EFF-DATE           PIC 9(08).
               10  ESCT-POST-DATE          PIC 9(08).
           05  ESC-TRAN-CODES.
               10  ESCT-TRAN-CD            PIC X(02).
                   88  ESCT-TRAN-CD-DS             VALUE 'DS'.
                   88  ESCT-TRAN-CD-PY             VALUE 'PY'.
                   88  ESCT-TRAN-CD-AN             VALUE 'AN'.
                   88  ESCT-TRAN-CD-RF             VALUE 'RF'.
                   88  ESCT-TRAN-CD-SH             VALUE 'SH'.
               10  ESCT-CHANNEL-CD         PIC X(03).
                   88  ESCT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  ESCT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  ESCT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  ESCT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  ESCT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  ESCT-REVERSAL-FLAG      PIC X(01).
                   88  ESCT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  ESC-TRAN-COUNTERS.
               10  ESCT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
