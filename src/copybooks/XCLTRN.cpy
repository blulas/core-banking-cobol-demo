      *================================================================*
      * COPYBOOK : XCLTRN                                              *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS DAILY TRANSACTION RECORD     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-22 EWS  ORIGINAL                                       *
      * 1991-04-26 RJT  PENALTY STATUS CODES ADDED CHG1445             *
      * 1992-04-25 MKL  ACCRUED CALCULATION CORRECTED CHG7012          *
      * 1992-01-26 BHS  GOAL ROUNDING - HALF UP PRB7568                *
      * 1994-12-10 BHS  CONTRIB RERUN PROTECTION INC9685               *
      * 1994-03-18 PQR  NEW ACCRUED REPORT COLUMNS PRB1210             *
      * 1998-11-28 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-21 GAF  ACCRUED CALCULATION CORRECTED REQ9794          *
      * 1999-06-19 DPS  PENALTY ROUNDING - HALF UP INC9634             *
      * 2001-08-23 DPS  WEEKLY CALCULATION CORRECTED PRB0242           *
      * 2004-04-27 LMB  CONTRIB TABLE EXPANDED INC6893                 *
      *================================================================*
       01  XCL-TRAN-REC.
           05  XCLT-KEY                    PIC X(12).
           05  XCLT-OPERATOR               PIC X(08).
           05  XCLT-REJECT-CD              PIC X(04).
           05  XCLT-MEMO                   PIC X(30).
           05  XCL-TRAN-AMOUNTS.
               10  XCLT-BONUS-AMT          PIC S9(13)V99 COMP-3.
               10  XCLT-WEEKLY-AMT         PIC S9(13)V99 COMP-3.
               10  XCLT-GOAL-AMT           PIC S9(13)V99 COMP-3.
               10  XCLT-CONTRIB-AMT        PIC S9(13)V99 COMP-3.
           05  XCL-TRAN-DATES.
               10  XCLT-EFF-DATE           PIC 9(08).
               10  XCLT-POST-DATE          PIC 9(08).
           05  XCL-TRAN-CODES.
               10  XCLT-TRAN-CD            PIC X(02).
                   88  XCLT-TRAN-CD-WD             VALUE 'WD'.
                   88  XCLT-TRAN-CD-DP             VALUE 'DP'.
                   88  XCLT-TRAN-CD-PO             VALUE 'PO'.
                   88  XCLT-TRAN-CD-BN             VALUE 'BN'.
                   88  XCLT-TRAN-CD-PN             VALUE 'PN'.
               10  XCLT-CHANNEL-CD         PIC X(03).
                   88  XCLT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  XCLT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  XCLT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  XCLT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  XCLT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  XCLT-REVERSAL-FLAG      PIC X(01).
                   88  XCLT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  XCL-TRAN-COUNTERS.
               10  XCLT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
