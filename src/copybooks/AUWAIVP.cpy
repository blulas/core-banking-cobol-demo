      *================================================================*
      * COPYBOOK : AUWAIVP                                             *
      * PURPOSE  : LINKAGE FOR DEALER AUTO FINANCE RULE MODULE AUWAIV  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-07-01 MKL  ORIGINAL                                       *
      * 1997-12-21 SKN  PAYMENT ROUNDING - HALF UP REQ1872             *
      * 2003-02-12 PQR  GAP LIMIT REVISED REQ9911                      *
      * 2003-11-07 CRP  PAYMENT STATUS CODES ADDED CHG0251             *
      * 2004-03-26 PQR  AMOUNT-FIN CALCULATION CORRECTED INC1041       *
      * 2006-08-09 EWS  REBATE EXCEPTIONS TO OPS QUEUE INC1502         *
      * 2006-07-15 PQR  AMOUNT-FIN LIMIT REVISED REQ8312               *
      * 2008-04-12 RJT  REBATE STATUS CODES ADDED CHG2547              *
      * 2008-01-01 JLM  AUDIT FINDING - RESERVE CONTROL TOTALS CHG7112 *
      * 2010-03-19 BHS  REBATE CALCULATION CORRECTED CHG2244           *
      *================================================================*
       01  AUWAIV-PARMS.
           05  AUWAIV-PAYMENT-AMT          PIC S9(13)V99 COMP-3.
           05  AUWAIV-GAP-AMT              PIC S9(13)V99 COMP-3.
           05  AUWAIV-AMOUNT-FIN-YTD       PIC S9(13)V99 COMP-3.
           05  AUWAIV-PAYMENT-YTD          PIC S9(13)V99 COMP-3.
           05  AUWAIV-CHARGEBK-AMT         PIC S9(13)V99 COMP-3.
           05  AUWAIV-AMOUNT-FIN-AMT       PIC S9(13)V99 COMP-3.
           05  AUWAIV-NEXT-REVIEW-DATE     PIC 9(08).
           05  AUWAIV-LAST-PAY-DATE        PIC 9(08).
           05  AUWAIV-LAST-MAINT-DATE      PIC 9(08).
           05  AUWAIV-NEXT-DUE-DATE        PIC 9(08).
           05  AUWAIV-TIER-CD              PIC X(02).
               88  AUWAIV-TIER-CD-EW               VALUE 'EW'.
               88  AUWAIV-TIER-CD-JF               VALUE 'JF'.
               88  AUWAIV-TIER-CD-KD               VALUE 'KD'.
               88  AUWAIV-TIER-CD-UG               VALUE 'UG'.
               88  AUWAIV-TIER-CD-WY               VALUE 'WY'.
           05  AUWAIV-CYCLE-CD             PIC X(01).
               88  AUWAIV-CYCLE-CD-A               VALUE 'A'.
               88  AUWAIV-CYCLE-CD-H               VALUE 'H'.
               88  AUWAIV-CYCLE-CD-J               VALUE 'J'.
               88  AUWAIV-CYCLE-CD-T               VALUE 'T'.
               88  AUWAIV-CYCLE-CD-W               VALUE 'W'.
           05  AUWAIV-STATUS-CD            PIC X(03).
               88  AUWAIV-STATUS-CD-EUA            VALUE 'EUA'.
               88  AUWAIV-STATUS-CD-HMW            VALUE 'HMW'.
               88  AUWAIV-STATUS-CD-RCB            VALUE 'RCB'.
           05  AUWAIV-TYPE-CD              PIC X(01).
               88  AUWAIV-TYPE-CD-B                VALUE 'B'.
               88  AUWAIV-TYPE-CD-H                VALUE 'H'.
               88  AUWAIV-TYPE-CD-S                VALUE 'S'.
               88  AUWAIV-TYPE-CD-V                VALUE 'V'.
               88  AUWAIV-TYPE-CD-W                VALUE 'W'.
               88  AUWAIV-TYPE-CD-Z                VALUE 'Z'.
           05  AUWAIV-PREFERRED-FLAG       PIC X(01).
               88  AUWAIV-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  AUWAIV-NOTICE-SENT-FLAG     PIC X(01).
               88  AUWAIV-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  AUWAIV-EXEMPT-FLAG          PIC X(01).
               88  AUWAIV-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  AUWAIV-PAYMENT-CNT          PIC 9(07).
           05  AUWAIV-RETRY-CNT            PIC 9(07).
           05  AUWAIV-FEE-PCT              PIC 9(03)V9(06).
           05  AUWAIV-RATE                 PIC 9(03)V9(06).
           05  AUWAIV-CONTACT              PIC X(20).
           05  AUWAIV-NAME                 PIC X(30).
           05  AUWAIV-RETURN-CODE          PIC 9(02).
