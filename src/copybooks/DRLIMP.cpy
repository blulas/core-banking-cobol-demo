      *================================================================*
      * COPYBOOK : DRLIMP                                              *
      * PURPOSE  : LINKAGE FOR DIVIDEND REINVESTMENT RULE MODULE DRLIM *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-03-18 CRP  ORIGINAL                                       *
      * 1991-09-08 MKL  DIVIDEND STATUS CODES ADDED PRB3632            *
      * 1995-06-20 SKN  REINVEST ROUNDING - HALF UP CHG6896            *
      * 1995-12-26 MKL  DIVIDEND NOTICE TEXT PER LEGAL INC8314         *
      * 1997-09-02 EWS  PERFORMANCE - WITHHOLD READ AHEAD INC4719      *
      * 1998-03-04 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-06-06 GAF  CASH-PAY STATUS CODES ADDED PRB0188            *
      * 2000-12-23 EWS  FEE RERUN PROTECTION REQ2808                   *
      * 2003-08-18 JLM  FEE EXCEPTIONS TO OPS QUEUE REQ4353            *
      * 2009-02-19 EWS  DIVIDEND ROUNDING - HALF UP REQ2038            *
      * 2010-09-15 GAF  CASH-PAY NOTICE TEXT PER LEGAL REQ4253         *
      *================================================================*
       01  DRLIM-PARMS.
           05  DRLIM-CASH-PAY-AMT          PIC S9(13)V99 COMP-3.
           05  DRLIM-DIVIDEND-AMT          PIC S9(13)V99 COMP-3.
           05  DRLIM-CASH-PAY-YTD          PIC S9(13)V99 COMP-3.
           05  DRLIM-WITHHOLD-YTD          PIC S9(13)V99 COMP-3.
           05  DRLIM-WITHHOLD-AMT          PIC S9(13)V99 COMP-3.
           05  DRLIM-REINVEST-AMT          PIC S9(13)V99 COMP-3.
           05  DRLIM-NEXT-REVIEW-DATE      PIC 9(08).
           05  DRLIM-LAST-PAY-DATE         PIC 9(08).
           05  DRLIM-MATURITY-DATE         PIC 9(08).
           05  DRLIM-LAST-MAINT-DATE       PIC 9(08).
           05  DRLIM-HOLDER-TYPE-CD        PIC X(01).
               88  DRLIM-HOLDER-TYPE-CD-C          VALUE 'C'.
               88  DRLIM-HOLDER-TYPE-CD-H          VALUE 'H'.
               88  DRLIM-HOLDER-TYPE-CD-K          VALUE 'K'.
               88  DRLIM-HOLDER-TYPE-CD-L          VALUE 'L'.
               88  DRLIM-HOLDER-TYPE-CD-M          VALUE 'M'.
               88  DRLIM-HOLDER-TYPE-CD-X          VALUE 'X'.
           05  DRLIM-CHANNEL-CD            PIC X(02).
               88  DRLIM-CHANNEL-CD-RM             VALUE 'RM'.
               88  DRLIM-CHANNEL-CD-VP             VALUE 'VP'.
               88  DRLIM-CHANNEL-CD-YS             VALUE 'YS'.
           05  DRLIM-SOURCE-CD             PIC X(02).
               88  DRLIM-SOURCE-CD-BT              VALUE 'BT'.
               88  DRLIM-SOURCE-CD-FL              VALUE 'FL'.
               88  DRLIM-SOURCE-CD-NK              VALUE 'NK'.
               88  DRLIM-SOURCE-CD-YC              VALUE 'YC'.
           05  DRLIM-PLAN-OPTION-CD        PIC X(01).
               88  DRLIM-PLAN-OPTION-CD-E          VALUE 'E'.
               88  DRLIM-PLAN-OPTION-CD-R          VALUE 'R'.
               88  DRLIM-PLAN-OPTION-CD-T          VALUE 'T'.
               88  DRLIM-PLAN-OPTION-CD-Z          VALUE 'Z'.
           05  DRLIM-EXEMPT-FLAG           PIC X(01).
               88  DRLIM-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  DRLIM-PREFERRED-FLAG        PIC X(01).
               88  DRLIM-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  DRLIM-SUSPEND-FLAG          PIC X(01).
               88  DRLIM-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  DRLIM-PAYMENT-CNT           PIC 9(07).
           05  DRLIM-NOTICE-CNT            PIC 9(07).
           05  DRLIM-RATE                  PIC 9(03)V9(06).
           05  DRLIM-DISCOUNT              PIC 9(03)V9(06).
           05  DRLIM-CUST-ID               PIC X(10).
           05  DRLIM-DESC                  PIC X(30).
           05  DRLIM-RETURN-CODE           PIC 9(02).
