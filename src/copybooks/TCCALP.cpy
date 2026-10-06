      *================================================================*
      * COPYBOOK : TCCALP                                              *
      * PURPOSE  : LINKAGE FOR TRAVELERS CHEQUES RULE MODULE TCCAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-15 MKL  ORIGINAL                                       *
      * 1990-07-18 BHS  REMIT FEE SCHEDULE UPDATE PRB3461              *
      * 1997-01-04 GAF  REMIT LIMIT REVISED PRB2217                    *
      * 1999-11-16 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-04 CRP  REMIT EXCEPTIONS TO OPS QUEUE PRB5964          *
      * 2000-06-04 BHS  AUDIT FINDING - REMIT CONTROL TOTALS CHG3786   *
      * 2001-07-06 GAF  CONSIGN RERUN PROTECTION INC7064               *
      * 2003-02-11 PQR  FACE CALCULATION CORRECTED REQ9836             *
      * 2008-12-10 BHS  AUDIT FINDING - SHORTAGE CONTROL TOTALS PRB6765*
      * 2011-06-25 LMB  AUDIT FINDING - COMMISSION CONTROL TOTALS CHG23*
      * 2012-08-01 PQR  CONSIGN STATUS CODES ADDED PRB5346             *
      *================================================================*
       01  TCCAL-PARMS.
           05  TCCAL-COMMISSION-YTD        PIC S9(13)V99 COMP-3.
           05  TCCAL-SHORTAGE-YTD          PIC S9(13)V99 COMP-3.
           05  TCCAL-COMMISSION-AMT        PIC S9(13)V99 COMP-3.
           05  TCCAL-SHORTAGE-AMT          PIC S9(13)V99 COMP-3.
           05  TCCAL-CONSIGN-AMT           PIC S9(13)V99 COMP-3.
           05  TCCAL-CONSIGN-YTD           PIC S9(13)V99 COMP-3.
           05  TCCAL-MATURITY-DATE         PIC 9(08).
           05  TCCAL-LAST-ACT-DATE         PIC 9(08).
           05  TCCAL-EFFECTIVE-DATE        PIC 9(08).
           05  TCCAL-NEXT-DUE-DATE         PIC 9(08).
           05  TCCAL-BRANCH-CD             PIC X(02).
               88  TCCAL-BRANCH-CD-EG              VALUE 'EG'.
               88  TCCAL-BRANCH-CD-HZ              VALUE 'HZ'.
               88  TCCAL-BRANCH-CD-JF              VALUE 'JF'.
               88  TCCAL-BRANCH-CD-NZ              VALUE 'NZ'.
           05  TCCAL-DENOM-CD              PIC X(01).
               88  TCCAL-DENOM-CD-G                VALUE 'G'.
               88  TCCAL-DENOM-CD-M                VALUE 'M'.
               88  TCCAL-DENOM-CD-N                VALUE 'N'.
               88  TCCAL-DENOM-CD-P                VALUE 'P'.
               88  TCCAL-DENOM-CD-V                VALUE 'V'.
               88  TCCAL-DENOM-CD-Y                VALUE 'Y'.
           05  TCCAL-ISSUER-CD             PIC X(02).
               88  TCCAL-ISSUER-CD-AF              VALUE 'AF'.
               88  TCCAL-ISSUER-CD-CJ              VALUE 'CJ'.
               88  TCCAL-ISSUER-CD-DU              VALUE 'DU'.
               88  TCCAL-ISSUER-CD-JY              VALUE 'JY'.
               88  TCCAL-ISSUER-CD-RR              VALUE 'RR'.
               88  TCCAL-ISSUER-CD-TK              VALUE 'TK'.
           05  TCCAL-SOURCE-CD             PIC X(02).
               88  TCCAL-SOURCE-CD-EG              VALUE 'EG'.
               88  TCCAL-SOURCE-CD-GH              VALUE 'GH'.
               88  TCCAL-SOURCE-CD-LC              VALUE 'LC'.
               88  TCCAL-SOURCE-CD-LU              VALUE 'LU'.
               88  TCCAL-SOURCE-CD-YS              VALUE 'YS'.
           05  TCCAL-EXEMPT-FLAG           PIC X(01).
               88  TCCAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  TCCAL-REVIEW-FLAG           PIC X(01).
               88  TCCAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TCCAL-STOP-FLAG             PIC X(01).
               88  TCCAL-STOP-FLAG-YES             VALUE 'Y'.
           05  TCCAL-EXCEPT-CNT            PIC 9(07).
           05  TCCAL-ITEM-CNT              PIC 9(07).
           05  TCCAL-RATE                  PIC 9(03)V9(06).
           05  TCCAL-FEE-PCT               PIC 9(03)V9(06).
           05  TCCAL-NAME                  PIC X(30).
           05  TCCAL-ADDR                  PIC X(30).
           05  TCCAL-RETURN-CODE           PIC 9(02).
