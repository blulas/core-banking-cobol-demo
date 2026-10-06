      *================================================================*
      * COPYBOOK : TRVALP                                              *
      * PURPOSE  : LINKAGE FOR PERSONAL TRUST RULE MODULE TRVAL        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-08-27 JLM  ORIGINAL                                       *
      * 1988-02-16 SKN  INCOME EXCEPTIONS TO OPS QUEUE CHG9262         *
      * 1988-05-09 BHS  PRINCIPAL STATUS CODES ADDED PRB9262           *
      * 1994-11-07 GAF  PRINCIPAL ROUNDING - HALF UP CHG4879           *
      * 1997-02-14 GAF  INCOME CUTOFF MOVED PRB2900                    *
      * 2001-08-12 JLM  INCOME NOTICE TEXT PER LEGAL CHG6204           *
      * 2002-03-25 MKL  PERFORMANCE - DISTRIB READ AHEAD PRB7809       *
      *================================================================*
       01  TRVAL-PARMS.
           05  TRVAL-MARKET-AMT            PIC S9(13)V99 COMP-3.
           05  TRVAL-TAX-AMT               PIC S9(13)V99 COMP-3.
           05  TRVAL-PRINCIPAL-YTD         PIC S9(13)V99 COMP-3.
           05  TRVAL-DISTRIB-YTD           PIC S9(13)V99 COMP-3.
           05  TRVAL-MARKET-YTD            PIC S9(13)V99 COMP-3.
           05  TRVAL-PRINCIPAL-AMT         PIC S9(13)V99 COMP-3.
           05  TRVAL-NOTICE-DATE           PIC 9(08).
           05  TRVAL-LAST-MAINT-DATE       PIC 9(08).
           05  TRVAL-MATURITY-DATE         PIC 9(08).
           05  TRVAL-CLOSE-DATE            PIC 9(08).
           05  TRVAL-REASON-CD             PIC X(01).
               88  TRVAL-REASON-CD-H               VALUE 'H'.
               88  TRVAL-REASON-CD-M               VALUE 'M'.
               88  TRVAL-REASON-CD-T               VALUE 'T'.
           05  TRVAL-REGION-CD             PIC X(01).
               88  TRVAL-REGION-CD-A               VALUE 'A'.
               88  TRVAL-REGION-CD-D               VALUE 'D'.
               88  TRVAL-REGION-CD-N               VALUE 'N'.
               88  TRVAL-REGION-CD-T               VALUE 'T'.
           05  TRVAL-STATUS-CD             PIC X(02).
               88  TRVAL-STATUS-CD-EH              VALUE 'EH'.
               88  TRVAL-STATUS-CD-GG              VALUE 'GG'.
               88  TRVAL-STATUS-CD-JG              VALUE 'JG'.
           05  TRVAL-TRUST-TYPE-CD         PIC X(03).
               88  TRVAL-TRUST-TYPE-CD-FLJ         VALUE 'FLJ'.
               88  TRVAL-TRUST-TYPE-CD-MNG         VALUE 'MNG'.
               88  TRVAL-TRUST-TYPE-CD-VFN         VALUE 'VFN'.
               88  TRVAL-TRUST-TYPE-CD-XPL         VALUE 'XPL'.
           05  TRVAL-NOTICE-SENT-FLAG      PIC X(01).
               88  TRVAL-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  TRVAL-REVIEW-FLAG           PIC X(01).
               88  TRVAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TRVAL-EXEMPT-FLAG           PIC X(01).
               88  TRVAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  TRVAL-CYCLE-CNT             PIC 9(07).
           05  TRVAL-NOTICE-CNT            PIC 9(07).
           05  TRVAL-RATE                  PIC 9(03)V9(06).
           05  TRVAL-MARGIN                PIC 9(03)V9(06).
           05  TRVAL-ADDR                  PIC X(30).
           05  TRVAL-CONTACT               PIC X(20).
           05  TRVAL-RETURN-CODE           PIC 9(02).
