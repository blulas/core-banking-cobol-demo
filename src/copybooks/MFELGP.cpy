      *================================================================*
      * COPYBOOK : MFELGP                                              *
      * PURPOSE  : LINKAGE FOR MICROFICHE STATEMENTS RULE MODULE MFELG *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-07 SKN  ORIGINAL                                       *
      * 1997-09-23 JLM  STORAGE ROUNDING - HALF UP REQ3094             *
      * 1998-09-11 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-24 TWH  POSTAGE CUTOFF MOVED PRB1159                   *
      * 2001-07-02 GAF  STORAGE STATUS CODES ADDED REQ8713             *
      * 2002-05-24 RJT  COPY-FEE FEE SCHEDULE UPDATE PRB0597           *
      * 2002-03-06 TWH  RESEARCH LIMIT REVISED INC9882                 *
      * 2003-11-07 BHS  PERFORMANCE - POSTAGE READ AHEAD PRB1811       *
      * 2005-02-07 LMB  POSTAGE LIMIT REVISED INC0572                  *
      * 2008-03-07 PQR  STORAGE CUTOFF MOVED CHG5042                   *
      *================================================================*
       01  MFELG-PARMS.
           05  MFELG-POSTAGE-YTD           PIC S9(13)V99 COMP-3.
           05  MFELG-POSTAGE-AMT           PIC S9(13)V99 COMP-3.
           05  MFELG-RESEARCH-AMT          PIC S9(13)V99 COMP-3.
           05  MFELG-COPY-FEE-AMT          PIC S9(13)V99 COMP-3.
           05  MFELG-STORAGE-AMT           PIC S9(13)V99 COMP-3.
           05  MFELG-COPY-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  MFELG-NOTICE-DATE           PIC 9(08).
           05  MFELG-LAST-STMT-DATE        PIC 9(08).
           05  MFELG-CLOSE-DATE            PIC 9(08).
           05  MFELG-MATURITY-DATE         PIC 9(08).
           05  MFELG-BRANCH-CD             PIC X(02).
               88  MFELG-BRANCH-CD-AT              VALUE 'AT'.
               88  MFELG-BRANCH-CD-KR              VALUE 'KR'.
               88  MFELG-BRANCH-CD-LK              VALUE 'LK'.
               88  MFELG-BRANCH-CD-US              VALUE 'US'.
           05  MFELG-CHANNEL-CD            PIC X(02).
           05  MFELG-TYPE-CD               PIC X(01).
               88  MFELG-TYPE-CD-G                 VALUE 'G'.
               88  MFELG-TYPE-CD-K                 VALUE 'K'.
               88  MFELG-TYPE-CD-W                 VALUE 'W'.
               88  MFELG-TYPE-CD-Y                 VALUE 'Y'.
           05  MFELG-FICHE-TYPE-CD         PIC X(02).
               88  MFELG-FICHE-TYPE-CD-AW          VALUE 'AW'.
               88  MFELG-FICHE-TYPE-CD-JM          VALUE 'JM'.
               88  MFELG-FICHE-TYPE-CD-KV          VALUE 'KV'.
               88  MFELG-FICHE-TYPE-CD-MD          VALUE 'MD'.
               88  MFELG-FICHE-TYPE-CD-NR          VALUE 'NR'.
           05  MFELG-EXEMPT-FLAG           PIC X(01).
               88  MFELG-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  MFELG-NOTICE-SENT-FLAG      PIC X(01).
               88  MFELG-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  MFELG-REVIEW-FLAG           PIC X(01).
               88  MFELG-REVIEW-FLAG-YES           VALUE 'Y'.
           05  MFELG-ITEM-CNT              PIC 9(07).
           05  MFELG-PAYMENT-CNT           PIC 9(07).
           05  MFELG-DISCOUNT              PIC 9(03)V9(06).
           05  MFELG-MARGIN                PIC 9(03)V9(06).
           05  MFELG-ADDR                  PIC X(30).
           05  MFELG-CUST-ID               PIC X(10).
           05  MFELG-RETURN-CODE           PIC 9(02).
