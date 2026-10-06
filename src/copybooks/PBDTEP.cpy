      *================================================================*
      * COPYBOOK : PBDTEP                                              *
      * PURPOSE  : LINKAGE FOR PASSBOOK PRINTING RULE MODULE PBDTE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-12 BHS  ORIGINAL                                       *
      * 2000-01-06 TWH  CARRYFWD FEE SCHEDULE UPDATE REQ6005           *
      * 2003-12-07 LMB  UNPOSTED EXCEPTIONS TO OPS QUEUE PRB5816       *
      * 2003-09-25 MKL  BALANCE STATUS CODES ADDED INC3937             *
      * 2008-07-27 PQR  BALANCE CUTOFF MOVED REQ3716                   *
      * 2010-04-09 DPS  LINE EXCEPTIONS TO OPS QUEUE REQ6481           *
      * 2012-12-05 GAF  PRINTED LIMIT REVISED INC7427                  *
      *================================================================*
       01  PBDTE-PARMS.
           05  PBDTE-PRINTED-YTD           PIC S9(13)V99 COMP-3.
           05  PBDTE-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  PBDTE-UNPOSTED-YTD          PIC S9(13)V99 COMP-3.
           05  PBDTE-UNPOSTED-AMT          PIC S9(13)V99 COMP-3.
           05  PBDTE-CARRYFWD-AMT          PIC S9(13)V99 COMP-3.
           05  PBDTE-CARRYFWD-YTD          PIC S9(13)V99 COMP-3.
           05  PBDTE-NEXT-DUE-DATE         PIC 9(08).
           05  PBDTE-MATURITY-DATE         PIC 9(08).
           05  PBDTE-OPEN-DATE             PIC 9(08).
           05  PBDTE-CLOSE-DATE            PIC 9(08).
           05  PBDTE-REGION-CD             PIC X(02).
               88  PBDTE-REGION-CD-ER              VALUE 'ER'.
               88  PBDTE-REGION-CD-GL              VALUE 'GL'.
               88  PBDTE-REGION-CD-PE              VALUE 'PE'.
               88  PBDTE-REGION-CD-RJ              VALUE 'RJ'.
               88  PBDTE-REGION-CD-VB              VALUE 'VB'.
           05  PBDTE-SOURCE-CD             PIC X(02).
               88  PBDTE-SOURCE-CD-JX              VALUE 'JX'.
               88  PBDTE-SOURCE-CD-ZC              VALUE 'ZC'.
               88  PBDTE-SOURCE-CD-ZY              VALUE 'ZY'.
           05  PBDTE-BRANCH-CD             PIC X(02).
               88  PBDTE-BRANCH-CD-DF              VALUE 'DF'.
               88  PBDTE-BRANCH-CD-NV              VALUE 'NV'.
               88  PBDTE-BRANCH-CD-PP              VALUE 'PP'.
               88  PBDTE-BRANCH-CD-SH              VALUE 'SH'.
               88  PBDTE-BRANCH-CD-ZP              VALUE 'ZP'.
           05  PBDTE-PRINTER-CD            PIC X(02).
           05  PBDTE-REVIEW-FLAG           PIC X(01).
               88  PBDTE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  PBDTE-EXEMPT-FLAG           PIC X(01).
               88  PBDTE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  PBDTE-SUSPEND-FLAG          PIC X(01).
               88  PBDTE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  PBDTE-CYCLE-CNT             PIC 9(07).
           05  PBDTE-RETRY-CNT             PIC 9(07).
           05  PBDTE-MARGIN                PIC 9(03)V9(06).
           05  PBDTE-FEE-PCT               PIC 9(03)V9(06).
           05  PBDTE-CUST-ID               PIC X(10).
           05  PBDTE-NAME                  PIC X(30).
           05  PBDTE-RETURN-CODE           PIC 9(02).
