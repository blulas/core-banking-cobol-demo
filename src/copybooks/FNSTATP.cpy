      *================================================================*
      * COPYBOOK : FNSTATP                                             *
      * PURPOSE  : LINKAGE FOR FOREIGN BANKNOTES RULE MODULE FNSTAT    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-21 BHS  ORIGINAL                                       *
      * 1996-05-06 TWH  BUYBACK EXCEPTIONS TO OPS QUEUE CHG0389        *
      * 1997-08-13 EWS  USD RERUN PROTECTION REQ8672                   *
      * 1997-05-08 CRP  BUYBACK RERUN PROTECTION PRB5198               *
      * 1999-04-06 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-10-14 EWS  AUDIT FINDING - MARGIN CONTROL TOTALS REQ5997  *
      * 2000-05-17 PQR  SHIPPING CUTOFF MOVED REQ8045                  *
      * 2001-05-27 JLM  USD EXCEPTIONS TO OPS QUEUE INC7120            *
      * 2003-07-28 CRP  AUDIT FINDING - BUYBACK CONTROL TOTALS REQ9737 *
      * 2004-08-06 TWH  MARGIN TABLE EXPANDED CHG1517                  *
      * 2004-12-04 MKL  PERFORMANCE - SHIPPING READ AHEAD REQ3328      *
      * 2005-09-24 DPS  USD EXCEPTIONS TO OPS QUEUE PRB3263            *
      * 2005-10-16 CRP  MARGIN FEE SCHEDULE UPDATE REQ1773             *
      *================================================================*
       01  FNSTAT-PARMS.
           05  FNSTAT-BUYBACK-AMT          PIC S9(13)V99 COMP-3.
           05  FNSTAT-USD-YTD              PIC S9(13)V99 COMP-3.
           05  FNSTAT-MARGIN-AMT           PIC S9(13)V99 COMP-3.
           05  FNSTAT-SHIPPING-YTD         PIC S9(13)V99 COMP-3.
           05  FNSTAT-USD-AMT              PIC S9(13)V99 COMP-3.
           05  FNSTAT-FOREIGN-AMT          PIC S9(13)V99 COMP-3.
           05  FNSTAT-EXPIRY-DATE          PIC 9(08).
           05  FNSTAT-OPEN-DATE            PIC 9(08).
           05  FNSTAT-MATURITY-DATE        PIC 9(08).
           05  FNSTAT-LAST-ACT-DATE        PIC 9(08).
           05  FNSTAT-SOURCE-CD            PIC X(02).
           05  FNSTAT-ORDER-TYPE-CD        PIC X(03).
               88  FNSTAT-ORDER-TYPE-CD-DTU        VALUE 'DTU'.
               88  FNSTAT-ORDER-TYPE-CD-KZV        VALUE 'KZV'.
               88  FNSTAT-ORDER-TYPE-CD-PHV        VALUE 'PHV'.
               88  FNSTAT-ORDER-TYPE-CD-PZZ        VALUE 'PZZ'.
               88  FNSTAT-ORDER-TYPE-CD-WMC        VALUE 'WMC'.
               88  FNSTAT-ORDER-TYPE-CD-XWF        VALUE 'XWF'.
           05  FNSTAT-TYPE-CD              PIC X(03).
               88  FNSTAT-TYPE-CD-CFP              VALUE 'CFP'.
               88  FNSTAT-TYPE-CD-FFU              VALUE 'FFU'.
               88  FNSTAT-TYPE-CD-JNR              VALUE 'JNR'.
               88  FNSTAT-TYPE-CD-UGS              VALUE 'UGS'.
               88  FNSTAT-TYPE-CD-XUM              VALUE 'XUM'.
           05  FNSTAT-STATUS-CD            PIC X(03).
               88  FNSTAT-STATUS-CD-CXH            VALUE 'CXH'.
               88  FNSTAT-STATUS-CD-ESS            VALUE 'ESS'.
               88  FNSTAT-STATUS-CD-XDE            VALUE 'XDE'.
               88  FNSTAT-STATUS-CD-XFD            VALUE 'XFD'.
           05  FNSTAT-REVIEW-FLAG          PIC X(01).
               88  FNSTAT-REVIEW-FLAG-YES          VALUE 'Y'.
           05  FNSTAT-OVERRIDE-FLAG        PIC X(01).
               88  FNSTAT-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  FNSTAT-PREFERRED-FLAG       PIC X(01).
               88  FNSTAT-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  FNSTAT-PAYMENT-CNT          PIC 9(07).
           05  FNSTAT-NOTICE-CNT           PIC 9(07).
           05  FNSTAT-RATE                 PIC 9(03)V9(06).
           05  FNSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  FNSTAT-ADDR                 PIC X(30).
           05  FNSTAT-CONTACT              PIC X(20).
           05  FNSTAT-RETURN-CODE          PIC 9(02).
