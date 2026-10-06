      *================================================================*
      * COPYBOOK : PBKOLD1                                             *
      * PURPOSE  : PASSBOOK PRINTING ARCHIVE RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-07-06 BHS  ORIGINAL                                       *
      * 1996-07-20 SKN  PRINTED CALCULATION CORRECTED REQ4901          *
      * 1998-12-20 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-26 EWS  INTEREST NOTICE TEXT PER LEGAL PRB7744         *
      * 2004-03-12 JLM  CARRYFWD STATUS CODES ADDED INC5419            *
      * 2005-07-24 TWH  CARRYFWD LIMIT REVISED REQ0800                 *
      * 2006-05-24 JLM  INTEREST RERUN PROTECTION INC2147              *
      * 2009-07-23 GAF  PERFORMANCE - PRINTED READ AHEAD PRB1767       *
      *================================================================*
       01  PBK-H1-REC.
           05  PBKH1-DESC                  PIC X(30).
           05  PBKH1-REF-NO                PIC X(16).
           05  PBKH1-CUST-ID               PIC X(10).
           05  PBKH1-CONTACT               PIC X(20).
           05  PBKH1-KEY                   PIC X(12).
           05  PBKH1-ADDR                  PIC X(30).
           05  PBK-H1-AMOUNTS.
               10  PBKH1-CARRYFWD-YTD      PIC S9(13)V99 COMP-3.
               10  PBKH1-LINE-AMT          PIC S9(13)V99 COMP-3.
               10  PBKH1-PRINTED-YTD       PIC S9(13)V99 COMP-3.
               10  PBKH1-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  PBKH1-CARRYFWD-AMT      PIC S9(13)V99 COMP-3.
               10  PBKH1-BALANCE-YTD       PIC S9(13)V99 COMP-3.
               10  PBKH1-PRINTED-AMT       PIC S9(13)V99 COMP-3.
               10  PBKH1-UNPOSTED-YTD      PIC S9(13)V99 COMP-3.
               10  PBKH1-LINE-YTD          PIC S9(13)V99 COMP-3.
               10  PBKH1-UNPOSTED-AMT      PIC S9(13)V99 COMP-3.
               10  PBKH1-INTEREST-YTD      PIC S9(13)V99 COMP-3.
           05  PBK-H1-DATES.
               10  PBKH1-MATURITY-DATE     PIC 9(08).
               10  PBKH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  PBKH1-LAST-ACT-DATE     PIC 9(08).
               10  PBKH1-EFFECTIVE-DATE    PIC 9(08).
               10  PBKH1-LAST-MAINT-DATE   PIC 9(08).
               10  PBKH1-CLOSE-DATE        PIC 9(08).
               10  PBKH1-OPEN-DATE         PIC 9(08).
               10  PBKH1-EXPIRY-DATE       PIC 9(08).
               10  PBKH1-NEXT-DUE-DATE     PIC 9(08).
           05  PBK-H1-CODES.
               10  PBKH1-REGION-CD         PIC X(02).
                   88  PBKH1-REGION-CD-ER          VALUE 'ER'.
                   88  PBKH1-REGION-CD-GL          VALUE 'GL'.
                   88  PBKH1-REGION-CD-PE          VALUE 'PE'.
                   88  PBKH1-REGION-CD-RJ          VALUE 'RJ'.
                   88  PBKH1-REGION-CD-VB          VALUE 'VB'.
               10  PBKH1-EXEMPT-FLAG       PIC X(01).
                   88  PBKH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  PBKH1-BOOK-TYPE-CD      PIC X(02).
                   88  PBKH1-BOOK-TYPE-CD-CH       VALUE 'CH'.
                   88  PBKH1-BOOK-TYPE-CD-HG       VALUE 'HG'.
                   88  PBKH1-BOOK-TYPE-CD-KB       VALUE 'KB'.
                   88  PBKH1-BOOK-TYPE-CD-KW       VALUE 'KW'.
                   88  PBKH1-BOOK-TYPE-CD-PA       VALUE 'PA'.
                   88  PBKH1-BOOK-TYPE-CD-TR       VALUE 'TR'.
               10  PBKH1-SOURCE-CD         PIC X(02).
                   88  PBKH1-SOURCE-CD-JX          VALUE 'JX'.
                   88  PBKH1-SOURCE-CD-ZC          VALUE 'ZC'.
                   88  PBKH1-SOURCE-CD-ZY          VALUE 'ZY'.
               10  PBKH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  PBKH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  PBKH1-SUSPEND-FLAG      PIC X(01).
                   88  PBKH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  PBKH1-REVIEW-FLAG       PIC X(01).
                   88  PBKH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  PBKH1-HOLD-FLAG         PIC X(01).
                   88  PBKH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  PBKH1-PRINTER-CD        PIC X(02).
               10  PBKH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  PBKH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  PBKH1-CHANNEL-CD        PIC X(01).
                   88  PBKH1-CHANNEL-CD-B          VALUE 'B'.
                   88  PBKH1-CHANNEL-CD-D          VALUE 'D'.
                   88  PBKH1-CHANNEL-CD-H          VALUE 'H'.
                   88  PBKH1-CHANNEL-CD-L          VALUE 'L'.
                   88  PBKH1-CHANNEL-CD-M          VALUE 'M'.
                   88  PBKH1-CHANNEL-CD-W          VALUE 'W'.
           05  PBK-H1-COUNTERS.
               10  PBKH1-FEE-PCT           PIC 9(03)V9(06).
               10  PBKH1-RETRY-CNT         PIC 9(07).
               10  PBKH1-CYCLE-CNT         PIC 9(07).
               10  PBKH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  PBKH1-ITEM-CNT          PIC 9(07).
               10  PBKH1-PAYMENT-CNT       PIC 9(07).
           05  FILLER                      PIC X(20).
