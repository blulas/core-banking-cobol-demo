      *================================================================*
      * COPYBOOK : FNWAIVP                                             *
      * PURPOSE  : LINKAGE FOR FOREIGN BANKNOTES RULE MODULE FNWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-23 GAF  ORIGINAL                                       *
      * 1995-10-19 DPS  FOREIGN NOTICE TEXT PER LEGAL PRB0637          *
      * 1997-12-03 RJT  USD EXCEPTIONS TO OPS QUEUE INC8073            *
      * 1997-01-04 SKN  AUDIT FINDING - USD CONTROL TOTALS PRB6025     *
      * 1998-08-21 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-11-21 TWH  NEW USD REPORT COLUMNS CHG8154                 *
      * 1999-09-18 LMB  FOREIGN FEE SCHEDULE UPDATE PRB9810            *
      * 1999-11-04 LMB  USD STATUS CODES ADDED REQ0913                 *
      * 2000-08-08 MKL  AUDIT FINDING - FOREIGN CONTROL TOTALS CHG6596 *
      * 2001-05-07 EWS  BUYBACK EXCEPTIONS TO OPS QUEUE CHG1171        *
      * 2003-01-05 MKL  BUYBACK CUTOFF MOVED PRB2779                   *
      * 2003-11-13 LMB  FOREIGN EXCEPTIONS TO OPS QUEUE CHG6760        *
      * 2003-02-26 EWS  BUYBACK TABLE EXPANDED REQ4298                 *
      *================================================================*
       01  FNWAIV-PARMS.
           05  FNWAIV-FOREIGN-AMT          PIC S9(13)V99 COMP-3.
           05  FNWAIV-BUYBACK-YTD          PIC S9(13)V99 COMP-3.
           05  FNWAIV-MARGIN-YTD           PIC S9(13)V99 COMP-3.
           05  FNWAIV-USD-YTD              PIC S9(13)V99 COMP-3.
           05  FNWAIV-MARGIN-AMT           PIC S9(13)V99 COMP-3.
           05  FNWAIV-USD-AMT              PIC S9(13)V99 COMP-3.
           05  FNWAIV-NOTICE-DATE          PIC 9(08).
           05  FNWAIV-LAST-MAINT-DATE      PIC 9(08).
           05  FNWAIV-EFFECTIVE-DATE       PIC 9(08).
           05  FNWAIV-LAST-ACT-DATE        PIC 9(08).
           05  FNWAIV-CURRENCY-CD          PIC X(01).
               88  FNWAIV-CURRENCY-CD-C            VALUE 'C'.
               88  FNWAIV-CURRENCY-CD-E            VALUE 'E'.
               88  FNWAIV-CURRENCY-CD-G            VALUE 'G'.
               88  FNWAIV-CURRENCY-CD-P            VALUE 'P'.
               88  FNWAIV-CURRENCY-CD-W            VALUE 'W'.
           05  FNWAIV-SOURCE-CD            PIC X(02).
           05  FNWAIV-ORDER-TYPE-CD        PIC X(03).
               88  FNWAIV-ORDER-TYPE-CD-DTU        VALUE 'DTU'.
               88  FNWAIV-ORDER-TYPE-CD-KZV        VALUE 'KZV'.
               88  FNWAIV-ORDER-TYPE-CD-PHV        VALUE 'PHV'.
               88  FNWAIV-ORDER-TYPE-CD-PZZ        VALUE 'PZZ'.
               88  FNWAIV-ORDER-TYPE-CD-WMC        VALUE 'WMC'.
               88  FNWAIV-ORDER-TYPE-CD-XWF        VALUE 'XWF'.
           05  FNWAIV-TYPE-CD              PIC X(03).
               88  FNWAIV-TYPE-CD-CFP              VALUE 'CFP'.
               88  FNWAIV-TYPE-CD-FFU              VALUE 'FFU'.
               88  FNWAIV-TYPE-CD-JNR              VALUE 'JNR'.
               88  FNWAIV-TYPE-CD-UGS              VALUE 'UGS'.
               88  FNWAIV-TYPE-CD-XUM              VALUE 'XUM'.
           05  FNWAIV-REVIEW-FLAG          PIC X(01).
               88  FNWAIV-REVIEW-FLAG-YES          VALUE 'Y'.
           05  FNWAIV-EXEMPT-FLAG          PIC X(01).
               88  FNWAIV-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  FNWAIV-PREFERRED-FLAG       PIC X(01).
               88  FNWAIV-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  FNWAIV-EXCEPT-CNT           PIC 9(07).
           05  FNWAIV-NOTICE-CNT           PIC 9(07).
           05  FNWAIV-PENALTY-PCT          PIC 9(03)V9(06).
           05  FNWAIV-RATE                 PIC 9(03)V9(06).
           05  FNWAIV-NAME                 PIC X(30).
           05  FNWAIV-CUST-ID              PIC X(10).
           05  FNWAIV-RETURN-CODE          PIC 9(02).
