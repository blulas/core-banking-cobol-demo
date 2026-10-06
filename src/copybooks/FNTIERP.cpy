      *================================================================*
      * COPYBOOK : FNTIERP                                             *
      * PURPOSE  : LINKAGE FOR FOREIGN BANKNOTES RULE MODULE FNTIER    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-21 MKL  ORIGINAL                                       *
      * 2002-10-14 CRP  MARGIN STATUS CODES ADDED PRB4548              *
      * 2002-06-13 LMB  SHIPPING CUTOFF MOVED PRB7915                  *
      * 2003-11-09 RJT  MARGIN TABLE EXPANDED INC7088                  *
      * 2004-03-06 MKL  NEW SHIPPING REPORT COLUMNS CHG3947            *
      * 2005-08-11 JLM  USD RERUN PROTECTION INC2837                   *
      *================================================================*
       01  FNTIER-PARMS.
           05  FNTIER-FOREIGN-AMT          PIC S9(13)V99 COMP-3.
           05  FNTIER-USD-AMT              PIC S9(13)V99 COMP-3.
           05  FNTIER-SHIPPING-AMT         PIC S9(13)V99 COMP-3.
           05  FNTIER-BUYBACK-AMT          PIC S9(13)V99 COMP-3.
           05  FNTIER-BUYBACK-YTD          PIC S9(13)V99 COMP-3.
           05  FNTIER-FOREIGN-YTD          PIC S9(13)V99 COMP-3.
           05  FNTIER-CLOSE-DATE           PIC 9(08).
           05  FNTIER-LAST-MAINT-DATE      PIC 9(08).
           05  FNTIER-OPEN-DATE            PIC 9(08).
           05  FNTIER-EFFECTIVE-DATE       PIC 9(08).
           05  FNTIER-STATUS-CD            PIC X(03).
               88  FNTIER-STATUS-CD-CXH            VALUE 'CXH'.
               88  FNTIER-STATUS-CD-ESS            VALUE 'ESS'.
               88  FNTIER-STATUS-CD-XDE            VALUE 'XDE'.
               88  FNTIER-STATUS-CD-XFD            VALUE 'XFD'.
           05  FNTIER-SOURCE-CD            PIC X(02).
           05  FNTIER-TYPE-CD              PIC X(03).
               88  FNTIER-TYPE-CD-CFP              VALUE 'CFP'.
               88  FNTIER-TYPE-CD-FFU              VALUE 'FFU'.
               88  FNTIER-TYPE-CD-JNR              VALUE 'JNR'.
               88  FNTIER-TYPE-CD-UGS              VALUE 'UGS'.
               88  FNTIER-TYPE-CD-XUM              VALUE 'XUM'.
           05  FNTIER-BRANCH-CD            PIC X(03).
               88  FNTIER-BRANCH-CD-APZ            VALUE 'APZ'.
               88  FNTIER-BRANCH-CD-BBP            VALUE 'BBP'.
               88  FNTIER-BRANCH-CD-ENL            VALUE 'ENL'.
               88  FNTIER-BRANCH-CD-EZB            VALUE 'EZB'.
               88  FNTIER-BRANCH-CD-KHV            VALUE 'KHV'.
               88  FNTIER-BRANCH-CD-TDN            VALUE 'TDN'.
           05  FNTIER-AUTO-RENEW-FLAG      PIC X(01).
               88  FNTIER-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  FNTIER-NOTICE-SENT-FLAG     PIC X(01).
               88  FNTIER-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  FNTIER-PREFERRED-FLAG       PIC X(01).
               88  FNTIER-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  FNTIER-PAYMENT-CNT          PIC 9(07).
           05  FNTIER-EXCEPT-CNT           PIC 9(07).
           05  FNTIER-MARGIN               PIC 9(03)V9(06).
           05  FNTIER-RATE                 PIC 9(03)V9(06).
           05  FNTIER-DESC                 PIC X(30).
           05  FNTIER-REF-NO               PIC X(16).
           05  FNTIER-RETURN-CODE          PIC 9(02).
