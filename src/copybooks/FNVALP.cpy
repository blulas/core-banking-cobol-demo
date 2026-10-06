      *================================================================*
      * COPYBOOK : FNVALP                                              *
      * PURPOSE  : LINKAGE FOR FOREIGN BANKNOTES RULE MODULE FNVAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-14 DPS  ORIGINAL                                       *
      * 1995-11-02 CRP  BUYBACK EXCEPTIONS TO OPS QUEUE INC3555        *
      * 1996-11-20 MKL  MARGIN FEE SCHEDULE UPDATE REQ5744             *
      * 1996-03-17 EWS  BUYBACK TABLE EXPANDED INC2218                 *
      * 1999-08-22 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-05 LMB  MARGIN CALCULATION CORRECTED PRB9417           *
      * 2000-10-16 SKN  FOREIGN EXCEPTIONS TO OPS QUEUE REQ7042        *
      * 2000-09-28 CRP  BUYBACK CALCULATION CORRECTED INC2011          *
      * 2001-07-12 RJT  BUYBACK FEE SCHEDULE UPDATE REQ6825            *
      * 2002-05-09 BHS  AUDIT FINDING - FOREIGN CONTROL TOTALS INC2134 *
      * 2004-07-17 MKL  USD TABLE EXPANDED REQ7122                     *
      * 2004-12-28 RJT  SHIPPING CUTOFF MOVED INC4567                  *
      *================================================================*
       01  FNVAL-PARMS.
           05  FNVAL-FOREIGN-AMT           PIC S9(13)V99 COMP-3.
           05  FNVAL-SHIPPING-AMT          PIC S9(13)V99 COMP-3.
           05  FNVAL-FOREIGN-YTD           PIC S9(13)V99 COMP-3.
           05  FNVAL-SHIPPING-YTD          PIC S9(13)V99 COMP-3.
           05  FNVAL-MARGIN-AMT            PIC S9(13)V99 COMP-3.
           05  FNVAL-USD-AMT               PIC S9(13)V99 COMP-3.
           05  FNVAL-OPEN-DATE             PIC 9(08).
           05  FNVAL-EXPIRY-DATE           PIC 9(08).
           05  FNVAL-NOTICE-DATE           PIC 9(08).
           05  FNVAL-LAST-ACT-DATE         PIC 9(08).
           05  FNVAL-TYPE-CD               PIC X(03).
               88  FNVAL-TYPE-CD-CFP               VALUE 'CFP'.
               88  FNVAL-TYPE-CD-FFU               VALUE 'FFU'.
               88  FNVAL-TYPE-CD-JNR               VALUE 'JNR'.
               88  FNVAL-TYPE-CD-UGS               VALUE 'UGS'.
               88  FNVAL-TYPE-CD-XUM               VALUE 'XUM'.
           05  FNVAL-ORDER-TYPE-CD         PIC X(03).
               88  FNVAL-ORDER-TYPE-CD-DTU         VALUE 'DTU'.
               88  FNVAL-ORDER-TYPE-CD-KZV         VALUE 'KZV'.
               88  FNVAL-ORDER-TYPE-CD-PHV         VALUE 'PHV'.
               88  FNVAL-ORDER-TYPE-CD-PZZ         VALUE 'PZZ'.
               88  FNVAL-ORDER-TYPE-CD-WMC         VALUE 'WMC'.
               88  FNVAL-ORDER-TYPE-CD-XWF         VALUE 'XWF'.
           05  FNVAL-SOURCE-CD             PIC X(02).
           05  FNVAL-BRANCH-CD             PIC X(03).
               88  FNVAL-BRANCH-CD-APZ             VALUE 'APZ'.
               88  FNVAL-BRANCH-CD-BBP             VALUE 'BBP'.
               88  FNVAL-BRANCH-CD-ENL             VALUE 'ENL'.
               88  FNVAL-BRANCH-CD-EZB             VALUE 'EZB'.
               88  FNVAL-BRANCH-CD-KHV             VALUE 'KHV'.
               88  FNVAL-BRANCH-CD-TDN             VALUE 'TDN'.
           05  FNVAL-EXEMPT-FLAG           PIC X(01).
               88  FNVAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  FNVAL-NOTICE-SENT-FLAG      PIC X(01).
               88  FNVAL-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  FNVAL-REVIEW-FLAG           PIC X(01).
               88  FNVAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  FNVAL-EXCEPT-CNT            PIC 9(07).
           05  FNVAL-PAYMENT-CNT           PIC 9(07).
           05  FNVAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  FNVAL-RATE                  PIC 9(03)V9(06).
           05  FNVAL-CONTACT               PIC X(20).
           05  FNVAL-REF-NO                PIC X(16).
           05  FNVAL-RETURN-CODE           PIC 9(02).
