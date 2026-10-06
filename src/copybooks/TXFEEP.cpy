      *================================================================*
      * COPYBOOK : TXFEEP                                              *
      * PURPOSE  : LINKAGE FOR TELEX WIRE MESSAGING RULE MODULE TXFEE  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-02-26 MKL  ORIGINAL                                       *
      * 1998-07-20 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-21 BHS  CHARGE CUTOFF MOVED REQ5045                    *
      * 1999-08-02 JLM  CHARGE ROUNDING - HALF UP PRB9636              *
      * 2004-03-19 SKN  CORRESP ROUNDING - HALF UP CHG3500             *
      * 2007-03-09 MKL  CHARGE NOTICE TEXT PER LEGAL CHG9146           *
      *================================================================*
       01  TXFEE-PARMS.
           05  TXFEE-SETTLE-AMT            PIC S9(13)V99 COMP-3.
           05  TXFEE-SETTLE-YTD            PIC S9(13)V99 COMP-3.
           05  TXFEE-COVER-AMT             PIC S9(13)V99 COMP-3.
           05  TXFEE-QUEUED-YTD            PIC S9(13)V99 COMP-3.
           05  TXFEE-WIRE-YTD              PIC S9(13)V99 COMP-3.
           05  TXFEE-CORRESP-AMT           PIC S9(13)V99 COMP-3.
           05  TXFEE-CLOSE-DATE            PIC 9(08).
           05  TXFEE-LAST-PAY-DATE         PIC 9(08).
           05  TXFEE-LAST-ACT-DATE         PIC 9(08).
           05  TXFEE-NEXT-DUE-DATE         PIC 9(08).
           05  TXFEE-REGION-CD             PIC X(01).
               88  TXFEE-REGION-CD-G               VALUE 'G'.
               88  TXFEE-REGION-CD-M               VALUE 'M'.
               88  TXFEE-REGION-CD-T               VALUE 'T'.
           05  TXFEE-TYPE-CD               PIC X(01).
               88  TXFEE-TYPE-CD-K                 VALUE 'K'.
               88  TXFEE-TYPE-CD-L                 VALUE 'L'.
               88  TXFEE-TYPE-CD-M                 VALUE 'M'.
               88  TXFEE-TYPE-CD-P                 VALUE 'P'.
               88  TXFEE-TYPE-CD-W                 VALUE 'W'.
               88  TXFEE-TYPE-CD-Z                 VALUE 'Z'.
           05  TXFEE-BRANCH-CD             PIC X(02).
               88  TXFEE-BRANCH-CD-CC              VALUE 'CC'.
               88  TXFEE-BRANCH-CD-KR              VALUE 'KR'.
               88  TXFEE-BRANCH-CD-LD              VALUE 'LD'.
               88  TXFEE-BRANCH-CD-LS              VALUE 'LS'.
               88  TXFEE-BRANCH-CD-XL              VALUE 'XL'.
           05  TXFEE-TEST-KEY-CD           PIC X(02).
           05  TXFEE-NOTICE-SENT-FLAG      PIC X(01).
               88  TXFEE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  TXFEE-SUSPEND-FLAG          PIC X(01).
               88  TXFEE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TXFEE-PREFERRED-FLAG        PIC X(01).
               88  TXFEE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  TXFEE-EXCEPT-CNT            PIC 9(07).
           05  TXFEE-ITEM-CNT              PIC 9(07).
           05  TXFEE-FEE-PCT               PIC 9(03)V9(06).
           05  TXFEE-RATE                  PIC 9(03)V9(06).
           05  TXFEE-CUST-ID               PIC X(10).
           05  TXFEE-CONTACT               PIC X(20).
           05  TXFEE-RETURN-CODE           PIC 9(02).
