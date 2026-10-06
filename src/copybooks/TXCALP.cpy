      *================================================================*
      * COPYBOOK : TXCALP                                              *
      * PURPOSE  : LINKAGE FOR TELEX WIRE MESSAGING RULE MODULE TXCAL  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-05-19 BHS  ORIGINAL                                       *
      * 1994-09-20 DPS  PERFORMANCE - QUEUED READ AHEAD PRB1930        *
      * 1998-01-01 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-16 GAF  CORRESP CALCULATION CORRECTED CHG8320          *
      * 2000-11-22 CRP  QUEUED LIMIT REVISED CHG7603                   *
      * 2001-11-02 BHS  COVER LIMIT REVISED REQ8044                    *
      * 2002-04-28 LMB  QUEUED TABLE EXPANDED CHG9433                  *
      * 2004-10-14 PQR  SETTLE LIMIT REVISED INC3874                   *
      * 2005-10-18 GAF  NEW WIRE REPORT COLUMNS INC3095                *
      * 2007-04-21 EWS  CHARGE EXCEPTIONS TO OPS QUEUE INC5529         *
      * 2008-03-18 JLM  CHARGE NOTICE TEXT PER LEGAL CHG4054           *
      * 2010-07-08 DPS  COVER FEE SCHEDULE UPDATE INC5872              *
      *================================================================*
       01  TXCAL-PARMS.
           05  TXCAL-CHARGE-AMT            PIC S9(13)V99 COMP-3.
           05  TXCAL-SETTLE-AMT            PIC S9(13)V99 COMP-3.
           05  TXCAL-QUEUED-YTD            PIC S9(13)V99 COMP-3.
           05  TXCAL-CHARGE-YTD            PIC S9(13)V99 COMP-3.
           05  TXCAL-COVER-YTD             PIC S9(13)V99 COMP-3.
           05  TXCAL-QUEUED-AMT            PIC S9(13)V99 COMP-3.
           05  TXCAL-LAST-ACT-DATE         PIC 9(08).
           05  TXCAL-NEXT-DUE-DATE         PIC 9(08).
           05  TXCAL-EXPIRY-DATE           PIC 9(08).
           05  TXCAL-NEXT-REVIEW-DATE      PIC 9(08).
           05  TXCAL-REASON-CD             PIC X(03).
           05  TXCAL-BRANCH-CD             PIC X(02).
               88  TXCAL-BRANCH-CD-CC              VALUE 'CC'.
               88  TXCAL-BRANCH-CD-KR              VALUE 'KR'.
               88  TXCAL-BRANCH-CD-LD              VALUE 'LD'.
               88  TXCAL-BRANCH-CD-LS              VALUE 'LS'.
               88  TXCAL-BRANCH-CD-XL              VALUE 'XL'.
           05  TXCAL-TYPE-CD               PIC X(01).
               88  TXCAL-TYPE-CD-K                 VALUE 'K'.
               88  TXCAL-TYPE-CD-L                 VALUE 'L'.
               88  TXCAL-TYPE-CD-M                 VALUE 'M'.
               88  TXCAL-TYPE-CD-P                 VALUE 'P'.
               88  TXCAL-TYPE-CD-W                 VALUE 'W'.
               88  TXCAL-TYPE-CD-Z                 VALUE 'Z'.
           05  TXCAL-MSG-TYPE-CD           PIC X(02).
               88  TXCAL-MSG-TYPE-CD-LG            VALUE 'LG'.
               88  TXCAL-MSG-TYPE-CD-LX            VALUE 'LX'.
               88  TXCAL-MSG-TYPE-CD-ND            VALUE 'ND'.
               88  TXCAL-MSG-TYPE-CD-PT            VALUE 'PT'.
           05  TXCAL-PREFERRED-FLAG        PIC X(01).
               88  TXCAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  TXCAL-REVIEW-FLAG           PIC X(01).
               88  TXCAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TXCAL-SUSPEND-FLAG          PIC X(01).
               88  TXCAL-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TXCAL-PAYMENT-CNT           PIC 9(07).
           05  TXCAL-EXCEPT-CNT            PIC 9(07).
           05  TXCAL-FEE-PCT               PIC 9(03)V9(06).
           05  TXCAL-RATE                  PIC 9(03)V9(06).
           05  TXCAL-DESC                  PIC X(30).
           05  TXCAL-NAME                  PIC X(30).
           05  TXCAL-RETURN-CODE           PIC 9(02).
