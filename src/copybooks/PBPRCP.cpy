      *================================================================*
      * COPYBOOK : PBPRCP                                              *
      * PURPOSE  : LINKAGE FOR PASSBOOK PRINTING RULE MODULE PBPRC     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-03-22 DPS  ORIGINAL                                       *
      * 2003-01-12 LMB  LINE FEE SCHEDULE UPDATE REQ2029               *
      * 2007-05-02 JLM  PRINTED CALCULATION CORRECTED CHG3189          *
      * 2008-05-10 TWH  AUDIT FINDING - UNPOSTED CONTROL TOTALS REQ6140*
      * 2010-11-09 EWS  LINE LIMIT REVISED CHG5133                     *
      * 2012-01-09 GAF  CARRYFWD NOTICE TEXT PER LEGAL REQ9221         *
      *================================================================*
       01  PBPRC-PARMS.
           05  PBPRC-PRINTED-AMT           PIC S9(13)V99 COMP-3.
           05  PBPRC-LINE-YTD              PIC S9(13)V99 COMP-3.
           05  PBPRC-UNPOSTED-AMT          PIC S9(13)V99 COMP-3.
           05  PBPRC-CARRYFWD-YTD          PIC S9(13)V99 COMP-3.
           05  PBPRC-INTEREST-AMT          PIC S9(13)V99 COMP-3.
           05  PBPRC-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  PBPRC-MATURITY-DATE         PIC 9(08).
           05  PBPRC-EXPIRY-DATE           PIC 9(08).
           05  PBPRC-EFFECTIVE-DATE        PIC 9(08).
           05  PBPRC-NEXT-REVIEW-DATE      PIC 9(08).
           05  PBPRC-CHANNEL-CD            PIC X(01).
               88  PBPRC-CHANNEL-CD-B              VALUE 'B'.
               88  PBPRC-CHANNEL-CD-D              VALUE 'D'.
               88  PBPRC-CHANNEL-CD-H              VALUE 'H'.
               88  PBPRC-CHANNEL-CD-L              VALUE 'L'.
               88  PBPRC-CHANNEL-CD-M              VALUE 'M'.
               88  PBPRC-CHANNEL-CD-W              VALUE 'W'.
           05  PBPRC-REGION-CD             PIC X(02).
               88  PBPRC-REGION-CD-ER              VALUE 'ER'.
               88  PBPRC-REGION-CD-GL              VALUE 'GL'.
               88  PBPRC-REGION-CD-PE              VALUE 'PE'.
               88  PBPRC-REGION-CD-RJ              VALUE 'RJ'.
               88  PBPRC-REGION-CD-VB              VALUE 'VB'.
           05  PBPRC-SOURCE-CD             PIC X(02).
               88  PBPRC-SOURCE-CD-JX              VALUE 'JX'.
               88  PBPRC-SOURCE-CD-ZC              VALUE 'ZC'.
               88  PBPRC-SOURCE-CD-ZY              VALUE 'ZY'.
           05  PBPRC-PRINTER-CD            PIC X(02).
           05  PBPRC-AUTO-RENEW-FLAG       PIC X(01).
               88  PBPRC-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  PBPRC-NOTICE-SENT-FLAG      PIC X(01).
               88  PBPRC-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  PBPRC-HOLD-FLAG             PIC X(01).
               88  PBPRC-HOLD-FLAG-YES             VALUE 'Y'.
           05  PBPRC-PAYMENT-CNT           PIC 9(07).
           05  PBPRC-ITEM-CNT              PIC 9(07).
           05  PBPRC-PENALTY-PCT           PIC 9(03)V9(06).
           05  PBPRC-MARGIN                PIC 9(03)V9(06).
           05  PBPRC-CUST-ID               PIC X(10).
           05  PBPRC-ADDR                  PIC X(30).
           05  PBPRC-RETURN-CODE           PIC 9(02).
