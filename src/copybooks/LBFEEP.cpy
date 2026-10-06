      *================================================================*
      * COPYBOOK : LBFEEP                                              *
      * PURPOSE  : LINKAGE FOR LOCKBOX REMITTANCE RULE MODULE LBFEE    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-06 RJT  ORIGINAL                                       *
      * 2003-07-04 EWS  FLOAT TABLE EXPANDED PRB7253                   *
      * 2003-05-25 PQR  DEPOSIT STATUS CODES ADDED INC7543             *
      * 2005-09-04 MKL  AUDIT FINDING - DEPOSIT CONTROL TOTALS REQ7058 *
      * 2006-07-01 JLM  EXCEPT RERUN PROTECTION INC6041                *
      * 2008-04-15 TWH  DEPOSIT ROUNDING - HALF UP PRB1899             *
      * 2009-12-14 PQR  FLOAT TABLE EXPANDED INC9190                   *
      * 2009-08-23 LMB  REMIT NOTICE TEXT PER LEGAL INC2006            *
      * 2011-11-03 GAF  CASH-ON-HAND LIMIT REVISED REQ0920             *
      * 2013-05-27 GAF  NEW CHECK REPORT COLUMNS REQ6793               *
      * 2014-01-14 RJT  FLOAT NOTICE TEXT PER LEGAL REQ0528            *
      *================================================================*
       01  LBFEE-PARMS.
           05  LBFEE-EXCEPT-YTD            PIC S9(13)V99 COMP-3.
           05  LBFEE-DEPOSIT-YTD           PIC S9(13)V99 COMP-3.
           05  LBFEE-REMIT-YTD             PIC S9(13)V99 COMP-3.
           05  LBFEE-EXCEPT-AMT            PIC S9(13)V99 COMP-3.
           05  LBFEE-FLOAT-YTD             PIC S9(13)V99 COMP-3.
           05  LBFEE-FLOAT-AMT             PIC S9(13)V99 COMP-3.
           05  LBFEE-CLOSE-DATE            PIC 9(08).
           05  LBFEE-NEXT-DUE-DATE         PIC 9(08).
           05  LBFEE-LAST-PAY-DATE         PIC 9(08).
           05  LBFEE-OPEN-DATE             PIC 9(08).
           05  LBFEE-LOCKBOX-CD            PIC X(02).
               88  LBFEE-LOCKBOX-CD-DX             VALUE 'DX'.
               88  LBFEE-LOCKBOX-CD-JM             VALUE 'JM'.
               88  LBFEE-LOCKBOX-CD-WW             VALUE 'WW'.
               88  LBFEE-LOCKBOX-CD-XE             VALUE 'XE'.
           05  LBFEE-REASON-CD             PIC X(02).
               88  LBFEE-REASON-CD-GF              VALUE 'GF'.
               88  LBFEE-REASON-CD-UK              VALUE 'UK'.
               88  LBFEE-REASON-CD-VR              VALUE 'VR'.
               88  LBFEE-REASON-CD-XG              VALUE 'XG'.
           05  LBFEE-BATCH-TYPE-CD         PIC X(01).
           05  LBFEE-CHANNEL-CD            PIC X(01).
               88  LBFEE-CHANNEL-CD-B              VALUE 'B'.
               88  LBFEE-CHANNEL-CD-H              VALUE 'H'.
               88  LBFEE-CHANNEL-CD-Z              VALUE 'Z'.
           05  LBFEE-STOP-FLAG             PIC X(01).
               88  LBFEE-STOP-FLAG-YES             VALUE 'Y'.
           05  LBFEE-EXEMPT-FLAG           PIC X(01).
               88  LBFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  LBFEE-NOTICE-SENT-FLAG      PIC X(01).
               88  LBFEE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  LBFEE-PAYMENT-CNT           PIC 9(07).
           05  LBFEE-ITEM-CNT              PIC 9(07).
           05  LBFEE-DISCOUNT              PIC 9(03)V9(06).
           05  LBFEE-PENALTY-PCT           PIC 9(03)V9(06).
           05  LBFEE-NAME                  PIC X(30).
           05  LBFEE-ADDR                  PIC X(30).
           05  LBFEE-RETURN-CODE           PIC 9(02).
