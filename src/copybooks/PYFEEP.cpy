      *================================================================*
      * COPYBOOK : PYFEEP                                              *
      * PURPOSE  : LINKAGE FOR PAYROLL SERVICES RULE MODULE PYFEE      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-23 DPS  ORIGINAL                                       *
      * 1993-09-07 DPS  AUDIT FINDING - FED-TAX CONTROL TOTALS CHG7232 *
      * 1998-04-11 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-12 LMB  AUDIT FINDING - NET CONTROL TOTALS PRB3846     *
      * 2004-04-07 EWS  FICA LIMIT REVISED PRB5444                     *
      * 2006-07-04 JLM  PERFORMANCE - FED-TAX READ AHEAD CHG1951       *
      * 2007-02-02 PQR  PERFORMANCE - FICA READ AHEAD INC9181          *
      * 2007-11-28 LMB  NET CUTOFF MOVED REQ4547                       *
      * 2008-07-10 PQR  GROSS RERUN PROTECTION PRB3048                 *
      * 2008-05-26 TWH  AUDIT FINDING - STATE-TAX CONTROL TOTALS PRB946*
      * 2008-03-08 SKN  DEDUCT EXCEPTIONS TO OPS QUEUE CHG9368         *
      * 2011-04-22 BHS  PERFORMANCE - DEDUCT READ AHEAD REQ7933        *
      * 2012-04-22 CRP  GROSS LIMIT REVISED REQ5723                    *
      *================================================================*
       01  PYFEE-PARMS.
           05  PYFEE-GROSS-YTD             PIC S9(13)V99 COMP-3.
           05  PYFEE-DEDUCT-AMT            PIC S9(13)V99 COMP-3.
           05  PYFEE-STATE-TAX-AMT         PIC S9(13)V99 COMP-3.
           05  PYFEE-NET-AMT               PIC S9(13)V99 COMP-3.
           05  PYFEE-STATE-TAX-YTD         PIC S9(13)V99 COMP-3.
           05  PYFEE-DEDUCT-YTD            PIC S9(13)V99 COMP-3.
           05  PYFEE-NOTICE-DATE           PIC 9(08).
           05  PYFEE-LAST-ACT-DATE         PIC 9(08).
           05  PYFEE-NEXT-DUE-DATE         PIC 9(08).
           05  PYFEE-CLOSE-DATE            PIC 9(08).
           05  PYFEE-REGION-CD             PIC X(02).
               88  PYFEE-REGION-CD-DP              VALUE 'DP'.
               88  PYFEE-REGION-CD-TU              VALUE 'TU'.
               88  PYFEE-REGION-CD-WD              VALUE 'WD'.
           05  PYFEE-PAY-FREQ-CD           PIC X(02).
               88  PYFEE-PAY-FREQ-CD-JT            VALUE 'JT'.
               88  PYFEE-PAY-FREQ-CD-XM            VALUE 'XM'.
               88  PYFEE-PAY-FREQ-CD-ZT            VALUE 'ZT'.
           05  PYFEE-SOURCE-CD             PIC X(02).
               88  PYFEE-SOURCE-CD-NY              VALUE 'NY'.
               88  PYFEE-SOURCE-CD-PL              VALUE 'PL'.
               88  PYFEE-SOURCE-CD-VK              VALUE 'VK'.
               88  PYFEE-SOURCE-CD-WB              VALUE 'WB'.
               88  PYFEE-SOURCE-CD-XL              VALUE 'XL'.
           05  PYFEE-TAX-STATE-CD          PIC X(03).
               88  PYFEE-TAX-STATE-CD-CAZ          VALUE 'CAZ'.
               88  PYFEE-TAX-STATE-CD-LZU          VALUE 'LZU'.
               88  PYFEE-TAX-STATE-CD-TUF          VALUE 'TUF'.
               88  PYFEE-TAX-STATE-CD-WKE          VALUE 'WKE'.
           05  PYFEE-STOP-FLAG             PIC X(01).
               88  PYFEE-STOP-FLAG-YES             VALUE 'Y'.
           05  PYFEE-AUTO-RENEW-FLAG       PIC X(01).
               88  PYFEE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  PYFEE-HOLD-FLAG             PIC X(01).
               88  PYFEE-HOLD-FLAG-YES             VALUE 'Y'.
           05  PYFEE-CYCLE-CNT             PIC 9(07).
           05  PYFEE-NOTICE-CNT            PIC 9(07).
           05  PYFEE-MARGIN                PIC 9(03)V9(06).
           05  PYFEE-RATE                  PIC 9(03)V9(06).
           05  PYFEE-CONTACT               PIC X(20).
           05  PYFEE-NAME                  PIC X(30).
           05  PYFEE-RETURN-CODE           PIC 9(02).
