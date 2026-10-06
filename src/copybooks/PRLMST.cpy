      *================================================================*
      * COPYBOOK : PRLMST                                              *
      * PURPOSE  : PAYROLL SERVICES MASTER RECORD (PRLMST KSDS)        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-05 EWS  ORIGINAL                                       *
      * 1992-01-24 EWS  AUDIT FINDING - FED-TAX CONTROL TOTALS PRB7888 *
      * 1992-02-19 CRP  GROSS RERUN PROTECTION REQ0438                 *
      * 1993-12-15 RJT  DEDUCT STATUS CODES ADDED REQ2624              *
      * 1995-02-25 LMB  AUDIT FINDING - FICA CONTROL TOTALS REQ2562    *
      * 1996-06-11 JLM  FICA CUTOFF MOVED PRB3474                      *
      * 1996-02-16 MKL  GROSS NOTICE TEXT PER LEGAL PRB7407            *
      * 2006-06-11 EWS  NET CALCULATION CORRECTED PRB1525              *
      * 2009-12-09 RJT  DEDUCT LIMIT REVISED INC3260                   *
      * 2009-01-04 MKL  FICA EXCEPTIONS TO OPS QUEUE REQ2284           *
      *================================================================*
       01  PRL-MASTER-REC.
           05  PRLM-KEY                    PIC X(12).
           05  PRLM-CUST-ID                PIC X(10).
           05  PRLM-NAME                   PIC X(30).
           05  PRLM-DESC                   PIC X(30).
           05  PRLM-REF-NO                 PIC X(16).
           05  PRLM-ADDR                   PIC X(30).
           05  PRLM-CONTACT                PIC X(20).
           05  PRL-MASTER-AMOUNTS.
               10  PRLM-GROSS-AMT          PIC S9(13)V99 COMP-3.
               10  PRLM-GROSS-YTD          PIC S9(13)V99 COMP-3.
               10  PRLM-NET-AMT            PIC S9(13)V99 COMP-3.
               10  PRLM-NET-YTD            PIC S9(13)V99 COMP-3.
               10  PRLM-FED-TAX-AMT        PIC S9(13)V99 COMP-3.
               10  PRLM-FED-TAX-YTD        PIC S9(13)V99 COMP-3.
               10  PRLM-STATE-TAX-AMT      PIC S9(13)V99 COMP-3.
               10  PRLM-STATE-TAX-YTD      PIC S9(13)V99 COMP-3.
               10  PRLM-FICA-AMT           PIC S9(13)V99 COMP-3.
               10  PRLM-FICA-YTD           PIC S9(13)V99 COMP-3.
               10  PRLM-DEDUCT-AMT         PIC S9(13)V99 COMP-3.
               10  PRLM-DEDUCT-YTD         PIC S9(13)V99 COMP-3.
           05  PRL-MASTER-DATES.
               10  PRLM-EXPIRY-DATE        PIC 9(08).
               10  PRLM-NEXT-DUE-DATE      PIC 9(08).
               10  PRLM-LAST-ACT-DATE      PIC 9(08).
               10  PRLM-OPEN-DATE          PIC 9(08).
               10  PRLM-CLOSE-DATE         PIC 9(08).
               10  PRLM-LAST-PAY-DATE      PIC 9(08).
               10  PRLM-NEXT-REVIEW-DATE   PIC 9(08).
               10  PRLM-NOTICE-DATE        PIC 9(08).
               10  PRLM-EFFECTIVE-DATE     PIC 9(08).
           05  PRL-MASTER-CODES.
               10  PRLM-PAY-FREQ-CD        PIC X(02).
                   88  PRLM-PAY-FREQ-CD-JT         VALUE 'JT'.
                   88  PRLM-PAY-FREQ-CD-XM         VALUE 'XM'.
                   88  PRLM-PAY-FREQ-CD-ZT         VALUE 'ZT'.
               10  PRLM-TAX-STATE-CD       PIC X(03).
                   88  PRLM-TAX-STATE-CD-CAZ       VALUE 'CAZ'.
                   88  PRLM-TAX-STATE-CD-LZU       VALUE 'LZU'.
                   88  PRLM-TAX-STATE-CD-TUF       VALUE 'TUF'.
                   88  PRLM-TAX-STATE-CD-WKE       VALUE 'WKE'.
               10  PRLM-STATUS-CD          PIC X(02).
                   88  PRLM-STATUS-CD-MV           VALUE 'MV'.
                   88  PRLM-STATUS-CD-RD           VALUE 'RD'.
                   88  PRLM-STATUS-CD-ST           VALUE 'ST'.
                   88  PRLM-STATUS-CD-TJ           VALUE 'TJ'.
                   88  PRLM-STATUS-CD-UC           VALUE 'UC'.
               10  PRLM-SOURCE-CD          PIC X(02).
                   88  PRLM-SOURCE-CD-NY           VALUE 'NY'.
                   88  PRLM-SOURCE-CD-PL           VALUE 'PL'.
                   88  PRLM-SOURCE-CD-VK           VALUE 'VK'.
                   88  PRLM-SOURCE-CD-WB           VALUE 'WB'.
                   88  PRLM-SOURCE-CD-XL           VALUE 'XL'.
               10  PRLM-REGION-CD          PIC X(02).
                   88  PRLM-REGION-CD-DP           VALUE 'DP'.
                   88  PRLM-REGION-CD-TU           VALUE 'TU'.
                   88  PRLM-REGION-CD-WD           VALUE 'WD'.
               10  PRLM-REASON-CD          PIC X(01).
               10  PRLM-PREFERRED-FLAG     PIC X(01).
                   88  PRLM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  PRLM-AUTO-RENEW-FLAG    PIC X(01).
                   88  PRLM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  PRLM-HOLD-FLAG          PIC X(01).
                   88  PRLM-HOLD-FLAG-YES          VALUE 'Y'.
               10  PRLM-STOP-FLAG          PIC X(01).
                   88  PRLM-STOP-FLAG-YES          VALUE 'Y'.
               10  PRLM-NOTICE-SENT-FLAG   PIC X(01).
                   88  PRLM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  PRLM-SUSPEND-FLAG       PIC X(01).
                   88  PRLM-SUSPEND-FLAG-YES       VALUE 'Y'.
           05  PRL-MASTER-COUNTERS.
               10  PRLM-PAYMENT-CNT        PIC 9(07).
               10  PRLM-ITEM-CNT           PIC 9(07).
               10  PRLM-NOTICE-CNT         PIC 9(07).
               10  PRLM-CYCLE-CNT          PIC 9(07).
               10  PRLM-MARGIN             PIC 9(03)V9(06).
               10  PRLM-FEE-PCT            PIC 9(03)V9(06).
               10  PRLM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
