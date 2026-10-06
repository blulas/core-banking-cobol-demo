      *================================================================*
      * COPYBOOK : DRSTATP                                             *
      * PURPOSE  : LINKAGE FOR DIVIDEND REINVESTMENT RULE MODULE DRSTAT*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-01-18 CRP  ORIGINAL                                       *
      * 1987-07-17 RJT  NEW REINVEST REPORT COLUMNS CHG9380            *
      * 1987-05-19 GAF  REINVEST FEE SCHEDULE UPDATE REQ7190           *
      * 1989-02-28 BHS  SHARES-AMT ROUNDING - HALF UP REQ8230          *
      * 1991-02-10 GAF  CASH-PAY CUTOFF MOVED REQ7155                  *
      * 1995-07-01 GAF  AUDIT FINDING - WITHHOLD CONTROL TOTALS REQ1410*
      * 1996-02-03 JLM  NEW REINVEST REPORT COLUMNS PRB8196            *
      * 1997-05-16 SKN  DIVIDEND LIMIT REVISED INC1325                 *
      * 1997-12-09 LMB  FEE CALCULATION CORRECTED PRB2069              *
      * 1998-05-17 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-18 TWH  AUDIT FINDING - WITHHOLD CONTROL TOTALS REQ1574*
      * 2005-01-12 MKL  FEE NOTICE TEXT PER LEGAL CHG7703              *
      * 2010-07-14 PQR  DIVIDEND EXCEPTIONS TO OPS QUEUE PRB7508       *
      *================================================================*
       01  DRSTAT-PARMS.
           05  DRSTAT-CASH-PAY-AMT         PIC S9(13)V99 COMP-3.
           05  DRSTAT-FEE-YTD              PIC S9(13)V99 COMP-3.
           05  DRSTAT-DIVIDEND-AMT         PIC S9(13)V99 COMP-3.
           05  DRSTAT-REINVEST-AMT         PIC S9(13)V99 COMP-3.
           05  DRSTAT-SHARES-AMT-AMT       PIC S9(13)V99 COMP-3.
           05  DRSTAT-WITHHOLD-YTD         PIC S9(13)V99 COMP-3.
           05  DRSTAT-LAST-PAY-DATE        PIC 9(08).
           05  DRSTAT-NEXT-REVIEW-DATE     PIC 9(08).
           05  DRSTAT-NEXT-DUE-DATE        PIC 9(08).
           05  DRSTAT-MATURITY-DATE        PIC 9(08).
           05  DRSTAT-PLAN-OPTION-CD       PIC X(01).
               88  DRSTAT-PLAN-OPTION-CD-E         VALUE 'E'.
               88  DRSTAT-PLAN-OPTION-CD-R         VALUE 'R'.
               88  DRSTAT-PLAN-OPTION-CD-T         VALUE 'T'.
               88  DRSTAT-PLAN-OPTION-CD-Z         VALUE 'Z'.
           05  DRSTAT-REASON-CD            PIC X(01).
           05  DRSTAT-CHANNEL-CD           PIC X(02).
               88  DRSTAT-CHANNEL-CD-RM            VALUE 'RM'.
               88  DRSTAT-CHANNEL-CD-VP            VALUE 'VP'.
               88  DRSTAT-CHANNEL-CD-YS            VALUE 'YS'.
           05  DRSTAT-HOLDER-TYPE-CD       PIC X(01).
               88  DRSTAT-HOLDER-TYPE-CD-C         VALUE 'C'.
               88  DRSTAT-HOLDER-TYPE-CD-H         VALUE 'H'.
               88  DRSTAT-HOLDER-TYPE-CD-K         VALUE 'K'.
               88  DRSTAT-HOLDER-TYPE-CD-L         VALUE 'L'.
               88  DRSTAT-HOLDER-TYPE-CD-M         VALUE 'M'.
               88  DRSTAT-HOLDER-TYPE-CD-X         VALUE 'X'.
           05  DRSTAT-OVERRIDE-FLAG        PIC X(01).
               88  DRSTAT-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  DRSTAT-STOP-FLAG            PIC X(01).
               88  DRSTAT-STOP-FLAG-YES            VALUE 'Y'.
           05  DRSTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  DRSTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  DRSTAT-EXCEPT-CNT           PIC 9(07).
           05  DRSTAT-NOTICE-CNT           PIC 9(07).
           05  DRSTAT-RATE                 PIC 9(03)V9(06).
           05  DRSTAT-DISCOUNT             PIC 9(03)V9(06).
           05  DRSTAT-CUST-ID              PIC X(10).
           05  DRSTAT-REF-NO               PIC X(16).
           05  DRSTAT-RETURN-CODE          PIC 9(02).
