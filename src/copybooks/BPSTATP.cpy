      *================================================================*
      * COPYBOOK : BPSTATP                                             *
      * PURPOSE  : LINKAGE FOR BILL PAYMENT SERVICE RULE MODULE BPSTAT *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-07-26 GAF  ORIGINAL                                       *
      * 1995-05-20 PQR  RETURNED FEE SCHEDULE UPDATE INC0803           *
      * 1996-06-01 CRP  REFUND CALCULATION CORRECTED PRB3287           *
      * 1997-05-14 LMB  PERFORMANCE - REFUND READ AHEAD INC9501        *
      * 1997-01-09 LMB  PAYMENT STATUS CODES ADDED INC2470             *
      * 1998-06-09 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-07-25 DPS  PAYMENT ROUNDING - HALF UP REQ2632             *
      * 2002-04-22 CRP  REFUND CUTOFF MOVED PRB1386                    *
      * 2003-06-18 CRP  REFUND CUTOFF MOVED REQ6838                    *
      * 2004-08-04 LMB  PERFORMANCE - REFUND READ AHEAD CHG0451        *
      *================================================================*
       01  BPSTAT-PARMS.
           05  BPSTAT-PAYMENT-AMT          PIC S9(13)V99 COMP-3.
           05  BPSTAT-RETURNED-AMT         PIC S9(13)V99 COMP-3.
           05  BPSTAT-REFUND-AMT           PIC S9(13)V99 COMP-3.
           05  BPSTAT-PAYMENT-YTD          PIC S9(13)V99 COMP-3.
           05  BPSTAT-RETURNED-YTD         PIC S9(13)V99 COMP-3.
           05  BPSTAT-FEE-AMT              PIC S9(13)V99 COMP-3.
           05  BPSTAT-OPEN-DATE            PIC 9(08).
           05  BPSTAT-MATURITY-DATE        PIC 9(08).
           05  BPSTAT-LAST-STMT-DATE       PIC 9(08).
           05  BPSTAT-CLOSE-DATE           PIC 9(08).
           05  BPSTAT-CYCLE-CD             PIC X(02).
           05  BPSTAT-REMIT-METHOD-CD      PIC X(03).
           05  BPSTAT-STATUS-CD            PIC X(02).
               88  BPSTAT-STATUS-CD-JX             VALUE 'JX'.
               88  BPSTAT-STATUS-CD-LR             VALUE 'LR'.
               88  BPSTAT-STATUS-CD-NZ             VALUE 'NZ'.
               88  BPSTAT-STATUS-CD-RV             VALUE 'RV'.
               88  BPSTAT-STATUS-CD-TM             VALUE 'TM'.
               88  BPSTAT-STATUS-CD-TZ             VALUE 'TZ'.
           05  BPSTAT-REASON-CD            PIC X(02).
           05  BPSTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  BPSTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  BPSTAT-EXEMPT-FLAG          PIC X(01).
               88  BPSTAT-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  BPSTAT-SUSPEND-FLAG         PIC X(01).
               88  BPSTAT-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  BPSTAT-PAYMENT-CNT          PIC 9(07).
           05  BPSTAT-EXCEPT-CNT           PIC 9(07).
           05  BPSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  BPSTAT-RATE                 PIC 9(03)V9(06).
           05  BPSTAT-REF-NO               PIC X(16).
           05  BPSTAT-CUST-ID              PIC X(10).
           05  BPSTAT-RETURN-CODE          PIC 9(02).
