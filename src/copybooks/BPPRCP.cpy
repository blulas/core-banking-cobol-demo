      *================================================================*
      * COPYBOOK : BPPRCP                                              *
      * PURPOSE  : LINKAGE FOR BILL PAYMENT SERVICE RULE MODULE BPPRC  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-04-16 JLM  ORIGINAL                                       *
      * 1994-10-26 CRP  PAYMENT EXCEPTIONS TO OPS QUEUE PRB8390        *
      * 1999-03-19 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-09 SKN  REFUND CUTOFF MOVED INC9092                    *
      * 2005-08-04 DPS  PAYMENT TABLE EXPANDED REQ8423                 *
      * 2006-02-23 GAF  RETURNED LIMIT REVISED CHG5127                 *
      * 2008-04-21 GAF  PAYMENT CUTOFF MOVED REQ2084                   *
      *================================================================*
       01  BPPRC-PARMS.
           05  BPPRC-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  BPPRC-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  BPPRC-PAYMENT-YTD           PIC S9(13)V99 COMP-3.
           05  BPPRC-REFUND-YTD            PIC S9(13)V99 COMP-3.
           05  BPPRC-RETURNED-YTD          PIC S9(13)V99 COMP-3.
           05  BPPRC-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  BPPRC-EXPIRY-DATE           PIC 9(08).
           05  BPPRC-LAST-ACT-DATE         PIC 9(08).
           05  BPPRC-EFFECTIVE-DATE        PIC 9(08).
           05  BPPRC-NEXT-DUE-DATE         PIC 9(08).
           05  BPPRC-SOURCE-CD             PIC X(02).
           05  BPPRC-REMIT-METHOD-CD       PIC X(03).
           05  BPPRC-REASON-CD             PIC X(02).
           05  BPPRC-STATUS-CD             PIC X(02).
               88  BPPRC-STATUS-CD-JX              VALUE 'JX'.
               88  BPPRC-STATUS-CD-LR              VALUE 'LR'.
               88  BPPRC-STATUS-CD-NZ              VALUE 'NZ'.
               88  BPPRC-STATUS-CD-RV              VALUE 'RV'.
               88  BPPRC-STATUS-CD-TM              VALUE 'TM'.
               88  BPPRC-STATUS-CD-TZ              VALUE 'TZ'.
           05  BPPRC-SUSPEND-FLAG          PIC X(01).
               88  BPPRC-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  BPPRC-OVERRIDE-FLAG         PIC X(01).
               88  BPPRC-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  BPPRC-HOLD-FLAG             PIC X(01).
               88  BPPRC-HOLD-FLAG-YES             VALUE 'Y'.
           05  BPPRC-EXCEPT-CNT            PIC 9(07).
           05  BPPRC-CYCLE-CNT             PIC 9(07).
           05  BPPRC-MARGIN                PIC 9(03)V9(06).
           05  BPPRC-PENALTY-PCT           PIC 9(03)V9(06).
           05  BPPRC-ADDR                  PIC X(30).
           05  BPPRC-NAME                  PIC X(30).
           05  BPPRC-RETURN-CODE           PIC 9(02).
