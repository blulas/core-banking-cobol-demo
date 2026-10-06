      *================================================================*
      * COPYBOOK : ATNMST                                              *
      * PURPOSE  : ATM NETWORK SETTLEMENT MASTER RECORD (ATNMST KSDS)  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-18 EWS  ORIGINAL                                       *
      * 1997-07-20 CRP  ADJUST NOTICE TEXT PER LEGAL CHG7287           *
      * 1998-08-01 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-23 DPS  DISPENSED TABLE EXPANDED INC4344               *
      * 2001-01-18 LMB  INTERCHG CUTOFF MOVED PRB5785                  *
      * 2006-10-09 MKL  SETTLE NOTICE TEXT PER LEGAL CHG8288           *
      * 2006-11-28 TWH  INTERCHG STATUS CODES ADDED INC2233            *
      * 2009-11-24 MKL  SURCHG CALCULATION CORRECTED PRB8306           *
      * 2010-05-01 PQR  SURCHG TABLE EXPANDED INC5909                  *
      * 2013-08-05 DPS  ADJUST FEE SCHEDULE UPDATE REQ9667             *
      * 2013-06-24 EWS  SURCHG RERUN PROTECTION CHG0305                *
      *================================================================*
       01  ATN-MASTER-REC.
           05  ATNM-KEY                    PIC X(12).
           05  ATNM-CUST-ID                PIC X(10).
           05  ATNM-NAME                   PIC X(30).
           05  ATNM-DESC                   PIC X(30).
           05  ATNM-REF-NO                 PIC X(16).
           05  ATNM-ADDR                   PIC X(30).
           05  ATNM-CONTACT                PIC X(20).
           05  ATN-MASTER-AMOUNTS.
               10  ATNM-DISPENSED-AMT      PIC S9(13)V99 COMP-3.
               10  ATNM-DISPENSED-YTD      PIC S9(13)V99 COMP-3.
               10  ATNM-INTERCHG-AMT       PIC S9(13)V99 COMP-3.
               10  ATNM-INTERCHG-YTD       PIC S9(13)V99 COMP-3.
               10  ATNM-SURCHG-AMT         PIC S9(13)V99 COMP-3.
               10  ATNM-SURCHG-YTD         PIC S9(13)V99 COMP-3.
               10  ATNM-SETTLE-AMT         PIC S9(13)V99 COMP-3.
               10  ATNM-SETTLE-YTD         PIC S9(13)V99 COMP-3.
               10  ATNM-ADJUST-AMT         PIC S9(13)V99 COMP-3.
               10  ATNM-ADJUST-YTD         PIC S9(13)V99 COMP-3.
               10  ATNM-CASSETTE-AMT       PIC S9(13)V99 COMP-3.
               10  ATNM-CASSETTE-YTD       PIC S9(13)V99 COMP-3.
           05  ATN-MASTER-DATES.
               10  ATNM-NEXT-DUE-DATE      PIC 9(08).
               10  ATNM-OPEN-DATE          PIC 9(08).
               10  ATNM-NOTICE-DATE        PIC 9(08).
               10  ATNM-LAST-PAY-DATE      PIC 9(08).
               10  ATNM-NEXT-REVIEW-DATE   PIC 9(08).
               10  ATNM-CLOSE-DATE         PIC 9(08).
               10  ATNM-EFFECTIVE-DATE     PIC 9(08).
               10  ATNM-MATURITY-DATE      PIC 9(08).
               10  ATNM-EXPIRY-DATE        PIC 9(08).
           05  ATN-MASTER-CODES.
               10  ATNM-NETWORK-CD         PIC X(01).
               10  ATNM-TERMINAL-CD        PIC X(02).
                   88  ATNM-TERMINAL-CD-BJ         VALUE 'BJ'.
                   88  ATNM-TERMINAL-CD-JJ         VALUE 'JJ'.
                   88  ATNM-TERMINAL-CD-JZ         VALUE 'JZ'.
                   88  ATNM-TERMINAL-CD-KK         VALUE 'KK'.
               10  ATNM-REASON-CD          PIC X(02).
                   88  ATNM-REASON-CD-AK           VALUE 'AK'.
                   88  ATNM-REASON-CD-HX           VALUE 'HX'.
                   88  ATNM-REASON-CD-PR           VALUE 'PR'.
                   88  ATNM-REASON-CD-XD           VALUE 'XD'.
                   88  ATNM-REASON-CD-ZJ           VALUE 'ZJ'.
               10  ATNM-BRANCH-CD          PIC X(02).
                   88  ATNM-BRANCH-CD-DU           VALUE 'DU'.
                   88  ATNM-BRANCH-CD-HX           VALUE 'HX'.
                   88  ATNM-BRANCH-CD-YC           VALUE 'YC'.
                   88  ATNM-BRANCH-CD-ZB           VALUE 'ZB'.
               10  ATNM-REGION-CD          PIC X(01).
                   88  ATNM-REGION-CD-F            VALUE 'F'.
                   88  ATNM-REGION-CD-G            VALUE 'G'.
                   88  ATNM-REGION-CD-H            VALUE 'H'.
                   88  ATNM-REGION-CD-P            VALUE 'P'.
                   88  ATNM-REGION-CD-V            VALUE 'V'.
                   88  ATNM-REGION-CD-X            VALUE 'X'.
               10  ATNM-STATUS-CD          PIC X(02).
                   88  ATNM-STATUS-CD-DF           VALUE 'DF'.
                   88  ATNM-STATUS-CD-DP           VALUE 'DP'.
                   88  ATNM-STATUS-CD-FJ           VALUE 'FJ'.
                   88  ATNM-STATUS-CD-NA           VALUE 'NA'.
               10  ATNM-SUSPEND-FLAG       PIC X(01).
                   88  ATNM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  ATNM-STOP-FLAG          PIC X(01).
                   88  ATNM-STOP-FLAG-YES          VALUE 'Y'.
               10  ATNM-EXEMPT-FLAG        PIC X(01).
                   88  ATNM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  ATNM-REVIEW-FLAG        PIC X(01).
                   88  ATNM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  ATNM-NOTICE-SENT-FLAG   PIC X(01).
                   88  ATNM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  ATNM-HOLD-FLAG          PIC X(01).
                   88  ATNM-HOLD-FLAG-YES          VALUE 'Y'.
           05  ATN-MASTER-COUNTERS.
               10  ATNM-RETRY-CNT          PIC 9(07).
               10  ATNM-NOTICE-CNT         PIC 9(07).
               10  ATNM-ITEM-CNT           PIC 9(07).
               10  ATNM-PAYMENT-CNT        PIC 9(07).
               10  ATNM-DISCOUNT           PIC 9(03)V9(06).
               10  ATNM-RATE               PIC 9(03)V9(06).
               10  ATNM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
