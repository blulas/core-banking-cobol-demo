      *================================================================*
      * COPYBOOK : TLXMST                                              *
      * PURPOSE  : TELEX WIRE MESSAGING MASTER RECORD (TLXMST KSDS)    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-09-21 PQR  ORIGINAL                                       *
      * 1998-12-21 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-27 BHS  CHARGE LIMIT REVISED PRB9812                   *
      * 2000-06-05 DPS  CORRESP NOTICE TEXT PER LEGAL INC9488          *
      * 2000-12-18 TWH  SETTLE EXCEPTIONS TO OPS QUEUE REQ5230         *
      * 2000-07-09 MKL  PERFORMANCE - QUEUED READ AHEAD INC6685        *
      * 2004-10-15 DPS  QUEUED LIMIT REVISED REQ3432                   *
      * 2004-01-02 BHS  CORRESP ROUNDING - HALF UP CHG9510             *
      * 2006-09-05 DPS  PERFORMANCE - CORRESP READ AHEAD CHG1764       *
      * 2006-05-28 MKL  QUEUED LIMIT REVISED CHG2661                   *
      * 2008-10-16 DPS  CORRESP CALCULATION CORRECTED CHG8770          *
      *================================================================*
       01  TLX-MASTER-REC.
           05  TLXM-KEY                    PIC X(12).
           05  TLXM-CUST-ID                PIC X(10).
           05  TLXM-NAME                   PIC X(30).
           05  TLXM-DESC                   PIC X(30).
           05  TLXM-REF-NO                 PIC X(16).
           05  TLXM-ADDR                   PIC X(30).
           05  TLXM-CONTACT                PIC X(20).
           05  TLX-MASTER-AMOUNTS.
               10  TLXM-WIRE-AMT           PIC S9(13)V99 COMP-3.
               10  TLXM-WIRE-YTD           PIC S9(13)V99 COMP-3.
               10  TLXM-CHARGE-AMT         PIC S9(13)V99 COMP-3.
               10  TLXM-CHARGE-YTD         PIC S9(13)V99 COMP-3.
               10  TLXM-COVER-AMT          PIC S9(13)V99 COMP-3.
               10  TLXM-COVER-YTD          PIC S9(13)V99 COMP-3.
               10  TLXM-CORRESP-AMT        PIC S9(13)V99 COMP-3.
               10  TLXM-CORRESP-YTD        PIC S9(13)V99 COMP-3.
               10  TLXM-SETTLE-AMT         PIC S9(13)V99 COMP-3.
               10  TLXM-SETTLE-YTD         PIC S9(13)V99 COMP-3.
               10  TLXM-QUEUED-AMT         PIC S9(13)V99 COMP-3.
               10  TLXM-QUEUED-YTD         PIC S9(13)V99 COMP-3.
           05  TLX-MASTER-DATES.
               10  TLXM-LAST-PAY-DATE      PIC 9(08).
               10  TLXM-NEXT-DUE-DATE      PIC 9(08).
               10  TLXM-NEXT-REVIEW-DATE   PIC 9(08).
               10  TLXM-NOTICE-DATE        PIC 9(08).
               10  TLXM-LAST-ACT-DATE      PIC 9(08).
               10  TLXM-EXPIRY-DATE        PIC 9(08).
               10  TLXM-OPEN-DATE          PIC 9(08).
               10  TLXM-EFFECTIVE-DATE     PIC 9(08).
               10  TLXM-CLOSE-DATE         PIC 9(08).
           05  TLX-MASTER-CODES.
               10  TLXM-MSG-TYPE-CD        PIC X(02).
                   88  TLXM-MSG-TYPE-CD-LG         VALUE 'LG'.
                   88  TLXM-MSG-TYPE-CD-LX         VALUE 'LX'.
                   88  TLXM-MSG-TYPE-CD-ND         VALUE 'ND'.
                   88  TLXM-MSG-TYPE-CD-PT         VALUE 'PT'.
               10  TLXM-TEST-KEY-CD        PIC X(02).
               10  TLXM-REGION-CD          PIC X(01).
                   88  TLXM-REGION-CD-G            VALUE 'G'.
                   88  TLXM-REGION-CD-M            VALUE 'M'.
                   88  TLXM-REGION-CD-T            VALUE 'T'.
               10  TLXM-TYPE-CD            PIC X(01).
                   88  TLXM-TYPE-CD-K              VALUE 'K'.
                   88  TLXM-TYPE-CD-L              VALUE 'L'.
                   88  TLXM-TYPE-CD-M              VALUE 'M'.
                   88  TLXM-TYPE-CD-P              VALUE 'P'.
                   88  TLXM-TYPE-CD-W              VALUE 'W'.
                   88  TLXM-TYPE-CD-Z              VALUE 'Z'.
               10  TLXM-REASON-CD          PIC X(03).
               10  TLXM-BRANCH-CD          PIC X(02).
                   88  TLXM-BRANCH-CD-CC           VALUE 'CC'.
                   88  TLXM-BRANCH-CD-KR           VALUE 'KR'.
                   88  TLXM-BRANCH-CD-LD           VALUE 'LD'.
                   88  TLXM-BRANCH-CD-LS           VALUE 'LS'.
                   88  TLXM-BRANCH-CD-XL           VALUE 'XL'.
               10  TLXM-NOTICE-SENT-FLAG   PIC X(01).
                   88  TLXM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  TLXM-PREFERRED-FLAG     PIC X(01).
                   88  TLXM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  TLXM-OVERRIDE-FLAG      PIC X(01).
                   88  TLXM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  TLXM-SUSPEND-FLAG       PIC X(01).
                   88  TLXM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  TLXM-REVIEW-FLAG        PIC X(01).
                   88  TLXM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  TLXM-STOP-FLAG          PIC X(01).
                   88  TLXM-STOP-FLAG-YES          VALUE 'Y'.
           05  TLX-MASTER-COUNTERS.
               10  TLXM-EXCEPT-CNT         PIC 9(07).
               10  TLXM-PAYMENT-CNT        PIC 9(07).
               10  TLXM-ITEM-CNT           PIC 9(07).
               10  TLXM-NOTICE-CNT         PIC 9(07).
               10  TLXM-RATE               PIC 9(03)V9(06).
               10  TLXM-DISCOUNT           PIC 9(03)V9(06).
               10  TLXM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
