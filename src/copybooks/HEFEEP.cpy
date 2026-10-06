      *================================================================*
      * COPYBOOK : HEFEEP                                              *
      * PURPOSE  : LINKAGE FOR HOME EQUITY LINES RULE MODULE HEFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-27 RJT  ORIGINAL                                       *
      * 1998-02-15 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-07 DPS  INTEREST TABLE EXPANDED INC6303                *
      * 2003-05-28 RJT  BALANCE RERUN PROTECTION REQ4002               *
      * 2003-03-05 GAF  AUDIT FINDING - LIMIT CONTROL TOTALS INC1208   *
      * 2005-04-05 SKN  LIMIT LIMIT REVISED PRB7830                    *
      * 2007-05-27 SKN  APPRAISE CUTOFF MOVED REQ5339                  *
      * 2007-05-02 LMB  DRAW EXCEPTIONS TO OPS QUEUE PRB7797           *
      * 2007-02-28 TWH  INTEREST RERUN PROTECTION REQ4246              *
      *================================================================*
       01  HEFEE-PARMS.
           05  HEFEE-INTEREST-AMT          PIC S9(13)V99 COMP-3.
           05  HEFEE-ANNUAL-FEE-YTD        PIC S9(13)V99 COMP-3.
           05  HEFEE-DRAW-AMT              PIC S9(13)V99 COMP-3.
           05  HEFEE-BALANCE-YTD           PIC S9(13)V99 COMP-3.
           05  HEFEE-ANNUAL-FEE-AMT        PIC S9(13)V99 COMP-3.
           05  HEFEE-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  HEFEE-OPEN-DATE             PIC 9(08).
           05  HEFEE-NEXT-DUE-DATE         PIC 9(08).
           05  HEFEE-LAST-PAY-DATE         PIC 9(08).
           05  HEFEE-NEXT-REVIEW-DATE      PIC 9(08).
           05  HEFEE-REASON-CD             PIC X(03).
               88  HEFEE-REASON-CD-KST             VALUE 'KST'.
               88  HEFEE-REASON-CD-NWS             VALUE 'NWS'.
               88  HEFEE-REASON-CD-ZJM             VALUE 'ZJM'.
           05  HEFEE-DRAW-PERIOD-CD        PIC X(03).
               88  HEFEE-DRAW-PERIOD-CD-CCV        VALUE 'CCV'.
               88  HEFEE-DRAW-PERIOD-CD-JSU        VALUE 'JSU'.
               88  HEFEE-DRAW-PERIOD-CD-KXA        VALUE 'KXA'.
               88  HEFEE-DRAW-PERIOD-CD-VMR        VALUE 'VMR'.
               88  HEFEE-DRAW-PERIOD-CD-WLJ        VALUE 'WLJ'.
               88  HEFEE-DRAW-PERIOD-CD-XRA        VALUE 'XRA'.
           05  HEFEE-TYPE-CD               PIC X(02).
               88  HEFEE-TYPE-CD-CD                VALUE 'CD'.
               88  HEFEE-TYPE-CD-JW                VALUE 'JW'.
               88  HEFEE-TYPE-CD-KV                VALUE 'KV'.
               88  HEFEE-TYPE-CD-WS                VALUE 'WS'.
               88  HEFEE-TYPE-CD-XL                VALUE 'XL'.
               88  HEFEE-TYPE-CD-ZV                VALUE 'ZV'.
           05  HEFEE-STATUS-CD             PIC X(02).
               88  HEFEE-STATUS-CD-GR              VALUE 'GR'.
               88  HEFEE-STATUS-CD-JC              VALUE 'JC'.
               88  HEFEE-STATUS-CD-KH              VALUE 'KH'.
               88  HEFEE-STATUS-CD-KN              VALUE 'KN'.
               88  HEFEE-STATUS-CD-PC              VALUE 'PC'.
               88  HEFEE-STATUS-CD-UZ              VALUE 'UZ'.
           05  HEFEE-HOLD-FLAG             PIC X(01).
               88  HEFEE-HOLD-FLAG-YES             VALUE 'Y'.
           05  HEFEE-PREFERRED-FLAG        PIC X(01).
               88  HEFEE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  HEFEE-NOTICE-SENT-FLAG      PIC X(01).
               88  HEFEE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  HEFEE-NOTICE-CNT            PIC 9(07).
           05  HEFEE-RETRY-CNT             PIC 9(07).
           05  HEFEE-PENALTY-PCT           PIC 9(03)V9(06).
           05  HEFEE-DISCOUNT              PIC 9(03)V9(06).
           05  HEFEE-CONTACT               PIC X(20).
           05  HEFEE-NAME                  PIC X(30).
           05  HEFEE-RETURN-CODE           PIC 9(02).
