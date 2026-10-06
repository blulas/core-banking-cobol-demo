      *================================================================*
      * COPYBOOK : CNPENP                                              *
      * PURPOSE  : LINKAGE FOR COIN AND CURRENCY RULE MODULE CNPEN     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-12-01 EWS  ORIGINAL                                       *
      * 1987-03-05 EWS  COIN FEE SCHEDULE UPDATE INC0316               *
      * 1991-12-02 TWH  STRAP CUTOFF MOVED CHG1976                     *
      * 1995-04-25 SKN  FEE CALCULATION CORRECTED CHG6602              *
      * 1997-03-03 TWH  VARIANCE TABLE EXPANDED INC6417                *
      * 2002-12-25 RJT  CURRENCY NOTICE TEXT PER LEGAL INC4576         *
      * 2003-12-02 CRP  STRAP STATUS CODES ADDED CHG9809               *
      * 2004-11-03 RJT  FEE EXCEPTIONS TO OPS QUEUE PRB0459            *
      *================================================================*
       01  CNPEN-PARMS.
           05  CNPEN-SHIPMENT-AMT          PIC S9(13)V99 COMP-3.
           05  CNPEN-COIN-AMT              PIC S9(13)V99 COMP-3.
           05  CNPEN-STRAP-YTD             PIC S9(13)V99 COMP-3.
           05  CNPEN-SHIPMENT-YTD          PIC S9(13)V99 COMP-3.
           05  CNPEN-CURRENCY-YTD          PIC S9(13)V99 COMP-3.
           05  CNPEN-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  CNPEN-NOTICE-DATE           PIC 9(08).
           05  CNPEN-NEXT-REVIEW-DATE      PIC 9(08).
           05  CNPEN-LAST-PAY-DATE         PIC 9(08).
           05  CNPEN-MATURITY-DATE         PIC 9(08).
           05  CNPEN-CARRIER-CD            PIC X(01).
               88  CNPEN-CARRIER-CD-J              VALUE 'J'.
               88  CNPEN-CARRIER-CD-L              VALUE 'L'.
               88  CNPEN-CARRIER-CD-M              VALUE 'M'.
               88  CNPEN-CARRIER-CD-X              VALUE 'X'.
           05  CNPEN-REASON-CD             PIC X(02).
               88  CNPEN-REASON-CD-JL              VALUE 'JL'.
               88  CNPEN-REASON-CD-LW              VALUE 'LW'.
               88  CNPEN-REASON-CD-PS              VALUE 'PS'.
           05  CNPEN-SOURCE-CD             PIC X(03).
           05  CNPEN-CHANNEL-CD            PIC X(02).
               88  CNPEN-CHANNEL-CD-AW             VALUE 'AW'.
               88  CNPEN-CHANNEL-CD-DW             VALUE 'DW'.
               88  CNPEN-CHANNEL-CD-GN             VALUE 'GN'.
               88  CNPEN-CHANNEL-CD-RS             VALUE 'RS'.
               88  CNPEN-CHANNEL-CD-UZ             VALUE 'UZ'.
               88  CNPEN-CHANNEL-CD-YR             VALUE 'YR'.
           05  CNPEN-STOP-FLAG             PIC X(01).
               88  CNPEN-STOP-FLAG-YES             VALUE 'Y'.
           05  CNPEN-HOLD-FLAG             PIC X(01).
               88  CNPEN-HOLD-FLAG-YES             VALUE 'Y'.
           05  CNPEN-AUTO-RENEW-FLAG       PIC X(01).
               88  CNPEN-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  CNPEN-RETRY-CNT             PIC 9(07).
           05  CNPEN-ITEM-CNT              PIC 9(07).
           05  CNPEN-RATE                  PIC 9(03)V9(06).
           05  CNPEN-PENALTY-PCT           PIC 9(03)V9(06).
           05  CNPEN-REF-NO                PIC X(16).
           05  CNPEN-CONTACT               PIC X(20).
           05  CNPEN-RETURN-CODE           PIC 9(02).
