      *================================================================*
      * COPYBOOK : CNCALP                                              *
      * PURPOSE  : LINKAGE FOR COIN AND CURRENCY RULE MODULE CNCAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-14 EWS  ORIGINAL                                       *
      * 1988-02-24 MKL  SHIPMENT FEE SCHEDULE UPDATE INC6747           *
      * 1995-05-07 CRP  CURRENCY NOTICE TEXT PER LEGAL PRB9708         *
      * 1998-01-10 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-19 SKN  FEE LIMIT REVISED PRB3508                      *
      * 2002-07-23 CRP  COIN CALCULATION CORRECTED INC0150             *
      * 2002-07-21 EWS  CURRENCY RERUN PROTECTION PRB6335              *
      * 2003-05-09 EWS  CURRENCY CUTOFF MOVED INC9024                  *
      * 2004-09-11 PQR  VARIANCE TABLE EXPANDED REQ3417                *
      * 2004-03-19 LMB  STRAP TABLE EXPANDED CHG5604                   *
      *================================================================*
       01  CNCAL-PARMS.
           05  CNCAL-STRAP-YTD             PIC S9(13)V99 COMP-3.
           05  CNCAL-VARIANCE-AMT          PIC S9(13)V99 COMP-3.
           05  CNCAL-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  CNCAL-SHIPMENT-AMT          PIC S9(13)V99 COMP-3.
           05  CNCAL-SHIPMENT-YTD          PIC S9(13)V99 COMP-3.
           05  CNCAL-COIN-AMT              PIC S9(13)V99 COMP-3.
           05  CNCAL-EXPIRY-DATE           PIC 9(08).
           05  CNCAL-MATURITY-DATE         PIC 9(08).
           05  CNCAL-OPEN-DATE             PIC 9(08).
           05  CNCAL-NEXT-REVIEW-DATE      PIC 9(08).
           05  CNCAL-STATUS-CD             PIC X(03).
           05  CNCAL-CARRIER-CD            PIC X(01).
               88  CNCAL-CARRIER-CD-J              VALUE 'J'.
               88  CNCAL-CARRIER-CD-L              VALUE 'L'.
               88  CNCAL-CARRIER-CD-M              VALUE 'M'.
               88  CNCAL-CARRIER-CD-X              VALUE 'X'.
           05  CNCAL-SOURCE-CD             PIC X(03).
           05  CNCAL-CHANNEL-CD            PIC X(02).
               88  CNCAL-CHANNEL-CD-AW             VALUE 'AW'.
               88  CNCAL-CHANNEL-CD-DW             VALUE 'DW'.
               88  CNCAL-CHANNEL-CD-GN             VALUE 'GN'.
               88  CNCAL-CHANNEL-CD-RS             VALUE 'RS'.
               88  CNCAL-CHANNEL-CD-UZ             VALUE 'UZ'.
               88  CNCAL-CHANNEL-CD-YR             VALUE 'YR'.
           05  CNCAL-OVERRIDE-FLAG         PIC X(01).
               88  CNCAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  CNCAL-EXEMPT-FLAG           PIC X(01).
               88  CNCAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  CNCAL-NOTICE-SENT-FLAG      PIC X(01).
               88  CNCAL-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  CNCAL-NOTICE-CNT            PIC 9(07).
           05  CNCAL-RETRY-CNT             PIC 9(07).
           05  CNCAL-FEE-PCT               PIC 9(03)V9(06).
           05  CNCAL-RATE                  PIC 9(03)V9(06).
           05  CNCAL-NAME                  PIC X(30).
           05  CNCAL-DESC                  PIC X(30).
           05  CNCAL-RETURN-CODE           PIC 9(02).
