      *================================================================*
      * COPYBOOK : CNDTEP                                              *
      * PURPOSE  : LINKAGE FOR COIN AND CURRENCY RULE MODULE CNDTE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-01-02 BHS  ORIGINAL                                       *
      * 1987-05-14 DPS  STRAP STATUS CODES ADDED PRB5883               *
      * 1988-06-14 JLM  PERFORMANCE - CURRENCY READ AHEAD CHG8642      *
      * 1992-09-09 GAF  AUDIT FINDING - CURRENCY CONTROL TOTALS INC9104*
      * 1994-03-05 TWH  COIN CUTOFF MOVED CHG5193                      *
      * 1996-05-14 LMB  COIN STATUS CODES ADDED INC2518                *
      * 1999-03-28 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-04 LMB  AUDIT FINDING - COIN CONTROL TOTALS REQ6472    *
      * 1999-09-22 TWH  SHIPMENT ROUNDING - HALF UP INC5653            *
      * 2002-07-23 GAF  FEE ROUNDING - HALF UP REQ7738                 *
      * 2002-07-26 DPS  FEE EXCEPTIONS TO OPS QUEUE REQ2609            *
      *================================================================*
       01  CNDTE-PARMS.
           05  CNDTE-SHIPMENT-AMT          PIC S9(13)V99 COMP-3.
           05  CNDTE-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  CNDTE-COIN-YTD              PIC S9(13)V99 COMP-3.
           05  CNDTE-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  CNDTE-COIN-AMT              PIC S9(13)V99 COMP-3.
           05  CNDTE-CURRENCY-YTD          PIC S9(13)V99 COMP-3.
           05  CNDTE-EFFECTIVE-DATE        PIC 9(08).
           05  CNDTE-LAST-ACT-DATE         PIC 9(08).
           05  CNDTE-NEXT-REVIEW-DATE      PIC 9(08).
           05  CNDTE-MATURITY-DATE         PIC 9(08).
           05  CNDTE-CHANNEL-CD            PIC X(02).
               88  CNDTE-CHANNEL-CD-AW             VALUE 'AW'.
               88  CNDTE-CHANNEL-CD-DW             VALUE 'DW'.
               88  CNDTE-CHANNEL-CD-GN             VALUE 'GN'.
               88  CNDTE-CHANNEL-CD-RS             VALUE 'RS'.
               88  CNDTE-CHANNEL-CD-UZ             VALUE 'UZ'.
               88  CNDTE-CHANNEL-CD-YR             VALUE 'YR'.
           05  CNDTE-DENOM-CD              PIC X(01).
               88  CNDTE-DENOM-CD-B                VALUE 'B'.
               88  CNDTE-DENOM-CD-D                VALUE 'D'.
               88  CNDTE-DENOM-CD-P                VALUE 'P'.
               88  CNDTE-DENOM-CD-S                VALUE 'S'.
               88  CNDTE-DENOM-CD-W                VALUE 'W'.
           05  CNDTE-STATUS-CD             PIC X(03).
           05  CNDTE-CARRIER-CD            PIC X(01).
               88  CNDTE-CARRIER-CD-J              VALUE 'J'.
               88  CNDTE-CARRIER-CD-L              VALUE 'L'.
               88  CNDTE-CARRIER-CD-M              VALUE 'M'.
               88  CNDTE-CARRIER-CD-X              VALUE 'X'.
           05  CNDTE-NOTICE-SENT-FLAG      PIC X(01).
               88  CNDTE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  CNDTE-HOLD-FLAG             PIC X(01).
               88  CNDTE-HOLD-FLAG-YES             VALUE 'Y'.
           05  CNDTE-AUTO-RENEW-FLAG       PIC X(01).
               88  CNDTE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  CNDTE-ITEM-CNT              PIC 9(07).
           05  CNDTE-NOTICE-CNT            PIC 9(07).
           05  CNDTE-PENALTY-PCT           PIC 9(03)V9(06).
           05  CNDTE-FEE-PCT               PIC 9(03)V9(06).
           05  CNDTE-CUST-ID               PIC X(10).
           05  CNDTE-DESC                  PIC X(30).
           05  CNDTE-RETURN-CODE           PIC 9(02).
