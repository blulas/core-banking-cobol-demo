      *================================================================*
      * COPYBOOK : ESLIMP                                              *
      * PURPOSE  : LINKAGE FOR MORTGAGE ESCROW RULE MODULE ESLIM       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-04-21 MKL  ORIGINAL                                       *
      * 1995-10-14 RJT  DISBURSE LIMIT REVISED PRB9858                 *
      * 1998-01-07 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-19 JLM  SHORTAGE ROUNDING - HALF UP CHG0995            *
      * 2000-02-06 CRP  PERFORMANCE - PAYMENT READ AHEAD CHG5229       *
      * 2005-08-09 MKL  PERFORMANCE - TAX READ AHEAD REQ4074           *
      * 2009-11-13 CRP  CUSHION ROUNDING - HALF UP CHG8773             *
      * 2012-06-26 BHS  SURPLUS TABLE EXPANDED REQ4282                 *
      * 2013-08-05 TWH  AUDIT FINDING - SURPLUS CONTROL TOTALS INC4468 *
      * 2013-08-08 JLM  PAYMENT STATUS CODES ADDED CHG4206             *
      *================================================================*
       01  ESLIM-PARMS.
           05  ESLIM-TAX-YTD               PIC S9(13)V99 COMP-3.
           05  ESLIM-INSURANCE-AMT         PIC S9(13)V99 COMP-3.
           05  ESLIM-SHORTAGE-YTD          PIC S9(13)V99 COMP-3.
           05  ESLIM-SURPLUS-AMT           PIC S9(13)V99 COMP-3.
           05  ESLIM-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  ESLIM-SHORTAGE-AMT          PIC S9(13)V99 COMP-3.
           05  ESLIM-EXPIRY-DATE           PIC 9(08).
           05  ESLIM-LAST-ACT-DATE         PIC 9(08).
           05  ESLIM-LAST-PAY-DATE         PIC 9(08).
           05  ESLIM-NOTICE-DATE           PIC 9(08).
           05  ESLIM-REASON-CD             PIC X(01).
               88  ESLIM-REASON-CD-C               VALUE 'C'.
               88  ESLIM-REASON-CD-F               VALUE 'F'.
               88  ESLIM-REASON-CD-K               VALUE 'K'.
               88  ESLIM-REASON-CD-L               VALUE 'L'.
               88  ESLIM-REASON-CD-S               VALUE 'S'.
               88  ESLIM-REASON-CD-X               VALUE 'X'.
           05  ESLIM-PAYEE-TYPE-CD         PIC X(02).
               88  ESLIM-PAYEE-TYPE-CD-AM          VALUE 'AM'.
               88  ESLIM-PAYEE-TYPE-CD-UE          VALUE 'UE'.
               88  ESLIM-PAYEE-TYPE-CD-UP          VALUE 'UP'.
           05  ESLIM-TYPE-CD               PIC X(03).
           05  ESLIM-STATUS-CD             PIC X(01).
               88  ESLIM-STATUS-CD-A               VALUE 'A'.
               88  ESLIM-STATUS-CD-E               VALUE 'E'.
               88  ESLIM-STATUS-CD-R               VALUE 'R'.
               88  ESLIM-STATUS-CD-U               VALUE 'U'.
               88  ESLIM-STATUS-CD-Z               VALUE 'Z'.
           05  ESLIM-NOTICE-SENT-FLAG      PIC X(01).
               88  ESLIM-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  ESLIM-SUSPEND-FLAG          PIC X(01).
               88  ESLIM-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  ESLIM-OVERRIDE-FLAG         PIC X(01).
               88  ESLIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  ESLIM-RETRY-CNT             PIC 9(07).
           05  ESLIM-EXCEPT-CNT            PIC 9(07).
           05  ESLIM-FEE-PCT               PIC 9(03)V9(06).
           05  ESLIM-RATE                  PIC 9(03)V9(06).
           05  ESLIM-CUST-ID               PIC X(10).
           05  ESLIM-DESC                  PIC X(30).
           05  ESLIM-RETURN-CODE           PIC 9(02).
