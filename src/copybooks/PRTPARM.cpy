      *================================================================*
      * COPYBOOK : PRTPARM                                             *
      * PURPOSE  : LINKAGE FOR PAYROUTE - PAYMENT RAIL SELECTION       *
      * INPUT    : CUSTOMER PREFERENCE F FASTEST / C CHEAPEST, AMOUNT, *
      *            BENEFICIARY ROUTING AND COUNTRY, INTERNAL ACCOUNT   *
      *            FLAG (BENE ACCOUNT FOUND ON ACCTMST), URGENCY, TIME *
      *            OF REQUEST AND SAME-DAY ACH CUTOFF                  *
      * OUTPUT   : RAIL B BOOK / A ACH / S SDACH / W WIRE / I INTL,    *
      *            CUSTOMER FEE AND ESTIMATED SETTLEMENT DAYS          *
      * RETURN   : 00 OK  12 INVALID INPUT                             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2017-03-20 AKW  ORIGINAL - MARCUS PAY ANYONE PROJECT           *
      *================================================================*
       01  PAY-ROUTE-PARMS.
           05  PRT-PREFERENCE              PIC X(01).
               88  PRT-FASTEST                       VALUE 'F'.
               88  PRT-CHEAPEST                      VALUE 'C'.
           05  PRT-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  PRT-BENE-ROUTING            PIC X(09).
           05  PRT-BENE-COUNTRY            PIC X(02).
           05  PRT-INTERNAL-FLAG           PIC X(01).
               88  PRT-BENE-IS-INTERNAL              VALUE 'Y'.
           05  PRT-URGENT-FLAG             PIC X(01).
               88  PRT-IS-URGENT                     VALUE 'Y'.
           05  PRT-REQUEST-TIME            PIC 9(04).
           05  PRT-SDACH-CUTOFF            PIC 9(04).
      *--- OUTPUT -----------------------------------------------------*
           05  PRT-RAIL                    PIC X(01).
           05  PRT-RAIL-NAME               PIC X(10).
           05  PRT-FEE-AMOUNT              PIC S9(07)V99 COMP-3.
           05  PRT-EST-SETTLE-DAYS         PIC 9(01).
      *        INTL INTR LGAM SDOK SDLT STD  CHEP
           05  PRT-REASON-CODE             PIC X(04).
           05  PRT-REASON-TEXT             PIC X(40).
           05  PRT-RETURN-CODE             PIC 9(02).
