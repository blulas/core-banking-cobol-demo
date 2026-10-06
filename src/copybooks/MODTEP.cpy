      *================================================================*
      * COPYBOOK : MODTEP                                              *
      * PURPOSE  : LINKAGE FOR MONEY ORDERS RULE MODULE MODTE          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-13 RJT  ORIGINAL                                       *
      * 1991-12-13 EWS  FACE EXCEPTIONS TO OPS QUEUE REQ4961           *
      * 1991-04-27 PQR  PAID NOTICE TEXT PER LEGAL REQ0239             *
      * 1991-04-03 EWS  AUDIT FINDING - OUTSTAND CONTROL TOTALS INC3517*
      * 1994-07-10 DPS  PERFORMANCE - PAID READ AHEAD REQ5118          *
      * 1995-12-08 CRP  ESCHEAT CALCULATION CORRECTED CHG7136          *
      * 1999-04-09 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-28 PQR  PERFORMANCE - PAID READ AHEAD REQ3169          *
      * 2000-11-15 GAF  PERFORMANCE - FACE READ AHEAD REQ6906          *
      * 2001-06-12 GAF  FACE FEE SCHEDULE UPDATE INC3809               *
      * 2003-12-01 RJT  FACE CALCULATION CORRECTED REQ5338             *
      * 2003-11-21 CRP  VOIDED NOTICE TEXT PER LEGAL CHG0254           *
      * 2006-08-02 RJT  PAID ROUNDING - HALF UP PRB8009                *
      *================================================================*
       01  MODTE-PARMS.
           05  MODTE-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  MODTE-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  MODTE-PAID-AMT              PIC S9(13)V99 COMP-3.
           05  MODTE-VOIDED-AMT            PIC S9(13)V99 COMP-3.
           05  MODTE-ESCHEAT-AMT           PIC S9(13)V99 COMP-3.
           05  MODTE-FACE-AMT              PIC S9(13)V99 COMP-3.
           05  MODTE-LAST-MAINT-DATE       PIC 9(08).
           05  MODTE-EXPIRY-DATE           PIC 9(08).
           05  MODTE-CLOSE-DATE            PIC 9(08).
           05  MODTE-OPEN-DATE             PIC 9(08).
           05  MODTE-ITEM-TYPE-CD          PIC X(01).
               88  MODTE-ITEM-TYPE-CD-B            VALUE 'B'.
               88  MODTE-ITEM-TYPE-CD-H            VALUE 'H'.
               88  MODTE-ITEM-TYPE-CD-S            VALUE 'S'.
               88  MODTE-ITEM-TYPE-CD-T            VALUE 'T'.
           05  MODTE-REASON-CD             PIC X(01).
               88  MODTE-REASON-CD-B               VALUE 'B'.
               88  MODTE-REASON-CD-L               VALUE 'L'.
               88  MODTE-REASON-CD-P               VALUE 'P'.
               88  MODTE-REASON-CD-S               VALUE 'S'.
               88  MODTE-REASON-CD-T               VALUE 'T'.
               88  MODTE-REASON-CD-Z               VALUE 'Z'.
           05  MODTE-TYPE-CD               PIC X(03).
               88  MODTE-TYPE-CD-AGE               VALUE 'AGE'.
               88  MODTE-TYPE-CD-JDX               VALUE 'JDX'.
               88  MODTE-TYPE-CD-LDX               VALUE 'LDX'.
               88  MODTE-TYPE-CD-RHT               VALUE 'RHT'.
           05  MODTE-CYCLE-CD              PIC X(03).
               88  MODTE-CYCLE-CD-DGU              VALUE 'DGU'.
               88  MODTE-CYCLE-CD-GEJ              VALUE 'GEJ'.
               88  MODTE-CYCLE-CD-JBE              VALUE 'JBE'.
           05  MODTE-STOP-FLAG             PIC X(01).
               88  MODTE-STOP-FLAG-YES             VALUE 'Y'.
           05  MODTE-AUTO-RENEW-FLAG       PIC X(01).
               88  MODTE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  MODTE-EXEMPT-FLAG           PIC X(01).
               88  MODTE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  MODTE-PAYMENT-CNT           PIC 9(07).
           05  MODTE-ITEM-CNT              PIC 9(07).
           05  MODTE-FEE-PCT               PIC 9(03)V9(06).
           05  MODTE-DISCOUNT              PIC 9(03)V9(06).
           05  MODTE-NAME                  PIC X(30).
           05  MODTE-REF-NO                PIC X(16).
           05  MODTE-RETURN-CODE           PIC 9(02).
