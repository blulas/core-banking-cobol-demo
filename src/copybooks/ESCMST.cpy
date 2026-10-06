      *================================================================*
      * COPYBOOK : ESCMST                                              *
      * PURPOSE  : MORTGAGE ESCROW MASTER RECORD (ESCMST KSDS)         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-11 DPS  ORIGINAL                                       *
      * 1992-01-12 JLM  SHORTAGE TABLE EXPANDED REQ2107                *
      * 1993-10-04 BHS  TAX NOTICE TEXT PER LEGAL INC4794              *
      * 1996-07-17 JLM  INSURANCE ROUNDING - HALF UP REQ6146           *
      * 1998-06-18 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-04-10 PQR  PAYMENT STATUS CODES ADDED REQ7896             *
      * 2000-11-04 RJT  PERFORMANCE - CUSHION READ AHEAD INC8737       *
      * 2002-11-21 JLM  SURPLUS CALCULATION CORRECTED INC4665          *
      * 2004-05-23 PQR  PAYMENT LIMIT REVISED INC5339                  *
      * 2004-02-01 JLM  INSURANCE CALCULATION CORRECTED CHG6905        *
      * 2008-10-13 JLM  DISBURSE ROUNDING - HALF UP REQ3676            *
      * 2009-01-12 SKN  INSURANCE NOTICE TEXT PER LEGAL REQ6629        *
      * 2011-04-23 CRP  INSURANCE ROUNDING - HALF UP REQ2242           *
      *================================================================*
       01  ESC-MASTER-REC.
           05  ESCM-KEY                    PIC X(12).
           05  ESCM-CUST-ID                PIC X(10).
           05  ESCM-NAME                   PIC X(30).
           05  ESCM-DESC                   PIC X(30).
           05  ESCM-REF-NO                 PIC X(16).
           05  ESCM-ADDR                   PIC X(30).
           05  ESCM-CONTACT                PIC X(20).
           05  ESC-MASTER-AMOUNTS.
               10  ESCM-TAX-AMT            PIC S9(13)V99 COMP-3.
               10  ESCM-TAX-YTD            PIC S9(13)V99 COMP-3.
               10  ESCM-INSURANCE-AMT      PIC S9(13)V99 COMP-3.
               10  ESCM-INSURANCE-YTD      PIC S9(13)V99 COMP-3.
               10  ESCM-CUSHION-AMT        PIC S9(13)V99 COMP-3.
               10  ESCM-CUSHION-YTD        PIC S9(13)V99 COMP-3.
               10  ESCM-SHORTAGE-AMT       PIC S9(13)V99 COMP-3.
               10  ESCM-SHORTAGE-YTD       PIC S9(13)V99 COMP-3.
               10  ESCM-SURPLUS-AMT        PIC S9(13)V99 COMP-3.
               10  ESCM-SURPLUS-YTD        PIC S9(13)V99 COMP-3.
               10  ESCM-DISBURSE-AMT       PIC S9(13)V99 COMP-3.
               10  ESCM-DISBURSE-YTD       PIC S9(13)V99 COMP-3.
               10  ESCM-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  ESCM-PAYMENT-YTD        PIC S9(13)V99 COMP-3.
           05  ESC-MASTER-DATES.
               10  ESCM-LAST-ACT-DATE      PIC 9(08).
               10  ESCM-EFFECTIVE-DATE     PIC 9(08).
               10  ESCM-LAST-PAY-DATE      PIC 9(08).
               10  ESCM-EXPIRY-DATE        PIC 9(08).
               10  ESCM-LAST-STMT-DATE     PIC 9(08).
               10  ESCM-NEXT-DUE-DATE      PIC 9(08).
               10  ESCM-NEXT-REVIEW-DATE   PIC 9(08).
               10  ESCM-NOTICE-DATE        PIC 9(08).
               10  ESCM-CLOSE-DATE         PIC 9(08).
           05  ESC-MASTER-CODES.
               10  ESCM-PAYEE-TYPE-CD      PIC X(02).
                   88  ESCM-PAYEE-TYPE-CD-AM       VALUE 'AM'.
                   88  ESCM-PAYEE-TYPE-CD-UE       VALUE 'UE'.
                   88  ESCM-PAYEE-TYPE-CD-UP       VALUE 'UP'.
               10  ESCM-ANALYSIS-CD        PIC X(02).
                   88  ESCM-ANALYSIS-CD-GD         VALUE 'GD'.
                   88  ESCM-ANALYSIS-CD-LY         VALUE 'LY'.
                   88  ESCM-ANALYSIS-CD-XJ         VALUE 'XJ'.
               10  ESCM-STATUS-CD          PIC X(01).
                   88  ESCM-STATUS-CD-A            VALUE 'A'.
                   88  ESCM-STATUS-CD-E            VALUE 'E'.
                   88  ESCM-STATUS-CD-R            VALUE 'R'.
                   88  ESCM-STATUS-CD-U            VALUE 'U'.
                   88  ESCM-STATUS-CD-Z            VALUE 'Z'.
               10  ESCM-TYPE-CD            PIC X(03).
               10  ESCM-REGION-CD          PIC X(01).
                   88  ESCM-REGION-CD-B            VALUE 'B'.
                   88  ESCM-REGION-CD-G            VALUE 'G'.
                   88  ESCM-REGION-CD-Y            VALUE 'Y'.
               10  ESCM-REASON-CD          PIC X(01).
                   88  ESCM-REASON-CD-C            VALUE 'C'.
                   88  ESCM-REASON-CD-F            VALUE 'F'.
                   88  ESCM-REASON-CD-K            VALUE 'K'.
                   88  ESCM-REASON-CD-L            VALUE 'L'.
                   88  ESCM-REASON-CD-S            VALUE 'S'.
                   88  ESCM-REASON-CD-X            VALUE 'X'.
               10  ESCM-SUSPEND-FLAG       PIC X(01).
                   88  ESCM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  ESCM-HOLD-FLAG          PIC X(01).
                   88  ESCM-HOLD-FLAG-YES          VALUE 'Y'.
               10  ESCM-PREFERRED-FLAG     PIC X(01).
                   88  ESCM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  ESCM-OVERRIDE-FLAG      PIC X(01).
                   88  ESCM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  ESCM-NOTICE-SENT-FLAG   PIC X(01).
                   88  ESCM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  ESCM-STOP-FLAG          PIC X(01).
                   88  ESCM-STOP-FLAG-YES          VALUE 'Y'.
           05  ESC-MASTER-COUNTERS.
               10  ESCM-RETRY-CNT          PIC 9(07).
               10  ESCM-CYCLE-CNT          PIC 9(07).
               10  ESCM-NOTICE-CNT         PIC 9(07).
               10  ESCM-EXCEPT-CNT         PIC 9(07).
               10  ESCM-FEE-PCT            PIC 9(03)V9(06).
               10  ESCM-DISCOUNT           PIC 9(03)V9(06).
               10  ESCM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
