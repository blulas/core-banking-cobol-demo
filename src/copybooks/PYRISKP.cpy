      *================================================================*
      * COPYBOOK : PYRISKP                                             *
      * PURPOSE  : LINKAGE FOR PAYROLL SERVICES RULE MODULE PYRISK     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-09-25 RJT  ORIGINAL                                       *
      * 1991-01-23 LMB  PERFORMANCE - NET READ AHEAD INC8594           *
      * 1991-08-04 SKN  DEDUCT FEE SCHEDULE UPDATE REQ8154             *
      * 1992-11-28 CRP  NET EXCEPTIONS TO OPS QUEUE INC5755            *
      * 1992-05-10 DPS  NEW STATE-TAX REPORT COLUMNS PRB3331           *
      * 2006-02-15 PQR  PERFORMANCE - GROSS READ AHEAD INC1757         *
      * 2006-06-01 LMB  PERFORMANCE - DEDUCT READ AHEAD CHG5653        *
      *================================================================*
       01  PYRISK-PARMS.
           05  PYRISK-GROSS-YTD            PIC S9(13)V99 COMP-3.
           05  PYRISK-DEDUCT-AMT           PIC S9(13)V99 COMP-3.
           05  PYRISK-FICA-AMT             PIC S9(13)V99 COMP-3.
           05  PYRISK-STATE-TAX-YTD        PIC S9(13)V99 COMP-3.
           05  PYRISK-DEDUCT-YTD           PIC S9(13)V99 COMP-3.
           05  PYRISK-GROSS-AMT            PIC S9(13)V99 COMP-3.
           05  PYRISK-NEXT-DUE-DATE        PIC 9(08).
           05  PYRISK-EFFECTIVE-DATE       PIC 9(08).
           05  PYRISK-OPEN-DATE            PIC 9(08).
           05  PYRISK-NEXT-REVIEW-DATE     PIC 9(08).
           05  PYRISK-REGION-CD            PIC X(02).
               88  PYRISK-REGION-CD-DP             VALUE 'DP'.
               88  PYRISK-REGION-CD-TU             VALUE 'TU'.
               88  PYRISK-REGION-CD-WD             VALUE 'WD'.
           05  PYRISK-TAX-STATE-CD         PIC X(03).
               88  PYRISK-TAX-STATE-CD-CAZ         VALUE 'CAZ'.
               88  PYRISK-TAX-STATE-CD-LZU         VALUE 'LZU'.
               88  PYRISK-TAX-STATE-CD-TUF         VALUE 'TUF'.
               88  PYRISK-TAX-STATE-CD-WKE         VALUE 'WKE'.
           05  PYRISK-STATUS-CD            PIC X(02).
               88  PYRISK-STATUS-CD-MV             VALUE 'MV'.
               88  PYRISK-STATUS-CD-RD             VALUE 'RD'.
               88  PYRISK-STATUS-CD-ST             VALUE 'ST'.
               88  PYRISK-STATUS-CD-TJ             VALUE 'TJ'.
               88  PYRISK-STATUS-CD-UC             VALUE 'UC'.
           05  PYRISK-SOURCE-CD            PIC X(02).
               88  PYRISK-SOURCE-CD-NY             VALUE 'NY'.
               88  PYRISK-SOURCE-CD-PL             VALUE 'PL'.
               88  PYRISK-SOURCE-CD-VK             VALUE 'VK'.
               88  PYRISK-SOURCE-CD-WB             VALUE 'WB'.
               88  PYRISK-SOURCE-CD-XL             VALUE 'XL'.
           05  PYRISK-STOP-FLAG            PIC X(01).
               88  PYRISK-STOP-FLAG-YES            VALUE 'Y'.
           05  PYRISK-NOTICE-SENT-FLAG     PIC X(01).
               88  PYRISK-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  PYRISK-AUTO-RENEW-FLAG      PIC X(01).
               88  PYRISK-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  PYRISK-CYCLE-CNT            PIC 9(07).
           05  PYRISK-NOTICE-CNT           PIC 9(07).
           05  PYRISK-RATE                 PIC 9(03)V9(06).
           05  PYRISK-MARGIN               PIC 9(03)V9(06).
           05  PYRISK-NAME                 PIC X(30).
           05  PYRISK-REF-NO               PIC X(16).
           05  PYRISK-RETURN-CODE          PIC 9(02).
