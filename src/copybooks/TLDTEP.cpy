      *================================================================*
      * COPYBOOK : TLDTEP                                              *
      * PURPOSE  : LINKAGE FOR TELLER BALANCING RULE MODULE TLDTE      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-07-07 MKL  ORIGINAL                                       *
      * 1998-09-27 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-06-26 BHS  VAULT-BUY NOTICE TEXT PER LEGAL REQ6999        *
      * 2005-01-14 SKN  NEW CASH-IN REPORT COLUMNS CHG6040             *
      * 2011-09-04 TWH  NEW CASH-OUT REPORT COLUMNS CHG8956            *
      *================================================================*
       01  TLDTE-PARMS.
           05  TLDTE-CASH-IN-YTD           PIC S9(13)V99 COMP-3.
           05  TLDTE-CASH-OUT-YTD          PIC S9(13)V99 COMP-3.
           05  TLDTE-CASH-OUT-AMT          PIC S9(13)V99 COMP-3.
           05  TLDTE-VAULT-BUY-YTD         PIC S9(13)V99 COMP-3.
           05  TLDTE-BATCH-AMT             PIC S9(13)V99 COMP-3.
           05  TLDTE-CASH-IN-AMT           PIC S9(13)V99 COMP-3.
           05  TLDTE-LAST-STMT-DATE        PIC 9(08).
           05  TLDTE-LAST-ACT-DATE         PIC 9(08).
           05  TLDTE-NOTICE-DATE           PIC 9(08).
           05  TLDTE-EFFECTIVE-DATE        PIC 9(08).
           05  TLDTE-CYCLE-CD              PIC X(02).
               88  TLDTE-CYCLE-CD-KX               VALUE 'KX'.
               88  TLDTE-CYCLE-CD-TA               VALUE 'TA'.
               88  TLDTE-CYCLE-CD-TG               VALUE 'TG'.
               88  TLDTE-CYCLE-CD-WG               VALUE 'WG'.
           05  TLDTE-CHANNEL-CD            PIC X(01).
               88  TLDTE-CHANNEL-CD-D              VALUE 'D'.
               88  TLDTE-CHANNEL-CD-F              VALUE 'F'.
               88  TLDTE-CHANNEL-CD-M              VALUE 'M'.
               88  TLDTE-CHANNEL-CD-N              VALUE 'N'.
               88  TLDTE-CHANNEL-CD-R              VALUE 'R'.
               88  TLDTE-CHANNEL-CD-T              VALUE 'T'.
           05  TLDTE-SOURCE-CD             PIC X(02).
               88  TLDTE-SOURCE-CD-AF              VALUE 'AF'.
               88  TLDTE-SOURCE-CD-PT              VALUE 'PT'.
               88  TLDTE-SOURCE-CD-SZ              VALUE 'SZ'.
               88  TLDTE-SOURCE-CD-VK              VALUE 'VK'.
               88  TLDTE-SOURCE-CD-VW              VALUE 'VW'.
           05  TLDTE-DRAWER-CD             PIC X(01).
               88  TLDTE-DRAWER-CD-D               VALUE 'D'.
               88  TLDTE-DRAWER-CD-F               VALUE 'F'.
               88  TLDTE-DRAWER-CD-R               VALUE 'R'.
           05  TLDTE-SUSPEND-FLAG          PIC X(01).
               88  TLDTE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TLDTE-HOLD-FLAG             PIC X(01).
               88  TLDTE-HOLD-FLAG-YES             VALUE 'Y'.
           05  TLDTE-STOP-FLAG             PIC X(01).
               88  TLDTE-STOP-FLAG-YES             VALUE 'Y'.
           05  TLDTE-NOTICE-CNT            PIC 9(07).
           05  TLDTE-RETRY-CNT             PIC 9(07).
           05  TLDTE-DISCOUNT              PIC 9(03)V9(06).
           05  TLDTE-FEE-PCT               PIC 9(03)V9(06).
           05  TLDTE-DESC                  PIC X(30).
           05  TLDTE-CONTACT               PIC X(20).
           05  TLDTE-RETURN-CODE           PIC 9(02).
