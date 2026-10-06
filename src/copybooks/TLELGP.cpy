      *================================================================*
      * COPYBOOK : TLELGP                                              *
      * PURPOSE  : LINKAGE FOR TELLER BALANCING RULE MODULE TLELG      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-06-04 RJT  ORIGINAL                                       *
      * 1999-08-11 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-07-23 PQR  OVERSHORT EXCEPTIONS TO OPS QUEUE INC7650      *
      * 2007-09-28 EWS  CASH-OUT RERUN PROTECTION REQ5223              *
      * 2011-06-21 GAF  VAULT-BUY ROUNDING - HALF UP CHG0375           *
      *================================================================*
       01  TLELG-PARMS.
           05  TLELG-VAULT-SELL-YTD        PIC S9(13)V99 COMP-3.
           05  TLELG-CASH-IN-AMT           PIC S9(13)V99 COMP-3.
           05  TLELG-CASH-OUT-AMT          PIC S9(13)V99 COMP-3.
           05  TLELG-CASH-OUT-YTD          PIC S9(13)V99 COMP-3.
           05  TLELG-OVERSHORT-YTD         PIC S9(13)V99 COMP-3.
           05  TLELG-OVERSHORT-AMT         PIC S9(13)V99 COMP-3.
           05  TLELG-EXPIRY-DATE           PIC 9(08).
           05  TLELG-LAST-ACT-DATE         PIC 9(08).
           05  TLELG-LAST-PAY-DATE         PIC 9(08).
           05  TLELG-EFFECTIVE-DATE        PIC 9(08).
           05  TLELG-PROOF-TYPE-CD         PIC X(02).
               88  TLELG-PROOF-TYPE-CD-JG          VALUE 'JG'.
               88  TLELG-PROOF-TYPE-CD-RX          VALUE 'RX'.
               88  TLELG-PROOF-TYPE-CD-TW          VALUE 'TW'.
               88  TLELG-PROOF-TYPE-CD-UG          VALUE 'UG'.
               88  TLELG-PROOF-TYPE-CD-XY          VALUE 'XY'.
           05  TLELG-CYCLE-CD              PIC X(02).
               88  TLELG-CYCLE-CD-KX               VALUE 'KX'.
               88  TLELG-CYCLE-CD-TA               VALUE 'TA'.
               88  TLELG-CYCLE-CD-TG               VALUE 'TG'.
               88  TLELG-CYCLE-CD-WG               VALUE 'WG'.
           05  TLELG-SOURCE-CD             PIC X(02).
               88  TLELG-SOURCE-CD-AF              VALUE 'AF'.
               88  TLELG-SOURCE-CD-PT              VALUE 'PT'.
               88  TLELG-SOURCE-CD-SZ              VALUE 'SZ'.
               88  TLELG-SOURCE-CD-VK              VALUE 'VK'.
               88  TLELG-SOURCE-CD-VW              VALUE 'VW'.
           05  TLELG-CHANNEL-CD            PIC X(01).
               88  TLELG-CHANNEL-CD-D              VALUE 'D'.
               88  TLELG-CHANNEL-CD-F              VALUE 'F'.
               88  TLELG-CHANNEL-CD-M              VALUE 'M'.
               88  TLELG-CHANNEL-CD-N              VALUE 'N'.
               88  TLELG-CHANNEL-CD-R              VALUE 'R'.
               88  TLELG-CHANNEL-CD-T              VALUE 'T'.
           05  TLELG-REVIEW-FLAG           PIC X(01).
               88  TLELG-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TLELG-OVERRIDE-FLAG         PIC X(01).
               88  TLELG-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  TLELG-STOP-FLAG             PIC X(01).
               88  TLELG-STOP-FLAG-YES             VALUE 'Y'.
           05  TLELG-ITEM-CNT              PIC 9(07).
           05  TLELG-NOTICE-CNT            PIC 9(07).
           05  TLELG-RATE                  PIC 9(03)V9(06).
           05  TLELG-DISCOUNT              PIC 9(03)V9(06).
           05  TLELG-DESC                  PIC X(30).
           05  TLELG-ADDR                  PIC X(30).
           05  TLELG-RETURN-CODE           PIC 9(02).
