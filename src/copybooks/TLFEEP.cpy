      *================================================================*
      * COPYBOOK : TLFEEP                                              *
      * PURPOSE  : LINKAGE FOR TELLER BALANCING RULE MODULE TLFEE      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-02-27 MKL  ORIGINAL                                       *
      * 1994-10-24 JLM  CASH-OUT CUTOFF MOVED INC5575                  *
      * 1999-11-22 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-04-19 PQR  VAULT-SELL LIMIT REVISED REQ1180               *
      * 2000-05-16 DPS  CASH-OUT FEE SCHEDULE UPDATE CHG7281           *
      * 2004-12-27 SKN  VAULT-BUY FEE SCHEDULE UPDATE INC6513          *
      * 2005-12-13 MKL  CASH-IN EXCEPTIONS TO OPS QUEUE PRB4670        *
      * 2006-04-07 SKN  CASH-OUT CALCULATION CORRECTED CHG2960         *
      * 2008-02-07 DPS  CASH-OUT RERUN PROTECTION CHG7003              *
      * 2009-11-03 MKL  VAULT-BUY CUTOFF MOVED INC4665                 *
      * 2010-09-11 RJT  BATCH TABLE EXPANDED PRB1540                   *
      * 2010-01-04 EWS  CASH-OUT CUTOFF MOVED INC4625                  *
      * 2011-12-28 SKN  OVERSHORT CALCULATION CORRECTED INC8783        *
      *================================================================*
       01  TLFEE-PARMS.
           05  TLFEE-VAULT-BUY-AMT         PIC S9(13)V99 COMP-3.
           05  TLFEE-VAULT-SELL-YTD        PIC S9(13)V99 COMP-3.
           05  TLFEE-CASH-IN-YTD           PIC S9(13)V99 COMP-3.
           05  TLFEE-VAULT-BUY-YTD         PIC S9(13)V99 COMP-3.
           05  TLFEE-VAULT-SELL-AMT        PIC S9(13)V99 COMP-3.
           05  TLFEE-CASH-OUT-YTD          PIC S9(13)V99 COMP-3.
           05  TLFEE-OPEN-DATE             PIC 9(08).
           05  TLFEE-NEXT-REVIEW-DATE      PIC 9(08).
           05  TLFEE-LAST-PAY-DATE         PIC 9(08).
           05  TLFEE-LAST-ACT-DATE         PIC 9(08).
           05  TLFEE-CHANNEL-CD            PIC X(01).
               88  TLFEE-CHANNEL-CD-D              VALUE 'D'.
               88  TLFEE-CHANNEL-CD-F              VALUE 'F'.
               88  TLFEE-CHANNEL-CD-M              VALUE 'M'.
               88  TLFEE-CHANNEL-CD-N              VALUE 'N'.
               88  TLFEE-CHANNEL-CD-R              VALUE 'R'.
               88  TLFEE-CHANNEL-CD-T              VALUE 'T'.
           05  TLFEE-PROOF-TYPE-CD         PIC X(02).
               88  TLFEE-PROOF-TYPE-CD-JG          VALUE 'JG'.
               88  TLFEE-PROOF-TYPE-CD-RX          VALUE 'RX'.
               88  TLFEE-PROOF-TYPE-CD-TW          VALUE 'TW'.
               88  TLFEE-PROOF-TYPE-CD-UG          VALUE 'UG'.
               88  TLFEE-PROOF-TYPE-CD-XY          VALUE 'XY'.
           05  TLFEE-DRAWER-CD             PIC X(01).
               88  TLFEE-DRAWER-CD-D               VALUE 'D'.
               88  TLFEE-DRAWER-CD-F               VALUE 'F'.
               88  TLFEE-DRAWER-CD-R               VALUE 'R'.
           05  TLFEE-CYCLE-CD              PIC X(02).
               88  TLFEE-CYCLE-CD-KX               VALUE 'KX'.
               88  TLFEE-CYCLE-CD-TA               VALUE 'TA'.
               88  TLFEE-CYCLE-CD-TG               VALUE 'TG'.
               88  TLFEE-CYCLE-CD-WG               VALUE 'WG'.
           05  TLFEE-EXEMPT-FLAG           PIC X(01).
               88  TLFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  TLFEE-STOP-FLAG             PIC X(01).
               88  TLFEE-STOP-FLAG-YES             VALUE 'Y'.
           05  TLFEE-OVERRIDE-FLAG         PIC X(01).
               88  TLFEE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  TLFEE-EXCEPT-CNT            PIC 9(07).
           05  TLFEE-NOTICE-CNT            PIC 9(07).
           05  TLFEE-DISCOUNT              PIC 9(03)V9(06).
           05  TLFEE-RATE                  PIC 9(03)V9(06).
           05  TLFEE-DESC                  PIC X(30).
           05  TLFEE-NAME                  PIC X(30).
           05  TLFEE-RETURN-CODE           PIC 9(02).
