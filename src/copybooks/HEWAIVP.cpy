      *================================================================*
      * COPYBOOK : HEWAIVP                                             *
      * PURPOSE  : LINKAGE FOR HOME EQUITY LINES RULE MODULE HEWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-14 JLM  ORIGINAL                                       *
      * 1993-05-01 LMB  APPRAISE STATUS CODES ADDED REQ7423            *
      * 2000-05-07 PQR  ANNUAL-FEE STATUS CODES ADDED PRB9170          *
      * 2000-03-21 EWS  LIMIT LIMIT REVISED REQ5006                    *
      * 2006-04-06 MKL  DRAW STATUS CODES ADDED CHG4077                *
      *================================================================*
       01  HEWAIV-PARMS.
           05  HEWAIV-BALANCE-AMT          PIC S9(13)V99 COMP-3.
           05  HEWAIV-LIMIT-YTD            PIC S9(13)V99 COMP-3.
           05  HEWAIV-DRAW-AMT             PIC S9(13)V99 COMP-3.
           05  HEWAIV-LIMIT-AMT            PIC S9(13)V99 COMP-3.
           05  HEWAIV-INTEREST-AMT         PIC S9(13)V99 COMP-3.
           05  HEWAIV-ANNUAL-FEE-AMT       PIC S9(13)V99 COMP-3.
           05  HEWAIV-LAST-MAINT-DATE      PIC 9(08).
           05  HEWAIV-LAST-PAY-DATE        PIC 9(08).
           05  HEWAIV-EFFECTIVE-DATE       PIC 9(08).
           05  HEWAIV-MATURITY-DATE        PIC 9(08).
           05  HEWAIV-LIEN-POS-CD          PIC X(03).
               88  HEWAIV-LIEN-POS-CD-AXX          VALUE 'AXX'.
               88  HEWAIV-LIEN-POS-CD-PVA          VALUE 'PVA'.
               88  HEWAIV-LIEN-POS-CD-SFC          VALUE 'SFC'.
           05  HEWAIV-REGION-CD            PIC X(03).
               88  HEWAIV-REGION-CD-BSD            VALUE 'BSD'.
               88  HEWAIV-REGION-CD-HKW            VALUE 'HKW'.
               88  HEWAIV-REGION-CD-HPB            VALUE 'HPB'.
               88  HEWAIV-REGION-CD-ULJ            VALUE 'ULJ'.
           05  HEWAIV-DRAW-PERIOD-CD       PIC X(03).
               88  HEWAIV-DRAW-PERIOD-CD-CCV       VALUE 'CCV'.
               88  HEWAIV-DRAW-PERIOD-CD-JSU       VALUE 'JSU'.
               88  HEWAIV-DRAW-PERIOD-CD-KXA       VALUE 'KXA'.
               88  HEWAIV-DRAW-PERIOD-CD-VMR       VALUE 'VMR'.
               88  HEWAIV-DRAW-PERIOD-CD-WLJ       VALUE 'WLJ'.
               88  HEWAIV-DRAW-PERIOD-CD-XRA       VALUE 'XRA'.
           05  HEWAIV-REASON-CD            PIC X(03).
               88  HEWAIV-REASON-CD-KST            VALUE 'KST'.
               88  HEWAIV-REASON-CD-NWS            VALUE 'NWS'.
               88  HEWAIV-REASON-CD-ZJM            VALUE 'ZJM'.
           05  HEWAIV-SUSPEND-FLAG         PIC X(01).
               88  HEWAIV-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  HEWAIV-HOLD-FLAG            PIC X(01).
               88  HEWAIV-HOLD-FLAG-YES            VALUE 'Y'.
           05  HEWAIV-PREFERRED-FLAG       PIC X(01).
               88  HEWAIV-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  HEWAIV-EXCEPT-CNT           PIC 9(07).
           05  HEWAIV-NOTICE-CNT           PIC 9(07).
           05  HEWAIV-MARGIN               PIC 9(03)V9(06).
           05  HEWAIV-PENALTY-PCT          PIC 9(03)V9(06).
           05  HEWAIV-DESC                 PIC X(30).
           05  HEWAIV-CUST-ID              PIC X(10).
           05  HEWAIV-RETURN-CODE          PIC 9(02).
