      *================================================================*
      * COPYBOOK : LBCALP                                              *
      * PURPOSE  : LINKAGE FOR LOCKBOX REMITTANCE RULE MODULE LBCAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-21 SKN  ORIGINAL                                       *
      * 2001-01-22 LMB  CASH-ON-HAND STATUS CODES ADDED REQ1063        *
      * 2010-03-01 MKL  NEW REMIT REPORT COLUMNS REQ5656               *
      * 2010-11-19 PQR  NEW FLOAT REPORT COLUMNS CHG3724               *
      * 2011-07-17 TWH  DEPOSIT EXCEPTIONS TO OPS QUEUE CHG0582        *
      *================================================================*
       01  LBCAL-PARMS.
           05  LBCAL-DEPOSIT-AMT           PIC S9(13)V99 COMP-3.
           05  LBCAL-DEPOSIT-YTD           PIC S9(13)V99 COMP-3.
           05  LBCAL-CHECK-AMT             PIC S9(13)V99 COMP-3.
           05  LBCAL-CASH-ON-HAND-YTD      PIC S9(13)V99 COMP-3.
           05  LBCAL-FLOAT-YTD             PIC S9(13)V99 COMP-3.
           05  LBCAL-REMIT-YTD             PIC S9(13)V99 COMP-3.
           05  LBCAL-NOTICE-DATE           PIC 9(08).
           05  LBCAL-NEXT-DUE-DATE         PIC 9(08).
           05  LBCAL-OPEN-DATE             PIC 9(08).
           05  LBCAL-EFFECTIVE-DATE        PIC 9(08).
           05  LBCAL-REGION-CD             PIC X(03).
               88  LBCAL-REGION-CD-DVP             VALUE 'DVP'.
               88  LBCAL-REGION-CD-FMZ             VALUE 'FMZ'.
               88  LBCAL-REGION-CD-KSL             VALUE 'KSL'.
               88  LBCAL-REGION-CD-NYK             VALUE 'NYK'.
           05  LBCAL-BATCH-TYPE-CD         PIC X(01).
           05  LBCAL-REASON-CD             PIC X(02).
               88  LBCAL-REASON-CD-GF              VALUE 'GF'.
               88  LBCAL-REASON-CD-UK              VALUE 'UK'.
               88  LBCAL-REASON-CD-VR              VALUE 'VR'.
               88  LBCAL-REASON-CD-XG              VALUE 'XG'.
           05  LBCAL-LOCKBOX-CD            PIC X(02).
               88  LBCAL-LOCKBOX-CD-DX             VALUE 'DX'.
               88  LBCAL-LOCKBOX-CD-JM             VALUE 'JM'.
               88  LBCAL-LOCKBOX-CD-WW             VALUE 'WW'.
               88  LBCAL-LOCKBOX-CD-XE             VALUE 'XE'.
           05  LBCAL-HOLD-FLAG             PIC X(01).
               88  LBCAL-HOLD-FLAG-YES             VALUE 'Y'.
           05  LBCAL-PREFERRED-FLAG        PIC X(01).
               88  LBCAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  LBCAL-NOTICE-SENT-FLAG      PIC X(01).
               88  LBCAL-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  LBCAL-EXCEPT-CNT            PIC 9(07).
           05  LBCAL-RETRY-CNT             PIC 9(07).
           05  LBCAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  LBCAL-DISCOUNT              PIC 9(03)V9(06).
           05  LBCAL-DESC                  PIC X(30).
           05  LBCAL-CONTACT               PIC X(20).
           05  LBCAL-RETURN-CODE           PIC 9(02).
