      *================================================================*
      * COPYBOOK : ARPRCP                                              *
      * PURPOSE  : LINKAGE FOR ACCOUNT RECONCILEMENT RULE MODULE ARPRC *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-22 TWH  ORIGINAL                                       *
      * 1997-03-21 PQR  FEE RERUN PROTECTION CHG6431                   *
      * 1997-02-26 JLM  VOID EXCEPTIONS TO OPS QUEUE REQ5760           *
      * 1997-03-07 MKL  NEW ISSUED REPORT COLUMNS CHG9436              *
      * 1999-01-07 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-11-23 SKN  STOP ROUNDING - HALF UP CHG0351                *
      * 2002-11-25 EWS  NEW OUTSTAND REPORT COLUMNS REQ2858            *
      * 2004-08-22 RJT  STOP CALCULATION CORRECTED INC4148             *
      * 2005-05-09 JLM  FEE EXCEPTIONS TO OPS QUEUE CHG0552            *
      * 2006-10-18 EWS  STOP FEE SCHEDULE UPDATE PRB2100               *
      * 2007-11-15 CRP  ISSUED TABLE EXPANDED CHG6108                  *
      * 2009-11-15 JLM  OUTSTAND LIMIT REVISED REQ5315                 *
      * 2009-06-20 CRP  PAID TABLE EXPANDED CHG9948                    *
      *================================================================*
       01  ARPRC-PARMS.
           05  ARPRC-OUTSTAND-YTD          PIC S9(13)V99 COMP-3.
           05  ARPRC-ISSUED-AMT            PIC S9(13)V99 COMP-3.
           05  ARPRC-ISSUED-YTD            PIC S9(13)V99 COMP-3.
           05  ARPRC-FEE-YTD               PIC S9(13)V99 COMP-3.
           05  ARPRC-OUTSTAND-AMT          PIC S9(13)V99 COMP-3.
           05  ARPRC-STOP-YTD              PIC S9(13)V99 COMP-3.
           05  ARPRC-OPEN-DATE             PIC 9(08).
           05  ARPRC-NEXT-DUE-DATE         PIC 9(08).
           05  ARPRC-NEXT-REVIEW-DATE      PIC 9(08).
           05  ARPRC-LAST-ACT-DATE         PIC 9(08).
           05  ARPRC-RECON-TYPE-CD         PIC X(03).
               88  ARPRC-RECON-TYPE-CD-EEH         VALUE 'EEH'.
               88  ARPRC-RECON-TYPE-CD-KBS         VALUE 'KBS'.
               88  ARPRC-RECON-TYPE-CD-PAC         VALUE 'PAC'.
               88  ARPRC-RECON-TYPE-CD-VKT         VALUE 'VKT'.
               88  ARPRC-RECON-TYPE-CD-YMX         VALUE 'YMX'.
               88  ARPRC-RECON-TYPE-CD-ZVT         VALUE 'ZVT'.
           05  ARPRC-BRANCH-CD             PIC X(02).
               88  ARPRC-BRANCH-CD-FN              VALUE 'FN'.
               88  ARPRC-BRANCH-CD-GM              VALUE 'GM'.
               88  ARPRC-BRANCH-CD-GV              VALUE 'GV'.
               88  ARPRC-BRANCH-CD-HK              VALUE 'HK'.
               88  ARPRC-BRANCH-CD-VX              VALUE 'VX'.
           05  ARPRC-CHANNEL-CD            PIC X(02).
               88  ARPRC-CHANNEL-CD-ME             VALUE 'ME'.
               88  ARPRC-CHANNEL-CD-SW             VALUE 'SW'.
               88  ARPRC-CHANNEL-CD-UA             VALUE 'UA'.
               88  ARPRC-CHANNEL-CD-WB             VALUE 'WB'.
           05  ARPRC-SOURCE-CD             PIC X(02).
               88  ARPRC-SOURCE-CD-GW              VALUE 'GW'.
               88  ARPRC-SOURCE-CD-MB              VALUE 'MB'.
               88  ARPRC-SOURCE-CD-MH              VALUE 'MH'.
               88  ARPRC-SOURCE-CD-UG              VALUE 'UG'.
               88  ARPRC-SOURCE-CD-UR              VALUE 'UR'.
           05  ARPRC-HOLD-FLAG             PIC X(01).
               88  ARPRC-HOLD-FLAG-YES             VALUE 'Y'.
           05  ARPRC-STOP-FLAG             PIC X(01).
               88  ARPRC-STOP-FLAG-YES             VALUE 'Y'.
           05  ARPRC-PREFERRED-FLAG        PIC X(01).
               88  ARPRC-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  ARPRC-PAYMENT-CNT           PIC 9(07).
           05  ARPRC-EXCEPT-CNT            PIC 9(07).
           05  ARPRC-DISCOUNT              PIC 9(03)V9(06).
           05  ARPRC-PENALTY-PCT           PIC 9(03)V9(06).
           05  ARPRC-NAME                  PIC X(30).
           05  ARPRC-ADDR                  PIC X(30).
           05  ARPRC-RETURN-CODE           PIC 9(02).
