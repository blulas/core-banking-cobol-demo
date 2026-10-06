      *================================================================*
      * COPYBOOK : ARPTRN                                              *
      * PURPOSE  : ACCOUNT RECONCILEMENT DAILY TRANSACTION RECORD      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-07 EWS  ORIGINAL                                       *
      * 2001-06-20 GAF  PERFORMANCE - STOP READ AHEAD REQ4134          *
      * 2004-08-26 BHS  OUTSTAND CALCULATION CORRECTED CHG9445         *
      * 2005-06-22 BHS  AUDIT FINDING - STOP CONTROL TOTALS CHG1930    *
      * 2007-01-08 TWH  OUTSTAND ROUNDING - HALF UP INC3926            *
      * 2009-05-19 GAF  STOP ROUNDING - HALF UP REQ5678                *
      * 2010-03-26 TWH  OUTSTAND RERUN PROTECTION REQ4932              *
      * 2010-10-03 GAF  STOP LIMIT REVISED CHG0533                     *
      *================================================================*
       01  ARP-TRAN-REC.
           05  ARPT-KEY                    PIC X(12).
           05  ARPT-OPERATOR               PIC X(08).
           05  ARPT-REJECT-CD              PIC X(04).
           05  ARPT-MEMO                   PIC X(30).
           05  ARP-TRAN-AMOUNTS.
               10  ARPT-OUTSTAND-AMT       PIC S9(13)V99 COMP-3.
               10  ARPT-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  ARPT-VOID-AMT           PIC S9(13)V99 COMP-3.
               10  ARPT-ISSUED-AMT         PIC S9(13)V99 COMP-3.
           05  ARP-TRAN-DATES.
               10  ARPT-EFF-DATE           PIC 9(08).
               10  ARPT-POST-DATE          PIC 9(08).
           05  ARP-TRAN-CODES.
               10  ARPT-TRAN-CD            PIC X(02).
                   88  ARPT-TRAN-CD-IS             VALUE 'IS'.
                   88  ARPT-TRAN-CD-PD             VALUE 'PD'.
                   88  ARPT-TRAN-CD-SP             VALUE 'SP'.
                   88  ARPT-TRAN-CD-VD             VALUE 'VD'.
                   88  ARPT-TRAN-CD-FE             VALUE 'FE'.
               10  ARPT-CHANNEL-CD         PIC X(03).
                   88  ARPT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  ARPT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  ARPT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  ARPT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  ARPT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  ARPT-REVERSAL-FLAG      PIC X(01).
                   88  ARPT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  ARP-TRAN-COUNTERS.
               10  ARPT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
