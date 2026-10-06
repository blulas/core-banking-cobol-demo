      *================================================================*
      * COPYBOOK : PPYTRN                                              *
      * PURPOSE  : POSITIVE PAY DAILY TRANSACTION RECORD               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-24 CRP  ORIGINAL                                       *
      * 1993-04-17 DPS  PERFORMANCE - ISSUED READ AHEAD REQ9356        *
      * 1998-12-23 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-01 RJT  PERFORMANCE - STALE READ AHEAD INC6742         *
      * 1999-01-08 RJT  VOID EXCEPTIONS TO OPS QUEUE REQ6141           *
      * 1999-11-18 LMB  PERFORMANCE - EXCEPT READ AHEAD INC8148        *
      * 2001-06-20 RJT  PAID ROUNDING - HALF UP PRB6039                *
      * 2003-05-22 BHS  EXCEPT LIMIT REVISED PRB9841                   *
      * 2003-03-04 EWS  EXCEPT STATUS CODES ADDED INC2244              *
      *================================================================*
       01  PPY-TRAN-REC.
           05  PPYT-KEY                    PIC X(12).
           05  PPYT-OPERATOR               PIC X(08).
           05  PPYT-REJECT-CD              PIC X(04).
           05  PPYT-MEMO                   PIC X(30).
           05  PPY-TRAN-AMOUNTS.
               10  PPYT-VOID-AMT           PIC S9(13)V99 COMP-3.
               10  PPYT-STALE-AMT          PIC S9(13)V99 COMP-3.
               10  PPYT-PAID-AMT           PIC S9(13)V99 COMP-3.
               10  PPYT-ISSUED-AMT         PIC S9(13)V99 COMP-3.
           05  PPY-TRAN-DATES.
               10  PPYT-EFF-DATE           PIC 9(08).
               10  PPYT-POST-DATE          PIC 9(08).
           05  PPY-TRAN-CODES.
               10  PPYT-TRAN-CD            PIC X(02).
                   88  PPYT-TRAN-CD-IS             VALUE 'IS'.
                   88  PPYT-TRAN-CD-PD             VALUE 'PD'.
                   88  PPYT-TRAN-CD-EX             VALUE 'EX'.
                   88  PPYT-TRAN-CD-VD             VALUE 'VD'.
                   88  PPYT-TRAN-CD-PR             VALUE 'PR'.
               10  PPYT-CHANNEL-CD         PIC X(03).
                   88  PPYT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  PPYT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  PPYT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  PPYT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  PPYT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  PPYT-REVERSAL-FLAG      PIC X(01).
                   88  PPYT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  PPY-TRAN-COUNTERS.
               10  PPYT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
