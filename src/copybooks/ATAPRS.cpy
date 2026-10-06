      *================================================================*
      * COPYBOOK : ATAPRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET ATAPRS MAP ATAPRM         *
      *            TRANSACTION AT35 - ATM NETWORK SETTLEMENT           *
      *            GENERATED FROM BMS SOURCE ATAPRS - DO NOT EDIT      *
      *================================================================*
       01  ATAPRMI.
           02  FILLER                      PIC X(12).
           02  TRANIDL                     PIC S9(4) COMP.
           02  TRANIDF                     PIC X.
           02  FILLER REDEFINES TRANIDF.
               03  TRANIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRANIDI                     PIC X(004).
           02  TITLEL                      PIC S9(4) COMP.
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    PIC S9(4) COMP.
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  FILLER                      PIC X(02).
           02  CURDATEI                    PIC X(010).
           02  KEYL                        PIC S9(4) COMP.
           02  KEYF                        PIC X.
           02  FILLER REDEFINES KEYF.
               03  KEYA                    PIC X.
           02  FILLER                      PIC X(02).
           02  KEYI                        PIC X(012).
           02  CLOSEDL                     PIC S9(4) COMP.
           02  CLOSEDF                     PIC X.
           02  FILLER REDEFINES CLOSEDF.
               03  CLOSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CLOSEDI                     PIC X(010).
           02  MATURIL                     PIC S9(4) COMP.
           02  MATURIF                     PIC X.
           02  FILLER REDEFINES MATURIF.
               03  MATURIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MATURII                     PIC X(010).
           02  BRANCHL                     PIC S9(4) COMP.
           02  BRANCHF                     PIC X.
           02  FILLER REDEFINES BRANCHF.
               03  BRANCHA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BRANCHI                     PIC X(002).
           02  NETWORL                     PIC S9(4) COMP.
           02  NETWORF                     PIC X.
           02  FILLER REDEFINES NETWORF.
               03  NETWORA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NETWORI                     PIC X(001).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  INTERCL                     PIC S9(4) COMP.
           02  INTERCF                     PIC X.
           02  FILLER REDEFINES INTERCF.
               03  INTERCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  INTERCI                     PIC X(016).
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  FEEPCTL                     PIC S9(4) COMP.
           02  FEEPCTF                     PIC X.
           02  FILLER REDEFINES FEEPCTF.
               03  FEEPCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEPCTI                     PIC X(010).
           02  CASSETL                     PIC S9(4) COMP.
           02  CASSETF                     PIC X.
           02  FILLER REDEFINES CASSETF.
               03  CASSETA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CASSETI                     PIC X(016).
           02  NOTICE1L                    PIC S9(4) COMP.
           02  NOTICE1F                    PIC X.
           02  FILLER REDEFINES NOTICE1F.
               03  NOTICE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICE1I                    PIC X(007).
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  ATAPRMO REDEFINES ATAPRMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TRANIDC                     PIC X.
           02  TRANIDH                     PIC X.
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TITLEC                      PIC X.
           02  TITLEH                      PIC X.
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEC                    PIC X.
           02  CURDATEH                    PIC X.
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  KEYC                        PIC X.
           02  KEYH                        PIC X.
           02  KEYO                        PIC X(012).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MATURIC                     PIC X.
           02  MATURIH                     PIC X.
           02  MATURIO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  BRANCHC                     PIC X.
           02  BRANCHH                     PIC X.
           02  BRANCHO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  NETWORC                     PIC X.
           02  NETWORH                     PIC X.
           02  NETWORO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  INTERCC                     PIC X.
           02  INTERCH                     PIC X.
           02  INTERCO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  FEEPCTC                     PIC X.
           02  FEEPCTH                     PIC X.
           02  FEEPCTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CASSETC                     PIC X.
           02  CASSETH                     PIC X.
           02  CASSETO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICE1C                    PIC X.
           02  NOTICE1H                    PIC X.
           02  NOTICE1O                    PIC X(007).
           02  FILLER                      PIC X(03).
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
