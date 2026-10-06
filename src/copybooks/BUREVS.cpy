      *================================================================*
      * COPYBOOK : BUREVS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET BUREVS MAP BUREVM         *
      *            TRANSACTION BU52 - BUMP-UP CERTIFICATES             *
      *            GENERATED FROM BMS SOURCE BUREVS - DO NOT EDIT      *
      *================================================================*
       01  BUREVMI.
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
           02  SOURCEL                     PIC S9(4) COMP.
           02  SOURCEF                     PIC X.
           02  FILLER REDEFINES SOURCEF.
               03  SOURCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SOURCEI                     PIC X(002).
           02  ADDONAL                     PIC S9(4) COMP.
           02  ADDONAF                     PIC X.
           02  FILLER REDEFINES ADDONAF.
               03  ADDONAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ADDONAI                     PIC X(016).
           02  STEPPLL                     PIC S9(4) COMP.
           02  STEPPLF                     PIC X.
           02  FILLER REDEFINES STEPPLF.
               03  STEPPLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STEPPLI                     PIC X(003).
           02  CLOSEDL                     PIC S9(4) COMP.
           02  CLOSEDF                     PIC X.
           02  FILLER REDEFINES CLOSEDF.
               03  CLOSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CLOSEDI                     PIC X(010).
           02  SUSPENL                     PIC S9(4) COMP.
           02  SUSPENF                     PIC X.
           02  FILLER REDEFINES SUSPENF.
               03  SUSPENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SUSPENI                     PIC X(001).
           02  PRINCIL                     PIC S9(4) COMP.
           02  PRINCIF                     PIC X.
           02  FILLER REDEFINES PRINCIF.
               03  PRINCIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRINCII                     PIC X(016).
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  LASTPAL                     PIC S9(4) COMP.
           02  LASTPAF                     PIC X.
           02  FILLER REDEFINES LASTPAF.
               03  LASTPAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTPAI                     PIC X(010).
           02  LASTSTL                     PIC S9(4) COMP.
           02  LASTSTF                     PIC X.
           02  FILLER REDEFINES LASTSTF.
               03  LASTSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTSTI                     PIC X(010).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  BUREVMO REDEFINES BUREVMI.
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
           02  SOURCEC                     PIC X.
           02  SOURCEH                     PIC X.
           02  SOURCEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  ADDONAC                     PIC X.
           02  ADDONAH                     PIC X.
           02  ADDONAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  STEPPLC                     PIC X.
           02  STEPPLH                     PIC X.
           02  STEPPLO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  SUSPENC                     PIC X.
           02  SUSPENH                     PIC X.
           02  SUSPENO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PRINCIC                     PIC X.
           02  PRINCIH                     PIC X.
           02  PRINCIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  LASTPAC                     PIC X.
           02  LASTPAH                     PIC X.
           02  LASTPAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  LASTSTC                     PIC X.
           02  LASTSTH                     PIC X.
           02  LASTSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
