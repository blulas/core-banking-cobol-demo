      *================================================================*
      * COPYBOOK : SDADDS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET SDADDS MAP SDADDM         *
      *            TRANSACTION SD89 - SAFE DEPOSIT BOXES               *
      *            GENERATED FROM BMS SOURCE SDADDS - DO NOT EDIT      *
      *================================================================*
       01  SDADDMI.
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
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  REASONL                     PIC S9(4) COMP.
           02  REASONF                     PIC X.
           02  FILLER REDEFINES REASONF.
               03  REASONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REASONI                     PIC X(001).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
           02  DEPOSIL                     PIC S9(4) COMP.
           02  DEPOSIF                     PIC X.
           02  FILLER REDEFINES DEPOSIF.
               03  DEPOSIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DEPOSII                     PIC X(016).
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  LATEFEL                     PIC S9(4) COMP.
           02  LATEFEF                     PIC X.
           02  FILLER REDEFINES LATEFEF.
               03  LATEFEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LATEFEI                     PIC X(016).
           02  DRILLYL                     PIC S9(4) COMP.
           02  DRILLYF                     PIC X.
           02  FILLER REDEFINES DRILLYF.
               03  DRILLYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRILLYI                     PIC X(016).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  KEYFEEL                     PIC S9(4) COMP.
           02  KEYFEEF                     PIC X.
           02  FILLER REDEFINES KEYFEEF.
               03  KEYFEEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  KEYFEEI                     PIC X(016).
           02  KEYFEE1L                    PIC S9(4) COMP.
           02  KEYFEE1F                    PIC X.
           02  FILLER REDEFINES KEYFEE1F.
               03  KEYFEE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  KEYFEE1I                    PIC X(016).
           02  REFNOL                      PIC S9(4) COMP.
           02  REFNOF                      PIC X.
           02  FILLER REDEFINES REFNOF.
               03  REFNOA                  PIC X.
           02  FILLER                      PIC X(02).
           02  REFNOI                      PIC X(016).
           02  RENTAML                     PIC S9(4) COMP.
           02  RENTAMF                     PIC X.
           02  FILLER REDEFINES RENTAMF.
               03  RENTAMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RENTAMI                     PIC X(016).
           02  OVERRIL                     PIC S9(4) COMP.
           02  OVERRIF                     PIC X.
           02  FILLER REDEFINES OVERRIF.
               03  OVERRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OVERRII                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  SDADDMO REDEFINES SDADDMI.
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
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  REASONC                     PIC X.
           02  REASONH                     PIC X.
           02  REASONO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  DEPOSIC                     PIC X.
           02  DEPOSIH                     PIC X.
           02  DEPOSIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  LATEFEC                     PIC X.
           02  LATEFEH                     PIC X.
           02  LATEFEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  DRILLYC                     PIC X.
           02  DRILLYH                     PIC X.
           02  DRILLYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  KEYFEEC                     PIC X.
           02  KEYFEEH                     PIC X.
           02  KEYFEEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  KEYFEE1C                    PIC X.
           02  KEYFEE1H                    PIC X.
           02  KEYFEE1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  REFNOC                      PIC X.
           02  REFNOH                      PIC X.
           02  REFNOO                      PIC X(016).
           02  FILLER                      PIC X(03).
           02  RENTAMC                     PIC X.
           02  RENTAMH                     PIC X.
           02  RENTAMO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OVERRIC                     PIC X.
           02  OVERRIH                     PIC X.
           02  OVERRIO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
