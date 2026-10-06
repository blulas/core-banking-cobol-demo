      *================================================================*
      * COPYBOOK : EUAPRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET EUAPRS MAP EUAPRM         *
      *            TRANSACTION EU79 - EURO CONVERSION                  *
      *            GENERATED FROM BMS SOURCE EUAPRS - DO NOT EDIT      *
      *================================================================*
       01  EUAPRMI.
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
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(001).
           02  OVERRIL                     PIC S9(4) COMP.
           02  OVERRIF                     PIC X.
           02  FILLER REDEFINES OVERRIF.
               03  OVERRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OVERRII                     PIC X(001).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  TRIANGL                     PIC S9(4) COMP.
           02  TRIANGF                     PIC X.
           02  FILLER REDEFINES TRIANGF.
               03  TRIANGA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRIANGI                     PIC X(016).
           02  CONVERL                     PIC S9(4) COMP.
           02  CONVERF                     PIC X.
           02  FILLER REDEFINES CONVERF.
               03  CONVERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONVERI                     PIC X(016).
           02  REFNOL                      PIC S9(4) COMP.
           02  REFNOF                      PIC X.
           02  FILLER REDEFINES REFNOF.
               03  REFNOA                  PIC X.
           02  FILLER                      PIC X(02).
           02  REFNOI                      PIC X(016).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  ROUNDIL                     PIC S9(4) COMP.
           02  ROUNDIF                     PIC X.
           02  FILLER REDEFINES ROUNDIF.
               03  ROUNDIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ROUNDII                     PIC X(016).
           02  FEEYTDL                     PIC S9(4) COMP.
           02  FEEYTDF                     PIC X.
           02  FILLER REDEFINES FEEYTDF.
               03  FEEYTDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEYTDI                     PIC X(016).
           02  REVIEWL                     PIC S9(4) COMP.
           02  REVIEWF                     PIC X.
           02  FILLER REDEFINES REVIEWF.
               03  REVIEWA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REVIEWI                     PIC X(001).
           02  ROUNDI1L                    PIC S9(4) COMP.
           02  ROUNDI1F                    PIC X.
           02  FILLER REDEFINES ROUNDI1F.
               03  ROUNDI1A                PIC X.
           02  FILLER                      PIC X(02).
           02  ROUNDI1I                    PIC X(016).
           02  EUROYTL                     PIC S9(4) COMP.
           02  EUROYTF                     PIC X.
           02  FILLER REDEFINES EUROYTF.
               03  EUROYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EUROYTI                     PIC X(016).
           02  RESIDUL                     PIC S9(4) COMP.
           02  RESIDUF                     PIC X.
           02  FILLER REDEFINES RESIDUF.
               03  RESIDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RESIDUI                     PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  EUAPRMO REDEFINES EUAPRMI.
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
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  OVERRIC                     PIC X.
           02  OVERRIH                     PIC X.
           02  OVERRIO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  TRIANGC                     PIC X.
           02  TRIANGH                     PIC X.
           02  TRIANGO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CONVERC                     PIC X.
           02  CONVERH                     PIC X.
           02  CONVERO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  REFNOC                      PIC X.
           02  REFNOH                      PIC X.
           02  REFNOO                      PIC X(016).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  ROUNDIC                     PIC X.
           02  ROUNDIH                     PIC X.
           02  ROUNDIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  FEEYTDC                     PIC X.
           02  FEEYTDH                     PIC X.
           02  FEEYTDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  REVIEWC                     PIC X.
           02  REVIEWH                     PIC X.
           02  REVIEWO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  ROUNDI1C                    PIC X.
           02  ROUNDI1H                    PIC X.
           02  ROUNDI1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  EUROYTC                     PIC X.
           02  EUROYTH                     PIC X.
           02  EUROYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  RESIDUC                     PIC X.
           02  RESIDUH                     PIC X.
           02  RESIDUO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
