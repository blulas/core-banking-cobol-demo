      *================================================================*
      * COPYBOOK : SDLSTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET SDLSTS MAP SDLSTM         *
      *            TRANSACTION SD88 - SAFE DEPOSIT BOXES               *
      *            GENERATED FROM BMS SOURCE SDLSTS - DO NOT EDIT      *
      *================================================================*
       01  SDLSTMI.
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
           02  ITEMCNL                     PIC S9(4) COMP.
           02  ITEMCNF                     PIC X.
           02  FILLER REDEFINES ITEMCNF.
               03  ITEMCNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ITEMCNI                     PIC X(007).
           02  DRILLYL                     PIC S9(4) COMP.
           02  DRILLYF                     PIC X.
           02  FILLER REDEFINES DRILLYF.
               03  DRILLYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRILLYI                     PIC X(016).
           02  RENTYTL                     PIC S9(4) COMP.
           02  RENTYTF                     PIC X.
           02  FILLER REDEFINES RENTYTF.
               03  RENTYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RENTYTI                     PIC X(016).
           02  KEYFEEL                     PIC S9(4) COMP.
           02  KEYFEEF                     PIC X.
           02  FILLER REDEFINES KEYFEEF.
               03  KEYFEEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  KEYFEEI                     PIC X(016).
           02  LATEFEL                     PIC S9(4) COMP.
           02  LATEFEF                     PIC X.
           02  FILLER REDEFINES LATEFEF.
               03  LATEFEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LATEFEI                     PIC X(016).
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  CLOSEDL                     PIC S9(4) COMP.
           02  CLOSEDF                     PIC X.
           02  FILLER REDEFINES CLOSEDF.
               03  CLOSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CLOSEDI                     PIC X(010).
           02  VAULTCL                     PIC S9(4) COMP.
           02  VAULTCF                     PIC X.
           02  FILLER REDEFINES VAULTCF.
               03  VAULTCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  VAULTCI                     PIC X(002).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  LATEFE1L                    PIC S9(4) COMP.
           02  LATEFE1F                    PIC X.
           02  FILLER REDEFINES LATEFE1F.
               03  LATEFE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  LATEFE1I                    PIC X(016).
           02  SOURCEL                     PIC S9(4) COMP.
           02  SOURCEF                     PIC X.
           02  FILLER REDEFINES SOURCEF.
               03  SOURCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SOURCEI                     PIC X(002).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  SDLSTMO REDEFINES SDLSTMI.
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
           02  ITEMCNC                     PIC X.
           02  ITEMCNH                     PIC X.
           02  ITEMCNO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  DRILLYC                     PIC X.
           02  DRILLYH                     PIC X.
           02  DRILLYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  RENTYTC                     PIC X.
           02  RENTYTH                     PIC X.
           02  RENTYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  KEYFEEC                     PIC X.
           02  KEYFEEH                     PIC X.
           02  KEYFEEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LATEFEC                     PIC X.
           02  LATEFEH                     PIC X.
           02  LATEFEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  VAULTCC                     PIC X.
           02  VAULTCH                     PIC X.
           02  VAULTCO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  LATEFE1C                    PIC X.
           02  LATEFE1H                    PIC X.
           02  LATEFE1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  SOURCEC                     PIC X.
           02  SOURCEH                     PIC X.
           02  SOURCEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
