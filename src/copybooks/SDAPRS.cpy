      *================================================================*
      * COPYBOOK : SDAPRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET SDAPRS MAP SDAPRM         *
      *            TRANSACTION SD22 - SAFE DEPOSIT BOXES               *
      *            GENERATED FROM BMS SOURCE SDAPRS - DO NOT EDIT      *
      *================================================================*
       01  SDAPRMI.
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
           02  DRILLAL                     PIC S9(4) COMP.
           02  DRILLAF                     PIC X.
           02  FILLER REDEFINES DRILLAF.
               03  DRILLAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRILLAI                     PIC X(016).
           02  KEYFEEL                     PIC S9(4) COMP.
           02  KEYFEEF                     PIC X.
           02  FILLER REDEFINES KEYFEEF.
               03  KEYFEEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  KEYFEEI                     PIC X(016).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  KEYFEE1L                    PIC S9(4) COMP.
           02  KEYFEE1F                    PIC X.
           02  FILLER REDEFINES KEYFEE1F.
               03  KEYFEE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  KEYFEE1I                    PIC X(016).
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  RENTYTL                     PIC S9(4) COMP.
           02  RENTYTF                     PIC X.
           02  FILLER REDEFINES RENTYTF.
               03  RENTYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RENTYTI                     PIC X(016).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
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
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  ITEMCNL                     PIC S9(4) COMP.
           02  ITEMCNF                     PIC X.
           02  FILLER REDEFINES ITEMCNF.
               03  ITEMCNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ITEMCNI                     PIC X(007).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  SDAPRMO REDEFINES SDAPRMI.
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
           02  DRILLAC                     PIC X.
           02  DRILLAH                     PIC X.
           02  DRILLAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  KEYFEEC                     PIC X.
           02  KEYFEEH                     PIC X.
           02  KEYFEEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  KEYFEE1C                    PIC X.
           02  KEYFEE1H                    PIC X.
           02  KEYFEE1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  RENTYTC                     PIC X.
           02  RENTYTH                     PIC X.
           02  RENTYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  VAULTCC                     PIC X.
           02  VAULTCH                     PIC X.
           02  VAULTCO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  ITEMCNC                     PIC X.
           02  ITEMCNH                     PIC X.
           02  ITEMCNO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
