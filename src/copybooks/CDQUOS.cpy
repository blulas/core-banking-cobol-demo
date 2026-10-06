      *================================================================*
      * COPYBOOK : CDQUOS                                                *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CDQUOS MAP CDQUOM          *
      *            TRANSACTION CB21 - CD EARLY WITHDRAWAL QUOTE         *
      *            GENERATED FROM BMS SOURCE CDQUOS - DO NOT EDIT     **
      *================================================================*
       01  CDQUOMI.
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
           02  ACCTNOL                     PIC S9(4) COMP.
           02  ACCTNOF                     PIC X.
           02  FILLER REDEFINES ACCTNOF.
               03  ACCTNOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTNOI                     PIC X(012).
           02  OWNERL                      PIC S9(4) COMP.
           02  OWNERF                      PIC X.
           02  FILLER REDEFINES OWNERF.
               03  OWNERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  OWNERI                      PIC X(030).
           02  WDAMTL                      PIC S9(4) COMP.
           02  WDAMTF                      PIC X.
           02  FILLER REDEFINES WDAMTF.
               03  WDAMTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  WDAMTI                      PIC X(015).
           02  WDDATEL                     PIC S9(4) COMP.
           02  WDDATEF                     PIC X.
           02  FILLER REDEFINES WDDATEF.
               03  WDDATEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WDDATEI                     PIC X(008).
           02  PRODCDL                     PIC S9(4) COMP.
           02  PRODCDF                     PIC X.
           02  FILLER REDEFINES PRODCDF.
               03  PRODCDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRODCDI                     PIC X(004).
           02  PRODNML                     PIC S9(4) COMP.
           02  PRODNMF                     PIC X.
           02  FILLER REDEFINES PRODNMF.
               03  PRODNMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRODNMI                     PIC X(030).
           02  PRINCL                      PIC S9(4) COMP.
           02  PRINCF                      PIC X.
           02  FILLER REDEFINES PRINCF.
               03  PRINCA                  PIC X.
           02  FILLER                      PIC X(02).
           02  PRINCI                      PIC X(017).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  FUNDDTL                     PIC S9(4) COMP.
           02  FUNDDTF                     PIC X.
           02  FILLER REDEFINES FUNDDTF.
               03  FUNDDTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FUNDDTI                     PIC X(010).
           02  MATDTL                      PIC S9(4) COMP.
           02  MATDTF                      PIC X.
           02  FILLER REDEFINES MATDTF.
               03  MATDTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  MATDTI                      PIC X(010).
           02  GRACDTL                     PIC S9(4) COMP.
           02  GRACDTF                     PIC X.
           02  FILLER REDEFINES GRACDTF.
               03  GRACDTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  GRACDTI                     PIC X(010).
           02  TERML                       PIC S9(4) COMP.
           02  TERMF                       PIC X.
           02  FILLER REDEFINES TERMF.
               03  TERMA                   PIC X.
           02  FILLER                      PIC X(02).
           02  TERMI                       PIC X(003).
           02  ACCINTL                     PIC S9(4) COMP.
           02  ACCINTF                     PIC X.
           02  FILLER REDEFINES ACCINTF.
               03  ACCINTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCINTI                     PIC X(017).
           02  PENDAYSL                    PIC S9(4) COMP.
           02  PENDAYSF                    PIC X.
           02  FILLER REDEFINES PENDAYSF.
               03  PENDAYSA                PIC X.
           02  FILLER                      PIC X(02).
           02  PENDAYSI                    PIC X(003).
           02  WAIVERL                     PIC S9(4) COMP.
           02  WAIVERF                     PIC X.
           02  FILLER REDEFINES WAIVERF.
               03  WAIVERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WAIVERI                     PIC X(002).
           02  PENAMTL                     PIC S9(4) COMP.
           02  PENAMTF                     PIC X.
           02  FILLER REDEFINES PENAMTF.
               03  PENAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENAMTI                     PIC X(017).
           02  PENINTL                     PIC S9(4) COMP.
           02  PENINTF                     PIC X.
           02  FILLER REDEFINES PENINTF.
               03  PENINTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENINTI                     PIC X(017).
           02  PENPRNL                     PIC S9(4) COMP.
           02  PENPRNF                     PIC X.
           02  FILLER REDEFINES PENPRNF.
               03  PENPRNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENPRNI                     PIC X(017).
           02  NETAMTL                     PIC S9(4) COMP.
           02  NETAMTF                     PIC X.
           02  FILLER REDEFINES NETAMTF.
               03  NETAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NETAMTI                     PIC X(017).
           02  NOTEL                       PIC S9(4) COMP.
           02  NOTEF                       PIC X.
           02  FILLER REDEFINES NOTEF.
               03  NOTEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NOTEI                       PIC X(077).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  CDQUOMO REDEFINES CDQUOMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  ACCTNOO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  OWNERO                      PIC X(030).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WDAMTO                      PIC X(015).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WDDATEO                     PIC X(008).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PRODCDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PRODNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PRINCO                      PIC X(017).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  FUNDDTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  MATDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  GRACDTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TERMO                       PIC X(003).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  ACCINTO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PENDAYSO                    PIC X(003).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WAIVERO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PENAMTO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PENINTO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PENPRNO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  NETAMTO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  NOTEO                       PIC X(077).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  MSGO                        PIC X(079).
