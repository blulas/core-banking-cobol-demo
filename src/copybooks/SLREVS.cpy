      *================================================================*
      * COPYBOOK : SLREVS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET SLREVS MAP SLREVM         *
      *            TRANSACTION SL74 - STUDENT LOANS                    *
      *            GENERATED FROM BMS SOURCE SLREVS - DO NOT EDIT      *
      *================================================================*
       01  SLREVMI.
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
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  LASTMAL                     PIC S9(4) COMP.
           02  LASTMAF                     PIC X.
           02  FILLER REDEFINES LASTMAF.
               03  LASTMAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTMAI                     PIC X(010).
           02  ITEMCNL                     PIC S9(4) COMP.
           02  ITEMCNF                     PIC X.
           02  FILLER REDEFINES ITEMCNF.
               03  ITEMCNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ITEMCNI                     PIC X(007).
           02  PREFERL                     PIC S9(4) COMP.
           02  PREFERF                     PIC X.
           02  FILLER REDEFINES PREFERF.
               03  PREFERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PREFERI                     PIC X(001).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  REGIONL                     PIC S9(4) COMP.
           02  REGIONF                     PIC X.
           02  FILLER REDEFINES REGIONF.
               03  REGIONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGIONI                     PIC X(001).
           02  PENALTL                     PIC S9(4) COMP.
           02  PENALTF                     PIC X.
           02  FILLER REDEFINES PENALTF.
               03  PENALTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PENALTI                     PIC X(010).
           02  LASTACL                     PIC S9(4) COMP.
           02  LASTACF                     PIC X.
           02  FILLER REDEFINES LASTACF.
               03  LASTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTACI                     PIC X(010).
           02  PRINCIL                     PIC S9(4) COMP.
           02  PRINCIF                     PIC X.
           02  FILLER REDEFINES PRINCIF.
               03  PRINCIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRINCII                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(002).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  SLREVMO REDEFINES SLREVMI.
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
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  LASTMAC                     PIC X.
           02  LASTMAH                     PIC X.
           02  LASTMAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  ITEMCNC                     PIC X.
           02  ITEMCNH                     PIC X.
           02  ITEMCNO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  PREFERC                     PIC X.
           02  PREFERH                     PIC X.
           02  PREFERO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  REGIONC                     PIC X.
           02  REGIONH                     PIC X.
           02  REGIONO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PENALTC                     PIC X.
           02  PENALTH                     PIC X.
           02  PENALTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  LASTACC                     PIC X.
           02  LASTACH                     PIC X.
           02  LASTACO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  PRINCIC                     PIC X.
           02  PRINCIH                     PIC X.
           02  PRINCIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
