      *================================================================*
      * COPYBOOK : CIREVS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CIREVS MAP CIREVM         *
      *            TRANSACTION CI69 - CHECK IMAGE ARCHIVE              *
      *            GENERATED FROM BMS SOURCE CIREVS - DO NOT EDIT      *
      *================================================================*
       01  CIREVMI.
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
           02  RETRIEL                     PIC S9(4) COMP.
           02  RETRIEF                     PIC X.
           02  FILLER REDEFINES RETRIEF.
               03  RETRIEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRIEI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(007).
           02  STORAGL                     PIC S9(4) COMP.
           02  STORAGF                     PIC X.
           02  FILLER REDEFINES STORAGF.
               03  STORAGA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STORAGI                     PIC X(016).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  PREFERL                     PIC S9(4) COMP.
           02  PREFERF                     PIC X.
           02  FILLER REDEFINES PREFERF.
               03  PREFERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PREFERI                     PIC X(001).
           02  MARGINL                     PIC S9(4) COMP.
           02  MARGINF                     PIC X.
           02  FILLER REDEFINES MARGINF.
               03  MARGINA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MARGINI                     PIC X(010).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  TYPECDL                     PIC S9(4) COMP.
           02  TYPECDF                     PIC X.
           02  FILLER REDEFINES TYPECDF.
               03  TYPECDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TYPECDI                     PIC X(002).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  CLOSEDL                     PIC S9(4) COMP.
           02  CLOSEDF                     PIC X.
           02  FILLER REDEFINES CLOSEDF.
               03  CLOSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CLOSEDI                     PIC X(010).
           02  CHANNEL                     PIC S9(4) COMP.
           02  CHANNEF                     PIC X.
           02  FILLER REDEFINES CHANNEF.
               03  CHANNEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHANNEI                     PIC X(002).
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  CIREVMO REDEFINES CIREVMI.
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
           02  RETRIEC                     PIC X.
           02  RETRIEH                     PIC X.
           02  RETRIEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  STORAGC                     PIC X.
           02  STORAGH                     PIC X.
           02  STORAGO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  PREFERC                     PIC X.
           02  PREFERH                     PIC X.
           02  PREFERO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MARGINC                     PIC X.
           02  MARGINH                     PIC X.
           02  MARGINO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  TYPECDC                     PIC X.
           02  TYPECDH                     PIC X.
           02  TYPECDO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CHANNEC                     PIC X.
           02  CHANNEH                     PIC X.
           02  CHANNEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
