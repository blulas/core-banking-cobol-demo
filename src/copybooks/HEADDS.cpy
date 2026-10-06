      *================================================================*
      * COPYBOOK : HEADDS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET HEADDS MAP HEADDM         *
      *            TRANSACTION HE61 - HOME EQUITY LINES                *
      *            GENERATED FROM BMS SOURCE HEADDS - DO NOT EDIT      *
      *================================================================*
       01  HEADDMI.
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
           02  DRAWAML                     PIC S9(4) COMP.
           02  DRAWAMF                     PIC X.
           02  FILLER REDEFINES DRAWAMF.
               03  DRAWAMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRAWAMI                     PIC X(016).
           02  BALANCL                     PIC S9(4) COMP.
           02  BALANCF                     PIC X.
           02  FILLER REDEFINES BALANCF.
               03  BALANCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BALANCI                     PIC X(016).
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  CLOSEDL                     PIC S9(4) COMP.
           02  CLOSEDF                     PIC X.
           02  FILLER REDEFINES CLOSEDF.
               03  CLOSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CLOSEDI                     PIC X(010).
           02  BALANC1L                    PIC S9(4) COMP.
           02  BALANC1F                    PIC X.
           02  FILLER REDEFINES BALANC1F.
               03  BALANC1A                PIC X.
           02  FILLER                      PIC X(02).
           02  BALANC1I                    PIC X(016).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
           02  APPRAIL                     PIC S9(4) COMP.
           02  APPRAIF                     PIC X.
           02  FILLER REDEFINES APPRAIF.
               03  APPRAIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  APPRAII                     PIC X(016).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  LIMITAL                     PIC S9(4) COMP.
           02  LIMITAF                     PIC X.
           02  FILLER REDEFINES LIMITAF.
               03  LIMITAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LIMITAI                     PIC X(016).
           02  OVERRIL                     PIC S9(4) COMP.
           02  OVERRIF                     PIC X.
           02  FILLER REDEFINES OVERRIF.
               03  OVERRIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OVERRII                     PIC X(001).
           02  PAYMENL                     PIC S9(4) COMP.
           02  PAYMENF                     PIC X.
           02  FILLER REDEFINES PAYMENF.
               03  PAYMENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMENI                     PIC X(007).
           02  MINPAYL                     PIC S9(4) COMP.
           02  MINPAYF                     PIC X.
           02  FILLER REDEFINES MINPAYF.
               03  MINPAYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MINPAYI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  HEADDMO REDEFINES HEADDMI.
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
           02  DRAWAMC                     PIC X.
           02  DRAWAMH                     PIC X.
           02  DRAWAMO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  BALANCC                     PIC X.
           02  BALANCH                     PIC X.
           02  BALANCO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  BALANC1C                    PIC X.
           02  BALANC1H                    PIC X.
           02  BALANC1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  APPRAIC                     PIC X.
           02  APPRAIH                     PIC X.
           02  APPRAIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  LIMITAC                     PIC X.
           02  LIMITAH                     PIC X.
           02  LIMITAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OVERRIC                     PIC X.
           02  OVERRIH                     PIC X.
           02  OVERRIO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PAYMENC                     PIC X.
           02  PAYMENH                     PIC X.
           02  PAYMENO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  MINPAYC                     PIC X.
           02  MINPAYH                     PIC X.
           02  MINPAYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
