      *================================================================*
      * COPYBOOK : CMPOSIS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP FOR MAPSET CMPOSIS MAP CMPOSIM     *
      *            TRANSACTION CB50 - SEE BMS SOURCE CMPOSIS           *
      *================================================================*
       01  CMPOSIMI.
           02  FILLER                      PIC X(12).
           02  TITLEL                      COMP PIC S9(4).
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    COMP PIC S9(4).
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  CURDATEI                    PIC X(010).
           02  CUSTIDL                     COMP PIC S9(4).
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  CUSTIDI                     PIC X(010).
           02  CUSTNML                     COMP PIC S9(4).
           02  CUSTNMF                     PIC X.
           02  FILLER REDEFINES CUSTNMF.
               03  CUSTNMA                 PIC X.
           02  CUSTNMI                     PIC X(030).
           02  SEGMTL                      COMP PIC S9(4).
           02  SEGMTF                      PIC X.
           02  FILLER REDEFINES SEGMTF.
               03  SEGMTA                  PIC X.
           02  SEGMTI                      PIC X(002).
           02  RELIDL                      COMP PIC S9(4).
           02  RELIDF                      PIC X.
           02  FILLER REDEFINES RELIDF.
               03  RELIDA                  PIC X.
           02  RELIDI                      PIC X(010).
           02  BUSDTL                      COMP PIC S9(4).
           02  BUSDTF                      PIC X.
           02  FILLER REDEFINES BUSDTF.
               03  BUSDTA                  PIC X.
           02  BUSDTI                      PIC X(010).
           02  ACLN01L                     COMP PIC S9(4).
           02  ACLN01F                     PIC X.
           02  FILLER REDEFINES ACLN01F.
               03  ACLN01A                 PIC X.
           02  ACLN01I                     PIC X(079).
           02  ACLN02L                     COMP PIC S9(4).
           02  ACLN02F                     PIC X.
           02  FILLER REDEFINES ACLN02F.
               03  ACLN02A                 PIC X.
           02  ACLN02I                     PIC X(079).
           02  ACLN03L                     COMP PIC S9(4).
           02  ACLN03F                     PIC X.
           02  FILLER REDEFINES ACLN03F.
               03  ACLN03A                 PIC X.
           02  ACLN03I                     PIC X(079).
           02  ACLN04L                     COMP PIC S9(4).
           02  ACLN04F                     PIC X.
           02  FILLER REDEFINES ACLN04F.
               03  ACLN04A                 PIC X.
           02  ACLN04I                     PIC X(079).
           02  ACLN05L                     COMP PIC S9(4).
           02  ACLN05F                     PIC X.
           02  FILLER REDEFINES ACLN05F.
               03  ACLN05A                 PIC X.
           02  ACLN05I                     PIC X(079).
           02  ACLN06L                     COMP PIC S9(4).
           02  ACLN06F                     PIC X.
           02  FILLER REDEFINES ACLN06F.
               03  ACLN06A                 PIC X.
           02  ACLN06I                     PIC X(079).
           02  ACLN07L                     COMP PIC S9(4).
           02  ACLN07F                     PIC X.
           02  FILLER REDEFINES ACLN07F.
               03  ACLN07A                 PIC X.
           02  ACLN07I                     PIC X(079).
           02  ACLN08L                     COMP PIC S9(4).
           02  ACLN08F                     PIC X.
           02  FILLER REDEFINES ACLN08F.
               03  ACLN08A                 PIC X.
           02  ACLN08I                     PIC X(079).
           02  ACLN09L                     COMP PIC S9(4).
           02  ACLN09F                     PIC X.
           02  FILLER REDEFINES ACLN09F.
               03  ACLN09A                 PIC X.
           02  ACLN09I                     PIC X(079).
           02  ACLN10L                     COMP PIC S9(4).
           02  ACLN10F                     PIC X.
           02  FILLER REDEFINES ACLN10F.
               03  ACLN10A                 PIC X.
           02  ACLN10I                     PIC X(079).
           02  TOTLEDL                     COMP PIC S9(4).
           02  TOTLEDF                     PIC X.
           02  FILLER REDEFINES TOTLEDF.
               03  TOTLEDA                 PIC X.
           02  TOTLEDI                     PIC X(017).
           02  TOTAVLL                     COMP PIC S9(4).
           02  TOTAVLF                     PIC X.
           02  FILLER REDEFINES TOTAVLF.
               03  TOTAVLA                 PIC X.
           02  TOTAVLI                     PIC X(017).
           02  TOTSWPL                     COMP PIC S9(4).
           02  TOTSWPF                     PIC X.
           02  FILLER REDEFINES TOTSWPF.
               03  TOTSWPA                 PIC X.
           02  TOTSWPI                     PIC X(017).
           02  POOLIDL                     COMP PIC S9(4).
           02  POOLIDF                     PIC X.
           02  FILLER REDEFINES POOLIDF.
               03  POOLIDA                 PIC X.
           02  POOLIDI                     PIC X(008).
           02  POOLNETL                    COMP PIC S9(4).
           02  POOLNETF                    PIC X.
           02  FILLER REDEFINES POOLNETF.
               03  POOLNETA                PIC X.
           02  POOLNETI                    PIC X(017).
           02  PLRATEL                     COMP PIC S9(4).
           02  PLRATEF                     PIC X.
           02  FILLER REDEFINES PLRATEF.
               03  PLRATEA                 PIC X.
           02  PLRATEI                     PIC X(010).
           02  PLINTL                      COMP PIC S9(4).
           02  PLINTF                      PIC X.
           02  FILLER REDEFINES PLINTF.
               03  PLINTA                  PIC X.
           02  PLINTI                      PIC X(017).
           02  PAGENOL                     COMP PIC S9(4).
           02  PAGENOF                     PIC X.
           02  FILLER REDEFINES PAGENOF.
               03  PAGENOA                 PIC X.
           02  PAGENOI                     PIC X(003).
           02  MSGL                        COMP PIC S9(4).
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  MSGI                        PIC X(079).
       01  CMPOSIMO REDEFINES CMPOSIMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CUSTNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  SEGMTO                      PIC X(002).
           02  FILLER                      PIC X(03).
           02  RELIDO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  BUSDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  ACLN01O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN02O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN03O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN04O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN05O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN06O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN07O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN08O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN09O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  ACLN10O                     PIC X(079).
           02  FILLER                      PIC X(03).
           02  TOTLEDO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  TOTAVLO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  TOTSWPO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  POOLIDO                     PIC X(008).
           02  FILLER                      PIC X(03).
           02  POOLNETO                    PIC X(017).
           02  FILLER                      PIC X(03).
           02  PLRATEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  PLINTO                      PIC X(017).
           02  FILLER                      PIC X(03).
           02  PAGENOO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  MSGO                        PIC X(079).
