# IMF global report, version 3

library(data.table)
library(stringi)
library(openxlsx)

df = fread("/home/sergiy/Documents/Work/Nutricia/Data/pivot2.csv")
dictCompany.Brand = fread("/home/sergiy/Documents/Work/Nutricia/Global/2019/dictCompany-Brand.csv")

df = df[order(Ynb, Mnb)]
df[, Index := .GRP, by = .(Ynb, Mnb)]
df = df[Index > (max(Index) - 36)]
df[, Index := NULL]

df = df[(PS0 == "IMF" | PS0 == "AMN")]
df = df[!(PS0 == "IMF" & Form == "Liquid")]

# YM
df[, YM := paste0(Ynb, stri_pad_left(Mnb, 2, pad = 0))]

# Channel
df[Channel == "PHARMA", Channel := "PH"]
df[, Channel := "UA"]  #??

# GL CATEGORY
df[, GL.CATEGORY := "ELN"]

# GL COREvs PAEDIATRICS SPECIALTIES
df[(PS3 != "Specials" | PS == "Goat"), CORE.VS.SPECIALITIES := "CORE"]
df[((PS3 == "Specials" & PS != "Goat") | PS0  == "AMN"), 
   CORE.VS.SPECIALITIES := "PAEDIATRIC SPECIALITIES"]

# GL VARIANTS
df[, 
   `:=`(GL.VARIANTS = stri_trans_toupper(PSV),
        GL.IF.ORGANIC = stri_trans_toupper(Organic),
        GL.IF.PROTEIN = stri_trans_toupper(Protein),
        GL.CONSUMER.SPECIALS = stri_trans_toupper(CSS),
        GL.FLAVOUR = stri_trans_toupper(Flavoured),
        GL.STORAGE = stri_trans_toupper(Storage)
   )]

# GL SUB SEGMENTS
df[, GL.SUB.SEGMENTS := toupper(PS)]
df[PS == "Goat" & PS2 %in% c("IF", "FO"), GL.SUB.SEGMENTS := PS2]
df[PS == "Goat" & PS2 == "Gum", GL.SUB.SEGMENTS := "GUM 1-3"]
df[PS2 == "Gum" & PS3 != "Specials", GL.SUB.SEGMENTS := "GUM 1-3"]

df[PS == "BIF" | PS == "BFO" |
           PS == "BPIF" | PS == "BPFO", GL.SUB.SEGMENTS := PS2]

df[PS == "Anti Reflux", GL.SUB.SEGMENTS := "ANTIREFLUX"]
df[PS == "DR-NL", GL.SUB.SEGMENTS := "DIGESTIVE COMFORT"]
df[PS == "Hypoallergenic", GL.SUB.SEGMENTS := "ALLERGY PREVENTION"]
df[PS0 == "AMN", GL.SUB.SEGMENTS := "CHALLENGED GROWTH"]
df[PS == "Metabolics", GL.SUB.SEGMENTS := "METABOLICS"]
df[PS == "Epilepsie", GL.SUB.SEGMENTS := "EPILEPSIE"]

# Soy SKUs
df[GL.VARIANTS == "AT SOY", `:=`(GL.SUB.SEGMENTS = "ALLERGY TREATMENT",
                                 GL.VARIANTS = "SOY")]

df[GL.VARIANTS == "DC SOY", `:=`(GL.SUB.SEGMENTS = "DIGESTIVE COMFORT",
                                 GL.VARIANTS = "SOY")]

# GL SEGMENTS
df[GL.SUB.SEGMENTS == "IF" | GL.SUB.SEGMENTS == "FO", 
   GL.SEGMENTS := "IFFO"]
df[GL.SUB.SEGMENTS == "GUM 1-3", GL.SEGMENTS := "GUM"]
df[GL.SUB.SEGMENTS == "PRETERM" | GL.SUB.SEGMENTS == "CHALLENGED GROWTH", 
   GL.SEGMENTS := "GROWTH"]
df[GL.SUB.SEGMENTS == "ALLERGY TREATMENT" | GL.SUB.SEGMENTS == "ALLERGY PREVENTION", 
   GL.SEGMENTS := "ALLERGY"]
df[GL.SUB.SEGMENTS == "ANTIREFLUX" | GL.SUB.SEGMENTS == "DIGESTIVE COMFORT", 
   GL.SEGMENTS := "GI"]
df[GL.SUB.SEGMENTS == "EPILEPSY" | GL.SUB.SEGMENTS == "METABOLICS", 
   GL.SEGMENTS := "METABOLICS"]


# GL MANUFACTURER, GL BRAND, GL SUB BRAND

df[dictCompany.Brand, on = "Company", GL.MANUFACTURER := i.GLOBAL.MANUFACTURER]
df[GL.MANUFACTURER == "" | is.na(GL.MANUFACTURER), GL.MANUFACTURER := "ALL_OTH"]
df[, GL.MANUFACTURER := stri_trans_toupper(GL.MANUFACTURER)]

df[dictCompany.Brand, on = "Brand", GL.BRAND := i.GLOBAL.BRAND]
df[GL.BRAND == "" | is.na(GL.BRAND), GL.BRAND := "ALL_OTH"]
df[, GL.BRAND := stri_trans_toupper(GL.BRAND)]

df[, GL.SUB.BRAND := stri_trans_toupper(SubBrand)]
df[GL.MANUFACTURER == "ALL_OTH", GL.BRAND := "ALL_OTH"]
df[GL.BRAND == "ALL_OTH", GL.SUB.BRAND := "ALL_OTH"]

# GL PEADS SPEC AGE SEGMENTS
df[PS2 == "IF" | PS2 == "FO", GL.PEADS.SPEC.AGE.SEGMENTS := "INFANT"]
df[!(PS2 == "IF" | PS2 == "FO"), GL.PEADS.SPEC.AGE.SEGMENTS := "UP AGE"]

# GL POWDER VS LIQUID
df[, GL.POWDER.VS.LIQUID := stri_trans_toupper(Form)]

# GL PACKSIZE
df[, GL.PACKSIZE := stri_trans_toupper(Size)]

# GL PACKTYPE
df[Package == "Foiled Can", GL.PACKTYPE := "TUB"]
df[Package == "Plastic", GL.PACKTYPE := "BOTTLE"]
df[Package == "Carton", GL.PACKTYPE := "BOX"]
df[GL.PACKTYPE == "" | is.na(GL.PACKTYPE), GL.PACKTYPE := "ALL OTHERS"]

# GL MULTIPACK
df[, GL.MULTIPACK := paste0(Items.in.pack, "X")]

# Price Segment
df[, GL.PRICE.SEGMENT := GlobalPriceSegment]
# df[GL.BRAND == "ALL_OTH", GL.PRICE.SEGMENT := "NOT CLASSIFIED"]

# Collect all together


df = df[, .(Value = sum(ValueC)/1000, Volume = sum(VolumeC)), 
        by = .(YM, Channel, GL.CATEGORY,
               CORE.VS.SPECIALITIES, GL.SEGMENTS, GL.SUB.SEGMENTS, GL.VARIANTS,
               GL.MANUFACTURER, GL.BRAND, GL.SUB.BRAND,
               GL.PEADS.SPEC.AGE.SEGMENTS,	GL.IF.ORGANIC, GL.IF.PROTEIN,
               GL.POWDER.VS.LIQUID, GL.CONSUMER.SPECIALS, 
               GL.PACKSIZE, 
               GL.PACKTYPE, 
               GL.MULTIPACK,
               GL.PRICE.SEGMENT, GL.FLAVOUR, GL.STORAGE,
               Brand, SubBrand, Size,
               Age, Scent, 
               Company,
               PS0, PS2, PS3, PS,
               Form, 
               PriceSegment,
               GlobalPriceSegment
        )]

#### CAREFULL!!!! VALUE and VALUE-C

df.c = df[, .(Value = sum(Value), Volume = sum(Volume)), 
          by = .(YM, Channel, GL.CATEGORY,
                 CORE.VS.SPECIALITIES, GL.SEGMENTS, GL.SUB.SEGMENTS, GL.VARIANTS,
                 GL.MANUFACTURER, GL.BRAND, GL.SUB.BRAND,
                 GL.PEADS.SPEC.AGE.SEGMENTS, GL.IF.ORGANIC, GL.IF.PROTEIN,
                 GL.POWDER.VS.LIQUID, GL.CONSUMER.SPECIALS, 
                 GL.PACKSIZE, GL.PACKTYPE, GL.MULTIPACK,
                 GL.PRICE.SEGMENT, GL.FLAVOUR, GL.STORAGE)]

names(df.c) = c("YM", "Channel", "GL CATEGORY",
                "GL COREvs PAEDIATRICS SPECIALTIES",
                "GL SEGMENTS", "GL SUB SEGMENTS",
                "GL VARIANTS", "GL MANUFACTURER",
                "GL BRAND", "GL SUB BRAND", "GL PEADS SPEC AGE SEGMENTS",
                "GL IF ORGANIC", "GL IF PROTEIN", "GL POWDER VS LIQUID",
                "GL CONSUMER SPECIALS", "GL PACKSIZE", "GL PACKTYPE",
                "GL MULTIPACK", "GL PRICE SEGMENT", "GL FLAVOUR", "GL STORAGE",
                "Value 000", "Volume")
write.xlsx(df, "/home/sergiy/Documents/Work/Nutricia/Global/2019/test4.xlsx")
write.xlsx(df.c, "/home/sergiy/Documents/Work/Nutricia/Global/2019/test3 excl L.xlsx")
