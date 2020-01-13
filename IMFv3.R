# IMF global report, version 3

library(data.table)
library(stringi)
library(openxlsx)

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201910")
df = fread("df.new.matrix.csv")
df.matrix = fread("/home/sergiy/Documents/Work/Nutricia/Global/2019/df.verified.csv")
df.pharma = fread("/home/sergiy/Documents/Work/Nutricia/Global/2019/df.pharma.csv")
dictCompany.Brand = fread("/home/sergiy/Documents/Work/Nutricia/Global/2019/dictCompany-Brand.csv")

df.pharma[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]
df.matrix = rbindlist(list(df.matrix, df.pharma), 
                      use.names = TRUE,
                      fill = TRUE)

df = df[Ynb == 2019 & (PS0 == "IMF" | PS0 == "AMN")]
df = df[!(PS0 == "IMF" & Form == "Liquid")]


df[, SKU2 := stri_trim_both(stri_replace_all_regex(SKU, "\\s+", " "))]

df = df[Channel == "MT"]

# YM
df[, YM := paste0(Ynb, stri_pad_left(Mnb, 2, pad = 0))]

# Channel
df[Channel == "PHARMA", Channel := "PH"]

# GL CATEGORY
# df[PS0 == "IMF", GL.CATEGORY := "ELN"]
# df[PS0 == "AMN", GL.CATEGORY := "AMN"]
df[, GL.CATEGORY := "ELN"]

# GL COREvs PAEDIATRICS SPECIALTIES
df[(PS3 != "Specials" | PS == "Goat"), CORE.VS.SPECIALITIES := "CORE"]
df[(PS3 == "Specials" | PS0  == "AMN") &
           PS != "Goat", 
   CORE.VS.SPECIALITIES := "PAEDIATRIC SPECIALITIES"]


# GL VARIANTS
df[df.matrix, on = "SKU2", 
   `:=`(GL.VARIANTS = stri_trans_toupper(i.PSV),
        GL.IF.ORGANIC = stri_trans_toupper(i.Organic),
        GL.IF.PROTEIN = stri_trans_toupper(i.Protein),
        GL.CONSUMER.SPECIALS = stri_trans_toupper(i.CSS),
        GL.FLAVOUR = stri_trans_toupper(i.Flavoured),
        GL.STORAGE = stri_trans_toupper(i.Storage)
   )]

# GL SUB SEGMENTS
df[, GL.SUB.SEGMENTS := toupper(PS)]
df[PS == "Goat" & PS2 %in% c("IF", "FO"), GL.SUB.SEGMENTS := PS2]
df[PS == "Goat" & PS2 == "Gum", GL.SUB.SEGMENTS := "GUM 1-3"]
df[PS2 == "Gum" & PS3 != "Specials", GL.SUB.SEGMENTS := "GUM 1-3"]


df[PS == "BIF" | PS == "BFO" |
     PS == "BPIF" | PS == "BPFO", GL.SUB.SEGMENTS := PS2]

df[PS == "Anti Reflux", GL.SUB.SEGMENTS := "ANTIREFLUX"]
df[PS == "DR-NL", GL.SUB.SEGMENTS := "REDUCED LACTOSE / LACTOSE FREE"]
df[PS == "Hypoallergenic", GL.SUB.SEGMENTS := "ALLERGY PREVENTION"]
df[PS0 == "AMN", GL.SUB.SEGMENTS := "CHALLANGED GROWTH"]
df[PS == "Metabolics", GL.SUB.SEGMENTS := "METABOLICS"]
df[PS == "Epilepsie", GL.SUB.SEGMENTS := "EPILEPSIE"]

# Soy SKUs
df[GL.VARIANTS == "AT Soy", `:=`(GL.SUB.SEGMENTS = "ALLERGY TREATMENT",
                                 GL.VARIANTS = "SOY")]

df[GL.VARIANTS == "DC Soy", `:=`(GL.SUB.SEGMENTS = "DIGESTIVE COMFORT",
                                 GL.VARIANTS = "SOY")]



# GL SEGMENTS
df[PS2 == "IF" | PS2 == "FO", GL.SEGMENTS := "IFFO"]
df[PS2 == "Gum", GL.SEGMENTS := "GUM"]
df[GL.SUB.SEGMENTS == "CHALLANGED GROWTH" | PS == "Preterm", GL.SEGMENTS := "GROWTH"]
df[PS == "Anti Reflux" | 
           PS == "DR-NL" | 
           PS == "Digestive Comfort" |
           PS == "Self Remedy" |
           GL.SUB.SEGMENTS == "DIGESTIVE COMFORT",
   GL.SEGMENTS := "GI"]

df[PS == "Hypoallergenic" | 
           PS == "Allergy Treatment" |
           GL.SUB.SEGMENTS == "ALLERGY TREATMENT", 
   GL.SEGMENTS := "ALLERGY"]

# GL MANUFACTURER, GL BRAND, GL SUB BRAND

df[dictCompany.Brand, on = "Company", GL.MANUFACTURER := i.GLOBAL.MANUFACTURER]
df[GL.MANUFACTURER == "" | is.na(GL.MANUFACTURER), GL.MANUFACTURER := "ALL_OTH"]
df[, GL.MANUFACTURER := stri_trans_toupper(GL.MANUFACTURER)]

df[dictCompany.Brand, on = "Brand", GL.BRAND := i.GLOBAL.BRAND]
df[GL.BRAND == "" | is.na(GL.BRAND), GL.BRAND := "ALL_OTH"]
df[, GL.BRAND := stri_trans_toupper(GL.BRAND)]

df[, GL.SUB.BRAND := stri_trans_toupper(SubBrand)]
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
df[, GL.MULTIPACK := stri_extract_first_regex(SKU2, "[0-9]{1,2}(X|\\*)([0-9]+)")]
df[, GL.MULTIPACK := as.numeric(stri_extract_first_regex(GL.MULTIPACK, "[0-9]+"))]
df[GL.MULTIPACK > 10, GL.MULTIPACK := 1]
df[GL.MULTIPACK == "" | is.na(GL.MULTIPACK), GL.MULTIPACK := 1]
df[, GL.MULTIPACK := paste0(GL.MULTIPACK, "X")]

# Price Segment
df[, GL.PRICE.SEGMENT := GlobalPriceSegment]

# Collect all together
df = df[, .(YM, Channel, GL.CATEGORY,
            CORE.VS.SPECIALITIES, GL.SEGMENTS, GL.SUB.SEGMENTS, GL.VARIANTS,
            GL.MANUFACTURER, GL.BRAND, GL.SUB.BRAND,
            GL.PEADS.SPEC.AGE.SEGMENTS,	GL.IF.ORGANIC, GL.IF.PROTEIN,
            GL.POWDER.VS.LIQUID, GL.CONSUMER.SPECIALS, 
            GL.PACKSIZE, GL.PACKTYPE, GL.MULTIPACK,
            GL.PRICE.SEGMENT, GL.FLAVOUR, GL.STORAGE,
            Value = sum(Value)/1000, Volume = sum(Volume)
)]

names(df) = c("YM", "Channel", "GL CATEGORY",
              "GL COREvs PAEDIATRICS SPECIALTIES",
              "GL SEGMENTS", "GL SUB SEGMENTS",
              "GL VARIANTS", "GL MANUFACTURER",
              "GL BRAND", "GL SUB BRAND", "GL PEADS SPEC AGE SEGMENTS",
              "GL IF ORGANIC", "GL IF PROTEIN", "GL POWDER VS LIQUID",
              "GL CONSUMER SPECIALS", "GL PACKSIZE", "GL PACKTYPE",
              "GL MULTIPACK", "GL PRICE SEGMENT", "GL FLAVOUR", "GL STORAGE",
              "Value 000", "Volume")
write.xlsx(df, "/home/sergiy/Documents/Work/Nutricia/Global/2019/test3.xlsx")
