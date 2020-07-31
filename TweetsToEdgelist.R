
# ------------------------------
# INLEZEN LIBRARIES E.D.
# ------------------------------

## setwd("~/Documents/R/TwitterAnalysis/TwitterAnalysis")

# Dit script gebruikt een Coosto Export (als .csv file) als uitgangspunt. 

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# ------------------------------
# INLEZEN VAN DE DATA
# ------------------------------

# Dit script gebruikt een Coosto Export (als .csv file) als uitgangspunt. 
# Let op separator. Is hiet ;. Op windows kan dat zomaar : zijn. 

# Coosto_messages <- read_delim("data/Coosto_messages.csv", ";", 
#                               col_types = cols(date = col_character(), 
#                                                `discussion length` = col_integer(), 
#                                                 views = col_integer(), 
#                                                 followers = col_integer(), 
#                                                 influence = col_number(), 
#                                                `GPS latitude` = col_character(), 
#                                                 title = col_character()), trim_ws = TRUE)

df_cm_raw <- as_tibble(read.csv2("data/Coosto_messages.csv", stringsAsFactors = FALSE))
df_cm_raw$influence <- as.numeric(df_cm_raw$influence)
df_cm_raw$date <- as.Date(df_cm_raw$date, "%Y-%m-%d $H:%M")

# Datum en een aantal andere variabelen worden nog niet 
# goed ingelezen maar zijn voor het script ook niet echt nodig. 

# ------------------------------
# LEZEN VAN DE DATUM
# ------------------------------

# 2020-07-01 23:48
str(Coosto_messages@date)
Coosto_messages@date <- sapply(Coosto_messages@date, ymd_hm)

# ??? 

# Maken van actief.

# ------------------------------
# MAKEN VAN SOURCE
# ------------------------------

# Maak variabe source (deze vervangt de oorspronkelijke source, maar die doet toch niks)
Coosto_messages$source = Coosto_messages$author

# Vervang hoofdletters door kleine letters. 
Coosto_messages$source <- tolower(Coosto_messages$source)

# ------------------------------
# MAKEN VAN TARGET 
# ------------------------------

# Opschonen van de data

# De volgende stap maakt een nieuwe variabele 'tweet' op basis van 'message text' die dient als basis voor je analyse. 

Coosto_messages$tweet = Coosto_messages$`message text` 

# Vervang hoofdletters door kleine letters. 
Coosto_messages$tweet <- tolower(Coosto_messages$tweet)

# Functie die alle punctuation behalve @ weghaalt
# Wat geberut hier? 

removeAllPunctuationButMention<-
    function (x, preserve_intra_word_dashes = FALSE) 
    {
        rmpunct <- function(x) {
            x <- gsub("@", "\002", x)
            x <- gsub("[[:punct:]]+", "", x)
            gsub("\002", "@", x, fixed = TRUE)
        }
        if (preserve_intra_word_dashes) { 
            x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
            x <- rmpunct(x)
            gsub("\001", "-", x, fixed = TRUE)
        } else {
            rmpunct(x)
        }
    }

# Toepassen van de functie. 

Coosto_messages$tweet <- removeAllPunctuationButMention(Coosto_messages$tweet)

# Extract @Mentions 

Coosto_messages$Mention <- str_extract_all(Coosto_messages$tweet, "@\\w+")
Coosto_messages$Mention <- sapply(Coosto_messages$Mention, 
                                  function(x) paste(x, collapse=", "))

Coosto_messages$Mention <- str_replace_all(Coosto_messages$Mention, "@", "")        



# ------------------------------
# MAKEN VAN HASHTAGS
# ------------------------------

# Zelfde als @Mentions maar dan per kolom
# Welke hashtags zijn er 
# Twintig meest voorkomende 
# Twintig kolommen 

# ------------------------------
# MAKEN VAN ZOEKWOORDEN
# ------------------------------

# Zelfde als @Mentions maar dan per kolom
# Twintig zoekwoorden. 

# ------------------------------
# MAKEN VAN EDGELIST
# ------------------------------

# Nieuwe tabel met Source en Target 
# Meenemen van gegevens 

# ------------------------------
# MAKEN VAN NODELIST
# ------------------------------

# Agregreren van nodes, gemiddelde aantal volgers, sum aantal berichten. 


