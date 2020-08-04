# Ik kom er niet uit... 

# Stel, ik heb een tabel met 1 kolom: 

library(dplyr)
library(stringr)

df_schijf <- as_tibble(c("dit is een test", "de schijf van vijf bevat fruit", 
                         "de schijf van vijf bevat groente en fruite", 
                         "groenten en fruit zijn lekker", 
                         "fruit, fruit, druit" , "groente fruit", 
                         "groente", "bananen zijn ook lekker"))

colnames(df_schijf) <- c("tweet")

# Ik wil voor een zoekwoord, in dit geval "groente", een nieuwe dummy variabele maken in df_schijf. 
# De nieuwe dummy heeft de naam van het zoekwoord en de waarde is 0 
# als het niet voorkomt in de string en 1 als het wel voorkomt. Zo ongeveer: 

# df_schijf <- mutate(df_schijf, groente = str_detect(df_schijf$tweet, "\\bgroente\\b"))
# df_schijf$groente <- as.integer(df_schijf$groente)
# Niet heel fraai dat het in twee stappen moet, maar ala. 
df_schijf <- mutate(df_schijf, groente = as.integer(str_detect(df_schijf$tweet, "\\bgroente\\b")))


# Nu wil ik dat niet alleen voor het woord "groente", maar voor een heel 
# lijstje hastags of zoekwoorden. Graag zou ik iets doen als: 
li_woord <- c("groente", "fruit", "lekker")
for (i in seq_along(li_woord)) {
  woord <- li_woord[i]
  pattern <- paste0("\\b", woord, "\\b")
  df_schijf <- mutate(df_schijf, !!woord := as.integer(str_detect(df_schijf$tweet, pattern)))
}

## ref. https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr

# Nog afgezien van de vraag hoe ik die \\b's om het zoekwoord zet, 
# doet hij het sowieso niet. Kan het zijn dat je for niet kan gebruiken met 
# een lijstje woorden?  Dit lijkt niet de weg. Maar hoe dan wel? 
# Kan niet vinden hoe ik dit op moet lossen. Suggesties? 
