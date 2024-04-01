
# Datum:  1 april 2024
# Auteur: Lidwien
# Titel:  Met dit script maak je plaatjes voor 2 variabelen tegelijk mogelijk.

# Laden van benodigde pakketten
library(dplyr)
library(haven)
library(ggplot2)
library(readxl)
library(tidyverse)
library(qpdf)
library(extrafont)
library(rlang)
library(scales)

# Set working directory
# setwd(
#   "~/Library/CloudStorage/OneDrive-SharedLibraries-EM+MACommunicatie/Kennis COVID - P 2. Internationaal vergelijkend onderzoek/ExtraAnalyses/Data/R"
# )

# Pad naar de data
# file_path <-
#   paste0(getwd(), "data/covid/10. maatregelen+indexen+covidgegevens+algemenegegevens+YouGovvragenlijst 10 europese landen januari 2020 tm januari 2023 nieuw compleet.sav")

dir_data <- paste0(getwd(), "/data/covid")
dir_out <- paste0(dir_data, "/pictures")
dir.create(dir_out, showWarnings = FALSE)
df_files <- list.files(dir_data, full.names = TRUE)
fpath_data <- df_files[10]
fpath_xls <- df_files[9]

# Volgorde waarin de landen in de facet worden geplaatst.
volgordeLanden <- c("Noorwegen", "Zweden", "Finland", "Denemarken",
                    "Groot Brittannië", "Nederland", "Duitsland", 
                    "Frankrijk", "Spanje", "Italië")

# Laad de data
# Vervang de Engelse namen van de landen voor Nederlandse
data_centraal <- haven::read_sav(fpath_data) %>%
  mutate(
    location2 = case_when(
      location == "Denmark" ~ "Denemarken",
      location == "Finland" ~ "Finland",
      location == "France" ~ "Frankrijk",
      location == "Germany" ~ "Duitsland",
      location == "Italy" ~ "Italië",
      location == "Netherlands" ~ "Nederland",
      location == "Norway" ~ "Noorwegen",
      location == "Spain" ~ "Spanje",
      location == "Sweden" ~ "Zweden",
      location == "United Kingdom" ~ "Groot Brittannië",
      TRUE ~ location
    )
  ) %>%
  rename(Land = location2) %>%
  mutate(Land = factor(Land, levels = volgordeLanden)) # Bepaal de volgorde

# Maak een datum voor de maanden en de kwartalen
data_centraal <- data_centraal %>%
  mutate(
    Jaar     = substr(jaarmaand, 1, 4),
    Maand    = substr(jaarmaand, 5, 6),
    Kwartaal = as.numeric(substr(kwartjaar, 5, 6))
  ) %>%
  mutate(DatumMaand    = as.Date(paste(Jaar, Maand, "01", sep = "-")),
         DatumKwartaal = as.Date(paste(Jaar, Kwartaal * 3 - 2, "01", sep = "-")))

# VOOR TWEE VARIABELEN ----

# Lees de excel met de combinaties die je wil plotten
df_variabelenEnLabels2 <- read_excel(fpath_xls,
                                     sheet = "TweeVariabelen")

# Maak eerst het aantal plotjes wat kleiner (om te testen):
df_variabelenEnLabels2 <- df_variabelenEnLabels2 %>%
  filter(Nr %in% c(12, 14, 15, 11))

# Ga door alle rijen van df_variabelenEnLabels en maak de plaatjes:

i <- 3
for (i in 1:nrow(df_variabelenEnLabels2)) {
  # Haal alle benodigde informatie op.
  titel     <- df_variabelenEnLabels2$Titel[i]
  
  variable1 <- df_variabelenEnLabels2$Variabele_1[i]
  axislabel1 <- df_variabelenEnLabels2$YasLabel_1[i]
  legenda1  <- df_variabelenEnLabels2$LegendaLabel_1[i]
  
  variable2 <- df_variabelenEnLabels2$Variabele_2[i]
  axislabel2 <- df_variabelenEnLabels2$YasLabel_2[i]
  legenda2  <- df_variabelenEnLabels2$LegendaLabel_2[i]
  
  toelichting <- paste0("\n", str_wrap(trimws(df_variabelenEnLabels2$Toelichting_1[i])),
                        "\n", str_wrap(trimws(df_variabelenEnLabels2$Toelichting_2[i])),
                        "\n\nBron: LerenVanDeCoronacrisis.nl\n")
  
  # Aggregeer de data voor een specifieke variabele
  # Bereken het maandgemiddelde
  df_result2 <- data_centraal %>%
    group_by(DatumMaand, Land) %>%
    summarise(Gem1 = mean(get(variable1), na.rm = TRUE),
              Gem2 = mean(get(variable2), na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(DatumMaand))
  
  # Bereken de conversionfactor.
  # Bepaal de maximale waarde
  max_Gem1 <- max(df_result2$Gem1, na.rm = TRUE)
  max_Gem2 <- max(df_result2$Gem2, na.rm = TRUE)
  
  # Bepaal de verhouding tussen Gem1 en Gem2
  verhoudingGem1en2 <- max_Gem1 / max_Gem2
  
  print(paste0("De verhouding tussen Gem1 en Gem2 is: ", verhoudingGem1en2))
  
  # Converteer Gem2 naar de schaal van Gem1
  df_result2$Gem2_scaled <- df_result2$Gem2 * verhoudingGem1en2

  df_result3 <- df_result2 %>% 
    select(-Gem2) %>% 
    pivot_longer(c(Gem1, Gem2_scaled), names_to = "var", values_to = "score")
    # mutate(var = case_when(str_detect(var, "scaled") ~ axislabel2,
    #                        TRUE ~ axislabel1))
    
  # even
  g2 <- ggplot(df_result3, aes(x = DatumMaand, group = Land)) + 
    geom_line(aes(y = score, colour = var, group = var)) + 
    # geom_line(aes(y = Gem1, colour = axislabel1)) + 
    # scale_color_manual("", values = rgb(237/255, 165/255, 6/255)) +
    facet_wrap(
      ~ Land,
      ncol = 4,
      nrow = 3,
      scales = "free_x",
      strip.position = "top"
    ) +
    scale_color_manual(
      values = c("#007bc7", "#ca005d"),
    # Eerste waarde blauw, tweede waarde roze
      name = "Legenda",
      labels = str_wrap(c(legenda1, legenda2), width = 20)) +
    labs(
      title = titel,
      caption = str_wrap(toelichting, width = 100),
      x = "",
      y = paste0("\n", axislabel1, "\n")
      ) +
      scale_y_continuous(sec.axis = sec_axis(~. / verhoudingGem1en2, 
                                             name = axislabel2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "#535353", hjust = 0),
      plot.caption.position = "plot",
      plot.caption = element_text(
        hjust = 1,
        color = "#535353",
        size = 8
      ),
      strip.placement = "inside",
      strip.background = element_blank(),
      strip.text = element_text(size = 10, color = "#535353"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_line(color = "#cccccc"),
      axis.text.x = element_text(color = "#535353", size = 8),
      
      axis.text.y        = element_text(color = "#007bc7", size = 8),
      # Primary y-as (links) wordt blauw
      axis.title.y       = element_text(color = "#007bc7", size = 10),
      axis.text.y.right  = element_text(color = "#ca005d", size = 8),
      # Secundaire y-as (rechts) wordt roze
      axis.title.y.right = element_text(color = "#ca005d", size = 10),
      panel.spacing = unit(2, "lines"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    theme(
      text = element_text(family = "RijksoverheidSansText"),
      legend.key = element_rect(color = NA, fill = NA),
      legend.key.size = unit(1, "cm")
    )
      

  print(g2d)
  
  # Verwijder alle rommeltjes
  # rm(titel, variable1, axislabel1, legenda1,
  #    variable2, axislabel2, legenda2,
  #    toelichting, max_Gem1, max_Gem2, verhoudingGem1en2,
  #    g2, g2a, g2b, g2c, g2d)
  
  # Bewaar het plaatje
  
  # Maak een bestandsnaam
  fpath_out <-paste0(dir_out, "/", "TweeVars_", variable1, "_", variable2, ".png")
  
  # Open de PNG device
  png(file = fpath_out, width = 3400, height = 1856, units = "px", res = 300)
  
  # Print de plot
  print(g2d)
  
  # Sluit de PNG device
  dev.off()
  
}
