###################################################################################################################################
##################################################                               ##################################################
##################################################      OOO   PPPP   GGG   1     ##################################################
##################################################     O   O  P   P G   G  1     ##################################################
##################################################     O   O  PPPP  G      1     ##################################################
##################################################     O   O  P     G  GG  1     ##################################################
##################################################      OOO   P      GGG   1     ##################################################
##################################################                               ##################################################
###################################################################################################################################
###################################################################################################################################
#                       Opgave 5.1 – Kvartalsvis årlig realvækst for en række Eurolande                                           #
#                                                                                                                                 #
#                       Beregn den kvartalsvise årlige realvækst for husholdningernes forbrugsudgift for Danmark,                 #
#                       Belgien, Holland, Sverige, Østrig, Tyskland, Frankrig, Italien og Spanien i perioden 1. kvartal 2000      #
#                       til og med 2. kvartal 2023. I skal hente data vha. API’et fra Eurostat.                                   #
###################################################################################################################################

library(eurostat)
eurodat <- get_eurostat("namq_10_fcs", time_format = "raw") #Kilde: https://ec.europa.eu/eurostat/databrowser/view/namq_10_fcs/default/table?lang=en

#definer lande vi vil kigge op og start/slut tidspunkt
lande_koder <- c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES")
start_period <- "1999-Q1"
end_period <- "2024-Q4"

#Filtrer for lande, variabler og tid
filtered_data <- subset(eurodat, 
                          s_adj == "SCA" & #Seasonally and calendar adjusted data
                          unit == "CLV20_MEUR" & #kædede værdier, 2020priser, million euro
                          na_item == "P31_S14" & #Final consumption expenditure of households
                          geo %in% lande_koder &
                          TIME_PERIOD >= start_period & TIME_PERIOD <= end_period)

#master dataframen bliver skabt, og indeholder fra fødslen af kun TIME_PERIOD
master_df <- data.frame(TIME_PERIOD = unique(filtered_data$TIME_PERIOD))

# Nu laver vi en funktion til at fylde master_df ud
add_country_data <- function(df, country_code) {
# Filtrering af data for et bestemt land
  country_data <- subset(df, geo == country_code, select = c("TIME_PERIOD", "values"))
# Omdøbning af kolonnen values:
  colnames(country_data)[2] <- country_code
# Sammenfletning af data med master_df
  merge(master_df, country_data, by = "TIME_PERIOD", all = TRUE)
}

# Loop gennem landende i landekoder, og fyld dem op med den rette data
for (land in lande_koder) {
  master_df <- add_country_data(filtered_data, land)
}

# Funktion til at beregne kvartalsvis realvækst ved brug af log-differensmetoden
calculate_growth_log <- function(country_col) {
  country_col <- as.numeric(country_col)
  # Beregner forskel med en lag på 4 kvartaler
  growth <- (exp(diff(log(country_col), lag = 4)) - 1) * 100
  # Fyld første 4 rows ud med NA - grundet manglende sammenligningsværdier
  growth <- c(rep(NA, 4), growth)
  return(growth)
}

# Loop igennem landene i lande_koder og tilføj nye kolonner for realvækst baseret på logaritmisk differens
for (land in lande_koder) {
  # giv nye kolonner navne baseret på originalen + "_growth"
  growth_col_name <- paste0(land, "_growth")
  master_df[[growth_col_name]] <- calculate_growth_log(master_df[[land]])
}

# tjek de færste 6 rækker, for at se om det virker
head(master_df)


# Fjern første fire rækker, NAværdier
mega_master_df <- master_df[5:nrow(master_df), ]

DK_master <- mega_master_df[,c(1,2,11)]

###################################################################################################################################
##################################################                               ##################################################
##################################################      OOO   PPPP   GGG   222   ##################################################
##################################################     O   O  P   P G   G    2   ##################################################
##################################################     O   O  PPPP  G       2    ##################################################
##################################################     O   O  P     G  GG  2     ##################################################
##################################################      OOO   P      GGG   222   ##################################################
##################################################                               ##################################################
###################################################################################################################################
###################################################################################################################################
#                                 Opgave 5.2 – Højeste kvartalsvise årlige realvækst                                              #
#                                                                                                                                 #
#                                 Hvilket af de landene har gennemsnitligt haft den højeste kvartalsvise årlige realvækst i       #
#                                 husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2023.                 #
###################################################################################################################################

library(ggplot2)

# Beregn gennemsnittet af vækstraterne for hvert land
# Grep søger efter mønstre i tekst: "_growth"
avg_growth <- colMeans(mega_master_df[, grep("_growth", colnames(mega_master_df))], na.rm = TRUE) 

# Vis resultaterne som en dataframe
avg_growth_df <- as.data.frame(avg_growth)

# Konverter rownames til en kolonne for ggplot
avg_growth_df$Land <- rownames(avg_growth_df)

avg_growth_df$Land <- gsub("_growth", "", avg_growth_df$Land)

# Lav en ggplot barplot
ggplot(avg_growth_df, aes(x = Land, y = avg_growth)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Skaber barplot med lightblue farve
  labs(title = "Svergie er i fronten for størst gennemsnitlige vækst fra 2000 til 2024", 
       x = "Land", 
       y = "Gennemsnitlig vækst (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roterer x-akse etiketterne for bedre læsbarhed



###################################################################################################################################
##################################################                               ##################################################
##################################################      OOO   PPPP   GGG   333   ##################################################
##################################################     O   O  P   P G   G    3   ##################################################
##################################################     O   O  PPPP  G        33  ##################################################
##################################################     O   O  P     G  GG     3  ##################################################
##################################################      OOO   P      GGG   333   ##################################################
##################################################                               ##################################################
###################################################################################################################################
###################################################################################################################################
#                         Opgave 5.3 – Coronakrisen som outlier                                                                   #
#                                                                                                                                 #
#                         Fjerne Coronakrisen fra jeres data og find igen den gennemsnitligt kvartalsvise realvækst i             #
#                         husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 3. kvartal 2024. I hvilket af landene    #
#                         har Coronakrisen haft den største effekt på den gennemsnitligt kvartalsvise realvækst.                   #
###################################################################################################################################

#covid periode
#11/3/2020 til 5/5/2023 kilde: https://en.wikipedia.org/wiki/COVID-19_pandemic
rownames(mega_master_df) <- NULL

minuscovidmaster<- mega_master_df[-c(81:90),]

minuscovid_growth <- as.data.frame(colMeans(minuscovidmaster[, grep("_growth",
                colnames(minuscovidmaster))], na.rm = TRUE))


# covid påvirkning af realvæksten for vores lande

realvækst_uden_covid <- data.frame(
  Land = rownames(avg_growth_df), 
  avg_vækst = avg_growth,     # Første kolonne med værdierne fra hele perioden
  avg_vækst_u_covid = minuscovid_growth,  # Anden kolonne med værdierne fra perioden uden Covid
  forskel = minuscovid_growth - avg_growth  # Tredje kolonne som er Diff
)

colnames(realvækst_uden_covid)[2] <- "realvækst for hele perioden"
colnames(realvækst_uden_covid)[3] <- "realvækst uden corona"
colnames(realvækst_uden_covid)[4] <- "Diff"

ggplot(realvækst_uden_covid, aes(x = Land, y = Diff)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Skaber barplot med lightblue farve
  labs(title = "Coronaperioden ramte spaniens realvækste hårdest", 
       x = "Land", 
       y = "Differencen på hele perioden med og uden Covid") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roterer x-akse etiketterne for bedre læsbarhed

#Spanien

###################################################################################################################################
##################################################                               ##################################################
##################################################      OOO   PPPP   GGG   4  4  ##################################################
##################################################     O   O  P   P G   G  4  4  ##################################################
##################################################     O   O  PPPP  G      4444  ##################################################
##################################################     O   O  P     G  GG     4  ##################################################
##################################################      OOO   P      GGG     4   ##################################################
##################################################                               ##################################################
###################################################################################################################################
###################################################################################################################################
#                           Opgave 5.4 – Effekt af Corona på forbruget                                                            #
#                                                                                                                                 #
#                           I hvilket europæiske land faldt den gennemsnitligt kvartalsvise realvækst i husholdningernes          #
#                           forbrugsudgift, i perioden 1. kvartal 2020 til 3. kvartal 2024, mest?                                 #
###################################################################################################################################

mest_covid <- mega_master_df[81:nrow(mega_master_df),]

covid_effekt <- as.data.frame(colMeans(mest_covid[, grep("_growth", colnames(mest_covid))], na.rm = TRUE))
colnames(covid_effekt)[1] <- "Difference af realvækst"
mest_covid_Q1_2020_Q2_2024 <- as.data.frame(t(mest_covid[c(1,19),c(11:19)]))
colnames(mest_covid_Q1_2020_Q2_2024) <- c("TYVEVYVEQ1", "TYVEFIRETYVEQ3")
mest_covid_Q1_2020_Q2_2024$differencen <- mest_covid_Q1_2020_Q2_2024$TYVEFIRETYVEQ3-mest_covid_Q1_2020_Q2_2024$TYVEVYVEQ1
mest_covid_Q1_2020_Q2_2024$land <- rownames(mest_covid_Q1_2020_Q2_2024)
mest_covid_Q1_2020_Q2_2024$land <- gsub("_growth", "", mest_covid_Q1_2020_Q2_2024$land)

ggplot(mest_covid_Q1_2020_Q2_2024, aes(x = land, y = differencen)) +
  geom_bar(stat = "identity", fill = "lightblue") +  # Skaber barplot med lightblue farve
  labs(title = "Svergie har haft sværest ved rette op på deres realvækst efter Covid", 
       x = "Land", 
       y = "Differencen på 2020Q1 og 2024Q3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roterer x-akse etiketterne for bedre læsbarhed



#a= italien
#p= spalien
#T= spa
#l =svergie

