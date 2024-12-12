library(dkstat) 
library(ggplot2)
POST <- dst_meta(table = "POSTNR1", lang = "da")
POST_filter <- list(
  PNR20="*",
  KØN="I alt",
  Tid="2024"
)

postlist <- dst_get_data(table = "POSTNR1", query = POST_filter, lang = "da")
postdata <- as.data.frame(postlist)

# Use strsplit to split the 'PNR20' column into two parts
split_postdata <- strsplit(as.character(postdata$PNR20), " ", fixed = TRUE)

# Create new columns for 'Postnummer' and 'By_omrade'
postdata$Postnummer <- sapply(split_postdata, function(x) x[1])
postdata$By_omrade <- sapply(split_postdata, function(x) paste(x[-1], collapse = " "))

# Remove any non-numeric characters from the 'Postnummer' column
postdata$PNR20 <- gsub("[^0-9]", "", postdata$PNR20)

postdata$PNR20 <- NULL

postdata$Postnummer <- as.factor(postdata$Postnummer)

# Check the resulting dataframe
head(postdata)

bolig <- read.csv("Documents/Dataanalyse/Projects/R/data2/boligsiden.csv")
bolig$by <- gsub("oe", "ø", bolig$by,ignore.case= TRUE)
bolig$by <- gsub("ae", "æ", bolig$by,ignore.case= TRUE)
bolig$by <- gsub("aa", "å", bolig$by,ignore.case= TRUE)

bolig[317, "pris"] <- 1695000


#gør alle bynavne til småt
bolig$by <- tolower(bolig$by)

merged_data <- merge(bolig, postdata, by.x = "postnr", by.y = "Postnummer", all.x = TRUE, all.y = TRUE)

# Remove rows where 'pris' is NA
merged_data <- merged_data[!is.na(merged_data$pris), ]

# Rename 'value' column to 'befolkning'
names(merged_data)[names(merged_data) == "value"] <- "befolkning"

# Check the column names to ensure the change
colnames(postdata)

merged_data <- merged_data[-(1:3),]

merged_data <- merged_data[,-c(4,5,8,10,11,13,14)]


merged_data$bystr <- with(merged_data,
                                  ifelse(befolkning <= 0, "Landet",
                                         ifelse(befolkning <= 250, "Landet",
                                                ifelse(befolkning > 250 & befolkning <= 1000, "Landsby",
                                                       ifelse(befolkning > 1000 & befolkning <= 2500, "lilleby",
                                                              ifelse(befolkning > 2500 & befolkning <= 10000, "Middelby",
                                                                     ifelse(befolkning > 10000 & befolkning <= 50000, "Mellemstorby", "Storby")))))))

#enfiredata <- merged_data[,c(1,2,6,13,14)] #Tager kun de fire kolonner som OLA beder om
#enfiredata <- na.omit(enfiredata)

merged_data$kvmpris <- NA
merged_data$størrelse <- as.numeric(merged_data$størrelse)

# Før jeg kan plotte med kvadratmeterpris, skal den omdannes fra character til numeric ved at fjerne "." og "kr"
merged_data$pris <- as.numeric(gsub("\\.|kr.", "", merged_data$pris)) # Vi bruger gsub til at fjerne punktum og "kr"

merged_data <- merged_data[merged_data$størrelse <= 500, ]
merged_data <- merged_data[merged_data$størrelse >= 50, ]


merged_data <- merged_data[merged_data$pris <= 30000000, ]

merged_data$kvmpris <- (merged_data$pris/merged_data$størrelse)

##################################
# Udarbejd nu grafen uden "Landet"
#DataMinusLand <- enfiredata
#DataMinusLand <- enfiredata[which(enfiredata$byStørrelse != "Landet"), ]

merged_data <- na.omit(merged_data)

merged_data$bystr <- factor(merged_data$bystr, 
                            levels = c("Landet", "Landsby", "lilleby", "Middelby", "Mellemstorby", "Storby"))

# Ændre priser for flere rækker
#bolig[317, "pris"] <- 1695000  # Ændrer prisen i række 764
#merged_data[1280, "pris"] <- 900000  # Ændrer prisen i række 1280
#merged_data[1531, "pris"] <- 3600000 # Ændrer prisen i række 1531

# Beregn gennemsnits kvadratmeterpris for hver unik byStørrelse
average_kvmpris <- tapply(merged_data$kvmpris, merged_data$bystr, mean, na.rm = TRUE)

average_kvmpris <- na.omit(average_kvmpris)

# Farver til hver byStørrelse
colors <- c("lightgreen", "lightcoral", "lightgoldenrod", "lightskyblue", "lightpink")

# Opret barplot med flere tilpasninger
bp <- barplot(average_kvmpris, 
              main = "M2 pris inddelt i by størrelse", 
              xlab = "", 
              ylab = "Gennemsnits Kvadratmeterpris", 
              col = colors,                 # Flere farver til barerne
              las = 2,                      # Rotér x-akse labels for lettere læsning
              ylim = c(0, max(average_kvmpris) * 1.2),  # Tilføj lidt ekstra plads over de højeste barer
              cex.names = 0.8)  

# Tilføj værdier oven på hver bar
text(bp, average_kvmpris, labels = round(average_kvmpris, 1), pos = 3, cex = 0.8)

# Tilføj grid-linjer for y-aksen
grid(nx = NA, ny = NULL, lty = "dotted", col = "gray")


#plot uden landet

# Beregn gennemsnits kvadratmeterpris for hver unik byStørrelse
average_kvmpris <- tapply(merged_data$kvmpris, merged_data$bystr, mean, na.rm = TRUE)

# Fjern 'Landet' fra average_kvmpris
average_kvmpris <- na.omit(average_kvmpris)
average_kvmpris <- average_kvmpris[names(average_kvmpris) != "Landet"]

# Farver til hver byStørrelse (juster længden af farver til de resterende kategorier)
colors <- c("lightcoral", "lightgoldenrod", "lightskyblue", "lightpink", "lightgreen")

# Opret barplot med flere tilpasninger
bp <- barplot(average_kvmpris, 
              main = "M2 pris inddelt i by størrelse", 
              xlab = "", 
              ylab = "Gennemsnits Kvadratmeterpris", 
              col = colors,                 # Flere farver til barerne
              las = 2,                      # Rotér x-akse labels for lettere læsning
              ylim = c(0, max(average_kvmpris) * 1.2),  # Tilføj lidt ekstra plads over de højeste barer
              cex.names = 0.8)  

# Tilføj værdier oven på hver bar
text(bp, average_kvmpris, labels = round(average_kvmpris, 1), pos = 3, cex = 0.8)

# Tilføj grid-linjer for y-aksen
grid(nx = NA, ny = NULL, lty = "dotted", col = "gray")





