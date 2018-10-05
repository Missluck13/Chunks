install.packages('xlsx', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(sf)
library(readr)
library(ggplot2)
library(maptools)
library(dplyr)
library(data.table)
library(tidyverse)
library(viridis)

#Import of Geodata from UK disctricts - all
Eng_location <- "Test4_shapefile_data/Local_Authority_Districts_May_2018_UK_BSC.shp"
Eng <- st_read(Eng_location)

#Map display of all(most) UK districts
gg2 <- ggplot(Eng) +
  geom_sf()
print(gg2)
colnames(Eng)

#Selection of the five districts forming the "Tees Valley" region
EngTees <- filter(Eng,
             lad18cd %in% c("E06000002", "E06000001", "E06000003", "E06000004", "E06000005"))

#Map display of the Tees Valley
ggTees <- ggplot(EngTees) +
  geom_sf()
print(ggTees)


#Import of 5 individual Data Files with the population of alone or not living in five Teesside regions  
PopTeesH <- read_csv("raw_data/TeesH.csv")
PopTeesS <- read_csv("raw_data/TeesS.csv")
PopTeesM <- read_csv("raw_data/TeesM.csv")
PopTeesR <- read_csv("raw_data/TeesR.csv")
PopTeesD <- read_csv("raw_data/TeesD.csv")

#Relabeling the Variable Names - preparing to merge data sets
names(PopTeesH) <- paste(names(PopTeesH), sep="_", "H")
names(PopTeesS) <- paste(names(PopTeesS), sep="_", "S")
names(PopTeesM) <- paste(names(PopTeesM), sep="_", "M")
names(PopTeesR) <- paste(names(PopTeesR), sep="_", "R")
names(PopTeesD) <- paste(names(PopTeesD), sep="_", "D")

PopTeesD <- edit(PopTeesD)
PopTeesH <- edit(PopTeesH)
PopTeesM <- edit(PopTeesM)
PopTeesR <- edit(PopTeesR)
PopTeesS <- edit(PopTeesS)

#Merging the five data sets
PopTees2 <- merge(PopTeesD, PopTeesH, by="Age_Group")
PopTees2 <- merge(PopTees2, PopTeesM, by="Age_Group")
PopTees2 <- merge(PopTees2, PopTeesR, by="Age_Group")
PopTees2 <- merge(PopTees2, PopTeesS, by="Age_Group")

View(PopTees2)

#Tidying up the data set
PopTees3 <- PopTees2[-c(1, 2, 17), ] 

#Summarizing of row data in excel
library(xlsx)
write.xlsx(PopTees3, file = "Tees3.xlsx", col.names = TRUE, row.names = FALSE)
PopTees4 <- read.xlsx("Tees3.xlsx", sheetName = 1)

#Reducing Rows
PopTees5 <- PopTees4[-c(1:11), ]
colnames(PopTees5)[1] <- "Age_Group"
colnames(PopTees5)

#Selecting Columns and calculating percentage how many more or less women exist in the age groups for each place 
PopTees6 <- select(PopTees5, -starts_with("ALL")) 
PopTees6 <- PopTees6[, c(1,4,7,10,13,16,19,22,25,28,31)]
colnames(PopTees6)
colnames(PopTees6)[2] <- "Male_Single_D"
colnames(PopTees6)[3] <- "Female_Single_D"
colnames(PopTees6)[4] <- "Male_Single_H"
colnames(PopTees6)[5] <- "Female_Single_H"
colnames(PopTees6)[6] <- "Male_Single_M"
colnames(PopTees6)[7] <- "Female_Single_M"
colnames(PopTees6)[8] <- "Male_Single_R"
colnames(PopTees6)[9] <- "Female_Single_R"
colnames(PopTees6)[10] <- "Male_Single_S"
colnames(PopTees6)[11] <- "Female_Single_S"
PopTees6 <- mutate(PopTees6,
        Darlington=round(Female_Single_D/Male_Single_D*100-100, digits=1))
PopTees6 <- mutate(PopTees6,
         Hartlepool=round(Female_Single_H/Male_Single_H*100-100, digits=1)) 
PopTees6 <- mutate(PopTees6,
         Middlesbrough=round(Female_Single_M/Male_Single_M*100-100, digits=1))
PopTees6 <- mutate(PopTees6,
         Redcar_Cleveland=round(Female_Single_R/Male_Single_R*100-100, digits=1))   
PopTees6 <- mutate(PopTees6,
         Stockton=round(Female_Single_S/Male_Single_S*100-100, digits=1)) 

PopTees7 <- PopTees6[, c(1, 12:16)]



# get data
as.data.frame("PopTees7")
data("PopTees7")

# transpose
PopTees_T <- transpose(PopTees7)
PopT <- PopTees_T

# get row and colnames in order
colnames(PopT) <- rownames(PopTees7)
rownames(PopT) <- colnames(PopTees7)
colnames(PopT) = PopTees_T[1, ]
PopT=PopT[-1,]
PopT[] <- lapply(PopT, function(x) as.numeric(as.character(x)))
PopT$lad18nm <- c("Darlington", "Hartlepool", "Middlesbrough", "Redcar and Cleveland", "Stockton-on-Tees")
PopT <- PopT[, -6]

#Merge the datasets! Geo and data. 
TeesSingle <- left_join(EngTees, PopT, by="lad18nm")

#Map!
ggplot(TeesSingle) +
  geom_sf(aes(fill=`20-24`)) +
  scale_fill_distiller(palette = "Blues", direction=1, name="% of Women less than men")+
  labs(title="Share of Single-Living Women compared to Men in the Tees Valley Region: Age Group 20-24", caption="Source: https://data.gov.uk")
  
ggplot(TeesSingle) +
  geom_sf(aes(fill=r kjn`25-39`)) +
  scale_fill_distiller(palette = "Oranges", direction=1, name="% of Women more than men")+
  labs(title="Share of Single-Living Women compared to Men in the Tees Valley Region: Age Group 25-39", caption="Source: https://data.gov.uk")

ggplot(TeesSingle) +
  geom_sf(aes(fill=`40-49`)) +
  scale_fill_distiller(palette = "Oranges", direction=1, name="% of Women more than men")+
  labs(title="Share of Single-Living Women compared to Men in the Tees Valley Region: Age Group 40-49", caption="Source: https://data.gov.uk")

ggplot(TeesSingle) +
  geom_sf(aes(fill=`50-64`)) +
  scale_fill_distiller(palette = "Oranges", direction=1, name="% of Women more than men")+
  labs(title="Share of Single-Living Women compared to Men in the Tees Valley Region: Age Group 50-64", caption="Source: https://data.gov.uk")

ggplot(TeesSingle) +
  geom_sf(aes(fill=`65+`)) +
  scale_fill_distiller(palette = "Reds", direction=1, name="% of Women more than men")+
  labs(title="Share of Single-Living Women compared to Men in the Tees Valley Region: Age Group 65+", caption="Source: https://data.gov.uk")



#Trial with facet_wrap...
ggplot(TeesSingle, aes(fill=`65+`)) +
  facet_wrap(~lad18nm) +
  geom_sf() +
  coord_sf(crs=)+
  scale_fill_viridis(direction=-1) +
  scale_color_viridis(direction=-1) +
  theme_void() +
  theme(panel.grid.major = element_line(colour="transparent")) +
  scale_fill_distiller(palette = "Reds", direction=1, name="Women") +
  labs(title="Share of Women living single in comparison to men in the Tees Valley region", caption="Source: https://data.gov.uk")
  
