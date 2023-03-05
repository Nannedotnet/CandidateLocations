#to do

if("tmap" %in% rownames(installed.packages()) == FALSE) {install.packages("tmap")}
if("cbsodataR" %in% rownames(installed.packages()) == FALSE) {install.packages("cbsodataR")}
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}

library(tmap)
library(dplyr)
library(readODS)
library(cbsodataR)
library(stringr)

#Not used - DEFINITION TABLE
#City_Information<-cbs_get_data(id = "82271NED")
#City_Information_Meta<-cbs_get_meta(id = "82271NED")

#City_information_3<-cbs_get_data(id = "70072NED", perioden = c("2017JJ00")) # only select period 2017
#City_information_3Meta<-cbs_get_meta(id = "70072NED")

#Used - DEFINITION TABLE 

City_Information_2<-cbs_get_data(id = "84489NED")
City_Information_2Meta<-cbs_get_meta(id = "84489NED")
colnames(City_Information_2Meta$Woonplaatsen)[2]<-"Woonplaats"

#Easier joining on identical names #lazy
colnames(City_Information_2)[2]<-"Key"
# Remove leading/trailing white spaces
City_Information_2 <- City_Information_2 %>% mutate(across(where(is.character), str_trim))

#CpP<-read.csv("https://raw.githubusercontent.com/Nannedotnet/CandidateLocations/7ec1cd461fbf12b8eae427b4e91cd4d551183d7f/CityPerProvince2006.csv")
Can<-read.csv("https://raw.githubusercontent.com/Nannedotnet/CandidateLocations/main/osv_3-5_overzicht_kandidaatgegevens_2021.csv", sep=";", skip =3)

#Clean up - various places are not official municipalities. Nor could I find an exhaustive list to do automatic replacement,therefore this manual solution
Can[Can$Nr. == 8 & Can$Lijstnummer == 5,13] <- "Voorburg" 

#Results 2021 election
Results_Elec<-data.frame(
  Politieke.Groepering=c(
    "VVD",
    "PVV (Partij voor de Vrijheid)",
    "CDA",
    "D66",
    "SP (Socialistische Partij)",
    "GROENLINKS",
    "Partij van de Arbeid (P.v.d.A.)",
    "ChristenUnie",
    "Partij voor de Dieren",
    "50PLUS",
    "Staatkundig Gereformeerde Partij (SGP)",
    "DENK",
    "Forum voor Democratie",
    "Volt",
    "JA21",
    "BBB",
    "BIJ1"),
Zetels=c(
    34,
    17,
    15,
    24,
    9,
    8,
    9,
    5,
    6,
    1,
    3,
    3,
    8,
    3,
    3,
    1,
    1)
)
CanDetails<-unique(Can[,c(3,7,13)])
Results_ElecBackup <-Results_Elec
CanDetailsT <-tibble(CanDetails)

Func<-function(Results_ElecBackup,CanDetailsT, j)
{
  Results_Elec<-cbind(Results_ElecBackup[,1],lapply(Results_ElecBackup[,2], function(Zetels) ifelse(Zetels >j, j, Zetels)))
  Results_Elec <- data.frame(Results_Elec)
  colnames(Results_Elec) <- colnames(Results_ElecBackup)
  
  aa<-data.frame(matrix(ncol = 0, nrow = 0))
  
  
  for (i in unique(CanDetailsT$Politieke.Groepering)){
    PG <-Results_Elec[Results_Elec$Politieke.Groepering == i,2]
    if(length(PG) < 1) {PG <- 0}
    a<-(CanDetailsT %>% filter(Politieke.Groepering == i, Nr. <= PG))
    aa<-bind_rows(a,aa)
  }
  
  ab<-aa %>% left_join(City_Information_2Meta$Woonplaatsen)
  ac<-ab %>% left_join(City_Information_2)
  ad<- ac %>% count(Naam_4)
  colnames(ad)<-"name"
  data(NLD_prov) # load province data for NL alternatively used "NLD_muni" to get city data 
  
  ae<-ad %>% left_join(NLD_prov)
  colnames(ae)[2] <- "Kamerleden" 
  
  colnames(ad)[2] <- "Kamerleden" 
  af<- NLD_prov %>% left_join(ad)
  
  Tot.No.Leden <- sum(sapply    (Results_Elec[,2],sum))
  
  af$RatioPopKamerlid<- af$population / af$Kamerleden
  af$RatioKamerleden<- af$Kamerleden / Tot.No.Leden
  
  titlegraph1 <- paste ("Positie lijst <=", j, "totaal:", Tot.No.Leden)
  titlegraph2 <- paste ("Positie lijst <=", j, "totaal:", Tot.No.Leden)
  
  Map1<-  tm_shape(af) + tm_fill("RatioPopKamerlid", n = 10, stye = "equal") + tm_legend(legend.outside =TRUE, main.title = titlegraph1)
  
  
  RatioPopKamerlid<-(data.frame(af[,c("name", "RatioPopKamerlid")]))
  
  Map2<-  tm_shape(af) + tm_fill("RatioKamerleden", n = 10, stye = "equal") + tm_legend(legend.outside =TRUE, main.title = titlegraph2)
  Map3<-tmap_arrange(Map1,Map2)  
  tmap_save(Map3)
  
  RatioKamerleden<-(data.frame(af[,c("name", "RatioKamerleden")]))
  
  out <- list(RatioPopKamerlid, RatioKamerleden)
  return(out)
}

x<-max(Results_ElecBackup[,2])
x<-1:x
for(i in x) {
  Func(Results_ElecBackup, CanDetailsT, i)
}

##Filter municipalities without province##
data.frame(ab %>% filter (is.na(Key)))




#City_Information_2Meta$Woonplaatsen[str_detect(City_Information_2Meta$Woonplaatsen$Woonplaats, "Voorburg")==TRUE,]



--

# werkt nog niet 
for (i in unique(CanDetailsT$Politieke.Groepering)){
  
  --aa <--bind_rows(aa,CanDetailsT %>% filter(Politieke.Groepering == i, Nr. <= test[test$test == i,2]$V2))
  aa <--bind_rows(aa,CanDetailsT %>% filter(Politieke.Groepering == i, Nr. <= test[test$test == i,2]))
  
}


aa<-CanDetailsT %>% filter( Politieke.Groepering == i, Nr. <= test[test$test == i,2]$V2)

ab<-CanDetailsT %>% filter( Politieke.Groepering == i, Nr. <= test[test$test == i,2]$V2)

-- test is a data.frame list of parties and number of seats

Results<-data.frame(
  Politieke.Groepering=c(
    "VVD",
    "PVV (Partij voor de Vrijheid)",
    "CDA",
    "D66",
    "SP (Socialistische Partij)",
    "GROENLINKS",
    "Partij van de Arbeid (P.v.d.A.)",
    "ChristenUnie",
    "Partij voor de Dieren",
    "50PLUS",
    "Staatkundig Gereformeerde Partij (SGP)",
    "DENK",
    "Forum voor Democratie"),
  Zetels=c(
    33,
    20,
    19,
    19,
    14,
    14,
    9,
    5,
    5,
    4,
    3,
    3,
    2)
)

Benthuizen           NA      <NA>
  2  Staatkundig Gereformeerde Partij (SGP)   2             Elspeet           NA      <NA>
  3                   Partij voor de Dieren   4              Geleen           NA      <NA>
  4         Partij van de Arbeid (P.v.d.A.)   6          Muiderberg           NA      <NA>
  5         Partij van de Arbeid (P.v.d.A.)   8            Den Burg           NA      <NA>
  6         Partij van de Arbeid (P.v.d.A.)   9           Easterein           NA      <NA>
  7                              GROENLINKS   4        Monnickendam           NA      <NA>
  8                                     D66  12           Woubrugge           NA      <NA>
  9                                     D66  16          Teteringen           NA      <NA>
  10                                    D66  19                Neer           NA      <NA>
  11                                    CDA   4              Gemert           NA      <NA>
  12                                    CDA   5            Hegelsom           NA      <NA>
  13                                    CDA   7                Edam           NA      <NA>
  14                                    CDA  10 Nederhorst den Berg           NA      <NA>
  15                                    CDA  11         Voorthuizen           NA      <NA>
  16                                    CDA  14           Kockengen           NA      <NA>
  17                                    CDA  15           Panningen           NA      <NA>
  18                                    CDA  18            Bennekom           NA      <NA>
  19          PVV (Partij voor de Vrijheid)   6          Overasselt           NA      <NA>
  20          PVV (Partij voor de Vrijheid)  10            Rockanje           NA      <NA>
  21          PVV (Partij voor de Vrijheid)  16            Voorburg           NA      <NA>
  22          PVV (Partij voor de Vrijheid)  18          Hoevelaken           NA      <NA>
  23                                    VVD   6              Hoeven           NA      <NA>
  24                                    VVD  21      's-Gravenzande 

