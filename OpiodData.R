


## grab the two datasets 

transfer <- read.csv("C:\\Users\\Pat\\Downloads\\us_zip_codes_to_census_tracts.csv")
    ##just alabama zip codes
transfer <- subset(transfer, transfer$ZIP >= 35004 & transfer$ZIP <= 36925)
View(transfer)


library(readxl)
acs.pre <- read_xlsx("C:\\Users\\Pat\\Documents\\ACS.xlsx")
acs <- subset(acs.pre, acs.pre$STATE == 'Alabama')
View(acs)


## restandardize fron census tracts to zipcodes

    #append a zipcode column

acs$Zip <- rep(0, length(acs$TRACTFIPS))
acs$TRACTFIPS <- as.numeric(acs$TRACTFIPS)
transfer$TRACT <- as.numeric(transfer$TRACT)

for (i in 1:length(acs$TRACTFIPS)){
  for (j in 1:length(transfer$ZIP)){
    if (acs$TRACTFIPS[i] == transfer$TRACT[j]) {acs$Zip[i] <- transfer$ZIP[j]}
  }
}

for (i in 1:length(acs$Zip)){
  if (acs$Zip[i] == 0){
    acs$Zip[i] <- acs$Zip[i-1] 
  }
}

acs <- subset(acs, acs$ACS_TOT_POP_WT !=0)

acs.safe <- acs
View(acs.safe)
length(unique(acs$Zip))

acs <- acs.safe
acs <- na.omit(acs)
acs <- subset(acs, select = -c(STATE,COUNTY,TRACTFIPS))
new.acs <- acs[1:475,]

for (i in 1:length(new.acs)){
  for (j in 1:length(new.acs$Zip)){
    new.acs[j,i] <-  0 
  }
}
new.acs$Zip <- unique(acs$Zip)
View(new.acs)




  ## merge all the observations from a zipcode together

for (i in 1:length(acs$Zip)) {
  for (j in 1:length(new.acs$Zip)){
    if (acs$Zip[i] == new.acs$Zip[j]){
      nullpop <- new.acs$ACS_TOT_POP_WT[j]
      new.acs$ACS_TOT_POP_WT[j] <- new.acs$ACS_TOT_POP_WT[j] + acs$ACS_TOT_POP_WT[i]
      for (b in 2:33){
        new.acs[j,b] <- ((new.acs[j,b]*(nullpop/new.acs$ACS_TOT_POP_WT[j])) + (acs[i,b]*(acs$ACS_TOT_POP_WT[i]/new.acs$ACS_TOT_POP_WT[j])))
      }
    }
  }
}


new.acs.safe <- new.acs

new.acs <- new.acs.safe



## Import pharmacy data, assign ID to every pharmacy

pharmacies <- read_xlsx("C:\\Users\\Pat\\Downloads\\pharmacies.xlsx")
pharmacies <- pharmacies [,2:17]
View(pharmacies)

pharmacies$PSZ <- rep(0,343)
for (i in 1:length(pharmacies$Zipcode)){
  pharmacies$PSZ[i] <- i
}



## Loop to assign each zip code and compiled demographic data to the geographically closest pharmacy


library(zipcodeR)
new.acs$PSZ <- rep(0,475)

for (i in 1:475){
  min_dist <- 1000
  current_zip <- new.acs$Zip[i]
  for (b in 1:343){
    current_pharmacy <- pharmacies$Zipcode[b]
    dist.table <- zip_distance(current_zip, current_pharmacy)
    current_dist <- dist.table$distance
    
    if ((current_dist <= min_dist)== 'TRUE') {
      new.acs$PSZ[i] <- pharmacies$PSZ[b]
      min_dist <- current_dist
    }
  }
  min_dist <-1000
 print(i) 
}  
 
write.xlsx(new.acs, "C:\\Users\\Pat\\Documents\\WITHPSZ.xlsx")


final <- new.acs[1:343,]
final[1:343,] <- 0
View(final)



## Load in PSZ's and their respective pharmacy zipcodes 



for (i in 1:length(final$Zip)) {
  final$Zip[i] <- pharmacies$Zipcode[i]
  final$PSZ[i] <- pharmacies$PSZ[i]
}

final <- final %>% relocate(PSZ)
new.acs <- new.acs %>% relocate(PSZ)

final$ACS_TOT_POP_WT = as.integer(final$ACS_TOT_POP_WT)
new.acs$ACS_TOT_POP_WT = as.integer(new.acs$ACS_TOT_POP_WT)
final$PSZ = as.integer(final$PSZ)
new.acs$PSZ = as.integer(new.acs$PSZ)

final <- as.data.frame(final)
new.acs <- as.data.frame(new.acs)





## Aggregate data of each pharmacy-serviced zone 



length(unique(new.acs$PSZ))


for (i in 1:475) {
  nullpop <- final[new.acs$PSZ[i], 'ACS_TOT_POP_WT']
  final[new.acs$PSZ[i], 'ACS_TOT_POP_WT'] = final[new.acs$PSZ[i], 'ACS_TOT_POP_WT'] + new.acs$ACS_TOT_POP_WT[i]
  for (b in 3:35) {
    final[new.acs$PSZ[i], b] <- ((final[new.acs$PSZ[i], b]*(nullpop/final[new.acs$PSZ[i], 'ACS_TOT_POP_WT'])) + (new.acs[i, b]*(new.acs$ACS_TOT_POP_WT[i]/final[new.acs$PSZ[i], 'ACS_TOT_POP_WT'])))
  }
}

safe.final <- final


Pharmacies2 <- pharmacies[, -17]
View(pharmacies)
Pharmacies2 <- pharmacies2[, -1]
final <- cbind(final, Pharmacies2 )
View(final)




## Standardize drug counts by population of Zone 


for (i in 1:343){
  for (b in 37:51){
    final[i,b] <- (final[i,b]/final$ACS_TOT_POP_WT[i])
  }
}

final$totalDoses <- final[,37] + final[,38] + final[,39] + final[,40] + final[,41] + final[,42] + final[,43] + final[,44] + final[,45] + final[,46] + final[,47] + final[,48] + final[,49] + final[,50]


final$PctAddiction <- (final[,43] + final[,38])
final$PctPain <- (final$totalDoses - final$PctAddiction)
final$PctAddiction <- final$PctAddiction/final$totalDoses
final$PctPain <- final$PctPain/final$totalDoses
  
library(xlsx)
write.xlsx(final, "C:\\Users\\Pat\\Documents\\final(6.0).xlsx")  







  
  
  
  




