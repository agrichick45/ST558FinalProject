mergedData<-read_csv("mergedData.csv")
mergedData[is.na(mergedData)]<-0
#Convert to Factors
mergedData$NCHS_URCS_2013<-as.factor(mergedData$NCHS_URCS_2013)
mergedData$InitialCluster<-as.factor(mergedData$InitialCluster)
mergedData$Megacluster<-as.factor(mergedData$Megacluster)

agIntenSlope<-read.csv("agIntenSlope.csv")
agIntenSlope$GEOID<-agIntenSlope$ï..GEOID
agIntenSlope$ï..GEOID<-NULL
agIntenSlope[is.na(agIntenSlope)]<-0


saveRDS(agIntenSlope, "./data/agIntenSlope.rds")
saveRDS(mergedData, "./data/mergedData.rds")


