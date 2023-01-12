W_D_data_prep_V2 <- function(){
  Data<- read.csv("2021_QLD_SA2_V2.csv")
  Data$X.1 = NULL # Removing a column.
  
  ## Shapefiles:
  QLD_SA2_SHP <- read_sf("sQLD.shp")
  
  mydata_and_myMap<- inner_join(QLD_SA2_SHP,
                                Data,
                                by = c("SA2_NAME16" = "SA2_NAME16"))
  
  
  ### Converting variables:
  set.seed(123)
  mydata_and_myMap$Remoteness<- as.factor(mydata_and_myMap$Remoteness)
  mapvalues(mydata_and_myMap$Remoteness, from = c("1", "2","3","4","5"), to = c("major cities",  "Inner regional","Outer regional","remote","very remote area"))
  
  mydata_and_myMap$IRSD<- as.factor(mydata_and_myMap$IRSD)
  mapvalues(mydata_and_myMap$IRSD, from = c("1", "2","3","4","5","6","7","8","9","10"), to = c( "Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10"))
  
  mydata_and_myMap<-  na.omit(mydata_and_myMap)
  
  ### Scaling Health Vulnerabilities and Covariates:
  ScalePhysical<- log(mydata_and_myMap$Physical+1)
  ScaleSocial<- log(mydata_and_myMap$Social+1)
  ScaleEmotional<- log(mydata_and_myMap$Emotional+1)
  ScaleLanguage<- log(mydata_and_myMap$Language+1)
  ScaleCommunication<- log(mydata_and_myMap$Communication+1)
  ScalePreschool<- log(mydata_and_myMap$Preschool+1)
  ScaleIndigenous<- log(mydata_and_myMap$Indigenous+1)
  ScaleAustralia<- log(mydata_and_myMap$Australia+1)
  ScaleEnglish<- log(mydata_and_myMap$English+1)
  ScaleVuln.1<- log(mydata_and_myMap$Vuln.1+1)
  ScaleVuln.2<- log(mydata_and_myMap$Vuln.2+1)
  
  ### Creating Data Frame:
  DF1  <- as.data.frame(cbind( ScalePhysical, ScaleSocial, ScaleEmotional,ScaleLanguage, ScaleCommunication, ScaleVuln.1,ScaleVuln.2,ScalePreschool,ScaleIndigenous,
                               ScaleAustralia,ScaleEnglish))
  
  DF <- cbind(mydata_and_myMap,DF1)
  DF[,c(2,4:16)] <- NULL
  return(DF)
}




