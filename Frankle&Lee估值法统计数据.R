#get the files derived from last programme which named V_Data
library(Hmisc)
library(gdata) #for rename.vars
#OutputDirectory<<-"C:/Users/User/VData/" 
#name output path
#MarketDataDirectory<<-"C:/Users/User/FlCsvData/"
#name market data files path

for (curYear in 2014:2019) 
{
  curDataDF<-read.csv(paste(OutputDirectory,"VData_",curYear,".csv",sep=""))
  curColNames<-c("Code","FY1_T1","BV_T0","BV_TM1","fcROEt_T1","k","BHat","FY2_T1","fcROEt1_T2","BHatT1","FY3_T0","numerator","fcROEt2_T3","PVRI1","PVRI2_V1","V1","PVRI2_V2","V2","PVRI2_V3","PVRI3_V3","V3","Name","MV_T1","MVtoV1","MVtoV2","MVtoV3","V1Decile","V2Decile","V3Decile")
  names(curDataDF)<-curColNames
  curDataDF$MVtoBV<-curDataDF$MV_T1/curDataDF$BV_T0
  curDataDF$BVtoMV<-curDataDF$BV_T0/curDataDF$MV_T1
  if (curYear==2014)
  {
    VDataDF<-curDataDF 
  }else
  {
    head(VDataDF)
    head(curDataDF)
    VDataDF<-rbind(curDataDF)
  }
  print(curYear)
  print(dim(VDataDF))
}


 
CleanDataDF<-VDataDF 
dim(CleanDataDF) 
CleanDataDF<-CleanDataDF[!is.na(CleanDataDF$BV_T0),] 
CleanDataDF<-CleanDataDF[!is.na(CleanDataDF$k),] 
CleanDataDF<-CleanDataDF[!is.na(CleanDataDF$FY2_T1),] 
CleanDataDF<-CleanDataDF[!is.na(CleanDataDF$FY3_T0),] 
dim(CleanDataDF) 

CleanDataDF<-CleanDataDF[!is.na(CleanDataDF$MV_T1),] 
#Firms with negative book values
CleanDataDF<-CleanDataDF[CleanDataDF$BV_T0>0,] 
#ROE or FROE > 100%
CleanDataDF<-CleanDataDF[CleanDataDF$fcROEt_T1<1,] 
CleanDataDF<-CleanDataDF[CleanDataDF$fcROEt_T1>0,] 
#Stock price < $1 at value date
CleanDataDF<-CleanDataDF[CleanDataDF$MV_T1>1,]
# price was not collected, instead we use MV.

InitialVDataDF<-VDataDF
VDataDF<-CleanDataDF 
dim(InitialVDataDF) 
dim(VDataDF) 
# we have a routine here for removing outliers at +/- 3sd
RemoveOutliers<-function(curDF,ItemName,RemoveNAFlag=TRUE,RemoveZFlag=TRUE)
{
  curDF<-rename.vars(curDF,ItemName,"ItemName",info=FALSE)
  curMean<-mean(curDF$ItemName,na.rm=TRUE)
  curSD<-sd(curDF$ItemName,na.rm=TRUE)
  curDF$Z<-(curDF$ItemName-curMean)/curSD
  curDF$Z<-ifelse(curDF$Z>3,NA,curDF$Z)
  curDF$Z<-ifelse(curDF$Z<(-3),NA,curDF$Z)
  if (RemoveNAFlag)
  {
    curDF<-curDF[!is.na(curDF$Z),]
  }
  if(RemoveZFlag)
  {
    curDF<-subset(curDF,select=-c(Z))
  }
  curDF<-rename.vars(curDF,"ItemName",ItemName,info=FALSE)
  return(curDF)
}

GetTable6Stats<-function(VDataDF,Item)
{
  VDataDF<-rename.vars(VDataDF,Item,"Item",info=FALSE) 
  VDataDF<-VDataDF
  NValue<-length(VDataDF$Item)
  ItemMean<-mean(VDataDF$Item,na.rm=TRUE) 
  ItemSD<-sd(VDataDF$Item,na.rm=TRUE) 
  Qtiles<-quantile(VDataDF$Item,probs = (0:5)/5,na.rm=TRUE)
  VDataDF<-rename.vars(VDataDF,"Item",Item,info=FALSE)
  print(paste("The Item is: ",Item, sep = " "))
  print(paste("NValue is: ",NValue, sep = " "))
  print(paste("Mean is: ",ItemMean, sep = " "))
  print(paste("SD is: ",ItemSD, sep = " "))
  print(paste("Quartiles are: ",Qtiles, sep = " "))
  ReturnDF<-data.frame(Item,NValue,ItemMean,ItemSD,t(Qtiles[1:5]))
  names(ReturnDF)<-c("Variable","N","Mean","SD","Min","Q1","Media","Q3","Max")
  
}

GetTable6Stats(VDataDF,"MV_T1")
GetTable6Stats(VDataDF,"fcROEt_T1") # ROE
GetTable6Stats(VDataDF,"MVtoBV") # P/B 
GetTable6Stats(VDataDF,"BVtoMV") # B/P
GetTable6Stats(VDataDF,"BV_T0") # Book Value


#PriceDataFile<-"C:/Users/User/Output/Date/YYYY/MM/YYYYMM.csv"
#point to date files
curMonth<-"05" # assumes a fixed rebalance every May 

for (curYear in 2014:2019) 
{
  print(paste("Doing for Year ", curYear, sep=""))
  nextYear<-curYear+1
  curPDF<-read.csv(gsub("YYYY",curYear,gsub("MM",curMonth,PriceDataFile)))
  curPDF<-curPDF[,1:3] 
  colnames(curPDF)<-c("Code","CompanyName","curPrice") 
  nextPDF<-read.csv(gsub("YYYY",nextYear,gsub("MM",curMonth,PriceDataFile)))
  nextPDF<-nextPDF[,c(1,3)]
  colnames(nextPDF)<-c("Code","nextPrice") 
  RtnDF<-merge(curPDF,nextPDF,by.x="Code",by.y="Code") 
  if (curYear<=2017)
  {
    TwoYearOut<-curYear+2
    TwoYrDF<-read.csv(gsub("YYYY",TwoYearOut,gsub("MM",curMonth,PriceDataFile)))
    TwoYrDF<-TwoYrDF[,c(1,3)]
    colnames(TwoYrDF)<-c("Code","TwoYrPrice")
    RtnDF<-merge(RtnDF,TwoYrDF,by.x="Code",by.y="Code")
  }else
  {
    RtnDF$TwoYrPrice<-NA
  }
  if (curYear<=2016)
  {
    ThreeYearOut<-curYear+3
    ThreeYrDF<-read.csv(gsub("YYYY",ThreeYearOut,gsub("MM",curMonth,PriceDataFile)))
    ThreeYrDF<-ThreeYrDF[,c(1,3)]
    colnames(ThreeYrDF)<-c("Code","ThreeYrPrice")
    RtnDF<-merge(RtnDF,ThreeYrDF,by.x="Code",by.y="Code")
  }else
  {
    RtnDF$ThreeYrPrice<-NA
  }
  
  if (curYear==2014) 
  {
    FwdRtnDF<-RtnDF
  }else
  {
    FwdRtnDF<-rbind(FwdRtnDF,RtnDF)
  }
}
FwdRtnDF$OneYearRtn<-(((FwdRtnDF$nextPrice-FwdRtnDF$curPrice)/FwdRtnDF$curPrice)) 
FwdRtnDF$TwoYearRtn<-(((FwdRtnDF$TwoYrPrice-FwdRtnDF$curPrice)/FwdRtnDF$curPrice)) 
FwdRtnDF$ThreeYearRtn<-(((FwdRtnDF$ThreeYrPrice-FwdRtnDF$curPrice)/FwdRtnDF$curPrice)) 
FwdRtnDF$OneYearRtn<-ifelse(is.infinite(FwdRtnDF$OneYearRtn),NA,FwdRtnDF$OneYearRtn)
FwdRtnDF$TwoYearRtn<-ifelse(is.infinite(FwdRtnDF$TwoYearRtn),NA,FwdRtnDF$TwoYearRtn)
FwdRtnDF$ThreeYearRtn<-ifelse(is.infinite(FwdRtnDF$ThreeYearRtn),NA,FwdRtnDF$ThreeYearRtn)

# remove outliers from returns.
OneYrDF<-RemoveOutliers(FwdRtnDF,"OneYearRtn")
GetTable6Stats(OneYrDF,"OneYearRtn") # Return12
TwoYrDF<-RemoveOutliers(FwdRtnDF,"TwoYearRtn")
GetTable6Stats(TwoYrDF,"TwoYearRtn") # Return24
ThreeYrDF<-RemoveOutliers(FwdRtnDF,"ThreeYearRtn")
GetTable6Stats(ThreeYrDF,"ThreeYearRtn") # Return36



CollectBetaItem<-function(ItemName,curYear)
{
  AnnualDate<-paste(curYear,"0531",sep="")
  ItemDF<-read.csv(paste(MarketDataDirectory,ItemName,".csv",sep=""))
  ColNames<-names(ItemDF)  
  if (ItemName %in% c("Beta"))
  {
    DateList<-ColNames[3:length(ColNames)] 
  }
  DateList<-FormatDates(DateList)
  RelevantItemPos<-length(DateList[DateList<=AnnualDate])
  ColNames<-gsub("X",paste(ItemName,"_",sep=""),gsub("\\.","_",ColNames))  # lets replace X with ItemName_, and replace the "." with a "_"
  ColNames[1]<-"CompanyName"
  names(ItemDF)<-ColNames  # now put these new names back into the ItemDF column names.
  if (ItemName=="Beta")
  {
    BetaDF<-ItemDF[1:(RelevantItemPos+2)] 
    BetaDF<-subset(BetaDF,select=c(2,dim(BetaDF)[2]))
    BetaDF<-BetaDF[!is.na(BetaDF$Code),]
    BetaDF$Code<-gsub("\\)","",gsub("897E\\(","",BetaDF$Code))
    names(BetaDF)[2]<-"Beta" 
    ItemDF<-BetaDF
  }
  return(ItemDF)
}

for (curYear in 2014:2019) 
{ 
  print(paste("Doing for Beta Year ",curYear, sep=""))
  curBetaDF<-CollectBetaItem("Beta",curYear)
  # Remove beta > +/- 3 sd
  curBetaDF<-RemoveOutliers(curBetaDF,"Beta")
  if (curYear==2014)
  {
    AllBetaDF<-curBetaDF 
  }else
  {
    AllBetaDF<-rbind(AllBetaDF,curBetaDF)
  }
}
GetTable6Stats(AllBetaDF,"Beta") 

