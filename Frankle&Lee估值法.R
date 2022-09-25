install.packages("StatMeasures")
install.packages("Hmisc")
install.packages("gdata")
install.packages("stringr")
library(Hmisc) 
library(gdata) 
library(stringr) 
library(StatMeasures) 

#HomeDataDirectory<<-"C:/Users/User/CsvDir/"  
#csv files location
#MarketDataDirectory<<-"C:/Users/User/Documents/FlCsvData/" 
# make this point to your directory where the market data is saved, this should include, price, nosh, mv, beta etc
#OutputDirectory<<-"C:/Users/User/Output/"
# define a global string variable to represent the output directory path
#OutputMMYYYYDirectory<-"C:/Users/User/Output/Date/YYYY/MM/"
# make date files of output
DataItemSheets<-paste(HomeDataDirectory,"OutputSHEET_TAB.csv",sep="")

ImportData<-function(ItemName) # declare a function to import CSV data
{
  # BookValue = 03501 - sheet 3_4; NetIncome = 01751 - sheet 4_6; Dividend = 05376 - sheet 3_6; FY1 = EPS1MN - sheet 5_3; FY2 = EPS2MN - sheet 5_4; FY3 = EPS3MN - sheet 5_5; Ltg = LTMN - sheet 5_9;
  switch(ItemName,
    "BookValue" = 
    {
      curItemSheet<-"3_4"
    },
    "NetIncome" = 
    {
      curItemSheet<-"4_6"
    },
    "Dividend" = 
    { 
      curItemSheet<-"3_6"
    },
    "FY1" = 
    {
      curItemSheet<-"5_3"
    },
    "FY2" = 
    {
      curItemSheet<-"5_4"
    },
    "FY3" = 
    {
      curItemSheet<-"5_5"
    },    
    "Ltg" = 
    {
      curItemSheet<-"5_9"
    }
  )

  FileString<-gsub("SHEET_TAB",curItemSheet,DataItemSheets)  # replace the string "SHEET_TAB" with the curItemSheet String
  ItemDF<-read.csv(FileString) # now read the file into the variable "item Data Frame" (ItemDF)
  ItemDF<-ItemDF[,2:dim(ItemDF)[2]] # drop the first column
  ItemDF<-rename.vars(ItemDF,"X0","CompanyName",info=FALSE)  # rename the first column from X0 to Company Name, and make the display silent (info = FALSE)
  ItemDF$Code<-substr(ItemDF$Code,1,6) # take only the DSCode, strip the rest of the Code
  ItemDF<-ItemDF[,1:11] # there appear to be erroneous columns in Paulo's data, strip this after column 11
  ColNames<-names(ItemDF)  
  ColNames<-gsub("X",paste(ItemName,"_",sep=""),gsub("\\.","_",ColNames))  # lets replace X with ItemName_, and replace the "." with a "_"
  names(ItemDF)<-ColNames  # now put these new names back into the ItemDF column names.
  ItemDF<-ItemDF[nchar(ItemDF$Code)==6,] # remove any rows where the code is not 6 characters long (use the nchar function), length is for vectors.
  return(ItemDF)  # return the ItemDF to the function that called ImportData
}

# sometimes there are write permission issues (admin vs user) on your laptop - this function will help
MakeDirReadable<-function(DirName) # chmod is usually a linux function
{
  command<-paste("chmod -R --silent 777 ", DirName,sep="") # 777 means read, write etc. see https://www.mkssoftware.com/docs/man1/chmod.1.asp for help.
  system(command) # execute the command in the string above
}

CollectRelevantItems<-function()
{
  ItemList<-c("BookValue","NetIncome","Dividend","FY1","FY2","FY3","Ltg") # create a string vector to hold the list of items we need. BookValue is first, because there is well populated data for non financial companies
  
  for (i in 1:length(ItemList))  # for all of the items in ItemList
  {
    ItemName<-ItemList[i]        # store the current item name in a variable called ItemName
    curDF<-ImportData(ItemName)  # call the ImportData function with an ItemName
    switch(ItemName,             # rename the curDF that was returned from ImportData function into the correct NameDF based on the current item
      "BookValue" = {BVDF<-curDF},  # this copies curDF into BVDF
      "NetIncome" = {NIDF<-curDF},
      "Dividend" = {DivDF<-curDF},
      "FY1" = {FY1DF<-curDF},
      "FY2" = {FY2DF<-curDF},
      "FY3" = {FY3DF<-curDF},
      "Ltg" = {LtgDF<-curDF}
    ) 
    # then merge all the data together
    if (i==1)
    {
      AllMarketDataDF<-curDF # on the first item, there is nothing to merge
      print(dim(AllMarketDataDF)) # check the data frame dimensions are correct
    }else 
    {
      curDF<-subset(curDF,select=-c(CompanyName)) # we do not need to repeat the CompanyName column many times
      AllMarketDataDF<-merge(AllMarketDataDF,curDF,by.x="Code",by.y="Code",all.x=TRUE) # then merge new data with previous data
      print(dim(AllMarketDataDF)) # check the data frame dimensions are correct
    }
  }  
   str(AllMarketDataDF[,1:10]) # lets check on the first 10 columns
  write.table(AllMarketDataDF,paste(OutputDirectory,"AllMarketData.csv",sep=""),sep=',',row.names=FALSE,quote=FALSE,na='') # write the DF to a csv file
  return (AllMarketDataDF) 
}

FormatDates<-function(DateList)
{
  ReverseDateList<-paste(substr(DateList,7,10),substr(DateList,4,5),substr(DateList,1,2),sep="")
  return(ReverseDateList)
}

CollectStaticItem<-function(ItemName,curYear)
{
    AnnualDate<-paste(curYear,"0531",sep="")
    ItemDF<-read.csv(paste(MarketDataDirectory,ItemName,".csv",sep=""))
    ColNames<-colnames(ItemDF)  
    if (ItemName %in% c("Nosh","MarketValue"))
         {
             DateList<-ColNames[5:length(ColNames)]
    }
    DateList<-gsub("X","",DateList)
    ReverseDateList<-FormatDates(DateList)
    RelevantItemPos<-length(ReverseDateList[ReverseDateList < AnnualDate])
    ColNames<-gsub("X",paste(ItemName,"_",sep=""),gsub("\\.","_",ColNames))  # lets replace X with ItemName_, and replace the "." with a "_"
    names(ItemDF)<-ColNames  # now put these new names back into the ItemDF column names.
      if (ItemName=="Nosh")
        {
             NoshDF<-ItemDF[1:(RelevantItemPos+4)] 
             NoshDF<-subset(NoshDF,select=c(1,dim(NoshDF)[2]))
             ItemDF<-NoshDF
           }
       if (ItemName=="MarketValue")
         {
             MVDF<-ItemDF[1:(RelevantItemPos+4)] 
             MVDF<-subset(MVDF,select=c(1:2,dim(MVDF)[2]))
             MVDF[,3]<-MVDF[,3]*1000  # MV according to datastream is in units of k's
             ItemDF<-MVDF
           }
       return(ItemDF)
     }


RetrieveACol<-function(AllMarketDataDF,curYear,t,ItemName)
{
  ColNames<-colnames(AllMarketDataDF)
  ItemNameLength<-nchar(ItemName)
  relYear<-curYear+t  # clue - whats this for?
  YearCols<-c(1,which(str_sub(ColNames,-4,-1)==relYear)) # which columns are the current year columns + Col 1 for identifier
  YearDF<-AllMarketDataDF[,YearCols]
  YearColNames<-colnames(YearDF)
  ItemName<-paste(ItemName,"_",relYear,sep="") 
  ItemCol<-which(YearColNames==ItemName)
  ACol<-YearDF[,c(1,ItemCol)]
  ACol<-ACol[!is.na(ACol[,2]),]
  if (substr(ItemName,1,3) %in% c("FY1","FY2","FY3"))
  {
    NoshDF<-CollectStaticItem("Nosh",curYear)
    NoshDF<-NoshDF[!is.na(NoshDF[,2]),]
    ACol<-merge(ACol,NoshDF,by.x="Code",by.y="DSCode",all.x = TRUE)
    ACol[,2]<-as.numeric(ACol[,2]) 
    ACol[,2]<-ACol[,2]*ACol[,3]  # multiple EPS by Nosh to get Earnings
    ACol<-ACol[,1:2]
  }
  return(ACol)
}


RetrievePayoutRatio<-function(AllMarketDataDF,curYear,t)
{
  curD<-RetrieveACol(AllMarketDataDF,curYear,t-1,ItemName="Dividend")   # e.g. if t = May 2014, then use D 2013
  curNI<-RetrieveACol(AllMarketDataDF,curYear,t-1,ItemName="NetIncome") # e.g. if t = May 2014, then use NI 2013
  kDF<-merge(curD,curNI,by.x="Code",by.y="Code")  
  kDF$k<-kDF[,2]/kDF[,3]
  return(kDF)
}

ComputeNextB<-function(BhatDF)
{
  BhatDF$BHat<-(1+(1-BhatDF$k)*BhatDF[,3])*BhatDF[,4]  
  BhatDF<-subset(BhatDF,select=c(Code,BHat))
  return(BhatDF)
}

ComputefcROE<-function(nextFY,curB,prevB)
{
  roeDF<-merge(nextFY,curB,by.x="Code",by.y="Code")
  roeDF<-merge(roeDF,prevB,by.x="Code",by.y="Code")
  roeDF<-roeDF[!is.na(roeDF[,2]),]
  roeDF<-roeDF[!is.na(roeDF[,3]),]
  roeDF<-roeDF[!is.na(roeDF[,4]),]
  roeDF$fcROE<-roeDF[,2]/apply(roeDF[,3:4],1,mean)   # compute fcROE for 2014
  return(roeDF)
}



ComputeForecastROEs<-function()
{
  AllMarketDataDF<-CollectRelevantItems() 
  ColNames<-colnames(AllMarketDataDF)
  for (curYear in 2014:2018)
  {
    t<-0 # e.g if t = May 2014
    nextFY<-RetrieveACol(AllMarketDataDF,curYear,t,ItemName="FY1")        # fetch FY1 for 2014
    curB<-RetrieveACol(AllMarketDataDF,curYear,t-1,ItemName="BookValue")  # fetch B for 2013
    prevB<-RetrieveACol(AllMarketDataDF,curYear,t-2,ItemName="BookValue") # fetch B for 2012
    prevB[,2]<-as.numeric(prevB[,2])
    roeDF<-ComputefcROE(nextFY,curB,prevB)  # compute fcROE for 2014
    roeDF<-rename.vars(roeDF,"fcROE",paste("fcROEt_",toString(curYear),sep=""),info=FALSE)
    kDF<-RetrievePayoutRatio(AllMarketDataDF,curYear,t) # collect Payout Ratio, k for the previous year
    kDF<-subset(kDF,select=c(Code,k))
    dataDF<-merge(roeDF,kDF,by.x="Code",by.y="Code")

    # now compute the fcROE for 2015
    BhatDF<-subset(dataDF,select=c(1,6,5,3)) # extract only the columns needed for Bhat
    BhatDF<-ComputeNextB(BhatDF) 
    dataDF<-merge(dataDF,BhatDF,by.x="Code",by.y="Code")
    nextFY<-RetrieveACol(AllMarketDataDF,curYear,t-1,ItemName="FY2")   # fetch FY2 for 2014
    roeDF<-ComputefcROE(nextFY,BhatDF,curB)  # compute fcROE for 2015
    roeDF<-rename.vars(roeDF,"fcROE",paste("fcROEt1_",toString(curYear+t),sep=""),info=FALSE)
    roeDF<-roeDF[,c(1:2,5)] 
    dataDF<-merge(dataDF,roeDF,by.x="Code",by.y="Code")
    
    # now compute the fcROE for 2016
    BhatDF<-subset(dataDF,select=c(1,6,9,7)) # extract only the columns needed for BHatT1
    BhatDF<-ComputeNextB(BhatDF) 
    BhatDF<-rename.vars(BhatDF,"BHat","BHatT1",info=FALSE) 
    dataDF<-merge(dataDF,BhatDF,by.x="Code",by.y="Code")
    Ltg<-RetrieveACol(AllMarketDataDF,curYear,t-2,ItemName="Ltg")  # collect the last known Ltg for 2014
    prevFY<-dataDF[,c(1,8)]
    prevFY<-merge(prevFY,Ltg,by.x="Code",by.y="Code")
    prevFY[,2]<-as.numeric(prevFY[,2])
    prevFY[,3]<-as.numeric(prevFY[,3])
    prevFY[,2]<-prevFY[,2]*(1+(prevFY[,3]))
    prevFY<-prevFY[,1:2]
    nextFY<-RetrieveACol(AllMarketDataDF,curYear,t-2,ItemName="FY3") # fetch FY3 for 2014
    numeratorDF<-merge(prevFY,nextFY,by.x="Code",by.y="Code",all.x=TRUE)
    numeratorDF$numerator<-ifelse(!(is.na(numeratorDF[,2])),numeratorDF[,2],numeratorDF[,3])
    numeratorDF<-numeratorDF[!is.na(numeratorDF$numerator),]
    numeratorDF<-numeratorDF[,c(1,3:4)]
    dataDF<-merge(dataDF,numeratorDF,by.x="Code",by.y="Code")
    numeratorDF<-subset(numeratorDF,select=c(Code,numerator))
    BhatT1DF<-dataDF[,c(1,10)] # this is the BHat1
    BhatDF<-dataDF[,c(1,7)] # this is the BHat
    roeDF<-ComputefcROE(numeratorDF,BhatT1DF,BhatDF)  # compute fcROE for 2016
    roeDF<-rename.vars(roeDF,"fcROE",paste("fcROEt2_",toString(curYear+t),sep=""),info=FALSE)
    roeDF<-roeDF[,c(1,5)] 
    dataDF<-merge(dataDF,roeDF,by.x="Code",by.y="Code")

    V1DF<-dataDF[,c(1,3,5)]
    names(V1DF)<-c("Code","Anchor","fcROEt")
    V1DF<-ComputeV(V1DF,"V1")
    dataDF<-merge(dataDF,V1DF,by.x="Code",by.y="Code")

    V2DF<-dataDF[,c(1,3,5,7,9)]
    names(V2DF)<-c("Code","Anchor","fcROEt","BHat","fcROEt1")
    V2DF<-ComputeV(V2DF,"V2")
    dataDF<-merge(dataDF,V2DF,by.x="Code",by.y="Code")

    V3DF<-dataDF[,c(1,3,5,7,9,10,13)]
    names(V3DF)<-c("Code","Anchor","fcROEt","BHat","fcROEt1","BHatT1","fcROEt2")
    V3DF<-ComputeV(V3DF,"V3")
    dataDF<-merge(dataDF,V3DF,by.x="Code",by.y="Code")
    dataDF[1,] # check for company = 130042

    MVDF<-CollectStaticItem("MarketValue",curYear)
    MVDF<-MVDF[!is.na(MVDF[,3]),]
    dataDF<-merge(dataDF,MVDF,by.x="Code",by.y="DSCode",all.y = TRUE)
    MVCol<-dim(dataDF)[2]
    dataDF$MVtoV1<-dataDF[,MVCol]/dataDF$V1
    dataDF$MVtoV2<-dataDF[,MVCol]/dataDF$V2
    dataDF$MVtoV3<-dataDF[,MVCol]/dataDF$V3
    dataDF$V1Decile<-decile(dataDF$MVtoV1) # decile 1 is good, decile 10 is expensive
    dataDF$V2Decile<-decile(dataDF$MVtoV2) # decile 1 is good, decile 10 is expensive
    dataDF$V3Decile<-decile(dataDF$MVtoV3) # decile 1 is good, decile 10 is expensive
    write.table(dataDF,paste(OutputDirectory,"VData_",curYear,".csv",sep=""),sep=',',row.names=FALSE,quote=FALSE,na='') # write the DF to a csv file
  }
}
ComputeV<-function(VDF,VType)
{
  DiscountRate<-0.11  # Discount Rate 
  
  
  VDF$PVRI1<-(VDF$fcROEt-DiscountRate)*VDF$Anchor/(1+DiscountRate)
  if (VType=="V1")
  {
    VDF$PVRI2_V1<-(VDF$fcROEt-DiscountRate)*VDF$Anchor/(DiscountRate*(1+DiscountRate))
    VDF$V1<-apply(VDF[,c(2,4:5)],1,sum)
    VDF<-subset(VDF,select=c(Code,PVRI1,PVRI2_V1,V1))
  }
  if (VType=="V2")
  {
    VDF$PVRI2_V2<-(VDF$fcROEt1-DiscountRate)*VDF$BHat/(DiscountRate*(1+DiscountRate))
    VDF$V2<-apply(VDF[,c(2,6:7)],1,sum)
    VDF<-subset(VDF,select=c(Code,PVRI2_V2,V2))
  }
  if (VType=="V3")
  {
    VDF$PVRI2_V3<-(VDF$fcROEt1-DiscountRate)*VDF$BHat/((1+DiscountRate)^2)
    VDF$PVRI3_V3<-(VDF$fcROEt2-DiscountRate)*VDF$BHatT1/DiscountRate*(1+DiscountRate)^2
    VDF$V3<-apply(VDF[,c(2,8:10)],1,sum)
    VDF<-subset(VDF,select=c(Code,PVRI2_V3,PVRI3_V3,V3))
  }
  return(VDF)
}


 


