
# load the libraries you need for the functions you use.下好lirbrary
library(Hmisc)
library(stats)
library(MASS)
library(gdata)
library(stringr)
 library(xlsx)

#HomeDir<-"C:\\Users\\"
#create the input path 建立输入路径

epsDF<-read.csv(paste(HomeDir,"Earnings.csv",sep=""),skip=1)


epsDF$Company<-as.character(epsDF$Company)
epsDF$CompanyName<-as.character(epsDF$CompanyName)
epsDF$SectorName<-as.character(epsDF$SectorName)

str(epsDF) # check the data frame

TidyUpDF<-function(df, VarName)
{
  if (VarName %in% names(df)) 
  {
    df<-rename.vars(df,VarName,"TheVarName",info=FALSE) 
    df$TheVarName<-gsub(" ","",as.character(df$TheVarName))
    df<-rename.vars(df,"TheVarName",VarName,info=FALSE) 
  }
  return(df)
}

#priceDF<-read.xlsx(paste(HomeDir,"PriceAndEarnings.xlsx",sep="") #if you goes for xlsx files
priceDF<-read.csv(paste(HomeDir,"Prices.csv",sep=""))
str(priceDF) # still got those factors imported

# Get this list :- c("Company","CompanyName","Sector.Name")
TypeList<-names(subset(priceDF,select=c((sapply(priceDF,class)=="factor"))))

# now we have got rid of those factors and turned them into character strings.
for (aVarName in TypeList) {priceDF<-TidyUpDF(priceDF,aVarName)}
str(priceDF)

# get rid of the duplicated columns from the two tables
priceDF<-subset(priceDF,select=c(Company,Price))

# lets create an extra set of prices
ExtraPricesDF<-data.frame(Company=c("DXNS","GLXO","ULVR"),Price=c(439,1440,3073.5))

# lets add the extra lines by appending the Extra prices to the bottom of the original set
dim(priceDF)
priceDF<-rbind(priceDF,ExtraPricesDF)
dim(priceDF)
priceDF

# lets drop an entry from Prices
priceDF<-subset(priceDF,Company!="AV.")

# lets drop some negative entries from EPS
epsDF<-subset(epsDF,EPSYr0>0)

# now merge


PEu<-merge(priceDF,epsDF,by.x="Company",by.y="Company",all.x= TRUE,all.y=TRUE)


PEn$PE<-PEn$Price/(PEn$EPSYr0*100)
PEn$PE<-ifelse(PEn$PE>50,50,PEn$PE)


SectorAve<-aggregate(PEn$PE,list(SectorCode=PEn$SectorCode),mean)
SectorAve<-rename.vars(SectorAve,"x","SectorPE",info=FALSE)
SectorDF<-unique(subset(PEn,select=c(SectorCode,SectorName)))
SectorDF<-merge(SectorDF,SectorAve,by.x="SectorCode",by.y="SectorCode")

# lets put the sector averages next to the companies
# first get rid of the extra Sector Name, we can use a minus sign to drop a column
SectorDF<-subset(SectorDF,select=-c(SectorName))
PEn<-merge(PEn,SectorDF,by.x="SectorCode",by.y="SectorCode",all.x=TRUE)

#using min gives us the minimum in the list
min(PEn$PE)

# using pmin, gives us a minimum is subsets
# so we could cap the PE at the sector average maybe
PEn$CapPE<-pmin(PEn$PE,PEn$SectorPE)

# lets sort in alphabetical order
PEnOrder<-PEn[order(PEn$Company),]

# lets substitute the additional 00
PEn$SectorCode<-gsub("00","",PEn$SectorCode)

# lets remove all these special characters
PEn$SectorName<-str_replace_all(PEn$SectorName,"[[:punct:]]","")


ASequence<-seq(1,22,by = 3)
print(paste("The length of ASequence is",length(ASequence)," "))
Repeated<-rep(3,5) # gives a set of five 3's
AMatrix<-matrix(seq(10,90,by = 10),nrow=3,byrow=FALSE)
BMatrix<-matrix(seq(1,6), nrow = 3, byrow=TRUE)
ProductMatrix<-AMatrix %*% BMatrix
SumMatrix <- ProductMatrix + BMatrix
dim(SumMatrix)
ProductMatrix[1,2]-AMatrix[3,2] # 600 - 60 = 540 

# a normal distribution
x <- rnorm(400, mean=50, sd=10)
hist(x)

# lets do a bind on columns
AnotherColumn<-c(31,32,33,"",35,36,37,38) # PEn2<-PEn
PEn<-cbind(PEn,LastCol=as.numeric(AnotherColumn)) 
# use is.na
PEn$LastCol<-ifelse(is.na(PEn$LastCol),50,PEn$LastCol)


