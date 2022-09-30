library(RODBC) 
AccessChannel<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=c:/Users/Subbiah/Documents/Database2.accdb")
myFetchFrame<-sqlFetch(AccessChannel,'Price')
odbcClose(AccessChannel)

#----------------------------

library(RODBC) 
AccessChannel<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=c:/Users/Subbiah/Documents/Database2.accdb")
mySelectFrame<-sqlQuery(AccessChannel,'select * from EPS')
odbcClose(AccessChannel)

