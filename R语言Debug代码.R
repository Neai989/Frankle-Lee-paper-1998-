
options(error = recover) # turns the debug on
#options(error = NULL)   # turns the debug off
## return from within a function and 0 to escape browse

AFunction<-function(BreakValue=7)
{
  print("I am inside a function")
  stringValue<-"Z"
  for (i in 1:10)
  {
    print(paste("the value of i is ", i, sep=""))
    stringValue<-letters[i]
    print(paste("the ",i,"th letter is ",stringValue,sep=""))
    if (i==BreakValue) # lets break the code at i = 7
    {
      print(stringvalue) 
    }
  }
}

OuterFunction<-function(BreakValue)
{
  print("I am going to enter the inner function")
  AFunction(BreakValue)
  if (BreakValue>10)
  {
    print("I am in the outer function now")
    break;
    
  }
}

# stop here #######

OuterFunction(7)
OuterFunction(12)
options(error = NULL) # turns the debug off

