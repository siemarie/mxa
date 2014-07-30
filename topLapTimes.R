topLapTimes <- function(class,type) {
  
  ## DEFINITIONS ------------------------------------------------
  ## class:  the class of riders to pull top ten times from,
  ##         acceptable inputs: 250, 450
  ##
  ## ------------------------------------------------------------
  
  if(class=="450") {
    data <- readLines("M1F1RID.txt");
    data <- append(data, readLines("M1F2RID.txt"));
  } 
  if(class=="250") {
    data <- readLines("M2F1RID.txt");
    data <- append(data, readLines("M2F2RID.txt"));
  }
  i<-1;
  stopRows<-matrix(ncol=1,nrow=0);
  startRows<-matrix(ncol=1,nrow=0);
  times<-matrix(ncol=1,nrow=0);
  riders<-matrix(ncol=1,nrow=0);
  lines<-"";
  
  while(i<=length(data)) {
    startRow <-regexpr("#",data[i]);
    minRow<-regexpr("MIN ", data[i]);
    if(minRow!=-1) {
      stopRows <- append(stopRows,i);
      if(type=="MIN") {
        tempTime <- substr(data[i],5,nchar(data[i])-1);
      }
      if(type=="AVG") {
        tempTime <- substr(data[i-2],1,nchar(data[i-2])-1);
      }
      checkString<-substr(tempTime,0,2);
      startString<-"1:";
      startString2<-"2:";
      if(checkString==startString){
        newString<-as.numeric(substr(tempTime,4,nchar(tempTime)));
        oldStart<-(as.numeric(substr(tempTime,3,3))+6)*10;
        tempTime<-oldStart+newString;      
      }
      if(checkString==startString2){
        newString<-as.numeric(substr(tempTime,4,nchar(tempTime)));
        oldStart<-(as.numeric(substr(tempTime,3,3))+12)*10;
        tempTime<-oldStart+newString;      
      }
      times<-append(times,tempTime);
    }
    if(startRow!=-1) {
      startRows <- append(startRows,i+1);
      tempRider <- substr(data[i+1],0,nchar(data[i+1])-1);
      riders <- append(riders,tempRider);
    }
    i <- i+1;
  }
  
  data2 <- cbind(riders,times);
  data2 <- data2[order(data2[,2]),];
  a<-1;
  while(a<=25) {
    newTimeMins <- floor(as.numeric(data2[a,2])/60);
    newTimeSecs <- floor(as.numeric(data2[a,2])-(60*newTimeMins));
    newTimeHunds <- round(1000*(as.numeric(data2[a,2])-(60*newTimeMins)-newTimeSecs));
    if(nchar(newTimeSecs)<2) {
      newTimeSecs <- paste("0",newTimeSecs,sep="");
    }
    while(nchar(newTimeHunds)<3) {
      newTimeHunds <- paste("0",newTimeHunds,sep="");
    }
    newTime <- paste(newTimeMins,":",newTimeSecs,".",newTimeHunds,sep="")
    output <- paste(a,". ",data2[a,1]," - ",newTime,sep="");
    lines <- cat(lines,output,sep="\n");
    a<-a+1;
  }
  print(lines);
}
