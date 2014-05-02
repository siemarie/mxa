createLapChart <- function(file,rider) {
  
  ## DEFINITIONS ------------------------------------------------
  ## file:  the location of the file to read lap times from
  ##
  ## rider: name of the rider to track
  ## ------------------------------------------------------------
  
  data <- readLines(file);
  rider <- paste(rider," ",sep="");
  i <- 1;
  riderRows<-matrix(ncol=1,nrow=0);
  stopRows<-matrix(ncol=1,nrow=0);
  start <- 0;
  
  
  while(i<=length(data)) {
    if(data[i]==rider) {
      start <- i;
    } else {
      if(start>0) {
        minRow<-regexpr("MIN ", data[i]);
        if(minRow!=-1) {
          if(i>start) {
            stopRows <- append(stopRows,i);
          }
        }
      }
    }
    i <- i+1;
  }
  a<-start-1;
  curRider<-matrix(ncol=1,nrow=0);
  while(a<=stopRows[1]) {
    curRider<-append(curRider,data[a]);
    a<-a+1;
  }
  num<-substr(curRider[1],2,nchar(curRider[1])-1);
  name<-substr(curRider[2],0,nchar(curRider[2])-1);
  bike<-substr(curRider[3],0,nchar(curRider[3])-1);
  if(bike=="KAW") {
    bikeColor<-"#00FF00";
  }
  if(bike=="HON") {
    bikeColor<-"#FF0000";
  }
  if(bike=="YAM") {
    bikeColor<-"#0000FF";
  }
  if(bike=="SUZ") {
    bikeColor<-"#FFFF00";
  }
  if(bike=="KTM") {
    bikeColor<-"#FF6600";
  }
  laps<-length(curRider)-6;
  z<-1;
  riderLaps<-matrix(ncol=1,nrow=0);
  while(z<=laps) {
    lapNo<-z+3;
    tempLap<-substr(curRider[lapNo],(nchar(z)+2),(nchar(curRider[lapNo])-1));
    checkString<-substr(tempLap,0,2);
    startString<-"1:";
    if(checkString==startString){
      newString<-as.numeric(substr(tempLap,4,nchar(tempLap)));
      oldStart<-(as.numeric(substr(tempLap,3,3))+6)*10;
      tempLap<-oldStart+newString;      
    }
    riderLaps<-append(riderLaps,tempLap);
    z<-z+1;
  }
  mean<-mean(riderLaps);
  min<-min(riderLaps);
  output<-paste("name: '",name,"',","color: '",bikeColor,"', marker:{ },data: [",sep="");
  x<-1;
  while(x<=length(riderLaps)) {
   output<-paste(output,riderLaps[x],", ",sep="")
   x<-x+1;
  }
  output<-substr(output,0,nchar(output)-2);
  output<-paste(output,"]",sep="");
  print(output);
}
