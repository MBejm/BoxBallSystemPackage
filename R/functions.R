evolution<-function(k){
  stopifnot(is.numeric(k), is.vector(k), is.numeric(k), length(k)<=15,max(k)<=1, min(k)>=0,length(k)>0,all(k == floor(k)))
  basket<-0
  for (i in 1:length(k)){
    if (k[i]==1){
      k[i]<-0
      basket=basket+1
    } else {
      if (basket>0) {
        k[i]<-1
        basket=basket-1}
    }
  } 
  return(k)
}

#\eta->T^{n}\eta
evolutions<-function(k,n){
  if (n>0) {
    for (i in 1:n){
      k<-evolution(k)
    }
  }
  return(k)
}

#plot single configuartion T^{n}\eta
eta<-function(k,n=0){
  stopifnot(is.numeric(k), is.vector(k), is.numeric(k), length(k)<=15,max(k)==1, min(k)==0,length(k)>0,all(k == floor(k)))
  k<-evolutions(k,n)
  x<-seq(from=1, to=length(k),by=1)
  y<-rep(1,length(k))
  plot(x,y,
       xlab="",
       xaxt='n',
       ylab="",
       yaxt='n',
       pch=(k*15)+1,
       axes=F,
       cex=2)
}

#plot dynamics T^{0}\eta, T^{1}\eta,...,T^{n}\eta max n=9
T_eta<-function(k,n=0){
  stopifnot(is.numeric(k), is.vector(k), is.numeric(k), length(k)<=15,max(k)==1, min(k)==0,length(k)>0,all(k == floor(k)))
  x<-seq(from=1, to=length(k),by=1)
  y<-rep((n+1),length(k))
  plot(x,y,
       xlab="",
       xaxt='n',
       ylab="",
       yaxt='n',
       ylim = c(0.5,n+1.5),
       pch=(k*15)+1,
       axes=F,
       cex=2)
  axis(2, at = 1:(n+1), labels = n:0,tick = FALSE,las=1)
  for (j in 1:n){
    k<-evolution(k)
    y<-rep((n-j+1),length(k))
    points(x,y,pch=(k*15)+1,cex=2)
  }
}

#plots walk corresponding to a given configuration
configuration.to.walk.plot<-function(k){
  y<-rep(0,length(k))
  start=0
  for (i in 1:length(k)){
    if (start==0) {
      if(k[i]==1) {start=1
      j=2}
    }
    if (start==1){
      if (k[i]==1){y[j]=y[j-1]+1}
      else {y[j]=y[j-1]-1}
    }
    j=j+1
  }
  x<-seq(from=0, to=length(y)-1,by=1)
  df <- data.frame(x, y)
  ggplot(df, aes(x = x, y = y)) +
    geom_path() +
    labs(x = "",y="")+ scale_x_continuous(breaks=seq(from=0, to=length(y)-1,by=1))
}

idyntify.runs<-function(k){
  stopifnot(is.numeric(k), is.vector(k), is.numeric(k), length(k)<=30,max(k)<=1, min(k)>=0,length(k)>0,all(k == floor(k)),sum(k[which(k==1)])==sum(k[which(k==0)]+1))
  runs<-rep(0,length(k))
  value<-1
  for (i in 2:length(k)){
    if(k[i]==k[i-1]){
      value=value+1
      if (i==length(k)){
        for(l in 0:(value-1)){
          runs[i-l]<-value
        }
      }
    }
    else {
      if(i==length(k)){runs[i]=1}
      for(j in 1:(value)){
        runs[i-j]<-value
      }
      value<-1
    }
  }
  return(runs)
}
idyntify.solitons<-function(k){
  oryginal.index=seq(from=1,to=length(k),by=1)
  temp<-rbind(oryginal.index,k)
  soliton_list <- vector(mode = "list")
  runs.unique<-unique(idyntify.runs(temp[2,]))
  for(l in 1:max(runs.unique)){assign(paste('sol_list', l, sep='_'), vector(mode = "list"))}
  is.temp.empty<-1
  
  while(is.temp.empty==1){
    runs<-idyntify.runs(temp[2,])
    i<-runs[min(which((runs[-1]-runs[-length(runs)])>=0 & (temp[2,-1]-temp[2,-ncol(temp)])!=0))]
    index<-which(runs==i)
    x<-c(temp[1,seq(from=index[1],length.out=(2*i))])
    if(exists(paste('sol_list', i, sep='_'))==FALSE){assign(paste('sol_list', i, sep='_'), vector(mode = "list"))}
    assign(paste('sol_list', i, sep='_'),c(get(paste('sol_list', i, sep='_')),list(x)))
    soliton_list[[i]]<-get(paste('sol_list', i, sep='_'))
    if(ncol(temp)==length(x)){is.temp.empty=0}
    else {temp<-temp[,-which(temp[1,] %in% x)]}
  }
  return(soliton_list)
}

idyntify.slot.diagram<-function(k){
  solitions<-idyntify.solitons(k)
  M<-length(solitions)
  Diagram<-matrix(0,nrow=M,ncol=length(k))
  for (i in 2:M){
    for(j in 1:length(solitions[[i]])){
      ind<-solitions[[i]][[j]]
      for(k in 1:(i-1)){
        Diagram[M-(k-1),ind[c((k+1):i,(k+i+1):(2*i))]]<-k
      }
    }
  }
  Diagram<-cbind(c(M:1),Diagram,c(M:1)) #adding records columns
  #for(i in 1:M){Diagram[i,which(Diagram[i,]!=0)]<-c(0:(sum(Diagram[i,]!=0)-1))}
  return(Diagram)
}