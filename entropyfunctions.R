entropy <- function(d){
  if (missing(d)) 
    stop("Need to provide a vector")
  if (!is.factor(d))
    d<-as.factor(d)

  p <- summary(d)/sum(summary(d)) 
  -1*sum((p)*log2(p))
}

infogain <- function(d,a){
  if (missing(d) || missing(a)) 
    stop("Need to provide two vectors")

  if (!is.factor(d))
    d<-as.factor(d)
  if (!is.factor(a))
    a<-as.factor(a)

  s <- 0
  for (j in 1:length(levels(a))) {
    s <- s + (sum(a==levels(a)[j])/length(d))*entropy(d[which(a==levels(a)[j])])
  }

  entropy(d) - s
}

decide <- function(df,n){
  v <- vector(mode = "numeric", length = 0) 
  for (i in 1:ncol(df)){
    if (i != n) 
      v[i] <- infogain(df[,n],df[,i])
  }
  list(max=which(v==max(v)),gains = v)
}

dataset <- read.table(file = "C:\\Igor\\CUNY\\607_Data_Acq_Mgmt_R\\Week_03\\entropy_project\\entropy-test-file.csv", header = TRUE, sep = ",")
entropy(dataset$answer)
infogain(dataset$answer,dataset$attr1)
infogain(dataset$answer,dataset$attr2)
infogain(dataset$answer,dataset$attr3)
decide(dataset,4)