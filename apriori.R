#Kindly install the package R.matlab if not installed
#install.packages("R.matlab")

path <- "transaction.mat"

library(R.matlab)
data<-readMat(path)
data<-data.frame(data)

minsupport<-4/100*nrow(data)

c<-list()
l<-list()

#frequent itemsets generation
sink("frequentitemsets.txt",append=TRUE,split=TRUE)

for(i in 1:50)
{
  c[[i]]<- data.frame(item=character(0),count=numeric(0))
  l[[i]]<- data.frame(item=character(0),count=numeric(0))
}



for(i in 1:ncol(data)){
  m<-length(which(data[,i]==1))
  c[[1]]<-rbind(c[[1]],cbind(item=i,count=m))    
  if(m>=minsupport)
  {
    l[[1]]<-rbind(l[[1]],cbind(item=i,count=m))
    print(paste(i))
  }
}


for(k in 2:50)
{
  
  
  if(k==2)
  {
    for(i in 1:(nrow(l[[k-1]])-1))
    {
      for(j in (i+1):nrow(l[[k-1]]))
      {
        temp1 <- l[[k-1]][i,1]
        temp2 <- l[[k-1]][j,1]
                x<-c(temp1,temp2)
        m<-length(which(data[,temp1]==1 & data[,temp2]==1))
        c[[k]]<-rbind(c[[k]],cbind(item=list(x),count=m))    
        if(m>=minsupport)
        {
          l[[k]]<-rbind(l[[k]],cbind(item=list(x),count=m))
          
          print(paste(x, collapse=", "))
          
        }
      }  
    }
  }
  else
  {
    
    for(i in 1:(nrow(l[[k-1]])-1))
    {
      for(j in (i+1):nrow(l[[k-1]]))
      {
        
        a<-l[[k-1]][i,1]$item[-(k-1)]
        b<-l[[k-1]][j,1]$item[-(k-1)]
        if(length(setdiff(a,b))==0)
        { 
          x<- c(a,l[[k-1]][i,1]$item[k-1],l[[k-1]][j,1]$item[k-1])
          
          sub_data<-data
          for(r in 1:length(x))
          {
            sub_data<-subset(sub_data,sub_data[,x[r]]==1)
          }
          m<-nrow(sub_data)
          c[[k]]<-rbind(c[[k]],cbind(item=list(x),count=m))    
          if(m>=minsupport)
          {  l[[k]]<-rbind(l[[k]],cbind(item=list(x),count=m))
          print(paste(x, collapse=", "))
          
          }
        }
      }  
    }
  }
  
  if(nrow(l[[k]])==0 | nrow(l[[k]])==1)
  {
    no_of_sets <- k 
    break;  
  }
}

sink()


maximal<-function(a,b,k)
{
  
  for(i in 1:nrow(a))
  {
    n<-as.vector(a[i,1])
    for(j in 1:nrow(b))
    {
      m<-as.vector(b[j,1])
      o<-which(n[[1]] %in% m[[1]])
      
      if(length(o)!=0)
        break;
    }
    if(length(o)==0)
    {  
      if(k==1)
        print(paste(a[i,1]))
      else
        
        print(paste(a[i,1]$item, collapse=", "))
    }
  }
  
}

#maximal frequent itemsets generation
sink("maximalfrequentitemsets.txt", append=TRUE, split=TRUE)
for(i in 1:no_of_sets)
{
  if(i == no_of_sets)
  {
    for(j in 1:nrow(l[[no_of_sets]]))
    {
      print(paste(l[[no_of_sets]][j,1]$item,collapse=", "))  
    } 
  }   
  else{
    maximal(l[[i]],l[[i+1]],i) 
  }
}

sink()

#rules generation
sink("rules.txt", append=TRUE, split=TRUE)

combine<-function(g,p)
{
  combn(g, p)
}



for(k in 2:no_of_sets)
{
  for(n in 1:nrow(l[[k]]))
  {
    for(i in 1:(k-1))  
    {
      t <- combine(l[[k]][n,1]$item,i)
      for(j in 1:ncol(t))  
      {
        left<-t[,j]
        numerator <- l[[k]][n,2]$count
        
        s<-match(list(left),l[[length(left)]][1]$item)
        
        if(length(left) == 1)
        { denominator <- l[[length(left)]][s,2]
        }
        else{  
          denominator <- l[[length(left)]][s,2]$count
        }
        
        confidence <- numerator / denominator
        confidence <- confidence * 100
        if(confidence >= 10)
        {
          right <- setdiff(l[[k]][n,1]$item,left)
          if(length(left)>0)
            le <- paste(left, collapse=", ")
          
          if(length(right)>0)
            ri <- paste(right, collapse=", ")
          
          print(paste(c(le, "->",ri), collapse=" "))
          
          
        }
        
        
      }
    }
  }
  
}

sink()

