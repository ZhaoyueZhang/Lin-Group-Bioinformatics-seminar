#Data Type
#vector and factor
sample_num <- c('s1', 's3', 's5', 's2', 's4', 's6')
sample_factor <- factor(sample_num,levels = c('s1', 's3', 's5', 's2', 's4', 's6'))
sample_num
sample_factor


#list
score <- c(90,89,95,83,87,82)
list(sample_num,score)

#matrix
m <- matrix(y,nrow = 2, ncol = 3)
dim(m)
m[1,1]
m[1,]

#dataframe
data <- data.frame(sample_factor,score)
data
colnames(data) = c('sample_name','score')
data
data$score

#Array
a <- array(1:16,dim=c(2,4,2))
class(d)

class(sample_num)
class(sample_factor)
class(data)



################################Decision
#if-elseif-else
day = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
for(each in day){
  if(each == 'Thu') {
    print('Today is Thu, we have Biological Databases.')
  }
  else if(each == 'Fri'){
    print('Today is Fri, we have Programming.')
  }
  else if(each == 'Sun'){
    print('Today is Sun, we have Basic Statistics.')
  }
  else{
    print(paste0(paste0('Today is ',each),', no class today.'))
  }
}

#switch
x = 2
switch (x,
  'first res','second res','third res'
)
rm(x)


############################Loop
#repeat
n = 1
repeat{
  print(day[n])
  n <- n+1
  
  if(n>4){
    break
  }
}

#while
n = 1
while(n<5){
  print(day[n])
  n = n+1
}


################readtable
getwd()
setwd("D:/zzy/course/Programming_R")
x = read.table('file2.txt')
head(x)
x = read.table('file2.txt',header = T)
head(x)
dim(x)
apply(x[,2:5], 1, max)
rm(x)

###function
shannon.entropy <-function(x,type='raw'){
  if(type=='raw'){
    freqs <- x/sum(x)
    vec <- freqs
  }else{
    vec=x
  }
  -sum(vec * log(vec,2))
}

shannon.entropy(score,type='raw')


###packages
install.packages()
library()
if(!requireNamespace("BiocManager",quietly = TRUE))  install.packages("BiocManager")
BiocManager::install("GEOquery")


###statistic
#
mean(x$GSM2094855)
summary(x[,2:5])


#################################plot

names(score) <- sample_num
barplot(score,col = rainbow(6))


#data = data.frame(name = x, score = y, grps = c('g1','g1','g1','g0','g0','g0'))
##
#Data Mapping, Geometric, scale, statistics, coordinante, Layer, Facet, Theme
data$grps = c('g1','g1','g1','g0','g0','g0')
library(ggplot2)
suppressMessages(ggplot2)

p <- ggplot(data,mapping = aes(x = sample_name, y = score, fill = grps))
p + geom_bar(stat = 'identity',width = 0.8)+
  labs(x = 'Sample', y = 'Score')+
  theme_classic()+
  scale_fill_manual(values = c("#95C183","#376FAE"))



#stat = 'count'
#                            


