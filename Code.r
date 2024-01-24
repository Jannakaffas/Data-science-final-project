df<-read.csv("data.csv")
#Familiarizing ourselves with the Data frame dimensions and structure
dim(df)
str(df)

#convert 'yes' and 'no' values to TRUE and FALSE or 1 and 0 
df$paid<-ifelse(df$paid =="yes","TRUE","FALSE")
df$paid<-as.factor(df$paid)
class(df$paid)
df$schoolsup<-ifelse(df$schoolsup == "yes",1,0)
df$famsup<-ifelse(df$famsup=="yes",1,0)
# Creating a column of the total grades called "GT" 
GT<-c(df$G1+df$G2)
df<-cbind(df,GT)
View(df)

#Creating Boxplot of "G1" and "G2" 
library(tidyverse)
Boxplottable<-cbind(df$G1,df$G2)
b<-boxplot(Boxplottable)
View(Boxplottable)

#summary of GT
summary (df$GT)

# Calculating mean "GT" 
pmean<-mean(df[df$paid=="TRUE","GT"]) # Mean GT for those who receive paid tutoring
npmean<-mean(df[df$paid=="FALSE","GT"]) #  Mean GT for those who do not receive paid tutoring
fmean<-mean(df[df$famsup==1,"GT"]) # Mean GT for those who receive family support
nfmean<-mean(df[df$famsup==0,"GT"]) # Mean GT for those who do not receive family support
smean<-mean(df[df$schoolsup==1,"GT"]) # Mean GT for those who receive school support
nsmean<-mean(df[df$schoolsup==0,"GT"])# Mean GT for those who do not receive school support

#Calculating median 
pmedian<-median(df[df$paid=="TRUE","GT"])
npmedian<-median(df[df$paid=="FALSE","GT"])
fmedian<-median(df[df$famsup==1,"GT"])
nfmedian<-median(df[df$famsup==0,"GT"])
smedian<-median(df[df$schoolsup==1,"GT"])
nsmedian<-median(df[df$schoolsup==0,"GT"])

#Calculating quantiles 
pquantile<-quantile(df[df$paid=="TRUE","GT"])
npquantile<-quantile(df[df$paid=="FALSE","GT"])
fquantile<-quantile(df[df$famsup==1,"GT"])
nfquantile<-quantile(df[df$famsup==0,"GT"])
squantile<-quantile(df[df$schoolsup==1,"GT"])
nsquantile<-quantile(df[df$schoolsup==0,"GT"])

#Calculating Mode 
#Range 
prange<-max(df[df$paid=="TRUE","GT"])-min(df[df$paid=="TRUE","GT"])
nprange<-max(df[df$paid=="FALSE","GT"])-min(df[df$paid=="FALSE","GT"])
frange<-max(df[df$famsup==1,"GT"])-min(df[df$famsup==1,"GT"])
nfrange<-max(df[df$famsup==0,"GT"])-min(df[df$famsup==0,"GT"])
srange<-max(df[df$schoolsup==1,"GT"])-min(df[df$schoolsup==1,"GT"])
nsrange<-max(df[df$schoolsup==0,"GT"])-min(df[df$schoolsup==0,"GT"])

#IQR
pIQR<-IQR(df[df$paid=="TRUE","GT"])
npIQR<-IQR(df[df$paid=="FALSE","GT"])
fIQR<-IQR(df[df$famsup==1,"GT"])
nfIQR<-IQR(df[df$famsup==0,"GT"])
sIQR<-IQR(df[df$schoolsup==1,"GT"])
nsIQR<-IQR(df[df$schoolsup==0,"GT"])

#MAD 
pMAD<-mad(df[df$paid=="TRUE","GT"])
npMAD<-mad(df[df$paid=="FALSE","GT"])
fMAD<-mad(df[df$famsup==1,"GT"])
nfMAD<-mad(df[df$famsup==0,"GT"])
sMAD<-mad(df[df$schoolsup==1,"GT"])
nsMAD<-mad(df[df$schoolsup==0,"GT"])

#Variance 
pvar<-var(df[df$paid=="TRUE","GT"])
npvar<-var(df[df$paid=="FALSE","GT"])
fvar<-var(df[df$famsup==1,"GT"])
nfvar<-var(df[df$famsup==0,"GT"])
svar<-var(df[df$schoolsup==1,"GT"])
nsvar<-var(df[df$schoolsup==0,"GT"])

#Standard Deviation 
pSD<-sd(df[df$paid=="TRUE","GT"])
npSD<-sd(df[df$paid=="FALSE","GT"])
fSD<-sd(df[df$famsup==1,"GT"])
nfSD<-sd(df[df$famsup==0,"GT"])
sSD<-sd(df[df$schoolsup==1,"GT"])
nsSD<-sd(df[df$schoolsup==0,"GT"])

#Creating Histograms 
abc=subset(newdf,schoolsup == '1', select = GT)

efg<-subset(newdf, paid == '1', select = GT)

p<-ggplot(abc,aes(GT))
p+geom_histogram(bins=8)
schoolsupone<-p+geom_histogram(bins=8)
schoolsupone
p<-ggplot(efg,aes(GT))
paidhisto<-p+geom_histogram(bins=8)
paidhisto

#Creating pie chart 
library(DescTools)
df$freetime<-as.factor(df$freetime)
t<-Freq(df$freetime)
x<-pie(t$freq,labels=(t$level),col=rainbow(nrow(t)),main="Pie Chart of Free Time")

#Creating 1st group of bar graphs 
status={}
for (i in 1: length (df$GT)) {
  if (df$GT [i]>=20){
    status [i]= "pass"
  }
  else {
    status [i] = "fail"
}
}
df= cbind(df,status)
View (df)
x<-df[df$status=="pass",]
x<-as.data.frame(x)
t<-cbind(x$status,x$studytime,x$sex)
t<-as.data.frame(t)
colnames(t)<-c("Pass/Fail","StudyTime","Sex")

library(tidyverse)
library(ggplot2)
p<-ggplot(t,aes(StudyTime))
p+geom_bar(aes(fill=Sex))

t<-cbind(x$status,x$goout,x$sex)
t<-as.data.frame(t)
colnames(t)<-c("Pass/Fail","Goout","Sex")
p<-ggplot(t,aes(Goout))
p+geom_bar(aes(fill=Sex))

#creating contingency tables
x<-cbind(df$studytime,df$sex)
x<-as.data.frame(x)
attach(x)
table(x) # Table for Study Time 

y<-cbind(df$goout,df$sex)
y<-as.data.frame(y)
attach(y)
table(y) # Table for Going out 

#Creating second group of bar graphs 
nrow(df[df$studytime=="4",])
nrow(df[df$studytime=="3",])
nrow(df[df$studytime=="2",])
nrow(df[df$studytime=="1",])

a<-nrow(x[x$studytime=="4",])/nrow(df[df$studytime=="4",])
b<-nrow(x[x$studytime=="3",])/nrow(df[df$studytime=="3",])
c<-nrow(x[x$studytime=="2",])/nrow(df[df$studytime=="2",])
d<-nrow(x[x$studytime=="1",])/nrow(df[df$studytime=="1",])
e<-rbind(a,b,c,d)
v<-c(4,3,2,1)
g<-cbind(v,e)
g<-as.data.frame(g)
colnames(g)<-c("StudyTime","PassingPercentage")
p<-ggplot(g,aes(x=StudyTime,y=PassingPercentage))
p+geom_bar(stat="identity")



a<-nrow(x[x$goout=="5",])/nrow(df[df$goout=="5",])
b<-nrow(x[x$goout=="4",])/nrow(df[df$goout=="4",])
c<-nrow(x[x$goout=="3",])/nrow(df[df$goout=="3",])
d<-nrow(x[x$goout=="2",])/nrow(df[df$goout=="2",])
e<-nrow(x[x$goout=="1",])/nrow(df[df$goout=="1",])

f<-rbind(a,b,c,d,e)
v<-c(5,4,3,2,1)
g<-cbind(v,f)
g<-as.data.frame(g)
colnames(g)<-c("GoOut","PassingPercentage")
p<-ggplot(g,aes(x=GoOut,y=PassingPercentage))
p+geom_bar(stat="identity")


#Creating Scatter plot 
ggplot (df, aes(x=GT, y=absences))+ 
  geom_point (shape=19, size=2, col= "blue")


#Creating Dot plot 
ggplot(data = df, aes(absences)) + 
  geom_dotplot(binwidth = 1.0, fill = "blue") +
  scale_y_continuous(breaks = NULL)

#Frequency table in appendix 
Freq(df$freetime)