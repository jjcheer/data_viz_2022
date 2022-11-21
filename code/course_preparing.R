#' data vis sliding and course preparing
#' date: 0908,2022
#' progress:
#' 

# why we prefer bar or histogram to pie

library(ggplot2)
library(RColorBrewer)

#---------------------------one variable, bar graph and histogram----------------------------------------------------
mydata<-data.frame(cut=c("Fair","Good","Very Good","Premium","Ideal"),
                   price=c(4300,3800,3950,4700,3500))

#mydata$Cut <- factor(mydata$Cut, levels = mydata$Cut[order(mydata$Order)])
ggplot(data=mydata,aes(x=cut,y=price))+
  geom_bar(stat = "identity", 
           width = 0.8,colour="black",size=0.25,
           fill="#FC4E07",alpha=1)+
  ylim(0, 6000)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black")
  )

# using coordinate to transform the bar to pie

ggplot(data=mydata,aes(x=cut,y=price))+
  geom_bar(stat = "identity", 
           width = 0.8,colour="black",size=0.25,
           fill="#FC4E07",alpha=1)+
  ylim(0, 6000)+
  coord_polar()+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black")
  )

#

# pie chart ---------------------------------------------------------------

slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")

barplot(slices,names.arg = lbls)


# two variables -----------------------------------------------------------
library(reshape2)
mydata <- read.csv("./textbook/ch03/MultiColumn_Data.csv",
                   check.names=FALSE,
                   sep=",",na.strings="NA",
                   stringsAsFactors=FALSE)

mydata <- mydata %>% 
  pivot_longer(!Catergory, names_to = "variable", values_to = "value" )

ggplot(data=mydata,aes(Catergory,value,fill=variable))+
  geom_bar(stat="identity",position="dodge",
           color="black",width=0.7,size=0.25)+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"))+
  ylim(0, 10)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.88,0.88)
  )


# stacked bar graph -------------------------------------------------------

mydata<-read.csv("./textbook/ch03/StackedColumn_Data.csv",
                 sep=",",
                 na.strings="NA",
                 stringsAsFactors=FALSE)
mydata <- 
mydata %>% 
  pivot_longer(!Clarity, names_to = "variable", values_to = "value")

ggplot(data=mydata,aes(variable,value,fill=Clarity))+
  geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"YlOrRd")[c(6:2)])+
  ylim(0, 15000)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.85,0.82)
  )


# percent stack bar graph -------------------------------------------------

# data is the same in stacked bar graph

ggplot(data=mydata,aes(variable,value,fill=Clarity))+
  geom_bar(stat="identity", position="fill",color="black", width=0.8,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"GnBu")[c(7:2)])+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.position = "right"
  )


# 3.3.1 unequal-width bar graph -------------------------------------------

library(Cairo)
library(showtext)

mydata<-data.frame(name=paste0("Project",1:5),scale=c(35,30,20,10,5),ARPU=c(56,37,63,57,59))
mydata$xmin<-0
for (i in 2:5){
  mydata$xmin[i]<-sum(mydata$scale[1:i-1])
}

#构造矩形X轴的终点（最大点）
for (i in 1:5){
  mydata$xmax[i]<-sum(mydata$scale[1:i])
}
#构造数据标签的横坐标：
for (i in 1:5){
  mydata$label[i]<-sum(mydata$scale[1:i])-mydata$scale[i]/2
}

#CairoPDF(file="unequalwidth_bar.pdf",width=4.89,height=5.53)
#showtext.begin()
#windowsFonts(myFont = windowsFont("Songti SC"))
ggplot(mydata)+
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=0,ymax=ARPU,fill=name),colour="black",size=0.25) +
  geom_text(aes(x=label,y=ARPU+3,label=ARPU),size=4,col="black") +
  geom_text(aes(x=label,y=-2.5,label=name),size=4,col="black") +
  ylab("ARPU")+
  xlab("scale")+
  ylim(-5,80)+
  theme(panel.background=element_rect(fill="white",colour=NA),
        panel.grid.major = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        panel.grid.minor = element_line(colour = "grey60",size=.25,linetype ="dotted" ),
        text=element_text(size=15),
        plot.title=element_text(size=15,hjust=.5),#family="myfont",
        legend.position="none"
  )
#showtext.end()
#dev.off()


# 3.4.1 cleveland dot plot ------------------------------------------------


# lollipop plot -----------------------------------------------------------------

mydata<-read.csv("./textbook/ch03/DotPlots_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)
mydata$sum<-rowSums(mydata[,2:3])

order<-sort(mydata$sum,index.return=TRUE,decreasing = FALSE)
mydata$City<- factor(mydata$City, levels = mydata$City[order$ix])

ggplot(mydata, aes(sum, City)) +
  geom_segment(aes(x=0, 
                   xend=sum,
                   y=City, 
                   yend=City))+
  geom_point(shape=21,size=3,colour="black",fill="#FC4E07")+
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black")
  )


# Cleveland dot -----------------------------------------------------------

ggplot(mydata, aes(sum, City)) +
  geom_point(shape=21,size=3,colour="black",fill="#FC4E07")+
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black")
  )


# Dumbbell diagram --------------------------------------------------------

mydata<-read.csv("./textbook/ch03/DotPlots_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)

mydata$City <- factor(mydata$City, levels = mydata$City[order(mydata$Female)])

mydata <- mydata %>% 
  pivot_longer(!City, names_to = "variable", values_to = "value")

ggplot(mydata, aes(value,City,fill=variable)) +
  geom_line(aes(group = City)) +
  geom_point(shape=21,size=3,colour="black")+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07","#36BED9"))+
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=12,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position = c(0.85,0.12)
  )


# Nightingale Rose --------------------------------------------------------


# coordinate polar --------------------------------------------------------

mydata <- data.frame( a=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                      b=c(50, 60, 70, 20,90,110,30))
myAngle <-seq(-20,-340,length.out =7)

ggplot(mydata) +
  geom_bar(aes(x=a, y=b),width = 1,stat="identity",
           colour = "black",fill="#F8766D") +
  geom_text(aes(x=a,y = b-8,label = b),color="white") +
  coord_polar(theta = "x",start=0) +
  ylim(c(0,120))+
  theme_light()
  # theme( panel.background = element_blank(),
  #        panel.grid.major = element_line(colour = "grey80",size=.25),
  #        axis.text.y = element_text(size = 12,colour="black"),
  #        axis.line.y = element_line(size=0.25),
  #        axis.text.x=element_text(size = 13,colour="black",angle = myAngle))


# coordinate polar,  multi variables --------------------------------------

diamonds<-cbind(diamonds,Cou=rep(1,nrow(diamonds)))
sum_clarity<-aggregate(Cou~clarity,diamonds,sum)
sort_clarity<-arrange(sum_clarity,desc(Cou))
diamonds$clarity<- factor(diamonds$clarity, levels = sort_clarity$clarity)
myAngle <-seq(-20,-340,length.out = 8)

ggplot(diamonds,aes(x=clarity,fill=color))+
  geom_bar(width=1.0,colour="black",size=0.25)+
  coord_polar(theta = "x",start=0)+
  scale_fill_brewer(palette="GnBu")+
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+
  ylim(c(0,12000))+
  theme_light()
  # theme( panel.background = element_blank(),
  #        panel.grid.major = element_line(colour = "grey80",size=.25),
  #        axis.text.y = element_text(size = 12,colour="black"),
  #        axis.line.y = element_line(size=0.25),
  #        axis.text.x=element_text(size = 13,colour="black",angle = myAngle))

ggplot(diamonds,aes(x=clarity,fill=color))+
  geom_bar(width=1.0,colour="black",size=0.25)+
  coord_polar(theta = "x",start=0)+
  scale_fill_brewer(palette="Reds")+
  guides(fill=guide_legend(reverse=TRUE,title="Color"))+
  ylim(c(-2000,12000))+
  theme_light()
  # theme( panel.background = element_blank(),
  #        panel.grid.major = element_line(colour = "grey80",size=.25),
  #        axis.text.y = element_text(size = 12,colour="black"),
  #        axis.line.y = element_line(size=0.25),
  #        axis.text.x=element_text(size = 13,colour="black",angle = myAngle))


