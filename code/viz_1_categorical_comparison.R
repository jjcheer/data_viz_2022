#' data vis course preparing
#' @date     0908,2022
#' @updated  0920,2022
#' @progress: done
#' - done with tidytext
#' - stop at word cloud, tidytext
#' 

# why we prefer bar or histogram to pie
library(tidyverse)
library(RColorBrewer)

install.packages("RColorBrewer")
install.packages("jiebaR")
install.packages("wordcloud2")

theme_set(theme(text = element_text(family = "Songti SC")))

rm(list = ls())

data("airquality")

airquality %>% 
  ggplot(aes(Wind,Temp,color = (Temp > 90))) +
  geom_point() +
  geom_smooth(method = "lm")


library(rvest)
library(tidyverse)
library(jiebaR)
library(wordcloud2)
install.packages("worclouds")

news <- paste(scan("/Users/jameschen/Documents/04_Learning/R/RinBI/code/data/sina_2022-11-08_3.txt", what = character(0),sep = "",encoding = "UTF-8"), collapse = " ") 
news

segment(news,worker())

stop_words_chinese <- tibble(
  words = c("的",
                        "例","是","了","中国","在"))

### construct your own dictionary, and add to the worker() as a argument.

news_words <- tibble(segment(news,worker())) %>% rename(words = `segment(news, worker())`)

news_clean <- anti_join(news_words,stop_words_chinese)


wordcloud2(freq(news_clean$words),shape = "star")

# stop words
library(jiebaR)
library(wordcloud2)
#install.packages("wordcloud2")

load("")

wordcloud2(freq(segment(news,worker())))


wordcloud2(freq(segment(news,worker())))


# ggplot2 -----------------------------------------------------------------

data("airquality")

airquality %>% 
  ggplot(aes(Wind,Temp)) +
  geom_point()

ggplot(airquality,aes(Wind,Temp)) + geom_point() +
  geom_abline(slope = 5,intercept = 3,color = "red",size = 2,lty = "dashed") +
  geom_smooth(method = "lm",se=FALSE)
# color -------------------------------------------------------------------


display.brewer.all()

my_color <- brewer.pal(5,"Set3")

data("airquality")

ggplot(airquality, aes(Wind, Temp,color = as.factor(Month))) +
  geom_point() +
  scale_colour_brewer(palette = "Set1")


# bar graph with lj data --------------------------------------------------
## read_csv()
## read_xlsx()
load("data/lj_sh_2019.Rdata")

load("/Users/jameschen/Documents/02_Teaching/06_r4ds/slides/data/lj_sh_2019.RData")

wordcloud2(freq(segment(lj$property_name,worker())))


segment(lj$property_name,worker())

library(rvest)


news_title <- read_html("https://www.sina.com.cn/") %>%
  html_elements("li:nth-child(1) .linkNewsTopBold+ .linkNewsTopBold") %>% 
  html_text2()

news_link <- read_html("https://www.sina.com.cn/") %>%
  html_elements("li:nth-child(1) .linkNewsTopBold+ .linkNewsTopBold") %>% 
  html_attr("href")

news_content <- read_html(news_link) %>% html_elements("#article p") %>% html_text2()


wordcloud2(freq(segment(news_content,worker())))
theme_set(theme(text = element_text(family = "Songti SC")))
# draw the wordcloud on the property_name

#wordcloud2(freq(segment(lj$property_name,worker())))

ggplot(lj, aes(line)) +
  geom_bar() +
  theme(text = element_text(family = "Songti SC"),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1,
                                   size = 9))

  coord_polar()

  facet_wrap(~ building_style) +
  geom_hline(yintercept =0.5, color = "red",lty = "dashed",size =1) +
  theme(text = element_text(family = "Songti SC"),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 6))
ggsave("lines.jpeg")


# Beginners Guide on Data Viz with R --------------------------------------

## 1. the R and R studio
## 2. the ggplot2 package
## 3. plotting with ggplot2

# load pkgs ---------------------------------------------------------------

library(tidyverse)

#---------------------------one variable, bar graph and histogram----------------------------------------------------
mydata<-data.frame(cut=c("Fair","Good","Very Good","Premium","Ideal"),
                   price=c(4300,3800,3950,4700,3500))
mydata %>% 
  ggplot(aes(cut,price))


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

if(!dir.exists("img")) dir.create("img")
ggsave("img/test1.jpg")


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
## the difference between geom_bar and geom_col,
## the stat = "identity" in geom_bar


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

# construct the x-axis end 
for (i in 1:5){
  mydata$xmax[i]<-sum(mydata$scale[1:i])
}
# construct the x label
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
# not so elegant, polishing after data manipulation course

ggplot(mydata, aes(sum, City)) +
  geom_segment(aes(x=0, 
                   xend=sum,
                   y=City, 
                   yend=City)) +
  geom_point(shape=21,size=3,colour="black",fill="#FC4E07") +
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black")
  )
# lollipops on line counts of lj
theme_set(theme(text = element_text(family = "Songti SC")))
lj %>% #pipe
  count(line) %>%
  ggplot(aes(n,line)) +
  geom_segment(aes(x = 0, 
                xend = n,
                y = line,
                yend = line)) +
  geom_point(shape=21,size=3,colour="black",fill="#FC4E07")

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


# circle bar ---------------------------------------------------------------


df <- data.frame(item=rep(LETTERS[1:10], 5), 
                 score=rep(letters[1:5], each=10), 
                 value=rep((1:5), each=10) + rnorm(50, 0, .5))


myAng <-seq(-20,-340,length.out =10)
ggplot(data=df,aes(item,value,fill=score))+
  geom_bar(stat="identity", color="black", position=position_dodge(),width=0.7,size=0.25)+
  coord_polar(theta = "x",start=0) +
  ylim(c(-3,6))+
  scale_fill_brewer(palette="YlGnBu")+
  theme_light()
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour = "grey80",size=.25),
         axis.text.y = element_text(size = 12,colour="black"),
         axis.line.y = element_line(size=0.25),
         axis.text.x=element_text(size = 13,colour="black",angle = myAng))


# Polar coord. polar range ------------------------------------------------------------

  library(viridis)
  library(scales)
  
  df<-read.csv("./textbook/ch03/PloarRange_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)
  df$date<-as.Date(df$date)
  
  myAngle <-seq(-20,-340,length.out = 12)
  
  ggplot(df, aes(date,
                 ymin = min.temperaturec,
                 ymax = max.temperaturec,
                 color = mean.temperaturec)) + 
    geom_linerange(size = 1.3, alpha = 0.75) +
    scale_color_viridis("Temperature", option = "D") +
    scale_x_date(labels = date_format("%m"), breaks = breaks_width("1 month"))  +
    ylim(-10, 35) + 
    coord_polar() + 
    theme_light() 
    # theme( panel.background = element_blank(),
    #        panel.grid.major = element_line(colour = "grey80",size=.25),
    #        axis.text.y = element_text(size = 12,colour="black"),
    #        axis.line.y = element_line(size=0.25),
    #        axis.text.x=element_text(size = 13,colour="black",angle = myAngle))
    # 


# radar graph -------------------------------------------------------------

  #Reference:https://github.com/cardiomoon/ggplot2new/blob/4e50b7dcfee3246a169702f88f7dd46cbf933f4b/coord_radar.R
  
  coord_radar <- function (theta = "x", start = 0, direction = 1) 
  {  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)}  


# radar one variable --------------------------------------------------------

  label_data<-data.frame(car=c("Math" , "English" , "Biology" , "Music" , "R-Coding" ),
                         id=c(1:5) ,
                         value=c(12 , 2 ,14 ,20, 18))
  
  AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)])
  mydata<-rbind(label_data,AddRow)
  
  myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
  
  ggplot() + 
    geom_polygon(data=mydata,aes(x=id, y=value),color = "black", fill=brewer.pal(7,"Set1")[1],alpha=0.1)+
    geom_point(data=mydata,aes(x=id, y=value),size=5,shape=21,color = 'black', fill=brewer.pal(7,"Set1")[1])+
    coord_polar() + 
    ylim(0,22)+
    theme_light()+
    theme(axis.text.x=element_text(size = 11,colour="black"))
  
  ggplot() + 
    geom_polygon(data=mydata,aes(x=id, y=value),color = "black", fill=brewer.pal(7,"Set1")[1],alpha=0.1)+
    geom_point(data=mydata,aes(x=id, y=value),size=5,shape=21,color = 'black', fill=brewer.pal(7,"Set1")[1])+
    coord_polar() + 
    #coord_radar()+  #
    scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
    ylim(0,22)+
    theme_light()
    # theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle))
    # 
  #--------------------------------------------radar, multi-va-------------------------------------------------------
  label_data<-data.frame(
    car=c("biology" , "english" ,"math" ,  "music" , "R-coding" ),
    id=c(1:5) ,
    v1=sample( 0:20,5, replace=T),
    v2=sample( 0:20,5, replace=T)
  )
  
  AddRow<-c(NA,nrow(label_data)+1,label_data[1,ncol(label_data)-1],label_data[1,ncol(label_data)])
  mydata<-rbind(label_data,AddRow)
  
  myAngle<- 360- 360 * (label_data$id-1) /nrow(label_data)  
  
  mydata<-melt(mydata,id=c("car", "id"))
  
  ggplot(data=mydata,aes(x=id, y=value,group=variable,fill=variable)) + 
    geom_polygon(colour="black",alpha=0.1)+
    geom_point(size=4,shape=21,color = 'black')+
    coord_radar()+
    #coord_polar() +
    scale_x_continuous(breaks =label_data$id,labels=label_data$car)+
    theme_bw() +
    ylim(0,22)
# #    theme(axis.text.x=element_text(size = 11,colour="black",angle = myAngle),
#           axis.title=element_text(size=15,face="plain",color="black"),
#           axis.text = element_text(size=12,face="plain",color="black"),
#           panel.grid.major = element_line(color="grey80"),
#           axis.line = element_line(color="black"),
#           axis.ticks =  element_line(color="black"))


# 3.9 word cloud ----------------------------------------------------------

#' 20220921 stop here, will do in tidytext nlp session.
  
  
  library(wordcloud)
  
  Paper1<-paste(scan("./textbook/ch03/Paper1.txt", what = character(0),sep = ""), collapse = " ")  
  Paper2<-paste(scan("./textbook/ch03/Paper2.txt", what = character(0),sep = ""), collapse = " ")  
  
  tmpText<- data.frame(c(Paper1, Paper2),row.names=c("Text1","Text2"))
  
  df_title <- data.frame(doc_id=row.names(tmpText),
                         text=tmpText$c.Paper1..Paper2.)
  
#   ds <- DataframeSource(df_title)
#   corp = Corpus(ds)
#   corp = tm_map(corp,removePunctuation)
#   corp = tm_map(corp,PlainTextDocument)
# #  corp = tm_map(corp,removeNumbers)
# #  corp = tm_map(corp, function(x){removeWords(x,stopwords())})
#   
#   term.matrix <- TermDocumentMatrix(corp)
#   term.matrix <- as.matrix(term.matrix)
#   colnames(term.matrix) <- c("Paper1","Paper2")
  #------------------------------------------------------------------------------------------------------
  # comparison.cloud(term.matrix, max.words=300, random.order=FALSE, rot.per=.15, c(4,0.4), title.size=1.4)
  # 
  # comparison.cloud(term.matrix,max.words=300,random.order=FALSE,colors=c("#00B2FF", "red"))
  # commonality.cloud(term.matrix,max.words=100,random.order=FALSE,color="#E7298A")
  # 
  # 
  # # comparison cloud
  # comparison.cloud(term.matrix, random.order=FALSE, 
  #                  colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
  #                  title.size=1.5, max.words=500)
  # 
  # #------------------------------------------------------------------------------------------------------
  # df<-data.frame(term.matrix)
  # #Colors<-colorRampPalette(rev(brewer.pal(9,'RdBu')))(length(df$Paper1>10))
  # wordcloud(row.names(df) , df$Paper1 , min.freq=10,col=brewer.pal(8, "Dark2"), rot.per=0.3 )
  # 
  # using tidytext pkg
  
  library(tidytext)
  data("stop_words")
  df_freq <- 
  df_title%>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    count(word,sort = TRUE)
  
  df_freq
 
  wordcloud2(df_freq,shape = "star") 
 
  # using lj data for viz
  
library(jiebaR)
library(wordcloud2)
load("/Users/jameschen/Documents/02_Teaching/06_r4ds/slides/data/lj_sh_2019.RData")
  
wk <- worker()

wordcloud2(freq(segment(lj$property_name,wk)))

news <- paste(scan("../data/news.txt", what = character(0),sep = ""), collapse = " ") 
wordcloud2(freq(segment(news,wk)))


# assignment: dumbbell graph on the has_elevator among lines ---------------
lj %>% 
  count(line,has_elevator) %>% 
ggplot(aes(n,fct_reorder(line,-n),fill=factor(has_elevator))) +
  geom_line(aes(group = line)) +
  geom_point(shape=21,size=3,colour="black")+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07","#36BED9"))+
  theme(
    axis.title=element_text(size=13,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title=element_text(size=12,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position = c(0.85,0.12)
  )

