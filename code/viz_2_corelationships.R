#' data vis course preparing
#' date: 0908,2022
#' updated: 0920,2022
#' process: 
#' - stop at sankey diagram
#' 

# why we prefer bar or histogram to pie

library(ggplot2)
library(GGally)
library(RColorBrewer)

# error analysis ----------------------------------------------------------

library(ggplot2)

mydata<-read.csv("./textbook/ch04/Residual_Analysis_Data.csv",stringsAsFactors=FALSE)

fit <- lm(y2 ~ x, data = mydata)
mydata$predicted <- predict(fit)   # Save the predicted values
mydata$residuals <- residuals(fit) # Save the residual values
mydata$Abs_Residuals<-abs(mydata$residuals)  #

ggplot(mydata, aes(x = x, y = y2)) +
  geom_point(aes(fill =Abs_Residuals, size = Abs_Residuals),shape=21,colour="black") + # size also mapped
  scale_fill_continuous(low = "black", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_point(aes(y = predicted), shape = 1) +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +# > Color AND size adjustments made here...
  guides(fill = guide_legend((title="Rresidual")),
         size = guide_legend((title="Rresidual")))+
  ylim(c(0,150))+
  xlab("X-Axis")+
  ylab("Y-Axis")+
  theme(text=element_text(size=15,face="plain",color="black"),
        axis.title=element_text(size=10,face="plain",color="black"),
        axis.text = element_text(size=10,face="plain",color="black"),
        legend.position = "right",
        legend.title  = element_text(size=13,face="plain",color="black"),
        legend.text = element_text(size=10,face="plain",color="black"),
        legend.background = element_rect(fill=alpha("white",0)))


#4.1.5----------------------------------------------------------

d<-mydata
fit <- lm(y5 ~ x+I(x^2), data = d)

# Obtain predicted and residual values
d$predicted <- predict(fit)   # Save the predicted values
d$residuals0 <- residuals(fit) # Save the residual values
d$Residuals<-abs(d$residuals0 )

ggplot(d, aes(x = x, y = y5)) +
  geom_smooth(method = "lm",formula = y ~ x+I(x^2), se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(fill =Residuals, size = Residuals),shape=21,colour="black") + # size also mapped
  scale_fill_continuous(low = "black", high = "red") +
  #scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  geom_point(aes(y = predicted), shape = 1) +  # Size legend also removed
  #ylim(c(0,150))+
  xlab("X-Axis")+
  ylab("Y-Axis")+
  geom_point(aes(y = predicted), shape = 1) +
  guides(fill = guide_legend((title="Rresidual")),
         size = guide_legend((title="Rresidual")))+
  theme(text=element_text(size=15,face="plain",color="black"),
        axis.title=element_text(size=10,face="plain",color="black"),
        axis.text = element_text(size=10,face="plain",color="black"),
        legend.position = "right",
        legend.title  = element_text(size=13,face="plain",color="black"),
        legend.text = element_text(size=10,face="plain",color="black"),
        legend.background = element_rect(fill=alpha("white",0)))


# 4.1.7-a-histogram -------------------------------------------------------
library(ggplot2)
library(RColorBrewer)
library(scales)
x <- rnorm(250 , mean=10 , sd=1) 

#x <-sample(1:20, 250, replace = TRUE) 

step<-0.2
breaks<- seq(min(x)-step,max(x)+step,step)#"sturges"

hg <- hist(x, breaks = breaks , plot = FALSE) # Make histogram data but do not plot

bins <- length(hg$counts) # How many bin categories are needed?
yvals <- numeric(0)                  # A blank variable to fill in
xvals <- numeric(0) 
for(i in 1:bins) {                  # Start a loop
  yvals <- c(yvals, hg$counts[i]:0)  # Work out the y-values
  xvals <- c(xvals, rep(hg$mids[i], hg$counts[i]+1))  # Work out x-values
}    # End the loop
# End the loop
dat <- data.frame(xvals, yvals)  # Make data frame of x, y variables
dat <- dat[yvals > 0, ]          # Knock out any zero y-values

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)



#------------------------------------??״tile---------------------------------------
ggplot(dat, aes(x=xvals,y=yvals,fill=yvals))+
  geom_tile(colour="black")+
  scale_fill_gradientn(colours=colormap)+
  ylim (0, max(yvals)*1.3)+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.9,0.75)
  )

#-----------------------------------ԲȦpoint-----------------------------------------
ggplot(dat, aes(x=xvals,y=yvals,fill=yvals))+
  geom_point(colour="black",shape=21,size=4)+
  scale_fill_gradientn(colours=colormap)+
  ylim (0, max(yvals)*1.3)+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.background = element_blank(),
    legend.position=c(0.9,0.75)
  )


#---------------------------------------------Q-Q图---------------------------------------------------

set.seed(183)
x <- rnorm(250 , mean=10 , sd=1)
# # Compare the numbers sampled with rnorm() against normal distribution
qqnorm(x)
qqline(x)
abline(h = c(7:13)*1/2,col = "gray100",lty =1) # boe cpi target
abline(v = c(-3:3)*1/2,col = "gray100", lty = 1) # 2 year line
abline(h = c(7:13)*1,col = "gray100",lty =1) # boe cpi target
abline(v = c(-3:3),col = "gray100", lty = 1) # 2 year line
box(col="white")

#--------------------------------------------CircStats--P-P plot---------------------------------------------------
library(CircStats)
pp.plot(x)
abline(h = c(0:10)*1/10,col = "gray100",lty =1) # boe cpi target
abline(v = c(0:10)*1/10,col = "gray100", lty = 1) # 2 year line
abline(h = c(0:10)*2/10,col = "gray100",lty =1) # boe cpi target
abline(v = c(0:10)*2/10,col = "gray100", lty = 1) # 2 year line
box(col="white")

#---------------------------------------------ggplot2-Q-Q plot---------------------------------------------------
library(ggplot2)

df <-data.frame(x=rnorm(250 , mean=10 , sd=1))
ggplot(df, aes(sample = x))+
  geom_qq()  
#geom_qq_line(fill = "#00AFBB",size=3)


#---------------------------------------------ggqqplot-Q-Q图---------------------------------------------------
library(ggpubr)
x <- rnorm(250 , mean=10 , sd=1)
ggqqplot(x,shape=21,fill="white",colour="black",
         add = "none",
         ggtheme = ggplot2::theme_grey())


#-----------------------------4.1.8 transparent scatterplot------------------------------
mydata<-read.csv("./textbook/ch04/HighDensity_Scatter_Data.csv",stringsAsFactors=FALSE)

ggplot(data = mydata, aes(x,y)) +
  geom_point( colour="black",alpha=0.1)+
  labs(x = "Axis X",y="Axis Y")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",hjust=.5,color="black"),
    legend.position="none"
  )

#-----------------------------------4.1.8 kmeans-----------------------------

kmeansResult<- kmeans(mydata, 2, nstart = 20)

mydata$cluster <- as.factor(kmeansResult$cluster)

ggplot(data = mydata, aes(x,y,color=cluster)) +
  geom_point( alpha=0.2)+
  scale_color_manual(values=c("#00AFBB",  "#FC4E07"))+
  labs(x = "Axis X",y="Axis Y")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black"),
    legend.background=element_blank(),
    legend.position=c(0.85,0.15)
  )

#---------------------------------4.1.8 K-mean with oval --------------------------------------------------

ggplot(data = mydata, aes(x,y,color=cluster)) +
  geom_point (alpha=0.2)+
  # 绘制透明度为0.2 的散点图
  stat_ellipse(aes(x=x,y=y,fill= cluster), geom="polygon", level=0.95, alpha=0.2) +

  scale_color_manual(values=c("#00AFBB","#FC4E07")) +#使用不同颜色标定不同数据类别
  scale_fill_manual(values=c("#00AFBB","#FC4E07"))+  #使用不同颜色标定不同椭类别
  labs(x = "Axis X",y="Axis Y")+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black"),
    legend.background=element_blank(),
    legend.position=c(0.85,0.15)
  )


# 4.1.10 buble plot -------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(ggrepel)
attach(mtcars)

#-------------------------------(c) buble plot with label-------------------------------------------------------------

ggplot(data=mtcars, aes(x=wt,y=mpg))+
  geom_point(aes(size=disp,fill=disp),shape=21,colour="black",alpha=0.8)+
  scale_fill_gradient2(low="#377EB8",high="#E41A1C",midpoint = mean(mtcars$disp))+
  geom_text_repel(label = disp )+
  scale_size_area(max_size=12)+
  guides(size = guide_legend((title="Value")),
         fill = guide_legend((title="Value")))+
  theme(
    legend.text=element_text(size=10,face="plain",color="black"),
    axis.title=element_text(size=10,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.position = "right"
  )


#--------------------------(d) squared bubble--------------------------------------------------
ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(size=disp,fill=disp),shape=22,colour="black",alpha=0.8)+
  scale_fill_gradient2(low=brewer.pal(7,"Set1")[2],high=brewer.pal(7,"Set1")[1],
                       midpoint = mean(mtcars$disp))+
  scale_size_area(max_size=12)+
  guides(fill = guide_legend((title="Value")),
         size =  guide_legend((title="Value")))+
  theme(
    text=element_text(size=15,color="black"),
    plot.title=element_text(size=15,family="myfont",face="bold.italic",color="black")#,
    #legend.position=c(0.9,0.05)
  )


# Surface fitting diagram -------------------------------------------------
library(plot3D)
library(gridExtra)
library(RColorBrewer)

colormap<-colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(100)

#--------------------------------------------------polynomial------------------------------------------
mydata <- read.csv("./textbook/ch04/Surface_Data.csv", sep= ",", header=T)

#z=f(x,y)=a+bx+cy+dxx+eyy
x <- mydata$x
y <- mydata$y
z <- mydata$z
x2<-x*x
y2<-y*y
poly_z <- lm(z ~ x + y +x2+y2)
print(poly_z)

#30X30 grid(x, y), get the value by the fitted formula
N<-30
xmar <- seq(min(x),max(x),(max(x)-min(x))/N)
ymar <- seq(min(y),max(y),(max(y)-min(y))/N)
Grid_xy<-expand.grid(list(x=xmar,y=ymar))
Grid_xy$x2<-Grid_xy$x*Grid_xy$x
Grid_xy$y2<-Grid_xy$y*Grid_xy$y
Grid_z <- predict.lm(poly_z, newdata=Grid_xy)  


pred_z<-matrix(Grid_z, length(xmar), length(ymar))


persp3D (xmar, ymar, pred_z,
         theta = 150, phi = 40, d=3, 
         col = colormap, 
         scale = TRUE, border = "black", 
         bty = "f",box = TRUE,ticktype = "detailed",
         ylab = "0-60 mph (sec)", 
         xlab = "Gax Mileage (mpg)",
         zlab="Power (KW)",
         clab="Power (KW)",
         zlim=c(20,180),
         add=TRUE,
         colkey = list(length = 0.5, width = 1))


fitpoints <- predict(poly_z) 

scatter3D(z = z, x = x, y = y, pch = 21, cex = 1, 
          theta = 150, phi = 40, d=3,ticktype = "detailed",
          col = colormap,
          surf = list(x = xmar, y = ymar, z = pred_z,border = "black",shade=0,ffit = fitpoints), # fit参数增加预测值与真实值之间的连线
          bty = "f", col.panel = NA,
          xlab = "0-60 mph (sec)", 
          ylab = "Gax Mileage (mpg)",
          zlab="Power (KW)",
          clab="Power (KW)",
          zlim=c(20,180),colkey = list(length = 0.5, width = 1))# col.panel = NA则panel透明

#-#--------------------------------------------------loess回归式拟合------------------------------------------
mydata <- read.csv("./textbook/ch04/Surface_Data.csv", sep= ",", header=T)

x <- mydata$x
y <- mydata$y
#z <- mydata$z


elev.loess <- loess(z ~ x * y, mydata,span=0.95)
print(elev.loess)


xmar <- seq(min(x),max(x),(max(x)-min(x))/30)
ymar <- seq(min(y),max(y),(max(y)-min(y))/30)
# get fitted (interpolated) values
elev.interp <- predict(elev.loess, expand.grid(list(x=xmar,y=ymar)))

pred_z<-matrix(elev.interp, length(xmar),length(ymar))

# the theme of the grid
persp3D (xmar, ymar, pred_z,
         theta = 150, phi = 40, d=3, 
         col = colormap, 
         scale = TRUE, border = "black", 
         bty = "f",box = TRUE,ticktype = "detailed",#nticks=5,
         xlab = "0-60 mph (sec)", 
         ylab = "Gax Mileage (mpg)",
         zlab="Power (KW)",
         zlim=c(20,180))


fitpoints <- predict(elev.loess ) 

scatter3D(z = z, x = x, y = y, pch = 21, cex = 1, 
          theta = 150, phi = 40, d=3,ticktype = "detailed",
          col = colormap,
          surf = list(x = xmar, y = ymar, z = pred_z,border = "black",shade=0,ffit = fitpoints), # fit参数增加预测值与真实值之间的连线
          bty = "f", col.panel = NA,
          ylab = "0-60 mph (sec)", 
          xlab = "Gax Mileage (mpg)",
          zlab="Power (KW)",
          zlim=c(20,180))# col.panel = NA, transparent



# waterfall/ mountain chart ---------------------------------------------------------

library(ggplot2)
library(RColorBrewer)  

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

mydata0<-read.csv("./textbook/ch04/Facting_Data.csv",check.names =FALSE)

N<-ncol(mydata0)-1
labels_Y<-colnames(mydata0)[1:N+1]
colnames(mydata0)<-c("x",seq(1,N,1))
mydata<-data.frame(x=numeric(),y=numeric(),variable=character()) #创建空的Data.Frame

for (i in 1:N){
  newdata<-data.frame(spline(mydata0[,1],mydata0[,i+1],n=300,method= "natural"))
  newdata$variable<-colnames(mydata0)[i+1]
  mydata<-rbind(mydata,newdata)
}

Step<-5
mydata$offest<--as.numeric(mydata$variable)*Step
mydata$V1_density_offest<-mydata$y+mydata$offest

p<-ggplot()
for (i in 1:N){
  p<-p+ geom_linerange(data=mydata[mydata$variable==i,],aes(x=x,ymin=offest,ymax=V1_density_offest,group=variable,color=y),size =1, alpha =1) +
    geom_line(data=mydata[mydata$variable==i,],aes(x=x, y=V1_density_offest),color="black",size=0.5)
}
#ggplot() + 
#  geom_linerange(aes(x=x,ymin=offest,ymax=V1_density_offest,group=variable,color=y),mydata,size =1, alpha =1) +
p+scale_color_gradientn(colours=colormap)+
  #geom_line(aes(x, V1_density_offest,group=variable),mydata,color="black")+
  scale_y_continuous(breaks=seq(-Step*N,-Step,Step),labels=rev(labels_Y))+
  xlab("Time")+
  ylab("Class")+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=13),
    plot.title=element_text(size=15,hjust=.5),
    legend.position="right"
  )

#------------------------------------------------------------------------------------------------------

ggplot() + 
  geom_ribbon(aes(x, ymin=offest,ymax=V1_density_offest, fill=variable),mydata, alpha=1,colour=NA)+
  geom_line(aes(x, V1_density_offest, color=variable,group=variable),mydata, color="black")+
  scale_y_continuous(breaks=seq(-40,-5,5),labels=rev(labels_Y))+
  theme_classic()+
  theme(
    panel.background=element_rect(fill="white",colour=NA),
    panel.grid.major.x = element_line(colour = "grey80",size=.25),
    panel.grid.major.y = element_line(colour = "grey60",size=.25),
    axis.line = element_blank(),
    text=element_text(size=15),
    plot.title=element_text(size=15,hjust=.5),#family="myfont",
    legend.position="none"
  )


# correlation coefficient ---------------------------------------------


# Venn Diagram ------------------------------------------------------------

library(VennDiagram)
library(RColorBrewer)
venn.diagram(list(B = 1:1800, A = 1571:2020,c=500:1100),fill = c(brewer.pal(7,"Set1")[1:3]),
             alpha = c(0.5, 0.5,0.5), cex = 2,
             cat.cex=3,cat.fontface = 4,lty =2,
             resolution =300, filename = "venn.tiff")

# chord diagram -----------------------------------------------------------

library(circlize) 
library(RColorBrewer) 
set.seed(999) 
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3) 
colnames(mat) = paste0("E", 1:6) 
df = data.frame(from = rep(rownames(mat), 
                           times = ncol(mat)), 
                to = rep(colnames(mat), 
                         each = nrow(mat)), 
                value = as.vector(mat), 
                stringsAsFactors = FALSE) 
chordDiagram(df,
             grid.col = brewer.pal(9,"Set1")[1:9],
             link.border="grey") # use data.frame to draw chord diagram 
circos.clear() 
chordDiagram(mat,
             grid.col = brewer.pal(9,"Set1")[1:9],
             link.border="grey") # using matrix
circos.clear()


# 4.13 Sankey diagram -----------------------------------------------------

library(ggalluvial) # alluvial
library(ggplot2) 
data(vaccinations) 
levels(vaccinations$response) <- rev(levels(vaccinations$response)) 
ggplot(vaccinations, 
       aes(x = survey, 
           stratum = response, 
           alluvium = subject, 
           weight = freq, 
           fill = response, 
           label = response)) + 
  geom_flow(alpha = 0.7,
            color = "darkgray") + 
  geom_stratum(alpha = 1) + 
  geom_text(stat = "stratum", 
            size = 3.5) + 
  theme_classic()+ #coord_flip() + 
  theme(legend.position = "none", 
        axis.text.x =element_text(color="black",
                                  size=12), 
        axis.title.x = element_blank(), 
        axis.text.y =element_blank(), 
        axis.line = element_blank(), 
        axis.ticks =element_blank() ) +
  ggtitle("Vaccination Survey responses at three points in time")
