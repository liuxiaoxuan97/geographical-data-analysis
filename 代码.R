#加载一些必要的包
#地图处理和制作
library(raster)
library(rgdal)
library(tidyverse)
library(mapproj)
library(jpeg)
library(grid)
library(maptools)
library(sp)
#读入外部数据
library(readxl)
# 数据分组 取色
library(RColorBrewer)
library(classInt) 

setwd("F:/")
mland<-rgdal::readOGR("map/CHN_adm2.shp")
#分省及全国地图
guo<-rgdal::readOGR("map/国家.shp")
lguo<-rgdal::readOGR("map/中国线.shp")
l9<-rgdal::readOGR("map/九段线.shp")
sheng<-rgdal::readOGR("map/中国政区.shp")
sh<-rgdal::readOGR("map/省会城市.shp")
#合并地理单元数量
china_map<-mland

library(plyr)
library(readxl)
library(data.table)
#师兄给的csv在我的R 里面始终是乱码，可能是windows设置有问题，mac的话也许可以用下面的代码读成中文
bias<-read.csv("map/new_ensemble2.csv",encoding='UTF-8',header=FALSE)
#如果还是不行就 先打开成记事本，另存编码方式为ANSI,再用csv打开就好了
bias<-read.csv("map/new_ensemble2.csv",header=FALSE)
names(bias)<-c("name","bias")
mydat <- read_excel("map/市级平均数据.xlsx", sheet = "Average")
mydata<-join(bias,mydat,by="name")
names(mydata)<-c("NAME_11","bias","prov","NAME_2")


# 地图统计单元数据中ID_2,NL_NAME_2
x <- china_map@data
# ID加上行id,这个行id-与地理元素单元list的顺序一致
xs <- data.frame(id=row.names(x),x)
xs[,"ID_2"] <- xs[,"id"]
data<-plyr::join(xs,mydata,by="NAME_2")

data$inde<-paste(data$NAME_11,data$prov,data$NAME_2)
data_uni<-data[!(duplicated(data$inde)),]
summary(data_uni$bias)
#离散颜色标度分割：
qa<-c(-1,-0.02,-0.005,-0.00009,0.00009,0.005,0.02,1)
data_uni$b2004<-0-data_uni$bias
summary(data1$b2004)
data_uni$biasss<-cut(data_uni$b2004,qa,labels= c(">2%","0.5%-2%","0%-0.5%","0","-0.5-0%","-2%--0.5%","<-2%"),include.lowest = TRUE)
table(data_uni$biasss)
# 地图元素单元数据，有list 顺序信息，即前面的id信息
map_data <- fortify(china_map)
map_data <- plyr::join(map_data, data1, type ="full")
p <- ggplot()+
  geom_polygon(data=map_data,aes(long,lat,group=group,fill=biasss),colour=NA,size=0.25) + #地区
  geom_polygon(data=sheng,aes(long,lat,group=group,fill=NA),colour="black",size=0.25) + # 省界
  scale_fill_brewer(palette="RdYlGn")+
  coord_map() +  
  theme(
    legend.position=c(0.1, 0.20),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_text(size=rel(1.5)),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background=element_rect(I(0),linetype=0)
   )+guides(fill=guide_legend(title=NULL,reverse=TRUE))
#  theme_nothing() 
plot2 <- function(theplot, name, ...) {
  name <- paste0(name, ".png")
  png(filename=name,width = 960, height = 960, units = "px", pointsize = 12)
  print(theplot)
  dev.off()
} #plotting function-
plot2(p, name="地图zzz")

