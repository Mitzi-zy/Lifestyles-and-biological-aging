library(forestplot)
library(tidyverse)

tiff(file ="分层-连续性.tiff",width =2200,height = 2600,units = "px",res =300)
windowsFonts(myFont1 = windowsFont("Times New Roman"))
forestplot(labeltext = as.matrix(data_fig[,c("strata_index","n","CI")]),#显示的文本
           mean = as.numeric(data_fig[,2]), #误差条的均值
           lower = as.numeric(data_fig[,3]), #误差条的下界
           upper = as.numeric(data_fig[,4]), #误差条的上界
           is.summary=c(T,F,rep(c(T,F,F,F),6)),
           zero = 0, #显示y=0的垂直线
           xlog=FALSE, #x轴的坐标不取对数
           xticks = c(-0.5,0,0.5),
           col=fpColors(box="#08519C",summary="#8B008B",lines="#08519C",zero="#08519C"),
           #col=fpColors(box='black',summary='black',lines = 'black',zero = 'black'), 
           lty.ci = 7,   # 误差条的线的线型
           lwd.ci = 2,   # 误差条的线的宽度
           lwd.zero = 1,  #设置参考线的粗细
           #boxsize = as.numeric(data_fig[,6]),
           ci.vertices.height = 0, # # 误差条末端的长度
           txt_gp = fpTxtGp(label = gpar(cex = 1.2, fontfamily="myFont1"), # 设置文本字体大小
                            ticks = gpar(cex = 0.8), # 设置坐标轴刻度大小
                            xlab = gpar(cex = 1.45)), #文本大小设置
           lineheight = "auto", #线的高度
           #xlab = varname, #x轴的标题
           #title = "mmHg per 10 kg/m2 (95% CI)",
           new_page = FALSE,
           align = c(1,3,3),
           boxsize = 0.25,
           graph.pos = 3)
dev.off()

##二分类
tiff(file ="分层-二分类.tiff",width =2200,height = 2600,units = "px",res =300)
windowsFonts(myFont1 = windowsFont("Times New Roman"))
forestplot(labeltext = as.matrix(data_fig[,c("strata_index","n","CI_c")]),#显示的文本
           mean = as.numeric(data_fig[,7]), #误差条的均值
           lower = as.numeric(data_fig[,8]), #误差条的下界
           upper = as.numeric(data_fig[,9]), #误差条的上界
           is.summary=c(T,F,rep(c(T,F,F,F),6)),
           zero = 0, #显示y=0的垂直线
           xlog=FALSE, #x轴的坐标不取对数
           xticks = c(-0.5,0,0.5),
           col=fpColors(box="#08519C",summary="#8B008B",lines="#08519C",zero="#08519C"),
           #col=fpColors(box='black',summary='black',lines = 'black',zero = 'black'), 
           lty.ci = 7,   # 误差条的线的线型
           lwd.ci = 2,   # 误差条的线的宽度
           lwd.zero = 1,  #设置参考线的粗细
           #boxsize = as.numeric(data_fig[,6]),
           ci.vertices.height = 0, # # 误差条末端的长度
           txt_gp = fpTxtGp(label = gpar(cex = 1.2, fontfamily="myFont1"), # 设置文本字体大小
                            ticks = gpar(cex = 0.8), # 设置坐标轴刻度大小
                            xlab = gpar(cex = 1.45)), #文本大小设置
           lineheight = "auto", #线的高度
           #xlab = varname, #x轴的标题
           #title = "mmHg per 10 kg/m2 (95% CI)",
           new_page = FALSE,
           align = c(1,3,3),
           boxsize = 0.25,
           graph.pos = 3)
dev.off()
