library(qgcomp)
library(tidyverse)
library(ggplot2)
##figure 3 QGC作图
q1 <- ggplot(data = qgc_result , mapping = aes(x =weight ,y = reorder(exposure , weight) ,colour = group, fill = group))+
  # scale_colour_manual(values = c("#DEB2AE","#BEBEBE","#B2E0D6"))+
  # scale_fill_manual(values = c("#DEB2AE","#BEBEBE","#B2E0D6"))+
  scale_colour_manual(values = c("healthy" = "#08519C","unhealthy"="#E18727FF","non-significant"= "#B8B8B8FF"))+
  scale_fill_manual(values = c("healthy" = "#08519C","unhealthy"="#E18727FF","non-significant"= "#B8B8B8FF"))+
  geom_col()+  #条形图
  ylab(NULL)+
  scale_x_continuous(limits=c(0,0.45),breaks=round(seq(0,0.40,0.1),2))+
  xlab(label = "Negative weights")+
  #guides(colour="none" ) +#删除其中一个图例
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "#404040"))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="serif"))

q3_1 <- ggplot(data = qgc_result , mapping = aes(x =weight ,y = reorder(exposure , weight) ,colour = group, fill = group))+
  # scale_colour_manual(values = c("#DEB2AE","#BEBEBE","#B2E0D6"))+
  # scale_fill_manual(values = c("#DEB2AE","#BEBEBE","#B2E0D6"))+
  scale_colour_manual(values = c("Healthy" = "#08519C","Non-significant"= "#B8B8B8FF"))+
  scale_fill_manual(values = c("Healthy" = "#08519C","Non-significant"= "#B8B8B8FF"))+
  geom_col()+  #条形图
  ylab(NULL)+
  scale_x_continuous(limits=c(0,0.45),breaks=round(seq(0,0.40,0.1),2))+
  xlab(label = "Negative weights")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "#404040"))+
  theme(text=element_text(size=16,  family="serif"))