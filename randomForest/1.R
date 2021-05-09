library(microeco)
# use pipe operator in magrittr package
library(magrittr)
# set.seed is used to fix the random number generation to make the results repeatable
set.seed(2021)
# make the plotting background same with the tutorial
library(ggplot2)
theme_set(theme_bw())
otu_table_16S_1<-read.csv("16s.csv",row.names = 1)
#data(sample_info_16S)
sample_info_16S_1<-read.csv("SampleID.csv",row.names = 1)
taxonomy_table_16S_1<-read.csv("tax.csv",row.names = 1)
dataset <- microtable$new(sample_table = sample_info_16S_1, 
                          otu_table = otu_table_16S_1,
                          tax_table = taxonomy_table_16S_1)
#然后为使otu_table，tax_table中的OTU相同，使用tidy_dataset()。
dataset$tidy_dataset()
print(dataset)
dataset$cal_abund()
class(dataset$taxa_abund)
dir.create("taxa_abund")
dataset$save_abund(dirpath = "taxa_abund")
#t1 <- trans_diff$new(dataset = dataset, method = "rf", 
 #                    group = "Group",  rf_taxa_level = "Genus")
t1 <- trans_diff$new(dataset = dataset, method = "rf", 
                     group = "Group",  rf_ntree = 2000,
                     rf_taxa_level = "Genus")
#t1.1<-t1$res_rf
#t1.1<-t1$res_rf
#write.csv(t1.1,"t1.1.csv")
# t1$res_rf 是这个对象的分类结果
# plot the result
t2 <- t1$plot_diff_abund(use_number = 1:30, 
                         only_abund_plot = FALSE)

p<-gridExtra::grid.arrange(t2$p1, t2$p2, ncol=2,
                        nrow = 1, widths = c(2,2))

# 中间的星号代表显著性
ggsave("rf.pdf",p,width = 10,height = 8)
t2.1<-t2$p1$data
colnames(t2.1)<-c("Taxa","MeanDecreaseGini","pvalue")
write.csv(t2.1,"t2.1.csv")
df1<-read.csv("t2.1.csv",row.names = 1)
t2.2<-t2$p2$data
write.csv(t2.2,"t2.2.csv")
df2<-read.csv("t2.2.csv",row.names = 1)
df1$Taxa <- factor(df1$Taxa, 
                   levels = df1$Taxa[order(df1$MeanDecreaseGini,decreasing = F)])
p1 <- ggplot(data = df1, mapping = aes(x=Taxa,y=MeanDecreaseGini)) +
  geom_bar(stat="identity",fill="#1E90FF")+
  coord_flip()+
  theme_bw()+
  theme(panel.border = element_blank(), panel.background=element_rect(fill="white")) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) + #, panel.grid.minor.x = element_blank())
  theme(axis.title = element_text(size = 17), axis.text.y = element_text(size = 10, color = "black")) +
  theme(plot.margin = unit(c(.1, 0, .1, 0), "cm"))
p1
p2 <- ggplot(df2, aes(x=Taxa, y=Mean, color = Group, fill = Group, group = Group)) +
  geom_bar(stat="identity", position = position_dodge(), width = .7) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.45,
                position=position_dodge(.7), color = "black")+ 
  theme_bw() +
  coord_flip() +
  scale_color_manual(values=c("#80B1D3","#FDB462")) +
  scale_fill_manual(values=c("#80B1D3","#FDB462")) +
  ylab("Relative abundance") +
  theme(legend.position = "right") +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(), 
        panel.border = element_blank(), 
        panel.background=element_rect(fill="white")) +
  theme(axis.title = element_text(size = 17)) +
  guides(fill=guide_legend(reverse=TRUE, ncol=1), color = FALSE) +
  theme(axis.title.y=element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.margin = unit(c(.1, 0, .1, 0), "cm"))
p2

plot2_sig_color = "black"
plot2_sig_size = 1.2
Significance <- rev(as.character(cut(t2.1$pvalue, 
                                     breaks=c(-Inf, 0.001, 0.01, 0.05, Inf),
                                     label=c("***", "**", "*", ""))))
p2 <- p2 + scale_x_discrete(labels=Significance) +
  theme(axis.title.y=element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_text(color = plot2_sig_color, 
                                   size = rel(plot2_sig_size))) +
  theme(plot.margin = unit(c(.1, 0, .1, .8), "cm"))
p3 <- gridExtra::grid.arrange(p1, p2, ncol=2, nrow = 1, widths = c(2,2))
p3
ggsave("rf.1.pdf",p3,width = 12,height = 8)

#test






