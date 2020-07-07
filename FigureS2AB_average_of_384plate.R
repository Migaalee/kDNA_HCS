library(platetools)
library(ggplot2)
library(dplyr)
nucleiall<-read.csv("nucleicounts.csv")

#Group by wells

nucleiall_grouped_well<-group_by(nucleiall,nucleiall$Metadata_well)


#Summary of mean, median and sd of individual wells before and after normalization

summarise_wells_before_norm<-summarise(nucleiall_grouped_well,
                                       mean=mean(Count_Nuclei), 
                                       median=median(Count_Nuclei),
                                       sd=sd(Count_Nuclei))


summarise_wells_after_norm<-summarise(nucleiall_grouped_well,
                                       mean=mean(hc_Count_Nuclei_normalized), 
                                       median=median(hc_Count_Nuclei_normalized),
                                       sd=sd(hc_Count_Nuclei_normalized))
#library(viridis)
library(ggplot2)
library(platetools)
library(plater)
library(pheatmap)

#make a dataframe for heatmap using median

df_before <- data.frame(vals = summarise_wells_before_norm$median,
                  well = num_to_well(1:384, plate = 384))

df_after <- data.frame(vals = summarise_wells_after_norm$median,
                        well = num_to_well(1:384, plate = 384))

#Plate heatmap using platetools library
(heat_before<-raw_map(df_before$vals,df_before$well,plate=384)
  +geom_tile(aes(fill=df_before$vals))
  + labs(fill = "Number of nuclei \n")
  +theme_bw() 
  + theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.title = element_text(size = 14,hjust = 0.5,face = "italic",family = "sans"),
          legend.text = element_text(size = 12,family = "sans",color = "black"),
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(1,"cm"),
          axis.text = element_text(size = 10,family = "sans",color="black"),
          axis.ticks = element_blank(),
          panel.border = element_blank())
  + theme(legend.position = c(0.345, -0.055),
          legend.direction = "horizontal")
  + scale_fill_gradient(low = "#0072B2", high = "#D55E00"))



(heat_after<-raw_map(df_after$vals,df_after$well,plate=384)
  +geom_tile(aes(fill=df_after$vals))
  +theme_bw() 
  + labs(fill = "Normalised number of nuclei \n")
  + theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.title = element_text(size = 14,hjust = 0.5,face = "italic",family = "sans"),
          legend.text = element_text(size = 12,family = "sans",color = "black"),
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(1,"cm"),
          axis.text = element_text(size = 10,family = "sans",color="black"),
          axis.ticks = element_blank(),
          panel.border = element_blank())
  + theme(legend.position = c(0.43, -0.08),
          legend.direction = "horizontal")
  + scale_fill_gradient(low = "#0072B2", high = "#D55E00"))

ggsave("Fig2Abefore.tiff",
       heat_before,
       units="cm",
       width=17.3,   #1 column
       height=12,
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw",  # PLOS One 44MB w/out compression, 221KB w/compression
       family="Arial",  # ggplot default.  Could set to others depending on journal
       type="cairo")

ggsave("Fig2Bafter.tiff",
       heat_after,
       units="cm",
       width=17.3,   #1 column
       height=12,
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw",  # PLOS One 44MB w/out compression, 221KB w/compression
       family="Arial",# ggplot default.  Could set to others depending on journal
       type="cairo")

library(cowplot)

(combined<-plot_grid(heat_before, heat_after, align= "h",labels = c("A", "B"), nrow = 2, ncol = 1))


ggsave("Fig2AB.tiff",
       combined,
       units="cm",
       width=15.2,   #1 column
       height=23,
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw",  # PLOS One 44MB w/out compression, 221KB w/compression
       family="Arial",# ggplot default.  Could set to others depending on journal
       type="cairo")






