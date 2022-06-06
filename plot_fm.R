
# data --------------------------------------------------------------------
setwd("~/Documents/data_report_fm/")
pacman::p_load(tidyverse,viridis,ggpubr,cowplot)

Gps <- read_csv("Subjects_66.csv")
data_bold <- read_tsv("group_bold.tsv") %>% 
  filter(bids_name != "sub-001_task-rest_bold" & bids_name != "sub-003_task-rest_bold" & bids_name != "sub-048_task-rest_bold") %>% 
  select(bids_name,fd_mean,tsnr,dvars_nstd) %>% add_column(Group=Gps$gp,.before = 2)

data_T1W <- read_tsv("group_T1w.tsv") %>% 
  filter(bids_name != "sub-001_T1w" & bids_name != "sub-003_T1w" & bids_name != "sub-048_T1w") %>% 
  select(bids_name,snr_total,cnr,efc) %>% add_column(Group=Gps$gp,.before = 2)
data_T2W <- read_tsv("group_T2w.tsv") %>% 
  filter(bids_name != "sub-001_T2w" & bids_name != "sub-003_T2w" & bids_name != "sub-048_T2w") %>% 
  select(bids_name,snr_total,cnr,efc) %>% add_column(Group=Gps$gp,.before = 2)


data_bold$Group <- factor(data_bold$Group,labels = c("HC","FM"))
data_T1W$Group <- factor(data_T1W$Group,labels = c("HC","FM"))
data_T2W$Group <- factor(data_T1W$Group,labels = c("HC","FM"))

# graphs ------------------------------------------------------------------
# B O L D
plot_bold_fd <- ggdensity(data_bold,x="fd_mean",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "Framewise displacement",
                          palette = c("#3A5FCD", "#CD0000")) + labs(fill = "Group",color = "Group")
plot_bold_tsnr <- ggdensity(data_bold,x="tsnr",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "temporal Signal to Noise Ratio",
                          palette = c("#3A5FCD", "#CD0000"))
plot_bold_dvars <- ggdensity(data_bold,x="dvars_nstd",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "Successive difference images (sd)",
                          palette = c("#3A5FCD", "#CD0000"))      

# T1w
plot_T1w_snr <- ggdensity(data_T1W,x="snr_total",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "Signal to Noise Ratio (T1w)",
                          palette = c("#3A5FCD", "#CD0000")) + labs(fill = "Group",color = "Group") 
plot_T1w_cnr <- ggdensity(data_T1W,x="cnr",add="mean",rug = T,
                      fill = "Group", color="Group", xlab = "Contrast to Noise Ratio (T1w)",
                      palette = c("#3A5FCD", "#CD0000"))
plot_T1w_efc <- ggdensity(data_T1W,x="efc",add="mean",rug = T,
                      fill = "Group", color="Group", xlab = "Entropy-focus Criterion (T1w)",
                      palette = c("#3A5FCD", "#CD0000"))      

# T2w
plot_T2w_snr <- ggdensity(data_T2W,x="snr_total",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "Signal to Noise Ratio (T2w)",
                          palette = c("#3A5FCD", "#CD0000")) + labs(fill = "Group",color = "Group") 
plot_T2w_cnr <- ggdensity(data_T2W,x="cnr",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "Contrast to Noise Ratio (T2w)",
                          palette = c("#3A5FCD", "#CD0000"))
plot_T2w_efc <- ggdensity(data_T2W,x="efc",add="mean",rug = T,
                          fill = "Group", color="Group", xlab = "Entropy-focus Criterion (T2w)",
                          palette = c("#3A5FCD", "#CD0000"))      

# Plot structural
Fig1 <- plot_grid(
  plot_T1w_snr + theme(legend.position="none"),
  plot_T1w_cnr + theme(legend.position="none") + ylab(element_blank()),
  plot_T1w_efc + theme(legend.position="none") + ylab(element_blank()),
  plot_T2w_snr + theme(legend.position="none"),
  plot_T2w_cnr + theme(legend.position="none") + ylab(" "),
  plot_T2w_efc + theme(legend.position="none") + ylab(" "),
  align = 'vh', labels = "auto",
  hjust = -1, ncol = 3)

legend_b <- get_legend(plot_T1w_snr  +
                         theme(legend.position = "bottom"))

Fig1.1 <- plot_grid(Fig1, legend_b,ncol = 1, rel_heights = c(1, .1))
ggsave("Fig1.tiff",Fig1.1,dpi = 300,height = 10,width = 18,bg = "white")




Fig2 <- plot_grid(
  plot_bold_fd + theme(legend.position="none"),
  plot_bold_tsnr + theme(legend.position="none") + ylab(element_blank()),
  plot_bold_dvars + theme(legend.position="none") + ylab(element_blank()),
  align = 'vh', labels = "auto",
  hjust = -1, ncol = 3)

legend_b <- get_legend(plot_bold_fd  +
                         theme(legend.position = "bottom"))

Fig2 <- plot_grid(Fig2, legend_b,ncol = 1, rel_heights = c(1, .1))
ggsave("Fig2.tiff",Fig2,dpi = 300,height = 6,width = 13,bg = "white")

