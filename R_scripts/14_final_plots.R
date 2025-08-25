### plots all ###
libs <- c("ggplot2","ggpubr", "scales")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list = setdiff(ls(), "relative_abun"))
options(scipen=999)
# READ DATA ----
alldataNs <-read.csv("./Processed_data/Full_datasets/alldataNs_abun.csv")
alldataMMs <-read.csv("./Processed_data/Full_datasets/alldataMMs_abun.csv")
alldataMWs <-read.csv("./Processed_data/Full_datasets/alldataMWs_abun.csv")
alldataSEs <-read.csv("./Processed_data/Full_datasets/alldataSEs_abun.csv")
alldataSMs <-read.csv("./Processed_data/Full_datasets/alldataSMs_abun.csv")
alldataSWs <-read.csv("./Processed_data/Full_datasets/alldataSWs_abun.csv")

# Load MCP data

N_mcp <- read.csv("./Results/multiple_change_point/alldataNs_abun.csv")
MM_mcp <- read.csv("./Results/multiple_change_point/alldataMMs_abun.csv")
MW_mcp <- read.csv("./Results/multiple_change_point/alldataMWs_abun.csv")
SE_mcp <- read.csv("./Results/multiple_change_point/alldataSEs_abun.csv")
SM_mcp <- read.csv("./Results/multiple_change_point/alldataSMs_abun.csv")
SW_mcp <- read.csv("./Results/multiple_change_point/alldataSWs_abun.csv")

dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)

# Bayes factor thresholds
BF_10 <- 1
BF_25 <- 1.2
BF_50 <- 1.5

line_type = "dashed"

### COLOURS ###
#spd=firebrick4
#clim=blue2
#con=chartreuse4
#dec=seagreen2
#wetw=turquoise4
#wetm=blueviolet
#pas=orangered
#ara=turquoise4
#hea=seagreen2

# NORTH ----

SPD_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "SPD" & BF >= 10)

SPDN <- data.frame(age=alldataNs$yearsBP, spd=alldataNs$SPD)
N1 <- ggplot(SPDN, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = SPD_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=SPD_mcp_N[[1,4]], xmax=SPD_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

clim_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "clim" & BF >= 10)

climN <- data.frame(age=alldataNs$yearsBP, temp=alldataNs$clim)
N2 <- ggplot(climN, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = clim_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = clim_mcp_N[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=clim_mcp_N[[1,4]], xmax=clim_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  geom_rect(aes(xmin=clim_mcp_N[[2,4]], xmax=clim_mcp_N[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

con_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "conNs" & BF >= 10)

conN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$conNs)
N3 <- ggplot(conN, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = con_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=con_mcp_N[[1,4]], xmax=con_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="relative pollen abundance")


dec_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "decNs" & BF >= 10)

decN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$decNs)
N4 <- ggplot(decN, aes(x=age))+
  scale_colour_manual(values=c(gold="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = dec_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=dec_mcp_N[[1,4]], xmax=dec_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="relative pollen abundance")


wetw_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "wetwNs" & BF >= 10)

wetwN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$wetwNs)
N5 <- ggplot(wetwN, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = wetw_mcp_N[[1,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_N[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=wetw_mcp_N[[1,4]], xmax=wetw_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=wetw_mcp_N[[2,4]], xmax=wetw_mcp_N[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET WOODLAND", x="years BP", y="relative pollen abundance")

wetm_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "wetmNs" & BF >= 10)

wetmN <- data.frame(age=alldataNs$yearsBP, wetm=alldataNs$wetmNs)
N6 <- ggplot(wetmN, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=wetm), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = wetm_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=wetm_mcp_N[[1,4]], xmax=wetm_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET MEADOW", x="years BP", y="relative pollen abundance")


pasN_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "pasNs" & BF >= 10)

pasN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$pasNs)
N7 <- ggplot(pasN, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = pasN_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = pasN_mcp_N[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = pasN_mcp_N[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = pasN_mcp_N[[4,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=pasN_mcp_N[[1,4]], xmax=pasN_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=pasN_mcp_N[[2,4]], xmax=pasN_mcp_N[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=pasN_mcp_N[[3,4]], xmax=pasN_mcp_N[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=pasN_mcp_N[[4,4]], xmax=pasN_mcp_N[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="PASTURE", x="years BP", y="relative pollen abundance")

araN_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "araNs" & BF >= 10)

araN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$araNs)
N8 <- ggplot(araN, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = araN_mcp_N[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = araN_mcp_N[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = araN_mcp_N[[3,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = araN_mcp_N[[4,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = araN_mcp_N[[5,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=araN_mcp_N[[1,4]], xmax=araN_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=araN_mcp_N[[2,4]], xmax=araN_mcp_N[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=araN_mcp_N[[3,4]], xmax=araN_mcp_N[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=araN_mcp_N[[4,4]], xmax=araN_mcp_N[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=araN_mcp_N[[5,4]], xmax=araN_mcp_N[[5,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="ARABLE LAND", x="years BP", y="relative pollen abundance")


heaN_mcp_N <- N_mcp %>%
  dplyr::filter(ID == "heaNs" & BF >= 10)

heaN <- data.frame(age=alldataNs$yearsBP, hea=alldataNs$heaNs)
N9 <- ggplot(heaN, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=hea), color="orangered", size=1.3)+
  geom_vline(aes(xintercept = heaN_mcp_N[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = heaN_mcp_N[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = heaN_mcp_N[[3,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = heaN_mcp_N[[4,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=heaN_mcp_N[[1,4]], xmax=heaN_mcp_N[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=heaN_mcp_N[[2,4]], xmax=heaN_mcp_N[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=heaN_mcp_N[[3,4]], xmax=heaN_mcp_N[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=heaN_mcp_N[[4,4]], xmax=heaN_mcp_N[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="HEATH", x="years BP", y="relative pollen abundance")

# PLOT
N_all <- ggarrange(N1,N2,N3,N4,N5,N6,N7,N8,N9, 
          ncol = 1, nrow = 9)
annotate_figure(N_all, top = text_grob("North", color = "black", face = "bold"))
png(filename="./Results/Plots/LCC_abun/N_all.png", width = 1000, height = 1500,)
plot(N_all)
dev.off()

# SOUTHEAST ----

SPD_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "SPD" & BF >= 10)

spdSE <- data.frame(age=alldataSEs$yearsBP, spd=alldataSEs$SPD)
SE1 <- ggplot(spdSE, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

clim_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "clim" & BF >= 10)

climSE <- data.frame(age=alldataSEs$yearsBP, temp=alldataSEs$clim)
SE2 <- ggplot(climSE, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = clim_mcp_SE[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=clim_mcp_SE[[1,4]], xmax=clim_mcp_SE[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")


con_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "conSEs" & BF >= 10)

conSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$conSEs)
SE3 <- ggplot(conSE, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="chartreuse4", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="relative pollen abundance")

dec_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "decSEs" & BF >= 10)

decSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$decSEs)
SE4 <- ggplot(decSE, aes(x=age))+
  scale_colour_manual(values=c(gold="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="relative pollen abundance")

wetw_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "wetwSEs" & BF >= 10)

wetwSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$wetwSEs)
SE5 <- ggplot(wetwSE, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET WOODLAND", x="years BP", y="relative pollen abundance")

wetm_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "wetmSEs" & BF >= 10)

wetmSE <- data.frame(age=alldataSEs$yearsBP, wetm=alldataSEs$wetmSEs)
SE6 <- ggplot(wetmSE, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=wetm), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = wetm_mcp_SE[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = wetm_mcp_SE[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=wetm_mcp_SE[[1,4]], xmax=wetm_mcp_SE[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetm_mcp_SE[[2,4]], xmax=wetm_mcp_SE[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET MEADOW", x="years BP", y="relative pollen abundance")

pas_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "pasSEs" & BF >= 10)

pasSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$pasSEs)
SE7 <- ggplot(pasSE, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = pas_mcp_SE[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = pas_mcp_SE[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=pas_mcp_SE[[1,4]], xmax=pas_mcp_SE[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=pas_mcp_SE[[2,4]], xmax=pas_mcp_SE[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="PASTURE", x="years BP", y="relative pollen abundance")

ara_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "araSEs" & BF >= 10)

araSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$araSEs)
SE8 <- ggplot(araSE, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = ara_mcp_SE[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_SE[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_SE[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_SE[[4,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_SE[[5,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=ara_mcp_SE[[1,4]], xmax=ara_mcp_SE[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_SE[[2,4]], xmax=ara_mcp_SE[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_SE[[3,4]], xmax=ara_mcp_SE[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_SE[[4,4]], xmax=ara_mcp_SE[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_SE[[5,4]], xmax=ara_mcp_SE[[5,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="ARABLE LAND", x="years BP", y="relative pollen abundance")

hea_mcp_SE <- SE_mcp %>%
  dplyr::filter(ID == "heaSEs" & BF >= 10)

heaSE <- data.frame(age=alldataSEs$yearsBP, hea=alldataSEs$heaSEs)
SE9 <- ggplot(heaSE, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=hea), color="orangered", size=1.3)+
  geom_vline(aes(xintercept = hea_mcp_SE[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = hea_mcp_SE[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=hea_mcp_SE[[1,4]], xmax=hea_mcp_SE[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=hea_mcp_SE[[2,4]], xmax=hea_mcp_SE[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="HEATH", x="years BP", y="relative pollen abundance")

# PLOT #
SE_all <- ggarrange(SE1,SE2,SE3,SE4,SE5,SE6,SE7,SE8,SE9, 
                   ncol = 1, nrow = 9)
annotate_figure(SE_all, top = text_grob("SouthEast: the whole Holocene", color = "black", face = "bold"))
png(filename="./Results/Plots/LCC_abun/SE_all.png", width = 1000, height = 1500,)
plot(SE_all)
dev.off()

# MIDWEST ----

spd_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "SPD" & BF >= 10)

spdMW <- data.frame(age=alldataMWs$yearsBP, spd=alldataMWs$SPD)
MW1 <- ggplot(spdMW, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = spd_mcp_MW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_MW[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_MW[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_MW[[4,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=spd_mcp_MW[[1,4]], xmax=spd_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_MW[[2,4]], xmax=spd_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_MW[[3,4]], xmax=spd_mcp_MW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_MW[[4,4]], xmax=spd_mcp_MW[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")


clim_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "clim" & BF >= 10)

climMW <- data.frame(age=alldataMWs$yearsBP, temp=alldataMWs$clim)
MW2 <- ggplot(climMW, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = clim_mcp_MW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=clim_mcp_MW[[1,4]], xmax=clim_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

con_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "conMWs" & BF >= 10)

conMW <- data.frame(age=alldataMWs$yearsBP, con=alldataMWs$conMWs)
MW3 <- ggplot(conMW, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = con_mcp_MW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = con_mcp_MW[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=con_mcp_MW[[1,4]], xmax=con_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=con_mcp_MW[[2,4]], xmax=con_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))


dec_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "decMWs" & BF >= 10)

decMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$decMWs)
MW4 <- ggplot(decMW, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = dec_mcp_MW[[1,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = dec_mcp_MW[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = dec_mcp_MW[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=dec_mcp_MW[[1,4]], xmax=dec_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=dec_mcp_MW[[2,4]], xmax=dec_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=dec_mcp_MW[[3,4]], xmax=dec_mcp_MW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))


wetw_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "wetwMWs" & BF >= 10)

wetwMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$wetwMWs)
MW5 <- ggplot(wetwMW, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = wetw_mcp_MW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_MW[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_MW[[3,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=wetw_mcp_MW[[1,4]], xmax=wetw_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=wetw_mcp_MW[[2,4]], xmax=wetw_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=wetw_mcp_MW[[3,4]], xmax=wetw_mcp_MW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetm_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "wetmMWs" & BF >= 10)

wetmMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$wetmMWs)
MW6 <- ggplot(wetmMW, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = wetm_mcp_MW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = wetm_mcp_MW[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = wetm_mcp_MW[[3,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = wetm_mcp_MW[[4,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=wetm_mcp_MW[[1,4]], xmax=wetm_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetm_mcp_MW[[2,4]], xmax=wetm_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetm_mcp_MW[[3,4]], xmax=wetm_mcp_MW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetm_mcp_MW[[4,4]], xmax=wetm_mcp_MW[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET MEADOW", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasm_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "pasMWs" & BF >= 10)

fplot.pasmw_df <- data.frame(age=alldataMWs$yearsBP, pas=alldataMWs$pasMWs)
MW7 <- ggplot(fplot.pasmw_df, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = pasm_mcp_MW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = pasm_mcp_MW[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=pasm_mcp_MW[[1,4]], xmax=pasm_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.01, fill = "orange2") +
  geom_rect(aes(xmin=pasm_mcp_MW[[2,4]], xmax=pasm_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.01, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="PASTURE", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none")

ara_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "araMWs" & BF >= 10)

fplot.aramw_df <- araMW <- data.frame(age=alldataMWs$yearsBP, ara=alldataMWs$araMWs, spd=alldataMWs$SPD)
MW8 <- ggplot(fplot.aramw_df, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = ara_mcp_MW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=ara_mcp_MW[[1,4]], xmax=ara_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="ARABLE LAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

hea_mcp_MW <- MW_mcp %>%
  dplyr::filter(ID == "heaMWs" & BF >= 10)

heaMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$heaMWs)
MW9 <- ggplot(heaMW, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = hea_mcp_MW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = hea_mcp_MW[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = hea_mcp_MW[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = hea_mcp_MW[[4,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=hea_mcp_MW[[1,4]], xmax=hea_mcp_MW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=hea_mcp_MW[[2,4]], xmax=hea_mcp_MW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=hea_mcp_MW[[3,4]], xmax=hea_mcp_MW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=hea_mcp_MW[[4,4]], xmax=hea_mcp_MW[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="HEATH", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
      plot.title = element_text(size = 12),
      plot.background = element_rect(color = "grey", size = 1))

# PLOT #
MW_all <- ggarrange(MW1, MW2, MW3, MW4, MW5, MW6, MW7,MW8,MW9,
                   ncol = 1, nrow = 9)
annotate_figure(MW_all, top = text_grob("MidWest: the whole Holocene", color = "black", face = "bold"))
png(filename="./Results/Plots/LCC_abun/MW_all.png", width = 1000, height = 1500,)
plot(MW_all)
dev.off()

# MidMid ----

spd_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "SPD" & BF >= 10)

spdMM <- data.frame(age=alldataMMs$yearsBP, spd=alldataMMs$SPD)
MM1 <- ggplot(spdMM, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = spd_mcp_MM[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_MM[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_MM[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_MM[[4,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=spd_mcp_MM[[1,4]], xmax=spd_mcp_MM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_MM[[2,4]], xmax=spd_mcp_MM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_MM[[3,4]], xmax=spd_mcp_MM[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_MM[[4,4]], xmax=spd_mcp_MM[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

clim_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "clim" & BF >= 10)

climMM <- data.frame(age=alldataMMs$yearsBP, temp=alldataMMs$clim)
MM2 <- ggplot(climMM, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = clim_mcp_MM[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=clim_mcp_MM[[1,4]], xmax=clim_mcp_MM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")


con_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "conMMs" & BF >= 10)

conMM <- data.frame(age=alldataMMs$yearsBP, con=alldataMMs$conMMs)
MM3 <- ggplot(conMM, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = con_mcp_MM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=con_mcp_MM[[1,4]], xmax=con_mcp_MM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))


dec_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "decMMs" & BF >= 10)

decMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$decMMs)
MM4 <- ggplot(decMM, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetw_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "wetwMMs" & BF >= 10)

wetwMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$wetwMMs)
MM5 <- ggplot(wetwMM, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetm_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "wetmMMs" & BF >= 10)

wetmMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$wetmMMs)
MM6 <- ggplot(wetmMM, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = wetm_mcp_MM[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = wetm_mcp_MM[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=wetm_mcp_MM[[1,4]], xmax=wetm_mcp_MM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetm_mcp_MM[[2,4]], xmax=wetm_mcp_MM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET MEADOW", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))


pas_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "pasMMs" & BF >= 10)

pasMM <- data.frame(age=alldataMMs$yearsBP, pas=alldataMMs$pasMMs)
MM7 <- ggplot(pasMM, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="PASTURE", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

ara_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "araMMs" & BF >= 10)

araMM <- data.frame(age=alldataMMs$yearsBP, ara=alldataMMs$araMMs)
MM8 <- ggplot(araMM, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = ara_mcp_MM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_MM[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_MM[[3,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_MM[[4,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_MM[[5,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=ara_mcp_MM[[1,4]], xmax=ara_mcp_MM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_MM[[2,4]], xmax=ara_mcp_MM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_MM[[3,4]], xmax=ara_mcp_MM[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_MM[[4,4]], xmax=ara_mcp_MM[[4,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_MM[[5,4]], xmax=ara_mcp_MM[[5,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="ARABLE LAND", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))


hea_mcp_MM <- MM_mcp %>%
  dplyr::filter(ID == "heaMMs" & BF >= 10)

heaMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$heaMMs)
MM9 <- ggplot(heaMM, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = hea_mcp_MM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=hea_mcp_MM[[1,4]], xmax=hea_mcp_MM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="HEATH", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
MM_all <- ggarrange(MM1, MM2, MM3, MM4, MM5, MM6, MM7,MM8,MM9,
                    ncol = 1, nrow = 9)
annotate_figure(MM_all, top = text_grob("MidMid: the whole Holocene", color = "black", face = "bold"))
png(filename="./Results/Plots/LCC_abun/MM_all.png", width = 1000, height = 1500,)
plot(MM_all)
dev.off()

# SOUTHWEST ----

spd_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "SPD" & BF >= 10)

spdSW <- data.frame(age=alldataSWs$yearsBP, spd=alldataSWs$SPD)
SW1 <- ggplot(spdSW, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = spd_mcp_SW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_SW[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_SW[[3,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=spd_mcp_SW[[1,4]], xmax=spd_mcp_SW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_SW[[2,4]], xmax=spd_mcp_SW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_SW[[3,4]], xmax=spd_mcp_SW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

clim_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "clim" & BF >= 10)

climSW <- data.frame(age=alldataSWs$yearsBP, temp=alldataSWs$clim)
SW2 <- ggplot(climSW, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

con_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "conSWs" & BF >= 10)

conSW <- data.frame(age=alldataSWs$yearsBP, con=alldataSWs$conSWs)
SW3 <- ggplot(conSW, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = con_mcp_SW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = con_mcp_SW[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = con_mcp_SW[[3,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=con_mcp_SW[[1,4]], xmax=con_mcp_SW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=con_mcp_SW[[2,4]], xmax=con_mcp_SW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=con_mcp_SW[[3,4]], xmax=con_mcp_SW[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

dec_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "decSWs" & BF >= 10)

decSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$decSWs)
SW4 <- ggplot(decSW, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetw_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "wetwSWs" & BF >= 10)

wetwSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$wetwSWs)
SW5 <- ggplot(wetwSW, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetm_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "wetmSWs" & BF >= 10)

wetmSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$wetmSWs)
SW6 <- ggplot(wetmSW, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = wetm_mcp_SW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = wetm_mcp_SW[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_rect(aes(xmin=wetm_mcp_SW[[1,4]], xmax=wetm_mcp_SW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetm_mcp_SW[[2,4]], xmax=wetm_mcp_SW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET MEADOW", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pas_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "pasSWs" & BF >= 10)

pasSW <- data.frame(age=alldataSWs$yearsBP, pas=alldataSWs$pasSWs)
SW7 <- ggplot(pasSW, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = pas_mcp_SW[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = pas_mcp_SW[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=pas_mcp_SW[[1,4]], xmax=pas_mcp_SW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=pas_mcp_SW[[2,4]], xmax=pas_mcp_SW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="PASTURE", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

ara_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "araSWs" & BF >= 9.97)

araSW <- data.frame(age=alldataSWs$yearsBP, ara=alldataSWs$araSWs)
SW8 <- ggplot(araSW, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = ara_mcp_SW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=ara_mcp_SW[[1,4]], xmax=ara_mcp_SW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="ARABLE LAND", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

hea_mcp_SW <- SW_mcp %>%
  dplyr::filter(ID == "heaSWs" & BF >= 10)

heaSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$heaSWs)
SW9 <- ggplot(heaSW, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = hea_mcp_SW[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = hea_mcp_SW[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=hea_mcp_SW[[1,4]], xmax=hea_mcp_SW[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=hea_mcp_SW[[2,4]], xmax=hea_mcp_SW[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="HEATH", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
SW_all <- ggarrange(SW1, SW2,SW3,SW4,SW5,SW6,SW7,SW8,SW9,
                    ncol = 1, nrow = 9)
annotate_figure(SW_all, top = text_grob("SouthWest: the whole Holocene", color = "black", face = "bold"))
png(filename="./Results/Plots/LCC_abun/SW_all.png", width = 1000, height = 1500,)
plot(SW_all)
dev.off()

# SOUTHMID ----

spd_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "SPD" & BF >= 10)

spdSM <- data.frame(age=alldataSMs$yearsBP, spd=alldataSMs$SPD)
SM1 <- ggplot(spdSM, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = spd_mcp_SM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = spd_mcp_SM[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=spd_mcp_SM[[1,4]], xmax=spd_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=spd_mcp_SM[[2,4]], xmax=spd_mcp_SM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

clim_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "clim" & BF >= 10)

climSM <- data.frame(age=alldataSMs$yearsBP, temp=alldataSMs$clim)
SM2 <- ggplot(climSM, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

con_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "conSMs" & BF >= 10)

conSM <- data.frame(age=alldataSMs$yearsBP, con=alldataSMs$conSMs)
SM3 <- ggplot(conSM, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = con_mcp_SM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = con_mcp_SM[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=con_mcp_SM[[1,4]], xmax=con_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=con_mcp_SM[[2,4]], xmax=con_mcp_SM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

dec_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "decSMs" & BF >= 10)

decSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$decSMs)
SM4 <- ggplot(decSM, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = dec_mcp_SM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=dec_mcp_SM[[1,4]], xmax=dec_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetw_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "wetwSMs" & BF >= 10)

wetwSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$wetwSMs)
SM5 <- ggplot(wetwSM, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = wetw_mcp_SM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_SM[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_SM[[3,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=wetw_mcp_SM[[1,4]], xmax=wetw_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=wetw_mcp_SM[[2,4]], xmax=wetw_mcp_SM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=wetw_mcp_SM[[3,4]], xmax=wetw_mcp_SM[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET WOODLAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetm_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "wetmSMs" & BF >= 10)

wetmSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$wetmSMs)
SM6 <- ggplot(wetmSM, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = wetw_mcp_SM[[1,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_SM[[2,3]], color = "black"), size = BF_50, linetype = line_type) +
  geom_vline(aes(xintercept = wetw_mcp_SM[[3,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=wetw_mcp_SM[[1,4]], xmax=wetw_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetw_mcp_SM[[2,4]], xmax=wetw_mcp_SM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=wetw_mcp_SM[[3,4]], xmax=wetw_mcp_SM[[3,5]], ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET MEADOW", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pas_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "pasSMs" & BF >= 10)

pasSM <- data.frame(age=alldataSMs$yearsBP, pas=alldataSMs$pasSMs)
SM7 <- ggplot(pasSM, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = pas_mcp_SM[[1,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_vline(aes(xintercept = pas_mcp_SM[[2,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=pas_mcp_SM[[1,4]], xmax=pas_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=pas_mcp_SM[[2,4]], xmax=pas_mcp_SM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="PASTURE", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

ara_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "araSMs" & BF >= 10)

araSM <- data.frame(age=alldataSMs$yearsBP, ara=alldataSMs$araSMs)
SM8 <- ggplot(araSM, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = ara_mcp_SM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_vline(aes(xintercept = ara_mcp_SM[[2,3]], color = "black"), size = BF_25, linetype = line_type) +
  geom_rect(aes(xmin=ara_mcp_SM[[1,4]], xmax=ara_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=ara_mcp_SM[[2,4]], xmax=ara_mcp_SM[[2,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="ARABLE LAND", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

hea_mcp_SM <- SM_mcp %>%
  dplyr::filter(ID == "heaSMs" & BF >= 10)

heaSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$heaSMs)
SM9 <- ggplot(heaSM, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = hea_mcp_SM[[1,3]], color = "black"), size = BF_10, linetype = line_type) +
  geom_rect(aes(xmin=hea_mcp_SM[[1,4]], xmax=hea_mcp_SM[[1,5]], ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="HEATH", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
SM_all <- ggarrange(SM1, SM2,SM3,SM4,SM5,SM6,SM7,SM8,SM9,
                    ncol = 1, nrow = 9)
annotate_figure(SM_all, top = text_grob("SouthMid: the whole Holocene", color = "black", face = "bold"))
png(filename="./Results/Plots/LCC_abun/SM_all.png", width = 1000, height = 1500,)
plot(SM_all)
dev.off()