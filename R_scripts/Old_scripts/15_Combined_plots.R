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

# READ DATA ----
alldataNs <-read.csv("./Processed_data/Full_datasets/alldataNs_abun.csv")
alldataMMs <-read.csv("./Processed_data/Full_datasets/alldataMMs_abun.csv")
alldataMWs <-read.csv("./Processed_data/Full_datasets/alldataMWs_abun.csv")
alldataSEs <-read.csv("./Processed_data/Full_datasets/alldataSEs_abun.csv")
alldataSMs <-read.csv("./Processed_data/Full_datasets/alldataSMs_abun.csv")
alldataSWs <-read.csv("./Processed_data/Full_datasets/alldataSWs_abun.csv")

dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)


# ------ Abundance PLOTS ------


# NORTH ----
SPDN <- data.frame(age=alldataNs$yearsBP, spd=alldataNs$SPD)
N1 <- ggplot(SPDN, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 2410, color = "black")) +
  geom_vline(aes(xintercept = 3447, color = "black")) +
  geom_rect(aes(xmin=2309, xmax=2499, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=3384, xmax=3500, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="SPD")

climN <- data.frame(age=alldataNs$yearsBP, temp=alldataNs$clim)
N2 <- ggplot(climN, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 2694, color = "black")) +
  geom_vline(aes(xintercept = 7932, color = "black")) +
  geom_rect(aes(xmin=2600, xmax=2824, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  geom_rect(aes(xmin=7825, xmax=7999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="°C (annual mean)")

conN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$conNs)
N3 <- ggplot(conN, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 2361, color = "black")) +
  geom_vline(aes(xintercept = 5243, color = "black")) +
  geom_vline(aes(xintercept = 6161, color = "black")) +
  geom_vline(aes(xintercept = 7148, color = "black")) +
  geom_rect(aes(xmin=2000, xmax=2706, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=5185, xmax=5300, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=6100, xmax=6370, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=6800, xmax=7506, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

decN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$decNs)
N4 <- ggplot(decN, aes(x=age))+
  scale_colour_manual(values=c(gold="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 3007, color = "black")) +
  geom_vline(aes(xintercept = 7037, color = "black")) +
  geom_rect(aes(xmin=2735, xmax=3299, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=6867, xmax=7199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

wetwN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$wetwNs)
N5 <- ggplot(wetwN, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 5169, color = "black")) +
  geom_vline(aes(xintercept = 7957, color = "black")) +
  geom_rect(aes(xmin=4922, xmax=5299, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=7900, xmax=8034, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

wetmN <- data.frame(age=alldataNs$yearsBP, wetm=alldataNs$wetmNs)
N6 <- ggplot(wetmN, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=wetm), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 2942, color = "black")) +
  geom_vline(aes(xintercept = 5067, color = "black")) +
  geom_rect(aes(xmin=2898, xmax=2999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blueviolet") +
  geom_rect(aes(xmin=5000, xmax=5192, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

pasN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$pasNs)
N7 <- ggplot(pasN, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 6939, color = "black")) +
  geom_rect(aes(xmin=6821, xmax=6999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

araN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$araNs)
N8 <- ggplot(araN, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1855, color = "black")) +
  geom_rect(aes(xmin=1725, xmax=1999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

heaN <- data.frame(age=alldataNs$yearsBP, hea=alldataNs$heaNs)
N9 <- ggplot(heaN, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=hea), color="orangered", size=1.3)+
  geom_vline(aes(xintercept = 6533, color = "black")) +
  geom_vline(aes(xintercept = 7678, color = "black")) +
  geom_rect(aes(xmin=6385, xmax=6599, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7414, xmax=7899, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

# PLOT
N_all1 <- ggarrange(N1,N2,N3,N4,N5,N6,N7,N8,N9, 
                   ncol = 1, nrow = 9)
annotate_figure(N_all1, top = text_grob("North", color = "black", face = "bold"))

# SOUTHEAST ----

spdSE <- data.frame(age=alldataSEs$yearsBP, spd=alldataSEs$SPD)
SE1 <- ggplot(spdSE, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 1380, color = "black")) +
  geom_vline(aes(xintercept = 2944, color = "black")) +
  geom_rect(aes(xmin=1300, xmax=1484, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=2800, xmax=3059, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="SPD")

climSE <- data.frame(age=alldataSEs$yearsBP, temp=alldataSEs$clim)
SE2 <- ggplot(climSE, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5964, color = "black")) +
  geom_rect(aes(xmin=5294, xmax=6601, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="°C (annual mean)")

conSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$conSEs)
SE3 <- ggplot(conSE, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 849, color = "black")) +
  geom_vline(aes(xintercept = 2049, color = "black")) +
  geom_vline(aes(xintercept = 4840, color = "black")) +
  geom_rect(aes(xmin=802, xmax=899, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=2002, xmax=2098, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=4524, xmax=5100, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

decSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$decSEs)
SE4 <- ggplot(decSE, aes(x=age))+
  scale_colour_manual(values=c(gold="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 1270, color = "black")) +
  geom_vline(aes(xintercept = 4358, color = "black")) +
  geom_rect(aes(xmin=1200, xmax=1428, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=4300, xmax=4438, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

wetwSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$wetwSEs)
SE5 <- ggplot(wetwSE, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 4339, color = "black")) +
  geom_rect(aes(xmin=4200, xmax=4455, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

wetmSE <- data.frame(age=alldataSEs$yearsBP, wetm=alldataSEs$wetmSEs)
SE6 <- ggplot(wetmSE, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=wetm), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 1248, color = "black")) +
  geom_vline(aes(xintercept = 2594, color = "black")) +
  geom_vline(aes(xintercept = 4742, color = "black")) +
  geom_vline(aes(xintercept = 8193, color = "black")) +
  geom_rect(aes(xmin=1166, xmax=1299, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=2500, xmax=2693, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=4632, xmax=4800, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=8001, xmax=8399, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

pasSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$pasSEs)
SE7 <- ggplot(pasSE, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 574, color = "black")) +
  geom_vline(aes(xintercept = 1350, color = "black")) +
  geom_vline(aes(xintercept = 8549, color = "black")) +
  geom_rect(aes(xmin=500, xmax=681, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=1304, xmax=1399, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=8504, xmax=8599, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

araSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$araSEs)
SE8 <- ggplot(araSE, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 650, color = "black")) +
  geom_vline(aes(xintercept = 949, color = "black")) +
  geom_rect(aes(xmin=603, xmax=698, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  geom_rect(aes(xmin=902, xmax=996, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

heaSE <- data.frame(age=alldataSEs$yearsBP, hea=alldataSEs$heaSEs)
SE9 <- ggplot(heaSE, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=hea), color="orangered", size=1.3)+
  geom_vline(aes(xintercept = 1249, color = "black")) +
  geom_vline(aes(xintercept = 2456, color = "black")) +
  geom_rect(aes(xmin=1202, xmax=1297, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=2400, xmax=2505, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="relative pollen abundance")

# PLOT #
SE_all1 <- ggarrange(SE1,SE2,SE3,SE4,SE5,SE6,SE7,SE8,SE9, 
                    ncol = 1, nrow = 9)
annotate_figure(SE_all1, top = text_grob("SouthEast: the whole Holocene", color = "black", face = "bold"))

# MIDWEST ----
spdMW <- data.frame(age=alldataMWs$yearsBP, spd=alldataMWs$SPD)
MW1 <- ggplot(spdMW, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 750, color = "firebrick4")) +
  geom_vline(aes(xintercept = 850, color = "firebrick4")) +
  geom_vline(aes(xintercept = 1702, color = "firebrick4")) +
  geom_vline(aes(xintercept = 2848, color = "firebrick4")) +
  geom_rect(aes(xmin=705, xmax=799, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=805, xmax=899, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=1600, xmax=1833, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=2800, xmax=2895, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="SPD")

climMW <- data.frame(age=alldataMWs$yearsBP, temp=alldataMWs$clim)
MW2 <- ggplot(climMW, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5961, color = "black")) +
  geom_rect(aes(xmin=5431, xmax=6296, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="°C (annual mean)")

conMW <- data.frame(age=alldataMWs$yearsBP, con=alldataMWs$conMWs)
MW3 <- ggplot(conMW, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 2899, color = "black")) +
  geom_vline(aes(xintercept = 3939, color = "black")) +
  geom_vline(aes(xintercept = 7127, color = "black")) +
  geom_rect(aes(xmin=2810, xmax=2999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=3900, xmax=4077, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=6334, xmax=7399, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$decMWs)
MW4 <- ggplot(decMW, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 2953, color = "black")) +
  geom_vline(aes(xintercept = 4068, color = "black")) +
  geom_vline(aes(xintercept = 5739, color = "black")) +
  geom_vline(aes(xintercept = 6450, color = "black")) +
  geom_vline(aes(xintercept = 8258, color = "black")) +
  geom_rect(aes(xmin=2900, xmax=2999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=3958, xmax=4199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=5598, xmax=5799, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=6403, xmax=6498, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=8000, xmax=8472, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$wetwMWs)
MW5 <- ggplot(wetwMW, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 3005, color = "black")) +
  geom_vline(aes(xintercept = 4023, color = "black")) +
  geom_vline(aes(xintercept = 6011, color = "black")) +
  geom_vline(aes(xintercept = 7949, color = "black")) +
  geom_rect(aes(xmin=2806, xmax=3199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=3803, xmax=4139, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=5913, xmax=6099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=7903, xmax=7998, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$wetmMWs)
MW6 <- ggplot(wetmMW, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 3025, color = "black")) +
  geom_vline(aes(xintercept = 4116, color = "black")) +
  geom_rect(aes(xmin=2900, xmax=3102, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=3906, xmax=4399, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

fplot.pasmw_df <- data.frame(age=alldataMWs$yearsBP, pas=alldataMWs$pasMWs)
MW7 <- ggplot(fplot.pasmw_df, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 928, color = "black")) +
  geom_vline(aes(xintercept = 1283, color = "black")) +
  geom_rect(aes(xmin=804, xmax=999, ymin=-Inf, ymax=Inf), alpha=0.01, fill = "orange2") +
  geom_rect(aes(xmin=1200, xmax=1385, ymin=-Inf, ymax=Inf), alpha=0.01, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none")

fplot.aramw_df <- araMW <- data.frame(age=alldataMWs$yearsBP, ara=alldataMWs$araMWs, spd=alldataMWs$SPD)
MW8 <- ggplot(fplot.aramw_df, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 3925, color = "black")) +
  geom_vline(aes(xintercept = 8248, color = "black")) +
  geom_rect(aes(xmin=3730, xmax=4099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=8200, xmax=8294, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$heaMWs)
MW9 <- ggplot(heaMW, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = 2470, color = "black")) +
  geom_vline(aes(xintercept = 3981, color = "black")) +
  geom_vline(aes(xintercept = 8315, color = "black")) +
  geom_rect(aes(xmin=1745, xmax=2899, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=3900, xmax=4087, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7894, xmax=8499, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
MW_all1 <- ggarrange(MW1, MW2, MW3, MW4, MW5, MW6, MW7,MW8,MW9,
                    ncol = 1, nrow = 9)
annotate_figure(MW_all1, left = text_grob("MidWest: the whole Holocene", color = "black", face = "bold"))

# MidMid ----
spdMM <- data.frame(age=alldataMMs$yearsBP, spd=alldataMMs$SPD)
MM1 <- ggplot(spdMM, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 851, color = "firebrick4")) +
  geom_vline(aes(xintercept = 1350, color = "firebrick4")) +
  geom_vline(aes(xintercept = 2613, color = "firebrick4")) +
  geom_rect(aes(xmin=804, xmax=899, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=1304, xmax=1399, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=2399, xmax=2800, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="SPD")

climMM <- data.frame(age=alldataMMs$yearsBP, temp=alldataMMs$clim)
MM2 <- ggplot(climMM, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5931, color = "black")) +
  geom_rect(aes(xmin=5401, xmax=6299, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="°C (annual mean)")

conMM <- data.frame(age=alldataMMs$yearsBP, con=alldataMMs$conMMs)
MM3 <- ggplot(conMM, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 3891, color = "black")) +
  geom_vline(aes(xintercept = 5845, color = "black")) +
  geom_rect(aes(xmin=3800, xmax=3997, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=5494, xmax=6835, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$decMMs)
MM4 <- ggplot(decMM, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 2399, color = "black")) +
  geom_vline(aes(xintercept = 3860, color = "black")) +
  geom_vline(aes(xintercept = 8182, color = "black")) +
  geom_rect(aes(xmin=2290, xmax=2697, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=3799, xmax=3976, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=7831, xmax=8399, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$wetwMMs)
MM5 <- ggplot(wetwMM, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 3842, color = "black")) +
  geom_vline(aes(xintercept = 7139, color = "black")) +
  geom_rect(aes(xmin=3116, xmax=4095, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=7015, xmax=7199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$wetmMMs)
MM6 <- ggplot(wetmMM, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 1436, color = "black")) +
  geom_vline(aes(xintercept = 5918, color = "black")) +
  geom_rect(aes(xmin=1200, xmax=1518, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=5484, xmax=6315, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasMM <- data.frame(age=alldataMMs$yearsBP, pas=alldataMMs$pasMMs)
MM7 <- ggplot(pasMM, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 1448, color = "black")) +
  geom_vline(aes(xintercept = 1950, color = "black")) +
  geom_vline(aes(xintercept = 4106, color = "black")) +
  geom_vline(aes(xintercept = 7948, color = "black")) +
  geom_rect(aes(xmin=1401, xmax=1497, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=1900, xmax=1996, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=3900, xmax=4479, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=7900, xmax=7996, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

araMM <- data.frame(age=alldataMMs$yearsBP, ara=alldataMMs$araMMs)
MM8 <- ggplot(araMM, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1877, color = "black")) +
  geom_vline(aes(xintercept = 3777, color = "black")) +
  geom_vline(aes(xintercept = 5931, color = "black")) +
  geom_rect(aes(xmin=1700, xmax=1999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=3700, xmax=3882, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=5816, xmax=5999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$heaMMs)
MM9 <- ggplot(heaMM, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = 1450, color = "black")) +
  geom_vline(aes(xintercept = 7877, color = "black")) +
  geom_rect(aes(xmin=1404, xmax=1499, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7800, xmax=7987, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
MM_all1 <- ggarrange(MM1, MM2, MM3, MM4, MM5, MM6, MM7,MM8,MM9,
                    ncol = 1, nrow = 9)
annotate_figure(MM_all1, top = text_grob("MidMid: the whole Holocene", color = "black", face = "bold"))

# SOUTHWEST ----

spdSW <- data.frame(age=alldataSWs$yearsBP, spd=alldataSWs$SPD)
SW1 <- ggplot(spdSW, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 871, color = "firebrick4")) +
  geom_vline(aes(xintercept = 2346, color = "firebrick4")) +
  geom_rect(aes(xmin=800, xmax=978, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=2200, xmax=2400, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="SPD")

climSW <- data.frame(age=alldataSWs$yearsBP, temp=alldataSWs$clim)
SW2 <- ggplot(climSW, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5947, color = "black")) +
  geom_rect(aes(xmin=5400, xmax=6459, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="°C (annual mean)")

conSW <- data.frame(age=alldataSWs$yearsBP, con=alldataSWs$conSWs)
SW3 <- ggplot(conSW, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 1565, color = "black")) +
  geom_vline(aes(xintercept = 3621, color = "black")) +
  geom_vline(aes(xintercept = 7702, color = "black")) +
  geom_rect(aes(xmin=1500, xmax=1695, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=2901, xmax=4643, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=7609, xmax=7799, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$decSWs)
SW4 <- ggplot(decSW, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 4207, color = "black")) +
  geom_rect(aes(xmin=3400, xmax=5576, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$wetwSWs)
SW5 <- ggplot(wetwSW, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 3513, color = "black")) +
  geom_vline(aes(xintercept = 5517, color = "black")) +
  geom_vline(aes(xintercept = 8334, color = "black")) +
  geom_rect(aes(xmin=3405, xmax=3599, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=5302, xmax=5698, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=8161, xmax=8590, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$wetmSWs)
SW6 <- ggplot(wetmSW, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 1032, color = "black")) +
  geom_vline(aes(xintercept = 1550, color = "black")) +
  geom_vline(aes(xintercept = 3786, color = "black")) +
  geom_rect(aes(xmin=849, xmax=1199, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=1503, xmax=1599, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=3339, xmax=4290, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasSW <- data.frame(age=alldataSWs$yearsBP, pas=alldataSWs$pasSWs)
SW7 <- ggplot(pasSW, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 1053, color = "black")) +
  geom_vline(aes(xintercept = 3032, color = "black")) +
  geom_vline(aes(xintercept = 7733, color = "black")) +
  geom_rect(aes(xmin=999, xmax=1139, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=2700, xmax=3259, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=6824, xmax=8099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

araSW <- data.frame(age=alldataSWs$yearsBP, ara=alldataSWs$araSWs)
SW8 <- ggplot(araSW, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1150, color = "black")) +
  geom_rect(aes(xmin=1104, xmax=1199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$heaSWs)
SW9 <- ggplot(heaSW, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = 978, color = "black")) +
  geom_vline(aes(xintercept = 7801, color = "black")) +
  geom_rect(aes(xmin=800, xmax=1125, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7605, xmax=7999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
SW_all1 <- ggarrange(SW1, SW2,SW3,SW4,SW5,SW6,SW7,SW8,SW9,
                    ncol = 1, nrow = 9)
annotate_figure(SW_all1, top = text_grob("SouthWest: the whole Holocene", color = "black", face = "bold"))

# SOUTHMID ----

spdSM <- data.frame(age=alldataSMs$yearsBP, spd=alldataSMs$SPD)
SM1 <- ggplot(spdSM, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 3514, color = "black")) +
  geom_vline(aes(xintercept = 5802, color = "black")) +
  geom_rect(aes(xmin=3400, xmax=3900, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=5623, xmax=5900, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="SPD")

climSM <- data.frame(age=alldataSMs$yearsBP, temp=alldataSMs$clim)
SM2 <- ggplot(climSM, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5944, color = "black")) +
  geom_rect(aes(xmin=5394, xmax=6650, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="", x="years BP", y="°C (annual mean)")

conSM <- data.frame(age=alldataSMs$yearsBP, con=alldataSMs$conSMs)
SM3 <- ggplot(conSM, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 6445, color = "black")) +
  geom_rect(aes(xmin=6197, xmax=6724, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$decSMs)
SM4 <- ggplot(decSM, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 4116, color = "black")) +
  geom_rect(aes(xmin=3636, xmax=4678, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$wetwSMs)
SM5 <- ggplot(wetwSM, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 4053, color = "black")) +
  geom_vline(aes(xintercept = 5049, color = "black")) +
  geom_rect(aes(xmin=3778, xmax=4398, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=5004, xmax=5099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$wetmSMs)
SM6 <- ggplot(wetmSM, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 3355, color = "black")) +
  geom_vline(aes(xintercept = 6850, color = "black")) +
  geom_vline(aes(xintercept = 7449, color = "black")) +
  geom_rect(aes(xmin=3196, xmax=3540, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=6805, xmax=6899, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=7401, xmax=7496, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasSM <- data.frame(age=alldataSMs$yearsBP, pas=alldataSMs$pasSMs)
SM7 <- ggplot(pasSM, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 4054, color = "black")) +
  geom_rect(aes(xmin=3803, xmax=4284, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

araSM <- data.frame(age=alldataSMs$yearsBP, ara=alldataSMs$araSMs)
SM8 <- ggplot(araSM, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1650, color = "black")) +
  geom_vline(aes(xintercept = 3665, color = "black")) +
  geom_rect(aes(xmin=1601, xmax=1696, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=2300, xmax=4453, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$heaSMs)
SM9 <- ggplot(heaSM, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  #geom_vline(aes(xintercept = 978, color = "black")) +
  #geom_vline(aes(xintercept = 7801, color = "black")) +
  #geom_rect(aes(xmin=800, xmax=1125, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  #geom_rect(aes(xmin=7605, xmax=7999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="", x="years BP", y="relative pollen abundance") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
SM_all1 <- ggarrange(SM1, SM2,SM3,SM4,SM5,SM6,SM7,SM8,SM9,
                    ncol = 1, nrow = 9)
annotate_figure(SM_all1, top = text_grob("SouthMid: the whole Holocene", color = "black", face = "bold"))

# ------ COUNT PLOTS ------

alldataNs <-read.csv("./Processed_data/Full_datasets/alldataNs_count.csv")
alldataMMs <-read.csv("./Processed_data/Full_datasets/alldataMMs_count.csv")
alldataMWs <-read.csv("./Processed_data/Full_datasets/alldataMWs_count.csv")
alldataSEs <-read.csv("./Processed_data/Full_datasets/alldataSEs_count.csv")
alldataSMs <-read.csv("./Processed_data/Full_datasets/alldataSMs_count.csv")
alldataSWs <-read.csv("./Processed_data/Full_datasets/alldataSWs_count.csv")

dir.create(file.path("./Results/", "Plots"), showWarnings = FALSE)

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
SPDN <- data.frame(age=alldataNs$yearsBP, spd=alldataNs$SPD)
N1 <- ggplot(SPDN, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 2410, color = "black")) +
  geom_vline(aes(xintercept = 3447, color = "black")) +
  geom_rect(aes(xmin=2309, xmax=2499, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=3384, xmax=3500, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

climN <- data.frame(age=alldataNs$yearsBP, temp=alldataNs$clim)
N2 <- ggplot(climN, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 2694, color = "black")) +
  geom_vline(aes(xintercept = 7932, color = "black")) +
  geom_rect(aes(xmin=2600, xmax=2824, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  geom_rect(aes(xmin=7825, xmax=7999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

conN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$conNs)
N3 <- ggplot(conN, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 2361, color = "black")) +
  geom_vline(aes(xintercept = 5243, color = "black")) +
  geom_vline(aes(xintercept = 6161, color = "black")) +
  geom_vline(aes(xintercept = 7148, color = "black")) +
  geom_rect(aes(xmin=2000, xmax=2706, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=5185, xmax=5300, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=6100, xmax=6370, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=6800, xmax=7506, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="pollen count")

decN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$decNs)
N4 <- ggplot(decN, aes(x=age))+
  scale_colour_manual(values=c(gold="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 3007, color = "black")) +
  geom_vline(aes(xintercept = 7037, color = "black")) +
  geom_rect(aes(xmin=2735, xmax=3299, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=6867, xmax=7199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="pollen count")

wetwN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$wetwNs)
N5 <- ggplot(wetwN, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 5169, color = "black")) +
  geom_vline(aes(xintercept = 7957, color = "black")) +
  geom_rect(aes(xmin=4922, xmax=5299, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=7900, xmax=8034, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET WOODLAND", x="years BP", y="pollen count")

wetmN <- data.frame(age=alldataNs$yearsBP, wetm=alldataNs$wetmNs)
N6 <- ggplot(wetmN, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=wetm), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 2942, color = "black")) +
  geom_vline(aes(xintercept = 5067, color = "black")) +
  geom_rect(aes(xmin=2898, xmax=2999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blueviolet") +
  geom_rect(aes(xmin=5000, xmax=5192, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET MEADOW", x="years BP", y="pollen count")

pasN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$pasNs)
N7 <- ggplot(pasN, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 6939, color = "black")) +
  geom_rect(aes(xmin=6821, xmax=6999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="PASTURE", x="years BP", y="pollen count")

araN <- data.frame(age=alldataNs$yearsBP, LCC=alldataNs$araNs)
N8 <- ggplot(araN, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1855, color = "black")) +
  geom_rect(aes(xmin=1725, xmax=1999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="ARABLE LAND", x="years BP", y="pollen count")

heaN <- data.frame(age=alldataNs$yearsBP, hea=alldataNs$heaNs)
N9 <- ggplot(heaN, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=hea), color="orangered", size=1.3)+
  geom_vline(aes(xintercept = 6533, color = "black")) +
  geom_vline(aes(xintercept = 7678, color = "black")) +
  geom_rect(aes(xmin=6385, xmax=6599, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7414, xmax=7899, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 2500, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="HEATH", x="years BP", y="pollen count")

# PLOT
N_all <- ggarrange(N1,N2,N3,N4,N5,N6,N7,N8,N9, 
                   ncol = 1, nrow = 9)
annotate_figure(N_all, top = text_grob("North", color = "black", face = "bold"))

# SOUTHEAST ----

spdSE <- data.frame(age=alldataSEs$yearsBP, spd=alldataSEs$SPD)
SE1 <- ggplot(spdSE, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 1380, color = "black")) +
  geom_vline(aes(xintercept = 2944, color = "black")) +
  geom_rect(aes(xmin=1300, xmax=1484, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=2800, xmax=3059, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

climSE <- data.frame(age=alldataSEs$yearsBP, temp=alldataSEs$clim)
SE2 <- ggplot(climSE, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5964, color = "black")) +
  geom_rect(aes(xmin=5294, xmax=6601, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

conSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$conSEs)
SE3 <- ggplot(conSE, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 849, color = "black")) +
  geom_vline(aes(xintercept = 2049, color = "black")) +
  geom_vline(aes(xintercept = 4840, color = "black")) +
  geom_rect(aes(xmin=802, xmax=899, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=2002, xmax=2098, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=4524, xmax=5100, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="pollen count")

decSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$decSEs)
SE4 <- ggplot(decSE, aes(x=age))+
  scale_colour_manual(values=c(gold="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 1270, color = "black")) +
  geom_vline(aes(xintercept = 4358, color = "black")) +
  geom_rect(aes(xmin=1200, xmax=1428, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=4300, xmax=4438, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="pollen count")

wetwSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$wetwSEs)
SE5 <- ggplot(wetwSE, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 4339, color = "black")) +
  geom_rect(aes(xmin=4200, xmax=4455, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET WOODLAND", x="years BP", y="pollen count")

wetmSE <- data.frame(age=alldataSEs$yearsBP, wetm=alldataSEs$wetmSEs)
SE6 <- ggplot(wetmSE, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=wetm), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 1248, color = "black")) +
  geom_vline(aes(xintercept = 2594, color = "black")) +
  geom_vline(aes(xintercept = 4742, color = "black")) +
  geom_vline(aes(xintercept = 8193, color = "black")) +
  geom_rect(aes(xmin=1166, xmax=1299, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=2500, xmax=2693, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=4632, xmax=4800, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=8001, xmax=8399, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="WET MEADOW", x="years BP", y="pollen count")

pasSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$pasSEs)
SE7 <- ggplot(pasSE, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 574, color = "black")) +
  geom_vline(aes(xintercept = 1350, color = "black")) +
  geom_vline(aes(xintercept = 8549, color = "black")) +
  geom_rect(aes(xmin=500, xmax=681, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=1304, xmax=1399, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=8504, xmax=8599, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="PASTURE", x="years BP", y="pollen count")

araSE <- data.frame(age=alldataSEs$yearsBP, LCC=alldataSEs$araSEs)
SE8 <- ggplot(araSE, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 650, color = "black")) +
  geom_vline(aes(xintercept = 949, color = "black")) +
  geom_rect(aes(xmin=603, xmax=698, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  geom_rect(aes(xmin=902, xmax=996, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="ARABLE LAND", x="years BP", y="pollen count")

heaSE <- data.frame(age=alldataSEs$yearsBP, hea=alldataSEs$heaSEs)
SE9 <- ggplot(heaSE, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=hea), color="orangered", size=1.3)+
  geom_vline(aes(xintercept = 1249, color = "black")) +
  geom_vline(aes(xintercept = 2456, color = "black")) +
  geom_rect(aes(xmin=1202, xmax=1297, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=2400, xmax=2505, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 4000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="HEATH", x="years BP", y="pollen count")

# PLOT #
SE_all <- ggarrange(SE1,SE2,SE3,SE4,SE5,SE6,SE7,SE8,SE9, 
                    ncol = 1, nrow = 9)
annotate_figure(SE_all, top = text_grob("SouthEast: the whole Holocene", color = "black", face = "bold"))

# MIDWEST ----
spdMW <- data.frame(age=alldataMWs$yearsBP, spd=alldataMWs$SPD)
MW1 <- ggplot(spdMW, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 750, color = "firebrick4")) +
  geom_vline(aes(xintercept = 850, color = "firebrick4")) +
  geom_vline(aes(xintercept = 1702, color = "firebrick4")) +
  geom_vline(aes(xintercept = 2848, color = "firebrick4")) +
  geom_rect(aes(xmin=705, xmax=799, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=805, xmax=899, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=1600, xmax=1833, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=2800, xmax=2895, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

climMW <- data.frame(age=alldataMWs$yearsBP, temp=alldataMWs$clim)
MW2 <- ggplot(climMW, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5961, color = "black")) +
  geom_rect(aes(xmin=5431, xmax=6296, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

conMW <- data.frame(age=alldataMWs$yearsBP, con=alldataMWs$conMWs)
MW3 <- ggplot(conMW, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 2899, color = "black")) +
  geom_vline(aes(xintercept = 3939, color = "black")) +
  geom_vline(aes(xintercept = 7127, color = "black")) +
  geom_rect(aes(xmin=2810, xmax=2999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=3900, xmax=4077, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=6334, xmax=7399, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$decMWs)
MW4 <- ggplot(decMW, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 2953, color = "black")) +
  geom_vline(aes(xintercept = 4068, color = "black")) +
  geom_vline(aes(xintercept = 5739, color = "black")) +
  geom_vline(aes(xintercept = 6450, color = "black")) +
  geom_vline(aes(xintercept = 8258, color = "black")) +
  geom_rect(aes(xmin=2900, xmax=2999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=3958, xmax=4199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=5598, xmax=5799, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=6403, xmax=6498, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=8000, xmax=8472, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$wetwMWs)
MW5 <- ggplot(wetwMW, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 3005, color = "black")) +
  geom_vline(aes(xintercept = 4023, color = "black")) +
  geom_vline(aes(xintercept = 6011, color = "black")) +
  geom_vline(aes(xintercept = 7949, color = "black")) +
  geom_rect(aes(xmin=2806, xmax=3199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=3803, xmax=4139, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=5913, xmax=6099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=7903, xmax=7998, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$wetmMWs)
MW6 <- ggplot(wetmMW, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 3025, color = "black")) +
  geom_vline(aes(xintercept = 4116, color = "black")) +
  geom_rect(aes(xmin=2900, xmax=3102, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=3906, xmax=4399, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET MEADOW", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

fplot.pasmw_df <- data.frame(age=alldataMWs$yearsBP, pas=alldataMWs$pasMWs)
MW7 <- ggplot(fplot.pasmw_df, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 928, color = "black")) +
  geom_vline(aes(xintercept = 1283, color = "black")) +
  geom_rect(aes(xmin=804, xmax=999, ymin=-Inf, ymax=Inf), alpha=0.01, fill = "orange2") +
  geom_rect(aes(xmin=1200, xmax=1385, ymin=-Inf, ymax=Inf), alpha=0.01, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="PASTURE", x="years BP", y="pollen count") +
  theme(legend.position="none")

fplot.aramw_df <- araMW <- data.frame(age=alldataMWs$yearsBP, ara=alldataMWs$araMWs, spd=alldataMWs$SPD)
MW8 <- ggplot(fplot.aramw_df, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 3925, color = "black")) +
  geom_vline(aes(xintercept = 8248, color = "black")) +
  geom_rect(aes(xmin=3730, xmax=4099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=8200, xmax=8294, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="ARABLE LAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaMW <- data.frame(age=alldataMWs$yearsBP, LCC=alldataMWs$heaMWs)
MW9 <- ggplot(heaMW, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = 2470, color = "black")) +
  geom_vline(aes(xintercept = 3981, color = "black")) +
  geom_vline(aes(xintercept = 8315, color = "black")) +
  geom_rect(aes(xmin=1745, xmax=2899, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=3900, xmax=4087, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7894, xmax=8499, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="HEATH", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
MW_all <- ggarrange(MW1, MW2, MW3, MW4, MW5, MW6, MW7,MW8,MW9,
                    ncol = 1, nrow = 9)
annotate_figure(MW_all, top = text_grob("MidWest: the whole Holocene", color = "black", face = "bold"))

# MidMid ----
spdMM <- data.frame(age=alldataMMs$yearsBP, spd=alldataMMs$SPD)
MM1 <- ggplot(spdMM, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 851, color = "firebrick4")) +
  geom_vline(aes(xintercept = 1350, color = "firebrick4")) +
  geom_vline(aes(xintercept = 2613, color = "firebrick4")) +
  geom_rect(aes(xmin=804, xmax=899, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=1304, xmax=1399, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  geom_rect(aes(xmin=2399, xmax=2800, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

climMM <- data.frame(age=alldataMMs$yearsBP, temp=alldataMMs$clim)
MM2 <- ggplot(climMM, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5931, color = "black")) +
  geom_rect(aes(xmin=5401, xmax=6299, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

conMM <- data.frame(age=alldataMMs$yearsBP, con=alldataMMs$conMMs)
MM3 <- ggplot(conMM, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 3891, color = "black")) +
  geom_vline(aes(xintercept = 5845, color = "black")) +
  geom_rect(aes(xmin=3800, xmax=3997, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=5494, xmax=6835, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$decMMs)
MM4 <- ggplot(decMM, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 2399, color = "black")) +
  geom_vline(aes(xintercept = 3860, color = "black")) +
  geom_vline(aes(xintercept = 8182, color = "black")) +
  geom_rect(aes(xmin=2290, xmax=2697, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=3799, xmax=3976, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_rect(aes(xmin=7831, xmax=8399, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$wetwMMs)
MM5 <- ggplot(wetwMM, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 3842, color = "black")) +
  geom_vline(aes(xintercept = 7139, color = "black")) +
  geom_rect(aes(xmin=3116, xmax=4095, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=7015, xmax=7199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$wetmMMs)
MM6 <- ggplot(wetmMM, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 1436, color = "black")) +
  geom_vline(aes(xintercept = 5918, color = "black")) +
  geom_rect(aes(xmin=1200, xmax=1518, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=5484, xmax=6315, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="WET MEADOW", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasMM <- data.frame(age=alldataMMs$yearsBP, pas=alldataMMs$pasMMs)
MM7 <- ggplot(pasMM, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 1448, color = "black")) +
  geom_vline(aes(xintercept = 1950, color = "black")) +
  geom_vline(aes(xintercept = 4106, color = "black")) +
  geom_vline(aes(xintercept = 7948, color = "black")) +
  geom_rect(aes(xmin=1401, xmax=1497, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=1900, xmax=1996, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=3900, xmax=4479, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=7900, xmax=7996, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="PASTURE", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

araMM <- data.frame(age=alldataMMs$yearsBP, ara=alldataMMs$araMMs)
MM8 <- ggplot(araMM, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1877, color = "black")) +
  geom_vline(aes(xintercept = 3777, color = "black")) +
  geom_vline(aes(xintercept = 5931, color = "black")) +
  geom_rect(aes(xmin=1700, xmax=1999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=3700, xmax=3882, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=5816, xmax=5999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="ARABLE LAND", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaMM <- data.frame(age=alldataMMs$yearsBP, LCC=alldataMMs$heaMMs)
MM9 <- ggplot(heaMM, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = 1450, color = "black")) +
  geom_vline(aes(xintercept = 7877, color = "black")) +
  geom_rect(aes(xmin=1404, xmax=1499, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7800, xmax=7987, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 3200, color = "hotpink"), size=1) +
  labs(title="HEATH", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
MM_all <- ggarrange(MM1, MM2, MM3, MM4, MM5, MM6, MM7,MM8,MM9,
                    ncol = 1, nrow = 9)
annotate_figure(MM_all, top = text_grob("MidMid: the whole Holocene", color = "black", face = "bold"))


# SOUTHWEST ----

spdSW <- data.frame(age=alldataSWs$yearsBP, spd=alldataSWs$SPD)
SW1 <- ggplot(spdSW, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 871, color = "firebrick4")) +
  geom_vline(aes(xintercept = 2346, color = "firebrick4")) +
  geom_rect(aes(xmin=800, xmax=978, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=2200, xmax=2400, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

climSW <- data.frame(age=alldataSWs$yearsBP, temp=alldataSWs$clim)
SW2 <- ggplot(climSW, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5947, color = "black")) +
  geom_rect(aes(xmin=5400, xmax=6459, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

conSW <- data.frame(age=alldataSWs$yearsBP, con=alldataSWs$conSWs)
SW3 <- ggplot(conSW, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 1565, color = "black")) +
  geom_vline(aes(xintercept = 3621, color = "black")) +
  geom_vline(aes(xintercept = 7702, color = "black")) +
  geom_rect(aes(xmin=1500, xmax=1695, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=2901, xmax=4643, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_rect(aes(xmin=7609, xmax=7799, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$decSWs)
SW4 <- ggplot(decSW, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 4207, color = "black")) +
  geom_rect(aes(xmin=3400, xmax=5576, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$wetwSWs)
SW5 <- ggplot(wetwSW, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 3513, color = "black")) +
  geom_vline(aes(xintercept = 5517, color = "black")) +
  geom_vline(aes(xintercept = 8334, color = "black")) +
  geom_rect(aes(xmin=3405, xmax=3599, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=5302, xmax=5698, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=8161, xmax=8590, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$wetmSWs)
SW6 <- ggplot(wetmSW, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 1032, color = "black")) +
  geom_vline(aes(xintercept = 1550, color = "black")) +
  geom_vline(aes(xintercept = 3786, color = "black")) +
  geom_rect(aes(xmin=849, xmax=1199, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=1503, xmax=1599, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=3339, xmax=4290, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET MEADOW", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasSW <- data.frame(age=alldataSWs$yearsBP, pas=alldataSWs$pasSWs)
SW7 <- ggplot(pasSW, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 1053, color = "black")) +
  geom_vline(aes(xintercept = 3032, color = "black")) +
  geom_vline(aes(xintercept = 7733, color = "black")) +
  geom_rect(aes(xmin=999, xmax=1139, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=2700, xmax=3259, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_rect(aes(xmin=6824, xmax=8099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="PASTURE", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

araSW <- data.frame(age=alldataSWs$yearsBP, ara=alldataSWs$araSWs)
SW8 <- ggplot(araSW, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1150, color = "black")) +
  geom_rect(aes(xmin=1104, xmax=1199, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="ARABLE LAND", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaSW <- data.frame(age=alldataSWs$yearsBP, LCC=alldataSWs$heaSWs)
SW9 <- ggplot(heaSW, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  geom_vline(aes(xintercept = 978, color = "black")) +
  geom_vline(aes(xintercept = 7801, color = "black")) +
  geom_rect(aes(xmin=800, xmax=1125, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_rect(aes(xmin=7605, xmax=7999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="HEATH", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
SW_all <- ggarrange(SW1, SW2,SW3,SW4,SW5,SW6,SW7,SW8,SW9,
                    ncol = 1, nrow = 9)
annotate_figure(SW_all, top = text_grob("SouthWest: the whole Holocene", color = "black", face = "bold"))

# SOUTHMID ----

spdSM <- data.frame(age=alldataSMs$yearsBP, spd=alldataSMs$SPD)
SM1 <- ggplot(spdSM, aes(x=age))+
  scale_colour_manual(values=c(firebrick4="firebrick4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=spd), color="firebrick4", size=1.3)+
  geom_vline(aes(xintercept = 3514, color = "black")) +
  geom_vline(aes(xintercept = 5802, color = "black")) +
  geom_rect(aes(xmin=3400, xmax=3900, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  geom_rect(aes(xmin=5623, xmax=5900, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "firebrick4") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="SPD (predictor)", x="years BP", y="SPD")

climSM <- data.frame(age=alldataSMs$yearsBP, temp=alldataSMs$clim)
SM2 <- ggplot(climSM, aes(x=age))+
  scale_colour_manual(values=c(blue2="blue2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=temp), color="blue2", size=1.3)+
  geom_vline(aes(xintercept = 5944, color = "black")) +
  geom_rect(aes(xmin=5394, xmax=6650, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blue2") +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  theme(legend.position="none")+
  theme(plot.background = element_rect(color = "grey", size = 1))+
  labs(title="TEMPERATURE (predictor)", x="years BP", y="°C (annual mean)")

conSM <- data.frame(age=alldataSMs$yearsBP, con=alldataSMs$conSMs)
SM3 <- ggplot(conSM, aes(x=age))+
  scale_colour_manual(values=c(chartreuse4="chartreuse4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=con), color="chartreuse4", size=1.3)+
  geom_vline(aes(xintercept = 6445, color = "black")) +
  geom_rect(aes(xmin=6197, xmax=6724, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "chartreuse4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="CONIFEROUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

decSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$decSMs)
SM4 <- ggplot(decSM, aes(x=age))+
  scale_colour_manual(values=c(seagreen2="seagreen2",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="seagreen2", size=1.3)+
  geom_vline(aes(xintercept = 4116, color = "black")) +
  geom_rect(aes(xmin=3636, xmax=4678, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "seagreen2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="DECIDUOUS WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetwSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$wetwSMs)
SM5 <- ggplot(wetwSM, aes(x=age))+
  scale_colour_manual(values=c(turquoise4="turquoise4",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="turquoise4", size=1.3)+
  geom_vline(aes(xintercept = 4053, color = "black")) +
  geom_vline(aes(xintercept = 5049, color = "black")) +
  geom_rect(aes(xmin=3778, xmax=4398, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_rect(aes(xmin=5004, xmax=5099, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "turquoise4") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET WOODLAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

wetmSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$wetmSMs)
SM6 <- ggplot(wetmSM, aes(x=age))+
  scale_colour_manual(values=c(blueviolet="blueviolet",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="blueviolet", size=1.3)+
  geom_vline(aes(xintercept = 3355, color = "black")) +
  geom_vline(aes(xintercept = 6850, color = "black")) +
  geom_vline(aes(xintercept = 7449, color = "black")) +
  geom_rect(aes(xmin=3196, xmax=3540, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=6805, xmax=6899, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_rect(aes(xmin=7401, xmax=7496, ymin=-Inf, ymax=Inf), alpha=0.005, fill = "blueviolet") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="WET MEADOW", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

pasSM <- data.frame(age=alldataSMs$yearsBP, pas=alldataSMs$pasSMs)
SM7 <- ggplot(pasSM, aes(x=age))+
  scale_colour_manual(values=c(orange2="orange2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  geom_line(aes(y=pas), color="orange2", size=1.3)+
  geom_vline(aes(xintercept = 4054, color = "black")) +
  geom_rect(aes(xmin=3803, xmax=4284, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orange2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="PASTURE", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size=10),
        plot.background = element_rect(color = "grey", size = 1))

araSM <- data.frame(age=alldataSMs$yearsBP, ara=alldataSMs$araSMs)
SM8 <- ggplot(araSM, aes(x=age))+
  scale_colour_manual(values=c(gold2="gold2",black="black",hotpink="hotpink",firebrick4="firebrick4")) +
  #ara
  geom_line(aes(y=ara), color="gold2", size=1.3)+
  geom_vline(aes(xintercept = 1650, color = "black")) +
  geom_vline(aes(xintercept = 3665, color = "black")) +
  geom_rect(aes(xmin=1601, xmax=1696, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_rect(aes(xmin=2300, xmax=4453, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "gold2") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="ARABLE LAND", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.background = element_rect(color = "grey", size = 1))

heaSM <- data.frame(age=alldataSMs$yearsBP, LCC=alldataSMs$heaSMs)
SM9 <- ggplot(heaSM, aes(x=age))+
  scale_colour_manual(values=c(orangered="orangered",black="black",hotpink="hotpink")) +
  geom_line(aes(y=LCC), color="orangered", size=1)+
  #geom_vline(aes(xintercept = 978, color = "black")) +
  #geom_vline(aes(xintercept = 7801, color = "black")) +
  #geom_rect(aes(xmin=800, xmax=1125, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  #geom_rect(aes(xmin=7605, xmax=7999, ymin=-Inf, ymax=Inf), alpha=0.006, fill = "orangered") +
  geom_vline(aes(xintercept = 6000, color = "hotpink"), size=1) +
  scale_x_reverse(breaks = c(9000,8000,7000, 6000,5000,4000, 3000,2000,1000))+
  labs(title="HEATH", x="years BP", y="pollen count") +
  theme(legend.position="none",
        plot.title = element_text(size = 12),
        plot.background = element_rect(color = "grey", size = 1))

# PLOT #
SM_all <- ggarrange(SM1, SM2,SM3,SM4,SM5,SM6,SM7,SM8,SM9,
                    ncol = 1, nrow = 9)
annotate_figure(SM_all, top = text_grob("SouthMid: the whole Holocene", color = "black", face = "bold"))

#### COMBINES PLOTS #### -----

N_rel_count = ggarrange(N_all, N_all1, ncol = 2, nrow = 1)
png(filename="./Results/Plots/N_rel_count.png", width = 1500, height = 1500,)
plot(N_rel_count)
dev.off()

SE_rel_count = ggarrange(SE_all, SE_all1, ncol = 2, nrow = 1)
png(filename="./Results/Plots/SE_rel_count.png", width = 1500, height = 1500,)
plot(SE_rel_count)
dev.off()

MW_rel_count = ggarrange(MW_all, MW_all1, ncol = 2, nrow = 1)
png(filename="./Results/Plots/MW_rel_count.png", width = 1500, height = 1500,)
plot(MW_rel_count)
dev.off()

MM_rel_count = ggarrange(MM_all, MM_all1, ncol = 2, nrow = 1)
png(filename="./Results/Plots/MM_rel_count.png", width = 1500, height = 1500,)
plot(MM_rel_count)
dev.off()

SW_rel_count = ggarrange(SW_all, SW_all1, ncol = 2, nrow = 1)
png(filename="./Results/Plots/SW_rel_count.png", width = 1500, height = 1500,)
plot(SW_rel_count)
dev.off()

SM_rel_count = ggarrange(SM_all, SM_all1, ncol = 2, nrow = 1)
png(filename="./Results/Plots/SM_rel_count.png", width = 1500, height = 1500,)
plot(SM_rel_count)
dev.off()