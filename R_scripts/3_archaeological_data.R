# Data cleaning for human footprint data
# Last modified: 12/10/2024 by G.Vernham

### Install/load libraries ### ----
libs <- c("dplyr","ggplot2", "rnaturalearth", "rcarbon", "sf", "rnaturalearthdata")

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

dir.create(file.path("./Processed_data/", "archeological_data"), showWarnings = FALSE)

# Load Natural Earth world data ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Load Human footprint data
footprint <- p3k14c <- read.csv("./Raw_Data/p3k14c_original.csv")

### DATA WRANGLING ###
footprint <- footprint %>% subset(Long >= 4 & Long < 42) %>% 
  subset(Lat >= 55 & Lat < 71) %>% 
  subset(Age <= 15000)

human.withoutRussia = footprint %>%
  dplyr::filter(Country %in% c('Sweden', 'Finland', 'Norway')) %>%
  dplyr::filter(!Country == "Sweden" | !SiteName %in% "Auve")

human.footprint = footprint %>%
  dplyr::filter(Country %in% c('Sweden', 'Finland', 'Norway',"Russian Federation")) %>%
  dplyr::filter(!Country == "Sweden" | !SiteName %in% "Auve") %>%
  dplyr::filter(!Country == "Russian Federation" | 
                  !SiteName %in% c("Sakhtysh 2", "Sungir Soungir Sunghir",
                                   "Stanovoe 4", "Ivanovskoye", "Jagerhaushohle",
                                   "Starnska Skala", "Stanovoje", "Zolotoruce",
                                   "Turginovo 5", "Vergara", "Berendeevo 2a",
                                   "Ivanovskoe 3", "Podoli V", "Zolotovka"))

##################################
# Taken from SPD.R (Consider placing here instead?)
human.footprint$SiteName <- sub("_", "", human.footprint$SiteName)
hfSubset <- human.footprint[grep("Sundfj", human.footprint$SiteName), ]
hfSubset$SiteName <- "Sundfj"
human.footprint <- human.footprint[-grep("Sundfj", human.footprint$SiteName), ]
human.footprint <- rbind(human.footprint, hfSubset)
###################################

#footprint <- subset(footprint, Lat >= 61 & Lat < 67,
#                    select=c(ID, Weight))

Material = data.frame(unique(human.footprint$Material))

Age <- human.footprint %>% dplyr::select("Age") %>%
  dplyr::arrange(Age)

archaeological.data <- human.footprint %>% dplyr::select("Age", "Material") %>%
  dplyr::arrange(Age) %>%
  dplyr::rename(human.footprint.Age = Age,
                human.footprint.Material = Material)

### WRITE WRANGLED DATA ###
write.csv(human.withoutRussia, file = "./Processed_data/archeological_data/human.withoutRussia.csv")
write.csv(human.footprint, file = "./Processed_data/archeological_data/human.footprint.csv")
write.csv(Material, file = "./Processed_data/archeological_data/human.footprint.Material.csv")
write.csv(Age, file = "./Processed_data/archeological_data/human.footprint.Age.csv")
write.csv(archaeological.data, file = "./Processed_data/archeological_data/archaeological.data.csv")


### DATA EXPLORATION ###
# Map footprint

theme_set(theme_bw()) # classic dark on light theme

ggplot2::ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(4, 42), ylim = c(55, 71), expand = FALSE)


(sites <- data.frame(longitude = human.footprint$Long, latitude = human.footprint$Lat))
(sites <- data.frame(longitude = human.withoutRussia$Long, latitude = human.withoutRussia$Lat))

ggplot2::ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(4, 42), ylim = c(55, 71), expand = FALSE) +
  ggtitle("archaeological.data")

ggplot2::ggplot(human.footprint, aes(x = Age)) +
  geom_line(aes(y = Age, colour="red")) # GRANT:Unsure why we are plotting a variable against itself?

ggplot2::ggplot(human.footprint, aes(x=Age, y=Lat)) + geom_point() +
  ggtitle("archaeological.data") +
  labs(x = "Year BP",y = "Latitude")
