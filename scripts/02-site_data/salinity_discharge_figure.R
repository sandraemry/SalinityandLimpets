library(HH)
library(here)
library(ggplot2)
library(scales)

salinity <- read.csv(here::here("data", "raw", "Salinity Data.csv"), strip.white=TRUE)

discharge <- read.csv(here::here("data", "raw", "Discharge.csv"))

salinity$Date <- as.Date(salinity$Date,"%d/%m/%Y")
discharge$Date <- as.Date(discharge$Date,"%d/%m/%Y")

locs <- tapply(X=discharge$Date, FUN=min, INDEX=format(discharge$Date,'%Y%m')) #taking the minimum for each month I think?
t(t(locs))
at <- discharge$Date%in%locs
discharge$Date[at]
at2 <- at&format(discharge$Date,'%m') %in% c('01','03','05','07','09','11')
discharge$Date[at2]

all_data <- left_join(discharge, salinity, by = "Date") %>% 
  mutate(Discharge = Discharge/1000)

salinity$Salinity.Group <- factor(salinity$Salinity.Group, levels = c("L", "H"))
salinity$Site[salinity$Site == "HS"] <- "HS1"
salinity$Site[salinity$Site == "EC"] <- "HS2"
salinity$Site[salinity$Site == "RP"] <- "HS3"
salinity$Site[salinity$Site == "LB"] <- "LS1"
salinity$Site[salinity$Site == "CC"] <- "LS2"
salinity$Site[salinity$Site == "RS"] <- "LS3"

salinity$Site <- factor(salinity$Site, levels = c("LS1", "LS2", "LS3", "HS1", "HS2", "HS3"))

salinity_discharge <- ggplot(data = all_data) + 
  geom_point(data = salinity, aes(x = Date, y = Salinity, color = Site, shape = Site), size = 2) + 
  geom_line(aes(x = Date, y = Discharge*3.5), linetype = "dotted") + 
  scale_color_manual(name = "Sites", labels = c("LS1", "LS2", "LS3", "HS1", "HS2", "HS3"), values = c("orange","orange","orange", "steelblue", "steelblue", "steelblue")) + 
  scale_shape_manual(name = "Sites", , labels = c("LS1", "LS2", "LS3", "HS1", "HS2", "HS3"), values = c(21, 22, 24, 21, 22, 24)) + 
  scale_y_continuous(name = "Salinity (psu)",
                     breaks = seq(0,35,5), 
                     limits = c(0,35),
                     sec.axis = sec_axis(trans = ~ . /3.5, 
                                         name = expression("Fraser River discharge (10"^3~"m"^3~"s"^-1~")"),
                                         labels = seq(0,10,2),
                                         breaks = seq(0,10,2))) + 
  scale_x_date(date_labels = '%b-%y', date_breaks = "2 months") + 
  labs(x = "") + 
  theme_classic() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "right")

ggsave("./figures/salinity_discharge.jpeg", plot = salinity_discharge, width = 7, height = 4.5, dpi=300)

                                       