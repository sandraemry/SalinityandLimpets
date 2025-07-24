library(tidyverse)
library(gridExtra)
library(ggpubr)
library(survival)
library(survminer)
library(kableExtra)
library(broom)


# Data Prep ---------------------------------------------------------------


#Load Data
figdata <-read_csv("./data/tidy/mortality_expmt_sum.csv")
figdata$Salinity <- as.factor(figdata$Salinity)
figdata$site <- as.factor(figdata$site)

# Change treatment to population
figdata$Treatment[figdata$Treatment == "Low"] <- "West Vancouver"
figdata$Treatment[figdata$Treatment == "High"] <- "Galiano Island"

figdata <- figdata %>% rename(site = Treatment)



############### Modelling proportion dead as a function of population and salinity ###############


head(figdata)

# need a row for each limpet, and a column for how long that limpet lived for (integer from 1 - 28), 
# and a column for 'event' = 1 if dead and 0 if still alive at the end

data <- figdata %>% 
  mutate(number_alive = round(6*Prop_Alive)) %>% 
  mutate(total = 6) %>% 
  mutate(num_dead = 6 - number_alive) %>% 
  select(-c(N, Std_Error, number_alive, Prop_Alive))

binary_dat <-  pmap_dfr(data, 
                        function(site, Salinity, Day, num_dead, total) {
                          data.frame(site = site,
                                     Salinity = Salinity,
                                     Day = Day,
                                     status = c( rep(1, num_dead),
                                               rep(0, total - num_dead) ) )
                        }
  )

# create an ID column 
binary_dat$id <- paste(binary_dat$site, binary_dat$Salinity, rep(1:6, nrow(binary_dat)/6), sep = "_")

limpets_dead_end <- binary_dat %>% 
  mutate(Salinity = as.numeric(Salinity)) %>% 
           filter(Day == 28, status == 1) %>% distinct(id)

day_of_death <- data.frame(id = character(), Day = integer())

for (i in 1:nrow(limpets_dead_end)) {
  day_of_death[i, ] <- binary_dat %>% 
    filter(id == limpets_dead_end[i, ]) %>% 
    filter(status == 1) %>% 
    select(id, Day) %>% 
    .[1, ]
}

dead_limpets <- right_join(binary_dat, day_of_death)

alive_limpets <- binary_dat %>% filter(Day == 28, status == 0)

new_data <- rbind(dead_limpets, alive_limpets) %>% arrange(site, Salinity)


# Kaplan-Meier survival curves --------------------------------------------

# create a survival object to be fit  
surv_object <- Surv(time = new_data$Day, event = new_data$status)

# create Kaplan-Meier survival curves 
fit <- survfit(surv_object ~ Salinity + site, data = new_data)

print(fit, rmean = 5)

KMplot <- ggsurvplot(fit, data = new_data, pval = FALSE, 
                     facet.by = "Salinity", 
                     # conf.int = TRUE,
                     # conf.int.style = "step",
                     legend.title = "region",
                     legend.labs = c("high salinity", "low salinity"),
                     xlab = "Day",
                     ggtheme = theme_classic(), 
                     palette = c('steelblue', 'orange'))

KMplot <- ggpubr::ggpar(KMplot,
              axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
              font.legend = list(size = 10, face = "bold", color = "grey30"),
              legend.text = element_text(size = 9, colour = "grey30"))

ggsave("./figures/limpet_mortality_salinity.png", KMplot)


# Cox proportional hazard analysis ----------------------------------------

new_data$Salinity <- as.numeric(as.character(new_data$Salinity))
new_data$site <- factor(new_data$site)
cox_model <- coxph(Surv(Day, status) ~ site + Salinity, data = new_data)

cox_model %>% gtsummary::tbl_regression(exp = TRUE)
summary(cox_model)

# checking diagnostic plots and stats
fit_test <- cox.zph(cox_model)
print(fit_test) # display the results
ggcoxzph(fit_test)

ggforest(cox_model, data = new_data)

# Restricted Mean Survival Time
status <- new_data$status
new_data$site[new_data$site == "West Vancouver"] <- 1
new_data$site[new_data$site == "Galiano Island"] <- 0
arm <- new_data$site
tau <- 7
x <- tibble(as.numeric(as.character(new_data$Salinity)))

rmst2(time, status, arm, tau = tau)



# Fitting a binomial glm  -------------------------------------------------


raw_mort <- read_csv("./data/raw/limpet_mortality_local_adaptation.csv")

raw_mort <- raw_mort %>% 
  clean_names() %>% 
  select(site:tank) 
  
# raw_mort$id <- 1:nrow(raw_mort)

test <- raw_mort %>% 
  # filter(site == "High", salinity == 5, tank == "B") %>% 
  group_by(salinity, site, tank, days_survived) %>%
  tally() %>% 
  mutate(total = 6) %>% 
  rename(day = days_survived, number_dead = n) %>% 
  mutate(number_alive = total - number_dead) 




# Survival analysis by site -----------------------------------------------

westvan_data <- binary_dat %>% filter(site == "West Vancouver")
galiano_data <- binary_dat %>% filter(site == "Galiano Island")

wv_surv_object <- Surv(time = westvan_data$Day, event = westvan_data$status)

wv_fit <- survfit(wv_surv_object ~ Salinity, data = westvan_data)

westvan_KMplot <- ggsurvplot(wv_fit, data = westvan_data, pval = FALSE, 
           legend.labs=c("5", "8", "11", "14", "17", "20"), 
           legend.title = "Salinity",
           title = "West Vancouver", 
           ggtheme = theme_classic(), 
           legend = "none", 
           palette = "PuBuGn")

galiano_surv_object <- Surv(time = galiano_data$Day, event = galiano_data$status)


galiano_fit <- survfit(galiano_surv_object ~ Salinity, data = galiano_data)

galiano_KMplot <- ggsurvplot(galiano_fit, data = galiano_data, pval = FALSE, 
                             legend.labs=c("5", "8", "11", "14", "17", "20"), 
                             legend.title = "Salinity", 
                             title = "Galiano Island", 
                             ggtheme = theme_classic(), 
                             legend = c(0.2, 0.2),
                             palette = "PuBuGn")

splots <- list()
splots[[1]] <- westvan_KMplot 
splots[[2]] <- galiano_KMplot

# Arrange multiple ggsurvplots and print the output
arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 1, risk.table.height = 0.4)

res <- arrange_ggsurvplots(splots, print = FALSE)
ggsave("./figures/limpet_mortality_site.png", res)



# Survival Analysis by salinity -----------------------------------------------
five_data <- binary_dat %>% filter(Salinity == "5")
eight_data <- binary_dat %>% filter(Salinity == "8")
eleven_data <- binary_dat %>% filter(Salinity == "11")
fourteen_data <- binary_dat %>% filter(Salinity == "14")
seventeen_data <- binary_dat %>% filter(Salinity == "17")
twenty_data <- binary_dat %>% filter(Salinity == "20")

five_surv_object <- Surv(time = five_data$Day, event = five_data$status)

five_fit <- survfit(five_surv_object ~ site + Salinity, data = five_data)
summary(five_fit)

five_KMplot <- ggsurvplot(five_fit, data = five_data, pval = FALSE, 
                             legend.title = "Salinity",
                             title = "5 psu", 
                             ggtheme = theme_classic(), 
                             legend = "none", 
                             palette = c('#1b9e77', '#d95f02'))

eight_surv_object <- Surv(time = eight_data$Day, event = eight_data$status)

eight_fit <- survfit(eight_surv_object ~ site, data = eight_data)

eight_KMplot <- ggsurvplot(eight_fit, data = eight_data, pval = FALSE, 
                          legend.title = "Salinity",
                          title = "8 psu", 
                          ggtheme = theme_classic(), 
                          legend = "none", 
                          palette = c('#1b9e77', '#d95f02'))

eleven_surv_object <- Surv(time = eleven_data$Day, event = eleven_data$status)

eleven_fit <- survfit(eleven_surv_object ~ site, data = eleven_data)

eleven_KMplot <- ggsurvplot(eleven_fit, data = eleven_data, pval = FALSE, 
                           legend.title = "Salinity",
                           title = "11 psu", 
                           ggtheme = theme_classic(), 
                           legend = "none", 
                           palette = c('#1b9e77', '#d95f02'))

fourteen_surv_object <- Surv(time = fourteen_data$Day, event = fourteen_data$status)

fourteen_fit <- survfit(fourteen_surv_object ~ site, data = fourteen_data)

fourteen_KMplot <- ggsurvplot(fourteen_fit, data = fourteen_data, pval = FALSE, 
                            legend.title = "Salinity",
                            title = "14 psu", 
                            ggtheme = theme_classic(), 
                            legend = "none", 
                            palette = c('#1b9e77', '#d95f02'))

seventeen_surv_object <- Surv(time = seventeen_data$Day, event = seventeen_data$status)

seventeen_fit <- survfit(seventeen_surv_object ~ site, data = seventeen_data)

seventeen_KMplot <- ggsurvplot(seventeen_fit, data = seventeen_data, pval = FALSE, 
                              legend.title = "Salinity",
                              title = "17 psu", 
                              ggtheme = theme_classic(), 
                              legend = "none", 
                              palette = c('#1b9e77', '#d95f02'))

twenty_surv_object <- Surv(time = twenty_data$Day, event = twenty_data$status)

twenty_fit <- survfit(twenty_surv_object ~ site, data = twenty_data)

twenty_KMplot <- ggsurvplot(twenty_fit, data = twenty_data, pval = FALSE, 
                               legend.title = "Salinity",
                               legend.labs = c("Galiano Island", "West Vancouver"),
                               title = "20 psu", 
                               ggtheme = theme_classic(), 
                               legend = c(0.75, 0.2), 
                               palette = c('#1b9e77', '#d95f02'))


splots <- list()
splots[[1]] <- five_KMplot 
splots[[2]] <- fourteen_KMplot 
splots[[3]] <- eight_KMplot
splots[[4]] <- seventeen_KMplot 
splots[[5]] <- eleven_KMplot 
splots[[6]] <- twenty_KMplot

# Arrange multiple ggsurvplots and print the output
res <- arrange_ggsurvplots(splots, print = FALSE,
                    nrow = 2, ncol = 3, risk.table.height = 0.4)

ggsave("./figures/limpet_mortality_salinity.png", res)



# Separate plots for each salinity level  ---------------------------------
# Theraesa's code

#Subset Data
Treat5 <- subset(figdata, Salinity == 5)
Treat8 <- subset(figdata, Salinity == 8)
Treat11 <- subset(figdata, Salinity == 11)
Treat14 <- subset(figdata, Salinity == 14)

#Create individual plots
five <- ggplot(Treat5, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="A) 5 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = c(0.93, 0.6))

eight <- ggplot(Treat8, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="B) 8 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

eleven <- ggplot(Treat11, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="C) 11 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

fourteen <- ggplot(Treat14, aes(x=Day, y=Prop_Alive, shape = site, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 17))+
  scale_color_manual(values=c('black','grey50')) +
  geom_errorbar(aes(ymin = Prop_Alive-Std_Error, ymax = Prop_Alive+Std_Error), width = 0.2,
                position = position_dodge(0.05)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlim(1,28) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0), limits = c(0,1)) +
  labs(title="D) 14 psu") +
  theme(plot.title = element_text(hjust = 1)) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

#Create Multi-panel Figure
figure <- grid.arrange(five, eight, eleven, fourteen, nrow = 4)

annotate_figure(figure,
                left = text_grob("Mean Proportion Alive", rot=90,vjust=1),
                bottom = text_grob("Day",vjust=-1))

ggsave("./figures/limpet_mortality_local_adaptation.png", width = 12.31, height = 8.71)

