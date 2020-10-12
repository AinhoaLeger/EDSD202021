
#####################################################
# European Doctoral School of Demography (EDSD)
# Assigment for Computer Programming (E140)
#####################################################

# Team members
# Ozer Bakar  
# Liliana Patricia Calderon Bernal  
# Gonzalo Daniel Garcia  
# Ainhoa-Elena Leger  
# Ozge Elif Ozer

#-------------------------------
# Exercise 1 
#-------------------------------

# Download and unzip the SOEP data set  
soep_url <- 
  "https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.412698.de/soep_lebensz_en.zip"
destfile <- "soep_lebensz_en.zip"
download.file(soep_url, destfile)
unzip(zipfile = "soep_lebensz_en.zip")

# 1a) Load the data set into R
#-------------------------------

# Loading necessary libraries to solve the assignment
#install.packages("foreign")
#install.packages("tidyverse")

library(foreign)
library(tidyverse)

# Importing the Stata data into R framework by using foreign library
soep <- read.dta("soep_lebensz_en.dta", convert.factors = TRUE)

# Checking what's inside the data
glimpse(soep)

# 1b) How many unique individuals are included in the practice data set?
#-------------------------------------------------------------------------

soep$id %>% unique %>% length

length(unique(soep$id))

# 1c) Tabulate the number of observations per year
#---------------------------------------------------
  
obs_per_year <- soep %>% 
  group_by(year) %>% 
  tally()

prop.table(table(soep$year))

# 1d) Restrict the data to the most recent year
#-----------------------------------------------

last_soep <- soep %>% 
  filter(year==max(year))

dim(last_soep) # ok

# What is the proportion of females in this subset of the data? 
last_soep$sex %>% table %>% prop.table

# Is the average subjective health higher for men or for women?

# Levels of the variable health_org
levels(last_soep$health_org)

# Creation of the corresponding numerical variable
last_soep$health_num[last_soep$health_org == c("not valid", 
                                               "does not concern", 
                                               "no answer")] <- 0
last_soep$health_num[last_soep$health_org =="bad"]          <- 1
last_soep$health_num[last_soep$health_org =="poor"]         <- 2
last_soep$health_num[last_soep$health_org =="satisfactory"] <- 3
last_soep$health_num[last_soep$health_org =="good"]         <- 4
last_soep$health_num[last_soep$health_org =="very good"]    <- 5

# check whether the old variable and the new variable coincides
table(last_soep$health_org, last_soep$health_num)

# means for the females and the males
tapply(last_soep$health_num, last_soep$sex, mean)

# Alternatively
last_soep <- last_soep %>% 
              mutate(health_org_numeric = case_when(
                health_org %in% c("not valid", "does not concern", "no answer") ~ 0,
                health_org == "bad" ~ 1,
                health_org == "poor" ~ 2,
                health_org == "satisfactory" ~ 3,
                health_org == "good" ~ 4,
                health_org == "very good" ~ 5,
                TRUE ~ NA_real_
                ))

last_soep %>% 
  group_by(sex) %>% 
  filter(health_org != 0) %>% 
  summarise(mean_subjective_health = mean(health_org_numeric, na.rm = TRUE))


#-------------------------------
# Exercise 2
#-------------------------------

# 2a) Load the data
#--------------------

#install.packages("HMDHFDplus")
library(HMDHFDplus)

##### The user has to provide its own credentials (username and password) #####

italy_e0 <- readHMDweb("ITA","E0per","...","...")

head(italy_e0)
tail(italy_e0)

# 2b) Visualize the trend in life expectancy at birth
#------------------------------------------------------
  
# Customization of the theme 
theme_graphs <- function (base_size = 16, base_family = "sans") {
  theme(plot.title = element_text(size = 14, face = "bold", 
                                  hjust=0.5, margin = margin(20, 0, 5, 0)),
        plot.subtitle = element_text(colour = "#000000", size = 14,
                                     hjust=0.5, margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(colour = "#000000", size = 9, 
                                    hjust=1, margin = margin(10, 0, 20, 0)),
        plot.background = element_rect(fill = "#F4F5F0"), 
        panel.background = element_rect(fill = "white", 
                                        colour = "#000000", linetype = "solid"), 
        panel.grid.major.x = element_line(colour = "gray79", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "gray79", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10, colour = "#000000", hjust=0.5, 
                                    face = "bold", margin = margin(10, 0, 10, 0)), 
        axis.title.y = element_text(size = 10, colour = "#000000", face = "bold", 
                                    margin = margin(0, 10, 0, 0)), 
        axis.text = element_text(size = 10, colour = "#000000"),
        axis.line.y = element_line(colour = "#000000"),
        axis.line.x = element_line(colour = "#000000"),
        axis.ticks = element_line(colour = "#000000", size = 1),        
        legend.text = element_text(size = 12, colour = "#000000"),
        legend.background = element_rect(fill = "white", colour = "#000000", 
                                         size = 0.3, linetype = "solid"), 
        legend.key = element_rect(fill = NA), 
        legend.position = "bottom",
        legend.direction = "horizontal")
}

# Trends in life expectancy
italy_e0 %>% 
  select(Year, Female, Male) %>%
  pivot_longer(., cols = c(Male, Female), names_to = "Sex", values_to = "Life_Exp") %>%
  ggplot(aes(x = Year, y = Life_Exp, color = Sex)) +
  theme_graphs() +
  geom_line(size=1) +
  scale_colour_manual(limits=c("Female", "Male"), values=c("#008c45", "#cd212a")) +
  labs(title = "Evolution of life expectancy at birth", 
       subtitle = "Italy, 1872-2017",        
       x = "Years", y = "Life Expectancy at Birth", 
       caption = "Own elaboration. Source: Human Mortality Database. 
       University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany).", 
       colour = NULL) +
    scale_x_continuous(breaks=seq(from=1870,to=2020,by=10),limits=c(1870,2020))

ggsave(file="italy_e0.jpeg", width=16, height=8, dpi=300)

# 2c) Visualize the evolution of the gender gap in e0 over time 
#---------------------------------------------------------------

italy_e0 %>%
  ggplot(aes(x = Year)) +
  theme_graphs() +
  geom_line(aes(y = Male), color = "darkred") +
  geom_line(aes(y = Female), color="steelblue", linetype="twodash") +
  scale_color_manual(limits=c("Female", "Male"), values=c("#008c45", "#cd212a")) +
  labs(title = "Evolution of life expectancy at birth", 
       subtitle = "Italy, 1872-2017",        
       x = "Years", y = "Life Expectancy at Birth", 
       caption = "Own elaboration. Source: Human Mortality Database. 
       University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany).", 
       colour = NULL) +
  geom_ribbon(aes(ymin = Male, ymax = Female), fill = "blue", alpha = .5)

ggsave(file="italy_e0_gapA.jpeg", width=16, height=8, dpi=300)

# Measuring the gender gap
italy_e0$gender_gap <- italy_e0$Female - italy_e0$Male

# Evolution of the gender gap over time
italy_e0 %>% 
  ggplot(aes(x = Year, y = gender_gap)) +
  theme_graphs() +
  geom_line(size=1) +
  scale_colour_manual(limits=c("Female", "Male"), values=c("#008c45", "#cd212a")) +
  labs(title = "Evolution of gender gap in life expectancy at birth", 
       subtitle = "Italy, 1872-2017",        
       x = "Years", y = "Gender gap in Life Expectancy at Birth", 
       caption = "Own elaboration. Source: Human Mortality Database. 
       University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany).", 
       colour = NULL) +
  scale_x_continuous(breaks=seq(from=1870,to=2020,by=10),limits=c(1870,2020))

ggsave(file="italy_e0_gapB.jpeg", width=16, height=8, dpi=300)


