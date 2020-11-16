##%######################################################%##
#                                                          #
####        Exploratory Data Analysis for field         ####
####              and lab resistance # in               ####
####           poplar (Populus nigra) # Firza           ####
####  Riany (firzariany2@gmail.com) # 16 November 2020  ####
#                                                          #
##%######################################################%##

# What's in the script
## Raw dataset
## Identification of missing data (data cleaning)
## EDA and data dashboard
## Correlation plot

# Libraries ----
library(readxl)  # to import the data in xlsx file
library(finalfit)  # for function ffglimpse(), extension of summary
library(ggplot2)  # for plotting
library(plyr)  # for counting the frequency from histogram
library(ggpubr)  # to get the function ggarrange; to combine ggplots
library(cowplot)  # to draw density histogram at the sides of correlation plot

pop_raw = read_xlsx("Tested_Poplar.xlsx",
                    na = "NA")  # defining the code for missing data used in the file

# Defining the objectives ----
## Are there a completely resistant poplar in the field and in the lab?
## What is the average resistance for poplar in the field and in the lab?
## Is there a correlation between field resistance and lab resistance?
## Is there a correlation between the parameters of lab resistance?

# Getting only the important variables
wild_yr2000 = pop_raw[,c("Classe",  # the type of population source 
                         "Population",  # population of origin
                         "Clone",  # the observed poplar individuals
                         "Notemax.2010 Noveltree",  # field resistance 
                         "latence.93JE3.2010",   # latent period
                         "nbsores.93JE3.2010",  # number of spores
                         "taille.93JE3.2010")]  # size of spores

colnames(wild_yr2000)[4] = "Notemax.2010"  # because the name was too long

# Check the class of each variable
str(wild_yr2000)

# Coercing
## Classe = chr to factor
## Population = chr to factor
## Clone = chr to factor
cols = c("Classe", "Population", "Clone")  # cols to be coerced

wild_yr2000[cols] = lapply(wild_yr2000[cols], factor)  # coerced to factor

# New data structure
str(wild_yr2000)

# Raw distribution
par(mfrow = c(2,2))  # combine the graphs (2 columns, 2 rows)

# Create a dataframe for loop
raw_loop = data.frame("note" = wild_yr2000$Notemax.2010,
                      "latence" = wild_yr2000$latence.93JE3.2010,
                      "nbsores" = wild_yr2000$nbsores.93JE3.2010,
                      "taille" = wild_yr2000$taille.93JE3.2010)

for (i in 1:4) {  # 1:4 for the number of variables used
  x = raw_loop[,i]  # assign x for each iteration
  title = names(raw_loop[i])  # title for each iteration
  hist(x,
       main = title)  # create histogram in each iteration
}

# Data cleaning: identifying NAs ----
# Examine NAs with ff_glimpse (package = finalfit)
explanatory = c("latence.93JE3.2010",
                "nbsores.93JE3.2010",
                "taille.93JE3.2010")

dependent = "Notemax.2010"

wild_yr2000 %>%
  finalfit::ff_glimpse(dependent, explanatory)

# Plotting the missing data
wild_yr2000 %>% 
  missing_plot()

# NAs in notemax, latence, and taille
# When when latence is missing, taille is most likely to be missing as well
# Check if the missing data in taille is due to missing data in latence?

# Checking the relatedness of the missing data in these two variables 
# using missing_pairs
# testing H0 = missing points are MCAR (missing compeletely at random)
explanatory_1 = "latence.93JE3.2010"

dependent_1 = "taille.93JE3.2010"

wild_yr2000 %>% 
  missing_compare(dependent_1, explanatory_1) %>%
  knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r"))

# P significant, missing data in taille is due to missing data in latence

# What about latence and notemax?
explanatory_2 = "Notemax.2010"

dependent_2 = "latence.93JE3.2010"

wild_yr2000 %>% 
  missing_compare(dependent_2, explanatory_2) %>%
  knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r"))

# P insignificant, missing data in latence is not due to missing data in notemax

# Excluding the controls (temoin) and national collection
# We want to look at the pure nigrapop and parent only
rownames_remove <- c("témoin", "coll. Nationale")

wild_yr2000_clean = subset(wild_yr2000,!(Classe %in% rownames_remove))

# Obj 1 Data dashboard for resistant vs susceptible (in the field) ----
# Resistant poplars in the field: Assigning -5 to NA to avoid confusion in R
wild_yr2000_clean[is.na(wild_yr2000_clean)] = -5

# Add a new variable: resistant vs susceptible
# Loop, -5 = resistant, != -5 susceptible
wild_yr2000_clean[,8] = "NA"

names(wild_yr2000_clean)[8] = "Res_vs_sus_note"

for (i in c(1:nrow(wild_yr2000_clean))) {
  if(wild_yr2000_clean$Notemax.2010[i] == -5)
    wild_yr2000_clean[i,8] = "Resistant"
  
  if(wild_yr2000_clean$Notemax.2010[i] != -5)
    wild_yr2000_clean[i,8] = "Susceptible"
}

# Plotting resistant vs susceptible poplars in terms of field resistance
(resist_field = ggplot(wild_yr2000_clean, aes(x = Res_vs_sus_note)) +
  geom_bar(aes(fill = Res_vs_sus_note)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Resistant vs susceptible poplars",
       x = "Resistant vs Susceptibe",
       y = "Counts") +
  scale_fill_manual(values = c("#87CEEB", "#FF6A6A"),
                    breaks = c("Resistant", "Susceptible"),
                    name = "Resistant vs Susceptible",
                    labels = c("Resistant", "Susceptible")) +
  theme_classic() +
  theme(axis.title.x = element_blank()))

# Checking the population of origin of the resistant poplars
pop = plyr::count(wild_yr2000_clean$Population[
  which(wild_yr2000_clean$Notemax.2010 == -5)
])

names(pop)[1] = "Population"

clone = plyr::count(wild_yr2000_clean$Clone[
  which(wild_yr2000_clean$Notemax.2010 == -5)
])

names(clone)[1] = "Clone"

pop$Clone = paste(clone$Clone)

pop  # displaying the resistant poplar and its population of origin

# Add a new variable: resistant vs susceptible (lab)
# Loop, -5 = resistant, != -5 susceptible
wild_yr2000_clean[,9] = "NA"

names(wild_yr2000_clean)[9] = "Res_vs_sus_lab"

for (i in c(1:nrow(wild_yr2000_clean))) {
  if(wild_yr2000_clean$taille.93JE3.2010[i] == -5
     & wild_yr2000_clean$latence.93JE3.2010[i] == 14)
    wild_yr2000_clean[i,9] = "Resistant"
  
  if(wild_yr2000_clean$taille.93JE3.2010[i] != -5
     & wild_yr2000_clean$latence.93JE3.2010[i] != 14)
    wild_yr2000_clean[i,9] = "Susceptible"
}

# Plotting resistant vs susceptible poplars in terms of lab resistance
(resist_lab = ggplot(wild_yr2000_clean, aes(x = Res_vs_sus_lab)) +
  geom_bar(aes(fill = Res_vs_sus_lab)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Resistant vs susceptible poplars",
       x = "Resistant vs Susceptibe",
       y = "Counts") +
  scale_fill_manual(values = c("#458B74", "#EE3B3B"),
                    breaks = c("Resistant", "Susceptible"),
                    name = "Resistant vs Susceptible",
                    labels = c("Resistant", "Susceptible")) +
  theme_classic() +
  theme(axis.title.x = element_blank()))

# Checking the population of origin of the resistant poplars (lab)
pop2 = plyr::count(wild_yr2000_clean$Population[
  which(wild_yr2000_clean$Res_vs_sus_lab == "Resistant")
])

names(pop2)[1] = "Population"

clone2 = plyr::count(wild_yr2000_clean$Clone[
  which(wild_yr2000_clean$Res_vs_sus_lab == "Resistant")
])

names(clone2)[1] = "Clone"

pop2$Clone = paste(clone2$Clone)

pop2

## Are there a completely resistant poplar in the field and in the lab? Yes
## Different poplars are resistant in the field and in the lab

# Obj 2 Average resistance. Data dashboard for summary statistics ----
# Field resistance (notemax)
# Removing -5 because they're outliers
wild_yr2000_new = wild_yr2000_clean[which(wild_yr2000_clean$Notemax.2010 != -5 &
                                            wild_yr2000_clean$taille.93JE3.2010 != -5), ]

summary(wild_yr2000_new)

NM = ggplot(wild_yr2000_new, aes(x = Notemax.2010)) +
  geom_histogram(binwidth = 0.5) +
  geom_density(aes(y = 0.3 * ..count..)) +
  geom_vline(aes(xintercept = mean(Notemax.2010), color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median(Notemax.2010), color = "Median"), size = 1) +
  geom_vline(aes(xintercept = quantile(Notemax.2010, prob = 0.25), color = "Q1"), 
             size = 1) +
  geom_vline(aes(xintercept = quantile(Notemax.2010, prob = 0.75), color = "Q3"), 
             size = 1) +
  labs(title = "Field resistance",
       caption = "number of samples = 72, sd = 0.63",
       x = "Score",
       y = "Counts") +
  scale_color_manual(name = "Statistics", values = c(Mean = "red", Median = "orange", Q1 = "blue",
                                                     Q3 = "blue")) +
  xlim(1, 6) +
  ylim(0, 40)

# Latent period (latence)
LP = ggplot(wild_yr2000_new, aes(x = latence.93JE3.2010)) +
  geom_histogram(binwidth = 0.5) +
  geom_density(aes(y = 0.3 * ..count..)) +
  geom_vline(aes(xintercept = mean(latence.93JE3.2010), color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median(latence.93JE3.2010), color = "Median"), size = 1) +
  geom_vline(aes(xintercept = quantile(latence.93JE3.2010, prob = 0.25), color = "Q1"), 
             size = 1) +
  geom_vline(aes(xintercept = quantile(latence.93JE3.2010, prob = 0.75), color = "Q3"), 
             size = 1) +
  labs(title = "Latent period",
       caption = "sd = 0.69",
       x = "Days",
       y = "Counts") +
  scale_color_manual(name = "Statistics", values = c(Mean = "red", Median = "orange", Q1 = "blue",
                                                     Q3 = "blue")) +
  xlim(7, 12) +
  ylim(0, 40)

# sd(wild_yr2000_new$latence.93JE3.2010)

# Number of spores
NS = ggplot(wild_yr2000_new, aes(x = nbsores.93JE3.2010)) +
  geom_histogram(binwidth = 1.0) +
  geom_density(aes(y = 0.3 * ..count..)) +
  geom_vline(aes(xintercept = mean(nbsores.93JE3.2010), color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median(nbsores.93JE3.2010), color = "Median"), size = 1) +
  geom_vline(aes(xintercept = quantile(nbsores.93JE3.2010, prob = 0.25), color = "Q1"), 
             size = 1) +
  geom_vline(aes(xintercept = quantile(nbsores.93JE3.2010, prob = 0.75), color = "Q3"), 
             size = 1) +
  labs(title = "Number of spores",
       caption = "sd = 9.49",
       x = "Number (counts)",
       y = "Counts") +
  scale_color_manual(name = "Statistics", values = c(Mean = "red", Median = "orange", Q1 = "blue",
                                                     Q3 = "blue")) +
  xlim(0, 60) +
  ylim(0, 40)

# sd(wild_yr2000_new$nbsores.93JE3.2010)

# Spore size
SS = ggplot(wild_yr2000_new, aes(x = taille.93JE3.2010)) +
  geom_histogram(binwidth = 0.5) +
  geom_density(aes(y = 0.3 * ..count..)) +
  geom_vline(aes(xintercept = mean(taille.93JE3.2010), color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median(taille.93JE3.2010), color = "Median"), size = 1) +
  geom_vline(aes(xintercept = quantile(taille.93JE3.2010, prob = 0.25), color = "Q1"), 
             size = 1) +
  geom_vline(aes(xintercept = quantile(taille.93JE3.2010, prob = 0.75), color = "Q3"), 
             size = 1) +
  labs(title = "Spore size",
       caption = "sd = 0.68",
       x = "Score",
       y = "Counts") +
  scale_color_manual(name = "Statistics", values = c(Mean = "red", Median = "orange", Q1 = "blue",
                                                     Q3 = "blue")) +
  xlim(1, 5) +
  ylim(0, 40)

# sd(wild_yr2000_new$taille.93JE3.2010)

# Combining the plots
dist = ggpubr::ggarrange(NM, LP, NS, SS,
                         widths = c(4, 4))
dist

# Obj 3 Correlation between field resistance and lab resistance ----
LP_NM = ggpubr::ggscatter(wild_yr2000_new, 
                          x = "latence.93JE3.2010", y = "Notemax.2010",
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Latent Period", ylab = "Field Resistance")

NS_NM = ggpubr::ggscatter(wild_yr2000_new, 
                          x = "nbsores.93JE3.2010", y = "Notemax.2010",
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Number of spores", ylab = "Field Resistance")

SS_NM = ggpubr::ggscatter(wild_yr2000_new, 
                          x = "taille.93JE3.2010", y = "Notemax.2010",
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Spore Size", ylab = "Field Resistance")

corr = ggpubr::ggarrange(LP_NM, NS_NM, SS_NM)
corr

## Is there a correlation between field resistance and lab resistance? No

# Obj 4 Correlation between parameters of lab resistance ----
# Designing the main correlation plot latence x taille
(main_cor = ggpubr::ggscatter(wild_yr2000_new,
                              x = "latence.93JE3.2010", y = "taille.93JE3.2010",
                              add = "reg.line", conf.int = TRUE,
                              color = "Classe", palette = "jco",
                              shape = "Classe", size = 2,
                              alpha = 0.6, ggtheme = theme_bw(),
                              xlab = "Latent period (days)", 
                              ylab = "Spore size (score)") +
  stat_cor(aes(color = Classe)))  # correlation line

# Because I differentiated correlation for parent and nigrapop, I want to provide
# the density histogram for poplars from parent and nigrapop

# Density histogram for latent period, plotted at x axis
xdens = axis_canvas(main_cor, axis = "x") +  # create the canvas base
  geom_density(data = wild_yr2000_new, 
               aes(x = latence.93JE3.2010, fill = Classe),
               alpha = 0.7, size = 0.2) +  # density histogram for latence
  ggpubr::fill_palette("jco")  # color palette for fill 

# Density histogram for taille, plotted at y axis
ydens = axis_canvas(main_cor, axis = "y", coord_flip = TRUE) +
  geom_density(data = wild_yr2000_new, 
               aes(x = taille.93JE3.2010, fill = Classe),
               alpha = 0.7, size = 0.2) +  # density histogram for taille
  coord_flip() +  # flipping the coordinate, 90 degrees
  ggpubr::fill_palette("jco")


# Adding xdens on the x axis of correlation plot
main_cor1 = cowplot::insert_xaxis_grob(main_cor, xdens,
                                       grid::unit(0.2, "null"), 
                                       position = "top")

# Adding ydens on the y axis of correlation plot
main_cor2 = cowplot::insert_yaxis_grob(main_cor1, ydens,
                                       grid::unit(0.2, "null"), 
                                       position = "right")

cowplot::ggdraw(main_cor2)

## Is there a correlation between the parameters of lab resistance? Yes,
## between latence and taille

# Saving the plot
ggsave("correlation_plot.png",
       device = "png",
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)