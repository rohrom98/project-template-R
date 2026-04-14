#Install and Load Packages
install.packages("usethis")
install.packages("gitcreds")
install.packages("here")
install.packages("tidyverse")
install.packages("medicaldata")
install.packages("cowplot")
library(usethis)
library(gitcreds)
library(here)
library(tidyverse)
library(medicaldata)
library(cowplot)

## Tidyverse and Data Wrangling
#Option1 BaseR = Output is data.frame
data1 <- read.csv(here("data", "raw", "insurance_with_date.csv")) 
str(data1) #Inspect Data
#Option2 ReadR = Output is Tibble
library(readr)
data2 <- read_csv(here("data", "raw", "insurance_with_date.csv"))
str(data2) #Inspect Data

##Exercise2 using tidyverse
library(lubridate)
reformatted_data2 <- data2 |> 
  mutate(across(c(sex, region), factor),
         more2children = children > 2,
         smoker_yes = (smoker == "yes"),
         date_add6m = date + months(6)
  )
str(reformatted_data2)

##Exercise2 using BaseR
reformatted_data1 <- data1

reformatted_data1$sex <- factor(reformatted_data1$sex)
reformatted_data1$region <- factor(reformatted_data1$region)

reformatted_data1$more2children <- reformatted_data1$children > 2
reformatted_data1$smoker_yes <- reformatted_data1$smoker == "yes"
reformatted_data1$date_add6m <- seq(reformatted_data1$date, by = "6 months", length.out = 2)[2]

#Pivoting Datasets: pivot_longer(data, cols, names_to/from ="", values_to/from="")
#Merging datasets: left_join(), right_join(), inner_join(), full_join()
#Summarize data: summarize(n = n(), ...), group_by()
#Labelling Variables: library(labelled)
#Create Tables: library(gtsummary)
install.packages("labelled")
install.packages("gtsummary")
library(labelled)
library (gtsummary)

#Data Visualization
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

covid <- read_csv("data/raw/COVID19Cases_geoRegion.csv")
covid

#geom_point: 
covid_cantons_2020 <- read_csv("data/raw/covid_cantons_2020_06.csv")
plot_covid_point_v0 <- ggplot(data = covid_cantons_2020, 
                              mapping = aes(x = datum, y = entries)) + 
  geom_point()

#geom_line:
plot_covid_line_v0 <- ggplot(data = covid_cantons_2020, 
                             mapping = aes(x = datum, y = entries)) + 
  geom_line(mapping = aes(group = geoRegion))

#geom_col:
plot_covid_col_v0 <- ggplot(data = covid_cantons_2020, 
                            mapping = aes(x = datum, y = entries)) + 
  geom_col(position = "stack")

##Exercise 4A: Basic Plot
install.packages("readr")
library(readr)
library(lubridate)
ebola_data <- read_csv("data/raw/ebola.csv")
ebola_data <- ebola_data %>% arrange(Date)
ebola_data

ebola_data_filtered_cases <- ebola_data %>% 
  select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) %>% 
  filter(date <= ymd("2015-03-31") & 
           (country == "Guinea" | country ==  "Liberia" | country == "Sierra Leone"))
ebola_data_filtered_cases

#Point plot
ebola_plot_point_v0 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases)) +
  geom_point()
ebola_plot_point_v0

#Line plot
ebola_plot_line_v0 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases)) +
  geom_line(aes(group = country))
ebola_plot_line_v0

#column plot
ebola_plot_col_v0 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases)) +
  geom_col(position = "stack")
ebola_plot_col_v0

# Save the plot as a PNG using ggsave
ggsave("output/figures/ebola_plot_point_v01.png", plot = ebola_plot_point_v0, width = 8, height = 6, units = "in", dpi = 300)

#Colour and fill
ebola_plot_point_v1 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases)) +
  geom_point(alpha = 0.7, colour = "black", fill = "blue",
             shape = 21, size = 1.5, stroke = 1.5
  )
ebola_plot_point_v1

ebola_plot_line_v1 <- ggplot(data = ebola_data_filtered_cases,
                             mapping = aes(x=date, y=cum_conf_cases)) +
  geom_line(aes(group = country),
            alpha = 0.7, colour = "blue", linetype = "solid", linewidth = 1.5)
ebola_plot_line_v1

ebola_plot_col_v1 <- ggplot(data = ebola_data_filtered_cases,
                            mapping = aes(x=date, y=cum_conf_cases)) +
  geom_col(position = "stack", alpha = 0.7, fill = "blue", 
           linetype = "solid", linewidth = 0.5, width = 0.7)
ebola_plot_col_v1

#Different Color Per Country
ebola_plot_point_v2 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5)
ebola_plot_point_v2

ebola_plot_line_v2 <- ggplot(data = ebola_data_filtered_cases,
                             mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_line(aes(group = country),
            alpha = 0.7, linetype = "solid", linewidth = 1.5)
ebola_plot_line_v2

ebola_plot_col_v2 <- ggplot(data = ebola_data_filtered_cases,
                            mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_col(position = "stack", alpha = 0.7, 
           linetype = "solid", linewidth = 0.5, width = 0.7)
ebola_plot_col_v2


ebola_plot_point_line_v0 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  geom_line()
ebola_plot_point_line_v0

ebola_plot_point_line_v1 <- ggplot(data = ebola_data_filtered_cases,
                                   mapping = aes(x=date, y=cum_conf_cases)) + 
  geom_point(alpha = 0.7, colour = "black", shape = 21, size = 1.5, stroke = 1.5) +
  geom_line(aes(group = country),
            alpha = 0.7, linetype = "solid", linewidth = 1.5, colour = "red")
ebola_plot_point_line_v1


#Labels
ebola_plot_point_v3 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
ebola_plot_point_v3

ebola_plot_point_v4 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.8, shape = 21, size = 2.5, stroke = 0.2) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c("lightblue", "blue", "darkblue"), 
                    labels = c("G", "L", "SL")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c("black", "black", "black"), 
                      labels = c("G", "L", "SL")) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
ebola_plot_point_v4


ebola_plot_point_v5 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.8, shape = 21, size = 2.5, stroke = 0.2) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c("lightblue", "blue", "darkblue"), 
                    labels = c("G", "L", "SL")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c("black", "black", "black"), 
                      labels = c("G", "L", "SL")) +
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = ymd(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous (breaks = seq(from = 0, to = 9000, by = 1000),
                      limits = c(0, 9000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
ebola_plot_point_v5


ebola_plot_point_v6 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.8, shape = 21, size = 2.5, stroke = 0.2) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c("lightblue", "blue", "darkblue"), 
                    labels = c("G", "L", "SL")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c("black", "black", "black"), 
                      labels = c("G", "L", "SL")) +
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = ymd(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous (breaks = seq(from = 0, to = 9000, by = 1000),
                      limits = c(0, 9000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom")
ebola_plot_point_v6

ebola_plot_point_v7 <- ggplot(data = ebola_data_filtered_cases,
                              mapping = aes(x=date, y=cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.8, shape = 21, size = 2.5, stroke = 0.2) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c("lightblue", "blue", "darkblue"), 
                    labels = c("G", "L", "SL")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c("black", "black", "black"), 
                      labels = c("G", "L", "SL")) +
  scale_x_date(breaks = ymd(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = ymd(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous (breaks = seq(from = 0, to = 9000, by = 1000),
                      limits = c(0, 9000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  theme(panel.spacing = unit(2, "lines")) +
  facet_grid(cols = vars(country))
ebola_plot_point_v7

#Test changes for Github