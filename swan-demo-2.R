#
#                    ,-----.
#                 ,-,-o-   `\
#           __.--"`-,)       \                  
#          `---------'-.._    \             
#                        `.   :
#                         |   |
#                         |   |
#                         |   |
#                         ,   ;
#                        /    |        pb                              
#                       /    /                              /'|
#                      /    |        __,---,___________    /  /
#                    /     |    _,-'      ,         ,'_,-'  /
#                    /      \_,-'        .'       ,-'       /
#                   (                , -'        -,        /
#                    \                            ,       /
#                     \            -  -   -  "           /
#    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ▄▄▄▄▄▄▄ ▄     ▄ ▄▄▄▄▄▄▄ ▄▄    ▄    ▄▄▄▄▄▄  ▄▄▄▄▄▄▄ ▄▄   ▄▄ ▄▄▄▄▄▄▄    ▄▄▄▄▄▄▄ 
# █       █ █ ▄ █ █       █  █  █ █  █      ██       █  █▄█  █       █  █       █
# █  ▄▄▄▄▄█ ██ ██ █   ▄   █   █▄█ █  █  ▄    █    ▄▄▄█       █   ▄   █  █▄▄▄▄   █
# █ █▄▄▄▄▄█       █  █▄█  █       █  █ █ █   █   █▄▄▄█       █  █ █  █   ▄▄▄▄█  █
# █▄▄▄▄▄  █       █       █  ▄    █  █ █▄█   █    ▄▄▄█       █  █▄█  █  █ ▄▄▄▄▄▄█
#  ▄▄▄▄▄█ █   ▄   █   ▄   █ █ █   █  █       █   █▄▄▄█ ██▄██ █       █  █ █▄▄▄▄▄ 
# █▄▄▄▄▄▄▄█▄▄█ █▄▄█▄▄█ █▄▄█▄█  █▄▄█  █▄▄▄▄▄▄██▄▄▄▄▄▄▄█▄█   █▄█▄▄▄▄▄▄▄█  █▄▄▄▄▄▄▄█

# Water quality data analysis

# Data analysis library
library(tidyverse)

# Read data from disk
labdata <- read_csv("data/labdata.csv") 

# Exploring the data
labdata

glimpse(labdata)

labdata$Result

# Counting Data
count(labdata, Measure)

count(labdata, Measure, Suburb)

# Filtering data
turbidity <- filter(labdata, Measure == "Turbidity")

# Question: Create a table of the number of E. coli result in each suburb


# Descriptive Statistics

mean(turbidity$Result)
median(turbidity$Result)

quantile(turbidity$Result, p = 0.95, type = 6) # Check help file

# Skewness
x <- turbidity$Result
n <- length(x)

(sum((x - mean(x))^3) / n) / (sqrt(sum((x - mean(x))^2) / n)^3)

moments::skewness(x)


# Grouped analysis

group_by(labdata, Measure) %>% 
    summarise(min = min(Result),
              mean = mean(Result),
              sd = sd(Result),
              skewness = moments::skewness(Result))

# Visualise 

limits <- data.frame(Measure = c("Chlorine Total", "Chlorine Total",
                                 "THM", "Turbidity"),
                     Limit = c(1, 2, 0.25, 5))

ggplot(filter(labdata, Measure != "E. coli" &
                  Suburb %in% c("Merton", "Tarnstead", "Blancathey")),
       aes(Date, Result)) +
    geom_line(size = .1) +
    geom_hline(data = limits, aes(yintercept = Limit),
               col = "red", linetype = 2) + 
    facet_grid(Measure ~ Suburb, scales = "free_y") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
    theme_minimal() + 
    labs(title = "Gormsey Laboratory Data",
         subtitle = "Operational and regulatory compliance",
         caption = "Source: Gormsey Laboratory")





