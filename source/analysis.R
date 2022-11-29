## Package/Data loading
library(tidyverse)
library(usmap)
library(dplyr)
library(ggpubr)
incarcerations <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#AAPI Difference 2002/2000
aapi_2002 <- incarcerations %>%
  filter(year == "2002") %>%
  pull(aapi_jail_pop)
mean_aapi_2002 <- mean(aapi_2002, na.rm = TRUE)
  
aapi_2000<- incarcerations %>%
  filter(year == "2000") %>%
  pull(aapi_jail_pop)
mean_aapi_2000 <- mean(aapi_2000, na.rm = TRUE)

diff_aapi <- mean_aapi_2002 - mean_aapi_2000

#Most Black Incarcerated People in 2018
incarcerations <- mutate(incarcerations, location = paste(county_name, state, sep = ", "), remove = FALSE)

highest_black_2012 <- incarcerations %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  filter(year == max(year)) %>%
  pull(location)

#Proportion of White to Black people Incarcerated
avg_black_jail <- mean(incarcerations$black_jail_pop, na.rm = TRUE)
avg_total_black <- mean(incarcerations$black_pop_15to64, na.rm = TRUE)
ratio_black <- avg_black_jail/avg_total_black

avg_white_jail <- mean(incarcerations$white_jail_pop, na.rm = TRUE)
avg_total_white <- mean(incarcerations$white_pop_15to64, na.rm = TRUE)
ratio_white <- avg_white_jail/avg_total_white

diff_ratio_black_white <- ratio_black - ratio_white

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
get_year_jail_pop <- function() {
  plotdata <- incarcerations %>%
    select(year, total_jail_pop) %>%
    drop_na(total_jail_pop)
  return(plotdata)
}


plot_jail_pop_for_us <- function()  {
  barchart <-ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(labels = scales:: comma) %>%
    labs(title = "United States Jail Population Increase" , 
         x = "Year", y = "Total Jail Population", 
         caption = "Growth Between 1970-2018") 
  return(barchart)
}

plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
states <- c("NY", "CA", "WA", "TX", "WY")

get_jail_pop_by_states <- function(states) {
  plotdata2 <- incarcerations %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(state, year, total_jail_pop)
  return(plotdata2)
}


plot_jail_pop_by_states <- function(states)  {
  linechart2 <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(title = "United States Jail Population Increase by States" , x = "Year",
         y = "Total Jail Population", caption = "Growth Between 1970-2018")
  return(linechart2)
}


plot_jail_pop_by_states(c("NY", "CA", "WA", "TX", "WY"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Comparing Black, and Latinx Prison Populations Over Time By State Since 1990
jail_pop_place_race <- function() {
  plot_data <- incarcerations %>%
    group_by(state, year) %>%
    filter(year > 1990) %>%
    summarize(black_male_prison_pop = sum(black_male_prison_pop, na.rm = TRUE),
              latinx_male_prison_pop = sum(latinx_male_prison_pop, na.rm = TRUE),
              male_prison_pop = sum(male_prison_pop, na.rm = TRUE)) %>%
    mutate(black_male_prison_pop = black_male_prison_pop / male_prison_pop) %>%
    mutate(latinx_male_prison_pop = latinx_male_prison_pop / male_prison_pop) %>%
    select(state, year, black_male_prison_pop, latinx_male_prison_pop)
  return(plot_data)
}


plot_jail_pop_place_race <- function() {
  barchart_pop_race <- ggplot(data = jail_pop_place_race()) +
    geom_col(mapping = aes(x = state, y = black_male_prison_pop, fill = "Black")) +
    geom_col(mapping = aes(x = state, y = latinx_male_prison_pop, fill = "Hispanic")) +
    scale_alpha_continuous( 0, 20) +
    labs(title = "Growth of Male Jail Populations by Race (1990-2018)", x = "Year",
         y = "Jail Population", fill = "Race",
         caption = "Black, and Latinx Male Jail Populations Over Time Since 1990") 
  return(barchart_pop_race)
}

print(plot_jail_pop_place_race())

#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# Map Showing Distribution of Percentage of Black Americans Incarcerated by State
map_wrangle <- function() {
  plotdatamap <- incarcerations %>%
    group_by(state) %>%
    filter(year == max(year)) %>%
    summarize(average_black_pop = sum(black_pop_15to64, na.rm = TRUE),
              average_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
              black_percentage = (average_black_jail_pop / average_black_pop) * 100) %>%
    select(state, black_percentage)
  return(plotdatamap)
}


map_visual <- function() {
  map_visual <- plot_usmap(data = map_wrangle(), values = "black_percentage",
                         color ="black") +
    theme_set(theme_minimal()) +
    scale_fill_continuous(name = "Percent", limits = c(0, 2.5), low = "white", high = "red") +
    labs(title = "Percentage of Black Americans Incarcerated by State",
         caption = "Data is Caclulated From 2018")
  return(map_visual)
}

print(map_visual())
