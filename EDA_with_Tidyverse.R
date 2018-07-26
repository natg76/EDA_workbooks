
## R kernel for gapminder dataset 

library(gapminder)
library(dplyr)
library(ggplot2)
library(tidyverse)


# What type of dataset it is? Tibble? 
gapminder

dim(gapminder)
str(gapminder)
class(gapminder)
nrow(gapminder)
View(gapminder)


count()

# View Unique Countries in the dataset

gapminder %>% group_by(country) %>% summarise(no_of_years = n()) %>% arrange(desc(country))

# Exploratory Data Analysis 

gapminder %>% filter(year == 2007, country == "Bahrain")
gapminder %>% filter(country == "Oman" & year == 2007)

# Which countries have highest and lowest lifeexpectancy rates

# Sort in ascending order of lifeExp
gapminder %>% arrange(lifeExp)  %>% head(5)
# Sort in descending order of lifeExp
gapminder %>% arrange(desc(lifeExp)) %>% head(5)

# Any change post 2000
gapminder %>% filter(year >= 2000)  %>% arrange(lifeExp)  %>% head(5)
gapminder %>% filter(year >= 2000)  %>% arrange(desc(lifeExp)) %>% head(5)

# Filter for the year 1957, then arrange in descending order of population
gapminder %>% filter(year == 1957)  %>% arrange(desc(pop)) 

# Convert Population to Millions to millions


# Use mutate to change lifeExp to be in months
gapminder %>% mutate(lifeExp = 12 * lifeExp)

# Use mutate to create a new column called lifeExpMonths
gapminder %>% mutate(lifeExpMonths = 12 * lifeExp)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>% filter(year == 2007) %>% mutate(lifeExpMonths = 12 * lifeExp) %>% arrange(desc(lifeExpMonths))


## Visual Analysis of Variables

gapminder_1952 <- gapminder %>% filter(year == 1952)

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes(x=pop, y=lifeExp) ) + geom_point()
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) + geom_point() + scale_x_log10()
# Scatter plot comparing pop and gdpPercap, with both axes on a log scale
ggplot(gapminder_1952, aes(x=pop, y=gdpPercap))+ geom_point() + scale_x_log10()+ scale_y_log10()
ggplot(gapminder_1952, aes(x=gdpPercap, y=lifeExp))+ geom_point() + scale_x_log10() + scale_y_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x=pop, y=lifeExp, color=continent,  size=gdpPercap)) + scale_x_log10() + geom_point()
# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952,aes(x=pop, y=lifeExp))+scale_x_log10()+geom_point()+facet_wrap(~ continent)
ggplot(gapminder,aes(x=gdpPercap, y=lifeExp, color = continent, size = pop )) + scale_x_log10()+geom_point()+facet_wrap(~ year)



### Verb 3:  SUMMARIZE


# Summarize to find the median life expectancy
gapminder %>% summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

### Verb 4 : group_by

# Find median life expectancy and maximum GDP per capita in each year
gapminder %>% group_by(year) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>% filter(year==1957) %>% group_by(continent) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year/continent combination
gapminder %>% group_by(continent, year) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))



by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time

ggplot(by_year, aes(x=year, y=medianLifeExp)) + geom_point() + expand_limits(y=0)


## Visualizing summary data

##Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, color=continent)) + geom_point() + expand_limits(y=0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>% filter(year==2007) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap), medianLifeExp = median(lifeExp)
  )


######## 4.1 Line Plots

# Summarize the median gdpPercap by year, then save it as by_year

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time

ggplot(by_year, aes(x=year, y=medianGdpPercap)) + geom_line() + expand_limits(y=0)

ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, color=continent)) + geom_line() + expand_limits(y=0)


####  4.2 bar/col charts..


by_continent <- gapminder %>% filter(year==1952) %>% group_by(continent) %>% summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent

# Note that both the chart options below produces exactly same visual.
# understand differences between geom_bar and geom_col

ggplot(by_continent, aes(x=continent, y=medianGdpPercap)) + geom_bar(stat="identity") 
ggplot(by_continent, aes(x=continent, y=medianGdpPercap)) + geom_col(fill="orange") 


# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>% filter(year==1952, continent=="Oceania") 

# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x=country, y=gdpPercap))+ geom_col()


####  4.3 Histograms


gapminder_1952 <- gapminder %>% filter(year == 1952)

# Create a histogram of population (pop)
ggplot(gapminder_1952, aes(x=pop)) + geom_histogram()

# Data is too skewed towards left.. not able to see the distribution.. use logscale on x
ggplot(gapminder_1952, aes(x=pop)) + geom_histogram() + scale_x_log10()

####  4.4 Box Plots

# Create a boxplot comparing gdpPercap among continents

p <- ggplot(gapminder_1952, aes(x=continent, y=gdpPercap)) + geom_boxplot() + scale_y_log10()

p + ggtitle("Comparing GDP per capita across continents")


######## The END.. FOR NOW.. ##############