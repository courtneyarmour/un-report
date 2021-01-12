library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% summarise(averagePopulation = mean(pop),
                             recent_year = max(year))

gapminder_data %>% filter(year == 2007) %>% 
  summarise(averageLifeExp = mean(lifeExp))

gapminder_data %>% summarise(first_year = min(year)) #1952

gapminder_data %>% 
  filter(year == 1952) %>% 
  summarise(averageGDP_percap = mean(gdpPercap),first_year=min(year))

gapminder_data %>% 
  group_by(year) %>% 
  summarise(averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  mutate(gdp = gdpPercap*pop)

gapminder_data %>% 
  mutate(popMil = pop / 1000000)

year_pop <- gapminder_data %>% 
  select(year, pop)

gapminder_data %>% 
  select(-continent)

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider()

gapminder_Americas_2007 <- gapminder_data %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year)


minyear <- gapminder_data %>% summarise(min(year)) %>% deframe()
gapminder_data %>% filter(year == minyear) %>% summarise(mean(gdpPercap))
gapminder_data %>% filter(year==1952) %>% summarise(mean(gdpPercap))


co2_emissions <- read_csv("data/co2-un-data.csv", skip=2,
                          col_names=c("region", "country", "year", 
                                      "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela")
  )

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarise(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

anti_join(gapminder_data,co2_emissions)

gapminder_co2 <- inner_join(gapminder_data,co2_emissions,by="country")

gap_co2_region <- gapminder_co2 %>% 
  mutate(region = if_else(country == "Canada" | 
                            country == "United States" |
                            country == "Mexico","north","south"))

ggplot(gap_co2_region,aes(x=gdpPercap,y=per_capita,color=region)) + 
  geom_point() +
  labs(x="GDP per capita",y="CO2 Emissions per Capita")
