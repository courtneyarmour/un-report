# load tidyverse packages
library(tidyverse)

# read in data
gapminder_1997 <- read_csv("data/gapminder_1997.csv")

# make a plot
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") + 
  labs(title = "Do people in wealthy countries live longer?") + 
  geom_point() +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) + 
  labs(size = "Population (in millions)") 


#different color palettes
RColorBrewer::display.brewer.all()

#condense aes
ggplot(data = gapminder_1997, 
       aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000))  + 
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)") + 
  geom_point() +
  scale_color_brewer(palette = "Set1") 

# full dataset
gapminder_data <- read_csv("./gapminder_data.csv")
dim(gapminder_data)

ggplot(data = gapminder_data, aes(x = year, y = lifeExp, color = continent, group = country)) +
  geom_line()

# exercise
ggplot(data = gapminder_1997, aes(x=continent,y=lifeExp)) + 
  geom_boxplot()

violin_plot <- ggplot(data = gapminder_1997, aes(x=continent,y=lifeExp)) + 
  geom_jitter(aes(size=pop)) + 
  geom_violin(alpha = 0.5, aes(fill = gapminder_1997$continent))
violin_plot + theme_bw()
violin_plot 
violin_plot <- violin_plot + theme_bw()
ggsave("awesome_violin_plot.jpg", plot= violin_plot, width = 6, height = 4)

ggplot(gapminder_1997, aes(x=lifeExp)) + 
  geom_histogram(bins=20) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1))
ggsave("awesome_plot.jpg", width=6, height = 4)

# faceting
ggplot(gapminder_1997, aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point() + 
  #facet_wrap(vars(continent))
  facet_wrap(. ~ continent)

ggsave("my_awesome_plot.jpg")
