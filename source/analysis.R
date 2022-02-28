#Load packages
library(dplyr)
library(ggplot2)
library(maps)
library(usdata)
library(dplyr)
library(plotly)
library(tidyr)

#Load dataset
data <-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#1)calculate five variable

#variable 1: race group with highest jail population in  Washington in 2016 `highest_jail_pop`
#total jail population by race in Washington
total_jail_by_race <-data %>% 
  filter(year>=2006 & year<=2016, state=="WA") %>% 
  group_by(year) %>% 
  summarize(
    aapi_total=sum(aapi_jail_pop), 
    black_total=sum(black_jail_pop), 
    latinx_total=sum(latinx_jail_pop), 
    native_total=sum(native_jail_pop),
    white_total=sum(white_jail_pop),
    other_total=sum(other_race_jail_pop))
#find the highest race group in 2016
highest_jail_pop <-total_jail_by_race %>% 
  filter(year==2016) %>% 
  gather(key="race", value="pop_jail", 2:7) %>% 
  filter(pop_jail==max(pop_jail)) %>% 
  pull(race)
  

#variable 2: Average Jail population ratio for black in Washington 2006-2016 `jail_rate_black`
#total population by race in Washington
total_pop_by_race <-data %>% 
  filter(year>=2006 & year<=2016, state=="WA") %>% 
  group_by(year) %>% 
  summarize(
    aapi_total=sum(aapi_pop_15to64), 
    black_total=sum(black_pop_15to64), 
    latinx_total=sum(latinx_pop_15to64), 
    native_total=sum(native_pop_15to64),
    white_total=sum(white_pop_15to64),
    )

#calculate average Jail ratio for black in Washington 
jail_black <-total_jail_by_race %>% 
  select(year,black_total)%>% 
  rename(black_jail_total=black_total)

total_pop_black <-total_pop_by_race %>% 
  select(year,black_total)

jail_rate_black <-full_join(jail_black,total_pop_black) %>%     
  mutate(black_jail_rate = black_jail_total/black_total) %>% 
  summarize(average_rate_black=mean(black_jail_rate)) %>% 
  pull(average_rate_black)

#variable 3: Average jail population ratio for white in Washington 
jail_white <-total_jail_by_race %>% 
  select(year,white_total)%>% 
  rename(white_jail_total=white_total)

total_pop_white <-total_pop_by_race %>% 
  select(year,white_total)

jail_rate_white <-full_join(jail_white,total_pop_white) %>%     
  mutate(white_jail_rate = white_jail_total/white_total) %>% 
  summarize(average_rate_white=mean(white_jail_rate)) %>% 
  pull(average_rate_white)

#variable 4: Average incarceration rate for all races in 2006-2016 in Washington `jail_rate_all`
jail_rate_all <-data %>% 
  filter(year>=2006 & year<=2016, state=="WA") %>% 
  group_by(year) %>% 
  summarize(total_jail_pop=sum(total_jail_pop),total_pop_15to64=sum(total_pop_15to64)) %>% 
  mutate(inca_rate=total_jail_pop/total_pop_15to64) %>% 
  summarize(average_rate=mean(inca_rate)) %>% 
  pull(average_rate)

#variable 5: race with the highest jail ratio.`highest_race`
total_jail_new <-total_jail_by_race %>% 
  gather(key="race", value="pop", 2:7)

jail_race <- total_jail_new %>% 
  group_by(race) %>% 
  summarise(total_jail=sum(pop))

pop_race <-total_pop_by_race %>% 
  gather(key=race, value=pop, 2:6) %>% 
  group_by(race) %>% 
  summarise(total_pop=sum(pop))

highest_race <-full_join(pop_race,jail_race) %>% 
  mutate(ratio = total_jail/total_pop) %>% 
  filter(ratio==max(ratio, na.rm = TRUE)) %>% 
  pull(race)

#2) Trends over time chart
#change wide data format use gather
total_jail_new <-total_jail_by_race %>% 
  gather(key="race", value="pop", 2:7)
#a line graph showing the trend of different race group in jail

Line_graph <-total_jail_new %>% 
  ggplot(aes(x=year, y=pop, group=race)) + 
  geom_line(aes(colour=race), size=1)+
  geom_point()+
  ggtitle("Total Jail Population By Race in WA 2006-2016")+
  labs(x = "Year", y = "Total Jail population in Washington State", color="different ethnicities")

#make the line graph interactive
ggplotly(Line_graph)

#3)Variable comparison chart
#compare year and jail population rate for black
jail_rate_white_new <-full_join(jail_white,total_pop_white) %>%     
  mutate(white_jail_rate = white_jail_total/white_total) %>% 
  filter(year==2016) %>% 
  select(year, white_jail_rate)

jail_rate_all_new <-data %>% 
  filter(year>=2006 & year<=2016, state=="WA") %>% 
  group_by(year) %>% 
  summarize(total_jail_pop=sum(total_jail_pop),total_pop_15to64=sum(total_pop_15to64)) %>% 
  mutate(overall_rate=total_jail_pop/total_pop_15to64) %>% 
  filter(year==2016) %>% 
  select(year, overall_rate)

jail_rate_black_new <-full_join(jail_black,total_pop_black) %>%     
  mutate(black_jail_rate = black_jail_total/black_total) %>% 
  filter(year==2016) %>% 
  select(year, black_jail_rate)

graph_data <-full_join(jail_rate_all_new,jail_rate_white_new) %>% 
  full_join(jail_rate_black_new) %>% 
  select(black_jail_rate,overall_rate,white_jail_rate) %>% 
  gather(key = race, value=rate)
          

comparison_graph <- ggplot(graph_data,aes(x=race, y=rate,fill=race))+
  geom_bar(stat = "identity")+
  labs(x = "Race", y = "Incarceration Rate",title="Incarceration Rate By Race in WA in 2016")

#make the line graph interactive
ggplotly(comparison_graph)


#4)Map
# a map that show incarceration rate for black in 2016 in the US.
#3) Create US shape file and rename region column to state
us_data<-data %>% 
  filter(year==2016) %>% 
  select(state,total_pop) %>% 
  group_by(state) %>% 
  summarize(total_pop=sum(total_pop))

state_shape <- map_data("state") %>%
  rename(state = region)

#4) Prepare state_shape for joining by modifying state to be abbreviations.
state_shape <- state_shape %>%
  mutate(state = state2abbr(state))

#4): Join state_shape to crime_data_2008 dataframe
state_shape <- state_shape %>%
full_join(us_data,state_shape,by = "state")
                         
#5): Create a map
blank_theme <- theme_bw() + 
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

jail_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_pop), color="darkred", size=0.2
  ) +
  coord_map() +
    labs(fill="Total Population")+
  ggtitle('U.S. population distribution') + blank_theme


#plot out the map
jail_map

#Make the map interactive 
ggplotly(jail_map)

