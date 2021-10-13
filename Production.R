library(tidytuesdayR)
library(ggplot2)
library(forcats)

tuesdata <- tidytuesdayR::tt_load('2021-10-12')
seafood_consumption <- tuesdata$`seafood-and-fish-production-thousand-tonnes`
head(tuesdata)
head(seafood_consumption)

names(seafood_consumption) <- c('Entity', 'Code', 'Year', 'Pelagic', 
                                'Crustaceans', 'Cephalopods', 'Demersal', 
                                'Freshwater', 'Molluscs', 'Marine')

length(unique(seafood_consumption$Entity)) # 215 unique entities
entity_list <- unique(seafood_consumption$Entity)
# Create new column: Entity Type
# Set 'Country' as default
seafood_consumption$Entity_Type <- 'Country'
# Get list of economic group entities
econ <- entity_list[c(67, 103, 106, 111, 133, 172, 211)]
# Get list of regional entities
region <- entity_list[c(5, 11, 13, 36, 38, 55, 56, 57, 66, 123, 124, 143, 145, 
             155, 174, 176, 178, 
             179, 180, 209, 210, 211)]
continent <- c('Africa', 'Northern America', 'South America', 'Asia',
                           'Europe', 'Oceania')
cont_levels <- c('Asia', 'Europe', 'South America', 'Africa', 'Northern America', 'Oceania')

# Change entity type as appropriate
seafood_consumption$Entity_Type[seafood_consumption$Entity %in% region] <- 'Region'



seafood_consumption$Total <- rowSums(seafood_consumption[,c(4:10)], na.rm = TRUE)

seafood_consumption$Entity_Type[seafood_consumption$Entity %in% continent] <- 'Continent'
# Create line plot
#Create Subsets
continent_consumption <- seafood_consumption[seafood_consumption$Entity_Type == 'Continent',]
continent_consumption$Entity <- factor(continent_consumption$Entity, levels = cont_levels)
world_consumption <- seafood_consumption[seafood_consumption$Entity == 'World',]

ggplot(data = continent_consumption) +
  geom_smooth(aes(x = Year, y = Total/1000000, color = Entity, linetype = Entity), se = FALSE) +
  ylab("Total Fish/Seafood Consumption \n(Millions of Metric Tonnes)") +
  ggtitle("Almost all of the increase in seafood consumption is driven by Asia.") +
  theme(legend.title = element_blank())




################
# Create area plot
totals <- seafood_consumption$Total[(seafood_consumption$Entity_Type == 'Continent') & (seafood_consumption$Year == 2000)]
ggplot() +
  geom_area(data = continent_consumption, aes(x = Year, y = Total/1000000,
                    fill = Entity, 
                    linetype = Entity)) +
  geom_path(data = world_consumption, 
              aes(x = Year, y = Total/1000000), 
              color = 'black',
             size = 1) +
  ylab("Total Fish/Seafood Production \n(Millions of Metric Tonnes)") +
  xlab("") +
  theme_classic() +
  labs(title = "Since 1985, almost all of the increase in seafood production has been in Asia.") +
  theme(plot.title = element_text(size = 20, hjust = 0.2)) +
  theme(legend.position = 'none') +
  annotate(geom = "text", x = 2005, y = 75, label = 'Asia', size = 9) +
  annotate(geom = "text", x = 2006, y = 40, label = 'Europe', size = 9) +
  annotate(geom = "text", x = 1992, y = 20, label = 'South America', size = 9) +
  annotate(geom = "text", x = 2005, y = 12, label = 'Africa', size = 9) +
  annotate(geom = "text", x = 1993, y = 5, label = 'Northern America', size = 9) +
  annotate(geom = "text", x = 2006, y = 1.5, label = 'Oceania', size = 9) +
  annotate(geom = "text", x = 1980, y = 110, label = 'World Total', size = 9) +
  geom_segment(aes(x = 1985, y = 106, xend = 1988, yend = 98.5), size = 1) +
  geom_segment(aes(x = 1985, xend = 1985, y = 0, yend = 85), linetype = 2) +
  geom_segment(aes(x = 1985, xend = 2013, y = 46, yend = 46), linetype = 2) +
  annotate(geom= "text", x = 1985, y = -3, label = '1985')


########################




seafood_consumption$Entity_Type[seafood_consumption$Entity %in% econ] <- 'Economic Group'
# Create plot
ggplot(data = seafood_consumption[seafood_consumption$Entity_Type == 'Economic Group',]) +
  geom_area(aes(x = Year, y = Total/1000000, fill = Entity, linetype = Entity), se = FALSE) +
  geom_smooth(data = )
  ylab("Total Fish/Seafood Consumption \n(Millions of Metric Tonnes)") +
  ggtitle("NEED TITLE") +
  theme(legend.title = element_blank())

