library(tidyverse)
library(rgdal)
library(broom)
#set working directory
setwd("C:/Users/Quin/Desktop/UrbanCirculators")
# data from:
# https://www.zillow.com/research/data/
# https://hub.arcgis.com/datasets/d6f7ee6129e241cc9b6f75978e47128b/explore?location=35.322414%2C-95.529164%2C4.40
# https://www.ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?zip=631
#Shape file with neighborhood coordinate information.
#Note- This step is extremely data-intensive, so I saved it as a .csv at the end.
shape1 <- readOGR("USA_ZIP_Code_Boundaries.shp")
mo_shape <- subset(shape1, STATE == "MO")
mo_df_tidy <- tidy(mo_shape)
mo_map_data <- mo_shape@data %>%
  mutate(id=OBJECTID-1) %>%
  mutate(id = as.character(id))
mo_combined <- left_join(mo_df_tidy, mo_map_data, by= c("id")) 
write.csv(mo_combined, "mo_combined.csv")


#Get data on areas around loop trolley.
library(tidyverse)
library(ggthemes)
library(stringr)
library(lubridate)
library(plotly)
zip_data <- read.csv("zip_data.csv") %>%
  select(-c(SizeRank, RegionType, StateName)) %>%
  filter(State == "MO") %>%
  mutate(RegionName = factor(RegionName))

zip_longer <- zip_data %>%
  pivot_longer(cols = 7:285, names_to = "Date" ,values_to = "HomeValue") %>%
  filter(RegionName == "63130" | RegionName == "63112" 
         | RegionName == "63114" | RegionName == "63133" 
         | RegionName == "63108") %>%
  mutate(Date = str_remove_all(Date, "X"),
         Date = as.Date(Date,"%Y.%m.%d"))
#Bring back geographic data, join them together.

stl_zips <- read.csv("stl_zips.csv")

mo_csv <- read.csv("mo_combined.csv") %>%
  filter(ZIP_CODE %in% stl_zips$ï..Zip.Code) %>%
  mutate(ZIP_CODE = factor(ZIP_CODE))

stl_combined_data <- mo_csv %>%
  left_join(zip_data, by = c("ZIP_CODE"="RegionName"))
 
# Isolated zip codes containing trolley for purposes of highlighting later on map.
trolley_zips <- stl_combined_data %>%
  filter(ZIP_CODE == c("63130", "63112"))
# save(stl_combined_data, zip_longer, trolley_zips, file = "urbancircluators.Rdata")
#############################################################################
#Making basic Chloropleth Map

p1 <- ggplot() +
  geom_polygon(aes(x=long, y= lat, group=group, #fill=X2023.03.31,
                   zip = ZIP_CODE, name = PO_NAME, fill=X2010.07.31), data = stl_combined_data,
               color = "lightblue") + 
  geom_polygon(aes(x=long, y= lat, group=group, fill=X2010.07.31,
                   zip = ZIP_CODE, name = PO_NAME), data = trolley_zips,
               color = "red") +
  coord_quickmap(xlim = c(-90.45, -90.20), ylim = c(38.52, 38.78)) +
  #Actual Coordinates is (-90.3325, 38.6682)
  #Loop Trolley(38.656430, -90.309739) to (38.653941, -90.284413) to (38.644572, -90.285615)
  geom_segment(aes(x=-90.309739, y = 38.656430, xend=-90.284413, yend=38.653941),
               data = stl_combined_data, color = "orange", size = 1.5, lineend = "round") +
  geom_segment(aes(x=-90.284413, y = 38.653941, xend=-90.285615, yend=38.644572),
               data = stl_combined_data, color = "orange", size = 1.5, lineend = "round") +
  scale_fill_continuous(low="grey88", high="darkblue",
                        name = "Home Value(USD)",
                        breaks=c(250000, 500000, 750000, 1000000),
                        labels = c("250k", "500k", "750k", "1mil")) +
  theme_map() +
  theme(legend.position = "right",
        title = element_text(hjust = 0.5)) +
  labs(title = "The Delmar Loop Trolley",
       subtitle = "St. Louis, MO, Zip Codes",
       caption = "The areas marked in red are the ZIP codes which contain the Loop Trolley.
       The orange line represents the trolley's path.")

ggplotly(p1)


#Making Line Graph
p2 <- ggplot(zip_longer) +
  geom_line(aes(x=Date, y=HomeValue, group = RegionName, color = RegionName)) +
  theme_classic() +
  scale_y_continuous(name = "Home Value(USD)",
                     breaks = c(100000,200000,300000,400000),
                     labels = c("100k", "200k", "300k", "400k")) +
  ggtitle("Median Home Values, Selected St. Louis Zip Codes",
          subtitle = "Zillow.com") 
ggplotly(p2, tooltip = c("x","group", "y"))

#Difference in Difference
library(broom)
head(zip_longer)
#Approach One; Construction Date
zip_longer2 <- zip_data %>%
  select(c(RegionID:CountyName), c(X2015.11.30:X2021.11.30)) %>%
  pivot_longer(cols = 7:79, names_to = "Date" ,values_to = "HomeValue") %>%
  filter(RegionName == "63130" | RegionName == "63112" 
         | RegionName == "63114" | RegionName == "63133" 
         | RegionName == "63108") %>%
  mutate(Date = str_remove_all(Date, "X"),
         Date = as.Date(Date,"%Y.%m.%d"))


#Check Parallel Trends
ggplot(zip_longer2) +
  geom_line(aes(x=Date, y=HomeValue, group = RegionName, color = RegionName)) +
  theme_classic() +
  scale_y_continuous(name = "Home Value(USD)",
                     breaks = c(100000,200000,300000,400000),
                     labels = c("100k", "200k", "300k", "400k")) +
  ggtitle("Median Home Values, Selected St. Louis Zip Codes",
          subtitle = "Zillow.com") +
  geom_segment(aes(x=as.Date("2018-11-01"), y=0, xend=as.Date("2018-11-01"), yend=325000), color = "blue") 
  # geom_segment(aes(x=as.Date("2010-07-31"), y=0, xend=as.Date("2010-07-31"), yend=325000), color = "blue") #+
  #geom_segment(aes(x=as.Date("2015-03-31"), y=0, xend=as.Date("2015-03-31"), yend=325000), color = "blue")

#Regression
zip_regress2 <- zip_longer2 %>%
  mutate(time = ifelse(Date >= "2018-11-30", 1, 0),
         treated = ifelse(RegionName == "63130" |
                            RegionName == "63112", 1, 0),
         did = time*treated)

did_rg2 <- lm(HomeValue ~ time + treated + did, data = zip_regress2)
tidy(did_rg2, conf.int = T)


#Approach Two: From Date of Announcement
zip_longer3 <- zip_data %>%
  select(c(RegionID:CountyName), c(X2007.07.31:X2013.07.31)) %>%
  pivot_longer(cols = 7:79, names_to = "Date" ,values_to = "HomeValue") %>%
  filter(RegionName == "63130" | RegionName == "63112" 
         | RegionName == "63114" | RegionName == "63133" 
         | RegionName == "63108") %>%
  mutate(Date = str_remove_all(Date, "X"),
         Date = as.Date(Date,"%Y.%m.%d")) 
zip_regress3 <- zip_longer3 %>%
  mutate(time = ifelse(Date >= "2010-07-31", 1, 0),
         treated = ifelse(RegionName == "63130" |
                            RegionName == "63112", 1, 0),
         did = time*treated)
#Check Parallel Trends
ggplot(zip_longer3) +
  geom_line(aes(x=Date, y=HomeValue, group = RegionName, color = RegionName)) +
  theme_classic() +
  scale_y_continuous(name = "Home Value(USD)",
                     breaks = c(100000,200000,300000,400000),
                     labels = c("100k", "200k", "300k", "400k")) +
  ggtitle("Median Home Values, Selected St. Louis Zip Codes",
          subtitle = "Zillow.com") +
 # geom_segment(aes(x=as.Date("2018-11-01"), y=0, xend=as.Date("2018-11-01"), yend=325000), color = "blue") +
  geom_segment(aes(x=as.Date("2010-07-31"), y=0, xend=as.Date("2010-07-31"), yend=325000), color = "blue") #+
  #geom_segment(aes(x=as.Date("2015-03-31"), y=0, xend=as.Date("2015-03-31"), yend=325000), color = "blue")
  

did_rg3 <- lm(HomeValue ~ time + treated + did, data = zip_regress3)
tidy(did_rg3, conf.int = T)

#Approach Three: From Date of Construction
zip_longer4 <- zip_data %>%
  select(c(RegionID:CountyName), c(X2012.03.31:X2018.03.31)) %>%
  pivot_longer(cols = 7:79, names_to = "Date" ,values_to = "HomeValue") %>%
  filter(RegionName == "63130" | RegionName == "63112" 
         | RegionName == "63114" | RegionName == "63133" 
         | RegionName == "63108") %>%
  mutate(Date = str_remove_all(Date, "X"),
         Date = as.Date(Date,"%Y.%m.%d")) 
zip_regress4 <- zip_longer4 %>%
  mutate(time = ifelse(Date >= "2015-03-31", 1, 0),
         treated = ifelse(RegionName == "63130" |
                            RegionName == "63112", 1, 0),
         did = time*treated)
#Check Parallel Trends
ggplot(zip_longer4) +
  geom_line(aes(x=Date, y=HomeValue, group = RegionName, color = RegionName)) +
  theme_classic() +
  scale_y_continuous(name = "Home Value(USD)",
                     breaks = c(100000,200000,300000,400000),
                     labels = c("100k", "200k", "300k", "400k")) +
  ggtitle("Median Home Values, Selected St. Louis Zip Codes",
          subtitle = "Zillow.com") +
  geom_segment(aes(x=as.Date("2015-03-31"), y=0, xend=as.Date("2015-03-31"), yend=325000), color = "blue")

did_rg4 <- lm(HomeValue ~ time + treated + did, data = zip_regress4)
tidy(did_rg4, conf.int = T)

#Approach 4: From Announcement to Present
zip_longer5 <- zip_data %>%
  select(c(RegionID:CountyName), c(X2002.07.31:X2023.03.31)) %>%
  pivot_longer(cols = 7:255, names_to = "Date" ,values_to = "HomeValue") %>%
  filter(RegionName == "63130" | RegionName == "63112" 
         | RegionName == "63114" | RegionName == "63133" 
         | RegionName == "63108") %>%
  mutate(Date = str_remove_all(Date, "X"),
         Date = as.Date(Date,"%Y.%m.%d")) 
zip_regress5 <- zip_longer5 %>%
  mutate(time = ifelse(Date >= "2010-07-31", 1, 0),
         treated = ifelse(RegionName == "63130" |
                            RegionName == "63112", 1, 0),
         did = time*treated)
#Check Parallel Trends
ggplot(zip_longer5) +
  geom_line(aes(x=Date, y=HomeValue, group = RegionName, color = RegionName)) +
  theme_classic() +
  scale_y_continuous(name = "Home Value(USD)",
                     breaks = c(100000,200000,300000,400000),
                     labels = c("100k", "200k", "300k", "400k")) +
  ggtitle("Median Home Values, Selected St. Louis Zip Codes",
          subtitle = "Zillow.com") +
  labs(caption = "Zip Codes 63130 and 63112 contain the Loop Trolley,
       which was announced in 2010 and completed in 2018.") +
  #geom_segment(aes(x=as.Date("2018-11-01"), y=0, xend=as.Date("2018-11-01"), yend=325000), color = "blue") +
  geom_segment(aes(x=as.Date("2010-07-31"), y=0, xend=as.Date("2010-07-31"), yend=325000), color = "blue") 
  #geom_segment(aes(x=as.Date("2015-03-31"), y=0, xend=as.Date("2015-03-31"), yend=325000), color = "blue")

did_rg5 <- lm(HomeValue ~ time + treated + did, data = zip_regress5)
tidy(did_rg5, conf.int = T)
