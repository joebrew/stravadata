library(yaml)
library(httr)
library(dplyr)
library(jsonlite)
library(rStrava)

# Define credentials
credentials <- yaml::yaml.load_file('credentials.yaml')
app_name <- 'GPSart' # chosen by user
app_client_id  <- credentials$client_id
app_secret <- credentials$secret

# Authenticate
whos <- dir('tokens')
whos <- c('joe')
owd <- getwd()
out_list <- list()
for(i in 1:length(whos)){
  who <- whos[i]  
  setwd(paste0('tokens/', who))
  if(FALSE){
    stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all,activity:write"))
  } else {
    stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
  }
  # Get activities
  acts <- get_activity_list(stoken) # takes approximately 1 minute
  # Convert to dataframe
  act_data <- compile_activities(acts)
  # Dump in list
  out_list[[i]] <- act_data %>% mutate(who = who)
  setwd(owd)
}
# Create a dataframe of all activities
activities <- bind_rows(out_list)

# Get rolling average by activity type
pd <- activities %>%
  mutate(sport_type = ifelse(sport_type == 'EBikeRide', 'Ride',
                             ifelse(sport_type == 'Kayaking', 'Rowing',
                                    ifelse(sport_type == 'Walk', 'Hike',
                                           ifelse(sport_type == 'WeightTraining', 'Workout', sport_type))))) %>%
  filter(!sport_type %in% 'StandUpPaddling') %>%
  mutate(date = as.Date(start_date)) %>%
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(month, sport_type) %>%
  summarise(moving_time = sum(moving_time) / 60 / 60) %>%
  ungroup
library(ggplot2)
ggplot(data = pd,
       aes(x = month,
           y = moving_time,
           fill = sport_type)) +
  geom_bar(stat = 'identity',
           position = position_stack(),
           # color = 'black',
           color = NA,
           size = 0.2) +
  labs(x = 'Month',
       y = 'Hours per month') +
  scale_fill_manual(name = '', values = RColorBrewer::brewer.pal(n = length(unique(pd$sport_type)), 'Set3')) +
  theme(legend.position = 'bottom') +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'black', color = NA),
        axis.line = element_line(size = 0, linetype = "solid",
                                 colour = "black")) 

# Get coordinates of polylines
library(googlePolylines)
activities$polyline <- decode(activities$map.summary_polyline)

# Keep just SCQ
scq <- activities %>%
  filter(start_latlng1 > 41 & start_latlng1 < 41.75 &
           start_latlng2 > 1 & start_latlng2 < 1.7)
out_list <- list()
for(i in 1:nrow(scq)){
  right <- scq[i,] %>%
    dplyr::select(id, sport_type, start_date)
  left <- scq$polyline[i][[1]] 
  joined <- bind_cols(left, right)
  out_list[[i]] <- joined
}
gps <- bind_rows(out_list)
library(ggplot2)
ggplot(data = gps,
       aes(x = lon,
           y = lat,
           group = id)) +
  ggthemes::theme_map() +
  geom_path(size = 0.01, alpha = 0.1, color = 'green') +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'black', color = NA),
        axis.line = element_line(size = 0, linetype = "solid",
                                 colour = "black")) 


# Get most recent activity
recent_id <- act_data$id[1]
sub_acts <- act_data %>% filter(id == recent_id)
stream_list <- list()
counter <- 0
for(i in 1:nrow(sub_acts)){
  message(i, ' of ', nrow(sub_acts))
  this_act <- sub_acts[i,]
  ok <- !is.null(this_act$start_latlng1)
  if(ok){
    stream <- get_activity_streams(this_act, stoken = stoken)
    if(nrow(stream) > 0){
      counter <- counter +1
      stream_list[[counter]] <- stream
    }
  }
}
pd <- bind_rows(stream_list)

# Get watts by minute
pd$minute <- floor(pd$time / 60)



# Get average watts per minute
pdx <- pd %>%
  group_by(minute) %>%
  summarise(watts = mean(watts, na.rm = TRUE),
            pulsacions = mean(heartrate, na.rm = TRUE),
            lng = mean(lng),
            lat = mean(lat),
            altitude = mean(altitude)) %>%
  tidyr::gather(key, value, watts:pulsacions) %>%
  filter(minute >= 35)

# ggplot(data = pdx,
#        aes(x = minute,
#            y = altitude,
#            color = value,
#            size = value)) +
#   geom_point() +
#   facet_wrap(~key)

ggplot(data = pdx %>% filter(key == 'watts'),
       aes(x = lng,
           y = lat,
           color = value)) +
  geom_point(size = 3) +
  scale_colour_gradientn(name = 'Watts', colors = rev(rainbow(5)))

library(ggplot2)
ggplot(data = pdx,
       aes(x = minute,
           y = value,
           fill = key)) +
  # geom_area(alpha = 0.6, color = 'black') +
  geom_line(size = 3) +
  facet_wrap(~key, ncol = 1, scales = 'free_y') +
  theme(legend.position = 'none') +
  scale_fill_manual(name = '', values = c('red', 'blue'))

# Get rolling average
library(zoo)
pd <- pd %>%
  mutate(watts_roll_0020 = zoo::rollmean(watts, k = 20, fill = NA),
         watts_roll_01 = zoo::rollmean(watts, k = 60, fill = NA),
         watts_roll_05 = zoo::rollmean(watts, k = 300, fill = NA),
         watts_roll_10 = zoo::rollmean(watts, k = 600, fill = NA),
         watts_roll_20 = zoo::rollmean(watts, k = 1200, fill = NA),
         hr_roll_01 = zoo::rollmean(heartrate, k = 60, fill = NA)) %>%
  filter(time >= 2150,
         time <= 5000)

ggplot(data = pd,
       aes(x = time,
           y = watts_roll_0020)) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = watts_roll_05), color = 'red', size = 2) +
  labs(x = 'Seconds since start',
       y = 'Watts',
       title = '20 second moving average (black) and 5 minute moving average (red) watts')

ggplot(data = pd,
       aes(x = time,
           y = hr_roll_01)) +
  geom_line()

library(ggplot2)
ggplot(data = pd,
       aes(x = time,
           y = heartrate)) +
  facet_wrap(~who) +
  geom_line()
