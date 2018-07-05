
library(ggplot2)

d <- readRDS('3_summarize/out/sw_in_month.rds')

watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
                       stringsAsFactors = F,
                       header=T,
                       sep = '\t')

d <- d %>%
  left_join(watersheds, by = 'Permanent_') %>%
  dplyr::filter(!is.na(Area_m2))

swin <- d %>%
  mutate(sw_in = sw_in / Area_m2) %>%   # per catchment area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(sw_in)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

kd <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(kd)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

precip <- d %>%
  mutate(precip = precip / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(precip)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

area <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(area)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

stage <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(area)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

evap <- d %>%
  mutate(evap = evap / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(evap)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

baseflow <- d %>%
  mutate(baseflow = baseflow / Area_m2) %>%  # per catchment area
  group_by(month, period, gcm) %>%
  summarise(med_all = mean(baseflow)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

emit <- d %>%
  mutate(emit = emit / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(emit)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()


ggplot(swin, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(kd, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(precip, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(area, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(evap, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(baseflow, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(emit, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 3) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()
