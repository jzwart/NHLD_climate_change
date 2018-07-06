
library(ggplot2)

d <- readRDS('3_summarize/out/monthly_ave.rds')

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
  summarise(med_all = median(stage)) %>%
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

gpp <- d %>%
  mutate(gpp = gpp / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(gpp)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

doc_resp <- d %>%
  mutate(doc_resp = doc_resp / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(doc_resp)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

gw_in <- d %>%
  mutate(gw_in = gw_in / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(gw_in)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

doc_load <- d %>%
  mutate(doc_loads = doc_loads / area) %>% # per lake area
  group_by(month, period, gcm) %>%
  summarise(med_all = median(doc_loads)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

d_epi <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(d_epi)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

ph <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(ph)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

fhee <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(fhee)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

water_in <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(water_in)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

dic_v_resp <- d %>%
  group_by(month, period, gcm) %>%
  summarise(med_all = median(dic_v_resp)) %>%
  ungroup() %>%
  group_by(month, period) %>%
  summarise(med = median(med_all),
            min = min(med_all),
            max = max(med_all)) %>%
  ungroup()

ggplot(swin, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(kd, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(precip, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(area, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(stage, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(evap, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(baseflow, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(emit, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(gpp, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(d_epi, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(ph, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(doc_resp, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(gw_in, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(doc_load, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(fhee, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(dic_v_resp, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()

ggplot(water_in, aes(x = as.numeric(month), y = med, color = period)) +
  geom_line(size = 2) +
  geom_point(size= 4) +
  geom_ribbon(aes(x = as.numeric(month), y = med, ymax = max, ymin = min, color = period, fill = period), alpha = .2) +
  theme_classic()
