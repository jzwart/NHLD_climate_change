

conc_sens <- readRDS('4_conc_sens/out/conc_sens_summary.rds') %>% as_tibble() %>%
  mutate(scenario = paste(doc_change, dic_change, p_change, sep ='_'))

scenarios <- readRDS('1_data/out/scenarios_qa.rds') %>%
  dplyr::filter(period == '2080s', gcm == 'CESM1_CAM5', season =='all') %>%
  mutate(doc_change = 1,
         dic_change = 1,
         p_change = 1,
         scenario = paste(doc_change, dic_change, p_change, sep ='_')) %>%
  select(colnames(conc_sens))

conc_sens <- conc_sens %>%  # only keeping lakes that are in scenario file
  group_by(scenario) %>%
  dplyr::filter(Permanent_ %in% scenarios$Permanent_) %>%
  ungroup()


all <- bind_rows(conc_sens, scenarios)

total <- all %>%
  group_by(scenario) %>%
  summarise(Emit = sum(Emit * 12 * 365)) %>%
  ungroup()

g = ggplot(all, aes(y = log(Emit), group = scenario)) +
  geom_boxplot()

g


# would we expect a uniform increase / decrease in concentration change across all the NHLD? probably not...
#  we could sample each lake from the 9 scenarios, re-calculate total emissions, and do this 10,000 times to create a distribution;
#  should be OK since all lakes are run independently of each other
