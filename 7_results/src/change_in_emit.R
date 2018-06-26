
# why is there less emissions under future climate scenarios?

# I think this is driven by a reduction in SWin for large drainage lakes that have high DIC loading; this is despite the fact that
#  a majority of lakes increase in total emissions and areal emissions

emit = readRDS('2_analysis/out/emit.rds') %>%
  dplyr::filter(period == '2080s', season == 'all') %>%
  mutate(delta_emit_log = log10(abs(delta_emit)),
         sign_emit = case_when(delta_emit < 0 ~ -1,
                               delta_emit > 0 ~ 1))

emit_areal = readRDS('2_analysis/out/emit_areal.rds') %>%
  dplyr::filter(period == '2080s', season == 'all') %>%
  mutate(delta_emit_areal_log = log10(abs(delta_emit_areal)),
         sign_emit_areal = case_when(delta_emit_areal < 0 ~ -1,
                               delta_emit_areal > 0 ~ 1))

table(emit$sign_emit) # 775 lakes have lower total emissions, 2894 lakes have higher total emissions
table(emit_areal$sign_emit_areal) # 553 lakes have lower areal emissions, 3116 have higher areal emissions

dic_v_resp = readRDS('2_analysis/out/dic_v_resp.rds') %>%
  dplyr::filter(period == '2080s', season == 'all') %>%
  mutate(delta_dic_v_resp_log = log10(abs(delta_dic_v_resp)),
         sign_dic_v_resp = case_when(delta_dic_v_resp < 0 ~ -1,
                                     delta_dic_v_resp > 0 ~ 1))

fhee = readRDS('2_analysis/out/fhee.rds')%>%
  dplyr::filter(period == '2080s', season == 'all')

all = emit %>%
  left_join(emit_areal, by = 'Permanent_') %>%
  left_join(dic_v_resp, by = 'Permanent_') %>%
  left_join(fhee, by = 'Permanent_')


ggplot(emit, aes(x = retro_emit, y = delta_emit_log * sign_emit)) +
  geom_point() +
  scale_x_log10() +
  theme_classic()

ggplot(all, aes(x = delta_dic_v_resp_log * sign_dic_v_resp, y = delta_emit_log * sign_emit)) +
  geom_point() +
  theme_classic()

ggplot(all, aes(x = mean_fhee, y = delta_emit_log * sign_emit, color = ratio_fhee)) +
  geom_point() +
  theme_classic() +
  scale_color_continuous(low = 'grey', high = 'black')


summary(lm(all$delta_emit_log~all$mean_fhee+all$ratio_fhee))





