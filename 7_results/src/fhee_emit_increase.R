


low_fhee_emit_increase = stage_future %>%
  group_by(gcm_future, period_future) %>%
  arrange(percentEvap_future) %>%
  mutate(frac_lake = seq(1:n()) / n()) %>%
  dplyr::filter(frac_lake <= 0.1) %>%
  summarise(frac_emit_increase = sum(Emit_future > Emit_retro)/n(),
            p_e = mean(Precip_future - Evap_future),
            dic_v_resp = mean(dicLoadvResp_future)) %>%
  arrange(p_e)

low_fhee_emit_increase


high_fhee_emit_increase = stage_future %>%
  group_by(gcm_future, period_future) %>%
  arrange(percentEvap_future) %>%
  mutate(frac_lake = seq(1:n()) / n()) %>%
  dplyr::filter(frac_lake >= 0.9) %>%
  summarise(frac_emit_increase = sum(Emit_future > Emit_retro)/n(),
            p_e = mean(Precip_future - Evap_future),
            dic_v_resp = mean(dicLoadvResp_future)) %>%
  arrange(p_e)

high_fhee_emit_increase



plot(low_fhee_emit_increase$frac_emit_increase~low_fhee_emit_increase$p_e,pch=16)
points(high_fhee_emit_increase$frac_emit_increase~high_fhee_emit_increase$p_e, col ='red',pch=16)


mean(low_fhee_emit_increase$dic_v_resp)
sd(low_fhee_emit_increase$dic_v_resp)

mean(high_fhee_emit_increase$dic_v_resp)
