

# ggplot(dplyr::filter(merged, doc_conc_retro <=40),
#        aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
#            y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1), color = doc_conc_retro)) +
#   geom_point() +
#   ylim(c(-100,250)) +
#   xlim(c(-30,150)) +
#   theme_classic() +
#   xlab(expression(Delta~DOC~('%'))) +
#   ylab(expression(Delta~GPP~('%')))+
#   theme(axis.text = element_text(size=16),
#         axis.title = element_text(size = 16),
#         legend.title = element_text(size =14),
#         legend.position = c(.4,.85),
#         legend.background = element_blank(),
#         legend.text = element_text(size = 14))+
#   scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
#                          low = 'orange',high = 'darkblue') +
#   geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
#   geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)



quad = merged %>%
  mutate(quadrant = case_when(doc_conc_future > doc_conc_retro & GPP_future>GPP_retro ~ 2,
                              doc_conc_future > doc_conc_retro & GPP_future<GPP_retro ~ 3,
                              doc_conc_future < doc_conc_retro & GPP_future<GPP_retro ~ 4,
                              doc_conc_future < doc_conc_retro & GPP_future>GPP_retro ~ 1))

quad$quadrant = factor(quad$quadrant)


ggplot(data = quad, aes(x = quadrant, y = abs(Emit_future-Emit_retro)/Emit_retro*100*ifelse(Emit_future>Emit_retro,1,-1), group = quadrant)) +
  geom_boxplot()

ggplot(data = quad, aes(x = quadrant, y = abs(Bury_future-Bury_retro)/Bury_retro*100*ifelse(Bury_future>Bury_retro,1,-1), group = quadrant)) +
  geom_boxplot() +
  ylim(-100,100)

ggplot(data = quad, aes(x = quadrant,
                        y =abs(dicLoadvResp_future-dicLoadvResp_retro)/dicLoadvResp_retro*100*ifelse(dicLoadvResp_future>dicLoadvResp_retro,1,-1),
                        group = quadrant)) +
  geom_boxplot() +
  ylim(-100,100)

ggplot(data = quad, aes(x = quadrant, y = abs(waterIn_future-waterIn_retro)/waterIn_retro*100*ifelse(waterIn_future>waterIn_retro,1,-1), group = quadrant)) +
  geom_boxplot() +
  ylim(-100,100)

quad %>%
  group_by(quadrant) %>%
  summarise(count = n(),
            bury = sum(Bury_retro)/10^9,
            bury_future = sum(Bury_future)/10^9,
            frac_hetero = sum(NEP_future<NEP_retro)/count,
            emit_bury = sum((Emit_future - Bury_future)-(Emit_retro - Bury_retro))/10^9,
            delta_bury = median(abs(Bury_future-Bury_retro)/Bury_retro*100*ifelse(Bury_future>Bury_retro,1,-1)),
            delta_emit = median(abs(Emit_future-Emit_retro)/Emit_retro*100*ifelse(Emit_future>Emit_retro,1,-1)),
            delta_dic_v_resp = median(abs(dicLoadvResp_future-dicLoadvResp_retro)/dicLoadvResp_retro*100*ifelse(dicLoadvResp_future>dicLoadvResp_retro,1,-1)),
            dic_v_resp = median(dicLoadvResp_future),
            doc_resp = median(abs(DOC_Respired_future-DOC_Respired_retro)/DOC_Respired_retro*100*ifelse(DOC_Respired_future>DOC_Respired_retro,1,-1)),
            delta_fhee = median(abs(percentEvap_future-percentEvap_retro)/percentEvap_retro*100*ifelse(percentEvap_future>percentEvap_retro,1,-1)),
            fhee = median(percentEvap_retro),
            delta_water_in = median(abs(waterIn_future-waterIn_retro)/waterIn_retro*100*ifelse(waterIn_future>waterIn_retro,1,-1)),
            doc = mean(doc_conc_retro),
            doc_future = mean(doc_conc_future),
            area = median(Area_retro),
            gpp = median(GPP_retro),
            emit = sum(Emit_retro)/10^9,
            emit_future = sum(Emit_future)/sum(quad$Emit_future))

emit = ggplot(data = quad, aes(x = quadrant, y = abs(Emit_future-Emit_retro)/Emit_retro*100*ifelse(Emit_future>Emit_retro,1,-1), group = quadrant)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~Emissions~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  ylim(c(-60,60))+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

bury = ggplot(data = quad, aes(x = quadrant, y = abs(Bury_future-Bury_retro)/Bury_retro*100*ifelse(Bury_future>Bury_retro,1,-1), group = quadrant)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~Burial~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  ylim(c(-60,60))+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

emit_bury = ggplot(data = quad, aes(x = quadrant, y = abs((Emit_future-Bury_future)-(Emit_retro-Bury_retro))/(Emit_retro-Bury_retro)*
                          100*ifelse((Emit_future-Bury_future)>(Emit_retro-Bury_retro),1,-1), group = quadrant)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~(Emissions-Burial)~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  ylim(c(-80,150))+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

fracret = ggplot(data = quad, aes(x = quadrant, y = abs(FracRet_future*100 - FracRet_retro*100) * ifelse(FracRet_future>FracRet_retro,1,-1), group = quadrant)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~Percent~C~Removed~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  ylim(-30,30)+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

fhee = ggplot(data = quad, aes(x = quadrant, y = percentEvap_future, group = quadrant)) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(FHEE)) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

p_e = ggplot(data = quad, aes(x = quadrant, y = Precip_future - Evap_future, group = quadrant)) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Precipitation-Evapotranspiration~(mm~year^-1))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

dic_v_resp = ggplot(data = quad, aes(x = quadrant,
                        y = abs(dicLoadvResp_future-dicLoadvResp_retro)/dicLoadvResp_retro*100*ifelse(dicLoadvResp_future>dicLoadvResp_retro,1,-1),
                        group = quadrant)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~DIC~Load:DIC~Produced~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  ylim(-100,100)+
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

ggplot(data = quad, aes(x = quadrant,
                        y = dicLoadvResp_retro,
                        group = quadrant)) +
  geom_hline(yintercept = 1, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(DIC~Load:DIC~Produced)) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP')) +
  scale_y_log10()



quad$sed_ratio_future = quad$Sed_phyto_future / quad$Sed_tPOC_future
quad$sed_ratio_retro = quad$Sed_phyto_retro / quad$Sed_tPOC_retro

quad$bury_ratio_future = quad$Burial_phyto_future / quad$Burial_tPOC_future
quad$bury_ratio_retro = quad$Burial_phyto_retro / quad$Burial_tPOC_retro


sed_auto = ggplot(data = quad, aes(x = quadrant, y = abs(bury_ratio_future*100-bury_ratio_retro*100)*ifelse(bury_ratio_future>bury_ratio_retro,1,-1),
                                   group = factor(quadrant))) +
  geom_hline(yintercept = 0, linetype ='dashed',color ='grey40') +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~Percent~Sediment~Autochthony~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank()) +
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))

ggplot(data = quad, aes(x = quadrant, y = (NEP_future-NEP_retro)/abs(NEP_retro)*100, group = quadrant)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color ='grey40', size = 1.3) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Delta~NEP~('%'))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        legend.title = element_text(size =14),
        legend.position = c(.35,.7),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP')) +
  ylim(-200,100)

ggplot(data =quad, aes(x =abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1), y=(NEP_future-NEP_retro)/abs(NEP_retro) *100)) +
  geom_point() +
  ylim(-200,100)+
  xlim(-100,200)

# anova(lm(data = quad, formula = abs(FracRet_future*100 - FracRet_retro*100) * ifelse(FracRet_future>FracRet_retro,1,-1) ~ quadrant))
# anova(lm(data = quad, formula = abs((Emit_future-Bury_future)-(Emit_retro-Bury_retro))/(Emit_retro-Bury_retro)*
#   100*ifelse((Emit_future-Bury_future)>(Emit_retro-Bury_retro),1,-1) ~ quadrant))
#
# hist(quad$Sed_phyto_future/quad$Sed_tPOC_future)
# hist(quad$Sed_phyto_retro/quad$Sed_tPOC_retro)

g = plot_grid(bury, sed_auto, emit, fracret, p_e, fhee,  labels = c('A', 'B', 'C', 'D', 'E', 'F'), rows = 3, align = 'h')

ggsave('6_plot/out/fig_quadrants.png', plot=g, width = 14, height = 21)




ggplot(data = quad, aes(x = quadrant, y = Emit_future,
                        group = factor(quadrant))) +
  geom_boxplot(size =1.3) +
  theme_classic() +
  ylab(expression(Emissions~(g~C~yr^-1))) +
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank()) +
  scale_x_discrete(breaks = c(1,2,3,4),
                   labels = c('-DOC, +GPP','+DOC, +GPP','+DOC, -GPP','-DOC, -GPP'))+
  scale_y_log10()

merged$p_e = merged$Precip_future-merged$Evap_future

ggplot(dplyr::filter(merged, doc_conc_retro <=40),
       aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
           y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1), color = Precip_future-Evap_future, shape = period_future,
           group = Precip_future-Evap_future)) +
  geom_point() +
  ylim(c(-100,250)) +
  xlim(c(-30,150)) +
  theme_classic() +
  xlab(expression(Delta~DOC~('%'))) +
  ylab(expression(Delta~GPP~('%')))+
  facet_wrap(~p_e)+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.position = c(.4,.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  # scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
  #                        low = 'orange',high = 'darkblue') +
  scale_color_viridis_c(direction = -1) +
  geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
  geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)


ggplot(dplyr::filter(merged, doc_conc_retro <=10),
       aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
           y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1), color = percentEvap_future)) +
  geom_point() +
  ylim(c(-100,250)) +
  xlim(c(-30,150)) +
  theme_classic() +
  xlab(expression(Delta~DOC~('%'))) +
  ylab(expression(Delta~GPP~('%')))+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.position = c(.4,.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
                         low = 'orange',high = 'darkblue') +
  geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
  geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)


ggplot(dplyr::filter(merged, doc_conc_retro >=15),
       aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
           y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1), color = percentEvap_future)) +
  geom_point() +
  ylim(c(-100,250)) +
  xlim(c(-30,150)) +
  theme_classic() +
  xlab(expression(Delta~DOC~('%'))) +
  ylab(expression(Delta~GPP~('%')))+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.position = c(.4,.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
                         low = 'orange',high = 'darkblue') +
  geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
  geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)



ggplot(dplyr::filter(merged, doc_conc_retro <10),
       aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
           y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1),
           color = percentEvap_future)) +
  geom_point() +
  ylim(c(-100,250)) +
  xlim(c(-30,150)) +
  theme_classic() +
  xlab(expression(Delta~DOC~('%'))) +
  ylab(expression(Delta~GPP~('%')))+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.position = c(.4,.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
                         low = 'orange',high = 'darkblue') +
  geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
  geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)



ggplot(dplyr::filter(merged, doc_conc_retro <=40),
       aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
           y = abs(Emit_future-Emit_retro)/Emit_retro*100*ifelse(Emit_future>Emit_retro,1,-1), color = doc_conc_retro)) +
  geom_point() +
  xlim(c(-30,150)) +
  theme_classic() +
  xlab(expression(Delta~DOC~('%'))) +
  ylab(expression(Delta~GPP~('%')))+
  theme(axis.text = element_text(size=16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size =14),
        legend.position = c(.4,.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 14))+
  scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
                         low = 'orange',high = 'darkblue') +
  geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
  geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)

# gpp_doc_ratio
