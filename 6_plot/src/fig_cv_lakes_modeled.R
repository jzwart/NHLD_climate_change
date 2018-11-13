


scen = readRDS('1_data/out/scenarios_qa.rds') %>%
  dplyr::filter(period == 'Retro', season == 'all')

area_total = sum(scen$Area)

emit_total = sum(scen$Emit)

out <- data_frame(emit = emit_total, area = area_total, emit_areal = emit / area, regional_emit = emit_areal * area, samps = nrow(scen))

samps <- round(seq(3, length(unique(scen$Permanent_)), length.out = 40), digits = 0)
samps <- c(2,10,20,40,80,120,240,500,800,1000,2000,3000,3669)

for(j in 1:1000){
  for(i in 1:length(samps)){

    rows = sample(1:nrow(scen), size = samps[i], replace = F)

    cur <- scen %>%
      dplyr::slice(rows) %>%
      summarise(emit = sum(Emit),
                area = sum(Area)) %>%
      mutate(emit_areal = emit / area,
             regional_emit = emit_areal * area_total,
             samps = samps[i])

    out <- bind_rows(out, cur)
  }
  print(j)
}


perc = round(out$samps / nrow(scen) *100, 0)
sd = out %>%
  group_by(samps) %>%
  summarise(sd = sd(regional_emit)) %>%
  ungroup()
diff = abs(out$regional_emit - emit_total)/ emit_total * 100 * ifelse(out$regional_emit > emit_total,1,-1)
plot(log10(out$regional_emit) ~ perc)

plot(diff ~ perc, ylim = c(-100,200))
boxplot(diff~perc, outline = F)

plot(sd$sd/emit_total~sd$samps, type ='l')




