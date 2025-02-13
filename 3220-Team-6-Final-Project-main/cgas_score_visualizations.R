trainData %>%
  ggplot(aes(x = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(col = Day), lwd = 1) +
  labs(x = "Total bill", y = "Density") +
  # Change default colors
  scale_color_manual(values = colors)

colors <- c("#1f77b4","#ff7f0e", "#2ca02c", "#d62728",
            "#9467bd","#8c564b", "#e377c2", "#7f7f7f",
            "#bcbd22", "#17becf")

data_no_na %>%
  ggplot(aes(x = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(col = as.factor(sii)), lwd = 1) +
  labs(x = "CGAS Score", y = "Density") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na %>%
  ggplot(aes(x = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(col = as.factor(sii)), lwd = 1) +
  labs(x = "CGAS Score", y = "Density") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na |>
  ggplot(aes(x=CGAS.CGAS_Score)) + 
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(fill=as.factor(sii)), alpha=0.9, color="black") +
  labs(x="CGAS Score", y="Density") + 
  # Change default colors
  scale_fill_manual(values=colors) +
  # faceting creates separate plots for each day
  facet_wrap(~as.factor(sii))

#IMPORTANT: Fix this!
theme_set(theme_gray(base_size=11))

data_no_na |>
  ggplot(aes(x = BIA.BIA_FFM, y = PCIAT.PCIAT_Total)) +
  geom_point()

data_no_na |>
  ggplot(aes(x = CGAS.CGAS_Score, y = as.factor(sii))) +
  # jittering spreads points out vertically
  geom_jitter(aes(col = as.factor(sii), pch = as.factor(sii)), height = 0.5) +
  labs(x = "CGAS Score", y = "sii") +
  # Remove legend
  theme(legend.position = "none") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na |>
  ggplot(aes(x = as.factor(sii), y = CGAS.CGAS_Score)) +
  # jittering spreads points out horizontally
  geom_jitter(aes(col = as.factor(sii), pch = as.factor(sii)), width = 0.5) +
  labs(x = "sii", y = "CGAS Score") +
  # Remove legend
  theme(legend.position = "none") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na |>
  ggplot(aes(x = as.factor(sii), y = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_violin(aes(fill = as.factor(sii)), alpha = 0.7, color = "black") +
  labs(x = "sii", y = "CGAS Score") +
  # Change default colors
  scale_fill_manual(values = colors)


data_no_na |>
  ggplot(aes(x = CGAS.CGAS_Score)) +
  geom_histogram(aes(fill = as.factor(sii)), color = "black", bins = 10) +
  labs(x = "CGAS Score", y = "Count") +
  # Change default colors
  scale_fill_manual(values = colors) +
  # faceting creates separate plots for each day
  facet_wrap(~as.factor(sii))


data_no_na %>%
  ggplot(aes(x = BIA.BIA_FFM)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(col = as.factor(sii)), lwd = 1) +
  labs(x = "CGAS Score", y = "Density") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na %>%
  ggplot(aes(x = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(col = as.factor(sii)), lwd = 1) +
  labs(x = "CGAS Score", y = "Density") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na |>
  ggplot(aes(x=CGAS.CGAS_Score)) + 
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(fill=as.factor(sii)), alpha=0.9, color="black") +
  labs(x="CGAS Score", y="Density") + 
  # Change default colors
  scale_fill_manual(values=colors) +
  # faceting creates separate plots for each day
  facet_wrap(~as.factor(sii))

data_no_na |>
  ggplot(aes(x = CGAS.CGAS_Score, y = as.factor(sii))) +
  geom_point(aes(col = as.factor(sii)))

data_no_na |>
  ggplot(aes(x = CGAS.CGAS_Score, y = as.factor(sii))) +
  # jittering spreads points out vertically
  geom_jitter(aes(col = as.factor(sii), pch = as.factor(sii)), height = 0.5) +
  labs(x = "CGAS Score", y = "sii") +
  # Remove legend
  theme(legend.position = "none") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na |>
  ggplot(aes(x = as.factor(sii), y = CGAS.CGAS_Score)) +
  # jittering spreads points out horizontally
  geom_jitter(aes(col = as.factor(sii), pch = as.factor(sii)), width = 0.5) +
  labs(x = "sii", y = "CGAS Score") +
  # Remove legend
  theme(legend.position = "none") +
  # Change default colors
  scale_color_manual(values = colors)

data_no_na |>
  ggplot(aes(x = as.factor(sii), y = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_violin(aes(fill = as.factor(sii)), alpha = 0.7, color = "black") +
  labs(x = "sii", y = "CGAS Score") +
  # Change default colors
  scale_fill_manual(values = colors)


data_no_na |>
  ggplot(aes(x = CGAS.CGAS_Score)) +
  geom_histogram(aes(fill = as.factor(sii)), color = "black", bins = 10) +
  labs(x = "CGAS Score", y = "Count") +
  # Change default colors
  scale_fill_manual(values = colors) +
  # faceting creates separate plots for each day
  facet_wrap(~as.factor(sii))


trainData %>%
  ggplot(aes(x = CGAS.CGAS_Score)) +
  # alpha describes transparency (0=transparent, 1=solid)
  geom_density(aes(col = Day), lwd = 1) +
  labs(x = "Total bill", y = "Density") +
  # Change default colors
  scale_color_manual(values = colors)

for (x in 1:ncol(data_no_na)) {
  print(class(as.factor(data_no_na$sii)))
}