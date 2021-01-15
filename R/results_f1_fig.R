library(ggplot2)
mi <- remake::fetch('model_inventory') %>% filter(train_n_positives_train > 20)
ggplot(mi, aes(x = train_f1, y = test_f1, size = train_n_positives_train + n_positives_test)) + 
  geom_point() + 
  labs(x = 'Training set F1 score (1 = perfect)', y = 'Test set F1 score (1 = perfect)', 
       title = 'Gage height anomaly detection\nmodel performance by site',
       subtitle = 'Excluding sites with fewer than 20 anomalies in training set',
       size = 'Total anomalies') + 
  geom_abline(slope = 1, linetype = 2) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) 
ggsave('train_test_f1.png', width = 4, height = 4)


#plot precision and recall

prec_recall_long <- mi %>% select(contains(c('precision', 'recall', 'positives')), site) %>% 
  rename(n_positives_train = train_n_positives_train) %>% 
  pivot_longer(contains(c('precision', 'recall'))) %>% 
  separate(col = 'name', into = c('set', 'metric')) %>% 
  pivot_wider(id_cols = c('site', 'set', contains('positive')), names_from = metric) %>% 
  mutate(n_positives = case_when(set == 'train' ~ n_positives_train,
                                 set == 'test' ~ n_positives_test))
ggplot(prec_recall_long, aes(x = recall, y = precision, size = n_positives, color = set,
                             group = site)) + 
  geom_point() + geom_line(lwd = 0.5, color = 'black') +
  labs(title = 'Gage height models precision and recall')
ggsave('gage_height_test_train_prec_recall.png')
