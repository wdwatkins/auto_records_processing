library(ggplot2)
mi <- remake::fetch('model_inventory')
ggplot(mi, aes(x = train_f1, y = test_f1, size = train_n_positives_train + n_positives_test)) + 
  geom_point() +
  #geom_label() +
  labs(x = 'Training set F1 score (1 = perfect)', y = 'Test set F1 score (1 = perfect)', 
       title = 'Temperature anomaly detection\nmodel performance by site',
       size = 'total number of anomalies') + 
  geom_abline(slope = 1, linetype = 2) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) 
ggsave('train_test_f1.png', width = 4, height = 4)
