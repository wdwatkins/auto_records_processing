library(ggplot2)
mi <- remake::fetch('model_inventory')
ggplot(mi, aes(x = train_f1, y = test_f1)) + geom_point() + 
  labs(x = 'Training set F1 score (1 = perfect)', y = 'Test set F1 score (1 = perfect)', 
       title = 'Anomaly detection model\nperformance by site') + 
  geom_abline(slope = 1, linetype = 2)
ggsave('train_test_f1.png', width = 4, height = 4)
