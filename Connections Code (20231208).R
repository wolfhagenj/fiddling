
sim_results <- (sapply(1:200, function(y)
  #run the simulation 200 times to get a sense of variability
  mean(sapply(1:1e4, function(x)
    #take 10000 samples as a single simulation
    max(table(sample(rep(c("A", "B", "C", "D"), each = 4), 4, replace = F))) == 3))))
    #is the maximum number of any category in the sample 3?
sim_data <- data.frame(Iteration = 1:200, Rate = sim_results)

#visualize
ggplot(sim_data) +
  geom_histogram(aes(x = Rate)) +
  geom_segment(aes(x = median(Rate), xend = median(Rate), y = -1, yend = 20.5), linetype = "dashed") + 
  annotate("text", x = median(sim_data$Rate), y = 21, label = paste0("Median Rate = ", 100 * round(median(sim_data$Rate), 4), "%")) +
  coord_cartesian(ylim = c(-1, 22), xlim = c(0.095, 0.117), expand = F) +
  labs(title = "Histogram of Simulated Random Connections Guesses", x = "Rate of 'One Away...' Message (10,000 Guesses)", y = "Frequency") +
  theme_classic() + theme(legend.text = element_text(size = 10), legend.position = "bottom", legend.justification = c("center"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6), axis.text.x = element_text(), axis.title = element_text(size = 16))

