#Simulation Code for the December 8, 2023 Fiddler on the Proof Question
####Puzzle prompt####
#From Bart Wright comes a puzzle about a new game from The New York Times, Connections:
#In Connections, there are 16 words that must be arranged by you, the player, into four distinct sets of four words each, where the words in each set have some common property. Each turn, you must select four remaining words and press the “Submit” button. If they indeed represent one of the four sets, they are removed and you must try to find another set among the remaining words. The game ends when you have correctly identified all four sets.
#Importantly, the game also gives you hints. Anytime your four selected words include exactly three from a correct set, you are told that your selection is “One away…” from a correct set, meaning just one of your four choices was incorrect.
#When first presented with all 16 words, suppose you pick four words at random. What is your probability of seeing the “One away…” message?

####Solution####
#Simulate the probability of getting a "One away..." message (3 of one type, 1 of another) in Connections by selecting randomly
#Do two simulations: one doing the actual choosing, and the other running multiple simulations to assess variability
#Inner simulation: choose four random values from the vector, then determine whether the chosen sample contains 3 of one kind (run this simulation 10000 to produce an estimate)
#Outer simulation: run the simulation 200 times to produce 200 estimates
#Then visualize with a histogram and report the median simulation result value
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
