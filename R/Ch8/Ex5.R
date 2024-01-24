rm(list = ls())
probs <- c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)

# Majority voting
predClass_mv <- ifelse(test = sum(probs >= 0.5) > sum(probs < 0.5),
                    yes = "Red", no = "Green")
cat("Predicted class from majority voting is:", 
    predClass_mv, "\n", sep = "")

# Average Probability
predClass_ap <- ifelse(test = mean(probs) >= 0.5,
                       yes = "Red", no = "Green")
cat("Predicted class from Average Probability is:", 
    predClass_ap, "\n", sep = "")
