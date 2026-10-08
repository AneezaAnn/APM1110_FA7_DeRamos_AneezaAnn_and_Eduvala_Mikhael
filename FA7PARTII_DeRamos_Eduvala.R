library(readxl)


data <- read_excel("College Student Height .xlsx")


print(data)
height <- data$Height
height <- na.omit(height)

n <- length(height)


mean_height <- mean(height)


sd_height <- sd(height)

cat("Number of students:", n, "\n")
cat("Mean height:", mean_height, "\n")
cat("Standard deviation:", sd_height, "\n")





number_of_classes <- ceiling(1 + 3.322 * log10(n))


breaks <- seq(
  floor(min(height)),
  ceiling(max(height)),
  length.out = number_of_classes + 1
)


frequency_table <- table(cut(height,breaks = breaks,include.lowest = TRUE,right = FALSE))

frequency_table <- as.data.frame(frequency_table)

colnames(frequency_table) <- c("Height Interval", "Frequency")


frequency_table$Percentage <- (frequency_table$Frequency / n) * 100


print(frequency_table)


hist(
  height,
  breaks = "Sturges",
  main = "Frequency Distribution of College Student Heights",
  xlab = "Height (cm)",
  ylab = "Frequency",
  col = "lightblue",
  border = "black",
  yaxt = "n"
)


max_frequency <- max(hist(height, breaks = "Sturges", plot = FALSE)$counts)


axis(
  2,
  at = 0:max_frequency,
  labels = 0:max_frequency
)



normal_table <- data.frame(
  Range = c(
    "Below μ - 3σ",
    "μ - 3σ to μ - 2σ",
    "μ - 2σ to μ - 1σ",
    "μ - 1σ to μ + 1σ",
    "μ + 1σ to μ + 2σ",
    "μ + 2σ to μ + 3σ",
    "Above μ + 3σ"
  ),
  
  Lower = c(
    -Inf,
    mean_height - 3 * sd_height,
    mean_height - 2 * sd_height,
    mean_height - sd_height,
    mean_height + sd_height,
    mean_height + 2 * sd_height,
    mean_height + 3 * sd_height
  ),
  
  Upper = c(
    mean_height - 3 * sd_height,
    mean_height - 2 * sd_height,
    mean_height - sd_height,
    mean_height + sd_height,
    mean_height + 2 * sd_height,
    mean_height + 3 * sd_height,
    Inf
  )
)


normal_table$Frequency <- mapply(
  function(lower, upper) {
    sum(height >= lower & height < upper)
  },
  normal_table$Lower,
  normal_table$Upper
)


normal_table$Percentage <-
  (normal_table$Frequency / n) * 100

print(normal_table)





within_1sd <- sum(
  abs(height - mean_height) <= sd_height
)

within_2sd <- sum(
  abs(height - mean_height) <= 2 * sd_height
)

within_3sd <- sum(
  abs(height - mean_height) <= 3 * sd_height
)

percentage_1sd <- (within_1sd / n) * 100
percentage_2sd <- (within_2sd / n) * 100
percentage_3sd <- (within_3sd / n) * 100



cat("Within 1σ:", within_1sd, "students =",
    round(percentage_1sd, 2), "%\n")

cat("Within 2σ:", within_2sd, "students =",
    round(percentage_2sd, 2), "%\n")

cat("Within 3σ:", within_3sd, "students =",
    round(percentage_3sd, 2), "%\n")




cat("Within 1σ: 68.27%\n")
cat("Within 2σ: 95.45%\n")
cat("Within 3σ: 99.73%\n")


hist(
  height,
  probability = TRUE,
  breaks = "Sturges",
  main = "Height Distribution of College Students",
  xlab = "Height",
  ylab = "Density"
)


curve(
  dnorm(
    x,
    mean = mean_height,
    sd = sd_height
  ),
  col = "red",
  lwd = 2,
  add = TRUE
)


abline(
  v = mean_height,
  col = "blue",
  lwd = 2,
  lty = 2
)

abline(
  v = c(
    mean_height - sd_height,
    mean_height + sd_height,
    mean_height - 2 * sd_height,
    mean_height + 2 * sd_height,
    mean_height - 3 * sd_height,
    mean_height + 3 * sd_height
  ),
  lty = 3
)

legend(
  "topright",
  legend = c(
    "Normal Curve",
    "Mean"
  ),
  col = c("red", "blue"),
  lwd = 2
)

