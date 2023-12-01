install.packages("ggplot2")
library(ggplot2)

## read data
data <- read.csv("question-5-data/Cui_etal2014.csv")

# number of rows and columns
rows <- nrow(data)
columns <- ncol(data)
rows #33 rows
columns #13 columns

## logistic transformation and linear model
log_genome_length <- log(data$Genome.length..kb.)
log_virion_volume <- log(data$Virion.volume..nm.nm.nm.)
linear_model <- lm(log_virion_volume ~ log_genome_length) #linear model
summary(linear_model)

# alpha and beta
alpha <- coef(linear_model)["log_genome_length"] # alpha is the coefficient of log_genome_length
alpha 
beta <- exp(coef(linear_model)["(Intercept)"]) # scaling factor (beta) is e to the power of the intercept. 
beta 

## reproducing the plot
ggplot(data, aes(x = log_genome_length, y = log_virion_volume)) +
  geom_point() + #scatter plot with data points
  geom_smooth(method = "lm", se = TRUE) +  #draw a linear regression line with the confidence interval
  labs(x = "log [Genome Length (kb)]", y = "log [Virion Volume (nm3)]") +
  theme_bw()

## estimating the volume of a 300 kb dsDNA virus
volume <- beta*(300^alpha)
volume #unit = nm^3

## save a list of the packages used
sink(file = "package-versions.txt")
sessionInfo()
sink()
