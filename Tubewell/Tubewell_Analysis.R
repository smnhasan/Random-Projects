# improved correlation matrix
library(corrplot)

setwd('E:\\ResearchProject\\Jakariya')


dat <- read.csv("Tubewell_Project_concise.csv")


library(Hmisc)
res <- rcorr(as.matrix(dat)) # rcorr() accepts matrices only


corrplot(cor(dat),
         method = "number",
         type = "upper" # show only upper side
)



library(corrplot)
library(RColorBrewer)
M <-cor(dat)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


################Gene#############

ggplot(data = msleep, aes(x = sleep_total)) +
  geom_boxplot()

p <- ggplot(data = dat, aes(x = "", y = Depth)) + geom_boxplot(fill = 1,          
                                                               alpha = 0.5,        
                                                               color = 1,          
                                                               outlier.colour = 1) +labs(y="Feet", x = "Depth")
p

p <- ggplot(data = dat, aes(x = "", y = Arsenic)) + geom_boxplot(fill = 2,          
                                                               alpha = 0.5,        
                                                               color = 1,          
                                                               outlier.colour = 2) +labs(y="mg/L", x = "Arsenic")
p

p <- ggplot(data = dat, aes(x = "", y = Manganese)) + geom_boxplot(fill = 3,          
                                                                 alpha = 0.5,        
                                                                 color = 1,          
                                                                 outlier.colour = 3) +labs(y="mg/L", x = "Manganese")
p

p <- ggplot(data = dat, aes(x = "", y = Iron)) + geom_boxplot(fill = 4,          
                                                                   alpha = 0.5,        
                                                                   color = 1,          
                                                                   outlier.colour = 4) +labs(y="mg/L", x = "Iron")
p


library(PerformanceAnalytics)

chart.Correlation(dat, histogram = TRUE, method = "pearson")



fit <- lm(dat$Arsenic ~., data = dat)
summary(fit)

fit <- lm(dat$Iron ~., data = dat)
summary(fit)

fit <- lm(dat$Manganese ~., data = dat)
summary(fit)
