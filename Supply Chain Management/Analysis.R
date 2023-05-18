#######Barriers to green supply chain management: An emerging economy context#######
#                            Mohammad Nayeem Hasan                                 #
####################################################################################


library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)

#loding main data
dat <- read.table("F:\\ResearchProject\\Client Task\\SBhai\\Table3.csv",sep=',',header=T)
str(dat)

#descriptive statistics
stat.desc(dat)


#cluster Analysis
dat
fviz_nbclust(dat, kmeans, method = "silhouette")+ theme_classic()+
  labs(title ="", x = "Number of clusters", y = "Silhouette values")

row.names(dat) <- c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10",
                             "B11","B12","B13","B14","B15")
dat
datsc <- scale(dat)

# Compute dissimilarity matrix
res.dist <- dist(datsc, method = "euclidean")

# Enhanced hierarchical clustering
res.hc <- eclust(datsc, "hclust" , k=3) # compute hclust
fviz_dend(res.hc, cex = 1, 
          main = "",
          xlab = "Barriers", ylab = "Height", sub = "",rect = TRUE)


#scatter plot
library("car")

ZXRIV <- c(1.570, 1.254, -0.485, 0.305, 1.886, -1.907, 0.147, 1.412, -2.223,
           -1.591, -0.011, -0.169, 0.305, 0.621, -1.117)

ZXSDV <- c(0.750, 1.943, -1.092, 0.161, 1.784, -1.524, -0.008, 0.409, -0.874,
           -1.544, -1.253, -0.225, 0.447, 2.006, -0.979)

dat2 <- data.frame(ZXRIV, ZXSDV)

row.names(dat2) <- c("B1","B2","B3","B4","B5","B6","B7","B8","B9","B10",
                    "B11","B12","B13","B14","B15")

library(ggplot2)
# Simple scatter plot
sp <- ggplot(dat2, aes(ZXRIV, ZXSDV, label = rownames(dat2)))+
  geom_point()
sp
# Add texts
sp + geom_text()
# Change the size of the texts
sp + geom_text(size=4)
# Change vertical and horizontal adjustement
sp +  geom_text(hjust=1.2, vjust=0.4) + 
  xlab(bquote(Z(X[RIV])))+
  ylab(bquote(Z(X[SDV])))



