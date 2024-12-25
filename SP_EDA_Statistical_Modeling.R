# BOXPLOT
boxplot (Sepal.Length ~ Species, data = iris, ylab = "Sepal length (cm)", main = "Iris measurement", boxwex = 0.5)

input <- mtcars[,c('mpg', 'cyl')]
boxplot (mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders", ylab = "Miles Per Gallon", main = "Mileage Data")

# Low level graphic functions
plot (circumference~age, pch = as.numeric (as.character(Tree)), data = Orange)
abline(lm(circumference~age, data = Orange, subset = Tree == "1"), lty = 1)
abline(lm(circumference~age, data = Orange, subset = Tree == "2"), lty = 2)
abline(lm(circumference~age, data = Orange, subset = Tree == "3"), lty = 3, lwd = 2)
abline(lm(circumference~age, data = Orange, subset = Tree == "4"), lty = 4)
abline(lm(circumference~age, data = Orange, subset = Tree == "5"), lty = 5)
legend("topleft", legend = paste("Tree", 1:5), lty = 1:5, pch = 1:5, lwd = c(1, 1, 2, 1, 1))

# Other graphics systems
library(ggplot2)
region <- names(WorldPhones51)
phones51 <- data.frame(Region = factor(region, levels= region), Telephones = WorldPhones51)
ggplot (phones51, aes(Region, Telephones)) + geom_bar(stat = "identity")


library(lattice)
phones <- data.frame(Year = as.numeric (rep(rownames(WorldPhones), 7)),
                     Region = rep(colnames(WorldPhones), each = 7),
                     Telephones = as.numeric(WorldPhones))
xyplot(Telephones~Year | Region, data = phones)


library(grid)
grid.rect()  #draw black rectangle containing original viewport
vp <- viewport (h = .4, w = .6, angle = 60)
pushViewport (vp) # Create viewport rotated 60 degrees (counter clockwise)
                  # with height 40% and width 60% of original box
grid.rect(gp = gpar(c0l = "red")) # draw red rectangle around new viewport
pushViewport(vp) # Create new viewport nested in previous viewport
                # rotated 60 degress and with height 40%
                 # and width 60% of previous viewport
grid.rect(gp = gpar(col = "blue")) # Draw blue rectangle around viewport


library (leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers (lng = 174.768, lat = -36.852, popup = "The birthplace of R")



# DESCRIPTIVE STATISTICS
library (prettyR)
head(mtcars)
mean(mtcars$mpg)
median(mtcars$mpg)
hist(mtcars$mpg)
abline(v=mean(mtcars$mpg), lty=2, lwd=2)
abline(v=median(mtcars$mpg), col="blue", lwd=2)
mode(mtcars$mpg)
sort(table(mtcars$mpg), decreasing = TRUE)


# Range
range(mtcars$mpg)
max(mtcars$mpg) - min(mtcars$mpg)

# Variance and Standard Deviation
library(fBasics)
basicStats(mtcars$mpg)



# EXPLORATORY DATA ANALYSIS

custdata <- read.csv("C:/Users/khali/Downloads/custdata.csv", header = TRUE, sep="\t")
is.data.frame(custdata)

# To spot the problem
summary(custdata)

custdata$sex <- as.factor(custdata$sex)
custdata$marital.stat <- as.factor(custdata$marital.stat)
custdata$housing.type <- as.factor(custdata$housing.type)
custdata$state.of.res <- as.factor(custdata$state.of.res)


# Visualize the data
hist(custdata$age, breaks=seq(from=0, to=150, by=5))

plot(density(custdata$age))

library(ggplot2)
ggplot(custdata) + geom_density(aes(x=income))


ggplot(custdata) + geom_density(aes(x=income)) + scale_x_log10(breaks=c(100, 1000, 10000, 100000)) + annotation_logticks (sides="bt")

# Visualizing the relationship between two variables
custdata2 <- subset(custdata, (custdata$age>0))
cor(custdata2$age, custdata2$income)

ggplot(custdata2, aes(x=age, y=income)) + geom_point() + stat_smooth(method="lm") + ylim(0, 200000)

ggplot(custdata2, aes(x=age, y=income)) + geom_point() + geom_smooth() + ylim(0, 200000)

# Managing data - Treating Missing Values
summary(custdata[is.na(custdata$houding.type), c("recent.move", "num.vehicles")])

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed), "missing", ifelse(custdata$is.employed==T, "employed", "not employed"))
summary(as.factor(custdata$is.employed.fix))

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed), "not in the active workforce", ifelse(custdata$is.employed==T, "employed", "not employed"))
summary(as.factor(custdata$is.employed.fix))


x <- read.csv("C:/Users/khali/Downloads/Statistical Prog Files/income.txt")
summary(x)

# Replace the missing values with mean income
meanIncome <- mean(x$income, na.rm=T)
x$income.fix <- ifelse(is.na(x$income), meanIncome, x$income)
summary (x$income.fix)


# Select some income ranges of interest
breaks <- c(0, 10000, 50000, 100000, 250000, 1000000) 

# Cut the data into income ranges. The include.lowest=T ensures the zero income data is included in the lowest income range category
income.groups <- cut(x$income, breaks=breaks, include.lowest=T)

# Each row has been categorized into a range. NAs are preserved
summary(income.groups)

# Preserving the category names
income.groups <- as.character (income.groups)

# Add the "no income" category to replace the NAs
income.groups <- ifelse(is.na(income.groups), "no income", income.groups)

# View the summary of income.groups with "no income" category
summary (as.factor(income.groups))

# Combine income.groups to x
x <- cbind (x, income.groups)


# Converting continuous variables to discrete variables 
custdata$income.lt.20K <- custdata$income < 20000
summary (custdata$income.lt.20K)

# Converting continuous age variable to a discrete variable
brks <- c(0, 25, 65, Inf)
custdata$age.range <- cut(custdata$age, breaks=brks, include.lowest=T)
summary(custdata$age.range)


# Normalization and Rescaling - Log Transformation
data <- c(1200, 34567, 3456, 12, 3456, 0985, 1211)
summary(data)
log_scale <- log(as.data.frame(data))

data
log_scale

# Normalization & Rescaling - Min Max
# Min-Max using user defined function
myData <- data.frame(
  m <- c(300000, 213000, 180000, 13200, 3200, 54000, 432000, 87000, 320, 120300),
  n <- c(23, 45, 65, 32, 43, 59, 99, 76, 4, 34)
)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min (x))
}

m.norm <- min_max_norm(myData$m)
n.norm <- min_max_norm(myData$n)

m.norm
n.norm


# Min_Max using caret package
myData <- data.frame(
  m <- c(300000, 213000, 180000, 13200, 3200, 54000, 432000, 87000, 320, 120300),
  n <- c(23, 45, 65, 32, 43, 59, 99, 76, 4, 34)
)

library (caret)
preproc <- preProcess(myData, method=c("range"))
myData.norm <- predict(preproc, myData)


# Z-score (standard) scale
# Z-score using user defined function
myData <- data.frame(
  m <- c(300000, 213000, 180000, 13200, 3200, 54000, 432000, 87000, 320, 120300),
  n <- c(23, 45, 65, 32, 43, 59, 99, 76, 4, 34)
)

z_score_norm <- function (x) {
  (x - mean (x)) / sd(x)
}

m.norm <- z_score_norm(myData$m)
n.norm <- z_score_norm(myData$n)

m.norm
n.norm


# Z-score using scale() function & Caret package
myData <- data.frame(
  m <- c(300000, 213000, 180000, 13200, 3200, 54000, 432000, 87000, 320, 120300),
  n <- c(23, 45, 65, 32, 43, 59, 99, 76, 4, 34)
)

m.norm <- scale(myData$m, center=TRUE, scale=TRUE)
n.norm <- scale(myData$n, center=TRUE, scale=TRUE)

library (caret)
preproc <- preProcess(myData, method=c("range"))
myData.norm <- predict(preproc, myData)



