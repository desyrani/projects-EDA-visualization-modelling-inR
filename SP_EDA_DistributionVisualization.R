# Q1 _ Create Pie Chart for the outcome
# Load necessary libraries
library(plotrix)

# Load the dataset
df <- read.csv("C:/Users/khali/Downloads/diabetes.csv")

# Calculate the counts for each category (Diabetes and Non-diabetes)
diabetes_counts <- table(df$Outcome)
counts <- as.numeric(diabetes_counts)
labels <- c("Non-diabetes", "Diabetes")

# Calculate the percentages
percentages <- round(100 * counts / sum(counts), 1)
pie_labels <- paste(labels, percentages, "%", sep=" ")

# Create the 3D pie-chart
pie3D(counts, labels = pie_labels, explode = 0.1, main = "Pie Chart of Diabetes vs Non-diabetes Patients")

# Add a legend
legend("topright", labels, cex = 0.8, fill = rainbow(length(counts)))




# Q2 _ SD & Variance

# Install and load necessary libraries
library(fBasics)


# Initialize an empty list to store results
results_list <- list()

# Calculate basic statistics for each column except the last one (Outcome)
for (col in names(df)[1:8]) {
  stats <- basicStats(df[[col]])
  results_list[[col]] <- data.frame(
    Attribute = col,
    Standard_Deviation = stats["Stdev", ],
    Variance = stats["Variance", ]
  )
}

# Combine all results into a single data frame
results_df <- do.call(rbind, results_list)

# Print the results
print(results_df)




# Q3 Visualization

# Install and load necessary packages
library(GGally)
library(ggplot2)

# Create the pair plot
ggpairs(df, columns = 1:8, aes(color = as.factor(Outcome)), 
        title = "Pair Plot of Diabetes Dataset")




# Q4 BOXPLOT

# Load necessary package
library(ggplot2)


# Create a boxplot for Glucose vs Outcome
boxplot(Glucose ~ Outcome, data = df, 
        xlab = "Outcome (0 = Non-diabetes, 1 = Diabetes)", 
        ylab = "Glucose",
        main = "Glucose vs Outcome",
        col = c("skyblue", "pink"))

# Create a boxplot for BloodPressure vs Outcome
boxplot(BloodPressure ~ Outcome, data = df, 
        xlab = "Outcome (0 = Non-diabetes, 1 = Diabetes)", 
        ylab = "Blood Pressure",
        main = "BloodPressure vs Outcome",
        col = c("skyblue", "pink"))

# Create a boxplot for BMI vs Outcome
boxplot(BMI ~ Outcome, data = df, 
        xlab = "Outcome (0 = Non-diabetes, 1 = Diabetes)", 
        ylab = "BMI",
        main = "BMI vs Outcome",
        col = c("skyblue", "pink"))

# Create a boxplot for DiabetesPedigreeFunction vs Outcome
boxplot(DiabetesPedigreeFunction ~ Outcome, data = df, 
        xlab = "Outcome (0 = Non-diabetes, 1 = Diabetes)", 
        ylab = "Diabetes Pedigree Function",
        main = "DiabetesPedigreeFunction vs Outcome",
        col = c("skyblue", "pink"))




# Q5 SUBSET


# Create subsets for diabetic and non-diabetic patients
diabetes_df <- subset(df, Outcome == 1)
non_diabetes_df <- subset(df, Outcome == 0)

# Check the first few rows of each subset to confirm
head(diabetes_df)
head(non_diabetes_df)





# Q6

# Diabetic Patient of Pregnancies

# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' pregnancies
p <- create_distribution_plot(diabetes_df, "Pregnancies", "Distribution of Pregnancies for Diabetic Patients")
print(p)


# Non-diabetic patient of pregnancies

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' pregnancies
p <- create_distribution_plot(non_diabetes_df, "Pregnancies", "Distribution of Pregnancies for Non-diabetic Patients")
print(p)




# Diabetic patient of glucose

# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 10, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' glucose
p_diabetic <- create_distribution_plot(diabetes_df, "Glucose", "Distribution of Glucose for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of glucose

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' glucose
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "Glucose", "Distribution of Glucose for Non-diabetic Patients")
print(p_non_diabetic)



# Diabetic patient of BloodPressure

# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 5, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' BloodPressure
p_diabetic <- create_distribution_plot(diabetes_df, "BloodPressure", "Distribution of BloodPressure for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of BloodPressure

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' BloodPressure
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "BloodPressure", "Distribution of BloodPressure for Non-diabetic Patients")
print(p_non_diabetic)



# Diabetic patient of SkinThickness
# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 2, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' SkinThickness
p_diabetic <- create_distribution_plot(diabetes_df, "SkinThickness", "Distribution of SkinThickness for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of SkinThickness

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' SkinThickness
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "SkinThickness", "Distribution of SkinThickness for Non-diabetic Patients")
print(p_non_diabetic)




# Diabetic patient of Insulin
# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 50, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' Insulin
p_diabetic <- create_distribution_plot(diabetes_df, "Insulin", "Distribution of Insulin for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of Insulin

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' Insulin
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "Insulin", "Distribution of Insulin for Non-diabetic Patients")
print(p_non_diabetic)




# Diabetic patient of BMI
# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' BMI
p_diabetic <- create_distribution_plot(diabetes_df, "BMI", "Distribution of BMI for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of BMI

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' BMI
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "BMI", "Distribution of BMI for Non-diabetic Patients")
print(p_non_diabetic)





# Diabetic patient of DiabetesPedigreeFunction
# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 0.1, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' DiabetesPedigreeFunction
p_diabetic <- create_distribution_plot(diabetes_df, "DiabetesPedigreeFunction", "Distribution of DiabetesPedigreeFunction for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of DiabetesPedigreeFunction

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' DiabetesPedigreeFunction
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "DiabetesPedigreeFunction", "Distribution of DiabetesPedigreeFunction for Non-diabetic Patients")
print(p_non_diabetic)




# Diabetic patient of Age
# Function to create distribution plot without mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal()
  
  return(p)
}


# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Plot distribution for diabetic patients' Age
p_diabetic <- create_distribution_plot(diabetes_df, "Age", "Distribution of Age for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of Age

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' Age
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "Age", "Distribution of Age for Non-diabetic Patients")
print(p_non_diabetic)





# Q7


# Install and load necessary packages
library(prettyR)

# Create subsets for diabetic and non-diabetic patients
diabetes_df <- subset(df, Outcome == 1)
non_diabetes_df <- subset(df, Outcome == 0)

# Function to create histogram with mean and median lines
create_histogram <- function(data, column_name, outcome) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  hist(data[[column_name]], main = paste("Distribution of", column_name, "for", outcome, "Patients"))
  
  # Add vertical lines for mean and median
  abline(v = mean_value, lty = 2, lwd = 2)
  abline(v = median_value, col = "blue", lwd = 2)
  
  # Print mean and median with attribute name
  print(paste("Mean", column_name, ":", round(mean_value, 2)))
  print(paste("Median", column_name, ":", round(median_value, 2)))
}

# Loop through all attributes and create histograms with mean and median lines for diabetic patients
for (attribute in names(diabetes_df)[1:8]) {
  create_histogram(diabetes_df, attribute, "Diabetic")
}

# Loop through all attributes and create histograms with mean and median lines for non-diabetic patients
for (attribute in names(non_diabetes_df)[1:8]) {
  create_histogram(non_diabetes_df, attribute, "Non-diabetic")
}






# Q8

# Diabetic Patient of Pregnancies

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' pregnancies
p <- create_distribution_plot(diabetes_df, "Pregnancies", "Distribution of Pregnancies for Diabetic Patients")
print(p)


# Non-diabetic patient of pregnancies

# Install and load necessary packages
library(ggplot2)

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for non-diabetic patients' pregnancies
p <- create_distribution_plot(non_diabetes_df, "Pregnancies", "Distribution of Pregnancies for Non-diabetic Patients")
print(p)



# Diabetic patient of glucose

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 10, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' glucose
p_diabetic <- create_distribution_plot(diabetes_df, "Glucose", "Distribution of Glucose for Diabetic Patients")
print(p_diabetic)


# non-diabetic patient of glucose

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' glucose
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "Glucose", "Distribution of Glucose for Non-diabetic Patients")
print(p_non_diabetic)


# Diabetic patient of BloodPressure

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 5, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' BloodPressure
p_diabetic <- create_distribution_plot(diabetes_df, "BloodPressure", "Distribution of BloodPressure for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of BloodPressure

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' BloodPressure
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "BloodPressure", "Distribution of BloodPressure for Non-diabetic Patients")
print(p_non_diabetic)



# Diabetic patient of SkinThickness

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 2, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' SkinThickness
p_diabetic <- create_distribution_plot(diabetes_df, "SkinThickness", "Distribution of SkinThickness for Diabetic Patients")
print(p_diabetic)



# Non-diabetic patient of SkinThickness

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' SkinThickness
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "SkinThickness", "Distribution of SkinThickness for Non-diabetic Patients")
print(p_non_diabetic)



# Diabetic patient of Insulin

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 50, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' Insulin
p_diabetic <- create_distribution_plot(diabetes_df, "Insulin", "Distribution of Insulin for Diabetic Patients")
print(p_diabetic)


# Non-diabetic patient of Insulin

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' Insulin
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "Insulin", "Distribution of Insulin for Non-diabetic Patients")
print(p_non_diabetic)



# Diabetic patient of BMI

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' BMI
p_diabetic <- create_distribution_plot(diabetes_df, "BMI", "Distribution of BMI for Diabetic Patients")
print(p_diabetic)



# Non-diabetic patient of BMI

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' BMI
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "BMI", "Distribution of BMI for Non-diabetic Patients")
print(p_non_diabetic)



# Diabetic patient of DiabetesPedigreeFunction

# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 0.1, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' DiabetesPedigreeFunction
p_diabetic <- create_distribution_plot(diabetes_df, "DiabetesPedigreeFunction", "Distribution of DiabetesPedigreeFunction for Diabetic Patients")
print(p_diabetic)




# Non-diabetic patient of DiabetesPedigreeFunction

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' DiabetesPedigreeFunction
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "DiabetesPedigreeFunction", "Distribution of DiabetesPedigreeFunction for Non-diabetic Patients")
print(p_non_diabetic)




# Diabetic patient of Age


# Install and load necessary packages
library(ggplot2)

# Create subset for diabetic patients
diabetes_df <- subset(df, Outcome == 1)

# Function to create distribution plot with mean and median lines
create_distribution_plot <- function(data, column_name, title) {
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  median_value <- median(data[[column_name]], na.rm = TRUE)
  
  p <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "yellow", alpha = 0.7, color = "black") +
    geom_vline(aes(xintercept = mean_value), color = "navy", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_value), color = "maroon", linetype = "solid", size = 1) +
    labs(title = title, x = column_name, y = "Frequency") +
    theme_minimal() +
    annotate("text", x = mean_value, y = Inf, label = paste("Mean:", round(mean_value, 2)), vjust = 2, color = "navy") +
    annotate("text", x = median_value, y = Inf, label = paste("Median:", round(median_value, 2)), vjust = 1, color = "maroon")
  
  return(p)
}

# Plot distribution for diabetic patients' Age
p_diabetic <- create_distribution_plot(diabetes_df, "Age", "Distribution of Age for Diabetic Patients")
print(p_diabetic)




# Non-diabetic patient of Age

# Create subset for non-diabetic patients
non_diabetes_df <- subset(df, Outcome == 0)

# Plot distribution for non-diabetic patients' Age
p_non_diabetic <- create_distribution_plot(non_diabetes_df, "Age", "Distribution of Age for Non-diabetic Patients")
print(p_non_diabetic)



#Q9 is explanation




# Q10

# a)
# Function to categorize BMI values into categories
categorize_bmi <- function(bmi) {
  if (bmi < 18.5) {
    return("Underweight")
  } else if (bmi >= 18.5 && bmi < 25.0) {
    return("Normal weight")
  } else if (bmi >= 25.0 && bmi < 30.0) {
    return("Overweight")
  } else if (bmi >= 30.0 && bmi < 40.0) {
    return("Obese")
  } else {
    return("Morbidly obese")
  }
}

# Apply the function to create the new column "BMI-Category"
df$BMI_Category <- sapply(df$BMI, categorize_bmi)


# b)

library(ggplot2)

# Plot distribution of patients according to BMI category
ggplot(df, aes(x = BMI_Category, fill = as.factor(Outcome))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Patients According to BMI Category",
       x = "BMI Category", y = "Count", fill = "Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "maroon"))






# Q11

# a)
# Box plot of Glucose levels for diabetic and non-diabetic patients
ggplot(df, aes(x = as.factor(Outcome), y = Glucose, fill = as.factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Glucose Levels for Diabetic and Non-diabetic Patients",
       x = "Outcome", y = "Glucose") +
  scale_fill_manual(values = c("navy", "maroon"))



# b)
# Scatter plot of Glucose vs BMI for diabetic patients
ggplot(subset(df, Outcome == 1), aes(x = BMI, y = Glucose)) +
  geom_point(color = "navy") +
  labs(title = "Glucose vs BMI for Diabetic Patients",
       x = "BMI", y = "Glucose")



# c)
# Histogram of Diabetes Pedigree Function for diabetic and non-diabetic patients
ggplot(df, aes(x = DiabetesPedigreeFunction, fill = as.factor(Outcome))) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  labs(title = "Distribution of Diabetes Pedigree Function",
       x = "Diabetes Pedigree Function", y = "Count", fill = "Outcome") +
  scale_fill_manual(values = c("navy", "maroon"))



# d)
# Bar plot of patients within each BMI category
ggplot(df, aes(x = BMI_Category, fill = as.factor(Outcome))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Patients According to BMI Category",
       x = "BMI Category", y = "Count", fill = "Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("navy", "maroon"))




