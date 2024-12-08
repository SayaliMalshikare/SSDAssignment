# Univariate Analysis

  # Q1 Data Overview

    #Load the dataset
    car_data <- mtcars
    # Display the structure of the dataset
    str(car_data)
    
    # Display the number of observations and variables
    cat("Number of observations: ", nrow(car_data), "\n")
    cat("Number of variables: ", ncol(car_data), "\n")
  
  # Q2 Statistics Summary of a numeric variable  
    
    # Choose a numerical variable 'disp'
    disp <- car_data$disp
    
    # Calculate summary statistics
    mean_disp <- mean(disp)
    median_disp <- median(disp)
    sd_disp <- sd(disp)
    min_disp <- min(disp)
    max_disp <- max(disp)
    
    # Display the results
    cat("Mean Displacement: ", mean_disp, "\n")
    cat("Median Displacement: ", median_disp, "\n")
    cat("Standard deviation of Displacement: ", sd_disp, "\n")
    cat("Minimum Displacement: ", min_disp, "\n")
    cat("Maximum Displacement: ", max_disp, "\n")
 
  # Q3 Distribution Visualization of the numerical variable     
  
      # Histogram for 'Displacement'
    hist(disp, main = "Histogram of Displacement", xlab = "Displacement", col = "lightblue", border = "black")
    
    # Boxplot for 'Displacement'
    boxplot(disp, main = "Boxplot of Displacement", ylab = "Displacement", col = "lightgreen")
    
  # Q4 Categorical Variable Analysis
    vs_count <- table(car_data$vs)
    bp <- barplot(vs_count,main = "Distribution of Engine Types",xlab = "Engine type", 
                  ylab = "Frequency",col = c("pink", "lightblue"),border = "black", 
                  ylim = c(0, max(vs_count) * 1.2),names.arg = c("V-shaped", "Straight"))
    text(x = bp,y = vs_count,label = vs_count,pos = 3,cex = 1.2,col = "black")
 
# Bivariate Analysis
    
  # Q5 Correlation Analysis   
  
    # Choose two numerical variables
    mpg <- car_data$mpg
    hp <- car_data$hp
    
    # Calculate the Pearson correlation coefficient
    correlation <- cor(mpg,hp, method = "pearson")
    
    # Display the correlation coefficient
    cat("Pearson correlation between Miles / gallon  and Gross Horsepower : ", correlation, "\n")
    
  # Q6 Scatter Plot Visualization
    plot(car_data$hp, car_data$mpg,
         main = "Scatter Plot of Miles / gallon  vs Gross Horsepower",
         xlab = "Gross Horsepower", ylab = "Miles / gallon",
         col = "blue", pch = 19)  # Use blue color for points
    # Fit a linear regression model and add the trend line
    model <- lm(mpg ~ hp, data = car_data)
    abline(model, col = "red")  # Add the trend line in red
    
  # Q7 Multiple Linear Regression 
    model <- lm(mpg ~ hp + wt, data = car_data)
    
    # Display the summary of the model
    summary(model)
    
  # Q8 EModel Diagnostics
    residuals <- residuals(model)
    # distribution of Residuals 
    # Plot histogram of residuals
    hist(residuals, probability = TRUE, main = "Histogram and Density Plot of Residuals", 
         xlab = "Residuals", ylab = "Density", 
         col = "lightblue", border = "black", breaks = 20,ylim=c(0,0.3))
    # Add density plot on top of the histogram
    lines(density(residuals), col = "red", lwd = 2)
    
    # Set up a 2x2 grid for plots
    par(mfrow = c(1, 2))  # Adjust to show two plots in one row
    # 1. Residuals vs Fitted plot (Check for homoscedasticity)
    plot(model, which = 1, main = "Residuals vs Fitted", 
         col = "blue", pch = 19)
    abline(h = 0, col = "red", lty = 2)  # Add a horizontal red dashed line at 0
    # 2. Q-Q plot (Check for normality of residuals)
    qqnorm(residuals(model), main = "Q-Q Plot of Residuals")
    qqline(residuals(model), col = "red", lwd = 2)  # Add reference line
    par(mfrow = c(1, 1))

# Advance Analysis
    
  # Q9 Principle Component Analysis 
    
    # Select the relevant numerical columns for PCA
    numerical_vars <- car_data[, c("mpg","disp","hp","drat","wt","qsec")]
    # Check for missing values
    sum(is.na(numerical_vars))
    # Replace missing values (if any) with the mean of the respective column
    for (col in colnames(numerical_vars)) {
      numerical_vars[is.na(numerical_vars[, col]), col] <- mean(numerical_vars[, col], na.rm = TRUE)
    }
    # Standardize the data (mean = 0, sd = 1)
    scaled_data <- scale(numerical_vars)
    
    # Perform PCA
    pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
    
    # Print the summary of PCA to see explained variance
    summary(pca_result)
    
    # Plot the proportion of variance explained by each principal component
    # Extract the proportion of variance explained by each principal component
    explained_variance <- summary(pca_result)$importance[2, ]  # Second row corresponds to proportion of variance
    
    # Plot the proportion of variance explained
    plot(explained_variance, 
         type = "o",             # "o" stands for both points and lines
         col = "blue",           # Line color
         pch = 16,               # Point type (filled circle)
         xlab = "Principal Components", 
         ylab = "Proportion of Variance Explained", 
         main = "Proportion of Variance Explained by Each PC")

  # Q10 PCA Interpretation 
      
    # Biplot to visualize PCA results
    biplot(pca_result, main = "PCA Biplot", col = c("blue", "red"))
    
    # Print the loadings of the first two principal components
    loadings <- pca_result$rotation
    print(loadings[, 1:2])  # Loadings for the first two principal components
    
    # Optionally, visualize the contribution of each variable to the first two PCs using a plot
    # This can be useful to understand how each variable is associated with PC1 and PC2
    loadings_pc1 <- loadings[, 1]
    loadings_pc2 <- loadings[, 2]
    
    # Create a plot of loadings for the first two components
    par(mfrow = c(1, 2))  # Set up a 1x2 layout for two plots
    barplot(loadings_pc1, main = "Loadings for PC1", col = "lightblue", las = 2, cex.names = 0.7)
    barplot(loadings_pc2, main = "Loadings for PC2", col = "lightgreen", las = 2, cex.names = 0.7)
    par(mfrow = c(1, 1))
    
    # Correlation plot
    
    # Load the necessary library
    library(corrplot)
    
    # Compute the correlation matrix
    cor_matrix <- cor(scaled_data)
    
    # Visualize the correlation matrix using corrplot
    corrplot(cor_matrix, method = "circle", type = "upper", 
             col = colorRampPalette(c("blue", "white", "red"))(200), 
             tl.cex = 0.8, # Text label size
             number.cex = 0.7, # Correlation value size
             addCoef.col = "black", # Add correlation values
             diag = FALSE)  # Don't display the diagonal (1's)

    