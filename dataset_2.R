# Univariate Analysis

  # Q1 Data Overview
    
    library("faraway")
    
    #Load the dataset
    kanga_data <- kanga
    
    # Display the structure of the dataset
    str(kanga_data)
    
    # Display the number of observations and variables
    cat("Number of observations: ", nrow(kanga_data), "\n")
    cat("Number of variables: ", ncol(kanga_data), "\n")
    
    # Numerical Variables from the Dataset
    numerical_vars <- kanga_data[, c("basilar.length","occipitonasal.length","palate.length","palate.width",
                                     "nasal.length","nasal.width","squamosal.depth","lacrymal.width","zygomatic.width",
                                     "orbital.width",".rostral.width","occipital.depth","crest.width","foramina.length",
                                     "mandible.length","mandible.width","mandible.depth","ramus.height")]
    
    # Check for missing values
    sum(is.na(numerical_vars))
    
    # Replace missing values (if any) with the mean of the respective column
    for (col in colnames(numerical_vars)) {
      numerical_vars[is.na(numerical_vars[, col]), col] <- mean(numerical_vars[, col], na.rm = TRUE)
    }
    sum(is.na(numerical_vars))
    
  # Q2 Statistics Summary of a numeric variable    
    
    # Choose a numerical variable
    basilar_length <- numerical_vars$basilar.length
    
    # Calculate summary statistics and Display the results
    cat("Mean Basilar Length: ", mean(basilar_length), "\n")
    cat("Median Basilar Length: ", median(basilar_length), "\n")
    cat("Standard deviation of Basilar Length: ", sd(basilar_length), "\n")
    cat("Minimum Basilar Length: ", min(basilar_length), "\n")
    cat("Maximum Basilar Length: ", max(basilar_length), "\n")

  # Q3 Distribution Visualization of the numerical variable     
    
    # Histogram for 'Basilar Length'
    hist(basilar_length, main = "Histogram of Basilar Length", xlab = "Basilar Length", col = "lightblue", border = "black")
    
    # Boxplot for 'Basilar Length'
    boxplot(basilar_length, main = "Boxplot of Basilar Length", ylab = "Basilar Length", col = "lightgreen")
    
  # Q4 Categorical Variable Analysis
    species_count <- table(kanga_data$species)
    bp <- barplot(species_count,main = "Distribution of Species",xlab = "Species", ylab = "Frequency", 
                  col = c("pink", "lightgreen","lightblue"),border = "black",ylim = c(0, max(species_count) * 1.2))
    text(x = bp,y = species_count,label = species_count,pos = 3,cex = 1.2,col = "black")

# Bivariate Analysis
    
  # Q5 Correlation Analysis
        
    # Choose numerical variables
    basilar_length <- numerical_vars$basilar.length
    occipitonasal_length<-numerical_vars$occipitonasal.length
    palate_length<-numerical_vars$palate.length
    
    # Calculate the Pearson correlation coefficient
    correlation <- cor(basilar_length,occipitonasal_length, method = "pearson")
    
    # Display the correlation coefficient
    cat("Pearson correlation between Basilar length  and Occipitonasal length : ", correlation, "\n")
    
  # Q6 Scatter Plot Visualization
    plot(occipitonasal_length,basilar_length,
         main = "Scatter Plot of Basilar length vs Occipitonasal length",
         xlab = "Occipitonasal length", ylab = "Basilar length",
         col = "blue", pch = 19)  # Use blue color for points
    # Fit a linear regression model and add the trend line
    model <- lm(basilar_length ~ occipitonasal_length, data = numerical_vars)
    abline(model, col = "red")  # Add the trend line in red
    
  # Q7 Multiple Linear Regression
    model <- lm(basilar_length ~ occipitonasal_length + palate_length , data = numerical_vars)
    
    # Display the summary of the model
    summary(model)
    
  # Q8 Model Diagnostics8 Extract the residuals
    residuals <- residuals(model)
    # distribution of Residuals 
    hist(residuals, probability = TRUE, main = "Histogram and Density Plot of Residuals", 
         xlab = "Residuals", ylab = "Density", 
         col = "lightblue", border = "black", breaks = 20)
    lines(density(residuals), col = "red", lwd = 2)
    
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
    
    # Standardize the numerical variables data (mean = 0, sd = 1)
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
    
