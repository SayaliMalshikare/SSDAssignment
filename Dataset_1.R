# Univariate Analysis

  # Q1 Data Overview
    
    library(ggplot2)
    # Load the dataset
    diamond_data <- diamonds
    # Display the structure of the dataset
    str(diamond_data)
    # Display the number of observations and variables
    cat("Number of observations: ", nrow(diamond_data), "\n")
    cat("Number of variables: ", ncol(diamond_data), "\n")
  
  # Q2 Statistics Summary of a numeric variable  
    
    # Choose a numerical variable 'price'
    price <- diamond_data$price
    
    # Calculate summary statistics
    mean_price <- mean(price)
    median_price <- median(price)
    sd_price <- sd(price)
    min_price <- min(price)
    max_price <- max(price)
    
    # Display the results
    cat("Mean price: ", mean_price, "\n")
    cat("Median price: ", median_price, "\n")
    cat("Standard deviation of price: ", sd_price, "\n")
    cat("Minimum price: ", min_price, "\n")
    cat("Maximum price: ", max_price, "\n")
  
  # Q3 Distribution Visualization of the numerical variable      
    
    # Histogram for 'price'
    hist(price, main = "Histogram of Diamond Prices", xlab = "Price", col = "lightblue", border = "black")
    
    # Boxplot for 'price'
    boxplot(price, main = "Boxplot of Diamond Prices", ylab = "Price", col = "lightgreen")
  
  # Q4 Categorical Variable Analysis  
    # Bar plot for the categorical variable 'clarity'
      ggplot(diamond_data, aes(x = clarity)) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = "Distribution of Diamond clarity", x = "Clarity", y = "Count")

# Bivariate Analysis
      
  # Q5 Correlation Analysis    
      
    # Choose two numerical variables: carat and price
    carat <- diamond_data$carat
    price <- diamond_data$price
    
    # Calculate the Pearson correlation coefficient
    correlation <- cor(carat, price, method = "pearson")
    
    # Display the correlation coefficient
    cat("Pearson correlation between carat and price: ", correlation, "\n")
  
  # Q6 Scatter Plot Visualization
      
    # Create a scatter plot with a trend line
    ggplot(diamond_data, aes(x = carat, y = price)) +
      geom_point(color = "blue", alpha = 0.5) +  # Scatter plot
      geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Trend line (linear regression)
      labs(title = "Scatter Plot of Carat vs Price", x = "Carat", y = "Price") +
      theme_minimal()
    
  # Q7 Multiple Linear Regression   
    model <- lm(price ~ carat + depth, data = diamond_data)
    
    # Display the summary of the regression model
    summary(model)
  
  # Q8 Model Diagnostics
    residuals <- model$residuals
    fitted_values <- model$fitted.values
    # distribution of Residuals 
    # Plot histogram of residuals
    hist(residuals, probability = TRUE, main = "Histogram and Density Plot of Residuals", 
         xlab = "Residuals", ylab = "Density", 
         col = "lightblue", border = "black", breaks = 20,ylim=c(0,0.0005))
    # Add density plot on top of the histogram
    lines(density(residuals), col = "red", lwd = 2)
    
    # Set up a 2x2 grid for plots
    par(mfrow = c(1, 2))  # Adjust to show two plots in one row
    # 1. Plot residuals vs fitted values to check for homoscedasticity
    plot(fitted_values, residuals, 
         main = "Residuals vs Fitted Values", 
         xlab = "Fitted Values", 
         ylab = "Residuals",
         pch = 20, col = "blue")
    abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0
    
    # 2. Q-Q plot to check for normality of residuals
    qqnorm(residuals, main = "Q-Q Plot of Residuals", col = "blue")
    qqline(residuals, col = "red", lwd = 2)  # Add a reference line
    par(mfrow = c(1, 1))
    
# Advance Analysis
    
  # Q9 Principle Component Analysis 

    pca_data <- diamond_data[, c("carat", "depth", "table", "price", "x", "y", "z")]
    # Standardize the data (mean = 0, sd = 1)
    scaled_data <- scale(pca_data)
    
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
    
    
    
