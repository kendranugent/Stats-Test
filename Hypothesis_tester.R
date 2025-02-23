library(readr)
library(ggplot2)

# Function to perform One-Sample t-Test
perform_one_sample_t_test <- function(data) {
    var_name <- readline("Enter the column name for the test variable: ")
    hypothesized_mean <- as.numeric(readline("Enter the hypothesized mean: "))
    
    if (!var_name %in% names(data)) {
        stop("Error: Variable not found in dataset.")
    }
    
    test_result <- t.test(data[[var_name]], mu = hypothesized_mean)
    print(test_result)
}

# Function to perform Two-Sample t-Test
perform_two_sample_t_test <- function(data) {
    var1 <- readline("Enter the first variable: ")
    var2 <- readline("Enter the second variable: ")
    
    if (!(var1 %in% names(data) && var2 %in% names(data))) {
        stop("Error: One or both variables not found in dataset.")
    }
    
    test_result <- t.test(data[[var1]], data[[var2]])
    print(test_result)
}

# Function to perform Chi-Square Test
perform_chi_square_test <- function(data) {
    var1 <- readline("Enter the first categorical variable: ")
    var2 <- readline("Enter the second categorical variable: ")
    
    if (!(var1 %in% names(data) && var2 %in% names(data))) {
        stop("Error: One or both variables not found in dataset.")
    }
    
    contingency_table <- table(data[[var1]], data[[var2]])
    test_result <- chisq.test(contingency_table)
    print(test_result)
}

# Function to perform ANOVA Test
perform_anova_test <- function(data) {
    dependent_var <- readline("Enter the dependent variable column name: ")
    independent_var <- readline("Enter the independent variable column name: ")
    
    if (!(dependent_var %in% names(data) && independent_var %in% names(data))) {
        stop("Error: One or both variables not found in dataset.")
    }
    
    if (!is.factor(data[[independent_var]]) && !is.character(data[[independent_var]])) {
        stop("Error: Independent variable for ANOVA must be categorical.")
    }
    
    anova_model <- aov(as.formula(paste(dependent_var, "~ as.factor(", independent_var, ")")), data = data)
    print(summary(anova_model))
}

# Main function to load data and execute tests
run_statistical_tests <- function() {
    load_csv <- readline("Would you like to load a CSV file? (yes/no): ")
    if (tolower(load_csv) == "yes") {
        file_path <- readline("Enter the full path of the CSV file: ")
        data <- read_csv(file_path)
        print("CSV file loaded successfully!")
        print(head(data))
    } else {
        stop("No CSV file loaded. Exiting...")
    }
    
    cat("\nChoose a statistical test:\n")
    cat("1. One-Sample t-Test\n")
    cat("2. Two-Sample t-Test\n")
    cat("3. Chi-Square Test\n")
    cat("4. ANOVA Test\n")
    
    choice <- as.integer(readline("Enter choice (1-4): "))
    
    if (choice == 1) {
        perform_one_sample_t_test(data)
    } else if (choice == 2) {
        perform_two_sample_t_test(data)
    } else if (choice == 3) {
        perform_chi_square_test(data)
    } else if (choice == 4) {
        perform_anova_test(data)
    } else {
        cat("Invalid choice. Exiting...\n")
    }
}

# Run the program
run_statistical_tests()