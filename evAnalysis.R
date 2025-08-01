evAnalysis <- function(data_files) {
  # Initialize results list
  results <- list()
  
  # Generate random data for demonstration
  set.seed(Sys.time()) # Use current time as seed
  random_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    group = sample(LETTERS[1:4], 100, replace = TRUE)
  )
  
  # Create a random plot
  p <- ggplot(random_data, aes(x = x, y = y, color = group)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Random Data Visualization",
         x = "Random X Values",
         y = "Random Y Values",
         color = "Groups") +
    scale_color_brewer(palette = "Set1")
  
  # Add plot to results
  results$plot <- p
  
  # Create and add text file
  txt_file <- tempfile(pattern = "analysis_", fileext = ".txt")
  writeLines("hello world", txt_file)
  results$text_file <- list(
    path = txt_file,
    content = "hello world"
  )
  
  return(results)
}