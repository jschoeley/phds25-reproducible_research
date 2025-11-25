library(targets)
tar_dir({
  tar_script({
    library(targets)
    list(
      # Load the mtcars dataset.
      tar_target(mtcars_data, mtcars),
      
      # Compute the mean of the mpg column.
      tar_target(mpg_mean, mean(mtcars_data$mpg)),
      
      # Create a histogram plot of mpg using base R.
      # The plot is saved as a PNG file and the filename is returned.
      tar_target(plot_mpg, {
        png("hist_mpg.png")  # Open a PNG device.
        hist(mtcars_data$mpg,
             breaks = "Sturges",
             main = "Histogram of MPG",
             xlab = "Miles Per Gallon",
             ylab = "Frequency",
             col = "lightblue")
        dev.off()  # Close the device.
        "hist_mpg.png"  # Return the filename as the target value.
      })
    )
  }, ask = FALSE)
  
  # Run all targets defined in the pipeline.
  tar_make()
  
})