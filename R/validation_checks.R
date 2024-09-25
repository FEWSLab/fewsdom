#' Validate Tea Absorbance
#'
#'
#'
#' @param abs
#'
#' @return
#' @export
#'
#' @examples
validate_tea_absorbance <- function(abs_df,
                                    model = "data/tea_absorbance_model.rds",
                                    fail_threshold = 0.15) {

    # Pull out names of tea standard samples
    tea_std_lgl <- grepl("tea",
                         names(abs_df),
                         ignore.case = TRUE)

    # Check there is a Tea standard somewhere in the absorbance data.frame
    stopifnot(any(tea_std_lgl))

    # Load in tea absorbance model data.frame
    if(file_ext(model) == "csv") {
      model_abs <- read.csv(model)
    } else if (file_ext(model) == "rds") {
      model_abs <- readRDS(model)
    } else {
      stop("No valid tea absorbance model found")
    }


    # Pull out the wavelength vector
    wavelength <- abs_df$wavelength
    # Pull out the columns with tea standards
    .tea_abs <- subset(abs_df,
                       select = tea_std_lgl)
    tea_names <- colnames(.tea_abs)
    tea_abs <- cbind(wavelength,
                     .tea_abs)

    # Join them together just to make sure the wavelengths are right (probably an unnecessary step)
    test_dataset <- merge(model_abs,
                          tea_abs,
                          by = "wavelength")
    # Define the test lower and upper bounds
    lowbound <- test_dataset$sdmin_3x
    hibound <- test_dataset$sdmax_3x

    # Loop through
    for (sample in tea_names) {

      sample_abs <- test_dataset[, sample]
      test_low <- (sample_abs - lowbound) > 0 # FALSE if lower than low boundary
      test_hi <- (sample_abs - hibound) < 0   # FALSE if higher than high boundary
      test_pass <- test_low & test_hi
      test_dataset[, sample] <- test_pass

      # Throw a warning for any test passes that meet the following conditions
      pct_inside_interval <- sum(test_pass) / length(test_pass)
      if (pct_inside_interval == 1) {
        next
      } else if(pct_inside_interval < (1 - fail_threshold)) {

        # Plot the tea absorbance
        plot_absorbance_error(model_abs,
                              sample_abs,
                              sample,
                              condition = "ERROR")

        stop("ERROR: ",
             sample,
             " was outside the tea absorbance testing threshold.\n",
             "Manually check the absorbances to look for Aqualog run errors\n",
             "See plot for confirmation.")
      } else {

        # Plot the Tea absorbance
        plot_absorbance_error(model_abs,
                              sample_abs,
                              sample,
                              condition = "WARNING")

        warning("Warning: ",
                sample,
                " was borderline outside the absorbance testing threshold.")
      }
    }
    return(test_dataset)

}



# TODO Function to calculate generic absorbance curve from a bunch of given "good" data
# Max good absorbance at 239 nm is 0.2166726
# Min good absorbance at 239 nm is 0.1430418
# Calculate the mean and standard deviation for each wavelength
save_tea_absorbance_model <- function(path_to_abs,
                                      save_path,
                                      sd_multiplier = 3,
                                      overwrite = FALSE) {
  # NOTE: There is a model containing the good data from 2022-08 to 2024-08 saved
  # withing the package. DO NOT use this function to update the package. Instead,
  # save the updated model data to T: Drive in:
  # T:/Aqualog_Data/0_Validation/absorbance/...
  # Load the "good" absorbance data. These will have to be selected by hand.
  tea_absorbance <- absorbance_read(path_to_abs)

  # Drop rows with all NA
  tea_absorbance <- tea_absorbance[tea_absorbance$wavelength <= 791,]

  # Convert data to long for easy summarizing
  long_data <- tidyr::pivot_longer(tea_absorbance,
                                   cols = !matches("wavelength"),
                                   names_to = "sample",
                                   values_to = "absorbance")

  tea_absorbance_model <- dplyr::group_by(good_data_long,
                              wavelength) %>%
  dplyr::summarize(mean_abs_by_wavelength = mean(absorbance),
                   sd_abs_by_wavelength = sd(absorbance)) %>%
  dplyr::mutate(sdmin_mult = mean_abs_by_wavelength - sd_multiplier * sd_abs_by_wavelength,
                sdmax_mult = mean_abs_by_wavelength + sd_multiplier * sd_abs_by_wavelength)

  # Plot good absorbance data with mean and SD ribbon
  good_abs_plot <- ggplot2::ggplot(data = good_data_long) +
    ggplot2::geom_line(aes(x = wavelength,
                  y = absorbance,
                  color = sample),
              alpha = 0.1) +
    ggplot2::geom_line(data = tea_absorbance_model,
              aes(x = wavelength,
                  y = mean_abs_by_wavelength),
              linewidth = 1,
              linetype = "dash",
              color = "black") +
    ggplot2::geom_ribbon(data = tea_absorbance_model,
                aes(x = wavelength,
                    ymin = sdmin_mult,
                    ymax = sdmax_mult),
                alpha = 0.2) +
    ggplot2::theme(legend.position = "none")

  plot(good_abs_plot)

  # Save the good model of tea absorbance as an rds file in data folder

  saveRDS(tea_absorbance_model,
          file = )



}



# TODO Function to validate EEM data for tea standards vs. average of all other "good" tea standards

# TODO Function to calculate generic EEM from a bunch of given "good" data
