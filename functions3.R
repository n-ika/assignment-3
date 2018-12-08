# FUNCTIONS

# Permute the data
# Arguments: 
#   - the value of the observed mean difference
#   - the vectors of standard deviations to sample from
#   - number of observations:
#   either length of the table of filtered good/bad
#   observations or a number (3)
#   - adjust values: 
#       if 1 or more, will remove values below 0
#       and above 100;
#       if more than 1, will also set means
#       to 100 (for "good") and 0 (for "bad")

permute_for_h2 <- function(observed_mean_diff, 
                           both_sd,
                           n_obs_good,
                           n_obs_bad,
                           adjust_values) {
  coefficients_H2 <- rep(0, 9999)
  set.seed(29)
  for (i in 1:9999) {
    if (adjust_values > 1) {
      mn1 <- 100
      mn2 <- 0
    } else{
      mn1 <- sample(0:100, 1)
      mn2 <- mn1 - observed_mean_diff
    }
    
    sd1 <- sample(both_sd, 1)
    if (sd1 == both_sd[1]) {
      sd2 = both_sd[2]
    } else {
      sd2 = both_sd[1]
    }
    
    fake_good <- dplyr::data_frame(fake_rating = 
                                     rnorm(n = n_obs_good, 
                                           mean = mn1, 
                                           sd = sd1),
                                   fake_language = "good")
    
    fake_bad <- dplyr::data_frame(fake_rating = 
                                    rnorm(n = n_obs_bad, 
                                          mean = mn2, 
                                          sd = sd2),
                                  fake_language = "bad")
    
    H2_data <- dplyr::bind_rows(fake_good,
                                fake_bad)
    
    if (adjust_values >= 1) {
      H2_data <- H2_data %>%
        dplyr::mutate(
          similar_rating_to_observed = 
            ifelse(fake_rating < 0, 0, 
                   ifelse(fake_rating > 100,
                          100, fake_rating)))
      
      coef_permuted <- coef(lm(similar_rating_to_observed ~ 
                                 fake_language, 
                               data = H2_data))[2]
    } else {
      coef_permuted <- coef(lm(fake_rating ~ 
                                 fake_language, 
                               data = H2_data))[2]
    }
    
    coefficients_H2[i] <- coef_permuted
  }
  
  set.seed(NULL)
  return(coefficients_H2)
}


#####################################################################

# Plot the two hypothesis
# Arguments:
#   - The coefficients for H1 and H2 
#   obtained from the permutation function
#   - The xlim size for visualizing

plot_permuted_data <- function(
                      coefficients_H1,
                      coefficients_H2,
                      transparency,
                      vector_size
                      ) {
        coefs_table <- dplyr::data_frame(H1 = coefficients_H1,
                                         H2 = coefficients_H2)
        coefs_joined <- tidyr::gather(coefs_table,
                                      `H1`,
                                      `H2`,
                                      key = "Hypothesis",
                                      value = "Coef")
        
        plot_coefs_H1H2 <- ggplot2::ggplot(coefs_joined, 
                                           ggplot2::aes(
                                             x=Coef, 
                                             fill=Hypothesis)) +
                          ggplot2::geom_histogram(
                            colour = "black", 
                            position="identity",
                            bins = 80,
                            alpha = transparency) +
                          ggplot2::coord_cartesian(
                            xlim = vector_size)
        
        return(plot_coefs_H1H2)
}