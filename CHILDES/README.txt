install.packages("tidyverse") # install this package if you don't have already

source("PURSUIT_CHILDES.R") # No other dependencies

result <- meta_aggregate(10) # Save results of 10 iterations

eval_orig_algo(result) # Return metrics

# A tibble: 10 x 3
   Precision Recall    F1
       <dbl>  <dbl> <dbl>
 1     0.294  0.294 0.294
 2     0.333  0.353 0.343
 3     0.297  0.324 0.310
 4     0.275  0.324 0.297
 5     0.268  0.324 0.293
 6     0.256  0.294 0.274
 7     0.282  0.324 0.301
 8     0.25   0.294 0.270
 9     0.3    0.353 0.324
10     0.314  0.324 0.319


# All in one go

summarize_all(eval_orig_algo(meta_aggregate(50)), mean)

# A tibble: 1 x 3
  Precision Recall    F1
      <dbl>  <dbl> <dbl>
1     0.285  0.316 0.299
