
set_amt <- function(insured, amt_mix, group = c("gender", "age_band"), seed = 1234) {
  set.seed(seed)
  cols <- match_cols(insured, group)
  setorderv(insured, cols)
  demo <- insured[, .(count = .N), keyby = cols]
  data_list <- vector(mode = "list", length = nrow(demo))
  z <- rbindlist(
    lapply(seq_along(data_list), function(i) {
      demo_i <- demo[i]
      amt_mix_i <- amt_mix[demo_i, on = cols]
      amt_mix_i_spl <- split(amt_mix_i, by = "rn")
      sample_list <- vector(mode = "list", length = length(amt_mix_i_spl))
      for (j in seq_along(sample_list)) {
        amt_mix_i_spl_j <- amt_mix_i_spl[[j]]
        sample_list[[j]] <- random_sampling(
          x       = amt_mix_i_spl_j$amount,
          size    = demo_i$count,
          replace = TRUE,
          prob    = amt_mix_i_spl_j$prop
        )
      }
      sample_data <- do.call("cbind", sample_list)
      colnames(sample_data) <- names(amt_mix_i_spl)
      rownames(sample_data) <- insured[demo_i, on = cols]$id
      sample_data <- data.table(sample_data, keep.rownames = "id")
      data_list[[i]] <- sample_data
    }), fill = TRUE)
  return(z)
}
