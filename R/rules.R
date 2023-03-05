
apply_rules <- function(rule_info, df) {
  dm <- merge(df, rule_info, by = c("kcd"), all.x = TRUE, allow.cartesian = TRUE)
  dm[!is.na(kcd) & age >= age_min & age <= age_max & hos >= hos_min & hos <= hos_max & sur >= sur_min & sur <= sur_max & elp > elp_min,
     order_result := result]
  dm[!is.na(kcd) & is.na(order), `:=`(order = 0, order_result = "na")]
  dm[!is.na(kcd) & is.na(order_result), `:=`(order_result = "dec")]
  do <- dm[!is.na(order_result), .(order = min(order)), .(id, kcd)]
  dm[do, on = .(id, kcd, order), interim_result := order_result]
  dm[is.na(kcd), `:=`(order = 0, interim_result = "std")]
  z <- dm[!is.na(interim_result)]
  z[, final_result := sort_code(order_result), .(id)]
  z[final_result == "", final_result := "std"]
  rm_cols(z, .(result, order_result))
  setorder(z, id, final_result)
  z[grepl("dec", final_result, ignore.case = TRUE),
    decision := "dec"]
  z[is.na(decision) & grepl("na|uwer", final_result, ignore.case = TRUE),
    decision := "uwer"]
  z[is.na(decision) & grepl("excl", final_result, ignore.case = TRUE),
    decision := pull_code_all("excl[0-9]\\([0-9\\-\\|\\[\\]]+\\)", final_result,
                              collapse = ",", ignore.case = TRUE)]
  set(z, i = which(is.na(z$decision)), j = "decision", value = "std")
  set(z, j = "decision", value = toupper(z$decision))
  return(z[])
}
