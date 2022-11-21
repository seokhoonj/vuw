
apply_rules <- function(rule_info, df) {
  dm <- merge(df, rule_info, by = c("kcd"), all.x = TRUE, allow.cartesian = TRUE)
  dm[!is.na(kcd) & age >= age_min & age <= age_max & hos <= hos_max & sur >= sur_min & sur <= sur_max & elp >= elp_min,
     ord_res := res]
  dm[!is.na(kcd) & is.na(order), `:=`(order = 0, ord_res = "na")]
  do <- dm[!is.na(ord_res), .(order = min(order)), .(id, kcd)]
  dm[do, on = .(id, kcd, order), result := ord_res]
  dm[is.na(kcd), `:=`(order = 0, result = "std")]
  z <- dm[!is.na(result)]
  z[, final_result := sort_code(ord_res), .(id)]
  z[final_result == "", final_result := "std"]
  rm_cols(z, .(res, ord_res))
  z[grepl("na|uwer", final_result, ignore.case = TRUE), decision := "uwer"]
  z[is.na(decision) & grepl("excl", final_result, ignore.case = TRUE),
    decision := pull_code_all("excl[0-9]\\([0-9\\-\\|\\[\\]]+\\)", final_result, collapse = ",")]
  z[is.na(decision), decision := "std"]
  set(z, j = "decision", value = toupper(z$decision))
  return(z)
}
