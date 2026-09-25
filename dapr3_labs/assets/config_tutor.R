
# get current week:
project_config <- yaml::read_yaml("_quarto.yml")
current_week <- project_config$current_week

# get files 
week_f <- list.files(pattern = "^[0-9]+\\.qmd$")

# sol status for each week
sol_state <- list()
for (wf in week_f) {
  weekn <- as.numeric(sub(".*?([0-9]+).*", "\\1", wf))
  sol_state[[wf]] <- weekn <= current_week
}

message("Current Week Set To ",current_week)
message("Solutions visible for ", 
        paste0(names(unlist(sol_state))[which(unlist(sol_state))], collapse = ","))

# save out (gets read by .qmdss when rendreing)
saveRDS(list(current_week = current_week, sols = sol_state), "week_sols.rds")

