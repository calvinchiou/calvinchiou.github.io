install.packages("scholar")
library(scholar)
# pull my profile
id <- "WvORrXMAAAAJ"

profile <- get_profile(id)
profile$name
profile$h_index
profile$i10_index
profile$total_cites
# citation history by year
history <- get_citation_history(id)
history
# per-paper citation counts
pubs <- get_publications(id)
pubs[, c("title", "cites", "year")]

# run occasionally, not on every render
saveRDS(profile, "data/scholar_profile.rds")
write.csv(history, "data/citation_history.csv", row.names = FALSE)
