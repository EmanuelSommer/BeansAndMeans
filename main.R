library(googlesheets4)

ss = "https://docs.google.com/spreadsheets/d/16lh2Ta_ZVLKOXfN-Z4_369_zkxPGWx2kUHipjnsBHbw/edit?usp=sharing"

participants <- read_sheet(ss = ss, sheet = "ActiveParticipants")
last_matches <- read_sheet(ss = ss, sheet = "Matches")
past_matches <- read_sheet(ss = ss, sheet = "PastMatches")
past_matches$PastMatches <- ifelse(is.na(past_matches$PastMatches), "", past_matches$PastMatches)

active_names <- participants$Name

past_matches_list <- strsplit(past_matches$PastMatches, ",")
names(past_matches_list) <- past_matches$Name

for (name in names(past_matches_list)) {
  if (name %in% last_matches$Name1) {
    other_name <- last_matches$Name2[last_matches$Name1 == name]
  } else if (name %in% last_matches$Name2) {
    other_name <- last_matches$Name1[last_matches$Name2 == name]
  } else {
    next
  }
  if (!(other_name %in% past_matches_list[[name]])) {
    past_matches_list[[name]] <- ifelse(
      is.na(past_matches_list[[name]]),
      other_name,
      c(past_matches_list[[name]], other_name)
    )
  }
}

# The main matching comes now:
set.seed(2)
new_matches <- data.frame(Name1 = character(), Name2 = character(), stringsAsFactors = FALSE)
while (length(active_names) > 1) {
  name1 <- sample(active_names, 1)
  active_names <- active_names[active_names != name1]
  feasible_candidates <- active_names[!(active_names %in% past_matches_list[[name1]])]
  if (length(feasible_candidates) == 0) {
    next
  }
  name2 <- sample(feasible_candidates, 1)
  active_names <- active_names[active_names != name2]
  new_matches <- rbind(new_matches, data.frame(Name1 = name1, Name2 = name2))
}

# Check the results:
if (length(active_names) == 1) {
  print(paste("! Unmatched participant:", active_names))
}
new_matches

# Overwrite the sheets
new_past_matches <- data.frame(Name = names(past_matches_list), PastMatches = sapply(past_matches_list, function(x) paste(x, collapse = ",")), stringsAsFactors = FALSE)
rownames(new_past_matches) <- NULL
write_sheet(new_matches, ss = ss, sheet = "Matches")
write_sheet(new_past_matches, ss = ss, sheet = "PastMatches")
