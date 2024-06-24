
# Step 1: Load data --------------------------------------------------------

print("Getting data")
dbhandle <- odbcDriverConnect("connection string omitted")
cla <- data.table(sqlQuery(dbhandle, "SELECT * FROM CLA_Data_Supplied"))
rm(dbhandle)


setnames(cla, names(cla), tolower(names(cla)))

setnames(cla,
         c("date_epi_comm", "date_epi_ceased", "rec"),
         c("date_episode_started", "date_episode_ceased", "reason_episode_ceased"))

print("IDs")

cla[, cla_child_id_anon := sapply(cla_child_id_anon, function(x) paste0(x, collapse = ""))]
cla[, cla_child_la_code_anon := sapply(cla_child_la_code_anon, function(x) paste0(x, collapse = ""))]

# table(is.na(cla$cla_child_la_code_anon))
# table(cla$cla_child_la_code_anon == "")
# 
# table(is.na(cla$cla_pupilmatchingrefanonymous))
# table(cla$cla_pupilmatchingrefanonymous == "")

# unique la-child id
cla[, la_child_id_anon_concat := paste0(la, "-", cla_child_la_code_anon)]

# draw down PMR
cla <- cla[order(la_child_id_anon_concat, date_episode_started)]

cla[, cla_pupilmatchingrefanonymous := cla_pupilmatchingrefanonymous[which(!is.na(cla_pupilmatchingrefanonymous))][1],
    by = .(la_child_id_anon_concat)]

length(unique(cla$la_child_id_anon_concat))
length(unique(cla$cla_pupilmatchingrefanonymous))

# Step 2: missing episode start and end dates -------------------------------------------------------------

print("Episode dates")

# table(is.na(cla$date_episode_started))
# table(is.na(cla$date_episode_ceased))
# table(is.na(cla$poc_start))

# table(cla$date_episode_ceased < cla$date_episode_started)
cla <- cla[-which(date_episode_ceased < date_episode_started)]
# table(cla$date_episode_ceased < cla$date_episode_started)

# table(cla$date_episode_started > cla$date_episode_ceased)

# table(cla$reason_episode_ceased)
cla[reason_episode_ceased == "-10"]$date_episode_ceased <- NA

# any(cla$date_episode_started < cla$poc_start) 

cla[, epicyear := format(date_episode_started, format = "%Y")]


length(unique(cla$la_child_id_anon_concat))
length(unique(cla$cla_pupilmatchingrefanonymous))

# Step 3: first epi start == poc start ------------------------------

print("Ensure first epi and POC start are the same")

# first assign indices to get first episodes per poc
cla <- cla[order(la_child_id_anon_concat, poc_start, date_episode_started)]

cla[, epi_n_per_poc := seq_len(.N), by = rleid(la_child_id_anon_concat, poc_start)]

# table(cla[epi_n_per_poc == 1]$date_episode_started !=
#         cla[epi_n_per_poc == 1]$poc_start) 
  
#table(cla[epi_n_per_poc == 1 & date_episode_started != poc_start]$epicyear)
# all pre 2003

# are all poc start pre first epi start?
all(cla[epi_n_per_poc == 1 & date_episode_started != poc_start]$poc_start <
      cla[epi_n_per_poc == 1 & date_episode_started != poc_start]$date_episode_started) # true

# replace poc start with first epi start date
cla[epi_n_per_poc == 1 & date_episode_started != poc_start]$poc_start <-
  cla[epi_n_per_poc == 1 & date_episode_started != poc_start]$date_episode_started

# table(cla[epi_n_per_poc == 1]$date_episode_started !=
#         cla[epi_n_per_poc == 1]$poc_start) # 0


# Step 4: consistent gender, year and month of birth and eth across records ------------------

print("Cleaning ethnicity")
table(cla$ethnic, useNA = "always")
cla[ethnic %in% c("REFU", "NOBT", ""), ethnic := NA]
cla[, ethnic := mode_fun(ethnic), by = la_child_id_anon_concat]


print("Cleaning sex")
#table(cla$sex, useNA = "always")
cla[, female := sex - 1]
cla[, female := mode_fun(female), by = la_child_id_anon_concat]
cla[, sex := NULL]


print("Cleaning year and month of birth")
#table(cla$year_of_birth, useNA = "always")
cla[, year_of_birth := mode_fun(year_of_birth), by = la_child_id_anon_concat]

#table(cla$month_of_birth, useNA = "always")
cla[, month_of_birth := mode_fun(month_of_birth), by = la_child_id_anon_concat]

print("Cleaning episode data")
#table(cla$reason_for_new_episode, useNA = "always")
cla[reason_for_new_episode == "P ", reason_for_new_episode := "P"]

#table(cla$placement, useNA = "always")
#table(cla$legal_status, useNA = "always")

#table(cla$reason_for_placement_change, useNA = "always")
cla[reason_for_placement_change %in% c("", " "), reason_for_placement_change := NA]

#table(cla$reason_episode_ceased, useNA = "always")

# table(cla$cat_need, useNA = "always")
cla[cat_need %in% c("", " "), cat_need := NA]

# Step 5: derive year variables -------------------------------------------

print("Deriving year variables")
cla[, epicyear := format(date_episode_started, format = "%Y")]

lt <- as.POSIXlt(cla$date_episode_started)
cla$epiayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

lt <- as.POSIXlt(cla$date_episode_started)
cla$epifyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

# Step 6: UASC ------------------------------------------------------------

# table(cla$uasc_status, useNA = "always")
cla[is.na(uasc_status), uasc_status := 0]

# Step 7: trim dataset ------------------------------------------

print("Trimming and deduplicating dataset")
cla <- cla[, c("la_child_id_anon_concat",
               "la",
               "cla_pupilmatchingrefanonymous",
               "year_of_birth",
               "month_of_birth",
               "ethnic",
               "female",
               "uasc_status",
               "poc_start",
               "date_episode_started",
               "epicyear",
               "epiayear",
               "epifyear",
               "reason_for_new_episode",
               "cat_need",
               "placement",
               "legal_status",
               "reason_for_placement_change",
               "date_episode_ceased",
               "reason_episode_ceased")]

setnames(cla, "cla_pupilmatchingrefanonymous", "pupilmatchingrefanonymous")

cla <- cla[order(la_child_id_anon_concat, poc_start, date_episode_started)]
cla <- cla[!duplicated(cla)]

length(unique(cla$la_child_id_anon_concat))
length(unique(cla$pupilmatchingrefanonymous))


# Step 8: calculate poc durations -----------------------------------------

print("Calculating poc duration")
cla[, poc_end := max(date_episode_ceased), by = .(la_child_id_anon_concat, poc_start)]

#table(is.na(cla$poc_end))
cla[, poc_dur_days := as.integer(difftime(poc_end, poc_start, units = "days"))]
#summary(cla$poc_dur_days)

# Step 9: Calculate age -----------------------------------------------------------

print("Calculate age at epi start")

# create a notional DOB so we can calculate approx age
cla[, notional_dob := as.Date(paste0(year_of_birth, "-", month_of_birth, "-01"))]

# now work out approx age at referral
cla[, age_at_epi_days := as.integer(difftime(date_episode_started, notional_dob, units = "days"))]

# summary(cla$age_at_epi_days)

# age in years approx
cla[, age_at_epi_yrs := age_at_epi_days / 365.25]

# create Y of birth based on notional DOB
cla[, birthcyear := format(notional_dob, format = "%Y")]

lt <- as.POSIXlt(cla$notional_dob)
cla$birthayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

lt <- as.POSIXlt(cla$notional_dob)
cla$birthfyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)


# Step 10: assign indices -------------------------------------------------

print("Assign indices")

cla <- cla[order(la_child_id_anon_concat, date_episode_started)]
cla[, row_per_child := seq_len(.N), by = rleid(la_child_id_anon_concat)]
cla[, epi_index := frank(date_episode_started, ties.method = "dense"), by = la_child_id_anon_concat]

# Step 11: save -----------------------------------------------

print("Saving")
fwrite(cla, file = "processed/cla.csv")
rm(list = ls())
