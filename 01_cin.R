
print("Getting data")
dbhandle <- odbcDriverConnect("connection string omitted")
cin <- data.table(sqlQuery(dbhandle, "SELECT * FROM CIN_2009_to_2021"))
rm(dbhandle)

nrow(cin)

# Step 1: LAs -------------------------------------------------------------

print("LA codes and tidy col names")
# Doing LA first in order to remove duplicate LA variable

setnames(cin, names(cin), tolower(names(cin)))

cin[is.na(cin_la), cin_la := cin_cin_la]
cin[, cin_cin_la := NULL]

setnames(cin, names(cin), gsub("cin_", "", names(cin)))

# Step 2: IDs -------------------------------------------------

print("IDs")

cin[, lachildid_anon := sapply(lachildid_anon, function(x) paste0(x, collapse = ""))]

sum(is.na(cin$lachildid_anon))
sum(cin$lachildid_anon == "")

# where lachildid_anon is missing, drop (extremely small n)
cin <- cin[lachildid_anon != ""]

# concatenate with LA to ensure uniqueness
cin[, lachildid_anon_concat := paste0(lachildid_anon, "-", la)]

# draw down PMR where child is in same LA and has PMR in some records but not others
cin <- cin[order(lachildid_anon, la, acadyr)]

cin[, pupilmatchingrefanonymous := pupilmatchingrefanonymous[which(!is.na(pupilmatchingrefanonymous))][1],
    by = .(lachildid_anon_concat)]



length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))


# Step 3: Validity of start and end dates ---------------------------------------

print("Start and end dates")

# use cinreferraldate and cinclosuredate. Where missing use latestreferraldate and latestclosuredate
#cin[, latestreferraldate := as.Date(cin$latestreferraldate, format = "%Y-%m-%d")]
#cin[, cinreferraldate := as.Date(cin$cinreferraldate, format = "%Y-%m-%d")]

#cin[latestreferraldate < as.Date("1930-01-01"), latestreferraldate := NA]
cin[cinreferraldate < as.Date("1930-01-01"), cinreferraldate := NA]

#cin[latestreferraldate > as.Date("2021-03-31"), latestreferraldate := NA]
cin[cinreferraldate > as.Date("2021-03-31"), cinreferraldate := NA]

#cin[, latestclosuredate := as.Date(cin$latestclosuredate, format = "%Y-%m-%d")]
#cin[, cinclosuredate := as.Date(cin$cinclosuredate, format = "%Y-%m-%d")]

#cin[latestclosuredate < as.Date("1930-01-01"), latestclosuredate := NA]
cin[cinclosuredate < as.Date("1930-01-01"), cinclosuredate := NA]

#cin[latestclosuredate > as.Date("2021-03-31"), latestclosuredate := NA]
cin[cinclosuredate > as.Date("2021-03-31"), cinclosuredate := NA]


# cin[is.na(cinreferraldate) & !is.na(latestreferraldate), cinreferraldate := latestreferraldate]
# cin[is.na(cinclosuredate) & !is.na(latestclosuredate), cinclosuredate := latestclosuredate]


# cin[, cppstartdate := as.Date(cin$cppstartdate, format = "%Y-%m-%d")]
# cin[, cppenddate := as.Date(cin$cppenddate, format = "%Y-%m-%d")]

cin[cppstartdate < as.Date("1930-01-01"), cppstartdate := NA]
cin[cppenddate < as.Date("1930-01-01"), cppenddate := NA]



# Step 4: Referral date > closure date --------------------------------------

print("Referral date > closure date")

#table(cin$cinreferraldate > cin$cinclosuredate, useNA = "always")
# set closure date to referral date
cin[cinreferraldate > cinclosuredate]$cinclosuredate <-
  cin[cinreferraldate > cinclosuredate]$cinreferraldate

# cin[, latestreferraldate := NULL]
# cin[, latestclosuredate := NULL]

# Step 5: Referral NFA inconsistent --------------------------------------

print("Referral NFA inconsistent")

# table(cin$referralnfa, useNA = "always")

# cin[, check := any(length(unique(referralnfa)) > 1), by = .(lachildid_anon_concat, cinreferraldate)]
# table(cin$check)

cin[, referralnfa := mode_fun(referralnfa), by = .(lachildid_anon_concat, cinreferraldate)]

# cin[, check := any(length(unique(referralnfa)) > 1), by = .(lachildid_anon_concat, cinreferraldate)]
# table(cin$check)
# cin[, check := NULL]

# Step 6: NFA missing ----------------------------------------------------

print("NFA missing: set to 0")

# table(cin$referralnfa, useNA = "always")
cin[is.na(referralnfa)]$referralnfa <- 0

# Step 7: Closure reason inconsistent ------------------------------------

print("Closure reason")

#table(cin$reasonforclosure, useNA = "always")
cin[, reasonforclosure := toupper(reasonforclosure)]
cin[, reasonforclosure := gsub("[[:space:]]", "", reasonforclosure)]
cin[substr(reasonforclosure, 1, 1) != "R", reasonforclosure := NA]

cin[, reasonforclosure := factor(reasonforclosure)]
cin[, reasonforclosure := mode_fun(reasonforclosure), by = .(lachildid_anon_concat, cinreferraldate)]

# Step 8: Ethnicity inconsistent -----------------------------------------

print("Ethnicity")

# table(cin$ethnicgroupmajor, useNA = "always")
# table(cin$ethnicgroupminor, useNA = "always")

na_vals <- c("INVA", "MISS", "N/A ", "UNCL", "NOBT", "REFU")
cin[ethnicgroupminor %in% na_vals, ethnicgroupminor := NA]

cin[, ethnicgroupminor := mode_fun(ethnicgroupminor), by = .(lachildid_anon_concat)]

AOEG <- "OOTH"
ASIA <- c("ABAN", "AIND", "AOTH", "APKN")
BLAC <- c("BAFR", "BCRB", "BOTH")
CHIN <- "CHNE"
MIXD <- c("MOTH", "MWAS", "MWBA", "MWBC")
WHIT <- c("WBRI", "WIRI", "WIRT", "WOTH", "WROM")

cin[ethnicgroupminor %in% AOEG, ethnicgroupmajor := "AOEG"]
cin[ethnicgroupminor %in% ASIA, ethnicgroupmajor := "ASIA"]
cin[ethnicgroupminor %in% BLAC, ethnicgroupmajor := "BLAC"]
cin[ethnicgroupminor %in% CHIN, ethnicgroupmajor := "CHIN"]
cin[ethnicgroupminor %in% MIXD, ethnicgroupmajor := "MIXD"]
cin[ethnicgroupminor %in% WHIT, ethnicgroupmajor := "WHIT"]
cin[is.na(ethnicgroupminor), ethnicgroupmajor := NA]

#table(cin$ethnicgroupminor, cin$ethnicgroupmajor, useNA = "always")
rm(na_vals, AOEG, ASIA, BLAC, CHIN, MIXD, WHIT)

# Step 9: Gender inconsistent ---------------------------------------------

print("Gender")

# table(cin$gender, useNA = "always")
cin$female <- cin$gender - 1
cin[female %in% c(-1, 8), female := NA]
# table(cin$female, useNA = "always")

cin[, female := mode_fun(female), by = .(lachildid_anon_concat)]

# Step 10: primary need code inconsistent ---------------------------------

print("Primary need code")

#table(cin$primaryneedcode, useNA = "always")
cin[, primaryneedcode := toupper(primaryneedcode)]
cin[, primaryneedcode := gsub("[[:space:]]", "", primaryneedcode)]
cin[primaryneedcode == "A2", primaryneedcode := "N2"]
cin[primaryneedcode == "A3", primaryneedcode := "N3"]
cin[primaryneedcode == "A4", primaryneedcode := "N4"]
cin[primaryneedcode == "A5", primaryneedcode := "N5"]
cin[primaryneedcode %in% c("NO", "N0", "UNMAPPEDNEEDCODE"), primaryneedcode := NA]
cin[, primaryneedcode := mode_fun(primaryneedcode), by = .(lachildid_anon_concat, cinreferraldate)]


# Step 11: Calculate age -----------------------------------------------------------

print("Calculate age at referral")

# first clean month and year of birth
# table(cin$monthofbirth, useNA = "always")
# table(cin$yearofbirth, useNA = "always")

cin[, monthofbirth := mode_fun(monthofbirth), by = lachildid_anon_concat]
cin[, yearofbirth := mode_fun(yearofbirth), by = lachildid_anon_concat]

# create a notional DOB so we can calculate approx age
cin[, notional_dob := as.Date(paste0(yearofbirth, "-", monthofbirth, "-01"))]

# now work out approx age at referral
cin[, age_at_ref_days := as.integer(difftime(cinreferraldate, notional_dob, units = "days"))]

# summary(cin$age_at_ref_days)
# table(cin$age_at_ref_days > -7*31 & cin$age_at_ref_days < 0)
# table(cin$age_at_ref_days < -7*31)

# Flag pre-birth referrals
cin[, prebirth_referral := age_at_ref_days > -7*31 & age_at_ref_days < 0]

# flag possible dob errors
cin[, dob_flag := age_at_ref_days < -7*31]

# age in years approx
cin[, age_at_ref_yrs := age_at_ref_days / 365.25]

# table(cin$age_grp_at_ref, useNA = "always")

# create Y of birth based on notional DOB
cin[, birthcyear := format(notional_dob, format = "%Y")]

lt <- as.POSIXlt(cin$notional_dob)
cin$birthfyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

lt <- as.POSIXlt(cin$notional_dob)
cin$birthayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

# dfe supplied variables
table(cin$agestartofcinperiod, useNA = "always")
table(cin$ageendofcinperiod, useNA = "always")

setnames(cin, 
         c("agestartofcinperiod", "ageendofcinperiod"),
         c("agestartofcinperiod_dfe", "ageendofcinperiod_dfe"))

# Step 12: Derive year variables ---------------------------------------------------------

print("Derive year variables")


setnames(cin,
         c("cinreferraldate", "cinclosuredate"),
         c("referraldate", "closuredate"))

cin$refcyear <- format(cin$referraldate, "%Y")

lt <- as.POSIXlt(cin$referraldate)
cin$refayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

lt <- as.POSIXlt(cin$referraldate)
cin$reffyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)


# Step 13: Tidy -----------------------------------------------------------


print("Tidying")
# In theory these two are 2008/9 and the other two are 2009/10 onwards, but the
# others exist for 2008/9 as well. Will leave.
# table(is.na(cin$la_09))
# table(is.na(cin$la_lgr))


# table(cin$sessnsocialworker) # mostly NA - remove

# the following are 2008/9 only and so will be dropped
# table(cin$servicetype, useNA = "always")
# table(cin$serviceprovision, useNA = "always")
# summary(cin$serviceprovisionstartdate)
# summary(cin$serviceprovisionenddate)

cin <- cin[, c("lachildid_anon_concat",
               "lachildid_anon",
               "pupilmatchingrefanonymous",
               "acadyr",
               "la",
               "la_9code",
               "la_09",
               "la_lgr",
               "notional_dob",
               "dob_flag",
               "birthcyear",
               "birthfyear",
               "birthayear",
               "female",
               "ethnicgroupminor",
               "ethnicgroupmajor",
               "disability",
               "age_at_ref_yrs",
               "agestartofcinperiod_dfe",
               "ageendofcinperiod_dfe",
               "prebirth_referral",
               "referraldate",
               "refcyear",
               "reffyear",
               "refayear",
               "primaryneedcode",
               "referralsource",
               "referralnfa",
               "closuredate",
               "reasonforclosure",
               "dateofinitialcpc",
               "cppstartdate",
               "categoryofabuse",
               "initialcategoryofabuse",
               "latestcategoryofabuse",
               "numberofpreviouscpp",
               "cppenddate")]

# Step 14: Referral source & disability ------------------------------------------------

print("Referral source")
# table(cin$referralsource, useNA = "always")
cin[, referralsource := toupper(referralsource)]
cin[, referralsource := gsub("[[:space:]]", "", referralsource)]
cin[referralsource %in% c("10", "PR"), referralsource := NA]
cin[, referralsource := mode_fun(referralsource), by = .(lachildid_anon_concat, referraldate)]

print ("Disability")
# table(cin$disability, useNA = "always")
cin[, disability := mode_fun(disability), by = .(lachildid_anon_concat, referraldate)]

# Step 15: Deduplication -------------------------------------------------------

print("Dedup")

# multiple rows with same vals per child for following vars:
cin <- cin[!duplicated(cin)]

length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))

# Step 16: assign indices -------------------------------------------------

print("Assign indices")

cin <- cin[order(lachildid_anon_concat, referraldate)]
cin[, row_per_child := seq_len(.N), by = rleid(lachildid_anon_concat)]
cin[, epi_index := frank(referraldate, ties.method = "dense"), by = lachildid_anon_concat]

# Step 17: identify episode types -----------------------------------------

print("Episode types")

cin[, assessment_episode := referralnfa != 1]
cin[, need_episode := referralnfa != 1 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]

cin[, cpp := !is.na(cppstartdate) | !is.na(cppenddate) |
      !is.na(categoryofabuse) | !is.na(initialcategoryofabuse) |
      !is.na(latestcategoryofabuse)]

# Step 18: Save --------------------------------------------------------------------

print("Saving")
fwrite(cin, file = "processed/cin.csv")

rm(cin)
gc()
