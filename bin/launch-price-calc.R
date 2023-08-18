################################################################################
#                       Launch Price Calculation Script                        
################################################################################

# This R script calculates:
#   1. The average price per kg (to LEO) paid each year.
#   2. The number of countries launching payloads to space each year.

# The script calculates two types of launch price indices:
# 1. Payload-based Price Index: Average price based on the number of payloads launched.
#    Note: Doesn't consider the mass of each payload due to missing data.
# 2. Vehicle-based Price Index: Average price based on the number of vehicles launched.
#    Considered more defensible due to data limitations of the payload-based index.

# Data Sources:
#   - Price per kg data by vehicle: CSIS Aerospace Security Project (2022)
#     URL: https://aerospace.csis.org/data/space-launch-to-low-earth-orbit-how-much-does-it-cost/
#     Notes: Graciously provided by Thomas G. Roberts. Where multiple sources are provided, the most recent is used.
#   - Launch log: Jonathan McDowell's Launch Log
#     URL: https://planet4589.org/space/gcat/tsv/derived/launchlog.tsv
#   - ISS Data: List of ISS-related objects
#     URL: https://planet4589.org/space/iss/iss.txt
#     Notes: Used for adjustments to avoid misattributing payloads launch years if they were launched from ISS modules.

# Author:
# Akhil Rao - Last modified: 18 August 2023

# Dependencies:
# R packages: tidyverse, patchwork, scales, RColorBrewer
################################################################################


########## Load packages and data ##########

# Load packages
lapply(c("tidyverse", "patchwork", "scales", "RColorBrewer"), library, character.only = TRUE)

# Read in data
## 1: Launch price data
launch_cost = read_csv("../data/csis-launch-costs.csv")[,1:3] # first three columns have the data we need
launch_log = read_tsv("../data/launchlog.tsv")[-1,] # first row is an update notice
## 2: Country data. Also need JSR secondary payload data for payloads deployed from the ISS
satcat = read_csv("../data/spacetrack-satcat.csv") # satellite catalog
# Secondary payload data. In fixed-width format; removed rows 2 and 3 from the raw JSR log (datestamp info not in the fwf layout) and manually counted column widths
jsr_iss = read_fwf(
  "../data/iss.txt", 
  fwf_widths(c(15, 11, 33, 11, 10, 11, 37, 62, 15), 
    c("INTDES", "JCAT", "Name", "Mass", "StatCode", "ModJCAT", "ModName", "Launch info", "Current status")),
    skip=1) # skip first row, repeats colnames 

# Rename columns in launch_cost
colnames(launch_cost) = c("LV_Type", "price_per_kg", "max_mass")

########## Harmonize launch vehicle names ##########
# Clean/relabel launch vehicle names in launch_log to match launch_cost. The name in launch_cost is treated as authoritative.

original_name_list = unique(launch_log$LV_Type) # Check unique launch vehicle levels in launch_log before cleaning
length(original_name_list)
 
## Replacement rules. Any vehicle with more than two rules is given its own section.
launch_log = launch_log %>% 
  mutate(LV_Type = case_when(
    # Athena rules
    grepl("Athena-1", LV_Type) ~ "Athena 1",
    grepl("Athena-2", LV_Type) ~ "Athena 2",
    # Atlas rules
    grepl("Atlas.*Agena", LV_Type) ~ "Atlas Agena",
    grepl("Atlas.*Centaur", LV_Type) ~ "Atlas Centaur",
    grepl("^Atlas V", LV_Type) ~ "Atlas V",
    grepl("Atlas 3A", LV_Type) ~ "Atlas III",
    grepl("Atlas 3B", LV_Type) ~ "Atlas III",
    # Ariane rules
    grepl("^Ariane 5G", LV_Type) ~ "Ariane 5G",
    grepl("Ariane 42", LV_Type) ~ "Ariane 42",
    grepl("Ariane 44", LV_Type) ~ "Ariane 44",
    grepl("Ariane 5ECA", LV_Type) ~ "Ariane 5ECA",
    # Chang Zheng/Long March rules
    grepl("Chang Zheng 3B", LV_Type) ~ "Long March 3B",
    grepl("Chang Zheng 3C", LV_Type) ~ "Long March 3C",
    grepl("Chang Zheng 5", LV_Type) ~ "Long March 5",
    grepl("Chang Zheng 2C-III/SD", LV_Type, fixed=TRUE) ~ "Long March 2C",
    grepl("Chang Zheng 7A", LV_Type, fixed=TRUE) ~ "Long March 7",
    grepl("Chang Zheng 6A", LV_Type, fixed=TRUE) ~ "Long March 6",
    grepl("Chang Zheng 5/YZ-2", LV_Type, fixed=TRUE) ~ "Long March 5",
    grepl("Chang Zheng 3B/YZ-1", LV_Type, fixed=TRUE) ~ "Long March 3B",
    grepl("Chang Zheng 3C/YZ-1", LV_Type, fixed=TRUE) ~ "Long March 3C",
    grepl("Chang Zheng 7/YZ-1A", LV_Type, fixed=TRUE) ~ "Long March 7",
    grepl("Chang Zheng 2C/YZ-1S", LV_Type, fixed=TRUE) ~ "Long March 2C",
    grepl("Chang Zheng 2D/YZ-3", LV_Type, fixed=TRUE) ~ "Long March 2D",
    grepl("Chang Zheng 2D/YZ-3 ", LV_Type, fixed=TRUE) ~ "Long March 2D",
    # Delta rules
    grepl("^Delta 79|Delta 73|Delta 74", LV_Type) ~ "Delta II",
    grepl("^Delta 89", LV_Type) ~ "Delta III",
    grepl("^Delta 09|Delta 03", LV_Type) ~ "Delta 0100-Series",
    grepl("^Delta 19|Delta 191|Delta 14|Delta 16", LV_Type) ~ "Delta 1000-Series",
    grepl("^Delta 23|Delta 29", LV_Type) ~ "Delta 2000-Series",
    grepl("Delta 39", LV_Type) ~ "Delta 3000-Series",
    grepl("Delta 59", LV_Type) ~ "Delta 6000-Series",
    grepl("Delta 69", LV_Type) ~ "Delta 6000-Series",
    grepl("Delta 4", LV_Type) ~ "Delta IV",
    grepl("Delta 4M", LV_Type) ~ "Delta IV",
    grepl("^Delta 3910$", LV_Type) ~ "Delta 3910",
    grepl("Delta 4M", LV_Type) ~ "Delta IV",
    grepl("Delta 4H", LV_Type) ~ "Delta IV Heavy",
    grepl("Thor Delta [A-Z]\\d", LV_Type) ~ str_replace(LV_Type, "([A-Z])\\d+", "\\1"),
    grepl("Thor Delta A", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta B", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta C", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta D", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta E", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta G", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta J", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta L", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta M", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    grepl("Thor Delta N", LV_Type) ~ str_replace(LV_Type, "Thor ", ""),
    # KT rules
    grepl("KT-2", LV_Type) ~ "Kuaizhou",
    # Minotaur rules
    grepl("^Minotaur IV", LV_Type) ~ "Minotaur IV",
    grepl("^Minotaur-C 3210", LV_Type) ~ "Minotaur C",
    # Mu rules
    grepl("^Mu-", LV_Type) ~ "Mu",
    # Pegasus rules
    grepl("Pegasus/HAPS", LV_Type) ~ "Pegasus",
    grepl("Pegasus H", LV_Type) ~ "Pegasus",
    grepl("Pegasus XL/HAPS", LV_Type) ~ "Pegasus XL",
    # Proton rules
    grepl("Proton-K", LV_Type) ~ "Proton",
    grepl("Proton-K/D-1", LV_Type) ~ "Proton",
    grepl("Proton-K/D-2", LV_Type) ~ "Proton",
    grepl("Proton-K/DM", LV_Type) ~ "Proton",
    grepl("Proton-K/DM-2", LV_Type) ~ "Proton",
    grepl("Proton-K/DM-2M", LV_Type) ~ "Proton",
    grepl("Proton-K/17S40", LV_Type) ~ "Proton",
    grepl("Proton-K/Briz-M", LV_Type) ~ "Proton",
    grepl("Proton-M", LV_Type) ~ "Proton",
    # PSLV rules
    grepl("^PSLV", LV_Type) ~ "PSLV",
    grepl("PSLV-XL", LV_Type) ~ "PSLV",
    grepl("PSLV-DL", LV_Type) ~ "PSLV",
    grepl("PSLV-QL", LV_Type) ~ "PSLV",
    # Saturn rules
    grepl("Uprated Saturn I", LV_Type) ~ "Saturn I",
    grepl("Saturn IB", LV_Type) ~ "Saturn I",
    # Soyuz rules
    grepl("Soyuz-\\w+", LV_Type) ~ "Soyuz",
    grepl("Soyuz 11A511", LV_Type) ~ "Soyuz",
    grepl("Soyuz 11A511M", LV_Type) ~ "Soyuz",
    grepl("Soyuz 11A511L", LV_Type) ~ "Soyuz",
    # Start rules
    grepl("Start-1", LV_Type) ~ "Start",
    grepl("Start-1.2", LV_Type) ~ "Start",
    # Taurus rules
    grepl("Taurus \\d+", LV_Type) ~ "Taurus",
    grepl("ARPA Taurus", LV_Type) ~ "Taurus",
    # Titan rules
    grepl("Titan II SLV", LV_Type) ~ "Titan II",
    grepl("Titan II GLV", LV_Type) ~ "Titan II",
    grepl("^Titan 3", LV_Type) ~ "Titan III",
    grepl("Commercial Titan 3", LV_Type) ~ "Titan III",
    grepl("Titan IIIA", LV_Type) ~ "Titan III+",
    grepl("Titan IIIB", LV_Type) ~ "Titan III+",
    grepl("Titan 23B", LV_Type) ~ "Titan III+",
    grepl("Titan 24B", LV_Type) ~ "Titan III+",
    grepl("Titan 33B", LV_Type) ~ "Titan III+",
    grepl("Titan 34B", LV_Type) ~ "Titan III+",
    grepl("Titan IIIC", LV_Type) ~ "Titan III+",
    grepl("Titan IIID", LV_Type) ~ "Titan III+",
    grepl("Titan IIIE", LV_Type) ~ "Titan III+",
    grepl("^Titan 4", LV_Type) ~ "Titan IV",
    # Tsiklon rules
    grepl("R-36O 8K69", LV_Type) ~ "R-36 / Cyclone",
    grepl("R-36O 8K69M", LV_Type) ~ "R-36 / Cyclone",
    grepl("Tsiklon-2", LV_Type) ~ "R-36 / Cyclone",
    grepl("Tsiklon-2A", LV_Type) ~ "R-36 / Cyclone",
    grepl("Tsiklon-3", LV_Type) ~ "R-36 / Cyclone",
    # Zenit rules
    grepl("Zenit-2", LV_Type) ~ "Zenit 2",
    grepl("Zenit-2SB", LV_Type) ~ "Zenit 2",
    grepl("Zenit-3SL", LV_Type) ~ "Zenit 3SL",
    grepl("Zenit-3SLBF", LV_Type) ~ "Zenit 3SL",
    # Other rules
    grepl("Antares \\d+", LV_Type) ~ "Antares",
    grepl("^Angara", LV_Type) ~ "Angara",
    grepl("^Energiya", LV_Type) ~ "Energiya",
    grepl("^GSLV", LV_Type) ~ "GSLV",
    grepl("^H-II", LV_Type) ~ "H-II",
    grepl("Kuaizhou-\\d+", LV_Type) ~ "Kuaizhou",
    grepl("Kosmos", LV_Type) ~ "Kosmos",
    grepl("Molniya", LV_Type) ~ "Molniya",
    grepl("^Scout", LV_Type) ~ "Scout",
    grepl("^Shavit \\d", LV_Type) ~ "Shavit",
    grepl("^Shuang Quxian 1", LV_Type) ~ "Shian Quxian", # "Shian" is a typo in launch_cost, "Shuang" is correct.
    grepl("^Shtil'-", LV_Type) ~ "Shtil",
    grepl("SS-520", LV_Type) ~ "S-520",
    grepl("^Unha", LV_Type) ~ "Unha",
    grepl("^Vega", LV_Type) ~ "Vega",
    grepl("^Vostok", LV_Type) ~ "Vostok",
    grepl("/YZ", LV_Type) ~ str_replace(LV_Type, "/YZ.*", ""),
    TRUE ~ LV_Type
  ))

unique(launch_log$LV_Type) # Check unique launch vehicle levels in launch_log after cleaning
length(unique(launch_log$LV_Type))

## Summarize unique launch vehicle names in launch_log by frequency of occurrence. Then join to launch_cost by vehicle name. Mutate a column with 1 if matching in both datasets.
join_summary = launch_log %>% 
  group_by(LV_Type) %>% 
  summarize(n = n()) %>% 
  left_join(launch_cost, by="LV_Type") %>%
  mutate(
    match = ifelse(is.na(price_per_kg), 0, 1),
    n_match = n*match
    )

# Record the records for which launch vehicles which aren't matched. There is a crosswalk in VehicleFamilies.csv but not all of these have price values, so their exclusion doesn't affect the index calculation.
missing_lv_types = launch_log %>% 
  distinct(LV_Type) %>% 
  anti_join(launch_cost, by = "LV_Type")

missing_lv_types %>% arrange(LV_Type) %>% print(n=Inf)

write_csv(missing_lv_types, "../output/missing_lv_types.csv")

## List in alphabetical order of vehicle name
join_summary %>%
  arrange(-desc(LV_Type)) %>%
  print(n=Inf)

## List in descending order of number of observations in launch_log
join_summary %>%
  arrange(desc(n)) %>%
  print(n=Inf)

## calculate share of observations linked
join_summary %>% 
    ungroup() %>%
    summarize(
        n = sum(n),
        n_match = sum(n_match),
        share = n_match/n
        )

########## Calculate average price per kg index ##########

# Construct datasets. Since the launch log only shows payload objects -- and since the vehicle price index only accounts for flights -- no need to filter out debris/non-payload objects.

## Joined data with no further calculations
launch_df_base = left_join(launch_log, launch_cost, by="LV_Type")

write_csv(launch_df_base, "../output/launch_df_base.csv")

## Compute average over payloads launched in a year. For example, if there were 3 payloads launched on 2 Falcon 9s and 2 payloads launched on 1 Long March 5 in a given year, the average price paid would be calculated as (3/5)*price_F9 + (2/5)*price_LM5.
# This is an odd index given the price/kg data we have is for launch vehicles assuming a single payload per launch. Thomas Roberts has suggested that weighting by payload mass would be appropriate here. To preserve $/kg units, then, the mass weighting would need to be done in share terms, e.g. each payload is weighted by its share of the total mass launched in a given year. This is not done here since we lack mass data for all payloads.
launch_df_payload = launch_df_base %>%
    select(Launch_Date, Flight_ID, LV_Type, price_per_kg, max_mass) %>%
    mutate( 
        # extract the first four characters of Launch_Date, which is the year
        year = substr(Launch_Date, 1, 4)
    ) %>%
    group_by(year) %>%
    summarize(
        year = as.numeric(year[1]),
        mean_price_payload = mean(price_per_kg, na.rm=TRUE)
    )  

# Compute average over vehicles launched in a year. For example, if there were 3 payloads launched on 2 Falcon 9s and 2 payloads launched on 1 Long March 5 in a given year, the average price paid would be calculated as (2/3)*price_F9 + (1/3)*price_LM5.
launch_df_vehicle = launch_df_base %>%
    select(Launch_Date, Flight_ID, LV_Type, price_per_kg, max_mass) %>%
    mutate( 
        # extract the first four characters of Launch_Date, which is the year
        year = substr(Launch_Date, 1, 4)
    ) %>%
    group_by(Flight_ID) %>%
    mutate( # also make indicator for observations missing a price_per_kg value
        year = year[1],
        price_per_kg = price_per_kg[1],
        max_mass = max_mass[1],
        missing_price = ifelse(is.na(price_per_kg), 1, 0)
    ) %>% ungroup() %>%
    group_by(year) %>%
    summarize(
        year = as.numeric(year[1]),
        mean_price_vehicle = mean(price_per_kg, na.rm=TRUE),
        missing_share = mean(missing_price)
    )

# Join both datasets, pivot longer. Keep missing_share variable as a separate column.
launch_df = left_join(launch_df_payload, launch_df_vehicle, by="year") %>%
    pivot_longer(cols=c("mean_price_payload", "mean_price_vehicle"), names_to="price_type", values_to="price") %>%
    mutate(
        price_type = factor(price_type, levels=c("mean_price_payload", "mean_price_vehicle")),
        price_type = fct_recode(price_type, "payloads" = "mean_price_payload", "vehicles" = "mean_price_vehicle")
    )

########## Calculate number of countries launching each year ##########

# Replacment rules for COUNTRY entries that aren't countries. When multiple countries own an object, it retains a distinct organizational entity. For example:
# every ORB entry in COUNTRY is changed to US
# every GLOB entry in COUNTRY is changed to US
# Every EUTE entry in COUNTRY is left as EUTE if the launch year is before 2001, and is changed to FR in and after 2001
# This approach is imperfect but broadly consistent with the UCS scheme described here: https://s3.amazonaws.com/ucs-documents/nuclear-weapons/sat-database/4-11-17-update/User+Guide+1-1-17+wAppendix.pdf. Specifically,
# "The home country identified with the operator/owner given in column D, i.e., the country that operates or owns the satellite or the home country of the business entity that does so. If this includes three or fewer countries, each is listed; otherwise the project is simply designated as Multinational. An exception to this is projects of the European Space Agency (ESA), which represent the joint efforts of its 15 member states and are designated as ESA." (page 4/11)
satcat = satcat %>%
  mutate(COUNTRY = case_when(
    COUNTRY == "ORB" ~ "US",
    COUNTRY == "GLOB" ~ "US",
    COUNTRY == "EUTE" & year(LAUNCH) < 2001 ~ "EUTE",
    COUNTRY == "EUTE" & year(LAUNCH) >= 2001 ~ "FR",
    TRUE ~ COUNTRY
  ))

# Join satcat to JSR launch log
big_log = left_join(satcat %>% filter(OBJECT_TYPE=="PAYLOAD"), launch_log, by=c("INTLDES"="Piece")) %>%
  select(INTLDES, NORAD_CAT_ID, SATNAME, COUNTRY, LAUNCH, LAUNCH_YEAR, LAUNCH_NUM, OBJECT_NUMBER, OBJECT_TYPE, LAUNCH_PIECE, SITE, DECAY, Flight_ID, LV_Type, JCAT)

# Data cleaning: JSR lists multiple objects with the same COSPAR ID. The first character of the JCAT ID tells us which entities are in the original satcat (JCAT beginning with S) and which are auxiliary objects only in JSR (JCAT beginning with A). We remove the auxiliary objects.
# For example, JSR lists two objects with INTDES 1998-067KR. One is named "Hagoromo" (JCAT S41895), the other is named "STARS-C Koki" (JCAT A08607). The latter JCAT starts with A, so we remove it.
# The process: First identify any rows where the same INTDES value appears more than once. Flag those rows. Within those rows, remove any rows where the JCAT value starts with A. Then remove the flag column.
jsr_iss_clean = jsr_iss %>%
  group_by(INTDES) %>%
  mutate(
    flag = ifelse(n() > 1, 1, 0)
  ) %>%
  filter(flag==0 | (flag==1 & substr(JCAT, 1, 1)=="S")) %>%
  select(-flag)

# Create a new column from the "Launch info" column in jsr_iss_clean. The new column will contain the COSPAR number of the launch vehicle that deployed the payload. The COSPAR number is a sequence just after the words "Launch aboard". It is in the form NNNN-NNN[A-Z], where the first four digits are the year, the next three digits are the launch number, and the final letter is/letters are the launch piece. For example, "Launch aboard 1998-067A" means the payload was deployed from the first launch of 1998-067. Then pull the first 4 digits of the COSPAR number into a new column called "iss_deploy_year"
jsr_iss_clean = jsr_iss_clean %>%
  mutate(
    launch_cospar = str_extract(`Launch info`, "Launch aboard \\d{4}-\\d{3}[A-Z]?"),
    launch_cospar = str_remove(launch_cospar, "Launch aboard "),
    iss_deploy_year = as.numeric(substr(launch_cospar, 1, 4)))

# Calculate the share of rows in jsr_iss$`Launch info` that contain the string "Launch Unknown".
jsr_iss %>% 
  ungroup() %>%
  mutate(
    launch_unknown = ifelse(str_detect(`Launch info`, "Launch Unknown"), 1, 0)
  ) %>%
  summarize(
    share_unknown = mean(launch_unknown)
  ) %>%
  as.data.frame()

# Join big log to JSR ISS log on COSPAR and JCAT identifiers
big_log = left_join(big_log, jsr_iss_clean, by=c("INTLDES"="INTDES", "JCAT"="JCAT"))

# Create a new column in big log, "deploy_year", which takes the value of iss_deploy_year if it is not NA, and otherwise takes the value of LAUNCH_YEAR
big_log = big_log %>%
  mutate(
    deploy_year = ifelse(is.na(iss_deploy_year), LAUNCH_YEAR, iss_deploy_year)
  )

# Replacement rules for deploy year based on internet searches. 1998 still shows a lot of objects from from countries with only one satellite launched that year and a two-letter ending to the COSPAR ID. Digging shows that some are ISS deployments that don't appear in JSR's ISS log. These rules recode their launch year based on what's found online for that COSPAR ID, e.g. in Gunter's Space Page.
big_log = big_log %>%
  mutate(deploy_year = case_when(
    INTLDES == "1998-067UY" ~ 2022, #https://space.skyrocket.de/doc_sdat/surya-satellite-1.htm
    INTLDES == "1998-067UU" ~ 2022, #https://space.skyrocket.de/doc_sdat/dantesat.htm
    INTLDES == "1998-067QE" ~ 2019, #https://en.wikipedia.org/wiki/NepaliSat-1
    INTLDES == "1998-067UD" ~ 2022, #https://en.wikipedia.org/wiki/TUMnanoSAT
    INTLDES == "1998-067SJ" ~ 2021, #https://space.skyrocket.de/doc_sdat/lawkanat-1.htm
    INTLDES == "1998-067SP" ~ 2021, #https://space.skyrocket.de/doc_sdat/g-satellite.htm -- disputed, may also be https://www.wikidata.org/wiki/Q107612976 , which also launched in 2021
    INTLDES == "1998-067QG" ~ 2019, #https://space.skyrocket.de/doc_sdat/bird.htm
    INTLDES == "1998-067SW" ~ 2021, #https://space.skyrocket.de/doc_sdat/pr-cunar-2.htm
    INTLDES == "1998-067SH" ~ 2021, #https://space.skyrocket.de/doc_sdat/bird.htm
    INTLDES == "1998-067UR" ~ 2022, #https://space.skyrocket.de/doc_sdat/nutsat.htm
    INTLDES == "1998-067TG" ~ 2021, #https://space.skyrocket.de/doc_sdat/light-1.htm
    INTLDES == "1998-067UM" ~ 2022, #https://space.skyrocket.de/doc_sdat/bird.htm
    INTLDES == "1998-067UP" ~ 2022, #https://space.skyrocket.de/doc_sdat/bird.htm
    TRUE ~ deploy_year
  ))
# An interesting object story:
# 1998-069B -- http://www.astronautix.com/s/sac-a.html

big_log %>%
  filter(deploy_year==1998, OBJECT_TYPE=="PAYLOAD") %>%
  select(COUNTRY, INTLDES) %>%
  group_by(COUNTRY) %>%
  summarize( # Count the number of payloads each country launched, and concatenate the INTLDES values with __ separators into a single vector
    n_payloads = n(),
    INTLDES = str_c(INTLDES, collapse="__")
  ) %>%
  arrange(-n_payloads) %>%
  print(n=Inf)

country_count = big_log %>%
  filter(OBJECT_TYPE=="PAYLOAD") %>%
  select(COUNTRY, deploy_year) %>%
  group_by(deploy_year) %>%
  summarize(
    n_countries = n_distinct(COUNTRY)
  ) %>%
  rename(year = deploy_year)

########## Plot price and country trends ##########

# Plot time series of prices over time. Y axis is log10 scale but with linear labels.
# This plot includes both the vehicle-weighted index and the payload-weighted index.
price_plot = ggplot(data=launch_df, aes(x=year, group=price_type, color=price_type)) +
    geom_line(aes(y=price), size=1.5) +
    scale_y_continuous(trans = 'log10', labels = scales::comma) +
    theme_bw() +
    labs(x = "Year", y = "FY21$/kg", title = "Average price per kg to LEO, 1957-2023", color="Averaged over") +
    theme(
      # reset y and x axis ticks and labels to be font size 15
      axis.text = element_text(size=14),
      axis.title = element_text(size=16),
      legend.text = element_text(size=14),
      legend.title = element_text(size=14),
      title = element_text(size=18)
    ) +
    scale_color_brewer(palette="Set1")

ggsave("../output/price_plot_both.png", price_plot, width=12, height=6, dpi=300)

# Plot of just the vehicle-average calculation. This is the more-defensible index (and is reported in the paper) given that we don't have mass data for the payload index.
price_plot_vehicle = ggplot(data=launch_df %>% filter(price_type=="vehicles"), aes(x=year)) +
    geom_line(aes(y=price), size=1.5) +
    scale_y_continuous(trans = 'log10', labels = scales::comma) +
    theme_bw() +
    labs(x = "Year", y = "FY21$/kg", title = "Average price per kg to LEO, 1957-2023") +
    theme(
      # reset y and x axis ticks and labels to be font size 15
      axis.text = element_text(size=14),
      axis.title = element_text(size=16),
      title = element_text(size=18)
    )

ggsave("../output/price_plot.png", price_plot_vehicle, width=12, height=6, dpi=300)

# Plot of share of observations missing over time
# Most of the missing observations are concentrated in the earlier portion of the sample, likely due to missile-based launch vehicles without listed costs in the CSIS data.
missing_plot = ggplot(data=launch_df_vehicle, aes(x=year, y=missing_share)) +
    geom_col() +
    theme_bw() +
    labs(x = "Year", y = "Share", title = "Share of observations missing price data, 1957-2023") +
    theme(
      # reset y and x axis ticks and labels to be font size 15
      axis.text = element_text(size=14),
      axis.title = element_text(size=16),
      title = element_text(size=18)
    )

# Combined price/share missing plot. Size ratio is 70/30. Annotation tag_levels are (a). Plot axes are aligned.
combined_price_missing_plot = price_plot_vehicle + missing_plot + plot_annotation(tag_levels = "a", tag_suffix = ")") + plot_layout(ncol = 1, heights = c(7, 3))

ggsave("../output/price_missing_plot.png", combined_price_missing_plot, width=12, height=9, dpi=300)

# Plot of just the vehicle-average calculation, but only over 1961-2023 and without log scaling. Interesting picture! Note that the decline begins well before SpaceX enters the picture; visually, it's hard to see a change in the post-1990s-peak trend following SpaceX's entry.
price_plot_vehicle_zoom = ggplot(data=launch_df %>% filter(price_type=="vehicles" & year >= 1961), aes(x=year)) +
    geom_line(aes(y=price), linewidth=1.5) +
    theme_bw() +
    labs(x = "Year", y = "FY21$/kg", title = "Average price per kg to LEO, 1961-2023") +
    theme(
      # reset y and x axis ticks and labels to be font size 15
      axis.text = element_text(size=14),
      axis.title = element_text(size=16),
      title = element_text(size=18)
    )

ggsave("../output/price_plot_zoom.png", price_plot_vehicle_zoom, width=12, height=6, dpi=300)

# Plot of the number of countries launching payloads to space each year
country_plot = ggplot(data=country_count, aes(x=year, y=n_countries)) +
    geom_line(linewidth=1.5) +
    theme_bw() +
    labs(x = "Year", y = "Number of countries", title = "Number of countries deploying payloads to space, 1957-2023") +
    theme(
      # reset y and x axis ticks and labels to be font size 15
      axis.text = element_text(size=14),
      axis.title = element_text(size=16),
      title = element_text(size=18)
    )

ggsave("../output/country_plot--iss-corrected.png", country_plot, width=12, height=6, dpi=300)

# Plot of prince index and countries launching over time, arranged in patchwork. Plots are stacked vertically, aligned on x axis, and have tag_levels (a).
combined_price_country_plot = price_plot_vehicle + country_plot + plot_annotation(tag_levels = "a", tag_suffix = ")") + plot_layout(ncol = 1, heights = c(1, 1))

ggsave("../output/price_country_plot.png", combined_price_country_plot, width=12, height=12, dpi=300)
