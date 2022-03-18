library(glamr)
library(gophr)

library(janitor)
library(glue)
library(tidyverse)

library(data.table)
library(scales)

### Set Paths to download correct sources ====================================
# Path where GitHub folder can be found
git_dir <- "C:/Users/davidsong/Desktop/USAID"

### Global =====================================================================
fsd_cols <- c("record_type", "operatingunit", "countryname", "fundingagency", "fiscal_year",
              "primepartner", "mech_code", "mech_name", "program", "sub_program", 
              "interaction_type", "beneficiary","cop_budget_total", "expenditure_amt")
msd_cols <- c("operatingunit", "countryname", "fundingagency", "fiscal_year",
              "mech_code", "mech_name", "primepartner", "indicator", 
              "standardizeddisaggregate", "cumulative", "targets",
              "qtr1", "qtr2", "qtr3", "qtr4")


### Functions ==============================================
years <- c(2017, 2018, 2019, 2020, 2021)

# universal prep_fsd
fsd_selector <- function(df, cols) {
  df_out <- df %>% 
    filter(fiscal_year %in% years)%>%
    dplyr::select(cols) %>%
    glamr::remove_mo() %>%
    glamr::clean_agency()%>%
    # Changes USAID/WCF fundingagency categories to "USAID" 
    dplyr::mutate(fundingagency=dplyr::case_when(fundingagency== "WCF"~"USAID", 
                                                 TRUE ~fundingagency))%>%
    ##concatenate mech id and mech name
    dplyr::mutate( mech = paste(mech_code,"-", mech_name))
  return(df_out)
}

# MSD for use for UE and individual target data tables
indics <- c("TX_PVLS", "TX_CURR")
gen_msd_tgt <- function(df){
  df_out <- df %>%
    filter(fiscal_year %in% years)%>%
    dplyr::select(msd_cols) %>%
    filter(indicator %in% indics)%>%
    clean_agency()%>%
    # Changes USAID/WCF fundingagency categories to "USAID" 
    dplyr::mutate(fundingagency=dplyr::case_when(fundingagency== "WCF"~"USAID", 
                                                 TRUE ~fundingagency))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    # group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, 
    #          primepartner,indicator) %>% 
    # summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
    # ungroup()%>%
    filter(!fundingagency=="DEDUP") 
  return(df_out)
}

### Load data ==============================
# Assign pointer to original df that is read in, so that gc() can find and dispose of it
df_fsd_full <- si_path() %>% return_latest("Fin") %>% gophr::read_msd()
df_fsd_temp <- fsd_selector(df_fsd_full, fsd_cols) %>%
  # You MUST copy data.frame, else new pointer just points to the original 
  data.table::copy()

df_msd_full <- si_path() %>% return_latest("OU_IM") %>% gophr::read_msd()
df_msd_temp<- gen_msd_tgt(df_msd_full) %>%
  data.table::copy()%>%
  arrange(operatingunit, countryname, fundingagency, fiscal_year, mech_code, 
          indicator, standardizeddisaggregate)

mer_nat_full <- read.csv("MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20220211_v1_1.txt", 
                         sep='\t', header=T)
nat_indics <- c("PLHIV")
mer_nat_cols <-c("operatingunit", "countryname", "snu1", "indicator", "standardizeddisaggregate",
                 "fiscal_year", "targets", "qtr4")
mer_nat <- mer_nat_full %>%
  select(mer_nat_cols) %>%
  filter(fiscal_year %in% years) %>%
  arrange(operatingunit, countryname, fiscal_year, 
          indicator, standardizeddisaggregate) 

old_msd_path <- "C:/Users/davidsong/Desktop/USAID/viral_load/MER_Structured_Datasets_OU_IM_FY15-19_20220211_v1_1.txt"
df_msd_15_18_full <- old_msd_path %>% gophr::read_msd()
df_msd_15_18 <- gen_msd_tgt(df_msd_15_18_full) %>%
  filter(fiscal_year %in% c(2017, 2018)) %>%
  data.table::copy()%>%
  arrange(operatingunit, countryname, fundingagency, fiscal_year, mech_code, 
          indicator, standardizeddisaggregate)

# Remove full files and garbage collect to empty R memory
rm(df_fsd_full)
rm(df_msd_full)
rm(mer_nat_full)
rm(df_msd_15_18_full)
gc()

unique(df_msd_15_18$fiscal_year)
unique(df_msd_temp$fiscal_year)

df_msd <- rbind(df_msd_temp, df_msd_15_18) %>%
  arrange(operatingunit, countryname, fundingagency, fiscal_year, mech_code, 
          indicator, standardizeddisaggregate)

##################################################################
# Calculate MSD fields

df_plhiv <- mer_nat %>%
  filter(indicator %in% nat_indics) %>%
  filter(standardizeddisaggregate == "Total Numerator") %>%
  group_by(operatingunit, fiscal_year) %>% 
  summarise_at(vars(targets), sum, na.rm = TRUE) %>% 
  ungroup()%>%
  rename(plhiv = targets)

df_msd_ou <- df_msd %>%
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  group_by(operatingunit,fundingagency,fiscal_year,indicator, standardizeddisaggregate) %>%
  summarise_at(vars(cumulative,targets, qtr1, qtr2, qtr3, qtr4), sum, na.rm = TRUE) %>%
  ungroup()

unique(df_msd_ou$fiscal_year)

# # Attempts at trying to get TX_CURR lag sum
# #https://stackoverflow.com/questions/44874721/r-dplyr-lagging-variables-after-grouping-by-multiple-columns
# df_tx_curr <- df_msd_ou %>%
#   filter(indicator=="TX_CURR") %>%
#   mutate(prev_qtr3 = case_when(fiscal_year == 2017 ~ NA_real_, 
#                                TRUE ~ lag(qtr3))) %>%
#   mutate(prev_qtr4 = case_when(fiscal_year == 2017 ~ NA_real_,
#                                TRUE ~ lag(qtr4)))
# 
# test <- df_msd_ou %>%
#   filter(indicator=="TX_CURR") %>%
#   group_by(operatingunit, fundingagency) %>%
#   mutate(minim = min(fiscal_year, na.rm=T))
# 
# greater_test <- test%>% filter(minim >2017)
# # https://stackoverflow.com/questions/49330037/add-rows-based-on-missing-dates-within-a-group
# fill_test <- df_msd_ou %>% 
#   filter(indicator=="TX_CURR") %>%
#   group_by(operatingunit, fundingagency) %>%
#   complete(fiscal_year) %>%
#   mutate(minim = min(fiscal_year, na.rm=T))
# 
# greater_fill_test <- fill_test %>% filter(minim > 2017)

# Calculate 6-month lag for summing TX_CURR
# https://stackoverflow.com/questions/38119225/debugging-function-to-create-multiple-lags-for-multiple-columns-dplyr
df_tx_curr <- df_msd_ou %>%
  filter(indicator=="TX_CURR") %>%
  # group_by(operatingunit, fundingagency) %>%
  # arrange(operatingunit, fundingagency, fiscal_year) %>%
  # do(data.frame(., setNames(data.table::shift(.$qtr2, 1:2), c("tx_qtr2", "drop1")))) %>%
  # select(-c("drop1"))%>%
  # ungroup()  %>%
  # dropping year 2017 now that I've tested that this works
  filter(fiscal_year != 2017) %>%
  select(c("operatingunit", "fundingagency", "fiscal_year", "qtr2")) %>%
  rename(tx_qtr2 = qtr2)


df_pvls_denom <- df_msd_ou %>%
  filter(indicator == "TX_PVLS",
         standardizeddisaggregate == "Total Denominator",
         fiscal_year != 2017) %>%
  select(c("operatingunit", "fundingagency", "fiscal_year", "cumulative")) %>%
  rename(PVLS_denom = cumulative)

# Calculate viral load coverage (VL_coverage) based on the PVLS denominator and TX_CURR (lagged)
df_mer <- full_join(df_tx_curr, df_pvls_denom, by = c("operatingunit", "fundingagency",
                                                      "fiscal_year")) %>%
  mutate(VL_coverage = PVLS_denom / tx_qtr2)
##################################################################
# Calculate FSD fields
names(df_fsd_temp)

sort(unique(df_fsd_temp$sub_program))

unique(df_fsd_temp$program)

unique(filter(df_fsd_temp, program == "C&T")$sub_program)

# This confirms that the two sub-programs I am interested in are:
# "Laboratory systems strengthening" and "HIV Laboratory Services"
unique(filter(df_fsd_temp, sub_program == "HIV Laboratory Services")$program)

lab_programs = c("Laboratory systems strengthening", "HIV Laboratory Services")
df_lab <- df_fsd_temp %>%
  filter(sub_program %in% lab_programs) %>%
  group_by(operatingunit, fundingagency, fiscal_year, sub_program) %>%
  summarize_at(vars(expenditure_amt), sum, na.rm = T)%>%
  ungroup()


df_lab_total <- df_lab %>%
  group_by(operatingunit, fundingagency, fiscal_year) %>%
  summarize_at(vars(expenditure_amt), sum, na.rm=T) %>%
  ungroup() %>%
  rename(lab_str_serv = expenditure_amt)

df_lab_strength <- df_lab %>%
  filter(sub_program == "Laboratory systems strengthening") %>%
  rename(lab_strengthen = expenditure_amt) %>%
  select(-c("sub_program"))

df_lab_exp <- full_join(df_lab_strength, df_lab_total, by = c("operatingunit",
                                                                "fundingagency",
                                                                "fiscal_year")) %>%
  arrange(operatingunit, fundingagency, fiscal_year) %>%
  group_by(operatingunit, fundingagency) %>%
  mutate(cum_lab_strength = cumsum(lab_strengthen)) %>%
  mutate(cum_lab_str_serv = cumsum(lab_str_serv)) %>%
  ungroup()
#############################################################
## Combine MER and FSD metrics

df_ou <- full_join (df_mer, df_lab_exp, by = c("operatingunit",
                                               "fundingagency",
                                               "fiscal_year"))%>%
  full_join(., df_plhiv, by = c("operatingunit", "fiscal_year")) %>%
  mutate(cum_lab_str_capita = cum_lab_strength / plhiv) %>%
  mutate(cum_lab_str_serv_capita = cum_lab_str_serv / plhiv) %>%
  arrange(operatingunit, fundingagency, fiscal_year) 

## aggregate to Agency level
df_agency <- df_ou %>%
  group_by(fundingagency, fiscal_year) %>%
  summarize_at(vars(tx_qtr2, PVLS_denom, VL_coverage, 
                    cum_lab_strength,cum_lab_str_serv),
               sum, na.rm=T) %>%
  ungroup() %>%
  mutate(VL_coverage = PVLS_denom / tx_qtr2) %>%
  filter(fundingagency %in% c("USAID", "CDC", "DOD"))

################################################################
## Graph cumulative lab strengthening vs. PVLS denominator
coeff = 10
ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=cum_lab_strength)) +
  geom_line(aes(y=PVLS_denom * coeff), linetype= "dashed") +
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"),
                     sec.axis=sec_axis(~./coeff, name="People tested for viral load",
                                       label = unit_format(unit = "M", scale = 1e-6)))

ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=cum_lab_strength)) +
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"))

ggsave("lab_strengthening_trend.png", width=6, height=4)

ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=PVLS_denom)) +
  scale_y_continuous(name = "People tested for viral load",
                     label = unit_format(unit = "M", scale = 1e-6))
ggsave("pvls_denominator_trend.png", width=6, height=4)


### graph cum lab strength vs. viral load coverage #####################
ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=VL_coverage)) +
  scale_y_continuous(name = "Viral Load Coverage")
ggsave("viral_load_coverage.png", width=6, height=4)


coeff = 1e9
ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=cum_lab_strength)) +
  geom_line(aes(y=VL_coverage * coeff), linetype= "dashed") +
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"),
                     sec.axis=sec_axis(~./coeff, name="People tested for viral load",
                                       ))

### cumulative lab strengthening + services vs. PVLS denominator
coeff = 10
ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=cum_lab_str_serv)) +
  geom_line(aes(y=PVLS_denom * coeff), linetype= "dashed") +
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening and services",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"),
                     sec.axis=sec_axis(~./coeff, name="People tested for viral load",
                                       label = unit_format(unit = "M", scale = 1e-6)))

ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=cum_lab_str_serv)) +
  scale_y_continuous(name = "Cumul. expenditure: Lab Strengthening and Services",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"))

ggsave("lab_str_serv_trend.png", width=6, height=4)


### cumulative lab strengthening + services vs. viral load coverage
coeff = 1e10
ggplot(df_agency, aes(x=fiscal_year, color=fundingagency, group=fundingagency)) +
  geom_line(aes(y=cum_lab_str_serv)) +
  geom_line(aes(y=VL_coverage * coeff), linetype= "dashed") +
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening and services",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"),
                     sec.axis=sec_axis(~./coeff, name="People tested for viral load",
                     ))

### FY2021, OU scatter plot: lab strengthening expenditure per PLHIV vs.
###                          Viral Load coverage

df_ou_plot <- df_ou %>%filter(fiscal_year==2021) %>%
  na.omit()

ggplot(df_ou_plot, 
       aes(x = cum_lab_str_capita, y = VL_coverage, 
       color=fundingagency)) +
  geom_point(size=2) +
  scale_y_continuous(name = "Viral Load Coverage") + 
  scale_x_continuous(name = "Cumulative Lab System Strengthening Expenditure per PLHIV",
                     label = scales::dollar_format(prefix="$"))

ggsave("vl_coverage_vs_lab_str_capita.png", width=6, height=4)

###### All agencies combined ####################################################
df_pepfar <- df_ou %>%
  group_by(fiscal_year) %>%
  summarize_at(vars(tx_qtr2, PVLS_denom, VL_coverage, 
                    cum_lab_strength,cum_lab_str_serv),
               sum, na.rm=T) %>%
  ungroup() %>%
  mutate(VL_coverage = PVLS_denom / tx_qtr2) %>%
  filter(fiscal_year !=2017)

ggplot(df_pepfar, aes(x=fiscal_year)) +
  geom_line(aes(y=cum_lab_strength), color="tomato3", size=1) +
  geom_point(aes(y=cum_lab_strength), color="tomato3", size=2)+
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"))
ggsave("pepfar_lab_strengthen.png", width=6, height=4)

ggplot(df_pepfar, aes(x=fiscal_year)) +
  geom_line(aes(y=VL_coverage), color="tomato3", size=1) +
  geom_point(aes(y=VL_coverage), color="tomato3", size=2)+
  scale_y_continuous(name = "Viral Load Coverage",
                     label = unit_format(unit = "%", scale = 1e2))
ggsave("pepfar_viral_load_coverage.png", width=6, height=4)


coeff = 0.15*1e9
ggplot(df_pepfar, aes(x=fiscal_year, y = cum_lab_strength)) +
  geom_bar(stat = "identity", width=0.5, fill="dodgerblue4") +
  geom_line(aes(y=VL_coverage * coeff), color = "tomato3", size=1) +
  geom_point(aes(y=VL_coverage*coeff), color="tomato3", size=2)+
  scale_y_continuous(name = "Cumulative expenditure: Lab strengthening",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"),
                     sec.axis=sec_axis(~./coeff, name="Viral Load Coverage",
                                       label = unit_format(unit = "%", scale = 1e2)
                     ))
ggsave("pepfar_VL_vs_lab_spend.png", width = 6, height=4)
##################### Percent Change by OU ##############################
# calculate OU stats by combining all agency numbers
## aggregate to Agency level
pepfar_ou <- df_ou %>%
  group_by(operatingunit, fiscal_year) %>%
  summarize_at(vars(tx_qtr2, PVLS_denom, VL_coverage, 
                    cum_lab_strength,cum_lab_str_serv),
               sum, na.rm=T) %>%
  ungroup() %>%
  mutate(VL_coverage = PVLS_denom / tx_qtr2) %>%
  filter(fiscal_year != 2017) %>%
  full_join(., df_plhiv, by = c("operatingunit", "fiscal_year")) %>%
  mutate(cum_lab_str_capita = cum_lab_strength / plhiv)

# Initial cross-sectional snapshot
pepfar_ou_plot <- pepfar_ou %>%filter(fiscal_year==2021) %>%
  na.omit()

ggplot(pepfar_ou_plot, 
       aes(x = cum_lab_str_capita, y = VL_coverage)) +
  geom_point(size=2) +
  scale_y_continuous(name = "Viral Load Coverage") + 
  scale_x_continuous(name = "Cumulative Lab System Strengthening Expenditure per PLHIV",
                     label = scales::dollar_format(prefix="$"))

ggsave("pepfar_ou_vl_vs_spend.png", width=6, height=4)


##### Percent change########################
df_pepfar_change <- pepfar_ou %>%
  select(c("operatingunit", "fiscal_year", "VL_coverage", "cum_lab_str_capita",
           "cum_lab_strength"))%>%
  filter(fiscal_year %in% c(2020, 2021)) %>%
  pivot_wider(names_from=fiscal_year, values_from=c("VL_coverage", "cum_lab_str_capita",
                                                    "cum_lab_strength"))

# Base thresholds on quartiles
summary(df_pepfar_change$VL_coverage_2020)

df_pepfar_change <- df_pepfar_change %>%
  mutate(VL_diff = VL_coverage_2021 - VL_coverage_2020) %>%
  mutate(lab_diff = cum_lab_str_capita_2021 - cum_lab_str_capita_2020) %>%
  mutate(cum_lab_diff = cum_lab_strength_2021 - cum_lab_strength_2020) %>%
  mutate(VL_group = case_when(VL_coverage_2020 < 0.675~ "2020 VL coverage < 67.5%",
                              VL_coverage_2020 >=0.675 & VL_coverage_2020 <0.8~"2020 VL coverage < 80%",
                              VL_coverage_2020 >=0.8 & VL_coverage_2020 <0.85~"2020 VL coverage < 85%",
                              VL_coverage_2020 >=0.85 ~"2020 VL coverage >85%"))
ggplot(df_pepfar_change, 
       aes(x = lab_diff, y = VL_diff, color=VL_group)) +
  geom_point(size=2) +
  scale_y_continuous(name = "Viral Load Coverage",
                     label = unit_format(unit = "%", scale = 1e2)) + 
  scale_x_continuous(name = "Cumulative Lab System Strengthening Expenditure per PLHIV",
                     label = scales::dollar_format(prefix="$"))
ggsave("pepfar_percent_chg_per_plhiv.png", width=8, height=4)


# Drop outliers and try again
pepfar_chg_drop_outliers <- df_pepfar_change %>%
  filter(lab_diff >-15)

ggplot(pepfar_chg_drop_outliers, 
       aes(x = lab_diff, y = VL_diff, color=VL_group)) +
  geom_point(size=2) +
  scale_y_continuous(name = "Viral Load Coverage",
                     label = unit_format(unit = "%", scale = 1e2, accuracy=1)) + 
  scale_x_continuous(name = "2020 to 2021 change in Cumulative Lab System Strengthening Expenditure per PLHIV",
                     label = scales::dollar_format(prefix="$"))
ggsave("pepfar_percent_chg_per_plhiv_no_outlier.png", width=8, height=4)


# Compare just raw cumulative lab strength, not per PLHIV
ggplot(df_pepfar_change, 
       aes(x = cum_lab_diff, y = VL_diff, color=VL_group)) +
  geom_point(size=2) +
  scale_y_continuous(name = "Viral Load Coverage",
                     label = unit_format(unit = "%", scale = 1e2, accuracy=1)) + 
  scale_x_continuous(name = "Cumulative Lab System Strengthening Expenditure",
                     label= scales::dollar_format(scale=1e-6, prefix="$", suffix="M"))
ggsave("pepfar_percent_chg_total.png", width=8, height=4)

