## Source code for functions
## Based on v2.5

######## Global Variables
# Columns needed for analysis
# NOTE: "Program Area Modified" is only used during CDV check in Allison's original
fsd_cols <- c('operatingunit', 'primepartner', 'mech_code', 'mech_name',
              'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
              'program_mod', 'intervention', 'interaction_type', 
              'cost_category', 'sub_cost_category',
              'cop_budget_total', 'expenditure_amt')

msd_cols = c("operatingunit", "primepartner", "mech_code", "mech_name", 
             "indicator", 'cumulative')

######## Functions #############################################################
# Checks if budget == exp exactly and flags as error if so
check_budg_exp <- function(df){
  df_sum_interven <- df %>% 
    group_by(mech_code, mech_name, intervention) %>%
    summarize(sum_fast = sum(cop_budget_total, na.rm=T),
              sum_exp = sum(expenditure_amt, na.rm = T)) %>%
    mutate(diff = sum_exp - sum_fast)
  
  df_sum_mech <- df %>% 
    group_by(mech_code, mech_name) %>%
    summarize(sum_fast = sum(cop_budget_total, na.rm=T),
              sum_exp = sum(expenditure_amt, na.rm = T)) %>%
    mutate(diff = sum_exp - sum_fast)
  
  exp_budget_mech_check <- df_sum_mech %>%
    filter(diff == 0 ) %>%
    mutate(intervention = "TOTAL interventions")
  
  exp_budget_interv_check <- df_sum_interven %>%
    filter(diff == 0)
  
  results_exp_budget <- rbind(exp_budget_interv_check, exp_budget_mech_check) %>%
    arrange(mech_code, desc(intervention))
}

percent_other_check <- function(df){
  perc_other <- df %>%
    group_by(operatingunit, mech_code, mech_name) %>%
    mutate(mech_exp_sum = sum(expenditure_amt, na.rm = T)) %>%
    group_by(operatingunit, mech_code, mech_name, cost_category, sub_cost_category) %>%
    summarize(perc_exp = sum(expenditure_amt, na.rm=T) / first(mech_exp_sum)) %>%
    filter(cost_category == "Other",
           sub_cost_category == "Other",
           perc_exp > 0.1)
}

percent_pm_check <- function(df){
  perc_pm <- df %>%
    group_by(mech_code, mech_name) %>%
    mutate(mech_exp_sum = sum(expenditure_amt, na.rm = T)) %>%
    group_by(mech_code, mech_name, program) %>%
    summarize(perc_exp = sum(expenditure_amt, na.rm=T) / first(mech_exp_sum)) %>%
    filter(program == "PM")
}

# check expenditure's percentage of the budget 
### NOTE: do NOT use summary_rows func in GT to render this; summary rows are
###       already included, labelled "TOTAL" under "program".
###       Use conditional formatting to format these TOTAL rows
exp_perc_budg_check <- function(df){
  exp_perc_budg <- df %>%
    group_by(mech_code, mech_name, program) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm=T),
              sum_fast = sum(cop_budget_total, na.rm=T)) %>%
    mutate(perc_exp =  sum_exp/sum_fast ) %>%
    mutate(perc_exp = na_if(perc_exp, Inf))
  
  exp_perc_mech_level <- exp_perc_budg %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(sum_exp),
              sum_fast = sum(sum_fast)) %>%
    mutate(perc_exp = sum_exp/sum_fast) %>%
    mutate(perc_exp = na_if(perc_exp, Inf)) %>%
    mutate(program = "TOTAL")
  
  results_exp_perc <- rbind(exp_perc_budg, exp_perc_mech_level) %>%
    arrange(mech_code, desc(program))
}

# Select filter function and check if indicator result and expenditure entries
# both exist in the MSD and FSD respectively
# Inputs:
#         msd_filter: dyplr::filter() function with preset filters for MSD
#         fsd_filter: filter() function with preset filters for FSD
cross_data_check <- function(msd, fsd, msd_filter_str, 
                             cond, fsd_filter){
  sum_indicators <- msd %>%
    filter(indicator == msd_filter_str) %>%
    group_by(indicator, mech_code, mech_name) %>%
    summarize(sum_results = sum(cumulative, na.rm = T)) %>%
    filter(sum_results > cond)
  
  sum_exp <- fsd %>%
    fsd_filter %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm = T))
  
  # We only do left join because we are asking: "If results exist, do expenditures exist?"
  indic_exp <- merge(x=sum_indicators, y = sum_exp, by=c("mech_code", "mech_name"),
                     all.x = TRUE)
  
  cross_data_fail <- indic_exp[is.na(indic_exp$sum_exp) | (indic_exp$sum_exp ==0),] %>%
    filter(sum_results != 0) %>%
    mutate(indicator = msd_filter_str)
  
  return(cross_data_fail)
}

########  Set filter functions##################################################
filter_tx_fsd <- function(x){filter(x, program == "C&T")}

filter_pmtct_fsd <- function(x){filter(x,program == "C&T" &
                                         beneficiary == 'Pregnant & Breastfeeding Women')}

filter_kpmat_fsd <- function(x){filter(x,program == "PREV" &
                                         sub_program == "Medication assisted treatment" &
                                         beneficiary == 'Key Pops' &
                                         sub_beneficiary == 'People who inject drugs')}

filter_kpprev_fsd <- function(x){filter(x,program == "PREV" &
                                          beneficiary == 'Key Pops')}

filter_ovc_fsd <- function(x){filter(x, beneficiary == 'OVC' |
                                       sub_beneficiary == "Young women & adolescent females")}

filter_pp_fsd <- function(x){filter(x, beneficiary == 'Priority Pops')}

filter_prep_fsd <- function(x){filter(x, program_mod == 'PrEP')}

filter_vmmc_fsd <- function(x){filter(x, program_mod == 'VMMC' &
                                        beneficiary == 'Males')}

filter_hts_fsd <- function(x){filter(x, program_mod == 'HTS')}

msd_str_lst <- list("TX_CURR", "PMTCT_ART", "KP_MAT", 
                       "KP_PREV", "OVC_SERV", "PP_PREV", 
                       "PrEP_NEW", "VMMC_CIRC", "HTS_TST")

msd_cond <- list(200,50,10,100,50,50,100,100,100)

fsd_filter_lst <- list(filter_tx_fsd, filter_pmtct_fsd, filter_kpmat_fsd,
                       filter_kpprev_fsd, filter_ovc_fsd, filter_pp_fsd,
                       filter_prep_fsd, filter_vmmc_fsd, filter_hts_fsd)

########  GT functions #########################################################
# Input: outputted dataframe from check_budg_exp
gen_budg_exp_gt <- function(df){
  gt <- 
    # get rid of grouping in dataframe for GT formatting to work
    ungroup(df) %>%
    mutate(mech_code = case_when(intervention != "TOTAL interventions" 
                                 ~ "",
                                 TRUE ~mech_code)) %>%
    mutate(mech_name = case_when(intervention != "TOTAL interventions" 
                                 ~ "",
                                 TRUE ~mech_name)) %>%
    gt() %>%
    tab_header(
      title = "Budget vs. Expenditure",
      subtitle = "Below are mechanisms flagged for having 0% difference between budget and expenditure"
    ) %>%
    cols_align(
      align = "left",
      columns = intervention
    ) %>%
    
    cols_label(
      mech_code = html("Mechanism ID"),
      mech_name = html("Mechanism Name"),
      intervention = html("Intervention"),
      sum_fast = html("Total Planned Budget"),
      sum_exp = html("Total Expenditure"),
      diff = html("Difference")
    ) %>%
    fmt_currency(columns = c("sum_fast", "sum_exp"),
                 currency = "dollar")
}

# Input: dataframe outputted by percent_other_check
gen_perc_other_gt <- function(df){
  gt <-
    ungroup(df) %>%
    select(-cost_category, -sub_cost_category) %>%
    gt() %>%
    fmt_percent(columns = perc_exp) %>%
    tab_header(
      title = 'Top Expenditure in "Other: Other" Category',
      subtitle = 'Percent of Expenditure Spent in Other: Other exceeding 10%'
    ) %>%
    cols_label(
      operatingunit = html("Operating Unit"),
      mech_code = html("Mechanism Code"),
      mech_name = html("Mechanism Name"),
      perc_exp = html("% of Expenditure")
    )
}

# Input: output from percent_pm_check
gen_perc_pm_gt <- function(df){
  gt <- 
    ungroup(df) %>%
    select(-program) %>%
    gt() %>%
    fmt_percent(columns = perc_exp) %>%
    fmt_missing(
      columns = perc_exp,
      missing_text = "-"
    ) %>%
    tab_header(
      title = 'Program Management Expenditure',
      subtitle = 'Percent of Expenditure Spent on Program Management'
    ) %>%
    cols_label(
      mech_code = html("Mechanism Code"),
      mech_name = html("Mechanism Name"),
      perc_exp = html("% of Expenditure")
    )
}

# Input: dataframe outputted by exp_perc_budg_check
gen_exp_perc_budg_gt <- function(df){
  gt <-
    ungroup(df) %>%
    mutate(mech_code = case_when(program != "TOTAL" ~"",
                                 TRUE ~mech_code),
           mech_name = case_when(program != "TOTAL" ~"",
                                 TRUE ~mech_name)) %>%
    gt() %>%
    fmt_currency(columns = c("sum_fast", "sum_exp"),
                 currency = "dollar") %>%
    fmt_percent(columns = perc_exp) %>%
    fmt_missing(
      columns = perc_exp,
      missing_text = "-"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = 'lightgreen'),
        cell_text(weight = 'bold')),
      locations = cells_body(
        columns = perc_exp,
        rows = perc_exp >= 0.9 & perc_exp <= 1.1
      )
    ) %>%
    tab_header(
      title = 'Expenditure as Percent of Budget',
      subtitle = 'What percentage of the budget was spent/expended?'
    ) %>%
    cols_label(
      mech_code = html("Mechanism Code"),
      mech_name = html("Mechanism Name"),
      sum_exp = html("Total Expenditure"),
      sum_fast = html("Total Budgetted"),
      perc_exp = html("% of Budget Spent")
    )
}

# Inputs: 
#       * df: dataframe outputted by exp_perc_budg_check
#       * gt_func: function that produces gt table, in this case: "gen_exp_perc_budg_gt"
# Output: list of gt table objects
split_gts <- function(df, gt_func, row_limit = 50){
  df_len <- dim(df)[[1]]
  print(df_len)
  if (df_len > row_limit){
    num_dfs <- ceiling(df_len/row_limit)
    print(num_dfs)
    print(row_limit)
    row_start <- 1
    lst_gt <- list()
    
    for (i in 1:(num_dfs)){
      row_end <- i*row_limit
      # if the current "last row" is greater than the size of the dataframe,
      # just use the size of the dataframe instead of "last row" variable
      if (row_end > df_len){
        row_end <- df_len
      }
      
      df_short <- df[row_start:row_end,]
      gt_short <- df_short %>% gt_func
      print(class(gt_short))
      lst_gt[[i]] <- gt_short
      row_start <- row_end + 1
    }
    
    return(lst_gt)
  } else{return(df)}
}

gen_cdv <- function(msd, fsd){
  lst_dfs <- map2(.x = msd_str_lst, 
                  .y = fsd_filter_lst, 
                  ~cross_data_check(msd,
                                    fsd,
                                    msd_filter = .x, 
                                    fsd_filter = .y))
  gt <- bind_rows(lst_dfs) %>%
    group_by(indicator) %>%
    gt() %>%
    fmt_currency(columns = sum_exp) %>%
    tab_header(
      title = 'Cross Data Validation Checks',
      subtitle = 'Which IMs reported indicator results but did not report expenditures related to those indicators?'
    ) %>%
    cols_label(
      mech_code = html("Mechanism Code"),
      mech_name = html("Mechanism Name"),
      sum_exp = html("Total Expenditure"),
      sum_results = html("Results")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "lightgrey"),
        cell_text(weight = 'bold')),
      locations = cells_row_groups()
    )
}
