source("~/repo/ui_pubbias/source/funcs/setup.R")

PoPcites_path <- "/input/raw/PoPCites.csv"
xls_path <- "/input/raw/raw_review.xlsx"

file_path <- paste0(dir, PoPcites_path)
pop_cites <- read.csv(file_path, header = TRUE, encoding = "UTF-8")

# Import the data
data <- read.csv(file_path, header = TRUE, encoding = "UTF-8")

# Sources not wanted
not_wanted <- c("A Report by the Center on Budget and …",
                "A Study of",
                "ABT Thought Leadership Paper, ABT Associates",
                "ASTIN Bulletin: The Journal of the IAA",
                "Advances in Sociology Research",
                "American Journal of Political Science",
                "Annu. Conf. Am. Sociol …",
                "Annual Review of Economics",
                "Ariz. St. LJ",
                "Asian development bank economics working …",
                "Atlanta Fed Policy Hub",
                "Backgrounder-CD Howe Institute",
                "Bank of Italy Occasional Paper",
                "Bank of Italy, mimeo",
                "Book chapters authored by Upjohn Institute …",
                "Boston College Center for Retirement Research …",
                "Bulletin, December",
                "CEPS Special Reports No",
                "CESifo Economic Studies",
                "CESifo Forum",
                "California Policy Lab …",
                "Campbell Systematic …",
                "Carnegie-Rochester Conference Series on Public …",
                "Center for …",
                "Central Bank of Brazil …",
                "Centre for Economic Performance, London School of …",
                "Chicago Fed Letter",
                "Citeseer",
                "College Park, MD: University of Maryland …",
                "Comparative Political Studies",
                "Comparative political studies",
                "Compensation & Benefits Review",
                "DIW Economic Bulletin",
                "De Economist",
                "Development and Change",
                "Document de recherche de la …",
                "Draft Report to USDOL",
                "Draft, European University …",
                "EUROPEAN ECONOMY SPECIAL REPORT …",
                "EUROPEAN ECONOMY SPECIAL …",
                "Economic Aspects of Markets and Games",
                "Economic Review-Federal Reserve Bank of …",
                "Economic policy in Switzerland",
                "Economic theory",
                "Economics & Human Biology",
                "Employment Research Newsletter",
                "Employment Research …",
                "European Journal of Industrial Relations",
                "European Journal of Political Research",
                "European Sociological Review",
                "FRB St. Louis Working Paper",
                "Federal Reserve Bank of Cleveland …",
                "Final Report submitted to …",
                "FinanzArchiv/Public Finance Analysis",
                "Frydman, Roman (Hrsg.)",
                "GATE WP",
                "German Policy …",
                "Group",
                "Groupe D'Analyse, study …",
                "Hamilton Project Policy Proposal",
                "Handbook of labor economics",
                "Harvard Business School Finance …",
                "Health Affairs", "HeinOnline",
                "How 'Social'is Turkey? Working Paper IV",
                "IIFET 1992 proceedings",
                "ILO Working Paper",
                "IMPAQ …",
                "In Defence of Labour Market Institutions",
                "Instituto de Empresa Business School Working Paper …",
                "Intereconomics",
                "International Productivity Monitor",
                "International journal of forecasting",
                "J Epidemiol Community Health",
                "JAMA internal medicine",
                "JAMA network open",
                "JCMS: Journal of Common …",
                "JPMorgan Chase & Co …",
                "Jahrbücher für Nationalökonomie und Statistik",
                "Journal of Economic Surveys",
                "Journal of Health Economics",
                "Journal of Occupational Rehabilitation",
                "Journal of business ethics",
                "Journal of economic surveys",
                "LERA For Libraries",
                "Labor Law Journal",
                "Labor Market Policies in Canada and Latin …",
                "Legislative update, August. National …",
                "London School of Economics Working Paper",
                "Macroeconomic Dynamics",
                "MedRxiv",
                "Mercatus Special Edition Policy Brief",
                "Mismatch & Unemployment Insurance",
                "Monthly Lab. Rev.",
                "Monthly Labor Review",
                "NBER Working Paper", "NHH Dept. of Economics …",
                "National Center for Policy Analysis Report",
                "National Institute Economic Review",
                "New England Journal of …",
                "Nordic welfare states in the …",
                "Occasional Paper", "Order",
                "Oxford handbook of US social policy",
                "Panel Data Analysis",
                "Perspectives on Labour and Income",
                "Policy",
                "Policy Brief. Los Angeles …",
                "Policy Studies: Review Annual",
                "Policy report, US …",
                "Political Analysis",
                "Political Studies",
                "Preventive medicine",
                "Proceedings of the Industrial Relations Research …",
                "Proceedings. Annual Conference on Taxation and …",
                "Progressive Policy Institute, April",
                "Randomized Trials in Four …",
                "Recession and Recovery",
                "Reducing inflation: Motivation and strategy",
                "Reforming severance pay",
                "Regulating the Risk of …",
                "Regulating the risk of unemployment: National …",
                "Remarks at Center for American Progress, Washington …",
                "Report.[440]", "Revista Desarrollo y …",
                "Role of Macro Effects.\" Federal Reserve Bank of …",
                "SERIEs",
                "Sciences Po working …",
                "Seventh Annual Meeting",
                "Sinquefield Center for Applied Economic Research …",
                "Soc. Sec. Bull.",
                "Social Forces",
                "Social Policy & Administration",
                "Social Policy & …",
                "Social Science & …",
                "Social Security Bulletin",
                "Social policy & administration",
                "Social science & medicine",
                "Social security in the global village",
                "Society and Economy",
                "Sociology of Health & Illness",
                "Stiftung Wissenschaft & Politik Working Paper",
                "Studies in labor markets",
                "Studies in the Economics of Search",
                "Targeting Employment …",
                "Testimony to the Senate Finance Committee …",
                "The ANNALS of the American Academy of …",
                "The Nanxun Legacy and China's Development in the …",
                "The Nordic Model of Welfare: A Historical Reappraisal",
                "The Public Purpose",
                "The Third Dimension of Labor Markets …",
                "The Unemployment Crisi: All for Naught (edited with B …",
                "The University of Michigan",
                "Tilburg University",
                "Towards Higher Employment: the Role of Labour …",
                "UCLA L. Rev.",
                "US Department of …",
                "Unemployment Compensation: Studies and Research",
                "Unemployment Compensation: Studies and …",
                "Unemployment Crisis",
                "Unemployment Insurance Reform: Fixing a Broken …",
                "Unemployment Insurance Reform: Fixing a …",
                "Unemployment Insurance Reforms …",
                "Unemployment Insurance and the Functioning of the …",
                "Unemployment Insurance in the United States …",
                "Unemployment Insurance …",
                "Unemployment Insurance: The Second Half Century",
                "Unemployment Insurance: The Second Half-Century",
                "Unemployment compensation: studies and research",
                "University of Georgia",
                "University of Michigan, Working Paper",
                "Université du Main",
                "Université du Maine, Working paper",
                "Unpublished Manuscript",
                "Unpublished manuscript",
                "Unpublished manuscript, Cornell University",
                "Unpublished manuscript, Univ. Lausanne",
                "Unpublished manuscript, World Bank Human …",
                "Unpublished report, Department of Economics",
                "Urban Institute. http://www. urban. org/publications …",
                "VATT Research Reports",
                "Vand. L. Rev.",
                "Vierteljahrshefte zur Wirtschaftsforschung",
                "Vol. XX, No. XX, Issue, Year",
                "Wage Dispersion and the Re-Entitlement Effect",
                "Washington, DC, Urban Institute)(December)",
                "Washington, DC: The Urban. Institute. http://www …",
                "Work, Employment and Society",
                "Work. Pap., Bocconi Univ., Milan",
                "Workforce Policies for the Next Decade and Beyond …",
                "Working and Poor: How economic and policy changes …",
                "World Bank Social Protection …",
                "World Bank. Verfügbar unter: http://wbln0018 …",
                "Yale L. & Pol'y Rev.",
                "Yale University …",
                "ZEW policy brief",
                "ZEW-Centre for European …",
                "cdhowe.org",
                "documentos.fedea.net",
                "economics.yale.edu",
                "first version (www. sns. se/document/nber2_ph. pdf)",
                "manuscript June",
                "manuscript,  July",
                "no. April",
                "oui.doleta.gov",
                "parisschoolofeconomics.eu",
                "pure.vive.dk",
                "research.upjohn.org",
                "sef.hku.hk",
                "seminar \"Economic shock …",
                "upo.es",
                "… , and Unemployment Insurance (July …",
                "… 2000661The-Big-States-and-Unemployment-Insurance …",
                "… Moment: The Great Depression and the …",
                "… National Adaptations to Post-Industrial Labour …",
                "… Nutrition Assistance and Unemployment Insurance",
                "… Nutrition Assistance and Unemployment Insurance …",
                "… Research Association Series: Proceedings of the …",
                "… School Working Paper …",
                "… Seminar on Employment/Unemployment insurance …",
                "… Severance Pay: An …",
                "… Social Security Review",
                "… Unemployment Insurance …",
                "… and Unemployment Insurance",
                "… and Unemployment Insurance …",
                "… institutt, Universitetet i Oslo http://urn. nb …",
                "… interstate competition in the Unemployment Insurance …",
                "… the 57th Annual Meeting, January 7 …",
                "… unter http://brendanprice. ucdavis. edu/uploads …",
                "… working paper. file:///C:/Users/ksehn …",
                "Manuscript, University of Lausanne",
                "manuscript, July",
                "Unemployment insurance reform: Fixing a broken …",
                "Manuscript, Bocconi University")



pop_cites_cleaned <- pop_cites %>%
  filter(
    Source != "",
    !tolower(Source) %in% c("working", "manuscript", "fixing a"),
    !Source %in% not_wanted,
    !str_detect(Source, regex("^Available at SSRN"))
  )

write.csv(pop_cites_cleaned, paste0(dir, "/input/clean/PoPCites_cleaned.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# read in Excel workbook ----
multiplesheets_excel <- function(file_path, skips) {
  # Get the sheet names
  sheets <- excel_sheets(file_path)

  # Read the sheets and apply the skips
  data_list <- map2(sheets, skips, ~ suppressMessages(read_excel(file_path, sheet = .x, skip = .y)))
  data_frames <- map(data_list, as.data.frame)

  # Name the data frames list with sheet names
  names(data_frames) <- sheets
  return(data_frames)
}
skips <- c(1, 1, 0, 0, 0, 0)
df_list <- multiplesheets_excel(paste0(dir, xls_path), skips)

test_that(
  "paper_id + reform_id + estimate_id is unique key",
  expect_equal(
    df_list[["Auxiliary info"]] |>  nrow(),
    df_list[["Auxiliary info"]] |> distinct(paper_id, reform_id, estimate_id) |>  nrow()
  )
)

test_that(
  "paper_id + reform_id + policy margin + margin_outcome_identifier is unique key",
  expect_equal(
    df_list$Main |>  nrow(),
    df_list$Main |>
      distinct(paper_id, reform_id, pbd_vs_rr, estimate_id, margin_outcome_identifier) |>
      nrow()
  )
)

aux_info <- df_list[["Auxiliary info"]] %>%
  select(-ue_deviation, -ue_contemporaneous, -ue_rate_avg_91_21) %>%
  mutate(country = tolower(sample_country),
         year = sample_year) %>%
  mutate(across(where(is.list), ~ map_chr(.x, toString)))

other_countries <- c("usa", "germany", "austria", "canada")

wb_data <- df_list[["World Bank UE"]] %>%
  select(-`Constructed Average`, -`Country Code`,
         -`Indicator Name`, -`Indicator Code`, -`...36`) %>%
  rename("country" = `Country Name`) %>%
  pivot_longer(cols = !country,
               names_to = "year", values_to = "unemp_rate") %>%
  mutate(year = as.numeric(year), country = tolower(country)) %>%
  filter(
    !is.na(unemp_rate),
    !country %in% other_countries
  )

country_data_list <- list()

#US Data
usa_data <- df_list[["FRED UE"]] %>%
  mutate(year = year(Monthly)) %>%
  group_by(year) %>%
  summarise(unemp_rate = mean(USA))

country_data_list[["usa"]] <- usa_data


#Germany
germany_data <- df_list[["FRED UE"]] %>%
  mutate(year = year(Annual)) %>%
  rename("unemp_rate" = Germany) %>%
  select(year, unemp_rate) %>%
  filter(!is.na(unemp_rate))

country_data_list[["germany"]] <- germany_data


#Austria
austria_data <- df_list[["FRED UE"]] %>%
  mutate(year = year(Annual)) %>%
  rename("unemp_rate" = Austria) %>%
  select(year, unemp_rate) %>%
  filter(!is.na(unemp_rate))

country_data_list[["austria"]] <- austria_data

#Canada
canada_data <- df_list[["FRED UE"]] %>%
  mutate(year = year(Annual)) %>%
  rename("unemp_rate" = Canada) %>%
  select(year, unemp_rate) %>%
  filter(!is.na(unemp_rate))

country_data_list[["canada"]] <- canada_data

for (country in other_countries) {
  temp_df <- country_data_list[[country]] %>%
    filter(year >= 1968) %>%
    filter(year <= 2021)

  temp_df$country <- country

  wb_data <- rbind(wb_data, temp_df)
}

computed_unemp_data <- wb_data %>%
  rename("ue_contemporaneous" = "unemp_rate") %>%
  group_by(country) %>%
  mutate("ue_rate_avg_91_21" = mean(ue_contemporaneous)) %>%
  ungroup() %>%
  mutate(ue_deviation = ue_contemporaneous - ue_rate_avg_91_21)

#note the explicit edits to study 43 (Roed)
#it has country Norway and Sweden so treating as Norway for purpose of merge
#sample_country column is not changed so no external changes caused
new_aux_info <- aux_info %>%
  mutate(country = ifelse(paper_id == 43, "norway", country)) %>%
  left_join(computed_unemp_data, join_by(country, year)) %>%
  select(-year, -country) %>%
  mutate(paper_id = as.numeric(paper_id)) %>%
  filter(include == "Yes")

main <- df_list[["Main"]] %>%
  mutate(across(where(is.list), ~ map_chr(.x, toString)))

paper_info <- main %>%
  inner_join(new_aux_info, join_by(paper_id, reform_id, authors, title, estimate_id))

### also do join the other way to calculate some data characteristics
study_char <- new_aux_info %>%
  left_join(main, join_by(paper_id, reform_id, estimate_id)) %>%
  select(paper_id, macro_treatment, journal, sample_country,
         coef_type_nonemp, reg_outcome_nonemp, research_design) %>%
  distinct()

top_five_journal_list <- c("Journal of Political Economy",
                           "American Economic Review",
                           "The Quarterly Journal of Economics",
                           "The Review of Economic Studies",
                           "Econometrica: Journal of The Econometric Society")

field_list <- c("American Economic Journal: Economic Policy", "ILR Review",
                "Journal of Human Resources", "Journal of Labor Economics",
                "Journal of Public Economics", "LABOUR", "Labour Economics",
                "National Tax Journal",
                "The Review of Economics and Statistics")

method_list <- c("Journal of Applied Econometrics", "Journal of Econometrics")

out_char <- tribble(
  ~estimate, ~value,
  "num_studies", study_char |> summarise(n_distinct(paper_id)) |> pull(),
  "us_count", nrow(filter(study_char, sample_country == "USA")),
  "austria_count", nrow(filter(study_char, sample_country == "Austria")),
  "germany_count", nrow(filter(study_char, sample_country == "Germany")),
  "norway_count", nrow(filter(study_char, str_detect(sample_country, "Norway"))),
  "sweden_count", nrow(filter(study_char, str_detect(sample_country, "Sweden"))),
  "macro_treatment count", nrow(filter(study_char, macro_treatment == 1)),
  "top 5 count", nrow(filter(study_char, journal %in% top_five_journal_list)),
  "field journal count", nrow(filter(study_char, journal %in% field_list)),
  "econometric method count", nrow(filter(study_char, journal %in% method_list)),
  "hazard log-log count", nrow(filter(study_char, reg_outcome_nonemp == "hazard" &
                                        coef_type_nonemp == "log-log")),
  "hazard average range", mean(paper_info$eyeballed_hazard_range, na.rm = TRUE),
  "hazard average mean", mean(paper_info$eyeballed_hazard_mean, na.rm = TRUE),
  "num_quasi", nrow(filter(study_char, research_design %in% c("RDD", "RKD", "DID")))
)
data_characteristics_raw <- out_char %>%
  tibble::rownames_to_column(var = "X") %>%
  rename_with(~ gsub(" ", ".", .))

## distribution of countries among included studies
total_studies <- paper_info %>%
  distinct(paper_id, pbd_vs_rr) %>%
  nrow()
country_distr <- paper_info %>%
  group_by(sample_country) %>%
  #count studies that estimate both PBD and RR twice
  summarise(number_of_studies = n_distinct(paste(paper_id, pbd_vs_rr))) %>%
  arrange(desc(number_of_studies)) %>%
  mutate(share = number_of_studies / total_studies) %>%
  add_row(sample_country = "Total", number_of_studies = total_studies, share = 1)

country_distr_table <- country_distr %>%
  rename(
    "Country" = sample_country,
    "Number of Studies" = number_of_studies,
    "Share" = share
  ) %>%
  mutate(across(c(Share), ~ paste0(round(.x * 100, 0), "%")))

kable_output <- country_distr_table %>%
  kable("latex", booktabs = TRUE, linesep = "\\hline") %>%
  row_spec(16, extra_latex_after = "\\midrule")
kable_output <- gsub("\\\\begin\\{tabular\\}\\{lrl\\}|\\\\hline|\\\\toprule|Country & Number of Studies & Share",
                     "", kable_output)
added_text <- "
\\begin{tabular}{lcc}
\\toprule
\\textbf{Country} & \\textbf{Number of Studies} & \\textbf{Share} \\
"

sink(paste0(output, "/country_distribution.tex"))
cat(paste(added_text, kable_output, sep=""))
sink()

##cleaning
cleaned_data <- paper_info %>%
  #ad hoc UE rate cleaning for Roed et al. 2008
  #that estimates for two different countries
  mutate(
    ue_contemporaneous = case_when(
      str_detect(source_nonemp, "Sweden") & paper_id == 43 ~ 8.94,
      str_detect(source_nonemp, "Norway") & paper_id == 43 ~ 3.74,
      TRUE ~ ue_contemporaneous
    ),
    ue_deviation = if_else(
      is.na(ue_deviation),
      ue_contemporaneous - ue_rate_avg_91_21,
      ue_deviation
    )
  ) %>%
  #convert the preferred elasticities to numbers
  mutate(
    other_elasticity_claimed = as.numeric(other_elasticity_claimed),
    other_elasticity_nonemp = as.numeric(other_elasticity_nonemp)
  ) %>%
  #obtain elasticity and SE
  #* get single elasticity per observation
  #* order of priority for outcome:
  #* 1) total nonemployment duration
  #* 2) claimed duration
  mutate(
    elasticity_nonemp = case_when(
      other_elasticities_preferred == 1 &
        !is.na(other_elasticity_nonemp) ~ other_elasticity_nonemp,
      TRUE ~ implied_elasticity_nonemp
    ),
    se_nonemp = elasticity_nonemp / tstat_nonemp,
    hazard_nonemp = as.numeric((reg_outcome_nonemp == "hazard")),

    elasticity_claimed = case_when(
      other_elasticities_preferred == 1 &
        !is.na(other_elasticity_claimed) ~ other_elasticity_claimed,
      TRUE ~ implied_elasticity_claimed
    ),
    se_claimed = elasticity_claimed / tstat_claimed,
    hazard_claimed = as.numeric((reg_outcome_claimed == "hazard"))
  ) %>%
  group_by(paper_id, pbd_vs_rr) %>%
  #create unique identifiers and tag papers with multiple estimates
  mutate(
    estimates_per_study_margin = n(),
    estimate_study_num = row_number()
  ) %>%
  ungroup() %>%
  #format paper string ID
  mutate(
    paper_name_long = case_when(
      paper_name == "" ~ paper_name,
      TRUE ~ paste0(paper_name, "(", margin_outcome_identifier, ")")
    )
  ) %>%
  #create research design categorical variables
  #(relative to cross-sectional omitted group)
  mutate(
    design_DID = as.numeric(research_design == "DID"),
    design_RDD_RKD = as.numeric(research_design %in% c("RDD", "RKD")),
    PBD_indicator = as.numeric(pbd_vs_rr == "PBD")
  ) %>%
  #clean up numeric variables stored as strings
  mutate(
    mean_nonemp_dur = as.numeric(mean_nonemp_dur),
    mean_pbd = as.numeric(mean_pbd)
  ) %>%
  #impact factor z-score
  mutate(
    impact_factor_z = (impact_factor - mean(impact_factor, na.rm = TRUE)) /
      sd(impact_factor, na.rm = TRUE)
  )

# save long dataset with separate observations for claimed vs. nonemployment
df_long <- cleaned_data %>%
  pivot_longer(
    cols = starts_with("elasticity_") |
      starts_with("se_") |
      starts_with("tstat_") |
      starts_with("hazard_"),
    names_to = c(".value", "ue_measure"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  mutate(ue_measure = as.character(ue_measure)) %>%
  filter(!is.na(se)) %>%
  mutate(ue_measure_nonemp = ue_measure == "nonemp")

write.csv(df_long, paste0(dir, "/input/clean/clean_review_estimates_long.csv"), fileEncoding = "UTF-8")


long_path <- paste0(dir, "/input/clean/clean_review_estimates_long.csv")
df_long <- read.csv(long_path, fileEncoding = "UTF-8")

