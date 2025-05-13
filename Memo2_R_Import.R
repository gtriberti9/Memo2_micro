# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

setwd("C:/Users/HP/OneDrive/Documentos/UChicago/1. Courses/3. Spring Quarter 2025/ECON 35530 Microeconomics of Development/Memo/Memo 2")
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
#install.packages('ipumsr')

library(ipumsr)
ddi <- read_ipums_ddi("idhs_00002.xml")
data <- read_ipums_micro(ddi)

ddi_2010 <- read_ipums_ddi("idhs_00003.xml")
data_2010 <- read_ipums_micro(ddi_2010)


# According to the text code, some useful variables could be:
## SAMPLE
"
45401               Malawi 1992
45402               Malawi 2000
45403               Malawi 2004
45404               Malawi 2010
45406               Malawi 2012
45407               Malawi 2014
45405               Malawi 2016
45408               Malawi 2017
"

"
83401               Tanzania 1991
83402               Tanzania 1996
83403               Tanzania 1999
83404               Tanzania 2004
83405               Tanzania 2010
83406               Tanzania 2015
83407               Tanzania 2017
"

## COUNTRY
"
454                 Malawi
834                 Tanzania
"

## YEAR (as the SAMPLE variable)

## GEO_TZ1996_2017  Tanzania regions, 1996-2017 [integrated; GIS]

## AGE5YEAR

## MARSTAT  Woman's current marital or union status

## FPKNOTYP Knows any type of FP method
"
00                  Knows no method
10                  Knows only traditional or folkloric method
11                  Knows only folkloric method
12                  Knows only traditional method
20                  Knows modern method
99                  NIU (not in universe)
"

## FPKNOPILL           Knows about Pill for FP
"
00                  No
10                  Yes
11                  Yes, spontaneously
12                  Yes, probed
97                  Method not asked about at all
98                  Missing
99                  NIU (not in universe)
"

## FPKNOCON            Knows about condom for FP
"
00                  No
10                  Yes
11                  Yes, spontaneously
12                  Yes, probed
97                  Method not asked at all
98                  Missing
99                  NIU (not in universe)
"

## FPKNOFST            Knows about female sterilization for FP
"
00                  No
10                  Yes
11                  Yes, spontaneously
12                  Yes, probed
97                  Method not asked at all
98                  Missing
99                  NIU (not in universe)
"

## FPKNOLAM            Knows about lactational amenorrhea as a method of family planning
"
00                  No
10                  Yes
11                  Yes, spontaneously
12                  Yes, probed
97                  Method not asked about at all
98                  Missing
99                  NIU (not in universe)
"

## FPKNOWD             Knows about withdrawal for FP
"
00                  No
10                  Yes
11                  Yes, spontaneously
12                  Yes, probed
97                  Method not asked about at all
98                  Missing
99                  NIU (not in universe)
"

## FPKNORHY            Knows about rhythm for FP
"
00                  No
10                  Yes
11                  Yes, spontaneously
12                  Yes, probed
97                  Method not asked about at all
98                  Missing
99                  NIU (not in universe)
"

## FPTYPNOW            Current FP use by method type (folk, traditional, modern)
"
00                  No method
10                  Folkloric or traditional method
11                  Folkloric method
12                  Traditional method
20                  Modern method
98                  Missing
99                  NIU (not in universe)
"

## FPUSPATRN           Pattern of use of contraceptive method
"
10                  Currently using
20                  Ever used, but not currently using
21                  Used since last birth
22                  Used before last birth
30                  Never used
98                  Missing
99                  NIU (not in universe)
"

## AIDSHEARD           Heard of AIDS
"
0                   No
1                   Yes
8                   Missing
9                   NIU (not in universe)
"

## AIDHEALTHY          Thinks a healthy-looking person can have AIDS
"
0                   No
1                   Yes
7                   Don't know
8                   Missing
9                   NIU (not in universe)
"

## AIDASHAMED          Would be ashamed if family member had HIV
"
0                   No
1                   Yes
7                   Don't know
8                   Missing
9                   NIU (not in universe)
"

## MLSERPROB Initial to get if Tanzania also has this problem
"
01                  HIV/AIDS
02                  Tuberculosis
03                  Malaria
04                  Malnutrition
05                  Diabetes
06                  Cancer
07                  Flu
08                  Road traffic accidents
09                  Diarrhea
10                  Heart disease
96                  Other
97                  Don't know
98                  Missing
"


####################### CODE ###############################

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)

## Analyzing DHS 2016

## Malawi data from paper
# Create a data frame with Malawi variables extracted from the paper tables
malawi_data <- data.frame(
  # Demographics from Table 1
  avg_age = 31.25,                     # Average age (31.2-31.3 from Table 1, Panel B)
  pct_male = 0.185,                    # 18.5% male (average of 18-19% from Table 1)
  completed_primary = 0.405,           # 40.5% completed primary school (average of 40-41%)
  km_to_clinic = 4.76,                 # Average distance to clinic (4.45-5.07 km)
  
  # HIV testing rates from Table 2/A3
  monthly_test_rate = 0.58,          # 0.58% tested per month in control villages
  men_tested = 0.395,                  # Average (0.48 & 0.31) tested per month Table 1
  
  # ART usage from Table 1
  pct_on_art = 0.0428,                 # 4.28% on ART (average of 3.85-4.71%)
  
  # Beliefs about HIV from Table 3
  belief_art_reduces_transmission = 0.20,  # 20% selected ART from prevention methods list
  belief_hiv_transmission_high = 0.96      # 96% believed HIV transmission was high without ART/condoms
)


## Tanzania data from databases, we need to filter for Urban areas to be comparable

# Filter Tanzania data for 2015
tanzania_data2015 <- data %>%
  filter(COUNTRY == 834 & URBAN == 1)

# Filter Tanzania data for 2015
tanzania_data2010 <- data_2010 %>%
  filter(COUNTRY == 834 & URBAN == 1)

# Buildingg comparable statistics
# Age structure - focusing on reproductive age (15-49 women, 15-54 men)
tanzania_age <- tanzania_data2015 %>%
  filter(AGE >= 15 & AGE <= 54) %>%
  summarize(
    avg_age = mean(AGE, na.rm = TRUE),
    count = n()
  )

# Gender distribution
tanzania_gender <- tanzania_data2015 %>%
  summarize(
    pct_male = mean(HHEADSEX == 1, na.rm = TRUE)
  )

# Educational attainment
tanzania_edu <- tanzania_data2015 %>%
  filter(is.integer(EDYRTOTAL)) %>%
  summarize(
    completed_primary = mean(EDYRTOTAL >= 7, na.rm = TRUE)
  )


# Distance to healthcare facility 
tanzania_distance <- tanzania_data2015 %>%
  summarize(
    avg_distance_km = 3.204)

# To make it comparable with Malawi, we take the tests in the last 12 months and 
# take the average per month.
tanzania_data2010 <- tanzania_data2010 %>%
  mutate(AIDLTWHEN = haven::as_factor(AIDLTWHEN))

if("AIDLTWHEN" %in% colnames(tanzania_data2010)) {
  # Calculate percentage tested in last 12 months
  tanzania_test_rate <- tanzania_data2010 %>%
    summarize(
      tested_last_12_months = mean(AIDLTWHEN == "Less than 12 months ago", na.rm = TRUE),
      men_tested_12 = mean(AIDLTWHEN == "Less than 12 months ago" & HHEADSEX == 1, na.rm = TRUE),
      monthly_test_rate = (tested_last_12_months / 12) * 100,  # Divide by 12 to get monthly rate
      ment_tested = (men_tested_12 / 12) * 100
    )
}

# HIV/AIDS knowledge and beliefs
# AIDCONLOWRYN Thinks always using a condom reduces AIDS risk (yes/no)
# Proxy for the belief of "getting HIV if never using condom or ART"
tanzania_hiv_beliefs <- tanzania_data2010 %>%
  summarize(
    healthy_can_have_hiv = mean(AIDCONLOWRYN == 1, na.rm = TRUE)
  )


# Compare HIV as a health concern
if("MLSERPROB" %in% colnames(tanzania_data2015)) {
  tanzania_health_concerns <- tanzania_data2015 %>%
    filter(!is.na(MLSERPROB)) %>%
    group_by(MLSERPROB) %>%
    summarize(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(desc(percentage))
}

# Create comparison table
comparison_variables <- c("Average age", "Percent male", "Completed primary school", 
                          "Distance to clinic (km)", "% Monthly HIV testing rate",
                          "% Men tested per month", "Beliefs on transmission without using condom")

comparison_malawi <- c(malawi_data$avg_age, malawi_data$pct_male, 
                       malawi_data$completed_primary, malawi_data$km_to_clinic,
                       malawi_data$monthly_test_rate, malawi_data$men_tested,
                       malawi_data$belief_hiv_transmission_high)

# Create placeholders for Tanzania values that you'll populate with actual results
comparison_tanzania <- c(tanzania_age$avg_age, tanzania_gender$pct_male,
                         tanzania_edu$completed_primary, tanzania_distance$avg_distance_km,
                         tanzania_test_rate$monthly_test_rate, tanzania_test_rate$ment_tested,
                         tanzania_hiv_beliefs$healthy_can_have_hiv)

comparison_table <- data.frame(
  Variable = comparison_variables,
  Malawi = comparison_malawi,
  Tanzania = comparison_tanzania
)

# Print the comparison table template
print(comparison_table)





## Analyzing DHS 2015

subset_data <- data %>%
  select(COUNTRY, YEAR, AGE5YEAR, DECFEMHCARE, FPKNOTYP, FPTYPNOW, FPUSPATRN,
         FPHOMVISITY, FPHOMTALKFP, MLSERPROB) %>%
  filter(COUNTRY %in% c(834))

latest_year <- max(subset_data$YEAR)

table(subset_data$MLSERPROB)

# Define the labels
labels <- c(
  "1" = "HIV/AIDS",
  "2" = "Tuberculosis",
  "3" = "Malaria",
  "4" = "Malnutrition",
  "5" = "Diabetes",
  "6" = "Cancer",
  "7" = "Flu",
  "8" = "Road traffic accidents",
  "9" = "Diarrhea",
  "10" = "Heart disease",
  "96" = "Other",
  "97" = "Don't know",
  "98" = "Missing"
)

# Convert MLSERPROB to character and factor with labels
subset_data$MLSERPROB <- as.character(subset_data$MLSERPROB)
subset_data$MLSERPROB <- factor(subset_data$MLSERPROB, levels = names(labels), labels = labels)

# Create a frequency table
mlserprob_table <- table(subset_data$MLSERPROB)

# Convert to data frame and sort
df <- as.data.frame(mlserprob_table)
colnames(df) <- c("HealthProblem", "Count")
df <- df[order(-df$Count), ]

# Plot using ggplot2
library(ggplot2)
ggplot(df, aes(x = reorder(HealthProblem, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "indianred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Perceived Serious Health Problems", x = "Health Problem", y = "Count")



# Malaria is more problematic than HIV

# Now create the subset for Malaria information

# ------------------ PREVENTION KNOWLEDGE ------------------
prevention_vars <- c("MLPRECLEAN", "MLPRECLOSE", "MLPRECOIL", "MLPREGRASS", 
                     "MLPREIPTP", "MLPREIRS", "MLPRENET", "MLPREREPEL", 
                     "MLPRESCREEN", "MLPRESPRAY", "MLPREWATER")

data <- data %>%
  mutate(across(all_of(prevention_vars), haven::as_factor))

prevention_data <- data %>%
  summarize(across(all_of(prevention_vars), ~mean(. == "Yes", na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Percentage") %>%
  mutate(Label = case_when(
    Item == "MLPRECLEAN" ~ "Keep surroundings clean",
    Item == "MLPRECLOSE" ~ "Keep doors/windows closed",
    Item == "MLPRECOIL" ~ "Use mosquito coils",
    Item == "MLPREGRASS" ~ "Cut the grass",
    Item == "MLPREIPTP" ~ "Intermittent preventive treatment",
    Item == "MLPREIRS" ~ "Use indoor residual spray",
    Item == "MLPRENET" ~ "Sleep under mosquito net",
    Item == "MLPREREPEL" ~ "Use insect repellent",
    Item == "MLPRESCREEN" ~ "House screening",
    Item == "MLPRESPRAY" ~ "Use insecticide spray",
    Item == "MLPREWATER" ~ "Remove standing water",
    TRUE ~ Item
  ),
  Category = "Prevention Method Knowledge") %>%
  select(Label, Percentage, Category)

# ------------------ GENERAL MALARIA KNOWLEDGE ------------------
data <- data %>%
  mutate(across(c(MLFATAL, MLCURABLE, MLPREKNOW), haven::as_factor))

knowledge_data <- data %>%
  summarize(
    "Malaria is fatal" = mean(MLFATAL == "Yes", na.rm = TRUE),
    "Malaria is curable" = mean(MLCURABLE == "Yes", na.rm = TRUE),
    "Malaria is preventable" = mean(MLPREKNOW == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Label", values_to = "Percentage") %>%
  mutate(Category = "General Knowledge")

# ------------------ COMBINE & PLOT ------------------
combined_knowledge <- bind_rows(prevention_data, knowledge_data)

ggplot(combined_knowledge, aes(x = reorder(Label, Percentage), y = Percentage, fill = Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Category, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Knowledge about Malaria and Prevention Methods",
    x = "",
    y = "Percentage of Respondents"
  ) +
  theme_minimal()


# ---------------- BEDNET USE ------------------------
# Convert to factors
data <- data %>%
  mutate(across(c(BEDNETHH, BEDNETSLEPT, BEDNETKIDLT5), haven::as_factor))

# Bednet ownership and usage
bednet_data <- data %>%
  summarize(
    hh_has_net = mean(BEDNETHH == "Yes", na.rm = TRUE),
    slept_under_net = mean(BEDNETSLEPT %in% c("Yes", "Yes, treated bednet", "Yes, untreated bednet"), na.rm = TRUE),
    children_under_net = mean(BEDNETKIDLT5 %in% c("All children", "Some children"), na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Percentage")

ggplot(bednet_data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Bednet Ownership and Usage", 
       x = "", 
       y = "Percentage") +
  theme_minimal()


# -------------------- RELATION BETWEEN BEDNETS OWNERSHIP AND USAGE-----
# Convert to factors
data <- data %>%
  mutate(BEDNETNUM = haven::as_factor(BEDNETNUM))

# Group by number of nets and calculate usage rates
bednet_usage_by_number <- data %>%
  filter(!is.na(BEDNETNUM) & BEDNETNUM %in% c("0", "1", "2", "3", "4", "5", "6", "7+")) %>%
  group_by(BEDNETNUM) %>%
  summarize(
    children_under_net = mean(BEDNETKIDLT5 %in% c("All children", "Some children"), na.rm = TRUE),
    n = n()
  )

ggplot(bednet_usage_by_number, aes(x = BEDNETNUM, y = children_under_net)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = scales::percent(children_under_net, accuracy = 0.1)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Children's Bednet Usage by Number of Nets in Household", 
       x = "Number of Bednets", 
       y = "Percentage of Children Using Nets") +
  theme_minimal()


# ------------------------ ATTITUDES FOR PREVENTION ------------------

# Convert to factors
data <- data %>%
  mutate(across(c(MLPROTECTYN, MLSLEEPIMPYN, MLHANGNETYN, MLKIDSLEEPYN), haven::as_factor))

# Create a dataset for attitudes
attitudes_data <- data %>%
  summarize(
    can_protect = mean(MLPROTECTYN %in% c("Strongly agree", "Somewhat agree"), na.rm = TRUE),
    nets_important = mean(MLSLEEPIMPYN %in% c("Strongly agree", "Somewhat agree"), na.rm = TRUE),
    can_hang_nets = mean(MLHANGNETYN %in% c("Strongly agree", "Somewhat agree"), na.rm = TRUE),
    ensure_kids_use = mean(MLKIDSLEEPYN %in% c("Strongly agree", "Somewhat agree"), na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Attitude", values_to = "Agreement") %>%
  mutate(Attitude = case_when(
    Attitude == "can_protect" ~ "Can protect self/children from malaria",
    Attitude == "nets_important" ~ "Important to sleep under net nightly",
    Attitude == "can_hang_nets" ~ "Can easily hang children's nets",
    Attitude == "ensure_kids_use" ~ "Can ensure children sleep under net",
    TRUE ~ Attitude
  ))

ggplot(attitudes_data, aes(x = reorder(Attitude, Agreement), y = Agreement)) +
  geom_col(fill = "purple") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Attitudes Toward Malaria Prevention", 
       x = "", 
       y = "Percent in Agreement") +
  theme_minimal()


# ---------------- ACCESS TO TREATMENT ----------------
# Converting to factors
data <- data %>%
  mutate(across(c(MLHFACT, MLTREATEASY), haven::as_factor))

# Create graph showing treatment access and attitudes
treatment_data <- data %>%
  summarize(
    acts_available = mean(MLHFACT == "Yes", na.rm = TRUE),
    believe_easy_treatment = mean(MLTREATEASY == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Treatment_Measure", values_to = "Percentage") %>%
  mutate(Treatment_Measure = case_when(
    Treatment_Measure == "acts_available" ~ "ACTs available at health facility",
    Treatment_Measure == "believe_easy_treatment" ~ "Believe malaria easily treated",
    TRUE ~ Treatment_Measure
  ))

ggplot(treatment_data, aes(x = Treatment_Measure, y = Percentage, fill = Treatment_Measure)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Treatment Access and Beliefs", 
       x = "", 
       y = "Percentage") +
  theme_minimal()

