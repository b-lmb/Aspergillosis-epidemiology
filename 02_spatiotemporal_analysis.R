library(tidyverse)
library(data.table)
library(INLA)
library(SpatialEpi)
library(tigris)
library(spdep)
library(tictoc)
library(ggspatial)
library(sf)
library(usmap)
library(purrr)
library(glue)
library(ggrepel)
library(gt)



rm(list = ls())

#Loading data 
weights <- read_csv("For_Brittany_adults_weighted_cerner_data_12Mar25.csv")
case_denom_data <- read.csv("forweights.csv",row.names = 1, na = c("", "NA"))

#####################################################################
#####################################################################
#                         Data processing 
#####################################################################
#####################################################################


#cleaning column names 
weights <- weights %>% 
  rename_with(
    ~ tolower(             # make lowercase
      gsub(".",            # find . between column titles
           "_",            # replace . with underscore         
           .x,             # add placeholder for every col the df
           fixed = TRUE)   # pattern must match exactly
    ))

case_denom_data <- case_data_full <- case_denom_data %>% 
  rename_with(
    ~ tolower(             # make lowercase
      gsub(".",            # find . between column titles
           "_",            # replace . with underscore         
           .x,             # add placeholder for every col the df
           fixed = TRUE)   # pattern must match exactly
    )) %>%
  rename(prefstate = state, 
         ethrace = eth_race, 
         prefgender = gender)


#removing weights for those under 18 and extra cols 
#renaming race cols so they match 
weights <- weights %>%
  filter(age_group != "Under 18") %>%
  dplyr::select(-c("sample_total_denom", "acs_strata_denom", "weights_trim",
                   "acs_strata_denom","sample_strata_denom","acs_total_denom")) %>%
  mutate(ethrace = ifelse(ethrace == "African American", 
                          "Black or African American", ethrace))


#compiling unknown RaceEthn&gender b/c not recoverable in weighting 
U_O_cases <- case_denom_data %>% 
  filter(apply(case_denom_data, 1, 
               function(row) any(grepl('Unknown', row))))

#removing "DC" state in case data and "Unknown" Race and gender 
#(not recoverable). Also removing encounters and dropping NA
case_denom_data <- case_denom_data %>%
  filter(prefstate != "DC") %>%
  filter(!apply(.,1, function(row) any(grepl('Unknown', row)))) %>%
  dplyr::select(-c("nrwdenc", "n_fungalenc")) %>%
  drop_na()


#at this point, the weights and case denom data should be the same 

#add in in weights 
adult_weights <- df <- left_join(case_denom_data, 
                                 weights, by = c("year", "prefstate", 
                                                 "prefgender", "ethrace", 
                                                 "age_group")) %>%
  mutate(weighted_indiv = ceiling(n_rwdpts * weights), 
         weighted_cases = ceiling(n_fungal*weights)) 


## Weighted data summary 
state_all_year_cts_weight <- adult_weights %>%
  group_by(prefstate, year) %>%
  summarise(sum(n_fungal), sum(weighted_cases), sum(n_rwdpts), sum(weighted_indiv))


state_year_cts_weight <- adult_weights %>%
  group_by(prefstate) %>%
  summarise(
    sum_n_fungal = sum(n_fungal),
    sum_weighted_cases = sum(weighted_cases),
    sum_n_rwdpts = sum(n_rwdpts),
    sum_weighted_indiv = sum(weighted_indiv),
    .groups = "drop"
  ) %>%
  mutate(
    pct_n_fungal = sum_n_fungal / sum(sum_n_fungal) * 100,
    pct_weighted_cases = sum_weighted_cases / sum(sum_weighted_cases) * 100,
    pct_n_rwdpts = sum_n_rwdpts / sum(sum_n_rwdpts) * 100,
    pct_weighted_indiv = sum_weighted_indiv / sum(sum_weighted_indiv) * 100, 
    per_100k = (sum_weighted_cases / sum_weighted_indiv) * 100000
  )


state_cts_weight <- adult_weights %>%
  group_by(prefstate, year) %>%
  summarise(
    sum_n_fungal = sum(n_fungal),
    sum_weighted_cases = sum(weighted_cases),
    sum_n_rwdpts = sum(n_rwdpts),
    sum_weighted_indiv = sum(weighted_indiv),
    .groups = "drop") %>%
  mutate(
    pct_n_fungal = sum_n_fungal / sum(sum_n_fungal) * 100,
    pct_weighted_cases = sum_weighted_cases / sum(sum_weighted_cases) * 100,
    pct_n_rwdpts = sum_n_rwdpts / sum(sum_n_rwdpts) * 100,
    pct_weighted_indiv = sum_weighted_indiv / sum(sum_weighted_indiv) * 100, 
    per_100k = (sum_weighted_cases / sum_weighted_indiv) * 100000
  )

## UNweighted data summary 
state_all_year_cts <- state_all_year_cts <- adult_weights %>%
  group_by(prefstate, year) %>%
  summarise(sum(n_fungal), sum(n_rwdpts))


state_year_cts <- case_data_full %>%
  group_by(prefstate) %>%
  summarise(
    sum_n_fungal = sum(n_fungal),
    sum_n_rwdpts = sum(n_rwdpts),
    .groups = "drop"
  ) %>%
  mutate(
    pct_n_fungal = sum_n_fungal / sum(sum_n_fungal) * 100,
    pct_n_rwdpts = sum_n_rwdpts / sum(sum_n_rwdpts) * 100,
  ) 

#####################################################################
#####################################################################
#                         SIR Preparations  
#####################################################################
#####################################################################

### This should represent all possible patients/strata that could get a fungal infection
fullset <- expand.grid(
  unique(adult_weights$year),
  unique(adult_weights$prefstate),
  unique(adult_weights$ethrace),
  unique(adult_weights$prefgender),
  unique(adult_weights$age_group))

names(fullset) <- c('year','prefstate', 'ethrace' ,'prefgender','age_group')
fullset <- as.data.table(fullset) 
fullset %>% mutate(date = make_date(year, 1, 1))


##merging these groups that "should" exist if every year and 
#state had each demographic group 
adult_weights_full <- merge(fullset, adult_weights, by = c('year','prefstate', 'ethrace' ,'prefgender','age_group'), all.x = T)


## adding zeros for those that "should" exist 
adult_weights_full[is.na(adult_weights_full$n_rwdpts)]$n_rwdpts <- 0
adult_weights_full[is.na(adult_weights_full$n_fungal)]$n_fungal <- 0
adult_weights_full[is.na(adult_weights_full$weighted_indiv)]$weighted_indiv <- 0
adult_weights_full[is.na(adult_weights_full$weighted_cases)]$weighted_cases <- 0

#adding date to use as grouping column 
#renaming columns 
# dropping extra cols
#arranging to use expected() function
df_clean <- adult_weights_full %>% 
  mutate(date = make_date(year, 1, 1)) %>%
  rename(state = prefstate, 
         gender = prefgender, 
         denom = weighted_indiv, 
         cases = weighted_cases) %>%
  dplyr::select(-c("n_rwdpts", "weights", "n_fungal")) %>%
  arrange(state, year,  ethrace, gender, age_group)


# Define the number of strata
n.strata <- 72  # 6 ethrace, 6 age_group, 2 gender
cases <- df_clean$cases
population <- df_clean$denom



exp_cases <- expected(population,cases, n.strata)




####################################
# Prep for INLA 
####################################


#condensed table with expected cases added 
summary_df <- df_clean %>% 
  group_by(state, year) %>%
  summarize(
    sum_ppl = sum(denom), 
    sum_cases = sum(cases), 
    .groups = "drop"
  ) %>%
  arrange(state, year) %>%
  mutate(expected = exp_cases)


# Read in state shape files
# File can be downloaded from: https://www2.census.gov/geo/tiger/GENZ2021/shp/
states <- st_read("~/cb_2021_us_state_20m/cb_2021_us_state_20m.shp")



#removing Alaska and Hawaii (no neighbors) and teritorries 
states <- states %>%
  dplyr::filter(!STUSPS %in% c("AS", "MP" , "GU" , "PR" , "VI", "AK", "HI", "DC")) %>%
  arrange(GEOID)


#set adjacency matrix
state.adj <- poly2nb(states)
#setting queens matrix
W.state.rs <- nb2mat(state.adj, style = "W") 


#set col to merge on # convert to DF
states<- as.data.frame(states) %>%
  rename(state = STUSPS)

data_inla <- summary_df %>%
  filter(state != c("AK", "HI")) %>% #no adjacency with anything else 
  group_by(state, year) %>%
  arrange(state, year)

#merging states and data 
states <- merge(data_inla, states, by = 'state') %>%
  arrange(state, year)

#unique year ID creation 
year_id <- data.frame(year = sort(unique(states$year)), Y_ID = 1:11) 

#unique and ordered state IDS for adjacency matrix 
state_id <- data.frame(state = sort(unique(states$state)), S_ID = 1:length(unique(states$state)))


#merge ID year and ID state into states 
states <- merge(states, year_id, by = "year")
states <- merge(states, state_id, by = "state") 

#arrange states df by state and year 
states <- states2 <- states %>%
  arrange(state, year)

###############################################################################
###############################################################################
#####                    RINLA 
###############################################################################
###############################################################################


#spacio-tempral model 
set.seed(2025)
tic()
asp.st <- inla(sum_cases ~ 1 + 
                 f(Y_ID, model = "rw1") + 
                 f(as.numeric(S_ID), 
                   model = "bym", 
                   graph = W.state.rs) +
                 f(S_ID,Y_ID, model = "iid"),
               data = states, 
               E = expected, 
               family = "poisson",
               control.predictor = list(
                 compute = TRUE,
                 link = 1,
                 quantiles = c(0.0125, 0.025, 0.975, 0.9875)
               ),
               control.fixed = list(
                 quantiles = c(0.0125, 0.025, 0.975, 0.9875)
               ),
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

toc()
summary(asp.st)


##########. Plotting INLA model ############################

# Extract posterior summaries for each year and 
#add state and year ID
posterior.summary <- asp.st$summary.fitted.values %>%
  as.data.frame() %>%
  mutate(S_ID = rep(1:ceiling(n() / 11), each = 11, length.out = n()), 
         year = rep(2013:2023, length.out = n()))



# Merge posterior summaries with the spatial data
map_data <- left_join(states2, posterior.summary, by = c("S_ID", "year")) %>%
  mutate(year = as.factor(year))

map_data_sf <- st_as_sf(map_data) 

# Create the Main faceted map
map_plot <- ggplot(map_data_sf) +
  geom_sf(aes(fill = mean), color = "black", size = 0.2) +
  facet_wrap(~year, ncol = 4, scales = "fixed") +  
  scale_fill_distiller(
    palette = "RdYlBu", direction = -1, 
    limits = c(min(map_data_sf$mean, na.rm = TRUE), 2.5), 
    name = "aPR\n(Ref = 1)",
    values = scales::rescale(c(min(map_data_sf$mean, na.rm = TRUE), 1, 2.5))
  ) +
  labs(title = "State-Level Adjusted Prevalence Ratio of Aspergillosis Diagnosis in US Adults",
       subtitle = "Annual Estimates from 2013-2023",) +
  # caption = "Data source: [Your Data Source]") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")

# Combine both plots
final_plot <- ggdraw() +
  draw_plot(map_plot, 0, 0, 1, 1)     # Main map takes full space



final_plot

#ggsave("INLA_map_weighted.png", plot = final_plot, width = 10, height = 10, dpi = 600 )



##########. Total case counts map ########

states_all_cases <- adult_weights %>%
  rename( denom = n_rwdpts, 
          cases = n_fungal, 
          state = prefstate) %>%
  select(state, year, cases, denom) %>%
  group_by(state) %>%
  summarise(cases = sum(cases),
            denom = sum(denom)) %>%
  mutate(per_100k = cases/denom*100000)

# Plot total cases across states (with repositioned AK/HI)
case_map <- plot_usmap(data = states_all_cases, 
                       values = "cases", 
                       regions = "states", 
                       labels = F) +
  scale_fill_viridis_c(option = "magma",
                       direction = -1,
                       name = "Total Diagnoses"
                       
  ) +
  labs(
    title = "Total Aspergillosis Diagnoses by State (2013-2023)",
    subtitle = "Unweighted, raw counts"
    #caption = "Data: Cerner EHR" 
  ) +
  theme(
    legend.position = "right", 
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Show the map
case_map

#ggsave("case_map_2013_2023.png", case_map,bg = "white", width = 10, height = 6, dpi = 600)

#####
#model to extract national average aRR
#####

tic() #model with rw 1 (lowest DIC and WAIC vs )
asp.national.rw1 <- inla(sum_cases ~ 1 + 
                           f(Y_ID, model = "rw1"),
                         data = states, 
                         E = expected, 
                         family = "poisson",
                         control.predictor = list(
                           compute = TRUE,
                           link = 1,
                           quantiles = c(0.0125, 0.025, 0.975, 0.9875)
                         ),
                         control.fixed = list(
                           quantiles = c(0.0125, 0.025, 0.975, 0.9875)
                         ),
                         control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

toc()

summary(asp.national.rw1)


# extract quantiles and year ID 
posterior.summary.nat.rw1 <- asp.national.rw1$summary.fitted.values %>%
  as.data.frame() %>%
  mutate(year = as.factor(rep(2013:2023, length.out = n())), 
         State = "US")

posterior.summary.nat.rw1 



p.s.nat.rw1.top <- posterior.summary.nat.rw1 %>%
  slice_head(n=22) %>%
  select(c("year", "State", "mean", `0.0125quant`,`0.025quant`, `0.975quant`, `0.9875quant`)) %>%
  mutate(panel = rep(c("Highest 5 States", "Lowest 5 States"), each = 11))


########
#Summary Tables 
#######

# Process the data
summary_df2 <- map_data %>%
  arrange(state, year) %>%
  group_by(state) %>%
  select(c("year", "state", "mean", `0.0125quant`,`0.025quant`, `0.975quant`, `0.9875quant`)) %>%
  mutate(
    first_year_value = first(mean[year == 2013]),  # Mean in 2013
    last_year_value = first(mean[year == 2023]),   # Mean in 2023
    diff_2013_2023 = last_year_value - first_year_value,  # Absolute difference
    percent_change = (mean - lag(mean)) / lag(mean) * 100  # Yearly percent change
  ) %>%
  ungroup()

# Reshape to wide format
wide_df <- summary_df2 %>%
  select(state, year, percent_change) %>%
  pivot_wider(names_from = year, values_from = percent_change, names_prefix = "percent_change_") %>%
  left_join(
    summary_df2 %>%
      distinct(state, first_year_value, last_year_value, diff_2013_2023) %>%
      mutate(overall_percent_change = (last_year_value - first_year_value) / first_year_value * 100),
    by = "state"
  )


# Select 10 states to plot
selected_states <- map_data %>% #filtering for 5 highest and lowest
  filter(year == 2013) %>%
  arrange(desc(mean)) %>%
  reframe(
    state_names = c(head(state, 5), tail(state, 5))
  ) %>%
  pull(state_names)



# Add group labels for panels
state_groups <- tibble(
  state = selected_states,
  panel = rep(c("Highest 5 States", "Lowest 5 States"), each = 5)
)

# Filter data frame and assign panel group
df_filtered <- map_data %>%
  filter(state %in% selected_states) %>%
  left_join(state_groups, by = "state") %>%
  rename(State = state) %>%
  select(c("year", "State", "mean", `0.0125quant`,`0.025quant`, `0.975quant`, `0.9875quant`, "panel")) %>%
  bind_rows(p.s.nat.rw1.top)




# Relabel "US" to make it clearer in the legend
df_filtered <- df_filtered %>%
  mutate(State = ifelse(State == "US", "US Mean", State))

# Get state names
all_states <- unique(df_filtered$State)
non_us_states <- setdiff(all_states, "US Mean")

# Define custom palette (Paul Tol palette + black for US)
tol_colors <- c(
  "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
  "#44AA99", "#882255", "#999933", "#661100", "#6699CC"  # 11 colors for non-US states
)
us_color <- "#000000"
us_fill  <- "grey70"


# Assign colors to non-US states
state_colors <- setNames(tol_colors[seq_along(non_us_states)], non_us_states)

# Add US Mean
state_colors["US Mean"] <- us_color
state_fills <- state_colors
state_fills["US Mean"] <- us_fill

#####
#log transformed state graph 
#####

state_order <- df_filtered %>%
  filter(year == 2013) %>%
  group_by(State) %>%
  summarise(initial_mean = mean(mean, na.rm = TRUE)) %>%
  arrange(desc(initial_mean)) %>%
  pull(State)

df_filtered_log <- df_filtered %>%
  mutate(l_mean = log(mean), 
         l_0.025quant = log(`0.025quant`), 
         l_0.975quant = log(`0.975quant`), 
         State = factor(State, levels = state_order), 
         line_type = if_else(State == "US Mean", "dashed","solid"))

log_title_text <- glue("Log Scaled Adjusted Prevalence Ratio Over Time:\n States with the Highest and Lowest Initial aPR")

ggplot(df_filtered_log, aes(x = year, group = State)) +
  # US Mean Ribbon (mapped fill)
  geom_ribbon(
    data = df_filtered_log %>% filter(State == "US Mean"),
    aes(
      x = year,
      ymin = as.numeric(l_0.025quant),
      ymax = as.numeric(l_0.975quant),
      fill = State  # Mapped fill
    ),
    inherit.aes = T,
    alpha = 0.4,
    color = NA
  ) +
  
  # Lines for all states (including US Mean)
  geom_line(aes(y = l_mean, color = State,linetype = line_type), size = 0.5) +
  geom_point(aes(y = l_mean, color = State), size = 0.9) +
  
  labs(
    x = "Year", 
    #y = "log(aRR)",  un comment if you want 
    title = "Log Scaled Adjusted Prevalence Ratio Over Time",
    subtitle = "States with the Highest and Lowest Initial aPR",
    color = "State", 
    fill = "State"
  ) +
  
  # Custom color & fill scales
  scale_color_manual(values = c(state_colors, "US Mean" = "black")) +
  scale_fill_manual(
    name = "95% CrI",
    values = c(state_fills, "US Mean" = "grey70"),
    labels = "US Mean"
  ) +
  scale_y_continuous(
    name = "Adjusted Prevalence Ratio (aPR)",
    breaks = log(c(0.15,0.25, 0.5, 1, 2, 4, 8, 12)),  # positions on log scale
    labels = c(0.15,0.25, 0.5, 1, 2, 4, 8, 12)        # show unlogged values
  )+
  #facet_wrap(~panel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text=element_text(size=10), 
        axis.title.y=element_text(size=12,face="bold"), 
        axis.title.x=element_text(size=12, face="bold"), 
        legend.text=element_text(size=10), 
        plot.title=element_text(size=14, face="bold" ), 
        plot.subtitle=element_text(size=12)) +
  guides(linetype = "none")

