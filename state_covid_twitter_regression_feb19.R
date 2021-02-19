######################################################################################
#################################   Data Preparation   ###############################
######################################################################################

# set up packages
lapply(c("Hmisc", "arm", "readr", "tidyverse", "lme4", "stargazer", "texreg", "xtable",
         "interplot","glmmTMB", "dummies","effects", "ggthemes","caret", "ggeffects",'plm', "lmtest","scales","ggrepel","sandwich","reshape2"), require, character.only = TRUE)

# read respective data sets
setwd('/Users/taegyoon')
regression <- read_csv("Google Drive/state_covid_twitter/regression_Nov16_Feb1_merged.csv",
                       col_types = cols(X1 = col_skip())) # tweet data
pandemic <- read_csv("Google Drive/state_covid_twitter/11_16_2020_summary.csv") # state- and national-level pandemic statistics
policy <- read_csv("Google Drive/state_covid_twitter/mkdata-cusp02alt-totbyday.csv") # state covid policy variable
ideology <- read_csv("Google Drive/state_covid_twitter/df_ideology.csv") # legislator ideology variable
state <- read_csv("Google Drive/state_covid_twitter/state_data.csv") # other state-level data for robustness check
pop <- read_csv("Google Drive/state_covid_twitter/state_pop.csv") # other state-level data for robustness check

# pandemic group data 
pandemic$state_new_cases <- ifelse(pandemic$state_new_cases >= 0, pandemic$state_new_cases, 0)
pandemic$state_new_deaths <- ifelse(pandemic$state_new_deaths >= 0, pandemic$state_new_deaths, 0)
pandemic$state_new_hospitalizations <- ifelse(pandemic$state_new_hospitalizations >= 0, pandemic$state_new_hospitalizations, 0)
pandemic$national_new_cases <- ifelse(pandemic$national_new_cases >= 0, pandemic$national_new_cases, 0)
pandemic$national_new_deaths <- ifelse(pandemic$national_new_deaths >= 0, pandemic$national_new_deaths, 0)
pandemic$national_new_hospitalizations <- ifelse(pandemic$national_new_hospitalizations >= 0, pandemic$national_new_hospitalizations, 0)

table(pandemic$state_abbrev)
table(pandemic$party)
week_seq <- c(rep(5,2), rep(6:43, each=7))
week_seq_column <- rep(week_seq, times=150)
pandemic$week <- week_seq_column # week variable,,,

pandemic_grouped <- pandemic %>% 
  group_by(week, state_abbrev) %>% 
  summarise(national_case = sum(national_new_cases)/3,
            national_death = sum(national_new_deaths/3),
            national_hospitalization = sum(national_new_hospitalizations/3),
            state_case = sum(state_new_cases/3),
            state_death = sum(state_new_deaths)/3,
            state_hospitalization = sum(state_new_hospitalizations)/3) # aggregate

pandemic_grouped <- left_join(pandemic_grouped, pop, by = c('state_abbrev'))

# pop-normalized state-level pandemic variables
pandemic_grouped$state_case_pop <- pandemic_grouped$state_case / pandemic_grouped$pop * 10000
pandemic_grouped$state_hospitalization_pop <- pandemic_grouped$state_hospitalization / pandemic_grouped$pop * 10000
pandemic_grouped$state_death_pop <- pandemic_grouped$state_death / pandemic_grouped$pop * 10000

# pop-normalized national-level pandemic variables
pandemic_grouped$national_case_pop <- pandemic_grouped$national_case / 330656950 * 10000
pandemic_grouped$national_hospitalization_pop <- pandemic_grouped$national_hospitalization / 330656950 * 10000
pandemic_grouped$national_death_pop <- pandemic_grouped$national_death / 330656950 * 10000

# republican variable
regression$republican <- ifelse(regression$party=='R', 'Republican', 'Non-republican')

# join summary data
regression_final_full <- left_join(regression, pandemic_grouped, by = c('state_abbrev','week'))
regression_final_full <- subset(regression_final_full, select = -c(state.x,state.y))
col_order <- c("week", 
               "user.screen_name", 
               "state_abbrev",
               "party",
               'republican',
               "covid_relevant_1",
               "state_case",
               "state_death",             
               "state_hospitalization",
               "national_case",
               "national_death",
               "national_hospitalization",
               "state_case_pop",
               "state_death_pop",             
               "state_hospitalization_pop",
               "national_case_pop",
               "national_death_pop",
               "national_hospitalization_pop",
               "pop")
regression_final_full <- regression_final_full[, col_order]
regression_final_full <- regression_final_full[with(regression_final_full, order(week, state_abbrev)), ]

# truncate at week 14 (March 30)
regression_final <- regression_final_full[which(regression_final_full$week >= 14),]

# state policy variable
policy <- policy[which(policy$state!='DC'),]
table(policy$date)
policy$week <- rep(c(rep(1,5),rep(2:46, each=7), rep(47, 3)), length(unique(policy$state)))
policy <- policy[!grepl("jan2020", policy$date),]
policy_grouped <- policy %>% 
  group_by(week, state) %>% 
  summarise(state_covid_policy = sum(adoptions))
table(policy_grouped$state)
table(policy_grouped$week)
colnames(policy_grouped)[2] <- "state_abbrev"
regression_final <- left_join(regression_final, policy_grouped, by = c('state_abbrev','week'))

# legislator ideology variable
regression_final <- left_join(regression_final, ideology, by = c('user.screen_name'))

# state-level variables
regression_final <- left_join(regression_final, state, by = c('state_abbrev' = 'state'))

# standardizing state covid policy variable
regression_final$state_covid_policy_stan <- (regression_final$state_covid_policy - mean(regression_final$state_covid_policy)) / sd(regression_final$state_covid_policy)

# log-transforming pandemic variables
regression_final$state_covid_policy_log <- log(regression_final$state_covid_policy+1)
regression_final$state_case_log <- log(regression_final$state_case+1)
regression_final$state_death_log <- log(regression_final$state_death+1)
regression_final$state_hospitalization_log <- log(regression_final$state_hospitalization+1)
regression_final$national_case_log <- log(regression_final$national_case+1) 
regression_final$national_death_log <- log(regression_final$national_death+1)
regression_final$national_hospitalization_log <- log(regression_final$national_hospitalization+1)

# percentage change for all quantitative predictors
regression_final <- regression_final %>% 
  group_by(user.screen_name) %>% 
  mutate(state_case_change = (state_case+1)/(lag(state_case)+1),
         state_death_change = (state_death+1)/(lag(state_death)+1),
         state_hospitalization_change = (state_hospitalization+1)/(lag(state_hospitalization)+1),
         national_case_change = (national_case+1)/(lag(national_case)+1),
         national_death_change = (national_death+1)/(lag(national_death)+1),
         national_hospitalization_change = (national_hospitalization+1)/(lag(national_hospitalization)+1))

# chamber
file_names <- dir('/Users/taegyoon/Google Drive/state_covid_twitter/handles') # where you have your files
setwd('/Users/taegyoon/Google Drive/state_covid_twitter/handles')
handles <- do.call(rbind,lapply(file_names,read.csv))
handles$handle_1 <- tolower(handles$handle_1) 
handles$handle_2 <- tolower(handles$handle_2) 

handles_1 <- handles[c('handle_1', 'chamber')]
handles_1 <- handles_1[complete.cases(handles_1), ]
colnames(handles_1)[1] <- 'handle'
handles_2 <- handles[c('handle_2', 'chamber')]
handles_2 <- handles_2[complete.cases(handles_2), ]
colnames(handles_2)[1] <- 'handle'
handles_final <- rbind(handles_1, handles_2)
handles_final$handle <- tolower(handles_final$handle) 
regression_final <- left_join(regression_final, handles_final, by = c("user.screen_name" = "handle"))
table(regression_final$chamber)

# party control
regression_final$party_new <- ifelse(regression_final$party=='R', 'Republican', 
                                             ifelse(regression_final$party=='D', 'Democrat', 'Neither'))
regression_final$major_cham <- ifelse(regression_final$chamber=='S', regression_final$party_new==regression_final$s_control, regression_final$party_new==regression_final$h_control)
regression_final$major_cham <- ifelse(regression_final$leg_control=='Nonpartisan', NA, regression_final$major_cham)
regression_final_check <- regression_final %>% select(state_abbrev, chamber, party, party_new, leg_control, h_control, s_control, major_cham)

# legislator ideology standardization
regression_final$np_score_stan <- ifelse(is.na(regression_final$np_score)==TRUE,NA, 
                                                 (regression_final$np_score - mean(regression_final$np_score, na.rm = TRUE)) / sd(regression_final$np_score, na.rm = TRUE))

# logged DV
regression_final$covid_relevant_1_log <- log(regression_final$covid_relevant_1 + 1)

# remove Nebaraska rows
regression_final <- regression_final[which(regression_final$state_abbrev!='NE'),]

# new week column
regression_final$week_new <- regression_final$week - 13
regression_final$week_new <- as.integer(regression_final$week_new)
table(regression_final$week_new)


############################################################################################
#################################   Descriptive Statistics #################################   
############################################################################################

# legislator-week level variable: covid-tweets
summary(regression_final$covid_relevant_1_log)

# legislator level variable: partisan affiliation, ideology, party control
table(head(regression_final, length(unique(regression_final$user.screen_name)))$party)
table(head(regression_final, length(unique(regression_final$user.screen_name)))$republican)
table(head(regression_final, length(unique(regression_final$user.screen_name)))$major_cham)
summary(head(regression_final, length(unique(regression_final$user.screen_name)))$np_score)

# state-week level variable
summary(pandemic_grouped[which(pandemic_grouped$week>=14),][c('state_case', 'state_case_pop', 'state_death',  'state_death_pop')])
summary(policy_grouped[which(policy_grouped$week>=14),]$state_covid_policy)

# week level variable
pandemic_national <- pandemic_grouped %>% 
  group_by(week) %>% 
  summarise(national_case = sum(national_case)/50,
            national_case_pop = sum(national_case_pop)/50,
            national_death = sum(national_death)/50,
            national_death_pop = sum(national_death_pop)/50)
summary(pandemic_national[which(pandemic_national$week>=14),][c("national_case", 'national_case_pop','national_death','national_death_pop')])


# plots
des_rep_df <- data.frame(table(head(regression_final, length(unique(regression_final$user.screen_name)))$republican))
ggplot(data=des_rep_df, aes(x=Var1, y=Freq, fill = Var1)) + theme_gdocs() +
  geom_bar(stat="identity") +
  labs(title = "State Legislators' Partisan Affiliation \n",
       x = "Partisan Affiliation", y = "Frequency") +
  scale_fill_manual(values=c("skyblue","indianred"))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/bar_party.png')

des_control_df <- data.frame(table(head(regression_final, length(unique(regression_final$user.screen_name)))$major_cham))
des_control_df$Var1 <- c("Minority", "Majority")
ggplot(data=des_control_df, aes(x=Var1, y=Freq, fill = Var1)) + theme_gdocs() +
  geom_bar(stat="identity") +
  labs(title = "State Legislators' Majority Status in Chamber\n",
       x = "Majority Status in Chamber", y = "Frequency") +
  scale_fill_manual(values=c("gray","gray"))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/bar_chamber.png')

des_dv_plot <- ggplot(regression_final, aes(x=covid_relevant_1_log)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=0.5) + theme_gdocs() +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  labs(title = "Count of COVID-19 Relevant Tweets\n",
       x = "Logged Count of COVID-19 Relevant Tweets", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_dv_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_dv.png')

des_ideo_plot <- ggplot(head(regression_final, length(unique(regression_final$user.screen_name))), aes(x=np_score)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=0.3) + theme_gdocs() +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  labs(title = "Legislator Ideology Score\n",
       x = "Ideology", y = "Frequency",
       caption = "* Based on Berry et al. (1998)'s measure (lower values more conservative)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_ideo_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_ideo.png')

des_state_case_plot <- ggplot(pandemic_grouped[which(pandemic_grouped$week>=14),], aes(x=state_case_pop)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=5) + theme_gdocs() +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  labs(title = "State New Cases Per-10k\n",
       x = "New Cases Per-10k", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_state_case_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_state_case.png')

des_state_death_plot <- ggplot(pandemic_grouped[which(pandemic_grouped$week>=14),], aes(x=state_death_pop)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=0.1) + theme_gdocs() +
  #scale_x_continuous(breaks = pretty_breaks(6)) +
  labs(title = "State New Deaths Per-10k\n",
       x = "New Deaths Per-10k", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_state_death_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_state_death.png')

des_national_case_plot <- ggplot(pandemic_national[which(pandemic_national$week>=14),], aes(x=national_case_pop)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=1) + theme_gdocs() +
  #scale_x_continuous(breaks = pretty_breaks(6), limits = c(400, 1600)) +
  labs(title = "National New Cases Per-10k\n",
       x = "New Cases Per-10k", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_national_case_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_national_case.png')

des_national_death_plot <- ggplot(pandemic_national[which(pandemic_national$week>=14),], aes(x=national_death_pop)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=0.05) + theme_gdocs() +
  #scale_x_continuous(breaks = pretty_breaks(6)) +
  labs(title = "National New Deaths Per-10k\n",
       x = "New Deaths Per-10k", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_national_death_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_national_death.png')

des_state_policy_plot <- ggplot(policy_grouped[which(policy_grouped$week>=14),], aes(x=state_covid_policy)) + 
  geom_histogram(color="darkgray", fill="lightgray", binwidth=1) + theme_gdocs() +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  labs(title = "State New COVID-19 Policies\n",
       x = "Count of New Policies", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 13.5),
        axis.text.x= element_text(size = 12.5),
        axis.text.y= element_text(size = 12.5)) 
des_state_policy_plot
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/hist_state_policy.png')

# national case line plot
pandemic_national$date_start <- seq(as.Date("2020-1-27"), as.Date("2020-10-30"), by = "week")[1:39]
national_case_plot <- ggplot(tail(pandemic_national,30), aes(x=date_start, y=national_case)) + theme_gdocs() +
  geom_line() + 
  geom_point() + 
  ggtitle("National New Cases") +
  xlab("Week") + ylab("Count") +
  scale_x_date(date_labels = "%b", breaks = pretty_breaks(10))+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle=0, hjust = 1, size = 15),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        axis.text.y= element_text(size = 15))
print(national_case_plot)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/national_case_lineplot.png')
print(national_case_plot_pop)

# national death plot
national_death_plot <- ggplot(tail(pandemic_national,30), aes(x=date_start, y=national_death)) + theme_gdocs() +
  geom_line() + 
  geom_point() + 
  ggtitle("National New Deaths") +
  xlab("Week") + ylab("Count") +
  scale_x_date(date_labels = "%b", breaks = pretty_breaks(10))+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle=0, hjust = 1, size = 15),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        axis.text.y= element_text(size = 15))
print(national_death_plot)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/national_death_lineplot.png')

# state case heatmap
pandemic_grouped_march30 <- pandemic_grouped[which(pandemic_grouped$week>=14),]

pandemic_grouped_march30$date_start <- rep(seq(as.Date("2020-3-30"), as.Date("2020-10-19"), by='week'), each=50)
pandemic_grouped_march30$state_case_log <- log(pandemic_grouped_march30$state_case)

state_case_total <- pandemic_grouped_march30 %>% 
  group_by(state_abbrev) %>% 
  summarise(state_case_total = sum(state_case))

state_case_total <- state_case_total[order(state_case_total$state_case_total),] 
level_order_case <- as.vector(state_case_total$state_abbrev)

ggplot(pandemic_grouped_march30, aes(date_start,factor(state_abbrev, level = level_order_case))) + geom_tile(aes(fill = state_case_log), colour = "gray") +
  scale_x_date(date_labels = "%b %d", 
               breaks = as.Date(seq(as.Date("2020-3-30"), as.Date("2020-10-19"), "week")),
               expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 8) +  
  guides(fill=guide_legend(title="Logged New Cases")) +
  labs(title = "Trend of State New Cases",
       x = "\nWeek", y = "State\n") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 0),
        plot.title = element_text(hjust = 0.5)) 

# state death heatmap
pandemic_grouped_march30$state_death_log <- log(pandemic_grouped_march30$state_death + 1)

state_death_total <- pandemic_grouped_march30 %>% 
  group_by(state_abbrev) %>% 
  summarise(state_death_total = sum(state_death))

state_death_total <- state_death_total[order(state_death_total$state_death_total),] 
level_order_death <- as.vector(state_death_total$state_abbrev)

ggplot(pandemic_grouped_march30, aes(date_start,factor(state_abbrev, level = level_order_death))) + geom_tile(aes(fill = state_death_log), colour = "gray") +
  scale_x_date(date_labels = "%b %d", 
               breaks = as.Date(seq(as.Date("2020-3-30"), as.Date("2020-10-19"), "week")),
               expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 8) +  
  guides(fill=guide_legend(title="Logged New Deaths")) +
  labs(title = "Trend of State New Deaths",
       x = "\nWeek", y = "State\n") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 0),
        plot.title = element_text(hjust = 0.5)) 

# state case pop-normalized heatmap
state_case_total_pop <- pandemic_grouped_march30 %>% 
  group_by(state_abbrev) %>% 
  summarise(state_case_total = sum(state_case_pop))

state_case_total_pop <- state_case_total_pop[order(state_case_total_pop$state_case_total),] 
level_order_case_pop <- as.vector(state_case_total_pop$state_abbrev)

ggplot(pandemic_grouped_march30, aes(date_start,factor(state_abbrev, level = level_order_case_pop))) + geom_tile(aes(fill = state_case_pop), colour = "gray") +
  scale_x_date(date_labels = "%b %d", 
               breaks = as.Date(seq(as.Date("2020-3-30"), as.Date("2020-10-19"), "week")),
               expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 8) +  
  guides(fill=guide_legend(title="Per-million New Cases")) +
  labs(title = "Trend of State New Cases",
       x = "\nWeek", y = "State\n") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 0),
        plot.title = element_text(hjust = 0.5)) 
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/state_case_heatmap.png')

# state death pop-normalized heatmap
state_death_total_pop <- pandemic_grouped_march30 %>% 
  group_by(state_abbrev) %>% 
  summarise(state_death_total = sum(state_death_pop))

state_death_total_pop <- state_death_total_pop[order(state_death_total_pop$state_death_total),] 
level_order_death_pop <- as.vector(state_death_total_pop$state_abbrev)

ggplot(pandemic_grouped_march30, aes(date_start,factor(state_abbrev, level = level_order_death_pop))) + geom_tile(aes(fill = state_death_pop), colour = "gray") +
  scale_x_date(date_labels = "%b %d", 
               breaks = as.Date(seq(as.Date("2020-3-30"), as.Date("2020-10-19"), "week")),
               expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = "red", n.breaks = 8) +  
  guides(fill=guide_legend(title="Per-million New Deaths")) +
  labs(title = "Trend of State New Deaths",
       x = "\nWeek", y = "State\n") +
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 0),
        plot.title = element_text(hjust = 0.5)) 
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/state_death_heatmap.png')

# state individual plots
for (i in unique(pandemic_grouped_march30$state_abbrev)) {
  print(i)
  state_plot_df <- pandemic_grouped_march30[which(pandemic_grouped_march30$state_abbrev==i),]
  state_plot_df$date_start <- seq(as.Date("2020-3-30"), as.Date("2020-10-19"), by = "week")
  state_plot <- ggplot(state_plot_df, aes(x=date_start, y=state_case)) +
    geom_line() + 
    geom_point() + 
    ggtitle(paste0("State New Cases: ", i)) +
    xlab("Week") + ylab("Count") +
    scale_x_date(date_labels = "%b/%d", breaks = pretty_breaks(20))+
    scale_y_continuous(labels = comma) +
    theme(axis.text.x = element_text(angle=45, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  print(state_plot)}

# dv time trend plots
covid_weekly <- regression_final %>% 
  group_by(week) %>% 
  summarise(National = mean(covid_relevant_1))

covid_weekly_state <- regression_final %>% 
  group_by(week, state_abbrev) %>% 
  summarise(National = mean(covid_relevant_1))
covid_weekly_state_top_five <- covid_weekly_state[which(covid_weekly_state$state_abbrev=='CA' |
                                                          covid_weekly_state$state_abbrev=='TX' |
                                                          covid_weekly_state$state_abbrev=='FL' |
                                                          covid_weekly_state$state_abbrev=='NY' |
                                                          covid_weekly_state$state_abbrev=='IL'),]
covid_weekly_top_five <- covid_weekly_state_top_five %>% spread(state_abbrev, National) 

covid_count_df <- data.frame(cbind(covid_weekly, covid_weekly_top_five))
covid_count_df$date_start <- seq(as.Date("2020-3-30"), as.Date("2020-10-19"), by='week')

covid_count_df <- covid_count_df %>%
  select(date_start, National, CA, FL, IL, NY, TX) %>%
  gather(key = "level", value = "count", -date_start)

ggplot(covid_count_df, aes(x = date_start, y = count)) + 
  geom_line(aes(color = level)) + 
  ggtitle("Weekly COVID-19 Relevant Tweets") +
  labs(x = "Week",
       y = "Mean Count (per legislator)",
       color = "") +
  scale_x_date(date_labels = "%b", breaks = pretty_breaks(10))+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle=0, hjust = 1, size = 15),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),
        axis.text.y= element_text(size = 15)) +
  scale_color_manual(breaks = c("National","CA","FL","IL","NY","TX"),
                     values = c("black", "blue","orange","green",'purple','red'),
                     labels=c("National","CA","FL","IL","NY","TX"),name="")
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/covid_count.png')





############################################################################################################
################################## Conventional Approaches for Panel Data ################################## 
############################################################################################################

# set the panel structure
plm_df <- data.frame(regression_final)
plm_df <- pdata.frame(x = plm_df, index = c("user.screen_name", "week"))


# two-way RE + state FE models (pop-normalized)

twoway_re_state_fe_p1_pop <- plm(covid_relevant_1_log ~ 
                               state_case_pop + 
                               state_death_pop +
                               state_covid_policy_stan + 
                               republican +
                               state_abbrev,
                             data = plm_df,
                             index = c("user.screen_name", "week"),
                             model = 'random', effect = 'twoways')

twoway_re_state_fe_p2_pop <- plm(covid_relevant_1_log ~ 
                               national_case_pop + 
                               national_death_pop +
                               state_covid_policy_stan + 
                               republican +
                               state_abbrev,
                             data = plm_df,
                             index = c("user.screen_name", "week"),
                             model = 'random', effect = 'twoways')

twoway_re_state_fe_p3_pop <- plm(covid_relevant_1_log ~ 
                               state_case_pop + 
                               state_death_pop +
                               national_case_pop + 
                               national_death_pop +
                               state_covid_policy_stan + 
                               republican +
                               state_abbrev,
                             data = plm_df,
                             index = c("user.screen_name", "week"),
                             model = 'random', effect = 'twoways')

twoway_re_state_fe_p4_pop <- plm(covid_relevant_1_log ~ 
                               republican*state_case_pop + 
                               republican*state_death_pop +
                               republican*state_covid_policy_stan + 
                               state_abbrev,
                             data = plm_df,
                             index = c("user.screen_name", "week"),
                             model = 'random', effect = 'twoways')

twoway_re_state_fe_p5_pop <- plm(covid_relevant_1_log ~ 
                               republican*national_case_pop + 
                               republican*national_death_pop +
                               republican*state_covid_policy_stan + 
                               state_abbrev,
                             data = plm_df,
                             index = c("user.screen_name", "week"),
                             model = 'random', effect = 'twoways')

twoway_re_state_fe_full_pop <- plm(covid_relevant_1_log ~ 
                                 republican*state_case_pop + 
                                 republican*state_death_pop +
                                 republican*national_case_pop + 
                                 republican*national_death_pop +
                                 republican*state_covid_policy_stan + 
                                 state_abbrev,
                               data = plm_df,
                               index = c("user.screen_name", "week"),
                               model = 'random', effect = 'twoways')

twoway_re_state_fe_full_control_pop <- plm(covid_relevant_1_log ~ 
                                         republican*state_case_pop + 
                                         republican*state_death_pop +
                                         republican*national_case_pop + 
                                         republican*national_death_pop +
                                         republican*state_covid_policy_stan + 
                                         np_score +
                                         major_cham +
                                         state_abbrev,
                                       data = plm_df,
                                       index = c("user.screen_name", "week"),
                                       model = 'random', effect = 'twoways')

twoway_re_state_fe_p1_coef_pop <- coeftest(twoway_re_state_fe_p1_pop, vcovHC(twoway_re_state_fe_p1_pop, method="arellano"))
twoway_re_state_fe_p2_coef_pop <- coeftest(twoway_re_state_fe_p2_pop, vcovHC(twoway_re_state_fe_p2_pop, method="arellano"))
twoway_re_state_fe_p3_coef_pop <- coeftest(twoway_re_state_fe_p3_pop, vcovHC(twoway_re_state_fe_p3_pop, method="arellano"))
twoway_re_state_fe_p4_coef_pop <- coeftest(twoway_re_state_fe_p4_pop, vcovHC(twoway_re_state_fe_p4_pop, method="arellano"))
twoway_re_state_fe_p5_coef_pop <- coeftest(twoway_re_state_fe_p5_pop, vcovHC(twoway_re_state_fe_p5_pop, method="arellano"))
twoway_re_state_fe_full_coef_pop <- coeftest(twoway_re_state_fe_full_pop, vcovHC(twoway_re_state_fe_full_pop, method="arellano"))
twoway_re_state_fe_full_control_coef_pop <- coeftest(twoway_re_state_fe_full_control_pop, vcovHC(twoway_re_state_fe_full_control_pop, method="arellano"))

logLik.plm <- function(output){
  -(nobs(output) * log(2 * var(output$residuals) * pi))/2 - deviance(output)/(2 * var(output$residuals))}

lapply(list(twoway_re_state_fe_p1_pop, 
            twoway_re_state_fe_p2_pop, 
            twoway_re_state_fe_p3_pop, 
            twoway_re_state_fe_p4_pop, 
            twoway_re_state_fe_p5_pop, 
            twoway_re_state_fe_full_pop, 
            twoway_re_state_fe_full_control_pop), summary) 

lapply(list(twoway_re_state_fe_p1_pop, 
            twoway_re_state_fe_p2_pop,
            twoway_re_state_fe_p3_pop,
            twoway_re_state_fe_p4_pop,
            twoway_re_state_fe_p5_pop,
            twoway_re_state_fe_full_pop,
            twoway_re_state_fe_full_control_pop), logLik.plm)

texreg(list(twoway_re_state_fe_p1_pop,
            twoway_re_state_fe_p2_pop,
            twoway_re_state_fe_p3_pop,
            twoway_re_state_fe_p4_pop,
            twoway_re_state_fe_p5_pop,
            twoway_re_state_fe_full_pop,
            twoway_re_state_fe_full_control_pop),
       omit.coef = "abbrev",
       digits = 3,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'State COVID-19 Policies',
                             'Republican',
                             'National New Cases (10k)',
                             'National New Deaths (per 10k)',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Legislator Ideology',
                             'Chamber Majority Status'))

texreg(list(twoway_re_state_fe_p1_coef_pop,
            twoway_re_state_fe_p2_coef_pop,
            twoway_re_state_fe_p3_coef_pop,
            twoway_re_state_fe_p4_coef_pop,
            twoway_re_state_fe_p5_coef_pop,
            twoway_re_state_fe_full_coef_pop,
            twoway_re_state_fe_full_control_coef_pop),
       omit.coef = "abbrev",
       digits = 3,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'State COVID-19 Policies',
                             'Republican',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Legislator Ideology',
                             'Chamber Majority Status'))

texreg(twoway_re_state_fe_full_pop,
       omit.coef = "abbrev",
       digits = 3,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'Republican',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'State COVID-19 Policies',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies (stan)'))

texreg(twoway_re_state_fe_full_coef_pop,
       omit.coef = "abbrev",
       digits = 3,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'Republican',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'State COVID-19 Policies',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies (stan)'))


## two-way RE + state FE models (pop-normalized + week variable)

twoway_re_state_fe_p1_pop_week <- plm(covid_relevant_1_log ~ 
                                   state_case_pop + 
                                   state_death_pop +
                                   state_covid_policy_stan + 
                                   republican +
                                   state_abbrev +
                                   week_new,
                                 data = plm_df,
                                 index = c("user.screen_name", "week"),
                                 model = 'random', effect = 'twoways')

twoway_re_state_fe_p2_pop_week <- plm(covid_relevant_1_log ~ 
                                        national_case_pop + 
                                        national_death_pop +
                                        state_covid_policy_stan + 
                                        republican +
                                        state_abbrev +
                                        week_new,
                                      data = plm_df,
                                      index = c("user.screen_name", "week"),
                                      model = 'random', effect = 'twoways')

twoway_re_state_fe_p3_pop_week <- plm(covid_relevant_1_log ~ 
                                   state_case_pop + 
                                   state_death_pop +
                                   national_case_pop + 
                                   national_death_pop +
                                   state_covid_policy_stan + 
                                   republican +
                                   state_abbrev +
                                     week_new,
                                   data = plm_df,
                                   index = c("user.screen_name", "week"),
                                   model = 'random', effect = 'twoways')

twoway_re_state_fe_p4_pop_week <- plm(covid_relevant_1_log ~ 
                                   republican*state_case_pop + 
                                   republican*state_death_pop +
                                   republican*state_covid_policy_stan + 
                                   state_abbrev +
                                     as.numeric(week_new),
                                 data = plm_df,
                                 index = c("user.screen_name", "week_new"),
                                 model = 'random', effect = 'twoways')

twoway_re_state_fe_p5_pop_week <- plm(covid_relevant_1_log ~ 
                                   republican*national_case_pop + 
                                   republican*national_death_pop +
                                   republican*state_covid_policy_stan + 
                                   state_abbrev +
                                     week_new,
                                   data = plm_df,
                                   index = c("user.screen_name", "week"),
                                   model = 'random', effect = 'twoways')

twoway_re_state_fe_full_pop_week <- plm(covid_relevant_1_log ~ 
                                     republican*state_case_pop + 
                                     republican*state_death_pop +
                                     republican*national_case_pop + 
                                     republican*national_death_pop +
                                     republican*state_covid_policy_stan + 
                                     state_abbrev +
                                       week_new,
                                     data = plm_df,
                                     index = c("user.screen_name", "week"),
                                     model = 'random', effect = 'twoways')

twoway_re_state_fe_full_control_pop_week <- plm(covid_relevant_1_log ~ 
                                             republican*state_case_pop + 
                                             republican*state_death_pop +
                                             republican*national_case_pop + 
                                             republican*national_death_pop +
                                             republican*state_covid_policy_stan + 
                                             np_score +
                                             major_cham +
                                             state_abbrev +
                                               week_new,
                                             data = plm_df,
                                             index = c("user.screen_name", "week"),
                                             model = 'random', effect = 'twoways')

twoway_re_state_fe_p1_coef_pop_week <- coeftest(twoway_re_state_fe_p1_pop_week, vcovHC(twoway_re_state_fe_p1_pop_week, method="arellano"))
twoway_re_state_fe_p2_coef_pop_week <- coeftest(twoway_re_state_fe_p2_pop_week, vcovHC(twoway_re_state_fe_p2_pop_week, method="arellano"))
twoway_re_state_fe_p3_coef_pop_week <- coeftest(twoway_re_state_fe_p3_pop_week, vcovHC(twoway_re_state_fe_p3_pop_week, method="arellano"))
twoway_re_state_fe_p4_coef_pop_week <- coeftest(twoway_re_state_fe_p4_pop_week, vcovHC(twoway_re_state_fe_p4_pop_week, method="arellano"))
twoway_re_state_fe_p5_coef_pop_week <- coeftest(twoway_re_state_fe_p5_pop_week, vcovHC(twoway_re_state_fe_p5_pop_week, method="arellano"))
twoway_re_state_fe_full_coef_pop_week <- coeftest(twoway_re_state_fe_full_pop_week, vcovHC(twoway_re_state_fe_full_pop_week, method="arellano"))
twoway_re_state_fe_full_control_coef_pop_week <- coeftest(twoway_re_state_fe_full_control_pop_week, vcovHC(twoway_re_state_fe_full_control_pop_week, method="arellano"))

lapply(list(twoway_re_state_fe_p1_pop_week, 
            twoway_re_state_fe_p2_pop_week, 
            twoway_re_state_fe_p3_pop_week, 
            twoway_re_state_fe_p4_pop_week, 
            twoway_re_state_fe_p5_pop_week, 
            twoway_re_state_fe_full_pop_week, 
            twoway_re_state_fe_full_control_pop_week), summary) 

lapply(list(twoway_re_state_fe_p1_pop_week, 
            twoway_re_state_fe_p2_pop_week,
            twoway_re_state_fe_p3_pop_week,
            twoway_re_state_fe_p4_pop_week,
            twoway_re_state_fe_p5_pop_week,
            twoway_re_state_fe_full_pop_week,
            twoway_re_state_fe_full_control_pop_week), logLik.plm)

texreg(list(twoway_re_state_fe_p1_pop_week,
            twoway_re_state_fe_p2_pop_week,
            twoway_re_state_fe_p3_pop_week,
            twoway_re_state_fe_p4_pop_week,
            twoway_re_state_fe_p5_pop_week,
            twoway_re_state_fe_full_pop_week,
            twoway_re_state_fe_full_control_pop_week),
       omit.coef = "abbrev",
       digits = 3,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'State COVID-19 Policies (stan)',
                             'Republican',
                             'Week',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Legislator Ideology',
                             'Chamber Majority Status'))

texreg(list(twoway_re_state_fe_p1_coef_pop_week,
            twoway_re_state_fe_p2_coef_pop_week,
            twoway_re_state_fe_p3_coef_pop_week,
            twoway_re_state_fe_p4_coef_pop_week,
            twoway_re_state_fe_p5_coef_pop_week,
            twoway_re_state_fe_full_coef_pop_week,
            twoway_re_state_fe_full_control_coef_pop_week),
       omit.coef = "abbrev",
       digits = 3,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'State COVID-19 Policies (stan)',
                             'Republican',
                             'Week',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Legislator Ideology',
                             'Chamber Majority Status'))

texreg(twoway_re_state_fe_full_pop_week,
       omit.coef = "abbrev",
       digits = 4,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'Republican',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'State COVID-19 Policies (stan)',
                             'Week',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies'))

texreg(twoway_re_state_fe_full_coef_pop_week,
       omit.coef = "abbrev",
       digits = 4,
       caption = "Panel Regression Model (State Fixed Effect and Legislator-week Random Effect): State Population Normalized",
       custom.coef.names = c('(Intercept)',
                             'Republican',
                             'State New Cases (per 10k)',
                             'State New Deaths (per 10k)',
                             'National New Cases (per 10k)',
                             'National New Deaths (per 10k)',
                             'State COVID-19 Policies (stan)',
                             'Week',
                             'Republican * State New Cases (per 10k)',
                             'Republican * State New Deaths (per 10k)',
                             'Republican * National New Cases (per 10k)',
                             'Republican * National New Deaths (per 10k)',
                             'Republican * State COVID-19 Policies'))


##################################################################################################
#######################################   Panel Prediction   #####################################
##################################################################################################

# variance-covariance matrix

vcov <- vcovHC(twoway_re_state_fe_full_pop_week, method="arellano")

# state case

state_case_values <- seq(round(min(regression_final$state_case_pop),1),
                         round(max(regression_final$state_case_pop),1),
                         0.1)
state_case_pop_effect <- Effect(focal.predictors = c("state_case_pop","republican"), 
                                mod = twoway_re_state_fe_full_pop_week,
                                xlevels = list(state_case_pop = state_case_values),
                                given.values=c(
                                  state_abbrevCA = 0,
                                  state_abbrevTX = 0,
                                  state_abbrevAZ = 0,
                                  state_abbrevNY = 0,
                                  state_abbrevPA = 0,
                                  state_abbrevMA = 0,
                                  state_abbrevTN = 0,
                                  state_abbrevNM = 0,
                                  state_abbrevFL = 0,
                                  state_abbrevVA = 0,
                                  state_abbrevNC = 0,
                                  state_abbrevMN = 0,
                                  state_abbrevMD = 0,
                                  state_abbrevUT = 0,
                                  state_abbrevMI = 0,
                                  state_abbrevOH = 0,
                                  state_abbrevIA = 0,
                                  state_abbrevMO = 0,
                                  state_abbrevNJ = 0,
                                  state_abbrevNH = 0,
                                  state_abbrevWI = 0,
                                  state_abbrevAR = 0,
                                  state_abbrevOR = 0,
                                  state_abbrevGA = 0,
                                  state_abbrevCO = 1,
                                  state_abbrevSC = 0,
                                  state_abbrevCT = 0,
                                  state_abbrevOK = 0,
                                  state_abbrevAL = 0,
                                  state_abbrevDE = 0,
                                  state_abbrevNV = 0,
                                  state_abbrevKY = 0,
                                  state_abbrevIN = 0,
                                  state_abbrevND = 0,
                                  state_abbrevIL = 0,
                                  state_abbrevKS = 0,
                                  state_abbrevWA = 0,
                                  state_abbrevMS = 0,
                                  state_abbrevWV = 0,
                                  state_abbrevLA = 0,
                                  state_abbrevRI = 0,
                                  state_abbrevSD = 0,
                                  state_abbrevWY = 0,
                                  state_abbrevID = 0,
                                  state_abbrevHI = 0,
                                  state_abbrevMT = 0,
                                  state_abbrevME = 0,
                                  state_abbrevVT = 0,
                                  state_death_pop = median(regression_final$state_death_pop),
                                  national_case_pop = median(regression_final$national_case_pop),
                                  national_death_pop = median(regression_final$national_death_pop),
                                  state_covid_policy_stan = median(regression_final$state_covid_policy_stan),
                                  week_new = median(regression_final$week_new)
                                  ))

state_case_me_se_non_rep <- sqrt(vcov[3,3] 
                                 + (0)^2*vcov[57,57] + 
                                   2*0*vcov[3,57]) # SE for marginal effect of predictor
state_case_me_se_rep <- sqrt(vcov[3,3] 
                             + (1)^2*vcov[57,57] + 
                               2*1*vcov[3,57]) # SE for marginal effect of predictor

state_case_me_ci_non_rep <- cbind(state_case_values * (1.96*state_case_me_se_non_rep), state_case_values * -(1.96*state_case_me_se_non_rep))
state_case_me_ci_rep <- cbind(state_case_values * (1.96*state_case_me_se_rep), state_case_values * -(1.96*state_case_me_se_rep))

state_case_preds <- state_case_pop_effect$fit
state_case_upper <- state_case_pop_effect$fit + c(state_case_me_ci_non_rep[,1], state_case_me_ci_rep[,1])
state_case_lower <- state_case_pop_effect$fit + c(state_case_me_ci_non_rep[,2], state_case_me_ci_rep[,2])
state_case_final <- data.frame(Party = c(rep('Non-Republican',length(state_case_values)), rep('Republican',length(state_case_values))), 
                               X = as.numeric(state_case_values), 
                               Y = as.numeric(state_case_preds), 
                               U = as.numeric(state_case_upper), 
                               L = as.numeric(state_case_lower),
                               Y_linear = exp(as.numeric(state_case_preds))-1, 
                               U_linear = exp(as.numeric(state_case_upper))-1, 
                               L_linear = exp(as.numeric(state_case_lower))-1)

state_case_final$Party <- as.factor(state_case_final$Party)
state_case_final$Party <- factor(state_case_final$Party, levels = rev(levels(state_case_final$Party)))

ggplot(state_case_final, aes(x = X, y = Y_linear, color = Party)) + theme_gdocs() +
  theme(text = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") +
  geom_line() +
  #scale_x_continuous(limits = c(0, 30)) +
  #scale_y_continuous(limits = c(0, 100)) +
  scale_color_manual(values = c("darkred", "dodgerblue4")) +
  geom_ribbon(aes(ymin = L_linear, ymax = U_linear), linetype=2, alpha=0.1) +
  ggtitle("Weekly State New Cases") +
  xlab("Weekly Count of State New Cases (per 10k)") + ylab("Count of COVID-19 Tweets") +
  geom_rug(data=regression_final, aes(x = state_case_pop), inherit.aes = FALSE, color = 'gray60') +
  scale_shape_cleveland(overlap=FALSE)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/plm_plot_state_case_12Feb.png')


# state death

state_death_values <- seq(round(min(regression_final$state_death_pop),1),
                          round(max(regression_final$state_death_pop),1),
                          0.1)
state_death_pop_effect <- Effect(focal.predictors = c("state_death_pop","republican"), 
                                 mod = twoway_re_state_fe_full_pop_week,
                                 xlevels = list(state_death_pop = state_death_values),
                                 given.values=c(
                                   state_abbrevCA = 0,
                                   state_abbrevTX = 0,
                                   state_abbrevAZ = 0,
                                   state_abbrevNY = 0,
                                   state_abbrevPA = 0,
                                   state_abbrevMA = 0,
                                   state_abbrevTN = 0,
                                   state_abbrevNM = 0,
                                   state_abbrevFL = 0,
                                   state_abbrevVA = 0,
                                   state_abbrevNC = 0,
                                   state_abbrevMN = 0,
                                   state_abbrevMD = 0,
                                   state_abbrevUT = 0,
                                   state_abbrevMI = 0,
                                   state_abbrevOH = 0,
                                   state_abbrevIA = 0,
                                   state_abbrevMO = 0,
                                   state_abbrevNJ = 0,
                                   state_abbrevNH = 0,
                                   state_abbrevWI = 0,
                                   state_abbrevAR = 0,
                                   state_abbrevOR = 0,
                                   state_abbrevGA = 0,
                                   state_abbrevCO = 1,
                                   state_abbrevSC = 0,
                                   state_abbrevCT = 0,
                                   state_abbrevOK = 0,
                                   state_abbrevAL = 0,
                                   state_abbrevDE = 0,
                                   state_abbrevNV = 0,
                                   state_abbrevKY = 0,
                                   state_abbrevIN = 0,
                                   state_abbrevND = 0,
                                   state_abbrevIL = 0,
                                   state_abbrevKS = 0,
                                   state_abbrevWA = 0,
                                   state_abbrevMS = 0,
                                   state_abbrevWV = 0,
                                   state_abbrevLA = 0,
                                   state_abbrevRI = 0,
                                   state_abbrevSD = 0,
                                   state_abbrevWY = 0,
                                   state_abbrevID = 0,
                                   state_abbrevHI = 0,
                                   state_abbrevMT = 0,
                                   state_abbrevME = 0,
                                   state_abbrevVT = 0,
                                   state_case_pop = median(regression_final$state_case_pop),
                                   national_case_pop = median(regression_final$national_case_pop),
                                   national_death_pop = median(regression_final$national_death_pop),
                                   state_covid_policy_stan = median(regression_final$state_covid_policy_stan),
                                   week_new = median(regression_final$week_new)
                                 ))

state_death_me_se_non_rep <- sqrt(vcov[4,4] 
                                  + (0)^2*vcov[58,58] + 
                                    2*0*vcov[4,58]) # SE for marginal effect of predictor
state_death_me_se_rep <- sqrt(vcov[4,4] 
                              + (1)^2*vcov[58,58] + 
                                2*1*vcov[4,58]) # SE for marginal effect of predictor

state_death_me_ci_non_rep <- cbind(state_death_values * (1.96*state_death_me_se_non_rep), state_death_values * -(1.96*state_death_me_se_non_rep))
state_death_me_ci_rep <- cbind(state_death_values * (1.96*state_death_me_se_rep), state_death_values * -(1.96*state_death_me_se_rep))

state_death_preds <- state_death_pop_effect$fit
state_death_upper <- state_death_pop_effect$fit + c(state_death_me_ci_non_rep[,1], state_death_me_ci_rep[,1])
state_death_lower <- state_death_pop_effect$fit + c(state_death_me_ci_non_rep[,2], state_death_me_ci_rep[,2])
state_death_final <- data.frame(Party = c(rep('Non-Republican',length(state_death_values)), rep('Republican',length(state_death_values))), 
                                X = as.numeric(state_death_values), 
                                Y = as.numeric(state_death_preds), 
                                U = as.numeric(state_death_upper), 
                                L = as.numeric(state_death_lower),
                                Y_linear = exp(as.numeric(state_death_preds))-1, 
                                U_linear = exp(as.numeric(state_death_upper))-1, 
                                L_linear = exp(as.numeric(state_death_lower))-1)

state_death_final$Party <- as.factor(state_death_final$Party)
state_death_final$Party <- factor(state_death_final$Party, levels = rev(levels(state_death_final$Party)))

ggplot(state_death_final, aes(x = X, y = Y_linear, color = Party)) + theme_gdocs() +
  theme(text = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") +
  geom_line() +
#  scale_x_continuous(limits = c(0, 1.5)) +
  scale_color_manual(values = c("darkred", "dodgerblue4")) +
  geom_ribbon(aes(ymin = L_linear, ymax = U_linear), linetype=2, alpha=0.1) +
  ggtitle("Weekly State New deaths") +
  xlab("Weekly Count of State New Deaths (per 10k)") + ylab("Count of COVID-19 Tweets") +
  geom_rug(data=regression_final, aes(x = state_death_pop), inherit.aes = FALSE, color = 'gray60') +
  scale_shape_cleveland(overlap=FALSE)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/plm_plot_state_death_12Feb.png')



# national case

national_case_values <- seq(round(min(regression_final$national_case_pop),1),
                         round(max(regression_final$national_case_pop),1),
                         0.1)
national_case_pop_effect <- Effect(focal.predictors = c("national_case_pop","republican"), 
                                mod = twoway_re_state_fe_full_pop_week,
                                xlevels = list(national_case_pop = national_case_values),
                                given.values=c(
                                  state_abbrevCA = 0,
                                  state_abbrevTX = 0,
                                  state_abbrevAZ = 0,
                                  state_abbrevNY = 0,
                                  state_abbrevPA = 0,
                                  state_abbrevMA = 0,
                                  state_abbrevTN = 0,
                                  state_abbrevNM = 0,
                                  state_abbrevFL = 0,
                                  state_abbrevVA = 0,
                                  state_abbrevNC = 0,
                                  state_abbrevMN = 0,
                                  state_abbrevMD = 0,
                                  state_abbrevUT = 0,
                                  state_abbrevMI = 0,
                                  state_abbrevOH = 0,
                                  state_abbrevIA = 0,
                                  state_abbrevMO = 0,
                                  state_abbrevNJ = 0,
                                  state_abbrevNH = 0,
                                  state_abbrevWI = 0,
                                  state_abbrevAR = 0,
                                  state_abbrevOR = 0,
                                  state_abbrevGA = 0,
                                  state_abbrevCO = 1,
                                  state_abbrevSC = 0,
                                  state_abbrevCT = 0,
                                  state_abbrevOK = 0,
                                  state_abbrevAL = 0,
                                  state_abbrevDE = 0,
                                  state_abbrevNV = 0,
                                  state_abbrevKY = 0,
                                  state_abbrevIN = 0,
                                  state_abbrevND = 0,
                                  state_abbrevIL = 0,
                                  state_abbrevKS = 0,
                                  state_abbrevWA = 0,
                                  state_abbrevMS = 0,
                                  state_abbrevWV = 0,
                                  state_abbrevLA = 0,
                                  state_abbrevRI = 0,
                                  state_abbrevSD = 0,
                                  state_abbrevWY = 0,
                                  state_abbrevID = 0,
                                  state_abbrevHI = 0,
                                  state_abbrevMT = 0,
                                  state_abbrevME = 0,
                                  state_abbrevVT = 0,
                                  state_case_pop = median(regression_final$state_case_pop),
                                  state_death_pop = median(regression_final$state_death_pop),
                                  national_death_pop = median(regression_final$national_death_pop),
                                  state_covid_policy_stan = median(regression_final$state_covid_policy_stan),
                                  week_new = median(regression_final$week_new)
                                ))

national_case_me_se_non_rep <- sqrt(vcov[5,5] 
                                 + (0)^2*vcov[59,59] + 
                                   2*0*vcov[5,59]) # SE for marginal effect of predictor
national_case_me_se_rep <- sqrt(vcov[5,5] 
                             + (1)^2*vcov[59,59] + 
                               2*1*vcov[5,59]) # SE for marginal effect of predictor

national_case_me_ci_non_rep <- cbind(national_case_values * (1.96*national_case_me_se_non_rep), national_case_values * -(1.96*national_case_me_se_non_rep))
national_case_me_ci_rep <- cbind(national_case_values * (1.96*national_case_me_se_rep), national_case_values * -(1.96*national_case_me_se_rep))

national_case_preds <- national_case_pop_effect$fit
national_case_upper <- national_case_pop_effect$fit + c(national_case_me_ci_non_rep[,1], national_case_me_ci_rep[,1])
national_case_lower <- national_case_pop_effect$fit + c(national_case_me_ci_non_rep[,2], national_case_me_ci_rep[,2])
national_case_final <- data.frame(Party = c(rep('Non-Republican',length(national_case_values)), rep('Republican',length(national_case_values))), 
                               X = as.numeric(national_case_values), 
                               Y = as.numeric(national_case_preds), 
                               U = as.numeric(national_case_upper), 
                               L = as.numeric(national_case_lower),
                               Y_linear = exp(as.numeric(national_case_preds))-1, 
                               U_linear = exp(as.numeric(national_case_upper))-1, 
                               L_linear = exp(as.numeric(national_case_lower))-1)

national_case_final$Party <- as.factor(national_case_final$Party)
national_case_final$Party <- factor(national_case_final$Party, levels = rev(levels(national_case_final$Party)))

ggplot(national_case_final, aes(x = X, y = Y_linear, color = Party)) + theme_gdocs() +
  theme(text = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") +
  geom_line() +
  #scale_x_continuous(limits = c(0, 30)) +
  #scale_y_continuous(limits = c(0, 100)) +
  scale_color_manual(values = c("darkred", "dodgerblue4")) +
  geom_ribbon(aes(ymin = L_linear, ymax = U_linear), linetype=2, alpha=0.1) +
  ggtitle("Weekly National New Cases") +
  xlab("Weekly Count of National New Cases (per 10k)") + ylab("Count of COVID-19 Tweets") +
  geom_rug(data=regression_final, aes(x = national_case_pop), inherit.aes = FALSE, color = 'gray60') +
  scale_shape_cleveland(overlap=FALSE)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/plm_plot_national_case_12Feb.png')



# national death

national_death_values <- seq(round(min(regression_final$national_death_pop),1),
                             round(max(regression_final$national_death_pop),1),
                             0.001)
national_death_pop_effect <- Effect(focal.predictors = c("national_death_pop","republican"), 
                                    mod = twoway_re_state_fe_full_pop_week,
                                    xlevels = list(national_death_pop = national_death_values),
                                    given.values=c(
                                      state_abbrevCA = 0,
                                      state_abbrevTX = 0,
                                      state_abbrevAZ = 0,
                                      state_abbrevNY = 0,
                                      state_abbrevPA = 0,
                                      state_abbrevMA = 0,
                                      state_abbrevTN = 0,
                                      state_abbrevNM = 0,
                                      state_abbrevFL = 0,
                                      state_abbrevVA = 0,
                                      state_abbrevNC = 0,
                                      state_abbrevMN = 0,
                                      state_abbrevMD = 0,
                                      state_abbrevUT = 0,
                                      state_abbrevMI = 0,
                                      state_abbrevOH = 0,
                                      state_abbrevIA = 0,
                                      state_abbrevMO = 0,
                                      state_abbrevNJ = 0,
                                      state_abbrevNH = 0,
                                      state_abbrevWI = 0,
                                      state_abbrevAR = 0,
                                      state_abbrevOR = 0,
                                      state_abbrevGA = 0,
                                      state_abbrevCO = 1,
                                      state_abbrevSC = 0,
                                      state_abbrevCT = 0,
                                      state_abbrevOK = 0,
                                      state_abbrevAL = 0,
                                      state_abbrevDE = 0,
                                      state_abbrevNV = 0,
                                      state_abbrevKY = 0,
                                      state_abbrevIN = 0,
                                      state_abbrevND = 0,
                                      state_abbrevIL = 0,
                                      state_abbrevKS = 0,
                                      state_abbrevWA = 0,
                                      state_abbrevMS = 0,
                                      state_abbrevWV = 0,
                                      state_abbrevLA = 0,
                                      state_abbrevRI = 0,
                                      state_abbrevSD = 0,
                                      state_abbrevWY = 0,
                                      state_abbrevID = 0,
                                      state_abbrevHI = 0,
                                      state_abbrevMT = 0,
                                      state_abbrevME = 0,
                                      state_abbrevVT = 0,
                                      state_case_pop = median(regression_final$state_case_pop),
                                      state_death_pop = median(regression_final$state_death_pop),
                                      national_case_pop = median(regression_final$national_case_pop),
                                      state_covid_policy_stan = median(regression_final$state_covid_policy_stan),
                                      week_new = median(regression_final$week_new)
                                    ))

national_death_me_se_non_rep <- sqrt(vcov[6,6] 
                                     + (0)^2*vcov[60,60] + 
                                       2*0*vcov[6,60]) # SE for marginal effect of predictor
national_death_me_se_rep <- sqrt(vcov[6,6] 
                                 + (1)^2*vcov[60,60] + 
                                   2*1*vcov[6,60]) # SE for marginal effect of predictor

national_death_me_ci_non_rep <- cbind(national_death_values * (1.96*national_death_me_se_non_rep), national_death_values * -(1.96*national_death_me_se_non_rep))
national_death_me_ci_rep <- cbind(national_death_values * (1.96*national_death_me_se_rep), national_death_values * -(1.96*national_death_me_se_rep))

national_death_preds <- national_death_pop_effect$fit
national_death_upper <- national_death_pop_effect$fit + c(national_death_me_ci_non_rep[,1], national_death_me_ci_rep[,1])
national_death_lower <- national_death_pop_effect$fit + c(national_death_me_ci_non_rep[,2], national_death_me_ci_rep[,2])
national_death_final <- data.frame(Party = c(rep('Non-Republican',length(national_death_values)), rep('Republican',length(national_death_values))), 
                                   X = as.numeric(national_death_values), 
                                   Y = as.numeric(national_death_preds), 
                                   U = as.numeric(national_death_upper), 
                                   L = as.numeric(national_death_lower),
                                   Y_linear = exp(as.numeric(national_death_preds))-1, 
                                   U_linear = exp(as.numeric(national_death_upper))-1, 
                                   L_linear = exp(as.numeric(national_death_lower))-1)

national_death_final$Party <- as.factor(national_death_final$Party)
national_death_final$Party <- factor(national_death_final$Party, levels = rev(levels(national_death_final$Party)))

ggplot(national_death_final, aes(x = X, y = Y_linear, color = Party)) + theme_gdocs() +
  theme(text = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") +
  geom_line() +
#  scale_x_continuous(limits = c(0.1, 0.4)) +
#  scale_y_continuous(limits = c(0, 1250)) +
  scale_color_manual(values = c("darkred", "dodgerblue4")) +
  geom_ribbon(aes(ymin = L_linear, ymax = U_linear), linetype=2, alpha=0.1) +
  ggtitle("Weekly National New deaths") +
  xlab("Weekly Count of National New Deaths (per 10k)") + ylab("Count of COVID-19 Tweets") +
  geom_rug(data=regression_final, aes(x = national_death_pop), inherit.aes = FALSE, color = 'gray60') +
  scale_shape_cleveland(overlap=FALSE)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/plm_plot_national_death_12Feb.png')



# state policy 

state_policy_values <- seq(round(min(regression_final$state_covid_policy_stan),1),
                           round(max(regression_final$state_covid_policy_stan),1),
                           0.01)
state_policy_stan_effect <- Effect(focal.predictors = c("state_covid_policy_stan","republican"), 
                                   mod = twoway_re_state_fe_full_pop_week,
                                   xlevels = list(state_covid_policy_stan = state_policy_values),
                                   given.values=c(
                                     state_abbrevCA = 0,
                                     state_abbrevTX = 0,
                                     state_abbrevAZ = 0,
                                     state_abbrevNY = 0,
                                     state_abbrevPA = 0,
                                     state_abbrevMA = 0,
                                     state_abbrevTN = 0,
                                     state_abbrevNM = 0,
                                     state_abbrevFL = 0,
                                     state_abbrevVA = 0,
                                     state_abbrevNC = 0,
                                     state_abbrevMN = 0,
                                     state_abbrevMD = 0,
                                     state_abbrevUT = 0,
                                     state_abbrevMI = 0,
                                     state_abbrevOH = 0,
                                     state_abbrevIA = 0,
                                     state_abbrevMO = 0,
                                     state_abbrevNJ = 0,
                                     state_abbrevNH = 0,
                                     state_abbrevWI = 0,
                                     state_abbrevAR = 0,
                                     state_abbrevOR = 0,
                                     state_abbrevGA = 0,
                                     state_abbrevCO = 1,
                                     state_abbrevSC = 0,
                                     state_abbrevCT = 0,
                                     state_abbrevOK = 0,
                                     state_abbrevAL = 0,
                                     state_abbrevDE = 0,
                                     state_abbrevNV = 0,
                                     state_abbrevKY = 0,
                                     state_abbrevIN = 0,
                                     state_abbrevND = 0,
                                     state_abbrevIL = 0,
                                     state_abbrevKS = 0,
                                     state_abbrevWA = 0,
                                     state_abbrevMS = 0,
                                     state_abbrevWV = 0,
                                     state_abbrevLA = 0,
                                     state_abbrevRI = 0,
                                     state_abbrevSD = 0,
                                     state_abbrevWY = 0,
                                     state_abbrevID = 0,
                                     state_abbrevHI = 0,
                                     state_abbrevMT = 0,
                                     state_abbrevME = 0,
                                     state_abbrevVT = 0,
                                     state_case_pop = median(regression_final$state_case_pop),
                                     state_death_pop = median(regression_final$state_death_pop),
                                     national_case_pop = median(regression_final$national_case_pop),
                                     national_death_pop = median(regression_final$national_death_pop),
                                     week_new = median(regression_final$week_new)
                                   ))

state_policy_me_se_non_rep <- sqrt(vcov[7,7] 
                                   + (0)^2*vcov[61,61] + 
                                     2*0*vcov[7,61]) # SE for marginal effect of predictor
state_policy_me_se_rep <- sqrt(vcov[7,7] 
                               + (1)^2*vcov[61,61] + 
                                 2*1*vcov[7,61]) # SE for marginal effect of predictor

state_policy_me_ci_non_rep <- cbind(state_policy_values * (1.96*state_policy_me_se_non_rep), state_policy_values * -(1.96*state_policy_me_se_non_rep))
state_policy_me_ci_rep <- cbind(state_policy_values * (1.96*state_policy_me_se_rep), state_policy_values * -(1.96*state_policy_me_se_rep))

state_policy_preds <- state_policy_stan_effect$fit
state_policy_upper <- state_policy_stan_effect$fit + c(state_policy_me_ci_non_rep[,1], state_policy_me_ci_rep[,1])
state_policy_lower <- state_policy_stan_effect$fit + c(state_policy_me_ci_non_rep[,2], state_policy_me_ci_rep[,2])
state_policy_final <- data.frame(Party = c(rep('Non-Republican',length(state_policy_values)), rep('Republican',length(state_policy_values))), 
                                 X = as.numeric(state_policy_values), 
                                 Y = as.numeric(state_policy_preds), 
                                 U = as.numeric(state_policy_upper), 
                                 L = as.numeric(state_policy_lower),
                                 Y_linear = exp(as.numeric(state_policy_preds))-1, 
                                 U_linear = exp(as.numeric(state_policy_upper))-1, 
                                 L_linear = exp(as.numeric(state_policy_lower))-1)

state_policy_final$Party <- as.factor(state_policy_final$Party)
state_policy_final$Party <- factor(state_policy_final$Party, levels = rev(levels(state_policy_final$Party)))

ggplot(state_policy_final, aes(x = X, y = Y_linear, color = Party)) + theme_gdocs() +
  theme(text = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") +
  geom_line() +
  #scale_x_continuous(limits = c(0, 1.5)) +
  scale_color_manual(values = c("darkred", "dodgerblue4")) +
  geom_ribbon(aes(ymin = L_linear, ymax = U_linear), linetype=2, alpha=0.1) +
  ggtitle("Weekly State New Policies") +
  xlab("Weekly Count of State New COVID-19 Policies (standardized)") + ylab("Count of COVID-19 Tweets") +
  geom_rug(data=regression_final, aes(x = state_covid_policy_stan), inherit.aes = FALSE, color = 'gray60') +
  scale_shape_cleveland(overlap=FALSE)
ggsave('/Users/taegyoon/Google Drive/state_covid_twitter/plots/plm_plot_state_policy_12Feb.png')