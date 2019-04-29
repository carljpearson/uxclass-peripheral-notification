#load packages
library(tidyverse)
library(lmerTest)
library(googlesheets)
library(ggthemes)
library(sjPlot)


#public key access via googlesheets
key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/1oMhh2KYhjVSSbOgXHv0DEU_xi9wMyc3aMkDO-dU7ZmE/edit?usp=sharing")


#get data sheet into dataframe
key %>%
  gs_key() %>%
  gs_read('Form Responses 1') -> df_raw

#get stimulus key to eventually bind with data
key %>%
  gs_key() %>%
  gs_read('key') -> df_key

df_key$Trial <- rep(1:12,2) #manually add trial order

df_key <- df_key %>% rename(Order=Prototype) #rename for innerjoin

#elongate data for plotting and analysis
df_raw %>% 
  #select vars
  select(contains("Participant"),contains("Mark"),contains("[Notification")) %>%
  #rename variables to be shorter
  rename(Participant='Participant initials (Last, First = LF)',
    Order='{Do not read; Mark which the user has running}') %>%
  #enlongate data
  gather(Trial,Rating,-Participant,-Order) %>%
  #Remove alpha chars from trial var
  mutate(Trial=as.numeric(gsub("[^0-9-]", "", Trial))) %>%
  #Get only prototype A/B letter
  mutate(Order=substr(Order,11,11)) %>%
  #join with data key df
  inner_join(df_key) %>%
  #remove unneeded characters from likert rating
  mutate(Rating = gsub("[^0-9]", "", Rating)) %>%
  #Make likert rating numeric type
  mutate(Rating=as.numeric(Rating)) %>%
  #properly define trial ID based on viewing order
  mutate(Notification_ID = 
           case_when(
             Order == "A" ~ Trial,
             Order == "B" ~ 13 - Trial
             
           )
           ) -> df_long

#lmer requires numeric participant ID var
df_long$par <- as.numeric(as.factor(df_long$Participant))

#linear mixed effects model
lmer(Rating ~ Color*Shape*Speed + (1|par), data=df_long) -> lm_mod
#summary and table output of linear model
summary(lm_mod)
tab_model(lm_mod)
plot_model(lm_mod,type="int")

#plot in violin geom to view data distribution
df_long %>%
  group_by(Color,Shape,Speed) %>%
  ggplot(aes(y=Rating,x=Color,fill=Color)) +
  geom_violin(draw_quantiles = c(.25,.5,.75)) +
  scale_fill_manual(values=c("lightblue","orange","red")) +
  facet_grid(.~Speed) +
  theme_tufte(base_family="sans") +
  coord_cartesian(ylim=c(1,7)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)
        ) +
  labs(title = "Urgency Ratings Across Notification Types"
       
       )
