table_refpts<-function() {
  survey.df<-read.csv(here::here("data","survey_biomass.csv"))
  app.df<-survey.df %>% filter(year>2011)
  avg.df<-app.df %>% mutate(avgcentral = mean(central),avgeastern = mean(eastern),avgwestern = mean(western))
  avg.df<-avg.df %>% mutate(total = avgwestern + avgcentral + avgeastern)
  prop.western<-avg.df$avgwestern[1]/avg.df$total[1]
  prop.central<-avg.df$avgcentral[1]/avg.df$total[1]
  prop.eastern<-avg.df$avgeastern[1]/avg.df$total[1]
  }
