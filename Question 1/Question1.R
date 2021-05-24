# Rotten Tomatoes
#CLAIM 1
 Movies <- Movies %>%
    rename(Rotten_Tomatoes = "Rotten Tomatoes %", Audience_Score = `Audience  score %`, Lead_Studio = "Lead Studio")

 Movies_adj <- function(Movies) {Movies %>%
     filter(Rotten_Tomatoes > 80 & Audience_Score < 85)
}
 #CLAIM 2

 Disney_Profit <- function(Movies) {
     Movies %>%
     drop_na()%>%
     group_by(Lead_Studio) %>%
     summarise(Average_Profitability = mean(Profitability), Average_Grossing = mean(`Worldwide Gross`)) %>%
     arrange(desc(Average_Profitability)) %>%
     ggplot(.) +
     geom_bar(aes(Lead_Studio, Average_Profitability, fill = Lead_Studio), stat = 'identity')+
     geom_text(aes(Lead_Studio, y = Average_Profitability, label = Average_Grossing), vjust = 0, size=2 ) +
     theme_bw()  +
     scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
     labs(title = "Average Profitability of Leading Studios", subtitle = "", x = "", y = "Average Profitability") +
     theme(legend.position = "top", legend.title = element_blank()) +
     theme(plot.title = element_text(size = 14),
           plot.subtitle = element_text(size = 12),
           axis.text.x = element_text(size = 6))
}
 #CLAIM 3
 library(knitr)
 library(kableExtra)
 library(tidyverse)
 library(pROC)
 library(GGally)
 Movies <- Movies %>% drop_na()
 Correlation_plot <- ggcorr(Movies[-c(1,2,3)], nbreaks = 6, label = T, low = "red3", high = "green3",
        label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
     ggtitle(label = "Beautiful Movie Variables Correlation Plot") +
     theme(plot.title = element_text(hjust = 0.75))
