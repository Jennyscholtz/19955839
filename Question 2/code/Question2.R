#DATA LOADING



Dat_Col <- function(path) {
    Forbes <- list.files(path,
               pattern = "*.csv",
               full.names = T) %>%
    map_df(~read_csv(., col_types = cols(.default = "c")))
}
Forbes <- Dat_Col("/Users/jenny/Meesters/19955839/Question 2/data/Forbes")


#FIGURE 1

Forbes$NetWorth <- as.numeric(gsub("\\D", "", Forbes$NetWorth))

Forbes1 <- Forbes %>%
    group_by(Country, Industry) %>%
    summarise(Total_NetWorth = sum(NetWorth))

Forbes1 <- Forbes1 %>% group_by(Country) %>%
    slice( which.max(Total_NetWorth))
 Forbes2 <- Forbes1 %>% ungroup()%>% top_n(20)

Forbes_plot1 <- Forbes2 %>%
    ggplot() +
    geom_bar(aes(Industry, Total_NetWorth, fill=Country), stat="identity", position = "dodge") +
    theme_bw() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(title = "Countries' Top Net Worth Industry", x = "", y = "Total Net Worth") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8), axis.text.x = element_text(size = 6))

