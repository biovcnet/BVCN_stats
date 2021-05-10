
`%notin%` <- Negate(`%in%`) ### function to reverse %in%

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(patchwork)
library(colorBlindness)


BVCN_colors <- c("#2C76B5", "#FF0000")

theme_set(theme_minimal(base_family="Arial")+
          theme(text=element_text(size=10, color="black"),
                axis.title=element_text(size=12, color = "black"), 
                axis.text=element_text(size=10, color = "black"),
                strip.text=element_text(size=14, color="black"),
                legend.text=element_text(size=10),
                legend.title=element_text(size=12),
          axis.title.y=element_text(margin=margin(l=10, unit="pt"))))




watches_plot <- read_csv("Date 2020-04-05_2021-04-22 Bioinformatics Virtual Coordination Network/Table data.csv") %>% 
  mutate(Date=ymd(Date)) %>% 
  drop_na() %>% 
  arrange(Date) %>% 
  clean_names() %>% ### makes lower case values
  group_by(month=floor_date(date, "month")) %>% 
  summarise(views=sum(views)) %>% 
  ggplot() +
  geom_line(aes(x = month, y = views), size = 1, col = "darkorange",alpha=0.6, linetype="dashed") +
  geom_point(aes(x=month, y=views),pch=21, fill="darkorange", colour="#020202", stroke=1.2, size=3)+
  xlab("Date") +
  ylab("Monthly \n viewing \ntotals") +
  coord_cartesian(ylim=c(0,3000))+
  theme(axis.title.y = element_text(angle = 0, vjust=0.5))


playlist_plot <- read_csv("Playlist 2020-04-05_2021-04-22 Bioinformatics Virtual Coordination Network/Table data.csv") %>%
  drop_na() %>% 
  clean_names() %>% 
  mutate(playlist_title=str_remove_all(playlist_title, "Topic: ")) %>% 
  filter(!str_detect(playlist_title,"Speaker Introduction")) %>% 
  filter(playlist_title %notin% c("General Information", "BVCN Essentials")) %>% 
  mutate(prop_views=(views/sum(views))*100) %>% 
  ggplot(aes(x=playlist_title, y=prop_views))+
  geom_bar(stat="identity", aes(x=reorder(playlist_title, prop_views), y=prop_views), col=NA, fill="darkorange", size=2, alpha=0.8)+
  geom_text(aes(label=sprintf("%1.2f%%", prop_views), group=1),stat="identity", hjust= -0.3, size=3, colour="black")+
  coord_flip()+
  theme(axis.title.x = element_text(hjust=1.55), axis.title.y=element_blank())+
  ylab("% Views")+
  ylim(0,35)+
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 12))


traffic_plot <- read_csv("Traffic source 2020-04-05_2021-04-22 Bioinformatics Virtual Coordination Network/Table data.csv") %>% 
  clean_names() %>% 
  mutate(source=if_else(traffic_source %in% c("External","Direct or unknown"), "External", "within \nYouTube")) %>% 
  filter(traffic_source != "Total") %>% 
  mutate(prop_views=(views/sum(views))*100) %>% 
  ggplot(aes(x=traffic_source, y=prop_views))+
  geom_bar(stat="identity", aes(x=reorder(traffic_source, prop_views), y=prop_views, fill=source), col=NA, size=2, alpha=0.8)+
  geom_text(aes(label=sprintf("%1.2f%%", prop_views), group=1),stat="identity", hjust= -0.3, size=3, colour="black")+
  scale_fill_manual(values=BVCN_colors)+
  coord_flip()+
  labs(fill="Traffic source")+
  facet_grid(rows=vars(source), scales="free_y", space="free_y", switch="y")+
  theme(strip.text.y=element_blank(),legend.position = c(0.7, 0.2), axis.title.x = element_blank(), axis.title.y=element_blank())+
  ylim(0,35)+
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 12))

slack_plot <- read_csv("slack data/Bioinformatics Virtual Coordination Network - Pandemic Enterprise Analytics All time - May 4, 2021.csv") %>% 
  arrange(Date) %>% 
  filter(Date < "2021-04-01") %>% 
  filter(Date >"2020-04-06") %>% 
  clean_names() %>% 
  select(date, weekly_active_members, weekly_members_posting_messages) %>% 
  pivot_longer(!date, names_to="member", values_to="value") %>% 
ggplot(aes(x=date, y=value, color=member, linetype=member)) +
  geom_line(size = 1)+
  scale_color_manual(values=c("#006F51", "purple"), name="", labels=c("Active members","Members posting messages"))+
  scale_linetype_manual(values=c("solid", "twodash"),name="", labels=c("Active members","Members posting messages"))+
  xlab("Date") +
  ylab("Slack \nactive \nusers") +
  theme(axis.title.y = element_text(angle = 0, vjust=0.5), legend.position = c(0.7, 0.8))



plot1 <- (watches_plot+theme(axis.title.y=element_text(margin = margin(t = 0, r = -40, b = 0, l = 10))))/(playlist_plot+traffic_plot)+
  plot_annotation(tag_levels="A")&
  theme(plot.caption=element_text(size=12, color = "black", hjust=0.5),
        plot.tag = element_text(size = 16, face="bold"))

Figure1 <- plot1/(slack_plot+theme(axis.title.y=element_text(margin = margin(t = 0, r = -40, b = 0, l = 10))))+plot_layout(heights=c(1,2,1))&  
  theme(plot.tag = element_text(size = 16, face="bold"))
Figure1a


cvdPlot(Figure1a) ### check colour blind compatible


### ggsave(plot=Figure1a, "Figure/Figure1.tiff", type="cairo", dpi=300, units="mm", width=180, height=300) ### specifications for Frontiers submission
### ggsave(plot=Figure1a, "Figure/Figure1.png", type="cairo", dpi=300, units="mm", width=180, height=300) ### specification to embed in Google Doc MS
