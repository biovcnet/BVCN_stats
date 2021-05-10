colorfindr::get_colors("pick colours/LOGO.png") %>%  slice_max(col_share, n = 10) %>%  
  mutate(ypos=row_number(col_share)) %>%  ## I decided to stack colours by value. 
  ggplot(aes(x=fct_infreq(col_hex),y=1, fill=col_hex)) +  
  geom_tile() +
  geom_text(aes(label=col_hex), color="#ffffffbe", 
            size=4, family="Roboto Condensed") +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  theme_void(base_family="Roboto Condensed") +
  coord_flip(ylim=c(1,12)) +
  theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust=1)) +
  labs(caption="Using different colourspce to reduce the colour used in images")

colorfindr::get_colors("pick colours/Network.jpg") %>%  slice_max(col_share, n = 10) %>%  
  mutate(ypos=row_number(col_share)) %>%  ## I decided to stack colours by value. 
  ggplot(aes(x=fct_infreq(col_hex),y=1, fill=col_hex)) +  
  geom_tile() +
  geom_text(aes(label=col_hex), color="#ffffffbe", 
            size=4, family="Roboto Condensed") +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  theme_void(base_family="Roboto Condensed") +
  coord_flip(ylim=c(1,12)) +
  theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust=1)) +
  labs(caption="Using different colourspce to reduce the colour used in images")