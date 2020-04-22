
library(tidyverse)
library(vtable)
library(lubridate)
# library(Hmisc)
library(janitor)
library(directlabels)
library(extrafont)
library(gganimate)
library(devtools)
library(transformr)
library(scales)
library(ggrepel)

# font_import()


# setwd("projects/scratches")
getwd()


scratches_raw <- read_csv("data/scratches - Sheet1.csv")

field_size <- read_csv("data/scratches - field_size.csv")
field_size <- field_size %>% mutate (year_date= paste0(year, "-01-01"))
field_size <- field_size %>% mutate (year_date= ymd(year_date))
field_size <- field_size %>% mutate (year_date= as.Date(year_date))
field_size<- field_size %>% mutate (year_short= substr(year, 3,4))
field_size<- field_size %>% mutate (year_short= paste0("'", year_short))

checkpoints_southern <- read_csv("data/scratches - checkpoints_southern.csv")  
checkpoints_northern <- read_csv("data/scratches - checkpoints_northern.csv")  
checkpoints_fairbanks <- read_csv("data/scratches - checkpoints_fairbanks.csv")  


scratches <- scratches_raw %>% filter (status == 'Scratched') %>% select (-bib, -dogs)


not_scratches <- scratches_raw %>% filter (status != 'Scratched')



scratches <- scratches %>% arrange(year, time)
scratches <- scratches %>% left_join(field_size, by="year")


scratches_southern <- scratches %>% filter (route == "southern")
scratches_northern <- scratches %>% filter (route == "northern")
scratches_fairbanks <- scratches %>% filter (route == "fairbanks")


scratches_southern <- scratches_southern %>% left_join(checkpoints_southern, by="checkpoint")
scratches_northern <- scratches_northern %>% left_join(checkpoints_northern, by="checkpoint")
scratches_fairbanks <- scratches_fairbanks %>% left_join(checkpoints_fairbanks, by="checkpoint")


scratches_joined <- rbind(scratches_southern, scratches_northern, scratches_fairbanks)


scratches_joined <- scratches_joined %>% group_by(year) %>%  arrange(order, time, .by_group = TRUE)


scratches_joined <- scratches_joined %>% mutate (c_scratches = row_number())
scratches_joined <- scratches_joined %>% mutate (c_scratches_percent = c_scratches/total_mushers)

scratches_joined <- scratches_joined %>% mutate (year2= year) %>% mutate(cumulative2 = cumulative)
scratches_joined_remove <- scratches_joined %>% ungroup() %>%  select (-year, -cumulative)

# View(scratches_joined_remove)


#all at once

all_animated <- ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
  geom_line(aes(x=cumulative, y=c_scratches, group=year, color=factor(year) ),size=1.2, show.legend=F)+
  theme_minimal()+
  # geom_text(aes(x=cumulative, y=c_scratches,label=year))+
  theme(text = element_text(family = "Fira Code", size = 30), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=42, color="#333333"))+
  geom_dl(aes(x=cumulative, y=c_scratches,label=year, color=factor(year), fontfamily = "Fira Code"),method=list("last.points", cex=2))+
  
  xlab('Miles into Race')+
  ylab("Total Scratches")+
  ggtitle("Cumulative Iditarod Musher Scratches")+
  # scale_color_brewer(palette="Set2")+
  # ggsave("plots/scratches_all.png", width=10, height=5, dpi=300, units="in")
transition_reveal(cumulative, id=year)


all_anim <- animate (all_animated, nframes = 100, fps = 15, renderer=ffmpeg_renderer(), width=1500, height=750)
all_anim_gif <- animate (all_animated, nframes = 100, fps = 15, width=1500, height=750)


anim_save("plots/all_animation.mp4", animation = all_anim )
anim_save("plots/all_animation.gif", animation = all_anim_gif )




#PERCENT ANIMATION


all_percent <- ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
  geom_line(aes(x=as.numeric(cumulative), y=c_scratches_percent, group=year, color=factor(year) ), size=1.2, show.legend=F)+
  theme_minimal()+
  # geom_text(aes(x=cumulative, y=c_scratches,label=year))+
  geom_dl(aes(x=cumulative, y=c_scratches_percent,label=year, color=factor(year), fontfamily = "Fira Code"),method=list("last.points", cex=2))+
  theme(text = element_text(family = "Fira Code", size = 30), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=42, color="#333333"))+
  xlim(0, 1005)+
  scale_y_continuous(labels=percent_format(accuracy = 1))+

  
  xlab('Miles into Race')+
  ylab("Total Scratches")+
  ggtitle("Cumulative Iditarod Musher Scratches")+
  transition_reveal(cumulative, id=year)
  
  # transition_states(cumulative,
  # transition_length = 2,
  # state_length = 1)

#   # scale_color_brewer(palette="RdYlBu")+
  # ggsave("plots/scratches_percent_all.png", width=10, height=5, dpi=300, units="in")



all_percent_anim <- animate (all_percent, nframes =100, fps = 15, renderer=ffmpeg_renderer(), width=1500, height=750 )
all_percent_anim_gif <- animate (all_percent, nframes =100, fps = 15, width=1500, height=750 )
anim_save("plots/all_percent_animation.mp4", animation = all_percent_anim )
anim_save("plots/all_percent_animation.gif", animation = all_percent_anim_gif )









 ###facet all


all_facet <- ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
  geom_line(aes(x=cumulative, y=c_scratches_percent, group=year,  color=factor(year) ), show.legend = F, size=1.9)+
  theme_minimal()+
  facet_wrap(.~year)+
  geom_line(data =scratches_joined_remove %>% filter (!is.na(cumulative2)) ,aes(x=cumulative2, y=c_scratches_percent, group=year2), show.legend = F, size=.8, color="#333333", alpha=.07)+
  
  xlab("Miles into Race")+
  ylab("Percent of Mushers Scratched")+
  theme(text = element_text(family = "Fira Code", size = 25), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=42, color="#333333"))+  ggtitle("Percent of Iditarod Mushers Who Scratched ")+
  scale_y_continuous(labels=percent_format(accuracy = 1))+

  transition_reveal(cumulative, id=year)

# +
#   ggsave("plots/scratches_facet_percent.png", width=10, height=5, dpi=300, units="in")

all_facet_percent_anim <- animate (all_facet, nframes = 100, fps = 25, width=1500, height=750, renderer=ffmpeg_renderer())
all_facet_percent_anim_gif <- animate (all_facet, nframes = 100, fps = 25, width=1500, height=750)
anim_save("plots/all_facet_percent_animation.gif", animation = all_facet_percent_anim_gif )

anim_save("plots/all_facet_percent_animation.mp4", animation = all_facet_percent_anim )

# 
# all_percent_anim <- animate (all_percent, nframes =100, fps = 15, width=1500, height=750, renderer=ffmpeg_renderer())
# anim_save("plots/all_percent_animation.mp4", animation = all_percent_anim )




############static 




static_all <- function () {
  
  all_animated <- ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
    geom_line(aes(x=cumulative, y=c_scratches, group=year, color=factor(year) ), show.legend=F, size=.9)+
    theme_minimal()+
    # geom_text(aes(x=cumulative, y=c_scratches,label=year))+
    theme(text = element_text(family = "Fira Code", size = 14), 
          plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333")
          # plot.margin = margin(.5, .5, .5, .5, "cm")
          
          
          )+
    geom_dl(aes(x=cumulative, y=c_scratches,label=year, color=factor(year), fontfamily = "Fira Code"),method=list("last.bumpup", hjust = -.1, vjust=-.5, cex=1.1))+
    
    xlab('Miles into Race')+
    xlim(0, 1005)+
    ylab("Total Scratches")+
    ggtitle("Cumulative Iditarod Scratches")+
    # scale_color_manual(values = wes_palette("GrandBudapest1", n = 25))+
  
    # scale_color_brewer(palette="Set2")+
    ggsave("plots/scratches_all.png", width=10, height=5, dpi=300, units="in")
  
}

static_all()

static_all_percent <- function () {
  
  
  
  all_percent <- ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
    geom_line(aes(x=as.numeric(cumulative), y=c_scratches_percent, group=year, color=factor(year) ), size=.9, show.legend=F)+
    theme_minimal()+
    # geom_text(aes(x=cumulative, y=c_scratches,label=year))+
    geom_dl(aes(x=cumulative, y=c_scratches_percent,label=year, color=factor(year), fontfamily = "Fira Code"),method=list("last.bumpup", hjust = -.1, vjust=-.5, cex=1.1))+
    theme(text = element_text(family = "Fira Code", size = 14), 
          plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
    
    xlab('Miles into Race')+
    ylab("Percent of Iditarod Mushers Who Scratched")+
    xlim(0, 1005)+
    scale_y_continuous(labels=percent_format(accuracy = 1))+
  
    
    ggtitle("Percent of Iditarod Field Scratching")+

  #   # scale_color_brewer(palette="RdYlBu")+
  ggsave("plots/scratches_percent_all.png", width=10, height=5, dpi=300, units="in")
  
  
}

static_all_percent()


static_facet <- function () {
  
  
  all_facet <- ggplot(data= scratches_joined %>% filter (!is.na(cumulative)))+
    geom_line(aes(x=cumulative, y=c_scratches_percent, group=year, color=factor(year) ), show.legend = F, size=1.1)+
    theme_minimal()+
    xlab("Miles into Race")+
    ylab("Percent of Mushers Scratched")+
    geom_line(data =scratches_joined_remove %>% filter (!is.na(cumulative2)) ,aes(x=cumulative2, y=c_scratches_percent, group=year2), show.legend = F, size=.8, color="#333333", alpha=.07)+
    scale_y_continuous(labels=percent_format(accuracy = 1))+
    
    
    theme(text = element_text(family = "Fira Code", size = 12), 
          plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+  ggtitle("Percent of Iditarod Mushers Who Scratched")+
    facet_wrap(~year)+
    
    ggsave("plots/scratches_facet_percent.png", width=10, height=5, dpi=300, units="in")
  
  
  
}


static_facet()

static_facet_not_percent <- function () {
  
  
  all_facet_scratch <- ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
    geom_line(aes(x=cumulative, y=c_scratches, group=year, color=factor(year) ), show.legend = F, size=1.1)+
    theme_minimal()+
    facet_wrap(.~year)+
    geom_line(data =scratches_joined_remove %>% filter (!is.na(cumulative2)) ,aes(x=cumulative2, y=c_scratches, group=year2), show.legend = F, size=.8, color="#333333", alpha=.07)+
    xlab("Miles into Race")+
    ylab("Scratches by Race Mile")+
    theme(text = element_text(family = "Fira Code", size = 12), 
          plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+  ggtitle("Iditarod Mushers Who Scratched", subtitle="2006-2020")+
    ggsave("plots/scratches_facet.png", width=10, height=5, dpi=300, units="in")
  
  
  
}


static_facet_not_percent()





##run them



###facet all

# ggplot(scratches_joined %>% filter (!is.na(cumulative)))+
#   geom_line(aes(x=cumulative, y=c_scratches, group=year ))+
#   theme_minimal()+
#   facet_wrap(.~year)+
#   transition_reveal(cumulative, id=year)
  
  # ggsave("plots/scratches_facet.png", width=10, height=5, dpi=300, units="in")






###basic histo

ggplot(scratches_joined)+
  geom_histogram(aes(x=cumulative, fill=route ), show.legend=T, binwidth = 20)+
  theme_minimal()+
  theme(text = element_text(family = "Fira Code", size = 12), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
  ggtitle("Iditarod Scratches by Mile and Route (2006-2020)")+
  xlab("Miles into Race")+
  ylab("Scratches")+
  ggsave("plots/hist_scratches_all.png", width=10, height=5, dpi=300, units="in")



ggplot(scratches_joined)+
  geom_histogram(aes(x=cumulative, fill=factor(year) ), show.legend=F, binwidth = 20)+
  theme_minimal()+
  theme(text = element_text(family = "Fira Code", size = 12), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
  ggtitle("Iditarod Scratches by Race Mile")+
  facet_wrap(.~year)+
  xlab("Miles into Race")+
  ylab("Scratches")+
  ggsave("plots/hist_scratches_facet.png", width=10, height=5, dpi=300, units="in")





scratches_joined_summary <- scratches_joined %>% group_by(year) %>% summarize(total_scratch_widthdrawn =first(number_scratched_withdrawn), total_mushers = first(total_mushers), percent_scratch_withdrawn = total_scratch_widthdrawn/total_mushers, total_scratch = first(number_scratched), percent_scratch = first(percent_scratched))




scratches_joined_summary_chk <- scratches_joined %>% group_by(checkpoint, route) %>% summarize(total =n())


scratches_joined_summary_chk_overall <- scratches_joined %>% group_by(checkpoint) %>% summarize(total_overall =n())

scratches_joined_summary_chk <- scratches_joined_summary_chk %>% left_join(scratches_joined_summary_chk_overall)



ggplot(scratches_joined_summary_chk)+
  geom_col(aes(y=total, x=reorder(checkpoint, total_overall), fill=route), position="stack",show.legend=T)+
  
  theme_minimal()+
  coord_flip()+
  ylab("")+
  xlab("Scratches")+
  theme(text = element_text(family = "Fira Code", size = 12), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
  
  ggtitle("Iditarod Scratches by Checkpoint and Route", subtitle="2006-2020")+
  ggsave("plots/bar_scratches_all.png", width=10, height=5, dpi=300, units="in")


###bar by percent

ggplot(field_size)+
  # geom_col(aes(y=percent_scratched, x=reorder(as.Date(year_date), -percent_scratched), position="stack"), fill="#009BE4")+
  geom_col(aes(y=percent_scratched, x=reorder(year_short, -percent_scratched), position="stack"), fill="#009BE4")+
  geom_col(data=field_size %>% filter(year==2020), aes(y=percent_scratched, x=reorder(year_short, -percent_scratched), position="stack"), fill="salmon")+
    theme_minimal()+
  # coord_flip()+
  ylab("Percent Scratched")+
  xlab("Year")+
  scale_y_continuous(labels=percent_format(accuracy = 1))+
  # scale_x_date()+
  # scale_x_discrete(breaks= c("73", "74", "75"))+
  
  theme(text = element_text(family = "Fira Code", size = 12), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
  
  ggtitle("Iditarod Scratches by Percent of Field")+
  ggsave("plots/bar_scratches_basic.png", width=14, height=5, dpi=300, units="in")




ggplot(field_size)+
  geom_col(aes(y=percent_scratched, x=reorder(year_short, year)),fill="#009BE4")+
  geom_col(data=field_size %>% filter(year==2020), aes(y=percent_scratched, x=reorder(year_short, -percent_scratched), position="stack"), fill="salmon")+
  
  theme_minimal()+
  # coord_flip()+
  ylab("Percent Scratched")+
  xlab("Year")+
  scale_y_continuous(labels=percent_format(accuracy = 1))+
  
  theme(text = element_text(family = "Fira Code", size = 12), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
  
  ggtitle("Iditarod Scratches by Percent of Field")+
  ggsave("plots/bar_scratches_basic_not_ordered.png", width=14, height=5, dpi=300, units="in")






###basic hostor
# 
# ggplot(scratches_joined_summary)+
#   geom_point(aes(x=total_mushers, y=total_scratch_widthdrawn), show.legend=F)+
#   geom_text(aes(x=total_mushers, y=total_scratch_widthdrawn, label=year), show.legend=F, binwidth = 20)+
#   theme_minimal()+
#   theme(text = element_text(family = "Fira Code", size = 12), 
#         plot.title = element_text(family="IBM Plex Sans Medium",size=22, color="#333333"))+
#   
#   ggtitle("Cumulative Iditarod Scratches")+
#   ggsave("plots/point_scratches_all.png", width=10, height=5, dpi=300, units="in")
# 


select_years = c(1974,
                 1980,
                 1975,
                 2020,
                 1973,
                 2003,
                 1985,
                 1984,
                 1981,
                 1976,
                 2014,
                 2007,
                 1986,
                 1977,
                 2019,
                 2011,
                 2010,
                 # 1989,
                 2009)


ggplot(field_size)+
geom_segment(aes(x=10, y=total_mushers, xend=100, yend=number_finished), alpha=.3, size=.8)+  
geom_segment(data = field_size %>% filter (year==2020),aes(x=10, y=total_mushers, xend=100, yend=number_finished), color="salmon", size=2)+  
geom_segment(data = field_size %>% filter (year==1974),aes(x=10, y=total_mushers, xend=100, yend=number_finished), color="blue", size=2)+  
  geom_point(aes(x=10, y=total_mushers), shape = 21, alpha=.9, stroke = 1,color="#444444", fill="white", size=2)+
  geom_point(aes(x=100, y=number_finished),shape = 21, alpha=.9, stroke=1, color="#444444", fill="white", size=2)+
# geom_dl(aes(x=100,y=number_finished,group=year, label=year),method="smart.grid")+
  geom_text(data = field_size %>% filter (year %in% select_years), aes(x=5, y=total_mushers, label=year), shape = 21, alpha=.9, color="#444444", fontface='bold', size=4, fill="white")+
  # geom_text_repel(aes(x=110, y=number_finished, label=year),
  #   nudge_x      = -0.35,
  #   direction    = "y",
  #   hjust        = 1,
  #   segment.size = 0.2
  # ) +
  theme_minimal()+
  # ylab("Mushers")+
  scale_x_continuous(breaks = c(0,100),labels = c("mushers \n start", "mushers\n finish"), limits = c(0,110))+
  # ylim(40,70)+
  xlab("")+
  ylab("")+
  # scale_y_continuous(labels=percent_format(accuracy = 1))+
  theme(text = element_text(family = "Fira Code", size = 32), 
        plot.title = element_text(family="IBM Plex Sans Medium",size=32, color="#333333"))+
  
  ggtitle("Iditarod Field Start and Finish Sizes")+
  ggsave("plots/line_segment_scratches_segments.png", width=10, height=16, dpi=300, units="in")








