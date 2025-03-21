library(tidyverse)
library(gganimate)
library(ggimage)
library(GGally)
library(jpeg)
library(ggpubr)
library(showtext)
library(patchwork)

font_add_google('Fira Code', 'firacode')

showtext_auto()

rdata <- read_csv("Data/streaming_service.csv")
stream <- rdata |>
  # you have to use paste0 because default paste adds a space in between
  mutate(date=as.POSIXct(paste0("01-",date),format='%d-%b-%Y'))|> 
  mutate(service = as.factor(service))

View(stream)

stream_img <- stream |>
  mutate(
    single=case_when(
      service == "Apple TV+" ~ "Playground/img_single/single_apple.lq.jpg",
      service == "Crunchyroll" ~ "Playground/img_single/single_crunchyroll.lq.jpg",
      service == "Disney+" ~ "Playground/img_single/single_disney.lq.jpg",
      service == "HBO Max" ~ "Playground/img_single/single_hbo.lq.jpg",
      service == "Hulu" ~ "Playground/img_single/single_hulu.lq.jpg",
      service == "Netflix" ~ "Playground/img_single/single_netflix.lq.jpg",
      service == "Paramount+" ~ "Playground/img_single/single_paramount.lq.jpg",
      service == "Peacock" ~ "Playground/img_single/single_peacock.lq.jpg",
      service == "Prime Video" ~ "Playground/img_single/single_amazon.lq.jpg",
      service == "Shudder" ~ "Playground/img_single/single_shudder.lq.jpg"
      ),
    clock=case_when(
      service == "Apple TV+" ~ "Playground/img_clock/clock_apple.lq.jpg",
      service == "Crunchyroll" ~ "Playground/img_clock/clock_crunchyroll.lq.jpg",
      service == "Disney+" ~ "Playground/img_clock/clock_disney.lq.jpg",
      service == "HBO Max" ~ "Playground/img_clock/clock_hbo.lq.jpg",
      service == "Hulu" ~ "Playground/img_clock/clock_hulu.lq.jpg",
      service == "Netflix" ~ "Playground/img_clock/clock_netflix.lq.jpg",
      service == "Paramount+" ~ "Playground/img_clock/clock_paramount.lq.jpg",
      service == "Peacock" ~ "Playground/img_clock/clock_peacock.lq.jpg",
      service == "Prime Video" ~ "Playground/img_clock/clock_amazon.lq.jpg",
      service == "Shudder" ~ "Playground/img_clock/clock_shudder.lq.jpg"
      ),
    # https://sites.stat.columbia.edu/tzheng/files/Rcolor.pdf is an excellent
    # resource when color picking
    # color=case_when(
    #   service == "Shudder" ~ "deeppink1",
    #   service == "Netflix" ~ "mediumvioletred",
    #   service == "Prime Video" ~ "firebrick1",
    #   service == "Paramount+" ~ "darkorange",
    #   service == "Hulu" ~ "gold2",
    #   service == "HBO Max" ~ "chartreuse3",
    #   service == "Disney+" ~ "dodgerblue3",
    #   service == "Peacock" ~ "darkslateblue",
    #   service == "Apple TV+" ~ "darkorchid",
    #   service == "Crunchyroll" ~ "magenta3"
    # )
    color=case_when(
      service == "Shudder" ~ "A_Shudder",
      service == "Netflix" ~ "B_Netflix",
      service == "Prime Video" ~ "C_Prime Video",
      service == "Paramount+" ~ "D_ Paramount+",
      service == "Hulu" ~ "E Hulu",
      service == "HBO Max" ~ "F_Max",
      service == "Disney+" ~ "G_ Disney",
      service == "Peacock" ~ "H_peacock",
      service == "Apple TV+" ~ "I_apple TV",
      service == "Crunchyroll" ~ "J_chunchyroll"
    )
  )


rainbow <- stream_img |>
  mutate(date=as.numeric(date))|>
  mutate(date=date-1300000000)|>
  sample_frac(1L)|>
  #arrange(date) |>
  ggplot(aes(x=date,y=price,color=color))+
  scale_color_manual(values=c("deeppink1","mediumvioletred","firebrick1","darkorange","gold2","chartreuse3","dodgerblue3","darkslateblue","darkorchid","magenta3"))+
  scale_fill_gradient(low="#FF0000",high ="#00FF00",na.value = "#0000FF")+
  scale_x_continuous(trans="log2")+
  #scale_fill_brewer(palette ="Spectral")+
  scale_x_reverse()+
  scale_y_reverse()+
  #geom_point()+
  #geom_twitchemote(aes(image="notlikethis"))+
  #geom_crossbar(aes(ymin=10,ymax=100))+
  geom_curve(aes(xend=233260000,yend=10))+
  geom_polygon(aes(fill=date))+
  theme(
    panel.background = element_rect(color="white",fill="black"),
    plot.background = element_rect(color="white",fill="black")
  )

  #geom_line()

stream_img |>
  #mutate(date=as.numeric(date))|>
  #mutate(date=date-1300000000)|>
  #sample_frac(1L)|>
  ggplot(aes(x=date,y=price,color=service))+
  geom_point(alpha=0.25)+
  geom_line()
  #geom_image(aes(image=single))

blue <- stream_img |>
  mutate(date=as.numeric(date))|>
  mutate(date=date-1300000000)|>
  mutate(price_smol=price*10)|>
  sample_frac(1L)|>
  ggplot(aes(x=date,y=price,group=service,linewidth = date))+
  background_image(readJPEG("Playground/putin.jpeg"))+
  geom_image(aes(image="Playground/img_single/single_apple.lq.jpg"))+
  labs(
    title = "Mister Putin needs to have hobbies",
    x="Beeg nubmer this way -->",
    y="Stonks -->"
       )+
  #scale_x_continuous(trans="log2")+
  scale_y_continuous(breaks = seq(5,20,0.1),trans="log2")+
  scale_x_continuous(breaks = seq(0,400000000,500000))+
  geom_crossbar(aes(ymin=5,ymax=20,color=service,alpha=0.1))+
  scale_color_manual(values=c("blue","darkblue","deepskyblue","dodgerblue","cyan","darkslategrey","darkturquoise","lightblue3","lightslateblue","midnightblue","blue3"))+
  geom_image(aes(image=single))+
  #stat_ellipse()+
  geom_smooth(aes(color="firebrick"))+
  theme_bw()+ 
  theme(
    text = element_text(family='firacode'),
    panel.background = element_rect(color="yellow",fill="orange"),
    plot.background = element_rect(color="yellow",fill="orange"),
    axis.text.y = element_text(size=17,angle=42),
    axis.text.x = element_text(size=8,angle=90),
    axis.ticks = element_line(linewidth = 40,size = 40,color="magenta"),
    legend.position = "none"
  )

blue + transition_states(price) + view_follow()

blue

rainbow


