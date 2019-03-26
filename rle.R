library(tidyverse)
library(glue)
library(assertthat)

decode_rle <- function(s) s %>%
    str_sub(end=-2) %>% # get rid of !
    str_split(fixed('$')) %>% # split in lines
    first 

split_rle <- function(s) s %>%
  str_extract_all("(\\d*?[bo])","\\1,")

expand_rle_items <- function(s) {
  s_ext <- s%>%str_extract_all("\\d+|[ob]")%>%first
  l <- length(s_ext)
  assert_that(l<=2,msg="item has wrong fmt")
  if(l==1) s_ext else {
    rep(s_ext[2], as.integer(s_ext[1])) %>%
    str_c(collapse="")
  }
}

expand_rle_items_v <- Vectorize(expand_rle_items)

get_ons <- function(s) s %>%
  str_extract_all(".")%>%
  map(str_which,fixed("o"))

rle2df <- function(rle) {
  df_rle <- tibble(rle=decode_rle(rle),
                   spl=rle %>% map(split_rle),
                   items=spl%>%map(expand_rle_items_v)%>%
                     map_chr(str_c,collapse=""),
                   x=items%>%get_ons,
                   xmax=x%>%map_int(last))%>%
    mutate(y=row_number())
  
  df_narrow <- df_rle %>%
    select(x,y) %>%
    unnest(x)
  
  attr(df_narrow,"xmax") <- max(df_rle$xmax)
  attr(df_narrow,"ymax") <- nrow(df_rle)
  df_narrow
}

# http://www.conwaylife.com/wiki/Spaceship

plot_rle <- function(rle,title) {
  df <- rle2df(rle)
  xmax <- attr(df,"xmax")
  ymax <- attr(df,"ymax")
  df %>%
    ggplot(aes(x,y)) +
    geom_tile(width=.75,height=.75,fill="blue") +
    coord_fixed() +
    scale_x_continuous(breaks=1:xmax,labels=1:xmax) +
    scale_y_continuous(breaks=1:ymax,labels=1:ymax,
                       trans="reverse") +
    labs(title=title) +
    theme(axis.title=element_blank(),
          #axis.ticks = element_blank(),
          #axis.text=element_blank()
          panel.grid.minor = element_blank()
          )
}

df_rle <- read_csv("rle.csv")
# df_gosper <- rle2df(rle_gosper)
df_rle%>%pmap(~plot_rle(..3,..1))