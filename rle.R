library(tidyverse)
suppressMessages(library(glue))
suppressMessages(library(assertthat))

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

expand_rle_items3 <- function(s) {
  s%>%
    map2_dfr(names(.),.,~list(name=.x,run=.y))%>%
    mutate(run_to=cumsum(run),run_from=lag(1+run_to,default=1))%>%
    filter(name=="o")%>%
    select(run_from,run_to)%>%
    pmap(~.x:.y)%>%
    unlist
}

expand_rle_items2 <- function(s) {
  s_ext <- s%>%str_extract_all("\\d+|[ob]")%>%first
  l <- length(s_ext)
  assert_that(l<=2,msg="item has wrong fmt")
  if(l==1)
    c(1L)%>%set_names(s_ext)
  else
    as.integer(s_ext[1])%>%set_names(s_ext[2])
}

expand_rle_items_v <- Vectorize(expand_rle_items)
expand_rle_items2_v <- Vectorize(expand_rle_items2,USE.NAMES=F)
expand_rle_items3_v <- Vectorize(expand_rle_items3,USE.NAMES=F)

get_ons <- function(s) s %>%
  str_extract_all(".")%>%
  map(str_which,fixed("o"))

rle2df <- function(rle) {
  df_rle <- tibble(rle=decode_rle(rle),
                   spl=rle%>%map(split_rle),
                   #items=spl%>%map(expand_rle_items_v)%>%map_chr(str_c,collapse=""),
                   #i=items%>%get_ons,
                   items=spl%>%map(expand_rle_items2_v),
                   i=items%>%map(expand_rle_items3),
                   xmax=i%>%map_int(last))%>%
    mutate(j=row_number())
  
  df_narrow <- df_rle %>%
    unnest(i) %>%
    select(i,j) %>%
    mutate(status="live")
  
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
    ggplot(aes(i,j)) +
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

read_and_plot <- function(fname="rle.csv") {
  df_rle <- read_csv(fname)
  df_rle%>%pmap(~plot_rle(..3,..1))
}

# read_and_plot()