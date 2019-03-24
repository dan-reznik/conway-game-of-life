
# Conway’s “Game of Life” w/ `tibble()` logic

``` r
library(tidyverse)
library(Matrix)
library(fs)
library(magick)
source("conway.R")
```

Plot a dataframe with columns (i,j) listing which cells are on.

``` r
plot_df <- function(df,width,...) {
  df %>%
    mutate(density=T) %>%
    ggplot(aes(i,j)) +
    geom_tile(aes(fill=density),height=1,width=1,...) +
    coord_fixed(xlim=c(1,width),ylim=c(1,width)) +
    scale_fill_manual(values = c("black","white")) +
    #coord_cartesian(xlim=c(1L,width),ylim=c(1L,width)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          #panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
}
```

Generate random samples in matrix

``` r
random_df <- function(width,samples) {
  tibble(i=sample.int(width,samples,replace=T),
         j=sample.int(width,samples,replace=T))
}
df_to_sparse <- function(df,width) {
  m <- Matrix(F,width,width,sparse=T) #"lsparseMatrix"
  df %>% pmap(~{m[..1,..2]<<-T})
  m
}
```

Conway simulation and plots. Main workhorse is `conway_step()` in the
“conway.R” file

``` r
set.seed(1)
side<- 100
dfs <- 1:60 %>%
  accumulate(~{print(.y);conway_step(.x,side)},
             .init=random_df(side,(side^2)/10))
```

Save frames to .png files

``` r
dir_frames <- "frames"
if(dir_exists(dir_frames))
  dir_delete(dir_frames)
dir_create(dir_frames)

dfs%>%iwalk(~ggsave(sprintf("%s/%03d.png",dir_frames,.y),
                      plot_df(.x,100)))
```

Create animated .gif of frames

``` r
dir_ls("frames",regexp="\\d+\\.png") %>%
  map(image_read) %>%
  image_join() %>% 
  image_animate(fps=2) %>% 
  image_write("conway.gif")
```

![Conway Frames](conway.gif)
