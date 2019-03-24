
# Conway’s “Game of Life” w/ `tibble()` logic

The [Game of
Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), also known
simply as Life, is a cellular automaton devised by the British
mathematician John Horton Conway in 1970.

## Survival rules:

  - Any live cell with fewer than two live neighbours dies, as if by
    underpopulation.
  - Any live cell with two or three live neighbours lives on to the next
    generation.
  - Any live cell with more than three live neighbours dies, as if by
    overpopulation.
  - Any dead cell with exactly three live neighbours becomes a live
    cell, as if by reproduction.

## R Implementation

The code below will use `tibbles()` and sparse matrices to evolve an
initial configuration of 100x100 cells, 10% of which are randomly
initialized as “alive”. The update logic is contained in file `conway.R`
included in the project.

``` r
suppressMessages(library(tidyverse))
library(Matrix)
library(fs)
library(magick)
library(tictoc)
source("conway.R")
```

Plot a dataframe with columns (i,j) listing which cells are on.

``` r
plot_df <- function(df,frame=NULL) {
  width <- attr(df,"width")
  frame_s <- if(is.null(frame)) "" else sprintf(", frame=%d",frame)
  df %>%
    ggplot(aes(i,j)) +
    geom_tile(aes(fill=status),height=1,width=1) +
    coord_fixed(xlim=c(1,width),ylim=c(1,width)) +
    scale_fill_manual(values = c(live="blue",born="red",dead="white")) +
    labs(title=sprintf("%d x %d%s",width,width,frame_s)) +
    #coord_cartesian(xlim=c(1L,width),ylim=c(1L,width)) +
    theme(#legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          #panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(2, "mm"))
}
```

Generate random samples in matrix

``` r
random_df <- function(width,pct_on) {
  samples <- (width*width*pct_on) %>% as.integer
  df <- tibble(i=sample.int(width,samples,replace=T),
               j=sample.int(width,samples,replace=T),
               status="live")
  attr(df,"width") <- width
  df
}
```

Create and plot random array

``` r
random_df(50,.1) %>% plot_df
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Run one iteration of a conway\_step (see `conway.R`)

``` r
random_df(50,.1) %>% conway_step %>% plot_df
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Conway simulation and plots. Main workhorse is `conway_step()` in the
“conway.R” file

``` r
set.seed(1)
side <- 64
tic()
dfs <- 1:60 %>%
  accumulate(~{print(.y);conway_step(.x)},
             .init=random_df(side,.2))
toc()
```

Save frames to .png files

``` r
dir_frames <- "frames"
if(dir_exists(dir_frames))
  dir_delete(dir_frames)
dir_create(dir_frames)

dfs%>%iwalk(~{
  fname<-sprintf("%s/%03d.png",dir_frames,.y)
  ggsave(fname,plot_df(.x,.y))
})
```

Create animated .gif of frames

``` r
dir_ls("frames",regexp="\\d+\\.png") %>%
  map(image_read) %>%
  image_join() %>% 
  image_animate(fps=2) %>% 
  image_write("conway.gif")
```

<img src="conway.gif" width="100%" />
