
# Conway’s “Game of Life” w/ `tibble()` logic

The [Game of
Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), also known
simply as Life, is a cellular automaton devised by the British
mathematician John Horton Conway in 1970. The game is a zero-player
game, meaning that its evolution is determined by its initial state,
requiring no further input. One interacts with the Game of Life by
creating an initial configuration and observing how it evolves, or, for
advanced players, by creating patterns with particular properties.

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
