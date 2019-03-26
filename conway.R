### utilities

# plot on-cells listed in data frame
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

# Generate random array of on-cells
random_df <- function(width,pct_on) {
  samples <- (width*width*pct_on) %>% as.integer
  df <- tibble(i=sample.int(width,samples,replace=T),
               j=sample.int(width,samples,replace=T),
               status="live")
  attr(df,"width") <- width
  df
}


### tibble-based conway step routines

get_neighs <- function(i,j)
  tibble(i=c(rep(i-1,3),rep(i,3),rep(i+1,3)),
         j=rep(c(j-1,j,j+1),3))

get_neighs0 <- function(i,j,width) get_neighs(i,j) %>%
  slice(-5) %>% # remove center cell
  filter(between(i,1L,width),
         between(j,1L,width))

count_on <- function(df,m) {
  df %>% pmap_int(~m[..1,..2]) %>% sum
}

count_neighs0 <- function(i,j,m,...) {
  width <- ncol(m)
  neighs <- get_neighs0(i,j,width) %>%
    count_on(m)
  neighs
}

df_to_sparse <- function(df,width) {
  m <- Matrix(F,width,width,sparse=T) #"lsparseMatrix"
  df %>% pmap(~{m[..1,..2]<<-T})
  m
}

# Conway's rules: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
# Any live cell w/ (2,3) neighbors lives
# Any dead cell w/ 3 neighbors lives

conway_step <- function(df_m) { # sparse
  width <- attr(df_m,"width")
  m <- df_m %>% df_to_sparse(width)
  
  live_cells <- df_m %>%
    select(-status) %>% # so pmap works
    mutate(neighs=pmap_int(.,count_neighs0,m)) %>%
    filter(neighs%in%c(2,3)) %>%
    mutate(status="live")
  # only consider dead_cells near live ones
  dead_cells0 <- df_m %>%
    select(-status) %>% # so pmap works
    pmap(get_neighs) %>%
    bind_rows %>%
    filter(between(i,1L,width),
           between(j,1L,width)) %>%
    distinct
  
  dead_cells <- if(nrow(dead_cells0)==0)
    dead_cells0
  else {
    dead_cells0 %>%
      anti_join(df_m,by=c("i","j")) %>%
      mutate(neighs=pmap_int(.,count_neighs0,m)) %>%
      filter(neighs==3) %>%
      mutate(status="born")
  }
  
  df_out <- live_cells %>%
    bind_rows(dead_cells) %>%
    select(i,j,status)
  attr(df_out,"width") <- width
  df_out
}

conway_sim <- function(df_init,frames) {
  tic()
  dfs <- 1:frames %>%
    accumulate(~{print(.y);conway_step(.x)},
               .init=df_init)
  toc()
  dfs
}

### Saving frames and creating anumated gif
# Save frames as .png's to output directory

make_frames <- function(dfs,dir_frames="frames") {
  if(dir_exists(dir_frames))
    dir_delete(dir_frames)
  dir_create(dir_frames)
  
  dfs%>%iwalk(~{
    fname<-sprintf("%s/%03d.png",dir_frames,.y)
    ggsave(fname,plot_df(.x,.y))
  })  
}

# Create animated .gif of frames

make_anim_gif <- function(dfs, fname_gif,
                          fps=2,
                          dir_frames="frames") {
  dfs%>%make_frames(dir_frames)
  
  dir_ls(dir_frames,regexp="\\d+\\.png") %>%
    map(image_read) %>%
    image_join() %>% 
    image_animate(fps=fps) %>% 
    image_write(fname_gif)
  
  dir_delete(dir_frames)
  
}