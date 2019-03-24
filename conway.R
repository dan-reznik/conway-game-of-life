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
  dead_cells <- df_m %>%
    select(-status) %>% # so pmap works
    pmap(get_neighs) %>%
    bind_rows %>%
    filter(between(i,1L,width),
           between(j,1L,width)) %>%
    distinct %>%
    anti_join(df_m,by=c("i","j")) %>%
    mutate(neighs=pmap_int(.,count_neighs0,m)) %>%
    filter(neighs==3) %>%
    mutate(status="born")
  
  df_out <- live_cells %>%
    bind_rows(dead_cells) %>%
    select(i,j,status)
  attr(df_out,"width") <- width
  df_out
}