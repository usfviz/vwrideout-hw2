library(shiny)
library(ggvis)
library(dplyr)

get_df <- function(){
  fert.df <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv")
  fert.df <- fert.df[,-c(3,4,ncol(fert.df))]
  varying <- colnames(fert.df)[3:ncol(fert.df)]
  fert.df <- reshape(fert.df, varying=varying, v.names="Fert", timevar="Year", times=varying, direction="long")
  fert.df <- fert.df[,-5]
  
  LE.df <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv")
  LE.df <- LE.df[,-c(3,4,ncol(LE.df))]
  varying <- colnames(LE.df)[3:ncol(LE.df)]
  LE.df <- reshape(LE.df, varying=varying, v.names="LE", timevar="Year", times=varying, direction="long")
  LE.df <- LE.df[,-c(1,5)]
  
  pop.df <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv")
  pop.df <- pop.df[,-c(3,4,ncol(pop.df))]
  varying <- colnames(pop.df)[3:ncol(pop.df)]
  pop.df <- reshape(pop.df, varying=varying, v.names="pop", timevar="Year", times=varying, direction="long")
  pop.df <- pop.df[,-c(1,5)]
  
  df <- merge(merge(fert.df, LE.df, by=c("Country.Code", "Year"), all.x=TRUE), pop.df, by=c("Country.Code", "Year"), all.x=TRUE)
  df$Year <- as.numeric(substr(df$Year, 2, 5))
  df <- df[complete.cases(df),]
  df$pop <- df$pop / min(df$pop) / 50
  
  meta.df <- read.csv("Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv")
  meta.df <- meta.df[,1:2]
  df <- merge(df, meta.df, by="Country.Code", all.x=TRUE)
  df <- df[df$Region != "",]
  df$Region <- factor(df$Region)
  return(df)
}


server <- function(input, output) {
  df <- get_df()
  df$id <- 1:nrow(df)
  
  show_name <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[df$id == x$id, ]
    return(row$Country.Name)
    #paste0(names(row), ": ", format(row), collapse = "<br />")
  }
  
  df %>% 
    ggvis(~LE, ~Fert, size := ~pop, key := ~id, fill = ~Region) %>%
    filter(Year == eval(input_slider(1960, 2014, sep="",
      animate=animationOptions(interval = 500, loop = FALSE)))) %>%
    set_options(height = 400, width = 800) %>%
    add_axis("x", title="Life Expectancy") %>%
    add_axis("y", title="Fertility Rate") %>%
    scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
    scale_numeric("y", domain = c(0, 9), nice = FALSE) %>%
    add_tooltip(show_name, "hover") %>%
    add_tooltip(show_name, "click") %>%
    layer_points() %>%
    bind_shiny("ggvis", "ggvis_ui")
    
}
