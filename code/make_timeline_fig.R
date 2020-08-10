# based on https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

# load pkgs set options ----
library(ggplot2)
library(scales)
library(lubridate)
library(dplyr)
options(stringsAsFactors = FALSE) # needed if R < 4.0.0

# load data ----
df <- read.csv(file.path("data_raw", "ssmse_timeline.csv"))

# define a date ----
df$date <- with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df <- df[with(df, order(date)),]


# make status a factor ----
# could use use 3 color Dark 2 palette from 
# https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
# currently using hexes from NOAA palette
status_levels <- c("complete", "active", "planned")
status_colors <- c("#008998", "#D65F00", "#625BC4")
df$status <- factor(df$status, levels = status_levels, ordered = TRUE)

# define positions for events ----
positions <- c(0.3,-0.3, 0.5,-0.5, 0.7,-0.7)
directions <- c(1,-1)

line_pos <- data.frame(
  "date" = unique(df$date),
  "position" = rep(positions, length.out = length(unique(df$date))),
  "direction" = rep(directions, length.out = length(unique(df$date)))
)

df <- merge(x = df,
            y = line_pos,
            by = "date",
            all = TRUE)
df <- df[with(df, order(date, status)),]

# define the buffer ---
month_buffer <- 2
month_date_range <-
  seq(min(df$date) - months(month_buffer),
      max(df$date) + months(month_buffer),
      by = 'month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <-
  seq(min(df$date) - months(month_buffer),
      max(df$date) + months(month_buffer),
      by = 'year')
year_date_range <- as.Date(intersect(
  ceiling_date(year_date_range, unit = "year"),
  floor_date(year_date_range, unit = "year")
),  origin = "1970-01-01")
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

# plot ----

timeline_plot <-
  ggplot(df, aes(x = date, y = 0, col = status, label = event)) +
  labs(col = "") +
  scale_color_manual(values = status_colors,
                     labels = status_levels,
                     drop = FALSE) +
  geom_hline(yintercept = 0, # horizonal lines
             color = "black",
             size = 0.3) +
  geom_segment( #vertical lines
    aes(y = position, yend = 0, xend = date, color = status),
    #color = "black",
    size = 0.2) +
  geom_point(aes(y = 0), size = 3) +
  geom_text(
    data = month_df,
    aes(x = month_date_range, y = -0.1, label = month_format),
    size = 2.5,
    vjust = 0.5,
    color = 'black',
    angle = 90) +
  geom_text(
    data = year_df,
    aes(x = year_date_range, y = -0.2, label = year_format,
        fontface = "bold"), size = 2.5, color = 'black') +
  geom_text(data = df, aes(y = position, label = event, fontface = "bold"), size = 2.5)+
  theme_classic() +
  theme(# Don't show axes, appropriately position legend
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "none")
ggsave(file.path("figures", "SSMSE_timeline.png"), width = 6, height = 4, units = "in")
