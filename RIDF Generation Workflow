# =====================================================
# RIDF Curve Generation + Table Export
# EXACTLY aligned with Fiji IDF paper (De Zoysa et al.)
# =====================================================

library(tidyverse)

# ---- 1. Load daily rainfall data ----
# Change the file path depending on your local directory. This will only read .CSV file.
input_file <- "C:/Users/GIFFORD/Desktop/IOM/Fiji/Rainfall/LaucalaBay_Rainfall.csv" 
rain <- read.csv(input_file,na.strings = c("", "NA", "NaN", "-999"))

rain$date <- as.Date(rain$date)
rain$year <- as.integer(format(rain$date, "%Y"))

# ---- 2. Data quality control (missing data screening) ----
qc_year <- rain %>%
  group_by(year) %>%
  summarise(
    total_days   = n(),
    missing_days = sum(is.na(rain_mm)),
    missing_pct  = (missing_days / total_days) * 100
  )

valid_years <- qc_year %>%
  filter(missing_pct <= 10) %>%
  pull(year)

rain_clean <- rain %>%
  filter(year %in% valid_years)

# ---- 3. Annual Maximum Series (AMS) ----
AMS <- rain_clean %>%
  group_by(year) %>%
  summarise(
    R24 = max(rain_mm, na.rm = TRUE)
  ) %>%
  drop_na()

# ---- 4. Gumbel distribution (Method of Moments) ----
mu    <- mean(AMS$R24)
sigma <- sd(AMS$R24)

# ---- 5. Return periods ----
T <- c(2, 5, 10, 25, 50, 100)

KT <- function(T){
  -sqrt(6) / pi * log(log(T / (T - 1)))
}

R24_T <- mu + KT(T) * sigma

# ---- 6. IMD scaling exponent (EXACT paper value) ----
H <- 1/3   # Equation (3) in the paper

# ---- 7. Durations (hours) ----
durations <- c(0.0833, 0.1667, 0.25, 0.5, 1, 2, 6, 12, 24)
# (5, 10, 15, 30 min + hourly durations)

# ---- 8. Generate RIDF table ----
RIDF_table <- expand.grid(
  Duration_hr = durations,
  Return_Period_yr = T
) %>%
  mutate(
    R24_mm = R24_T[match(Return_Period_yr, T)],
    Depth_mm = R24_mm * (Duration_hr / 24)^H,
    Intensity_mm_hr = Depth_mm / Duration_hr
  ) %>%
  arrange(Return_Period_yr, Duration_hr)

# ---- 9. Save table to SAME folder as input data ----
output_path <- file.path(
  dirname(normalizePath(input_file)),
  "RIDF_Table_IMD_Gumbel.csv"
)

write.csv(RIDF_table, output_path, row.names = FALSE)

# ---- 10. Confirmation message ----
cat("RIDF table successfully saved at:\n", output_path, "\n")

# ---- 11. Optional: Plot RIDF curves ----

plot <- ggplot(RIDF_table,
        aes(Duration_hr, Intensity_mm_hr,
             color = factor(Return_Period_yr))) +
        geom_line(size = 0.5) +
        geom_point() +
        #scale_x_log10() +
        #scale_y_log10() +
        labs(
          title = "RIDF Curves (IMD Scaling + Gumbel)",
          x = "Duration (hours)",
          y = "Rainfall Intensity (mm/hr)",
          color = "Return Period (years)"
        ) +
        theme_bw() + theme(legend.position="bottom")
ggsave(filename = "00PlotTest.png",plot=plot,path = "C:/Users/GIFFORD/Desktop/IOM/Fiji/Rainfall/Graph",
       width=6,height = 6,units = "in")  ##Modify this part of the code based on your local directory also.
plot
