

library(rmarkdown)

# ============================================================
# POND INLET MONTHLY QC RENDER
# ============================================================

year <- 2025
month <- 5

# Rmd location
rmd_file <- paste0(
  "C:/Users/klynoe/Documents/pp github reps/toolik_cleaning_processing/toolik_cleaning/",
  "toolik_monthly_qc.Rmd"
)

# QC output directory
year_dir <- file.path(
  "C:/Users/klynoe/Documents/pp github reps/toolik_cleaning_processing/",
  "toolik_qc_files",
  year
)

if (!dir.exists(year_dir)) {
  dir.create(year_dir, recursive = TRUE)
}

# Output filename
month_name <- sprintf("%04d_%02d", year, month)

output_file <- file.path(
  year_dir,
  paste0(
    "toolik_qc_",
    month_name,
    ".html"
  )
)

# Knit
rmarkdown::render(
  input = rmd_file,
  output_file = output_file,
  params = list(
    year = year,
    month = month
  )
)

