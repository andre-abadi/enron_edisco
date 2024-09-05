enron_edisco
================
Andre Abadi

## Introduction

**This Project** called **enron_edisco** is our attempt to create a
version of the Enron dataset (**This Dataset**) that has the same
features of datasets used in e-discovery (“e-Disco”). Those features
are:

- Data and metadata for model training.
- Data labels:
  - `1` = relevant/responsive.
  - `NA` = neither.
  - `0` = irrelevant/not responsive.

It is the author’s hope that this dataset is easily ingestible for the
purposes of developing active learning models and other such systems as
used in e-discovery.

This project will inherently be a sub-setting exercise, as a
distillation of the Enron dataset. It is our hope that by making the
Enron corpus slightly more approachable especially for students of data
science, it can encourage wider use of this dataset for learning
purposes.

### Style and Approach

Code is written using `snake_case` and attempts to follow the guidance
of the online resource [*R for Data Science
(2e)*](https://r4ds.hadley.nz/) by Wickham, Çetinkaya-Rundel, and
Grolemund. We also adopt the [Tidyverse Style
Guide](https://style.tidyverse.org/) with a preference toward piping
operations where possible for code clarity. We use the native pipe where
possible and fall back to the
[magrittr](https://magrittr.tidyverse.org/) pipe where necessary.

We attempt to use R best practices when dealing with large datasets, for
example most efficient import and export according to such literature as
[Efficient R
Programming](https://bookdown.org/csgillespie/efficientR/input-output.html).

## Source

The author located this dataset from [a
question](https://datascience.stackexchange.com/questions/92341/how-to-read-the-labeled-enron-dataset-categories)
posted on the Data Science Stack Exchange. This led to the [Enron Email
Dataset](https://data.world/brianray/enron-email-dataset) on
[data.world](https://data.world/), which had several desirable features:

- Only a free account required to download.
- Appropriately tabulated data.
- Optional other metadata field columns ready-to-go.
  - Subject
  - From/To/etc.
  - Human labelling of about “1700 records”

## Scope of Project

- Reduction of columns to just those needed for training and evaluation
  of models.
- Basic cleaning of dataset not for model ingestion but for portability
  only. We propose to remove only the artifacts that might impede the
  import of this dataset.

## Method

We recount herein the method used to develop This Dataset. Below are the
libraries used in this project.

``` r
library(tidyverse) # as_tibble
library(data.table) # fread
library(janitor) # clean_names
library(stringr) # str_replace
library(zip) # zip, unzip
library(digest) # digest
```

### Unzip

If not already complete, unzip the data.

``` r
require(zip)
file_path <- "data/enron_05_17_2015_with_labels_v2.csv"
zip_path <- "data/enron_05_17_2015_with_labels_v2.csv.zip"
print("Checking if the CSV file exists...")
```

    ## [1] "Checking if the CSV file exists..."

``` r
if (!file.exists(file_path)) {
  print("CSV file does not exist.")
  print("Checking if the ZIP file exists...")
  if (file.exists(zip_path)) {
    print("ZIP file found. Attempting to unzip the file...")
    unzip_status <- tryCatch({
      unzip(zip_path, exdir = "data")
      TRUE
    }, error = function(e) {
      print("An error occurred during unzipping.")
      FALSE
    })
    
    if (unzip_status) {
      print("File successfully unzipped.")
    } else {
      stop("Failed to unzip the file.")
    }
  } else {
    print("ZIP file does not exist.")
    stop("Neither the CSV file nor the ZIP file exists.")
  }
} else {
  print("CSV file exists. No need to unzip.")
}
```

    ## [1] "CSV file exists. No need to unzip."

``` r
rm(
  file_path,
  zip_path
)
```

### Import

We import the dataset aiming to confine scope at the earliest
opportunity to minimise size. We opt for `fread()` from the `data.table`
library because we understand it to be the fastest import method, as
described in the relevant chapter of [Efficient R
Programming](https://bookdown.org/csgillespie/efficientR/input-output.html).

``` r
require(data.table) # fread
require(janitor) # clean_names
require(tidyverse) # as_tibble
enron_edisco <-
  fread(
    "data/enron_05_17_2015_with_labels_v2.csv",
    # nrows = 100, # comment out for full load
    drop = c( # extraneous metadata
      "V1", "Message-ID",
      "X-From", "X-To", "X-cc", "X-bcc", "X-Folder", "X-Origin",
      "X-FileName", "user", "labeled",
      # weights
      "Cat_1_weight", "Cat_2_weight", "Cat_3_weight", "Cat_4_weight",
      "Cat_5_weight", "Cat_6_weight", "Cat_7_weight", "Cat_8_weight",
      "Cat_9_weight", "Cat_10_weight", "Cat_11_weight", "Cat_12_weight",
      # extraneous categories
      "Cat_1_level_1",
      "Cat_2_level_1", "Cat_2_level_2", "Cat_3_level_1", "Cat_3_level_2",
      "Cat_4_level_1", "Cat_4_level_2", "Cat_5_level_1", "Cat_5_level_2",
      "Cat_6_level_1", "Cat_6_level_2", "Cat_7_level_1", "Cat_7_level_2",
      "Cat_8_level_1", "Cat_8_level_2", "Cat_9_level_1", "Cat_9_level_2",
      "Cat_10_level_1", "Cat_10_level_2", "Cat_11_level_1",
      "Cat_11_level_2", "Cat_12_level_1", "Cat_12_level_2"
    )
  ) |>
  distinct() |>
  clean_names() |>
  as_tibble()
enron_edisco |> glimpse()
```

    ## Rows: 255,871
    ## Columns: 6
    ## $ date          <dttm> 2001-05-14 23:39:00, 2001-05-04 20:51:00, 2000-10-18 10…
    ## $ from          <chr> "frozenset({'phillip.allen@enron.com'})", "frozenset({'p…
    ## $ to            <chr> "frozenset({'tim.belden@enron.com'})", "frozenset({'john…
    ## $ subject       <chr> "", "Re:", "Re: test", "", "Re: Hello", "Re: Hello", "",…
    ## $ content       <chr> "Here is our forecast", "Traveling to have a business me…
    ## $ cat_1_level_2 <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

### Classification

``` r
require(tidyverse)
if ("cat_1_level_2" %in% names(enron_edisco)) {
  enron_edisco <-
    enron_edisco |>
    mutate(
      cat_1_level_2 = case_when(
        #is.na(cat_1_level_2) ~ 0,
        cat_1_level_2 > 1 ~ 0,
        TRUE ~ cat_1_level_2
      )
    ) |>
    rename(relevant = cat_1_level_2)
}
enron_edisco |> count(relevant) |> as.data.frame()
```

    ##   relevant      n
    ## 1        0    845
    ## 2        1    817
    ## 3       NA 254209

### e-Discovery Formatting

In this section we add new columns to maximise compatibility when
ingesting into e-Discovery platforms.

### Email Normalisation

``` r
library(tidyverse)
enron_edisco <- enron_edisco %>%
  mutate(
    from = gsub("frozenset\\(\\{", "", from),
    from = gsub("\\}\\)", "", from),
    from = gsub("([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}),", "'\\1',", from),
    to = gsub("frozenset\\(\\{", "", to),
    to = gsub("\\}\\)", "", to),
    to = gsub("([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}),", "'\\1',", to)
  )
```

### Document ID

We add a Document ID in Australian Bates number format, so the first
document will have Document ID ENR.0001.0001.0001, the second will have
ENR.0001.0001.0001, and so on.

``` r
library(tidyverse)
if (!"doc_id" %in% names(enron_edisco)) {
  enron_edisco <- 
    enron_edisco |>
    mutate(doc_id = sprintf("ENR.%04d.%04d.%04d", 
                            9000 + (row_number() - 1) %/% 1000000, 
                            ((row_number() - 1) %/% 1000) %% 1000 + 1, 
                            (row_number() - 1) %% 1000 + 1)) |>
    select(doc_id,relevant,everything())
}
enron_edisco |>
  arrange(desc(doc_id)) |>
  slice_head(n = 1) |>
  pull(doc_id)
```

    ## [1] "ENR.9000.0256.0871"

## Subsetting

``` r
set.seed(1)
require(data.table)
enron_na <- 
  enron_edisco |>
  filter(is.na(relevant))
enron_classified <-
  enron_edisco |>
  filter(!is.na(relevant))
required_rows <-
  5000 - nrow(enron_classified)
additional_rows <-
  enron_na |>
  sample_n(required_rows)
enron_5k <-
  enron_classified |>
  rbind(additional_rows) |>
  arrange(doc_id)
rm(
  enron_na,
  enron_classified,
  required_rows,
  additional_rows
)
enron_5k |> 
  count(relevant)
```

    ## # A tibble: 3 × 2
    ##   relevant     n
    ##      <dbl> <int>
    ## 1        0   845
    ## 2        1   817
    ## 3       NA  3338

## Training/Testing Split

``` r
set.seed(1)
enron_5k <- enron_5k |>
  mutate(
    train_test = if_else(!is.na(relevant),
                          sample(c("train",
                                   "test"),
                                 n(),
                                 replace = TRUE,
                                 prob = c(0.8, 0.2)),
                              NA_character_),
    train_test  = factor(train_test))
enron_5k |> count(train_test)
```

    ## # A tibble: 3 × 2
    ##   train_test     n
    ##   <fct>      <int>
    ## 1 test         333
    ## 2 train       1329
    ## 3 <NA>        3338

## Output

### Combined

``` r
require(tidyverse)
require(data.table)
require(zip)
require(digest)
output_folder <- "output/"
output_scope <- "enron_5k"
zip_name <- "enron_edisco.zip"
path <- "output"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}
if (dir.exists(output_folder)) {
  file.remove(list.files(output_folder, full.names = TRUE)) |> invisible()
}
write_content_to_file <- 
  function(doc_id, content) {
    parts <- strsplit(doc_id, "\\.")[[1]]
    dir_path <- paste0(output_folder, "ENR/", parts[2], "/", parts[3], "/")
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    file_name <- paste0(doc_id, ".txt")
    file_path <- paste0(dir_path, file_name)
    if (!file.exists(file_path)) {
      writeLines(content, con = file_path)
    }
    return(paste0("ENR/", parts[2], "/", parts[3], "/", file_name))
  }
assign(output_scope, get(output_scope) |>
  rowwise() |>
  mutate(
    native = write_content_to_file(doc_id, content),
    md5 = digest::digest(file = paste0(output_folder, native), algo = "md5")
  ) |>
  ungroup())
tibble_data <- get(output_scope) |>
  mutate(
    across(everything(), ~ gsub('["\n\r\t]', "", .)),
    content = NULL
  ) |>
  setDT()
file_name <- paste0(output_folder, "enron_edisco", ".csv")
fwrite(tibble_data, file_name, na = "", quote = "auto")
zip_exclusions <- c(zip_name,
                    "desktop.ini")
files_to_zip <- list.files("output/", full.names = TRUE)
files_to_zip <- files_to_zip[!basename(files_to_zip) %in% zip_exclusions]
zip("output/enron_edisco.zip", files = files_to_zip)
unlink("output/ENR", recursive = TRUE)
unlink("output/enron_edisco.csv")
rm(
  write_content_to_file, 
  tibble_data, 
  file_name,
  output_folder,
  output_scope,
  files_to_zip,
  zip_exclusions,
  path,
  zip_name
)
```
