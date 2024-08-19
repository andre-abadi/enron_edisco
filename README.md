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
```

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
```

## e-Discovery Formatting

In this section we add new columns to maximise compatibility when
ingesting into e-Discovery platforms.

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
                            (row_number() - 1) %/% 1000000 + 1, 
                            ((row_number() - 1) %/% 1000) %% 1000 + 1, 
                            (row_number() - 1) %% 1000 + 1)) |>
    select(doc_id,relevant,everything())
}
```

## Output

``` r
require(data.table)
enron_edisco |> setDT()
fwrite(enron_edisco, 
       "enron_edisco.csv", 
       na = "", 
       quote = "auto")
```