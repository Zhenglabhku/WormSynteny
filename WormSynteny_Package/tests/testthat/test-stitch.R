library(testthat)
library(tidyverse)

# No stitching -> all gaps > 20 bp
# 1- One seq
test_that("stitch function returns the expected data frame", {
  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III", "III"),
    start = c(1, 2000, 3000, 30000, 31000),
    end = c(1000, 2100, 3100, 30100, 31100),
    strand = c("+", "+", "+", "+", "+"),
    length = c(1000, 100, 100, 100, 100)
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III", "III"),
    start = c(1, 2000, 3000, 30000, 31000),
    end = c(1000, 2100, 3100, 30100, 31100),
    strand = c("+", "+", "+", "+", "+"),
    length = c(1000, 100, 100, 100, 100),
    gap = c(0, 0, 0, 0, 0))

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# No stitching -> all gaps > 20 bp
# 2- Three seqs
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_IV", "briggsae_I"),
    seqnames = c("III", "IV", "I"),
    start = c(1, 40100, 789710),
    end = c(3145, 43000, 790000),
    strand = c("+", "+", "+"),
    length = c(3145, 2900, 290)
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III", "briggsae_IV", "briggsae_I"),
    seqnames = c("III", "IV", "I"),
    start = c(1, 40100, 789710),
    end = c(3145, 43000, 790000),
    strand = c("+", "+", "+"),
    length = c(3145, 2900, 290),
    gap = c(0, 0, 0)
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})


#Full stitching -> all gaps < 20 bp
#1.1- One seq / right order
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III", "III", "III"),
    start = c(1, 1001, 2000, 3005, 3029, 3120),
    end = c(1000, 2000, 3000, 3010, 3100, 3145),
    strand = c("+", "+", "+", "+", "+", "+"),
    length = c(1000, 999, 1000, 5, 71, 25)
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III"),
    seqnames = c("III"),
    start = c(1),
    end = c(3145),
    strand = c("+"),
    length = c(3100),
    gap = c(45)
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Full stitching -> all gaps < 20 bp
#1.2- One seq / no order
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"
    ),
    seqnames = c(
      "III", "III", "III", "III", "III", "III"
    ),
    start = c(
      3120, 1, 3029, 2000, 1001, 3005
    ),
    end = c(
      3145, 1000, 3100, 3000, 2000, 3010
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      25, 1000, 71, 1000, 999, 5
    )
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III"),
    seqnames = c("III"),
    start = c(1),
    end = c(3145),
    strand = c("+"),
    length = c(3100),
    gap = c(45)
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Full stitching -> all gaps < 20 bp
#2- Two seqs
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_IV", "briggsae_IV", "briggsae_IV", "briggsae_IV", "briggsae_IV"),
    seqnames = c("III", "III", "III", "III", "III", "III", "IV", "IV", "IV", "IV", "IV"),
    start = c(1, 1001, 2000, 3005, 3029, 3120, 40100, 40155, 40772, 40933, 42170),
    end = c(1000, 2000, 3000, 3010, 3100, 3145, 40155, 40752, 40922, 42168, 43000),
    strand = c("+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+"),
    length = c(1000, 999, 1000, 5, 71, 25, 55, 597, 150, 1235, 830)
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III","briggsae_IV"),
    seqnames = c("III","IV"),
    start = c(1, 40100),
    end = c(3145, 43000),
    strand = c("+", "+"),
    length = c(3100, 2867),
    gap = c(45,33)
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Full stitching -> all gaps < 20 bp
#3- Three seqs
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III",
      "briggsae_IV", "briggsae_IV", "briggsae_IV", "briggsae_IV", "briggsae_IV",
      "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I"
    ),
    seqnames = c(
      "III", "III", "III", "III", "III", "III",
      "IV", "IV", "IV", "IV", "IV",
      "I", "I", "I", "I", "I"
    ),
    start = c(
      1, 1001, 2000, 3005, 3029, 3120,
      40100, 40155, 40772, 40933, 42170,
      789456, 789478, 789520, 789614, 789710
    ),
    end = c(
      1000, 2000, 3000, 3010, 3100, 3145,
      40155, 40752, 40922, 42168, 43000,
      789478, 789500, 789600, 789700, 790000
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+",
      "+", "+", "+", "+", "+",
      "+", "+", "+", "+", "+"
    ),
    length = c(
      1000, 999, 1000, 5, 71, 25,
      55, 597, 150, 1235, 830,
      22, 22, 80, 86, 290
    )
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III","briggsae_IV","briggsae_I"),
    seqnames = c("III","IV", "I"),
    start = c(1, 40100,789456),
    end = c(3145, 43000,790000),
    strand = c("+", "+","+"),
    length = c(3100, 2867,500),
    gap = c(45,33,44)
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Mix stitching
#1- Two seqs + one seq
test_that("stitch function is not returning the expected data frame", {

  raw_df <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III",
      "briggsae_IV", "briggsae_IV", "briggsae_IV", "briggsae_IV", "briggsae_IV",
      "briggsae_I"
    ),
    seqnames = c(
      "III", "III", "III", "III", "III", "III",
      "IV", "IV", "IV", "IV", "IV",
      "I"
    ),
    start = c(
      1, 1001, 2000, 3005, 3029, 3120,
      40100, 40155, 40772, 40933, 42170,
      789710
    ),
    end = c(
      1000, 2000, 3000, 3010, 3100, 3145,
      40155, 40752, 40922, 42168, 43000,
      790000
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+",
      "+", "+", "+", "+", "+",
      "+"
    ),
    length = c(
      1000, 999, 1000, 5, 71, 25,
      55, 597, 150, 1235, 830,
      290
    )
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_IV", "briggsae_I"
    ),
    seqnames = c(
      "III", "IV", "I"
    ),
    start = c(
      1, 40100, 789710
    ),
    end = c(
      3145, 43000, 790000
    ),
    strand = c(
      "+", "+", "+"
    ),
    length = c(
      3100, 2867, 290
    ),
    gap = c(
      45, 33, 0
    )
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Mix stitching
#2- One seq : 4 stiched groups
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"
    ),
    seqnames = c(
      "III", "III", "III", "III", "III", "III", "III", "III"
    ),
    start = c(
      1, 1145, 2000, 3005, 4582, 4806, 4920, 5021
    ),
    end = c(
      1000, 2000, 3000, 3010, 4800, 4900, 5000, 5210
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      1000, 855, 1000, 5, 218, 94, 80, 189
    )
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"
    ),
    seqnames = c(
      "III", "III", "III", "III"
    ),
    start = c(
      1, 1145, 4582, 5021
    ),
    end = c(
      1000, 3010, 5000, 5210
    ),
    strand = c(
      "+", "+", "+", "+"
    ),
    length = c(
      1000, 1860, 392, 189
    ),
    gap = c(
      0, 5, 26, 0
    )
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})


#Mix stitching
#3- Two seqs : 4 stiched groups + 4 stiched groups
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I"
    ),
    seqnames = c(
      "III", "III", "III", "III", "III", "III", "III", "III", "I", "I", "I", "I", "I", "I", "I", "I"
    ),
    start = c(
      1, 1145, 2000, 3005, 4582, 4806, 4920, 5021, 6800, 7000, 9000, 9119, 9220, 9321, 4500, 4556
    ),
    end = c(
      1000, 2000, 3000, 3010, 4800, 4900, 5000, 5210, 7000, 8000, 9100, 9200, 9300, 10000, 4555, 5000
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      1000, 855, 1000, 5, 218, 94, 80, 189, 200, 1000, 100, 81, 80, 679, 55, 444
    )
  )

  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_I", "briggsae_I", "briggsae_I","briggsae_I"
    ),
    seqnames = c(
      "III", "III", "III", "III", "I", "I", "I", "I"
    ),
    start = c(
      1, 1145, 4582, 5021, 4500,6800, 9000, 9321
    ),
    end = c(
      1000, 3010, 5000, 5210, 5000, 8000, 9300, 10000
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      1000, 1860, 392, 189,  499, 1200, 261, 679
    ),
    gap = c(
      0, 5, 26, 0,  1, 0, 39,0
    )
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Mix stitching
#4- Three seqs : 4 stiched groups + 4 stiched groups + 1 group
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I"
      ,"briggsae_IV"),
    seqnames = c(
      "III", "III", "III", "III", "III", "III", "III", "III", "I", "I", "I", "I", "I", "I", "I", "I", "IV"
    ),
    start = c(
      1, 1145, 2000, 3005, 4582, 4806, 4920, 5021, 6800, 7000, 9000, 9119, 9220, 9321, 4500, 4556, 9333
    ),
    end = c(
      1000, 2000, 3000, 3010, 4800, 4900, 5000, 5210, 7000, 8000, 9100, 9200, 9300, 10000, 4555, 5000, 10654
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      1000, 855, 1000, 5, 218, 94, 80, 189, 200, 1000, 100, 81, 80, 679, 55, 444, 1321
    )
  )


  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_I", "briggsae_I", "briggsae_I","briggsae_I", "briggsae_IV"
    ),
    seqnames = c(
      "III", "III", "III", "III", "I", "I", "I", "I", "IV"
    ),
    start = c(
      1, 1145, 4582, 5021, 4500,6800, 9000, 9321, 9333
    ),
    end = c(
      1000, 3010, 5000, 5210, 5000, 8000, 9300, 10000, 10654
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+", "+" ,"+"
    ),
    length = c(
      1000, 1860, 392, 189,  499, 1200, 261, 679, 1321
    ),
    gap = c(
      0, 5, 26, 0,  1, 0, 39, 0, 0
    )
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Mix stitching
#5- One seq : 4 stiched groups with overlapping and not right order and negative gap
test_that("stitch function returns the expected data frame", {

  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III", "III", "III", "III", "III"),
    start = c(1, 500, 1007, 2020, 85000, 70000, 70000, 70115),
    end = c(1000, 2000, 1010, 2023, 90000, 85010, 70100, 75000),
    strand = c("+", "+", "+", "+", "+", "+", "+", "+"),
    length = c(1000, 1500, 3, 3, 5000, 15010, 100, 4885))

  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III"),
    seqnames = c("III", "III"),
    start = c(1, 70000),
    end = c(2023, 90000),
    strand = c("+", "+"),
    length = c(2506, 24995),
    gap = c(-483, -4995)
  )

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

#Mix stitching
#6- Three seqs
test_that("stitch function returns the expected data frame", {

  raw_df <-data.frame(
    seq_id = c(
      "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_IV"
    ),
    seqnames = c(
      "III", "III", "III", "III", "III", "III", "III", "III", "I", "I", "I", "I", "I", "I", "I", "I", "IV"
    ),
    start = c(
      1, 500, 1007, 2020, 85000, 70000, 70000, 70115, 75004, 79000, 89990, 9119, 9220, 9321, 4500, 4556, 9333
    ),
    end = c(
      1000, 2000, 1010, 2023, 90000, 85010, 70100, 75000, 80000, 90000, 90100, 9200, 9300, 10000, 4555, 5000, 10654
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      1000, 1500, 3, 3, 5000, 15010, 100, 4885, 4996, 11000, 110, 81, 80, 679, 55, 444, 1321
    )
  )
  gap_max <- 20
  expected_output <- data.frame(
    seq_id = c(
      "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_I", "briggsae_III", "briggsae_III","briggsae_IV"
    ),
    seqnames = c(
      "I", "I", "I", "I", "III", "III", "IV"
    ),
    start = c(
      4500,  9119, 9321, 75004, 1, 70000, 9333
    ),
    end = c(
      5000, 9300, 10000, 90100, 2023, 90000, 10654
    ),
    strand = c(
      "+", "+", "+", "+", "+", "+", "+"
    ),
    length = c(
      499, 161, 679, 16106, 2506, 24995, 1321
    ),
    gap = c(
      1, 20, 0, -1010, -483, -4995, 0
    )
  )

  result <- stitch(raw_df, gap_max)
  result <- result %>% arrange(seq_id, start)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)
})

# No stitching -> all gaps > 0 bp
# 1- One seq and gap ==0
test_that("stitch function returns the expected data frame", {
  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III", "III"),
    start = c(1, 2000, 3000, 30000, 31000),
    end = c(1000, 2100, 3100, 30100, 31100),
    strand = c("+", "+", "+", "+", "+"),
    length = c(1000, 100, 100, 100, 100)
  )
  gap_max <- 0
  expected_output <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III", "III"),
    start = c(1, 2000, 3000, 30000, 31000),
    end = c(1000, 2100, 3100, 30100, 31100),
    strand = c("+", "+", "+", "+", "+"),
    length = c(1000, 100, 100, 100, 100),
    gap = c(0, 0, 0, 0, 0))

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# 2- Gap == 0 and one inter gap == 0...reducing of one line
test_that("stitch function returns the expected data frame", {
  raw_df <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III", "III"),
    start = c(700, 850, 4519, 4781),
    end = c(800, 4518, 4781, 4785),
    strand = c("+", "+", "+", "+"),
    length = c(100, 3668, 262, 4)
  )
  gap_max <- 0
  expected_output <- data.frame(
    seq_id = c("briggsae_III", "briggsae_III", "briggsae_III"),
    seqnames = c("III", "III", "III"),
    start = c(700, 850, 4519),
    end = c(800, 4518, 4785),
    strand = c("+", "+", "+"),
    length = c(100, 3668, 266),
    gap = c(0, 0, 0))

  result <- stitch(raw_df, gap_max)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})
