# Case 1 : one block and positive strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =100000, seqnames = "I",	start2 = 1,	end2= 10,	blockCount =1,	blockSizes ="9,"	,tStarts	= "1,",	strand2 = "+")
  expected_output <- data.frame(seqnames = "I", start = 1,	end	= 10, length = 9)

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 2 : one block and negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =100000, seqnames = "I",	start2 = 1,	end2= 10,	blockCount =1,	blockSizes ="9,"	,tStarts	= "99990,",	strand2 = "-")
  expected_output <- data.frame(seqnames = "I", start = 1,	end	= 10, length = 9)

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 3 : one block, start == end, blocksize == 1, positive strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =100000, seqnames = "I",	start2 = 1,	end2= 1,	blockCount =1,	blockSizes ="1,"	,tStarts	= "1,",	strand2 = "+")
  expected_output <- data.frame(seqnames = "I", start = 1,	end	= 1, length =1 )

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})


# Case 4 : one block, start == end, blocksize == 1, negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =100000, seqnames = "I",	start2 = 1,	end2= 1,	blockCount =1,	blockSizes ="1,"	,tStarts	= "99999,",	strand2 = "-")
  expected_output <- data.frame(seqnames = "I", start = 1,	end	= 1, length =1 )

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 5 : two blocks, blocksize == 1, negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =100000, seqnames = "I",	start2 = 1,	end2= 3,	blockCount =2,	blockSizes ="1,1,"	,tStarts	= "99997,99998,",	strand2 = "-")
  expected_output <- data.frame(seqnames = c("I", "I"), start = c(1,2),	end	= c(2,3), length =c(1,1) )

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# case 7 : two blocks, positive strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =20594552, seqnames = "Sp34_Chr1",	start2 = 13003882,	end2= 13003898,	blockCount =2,	blockSizes ="4,12,"	,tStarts	= "13003882,13003886,",	strand2 = "+")
  expected_output <- data.frame(seqnames = c("Sp34_Chr1", "Sp34_Chr1"), start = c(13003882,13003886),	end	= c(13003886,13003898), length =c(4,12))

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 8 : two blocks, negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =20594552, seqnames = "Sp34_Chr1",	start2 = 13003882,	end2= 13003898,	blockCount =2,	blockSizes ="12,4,"	,tStarts	= "7590654,7590666,",	strand2 = "-")
  expected_output <- data.frame(seqnames = c("Sp34_Chr1", "Sp34_Chr1"), start = c(13003882,13003886),	end	= c(13003886,13003898), length =c(4,12))

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 9 : three blocks, negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =20594552, seqnames = "Sp34_Chr1",	start2 = 13003876,	end2= 13003898,	blockCount =3,	blockSizes ="12,4,6,"	,tStarts	= "7590654,7590666,7590670,",	strand2 = "-")
  expected_output <- data.frame(seqnames = c("Sp34_Chr1", "Sp34_Chr1", "Sp34_Chr1"), start = c(13003876,13003882,13003886),	end	= c(13003882,13003886,13003898), length =c(6,4,12))

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 10 : three blocks, positive and negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =c(20594552,20594552), seqnames = c("Sp34_Chr1","Sp34_Chr1"),	start2 = c(13003876,14100500),	end2= c(13003898,14100550),	blockCount =c(3,1),	blockSizes =c("12,4,6,","50,"), tStarts	=c( "7590654,7590666,7590670,","14100500,"),	strand2 = c("-","+"))
  expected_output <- data.frame(seqnames = c("Sp34_Chr1", "Sp34_Chr1", "Sp34_Chr1","Sp34_Chr1"), start = c(14100500,13003876,13003882,13003886),	end	= c(14100550,13003882,13003886,13003898), length =c(50,6,4,12))

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})

# Case 11 : four blocks, positive and negative strand
test_that("sort_haliftover function doesn't return the right data frame", {
  raw_df <- data.frame(
    tSize =c(20594552,20594552,20594552), seqnames = c("Sp34_Chr1","Sp34_Chr1","Sp34_Chr1"),	start2 = c(13003876,14100500,14100600),	end2= c(13003898,14100550,14100665),	blockCount =c(3,1,2),	blockSizes =c("12,4,6,","50,", "22,43,"), tStarts	=c( "7590654,7590666,7590670,","14100500,", "14100600,14100622,"),	strand2 = c("-","+","+"))
  expected_output <- data.frame(seqnames = c("Sp34_Chr1", "Sp34_Chr1", "Sp34_Chr1","Sp34_Chr1","Sp34_Chr1","Sp34_Chr1"), start = c(14100500,14100600,14100622,13003876,13003882,13003886),	end	= c(14100550,14100622,14100665,13003882,13003886,13003898), length =c(50,22,43,6,4,12))

  result <- sort_haliftover_data(raw_df)
  rownames(result) <- as.integer(rownames(result))
  expect_identical(result, expected_output)

})


