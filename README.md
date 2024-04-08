# SEPwC sentiment

## Introduction

Your task is to analyse "toots" from Mastodon for sentiments. All toots are contained 
a CSV file and were fetched in Feb 2024. You should analyse the data for words that are 
classified as representing an emotion. The script should output a table of the top 10 
word used for the emotion given by the user. Something like:

```
       word   n
1  politics 433
2     money 221
3      loss 220
4     fight 202
5    damage 190
6     storm 186
7       bad 185
8       hit 174
9    threat 150
10      hot 144
```

This should be formatted nicely and be clear to the user. 

There should also be an option to plot the overall
sentiment of the "toot", against three different classification schemes, against some
relevant axis (e.g. hour of the toot was published, if it was favourited or not, etc). 

The exact plot is not checked. 

## The tests

The test suite uses a test data set, consisting of 10 random toots form 
the main dataset.

You can run the tests by running `Rscript test_script.R` in the `test` directory, 
or from R directly:

```R
library(testthat)
test_file("test_script.R")
```

from the test directory. Try it now, before you make any changes!

## The data

The data were pulled from [Mastodon]https://mastodon.social/explore 
on 22 Feb 2024 via a python script (also in the repo) that pulled public toots
that contain the hashtag `#climatechange`.
They were saved as a CSV file with the top level of the json file downloaded, saved
as columns. Some columns also contain more infomation as a json structure. 

The script should not print anything apart from the required output and information
if the user has used the '--versbose' flag. The plot should be saved to a PDF file
if the user requests it. 

The script should be run like:

```bash
Rscript sentiment_analysis.R data/toots.csv
```

## Hints and tips

The sentiment analysis is fairly straight forward but wrangling the data into
the right kind of format is tricky. Make good use of the `%>%` filter operator
to pass the data through various functions. I made good use of `inner_join`, 
`arrange`, `group_by` and `summerise` amongst others.

It might be useful to do some filters and store the output in a variable, 
the do an `inner_join` with the main data using the ID as hook. This might be
easier than wrestling with filters all day!

The graph doesn't need to be scientifically realistic or justified in any way. 

## The rules

You cannot alter any of the `expect_*()` calls in `test/test_script.R`

If you alter any function names in the main code, you *can* alter the name
in the test file to match; however the rest of the test must remain unchanged. 
This will be checked.

If you wish to add more tests, please do, but place them in a separate file
in the `test` directory. Remember to name the file `test_something.R`.

You can also add extra functionality, but the command-line interface must pass
the tests set.

