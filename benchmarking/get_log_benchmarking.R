library("microbenchmark")
bench <- microbenchmark(read_only = get_log_regex(file_name = "tests/testthat/test_logs/.log_refactor.txt"),
                        write_and_read = get_log_regex(path = "../refactor"),
               times = 10, unit = "s")


microbenchmark(stringr = str_match("Date:   Sat May 20 22:33:30 2017 +0200", "^Date:\\s{3}(\\w+)\\s"),
               stringr_full = str_match("Date:   Sat May 20 22:33:30 2017 +0200", "^Date\\:\\s*(\\w+)\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s.*"),
               base_r = gsub("^Date\\:\\s*(\\w+)\\s\\w+\\s+\\d+\\s\\d+:\\d+:\\d+\\s\\d+\\s.*", "\\1", "Date:   Sat May 20 22:33:30 2017 +0200"),
               times = 100, unit = "ms")


write_rds(bench, "benchmarking/at_86acd_dplyr_parse_one2_extract_multiple.rds")
