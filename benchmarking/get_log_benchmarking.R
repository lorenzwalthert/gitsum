library("microbenchmark")
bench <- microbenchmark(read_only = get_log_regex(file_name = "tests/testthat/test_logs/.log_refactor.txt"),
                        write_and_read = get_log_regex(path = "tests/testthat/test_repos/refactor"),
               times = 3, unit = "s")
