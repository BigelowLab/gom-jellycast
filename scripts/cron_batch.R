path <- "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev"
app <- "Rscript"

# script1 <- sprintf("batch_v02.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))
# script2 <- sprintf("batch_v12.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))
# 
# args1 = sprintf("%s/scripts/%s --configs %s/data/versions/v0/v0.015/v0.015.yaml %s/data/versions/v0/v0.038/v0.038.yaml", path, script1, path, path)
# args2 = sprintf("%s/scripts/%s --configs %s/data/versions/v1/v1.015/v1.015.yaml %s/data/versions/v1/v1.038/v1.038.yaml", path, script2, path, path)
# 
# out1 <- system2(app, args1, stdout = TRUE, stderr = TRUE)
# charlier::sendmail(to = "llngai26@colby.edu", subject = "done running batch_v02.R")
# status1 <- attr(out1, "status")
# if (is.null(status1)) status1 <- 0
# if (status1 != 0) {
#   charlier::sendmail(to = "llngai26@colby.edu", subject = "error in batch_v02.R", message = out1)
# }
# 
# out2 <- system2(app, args2, stdout = TRUE, stderr = TRUE)
# charlier::sendmail(to = "llngai26@colby.edu", subject = "done running batch_v12.R")
# status2 <- attr(out2, "status")
# if (is.null(status2)) status2 <- 0
# if (status2 != 0) {
#   charlier::sendmail(to = "llngai26@colby.edu", subject = "error in batch_v12.R", message = out2)
# }

script1 <- sprintf("batch_v23.R --dates %s %s", format(Sys.Date(), "%Y-%m-%d"), format(Sys.Date() + 8, "%Y-%m-%d"))
args1 <- sprintf("%s/scripts/%s --configs %s/data/versions/v2/v2.001/v2.001.yaml %s/data/versions/v2/v2.080/v2.080.yaml", path, script1, path, path)

system2(app1, args1)

script2 <- sprintf("batch_v4.R --dates %s %s", format(Sys.Date(), "%Y-%m-%d"), format(Sys.Date() + 8, "%Y-%m-%d"))
args2 <- sprintf("%s/scripts/%s --configs %s/data/versions/v4/v4.001/v4.001.yaml %s/data/versions/v4/v4.160/v4.160.yaml", path, script2, path, path)

system2(app2, args2)


charlier::sendmail(to = "llngai26@colby.edu", subject = "updating forecasts")
