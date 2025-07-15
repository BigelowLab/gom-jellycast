path = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev"
app = "Rscript"
script1 = sprintf("batch_v02.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))
script2 = sprintf("batch_v12.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))

args1 = sprintf("%s/scripts/%s --configs %s/data/versions/v0/v0.001/v0.001.yaml %s/data/versions/v0/v0.038/v0.038.yaml", path, script1, path, path)
args2 = sprintf("%s/scripts/%s --configs %s/data/versions/v1/v1.015/v1.015.yaml %s/data/versions/v1/v1.038/v1.038.yaml", path, script2, path, path)

system2(app, args1)
system2(app, args2)

charlier::sendmail(to = "llngai26@colby.edu", subject = "updating predictions")

