path = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev"
app = "Rscript"
script1 = sprintf("batch_v01.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))
script2 = sprintf("batch_v11.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))

args1 = sprintf("%s/scripts/%s --configs %s/data/versions/v0/v0.015/v0.015.yaml %s/data/versions/v0/v0.021/v0.021.yaml %s/data/versions/v0/v0.028/v0.028.yaml %s/data/versions/v0/v0.033/v0.033.yaml", path, script1, path, path, path, path) 
args2 = sprintf("%s/scripts/%s --configs %s/data/versions/v1/v1.015/v1.015.yaml %s/data/versions/v1/v1.021/v1.021.yaml %s/data/versions/v1/v1.028/v1.028.yaml %s/data/versions/v1/v1.033/v1.033.yaml", path, script2, path, path, path, path)

system2(app, args1)
system2(app, args2)

charlier::sendmail(to = "llngai26@colby.edu", subject = "updating predictions")