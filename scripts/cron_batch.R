path = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev"
app = "Rscript"
script1 = sprintf("batch_v01.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))
script2 = sprintf("batch_v11.R %s", format(Sys.Date() + 8, "%Y-%m-%d"))

args1 = sprintf("%s/scripts/%s --configs %s/data/versions/v0/v0.015/v0.015.yaml %s/data/versions/v0/v0.021/v0.021.yaml %s/data/versions/v0/v0.028/v0.028.yaml %s/data/versions/v0/v0.033/v0.033.yaml", path, script1, path, path, path, path) 
args2 = sprintf("%s/scripts/%s --configs %s/data/versions/v1/v1.020/v1.020.yaml %s/data/versions/v1/v1.022/v1.022.yaml %s/data/versions/v1/v1.027/v1.027.yaml %s/data/versions/v1/v1.036/v1.036.yaml", path, script2, path, path, path, path)

system2(app, args1)
system2(app, args2)

charlier::sendmail(to = "llngai26@colby.edu", subject = "updating predictions")