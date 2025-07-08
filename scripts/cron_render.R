# usage: render.R [--] [--help] [--push] [--doc DOC] [--path PATH]
# 
# Render and optionaly push documentation
# 
# flags:
#   -h, --help  show this help message and exit
#   --push      push repos to github?
#   
#   optional arguments:
#   -d, --doc   the name of the document to render [default: README.Rmd]
#   -p, --path  the path to the document [default:
#                                         /mnt/s1/projects/ecocast/projects/tcof-reporter]

suppressPackageStartupMessages({
  library(argparser)
})


Args = argparser::arg_parser("Render and push documentation",
                             name = "cron_render.R",
                             hide.opts = TRUE) |>
  add_argument("--doc", 
               default = "README.Rmd",
               help = "the name of the document to render") |>
  add_argument("--path",
               default = "/mnt/s1/projects/ecocast/projects/gom-jellycast-dev",
               help = 'the path to the document') |>
  add_argument("--push",
               flag = TRUE,
               help = "push repos to github?") |>
  parse_args()

orig_path = setwd(Args$path)
# system2("quarto", sprintf("render %s", Args$path))
cmd = sprintf("/opt/bin/quarto render %s", Args$path)
system(cmd)


if (Args$push){
  # add, commit, push
  ok = system("git add *")
  if (ok == 0){
    ok = system(sprintf("git commit -a -m '%s'", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
    if (ok == 0){
      ok = system("git push origin main")
      if (ok != 0) stop("unable to push to github")
    } else {
      stop("unable to make commitment")
    }
  } else {
    stop("unable to add new items to repos")
  }
}

setwd(orig_path)
if (!interactive()) quit(save = "no", status = 0)