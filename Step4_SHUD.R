# Task:
# 1. Download SHUD++ from GitHub.
# 2. Compile SHUD++ locally. SHUD++ requires Sundials v3.1;  OpenMP and MPI is recommended if using parallel SHUD.
# 3. Run SHUD. And export the screen output of SHUD.
# 4.
# 5.
# 6.
# 7.
# 8.
rm(list=ls())
source('GetReady.R')
cdir=getwd()
CMD.EXE='shud'
# download SHUD from github.
# destfile=file.path(dir.out, 'SHUD_github.zip')
# download.file(url='https://github.com/SHUD-System/SHUD/archive/master.zip',
#               destfile = destfile)
system('git clone git@github.com:SHUD-System/SHUD.git shud_src')
# unzip and compile SHUD.
# unzip(zipfile = destfile, exdir = dir.out)
# setwd(file.path(dir.out, 'SHUD-4.0-master'))
setwd('shud_src')
cmd='make clean & make shud'
message('Compile SHUD: ')
message('\t', cmd)
system(cmd, wait = T, intern = FALSE)
file.rename(from=CMD.EXE, to =file.path('..', CMD.EXE))

# Run SHUD
setwd('../')
fn1 = 'shud'
fn2 = file.path('.', prjname, 'shud')
file.copy(from = fn1, to = fn2, overwrite = TRUE)
setwd(prjname)
cmd = paste( paste('./shud', prjname) )

message('Run SHUD: ')
message('\t', cmd)
sout <-system(cmd, wait = T,intern=TRUE, ignore.stdout = FALSE,
              ignore.stderr = FALSE)
write(sout, file.path(dir.SHUDout, paste0(prjname, '.log') ) )
setwd(cdir)
