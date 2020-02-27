# Task:
# 1. Download PIHM++ from GitHub.
# 2. Compile PIHM++ locally. PIHM++ requires Sundials v3.1;  OpenMP and MPI is recommended if using parallel PIHM.
# 3. Run PIHM. And export the screen output of PIHM.
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
# destfile=file.path(dir.out, 'PIHM_github.zip')
# download.file(url='https://github.com/SHUD-System/SHUD/archive/master.zip',
#               destfile = destfile)
system('git clone git@github.com:SHUD-System/SHUD.git shud_src')
# unzip and compile SHUD.
# unzip(zipfile = destfile, exdir = dir.out)
# setwd(file.path(dir.out, 'PIHM-4.0-master'))
setwd('shud_src')
cmd='make clean & make shud'
message('Compile PIHM: ')
message('\t', cmd)
system(cmd, wait = T, intern = FALSE)
file.rename(from=CMD.EXE, to =file.path('..', CMD.EXE))

# Run PIHM
setwd('../')
fn1 = 'shud'
fn2 = file.path('.', prjname, 'shud')
file.copy(from = fn1, to = fn2, overwrite = TRUE)
setwd(prjname)
cmd = paste( paste('./shud', prjname) )

message('Run PIHM: ')
message('\t', cmd)
sout <-system(cmd, wait = T,intern=TRUE, ignore.stdout = FALSE,
              ignore.stderr = FALSE)
write(sout, file.path(dir.pihmout, paste0(prjname, '.log') ) )
setwd(cdir)
