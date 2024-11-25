source('GetReady.R')

shud.env(prjname = xfg$prjname, inpath = dir.modelin, outpath = dir.modelout)
outpath=dir.modelout
# cfg.para=readpara()
# print(cfg.para)
# dt.model = 2
# dt.out = max(dt.model, 60)
# cfg.para['MAX_SOLVER_STEP'] = dt.model
# cfg.para['END'] = 20
# id=which(grepl('DT_', names(cfg.para)))
# cfg.para[id]=dt.out;
# 
# cfg.calib=readcalib(); cfg.calib
# cfg.calib[c('EC', 'ETT', 'EDIR')]=dt.out
# 

fx.ds <- function(x){ dx = as.numeric(x[nrow(x),]) - as.numeric(x[1,]) }
pngout = file.path(outpath, 'figure')
dir.create(pngout, recursive = T, showWarnings = F)
pr=readriv()
oid=getOutlets(pr)
pm = readmesh()
ia=getArea()
AA=sum(ia)
seg=readrivseg()
cfg.para=readpara()
dt=as.numeric(cfg.para['DT_QR_DOWN'])
minfo = c(Ncell = length(ia), Nriv=nrow(pr@river), Nseg =nrow(seg),
          AreaMean = mean(ia))
xinit= readic()
vns=c(paste0('eley',c( 'surf' ) ), 
      paste0('elev',c('prcp') ),
      'elevinfil', 'elevrech', 'eleqsurf',
      'eleyunsat', 'eleygw',
      paste0('elevet', c('p', 'tr', 'ic', 'ev')),
      paste0('rivy','stage'),
      paste0('rivq',c('down', 'sub', 'surf')) ) 
att=readatt()
geol=readgeol(); soil=readsoil()
porosity = geol[att[, 'GEOL'], 4]- geol[att[, 'GEOL'], 5]
xl = BasicPlot(varname=vns, plot = T, imap = F, return = T, iRDS = F)

smax = apply(xl$eleysurf, 2, max)
spm=sp.mesh2Shape(dbf = smax)
prcp = xl$elevprcp[,1]
nt=nrow(prcp)
ma=t(matrix(rep(ia, nt), ncol=nt))
dim(ma)
wb=cbind(P=prcp, 
         Etp=rowSums(xl$elevetp*ma)/AA,
         Et=rowSums(xl$elevettr*ma)/AA,
         Ev=rowSums(xl$elevetev*ma)/AA,
         Ec=rowSums(xl$elevetic*ma)/AA,
         Qs=-rowSums(xl$rivqsurf)/AA, 
         Qg=-rowSums(xl$rivqsub)/AA, 
         Qd=xl$rivqdown[,oid]/AA) * dt 
twb=time(wb); tid=twb
plot(wb[tid,], col=c(4, 3, 4, 2), lty=c(1, 2, 2,2),  
     legend.loc='topright')

dsf=sum((last(xl$eleysurf) -xinit$minit[,4] )* ia * 1 )/AA 
dus=sum((last(xl$eleyunsat) -xinit$minit[,5]) * ia * porosity)/AA  
dgw=sum((last(xl$eleygw) -xinit$minit[,6] )* ia * porosity)/AA
rivArea = pr@rivertype[, 'Width'] * pr@river[,'Length']
dst=sum( (as.numeric(last(xl$rivystage)) - xinit$rinit[,2]) * rivArea ) /AA
dst
ss=c(apply(wb, 2,sum), 
     dsf=dsf, dus=dus, dgw=dgw, dst=dst)
ss
P = ss[1]; Q = ss['Qd']; DS = sum(ss[c('dsf', 'dus', 'dgw', 'dst')]); 
E = sum(ss[c("Et", "Ev", "Ec" )])
pqe=P-Q-E
# message('P=',P, '\tQ=',Q, '\tE=', E )
# message('P-Q-E = ',pqe)
# message('DS(sim) =', DS)
Snet = P-E-Q-DS; Snet
MB = Snet/P
minfo
summary(ia)

message('Balance: ');print(ss)
message('P=', round(P,3), '\tQ=',round(Q,3), '\tE=', round(E,3) )
message('P-Q-E = ',round(pqe,3))
message('DS(sim) =', round(DS,3))

message('Mass-Balance Error = ', 
        formatC(Snet, format = "e", digits = 1), 'mm\t',
        round(MB*100, 3), '%')
plot(xl$rivystage)

