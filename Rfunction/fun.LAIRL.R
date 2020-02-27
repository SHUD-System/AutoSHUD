#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu )
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4} ,
#'

cfun <- function (x,tab, type=1) {
  #Source: http://www.pihm.psu.edu/EstimationofVegitationParameters.htm
  dlai=rbind(c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
             c(   8.76,  9.16,  9.827,  10.093,  10.36,  10.76,  10.493,  10.227,  10.093,  9.827,  9.16,  8.76),
             c(   5.117,  5.117,  5.117,  5.117,  5.117,  5.117,  5.117,  5.117,  5.117,  5.117,  5.117,  5.117),
             c(    8.76,  9.16,  9.827,  10.093,  10.36,  10.76,  10.493,  10.227,  10.093,  9.827,  9.16,  8.76),
             c(    0.52,  0.52,  0.867,  2.107,  4.507,  6.773,  7.173,  6.507,  5.04,  2.173,  0.867,  0.52),
             c(    4.64,  4.84,  5.347,  6.1,  7.4335,  8.7665,  8.833,  8.367,  7.5665,  6,  5.0135,  4.64),
             c(    5.276088,  5.528588,  6.006132,  6.4425972,  7.2448806,  8.3639474,  8.540044,  8.126544,  7.2533006,  6.3291908,  5.6258086,  5.300508),
             c(   2.3331824,  2.4821116,  2.7266101,  3.0330155,  3.8849492,  5.5212224,  6.2395131,  5.7733017,  4.1556703,  3.1274641,  2.6180116,  2.4039116 ),
             c(   0.580555,  0.6290065,  0.628558,  0.628546,  0.919255,  1.7685454,  2.5506969,  2.5535975,  1.7286418,  0.9703975,  0.726358,  0.6290065 ),
             c(    0.3999679,  0.4043968,  0.3138257,  0.2232945,  0.2498679,  0.3300675,  0.4323964,  0.7999234,  1.1668827,  0.7977234,  0.5038257,  0.4043968),
             c(    0.782,  0.893,  1.004,  1.116,  1.782,  3.671,  4.782,  4.227,  2.004,  1.227,  1.004,  0.893),
             c(    0.782,  0.893,  1.004,  1.116,  1.782,  3.671,  4.782,  4.227,  2.004,  1.227,  1.004,  0.893),
             c(   0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001 ),
             c(    1.2867143,  1.3945997,  1.5506977,  1.7727263,  2.5190228,  4.1367678,  5.0212291,  4.5795799,  2.8484358,  1.8856229,  1.5178736,  1.3656797)
  );
  drl=rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            c(	1.112, 1.103, 1.088, 1.082, 1.076, 1.068, 1.073, 1.079, 1.082, 1.088, 1.103, 1.112),
            c(	2.653, 2.653, 2.653, 2.653, 2.653, 2.653, 2.653, 2.653, 2.653, 2.653, 2.653, 2.653),
            c(	1.112, 1.103, 1.088, 1.082, 1.076, 1.068, 1.073, 1.079, 1.082, 1.088, 1.103, 1.112),
            c(	0.52, 0.52, 0.666, 0.91, 1.031, 1.044, 1.042, 1.037, 1.036, 0.917, 0.666, 0.52),
            c(	0.816, 0.8115, 0.877, 0.996, 1.0535, 1.056, 1.0575, 1.058, 1.059, 1.0025, 0.8845, 0.816),
            c(	0.7602524, 0.7551426, 0.7772204, 0.8250124, 0.846955, 0.8449668, 0.8471342, 0.8496604, 0.8514252, 0.8299022, 0.7857734, 0.7602744),
            c(	0.35090494, 0.34920916, 0.36891486, 0.40567288, 0.42336056, 0.42338372, 0.42328378, 0.42485112, 0.42631836, 0.40881268, 0.37218526, 0.35096866),
            c(	0.05641527, 0.05645892, 0.05557872, 0.05430207, 0.05425842, 0.05399002, 0.05361482, 0.0572041, 0.05892068, 0.05821407, 0.05709462, 0.05645892),
            c(	0.03699235, 0.03699634, 0.03528634, 0.03272533, 0.03272134, 0.03270066, 0.03268178, 0.03907616, 0.04149324, 0.04032533, 0.03823134, 0.03699634),
            c(	0.0777, 0.0778, 0.0778, 0.0779, 0.0778, 0.0771, 0.0759, 0.0766, 0.0778, 0.0779, 0.0778, 0.0778),
            c(	0.0777, 0.0778, 0.0778, 0.0779, 0.0778, 0.0771, 0.0759, 0.0766, 0.0778, 0.0779, 0.0778, 0.0778),
            c(	0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112),
            c(	0.1947138, 0.19413424, 0.20831414, 0.23348558, 0.24574614, 0.24605016, 0.24538258, 0.24630454, 0.247455, 0.23527388, 0.20963734, 0.19478494)
  );
  if(missing('tab') ){     #undefined table, use the default table.
    tab=switch(type,'lai'=dlai, 'rl'=drl)
  }
  ret = tab[x,]
}


rep.row<-function(x,n){
  for(i in 1:n){
    if(i==1){
      ret = x;
    }else{
      ret=rbind(ret, x)
    }
  }
  return(ret)
}

rep.col<-function(x,n){
  for(i in 1:n){
    if(i==1){
      ret = x;
    }else{
      ret=cbind(ret, x)
    }
  }
  return(ret)
}

GLC.LaiRf <- function(lc,years=2000+1:2, if.daily=FALSE){
  #years=2000:(2010+1);
  years=sort(c(years,max(years)+1))
  yrlim=range(years);
  ny = length(years)
  t1=as.Date(paste(yrlim[1],'-01-01',sep=''))
  t2=as.Date(paste(yrlim[2],'-12-31',sep=''))
  tdaily = seq.Date(t1,t2,by=1)
  DataDaily=xts::as.xts(numeric(length(tdaily)),order.by=tdaily)
  DataMon=xts::apply.monthly(DataDaily,FUN=sum)
  tmon =as.Date( format(time(DataMon), "%Y-%m-01"))
  #tmon = time(DataMon)- days_in_month(time(DataMon))+1
  nlc=length(lc)
  l = matrix(0, nrow=12, ncol=nlc)
  r = matrix(0, nrow=12, ncol=nlc)
  for (i in 1:nlc){
    l[,i] = cfun(lc[i], type=1)
    r[,i] = cfun(lc[i], type=2)
  }
  lmat = xts::as.xts(rep.row(l, ny), order.by=tmon)
  rmat = xts::as.xts(rep.row(r, ny), order.by=tmon)
  colnames(lmat)=lc
  colnames(rmat)=lc
  ret=list('LAI'=lmat, 'RL'=rmat)
  if(if.daily){
    ld = NA*rep.col(DataDaily, nlc);
    rd = NA*rep.col(DataDaily, nlc);
    ld[time(lmat),]=lmat
    rd[time(rmat),]=rmat
    ld=na.approx(ld)
    rd=na.approx(ld)
    colnames(ld)=lc
    colnames(rd)=lc
    ret=list('LAI'=ld, 'RL'=rd)
  }
  return(ret)
}

fun.MeltFactor <- function(years=2000+1:2){
  mf=c(0.001308019, 0.001633298,  0.002131198, 0.002632776, 0.003031171,  0.003197325, 0.003095839, 0.00274524,     0.002260213, 0.001759481, 0.001373646,  0.001202083);
  years=sort(c(years,max(years)+1))
  yrlim=range(years);
  ny = length(years)
  t1=as.Date(paste(yrlim[1],'-01-01',sep=''))
  t2=as.Date(paste(yrlim[2],'-12-31',sep=''))
  tdaily = seq.Date(t1,t2,by=1)
  DataDaily=as.xts(numeric(length(tdaily)),order.by=tdaily)
  DataMon=apply.monthly(DataDaily,FUN=sum)
  #tmon = time(DataMon)- days_in_month(time(DataMon))+1
  tmon =as.Date( format(time(DataMon), "%Y-%m-01"))
  ret = as.xts(rep(mf, ny), order.by=tmon)
  ret
}

fun.vegtable <- function (lc, file){
  x=read.csv(file=file.path('table','lc_table.csv'), header = T)
  y = x[lc,1:8]
  nr = nrow(y)
  write(nr, file=file, append = F)
  write.table(y, file=file, append=T,  row.names = F, col.names = F, quote = F)
  y
}
