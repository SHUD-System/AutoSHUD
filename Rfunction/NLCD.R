#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 10:53:00 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
#' 

NLCD2lc <- function (years=1979:2016){
    att=readatt()
    lc= att[,4];
        message('LC  = ')
    ulc=sort(unique(att[,4]))
        print( ulc)
    if (length( which (ulc <10 | ulc >100) ) ){
        stop('Current LC code is not NLCD')
    }
    # VEGPRMT.TBL
    veg = fun.vegtable(lc =ulc )
    if (PIHMVER ==2.4){
        writeveg(path=inpath,fn=paste0(PRJNAME,'.lc'), x=veg)
    }else{
        writeveg(path=dirname(inpath), x=veg)
    }

    # att file
    for (i in 1:length(ulc)){
         key = ulc[i];
        lc[which(lc == key) ]= i
    }
    att[,'LC']= lc
    #att[,'LAI']=lc
    writeatt(att)
    
    #.lai FILE.
    lr = fun.lairl(lc=ulc,years=years)
    writelai(x=lr)
}



lc.OID2NLCD <- function(lc){
    att <- readatt();
#lc=sort(unique( att[,'LC'] ) );

    oid <- att[,'LC'] ;
    ids <- numeric(length(lc));
    len <- length(lc);

    for( i in 1:len) {
        att[oid==i,'LC']=lc[i];
    }
    ret <- att;
   writeatt(att);
}
lc.NLCD2OID <- function(){
    att <- readatt();
    lc=sort(unique( att[,'LC'] ) );
    if (min(lc) < 11 | max(lc) > 95){
        print(lc)
        stop('Check the value of LC in att, they may not be the NLCD code')
    }
    oid <- att[,'LC'] ;
    len <- length(lc);

    
    write.table(file=paste0(PRJNAME,'.NLCD.csv'), 
                cbind('OID'=1:len, 'NLCD'=lc),
                quote=FALSE, row.names=FALSE)
    for( i in 1:len) {
        att[oid==lc[i],'LC']=i;
    }
    ret <- att;
    
   writeatt(att);
}
