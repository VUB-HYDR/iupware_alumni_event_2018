
### Pre & postprocessing MODFLOW ###
library(RMODFLOW)
library(ggplot2)
library(raster)


## read input data
# topography
nam_topo = raster("./data/nam_topo.tif")
plot(nam_topo)

# bottoms
nam_bottoms = brick("./data/nam_bottoms.tif")
plot(nam_bottoms)

## create input files
dis = rmf_create_dis(nlay = 16, nrow = 96, ncol = 146, nper = 1, itmuni = 4, lenuni = 2, 
                     laycbd = rep(0, 16), delr = rep(400, 146), delc = rep(400, 96),
                     top = as.matrix(nam_topo), 
                     botm = as.array(nam_bottoms),
                     perlen = 1, nstp = 1, tsmult = 1, sstr = 'SS')
str(dis)


# can read input files
bas = rmf_read_bas(dis = dis)
riv = rmf_read_riv(dis=dis, file = "./data/nam/input.riv")
pvl = rmf_read_pvl("./data/nam/input.pvl")
huf = rmf_read_huf("./data/nam/input.huf", dis=dis)
prj = rmf_read_prj("./data/nam/info.prj")


## Plot
rmf_plot(dis$top, dis=dis, bas=bas, prj=prj) 

# cross sections
rmf_plot(huf, dis=dis, bas=bas, prj=prj, i=45)

# stress packages
rmf_plot(riv, dis=dis, bas=bas, prj=prj, variable = 'stage', k = 1)
getwd()


## write input files
rmf_write_dis("./data/nam/input.dis", dis=dis)
# skip others

## forward run
mfex = 'C:/WRDAPP/MF2005.1_11/bin/mf2005.exe'
name_path = paste0(getwd(),'/data/nam/input.nam')

rmf_run_modflow(file = name_path, executable = mfex)

## read output & plot
# heads
heads = rmf_read_hed(file = "./data/nam/output.hed", dis=dis, bas=bas, binary=F)
rmf_plot(heads, dis=dis, bas=bas, prj=prj, k=4) 

# hpr
hpr = rmf_read_hpr("./data/nam/output.hpr")
rmf_plot(hpr)
rmf_performance(hpr)

# water balance
balance = rmf_read_balance('./data/nam/output.lst')
str(balance)
balance[[1]][[1]]


## sensitivity analysis
str(pvl)

sen = rmf_run_sen(file = name_path, executable = mfex )
rmf_plot(sen)
str(sen)

## optimization
# set up cost function

cost = function(values){
    
    # adjust & write parameters
    pvl$parval = values
    rmf_write_pvl(pvl, file = './data/nam/input.pvl')
    
    # run model
    run_modflow(file = name_path, executable = mfex)
    
    # read output and compute cost
    output = rmf_read_hpr("./data/nam/output.hpr")
    cost = rmf_performance(output)$rmse
    
    print(cost)
    return(cost)
    
}

# run algorithm
opt = optim(pvl$parval, cost, method = "BFGS")



