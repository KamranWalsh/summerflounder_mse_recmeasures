#####################################################
#####################################################
##    Function for calling in gam object           ##
##      and predicting with it                     ##
##      J. McNamee   7/03/19                       ##
##      G. Fay       8/09/19                       ##
#####################################################
#####################################################
#needs to go at top of script for FORTRAN code
args = commandArgs(trailingOnly=TRUE)

#Switch argument is: 1 = mean prediction, 2 = observation sampled from uncertainty

pred.catch = function(MinLen, Bag, SeasonLen, switch) {

  #load("C:/Z Drive stuff/ASMFC/TCs/Fluke Scup BCB info/Summer Flounder/2019/Fluke_MSE/gam_obj.RData")
  #load("data/gam_obj.RData")

    gam_obj_land <- readRDS("gam_land.rds") #sbs.mdl.land
    gam_obj_disc <- readRDS("gam_disc.rds") #sbs.mdl.disc

    # Bag <- new_regs[1]
    # MinLen <- new_regs[2]
    #
    # set up data frame
    len <- seq(10,28,1)
    states <- c("DE", "MD", "NJ", "NY", "VA", "CT", "MA", "RI", "NC")
    waves <- 2:6

    dat.all <- expand.grid(State = states,
                           Length = len,
                           Bag = Bag,
                           MinLen = MinLen,
                           Wave = waves)
    dat.all$SeasonLen <- SeasonLen[dat.all$Wave-1]



###Output

    if (switch ==1) {

      land <- mgcv::predict.gam(gam_obj_land, newdata = dat.all, type = "response")
      disc =  mgcv::predict.gam(gam_obj_disc, newdata = dat.all, type= "response")
      output = cbind(dat.all, land, disc)
      return(output)

    }  #end of switch condition 1

    if (switch ==2) {

      land <- simulate(gam_obj_land, nsim = 2, seed = 42, newdata = dat.all)[,1]
      disc <- simulate(gam_obj_disc, nsim = 2, seed = 42, newdata = dat.all)[,1]
      output = cbind(dat.all, land, disc)
      return(output)
    }  #end of switch condition 2


}  #end of function



wave_seasons <- data.frame(seas = seq(60,300,15),
                           w2 = c(rep(0,9),seq(15,60,15),rep(60,4)),
                           w3 = c(rep(0,1),seq(15,60,15),rep(60,12)),
                           w4 = rep(60,17),
                           w5 = c(rep(0,5),seq(15,60,15),rep(60,8)),
                           w6 = c(rep(0,13),seq(15,60,15)))

print(args)
# generate realized catch based on new regulations
x <- pred.catch(MinLen = as.integer(args[2]),
                Bag = as.integer(args[1]),
                SeasonLen = as.numeric(wave_seasons[wave_seasons$seas==as.integer(args[3]),-1]),
                switch = 2)
#write(sum(x),file="recland.out")
print(c(sum(x$land),sum(x$disc)))
write(c(as.integer(sum(x$land)),as.integer(sum(x$disc))),file="recland.out")
xx <- dplyr::group_by(x, Length) %>% summarize(land = sum(land),
                                               disc = sum(disc))
write.table(t(as.integer(xx$land)),file="recland.out", append = TRUE, col.names = FALSE, row.names = FALSE)
write.table(t(as.integer(xx$disc)),file="recland.out", append = TRUE, col.names = FALSE, row.names = FALSE)

print("done!")



