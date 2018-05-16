
# packages
library(icesTAF)

# make data directopry
mkdir("data")

# script to create data or copy data from another directory

cp("../../ices-eg/wg_WGSFD/data/QC_2018/ICES_LE_BEL.csv", "data/LE.csv")
cp("../../ices-eg/wg_WGSFD/data/QC_2018/ICES_VE_BEL.csv", "data/VE.csv")
