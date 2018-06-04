
# packages
library(icesTAF)

# make data directopry
mkdir("data")

# script to create data or copy data from another directory

#cp("../../ices-eg/wg_WGSFD/data/QC_2018/ICES_LE_BEL.csv", "data/LE.csv")
#cp("../../ices-eg/wg_WGSFD/data/QC_2018/ICES_VE_BEL.csv", "data/VE.csv")

# LE format:
# recordtype	country	year	month	ICES_rectangle	gear_code	LE_MET_level6	fishing_days	vessel_length_category	vms_enabled	kw_fishing_days	totweight	totvalue

# VE format:
# recordtype	country	year	month	c_square	vessel_length_category	gear_code	LE_MET_level6	avg_fishing_speed	fishing_hours	avg_oal	avg_kw	kw_fishinghours	totweight	totvalue	ICES_avg_fishing_speed	avg_gearWidth

# use NA for where data is not available

