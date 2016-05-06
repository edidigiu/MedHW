#!/bin/sh


for var in tx tn
do


FILEOUT="Data/ECAD-SUB_"$var"90_filled_binary.nc"
if [ -e "$FILEOUT" ]
then
	echo "il file esiste..."
else
# per la finestra a cinque termini che poi diventano 150 anni 
#
# ncks -d longitude,6.0,19.0 -d latitude,36.0,48.0 tx_0.25deg_reg_v10.0.nc ITA_tx_0.25deg_reg_v10.0.nc
#

	cdo settaxis,1901-01-01,12:00:00,1day -del29feb -selyear,1971/2000 -shifttime,-2days MED_"$var"_0.25deg_reg_v12.0.nc Data/Appo1.nc
	cdo settaxis,1931-01-01,12:00:00,1day -del29feb -selyear,1971/2000 -shifttime,-1day MED_"$var"_0.25deg_reg_v12.0.nc Data/Appo2.nc
	cdo settaxis,1961-01-01,12:00:00,1day -del29feb -selyear,1971/2000 MED_"$var"_0.25deg_reg_v12.0.nc Data/Appo3.nc
	cdo settaxis,1991-01-01,12:00:00,1day -del29feb -selyear,1971/2000 -shifttime,1day MED_"$var"_0.25deg_reg_v12.0.nc Data/Appo4.nc
	cdo settaxis,2021-01-01,12:00:00,1day -del29feb -selyear,1971/2000 -shifttime,2days MED_"$var"_0.25deg_reg_v12.0.nc Data/Appo5.nc

	cdo mergetime Data/Appo?.nc Data/Glob-"$var"-LongTerm.nc

	cdo selyear,2050 -ydaypctl,90 Data/Glob-"$var"-LongTerm.nc -ydaymin Data/Glob-"$var"-LongTerm.nc -ydaymax Data/Glob-"$var"-LongTerm.nc Data/PCTL-90_MED_"$var"_0.25deg_reg_v12.0.nc

	cdo gec,0 -fillmiss -sub -del29feb MED_"$var"_0.25deg_reg_v12.0.nc Data/PCTL-90_MED_"$var"_0.25deg_reg_v12.0.nc Data/ECAD-SUB_"$var"90_filled_binary.nc

	rm -f Data/Appo*.nc 
	rm -f Data/Glob*.nc
fi
done


	
FILEOUT2="Data/tn_ECAD_Begin_HeatWave.nc"
# semplice check se il secondo file è stato creato
#
if [ -e "$FILEOUT2" ]
then
        echo "il file creato da R esiste..."
else
	/usr/bin/R --slave -f HeatWave_3files_generator.R

fi

# Reset della variabile tempo nei files prodotti da R/selezione mesi Mag-Sept:
for var in tx tn
do
	for i in Begin Duration #MonthCrossing
	do
		#####
		# METTERE I FILES DENTRO la dir: Data
		ncrename -O -v z,time -d z,time Data/"$var"_ECAD_"$i"_HeatWave.nc Data/Test.nc
		# i file ECAD partono dal 1951 qui c'era un settaggio al 1950. Corretto al 1951 il 10.11.2015
		cdo settaxis,1951-01-01,12:00:00,1day -setcalendar,standard -settunits,day Data/Test.nc Data/"$var"_ECAD_"$i"_HeatWave2.nc
		#Corretto file HeatWave_3files_generator.R con settaggio TIME (21 Nov 2015)---la seguente riga potrebbe essere superflua---:
#		cdo settaxis,1951-01-01,12:00:00,1day Data/Test.nc Data/"$var"_ECAD_"$i"_HeatWave.nc
		cdo -ifthen ./MaskTXOk.nc -invertlat Data/"$var"_ECAD_"$i"_HeatWave2.nc Data/"$var"_ECAD_"$i"_HeatWave3.nc	
#		cdo -ifthen ./TopoGlobe-MED.nc -invertlat Data/"$var"_ECAD_"$i"_HeatWave2.nc Data/"$var"_ECAD_"$i"_HeatWave3.nc	
#		cdo -ifthen ./TopoGlobe-MED.nc -invertlat Data/"$var"_ECAD_"$i"_HeatWave.nc Data/"$var"_ECAD_"$i"_HeatWave4.nc
	done

done



rm -f  Data/Test.nc
