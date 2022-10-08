# RegCM4_3DSTSRE

This repository contains the codes for coupling the 3-Dimensional Sub-grid terrain Solar Radiative Effect (3DSTSRE) Parameterization Scheme (Huang et al., 2022) to the Regional Climate Model Version 4. The 3DSTSRE scheme is developed by Anning Huang et al. (School of Atmospheric Sciences, Nanjing University, China), which considers the terrain radiative effects on the surface solar radiation fluxes. The RegCM4 is developed and maintained by Abdus Salam International Centre for Theoretical Physics (ICTP) and available at https://github.com/ictp-esp/RegCM).

About the Codes:
The codes in this repository include three parts: 
1) the codes for calculations of sub-grid scale terrain elements (in the folder “CalSubgrid”); 
2) the codes for calculations of grid-scale 3DSTSRE modification factors (in the folder “CalGrid”);
3) the codes from the RegCM4 and revised to including the 3DSTSRE scheme (in the folder “RegCM4-3DSTSRE”).
List of codes from RegCM4 which are revised for coupling the 3DSTSRE scheme
\RegCM4\Main\mod_atm_interface.F90	
\RegCM4\Main\mod_init.F90	  
\RegCM4\Main\mod_params.F90
\RegCM4\Main\mod_rad_interface.F90 
\RegCM4\Main\mod_sun.F90
\RegCM4\Main\mod_tendency.F90
\RegCM4\Main\mpplib\mod_regcm_types.F90
\RegCM4\Main\radlib\mod_rad_colmod3.F90
\RegCM4\Main\radlib\mod_rad_radiation.F90
\RegCM4\Share\mod_sunorbit.F90	



Usage:
To run the RegCM4 with the 3DSTSRE scheme, 3 steps should be conducted. 
1) Download certain high resolution Digital Elevation Model (DEM) data (such as SRTM, Aster GDEM, TanDEM-X, et al.) and the use the codes in the folder “CalSubgrid” with the DEM data to calculate the sub-grid scale terrain elements.
2) Get the longitude and latitude information of the model grid from the domain file generated by the RegCM4. The domain file is name as “XXXX_DOMAIN000.nc”. “XXXX” is the name of the domain/experiment. Use the sub-grid scale terrain elements, longitude, latitude, and the codes in the folder “CalGrid” to calculate the model grid-scale 3DSTSRE modification factors.
3) Download the codes in the folder “RegCM4-3DSTSRE” and replace the original code (available at https://github.com/ictp-esp/RegCM or https://doi.org/10.5281/zenodo.7161718) in RegCM4 with them. Set the names and routines of the 3DSTSRE modification factor files in the code “/RegCM4/Main/mod_params.F90”. Then run the RegCM4 as usual. Search the key words “3DSTSRE” in these codes and then you can find the lines we added or revised.

Other things:
It will take a lot of computing resources and time to calculate the sub-grid skyview factor and maximal elevation angle. We have calculated the global sub-grid scale terrain elements with a resolution of ~90m. It is waste of time and resources to calculate these sub-grid scale terrain elements by yourself. However, due to the file size (~7 TB) and the copyright policy of the DEM data, we cannot upload these data to the web now. You can contact us and ask us to calculate the 3DSTSRE modification factors for you (only under the condition of non-commercial use). You can find the specific information about the 3DSTSRE scheme in the reference paper Huang et al. (2022). 

Reference:
Huang, A., Gu, C., Zhang, Y., et al. (2022). Development of a Clear‐Sky 3D Sub‐Grid Terrain Solar Radiative Effect Parameterization Scheme Based on the Mountain Radiation Theory. Journal of Geophysical Research: Atmospheres, 127(13). https://doi.org/10.1029/2022jd036449
Gu, C., Huang, A., Zhang, Y., et al. (2022). The Wet Bias of RegCM4 over Tibet Plateau in Summer Reduced by Adopting the 3D Sub-grid Terrain Solar Radiative Effect. Manuscript submitted to Journal of Geophysical Research: Atmospheres. [Paper #2022JD037434, Under Review] 

If there’s any question, please feel free to contact us.




                                                      Anning Huang (anhuang@nju.edu.cn)
                                                      Chunlei Gu (chlgu@smail.nju.edu.cn)
                                                           School of Atmospheric Sciences
                                                                       Nanjing University
                                                                           Nanjing, China
                                                                             Aug 22, 2022
























