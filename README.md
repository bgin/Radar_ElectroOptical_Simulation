# Missile_Modeling_Simulation
Air-to-Air Missile Simulation written in modern Fortran
This is newer and updated version of my older "Missile Simulation" project
written in C++.
Main changes are related to drastically improved modeling of Radar interaction
with targets and environment.
T-Matrix models(by Mishchenko and by Xu) will be used to compute scattering coefficients returned by
single particles and ensemble of particles.
Large scatterers will be modelled as a OBJ, gmsh meshes and later converted to nec format files.
Radar scattering will be calculated by Method of Moments with the help of modified NEC-2 program.
Atmospheric quantities and wind will be based on WRF modules and WRF output files.
Work in progress...

List of references:

1) Detection, Estimation, and Modulation Theory Part III: Radar-Sonar Signal Processing and Gaussian Signals in Noise
   Harry L. van Trees
   ISBN-10: 047110793X
   ISBN-13: 978-0471107934
   
 2) Detection Estimation and Modulation Theory, Part I: Detection, Estimation, and Filtering Theory 
    Harry L. van Trees
    ISBN-10: 9780470542965
    ISBN-13: 978-0470542965
    ASIN: 0470542969
    
3) Automatic Control of Aircraft and Missiles 
   John H. Blakelock
   ASIN: B01FJ0JOU2
