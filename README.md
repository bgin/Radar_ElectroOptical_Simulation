# Missile_Modeling_Simulation
Air-to-Air Missile Simulation written in modern Fortran
This is newer and updated version of my older "Missile Simulation" project
written in C++.
Main changes are related to drastically improved modeling of Radar interaction
with targets and environment.
T-Matrix models(by Mishchenko and by Xu) will be used to compute scattering coefficients returned by
single particles and ensemble of particles.
Large scatterers will be modelled as a OBJ, gmsh meshes or converted nec files.
Radar scattering will be calculated by Method of Moments with the help of modified NEC-2 program.
Atmospheric quantities and wind will be based on WRF modules and WRF output files.
