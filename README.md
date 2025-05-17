# Radar-IR-EOS-Simulation

This project attempts to model and simulate the innerworking of Radar system, Electro-Optical active and passive sources of IR radiation. The main purpose is to achieve a realisitic system modeling and simulation as much as possible, hence the main sources of knowledge is engineering and technical literature reaching design level (candidate of science and experienced system engineers and designers). The second firm foundation which this project stand upon is being wholly optimized at basic level of massive manual vectorization by relying of Fortran array syntax,
better known as a packed array of small structures easily replicating SIMD registers of the CPU. This kind of approach allows mimicking of SIMD (C) intrinsics and leverages compiler autovectorization of the
computational kernels.
The helper code execution path rely on libSIMD which collects various manually vectorized algorithms written in C and may provide a collection of helper routines for some tasks, albeit at the cost of losing the possibility of function body inlining.

Compiler-level autovectorization is of secondary importance and is being inserted mainly to vectorize descriptive statistics routines and profiling metrics calculations. The second code path beside the packed arrays of structures aka PAOS is the GPGPU Cuda implementation counting so far close to 15000 lines of code of computational and helper routines and kernels.
The main programming language of this project is Fortran 90 helped by C language implementation.

I envision five main components:

1) Radar system modeling and simulation.
2) Radio altimeter modeling and simulation.
3) Propagation of laser and IR radiation through the turbulent atmospheric channels.
4) Optical signals processing (background noise extraction).
5) Electro-optical sensor modeling and simulation.
   
The main structure of the projects is a collection of free standing 'modules' programmatically describing various modelled components. It is a software library of framework and may be used as computational backend of larger program of be connected to GUI front-end. Currently only hundreds (circa 400) kernels belonging to AVX512 double and single precision executing path were implemented. All of these kernels compute analytical Radar Cross Section of simple and to lesser extent complex objects


  


