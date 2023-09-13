# Guided-Missile-Radar-IR-EOS-Simulation

This project attempts to model and simulate the innerworking of Radar system, Electro-Optical active and passive sources of IR radiation and missile guidance system. The main purpose is to achieve a realisitic system modeling and simulation as much as possible, hence the main sources of knowledge are russian-language (soviet-era) engineering and technical literature reaching design level (candidate of science and experienced system engineers and designers). The second firm foundation which this project stand upon is being wholly optimized at basic level of massive manual vectorization by relying of Fortran array syntax,
better known as a packed array of small structures easily replicating SIMD registers of the CPU. This kind of approach allows mimicking of SIMD (C) intrinsics and leverages compiler autovectorization of the
computational kernels.
The helper code execution path rely on libSIMD which collects various manually vectorized algorithms written in C and may provide a collection of helper routines for some tasks, albeit at the cost of losing the possibility of function body inlining.
Compiler-level autovectorization is of secondary importance and is being inserted mainly to vectorize descriptive statistics routines and profiling metrics calculations. The second code path beside the packed arrays of structures aka PAOS is the GPGPU Cuda implementation counting so far close to 15000 lines of code of computational and helper routines and kernels.
The main programming language of this project is Fortran 90 helped by C language implementation.
I envision four main components:

Radar system modeling and simulation.
Radio altimeter modeling and simulation.
Propagation of laser and IR radiation through the turbulent atmospheric channels.
Optical signals processing (background noise extraction).
The main structure of the projects is a collection of free standing 'modules' programmatically describing various modelled components. It is a software library of framework and may be used as computational backend of larger program of be connected to GUI front-end. Currently only hundreds (circa 400) kernels belonging to AVX512 double and single precision executing path were implemented. All of these kernels compute analytical Radar Cross Section of simple and to lesser extent complex objects

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
    
  4) Principles of High-Resolution Radar (Artech House Radar Library), August W. Rihaczek ISBN-10: 089006900X
     ISBN-13: 978-0890069004

  5) Леонов А.И. - Моделирование в радиолокации (1979, Сов. радио )

  6) Кремер И.Я. - Модулирующие помехи и прием радиосигналов (1972)

  7) Abramowitz M., Stegun I.A. (eds.) - Handbook of mathematical functions (1972, NBS)

  8) Шифрин Я.С. - Вопросы статистической теории антенн

  9) Горяинов В.Т., Журавлев А.Г., Тихонов В.И - Статистическая радиотехника. Примеры и задачи (1980, Советское радио)

  10) George T. Ruck, Donald E. Barrick , William D. Stuart , - Radar Cross Section Handbook  (1970, Kluwer Academic Plenum Publishers)

  11) Тихонов В. И. Статистическая радиотехника. «Сов. радио», 1966

  12) Вайнштейн Л. А., Зубаков В. Д. Выделение сигналов на фоне случайных помех. «Сов. радио», 1960.

  13) Зубковнч С. Г. Статистические характеристики радиосигналов, отраженных от земной поверхности. «Сов. радио», 1968


