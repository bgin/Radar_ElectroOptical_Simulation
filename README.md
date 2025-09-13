# Radar-ElectroOptical-Simulation

This project provides a high-fidelity simulation framework for Radar systems and Electro-Optical (EO) sources, including both active and passive infrared (IR) radiation modeling. The goal is to achieve realistic, design-level system modeling and simulation, following standards and methodologies found in advanced engineering and technical literature.

## Key Features

- **Comprehensive System Modeling:**  
  Models core components of radar and electro-optical systems, including analytical Radar Cross Section (RCS) calculations and antennae radiation characteristics.
- **Performance-Optimized Kernels:**  
  Implements highly optimized algorithms, leveraging Intel Intrinsics (SSE/AVX/AVX2/AVX512) for massive manual vectorization. Compiler-level autovectorization is used for descriptive statistics and profiling.
- **GPGPU Acceleration:**  
  Includes a substantial CUDA codebase (~15,000 lines) covering computational kernels and helper routines for GPU-accelerated simulation.
- **Modular Architecture:**  
  Organized as a collection of standalone modules, each describing distinct modeled components. The library can serve as a computational backend or be integrated with a GUI frontend.
- **Component Scope:**  
  The framework is structured around five main simulation domains:
  1. Radar system modeling and simulation
  2. Radio altimeter modeling and simulation
  3. Propagation of laser and IR radiation through turbulent atmospheric channels
  4. Optical signal processing (e.g., background noise extraction)
  5. Electro-optical sensor modeling and simulation

## Implementation Overview

- **SIMD Execution Paths:**  
  Hundreds of computational kernels implemented for both double and single precision, focusing on analytical RCS and antenna modeling.
- **CUDA Path:**  
  GPU kernels for large-scale, high-performance computations.
- **Current Status:**  
  The project contains approximately 1,000 SIMD kernels and a comprehensive set of analytical and simulation tools for radar and EO system analysis.

## Usage

This software is intended as a backend computational library for advanced simulation and modeling applications. It can be integrated into larger software environments or connected to graphical user interfaces for visualization and analysis.

## Contributing

Contributions are welcome, especially from those with expertise in:
- Radar and EO system modeling
- High-performance computing (SIMD, CUDA)
- Numerical methods and scientific computing

If you are interested, please open an issue or a pull request.

## License

This project is licensed under the terms of the MIT license.

## Acknowledgments

This project builds on a foundation of engineering and technical literature and is driven by a commitment to realistic and efficient system modeling.

  


