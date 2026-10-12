# 🛰️ (REOS) Radar & Electro-Optical Simulation Framework

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Language](https://img.shields.io/badge/language-Fortran%2090%20%2F%20C99-orange.svg)](https://github.com/bgin/Radar_ElectroOptical_Simulation)
[![HPC](https://img.shields.io/badge/HPC-SIMD%20%7C%20OpenMP%20%7C%20CUDA-red.svg)](https://github.com/bgin/Radar_ElectroOptical_Simulation)

Welcome to the **Radar and Electro-Optical Simulation (REOS)** framework—a high-fidelity, design-level system modeling suite written primarily in modern Fortran. Designed for military, aerospace, and advanced engineering applications, REOS delivers realistic simulations of active/passive sensor systems, complex atmospheric interactions by executing high-performance mathematical kernels on modern hardware architectures.

This project provides a high-fidelity simulation framework for Radar systems and Electro-Optical (EO) sources, including both active and passive infrared (IR) radiation modeling. The goal is to achieve realistic, design-level system modeling and simulation, following standards and methodologies found in advanced engineering and technical literature.
---

## 🎯 Key Features & Framework Strengths

### 🌌 1. Multi-Domain Simulation Scope
The suite captures complex physical and electromagnetic scenarios split across five core technical pillars:
*   **Radar Systems:** High-fidelity modeling of transceiver electronics, antenna radiation characteristics, and analytical **Radar Cross Section (RCS)** computations.
*   **Radio Altimeters:** Precision modeling of terrain-relative altimeter mechanics, reflection characteristics, and radar tracking loops.
*   **Atmospheric Propagation:** High-fidelity simulations of laser and infrared (IR) beam propagation through highly turbulent atmospheric channels, simulating scintillation and thermal blooming.
*   **Optical Signal Processing:** Advanced algorithms for raw optical data processing, featuring specialized **background noise extraction** and clutter filtering.
*   **Electro-Optical Sensors:** Modeling of passive and active infrared (IR) detectors, optics transmission profiles, and photon-to-electron conversions.

### ⚡ 2. Bare-Metal Performance Optimization
REOS is engineered from the ground up for massive computational throughput, avoiding runtime bottlenecks through extreme hardware-level optimization:
*   **Manual SIMD Vectorization:** Over **1,000 custom-designed SIMD kernels** utilizing native Intel Intrinsics (**SSE, AVX, AVX2, and AVX-512**) for ultra-fast, double and single-precision analytical RCS calculations.
*   **Compiler Autovectorization:** Explicitly structured code paths optimized for modern compilers to auto-vectorize descriptive statistics, profiling metrics, and linear algebra routines.
*   **Multi-Threaded Concurrency:** Seamless multi-core processor exploitation utilizing **OpenMP** for macro-level parallelism across multi-channel simulation sweeps.

### 🚀 3. Massively Parallel GPGPU Backend
To scale up to massive environments, REOS houses an extensive accelerated backend:
*   **~15,000 Lines of Production CUDA:** Dedicated GPU-accelerated computing kernels and host-to-device memory layout management routines.
*   **Cross-Vendor Support:** Flexible configurations built out to interface seamlessly with both NVIDIA (CUDA) and AMD (AMDGPU) ecosystems for raw simulation scalability.

---

## 🏗️ Architecture & Component Layout

REOS features a strictly decoupled, modular architecture designed to act as a blazing-fast core computational engine or serve as a backend for standalone graphical user interfaces (GUIs).

# Radiolocation Component Directory

This document provides a technical and scientific summary of the source files within the `Radiolocation` component of the Radar and ElectroOptical Simulation framework.

| File Name | Category / Core Domain | Technical Description | Vectorization / Target ISA |
| :--- | :--- | :--- | :--- |
| `GMS_atmos_refraction.f90` | Atmospheric Physics | Models electromagnetic wave refraction through atmospheric layers. | Scalar / General |
| `GMS_atmos_refraction_xmm4r4.f90` | Atmospheric Physics | Vectorized atmospheric refraction implementation using 4-element single-precision floating-point operations. | SSE (XMM) |
| `GMS_em_fields.f90` | Electromagnetics | Primary module for calculating electromagnetic field distributions and vectors. | Scalar / General |
| `GMS_em_fields_simd.f90` | Electromagnetics | SIMD-accelerated computation of electromagnetic field interactions. | SIMD Generic |
| `GMS_em_fields_types.f90` | Electromagnetics | Data type definitions and structures for electromagnetic field states. | Infrastructure |
| `GMS_gauss_quadrature.f90` | Mathematics | Numerical integration routines using Gaussian quadrature rules for scattering integrals. | Computational |
| `GMS_gmsh_mesh.f90` / `GMS_gmsh_reader.f90` | Computational Geometry | Interactivity and ingestion routines for Gmsh finite element mesh definitions. | Infrastructure |
| `GMS_idealized_topog.f90` | Terrain Modeling | Defines analytical and synthetic surface topography for ground boundary conditions. | Analytical |
| `GMS_jones_vector.f90` | Polarimetry | Computation of Jones vectors representing fully polarized electromagnetic waves. | Mathematical |
| `GMS_leaf_phase_matrices.f90` | Radar Scattering | Computes phase matrices for bio-scatterers (leaves) to model canopy clutter. | Phenomenological |
| `GMS_mesh.f90` | Computational Geometry | Base mesh representations and geometry primitives for numerical solvers. | Infrastructure |
| `GMS_mm_vie.f90` | Numerical Solvers | Volume Integral Equation (VIE) method implementations for millimeter-wave interactions. | Computational |
| `GMS_mod_chebyshev_hydrometeors_AVX512.f90` | Radar Scattering | Chebyshev-shaped hydrometeor scattering computations using 512-bit vector registers. | AVX-512 (ZMM) |
| `GMS_mod_chebyshev_hydrometeors_common_AVX.f90` | Radar Scattering | Shared mathematical primitives for hydrometeor scattering optimization on AVX platforms. | AVX / AVX2 (YMM) |
| `GMS_mod_chebyshev_hydrometeors_common_AVX512.f90` | Radar Scattering | Shared mathematical primitives for hydrometeor scattering optimization on AVX-512 platforms. | AVX-512 (ZMM) |
| `GMS_mod_lnf.f90` | Mathematics | Special functions or localized numerical frameworks used across scattering solvers. | Computational |
| `GMS_mod_tmatrix.f90` / `_mps.f90` | Radar Scattering | T-matrix method implementations for single and multi-particle acoustic/electromagnetic scattering. | Computational |
| `GMS_mod_tree_orient.f90` | Radar Scattering | Statistical orientation distributions for vegetation and canopy elements. | Phenomenological |
| `GMS_mueller_calculus_sse.f90` / `_avx.f90` / `_avx512.f90` | Polarimetry | Vector-accelerated Mueller calculus operations for incoherent scattering matrix conversions. | SSE / AVX / AVX512 |
| `GMS_mueller_types_avx.f90` / `_avx512.f90` | Polarimetry | Memory-aligned data structures optimized for SIMD-based Mueller calculus. | Infrastructure |
| `GMS_obj_reader.f90` / `GMS_stla_reader.f90` | Computational Geometry | File parsers for Wavefront OBJ and ASCII Stereolithography (STL) geometric surface formats. | Infrastructure |
| `GMS_radar_atmos_loss.f90` | Radar Systems | Calculates multi-path and molecular absorption attenuation losses in the atmosphere. | Phenomenological |
| `GMS_radar_types.f90` | Radar Systems | Structural data definitions for transceivers, targets, and signal characteristics. | Infrastructure |
| `GMS_radio_refractivity.f90` | Atmospheric Physics | Base calculation of the radio refractive index of air based on pressure, temperature, and humidity. | Scalar / General |
| `GMS_radio_refractivity_1D_*.f90` | Atmospheric Physics | One-dimensional radio refractivity array processing mapped to XMM, YMM, and ZMM widths. | SSE / AVX2 / AVX-512 |
| `GMS_radio_refractivity_3D_*.f90` | Atmospheric Physics | Three-dimensional heterogeneous atmospheric refractivity volume processing via SIMD lanes. | SSE / AVX2 / AVX-512 |
| `GMS_rcs_common_*.f90` | Radar Cross Section | Collective core subroutines for Radar Cross Section computations optimized for specific register sizes and precisions (r4/r8). | SSE / AVX2 / AVX-512 |
| `GMS_rcs_cylinder_*.f90` | Radar Cross Section | Analytical and numerical RCS determination for cylindrical geometries across multiple ISA configurations. | SSE / AVX2 / AVX-512 |
| `GMS_rcs_planar_*.f90` | Radar Cross Section | RCS computations for finite planar facets or surfaces under vector acceleration. | SSE / AVX2 / AVX-512 |
| `GMS_rwsamples.f90` | Signal Processing | Data management routines for reading and writing raw digitized signal samples. | Infrastructure |
| `GMS_scattering_cylinder.f90` | Radar Scattering | Exact modal series solutions for scattering by infinite and finite dielectric/conducting cylinders. | Analytical |
| `GMS_solver_pstd_3d.f90` | Numerical Solvers | Three-dimensional Pseudospectral Time-Domain (PSTD) electromagnetic wave solver core. | Computational |
| `GMS_trapezoid_waveform.f90` | Signal Processing | Generation and synthesis profiles for trapezoidal radar pulses. | Computational |
| `GMS_tree_scatterer_AVX.f90` / `_AVX512.f90` | Radar Scattering | Vectorized scattering matrix calculations for complex tree and vegetation branch objects. | AVX / AVX-512 |
| `GMS_um_diffracted_waves.f90` | Wave Propagation | High-frequency asymptotic or Uniform Theory of Diffraction (UTD) wave components. | Analytical |
| `GMS_urban_model.f90` | Terrain Modeling | Defines geometry layouts, material characteristics, and boundary properties of built environments. | Phenomenological |
| `GMS_waveform_analysis_types.f90` | Signal Processing | System types defining pulse descriptors, spectrum profiles, and signal metrics. | Infrastructure |
| `l4x4phm_*.inc` / `sm2x2avg_*.inc` | Numerical Kernels | Low-level compiler include files containing unrolled matrix multiplication and phase mapping blocks. | Inline Kernels |
| `test_AM_bb_signal_trapez_normal.f90` | Verification | Validation suite for baseband amplitude-modulated trapezoidal pulse configurations. | Testing |

---

## 🛠️ Technical Stack

*   **Primary Languages:** Fortran (Modern Fortran 90/2003+ standards), C99
*   **Parallelism Paradigms:** OpenMP (CPU Multi-threading), CUDA / HIP (GPGPU Acceleration)
*   **Vector Extensions:** Intel SSE, AVX, AVX2, AVX-512 Intrinsics
*   **Target Systems:** High-Performance Computing (HPC) environments, Linux, and Windows x86_64 architectures

---

## 🚀 Getting Started

### Prerequisites
Ensure your build environment features modern toolchains with support for Fortran/C interoperability and GPU extensions:
*   **Compilers:** Intel oneAPI (ifort/icx) or GNU Compiler Collection (gfortran >= 10.0, gcc)
*   **GPU Runtimes:** NVIDIA CUDA Toolkit (>= 11.8) or AMD ROCm suite (for AMDGPU builds)
*   **Build Tools:** CMake (>= 3.20) and Make/Ninja

### Compilation
1. Clone the repository:
   ```bash
   git clone https://github.com/bgin/Radar_ElectroOptical_Simulation.git
   cd Radar_ElectroOptical_Simulation
   ```
2. Create a build directory and configure the environment:
   ```bash
   mkdir build && cd build
   cmake -DCMAKE_BUILD_TYPE=Release -DENABLE_CUDA=ON -DENABLE_AVX512=ON ..
   ```
3. Compile the framework:
   ```bash
   make -j$(nproc)
   ```

---

## 🤝 Contributing & Support

Contributions are welcome! If you want to optimize a vector kernel, add new radiative transfer models, or improve missile guidance scripts:
1. **Fork** the repository [[GitHub Source](https://github.com/bgin/Radar_ElectroOptical_Simulation)].
2. Create your feature branch (`git checkout -b feature/AmazingFeature`).
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`).
4. Push to the branch (`git push origin feature/AmazingFeature`).
5. Open a **Pull Request**.

---

## 📜 License

Distributed under the **MIT License**. See the `LICENSE` file in the root repository for more information.

  


