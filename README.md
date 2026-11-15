# REOS — Radar and Electro-Optical Simulation Framework

**REOS** is a scientific and engineering simulation framework written in Modern Fortran (Fortran 90/2003+) and C99. It provides numerical solvers, electromagnetic scattering models, atmospheric propagation routines, and signal processing kernels for radar and electro-optical/infrared (EO/IR) system design.

---

## Core Capabilities

### 1. Radar & Electromagnetic Modeling
* **Radar Cross Section (RCS):** Analytical and numerical RCS calculations for canonical geometries (cylinders, planar facets) and complex meshed targets (`Gmsh`, `OBJ`, `STL`).
* **Electromagnetic Solvers:** Method of Moments (`MoM`), 3D Pseudospectral Time-Domain (`PSTD`), Volume Integral Equation (`VIE`), and Uniform Theory of Diffraction (`UTD`) implementations.
* **Clutter & Environmental Scattering:** T-matrix and Chebyshev hydrometeor scattering models, polarimetric Mueller/Jones calculus, and vegetation/canopy phase matrices.
* **Radar Systems & Waveforms:** Transceiver modeling, antenna radiation patterns, radio altimeter tracking loops, and baseband pulse synthesis/analysis.

### 2. Electro-Optical & Infrared (EO/IR) Simulation
* **Radiative Transfer & Atmosphere:** Models for electromagnetic wave refraction, molecular absorption attenuation, and laser/IR beam propagation through turbulent media (scintillation, thermal blooming).
* **Sensor & Detector Modeling:** Passive and active IR detector response, optical transmission profiles, and photon-to-electron conversion.
* **Optical Signal Processing:** Background noise estimation, clutter filtering, and raw digitized sample handling.

### 3. Hardware Acceleration & Vectorization
* **Explicit SIMD Kernels:** Over 1,000 hand-tuned subroutines and Fortran-to-C bindings utilizing **SSE, AVX, AVX2, and AVX-512** intrinsics for single- (`r4`) and double-precision (`r8`) workloads.
* **GPGPU Offloading:** ~15,000 lines of **CUDA** kernels and host-device memory routines, alongside **AMDGPU/HIP** support.
* **Shared-Memory Parallelism:** **OpenMP** directives for multi-threaded execution across multi-channel simulation sweeps.

---

## Repository Structure

The codebase is organized as a modular library of standalone computational components:

| Directory | Description |
| :--- | :--- |
| `Radiolocation/` | Core radar systems, RCS solvers, wave refraction, polarimetry, and scattering models. |
| `Infrared EOS/` | Active and passive infrared sensor models and optical detector routines. |
| `Radiative Transfer/` | Atmospheric radiative transfer equations and propagation solvers. |
| `Atmosphere_model/` | Atmospheric refractivity, turbulence, and environmental profiles. |
| `Antenna/` | Antenna radiation patterns, gain calculations, and array modeling. |
| `MoM/` | Method of Moments electromagnetic surface/volume solvers. |
| `Mesh/` | Mesh readers (`Gmsh`, `OBJ`, `STL`) and computational geometry primitives. |
| `Mathematics/` & `ODE-Solvers/` | Special functions, Gaussian quadrature, linear algebra, and differential equation solvers. |
| `FFT/` & `dsp/` | Fast Fourier Transforms, digital filtering, and waveform signal processing. |
| `Statistics/` & `estimation_lib/` | Statistical distributions, stochastic modeling, and state estimation filters. |
| `SIMD-kernels/` & `LibSIMD/` | Vectorized mathematical and physical kernels (SSE, AVX, AVX2, AVX-512). |
| `SIMD Intrinsics Fortran bindings/` | ISO_C_BINDING interfaces exposing C SIMD intrinsics to Fortran. |
| `CUDA bindings/` & `GPU/` | CUDA/AMDGPU kernels and Fortran device interfaces. |
| `blas-kernels/` | Optimized Basic Linear Algebra Subprograms routines. |
| `ML-NN/ nn-fortran/` | Neural network modules integrated for Fortran workflows. |
| `Hardware/`, `Profiling/`, `Sys/` | Low-level system utilities, hardware timing, and performance profiling tools. |
| `REOS_UnitTests/` & `REOS_PerfTests/` | Verification test suites and hardware benchmarks. |

> *Note: Detailed file-by-file documentation for specific subsystems (such as the Radiolocation component) can be found inside the `Docs/` directory.*

---

## Requirements & Compilation

REOS is designed for x86_64 Linux and Windows HPC environments. Because components are structured as modular libraries and kernels, verify that your toolchain supports Fortran/C interoperability (`ISO_C_BINDING`) and your target instruction set.

### Supported Toolchains
* **Compilers:** Intel oneAPI (`ifx` / `ifort` / `icx`) or GNU Compiler Collection (`gfortran >= 10.0`, `gcc`).
* **Parallel Runtimes:** OpenMP, NVIDIA CUDA Toolkit (`>= 11.8`), or AMD ROCm.
* **Architecture Flags:** Enable target vector extensions when compiling SIMD modules (e.g., `-mavx2`, `-mavx512f`, or `-xHost`).

*(Insert your actual build script, Makefile command, or compiler invocation example here—for example, how to compile a standalone test from `REOS_UnitTests/`.)*

---

## License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) for details, and [third_party_licences.txt](third_party_licences.txt) for third-party component attributions.

