# REOS — Radar and Electro-Optical Simulation Framework

**REOS** is a modular source library of scientific and engineering simulation routines written in Modern Fortran (Fortran 90/2003+) and C99. It provides numerical solvers, electromagnetic scattering models, atmospheric propagation routines, and hardware-accelerated signal processing kernels for radar and electro-optical/infrared (EO/IR) system modeling.

---

## Project Philosophy & Design

REOS is structured as a **collection of decoupled, reusable source modules** rather than a single monolithic executable with a unified entry point:

* **Library-First Source Tree:** Each top-level directory acts as a standalone domain or computational building block (e.g., `Antenna/`, `MoM/`, `Radiative Transfer/`, `SIMD-kernels/`). Users compile and link only the specific modules required for their simulation target.
* **Standalone Applications (`apps/`):** Concrete, end-to-end simulation executables and complex system models are intended to reside in a dedicated `apps/` directory, consuming the core library modules as dependencies.
* **Minimal External Dependencies:** Routines rely primarily on standard Fortran 90/2003+ (`ISO_C_BINDING`), C99, OpenMP, and vendor SIMD/GPU intrinsics rather than heavy third-party frameworks.

---

## Core Capabilities

### 1. Radar & Electromagnetic Modeling
* **Radar Cross Section (RCS):** Analytical and numerical RCS calculations for canonical geometries (cylinders, planar facets) and complex meshed targets (`Gmsh`, `OBJ`, `STL`).
* **Electromagnetic Solvers:** Method of Moments (`MoM`), 3D Pseudospectral Time-Domain (`PSTD`), Volume Integral Equation (`VIE`), and Uniform Theory of Diffraction (`UTD`) implementations.
* **Clutter & Environmental Scattering:** T-matrix and Chebyshev hydrometeor scattering models, polarimetric Mueller/Jones calculus, and vegetation/canopy phase matrices.
* **Radar Systems & Waveforms:** Transceiver modeling, antenna radiation patterns, radio altimeter tracking loops, and baseband pulse synthesis/analysis.

### 2. Electro-Optical & Infrared (EO/IR) Simulation
* **Radiative Transfer & Atmosphere:** Electromagnetic wave refraction, molecular absorption attenuation, and laser/IR beam propagation through turbulent media (scintillation, thermal blooming).
* **Sensor & Detector Modeling:** Passive and active IR detector response, optical transmission profiles, and photon-to-electron conversion.
* **Optical Signal Processing:** Background noise estimation, clutter filtering, and raw digitized sample handling.

### 3. Hardware Acceleration & Vectorization
* **Explicit SIMD Kernels:** Over 1,000 hand-tuned subroutines and Fortran-to-C bindings utilizing **SSE, AVX, AVX2, and AVX-512** intrinsics for single- (`r4`) and double-precision (`r8`) workloads.
* **GPGPU Offloading:** ~15,000 lines of **CUDA** kernels and host-device memory routines, alongside **AMDGPU/HIP** support.
* **Shared-Memory Parallelism:** **OpenMP** directives for multi-threaded execution across multi-channel simulation sweeps.

---

## Repository Structure

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
| `SIMD Intrinsics Fortran bindings/` | `ISO_C_BINDING` interfaces exposing C SIMD intrinsics to Fortran. |
| `CUDA bindings/` & `GPU/` | CUDA/AMDGPU kernels and Fortran device interfaces. |
| `blas-kernels/` | Optimized Basic Linear Algebra Subprograms routines. |
| `ML-NN/ nn-fortran/` | Neural network modules integrated for Fortran workflows. |
| `Hardware/`, `Profiling/`, `Sys/` | Low-level system utilities, hardware timing, and performance profiling tools. |
| `REOS_UnitTests/` & `REOS_PerfTests/` | Verification test suites and hardware benchmarks. |

> *Note: Detailed component documentation (such as the file-level breakdown for `Radiolocation/`) is maintained inside the `Docs/` directory.*

---

## Toolchain & Usage

Because REOS is organized as a modular source collection without a single top-level build script, integrate modules directly into your application's build pipeline (`Make`, `CMake`, `fpm`, or direct compiler invocation).

### Compiler & Runtime Requirements
* **Fortran / C Compilers:** Intel oneAPI (`ifx` / `ifort` / `icx`) or GNU Compiler Collection (`gfortran >= 10.0`, `gcc`).
* **Parallel & GPU Runtimes (Optional):** OpenMP, NVIDIA CUDA Toolkit (`>= 11.8`), or AMD ROCm.
* **Target ISA Flags:** Pass the appropriate vector architecture flags for the SIMD modules you link (e.g., `-mavx2`, `-mavx512f`, or `-xHost`).

### Example: Compiling a Standalone Driver
To build a custom program or test driver, compile the required type definitions and dependencies first, followed by your entry-point file:

```bash
# Example using gfortran with OpenMP and AVX2 enabled
gfortran -O3 -fopenmp -mavx2 \
  Common/GMS_kinds.f90 \
  Radiolocation/GMS_radar_types.f90 \
  Radiolocation/GMS_radio_refractivity.f90 \
  apps/my_simulation_driver.f90 \
  -o my_simulation
```

*(Note: File paths above are illustrative; inspect module `USE` statements within each subsystem to resolve exact dependency chains.)*

---

## License

This project is distributed under the **MIT License**. See [LICENSE](LICENSE) for details, and [third_party_licences.txt](third_party_licences.txt) for third-party component attributions.

