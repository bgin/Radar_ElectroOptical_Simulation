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

  


