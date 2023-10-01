
!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module fft_blocks_2xr4



 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:   fft_blocks_2xr4
 !                         
 !          
 !          Purpose:
 !                       FFT various building blocks
 !                       optimized mainly for embedded systems
 !                        
 !          History:
 !                        Date: 01-10-2023
 !                        Time: 15:03 GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Adapted by Bernard Gingold from the book
 !                      "Handbook of Real-Time Fast Fourier Transforms"
 !                 
 !          References:
 !         
 !                  Winthrop W. Smith, Joanne M. Smith - "Handbook of Real-Time Fast Fourier Transforms 
 !                  Algorithms to Product Testing (1995, Wiley-IEEE Press)"
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,    only : i4,sp
    
    
    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: FFT_BLOCKS_2XR4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: FFT_BLOCKS_2XR4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: FFT_BLOCKS_2XR4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: FFT_BLOCKS_2XR4_FULLVER =   &
            1000*FFT_BLOCKS_2XR4_MAJOR+100*FFT_BLOCKS_2XR4_MINOR+10*FFT_BLOCKS_2XR4_MICRO
     ! Module creation date
     character(*),        parameter :: FFT_BLOCKS_2XR4_CREATE_DATE = "01-10-2022 03:11PM +00200 (SUN 01 OCT 2023 GMT+2)"
     ! Module build date
     character(*),        parameter :: FFT_BLOCKS_2XR4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: FFT_BLOCKS_2XR4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: FFT_BLOCKS_2XR4_SYNOPSIS    = "Optimized (for embedded CPUs) FFT building blocks (scalar)." 
     
#define C150 -1.5_sp
#define C08660256403784 0.866025403784_sp

     contains
     
     
     subroutine dft_2x_2xr4(xr,xi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: dft_2x_2xr4
        !dir$ attributes forceinline :: dft_2x_2xr4
        real(kind=sp), dimension(0:1), intent(inout) :: xr
        real(kind=sp), dimension(0:1), intent(inout) :: xi
        real(kind=sp), automatic :: a0r,a0i
        real(kind=sp), automatic :: a1r,a1i
        real(kind=sp), automatic :: tr,ti
        a0r   = xr(0)
        a0i   = xi(0)
        a1r   = xr(1)
        a1i   = xi(1)
        tr    = a0r-a1r
        ti    = a0i-a1i
        xr(1) = tr
        xi(1) = ti
        xr(0) = a0r+a1r
        xi(0) = a0i+a1i 
     end subroutine dft_2x_2xr4
     
     subroutine dft_3x_2xr4(xr,xi)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: dft_3x_2xr4
        !dir$ attributes forceinline :: dft_3x_2xr4
        real(kind=sp), dimension(0:2), intent(inout) :: xr
        real(kind=sp), dimension(0:2), intent(inout) :: xi
        real(kind=sp), automatic :: a0r,a0i,b0r,b0i,d0r,d0i
        real(kind=sp), automatic :: a1r,a1i,b1r,b1i,c1r,c1i
        real(kind=sp), automatic :: a2r,a2i,b2r,b2i,c2r,c2i
        a0r   = xr(0)
        a0i   = xi(0)
        a1r   = xr(1)
        a1i   = xi(1)
        a2r   = xr(2)
        a2i   = xi(2)
        b1r   = a1r+a2r
        b1i   = a1i+a2i
        b2r   = a1r-a2r
        b2i   = a1i-a2i
        b0r   = a0r+b1r
        b0i   = a0i+b1i
        c1r   = C150*b1r
        c1i   = C150*b1i
        c2r   = C08660256403784*b2i
        c2i   = C08660256403784*b2r
        d0r   = b0r+c1r
        d0i   = b0i+c1i
        xr(2) = d0r-c2r
        xi(2) = d0i+c2i
        xr(1) = d0r+c2r
        xi(1) = d0i-c2i
        xr(0) = b0r
        xi(0) = b0i
     end subroutine dft_3x_2xr4





















end module fft_blocks_2xr4
