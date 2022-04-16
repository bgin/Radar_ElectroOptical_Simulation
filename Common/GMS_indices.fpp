
#ifndef __GMS_INDICES_FPP__
#define __GMS_INDICES_FPP__

/*
	Flat multidiensional array indexing macros
*/

// To be used with CUDA
#ifndef INDEX2
#define INDEX2(isize,i,j) i + isize*j
#endif
#ifndef INDEX3
#define INDEX3(isize,jsize,i,j,k) i + isize*(j + jsize*k)
#endif
#ifndef INDEX4
#define INDEX4(isize,jsize,ksize,i,j,k,x) i + isize*(j + jsize*(k + ksize*x))
#endif
#ifndef INDEX5
#define INDEX5(isize,jsize,ksize,xsize,i,j,k,x,y) i + isize*(j + jsize*(k + ksize*(x + xsize*y)))
#endif

/*#define I2D(i,j,dim0)                              i+j*dim0
#define I3D(i,j,dim0,k,dim1)                       i+j*dim0+k*dim0*dim1
#define I4D(i,j,dim0,k,dim1,l,dim2)                i+j*dim0+k*dim0*dim1+l*dim0*dim1*dim2
#define I5D(i,j,dim0,k,dim1,l,dim2,m,dim3)         i+j*dim0+k*dim0*dim1+l*dim0*dim1*dim2+m*dim0*dim1*dim2*dim3
#define I6D(i,j,dim0,k,dim1,l,dim2,m,dim3,n,dim4)  i+j*dim0+k*dim0*dim1+l*dim0*dim1*dim2+m*dim0*dim1*dim2*dim3+n*dim0*dim1*dim2*dim3*dim4
*/

//More optimized version
#define I2D(i,j,dim0)                                        i+j*dim0
#define I3D(i,j,dim0,k,dim01)                                i+j*dim0+k*dim01
#define I4D(i,j,dim0,k,dim01,l,dim012)                       i+j*dim0+k*dim01+l*dim012
#define I5D(i,j,dim0,k,dim01,l,dim012,m,dim0123)             i+j*dim0+k*dim01+l*dim012+m*dim0123
#define I6D(i,j,dim0,k,dim01,l,dim012,m,dim0123,n,dim01234)  i+j*dim0+k*dim01+l*dim012+m*dim0123+n*dim01234



#define Ix2D(i,ny,j) ((i) * (ny) + (j))

#define Ix3D(i,ny,j,nz,k) ((i) * (ny) + ((j) * (nz) + (k)))


// WRF indexing scheme
// m_jme and m_kme must be present in the calling scope.
//#define Dim3(j,k,i) ((j) + (m_jme) * ((k) + (m_kme) * (i)))



#endif /*__GMS_INDICES_FPP__*/
