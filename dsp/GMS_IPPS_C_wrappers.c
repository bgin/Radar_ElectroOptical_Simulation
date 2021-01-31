
#include <ipps.h>
#include <ipp.h>
#include <ippcore.h>
#include <immintrin.h>
#include "GMS_IPPS_C_wrappers.h"




/*
        Vector Initialization Functions
*/



IppStatus gms_ippsCopy_32f(const Ipp32f * pSrc, Ipp32f * pDst, int32_t len) {

     return (ippsCopy_32f(pSrc,pDst,len));
}


IppStatus gms_ippsCopy_64f(const Ipp64f * pSrc, Ipp64f * pDst, int32_t len) {

      return (ippsCopy_64f(pSrc,pDst,len));
}


IppStatus gms_ippsCopy_32fc(const Ipp32fc * pSrc, Ipp32fc * pDst, int32_t len) {

      return (ippsCopy_32fc(pSrc,pDst,len));
}


IppStatus gms_ippsCopy_64fc(const Ipp64fc * pSrc, Ipp64fc * pDst, int32_t len) {

      return (ippsCopy_64fc(pSrc,pDst,len));
}


IppStatus gms_ippsCopy_8u(const Ipp8u * pSrc, Ipp8u * pDst, int32_t len) {

      return (ippsCopy_8u(pSrc,pDst,len));
}

/*
    Parameters
pSrc Pointer to the source vector.
pDst Pointer to the destination vector.
len  Number of elements to copy.
srcBitOffset Offset, in bits, from the first byte of the source vector.
dstBitOffset Offset, in bits, from the first byte of the destination vector.
*/


IppStatus gms_ippsCopyLE_1u(const Ipp8u * pSrc, int32_t srcBitOffset,
                            Ipp8u * pDst, int32_t dstBitOffset, int32_t len) {

      return (ippsCopyLE_1u(pSrc,srcBitOffset,pDst,dstBitOffset,len));
}



IppStatus gms_ippsCopyBE_1u(const Ipp8u * pSrc, int32_t srcBitOffset,
                            Ipp8u * pDst, int32_t dstBitOffset, int32_t len) {

      return (ippsCopyBE_1u(pSrc,srcBitOffset,pDst,dstBitOffset,len));
}


/*
    Parameters
pSrc Pointer to the source vector used to initialize pDst .
pDst Pointer to the destination vector to be initialized.
len Number of elements to move.
    Description
This function moves the first len elements from a source vector pSrc into the destination vector pDst . If
some parts of the source and destination vectors are overlapping, then the function ensures that the original
source bytes in the overlapping parts are moved (it means that they are copied before being overwritten) to
the appropriate parts of the destination vector.
    Return Values
ippStsNoErr Indicates no error.
ippStsNullPtrErr Indicates an error when the pSrc or pDst pointer is NULL .
ippStsSizeErr Indicates an error when len is less than or equal to zero.


     Example
The example below shows how to use the function ippsMove .
Ipp8u pSrc[10] = { "123456789" };
Ipp8u pDst[6];
int len = 6;
IppStatus status;
status = ippsMove_8u ( pSrc, pDst, len );
if(ippStsNoErr != status)
printf("Intel(R) IPP Error: %s",ippGetStatusString(status));
Result:
pSrc = 123456789
pDst = 123456
*/


IppStatus gms_ippsMove_8u(const Ipp8u * pSrc, Ipp8u * pDst, int32_t len) {

      return (ippsMove_8u(pSrc,pDst,len));
}



IppStatus gms_ippsMove_32f(const Ipp32f * pSrc, Ipp32f * pDst, int32_t len) {

       return (ippsMove_32f(pSrc,pDst,len));
}



IppStatus gms_ippsMove_64f(const Ipp64f * pSrc, Ipp64f * pDst, int32_t len) {

       return (ippsMove_64f(pSrc,pDst,len));
}



IppStatus gms_ippsMove_32fc(const Ipp32fc * pSrc, Ipp32fc * pDst, int32_t len) {

       return (ippsMove_32fc(pSrc,pDst,len));
}



IppStatus gms_ippsMove_64fc(const Ipp64fc * pSrc, Ipp64fc * pDst, int32_t len) {

       return (ippsMove_64fc(pSrc,pDst,len));
}

/*
   Set functions
   
   Initializes vector elements to a specified common
   value.
*/

/*
     Parameters
pDst Pointer to the vector to be initialized.
len Number of elements to initialize.
val Value used to initialize the vector pDst .
Description
This function initializes the first len elements of the real or complex vector pDst to contain the same value
val .
Return Values
ippStsNoErr Indicates no error.
ippStsNullPtrErr Indicates an error when the pDst pointer is NULL .
ippStsSizeErr Indicates an error when len is less than or equal to zero.
*/


IppStatus gms_ippSet_8u(Ipp8u val, Ipp8u * pDst, int32_t len) {

       return (ippsSet_8u(val,pDst,len));
}



IppStatus gms_ippSet_32f(Ipp32f val, Ipp32f * pDst, int32_t len) {

       return (ippsSet_32f(val,pDst,len));
}



IppStatus gms_ippSet_64f(Ipp64f val, Ipp64f * pDst, int32_t len) {

       return (ippsSet_64f(val,pDst,len));
}



IppStatus gms_ippSet_32fc(Ipp32fc val, Ipp32fc * pDst, int32_t len) {

       return (ippsSet_32fc(val,pDst,len));
}



IppStatus gms_ippSet_64fc(Ipp64fc val, Ipp64fc * pDst, int32_t len) {

       return (ippsSet_64fc(val,pDst,len));
}


/*
    Zero
    Initializes a vector to zero.
    
*/

/*
    Parameters
pDst Pointer to the vector to be initialized to zero.
len Number of elements to initialize.
Description
This function initializes the first len elements of the vector pDst to zero. If pDst is a complex vector, both
real and imaginary parts are zeroed.
Return Values
ippStsNoErr Indicates no error.
ippStsNullPtrErr Indicates an error when the pDst pointer is NULL .
ippStsSizeErr Indicates an error when len is less than or equal to zero.
*/


IppStatus gms_ippsZero_8u(Ipp8u * pDst, int32_t len) {

        return (ippsZero_8u(pDst,len));
}



IppStatus gms_ippsZero_32f(Ipp32f * pDst, int32_t len) {

        return (ippsZero_32f(pDst,len));
}



IppStatus gms_ippsZero_64f(Ipp64f * pDst, int32_t len) {

        return (ippsZero_64f(pDst,len));
}


IppStatus gms_ippsZero_32fc(Ipp32fc * pDst, int32_t len) {

        return (ippsZero_32fc(pDst,len));
}


IppStatus gms_ippsZero_64fc(Ipp64fc * pDst, int32_t len) {

        return (ippsZero_64fc(pDst,len));
}

/*
    Sample-Generating Functions
*/

/*

    Parameters
pDst Magnitude of the tone, that is, the maximum value attained by
the wave.
Pointer to the phase of the tone relative to a cosine wave. It
must be in range [0.0, 2π). You can use the returned value to
compute the next continuous data block.
Frequency of the tone relative to the sampling frequency. It
must be in the interval [0.0, 0.5) for real tone and in [0.0, 1.0)
for complex tone.
Pointer to the array that stores the samples.
len Number of samples to be computed.


hint
Suggests using specific code. The possible values for the hint
argument are described in Hint Arguments.

     Description
This function generates the tone with the specified frequency rFreq , phase pPhase , and magnitude magn .
The function computes len samples of the tone, and stores them in the array pDst . For real tones, each
generated value x[n] is defined as:
x[n] = magn * cos(2πn*rFreq + phase)
For complex tones, x[n] is defined as:
x[n] = magn * (cos(2πn*rFreq + phase)+j* sin(2πn*rFreq + phase))
The parameter hint suggests using specific code, which provides for either fast but less accurate calculation,
or more accurate but slower execution.
Return Values
ippStsNoErr Indicates no error.
ippStsNullPtrErr Indicates an error when the pDst or pPhase pointer is NULL .
ippStsSizeErr Indicates an error when len is less than, or equal to zero.
ippStsToneMagnErr Indicates an error when magn is less than, or equal to zero.
ippStsToneFreqErr
ippStsTonePhaseErr
Indicates an error when rFreq is negative, or greater than, or
equal to 0.5 for real tone and to 1.0 for complex tone.
Indicates an error when the pPhase value is negative, or
greater than or equal to IPP_2PI .
*/


IppStatus gms_ippsTone_32f(Ipp32f * pDst, int32_t len, Ipp32f mag, Ipp32f freq,
                           Ipp32f * pPhase, IppHintAlgorithm hint) {

	 return (ippsTone_32f(pDst,len,mag,freq,pPhase,hint));
}



IppStatus gms_ippsTone_64f(Ipp64f * pDst, int32_t len, Ipp64f mag, Ipp64f freq,
                           Ipp64f * pPhase, IppHintAlgorithm hint) {

	 return (ippsTone_64f(pDst,len,mag,freq,pPhase,hint));
}


IppStatus gms_ippsTone_32fc(Ipp32fc * pDst, int32_t len, Ipp32f mag, Ipp32f freq,
                           Ipp32f * pPhase, IppHintAlgorithm hint) {

	 return (ippsTone_32fc(pDst,len,mag,freq,pPhase,hint));
}


IppStatus gms_ippsTone_64fc(Ipp64fc * pDst, int32_t len, Ipp64f mag, Ipp64f freq,
                           Ipp64f * pPhase, IppHintAlgorithm hint) {

	 return (ippsTone_64fc(pDst,len,mag,freq,pPhase,hint));
}


/*
    Triangle
    Generates a triangle with a given frequency, phase,
    and magnitude.
*/


IppStatus gms_ippsTriangle_32f(Ipp32f * pDst, int32_t len, Ipp32f mag, Ipp32f freq,
                               Ipp32f asym, Ipp32f * pPhase) {

	 return (ippsTriangle_32f(pDst,len,mag,freq,asym,pPhase));
}


IppStatus gms_ippsTriangle_64f(Ipp64f * pDst, int32_t len, Ipp64f mag, Ipp64f freq,
                               Ipp64f asym, Ipp64f * pPhase) {

	 return (ippsTriangle_64f(pDst,len,mag,freq,asym,pPhase));
}


IppStatus gms_ippsTriangle_32fc(Ipp32fc * pDst, int32_t len, Ipp32f mag, Ipp32f freq,
                               Ipp32f asym, Ipp32f * pPhase) {

	 return (ippsTriangle_32fc(pDst,len,mag,freq,asym,pPhase));
}


IppStatus gms_ippsTriangle_64fc(Ipp64fc * pDst, int32_t len, Ipp64f mag, Ipp64f freq,
                               Ipp64f asym, Ipp64f * pPhase) {

	 return (ippsTriangle_64fc(pDst,len,mag,freq,asym,pPhase));
}


/*
   RandUniformInit
   Initializes a noise generator with uniform distribution.
*/


IppStatus gms_ippsRandUniformInit_32f(IppsRandUniState_32f * pRandUniState, Ipp32f low,
                                  Ipp32f high, uint32_t seed) {

	  return (ippsRandUniformInit_32f(&pRandUniState,low,high,seed));
}



IppStatus gms_ippsRandUniformInit_64f(IppsRandUniState_64f * pRandUniState, Ipp64f low,
                                  Ipp64f high, uint32_t seed) {

	  return (ippsRandUniformInit_64f(&pRandUniState,low,high,seed));
}


/*
    RandUniformGetSize
    Computes the length of the uniform distribution
    generator structure
*/


IppStatus gms_ippsRandUniformGetSize_32f(int * pRandUniformStateSize) {

          return (ippsRandUniformGetSize_32f(&pRandUniformStateSize));
}



IppStatus gms_ippsRandUniformGetSize_64f(int * pRandUniformStateSize) {

          return (ippsRandUniformGetSize_64f(&pRandUniformStateSize));
}



IppStatus gms_ippsRandUniform_32f(Ipp32f * pDst, int32_t len, IppsRandUniState_32f *
				  pRandUniState) {

	  return (ippsRandUniform_32f(pDst,len,pRandUniState));
}



IppStatus gms_ippsRandUniform_64f(Ipp64f * pDst, int32_t len, IppsRandUniState_64f *
				  pRandUniState) {

	  return (ippsRandUniform_64f(pDst,len,pRandUniState));
}


bool      vecf32_fill_ippsRandUniform_32f(Ipp32f * __restrict pDst, int32_t len,
                                          Ipp32f low, Ipp32f high) {

      IppsRandUniState_32f * __restrict pRandStateObj = NULL;
      IppStatus stat;
      int32_t sizeRndObj;
      uint32_t seed; 
      stat = ippsRandUniformGetSize_32f(&sizeRndObj);
      if(stat != ippStsNoErr) { goto Failed;}
      pRandStateObj = (IppsRandUniState_32f*)ippsMalloc_32f(sizeRndObj);
      if(NULL==pRandStateObj && sizeRndObj != 0) {goto Failed;}
      _rdseed32_step(&seed);
      if(0==seed) { seed = (uint32_t)__rdtsc();}
      stat = ippsRandUniformInit_32f(pRandStateObj,low,high,seed);
      if(stat != ippStsNoErr) { goto Failed;}
      stat = ippsRandUniform_32f(pDst,len,pRandStateObj);
      if(stat != ippStsNoErr) { goto Failed;}
      ippsFree(pRandStateObj);
      return (true); // Success
Failed:
     {
        if(NULL!=pRandStateObj) {ippsFree(pRandStateObj);}
        return (false);
   }
}


bool      vecf64_fill_ippsRandUniform_64f(Ipp64f * __restrict pDst, int32_t len,
                                          Ipp64f low, Ipp64f high) {

      IppsRandUniState_64f * __restrict pRandStateObj = NULL;
      IppStatus stat;
      int32_t sizeRndObj;
      uint32_t seed;
      stat = ippsRandUniformGetSize_64f(&sizeRndObj);
      if(stat != ippStsNoErr) { goto Failed;}
      pRandStateObj = (IppsRandUniState_64f*)ippsMalloc_64f(sizeRndObj);
      if(NULL==pRandStateObj && sizeRndObj != 0) { goto Failed;}
      _rdseed32_step(&seed);
      if(0==seed) {seed = (uint32_t)__rdtsc();}
      stat = ippsRandUniformInit_64f(pRandStateObj,low,high,seed);
      if(stat != ippStsNoErr) { goto Failed;}
      stat = ippsRandUniform_64f(pDst,len,pRandStateObj);
      if(stat != ippStsNoErr) { goto Failed;}
      ippsFree(pRandStatObj);
      return (true);
Failed:
     {
        if(NULL!=pRandStateObj) {ippsFree(pRandStateObj);}
        return (false);
   }      
}

/*
        RandGaussInit
Initializes a noise generator with Gaussian
distribution.
*/


IppStatus gms_ippsRandGaussInit_32f(IppsRandGaussState_32f * pRandGaussState, Ipp32f mean,
                                    Ipp32f stdDev, uint32_t seed) {

	return (ippsRandGaussinit_32f(pRandGaussState,mean,stdDev,seed));
}



IppStatus gms_ippsRandGaussInit_64f(IppsRandGaussState_64f * pRandGaussState, Ipp64f mean,
                                    Ipp64f stdDev, uint32_t seed) {

	return (ippsRandGaussinit_64f(pRandGaussState,mean,stdDev,seed));
}

/*
    RandGaussGetSize
Computes the length of the Gaussian distribution
generator structure.
*/


IppStatus gms_ippsRandGaussGetSize_32f(int32_t * pRandGaussStateSize) {

        return (ippsRandGaussGetSize_32f(&pRandGaussStateSize));
}



IppStatus gms_ippsRandGaussGetSize_64f(int32_t * pRandGaussStateSize) {

        return (ippsRandGaussGetSize_64f(&pRandGaussStateSize));
}

/*
   RandGauss
Generates the pseudo-random samples with a
Gaussian distribution.
*/


IppStatus gms_ippsRandGauss_32f(Ipp32f * pDst, int32_t len,
                                IppsRandGaussState_32f * pRandGaussState) {

	  return (ippsRandGauss_32f(pDst,len,pRandGaussState));
}


IppStatus gms_ippsRandGauss_64f(Ipp64f * pDst, int32_t len,
                                IppsRandGaussState_64f * pRandGaussState) {

	  return (ippsRandGauss_64f(pDst,len,pRandGaussState));
}



bool gms_vecf32_fill_ippsRandGauss_f32(Ipp32f * pDst, int32_len,
                                       Ipp32f mean,   Ipp32f stdDev) {

      IppsRandGaussState_32f * __restrict pRandGaussState = NULL;
      IppStatus stat;
      int32_t sizeRndObj;
      uint32_t seed;
      stat = ippsRandGaussGetSize_32f(&sizeRndObj);
      if(stat != ippStsNoErr) { goto Failed;}
      pRandGaussState = (IppsRandGaussState_32f*)ippMalloc_32f(sizeRndObj);
      if(NULL==pRandGaussState && 0 != sizeRndObj) { goto Failed;}
      _rdseed32_step(&seed);
      if(0==seed) { seed = (uint32_t)__rdtsc();}
      stat = ippsRandGaussInit_32f(pRandGaussState,mean,stdDev,seed);
      if(stat != ippStsNoErr) { goto Failed;}
      stat = ippsRandGauss_32f(pDst,len,pRandGaussState);
      if(stat != ippStsNoErr) { goto Failed;}
      ippsFree(pRandGaussState);
      return (true);
Failed:
         {
           if(NULL!=pRandGaussState) { ippsFree(pRandGaussState); }
	   return (false);
   }
}



bool gms_vecf64_fill_ippsRandGauss_f64(Ipp64f * pDst, int32_len,
                                       Ipp64f mean,   Ipp64f stdDev) {

      IppsRandGaussState_64f * __restrict pRandGaussState = NULL;
      IppStatus stat;
      int32_t sizeRndObj;
      uint32_t seed;
      stat = ippsRandGaussGetSize_64f(&sizeRndObj);
      if(stat != ippStsNoErr) { goto Failed;}
      pRandGaussState = (IppsRandGaussState_64f*)ippMalloc_64f(sizeRndObj);
      if(NULL==pRandGaussState && 0 != sizeRndObj) { goto Failed;}
      _rdseed32_step(&seed);
      if(0==seed) { seed = (uint32_t)__rdtsc();}
      stat = ippsRandGaussInit_64f(pRandGaussState,mean,stdDev,seed);
      if(stat != ippStsNoErr) { goto Failed;}
      stat = ippsRandGauss_64f(pDst,len,pRandGaussState);
      if(stat != ippStsNoErr) { goto Failed;}
      ippsFree(pRandGaussState);
      return (true);
Failed:
         {
           if(NULL!=pRandGaussState) { ippsFree(pRandGaussState); }
	   return (false);
   }
}

/*
    VectorJaehne
    Creates a Jaehne vector.
*/


IppStatus gms_ippsVectorJaehne_32f(Ipp32f * pDst, int32_t len, Ipp32f magn) {

         return (ippsVectorJaehne_32f(pDst,len,magn));
}



IppStatus gms_ippsVectorJaehne_64f(Ipp64f * pDst, int32_t len, Ipp64f magn) {

         return (ippsVectorJaehne_64f(pDst,len,magn));
}

/*
    VectorSlope
Creates a slope vector.
*/


IppStatus gms_ippsVectorSlope_32f(Ippf32 * pDst, int32_t len, Ipp32f offset, Ipp32f slope) {

         return (ippsVectorSlope_32f(pDst,len,offset,slope));
}



IppStatus gms_ippsVectorSlope_64f(Ippf64 * pDst, int32_t len, Ipp64f offset, Ipp64f slope) {

         return (ippsVectorSlope_64f(pDst,len,offset,slope));
}


/*
    AddC
    Adds a constant value to each element of a vector.
*/


IppStatus gms_ippsAddC_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {

         return (ippsAddC_32f(pSrc,val,pDst,len));
}



IppStatus gms_ippsAddC_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {

         return (ippsAddC_64f(pSrc,val,pDst,len));
}



IppStatus gms_ippsAddC_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {

         return (ippsAddC_32fc(pSrc,val,pDst,len));
}


IppStatus gms_ippsAddC_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {

         return (ippsAddC_64fc(pSrc,val,pDst,len));
}



IppStatus gms_ippsAddC_32f_I(Ipp32f val, Ipp32f * pSrcDst, int32_t len) {

         return (ippsAddC_32f_I(val,pSrcDst,len));
}




IppStatus gms_ippsAddC_64f_I(Ipp64f val, Ipp64f * pSrcDst, int32_t len) {

         return (ippsAddC_64f_I(val,pSrcDst,len));
}



IppStatus gms_ippsAddC_32fc_I(Ipp32fc val, Ipp32fc * pSrcDst, int32_t len) {

         return (ippsAddC_32fc_I(val,pSrcDst,len));
}



IppStatus gms_ippsAddC_64fc_I(Ipp64fc val, Ipp64fc * pSrcDst, int32_t len) {

         return (ippsAddC_64fc_I(val,pSrcDst,len));
}


IppStatus gms_ippsAdd_32f(const Ipp32f * pSrc1, const Ipp32f * pSrc2, Ipp32f * pDst, int32_t len) {

         return (ippsAdd_32f(pSrc1,pSrc2,pDst,len));
}



IppStatus gms_ippsAdd_64f(const Ipp64f * pSrc1, const Ipp64f * pSrc2, Ipp64f * pDst, int32_t len) {

         return (ippsAdd_64f(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsAdd_32fc(const Ipp32fc * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pDst, int32_t len) {

         return (ippsAdd_32fc(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsAdd_64fc(const Ipp64fc * pSrc1, const Ipp64fc * pSrc2, Ipp64fc * pDst, int32_t len) {

         return (ippsAdd_64fc(pSrc1,pSrc2,pDst,len));
}



IppStatus gms_ippsAdd_32f_I(const Ipp32f * pSrc, Ipp32f * pSrcDst, int32_t len) {

          return (ippsAdd_32f_I(pSrc,pSrcDst,len));
}



IppStatus gms_ippsAdd_64f_I(const Ipp64f * pSrc, Ipp64f * pSrcDst, int32_t len) {

          return (ippsAdd_64f_I(pSrc,pSrcDst,len));
}


IppStatus gms_ippsAdd_32fc_I(const Ipp32fc * pSrc, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsAdd_32fc_I(pSrc,pSrcDst,len));
}


IppStatus gms_ippsAdd_64fc_I(const Ipp64fc * pSrc, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsAdd_64fc_I(pSrc,pSrcDst,len));
}

/*
        AddProductC
Adds product of a vector and a constant to the
accumulator vector
*/

	      

IppStatus gms_ippsAddProdC_32f(const Ipp32f * pSrc, const Ipp32f val, Ipp32f * pSrcDst, int32_t len) {

          return (ippsAddProdC_32f(pSrc,val,pSrcDst,len));
}



IppStatus gms_ippsAddProdC_64f(const Ipp64f * pSrc, const Ipp64f val, Ipp64f * pSrcDst, int32_t len) {

          return (ippsAddProdC_64f(pSrc,val,pSrcDst,len));
}



IppStatus gms_ippsAddProduct_32f(const Ipp32f * pSrc1, const Ipp32f * pSrc2, Ipp32f * pSrcDst, int32_t len) {

          return (ippsAddProduct_32f(pSrc1,pSrc2,pSrcDst,len));
}



IppStatus gms_ippsAddProduct_64f(const Ipp64f * pSrc1, const Ipp64f * pSrc2, Ipp64f * pSrcDst, int32_t len) {

          return (ippsAddProduct_64f(pSrc1,pSrc2,pSrcDst,len));
}


IppStatus gms_ippsAddProduct_32fc(const Ipp32fc * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsAddProduct_32fc(pSrc1,pSrc2,pSrcDst,len));
}


IppStatus gms_ippsAddProduct_64fc(const Ipp64fc * pSrc1, const Ipp64fc * pSrc2, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsAddProduct_64fc(pSrc1,pSrc2,pSrcDst,len));
}

/*
     MulC
Multiplies each element of a vector by a constant
value.
*/


IppStatus gms_ippsMulC_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {

          return (ippsMulC_32f(pSrc,val,pDst,len));
}



IppStatus gms_ippsMulC_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {

          return (ippsMulC_64f(pSrc,val,pDst,len));
}


IppStatus gms_ippsMulC_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {

          return (ippsMulC_32fc(pSrc,val,pDst,len));
}


IppStatus gms_ippsMulC_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {

          return (ippsMulC_64fc(pSrc,val,pDst,len));
}

/*
      In-place operations without scaling.
*/



IppStatus gms_ippsMulC_32f_I(Ipp32f val, Ipp32f * pSrcDst, int32_t len) {
 
          return (ippsMulC_32f_I(val,pSrcDst,len));
}



IppStatus gms_ippsMulC_64f_I(Ipp64f val, Ipp64f * pSrcDst, int32_t len) {
 
          return (ippsMulC_64f_I(val,pSrcDst,len));
}



IppStatus gms_ippsMulC_32fc_I(Ipp32fc val, Ipp32fc * pSrcDst, int32_t len) {
 
          return (ippsMulC_32fc_I(val,pSrcDst,len));
}


IppStatus gms_ippsMulC_64fc_I(Ipp64fc val, Ipp64fc * pSrcDst, int32_t len) {
 
          return (ippsMulC_64fc_I(val,pSrcDst,len));
}


/*
       Mul
Multiplies the elements of two vectors.
*/


IppStatus gms_ippsMul_32f(const Ipp32f * pSrc1, const Ipp32f * pSrc2, Ipp32f * pDst, int32_t len) {

          return (ippsMul_32f(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsMul_64f(const Ipp64f * pSrc1, const Ipp64f * pSrc2, Ipp64f * pDst, int32_t len) {

          return (ippsMul_64f(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsMul_32fc(const Ipp32fc * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pDst, int32_t len) {

          return (ippsMul_32fc(pSrc1,pSrc2,pDst,len));
}



IppStatus gms_ippsMul_64fc(const Ipp64fc * pSrc1, const Ipp64fc * pSrc2, Ipp64fc * pDst, int32_t len) {

          return (ippsMul_64fc(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsMul_32f32fc(const Ipp32f * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pDst, int32_t len) {

          return (ippsMul_32f32fc(pSrc1,pSrc2,pDst,len));
}



/*
    In-place operations on floating point and integer data without scaling
*/


IppStatus gms_ippsMul_32f_I(const Ipp32f * pSrc, Ipp32f * pSrcDst, int32_t len) {
 
          return (ippsMul_32f_I(pSrc,pSrcDst,len));
}



IppStatus gms_ippsMul_64f_I(const Ipp64f * pSrc, Ipp64f * pSrcDst, int32_t len) {
 
          return (ippsMul_64f_I(pSrc,pSrcDst,len));
}


IppStatus gms_ippsMul_32fc_I(const Ipp32fc * pSrc, Ipp32fc * pSrcDst, int32_t len) {
 
          return (ippsMul_32fc_I(pSrc,pSrcDst,len));
}



IppStatus gms_ippsMul_64fc_I(const Ipp64fc * pSrc, Ipp64fc * pSrcDst, int32_t len) {
 
          return (ippsMul_64fc_I(pSrc,pSrcDst,len));
}


/*
   SubC
Subtracts a constant value from each element of a
vector.
*/

/*
     Not-in-place operations on floating point data.
*/



IppStatus gms_ippsSubC_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {
  
          return (ippsSubC_32f(pSrc,val,pDst,len));
}



IppStatus gms_ippsSubC_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {
  
          return (ippsSubC_64f(pSrc,val,pDst,len));
}


IppStatus gms_ippsSubC_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {
  
          return (ippsSubC_32fc(pSrc,val,pDst,len));
}


IppStatus gms_ippsSubC_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {
  
          return (ippsSubC_64fc(pSrc,val,pDst,len));
}

/*
     In-place operations on floating point data.
*/


IppStatus gms_ippsSubC_32f_I(Ipp32f val, Ipp32f * pSrcDst, int32_t len) {

          return (ippsSubC_32f(val,pSrcDst,len));
}


IppStatus gms_ippsSubC_64f_I(Ipp64f val, Ipp64f * pSrcDst, int32_t len) {

          return (ippsSubC_64f(val,pSrcDst,len));
}


IppStatus gms_ippsSubC_32fc_I(Ipp32fc val, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsSubC_32fc(val,pSrcDst,len));
}


IppStatus gms_ippsSubC_64fc_I(Ipp64fc val, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsSubC_64fc(val,pSrcDst,len));
}

/*
   SubCRev
Subtracts each element of a vector from a constant
value.
*/


IppStatus gms_ippsSubCRev_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {

          return (ippsSubCRev_32f(pSrc,val,pDst,len));
}



IppStatus gms_ippsSubCRev_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {

          return (ippsSubCRev_64f(pSrc,val,pDst,len));
}



IppStatus gms_ippsSubCRev_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {

          return (ippsSubCRev_32fc(pSrc,val,pDst,len));
}



IppStatus gms_ippsSubCRev_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {

          return (ippsSubCRev_64fc(pSrc,val,pDst,len));
}

/*
    In-place operations on floating point data.
*/



IppStatus gms_ippsSubCRev_32f_I(Ipp32f val, Ipp32f * pSrcDst, int32_t len) {

          return (ippsSubCRev_32f_I(val,pSrcDst,len));
}



IppStatus gms_ippsSubCRev_64f_I(Ipp64f val, Ipp64f * pSrcDst, int32_t len) {

          return (ippsSubCRev_64f_I(val,pSrcDst,len));
}



IppStatus gms_ippsSubCRev_32fc_I(Ipp32fc val, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsSubCRev_32fc_I(val,pSrcDst,len));
}



IppStatus gms_ippsSubCRev_64fc_I(Ipp64fc val, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsSubCRev_64fc_I(val,pSrcDst,len));
}

/*
    Sub
Subtracts the elements of two vectors.
*/


IppStatus gms_ippsSub_32f(const Ipp32f * pSrc1, const Ipp32f * pSrc2, Ipp32f * pDst, int32_t len) {

          return (ippsSub_32f(pSrc1,pSrc2,pDst,len));
}



IppStatus gms_ippsSub_64f(const Ipp64f * pSrc1, const Ipp64f * pSrc2, Ipp64f * pDst, int32_t len) {

          return (ippsSub_64f(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsSub_32fc(const Ipp32fc * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pDst, int32_t len) {

          return (ippsSub_32fc(pSrc1,pSrc2,pDst,len));
}


IppStatus gms_ippsSub_64fc(const Ipp64fc * pSrc1, const Ipp64fc * pSrc2, Ipp64fc * pDst, int32_t len) {

          return (ippsSub_64fc(pSrc1,pSrc2,pDst,len));
}

/*
    In-place operations on floating point data and integer data without scaling.
*/


IppStatus gms_ippsSub_32f_I(const Ipp32f * pSrc, Ipp32f * pSrcDst, int32_t len) {

          return (ippsSub_32f_I(pSrc,pSrcDst,len));
}



IppStatus gms_ippsSub_64f_I(const Ipp64f * pSrc, Ipp64f * pSrcDst, int32_t len) {

          return (ippsSub_64f_I(pSrc,pSrcDst,len));
}



IppStatus gms_ippsSub_32fc_I(const Ipp32fc * pSrc, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsSub_32fc_I(pSrc,pSrcDst,len));
}



IppStatus gms_ippsSub_64fc_I(const Ipp64fc * pSrc, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsSub_64fc_I(pSrc,pSrcDst,len));
}

/*
   DivC
Divides each element of a vector by a constant value
*/


IppStatus gms_ippsDivC_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {

          return (ippsDivC_32f(pSrc,val,pDst,len));
}


IppStatus gms_ippsDivC_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {

          return (ippsDivC_64f(pSrc,val,pDst,len));
}


IppStatus gms_ippsDivC_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {

          return (ippsDivC_32fc(pSrc,val,pDst,len));
}


IppStatus gms_ippsDivC_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {

          return (ippsDivC_64fc(pSrc,val,pDst,len));
}


IppStatus
gms_ippsDivC_32f_I(Ipp32f val, Ipp32f* pSrcDst, int32_t len) {

          return (ippsDivC_32f_I(val,pSrcDst,len));
}


IppStatus
gms_ippsDivC_64f_I(Ipp64f val, Ipp64f* pSrcDst, int32_t len) {

          return (ippsDivC_64f_I(val,pSrcDst,len));
}

IppStatus
gms_ippsDivC_32fc_I(Ipp32fc val, Ipp32fc* pSrcDst, int32_t len) {

          return (ippsDivC_32fc_I(val,pSrcDst,len));
}

IppStatus
gms_ippsDivC_64fc_I(Ipp64fc val, Ipp64fc* pSrcDst, int32_t len) {

          return (ippsDivC_64fc_I(val,pSrcDst,len));
}

IppStatus
gms_ippsDivCRev_32f(const Ipp32f* pSrc, Ipp32f val, Ipp32f* pDst, int32_t len) {

          return (ippsDivCRev_32f(pSrc,val,pDst,len));
}


IppStatus
gms_ippsDivCRev_32f_I(const Ipp32f* pSrcDst, Ipp32f val,int32_t len) {

          return (ippsDivCRev_32f_I(pSrcDst,val,len));
}

IppStatus
gms_ippsDiv_32f(const Ipp32f* pSrc1, const Ipp32f* pSrc2, Ipp32f* pDst, int32_t len) {

          return (ippsDiv_32f(pSrc1,pSrc2,pDst,len));
}


IppStatus
gms_ippsDiv_64f(const Ipp64f* pSrc1, const Ipp64f* pSrc2, Ipp64f* pDst, int32_t len) {

          return (ippsDiv_64f(pSrc1,pSrc2,pDst,len));
}

IppStatus
gms_ippsDiv_32fc(const Ipp32fc* pSrc1, const Ipp32fc* pSrc2, Ipp32fc* pDst, int32_t len) {

          return (ippsDiv_32fc(pSrc1,pSrc2,pDst,len));
}

IppStatus
gms_ippsDiv_64fc(const Ipp64fc* pSrc1, const Ipp64fc* pSrc2, Ipp64fc* pDst, int32_t len) {

          return (ippsDiv_64fc(pSrc1,pSrc2,pDst,len));
}


IppStatus
gms_ippsDiv_32f_I(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsDiv_32f_I(pSrc,pDst,len));
}


IppStatus
gms_ippsDiv_64f_I(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsDiv_64f_I(pSrc,pDst,len));
}

IppStatus
gms_ippsDiv_32fc_I(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

          return (ippsDiv_32fc_I(pSrc,pDst,len));
}

IppStatus
gms_ippsDiv_64fc_I(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

          return (ippsDiv_64fc_I(pSrc,pDst,len));
}


IppStatus
gms_ippsAbs_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsAbs_32f(pSrc,pDst,len));
}

IppStatus
gms_ippsAbs_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsAbs_64f(pSrc,pDst,len));
}


IppStatus
gms_ippsAbs_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsAbs_32f_I(pSrcDst,len));
}

IppStatus
gms_ippsAbs_64f_I(Ipp64f* pSrcDst, int32_t len) {

          return (ippsAbs_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsSqr_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsSqr_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsSqr_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsSqr_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsSqr_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

          return (ippsSqr_32fc(pSrc,pDst,len));
}

IppStatus
gms_ippsSqr_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

          return (ippsSqr_64fc(pSrc,pDst,len));
}

IppStatus					      
gms_ippsSqr_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsSqr_32f_I(pSrcDst,len));
}


IppStatus					      
gms_ippsSqr_64f_I(Ipp64f* pSrcDst, int32_t len) {

          return (ippsSqr_64f_I(pSrcDst,len));
}

IppStatus					      
gms_ippsSqr_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

          return (ippsSqr_32fc_I(pSrcDst,len));
}

IppStatus					      
gms_ippsSqr_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

          return (ippsSqr_64fc_I(pSrcDst,len));
}

IppStatus			
gms_ippsSqrt_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsSqrt_32f(pSrc,pDst,len));
}


IppStatus			
gms_ippsSqrt_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsSqrt_64f(pSrc,pDst,len));
}

IppStatus			
gms_ippsSqrt_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

          return (ippsSqrt_32fc(pSrc,pDst,len));
}

IppStatus			
gms_ippsSqrt_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

          return (ippsSqrt_64fc(pSrc,pDst,len));
}

IppStatus
gms_ippsSqrt_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsSqrt_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsSqrt_64f_I(Ipp64f* pSrcDst, int32_t len) {

          return (ippsSqrt_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsSqrt_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

          return (ippsSqrt_32fc_I(pSrcDst,len));
}

IppStatus
gms_ippsSqrt_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

          return (ippsSqrt_64fc_I(pSrcDst,len));
}

IppStatus
gms_ippsCubrt_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsCubrt_32f(pSrc,pDst,len));
}

IppStatus
gms_ippsExp_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsExp_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsExp_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsExp_64f(pSrc,pDst,len));
}


IppStatus
gms_ippsExp_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsExp_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsExp_64f_I(Ipp64f* pSrcDst, int32_t len) {

          return (ippsExp_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsLn_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsLn_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsLn_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsLn_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsLn_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsLn_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsLn_64f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsLn_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsSumLn_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pDst) {

          return (ippsSumLn_32f(pSrc,len,pDst));
}


IppStatus
gms_ippsSumLn_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pDst) {

          return (ippsSumLn_64f(pSrc,len,pDst));
}

IppStatus
gms_ippsSumLn_32f64f(const Ipp32f* pSrc, int32_t len, Ipp64f* pDst) {

          return (ippsSumLn_32f64f(pSrc,len,pDst));
}

IppStatus
gms_ippsArctan_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsArctan_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsArctan_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsArctan_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsArctan_32f_I(Ipp32f* pSrcDst, int32_t len) {

           return (ippsArctan_32f_I(pSrcDst,len));
}

IppStatus
gms_ippsArctan_64f_I(Ipp64f* pSrcDst, int32_t len) {

           return (ippsArctan_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsNormalize_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len,
                      Ipp32f vSub, Ipp32f vDiv) {
           return (ippsNormalize_32f(pSrc,pDst,len,vSub,vDiv));

}

IppStatus
gms_ippsNormalize_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len,
                      Ipp64f vSub, Ipp64f vDiv) {
           return (ippsNormalize_64f(pSrc,pDst,len,vSub,vDiv));

}

IppStatus
gms_ippsNormalize_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len,
                      Ipp32fc vSub, Ipp32fc vDiv) {
           return (ippsNormalize_32fc(pSrc,pDst,len,vSub,vDiv));

}

IppStatus
gms_ippsNormalize_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len,
                      Ipp64fc vSub, Ipp64fc vDiv) {
           return (ippsNormalize_64fc(pSrc,pDst,len,vSub,vDiv));

}

IppStatus
gms_ippsNormalize_32f_I(Ipp32f* pSrcDst, int32_t len,
                        Ipp32f vSub, Ipp32f vDiv) {

           return (ippsNormalize_32f_I(pSrcDst,len,vSub,vDiv));
}

IppStatus
gms_ippsNormalize_64f_I(Ipp64f* pSrcDst, int32_t len,
                        Ipp64f vSub, Ipp64f vDiv) {

           return (ippsNormalize_64f_I(pSrcDst,len,vSub,vDiv));
}

IppStatus
gms_ippsNormalize_32fc_I(Ipp32fc* pSrcDst, int32_t len,
                        Ipp32fc vSub, Ipp32fc vDiv) {

           return (ippsNormalize_32fc_I(pSrcDst,len,vSub,vDiv));
}

IppStatus
gms_ippsNormalize_64fc_I(Ipp64fc* pSrcDst, int32_t len,
                        Ipp64fc vSub, Ipp64fc vDiv) {

           return (ippsNormalize_64fc_I(pSrcDst,len,vSub,vDiv));
}


IppStatus
gms_ippsSortAscend_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsSortAscend_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsSortAscend_64f_I(Ipp64f* pSrcDst, int32_t len) {

          return (ippsSortAscend_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsSortDescend_32f_I(Ipp32f* pSrcDst, int32_t len) {

          return (ippsSortDescend_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsSortDescend_64f_I(Ipp64f* pSrcDst, int32_t len) {

          return (ippsSortDescend_64f_I(pSrcDst,len));
}

IppStatus
gms_ippsConvert32f64f(const Ipp32f* pSrc, Ipp64f* pDst, int32_t len) {

          return (ippsConvert32f64f(pSrc,pDst,len));
}

IppStatus
gms_ippsConvert64f32f(const Ipp64f* pSrc, Ipp32f* pDst, int32_t len) {

          return (ippsConvert64f32f(pSrc,pDst,len));
}


IppStatus
gms_ippsConj_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

          return (ippsConj_32fc(pSrc,pDst,len));
}

IppStatus
gms_ippsConj_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

          return (ippsConj_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsConj_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

          return (ippsConj_32fc_I(pSrcDst,len));
}


IppStatus
gms_ippsConj_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

          return (ippsConj_64fc_I(pSrcDst,len));
}


IppStatus
gms_ippsConjFlip_32fc(const Ipp32fc* pSrc ,Ipp32fc* pDst , int32_t len) {

         return (ippsConjFlip_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsConjFlip_64fc(const Ipp64fc* pSrc ,Ipp64fc* pDst , int32_t len) {

         return (ippsConjFlip_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsMagnitude_32f(const Ipp32f* pSrcRe, const Ipp32f* pSrcIm, Ipp32f* pDst, int32_t len) {

         return (ippsMagnitude_32f(pSrcRe,pSrcIm,pDst,len));
}


IppStatus
gms_ippsMagnitude_64f(const Ipp64f* pSrcRe, const Ipp64f* pSrcIm, Ipp64f* pDst, int32_t len) {

         return (ippsMagnitude_64f(pSrcRe,pSrcIm,pDst,len));
}


IppStatus
gms_ippsMagnitude_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

         return (ippsMagnitude_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsMagnitude_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

         return (ippsMagnitude_64fc(pSrc,pDst,len));
}

IppStatus
gms_ippsPhase_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

         return (ippsPhase_64fc(pSrc,pDst,len)); 
}


IppStatus
gms_ippsPhase_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

         return (ippsPhase_32fc(pSrc,pDst,len)); 
}

IppStatus
gms_ippPhase_32f(const Ipp32f* pSrcRe, const Ipp32f* pSrcIm, Ipp32f* pDst, int32_t len) {

         return (ippsPhase_32f(pSrcRe,pSrcIm,pDst,len));
}


IppStatus
gms_ippPhase_64f(const Ipp64f* pSrcRe, const Ipp64f* pSrcIm, Ipp64f* pDst, int32_t len) {

         return (ippsPhase_64f(pSrcRe,pSrcIm,pDst,len));
}


IppStatus
gms_ippsPowerSpectrum_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

         return (ippsPowerSpectrum_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsPowerSpectrum_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

         return (ippsPowerSpectrum_64fc(pSrc,pDst,len));
}

IppStatus
gms_ippsPowerSpectrum_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

         return (ippsPowerSpectrum_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsPowerSpectrum_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

         return (ippsPowerSpectrum_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsReal_32fc(const Ipp32fc* pSrc, Ipp32f* pDstRe, int32_t len) {

         return (ippsReal_32fc(pSrc,pDstRe,len));
}


IppStatus
gms_ippsReal_64fc(const Ipp64fc* pSrc, Ipp64f* pDstRe, int32_t len) {

         return (ippsReal_64fc(pSrc,pDstRe,len));
}


IppStatus
gms_ippsImag_32fc(const Ipp32fc* pSrc, Ipp32f* pDstIm, int32_t len) {

         return (ippsImag_32fc(pSrc,pDstIm,len));
}


IppStatus
gms_ippsImag_64fc(const Ipp64fc* pSrc, Ipp64f* pDstIm, int32_t len) {

         return (ippsImag_64fc(pSrc,pDstIm,len));
}



IppStatus
gms_ippsRealToCplx_32f(const Ipp32f* pSrcRe, const Ipp32f* pSrcIm,
                       Ipp32fc* pDst, int32_t len) {

         return (ippsRealToCplx_32f(pSrcRe,pSrcIm,pDst,len));
}


IppStatus
gms_ippsRealToCplx_64f(const Ipp64f* pSrcRe, const Ipp64f* pSrcIm,
                       Ipp64fc* pDst, int32_t len) {

         return (ippsRealToCplx_64f(pSrcRe,pSrcIm,pDst,len));
}


IppStatus
gms_ippsCplxToReal_32f(const Ipp32fc pSrc*, Ipp32f* pDstRe,
                       Ipp32f* pDstIm, int32_t len) {

          return (ippsCplxToReal_32f(pSrc,pDstRe,pDstIm,len));
}


IppStatus
gms_ippsCplxToReal_64f(const Ipp64fc pSrc*, Ipp64f* pDstRe,
                       Ipp64f* pDstIm, int32_t len) {

          return (ippsCplxToReal_64f(pSrc,pDstRe,pDstIm,len));
}


IppStatus
gms_ippsThreshold_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len, Ipp32f level,
                      IppCompOp opRel) {

          return (ippsThreshold_32f(pSrc,pDst,len,level,opRel));
}


IppStatus
gms_ippsThreshold_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len, Ipp64f level,
                      IppCompOp opRel) {

          return (ippsThreshold_64f(pSrc,pDst,len,level,opRel));
}


IppStatus
gms_ippsThreshold_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len, Ipp32f level,
                      IppCompOp opRel) {

          return (ippsThreshold_32fc(pSrc,pDst,len,level,opRel));
}


IppStatus
gms_ippsThreshold_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len, Ipp64f level,
                      IppCompOp opRel) {

          return (ippsThreshold_64fc(pSrc,pDst,len,level,opRel));
}


IppStatus
gms_ippsThreshold_LT_32f(const Ipp32f* pSrc, Ipp32f* pDst,
                         int32_t len, Ipp32f level) {

          return (ippsThreshold_LT_32f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_LT_64f(const Ipp64f* pSrc, Ipp64f* pDst,
                         int32_t len, Ipp64f level) {

          return (ippsThreshold_LT_64f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_LT_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst,
                         int32_t len, Ipp32f level) {

          return (ippsThreshold_LT_32fc(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_LT_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst,
                         int32_t len, Ipp64f level) {

          return (ippsThreshold_LT_64fc(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_GT_32f(const Ipp32f* pSrc, Ipp32f* pDst,
                         int32_t len, Ipp32f level) {

          return (ippsThreshold_GT_32f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_GT_64f(const Ipp64f* pSrc, Ipp64f* pDst,
                         int32_t len, Ipp64f level) {

          return (ippsThreshold_GT_64f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_GT_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst,
                         int32_t len, Ipp32f level) {

          return (ippsThreshold_GT_32fc(pSrc,pDst,len,level));
}

IppStatus
gms_ippsThreshold_GT_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst,
                         int32_t len, Ipp64f level) {

          return (ippsThreshold_GT_64fc(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_GT_32f_I(Ipp32f* pSrcDst, int32_t len,  Ipp32f level) {

           return (ippsThreshold_GT_32f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_GT_64f_I(Ipp64f* pSrcDst, int32_t len,  Ipp64f level) {

           return (ippsThreshold_GT_64f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_GT_32fc_I(Ipp32fc* pSrcDst, int32_t len,  Ipp32f level) {

           return (ippsThreshold_GT_32fc_I(pSrcDst,len,level));
}

IppStatus
gms_ippsThreshold_GT_64fc_I(Ipp64fc* pSrcDst, int32_t len,  Ipp64f level) {

           return (ippsThreshold_GT_64fc_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LT_32f_I(Ipp32f* pSrcDst, int32_t len,  Ipp32f level) {

           return (ippsThreshold_LT_32f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LT_64f_I(Ipp64f* pSrcDst, int32_t len,  Ipp64f level) {

           return (ippsThreshold_LT_64f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LT_32fc_I(Ipp32fc* pSrcDst, int32_t len,  Ipp32f level) {

           return (ippsThreshold_LT_32fc_I(pSrcDst,len,level));
}

IppStatus
gms_ippsThreshold_LT_64fc_I(Ipp64fc* pSrcDst, int32_t len,  Ipp64f level) {

           return (ippsThreshold_LT_64fc_I(pSrcDst,len,level));
}


IppStatus
gms_ippsLTAbs_32f(const Ipp32f* pSrc, Ipp32f* pDst,
                  int32_t len,  Ipp32f level) {

           return (ippsLTAbs_32f(pSrc,pDst,len,level));
}

IppStatus
gms_ippsLTAbs_64f(const Ipp64f* pSrc, Ipp64f* pDst,
                  int32_t len,  Ipp64f level) {

           return (ippsLTAbs_64f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsGTAbs_32f(const Ipp32f* pSrc, Ipp32f* pDst,
                  int32_t len,  Ipp32f level) {

           return (ippsGTAbs_32f(pSrc,pDst,len,level));
}

IppStatus
gms_ippsGTAbs_64f(const Ipp64f* pSrc, Ipp64f* pDst,
                  int32_t len,  Ipp64f level) {

           return (ippsGTAbs_64f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsGTAbs_32f_I(Ipp32f* pSrcDst, int32_t len, Ipp32f level) {

           return (ippsGTAbs_32f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsGTAbs_64f_I(Ipp64f* pSrcDst, int32_t len, Ipp64f level) {

           return (ippsGTAbs_64f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsLTAbs_32f_I(Ipp32f* pSrcDst, int32_t len, Ipp32f level) {

           return (ippsLTAbs_32f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsLTAbs_64f_I(Ipp64f* pSrcDst, int32_t len, Ipp64f level) {

           return (ippsLTAbs_64f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LTAbsVal_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len,
                                Ipp32f level, Ipp32f val) {

           return( ippsThreshold_LTAbsVal_32f(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTAbsVal_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len,
                                Ipp64f level, Ipp64f val) {

           return( ippsThreshold_LTAbsVal_64f(pSrc,pDst,len,level,val));
}

IppStatus
gms_ippsThreshold_LTAbsVal_32f_I(Ipp32f* pSrcDst, int32_t len,
                                 Ipp32f level, Ipp32f val) {

           return (ippsThreshold_LTAbsVal_32f_I(pSrcDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTAbsVal_64f_I(Ipp64f* pSrcDst, int32_t len,
                                 Ipp64f level, Ipp64f val) {

           return (ippsThreshold_LTAbsVal_64f_I(pSrcDst,len,level,val));
}

IppStatus
gms_ippsThreshold_LTVal_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len,
                            Ipp32f level, Ipp32f val) {

           return (ippsThreshold_LTVal_32f(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTVal_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len,
                            Ipp64f level, Ipp64f val) {

           return (ippsThreshold_LTVal_64f(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTVal_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len,
                            Ipp32f level, Ipp32f val) {

           return (ippsThreshold_LTVal_32fc(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTVal_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len,
                            Ipp64f level, Ipp64f val) {

           return (ippsThreshold_LTVal_64fc(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_GTVal_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len,
                            Ipp32f level, Ipp32f val) {

           return (ippsThreshold_GTVal_32fc(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_GTVal_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len,
                            Ipp64f level, Ipp64f val) {

           return (ippsThreshold_GTVal_64fc(pSrc,pDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTVal_32f_I(Ipp32f* pSrcDst, int32_t len,
                              Ipp32f level, Ipp32f val) {

	   return (ippsThreshold_LTVal_32f_I(pSrcDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTVal_64f_I(Ipp64f* pSrcDst, int32_t len,
                              Ipp64f level, Ipp64f val) {

	   return (ippsThreshold_LTVal_64f_I(pSrcDst,len,level,val));
}

IppStatus
gms_ippsThreshold_LTVal_32fc_I(Ipp32fc* pSrcDst, int32_t len,
                              Ipp32f level, Ipp32f val) {

	   return (ippsThreshold_LTVal_32fc_I(pSrcDst,len,level,val));
}

IppStatus
gms_ippsThreshold_LTVal_64fc_I(Ipp64fc* pSrcDst, int32_t len,
                              Ipp64f level, Ipp64f val) {

	   return (ippsThreshold_LTVal_64fc_I(pSrcDst,len,level,val));
}


IppStatus
gms_ippsThreshold_GTVal_32f_I(Ipp32f* pSrcDst, int32_t len,
                              Ipp32f level, Ipp32f val) {

	   return (ippsThreshold_GTVal_32f_I(pSrcDst,len,level,val));
}


IppStatus
gms_ippsThreshold_GTVal_64f_I(Ipp64f* pSrcDst, int32_t len,
                              Ipp64f level, Ipp64f val) {

	   return (ippsThreshold_GTVal_64f_I(pSrcDst,len,level,val));
}

IppStatus
gms_ippsThreshold_GTVal_32fc_I(Ipp32fc* pSrcDst, int32_t len,
                              Ipp32f level, Ipp32f val) {

	   return (ippsThreshold_GTVal_32fc_I(pSrcDst,len,level,val));
}

IppStatus
gms_ippsThreshold_GTVal_64fc_I(Ipp64fc* pSrcDst, int32_t len,
                              Ipp64f level, Ipp64f val) {

	   return (ippsThreshold_GTVal_64fc_I(pSrcDst,len,level,val));
}


IppStatus
gms_ippsThreshold_LTInv_32f(const Ipp32f* pSrc, Ipp32f* pDst,
			    int32_t len, Ipp32f level) {

           return (ippsThreshold_LTInv_32f(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_LTInv_64f(const Ipp64f* pSrc, Ipp64f* pDst,
			    int32_t len, Ipp64f level) {

           return (ippsThreshold_LTInv_64f(pSrc,pDst,len,level));
}

IppStatus
gms_ippsThreshold_LTInv_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst,
			    int32_t len, Ipp32f level) {

           return (ippsThreshold_LTInv_32fc(pSrc,pDst,len,level));
}

IppStatus
gms_ippsThreshold_LTInv_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst,
			    int32_t len, Ipp64f level) {

           return (ippsThreshold_LTInv_64fc(pSrc,pDst,len,level));
}


IppStatus
gms_ippsThreshold_LTInv_32f_I(Ipp32f* pSrcDst, int32_t len, Ipp32f level) {

           return (ippsThreshold_LTInv_32f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LTInv_64f_I(Ipp64f* pSrcDst, int32_t len, Ipp64f level) {

           return (ippsThreshold_LTInv_64f_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LTInv_32fc_I(Ipp32fc* pSrcDst, int32_t len, Ipp32f level) {

           return (ippsThreshold_LTInv_32fc_I(pSrcDst,len,level));
}


IppStatus
gms_ippsThreshold_LTInv_64fc_I(Ipp64fc* pSrcDst, int32_t len, Ipp64f level) {

           return (ippsThreshold_LTInv_64fc_I(pSrcDst,len,level));
}


IppStatus
gms_ippsCartToPolar_32f(const Ipp32f* pSrcRe, const Ipp32f* pSrcIm, Ipp32f pDstMgn*,
                        Ipp32f* pDstPhase, int32_t len) {

            return (ippsCartToPolar_32f(pSrcRe,pSrcIm,pDstMgn,pDstPhase,len));
}


IppStatus
gms_ippsCartToPolar_64f(const Ipp64f* pSrcRe, const Ipp64f* pSrcIm, Ipp64f pDstMgn*,
                        Ipp64f* pDstPhase, int32_t len) {

            return (ippsCartToPolar_64f(pSrcRe,pSrcIm,pDstMgn,pDstPhase,len));
}


IppStatus
gms_ippsCartToPolar_32fc(const Ipp32fc* pSrc, Ipp32f* pDstMgn,
                         Ipp32f* pDstPhase, int32_t len) {

            return (ippsCartToPolar_32fc(pSrc,pDstMgn,pDstPhase,len));
}


IppStatus
gms_ippsCartToPolar_64fc(const Ipp64fc* pSrc, Ipp64f* pDstMgn,
                         Ipp64f* pDstPhase, int32_t len) {

            return (ippsCartToPolar_64fc(pSrc,pDstMgn,pDstPhase,len));
}

IppStatus
gms_ippsPolarToCart_32f(const Ipp32f* pSrcMgn, const Ipp32f* pSrcPhase, Ipp32f* pDstRe,
                        Ipp32f* pDstIm, int32_t len) {

            return (ippsPolarToCart_32f(pSrcMgn,pSrcPhase,pDstRe,pDstIm,len));
}


IppStatus
gms_ippsPolarToCart_64f(const Ipp64f* pSrcMgn, const Ipp64f* pSrcPhase, Ipp64f* pDstRe,
                        Ipp64f* pDstIm, int32_t len) {

            return (ippsPolarToCart_64f(pSrcMgn,pSrcPhase,pDstRe,pDstIm,len));
}


IppStatus
gms_ippsPolarToCart_32fc(const Ipp32f* pSrcMgn, const Ipp32f* pSrcPhase,
                         Ipp32fc* pDst, int32_t len) {

            return (ippsPolarToCart_32fc(pSrcMgn,pSrcPhase,pDst,len));
}


IppStatus
gms_ippsPolarToCart_64fc(const Ipp64f* pSrcMgn, const Ipp64f* pSrcPhase,
                         Ipp64fc* pDst, int32_t len) {

            return (ippsPolarToCart_64fc(pSrcMgn,pSrcPhase,pDst,len));
}

IppStatus
gms_ippsMaxOrder_32f(const Ipp32f* pSrc, int32_t len, int32_t* pOrder) {

            return (ippsMaxOrder_32f(pSrc,len,pOrder));
}


IppStatus
gms_ippsMaxOrder_64f(const Ipp64f* pSrc, int32_t len, int32_t* pOrder) {

            return (ippsMaxOrder_64f(pSrc,len,pOrder));
}


IppStatus
gms_ippsFlip_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

             return (ippsFlip_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsFlip_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

             return (ippsFlip_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsFlip_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

             return (ippsFlip_32fc(pSrc,pDst,len));
}

IppStatus
gms_ippsFlip_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

             return (ippsFlip_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsFlip_32f_I(Ipp32f* pSrcDst, int32_t len) {

             return (ippsFlip_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsFlip_64f_I(Ipp64f* pSrcDst, int32_t len) {

             return (ippsFlip_64f_I(pSrcDst,len));
}


IppStatus
gms_ippsFlip_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

             return (ippsFlip_32fc_I(pSrcDst,len));
}


IppStatus
gms_ippsFlip_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

             return (ippsFlip_64fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBartlett_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

             return (ippsWinBartlett_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBartlett_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

             return (ippsWinBartlett_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsWinBartlett_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

             return (ippsWinBartlett_32fc(pSrc,pDst,len));
}

IppStatus
gms_ippsWinBartlett_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

             return (ippsWinBartlett_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBartlett_32f_I(Ipp32f* pSrcDst, int32_t len) {

             return (ippsWinBartlett_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBartlett_64f_I(Ipp64f* pSrcDst, int32_t len) {

             return (ippsWinBartlett_64f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBartlett_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

             return (ippsWinBartlett_32fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBartlett_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

             return (ippsWinBartlett_64fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackman_32f(const Ipp32f* pSrc, Ipp32f* pDst,
                        int32_t len, Ipp32f alpha) {

             return (ippsWinBlackman_32f(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinBlackman_64f(const Ipp64f* pSrc, Ipp64f* pDst,
                        int32_t len, Ipp64f alpha) {

             return (ippsWinBlackman_64f(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinBlackman_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst,
                        int32_t len, Ipp32f alpha) {

             return (ippsWinBlackman_32fc(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinBlackman_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst,
                        int32_t len, Ipp64f alpha) {

             return (ippsWinBlackman_64fc(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinBlackmanStd_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

             return (ippsWinBlackmanStd_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackmanStd_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

             return (ippsWinBlackmanStd_64f(pSrc,pDst,len));
}

IppStatus
gms_ippsWinBlackmanStd_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

             return (ippsWinBlackmanStd_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackmanStd_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

             return (ippsWinBlackmanStd_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

             return (ippsWinBlackmanOpt_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

             return (ippsWinBlackmanOpt_64f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

             return (ippsWinBlackmanOpt_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

             return (ippsWinBlackmanOpt_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinBlackman_32f_I(Ipp32f* pSrcDst, int32_t len, Ipp32f alpha) {

              return (ippsWinBlackman_32f_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinBlackman_64f_I(Ipp64f* pSrcDst, int32_t len, Ipp64f alpha) {

              return (ippsWinBlackman_64f_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinBlackman_32fc_I(Ipp32fc* pSrcDst, int32_t len, Ipp32f alpha) {

              return (ippsWinBlackman_32fc_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinBlackman_64fc_I(Ipp64fc* pSrcDst, int32_t len, Ipp64f alpha) {

              return (ippsWinBlackman_64fc_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinBlackmanOpt_32f_I(Ipp32f* pSrcDst, int32_t len) {

              return (ippsWinBlackmanOpt_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

              return (ippsWinBlackmanOpt_32fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_64f_I(Ipp64f* pSrcDst, int32_t len) {

              return (ippsWinBlackmanOpt_64f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanOpt_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

              return (ippsWinBlackmanOpt_64fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanStd_32f_I(Ipp32f* pSrcDst, int32_t len) {

              return (ippsWinBlackmanStd_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanStd_64f_I(Ipp64f* pSrcDst, int32_t len) {

              return (ippsWinBlackmanStd_64f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanStd_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

              return (ippsWinBlackmanStd_32fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinBlackmanStd_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

              return (ippsWinBlackmanStd_64fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinHammimg_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

              return (ippsWinHamming_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHammimg_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

              return (ippsWinHamming_64f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHammimg_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

              return (ippsWinHamming_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHammimg_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

              return (ippsWinHamming_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHamming_32f_I(Ipp32f* pSrcDst, int32_t len) {

              return (ippsWinHamming_32f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinHamming_64f_I(Ipp64f* pSrcDst, int32_t len) {

              return (ippsWinHamming_64f_I(pSrcDst,len));
}


IppStatus
gms_ippsWinHamming_32fc_I(Ipp32fc* pSrcDst, int32_t len) {

              return (ippsWinHamming_32fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinHamming_64fc_I(Ipp64fc* pSrcDst, int32_t len) {

              return (ippsWinHamming_64fc_I(pSrcDst,len));
}


IppStatus
gms_ippsWinHann_32f(const Ipp32f* pSrc, Ipp32f* pDst, int32_t len) {

               return (ippsWinHann_32f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHann_64f(const Ipp64f* pSrc, Ipp64f* pDst, int32_t len) {

               return (ippsWinHann_64f(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHann_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst, int32_t len) {

               return (ippsWinHann_32fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinHann_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst, int32_t len) {

               return (ippsWinHann_64fc(pSrc,pDst,len));
}


IppStatus
gms_ippsWinKaiser_32f(const Ipp32f* pSrc, Ipp32f* pDst,
                      int32_t len, Ipp32f alpha) {

                return (ippsWinKaiser_32f(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_64f(const Ipp64f* pSrc, Ipp64f* pDst,
                      int32_t len, Ipp64f alpha) {

                return (ippsWinKaiser_64f(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_32fc(const Ipp32fc* pSrc, Ipp32fc* pDst,
                      int32_t len, Ipp32f alpha) {

                return (ippsWinKaiser_32fc(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_64fc(const Ipp64fc* pSrc, Ipp64fc* pDst,
                      int32_t len, Ipp64f alpha) {

                return (ippsWinKaiser_64fc(pSrc,pDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_32f_I(Ipp32f* pSrcDst, int32_t len, Ipp32f alpha) {

                 return (ippsWinKaiser_32f_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_64f_I(Ipp64f* pSrcDst, int32_t len, Ipp64f alpha) {

                 return (ippsWinKaiser_64f_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_32fc_I(Ipp32fc* pSrcDst, int32_t len, Ipp32f alpha) {

                 return (ippsWinKaiser_32fc_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsWinKaiser_64fc_I(Ipp64fc* pSrcDst, int32_t len, Ipp64f alpha) {

                 return (ippsWinKaiser_64fc_I(pSrcDst,len,alpha));
}


IppStatus
gms_ippsSum_32f(const Ipp32f* pSrc, int32_t len,
                Ipp32f* pSum, IppHintAlgorithm hint) {

                 return (ippsSum_32f(pSrc,len,pSum,hint));
}


IppStatus
gms_ippsSum_64f(const Ipp64f* pSrc, int32_t len,
                Ipp64f* pSum, IppHintAlgorithm hint) {

                 return (ippsSum_64f(pSrc,len,pSum,hint));
}


IppStatus
gms_ippsSum_32fc(const Ipp32fc* pSrc, int32_t len,
                Ipp32fc* pSum, IppHintAlgorithm hint) {

                 return (ippsSum_32fc(pSrc,len,pSum,hint));
}


IppStatus
gms_ippsSum_64fc(const Ipp64fc* pSrc, int32_t len,
                Ipp64fc* pSum, IppHintAlgorithm hint) {

                 return (ippsSum_64fc(pSrc,len,pSum,hint));
}


IppStatus
gms_ippsMax_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pMax) {

                return (ippsMax_32f(pSrc,len,pMax));
}


IppStatus
gms_ippsMax_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pMax) {

                return (ippsMax_64f(pSrc,len,pMax));
}


IppStatus
gms_ippsMaxIndx_32f(const Ipp32f* pSrc, int32_t len,
                    Ipp32f* pMax, int32_t* pIndx) {

                return (ippsMaxIndx_32f(pSrc,len,pMax,pIndx));
}


IppStatus
gms_ippsMaxIndx_64f(const Ipp64f* pSrc, int32_t len,
                    Ipp64f* pMax, int32_t* pIndx) {

                return (ippsMaxIndx_64f(pSrc,len,pMax,pIndx));
}


IppStatus
gms_ippsMaxAbs_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pMaxAbs) {

                 return (ippsMaxAbs_32f(pSrc,len,pMaxAbs));
}


IppStatus
gms_ippsMaxAbs_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pMaxAbs) {

                 return (ippsMaxAbs_64f(pSrc,len,pMaxAbs));
}


IppStatus
gms_ippsMaxAbsIndx_32f(const Ipp32f* pSrc, int32_t len,
                       Ipp32f* pMaxAbs, int32_t* pIndx) {

                 return (ippsMaxAbsIndx_32f(pSrc,len,pMaxAbs,pIndx));
}


IppStatus
gms_ippsMaxAbsIndx_64f(const Ipp64f* pSrc, int32_t len,
                       Ipp64f* pMaxAbs, int32_t* pIndx) {

                 return (ippsMaxAbsIndx_64f(pSrc,len,pMaxAbs,pIndx));
}


IppStatus
gms_ippsMin_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pMin) {

                  return (ippsMin_32f(pSrc,len,pMin));
}


IppStatus
gms_ippsMin_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pMin) {

                  return (ippsMin_64f(pSrc,len,pMin));
}


IppStatus
gms_ippsMinIndx_32f(const Ipp32f* pSrc, int32_t len,
                    Ipp32f* pMin, int32_t* pIndx) {

                  return (ippsMinIndx_32f(pSrc,len,pMin,pIndx));
}


IppStatus
gms_ippsMinIndx_64f(const Ipp64f* pSrc, int32_t len,
                    Ipp64f* pMin, int32_t* pIndx) {

                  return (ippsMinIndx_64f(pSrc,len,pMin,pIndx));
}


IppStatus
gms_ippsMinAbs_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pMinAbs) {

                   return (ippsMinAbs_32f(pSrc,len,pMinAbs));
}


IppStatus
gms_ippsMinAbs_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pMinAbs) {

                   return (ippsMinAbs_64f(pSrc,len,pMinAbs));
}


IppStatus
gms_ippsMinAbsIndx_32f(const Ipp32f* pSrc , int32_t len,
                       Ipp32f* pMinAbs, int32_t* pIndx) {

                    return (ippsMinAbsIndx_32f(pSrc,len,pMinAbs,pIndx));
}


IppStatus
gms_ippsMinAbsIndx_64f(const Ipp64f* pSrc , int32_t len,
                       Ipp64f* pMinAbs, int32_t* pIndx) {

                    return (ippsMinAbsIndx_64f(pSrc,len,pMinAbs,pIndx));
}


IppStatus
gms_ippsMinMax_32f(const Ipp32f* pSrc, int32_t len,
                   Ipp32f* pMin, Ipp32f* pMax) {

                    return (ippsMinMax_32f(pSrc,len,pMin,pMax));
}


IppStatus
gms_ippsMinMax_64f(const Ipp64f* pSrc, int32_t len,
                   Ipp64f* pMin, Ipp64f* pMax) {

                    return (ippsMinMax_64f(pSrc,len,pMin,pMax));
}


IppStatus
gms_ippsMinMaxIndx_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pMin, int32_t* pMinIndx,
                       Ipp32f* pMax, int32_t* pMaxIndx) {

                    return (ippsMinMaxIndx_32f(pSrc,len,pMin,pMinIndx,pMax,pMaxIndx));
}


IppStatus
gms_ippsMinMaxIndx_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pMin, int32_t* pMinIndx,
                       Ipp64f* pMax, int32_t* pMaxIndx) {

                    return (ippsMinMaxIndx_64f(pSrc,len,pMin,pMinIndx,pMax,pMaxIndx));
}


IppStatus
gms_ippsMean_32f(const Ipp32f* pSrc, int32_t len,
                 Ipp32f* pMean, IppHintAlgorithm hint) {

                    return (ippsMean_32f(pSrc,len,pMean,hint));
}


IppStatus
gms_ippsMean_64f(const Ipp64f* pSrc, int32_t len,
                 Ipp64f* pMean, IppHintAlgorithm hint) {

                    return (ippsMean_64f(pSrc,len,pMean,hint));
}


IppStatus
gms_ippsMean_32fc(const Ipp32fc* pSrc, int32_t len,
                 Ipp32fc* pMean, IppHintAlgorithm hint) {

                    return (ippsMean_32fc(pSrc,len,pMean,hint));
}


IppStatus
gms_ippsMean_64fc(const Ipp64fc* pSrc, int32_t len,
                 Ipp64fc* pMean, IppHintAlgorithm hint) {

                    return (ippsMean_64fc(pSrc,len,pMean,hint));
}


IppStatus
gms_ippsStdDev_32f(const Ipp32f* pSrc, int32_t len,
                   Ipp32f* pStdDev, IppHintAlgorithm hint) {

                    return (ippsStdDev_32f(pSrc,len,pStdDev,hint));
}


IppStatus
gms_ippsStdDev_64f(const Ipp64f* pSrc, int32_t len,
                   Ipp64f* pStdDev, IppHintAlgorithm hint) {

                    return (ippsStdDev_64f(pSrc,len,pStdDev,hint));
}


IppStatus
gms_ippsMeanStdDev_32f(const Ipp32f* pSrc, int32_t len, Ipp32f* pMean, Ipp32f* pStdDev,
                       IppHintAlgorithm hint) {

                    return (ippsMeanStdDev_32f(pSrc,len,pMean,StdDev,hint));
}


IppStatus
gms_ippsMeanStdDev_64f(const Ipp64f* pSrc, int32_t len, Ipp64f* pMean, Ipp64f* pStdDev,
                       IppHintAlgorithm hint) {

                    return (ippsMeanStdDev_64f(pSrc,len,pMean,StdDev,hint));
}
