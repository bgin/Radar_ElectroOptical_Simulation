
#ifndef __GMS_SIMD_MEMOPS_DEFS_H__
#define __GMS_SIMD_MEMOPS_DEFS_H__



#if !defined (MEMMOVE_1ELEM)
#define MEMMOVE_1ELEM 1
#endif

#if !defined (MEMMOVE_16ELEMS)
#define MEMMOVE_16ELEMS 16
#endif

#if !defined (MEMMOVE_32ELEMS)
#define MEMMOVE_32ELEMS 32
#endif

#if !defined (MEMMOVE_64ELEMS)
#define MEMMOVE_64ELEMS 64
#endif

#if !defined (MEMMOVE_128ELEMS)
#define MEMMOVE_128ELEMS 128
#endif

#if !defined (MEMMOVE_256ELEMS)
#define MEMMOVE_256ELEMS 256
#endif
// float type (4-bytes)
#define YMM_LEN (8)
#define ZMM_LEN (16)

#if !defined (PAGE4KiB)
#define PAGE4KiB 4096
#endif

#if !defined (MAXFLOATSPERPAGE4KiB)
#define MAXFLOATSPERPAGE4KiB 1024
#endif


//
// Round down to 4 elements
//
#ifndef ROUND_TO_FOUR
#define ROUND_TO_FOUR(x,y) ((x) & ~((y)-1))
#endif
// Basically the same as above.
#ifndef ROUND_TO_EIGHT
#define ROUND_TO_EIGHT(x,y) ((x) & ~((y)-1))
#endif

#ifndef ROUND_TO_SIXTEEN
#define ROUND_TO_SIXTEEN(x,y) ((x) & ~((y)-1))
#endif












#endif /*__GMS_SIMD_MEMOPS_DEFS_H__*/
