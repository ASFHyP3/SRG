#include	"utilities.h"
#include	<unistd.h>
#include	<sys/time.h>
#include	<sys/times.h>
#include	<sys/resource.h>

/* The same code is used for both C and Fortran entry points.
 */
#define	WC_GUTS								\
									\
  static int     first = 1;						\
  static double  t0;							\
  struct timeval s_val;							\
									\
  gettimeofday(&s_val,0);						\
  if (first) {								\
    t0    = (double) s_val.tv_sec + 0.000001*s_val.tv_usec;		\
    first = 0;								\
    return (0.0);							\
  }									\
  return ((double) s_val.tv_sec + 0.000001*s_val.tv_usec - t0);
  
/* Returns the current value of the wall clock timer.
 * C entry point.
 */
double
wc_second()

{
  WC_GUTS;
}
  
/* Returns the current value of the wall clock timer.
 * Fortran entry point.
 */
double
wc_second_()

{
  WC_GUTS;
}

#define	US_GUTS								\
									\
  static int	first = 1;						\
  static double	t0;							\
  struct rusage	ru;							\
  double	tu, ts;							\
									\
  getrusage(RUSAGE_SELF,&ru);						\
  if (first) {								\
    t0    = ru.ru_utime.tv_sec + 1.0e-6*ru.ru_utime.tv_usec		\
          + ru.ru_stime.tv_sec + 1.0e-6*ru.ru_stime.tv_usec;		\
    first = 0;								\
    return (0.0);							\
  }									\
									\
  tu = ru.ru_utime.tv_sec + 1.0e-6*ru.ru_utime.tv_usec;			\
  ts = ru.ru_stime.tv_sec + 1.0e-6*ru.ru_stime.tv_usec;			\
									\
  return (tu + ts - t0);

/* Returns the current value of the user+system timer.  C entry point.
 */
double
us_second()

{
  US_GUTS;
}

/* Returns the current value of the user+system timer.  Fortran entry point.
 */
double
us_second_()

{
  US_GUTS;
}

/* Returns the current value of the wall clock timer, or
 * user+system timer depending on the valueof tmode: 
 * less than zero the wall-clock timer, and greater than zero
 * user+system time.  C entry point.
 */
double
/* C entry point name changed from secondo to secondoC to prevent
   symbol name conflict when using xlf Fortran compiler.
   Neither the below secondoC nor any of the above *_second routines
   are used within ROI_PAC.
   Only the secondo_ Fortran entry is used.
   A better solution would be to implement timing routines in libroipac
   based on Fortran 90 standard intrinsic time routines.
*/
secondoC(

int	tmode)

{
  if (tmode > 0) {
    US_GUTS;
  } else if (tmode < 0) {
    WC_GUTS;
  } else if (tmode == 0) {
    printf("Invalid tmode.\n");
    return(0.0);
  }
}

/* Returns the current value of the wall clock timer, or
 * user+system timer depending on the valueof tmode: 
 * less than zero the wall-clock timer, and greater than zero
 * user+system time.  Fortran entry point.
 */
double
secondo_(

int	*ptmode)

{
  int	tmode = *ptmode;

  if (tmode > 0) {
    US_GUTS;
  } else if (tmode < 0) {
    WC_GUTS;
  } else if (tmode == 0) {
    printf("Invalid tmode.\n");
    return(0.0);
  }

}
