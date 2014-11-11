/*
 * function [error,path1,path2] = dtw(s1, s2)
 * [error,path1,path2] = dtw(s1, s2)
 * Dynamic Time Warping search... by Malcolm Slaney
 * After this routine
 *	s1 approximately = s2(path1)
 *	s2 approximately = s1(path2)
 * and error is a scalar with the lowest energy found.
 * Both s1 and s2 are warped with respect to their columns
 *  (each column stays the same, but shifts in position)
 *
 * (c) Interval Research Corporation, 1995-1997
 * Thanks to Tony Robinson (Cambridge Univ.) for the structure
 * of this routine.  His algorithm was kindly posted to 
 * comp.speech on 29 Dec. 1994.
 *
 * To test this routine
 *	>> warp=[1 1 1 1 2 2 2 2 1 1 1 1 .5 .5 .5 .5]
 *	>> d1=randn(1,16);
 *	>> d2=d1(min(16,floor(cumsum(warp))));
 *	>> [err,p1,p2] = dtw(d1,d2);
 *
 * Unless your random numbers are really bizarre, you should get something
 * like this
 * p1 =
 *  Columns 1 through 12
 *	1     2     3     4     4     5     5     6     7     8     9    10
 *
 *  Columns 13 through 16
 *
 *      11    12    13    15
 *
 * p2 =
 *  Columns 1 through 12
 *
 *      1     2     3     4     6     8     9    10    11    12    13    14
 *
 * Columns 13 through 16
 *
 *     15    15    16    16
 */

#include	<stdlib.h>
#include	<math.h>

#ifndef	HUGE
	#define	HUGE	1e30
#endif

#define	s1(r,c)    v1[(r-1) + (c-1)*(d)]
#define	s2(r,c)    v2[(r-1) + (c-1)*(d)]
#define	ldist(i,j) larray[(i-1)+(j-1)*l1]
#define	gdist(i,j) garray[(i-1)+(j-1)*l1]
#define	path(i,j)  parray[(i-1)+(j-1)*l1]
#define	path1(i)   p1vector[i-1]
#define	path2(i)   p2vector[i-1]

#ifdef	MATLAB
#define	DATA	double
#define	INDEX	short
#else
#define	DATA	float
#define	INDEX	char
#endif

DATA dtw(DATA *v1, int l1, DATA *v2, int l2, int d, INDEX **pp1, INDEX **pp2);

DATA dtw(DATA *v1, int l1, DATA *v2, int l2, int d, INDEX **pp1, INDEX **pp2){
	float 	ldist;
	int	i, j, k, p1, p2;/* General Indices */
	int	topPath, midPath, botPath;
	float	*larray;	/* Local Distance Array */
	float	*garray;	/* Global Distance Array */
	char	*parray;	/* Path Array */
	INDEX	*p1vector;	/* Path 1 vector */
	INDEX	*p2vector;	/* Path 1 vector */
	DATA	error;		/* Return value */


/*
 * Now figure out all the relative distances all at once.  We'll
 * never use the outside corners of this array, but it's easier
 * to calculate all at once.
 * ldist=zeros(l1, l2);
 * for i=1:l1
 * 	for j=1:l2
 * 		ldist(i,j) = sum((s1(:,i)-s2(:,j)).^2);
 * 	end
 * end
*/
	larray = (float *)malloc(sizeof(*larray)*l1*l2);
	if (!larray)
		return 0;

	for (i=1; i<=l1; i++){
		for (j=1; j<=l2; j++){
			float	diff, sum;

			sum = 0;
			for (k=1; k<=d; k++){
				diff = s1(k,i)-s2(k,j);
				sum += diff*diff;
			}
			ldist(i,j) = sum;
		}
	}

/* We're only going to allow slopes of 2,1,.5.  */
	topPath=1;
	midPath=2;
	botPath=3;

/* We'll fill the global distance array with infinities. */
	
	garray = (float *)malloc(sizeof(*garray)*l1*l2);
	parray = (char *)malloc(sizeof(*parray)*l1*l2);
	if (!garray || !parray)
		return 0;

	for (i=1; i<=l1; i++){
		for (j=1; j<=l2; j++){
			gdist(i, j) = HUGE;
			path(i,j) = -1;
		}
	}

/* The first two directions are fixed.  They have to be the 
 * middle path.
 */
	gdist(1,1) = ldist(1,1);
	path(1,1) = midPath;
	gdist(2,2) = gdist(1,1) + ldist(2,2);
	path(2,2) = midPath;

	for (i=3; i<=l1; i++){
		for (j=3; j<=l2; j++){
			register float top, mid, bot;

			top = gdist(i-2,j-1) + ldist(i-1,j) + ldist(i,j);
			mid = gdist(i-1,j-1) + ldist(i,j);
			bot = gdist(i-1,j-2) + ldist(i,j-1) + ldist(i,j);

			if (top < mid && top < bot){
				gdist(i,j) = top;
				path(i,j) = topPath;
			} else if (bot < top && bot < mid){
				gdist(i,j) = bot;
				path(i,j) = botPath;
			} else {
				gdist(i,j) = mid;
				path(i,j) = midPath;
			}
		}
	}

	error = gdist(l1,l2);

/* Now backtrack through the array looking for the path that got
 * us the minimum distance.  Luckily we left a string of 
 * directions that we can use to find our way back to the origin.
 */
	p1 = l1;
	p2 = l2;
	
	p1vector = malloc(l1*sizeof(*p1vector));
	p2vector = malloc(l2*sizeof(*p2vector));
	
	if (!p1vector || !p2vector)
		return 0;
		
	while (p1 > 0 && p2 > 0){
		int	direct;

		path1(p1) = p2;
		path2(p2) = p1;
		direct = path(p1,p2);
		if (direct == topPath){
			p1 = p1 - 1;
			path1(p1) = p2;
			path2(p2) = p1;
		} else if (direct == botPath){
			p2 = p2 - 1;
			path1(p1) = p2;
			path2(p2) = p1;
		}
		p1 = p1 - 1;
		p2 = p2 - 1;
	}
	if (pp1)
		*pp1 = p1vector;

	if (pp2)
		*pp2 = p2vector;

	return error;
}

#ifdef	MAIN
#include	<stdio.h>


/* 
 * The following is useful test code... generate a known warping 
 * signal, generate two arrays of sort of random data, and then
 * use d1 and d2 as dtw input.
 */
void smooth(float data[][2], int l);

float	warp[] = {1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, .5, .5, .5, .5};
#define	l2	(sizeof(warp)/sizeof(warp[0]))

#define	min(x,y) ((x)<(y)?(x):(y))
#define	max(x,y) ((x)>(y)?(x):(y))
#define	d(i,j) data[min(max(0,i),l-1)][j]

void smooth(float data[][2], int l){
	int	i;

	for (i=0; i<l; i++){
		data[i][0] = 0.25*(d(i-3,0)+d(i-2,0)+d(i-1,0)+d(i,0));
		data[i][1] = 0.25*(d(i-3,1)+d(i-2,1)+d(i-1,1)+d(i,1));
	}
	for (i=0; i<l; i++){
		data[i][0] = (int)(100*data[i][0]);
		data[i][1] = (int)(100*data[i][1]);
	}
}

main(){
	int	i, l1;
	DATA	d1[2*l2][2], d2[l2][2], error;
	INDEX	*p1, *p2;

	for (i=1; i<l2; i++)			/* Accumulate sum */
		warp[i] = warp[i-1] + warp[i];
	for (i=1; i<l2; i++)			/* Convert to integers */
		warp[i] = (int)warp[i];
		
	l1 = (int)warp[l2-1];

	for (i=0; i<l1; i++){
		d1[i][0] = rand()/(float)((1<<15)-1);
		d1[i][1] = rand()/(float)((1<<15)-1);
		d2[i][0] = rand()/(float)((1<<15)-1);
		d2[i][1] = rand()/(float)((1<<15)-1);
	}
	smooth(d1, l1);
	
	printf("The warping path is d2=d1(warp(*)):\n");
	for (i=0;i<l2; i++){
		printf("%g ", warp[i]);
	}
	printf("\n");

	for (i=0;i<l2; i++){
		d2[i][0] = d1[(int)warp[i]-1][0];
		d2[i][1] = d1[(int)warp[i]-1 ][1];
	}

	printf("Signal 1 is:\n");
	for (i=0;i<l1;i++) printf("%2g ", d1[i][0]); printf("\n");
	for (i=0;i<l1;i++) printf("%2g ", d1[i][1]); printf("\n");

	printf("Signal 2 is:\n");
	for (i=0;i<l2;i++) printf("%2g ", d2[i][0]); printf("\n");
	for (i=0;i<l2;i++) printf("%2g ", d2[i][1]); printf("\n");

	error = dtw((DATA *)d1, l1, (DATA *)d2, l2, 2, &p1, &p2);

	printf("Total path error is %g.\n", error);
	
	printf("Path 1 is (s1 ~= s2(path1):\n");
	for (i=0; i<l1; i++){
		printf("%d ", p1[i]);
	}
	printf("\n");
		
	printf("Path 2 is (s2 ~= s1(path2):\n");
	for (i=0; i<l2; i++){
		printf("%d ", p2[i]);
	}
	printf("\n");
		
/* 
	warp = [ones(1,4) ones(1,4)*2 ones(1,4) ones(1,4)*.5];
	warp = cumsum(warp);
	
	d1 = rand(2,max(warp));
	d1(1,:) = filter([1 1 1 1],[1],d1(1,:)')';
	d1(2,:) = filter([1 1 1 1],[1],d1(2,:)')';	
	d2 = d1(:,warp);
 */

}

#endif

#ifdef	TIMING

#define	K	13
#define	N	450

DATA	s1[N][K], s2[N][K];

main(){
	int	i, j;
	
	for (i=0; i<N; i++){
		for (j=0; j<K; j++){
			s1[i][j] = rand()/(float)((1<<15)-1);
			s2[i][j] = rand()/(float)((1<<15)-1);
		}
	}
	
	dtw((DATA *)s1, N, (DATA *)s2, N, K, 0, 0);
}
	
#endif	
		
#ifdef	MATLAB

#define	pS1	prhs[0]
#define	pS2	prhs[1]

#include	<stdio.h>
#include 	<math.h>
#include	"mex.h"


#define	mxGetSize(m)	(mxGetN(m) * mxGetM(m))

   
/* =====================================================================*/
/*	Function to determine if arguments are valid. 			*/
/*	Also fill in some pointers we will need later.			*/

static  int CheckArguments(int nlhs, mxArray *plhs[], 
				int nrhs, const mxArray *prhs[])
{     
	int	k;

	if (nrhs < 2 || nlhs < 1){
		printf("Incorrect calling syntax:\n [error,p1,p2] = ");
		printf("dtw(s1,s2)\n");
		printf("	where s1 is KxN and S2 is KxM in size\n");
		return 1;
	}

/*------------ Check inputs are compatible ------------------------------ */	
	k = mxGetM(pS1);
	if (k != mxGetM(pS2)){
		printf("Two input arrays are not the same height\n");
		return 1;
	}
	return 0;
}


#define	kCommandSize	20

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])  
{ 	
	int	i;
	register double	error;
	mxArray	*mp1 = 0, *mp2 = 0, *ep = 0;
	INDEX	*md1, *md2;	

	if (CheckArguments(nlhs, plhs, nrhs, prhs) ){
		mexErrMsgTxt("dtw argument checking failed.");
		return ;
	}

	if (nlhs > 1){
		mp1 = mxCreateDoubleMatrix(1, mxGetN(pS1), mxREAL);
		plhs[1] = mp1;
	}

	if (nlhs > 2){
		mp2 = mxCreateDoubleMatrix(1, mxGetN(pS2), mxREAL);
		plhs[2] = mp2;
	}

	error = dtw(mxGetPr(pS1), mxGetN(pS1), 
		    mxGetPr(pS2), mxGetN(pS2), mxGetM(pS1),
		    &md1, &md2);

	if (nlhs > 0){
		plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
		if (plhs[0])
			*(mxGetPr(plhs[0])) = error;
	}

	if (mp1){
		double	*rp = mxGetPr(mp1);
		int	len = mxGetN(pS1);

		for (i=0;i<len;i++)
			*rp++ = *md1++;
	}

	if (mp2){
		double	*rp = mxGetPr(mp2);
		int	len = mxGetN(pS2);

		for (i=0;i<len;i++)
			*rp++ = *md2++;
	}
}
#endif
