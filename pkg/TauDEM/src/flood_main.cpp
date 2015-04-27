// flood_main.cpp:
#include "flood.h"
#include <R.h>

int  i, j;
int i1,i2,n1,n2;                        //flood, setdir
int nx, ny;
int npool, pooln, pstack;                       //flood
int nis, istack;                                                //flood, setdir
int filetype;                                                                   //all IO
short *dn, *is, *js;                                    //setdir, flood
short *ipool, *jpool, *tmp;             //flood
float emin, et;                                         //flood, setdir
int nf;                         //flood, setdir
int err;
int recursedepth;
int ccheck;   //Global contamination check flag
int useww;//Global use weights flag
float ndva;   //Global angle no data value
float ndv;                                              //area, setdir
float fdmval;
short useflowfile;
char *newflowfile;
int writeflowfile;

double dx, dy, csize;
int dd1[8],dd2[8];
double bndbox[4];
float mval;
double xllcenter=0,yllcenter=0;
//Offset pointers d1 and d2
int *d1 = dd1-1;
int *d2 = dd2-1;
fgrid felevg;
fgrid fslopeg;
sgrid sapoolg;
sgrid sdir;
igrid larr;
fgrid fareaw;
fgrid fweight;

// extern "C"
extern "C" {

//************************************************************************

void darea(int i,int j)
  {

    int in,jn,k,con=0;
      // con is a flag that signifies possible contaminatin of area
                //due to edge effects
    if(i!=0 && i!=ny-1 && j!=0 && j!=nx-1 && sdir.d[j][i] > -1)
                 //not on boundary
    {
      if(larr.d[j][i]==0)  // not touched yet
      {
       larr.d[j][i]=1;
                if(useww == 1)fareaw.d[j][i]=fweight.d[j][i];
        for(k=1; k<=8; k++)
        {  in=i+d1[k];
           jn=j+d2[k];
//test if neighbor drains towards cell excluding boundaryies
           if(sdir.d[jn][in]>0 && ( (sdir.d[jn][in]-k)==4 || (sdir.d[jn][in]-k)==-4))
             {
                darea(in,jn);
                if(larr.d[jn][in] < 0)con = -1;
                else larr.d[j][i]=larr.d[j][i]+larr.d[jn][in];
                                if(useww == 1)
                                {
                                        if(fareaw.d[jn][in] <= ndv  || fareaw.d[j][i] <=ndv)
                                        {  fareaw.d[j][i]=ndv;
                                        }
                                        else fareaw.d[j][i]=fareaw.d[j][i]+fareaw.d[jn][in];
                                }
             }
           if(sdir.d[jn][in] < 0)con = -1;
        }
        if(con == -1 && ccheck == 1)
                {
                        larr.d[j][i]=-1;
                        if(useww == 1)fareaw.d[j][i]=ndv;
                }
      }
    }
        else larr.d[j][i]=-1;
}

//************************************************************************

//  Function to climb recursively flow direction grid for min elevation to burn down
float climb(int i,int j)
{
        float emin,eneigh;
        int in,jn,k;
        emin=felevg.d[j][i];
        for(k=1; k<=8; k++)
        {
                in=i+d1[k];
jn=j+d2[k];
/* test if neighbor drains towards cell excluding boundaries */
if(sdir.d[jn][in]>0 && (sdir.d[jn][in]-k==4 || sdir.d[jn][in]-k ==-4))
                {
                        eneigh=climb(in,jn);
                        if(eneigh < emin)emin=eneigh;
                }
        }
        felevg.d[j][i] = emin;  // burn down elevation
        return(emin);

}

//************************************************************************



int dontcross(int k,int i,int j)
{
        int n1, n2, c1, c2, ans=0;
switch(k)
{
  case 2:
                n1=1; c1=4; n2=3; c2=8;
                if(sdir.d[j+d2[n1]][i+d1[n1]] == c1 ||
                        sdir.d[j+d2[n2]][i+d1[n2]] == c2)
                          ans=1;
                break;
  case 4:
                n1=3; c1=6; n2=5; c2=2;
                if(sdir.d[j+d2[n1]][i+d1[n1]] == c1 ||
                sdir.d[j+d2[n2]][i+d1[n2]] == c2)
                   ans=1;
                break;
          case 6:
                n1=7; c1=4; n2=5; c2=8;
                if(sdir.d[j+d2[n1]][i+d1[n1]] == c1 ||
                sdir.d[j+d2[n2]][i+d1[n2]] == c2)
                   ans=1;
                break;
          case 8:
                n1=1; c1=6; n2=7; c2=2;
                if(sdir.d[j+d2[n1]][i+d1[n1]] == c1 ||
                        sdir.d[j+d2[n2]][i+d1[n2]] == c2)
                           ans=1;
                break;
}
return(ans);
}

//************************************************************************

//  Function to burn down flow elevations for given flow directions.
//  This is a placeholder for what will eventually be simplex optimization
int fdsimplex()
{


        int in,jn,i,j;
        err = TD_NO_ERROR;
        float emin;
        for(i=1; i<ny-1; i++)
                for(j=1; j<nx-1; j++)
                {
                        if(sdir.d[j][i] > 0)
                        {
                                in=i+d1[sdir.d[j][i]];
                                jn=j+d2[sdir.d[j][i]];
                                if(sdir.d[jn][in] <= 0)  // Neighbour is out
                                        emin=climb(i,j);
                        }
                }
        return(err);

}

//************************************************************************

//  Function to clean given flow directions.
int dirclean(char *newflowfile, int writeflowfile)
{
/*  Find all locations that have an imposed flow direction that drain to a location
        that does not have imposed flow directions.
        Evaluate contrib area for these locations not doing contamination checking.
        At the end we have contrib area evaluated for the skeleton of cells that drain
        off the imposed raster.  Any other cells in the imposed raster are "cleaned"
        from it because they do not drain off the raster, i.e. have inconsistent directions
        */
        int in,jn,i,j;
        err = TD_NO_ERROR;
        ccheck=0;  // These are globals to remove contamination checking
        useww=0;  //  for the darea function
        for(i=1; i<ny-1; i++)
                for(j=1; j<nx-1; j++)
                {
                        if(sdir.d[j][i] > 0)
                        {
                                in=i+d1[sdir.d[j][i]];
                                jn=j+d2[sdir.d[j][i]];
                                if(sdir.d[jn][in] <= 0)  // Neighbour is out
                                        darea(i,j);
                        }
                }
        //  At this point contrib area has been evaluated for all grid cells that drain to
        //      an edge or internal end.  Set direction to no data for the rest.
        for(i=1; i<ny-1; i++)
                for(j=1; j<nx-1; j++)
                {
                        if(larr.d[j][i] <= 0)
                        {   // reinitialize   10/5/02.  DGT modified below to include elevation check
                                if(felevg.d[j][i] <= mval) sdir.d[j][i] =MISSINGSHORT; //-32767
                                else sdir.d[j][i]=0;
                        }
                }
        if(writeflowfile == 1)
        {// Use pool array as placeholder to write directions
                for(i=0; i<ny; i++)
                        for(j=0; j<nx; j++)
                        {
                                if(sdir.d[j][i] >0) sapoolg.d[j][i] = sdir.d[j][i];
                                else sapoolg.d[j][i]= MISSINGSHORT; //-32767
                        }

                        sapoolg.nodata=MISSINGSHORT;

                 /* DRF       if (gridwrite(newflowfile,sapoolg,filetype)==0 )
                                err=TD_NO_ERROR;
                        else{
                                err=TD_FAILED_GRID_SAVE;
                                return err;
                        }  DRF */
        }
        return(err);
}

//************************************************************************

/* function to compute pool recursively and at the same time determine
   the minimum elevation of the edge. */
void pool(int i,int j)
{

int in,jn,k;
recursedepth=recursedepth+1;

if( sapoolg.d[j][i] <=0)   /* not already part of a pool  */
{
if(sdir.d[j][i]!= MISSINGSHORT)  /* check only dir since dir was initialized  */
 /* not on boundary  */
  {
sapoolg.d[j][i] = pooln;  /*  apool assigned pool number */
npool=npool+1;
if(npool >= pstack)
{
  if(pstack < (nx*ny))
  {
/*  Try enlarging   */
//printf("\n Enlarging pool stack\n");
pstack=(int) (pstack + nx*ny*.1);
if(pstack > nx*ny)
{
//  printf("\n Pool stack too large, exiting ...\n");
}
ipool = (short *)realloc(ipool, sizeof(short) * pstack);
jpool = (short *)realloc(jpool, sizeof(short) * pstack);
if(ipool == NULL || jpool == NULL)
{
//  printf("\n Cannot reallocate pool stack, exiting ...\n");
}
  }
  else
  {
//printf("\n Could not enlarge Pool stack\n");
  }
}
ipool[npool]=i;
jpool[npool]=j;
/*printf("%d %d Pool %d\n",i,j,pooln);  */
for(k=1; k<=8; k++)
{
   in=i+d1[k];
   jn=j+d2[k];
/* test if neighbor drains towards cell excluding boundaries */
   if((sdir.d[jn][in] >0 && (sdir.d[jn][in]-k==4||sdir.d[jn][in]-k==-4))
  || (sdir.d[jn][in] == 0 && felevg.d[jn][in] >= felevg.d[j][i]
                          && dontcross(k,i,j) ==0))
   /* so that adjacent flats get included */
 {
                                   pool(in,jn);
 }
}
  }
}
}


//*************************************************************************


void set(int i,int j,float *fact,float mval,short useflowfile)
{
        float slope,smax;
        int k,amax,in,jn,aneigh=-1;
        short dirnb;
        sdir.d[j][i]=0; //This necessary for repeat passes after level raised */
        smax=0.;
        amax=0;

        for(k=1; k<=8; k=k+2)   // examine adjacent cells first
        {
                in=i+d1[k];
                jn=j+d2[k];
                if(felevg.d[jn][in] <= mval) sdir.d[j][i]=MISSINGSHORT; //-32767);
                if(sdir.d[j][i] != MISSINGSHORT)
                {
                        slope=fact[k]*(felevg.d[j][i] - felevg.d[jn][in]);
                        if(useflowfile == 1)aneigh=larr.d[jn][in];
                        if(aneigh > amax && slope >= 0.)
                        {
                                amax=aneigh;
                                dirnb=sdir.d[jn][in];
                                if(dirnb > 0 && abs(dirnb-k) != 4)
                                {
                                sdir.d[j][i]=k;   // Dont set opposing pointers
                                }
                        }
                        else if(slope > smax) // && amax <=0)
                        {
                                smax=slope;
                                dirnb=sdir.d[jn][in];
                                if(dirnb > 0 && abs(dirnb-k) ==4)  //Dont set an opposing pointer.
//  Here we have a definitive positive slope but a neighbor that drains uphill
//  This occurs with imposed flow directions
//  Resolve by setting the direction of the burned in direction to no data
                                {
                                sdir.d[jn][in]=MISSINGSHORT; //-32767
                                }
                                sdir.d[j][i]=k;
                        }
                }
        }
        for(k=2; k<=8; k=k+2)   // examine diagonal cells
        {
                in=i+d1[k];
                jn=j+d2[k];
                if(felevg.d[jn][in] <= mval)sdir.d[j][i]=MISSINGSHORT; //-32767
                if(sdir.d[j][i] != MISSINGSHORT) //-32767
                {
                        slope=fact[k]* (felevg.d[j][i]-felevg.d[jn][in]);
                        if(slope > smax && dontcross(k,i,j) ==0)  // Dont cross pointers
                        {
                                smax=slope;
                                dirnb=sdir.d[jn][in];
                                if(dirnb > 0 && abs(dirnb-k) ==4)  //Dont set an opposing pointer.
//  Here we have a definitive positive slope but a neighbor that drains uphill
//  This occurs with imposed flow directions
//  Resolve by setting the direction of the burned in direction to no data
                                {
                                sdir.d[jn][in] = MISSINGSHORT;  //-32767
                                }
                                sdir.d[j][i]=k;
                        }
                }
        }
}

//************************************************************************

int vdn(int n)
{



  int ip,k,imin;
  float ed;
  nis=n;
  do
  {
  n=nis;
  nis=0;
  for(ip=1; ip <=n; ip++)dn[ip]=0;
  for(k=1; k<=7; k=k+2)
                for(ip=1; ip<=n; ip++)
                {
                   ed= felevg.d[js[ip]][is[ip]] - felevg.d[js[ip]+d2[k]][is[ip]+d1[k]];
                   if(ed >= 0. && sdir.d[js[ip]+d2[k]][is[ip]+d1[k]] != 0
                                   && dn[ip] == 0)
                 dn[ip]=k;
                }
  for(k=2; k<=8; k=k+2)
for(ip=1; ip<=n; ip++)
{
   ed= felevg.d[js[ip]][is[ip]] - felevg.d[js[ip]+d2[k]][is[ip]+d1[k]];
   if(ed >= 0. && sdir.d[js[ip]+d2[k]][is[ip]+d1[k]] != 0
                   && dn[ip] == 0 && dontcross(k,is[ip],js[ip])==0)
 dn[ip]=k;
}
  imin=1;  /*  location of point on stack with lowest elevation  */
  for(ip=1; ip <= n; ip++)
  {
 if(dn[ip] > 0) sdir.d[js[ip]][is[ip]] = dn[ip];
 else
 {
nis++;
is[nis]=is[ip];
js[nis]=js[ip];
if( felevg.d[js[nis]][is[nis]] < felevg.d[js[imin]][is[imin]] )imin=nis;
 }
  }
 
  }while(nis < n);
  return(imin);

}  

//************************************************************************

float max2(float e1,float e2)
{
  float em;
  em=e1;
  if(e2 > em)em=e2;
  return(em);
}

int addstack(int i, int j)
{
        /*  Routine to add entry to is, js stack, enlarging if necessary   */
        nis=nis+1;
        if(nis >= istack )
  {
/*  Try enlarging   */
 istack=(int) (istack + nx*ny*.1);
 if(istack > nx*ny)
 {
//   printf("\n is,js stack too large, exiting ...\n");
   return(4);
 }
// printf("\n Enlarging is,js stack\n");
 is = (short *)realloc(is, sizeof(short) * istack);
 js = (short *)realloc(js, sizeof(short) * istack);
 dn = (short *)realloc(dn, sizeof(short) * istack);
 if(is == NULL || js == NULL || dn == NULL)
{
// printf("\n Could not enlarge stack\n");
 return(5);
}
  }
        is[nis]=i;
        js[nis]=j;
        return(0);
}



bool notfdr(short useflowfile, int i, int j)
{
        if(useflowfile == 0)
                return(true);
        else if (larr.d[j][i] <= 0)return(true);
        return(false);
}

int setdf(float mval, float fdmval, short useflowfile,char * newflowfile, int writeflowfile)
{
        int i,j,k,n,nflat,ni,ip,imin,jn,in,np1,nt;
        err = TD_NO_ERROR;
        float fact[9],per=1.;
        /*  initialize internal pointers */
        for(i=i2+1; i< n2-1; i++)for(j=i1+1; j<n1-1; j++)
        {
                if(felevg.d[j][i] <= mval){
                    sdir.d[j][i]=MISSINGSHORT; //-32767
                } else { sdir.d[j][i]=0;}
        }
       /*  Direction factors  */
        for(k=1; k<= 8; k++)
                fact[k]=(float)(1./sqrt(d1[k]*dy*d1[k]*dy+d2[k]*d2[k]*dx*dx));

        /*  Set positive slope directions - store unresolved on stack */
        nis=0;
        for(i=i2+1; i< n2-1; i++)for(j=i1+1; j<n1-1; j++)
        {
                if(felevg.d[j][i] > mval && sdir.d[j][i] == 0)set(i,j,fact,mval,useflowfile);
                /*  Put unresolved pixels on stack  */
                if(sdir.d[j][i] == 0)
                {
                        err=addstack(i,j);
                }
        }
        nflat=nis;
        /* routine to drain flats to neighbors  */
        imin=vdn(nflat);
        n=nis;
        REprintf("Number of pits to resolve: %d\n%f\n",n,useflowfile);
        return 1; 
        np1=n;
        nt=np1*(1-per/100);
        /*  initialize apool to zero  */
        for(i=i2; i< n2; i++)for(j=i1; j<n1; j++)
                sapoolg.d[j][i]=0;
        /*  store unresolved stack location in apool for easy deletion  */
        while(nis > 0)   /*  While AA */
        {
                i=is[imin];
                j=js[imin];
                pooln=1;
                npool=0;
                nf = 0;  /*  reset flag to that new min elev is found */
                recursedepth=0;
                pool(i,j);  /*  Recursive call on unresolved point with lowest elevation */
                /*  Find the pour point of the pool  */
                for(ip=1; ip<=npool; ip++)
                {
                        i=ipool[ip];
                        j=jpool[ip];
                        for(k=1; k <=8; k++)
                        {
                                jn=j+d2[k];
                                in=i+d1[k];
                                if(sapoolg.d[jn][in] != pooln && dontcross(k, i, j)==0)  /*  neighbor not in pool  */
                                {
                                        et=max2(felevg.d[j][i],felevg.d[jn][in]);
                                        if(nf == 0)  /*  this is the first edge found  */
                                        {
                                                emin=et;
                                                nf=1;
                                        }
                                       else
                                        {
                                                if(emin > et)
                                                {
                                                        emin = et;
                                                }
                                        }
                                }
                        }
                }
                /*  Fill the pool  */
                for(k=1; k<=npool; k++)
                {
                        i=ipool[k];
                        j=jpool[k];
                        if(felevg.d[j][i] <= emin)
                        {
                                if(sdir.d[j][i] > 0 && notfdr(useflowfile,i,j))   /*  Can be in pool, but not flat   */
                                {   // notfdr is used to avoid changing burned in directions
                                sdir.d[j][i]=0;
                                        /*  Add to stack  */
                                        err=addstack(i,j);
                                }
                                for(ip=1; ip <=8; ip++)
                                {
                                        jn=j+d2[ip];
                                        in=i+d1[ip];
                                        if(felevg.d[jn][in] > felevg.d[j][i] && sdir.d[jn][in] > 0
                                                && notfdr(useflowfile,in,jn))  // 7/29/04  DGT Fixed from notfdr(useflowfile,i,j)
                                        /*Only zero direction of neighbors that are higher - because
                                        lower  or equal may be a pour point in a pit that must not be disrupted  */
                                        {
                                                sdir.d[jn][in]=0;
                                                err=addstack(in,jn);
                                        }
                                }
                                felevg.d[j][i]=emin;
                        }
                        sapoolg.d[j][i]=0;  /*  Reinitialize for next time round  */
                }
                /* reset unresolved stack  */
                ni=0;
                for(ip=1; ip <= nis; ip++)
                {
                        set(is[ip],js[ip],fact,mval,useflowfile);
                        if(sdir.d[js[ip]][is[ip]] == 0)  /* still on stack */
                        {
                                ni++;
                                is[ni]=is[ip];
                                js[ni]=js[ip];
                        }
                }
               n=nis;
                imin=vdn(ni);
                if(nis < nt){
                        REprintf("Percentage done %f\n",per);
                        per=per+1;
                        nt=np1*(1-per/100);
                }
        }  /*  end of while AA  */
        return(err);
}

int flood(double *input, double *outputfel, int *nrow, int *ncol, double *cellsize, double *degree){

  nx = *nrow;
  ny = *ncol;
  dx = *cellsize;
  dy = *cellsize;
  csize = dx;
  felevg.nodata = -9999;
  felevg.d = (double **) R_alloc(nx,sizeof(double *));
  for(j=0; j<nx; j++){
    felevg.d[j] = (double *) R_alloc(ny,sizeof(double));
  }

  for(i=0; i< ny; i++){
    for(j=0; j< nx; j++){
      felevg.d[j][i] = input[j+(nx)*i];
    }
  }
  bndbox[0]=xllcenter-(dx/2);
  bndbox[1]=yllcenter-(dy/2);
  bndbox[2]=bndbox[0] + dx * (nx);
  bndbox[3]=bndbox[1] + dy * (ny);

  for(i=0;i<4;i++) felevg.head.bndbox[i]=bndbox[i];

  sapoolg.head.dx=dx;
  sapoolg.head.dy=dy;
  sapoolg.head.nx=nx;
  sapoolg.head.ny=ny;
  for(i=0;i<4;i++) sapoolg.head.bndbox[i]= felevg.head.bndbox[i];
  sapoolg.nodata=-1;
  
  sapoolg.d = (short **) R_alloc(nx,sizeof(short *));
  for(j=0; j<nx; j++){
    sapoolg.d[j] = (short *) R_alloc(ny,sizeof(short));
  }
  for(i=0; i<ny; i++) {
    for(j=0; j<nx; j++) {
      sapoolg.d[j][i]= sapoolg.nodata;
    }
  }

  sdir.head.dx=dx;
  sdir.head.dy=dy;
  sdir.head.nx=nx;
  sdir.head.ny=ny;
  for(i=0;i<4;i++)sdir.head.bndbox[i]=felevg.head.bndbox[i];
  sdir.nodata=MISSINGSHORT;

  sdir.d = (short **) R_alloc(nx,sizeof(short *));
  for(j=0; j<nx; j++){
    sdir.d[j] = (short *) R_alloc(ny,sizeof(short));
  }
  for(i=0; i<ny; i++) {
    for(j=0; j<nx; j++) {
      sdir.d[j][i]= sdir.nodata;
    }
  }


        istack = (int) (nx * ny * 0.1);
        pstack=istack;
        dn = (short *)malloc(sizeof(short) * istack);
        is = (short *)malloc(sizeof(short) * istack);
        js = (short *)malloc(sizeof(short) * istack);
        ipool = (short *)malloc(sizeof(short) * pstack);
        jpool = (short *)malloc(sizeof(short) * pstack);
        if(dn == NULL || is == NULL || js == NULL || ipool == NULL || jpool == NULL)
        {
                return(11);
        }
        i1=0; i2=0; n1=nx; n2=ny;  /*  full grid  */
        int writeflowfile=1;
       mval=felevg.nodata;
        i=500;j=500;
        REprintf("neighbors: %f %f %f \n",felevg.d[j-1][i+1],felevg.d[j][i+1],felevg.d[j+1][i+1]);
        REprintf("neighbors: %f %f %f \n",felevg.d[j-1][i],felevg.d[j][i],felevg.d[j+1][i]);
        REprintf("neighbors: %f %f %f \n",felevg.d[j-1][i-1],felevg.d[j][i-1],felevg.d[j+1][i-1]);

       REprintf("Hi Dan before setdf!\n %d\n%f\n%f",nx,dy,csize);
        err=setdf(mval, fdmval, useflowfile, newflowfile, writeflowfile);

 for(i=0; i < ny; i++){
    for(j=0; j < nx; j++){
      outputfel[j + (nx * i)] = double(felevg.d[j][i]);
    }
  }

   REprintf("Hi Dan!\n %d\n%f\n%f",nx,dy,csize);
   return(0);  /*  ALL OK return from flood  */

}
//************************************************************************

void sloped8(float nodata)
{

        int k,i,j,in,jn;
        float fact[9],ed;
        /*  Direction factors  */
        for(k=1; k<= 8; k++)
                fact[k]= (float) (1./sqrt(d1[k]*dy*d1[k]*dy+d2[k]*d2[k]*dx*dx));

        for(i=i2; i< n2; i++)for(j=i1; j<n1; j++)
        {
                if(sdir.d[j][i] > 0)
                {
                        jn=j+d2[sdir.d[j][i]];
                        in=i+d1[sdir.d[j][i]];
                        ed = felevg.d[j][i] - felevg.d[jn][in];
                        float tempvalue = ed*fact[sdir.d[j][i]];
                        fslopeg.d[j][i]= tempvalue;
                }
                else
                        fslopeg.d[j][i]= nodata;
        }

}
//************************************************************************


void set2(int i,int j,float *fact,float *elev1, float *elev2, int iter,
                  int **spos, short *s, short useflowfile)
{
/*  This function sets directions based upon secondary elevations for
  assignment of flow directions across flats according to Garbrecht and Martz
  scheme.  There are two possibilities:
        A.  The neighbor is outside the flat set
        B.  The neighbor is in the flat set.
        In the case of A the elevation of the neighbor is set to 0 for the purposes
        of computing slope.  Since the incremental elevations are all positive there is
        always a downwards slope to such neighbors, and if the previous elevation
        increment had 0 slope then a flow direction can be assigned.*/

        float slope,slope2,smax,ed;
        int k,spn,sp,kflat=0;
        short in,jn;
        smax=0.;
        sp=spos[j][i];
        for(k=1; k<=8; k++)
        {
                jn=j+d2[k];
                in=i+d1[k];
                spn=spos[jn][in];
                if(iter <= 1)
                {
                        ed = felevg.d[j][i] - felevg.d[jn][in];
                }
                else
                {
                        ed = elev1[sp] - elev1[spn];
                }
                slope=fact[k]* ed;
                if(spn < 0 || s[spn] < 0)
                {
                        /*  The neighbor is outside the flat set.  */
                        ed=0.;
                }
                else
                {
                        ed=elev2[spn];
                }
                slope2 =fact[k]*(elev2[sp]-ed);
                        /*  Only if latest iteration slope is
 //                     positive and previous iteration slope flat  */

//9/13/03  add check for opposing directions to resove a bug reported by Anna Katarina Mahlau

                int adrdirnn=0;
                if(sdir.d[jn][in] > 0 && sdir.d[jn][in]<=8)adrdirnn=abs(sdir.d[jn][in]-k);  // only calculate if sdir has been really set

//              if(adrdirnn == 4)
//                      in=in;
                if(slope2 > smax && slope >= 0. && (felevg.d[j][i] - felevg.d[jn][in])>=0.
                        && adrdirnn!=4)
                /*  Only if latest iteration slope is
                        positive and previous iteration slope flat and real elevation difference not uphill
                        The elevation check added 3/1/03 to resolve a bug reported by Anna Katarina Mahlau <au@ivu-umwelt.de> */
                {
                        smax=slope2;
                        sdir.d[j][i]=k;
                }
        }  /*  End of for  */

}

//************************************************************************

//************************************************************************

void incrise(int n, float *elev1, short *s2,int **spos, int iter, int *sloc)
{

        /*  This routine implements stage 2 drainage away from higher ground
        dn is used to flag pixels still being incremented */
        int done=0,ip,k,ninc,nincold,spn;
        float ed;
        short i,j,in,jn;
        nincold=0;
        while(done < 1)
        {
                done=1;
                ninc=0;
                for(ip=0; ip<n; ip++)
                {
                        for(k=1; k<=8; k++)
                        {
                                j=js[sloc[ip]];
                                i=is[sloc[ip]];
                                jn=j+d2[k];
                                in=i+d1[k];
                                spn=spos[jn][in];

                                if(iter <= 1)
                                {
                                        ed = felevg.d[j][i] - felevg.d[jn][in];
                                }
                                else
                                {
                                        ed= elev1[sloc[ip]] - elev1[spn];
                                }
                                if(ed < 0.)dn[sloc[ip]]=1;
                                if(spn >=0)
                                        if(s2[spn] > 0)dn[sloc[ip]] = 1;
                        }
                }
                for(ip=0; ip<n; ip++)
                {
                        s2[sloc[ip]]=s2[sloc[ip]]+dn[sloc[ip]];
                        ninc=ninc+ (dn[sloc[ip]]>0 ? 1: 0);   // DGT 9/13/03 changed to because now dn is not 0 or 1
                        if(dn[sloc[ip]] == 0)done=0;  /*  if still some not being incremented continue
                                                                        looping  */
                }
//              printf("incrise %d %d\n",ninc,n);
                if(ninc == nincold)done=1;   /*  If there are no new cells incremented
                                                                         stop - this is the case when a flat has
                                                                         no higher ground around it.  */
                nincold=ninc;
        }

}

//************************************************************************

void incfall(int n, float *elev1, short *s1,int **spos,
                         int iter, int *sloc)
{
        /* This routine implements drainage towards lower areas - stage 1 */
        int done=0,donothing,k,ip,ninc,nincold,spn;
        short st=1,i,j,in,jn;
        float ed;
        nincold= -1;
        while(done < 1)
        {
                done=1;
                ninc=0;
                for(ip=0; ip<n; ip++)
                {
/*                      if      adjacent to same level or lower that drains or
                                adjacent to pixel with s1 < st and dir not set
                                do nothing  */
                        donothing=0;
                        j=js[sloc[ip]];
                        i=is[sloc[ip]];
                        for(k=1; k<=8; k++)
                        {
                //7/28/04  DGT Dont cross check added
                          if(dontcross(k,i,j) ==0){   // Only examine neighbors that do not cross existing flow directions, e.g. due to burn in
                                jn=j+d2[k];
                                in=i+d1[k];
                                spn=spos[jn][in];
                                if(iter <= 1)
                                {
                                        ed= felevg.d[j][i] - felevg.d[jn][in];
                                }
                                else
                                {
                                        ed = elev1[sloc[ip]] - elev1[spn];
                                }
                                //9/13/03  DGT changed conditional below to >=1 <=8 rather than !=0 to also capture 9 case
                                //decided to revert back to old code.  This did not succeed in flagging the entire pit
                                if(ed >= 0. && sdir.d[jn][in] != 0)
                                {
                                        donothing = 1;  /* If neighbor drains */
                                }
                                if(spn >= 0) /* if neighbor is in flat   */
                                        if(s1[spn] >= 0 && s1[spn] < st   /*  If neighbor is not being  */
                                                && (sdir.d[jn][in]  == 0 )){
                                                        donothing = 1;   /*  Incremented  */
                                        }
                          }
                        }   //9/13/03  DGT added ==9 in the above for consistency
                        if(donothing == 0)
                        {
                                s1[sloc[ip]]++;
                                ninc++;
                                done=0;
                        }
                }   /*  End of loop over all flats  */
                st=st+1;
//              printf("Incfall %d %d \n",ninc,n);
                if(ninc == nincold)
                {
                        done = 1;
//                      printf("There are pits remaining, direction will not be set\n");
/*  Set the direction of these pits to 9 to flag them   */
                        for(ip=0; ip<n; ip++)  /*  loop 2 over all flats  */
                        {
/*                      if      adjacent to same level or lower that drains or
                                adjacent to pixel with s1 < st and dir not set
                                do nothing  */
                                donothing=0;
                                j=js[sloc[ip]];
                                i=is[sloc[ip]];
                                for(k=1; k<=8; k++)
                                {
                                        jn=j+d2[k];
                                        in=i+d1[k];
                                        spn=spos[jn][in];
                                        if(iter <= 1)
                                        {
                                                ed= felevg.d[j][i] - felevg.d[jn][in];
                                        }
                                        else
                                        {
                                                ed= elev1[sloc[ip]]- elev1[spn];
                                        }
                                //9/13/03  DGT changed conditional below to >=1 <=8 rather than !=0 to also capture 9 case
                                //decided to revert back to old code.  This did not succeed in flagging the entire pit
                                        if(ed >= 0. && sdir.d[jn][in] != 0)donothing = 1;  /* If neighbor drains */
                                        if(spn >= 0) /* if neighbor is in flat   */
                                        if(s1[spn] >= 0 && s1[spn] < st   /*  If neighbor is not being  */
                                                && (sdir.d[jn][in]  == 0 ))donothing = 1;   /*  Incremented  */
                                }  //9/13/03  DGT added ==9 in the above for consistency
                                if(donothing == 0)
                                {
                                   sdir.d[j][i] = 9;

                                }
                        }   /*  End of loop 2 over all flats  */
                }
                nincold=ninc;
        }  /*  End of while done loop  */
}

//************************************************************************

//************************************************************************

int flatrout(int n,int *sloc, short *s, int **spos,int iter,float *elev1,
                          float *elev2, float *fact, int ns, short useflowfile)
{
        int ip,nu, *sloc2,ipp,err=TD_NO_ERROR;
        float *elev3;

        incfall(n,elev1,s,spos,iter,sloc);
        for(ip=0; ip < n; ip++)
        {
                elev2[sloc[ip]]=(float)(s[sloc[ip]]);
                s[sloc[ip]]=0;   /*  Initialize for pass 2  */
        }

        incrise(n,elev1,s,spos,iter,sloc);
        for(ip=0; ip < n; ip++)
        {
                elev2[sloc[ip]] += (float)(s[sloc[ip]]);
        }

        nu=0;
        for(ip=0; ip < n; ip++)
        {
                set2(is[sloc[ip]],js[sloc[ip]],fact,elev1,elev2,iter,spos,s,useflowfile);
                if( sdir.d[js[sloc[ip]]][is[sloc[ip]]] == 0)nu++;
        }
        if(nu >= n)return(TD_NO_ERROR);  //  Here the recursion is not converging so bail
        //  DGT 7/31/04 No error reported because this usually occurs at pits and it is OK to have pits be no data
        if(nu > 0)
        {
                /*  Iterate Recursively   */
                /*  Now resolve flats following the Procedure of Garbrecht and Martz, Journal
                of Hydrology, 1997.  */
                iter=iter+1;
                //        printf("Resolving %d Flats, Iteration: %d \n",nu,iter);
                sloc2 = (int *)malloc(sizeof(int) * nu);
                elev3 = (float *)malloc(sizeof(float) *ns);

                if(sloc2 == NULL || elev3 == NULL)
                {
                        // printf("Unable to allocate at iteration %d\n",iter);
                }
                /*  Initialize elev3  */
                for(ip=0; ip < ns; ip++)elev3[ip]=0.;
                /*  Put unresolved pixels on new stacks - keeping in same positions  */
                ipp=0;
                for(ip=0; ip<n; ip++)
                {
                        if(sdir.d[js[sloc[ip]]][is[sloc[ip]]] == 0)
                        {
                                sloc2[ipp]=sloc[ip];
                                /*   Initialize the stage 1 array for flat routing   */
                                s[sloc[ip]] = 1;
                                ipp++;

                        }
                        else
                        {
                        s[sloc[ip]] = -1;  /*  Used to designate out of remaining flat on
                        higher iterations   */
                        }
                        dn[sloc[ip]]=0;  /*  Reinitialize for next time round.  */
                }
                err=flatrout(nu,sloc2,s,spos,iter,elev2,elev3,fact,ns,useflowfile);
                free(sloc2);
                free(elev3);
                //  printf("Done iteration %d\nFlats resolved %d\n",iter,n);
        } /*  end if nu > 0  */
   return(err);
}   /*  End flatrout  */


//************************************************************************



int setdfnoflood(float mval, float fdmval, short useflowfile)
/*  This version is stripped of pit filling  */
{
        int i,j,k,ip, n, iter, err=TD_NO_ERROR;
        float fact[9];
        short *s;  /*  variables for flat draining   */
        int **spos, *sloc;
        float *elev2;

        /*  initialize internal pointers to 0 except where elevation is no data*/
        for(i=i2+1; i< n2-1; i++)for(j=i1+1; j<n1-1; j++)
        {
                if(felevg.d[j][i] <= mval)
                {
                        sdir.d[j][i]=MISSINGSHORT ; // -32767

                }
                else sdir.d[j][i]=0;
        }
        /*  Direction factors  */
        for(k=1; k<= 8; k++)
                fact[k]= (float) (1./sqrt(d1[k]*dy*d1[k]*dy+d2[k]*d2[k]*dx*dx));

//  Set stream overlay directions
        if( useflowfile == 1 )
for(i=i2+1; i< n2-1; i++)
          for(j=i1+1; j<n1-1; j++)
          {
                  {
                          if(sapoolg.d[j][i] > fdmval )
                          {
                                  sdir.d[j][i] = sapoolg.d[j][i];
                                  larr.d[j][i]=0;
                          }
                  }
          }

//   Compute contrib area using overlayed directions for direction setting

        ccheck=0;   // dont worry about edge contamination
        useww=0;// dont worry about weights
for(i=i2+1; i< n2-1; i++)
          for(j=i1+1; j<n1-1; j++)
          {
                  //This allows for a stream overlay
                  if( sdir.d[j][i] > 0) darea(i,j);
          }

/*  Set positive slope directions   */
        n=0;
        for(i=i2+1; i< n2-1; i++)
          for(j=i1+1; j<n1-1; j++)
          {
                  if( sdir.d[j][i] == 0 )
                  {
                          if(felevg.d[j][i]  > mval)
                          {
                                  set(i,j,fact,mval,useflowfile);
                                  if(sdir.d[j][i] == 0)
                                          n++;
                          }
                  }
          }

  if(n > 0)
  {
/*  Now resolve flats following the Procedure of Garbrecht and Martz, Journal
   of Hydrology, 1997.  */

/*  Memory is utilized as follows
is, js, dn, s and elev2 are unidimensional arrays storing information for flats.
sloc is a indirect addressing array for accessing these - used during
recursive iteration
spos is a grid of pointers for accessing these to facilitate finding neighbors

The routine flatrout is recursive and at each recursion allocates a new sloc for
addressing these arrays and a new elev for keeping track of the elevations
for that recursion level.
  */
          iter=1;
//        printf("Resolving %d Flats, Iteration: %d \n",n,iter);
  spos = (int **) R_alloc(nx,sizeof(int *));
  for(j=0; j<nx; j++){
    spos[j] = (int *) R_alloc(ny,sizeof(int));
  }
//  spos = (int **) matalloc(nx, ny, RPINTDTYPE);
  dn = (short *)malloc(sizeof(short) * n);
  is = (short *)malloc(sizeof(short) * n);
  js = (short *)malloc(sizeof(short) * n);
  s = (short *)malloc(sizeof(short) * n);
          sloc = (int *)malloc(sizeof(int) * n);
  elev2 = (float *)malloc(sizeof(float) *n);

  if(dn == NULL || is == NULL || js == NULL || s == NULL ||
          spos == NULL || elev2 == NULL || sloc == NULL)
  {
// printf("Unable to allocate at iteration %d\n",iter);
  }
/*  Put unresolved pixels on stack  */
   ip=0;
   for(i=i2; i< n2; i++)
          for(j=i1; j<n1; j++)
  {
spos[j][i]=-1;   /*  Initialize stack position  */
if(sdir.d[j][i] == 0)
{
  is[ip]=i;
  js[ip]=j;
          dn[ip]=0;
          sloc[ip]=ip;
          /*   Initialize the stage 1 array for flat routing   */
          s[ip] = 1;
          spos[j][i]=ip;  /*  pointer for back tracking  */
          ip++;
//        if(ip > n)printf("PROBLEM - Stack logic\n");
}
  }
  err=flatrout(n,sloc,s,spos,iter,elev2,elev2,fact,n,useflowfile);
/*  The direction 9 was used to flag pits.  Set these to 0  */
  for(i=i2; i< n2; i++)
          for(j=i1; j<n1; j++)
          {
                  if(sdir.d[j][i] == 9) sdir.d[j][i]=sdir.nodata;
                  if(sdir.d[j][i] == 0) sdir.d[j][i]=sdir.nodata;
          }
  free(elev2);
  free(dn);
  free(is);
  free(js);
  free(s);
  free(sloc);
//  printf("Done iteration %d\nFlats resolved %d\n",iter,n);
  } /*  End if n > 0  */
   return(err);   /*  OK exit from setdir  */

}  /*  End setdfnoflood  */

//************************************************************************


int setdird8(double *input, double *outputsdir, double *outputslope,int *nrow, int *ncol, double *cellsize, double *degree){

  nx = *nrow;
  ny = *ncol;
  dx = *cellsize;
  dy = *cellsize;
  csize = dx;

  /* define directions */
  d1[1]=0; d1[2]= -1; d1[3]= -1; d1[4]= -1; d1[5]=0; d1[6]=1; d1[7]=1; d1[8]=1;
  d2[1]=1; d2[2]=1; d2[3]=0; d2[4]= -1; d2[5]= -1; d2[6]= -1; d2[7]=0; d2[8]=1;

  felevg.nodata = -9999;
  mval=felevg.nodata;
  felevg.d = (double **) R_alloc(nx,sizeof(double *));
  for(j=0; j<nx; j++){
    felevg.d[j] = (double *) R_alloc(ny,sizeof(double));
  }

  for(i=0; i< ny; i++){
    for(j=0; j< nx; j++){
      felevg.d[j][i] = input[j+(nx)*i];
    }
  }
  bndbox[0]=xllcenter-(dx/2);
  bndbox[1]=yllcenter-(dy/2);
  bndbox[2]=bndbox[0] + dx * (nx);
  bndbox[3]=bndbox[1] + dy * (ny);

  for(i=0;i<4;i++) felevg.head.bndbox[i]=bndbox[i];

  sdir.head.dx=dx;
  sdir.head.dy=dy;
  sdir.head.nx=nx;
  sdir.head.ny=ny;
  for(i=0;i<4;i++)sdir.head.bndbox[i]=felevg.head.bndbox[i];
  sdir.nodata=MISSINGSHORT;

  sdir.d = (short **) R_alloc(nx,sizeof(short *));
  for(j=0; j<nx; j++){
    sdir.d[j] = (short *) R_alloc(ny,sizeof(short));
  }
  for(i=0; i<ny; i++) {
    for(j=0; j<nx; j++) {
      sdir.d[j][i]= sdir.nodata;
    }
  }


        i1=0; i2=0; n1=nx; n2=ny;  /*  full grid  */
        int writeflowfile=1;
        i=500;j=500;
        REprintf("neighbors: %f %f %f \n",felevg.d[j-1][i+1],felevg.d[j][i+1],felevg.d[j+1][i+1]);
        REprintf("neighbors: %f %f %f \n",felevg.d[j-1][i],felevg.d[j][i],felevg.d[j+1][i]);
        REprintf("neighbors: %f %f %f \n",felevg.d[j-1][i-1],felevg.d[j][i-1],felevg.d[j+1][i-1]);

       REprintf("Hi Dan before setdf!\n %d\n%f\n%f",nx,dy,csize);

        err = setdfnoflood(mval, fdmval, useflowfile);

 for(i=0; i < ny; i++){
    for(j=0; j < nx; j++){
      outputsdir[j + (nx * i)] = double(sdir.d[j][i]);
    }
  }

 //       if (gridwrite(pfile,sdir,filetype)==0)

        fslopeg.head.dx=dx;
        fslopeg.head.dy=dy;
        fslopeg.head.nx=nx;
        fslopeg.head.ny=ny;
        for(i=0;i<4;i++) fslopeg.head.bndbox[i]=felevg.head.bndbox[i];

   fslopeg.d = (double **) R_alloc(nx,sizeof(double *));
   for(j=0; j<nx; j++){
     fslopeg.d[j] = (double *) R_alloc(ny,sizeof(double));
   }

   for(i=0; i< ny; i++){
     for(j=0; j< nx; j++){
       fslopeg.d[j][i] = -1.0;
     }
   }
  
   sloped8(fslopeg.nodata);

   for(i=0; i < ny; i++){
      for(j=0; j < nx; j++){
        outputslope[j + (nx * i)] = double(fslopeg.d[j][i]);
      }
   }

   REprintf("Hi Dan!\n %d\n%f\n%f",nx,dy,csize);
   return(0);  /*  ALL OK return from flood  */

}

}


 
 
