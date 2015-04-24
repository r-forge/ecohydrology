// flood.h
#define RPSHRDTYPE  1
#define RPINTDTYPE  2
#define RPFLTDTYPE  3
#define FN_FLOOD	1
#define TD_NO_ERROR	0
/* byte sizes corresponding to data types above */
#define RPSHRSIZE (sizeof(short))
#define RPINTSIZE (sizeof(int))
#define RPFLTSIZE (sizeof(double))

#define MISSINGINT      -2147483647     /* CELLMIN - 1 */
#define MISSINGSHORT -32767


void **matalloc(int nx,int ny,int datatype);

class X {
  public: X (); ~X ();
};
class Y {
  public: Y (); ~Y ();
};

class ghead
{
        public:
         int nx;
         int ny;
         double dx;
         double dy;
         double bndbox[4];

};

class fgrid
{
        public:
         double **d;
         double nodata;
         ghead head;

};

class igrid
{
        public:
         int **d;
         int nodata;
         ghead head;


};

class sgrid
{
        public:
         short **d;
         short nodata;
         ghead head;
};

int allocategrid(fgrid *grid, ghead head, double nodatavalue);
int allocategrid(igrid *grid, ghead head, int nodatavalue);
int allocategrid(sgrid *grid, ghead head, short nodatavalue);
