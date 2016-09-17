setcolor(15);
circle(14,25,14);

putpixel(21,12,15);
putpixel(22,11,15);
putpixel(23,11,15);

int i=0,j=0;
for(i=24,j=12;j<19;j++,i++)
    {
        putpixel(i,j,15);
    }

putpixel(30,19,15);

putpixel(29,20,15);

setfillstyle(1,0);
floodfill(15,20,15);

putpixel(21,28,15);
for(i=21,j=29;j<35;i--,j++)
    putpixel(i,j,15);

putpixel(15,34,15);

for(i=29;i<33;i++)
    putpixel(22,i,15);

for(i=16;i<20;i++)
    putpixel(i,35,15);

putpixel(21,33,15);
putpixel(20,34,15);

setfillstyle(1,15);
floodfill(20,32,15);

//---------фитиль----
putpixel(28,15,BROWN);
putpixel(29,15,BROWN);
putpixel(30,15,BROWN);

putpixel(30,14,BROWN);
putpixel(31,14,BROWN);
putpixel(32,14,BROWN);

putpixel(32,13,14);
putpixel(33,13,14);
putpixel(34,13,14);

putpixel(32,12,14);
putpixel(32,11,14);

for(i=7;i<11;i++)
    putpixel(31,i,14);

putpixel(30,7,14);
putpixel(30,6,14);

putpixel(32,7,14);

putpixel(33,8,14);

putpixel(34,7,14);
putpixel(35,6,14);
putpixel(36,5,14);

putpixel(36,6,14);
putpixel(36,7,14);
putpixel(37,7,14);

putpixel(38,7,14);
putpixel(38,6,14);
putpixel(39,5,14);

for(i=8;i<12;i++)
    putpixel(37,i,14);

putpixel(38,10,14);

putpixel(36,11,14);
putpixel(36,12,14);
putpixel(35,12,14);

setfillstyle(1,RED);
floodfill(34,10,14);

size=imagesize(0,0,39,39);
ptr=malloc(size);
getimage(0,0,39,39,ptr);

putimage(0,0,ptr,XOR_PUT);
