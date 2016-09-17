for(int i=17;i<26;i++)
    putpixel(i,1,c);

for(i=14;i<17;i++)
    {
        putpixel(i,2,c);
        putpixel(i+12,2,c);
    }

putpixel(12,3,c);
putpixel(13,3,c);
putpixel(29,3,c);
putpixel(30,3,c);

putpixel(11,4,c);
putpixel(31,4,c);

putpixel(10,5,c);
putpixel(32,5,c);

putpixel(9,6,c);
putpixel(33,6,c);

putpixel(8,7,c);
putpixel(34,7,c);

putpixel(7,8,c);
putpixel(35,8,c);

putpixel(7,9,c);
putpixel(35,9,c);

for(i=10;i<35;i++)
    {
        putpixel(6,i,c);
        putpixel(36,i,c);
    }

putpixel(7,35,c);
putpixel(14,35,c);
putpixel(21,35,c);
putpixel(28,35,c);
putpixel(35,35,c);

putpixel(7,36,c);
putpixel(13,36,c);
putpixel(15,36,c);
putpixel(20,36,c);
putpixel(22,36,c);
putpixel(27,36,c);
putpixel(29,36,c);
putpixel(35,36,c);

putpixel(8,37,c);
putpixel(13,37,c);
putpixel(15,37,c);
putpixel(20,37,c);
putpixel(22,37,c);
putpixel(27,37,c);
putpixel(29,37,c);
putpixel(34,37,c);

putpixel(8,38,c);
putpixel(12,38,c);
putpixel(16,38,c);
putpixel(19,38,c);
putpixel(23,38,c);
putpixel(26,38,c);
putpixel(30,38,c);
putpixel(34,38,c);

putpixel(9,39,c);
putpixel(12,39,c);
putpixel(16,39,c);
putpixel(19,39,c);
putpixel(23,39,c);
putpixel(26,39,c);
putpixel(30,39,c);
putpixel(33,39,c);

for(i=10;i<12;i++)
    {
        putpixel(i,40,c);
        putpixel(i+7,40,c);
        putpixel(i+14,40,c);
        putpixel(i+21,40,c);
    }
setfillstyle(1,c);
floodfill(18,2,c);

//-----------брови и глаза----------

for(i=9;i<15;i++)
    {
        putpixel(i,12,0);
        putpixel(i+19,12,0);
    }
for(i=8;i<16;i++)
    {
        putpixel(i,13,0);
        putpixel(i+18,13,0);
    }
for(i=14;i<18;i++)
    {
        putpixel(i,14,0);
        putpixel(i+11,14,0);
    }
for(i=16;i<21;i++)
    {
        putpixel(i,15,0);
        putpixel(i+6,15,0);
    }
for(i=13;i<16;i++)
    {
        putpixel(i,16,0);
        putpixel(i+4,16,0);
        putpixel(i+10,16,0);
        putpixel(i+14,16,0);
    }
putpixel(10,14,0);
putpixel(32,14,0);
for(i=15;i<20;i++)
    {
        putpixel(9,i,0);
        putpixel(33,i,0);
    }
putpixel(10,20,0);
putpixel(32,20,0);

putpixel(11,21,0);
putpixel(31,21,0);
for(i=12;i<16;i++)
    {
        putpixel(i,22,0);
        putpixel(i+15,22,0);
    }

putpixel(16,21,0);
putpixel(26,21,0);

putpixel(17,20,0);
putpixel(25,20,0);

for(i=17;i<20;i++)
    {
        putpixel(18,i,0);
        putpixel(24,i,0);
    }
setfillstyle(1,15);
floodfill(11,15,0);
floodfill(30,15,0);

for(i=17;i<20;i++)
    {
        putpixel(12,i,0);
        putpixel(16,i,0);
        putpixel(26,i,0);
        putpixel(30,i,0);
    }
for(i=13;i<16;i++)
    {
        putpixel(i,20,0);
        putpixel(i+14,20,0);
    }
setfillstyle(1,RED);
floodfill(13,17,0);
floodfill(27,17,0);

//----------рот--------------

putpixel(9,23,0);
putpixel(33,23,0);
for(i=10;i<13;i++)
    {
        putpixel(i,24,0);
        putpixel(i+20,24,0);
    }
for(i=13;i<18;i++)
    {
        putpixel(i,25,0);
        putpixel(i+12,25,0);
    }

for(i=18;i<25;i++)
    putpixel(i,26,0);

int j=0;
for(j=25,i=11;j<31;i++,j++)
    putpixel(i,j,0);

for(j=25,i=31;j<31;i--,j++)
    putpixel(i,j,0);

putpixel(17,30,0);
putpixel(25,30,0);

for(i=18;i<25;i++)
    putpixel(i,31,0);

setfillstyle(1,15);
floodfill(19,28,0);

for(i=26;i<30;i++)
    {
        putpixel(16,i,0);
        putpixel(26,i,0);
    }

for(i=27;i<31;i++)
    putpixel(21,i,0);

size=imagesize(1,1,40,40);
ptr=malloc(size);
getimage(1,1,40,40,ptr);

putimage(1,1,ptr,XOR_PUT);
