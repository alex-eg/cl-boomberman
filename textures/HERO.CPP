//------------помпон------------
putpixel(18,1,14);
putpixel(19,1,14);
putpixel(20,1,14);
putpixel(21,1,14);
putpixel(22,1,14);
putpixel(23,1,14);

putpixel(17,2,14);
putpixel(24,2,14);

putpixel(16,3,14);
putpixel(17,3,14);
putpixel(24,3,14);
putpixel(25,3,14);

putpixel(17,4,14);
putpixel(24,4,14);

putpixel(18,5,14);
putpixel(19,5,14);
putpixel(20,5,14);
putpixel(21,5,14);
putpixel(22,5,14);
putpixel(23,5,14);
setfillstyle(1,14);
floodfill(18,2,14);

//----------шапка---------

putpixel(12,4,3);
putpixel(13,4,3);
putpixel(14,4,3);
putpixel(15,4,3);
putpixel(16,4,3);
putpixel(25,4,3);
putpixel(26,4,3);
putpixel(27,4,3);
putpixel(28,4,3);
putpixel(29,4,3);

putpixel(10,5,3);
putpixel(11,5,3);
putpixel(17,5,3);
putpixel(24,5,3);
putpixel(30,5,3);
putpixel(31,5,3);

putpixel(8,6,3);
putpixel(9,6,3);
putpixel(18,6,3);
putpixel(19,6,3);
putpixel(20,6,3);
putpixel(21,6,3);
putpixel(22,6,3);
putpixel(23,6,3);
putpixel(32,6,3);
putpixel(33,6,3);

putpixel(6,7,3);
putpixel(7,7,3);
putpixel(34,7,3);
putpixel(35,7,3);

putpixel(5,8,3);
putpixel(36,8,3);

putpixel(4,9,3);
putpixel(37,9,3);

putpixel(3,10,3);
putpixel(38,10,3);

putpixel(3,11,3);
for(int i=12; i<30; i++)
    putpixel(i,11,3);
putpixel(38,11,3);

putpixel(2,12,3);
for(i=6; i<12; i++)
    putpixel(i,12,3);
for(i=30; i<36; i++)
    putpixel(i,12,3);
putpixel(39,12,3);

putpixel(2,13,3);
putpixel(3,13,3);
putpixel(4,13,3);
putpixel(5,13,3);
putpixel(36,13,3);
putpixel(37,13,3);
putpixel(38,13,3);
putpixel(39,13,3);
setfillstyle(1,3);
floodfill(20,8,3);

//---------------лицо-------------

for(i=1; i<6; i++)
    putpixel(i,14,15);
for(i=36; i<41; i++)
    putpixel(i,14,15);

for(i=6; i<12; i++)
    putpixel(i,13,15);
for(i=30; i<36; i++)
    putpixel(i,13,15);

for(i=12; i<30; i++)
    putpixel(i,12,15);

for(i=15; i<19; i++) {
    putpixel(1,i,15);
    putpixel(40,i,15);
 }

for(i=19;i<22;i++) {
    putpixel(2,i,15);
    putpixel(39,i,15);
 }

putpixel(3,22,15);
putpixel(38,22,15);

putpixel(4,23,15);
putpixel(37,23,15);

putpixel(5,24,15);
putpixel(36,24,15);

putpixel(6,25,15);
putpixel(7,25,15);
putpixel(35,25,15);
putpixel(34,25,15);

putpixel(8,26,15);
putpixel(9,26,15);
putpixel(33,26,15);
putpixel(32,26,15);

putpixel(10,27,15);
putpixel(11,27,15);
putpixel(31,27,15);
putpixel(30,27,15);

putpixel(12,28,15);
putpixel(13,28,15);
putpixel(14,28,15);
putpixel(29,28,15);
putpixel(28,28,15);
putpixel(27,28,15);

for(i=15;i<27;i++)
    putpixel(i,29,15);

setfillstyle(1,15);
floodfill(4,16,15);

//----глаза рот-------

for(i=16;i<20;i++) {
    putpixel(i,12,0);
    putpixel(i,20,0);
 }

for(i=23;i<27;i++) {
    putpixel(i,12,0);
    putpixel(i,20,0);
 }

for(i=15;i<18;i++)
    {
        putpixel(14,i,0);
        putpixel(21,i,0);
        putpixel(28,i,0);
    }
for(i=13;i<15;i++)
    {
        putpixel(15,i,0);
        putpixel(15,i+5,0);
        putpixel(20,i,0);
        putpixel(20,i+5,0);
        putpixel(22,i,0);
        putpixel(22,i+5,0);
        putpixel(27,i,0);
        putpixel(27,i+5,0);
    }
putpixel(18,16,0);
putpixel(24,16,0);

putpixel(23,25,0);
putpixel(24,25,0);
putpixel(25,25,0);
putpixel(25,24,0);
putpixel(26,24,0);
putpixel(27,23,0);

//-----тело---------

putpixel(4,24,RED);
putpixel(37,24,RED);

putpixel(5,25,RED);
putpixel(36,25,RED);

putpixel(6,26,RED);
putpixel(7,26,RED);
putpixel(35,26,RED);
putpixel(34,26,RED);

putpixel(8,27,RED);
putpixel(9,27,RED);
putpixel(33,27,RED);
putpixel(32,27,RED);

putpixel(10,28,RED);
putpixel(11,28,RED);
putpixel(31,28,RED);
putpixel(30,28,RED);

for(i=12;i<15;i++)
    {
        putpixel(i,29,RED);
        putpixel(i+15,29,RED);
    }

for(i=15;i<27;i++)
    putpixel(i,30,RED);

putpixel(3,23,RED);
putpixel(38,23,RED);

putpixel(3,24,RED);
putpixel(38,24,RED);

putpixel(2,25,RED);
putpixel(3,25,RED);
putpixel(38,25,RED);
putpixel(39,25,RED);

putpixel(2,26,RED);
putpixel(39,26,RED);

putpixel(2,27,RED);
putpixel(39,27,RED);

for(i=1;i<5;i++)
    {
        putpixel(i,28,RED);
        putpixel(i+36,28,RED);
    }
putpixel(5,29,RED);
putpixel(36,29,RED);

for(i=30;i<33;i++)
    {
        putpixel(6,i,RED);
        putpixel(36,i,RED);
    }

for(i=2;i<6;i++)
    {
        putpixel(i,33,RED);
        putpixel(i+34,33,RED);
    }

putpixel(2,33,RED);
putpixel(39,33,RED);

putpixel(2,34,RED);
putpixel(39,34,RED);

for(i=3;i<8;i++)
    {
        putpixel(i,35,RED);
        putpixel(i+31,35,RED);
    }

for(i=8;i<16;i++)
    {
        putpixel(i,36,RED);
        putpixel(i+18,36,RED);
    }

for(i=16;i<26;i++)
    putpixel(i,37,RED);

setfillstyle(1,RED);
floodfill(10,30,RED);

putpixel(20,30,0);
putpixel(21,30,0);
putpixel(21,31,0);
for(i=32;i<36;i++)
    putpixel(22,i,0);
putpixel(21,35,0);
putpixel(20,36,0);
putpixel(21,36,0);
putpixel(24,32,0);
putpixel(24,34,0);
putpixel(20,37,0);
putpixel(21,37,0);

//---------ноги--------
putpixel(5,36,BROWN);
putpixel(36,36,BROWN);

putpixel(5,37,BROWN);
putpixel(36,37,BROWN);

putpixel(6,38,BROWN);
putpixel(35,38,BROWN);

putpixel(6,39,BROWN);
putpixel(35,39,BROWN);
for(i=7;i<35;i++)
    putpixel(i,39,BROWN);

putpixel(6,36,BROWN);
putpixel(7,36,BROWN);
putpixel(34,36,BROWN);
putpixel(35,36,BROWN);
for(i=8;i<16;i++)
    {
        putpixel(i,37,BROWN);
        putpixel(i+18,37,BROWN);
    }
for(i=16;i<26;i++)
    putpixel(i,38,BROWN);

setfillstyle(1,BROWN);
floodfill(8,38,BROWN);
floodfill(33,38,BROWN);

putpixel(22,38,0);
putpixel(22,39,0);

//------ступни------
for(i=5;i<37;i++)
    putpixel(i,40,8);

//----перчатки------
for(i=1;i<5;i++)
    {
        putpixel(i,29,14);
        putpixel(i+36,29,14);
    }
for(i=30;i<33;i++)
    {
        putpixel(5,i,14);
        putpixel(36,i,14);
    }
for(i=1;i<5;i++)
    {
        putpixel(i,32,14);
        putpixel(i+36,32,14);
    }
putpixel(1,30,14);
putpixel(40,30,14);

putpixel(1,31,14);
putpixel(40,31,14);

setfillstyle(1,14);
floodfill(2,31,14);
floodfill(39,31,14);

size=imagesize(1,1,40,40);
ptr=malloc(size);
getimage(1,1,40,40,ptr);

putimage(1,1,ptr,XOR_PUT);
