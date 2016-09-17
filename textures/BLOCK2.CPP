setcolor(7);
rectangle(0,0,39,39);

setfillstyle(1,7);
floodfill(15,20,7);

setcolor(8);
line(0,0,39,0);
line(0,13,39,13);
line(39,1,39,12);
putpixel(0,1,8);

line(13,14,13,25);
line(0,26,39,26);
putpixel(14,14,8);

line(26,27,26,38);
line(0,39,39,39);
putpixel(27,27,8);

setcolor(15);
line(1,1,38,1);
line(0,2,0,12);

line(0,14,12,14);
line(15,14,39,14);

line(0,27,25,27);
line(28,27,39,27);

size=imagesize(0,0,39,39);
ptr=malloc(size);
getimage(0,0,39,39,ptr);

putimage(0,0,ptr,XOR_PUT);
