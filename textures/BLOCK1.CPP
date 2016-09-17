setcolor(7);
rectangle(0,0,39,39);

setfillstyle(1,7);
floodfill(15,20,7);

setcolor(15);
line(1,0,39,0);
line(0,1,0,38);

setcolor(8);
line(0,39,39,39);
line(1,38,37,38);
line(39,0,39,39);

size=imagesize(0,0,39,39);
ptr=malloc(size);
getimage(0,0,39,39,ptr);

putimage(0,0,ptr,XOR_PUT);
