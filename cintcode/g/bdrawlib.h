/*
This is the header file for the BCPL drawing library b/bdrawlib.b
It is based in the old library g/graphics.b

Implemented by Martin Richards (c) 11 Sep 2021

The default setting of the manifest g_bdrawbase=400 is defined in
libhdr.h, but can be overridden by a later definition.

Unlike g/graphics.b this library will always generate BMP images using
24-bit pixels. Internally the image is formed in a rectangular array
(canvas) of BCPL words holding pixels of the form #x00rrggbb.  The
canvas has xsize pixels in the x direction and ysize pixels in the y
direction.

The pixel with coordinates (0,0) is a unit square with opposite
veritices (0,0) and (1,1) and is the bottom leftmost pixel in the
image. The pixels on the bottom line runs from (0,0) to (xupb,0), and
the vertical line on the left of the rimage runs from (0,0) to
(0,yupb).

The variable currcol holds the current colour as #x00rrggbb.

Drawing in done using a pen function held in currpen. Some of the
possible pen functions are:

drawpixel(x,y) Draw the pixel of colour currcol at position (x,y).
penS1(x,y)     Draw pixels a 1x1 square of at (x,  y).
penS2(x,y)     Draw pixels a 2x2 square of from (x-1,y-1) to (x,  y).
penS3(x,y)     Draw pixels a 3x3 square of from (x-1,y-1) to (x+1,y+1).
penS4(x,y)     Draw pixels a 4x4 square of from (x-2,y-2) to (x+1,y+1).
penS5(x,y)     Draw pixels a 5x5 square of from (x-2,y-2) to (x+2,y+2).

penH2(x,y)     Draw pixels from (x-1,y) to (x,  y)
penH3(x,y)     Draw pixels from (x-1,y) to (x+1,y)
penH4(x,y)     Draw pixels from (x-2,y) to (x+1,y)
penH5(x,y)     Draw pixels from (x-2,y) to (x+2,y)

penV2(x,y)     Draw pixels from (x,y-1) to (x,y)
penV3(x,y)     Draw pixels from (x,y-1) to (x,y+1)
penV4(x,y)     Draw pixels from (x,y-2) to (x,y+1)
penV5(x,y)     Draw pixels from (x,y-2) to (x,y+2)

penC5(x,y)     Draw a filled circle of diameter 5 centred at (x,y)

arrow(dir, x, y, len) Draw an arrow head of length len with the point
                      at (x,y). The direction is specified by dir.
                      It points to the right if dir=0, up if zero,
                      left if 2 and down if 3. It uses currpen which
                      shold be set appropriately.
History

03/06/2021
Adding a 12x18 font as an alternative to the 8x12 font previously
available. The larger font is better when large window sizes are used.
Use selectfont(12) or selectfont(18) to select the font. It updates
fontW, fontH, drawch and write_ch_slice and sets fonttab to the
appropriate bitmap table.


*/


GLOBAL {
openbdraw: g_bdrawbase  // (xmax, ymax)

closebdraw              // ()

xsize       // Number of x pixel positions in the canvas
ysize       // Number of y pixel positions in the canvas

xupb        // The largest x coordinate in the drawing area
yupb        // The largest y coordinate in the drawing area
	    
canvas      // Rectangular array of 32-bit pixels 
            // Each element holds a 32-bit coulour #x00rrggbb

canvasupb   // UPB of canvas in words


col_white
col_majenta
col_blue
col_cyan
col_green
col_yellow
col_red
col_black

currx          // Current x position, 0<=currx<xsize
curry          // Cyrrent y position, 0<=currx<xsize
currcol        // Current colour of the form #X00rrggbb.

setcolour      // (col)             Set the current colour in currcol

currpen        // (x,y)             The current pen function
               //                   (0,0) position of the bottom left pixel

penS1          // (x,y)             Draw a point using currcol
penS2          // (x,y)             Draw a dot of size 2 using currcol
penS3          // (x,y)             Draw a dot of size 3 using currcol
penS4          // (x,y)             Draw a dot of size 4 using currcol
penS5          // (x,y)             Draw a dot of size 5 using currcol

penH2         // (x,y)             Draw a horizontal line length 2 of currcol
penH3         // (x,y)             Draw a horizontal line length 3 of currcol
penH4         // (x,y)             Draw a horizontal line length 4 of currcol
penH5         // (x,y)             Draw a horizontal line length 5 of currcol

penV2         // (x,y)             Draw a vertical line length 2 of currcol
penV3         // (x,y)             Draw a vertical line length 3 of currcol
penV4         // (x,y)             Draw a vertical line length 4 of currcol
penV5         // (x,y)             Draw a vertical line length 5 of currcol

penC5         // (x,y)             Draw a circle of radius 5 of currcol

drawarc45      // (n, x, y, r)
drawarc90      // (n, x, y, r)

drawarrow      // (dir, x, y, len)

rndcorner      // (dir, x, y, r)
drawgrid       // (sep, pen, colour)

fontW              // Typically  8 or 12
fontH              // Typically 12 or 18
charHsep           // Char horizontal separation in pixels, typically 2 or 3 
charVsep           // Char verticalal separation in pixels, typically 3 or 4 
charLmargin        // Typically 10 pixels
charleveloffset    // The level offset used by drawch
charmidleveloffset // fontH/2 - baselevel, set by selectfont

write_ch_slice     // Used by drawch
fonttab            // The font table used by write_ch_slice

selectfont         // Sets fontW, fontH, drawch, write_ch_slice and fonttab

drawch         // (ch)              Draw a character
drawstr        // (x,y,s)           Draw a string
drawstrcentred // (x,y,s)           Draw a string centred
strpixels      // (s)               Length of a string in pixels
drawf          // (x,y,format,<args>)
drawwrch       // (ch)              The version of wrch used by drawf
drawfstr       //                   Used by drawwch
moveto         // (x,y)             Set the current position  
moveby         // (dx,dy)           Increment the current position
drawto         // (x,y)             Draw a line from the current position
drawby         // (dx,dy)           Draw a line given its position increment
drawrect       // (x,y,w,h)         Draw a rectangle of width w and height h
               //                   bottom left corner at (x,y) using currcol
drawfillrect   // (x,y,w,h)         Draw a filled rectangle, width w height h
               //                   bottom left corner at (x,y) using currcol
drawrndrect    // ((x,y,w,h,radius) Draw a rectangle with rounded corners
               //                   and height h with bottom left corner at (x,y)
	       //                   using currcol
fillrndrect    // ((x,y,radius)     Draw a filled rectangle with rounded corners
               //                   of width w and height h, bottom left
	       //                   at (x,y) using currcol
drawcircle     // (x,y,radius)      Draw a circle with centre (x,y) given radius
fillcircle     // (x,y,radius)      Draw a filled circle with centre (x,y) and
               //                   given radius
	     
drawarc45      // (n, x, y, r)      Draw a 45 degree arc centred at (x,y)
               //                   with radius r in octant n

drawarc90      // (n, x, y, r)      Draw a 90 degree arc centred at (x,y)
               //                   with radius r in quadrant n

drawcurve      // (v)               Draw a piecewise linear curve. V!0 is
               //                   the upb of v, The point coordinates are
	       //                   pairs of words starting a v!1
drawsmooth     // (v)               Draw a smooth curve. V!0 is the upb of v.
               //                   The point coordinates are pairs of words
	       //                   starting a v!1. The first and last segments
	       //                   are linear, the others are smoothed. The
	       //                   first two and and last two point are
	       //                   normally close together.
drawcurlyH     // (x, y, xs, ys)    Draw a horizontal curly bracket
drawcurlyV     // (x, y, xs, ys)    Draw a vertical curly bracket

sqtextbox      // (x, y, text)      Draw text centred in a square box with
               //                   centre of the left edge at (x,y)
	       //                   returning the x coord of the right edge
rndtextbox     // (x, y, text)      Draw text centred in a rounded box with
               //                   centre of the left edge at (x,y)
	       //                   returning the x coord of the right edge

wrimage        // (filename)        Output the image as a .bpm file.
}


