--		Copyright 1994 by Daniel R. Grayson
--- interface to the graphics library libgrx which accompanies deLowrie's
--- port of gcc for MSDOS

use strings;
use C;

export GrWRITE:=0;
export GrXOR := 0x10000;
export GrOR := 0x20000;
export GrAND := 0x30000;
GR_VGA := 0;               -- VGA adapter
GR_EGA := 1;               -- EGA adapter
GR_HERC := 2;              -- Hercules mono adapter
GR_8514A := 3;             -- 8514/A or compatible
GR_S3 := 4;                -- S3 graphics accelerator
export adapter_type():string := (
	x := GrAdapterType();
 	if x == GR_VGA then return ("VGA");
 	if x == GR_EGA then return ("EGA");
 	if x == GR_HERC then return ("Hercules");
 	if x == GR_8514A then return ("8514/A");
 	if x == GR_S3 then return ("S3 graphics accelerator");
 	"???");
GR_80_25_text := 0;
GR_default_text := 1;
GR_width_height_text := 2;
GR_biggest_text := 3;
GR_320_200_graphics := 4;
GR_default_graphics := 5;
GR_width_height_graphics := 6;
GR_biggest_noninterlaced_graphics := 7;
GR_biggest_graphics := 8;
export xsize := 0;
export ysize := 0;
export graphics_mode():void := (
 	GrSetMode (GR_biggest_noninterlaced_graphics, 0, 0, 0);
 	xsize = GrSizeX();
 	ysize = GrSizeY(););
export text_mode():void := GrSetMode(GR_default_text, 0, 0, 0);
import grtext(x:int,y:int,s:array(char),fg:int,bg:int):void;
