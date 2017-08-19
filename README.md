# ColorBand
Pleasant-looking color slanted strips to draw in grids; use procedure in ClrBand.pas; Delphi 10 compatible.

Goal: outstanding color marking of some records in dataset, with possibility of random mix, and preservation of row selection.
Proposed metod drawing grid cell content on the canvas (Windows surface), and method returns string with content of the cell described as SVG fragment, for testing and fun.
Advantage of proposed method is its low resource consumption, since no secondary canvas is ever created, and no float arithmetics used.

Usage :

using
 Spring,
 Spring.Collections,
 ClrBand
 ;
 
...

  SVG: string;
  
  // how to put color strips on canvas
  
  SVG := ColorBandsOfListMovable(	Canvas, 
									Rect(Cell.Left, Cell.Top, Cell.Right, Cell.Bottom), 
									ColorList, BandWidth, BandShift, 'A some text'
								);
  
Bandwidth - it is width of single colored cell;
BandShift - it is shift of colored pattern by X axis.

Function returns SVG presentation of colored rectangle, with comments.

Used Spring4Delphi framework for collection handling ( I like interfaced collections) , and testing is done using DunitX framework; 


https://bitbucket.org/sglienke/spring4d
https://github.com/VSoftTechnologies/DUnitX


Example of use in ColorDatasetExample.dproj ( compiled in Win64).

Legacy version is preserved as branch ColorBandLegacyD7

Testing:
Visual testing to check behavior of the code on form resize: .\ColorBandTest\ColorBandVisualTest.dproj 
Formal testing project DunitX framework (Delphi 10) .\ColorBandTest\Delphi10Test\ColorBandTest.dproj
automate testing helper project: AutomatedTest.dproj

references:
Robert C. Martin; Clean Code: A handbook of agile software craftmanship; Rearson Education, Inc. 2009
Nick Hodges; Coding in Delphi; Trenchant Publishing, 2014





