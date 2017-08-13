# ColorBand
Pleasant-looking color slanted strips to draw in grids; use procedure in ClrBand.pas; 
Used Spring4Delphi framework for collection handling; 

https://bitbucket.org/sglienke/spring4d

Goal: outstanding color marking of some records in dataset, with possibility of random mix, and preservation of row selection.
Proposed metod drawing grid cell content on the canvas (Windows surface), and method returns string with content of the cell described as SVG fragment, for testing and fun.
Advantage of proposed method is its speed, since no secondary canvas is ever created, no float arithmetics and such.

Example of use in ColorDatasetExample project: event handler for cell draw make use of proposed method.

This branch is rewritten using advanced features of Delphi 10 and Spring For Delphi framework, and so is compatible with newer versions of Delphi only.
Legacy version is preserved as branch ColorBandLegacyD7

Testing:
Visual testing to check behavior of the code on form resize Delphi 7: .\ColorBandTest\ColorBandVisualTest.dpr 
Formal testing project DunitX framework (Delphi 10) .\ColorBandTest\Delphi10Test\ColorBandTest.dproj



