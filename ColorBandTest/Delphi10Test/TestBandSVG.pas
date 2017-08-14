unit TestBandSVG;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,
  ClrBandInterface,
  ComCtrls
  ,Spring.Collections
  ,StrUtils
  ;

type
  TSVGCellTester = class
    class function ListOfUsedPolygonCases(SVG: string): IList<integer>;
  end;
implementation

  class function TSVGCellTester.ListOfUsedPolygonCases(SVG: string): IList<integer>;
  begin
    Result := Spring.Collections.TCollections.CreateList<integer>;
  end;
end.
