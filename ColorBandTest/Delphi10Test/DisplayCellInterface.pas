unit DisplayCellInterface;

interface
uses
  Spring,
  Spring.Collections
  ,Graphics
  ,Types
  ;

type

  TSVGTestReturn = record
    SVG: string;
    PerimeterTop: string;
    PerimeterBottom: string;
  end;

  TSVGCallBack = procedure(SVGContent: TSVGTestReturn) of object;

  TCellDescription = record
    Text:string;
    Rect: TRect;
    Colors: IList<TColor>;
    BandWidth: integer;
    Bandshift: integer;
    WhiteBorderWidth: integer;
  end;

  IDisplayCell = interface
  ['{229F0640-57DC-4E03-A78A-153A997D91E8}']
    function DisplayCell(CellDescription: TCellDescription; SVGCallBack: TSVGCallback): string;
    procedure Free;
  end;

    ISyncDisplayCell = interface
  ['{229F0640-57DC-4E03-A78A-153A997D91E8}']
    function SyncDisplayCell(CellDescription: TCellDescription): TSVGTestReturn;
  end;
implementation

end.
