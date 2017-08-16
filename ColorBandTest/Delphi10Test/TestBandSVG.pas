unit TestBandSVG;

interface
uses
   Spring
  ,Spring.Collections
  ,StrUtils
  ,SysUtils
  ,AnsiStrings
  ;

  type
  ISVGTester = interface(IInvokable)
  ['{4EB381E9-48C1-43B6-B847-50569DE5822A}']
    function ExtractUsedPolygonKindsFromSVG(SVG: string): IList<integer>;
    procedure SubtractUsedCases(TotalListOfUsedCases: IList<integer>; const ListofCasesToSubtract: IList<integer>);
  end;


  TSVGTester = class(TInterfacedObject, ISVGTester)
  private
    class function ExtractListOfCommentsFromSVG(SVG: string): IList<string>;
    class function CommentsToPolygonKinds(Comments: IList<string>): IList<integer>;
  protected
    function ExtractUsedPolygonKindsFromSVG(SVG: string): IList<integer>;
    procedure SubtractUsedCases(TotalListOfUsedCases: IList<integer>; const ListofCasesToSubtract: IList<integer>);
  end;


implementation

{ TSVGTester }

function TSVGTester.ExtractUsedPolygonKindsFromSVG(
  SVG: string): IList<integer>;
var
 ExtractedSVGComments: IList<string>;
begin
   ExtractedSVGComments := ExtractListOfCommentsFromSVG(SVG);
   Result := CommentsToPolygonKinds(ExtractedSVGComments);
end;

procedure TSVGTester.SubtractUsedCases(TotalListOfUsedCases: IList<integer>;
  const ListOfCasesToSubtract: IList<integer>);
var
  MarkAsUsedInTotalList: TAction<integer>;
begin
  MarkAsUsedInTotalList := procedure( const Index: integer)
  begin
     TotalListOfUsedCases.items[Index] := -1;
  end;

  ListOfCasesToSubtract.ForEach(MarkAsUsedInTotalList);
end;

class function TSVGTester.CommentsToPolygonKinds(
  Comments: IList<string>): IList<integer>;
var
  AppendToOutputList: TAction<string>;
  CaseTail: string ;
  LocalCaseList: IList<integer>;
begin
  LocalCaseList := TCollections.CreateList<integer>;

  AppendToOutputList := procedure( const Comment: string)
  var
    iStartCommentPos: integer;
  begin
     iStartCommentPos := Pos('Polygon variation Case ', Comment);
     if iStartCommentPos > 0 then
     begin
       CaseTail := Copy(Comment, iStartCommentPos + Length('Polygon variation Case '), 2);
       LocalCaseList.Add(StrToInt(Trim(CaseTail)));
     end;
  end;

  Comments.ForEach(AppendToOutputList);
  Result := LocalCaseList;
end;

class function TSVGTester.ExtractListOfCommentsFromSVG(SVG: string): IList<string>;
var
  CommentHeadPlace, CommentTailPlace: integer;
  SVGComments: IList<string>;
  CommentLength: integer;
  CommentStart: integer;
begin
   CommentHeadPlace := 1;
   Result := TCollections.CreateList<string>;
   while CommentHeadPlace > 0 do
   begin
     CommentHeadPlace := Posex('<!--', SVG, CommentHeadPlace);
     if CommentHeadPlace > 0 then
     begin
       CommentTailPlace := Posex('-->', SVG, CommentHeadPlace);
       if (CommentTailPlace > 0) and (CommentTailPlace > 0) then
          CommentStart := CommentHeadPlace + Length('<!--');
          CommentLength := CommentTailPlace - CommentHeadPlace - Length('<!--');
          Result.Add(Copy(SVG,
                          CommentStart,
                          CommentLength
                          )
                    );
       CommentHeadPlace := CommentTailplace;
     end;
   end;
end;

end.
