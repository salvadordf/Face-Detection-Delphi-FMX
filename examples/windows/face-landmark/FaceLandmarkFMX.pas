unit FaceLandmarkFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, System.IOUtils, FMX.Utils, FMX.Graphics,
  TensorFlowLiteFMX;

const
  FaceLandmarkInputSize = 160;
  FaceLandmarkOutputSize = 136;

type
  PInputDataFaceLandmark = ^TInputDataFaceLandmark;
  TInputDataFaceLandmark = array [0 .. FaceLandmarkInputSize - 1] of array [0 .. FaceLandmarkInputSize - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceLandmark = ^TOutputDataFaceLandmark;
  TOutputDataFaceLandmark = array [0 .. 68 - 1] of array [0 .. 2 - 1] of Float32;

type
  TFaceLandmarkData = record
    Points: array of TPointF;
    Count: Int32;
  end;

type
  EFaceLandmarkFMXError = class(Exception);

  TFaceLandmarkFMX = class(TComponent)
  private
    FaceLandmark: TTensorFlowLiteFMX;

    FInputData: TInputDataFaceLandmark;
    FOutputData: TOutputDataFaceLandmark;
  public
    UseGpu: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function LoadModel(ModelPath: String; ThreadCount: Integer): TFLiteStatus;
    procedure UnloadModel;

    function GetLandmarkData(Bitmap: TBitmap): TFaceLandmarkData;
  end;

implementation

function TFaceLandmarkFMX.GetLandmarkData(Bitmap: TBitmap): TFaceLandmarkData;
var
  FPixel, X, Y: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FBitmap: TBitmap;
  FStatus: TFLiteStatus;
begin
  SetLength(Result.Points, 0);
  Result.Count := 0;

  if not Assigned(Bitmap) then
  begin
    Exit;
  end;

  FBitmap := TBitmap.Create;

  FBitmap.Width := FaceLandmarkInputSize;
  FBitmap.Height := FaceLandmarkInputSize;

  FBitmap.Canvas.BeginScene;
  try
    FBitmap.Canvas.Clear(TAlphaColorRec.Null);

    Bitmap.Canvas.BeginScene;
    try
      if (Bitmap.Width <> FaceLandmarkInputSize) and (Bitmap.Height <> FaceLandmarkInputSize) then
      begin
        if Bitmap.Width > Bitmap.Height then
        begin
          FBitmap.Canvas.DrawBitmap(
            Bitmap,
            Bitmap.BoundsF,
            RectF(0, 0, FaceLandmarkInputSize, Bitmap.Height / (Bitmap.Width / FaceLandmarkInputSize)),
            1, False);
        end
        else
        begin
          FBitmap.Canvas.DrawBitmap(
            Bitmap,
            Bitmap.BoundsF,
            RectF(0, 0, Bitmap.Width / (Bitmap.Height / FaceLandmarkInputSize), FaceLandmarkInputSize),
            1, False);
        end;
      end
      else
      begin
        FBitmap.Canvas.DrawBitmap(
          Bitmap,
          Bitmap.BoundsF,
          Bitmap.BoundsF,
          1, False);
      end;

    finally
      Bitmap.Canvas.EndScene;
    end;
  finally
    FBitmap.Canvas.EndScene;
  end;

  if (FBitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      for Y := 0 to FaceLandmarkInputSize - 1 do
      begin
        FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

        for X := 0 to FaceLandmarkInputSize - 1 do
        begin
          FInputData[Y][X][0] := (TAlphaColorRec(FColors[X]).R);
          FInputData[Y][X][1] := (TAlphaColorRec(FColors[X]).G);
          FInputData[Y][X][2] := (TAlphaColorRec(FColors[X]).B);
        end;
      end;

      FStatus := FaceLandmark.SetInputData(0, @FInputData, FaceLandmark.Input.Tensors[0].DataSize);

      if FStatus <> TFLiteOk then
      begin
        raise ETensorFlowLiteFMXError.Create('Error: SetInputData');
        Exit;
      end;

      FStatus := FaceLandmark.Inference;

      if FStatus <> TFLiteOk then
      begin
        raise ETensorFlowLiteFMXError.Create('Error: Inference');
        Exit;
      end;

      FStatus := FaceLandmark.GetOutputData(2, @FOutputData, FaceLandmark.Output.Tensors[2].DataSize);

      if FStatus <> TFLiteOk then
      begin
        raise ETensorFlowLiteFMXError.Create('Error: GetOutputData');
        Exit;
      end;

      SetLength(Result.Points, FaceLandmarkOutputSize div 2);
      Result.Count := FaceLandmarkOutputSize div 2;

      FPixel := 0;

      for X := 0 to FaceLandmarkOutputSize div 2 - 1 do
      begin
        Result.Points[FPixel].X := FOutputData[FPixel][0] * Bitmap.Width;
        Result.Points[FPixel].Y := FOutputData[FPixel][1] * Bitmap.Height;

        Inc(FPixel);
      end;

    finally
      FBitmap.Unmap(FBitmapData);
    end;
  end;
end;

function TFaceLandmarkFMX.LoadModel(ModelPath: String; ThreadCount: Integer): TFLiteStatus;
begin
  FaceLandmark.UseGpu := UseGpu;

  Result := FaceLandmark.LoadModel(ModelPath, ThreadCount);
end;

procedure TFaceLandmarkFMX.UnloadModel;
begin
  FaceLandmark.UnloadModel;
end;

constructor TFaceLandmarkFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  UseGpu := False;

  FaceLandmark := TTensorFlowLiteFMX.Create(Self);
end;

destructor TFaceLandmarkFMX.Destroy;
begin
  FaceLandmark.Destroy;

  inherited Destroy;
end;

end.
