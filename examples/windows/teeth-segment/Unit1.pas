unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Vcl.Graphics,
{$ENDIF}System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Utils,
  System.ImageList, FMX.ImgList, FMX.ListBox, FMX.Filter.Effects, FMX.Edit,
  FMX.Colors;

type
  TForm1 = class(TForm)
    ImageMain: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

  public
    FFilterBlur: TFilterBoxBlur;
    procedure LoadImage;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


uses TensorFlowLiteFMX;

const
  ModelsPath = '..\..\..\..\..\models\';

var
  TeethSegmentation: TTensorFlowLiteFMX;

const
  TeethSegmentationInputSize = 416;
  TeethSegmentationOutputSize = 416;

type
  PInputDataTeethSegmentation = ^TInputDataTeethSegmentation;
  TInputDataTeethSegmentation = array [0 .. 3 - 1] of array [0 .. TeethSegmentationInputSize - 1] of array [0 .. TeethSegmentationInputSize - 1] of Float32;

type
  POutputDataTeethSegmentation = ^TOutputDataTeethSegmentation;
  TOutputDataTeethSegmentation = array [0 .. 1 - 1] of array [0 .. TeethSegmentationOutputSize * TeethSegmentationOutputSize - 1] of Float32;

procedure TForm1.LoadImage;
var
  FScale: Single;
  FLeft, FTop, FWidth, FHeight: Single;
begin
{$IFDEF MSWINDOWS}
  if not FileExists(OpenDialog.FileName) then
    Exit;

  if ImageMain.MultiResBitmap.Count > 0 then
    ImageMain.MultiResBitmap[0].Free;

  ImageMain.MultiResBitmap.Add;

  if FileExists(OpenDialog.FileName) then
  begin
    if ImageList.Source[0].MultiResBitmap.Count > 0 then
      ImageList.Source[0].MultiResBitmap[0].Free;

    ImageList.Source[0].MultiResBitmap.Add;
    ImageList.Source[0].MultiResBitmap[0].Bitmap.LoadFromFile(OpenDialog.FileName);
  end;

  if ImageList.Source[1].MultiResBitmap.Count > 0 then
    ImageList.Source[1].MultiResBitmap[0].Free;

  ImageList.Source[1].MultiResBitmap.Add;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := TeethSegmentationInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := TeethSegmentationInputSize;

  ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
  try
    ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
    try
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.Clear(TAlphaColorRec.Null);

      if ImageList.Source[0].MultiResBitmap[0].Bitmap.Width > ImageList.Source[0].MultiResBitmap[0].Bitmap.Height then
      begin
        FScale := TeethSegmentationInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Width;

        FTop := (TeethSegmentationInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale) / 2);
        FHeight := ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          0,
          FTop,
          TeethSegmentationInputSize,
          FHeight + FTop),
          1, False);
      end
      else
      begin
        FScale := TeethSegmentationInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Height;
        FLeft := (TeethSegmentationInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale) / 2);
        FWidth := ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          FLeft,
          0,
          FWidth + FLeft,
          TeethSegmentationInputSize),
          1, False);
      end;

    finally
      ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.EndScene;
    end;
  finally
    ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.EndScene;
  end;

  ImageMain.Bitmap.Assign(ImageList.Source[1].MultiResBitmap[0].Bitmap);

{$ENDIF MSWINDOWS}
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadImage;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, X, Y, FPixel: DWORD;
  FColors: PAlphaColorArray;
  FBitmap: TBitmap;
  FBitmapData: TBitmapData;
  FInputData: PInputDataTeethSegmentation;
  FOutputData: POutputDataTeethSegmentation;
  FStatus: TFLiteStatus;
  FThresh: Single;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      GetMem(FInputData, TeethSegmentation.Input.Tensors[0].DataSize);
      try
        for Y := 0 to TeethSegmentationInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to TeethSegmentationInputSize - 1 do
          begin
            FInputData[0][Y][X] := (TAlphaColorRec(FColors[X]).R * 0.0078125) - 1;
            FInputData[1][Y][X] := (TAlphaColorRec(FColors[X]).G * 0.0078125) - 1;
            FInputData[2][Y][X] := (TAlphaColorRec(FColors[X]).B * 0.0078125) - 1;
          end;
        end;

        FStatus := TeethSegmentation.SetInputData(0, FInputData, TeethSegmentation.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := TeethSegmentation.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      GetMem(FOutputData, TeethSegmentation.Output.Tensors[0].DataSize);
      try
        FStatus := TeethSegmentation.GetOutputData(0, FOutputData, TeethSegmentation.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
          Exit;

        FBitmap := TBitmap.Create;
        try
          FBitmap.Width := TeethSegmentationOutputSize;
          FBitmap.Height := TeethSegmentationOutputSize;

          FBitmap.Canvas.BeginScene;
          try
            FBitmap.Canvas.Clear(TAlphaColorRec.Null);

            FPixel := 0;
            FThresh := 0.5;

            for Y := 0 to TeethSegmentationOutputSize - 1 do
            begin
              FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

              for X := 0 to TeethSegmentationOutputSize - 1 do
              begin
                if (FOutputData[0][FPixel] >= FThresh) and (FOutputData[0][FPixel] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Lime;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.35);
                end;

                Inc(FPixel);
              end;
            end;

          finally
            FBitmap.Canvas.EndScene;
          end;

          FFilterBlur.Input := FBitmap;
          FFilterBlur.BlurAmount := 0.25;
          FBitmap.Assign(FFilterBlur.Output);

          ImageMain.Bitmap.Canvas.BeginScene;
          ImageMain.Bitmap.Canvas.DrawBitmap(
            FBitmap,
            RectF(0, 0, FBitmap.Width, FBitmap.Height),
            RectF(0, 0, ImageMain.Bitmap.Width, ImageMain.Bitmap.Height),
            1, False);
          ImageMain.Bitmap.Canvas.EndScene;

          ImageMain.InvalidateRect(ImageMain.ClipRect);
        finally
          FBitmap.Free;
        end;
      finally
        FreeMem(FOutputData);
      end;
    finally
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Unmap(FBitmapData);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FFilterBlur := TFilterBoxBlur.Create(nil);

  TeethSegmentation := TTensorFlowLiteFMX.Create(Self);
  TeethSegmentation.LoadModel(ModelsPath + 'teeth_segment.tflite', 12);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TeethSegmentation.Destroy;
end;

end.
