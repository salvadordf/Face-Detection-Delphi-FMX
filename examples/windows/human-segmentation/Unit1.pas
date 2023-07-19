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
  Segmentation: TTensorFlowLiteFMX;

const
  SegmentationInputSize = 512;
  SegmentationOutputSize = 512;

type
  PInputDataSegmentation = ^TInputDataSegmentation;
  TInputDataSegmentation = array [0 .. 3 - 1] of array [0 .. SegmentationInputSize - 1] of array [0 .. SegmentationInputSize - 1] of Float32;

type
  POutputDataSegmentation = ^TOutputDataSegmentation;
  TOutputDataSegmentation = array [0 .. 1 - 1] of array [0 .. SegmentationOutputSize * SegmentationOutputSize - 1] of Float32;

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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := SegmentationInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := SegmentationInputSize;

  ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
  try
    ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
    try
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.Clear(TAlphaColorRec.Null);

      if ImageList.Source[0].MultiResBitmap[0].Bitmap.Width > ImageList.Source[0].MultiResBitmap[0].Bitmap.Height then
      begin
        FScale := SegmentationInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Width;

        FTop := (SegmentationInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale) / 2);
        FHeight := ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          0,
          FTop,
          SegmentationInputSize,
          FHeight + FTop),
          1, False);
      end
      else
      begin
        FScale := SegmentationInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Height;
        FLeft := (SegmentationInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale) / 2);
        FWidth := ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          FLeft,
          0,
          FWidth + FLeft,
          SegmentationInputSize),
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
  FInputData: PInputDataSegmentation;
  FOutputData: POutputDataSegmentation;
  FStatus: TFLiteStatus;
  FColor: TAlphaColorRec;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.Read, FBitmapData)) then
  begin
    try
      GetMem(FInputData, Segmentation.Input.Tensors[0].DataSize);
      try
        for Y := 0 to SegmentationInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to SegmentationInputSize - 1 do
          begin
            FInputData[0][Y][X] := (TAlphaColorRec(FColors[X]).R * 0.0078125) - 1;
            FInputData[1][Y][X] := (TAlphaColorRec(FColors[X]).G * 0.0078125) - 1;
            FInputData[2][Y][X] := (TAlphaColorRec(FColors[X]).B * 0.0078125) - 1;
          end;
        end;

        FStatus := Segmentation.SetInputData(0, FInputData, Segmentation.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := Segmentation.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      GetMem(FOutputData, Segmentation.Output.Tensors[0].DataSize);
      try
        FStatus := Segmentation.GetOutputData(0, FOutputData, Segmentation.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
          Exit;

        FBitmap := TBitmap.Create;
        try
          FBitmap.Width := SegmentationOutputSize;
          FBitmap.Height := SegmentationOutputSize;

          FBitmap.Canvas.BeginScene;
          try
            // background color
            FBitmap.Canvas.Clear(TAlphaColorRec.White);

            FPixel := 0;

            for Y := 0 to SegmentationOutputSize - 1 do
            begin
              FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

              for X := 0 to SegmentationOutputSize - 1 do
              begin
                if (FOutputData[0][FPixel] > 0) and (FOutputData[0][FPixel] <= 1) then
                begin
                  FColor.R := Round(255 * FOutputData[0][FPixel]);
                  FColor.G := Round(255 * FOutputData[0][FPixel]);
                  FColor.B := Round(255 * FOutputData[0][FPixel]);
                  FColor.A := 255;

                  FBitmap.Canvas.Stroke.Color := FColor.Color;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 1);

                  // to remove background use this code
                  { FColor.R := Round(TAlphaColorRec(FColors[X]).R);
                    FColor.G := Round(TAlphaColorRec(FColors[X]).G);
                    FColor.B := Round(TAlphaColorRec(FColors[X]).B);
                    FColor.A := Round(255 * FOutputData[0][FPixel]);

                    FBitmap.Canvas.Stroke.Color := FColor.Color;
                    FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 1); }
                end;

                Inc(FPixel);
              end;
            end;

          finally
            FBitmap.Canvas.EndScene;
          end;

          ImageMain.Bitmap.Assign(FBitmap);
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
  Segmentation := TTensorFlowLiteFMX.Create(Self);
  Segmentation.LoadModel(ModelsPath + 'human_segment.tflite', 12);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Segmentation.Destroy;
end;

end.
