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
  FaceSegmentation: TTensorFlowLiteFMX;

const
  FaceSegmentationInputSize = 384;
  FaceSegmentationOutputSize = 384;

type
  PInputDataFaceSegmentation = ^TInputDataFaceSegmentation;
  TInputDataFaceSegmentation = array [0 .. FaceSegmentationInputSize - 1] of array [0 .. FaceSegmentationInputSize - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceSegmentation = ^TOutputDataFaceSegmentation;
  TOutputDataFaceSegmentation = array [0 .. FaceSegmentationOutputSize * FaceSegmentationOutputSize - 1] of array [0 .. 15 - 1] of Float32;

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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := FaceSegmentationInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := FaceSegmentationInputSize;

  ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
  try
    ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
    try
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.Clear(TAlphaColorRec.Null);

      if ImageList.Source[0].MultiResBitmap[0].Bitmap.Width > ImageList.Source[0].MultiResBitmap[0].Bitmap.Height then
      begin
        FScale := FaceSegmentationInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Width;

        FTop := (FaceSegmentationInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale) / 2);
        FHeight := ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          0,
          FTop,
          FaceSegmentationInputSize,
          FHeight + FTop),
          1, False);
      end
      else
      begin
        FScale := FaceSegmentationInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Height;
        FLeft := (FaceSegmentationInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale) / 2);
        FWidth := ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          FLeft,
          0,
          FWidth + FLeft,
          FaceSegmentationInputSize),
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
  FInputData: PInputDataFaceSegmentation;
  FOutputData: POutputDataFaceSegmentation;
  FStatus: TFLiteStatus;
  FThresh: Single;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      GetMem(FInputData, FaceSegmentation.Input.Tensors[0].DataSize);
      try
        for Y := 0 to FaceSegmentationInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to FaceSegmentationInputSize - 1 do
          begin
            FInputData[Y][X][0] := (TAlphaColorRec(FColors[X]).R * 0.0078125);
            FInputData[Y][X][1] := (TAlphaColorRec(FColors[X]).G * 0.0078125);
            FInputData[Y][X][2] := (TAlphaColorRec(FColors[X]).B * 0.0078125);
          end;
        end;

        FStatus := FaceSegmentation.SetInputData(0, FInputData, FaceSegmentation.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := FaceSegmentation.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      GetMem(FOutputData, FaceSegmentation.Output.Tensors[0].DataSize);
      try
        FStatus := FaceSegmentation.GetOutputData(0, FOutputData, FaceSegmentation.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
          Exit;

        FBitmap := TBitmap.Create;
        try
          FBitmap.Width := FaceSegmentationOutputSize;
          FBitmap.Height := FaceSegmentationOutputSize;

          FBitmap.Canvas.BeginScene;
          try
            FBitmap.Canvas.Clear(TAlphaColorRec.Null);

            FPixel := 0;
            FThresh := 0.5;

            for Y := 0 to FaceSegmentationOutputSize - 1 do
            begin
              FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

              for X := 0 to FaceSegmentationOutputSize - 1 do
              begin
                // skin
                if (FOutputData[FPixel][0] >= FThresh) and (FOutputData[FPixel][0] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // nose
                if (FOutputData[FPixel][1] >= FThresh) and (FOutputData[FPixel][1] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // eye_left
                if (FOutputData[FPixel][2] >= FThresh) and (FOutputData[FPixel][2] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // eye_right
                if (FOutputData[FPixel][3] >= FThresh) and (FOutputData[FPixel][3] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Green;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // brow_left
                if (FOutputData[FPixel][4] >= FThresh) and (FOutputData[FPixel][4] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Green;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // brow_right
                if (FOutputData[FPixel][5] >= FThresh) and (FOutputData[FPixel][5] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // ear_left
                if (FOutputData[FPixel][6] >= FThresh) and (FOutputData[FPixel][6] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // ear_right
                if (FOutputData[FPixel][7] >= FThresh) and (FOutputData[FPixel][7] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // mouth
                if (FOutputData[FPixel][8] >= FThresh) and (FOutputData[FPixel][8] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Pink;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // lip_top
                if (FOutputData[FPixel][9] >= FThresh) and (FOutputData[FPixel][9] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Green;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;
                // lip_bottom
                if (FOutputData[FPixel][10] >= FThresh) and (FOutputData[FPixel][10] <= 1) then
                begin
                  FBitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                  FBitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.2);
                end;

                Inc(FPixel);
              end;
            end;

          finally
            FBitmap.Canvas.EndScene;
          end;

          FFilterBlur.Input := FBitmap;
          FFilterBlur.BlurAmount := 1;
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

  FaceSegmentation := TTensorFlowLiteFMX.Create(Self);
  // https://drive.google.com/file/d/1h9rr233-UT8BtB43tqyJ07QQenBv4-QZ/view?usp=sharing
  FaceSegmentation.LoadModel(ModelsPath + 'face_segment.tflite', 12);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceSegmentation.Destroy;
end;

end.
