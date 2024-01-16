unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Vcl.Graphics,
{$ENDIF}System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Utils, System.Math.Vectors,
  System.ImageList, FMX.ImgList, FMX.ListBox, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    ImageMain: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    Label4: TLabel;
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
  TextDetection: TTensorFlowLiteFMX;

var
  TextDetectionInputSize: Int32 = 800;
  TextDetectionOutputSize: Int32 = 13125;

type
  PInputDataTextDetection = ^TInputDataTextDetection;
  TInputDataTextDetection = array [0 .. 3 - 1] of array [0 .. 800 * 800 - 1] of Float32;

type
  POutputDataTextDetection = ^TOutputDataTextDetection;
  TOutputDataTextDetection = array [0 .. 5 - 1] of array [0 .. 13125 - 1] of Float32;

type
  TText = record
    Rect: TRectF;
    Probability: Float32;
  end;

type
  TTextList = record
    Texts: array of TText;
    Count: Int32;
  end;

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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := TextDetectionInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := TextDetectionInputSize;

  ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
  try
    ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
    try
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.Clear(TAlphaColorRec.Null);

      if ImageList.Source[0].MultiResBitmap[0].Bitmap.Width > ImageList.Source[0].MultiResBitmap[0].Bitmap.Height then
      begin
        FScale := TextDetectionInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Width;

        FTop := (TextDetectionInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale) / 2);
        FHeight := ImageList.Source[0].MultiResBitmap[0].Bitmap.Height * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          0,
          FTop,
          TextDetectionInputSize,
          FHeight + FTop),
          1, False);
      end
      else
      begin
        FScale := TextDetectionInputSize / ImageList.Source[0].MultiResBitmap[0].Bitmap.Height;
        FLeft := (TextDetectionInputSize / 2) - ((ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale) / 2);
        FWidth := ImageList.Source[0].MultiResBitmap[0].Bitmap.Width * FScale;

        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),

          RectF(
          FLeft,
          0,
          FWidth + FLeft,
          TextDetectionInputSize),
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

function GetTextList(Probability: Float32; NMS: Integer; OutputData: POutputDataTextDetection): TTextList;
var
  i, X, Y: DWORD;
  FListNMS: array of TText;
  FRect: TRectF;
  FExist: Boolean;
begin
  SetLength(Result.Texts, 0);
  Result.Count := 0;

  SetLength(FListNMS, 0);

  Y := 0;

  while True do
  begin
    if Y > TextDetectionOutputSize then
      Break;

    if (OutputData[4][Y] >= Probability) and (OutputData[4][Y] <= 1) then
    begin
      SetLength(FListNMS, Length(FListNMS) + 1);
      FListNMS[Length(FListNMS) - 1].Rect.Left := OutputData[0][Y] - (OutputData[2][Y] * 0.5);
      FListNMS[Length(FListNMS) - 1].Rect.Top := OutputData[1][Y] - (OutputData[3][Y] * 0.5);
      FListNMS[Length(FListNMS) - 1].Rect.Width := OutputData[2][Y];
      FListNMS[Length(FListNMS) - 1].Rect.Height := OutputData[3][Y];
      FListNMS[Length(FListNMS) - 1].Probability := OutputData[4][Y];

      if Length(FListNMS) > 0 then
      begin
        for i := Y - NMS to Y + NMS - 1 do
        begin
          if (OutputData[4][i] > OutputData[4][Y]) then
          begin
            FRect.Left := OutputData[0][i] - (OutputData[2][i] * 0.5);
            FRect.Top := OutputData[1][i] - (OutputData[3][i] * 0.5);
            FRect.Width := OutputData[2][i];
            FRect.Height := OutputData[3][i];

            for X := 0 to Length(FListNMS) - 1 do
            begin
              if IntersectRect(FListNMS[X].Rect, FRect) then
              begin
                if (TextDetectionInputSize * OutputData[0][i] > FListNMS[X].Rect.Left) and
                  (TextDetectionInputSize * OutputData[0][i] < FListNMS[X].Rect.Right) and
                  (TextDetectionInputSize * OutputData[1][i] > FListNMS[X].Rect.Top) and
                  (TextDetectionInputSize * OutputData[1][i] < FListNMS[X].Rect.Bottom) and
                  (OutputData[4][i] > FListNMS[X].Probability)
                then
                begin
                  FListNMS[X].Rect.Left := FRect.Left;
                  FListNMS[X].Rect.Top := FRect.Top;
                  FListNMS[X].Rect.Width := FRect.Width;
                  FListNMS[X].Rect.Height := FRect.Height;
                  FListNMS[X].Probability := OutputData[4][i];

                end;
              end;
            end;
          end;
        end;
      end;
    end;

    Inc(Y);
  end;

  if Length(FListNMS) > 0 then
  begin
    for Y := 0 to Length(FListNMS) - 1 do
    begin
      FExist := False;

      if (Length(Result.Texts) > 0) then
      begin
        for i := 0 to Length(Result.Texts) - 1 do
        begin
          if (IntersectRect(Result.Texts[i].Rect, FListNMS[Y].Rect)) then
          begin
            if ((Abs(Result.Texts[i].Rect.Top - FListNMS[Y].Rect.Top) < Result.Texts[i].Rect.Height / 2)) and
              ((Abs(Result.Texts[i].Rect.Bottom - FListNMS[Y].Rect.Bottom) < Result.Texts[i].Rect.Height / 2)) and
              ((Abs(Result.Texts[i].Rect.Left - FListNMS[Y].Rect.Left) < Result.Texts[i].Rect.Width / 2)) and
              ((Abs(Result.Texts[i].Rect.Right - FListNMS[Y].Rect.Right) < Result.Texts[i].Rect.Width / 2)) then
            begin
              if FListNMS[Y].Probability > Result.Texts[i].Probability then
              begin
                Result.Texts[i].Rect.Left := FListNMS[Y].Rect.Left;
                Result.Texts[i].Rect.Top := FListNMS[Y].Rect.Top;
                Result.Texts[i].Rect.Right := FListNMS[Y].Rect.Right;
                Result.Texts[i].Rect.Bottom := FListNMS[Y].Rect.Bottom;
                Result.Texts[i].Probability := FListNMS[Y].Probability;
              end;

              FExist := True;
              Break;
            end;
          end;
        end;
      end;

      if (FExist = False) then
      begin
        SetLength(Result.Texts, Length(Result.Texts) + 1);
        Result.Texts[Length(Result.Texts) - 1].Rect.Left := FListNMS[Y].Rect.Left;
        Result.Texts[Length(Result.Texts) - 1].Rect.Top := FListNMS[Y].Rect.Top;
        Result.Texts[Length(Result.Texts) - 1].Rect.Width := FListNMS[Y].Rect.Width;
        Result.Texts[Length(Result.Texts) - 1].Rect.Height := FListNMS[Y].Rect.Height;
        Result.Texts[Length(Result.Texts) - 1].Probability := FListNMS[Y].Probability;

        Result.Count := Length(Result.Texts);
      end;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, X, Y, FPixel: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FInputData: PInputDataTextDetection;
  FOutputData: POutputDataTextDetection;
  FStatus: TFLiteStatus;
  FTextList: TTextList;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.Read, FBitmapData)) then
  begin
    try
      GetMem(FInputData, TextDetection.Input.Tensors[0].DataSize);
      try
        FPixel := 0;

        for Y := 0 to TextDetectionInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to TextDetectionInputSize - 1 do
          begin
            FInputData[0][FPixel] := (TAlphaColorRec(FColors[X]).R / 255);
            FInputData[1][FPixel] := (TAlphaColorRec(FColors[X]).G / 255);
            FInputData[2][FPixel] := (TAlphaColorRec(FColors[X]).B / 255);

            Inc(FPixel);
          end;
        end;

        FStatus := TextDetection.SetInputData(0, FInputData, TextDetection.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := TextDetection.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      GetMem(FOutputData, TextDetection.Output.Tensors[0].DataSize);
      try
        FStatus := TextDetection.GetOutputData(0, FOutputData, TextDetection.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
          Exit;

        FTextList := GetTextList(0.25, 10, FOutputData);

        ImageMain.Bitmap.Canvas.BeginScene;
        try
          if FTextList.Count > 0 then
          begin
            for i := 0 to FTextList.Count - 1 do
            begin
              ImageMain.Bitmap.Canvas.Stroke.Thickness := 2;
              ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.red;
              ImageMain.Bitmap.Canvas.DrawRect(FTextList.Texts[i].Rect, 0, 0, AllCorners, 1);
            end;
          end;
        finally
          ImageMain.Bitmap.Canvas.EndScene;
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
  TextDetection := TTensorFlowLiteFMX.Create(Self);

  // download model and put to folder .../models  -  https://drive.google.com/file/d/1_-Rn9t8UrA22w9SGLuIEuZTeSoTr2v2M/view?usp=sharing

  //TextDetection.LoadModel(ModelsPath + 'text_detect.tflite', 12);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TextDetection.Destroy;
end;
end.
