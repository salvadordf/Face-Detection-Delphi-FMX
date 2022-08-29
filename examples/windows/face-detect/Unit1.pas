unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Vcl.Graphics,
{$ENDIF}System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Utils,
  System.ImageList, FMX.ImgList, FMX.ListBox, FMX.Edit;

type
  TForm1 = class(TForm)
    ImageMain: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ComboBox2: TComboBox;
    Label2: TLabel;
    ComboBox3: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
  private

  public
    procedure LoadImage;
    procedure ReloadModel;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


uses TensorFlowLiteFMX;

const
  ModelsPath = '..\..\..\..\..\models\';

var
  FaceDetection: TTensorFlowLiteFMX;

  // AMD Ryzen 5 3500X, Windows 10, 8 threads, CPU
var
  // tflite models with static input shape
  // 160, 1575 - image with 160x160 pixels, inference time 0.009 sec, 110 frame per sec, good detection quality, perfect for selfies
  // 192, 2268
  // 256, 4032
  // 320, 6300 - image with 320x320 pixels, inference time 0.031 sec, 32 frame per sec, high detection quality
  // 480, 14175 - image with 480x480 pixels, inference time 0.064 sec, 15 frame per sec, high detection quality
  // 640, 25200 - image with 640x640 pixels, inference time 0.109 sec, 9 frame per sec, high quality detection
  // 800, 39375 - image with 800x800pixels, inference time 0.109 sec, 9 frame per sec, high quality detection

  FaceDetectionInputSize: Int32 = 640;
  FaceDetectionOutputSize: Int32 = 25200;

type
  PInputDataFaceDetection = ^TInputDataFaceDetection;
  TInputDataFaceDetection = array [0 .. 800 * 800 - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceDetection = ^TOutputDataFaceDetection;
  TOutputDataFaceDetection = array [0 .. 39375 - 1] of array [0 .. 6 - 1] of Float32;

type
  TFace = record
    Rect: TRectF;
    Probability: Float32;
  end;

type
  TFaceList = record
    Faces: array of TFace;
    Count: Int32;
  end;

var
  HideProbability: Boolean = False;
  BatchSize: Int32 = 1;

procedure TForm1.LoadImage;
begin
  Label4.Text := '';

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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := FaceDetectionInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := FaceDetectionInputSize;

  ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
  try
    ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
    try
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.Clear(TAlphaColorRec.Null);

      if ImageList.Source[0].MultiResBitmap[0].Bitmap.Width > ImageList.Source[0].MultiResBitmap[0].Bitmap.Height then
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, FaceDetectionInputSize, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / FaceDetectionInputSize)),
          1, True);
      end
      else
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / FaceDetectionInputSize), FaceDetectionInputSize),
          1, True);
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

function GetFaceList(Probability: Float32; NMS: Integer; OutputData: POutputDataFaceDetection): TFaceList;
var
  i, X, Y: DWORD;
  FListNMS: array of TFace;
  FRect: TRectF;
  FExist: Boolean;
begin
  SetLength(Result.Faces, 0);
  Result.Count := 0;

  SetLength(FListNMS, 0);

  Y := 0;

  while True do
  begin
    if Y > FaceDetectionOutputSize then
      Break;

    if (OutputData[Y][4] >= Probability) and (OutputData[Y][4] <= 1.0) then
    begin
      SetLength(FListNMS, Length(FListNMS) + 1);
      FListNMS[Length(FListNMS) - 1].Rect.Left := ((FaceDetectionInputSize * OutputData[Y][0]) - ((FaceDetectionInputSize * OutputData[Y][2]) / 2));
      FListNMS[Length(FListNMS) - 1].Rect.Top := ((FaceDetectionInputSize * OutputData[Y][1]) - ((FaceDetectionInputSize * OutputData[Y][3]) / 2));
      FListNMS[Length(FListNMS) - 1].Rect.Width := (FaceDetectionInputSize * OutputData[Y][2]);
      FListNMS[Length(FListNMS) - 1].Rect.Height := (FaceDetectionInputSize * OutputData[Y][3]);
      FListNMS[Length(FListNMS) - 1].Probability := OutputData[Y][4];

      if Length(FListNMS) > 0 then
      begin
        for i := Y - NMS to Y + NMS - 1 do
        begin
          if (OutputData[i][4] > OutputData[Y][4]) then
          begin
            FRect.Left := ((FaceDetectionInputSize * OutputData[i][0]) - ((FaceDetectionInputSize * OutputData[i][2]) / 2));
            FRect.Top := ((FaceDetectionInputSize * OutputData[i][1]) - ((FaceDetectionInputSize * OutputData[i][3]) / 2));
            FRect.Width := (FaceDetectionInputSize * OutputData[i][2]);
            FRect.Height := (FaceDetectionInputSize * OutputData[i][3]);

            for X := 0 to Length(FListNMS) - 1 do
            begin
              if IntersectRect(FListNMS[X].Rect, FRect) then
              begin
                if (FaceDetectionInputSize * OutputData[i][0] > FListNMS[X].Rect.Left) and
                  (FaceDetectionInputSize * OutputData[i][0] < FListNMS[X].Rect.Right) and
                  (FaceDetectionInputSize * OutputData[i][1] > FListNMS[X].Rect.Top) and
                  (FaceDetectionInputSize * OutputData[i][1] < FListNMS[X].Rect.Bottom) and
                  (OutputData[i][4] > FListNMS[X].Probability)
                then
                begin
                  FListNMS[X].Rect.Left := FRect.Left;
                  FListNMS[X].Rect.Top := FRect.Top;
                  FListNMS[X].Rect.Width := FRect.Width;
                  FListNMS[X].Rect.Height := FRect.Height;
                  FListNMS[X].Probability := OutputData[i][4];
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

      if (Length(Result.Faces) > 0) then
      begin
        for i := 0 to Length(Result.Faces) - 1 do
        begin

          if (IntersectRect(Result.Faces[i].Rect, FListNMS[Y].Rect)) then
          begin
            if ((Abs(Result.Faces[i].Rect.Top - FListNMS[Y].Rect.Top) < Result.Faces[i].Rect.Height / 2)) and
              ((Abs(Result.Faces[i].Rect.Bottom - FListNMS[Y].Rect.Bottom) < Result.Faces[i].Rect.Height / 2)) and
              ((Abs(Result.Faces[i].Rect.Left - FListNMS[Y].Rect.Left) < Result.Faces[i].Rect.Width / 2)) and
              ((Abs(Result.Faces[i].Rect.Right - FListNMS[Y].Rect.Right) < Result.Faces[i].Rect.Width / 2)) then
            begin
              if FListNMS[Y].Probability > Result.Faces[i].Probability then
              begin
                Result.Faces[i].Probability := FListNMS[Y].Probability;
                Result.Faces[i].Rect.Left := FListNMS[Y].Rect.Left;
                Result.Faces[i].Rect.Top := FListNMS[Y].Rect.Top;
                Result.Faces[i].Rect.Right := FListNMS[Y].Rect.Right;
                Result.Faces[i].Rect.Bottom := FListNMS[Y].Rect.Bottom;
              end;

              FExist := True;
              Break;
            end;
          end;
        end;
      end;

      if (FExist = False) then
      begin
        SetLength(Result.Faces, Length(Result.Faces) + 1);
        Result.Faces[Length(Result.Faces) - 1].Rect.Left := FListNMS[Y].Rect.Left;
        Result.Faces[Length(Result.Faces) - 1].Rect.Top := FListNMS[Y].Rect.Top;
        Result.Faces[Length(Result.Faces) - 1].Rect.Width := FListNMS[Y].Rect.Width;
        Result.Faces[Length(Result.Faces) - 1].Rect.Height := FListNMS[Y].Rect.Height;
        Result.Faces[Length(Result.Faces) - 1].Probability := FListNMS[Y].Probability;

        Result.Count := Length(Result.Faces);
      end;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  FBatch, i, X, Y, FPixel: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FInputData: PInputDataFaceDetection;
  FOutputData: POutputDataFaceDetection;
  FStatus: TFLiteStatus;
  FFaceList: TFaceList;
  FRect: TRectF;
  FTickCountInference, FTickCountNMS: Int64;
begin
  BatchSize := StrToIntDef(Edit1.Text, 1);

  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      FTickCountInference := TThread.GetTickCount64;

      for FBatch := 0 to BatchSize - 1 do
      begin
        GetMem(FInputData, FaceDetection.Input.Tensors[0].DataSize);
        try
          FPixel := 0;

          for Y := 0 to FaceDetectionInputSize - 1 do
          begin
            FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

            for X := 0 to FaceDetectionInputSize - 1 do
            begin
              FInputData[FPixel][0] := (TAlphaColorRec(FColors[X]).R / 255);
              FInputData[FPixel][1] := (TAlphaColorRec(FColors[X]).G / 255);
              FInputData[FPixel][2] := (TAlphaColorRec(FColors[X]).B / 255);

              Inc(FPixel);
            end;
          end;

          FStatus := FaceDetection.SetInputData(0, FInputData, FaceDetection.Input.Tensors[0].DataSize);
        finally
          FreeMem(FInputData);
        end;

        if FStatus <> TFLiteOk then
        begin
          ShowMessage('SetInputData Error');
          Exit;
        end;

        FStatus := FaceDetection.Inference;

        if FStatus <> TFLiteOk then
        begin
          ShowMessage('Inference Error');
          Exit;
        end;

        GetMem(FOutputData, FaceDetection.Output.Tensors[0].DataSize);
        try
          FStatus := FaceDetection.GetOutputData(0, FOutputData, FaceDetection.Output.Tensors[0].DataSize);

          if FStatus <> TFLiteOk then
            Exit;

          FTickCountNMS := TThread.GetTickCount64;

          FFaceList := GetFaceList(StrToFloat(ComboBox1.Items[ComboBox1.ItemIndex]), 10, FOutputData);

          if FBatch = BatchSize - 1 then
          begin
            ImageMain.Bitmap.Canvas.BeginScene;
            try
              FRect.Width := Screen.Width;
              FRect.Height := Screen.Height;

              if FFaceList.Count > 0 then
              begin
                Label4.Text := 'detect time: ' + FloatToStr((TThread.GetTickCount64 - FTickCountInference) / 1000 / BatchSize) + ', nms time: ' + FloatToStr((TThread.GetTickCount64 - FTickCountNMS) / 1000) + ', face count: ' + IntToStr(FFaceList.Count);

                ImageMain.Bitmap.Canvas.Font.Size := 11;
                ImageMain.Bitmap.Canvas.MeasureText(FRect, '0,00', False, [], TTextAlign.Leading, TTextAlign.Leading);
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Orangered;
                ImageMain.Bitmap.Canvas.Stroke.Thickness := 1.5;

                for i := 0 to FFaceList.Count - 1 do
                begin
                  ImageMain.Bitmap.Canvas.DrawRect(FFaceList.Faces[i].Rect, 0, 0, AllCorners, 1);

                  ImageMain.Bitmap.Canvas.Fill.Color := TAlphaColorRec.White;
                  if not HideProbability then
                    ImageMain.Bitmap.Canvas.FillText(
                      RectF(FFaceList.Faces[i].Rect.Left, FFaceList.Faces[i].Rect.Top - FRect.Height, FFaceList.Faces[i].Rect.Right + FRect.Width, FFaceList.Faces[i].Rect.Bottom),
                      Copy(FloatToStr(FFaceList.Faces[i].Probability), 1, 4),
                      False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
                end;
              end;
            finally
              ImageMain.Bitmap.Canvas.EndScene;
            end;
          end;

        finally
          FreeMem(FOutputData);
        end;

      end;
    finally
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Unmap(FBitmapData);
    end;
  end;
end;

procedure TForm1.ReloadModel;
begin
  case ComboBox3.ItemIndex of
    0:
      begin
        FaceDetectionInputSize := 160;
        FaceDetectionOutputSize := 1575;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_160.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
    1:
      begin
        FaceDetectionInputSize := 192;
        FaceDetectionOutputSize := 2268;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_192.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
    2:
      begin
        FaceDetectionInputSize := 256;
        FaceDetectionOutputSize := 4032;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_256.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
    3:
      begin
        FaceDetectionInputSize := 320;
        FaceDetectionOutputSize := 6300;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_320.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
    4:
      begin
        FaceDetectionInputSize := 480;
        FaceDetectionOutputSize := 14175;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_480.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
    5:
      begin
        FaceDetectionInputSize := 640;
        FaceDetectionOutputSize := 25200;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_640.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
    6:
      begin
        FaceDetectionInputSize := 800;
        FaceDetectionOutputSize := 39375;
        FaceDetection.LoadModel(ModelsPath + 'face_detection_800.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
      end;
  end;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  ReloadModel;
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
  ReloadModel;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FaceDetection := TTensorFlowLiteFMX.Create(Self);

  // Currently Tensor Flow Lite for Windows supports only x64 CPU, GPU is not supported

  FaceDetection.LoadModel(ModelsPath + 'face_detection_640.tflite', 8);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceDetection.Destroy;
end;

end.
