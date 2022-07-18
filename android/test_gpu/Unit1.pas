unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Utils, System.IOUtils,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, System.ImageList,
  FMX.ImgList;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ImageMain: TImage;
    Label1: TLabel;
    ImageList: TImageList;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


uses TensorFlowLiteFMX;

var
  FaceDetection: TTensorFlowLiteFMX;

var
  FaceDetectionInputSize: Int32 = 160;
  FaceDetectionOutputSize: Int32 = 1575;

type
  PInputDataFaceDetection = ^TInputDataFaceDetection;
  TInputDataFaceDetection = array [0 .. 160 * 160 - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceDetection = ^TOutputDataFaceDetection;
  TOutputDataFaceDetection = array [0 .. 1575 - 1] of array [0 .. 6 - 1] of Float32;

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

procedure TForm1.Button1Click(Sender: TObject);
var
  i, X, Y, FPixel: DWORD;
  FColors: PAlphaColorArray;
  FBitmap: TBitmap;
  FBitmapData: TBitmapData;
  FInputData: PInputDataFaceDetection;
  FOutputData: POutputDataFaceDetection;
  FStatus: TFLiteStatus;
  FTickCountInference: Int64;
  FFaceList: TFaceList;
  FPath: String;
begin
  if ImageMain.MultiResBitmap.Count > 0 then
    ImageMain.MultiResBitmap[0].Free;

  ImageMain.MultiResBitmap.Add;

  FPath := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetHomePath, '3092_160.jpg');

  if FileExists(FPath) then
  begin
    if ImageList.Source[0].MultiResBitmap.Count > 0 then
      ImageList.Source[0].MultiResBitmap[0].Free;

    ImageList.Source[0].MultiResBitmap.Add;
    ImageList.Source[0].MultiResBitmap[0].Bitmap.LoadFromFile(FPath);
  end;

  ImageMain.Bitmap.Assign(ImageList.Source[0].MultiResBitmap[0].Bitmap);

  FTickCountInference := TThread.GetTickCount;

  if (ImageList.Source[0].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
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

      FStatus := FaceDetection.Inference;

      GetMem(FOutputData, FaceDetection.Output.Tensors[0].DataSize);
      try
        FStatus := FaceDetection.GetOutputData(0, FOutputData, FaceDetection.Output.Tensors[0].DataSize);

        FFaceList := GetFaceList(0.8, 10, FOutputData);

        Label1.Text := 'input size: ' + IntToStr(FaceDetectionInputSize) + ', detect time: ' + FloatToStr((TThread.GetTickCount - FTickCountInference) / 1000) + ' sec';

        ImageMain.Bitmap.Canvas.BeginScene;
        try
          ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Orangered;
          ImageMain.Bitmap.Canvas.Stroke.Thickness := 1.5;

          if FFaceList.Count > 0 then
          begin
            for i := 0 to FFaceList.Count - 1 do
            begin
              ImageMain.Bitmap.Canvas.DrawRect(FFaceList.Faces[i].Rect, 0, 0, AllCorners, 1);
            end;
          end;
        finally
          ImageMain.Bitmap.Canvas.EndScene;
        end;

      finally
        FreeMem(FOutputData);
      end;

    finally
      ImageList.Source[0].MultiResBitmap[0].Bitmap.Unmap(FBitmapData);
    end;

  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Width := Round(Screen.Width);
  Height := Round(Screen.Height);
  FullScreen := True;

  FaceDetection := TTensorFlowLiteFMX.Create(Self);
  FaceDetection.UseGpu := True;

  if FaceDetection.LoadModel('face_detection_160.tflite', 8) = TFLiteOk then
  begin
    if FaceDetection.GpuAvailable then
      Label1.Text := 'Load model OK: TFLite Version: ' + FaceDetection.GetVersion + ', GPU is available'
    else
      Label1.Text := 'Load model OK: TFLite Version: ' + FaceDetection.GetVersion + ', GPU is NOT available'

  end
  else
    Label1.Text := 'Load model Error';
end;

end.
