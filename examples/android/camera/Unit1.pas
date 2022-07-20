unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Utils,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Media, System.Permissions, Androidapi.Helpers,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os;

type
  TForm1 = class(TForm)
    Image1: TImage;
    CameraComponent1: TCameraComponent;
    Button1: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure Button1Click(Sender: TObject);
  private
    FPermissionCamera: String;
    procedure DisplayCameraPreviewFrame;
  public
    { Public declarations }
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  if CameraComponent1.Active = False then
  begin
    CameraComponent1.Active := True;
    Button1.Text := 'Stop';
  end
  else
  begin
    CameraComponent1.Active := False;
    Button1.Text := 'Start';
  end;
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

var
  FTickCount: Int32 = 0;

procedure TForm1.DisplayCameraPreviewFrame;
var
  i, X, Y, FPixel: DWORD;
  FColors: PAlphaColorArray;
  FBitmap: TBitmap;
  FBitmapData: TBitmapData;
  FInputData: PInputDataFaceDetection;
  FOutputData: POutputDataFaceDetection;
  FRect: TRectF;
  FTickCountInference: Int32;
  FFaceList: TFaceList;
begin
  CameraComponent1.SampleBufferToBitmap(Image1.Bitmap, False);

  FTickCountInference := TThread.GetTickCount;

  Image1.Bitmap.Canvas.BeginScene;
  try
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := 160;
      FBitmap.Height := 160;

      FBitmap.Canvas.BeginScene;
      try
        if Image1.Bitmap.Height > Image1.Width then
        begin
          FRect.Left := (FaceDetectionInputSize / 2) - (FaceDetectionInputSize / (Image1.Bitmap.Height / Image1.Bitmap.Width)) / 2;
          FRect.Top := 0;
          FRect.Width := FaceDetectionInputSize - FRect.Left * 2;
          FRect.Height := FaceDetectionInputSize;
        end
        else
        begin
          FRect.Left := 0;
          FRect.Top := (FaceDetectionInputSize / 2) - (FaceDetectionInputSize / (Image1.Bitmap.Width / Image1.Bitmap.Height)) / 2;
          FRect.Width := FaceDetectionInputSize;
          FRect.Height := FaceDetectionInputSize - FRect.Top * 2;
        end;

        FBitmap.Canvas.DrawBitmap(
          Image1.Bitmap,
          RectF(0, 0, Image1.Bitmap.Width, Image1.Bitmap.Height),
          FRect,
          1, False);

      finally
        FBitmap.Canvas.EndScene;
      end;

      if (FBitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
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

            FaceDetection.SetInputData(0, FInputData, FaceDetection.Input.Tensors[0].DataSize);
          finally
            FreeMem(FInputData);
          end;

        finally
          FBitmap.Unmap(FBitmapData);
        end;
      end;

      FaceDetection.Inference;

      GetMem(FOutputData, FaceDetection.Output.Tensors[0].DataSize);
      try
        FaceDetection.GetOutputData(0, FOutputData, FaceDetection.Output.Tensors[0].DataSize);

        FFaceList := GetFaceList(0.8, 10, FOutputData);

        if FFaceList.Count > 0 then
        begin

          Image1.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;
          Image1.Bitmap.Canvas.Stroke.Thickness := 2.0;

          for i := 0 to FFaceList.Count - 1 do
          begin
            Image1.Bitmap.Canvas.DrawRect(
              RectF(
              FFaceList.Faces[i].Rect.Left * (Image1.Bitmap.Width / FBitmap.Width),
              FFaceList.Faces[i].Rect.Top * (Image1.Bitmap.Height / FBitmap.Height),
              FFaceList.Faces[i].Rect.Right * (Image1.Bitmap.Width / FBitmap.Width),
              FFaceList.Faces[i].Rect.Bottom * (Image1.Bitmap.Height / FBitmap.Height)),
              0, 0, AllCorners, 1);
          end;

        end;

      finally
        FreeMem(FOutputData);
      end;
    finally
      FBitmap.Free;
    end;
  finally
    Image1.Bitmap.Canvas.EndScene;
  end;

  if TThread.GetTickCount - FTickCount >= 1000 then
  begin
    Label1.Text := 'FPS: ' + IntToStr(Round(1 / ((TThread.GetTickCount - FTickCountInference) / 1000)));

    FTickCount := TThread.GetTickCount;
  end;
end;

procedure TForm1.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  TThread.Synchronize(TThread.CurrentThread, DisplayCameraPreviewFrame);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.MultiResBitmap.Add;
  Image1.MultiResBitmap[0].Bitmap.Width := 800;
  Image1.MultiResBitmap[0].Bitmap.Height := 800;

  FaceDetection := TTensorFlowLiteFMX.Create(Self);
  FaceDetection.UseGpu := True;

  FaceDetection.LoadModel('face_detection_160.tflite', 8);

  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  PermissionsService.RequestPermissions([FPermissionCamera], nil, nil);
end;

end.
