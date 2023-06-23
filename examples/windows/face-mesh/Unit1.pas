unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Vcl.Graphics,
{$ENDIF}System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Utils,
  System.ImageList, FMX.ImgList, FMX.ListBox;

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
  FaceMesh: TTensorFlowLiteFMX;

  // MediaPipe Face Mesh v2
  // https://storage.googleapis.com/mediapipe-assets/Model%20Card%20MediaPipe%20Face%20Mesh%20V2.pdf
  // Undecoded face landmarks. Each landmark contains x, y, and z coordinates

const
  FaceMeshInputSize = 256;
  FaceMeshOutputSize = 1434;

type
  PInputDataFaceMesh = ^TInputDataFaceMesh;
  TInputDataFaceMesh = array [0 .. FaceMeshInputSize - 1] of array [0 .. FaceMeshInputSize - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceMesh = ^TOutputDataFaceMesh;
  TOutputDataFaceMesh = array [0 .. FaceMeshOutputSize - 1] of Float32;

procedure TForm1.LoadImage;
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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := FaceMeshInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := FaceMeshInputSize;

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
          RectF(0, 0, FaceMeshInputSize, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / FaceMeshInputSize)),
          1, False);
      end
      else
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / FaceMeshInputSize), FaceMeshInputSize),
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
  i, X, Y: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FInputData: PInputDataFaceMesh;
  FOutputData: POutputDataFaceMesh;
  FStatus: TFLiteStatus;
  FTickCountInference: Int64;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      FTickCountInference := TThread.GetTickCount;

      GetMem(FInputData, FaceMesh.Input.Tensors[0].DataSize);
      try
        for Y := 0 to FaceMeshInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to FaceMeshInputSize - 1 do
          begin
            FInputData[Y][X][0] := (TAlphaColorRec(FColors[X]).R / 255);
            FInputData[Y][X][1] := (TAlphaColorRec(FColors[X]).G / 255);
            FInputData[Y][X][2] := (TAlphaColorRec(FColors[X]).B / 255);
          end;
        end;

        FStatus := FaceMesh.SetInputData(0, FInputData, FaceMesh.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := FaceMesh.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      GetMem(FOutputData, FaceMesh.Output.Tensors[0].DataSize);
      try
        FStatus := FaceMesh.GetOutputData(0, FOutputData, FaceMesh.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
        begin
          ShowMessage('GetOutputData Error');
          Exit;
        end;

        //Caption := 'Inference time: ' + FloatToStr((TThread.GetTickCount - FTickCountInference) / 1000) + ' sec';

        ImageMain.Bitmap.Canvas.BeginScene;
        try
          i := 0;

          for Y := 0 to FaceMeshOutputSize div 3 - 1 do
          begin
            ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;

            ImageMain.Bitmap.Canvas.DrawRect(
              RectF(
              FOutputData[i],
              FOutputData[i + 1],
              FOutputData[i] + 1,
              FOutputData[i + 1] + 1),
              0, 0, AllCorners, 1);

            i := i + 3;
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

  FaceMesh := TTensorFlowLiteFMX.Create(Self);

  FaceMesh.LoadModel(ModelsPath + 'face_mesh.tflite', 12);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceMesh.Destroy;
end;

end.
